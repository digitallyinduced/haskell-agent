-- | Attribution state shared by every physical transport used during one
-- logical Codex turn.
--
-- One 'CodexTurnState' belongs to a connection (or to a disposable
-- per-submission scope for child agents). It holds:
--
-- * the current logical turn: a turn identifier minted on the first request
--   of the turn and reused by tool-output continuations, retries, reconnects,
--   HTTP fallback, and inline compaction, together with the server's
--   first-write-wins sticky-routing token (@x-codex-turn-state@);
-- * the thread-level context window: the number of committed compactions and
--   a fresh identifier per window, which survive turn boundaries;
-- * a fallback thread identifier for requests that carry no prompt cache
--   key, so even unpersisted sessions attribute consistently.
module Agent.OpenAI.TurnState
    ( CodexTurnState
    , newCodexTurnState
    , readCodexTurnState
    , recordCodexTurnState
    , resetCodexTurnState
    , copyCodexTurnRecord
    , readCodexTurnIdentifier
    , readCodexContextWindow
    , advanceCodexContextWindow
    , resolveCodexRequestIdentity
    , transientCodexRequestIdentity
    ) where

import Agent.OpenAI.RequestIdentity
    ( CodexRequestIdentity(..)
    , CodexRequestKind
    , isHeaderSafeIdentifier
    )
import Agent.Uuid (generateUuidV7)
import Control.Applicative ((<|>))
import Data.IORef
    ( IORef
    , atomicModifyIORef'
    , newIORef
    , readIORef
    )
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock.POSIX (getPOSIXTime)

data CodexTurnIdentity = CodexTurnIdentity
    { identityTurnId :: !Text
    , identityStartedAtUnixMs :: !Int64
    }

data CodexContextWindow = CodexContextWindow
    { windowGeneration :: !Int
    , windowContextId :: !Text
    }

data CodexTurnRecord = CodexTurnRecord
    { recordFallbackThreadId :: !Text
    , recordTurn :: !(Maybe CodexTurnIdentity)
    , recordRoutingToken :: !(Maybe Text)
    , recordWindow :: !CodexContextWindow
    }

newtype CodexTurnState = CodexTurnState (IORef CodexTurnRecord)

newCodexTurnState :: IO CodexTurnState
newCodexTurnState = do
    fallbackThreadId <- generateUuidV7
    contextWindowId <- generateUuidV7
    CodexTurnState <$> newIORef CodexTurnRecord
        { recordFallbackThreadId = fallbackThreadId
        , recordTurn = Nothing
        , recordRoutingToken = Nothing
        , recordWindow = CodexContextWindow
            { windowGeneration = 0
            , windowContextId = contextWindowId
            }
        }

-- | The sticky-routing token of the current turn, if the server sent one.
readCodexTurnState :: CodexTurnState -> IO (Maybe Text)
readCodexTurnState (CodexTurnState ref) =
    (.recordRoutingToken) <$> readIORef ref

-- | Retain the first non-blank routing token of the turn. Later values are
-- ignored: Codex treats the token as first-write-wins within a turn.
recordCodexTurnState :: CodexTurnState -> Text -> IO ()
recordCodexTurnState (CodexTurnState ref) value
    | Text.null (Text.strip value) = pure ()
    | otherwise =
        atomicModifyIORef' ref \record ->
            ( record
                { recordRoutingToken = record.recordRoutingToken <|> Just value
                }
            , ()
            )

-- | End the current logical turn. The next request mints a new turn
-- identifier; the context window and fallback thread identity are kept.
resetCodexTurnState :: CodexTurnState -> IO ()
resetCodexTurnState (CodexTurnState ref) =
    atomicModifyIORef' ref \record ->
        (record { recordTurn = Nothing, recordRoutingToken = Nothing }, ())

-- | Copy the complete attribution record between two states, for example
-- onto a disposable reconnect and back once it completes.
copyCodexTurnRecord :: CodexTurnState -> CodexTurnState -> IO ()
copyCodexTurnRecord (CodexTurnState source) (CodexTurnState destination) = do
    snapshot <- readIORef source
    atomicModifyIORef' destination (const (snapshot, ()))

-- | The turn identifier minted for the current logical turn, if any request
-- has been made since the last reset.
readCodexTurnIdentifier :: CodexTurnState -> IO (Maybe Text)
readCodexTurnIdentifier (CodexTurnState ref) =
    fmap (.identityTurnId) . (.recordTurn) <$> readIORef ref

-- | The committed-compaction count and the identifier of the current
-- context window.
readCodexContextWindow :: CodexTurnState -> IO (Int, Text)
readCodexContextWindow (CodexTurnState ref) = do
    record <- readIORef ref
    pure (record.recordWindow.windowGeneration, record.recordWindow.windowContextId)

-- | Start the next context window after a compaction has been committed to
-- the conversation. Requests already in flight keep the previous window;
-- every later request reports the new generation.
advanceCodexContextWindow :: CodexTurnState -> IO ()
advanceCodexContextWindow (CodexTurnState ref) = do
    contextWindowId <- generateUuidV7
    atomicModifyIORef' ref \record ->
        ( record
            { recordWindow = CodexContextWindow
                { windowGeneration = record.recordWindow.windowGeneration + 1
                , windowContextId = contextWindowId
                }
            }
        , ()
        )

-- | Resolve the identity for a request that belongs to this state's current
-- turn, minting the turn identifier on the first request of the turn. The
-- prompt cache key, when present and header-safe, is the session and thread
-- identifier exactly as in the official client; otherwise the state's
-- fallback thread identifier is used.
resolveCodexRequestIdentity
    :: CodexTurnState
    -> CodexRequestKind
    -> Maybe Text
    -> IO CodexRequestIdentity
resolveCodexRequestIdentity (CodexTurnState ref) requestKind cacheKey = do
    existing <- readIORef ref
    turn <- case existing.recordTurn of
        Just turn -> pure turn
        Nothing -> do
            candidate <- newTurnIdentity
            atomicModifyIORef' ref \record ->
                case record.recordTurn of
                    Just turn -> (record, turn)
                    Nothing ->
                        (record { recordTurn = Just candidate }, candidate)
    record <- readIORef ref
    let threadId = fromMaybe record.recordFallbackThreadId (headerSafe cacheKey)
    pure CodexRequestIdentity
        { sessionId = threadId
        , threadId
        , turnId = turn.identityTurnId
        , windowNumber = record.recordWindow.windowGeneration
        , contextWindowId = record.recordWindow.windowContextId
        , requestKind
        , turnStartedAtUnixMs = turn.identityStartedAtUnixMs
        }

-- | Identity for a standalone request outside any turn scope. Each call is
-- its own turn in its own first context window.
transientCodexRequestIdentity
    :: CodexRequestKind
    -> Maybe Text
    -> IO CodexRequestIdentity
transientCodexRequestIdentity requestKind cacheKey = do
    turn <- newTurnIdentity
    threadId <- maybe generateUuidV7 pure (headerSafe cacheKey)
    contextWindowId <- generateUuidV7
    pure CodexRequestIdentity
        { sessionId = threadId
        , threadId
        , turnId = turn.identityTurnId
        , windowNumber = 0
        , contextWindowId
        , requestKind
        , turnStartedAtUnixMs = turn.identityStartedAtUnixMs
        }

newTurnIdentity :: IO CodexTurnIdentity
newTurnIdentity = do
    turnId <- generateUuidV7
    now <- getPOSIXTime
    pure CodexTurnIdentity
        { identityTurnId = turnId
        , identityStartedAtUnixMs = floor (now * 1000)
        }

headerSafe :: Maybe Text -> Maybe Text
headerSafe = \case
    Just value | isHeaderSafeIdentifier value -> Just value
    _ -> Nothing
