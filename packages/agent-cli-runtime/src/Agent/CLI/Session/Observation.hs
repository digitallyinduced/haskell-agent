{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Read-only, same-user observation of a CLI-owned turn. The wire format is
-- private, explicitly versioned, and never accepts execution commands.
module Agent.CLI.Session.Observation
    ( SessionObservationPublisher
    , SessionObservationUpdate(..)
    , SessionObservationFrame(..)
    , SessionObservationState(..)
    , SessionObservationEvent(..)
    , SessionObservationEventKind(..)
    , withSessionObservationPublisher
    , withSessionObservationPublisherAt
    , withOptionalSessionObservationPublisher
    , beginObservedTurn
    , publishObservedLoopEvent
    , completeObservedTurn
    , setObservedWaiting
    , observeSession
    , observeSessionAt
    , observationSocketPath
    ) where

import Agent.Loop (LoopEvent(..), NativeAgentStatus(..))
import Agent.ToolDispatch (ToolCall, ToolCallResult(..), ToolCallMode(..), toolCallMode, toolCallResultMode)
import Agent.ToolOutcome (toolOutcomeSucceeded)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (mapConcurrently_, race_, withAsync)
import Control.Concurrent.STM
import Control.Exception.Safe (IOException, bracket, bracketOnError, catch, finally, throwIO, try)
import Control.Monad (forever, replicateM, unless, void, when)
import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict', encode)
import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (toList)
import Data.Int (Int64)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq, ViewL(..), (|>))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Word (Word64)
import GHC.Generics (Generic)
import Network.Socket
import qualified Network.Socket.ByteString as Socket
import System.Directory (createDirectoryIfMissing, getHomeDirectory, removeFile)
import System.Entropy (getEntropy)
import System.Environment (lookupEnv)
import qualified System.FileLock as FileLock
import System.FilePath (isAbsolute, (</>))
import System.Posix.Files
    ( fileMode, fileOwner, getSymbolicLinkStatus, isDirectory, isSocket
    , setFileMode
    )
import System.Posix.User (getEffectiveUserID)
import System.Timeout (timeout)

data SessionObservationState
    = ObservationRunning
    | ObservationWaiting
    | ObservationCompleted
    | ObservationInterrupted
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

data SessionObservationEventKind
    = ObservationText | ObservationReasoning | ObservationPlan
    | ObservationActivity | ObservationWarning
    | ObservationToolStarted | ObservationToolUpdated | ObservationToolOutput
    | ObservationToolFinished | ObservationToolRetracted
    | ObservationResponseRestarted | ObservationResponseDiscarded
    | ObservationResponseFailed
    | ObservationAgentStarted | ObservationAgentOutput | ObservationAgentFinished
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

data SessionObservationEvent = SessionObservationEvent
    { kind :: !SessionObservationEventKind
    , identifier :: !Text
    , name :: !Text
    , text :: !Text
    , arguments :: !Text
    , isError :: !Bool
    , summary :: !Text
    , argumentsEncrypted :: !Bool
    , isAsync :: !Bool
    , isTruncated :: !Bool
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data SessionObservationFrame = SessionObservationFrame
    { ownerID :: !Text
    , turnID :: !Text
    , sequence :: !Word64
    , generationStart :: !Int64
    , durableTurnCount :: !Int64
    , state :: !SessionObservationState
    , reset :: !Bool
    , truncated :: !Bool
    , userText :: !Text
    , events :: ![SessionObservationEvent]
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data SessionObservationUpdate
    = ObservationUnavailable
    | ObservationDisconnected
    | ObservationFrame !SessionObservationFrame
    deriving (Eq, Show)

data ObservationEnvelope = ObservationEnvelope
    { version :: !Int
    , sessionID :: !Text
    , frame :: !SessionObservationFrame
    } deriving (Generic, ToJSON, FromJSON)

data RetainedEvent = RetainedEvent
    { eventSequence :: !Word64
    , eventSize :: !Int
    , event :: !SessionObservationEvent
    }

data PublicationState = PublicationState
    { frame :: !SessionObservationFrame
    , retained :: !(Seq RetainedEvent)
    , retainedBytes :: !Int
    }

newtype SessionObservationPublisher =
    SessionObservationPublisher (TVar PublicationState)

-- The retained journal and every subscriber's immutable snapshot are bounded.
-- No subscriber has a producer-owned queue. Slow readers resume from this
-- journal, or receive an explicit replacement when its cursor has expired.
retainedByteLimit, retainedEventLimit, fieldCharacterLimit :: Int
retainedByteLimit = 4 * 1024 * 1024
retainedEventLimit = 8192
fieldCharacterLimit = 65536

observationDirectory :: IO FilePath
observationDirectory = do
    home <- getHomeDirectory
    configured <- lookupEnv "HASKELL_AGENT_OBSERVATION_DIRECTORY"
    pure (fromMaybe (home </> ".haskell-agent" </> "observation") configured)

observationSocketPath :: FilePath -> Text -> FilePath
observationSocketPath directory sessionID =
    directory </> take 24 (show (hash (Text.encodeUtf8 sessionID) :: Digest SHA256))

-- | Call only while owning the CLI session. Unlike the execution owner this
-- service never acquires, transfers, or releases a session lock.
withSessionObservationPublisher
    :: Text -> (SessionObservationPublisher -> IO a) -> IO a
withSessionObservationPublisher sessionID action =
    observationDirectory >>= \directory ->
        withSessionObservationPublisherAt directory sessionID action

-- | Observation is ancillary: failure to open its endpoint must not prevent
-- execution. Only acquisition failure falls back. Exceptions from execution
-- or resource cleanup are propagated and can never cause the action to run
-- twice.
withOptionalSessionObservationPublisher
    :: Text -> (Maybe SessionObservationPublisher -> IO a) -> IO a
withOptionalSessionObservationPublisher sessionID action = do
    entered <- newIORef False
    withSessionObservationPublisher sessionID (\publisher -> do
        writeIORef entered True
        action (Just publisher)) `catch` \(exception :: IOException) -> do
            hasEntered <- readIORef entered
            if hasEntered
                then throwIO exception
                else action Nothing

withSessionObservationPublisherAt
    :: FilePath -> Text -> (SessionObservationPublisher -> IO a) -> IO a
withSessionObservationPublisherAt directory sessionID action = do
    validateSocketPath directory (observationSocketPath directory sessionID)
    createDirectoryIfMissing True directory
    -- Reject symbolic links and foreign ownership before changing permissions.
    validateOwnedDirectory directory
    setFileMode directory 0o700
    ownerID <- randomIdentifier
    publication <- newTVarIO PublicationState
        { frame = SessionObservationFrame
            { ownerID, turnID = "", sequence = 0, generationStart = 0, durableTurnCount = 0
            , state = ObservationWaiting, reset = True, truncated = False
            , userText = "", events = []
            }
        , retained = Seq.empty
        , retainedBytes = 0
        }
    let publisher = SessionObservationPublisher publication
        path = observationSocketPath directory sessionID
    -- This is an endpoint lifetime lock, not the session execution lock.
    -- Its OS lifetime makes stale socket removal independent of PID reuse and
    -- of errors connecting to a live owner's temporarily full accept queue.
    bracket (FileLock.tryLockFile (path <> ".lock") FileLock.Exclusive)
        (mapM_ FileLock.unlockFile) \case
            Nothing -> throwIO (userError "Session observation is already owned.")
            Just _ -> do
                setFileMode (path <> ".lock") 0o600
                removeOwnedSocket path
                delivered <- replicateM 8 (newTVarIO Nothing)
                bracket (openServer path)
                    (\server -> close server `finally` removeOwnedSocket path) \server ->
                    -- Fixed workers cap subscribers and are joined on exit.
                    withAsync
                        (mapConcurrently_
                            (serveSubscribers server sessionID publication)
                            delivered)
                        (const (action publisher `finally` drainSubscribers publication delivered))

-- Give connected readers a bounded opportunity to receive the final journal
-- state. No readers means no delay; a stalled reader can delay shutdown by at
-- most 250ms. Execution's event publication never waits for subscribers.
drainSubscribers :: TVar PublicationState -> [TVar (Maybe Word64)] -> IO ()
drainSubscribers publication delivered = void $ timeout 250000 $ atomically do
    current <- readTVar publication
    cursors <- mapM readTVar delivered
    check (all (maybe True (>= current.frame.sequence)) cursors)

randomIdentifier :: IO Text
randomIdentifier =
    Text.pack . show . (hash :: BS.ByteString -> Digest SHA256) <$> getEntropy 32

validateOwnedDirectory :: FilePath -> IO ()
validateOwnedDirectory path = do
    status <- getSymbolicLinkStatus path
    uid <- getEffectiveUserID
    unless (isDirectory status && fileOwner status == uid) $
        throwIO (userError "Observation directory is not owned by the current user.")

validateSocketPath :: FilePath -> FilePath -> IO ()
validateSocketPath directory path = do
    unless (isAbsolute directory && BS.length (Text.encodeUtf8 (Text.pack path)) < 104) $
        throwIO (userError
            "Observation socket path must be absolute and shorter than 104 UTF-8 bytes; configure HASKELL_AGENT_OBSERVATION_DIRECTORY.")

validatePrivateSocket :: FilePath -> IO ()
validatePrivateSocket path = do
    status <- getSymbolicLinkStatus path
    uid <- getEffectiveUserID
    unless (isSocket status && fileOwner status == uid && fileMode status .&. 0o077 == 0) $
        throwIO (userError "Observation socket is not private to the current user.")

removeOwnedSocket :: FilePath -> IO ()
removeOwnedSocket path = do
    result <- try @_ @IOException (validatePrivateSocket path)
    case result of
        Left _ -> pure ()
        Right () -> removeFile path

openServer :: FilePath -> IO Socket
openServer path =
    bracketOnError (socket AF_UNIX Stream defaultProtocol) close \server -> do
        bind server (SockAddrUnix path)
        setFileMode path 0o600
        listen server 8
        pure server

beginObservedTurn :: SessionObservationPublisher -> Int64 -> Int64 -> Text -> IO ()
beginObservedTurn (SessionObservationPublisher publication) generationStart durableTurnCount userText = do
    turnID <- randomIdentifier
    atomically $ modifyTVar' publication \current ->
        current
            { frame = current.frame
                { turnID, generationStart, durableTurnCount
                , sequence = current.frame.sequence + 1
                , state = ObservationRunning
                , userText = boundText userText
                , truncated = Text.length userText > fieldCharacterLimit
                }
            , retained = Seq.empty
            , retainedBytes = 0
            }

completeObservedTurn :: SessionObservationPublisher -> Int64 -> Int64 -> Bool -> IO ()
completeObservedTurn (SessionObservationPublisher publication) generationStart durableTurnCount interrupted =
    atomically $ modifyTVar' publication \current ->
        current { frame = current.frame
            { generationStart, durableTurnCount
            , sequence = current.frame.sequence + 1
            , state = if interrupted then ObservationInterrupted else ObservationCompleted
            } }

setObservedWaiting :: SessionObservationPublisher -> Bool -> IO ()
setObservedWaiting (SessionObservationPublisher publication) waiting =
    atomically $ modifyTVar' publication \current ->
        current { frame = current.frame
            { sequence = current.frame.sequence + 1
            , state = if waiting then ObservationWaiting else ObservationRunning
            } }

publishObservedLoopEvent :: SessionObservationPublisher -> LoopEvent -> IO ()
publishObservedLoopEvent (SessionObservationPublisher publication) loopEvent =
    case observationEvent loopEvent of
        Nothing -> pure ()
        Just original -> do
            let event = boundEvent original
                size = eventBytes event
            atomically $ modifyTVar' publication \current ->
                let sequence = current.frame.sequence + 1
                in trimPublication current
                    { frame = current.frame
                        { sequence
                        , truncated = current.frame.truncated || original /= event
                        }
                    , retained = current.retained |> RetainedEvent sequence size event
                    , retainedBytes = current.retainedBytes + size
                    }

trimPublication :: PublicationState -> PublicationState
trimPublication current
    | current.retainedBytes <= retainedByteLimit
        && Seq.length current.retained <= retainedEventLimit = current
    | otherwise = case Seq.viewl current.retained of
        EmptyL -> current
        oldest :< remaining -> trimPublication current
            { frame = current.frame { truncated = True }
            , retained = remaining
            , retainedBytes = current.retainedBytes - oldest.eventSize
            }

boundText :: Text -> Text
boundText = Text.copy . Text.take fieldCharacterLimit

boundEvent :: SessionObservationEvent -> SessionObservationEvent
boundEvent event = event
    { identifier = boundText event.identifier
    , name = boundText event.name
    , text = boundText event.text
    , arguments = boundText event.arguments
    , summary = boundText event.summary
    , isTruncated = event.isTruncated || any ((> fieldCharacterLimit) . Text.length)
        [event.identifier, event.name, event.text, event.arguments, event.summary]
    }

eventBytes :: SessionObservationEvent -> Int
eventBytes event = 128 + sum (map (BS.length . Text.encodeUtf8)
    [event.identifier, event.name, event.text, event.arguments, event.summary])

observationEvent :: LoopEvent -> Maybe SessionObservationEvent
observationEvent = \case
    TextDelta text -> Just (message ObservationText text)
    ReasoningDelta text -> Just (message ObservationReasoning text)
    PlanDelta text -> Just (message ObservationPlan text)
    ActivityUpdated text -> Just (message ObservationActivity text)
    WarningRaised text -> Just (message ObservationWarning text)
    ResponseRestarted text -> Just (message ObservationResponseRestarted text)
    ResponseAttemptDiscarded -> Just (message ObservationResponseDiscarded "")
    ResponseAttemptFailed -> Just (message ObservationResponseFailed "")
    ToolStarted call -> Just (tool ObservationToolStarted call)
    ToolUpdated call -> Just (tool ObservationToolUpdated call)
    ToolArgumentsUpdated call -> Just (tool ObservationToolUpdated call)
    ToolOutputUpdated identifier text ->
        Just (message ObservationToolOutput (redactImageOutput text)) { identifier }
    ToolFinished result -> Just SessionObservationEvent
        { kind = ObservationToolFinished
        , identifier = result.callId
        , name = ""
        , text = redactImageOutput result.output
        , arguments = ""
        , isError = maybe False (not . toolOutcomeSucceeded) result.toolResultOutcome
        , summary = "", argumentsEncrypted = False
        , isAsync = toolCallResultMode result == AsyncToolCall
        , isTruncated = False
        }
    ToolRetracted identifier ->
        Just (message ObservationToolRetracted "") { identifier }
    NativeAgentStarted identifier name task _ ->
        Just (message ObservationAgentStarted task)
            { identifier, name = fromMaybe "" name }
    NativeAgentOutput identifier text ->
        Just (message ObservationAgentOutput text) { identifier }
    NativeAgentFinished identifier status ->
        Just (message ObservationAgentFinished (Text.pack (show status)))
            { identifier, isError = status == NativeAgentFailed }
    TurnStarted -> Nothing
    TurnFinished _ -> Nothing -- The durable append, not the provider, completes a turn.
    ModelContextReset -> Nothing
    ProviderLimitUpdated{} -> Nothing
  where
    message kind text = SessionObservationEvent
        { kind, identifier = "", name = "", text, arguments = "", isError = False
        , summary = "", argumentsEncrypted = False, isAsync = False, isTruncated = False
        }
    tool :: SessionObservationEventKind -> ToolCall -> SessionObservationEvent
    tool kind call = SessionObservationEvent
        { kind, identifier = call.callId, name = call.name, text = ""
        , arguments = if call.argumentsEncrypted then "" else call.arguments
        , isError = False
        , summary = call.name
        , argumentsEncrypted = call.argumentsEncrypted
        , isAsync = toolCallMode call == AsyncToolCall
        , isTruncated = False
        }

-- Observation carries presentation text, never inline screenshot payloads.
-- Conservatively omit mixed output too, including incomplete streamed JSON.
redactImageOutput :: Text -> Text
redactImageOutput output
    | "data:image/" `Text.isInfixOf` Text.toLower output = "Screenshot omitted from event"
    | otherwise = output

serveSubscribers :: Socket -> Text -> TVar PublicationState -> TVar (Maybe Word64) -> IO ()
serveSubscribers server sessionID publication delivered = forever $
    bracket (fst <$> accept server) close \client ->
        -- A client can only read. EOF (or any attempted command) releases its
        -- worker even while the publisher is idle.
        bracket (atomically (writeTVar delivered (Just 0)))
            (const (atomically (writeTVar delivered Nothing))) \_ ->
            void (try @_ @IOException (race_
                (void (Socket.recv client 1))
                (sendUpdates client Nothing)))
  where
    sendUpdates client previous = do
        current <- atomically do
            current <- readTVar publication
            check (not (Text.null current.frame.turnID))
            case previous of
                Just (_, sequence) -> check (current.frame.sequence /= sequence)
                Nothing -> pure ()
            pure current
        let frame = frameSince previous current
        sent <- timeout (5 * 1000000) $
            sendEnvelope client ObservationEnvelope { version = 1, sessionID, frame }
        case sent of
            Nothing -> pure ()
            Just () -> do
                atomically (writeTVar delivered (Just frame.sequence))
                threadDelay 50000
                sendUpdates client (Just (frame.turnID, frame.sequence))

frameSince :: Maybe (Text, Word64) -> PublicationState -> SessionObservationFrame
frameSince previous current =
    let reset = case previous of
            Nothing -> True
            Just (turnID, sequence) ->
                turnID /= current.frame.turnID || case Seq.viewl current.retained of
                    EmptyL -> False
                    oldest :< _ -> sequence + 1 < oldest.eventSequence
        after = maybe 0 snd previous
        retained = toList current.retained
        selected = if reset then retained else filter (\event -> event.eventSequence > after) retained
    in current.frame { reset, events = map (.event) selected }

sendEnvelope :: Socket -> ObservationEnvelope -> IO ()
sendEnvelope client envelope = do
    let bytes = LBS.toStrict (encode envelope)
        prefix = LBS.toStrict (Builder.toLazyByteString (Builder.word32BE (fromIntegral (BS.length bytes))))
    Socket.sendAll client (prefix <> bytes)

-- | Keeps watching across CLI turns/restarts. Cancellation interrupts reads and
-- reconnect waits and closes the socket before returning to the caller.
observeSession :: Text -> (SessionObservationUpdate -> IO ()) -> IO ()
observeSession sessionID callback =
    observationDirectory >>= \directory -> observeSessionAt directory sessionID callback

observeSessionAt :: FilePath -> Text -> (SessionObservationUpdate -> IO ()) -> IO ()
observeSessionAt directory sessionID callback = reconnect True
  where
    path = observationSocketPath directory sessionID
    reconnect firstAttempt = do
        connected <- newTVarIO False
        void $ try @_ @IOException $
            bracket (socket AF_UNIX Stream defaultProtocol) close \client -> do
                validateSocketPath directory path
                validateOwnedDirectory directory
                directoryStatus <- getSymbolicLinkStatus directory
                unless (fileMode directoryStatus .&. 0o077 == 0) $
                    throwIO (userError "Observation directory is not private.")
                validatePrivateSocket path
                connect client (SockAddrUnix path)
                forever do
                    envelope <- receiveEnvelope client
                    unless (envelope.version == 1 && envelope.sessionID == sessionID) $
                        throwIO (userError "Unsupported session observation protocol.")
                    atomically (writeTVar connected True)
                    callback (ObservationFrame envelope.frame)
        wasConnected <- readTVarIO connected
        when (wasConnected || firstAttempt) $
            callback (if wasConnected then ObservationDisconnected else ObservationUnavailable)
        threadDelay 500000
        reconnect False

receiveEnvelope :: Socket -> IO ObservationEnvelope
receiveEnvelope client = do
    prefix <- receiveExactly client 4
    let count = BS.foldl' (\size byte -> size * 256 + fromIntegral byte) (0 :: Int) prefix
    when (count <= 0 || count > 32 * 1024 * 1024) $
        throwIO (userError "Invalid observation frame size.")
    bytes <- receiveExactly client count
    either (throwIO . userError) pure (eitherDecodeStrict' bytes)

receiveExactly :: Socket -> Int -> IO BS.ByteString
receiveExactly client count = BS.concat . reverse <$> receive count []
  where
    receive 0 chunks = pure chunks
    receive remaining chunks = do
        bytes <- Socket.recv client (min remaining 65536)
        when (BS.null bytes) (throwIO (userError "Observation owner disconnected."))
        receive (remaining - BS.length bytes) (bytes : chunks)
