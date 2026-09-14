-- | Restore and reset persisted conversation history.
module Agent.Runtime.Session.History
    ( detectGitBranch
    , foldSessionItems
    , LiveConversation
    , TranscriptCheckpoint(..)
    , TranscriptGeneration
    , currentLiveTranscriptGeneration
    , commitLiveBackendState
    , durableTranscriptCheckpoint
    , evictLiveTranscript
    , replaceLiveConversation
    , withLiveTranscript
    , withLiveBackendState
    , readLiveAttachments
    , readLivePreviousResponseId
    , readLiveTranscript
    , retargetLiveTranscript
    , modifyLiveAttachments
    , resetLiveConversationState
    , resetLiveConversation
    , resetLiveConversationWith
    , writeLivePreviousResponseId
    , writeLiveTranscript
    ) where

import Agent.Runtime.Session
    ( SessionTurn(..)
    , TranscriptEffect(..)
    , loadActiveSession
    )
import Agent.Runtime.ConversationStore
    ( ConversationStore
    , TranscriptCheckpoint(..)
    , TranscriptGeneration
    )
import qualified Agent.Runtime.ConversationStore as ConversationStore
import Agent.Loop (BackendSnapshot, ImageAttachment)
import Agent.Responses.Types (ResponseItem)
import Agent.Tools.PlanMode
    ( PlanModeEnv
    , deactivatePlanMode
    )
import Agent.OsPath (unsafeToFilePath)
import Control.Exception.Safe
    ( SomeException
    , throwString
    , try
    )
import Control.Monad (void)
import Data.IORef
    ( IORef
    , readIORef
    )
import Data.Text (Text)
import qualified Data.Text as Text
import System.Exit (ExitCode(..))
import System.OsPath (OsPath)
import System.Process (readProcessWithExitCode)
import Agent.Store.Postgres.Connection (StorePool)

-- | The pieces of conversation state that must change together when a live
-- session is reset.
--
-- Keeping them in one value makes resets and multi-field updates atomic and
-- prevents callers from observing mismatched response IDs, transcripts, and
-- attachments.
type LiveConversation = ConversationStore

-- | Reset the live conversation atomically.
resetLiveConversationState :: LiveConversation -> IO ()
resetLiveConversationState = ConversationStore.resetConversationStore

readLivePreviousResponseId :: IORef LiveConversation -> IO (Maybe Text)
readLivePreviousResponseId ref =
    readIORef ref >>= ConversationStore.readConversationPreviousResponseId

readLiveTranscript :: IORef LiveConversation -> IO [ResponseItem]
readLiveTranscript ref =
    withLiveTranscript ref pure

withLiveTranscript
    :: IORef LiveConversation
    -> ([ResponseItem] -> IO a)
    -> IO a
withLiveTranscript ref action =
    readIORef ref >>= \store ->
        ConversationStore.withConversationTranscript store action

withLiveBackendState
    :: IORef LiveConversation
    -> (BackendSnapshot -> IO a)
    -> IO a
withLiveBackendState ref action =
    readIORef ref >>= \store ->
        ConversationStore.withConversationBackendState store action

commitLiveBackendState
    :: IORef LiveConversation
    -> BackendSnapshot
    -> IO BackendSnapshot
commitLiveBackendState ref snapshot =
    readIORef ref >>= \store ->
        ConversationStore.commitConversationBackendState store snapshot

readLiveAttachments :: IORef LiveConversation -> IO [ImageAttachment]
readLiveAttachments ref =
    readIORef ref >>= ConversationStore.readConversationAttachments

modifyLiveAttachments
    :: IORef LiveConversation
    -> ([ImageAttachment] -> ([ImageAttachment], a))
    -> IO a
modifyLiveAttachments ref update =
    readIORef ref >>= \store ->
        ConversationStore.modifyConversationAttachments store update

writeLivePreviousResponseId
    :: IORef LiveConversation
    -> Maybe Text
    -> IO ()
writeLivePreviousResponseId ref value =
    readIORef ref >>= \store ->
        ConversationStore.writeConversationPreviousResponseId store value

writeLiveTranscript
    :: IORef LiveConversation
    -> [ResponseItem]
    -> IO ()
writeLiveTranscript ref value =
    readIORef ref >>= \store ->
        void (ConversationStore.commitConversationTranscript store value)

replaceLiveConversation
    :: IORef LiveConversation
    -> Maybe Text
    -> [ResponseItem]
    -> IO TranscriptGeneration
replaceLiveConversation ref previousResponseId transcript =
    readIORef ref >>= \store ->
        ConversationStore.replaceConversationTranscript
            store previousResponseId transcript

currentLiveTranscriptGeneration
    :: IORef LiveConversation
    -> IO TranscriptGeneration
currentLiveTranscriptGeneration ref =
    readIORef ref >>= ConversationStore.currentTranscriptGeneration

evictLiveTranscript
    :: IORef LiveConversation
    -> TranscriptGeneration
    -> TranscriptCheckpoint
    -> IO Bool
evictLiveTranscript ref generation checkpoint =
    readIORef ref >>= \store ->
        ConversationStore.evictConversationTranscript
            store generation checkpoint

-- | Retarget an unchanged transcript to an equivalent durable session
-- snapshot without hydrating it.
retargetLiveTranscript
    :: IORef LiveConversation
    -> TranscriptCheckpoint
    -> IO ()
retargetLiveTranscript ref checkpoint =
    readIORef ref >>= \store ->
        ConversationStore.retargetConversationCheckpoint store checkpoint

-- | Reconstruct the exact root transcript from durable session turns.
--
-- The returned closure captures only durable identity, never the transcript
-- it is intended to release.
durableTranscriptCheckpoint
    :: StorePool
    -> OsPath
    -> Text
    -> TranscriptCheckpoint
durableTranscriptCheckpoint pool root sessionId =
    TranscriptCheckpoint
        { checkpointDescription = "session:" <> sessionId
        , checkpointLoad =
            -- Load only the checkpoint-bounded active suffix. Loading the
            -- complete immutable history would briefly rematerialize every
            -- superseded transcript before 'foldSessionItems' discarded it.
            loadActiveSession pool root sessionId >>= \case
                Left err ->
                    throwString
                        ("could not hydrate durable conversation: "
                            <> Text.unpack err)
                Right (_, turns) ->
                    pure (foldSessionItems turns)
        }

-- | Drop live conversation state without touching persisted session files.
resetLiveConversation
    :: IORef LiveConversation
    -> PlanModeEnv
    -> IO ()
resetLiveConversation =
    resetLiveConversationWith (pure ())

resetLiveConversationWith
    :: IO ()
    -> IORef LiveConversation
    -> PlanModeEnv
    -> IO ()
resetLiveConversationWith resetBackend conversationRef planMode = do
    resetBackend
    readIORef conversationRef >>= resetLiveConversationState
    deactivatePlanMode planMode

detectGitBranch :: OsPath -> IO Text
detectGitBranch cwd = do
    result <-
        (try $
            readProcessWithExitCode
                "git"
                ["-C", unsafeToFilePath cwd, "rev-parse", "--abbrev-ref", "HEAD"]
                "")
            :: IO (Either SomeException (ExitCode, String, String))
    pure $ case result of
        Right (ExitSuccess, output, _) ->
            let branch = Text.strip (Text.pack output)
            in if Text.null branch
                then ""
                else if branch == "HEAD" then "detached" else branch
        _ -> ""

-- | Apply compact turns as full transcript replacements when resuming.
foldSessionItems :: [SessionTurn] -> [ResponseItem]
foldSessionItems =
    concat . reverse . foldl' addTurn []
  where
    addTurn chunks turn = case turn.turnEffect of
        TranscriptAppend -> turn.turnItems : chunks
        TranscriptReplace -> [turn.turnItems]
        TranscriptReset -> [turn.turnItems]
