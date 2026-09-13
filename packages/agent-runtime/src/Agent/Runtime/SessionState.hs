-- | Mutable conversation state consumed by the shared turn lifecycle.
--
-- This is not a scheduler or a transactional persistence owner. The embedding
-- still serializes turns. Existing hosts may assemble this state from references
-- with different restart lifetimes; keeping those references does not duplicate
-- their contents or reset them when the frontend environment is rebuilt.
module Agent.Runtime.SessionState
    ( SessionState
    , newSessionState
    , newSessionStateWith
    , restartSessionState
    , borrowConversationRef
    , readSessionTranscript
    , withSessionTranscript
    , readSessionPreviousResponseId
    , readSessionAttachments
    , currentSessionTranscriptGeneration
    , evictSessionTranscript
    , readSessionUsage
    , addSessionUsage
    , readLastAssistant
    , clearLastAssistant
    , readAutomaticCompaction
    , installAutomaticCompaction
    , clearAutomaticCompaction
    , takeGrokContext
    , clearGrokContext
    , commitConversationPatch
    , restoreConsumedPromptContext
    , takeStartupContext
    ) where

import Agent.Loop (ImageAttachment, TokenUsage, addTokenUsage, emptyTokenUsage)
import Agent.Responses.Types (ResponseItem)
import Agent.Runtime.Compaction (AutomaticCompactionBoundary)
import Agent.Runtime.ConversationStore
    ( ConversationStore
    , TranscriptCheckpoint
    , TranscriptGeneration
    , commitConversationTranscript
    , currentTranscriptGeneration
    , evictConversationTranscript
    , newConversationStore
    , readConversationAttachments
    , readConversationPreviousResponseId
    , withConversationTranscript
    , writeConversationPreviousResponseId
    )
import Agent.Runtime.TurnState
    ( ConversationPatch(..)
    , FieldUpdate(..)
    , GrokContextUpdate(..)
    , StartupUpdate(..)
    , restoreStartupContext
    )
import Control.Monad (forM_, void)
import Data.IORef
    ( IORef, atomicModifyIORef', newIORef, readIORef, writeIORef )
import Data.Text (Text)

data SessionState = SessionState
    { stateConversation :: !(IORef ConversationStore)
    , stateStartupContext :: !(IORef (Maybe Text))
    , stateGrokFirstTurnContext :: !(IORef (Maybe Text))
    , stateUsage :: !(IORef TokenUsage)
    , stateLastAssistant :: !(IORef (Maybe Text))
    , stateAutomaticCompaction
        :: !(IORef (Maybe AutomaticCompactionBoundary))
    }

-- | Allocate an independent, empty conversation without a terminal or database.
newSessionState :: IO SessionState
newSessionState = do
    conversation <- newIORef =<< newConversationStore Nothing [] []
    startup <- newIORef Nothing
    usage <- newIORef emptyTokenUsage
    compaction <- newIORef Nothing
    newSessionStateWith conversation startup usage compaction Nothing

-- | Borrow conversation/request state whose lifetime can span provider
-- restarts, while allocating the context and last-answer state for this run.
-- The borrowed references are neither reset nor copied.
newSessionStateWith
    :: IORef ConversationStore
    -> IORef (Maybe Text)
    -> IORef TokenUsage
    -> IORef (Maybe AutomaticCompactionBoundary)
    -> Maybe Text
    -> IO SessionState
newSessionStateWith conversation startup usage compaction initialGrok = do
    grok <- newIORef initialGrok
    assistant <- newIORef Nothing
    pure SessionState
        { stateConversation = conversation
        , stateStartupContext = startup
        , stateGrokFirstTurnContext = grok
        , stateUsage = usage
        , stateLastAssistant = assistant
        , stateAutomaticCompaction = compaction
        }

-- | Rebuild the per-run state without copying or resetting references owned by
-- the surrounding session. The caller must have stopped the previous run.
restartSessionState :: SessionState -> Maybe Text -> IO SessionState
restartSessionState state =
    newSessionStateWith state.stateConversation state.stateStartupContext
        state.stateUsage state.stateAutomaticCompaction

-- | Migration seam for legacy attachment/model-selection APIs that still
-- accept the host-owned conversation slot. Do not use this to replace the slot
-- during a running turn. New consumers should use runtime operations instead.
-- This is deliberately the only reference exposed by the session state.
borrowConversationRef :: SessionState -> IORef ConversationStore
borrowConversationRef state = state.stateConversation

readSessionTranscript :: SessionState -> IO [ResponseItem]
readSessionTranscript state = withSessionTranscript state pure

withSessionTranscript :: SessionState -> ([ResponseItem] -> IO a) -> IO a
withSessionTranscript state action =
    readIORef state.stateConversation >>= \store ->
        withConversationTranscript store action

readSessionPreviousResponseId :: SessionState -> IO (Maybe Text)
readSessionPreviousResponseId state =
    readIORef state.stateConversation >>= readConversationPreviousResponseId

readSessionAttachments :: SessionState -> IO [ImageAttachment]
readSessionAttachments state =
    readIORef state.stateConversation >>= readConversationAttachments

currentSessionTranscriptGeneration :: SessionState -> IO TranscriptGeneration
currentSessionTranscriptGeneration state =
    readIORef state.stateConversation >>= currentTranscriptGeneration

evictSessionTranscript
    :: SessionState -> TranscriptGeneration -> TranscriptCheckpoint -> IO Bool
evictSessionTranscript state generation checkpoint =
    readIORef state.stateConversation >>= \store ->
        evictConversationTranscript store generation checkpoint

readSessionUsage :: SessionState -> IO TokenUsage
readSessionUsage state = readIORef state.stateUsage

addSessionUsage :: SessionState -> TokenUsage -> IO ()
addSessionUsage state delta =
    atomicModifyIORef' state.stateUsage \current ->
        (addTokenUsage current delta, ())

readLastAssistant :: SessionState -> IO (Maybe Text)
readLastAssistant state = readIORef state.stateLastAssistant

clearLastAssistant :: SessionState -> IO ()
clearLastAssistant state = writeIORef state.stateLastAssistant Nothing

readAutomaticCompaction :: SessionState -> IO (Maybe AutomaticCompactionBoundary)
readAutomaticCompaction state = readIORef state.stateAutomaticCompaction

installAutomaticCompaction :: SessionState -> AutomaticCompactionBoundary -> IO ()
installAutomaticCompaction state boundary =
    writeIORef state.stateAutomaticCompaction (Just boundary)

clearAutomaticCompaction :: SessionState -> IO ()
clearAutomaticCompaction state = writeIORef state.stateAutomaticCompaction Nothing

-- | Consume a prepared first-turn prefix once. Loading a fallback remains a
-- host operation, outside mutation of the session state.
takeGrokContext :: SessionState -> IO (Maybe Text)
takeGrokContext state =
    atomicModifyIORef' state.stateGrokFirstTurnContext \pending ->
        (Nothing, pending)

clearGrokContext :: SessionState -> IO ()
clearGrokContext state = writeIORef state.stateGrokFirstTurnContext Nothing

takeStartupContext :: SessionState -> IO (Maybe Text)
takeStartupContext state =
    atomicModifyIORef' state.stateStartupContext \pending ->
        (Nothing, pending)

-- | Restore consumed prompt context without overwriting newer skill refreshes.
-- Task-plan reminders are owned by the embedding, not this conversation state.
restoreConsumedPromptContext
    :: SessionState -> Maybe Text -> Maybe Text -> IO ()
restoreConsumedPromptContext state startup grok = do
    forM_ startup \consumed ->
        atomicModifyIORef' state.stateStartupContext \current ->
            (restoreStartupContext consumed current, ())
    forM_ grok \consumed ->
        atomicModifyIORef' state.stateGrokFirstTurnContext \current ->
            ( case current of
                Nothing -> Just consumed
                Just _ -> current
            , ()
            )

-- | Apply the existing ordered mutation protocol. In particular, a transcript
-- write invalidates every provider continuation even when the patch also sets
-- a legacy response id. Only a backend checkpoint commit installs continuation
-- together with its items and authoritative revision.
--
-- This deliberately does not claim cross-reference atomicity. Context and usage
-- modifications retain their individual atomic updates against newer writers.
commitConversationPatch :: SessionState -> ConversationPatch -> IO ()
commitConversationPatch state patch = do
    case patch.patchPreviousResponseId of
        KeepField -> pure ()
        SetField value ->
            readIORef state.stateConversation >>= \store ->
                writeConversationPreviousResponseId store value
    case patch.patchTranscript of
        KeepField -> pure ()
        SetField value ->
            readIORef state.stateConversation >>= \store ->
                void (commitConversationTranscript store value)
    restoreConsumedPromptContext state
        (case patch.patchStartupContext of
            KeepStartup -> Nothing
            RestoreStartup consumed -> Just consumed)
        (case patch.patchGrokFirstTurnContext of
            KeepGrokContext -> Nothing
            RestoreGrokContext consumed -> Just consumed)
    addSessionUsage state patch.patchUsageDelta
    case patch.patchLastAssistant of
        KeepField -> pure ()
        SetField value -> writeIORef state.stateLastAssistant value
