-- | Concurrency-safe residency control for the live root conversation.
--
-- A cold checkpoint must close over durable identity (for example a session
-- id and turn cursor), not over the transcript it is intended to release.
module Agent.Runtime.ConversationStore
    ( ConversationStore
    , ConversationResidency(..)
    , TranscriptCheckpoint(..)
    , TranscriptGeneration
    , commitConversationBackendState
    , commitConversationTranscript
    , conversationResidency
    , currentTranscriptGeneration
    , evictConversationTranscript
    , modifyConversationAttachments
    , newColdConversationStore
    , newConversationStore
    , readConversationAttachments
    , readConversationPreviousResponseId
    , replaceConversationTranscript
    , retargetConversationCheckpoint
    , resetConversationStore
    , withConversationTranscript
    , withConversationBackendState
    , writeConversationPreviousResponseId
    ) where

import Agent.Loop
    ( BackendContinuation(..)
    , BackendSnapshot(..)
    , ImageAttachment
    )
import Agent.Responses.Types (ResponseItem)
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar
    , modifyMVar_
    , newMVar
    , readMVar
    )
import Control.Exception.Safe (bracket)
import Data.Text (Text)
import Agent.Runtime.ConversationStore.Lifecycle
    ( ConversationState(..), TranscriptState(..), ResidentSource(..)
    , TranscriptGeneration(..), ConversationResidency(..)
    , openAiContinuation, snapshotFromState
    )
import qualified Agent.Runtime.ConversationStore.Lifecycle as Lifecycle

-- | A durable location from which an exact transcript can be reconstructed.
--
-- The description is for diagnostics only. The loader is deliberately opaque
-- so the store does not depend on PostgreSQL or session persistence details.
data TranscriptCheckpoint = TranscriptCheckpoint
    { checkpointDescription :: !Text
    , checkpointLoad :: !(IO [ResponseItem])
    }

newtype ConversationStore = ConversationStore (MVar (ConversationState TranscriptCheckpoint))

newConversationStore
    :: Maybe Text
    -> [ResponseItem]
    -> [ImageAttachment]
    -> IO ConversationStore
newConversationStore previousResponseId transcript attachments =
    ConversationStore <$> newMVar ConversationState
        { stateGeneration = TranscriptGeneration 0
        , stateTranscript = ResidentTranscript transcript CommittedResident
        , stateContinuation = openAiContinuation previousResponseId
        , stateAttachments = attachments
        }

newColdConversationStore
    :: Maybe Text
    -> TranscriptCheckpoint
    -> [ImageAttachment]
    -> IO ConversationStore
newColdConversationStore previousResponseId checkpoint attachments =
    ConversationStore <$> newMVar ConversationState
        { stateGeneration = TranscriptGeneration 0
        , stateTranscript = ColdTranscript checkpoint
        , stateContinuation = openAiContinuation previousResponseId
        , stateAttachments = attachments
        }

conversationResidency :: ConversationStore -> IO ConversationResidency
conversationResidency (ConversationStore stateVar) = do
    state <- readMVar stateVar
    pure case state.stateTranscript of
        ResidentTranscript _ _ -> ConversationResident
        ColdTranscript _ -> ConversationCold

currentTranscriptGeneration
    :: ConversationStore
    -> IO TranscriptGeneration
currentTranscriptGeneration (ConversationStore stateVar) =
    (.stateGeneration) <$> readMVar stateVar

-- | Provide the exact transcript for a scoped operation.
--
-- A cold transcript is hydrated once, then returned to the same checkpoint
-- when the scope ends, provided no writer committed a newer generation.
withConversationTranscript
    :: ConversationStore
    -> ([ResponseItem] -> IO a)
    -> IO a
withConversationTranscript
        store
        action =
    withConversationBackendState store (action . (.backendItems))

-- | Read one immutable backend checkpoint. Transcript hydration and
-- continuation/revision lookup happen under the same store lock.
withConversationBackendState
    :: ConversationStore
    -> (BackendSnapshot -> IO a)
    -> IO a
withConversationBackendState
        (ConversationStore stateVar)
        action =
    bracket acquire release \(_, _, snapshot) ->
        action snapshot
  where
    acquire =
        modifyMVar stateVar \state ->
            case Lifecycle.acquireTranscript state of
                Lifecycle.Acquired resident releaseHydration items ->
                    pure
                        ( resident
                        , ( state.stateGeneration
                          , releaseHydration
                          , snapshotFromState state items
                          )
                        )
                Lifecycle.LoadCheckpoint cold -> do
                    -- Keep hydration under modifyMVar: a failed/cancelled load
                    -- restores the cold state, and writers cannot interleave.
                    items <- cold.checkpointLoad
                    let resident = Lifecycle.hydratedTranscript cold items state
                    pure
                        ( resident
                        , ( state.stateGeneration
                          , True
                          , snapshotFromState state items
                          )
                        )
    release (generation, releaseHydration, _) =
        if releaseHydration
            then modifyMVar_ stateVar \state ->
                case Lifecycle.releaseHydratedTranscript generation state of
                    (next, ()) -> pure next
            else pure ()

-- | Publish a newer exact transcript and return its generation token.
commitConversationTranscript
    :: ConversationStore
    -> [ResponseItem]
    -> IO TranscriptGeneration
commitConversationTranscript (ConversationStore stateVar) transcript =
    modifyMVar stateVar (pure . Lifecycle.commitTranscript transcript)

-- | Replace transcript state outside a backend commit without disturbing
-- queued images. The legacy response-id argument is ignored: replacement
-- invalidates every provider continuation.
replaceConversationTranscript
    :: ConversationStore
    -> Maybe Text
    -> [ResponseItem]
    -> IO TranscriptGeneration
replaceConversationTranscript
        (ConversationStore stateVar)
        _previousResponseId
        transcript =
    modifyMVar stateVar (pure . Lifecycle.commitTranscript transcript)

-- | Release a resident transcript only when it is still the expected version.
--
-- This makes a delayed persistence callback harmless after a concurrent newer
-- commit. Returns 'True' exactly when the transcript became cold.
evictConversationTranscript
    :: ConversationStore
    -> TranscriptGeneration
    -> TranscriptCheckpoint
    -> IO Bool
evictConversationTranscript
        (ConversationStore stateVar)
        expectedGeneration
        checkpoint =
    modifyMVar stateVar \state ->
        -- Decide inside the callback, without forcing the new state. In
        -- particular an exceptional generation comparison must restore the
        -- old MVar, not publish a deferred exception as its next value.
        case Lifecycle.evictTranscript expectedGeneration checkpoint state of
            (next, evicted) -> pure (next, evicted)

-- | Point an unchanged cold or currently hydrated transcript at an equivalent
-- durable snapshot under a different identity (for example, after /fork).
-- A committed resident transcript has no checkpoint yet and is left alone.
retargetConversationCheckpoint
    :: ConversationStore
    -> TranscriptCheckpoint
    -> IO ()
retargetConversationCheckpoint
        (ConversationStore stateVar)
        checkpoint =
    modifyMVar_ stateVar (pure . Lifecycle.retargetCheckpoint checkpoint)

readConversationPreviousResponseId
    :: ConversationStore
    -> IO (Maybe Text)
readConversationPreviousResponseId (ConversationStore stateVar) =
    continuationResponseId . (.stateContinuation) <$> readMVar stateVar

writeConversationPreviousResponseId
    :: ConversationStore
    -> Maybe Text
    -> IO ()
writeConversationPreviousResponseId (ConversationStore stateVar) value =
    modifyMVar_ stateVar \state ->
        pure state { stateContinuation = openAiContinuation value }

-- | Atomically install a provider-produced checkpoint. The store, rather than
-- the provider, assigns the next authoritative monotonic revision.
commitConversationBackendState
    :: ConversationStore
    -> BackendSnapshot
    -> IO BackendSnapshot
commitConversationBackendState (ConversationStore stateVar) candidate =
    modifyMVar stateVar (pure . Lifecycle.commitBackendState candidate)

readConversationAttachments
    :: ConversationStore
    -> IO [ImageAttachment]
readConversationAttachments (ConversationStore stateVar) =
    (.stateAttachments) <$> readMVar stateVar

modifyConversationAttachments
    :: ConversationStore
    -> ([ImageAttachment] -> ([ImageAttachment], a))
    -> IO a
modifyConversationAttachments (ConversationStore stateVar) update =
    modifyMVar stateVar \state ->
        let (attachments, result) = update state.stateAttachments
        in pure (state { stateAttachments = attachments }, result)

resetConversationStore :: ConversationStore -> IO ()
resetConversationStore (ConversationStore stateVar) =
    modifyMVar_ stateVar (pure . Lifecycle.resetState)

continuationResponseId :: Maybe BackendContinuation -> Maybe Text
continuationResponseId =
    fmap (.continuationToken)
