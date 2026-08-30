-- | Concurrency-safe residency control for the live root conversation.
--
-- A cold checkpoint must close over durable identity (for example a session
-- id and turn cursor), not over the transcript it is intended to release.
module Agent.CLI.Session.ConversationStore
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
    , resetConversationStore
    , withConversationTranscript
    , withConversationBackendState
    , writeConversationPreviousResponseId
    ) where

import Agent.Loop
    ( BackendContinuation(..)
    , BackendRevision(..)
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
import Control.Exception.Safe (finally, mask)
import Data.Text (Text)
import Data.Word (Word64)

newtype TranscriptGeneration = TranscriptGeneration Word64
    deriving (Eq, Ord, Show)

-- | A durable location from which an exact transcript can be reconstructed.
--
-- The description is for diagnostics only. The loader is deliberately opaque
-- so the store does not depend on PostgreSQL or session persistence details.
data TranscriptCheckpoint = TranscriptCheckpoint
    { checkpointDescription :: !Text
    , checkpointLoad :: !(IO [ResponseItem])
    }

data ConversationResidency
    = ConversationResident
    | ConversationCold
    deriving (Eq, Show)

data TranscriptState
    = ResidentTranscript ![ResponseItem] !ResidentSource
    | ColdTranscript !TranscriptCheckpoint

data ResidentSource
    = CommittedResident
    | HydratedResident !TranscriptCheckpoint !Int

data ConversationState = ConversationState
    { stateGeneration :: !TranscriptGeneration
    , stateTranscript :: !TranscriptState
    , stateContinuation :: !(Maybe BackendContinuation)
    , stateAttachments :: ![ImageAttachment]
    }

newtype ConversationStore = ConversationStore (MVar ConversationState)

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
        store@(ConversationStore stateVar)
        action =
    mask \restore -> do
        (generation, releaseHydration, snapshot) <-
            modifyMVar stateVar \state ->
                case state.stateTranscript of
                    ResidentTranscript items CommittedResident ->
                        pure
                            ( state
                            , ( state.stateGeneration
                              , False
                              , snapshotFromState state items
                              )
                            )
                    ResidentTranscript items
                            (HydratedResident checkpoint readers) ->
                        pure
                            ( state
                                { stateTranscript =
                                    ResidentTranscript
                                        items
                                        (HydratedResident
                                            checkpoint
                                            (readers + 1))
                                }
                            , ( state.stateGeneration
                              , True
                              , snapshotFromState state items
                              )
                            )
                    ColdTranscript cold -> do
                        items <- cold.checkpointLoad
                        let resident =
                                state
                                    { stateTranscript =
                                        ResidentTranscript
                                            items
                                            (HydratedResident cold 1)
                                    }
                        pure
                            ( resident
                            , ( state.stateGeneration
                              , True
                              , snapshotFromState state items
                              )
                            )
        restore (action snapshot)
            `finally`
                if releaseHydration
                    then releaseHydratedTranscript store generation
                    else pure ()

-- | Publish a newer exact transcript and return its generation token.
commitConversationTranscript
    :: ConversationStore
    -> [ResponseItem]
    -> IO TranscriptGeneration
commitConversationTranscript (ConversationStore stateVar) transcript =
    modifyMVar stateVar \state -> do
        let generation = nextGeneration state.stateGeneration
        pure
            ( state
                { stateGeneration = generation
                , stateTranscript =
                    ResidentTranscript transcript CommittedResident
                , stateContinuation = Nothing
                }
            , generation
            )

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
    modifyMVar stateVar \state -> do
        let generation = nextGeneration state.stateGeneration
        pure
            ( state
                { stateGeneration = generation
                , stateTranscript =
                    ResidentTranscript transcript CommittedResident
                , stateContinuation = Nothing
                }
            , generation
            )

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
        case state.stateTranscript of
            ResidentTranscript _ CommittedResident
                | state.stateGeneration == expectedGeneration ->
                    pure
                        ( state
                            { stateTranscript =
                                ColdTranscript checkpoint
                            }
                        , True
                        )
            ResidentTranscript items (HydratedResident _ readers)
                | state.stateGeneration == expectedGeneration ->
                    -- Readers keep their immutable value, while their final
                    -- release installs the newest durable checkpoint.
                    pure
                        ( state
                            { stateTranscript =
                                ResidentTranscript
                                    items
                                    (HydratedResident checkpoint readers)
                            }
                        , False
                        )
            _ -> pure (state, False)

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
    modifyMVar stateVar \state -> do
        let generation = nextGeneration state.stateGeneration
            committed = candidate
                { backendRevision = generationRevision generation }
        pure
            ( state
                { stateGeneration = generation
                , stateTranscript =
                    ResidentTranscript
                        committed.backendItems
                        CommittedResident
                , stateContinuation = committed.backendContinuation
                }
            , committed
            )

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
    modifyMVar_ stateVar \state ->
        pure state
            { stateGeneration = nextGeneration state.stateGeneration
            , stateTranscript =
                ResidentTranscript [] CommittedResident
            , stateContinuation = Nothing
            , stateAttachments = []
            }

nextGeneration :: TranscriptGeneration -> TranscriptGeneration
nextGeneration (TranscriptGeneration generation) =
    TranscriptGeneration (generation + 1)

generationRevision :: TranscriptGeneration -> BackendRevision
generationRevision (TranscriptGeneration generation) =
    BackendRevision generation

openAiContinuation :: Maybe Text -> Maybe BackendContinuation
openAiContinuation =
    fmap (BackendContinuation "openai.responses")

continuationResponseId :: Maybe BackendContinuation -> Maybe Text
continuationResponseId =
    fmap (.continuationToken)

snapshotFromState :: ConversationState -> [ResponseItem] -> BackendSnapshot
snapshotFromState state items = BackendSnapshot
    { backendItems = items
    , backendRevision = generationRevision state.stateGeneration
    , backendContinuation = state.stateContinuation
    }

releaseHydratedTranscript
    :: ConversationStore
    -> TranscriptGeneration
    -> IO ()
releaseHydratedTranscript
        (ConversationStore stateVar)
        expectedGeneration =
    modifyMVar_ stateVar \state ->
        if state.stateGeneration /= expectedGeneration
            then pure state
            else case state.stateTranscript of
                ResidentTranscript _
                        (HydratedResident checkpoint 1) ->
                    pure state
                        { stateTranscript = ColdTranscript checkpoint }
                ResidentTranscript items
                        (HydratedResident checkpoint readers) ->
                    pure state
                        { stateTranscript =
                            ResidentTranscript
                                items
                                (HydratedResident
                                    checkpoint
                                    (readers - 1))
                        }
                _ -> pure state
