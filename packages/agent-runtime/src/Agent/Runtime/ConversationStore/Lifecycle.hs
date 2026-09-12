-- | Pure conversation lifecycle. Checkpoints are opaque values: only the IO
-- shell knows how to load them. All transitions run under that shell's lock.
module Agent.Runtime.ConversationStore.Lifecycle
    ( TranscriptGeneration(..)
    , ConversationResidency(..)
    , ConversationState(..)
    , TranscriptState(..)
    , ResidentSource(..)
    , Acquisition(..)
    , acquireTranscript
    , hydratedTranscript
    , releaseHydratedTranscript
    , commitTranscript
    , commitBackendState
    , evictTranscript
    , retargetCheckpoint
    , resetState
    , snapshotFromState
    , openAiContinuation
    ) where

import Agent.Loop
    ( BackendContinuation(..), BackendRevision(..), BackendSnapshot(..)
    , ImageAttachment
    )
import Agent.Responses.Types (ResponseItem)
import Data.Text (Text)
import Data.Word (Word64)

newtype TranscriptGeneration = TranscriptGeneration Word64
    deriving (Eq, Ord, Show)

data ConversationResidency = ConversationResident | ConversationCold
    deriving (Eq, Show)

data TranscriptState checkpoint
    = ResidentTranscript ![ResponseItem] !(ResidentSource checkpoint)
    | ColdTranscript !checkpoint
    deriving (Eq, Show)

data ResidentSource checkpoint
    = CommittedResident
    | HydratedResident !checkpoint !Int
    deriving (Eq, Show)

data ConversationState checkpoint = ConversationState
    { stateGeneration :: !TranscriptGeneration
    , stateTranscript :: !(TranscriptState checkpoint)
    , stateContinuation :: !(Maybe BackendContinuation)
    , stateAttachments :: ![ImageAttachment]
    }
    deriving (Eq, Show)

-- | A resident acquisition increments the shared lease count, if needed.
-- A cold acquisition requests IO without changing state. The shell must load
-- and install the result under the SAME lock; failure leaves state untouched.
data Acquisition checkpoint
    = Acquired (ConversationState checkpoint) Bool [ResponseItem]
    | LoadCheckpoint checkpoint

acquireTranscript :: ConversationState checkpoint -> Acquisition checkpoint
acquireTranscript state = case state.stateTranscript of
    ResidentTranscript items CommittedResident -> Acquired state False items
    ResidentTranscript items (HydratedResident checkpoint readers) ->
        Acquired
            state { stateTranscript = ResidentTranscript items
                (HydratedResident checkpoint (readers + 1)) }
            True items
    ColdTranscript checkpoint -> LoadCheckpoint checkpoint

-- | Complete a successful cold acquisition before releasing the store lock.
hydratedTranscript
    :: checkpoint -> [ResponseItem] -> ConversationState checkpoint
    -> ConversationState checkpoint
hydratedTranscript checkpoint items state = state
    { stateTranscript = ResidentTranscript items (HydratedResident checkpoint 1) }

-- | The outer pair separates deciding a release from evaluating the updated
-- state. The shell scrutinizes it inside modifyMVar_'s callback, preserving
-- the original exception checkpoint without forcing the new resident value.
releaseHydratedTranscript
    :: TranscriptGeneration -> ConversationState checkpoint
    -> (ConversationState checkpoint, ())
releaseHydratedTranscript expectedGeneration state
    | state.stateGeneration /= expectedGeneration = (state, ())
    | otherwise = case state.stateTranscript of
        ResidentTranscript _ (HydratedResident checkpoint 1) ->
            (state { stateTranscript = ColdTranscript checkpoint }, ())
        ResidentTranscript items (HydratedResident checkpoint readers) ->
            (state { stateTranscript = ResidentTranscript items
                (HydratedResident checkpoint (readers - 1)) }, ())
        _ -> (state, ())

-- | Both manual commits and replacements invalidate provider continuation,
-- but leave attachments alone.
commitTranscript
    :: [ResponseItem] -> ConversationState checkpoint
    -> (ConversationState checkpoint, TranscriptGeneration)
commitTranscript transcript state =
    let generation = nextGeneration state.stateGeneration
    in ( state
            { stateGeneration = generation
            , stateTranscript = ResidentTranscript transcript CommittedResident
            , stateContinuation = Nothing
            }
       , generation
       )

commitBackendState
    :: BackendSnapshot -> ConversationState checkpoint
    -> (ConversationState checkpoint, BackendSnapshot)
commitBackendState candidate state =
    let generation = nextGeneration state.stateGeneration
        committed = candidate { backendRevision = generationRevision generation }
    in ( state
            { stateGeneration = generation
            , stateTranscript = ResidentTranscript committed.backendItems CommittedResident
            , stateContinuation = committed.backendContinuation
            }
       , committed
       )

-- | A delayed eviction cannot affect a newer generation. Active readers keep
-- their immutable items; only their final release installs the new checkpoint.
evictTranscript
    :: TranscriptGeneration -> checkpoint -> ConversationState checkpoint
    -> (ConversationState checkpoint, Bool)
evictTranscript expectedGeneration checkpoint state =
    case state.stateTranscript of
        ResidentTranscript _ CommittedResident
            | state.stateGeneration == expectedGeneration ->
                (state { stateTranscript = ColdTranscript checkpoint }, True)
        ResidentTranscript items (HydratedResident _ readers)
            | state.stateGeneration == expectedGeneration ->
                ( state { stateTranscript = ResidentTranscript items
                    (HydratedResident checkpoint readers) }
                , False
                )
        _ -> (state, False)

retargetCheckpoint
    :: checkpoint -> ConversationState checkpoint -> ConversationState checkpoint
retargetCheckpoint checkpoint state = state
    { stateTranscript = case state.stateTranscript of
        ColdTranscript _ -> ColdTranscript checkpoint
        ResidentTranscript items (HydratedResident _ readers) ->
            ResidentTranscript items (HydratedResident checkpoint readers)
        resident@ResidentTranscript{} -> resident
    }

resetState :: ConversationState checkpoint -> ConversationState checkpoint
resetState state = state
    { stateGeneration = nextGeneration state.stateGeneration
    , stateTranscript = ResidentTranscript [] CommittedResident
    , stateContinuation = Nothing
    , stateAttachments = []
    }

nextGeneration :: TranscriptGeneration -> TranscriptGeneration
nextGeneration (TranscriptGeneration generation) = TranscriptGeneration (generation + 1)

generationRevision :: TranscriptGeneration -> BackendRevision
generationRevision (TranscriptGeneration generation) = BackendRevision generation

openAiContinuation :: Maybe Text -> Maybe BackendContinuation
openAiContinuation = fmap (BackendContinuation "openai.responses")

snapshotFromState :: ConversationState checkpoint -> [ResponseItem] -> BackendSnapshot
snapshotFromState state items = BackendSnapshot
    { backendItems = items
    , backendRevision = generationRevision state.stateGeneration
    , backendContinuation = state.stateContinuation
    }
