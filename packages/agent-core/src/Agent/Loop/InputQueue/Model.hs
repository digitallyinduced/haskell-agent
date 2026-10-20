-- | Pure queue transitions shared by steering and pending submission batches.
module Agent.Loop.InputQueue.Model
    ( InputQueueState(..)
    , InputEntry(..)
    , PendingBatch
    , PendingNoticeKind(..)
    , emptySteeringState
    , emptyPendingState
    , clearPendingState
    , enqueueInput
    , enqueueNotice
    , appendEntries
    , promoteDeferredCompletions
    , drain
    , requeue
    , commit
    , batchInputs
    , retainedCount
    , retainedBytes
    , pendingInputCountLimit
    , pendingInputByteLimit
    , steeringInputCountLimit
    , steeringInputByteLimit
    ) where

import Agent.Loop.Input (TurnInput)
import Agent.Loop.InputBudget (logicalTurnInputBytes, saturatingAdd)
import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import Data.Text (Text)

pendingInputCountLimit, pendingInputByteLimit :: Int
pendingInputCountLimit = 256
pendingInputByteLimit = 8 * 1024 * 1024

steeringInputCountLimit, steeringInputByteLimit :: Int
steeringInputCountLimit = 128
steeringInputByteLimit = 64 * 1024 * 1024

data PendingNoticeKind
    = PendingMcpNotice
    | PendingSubagentNotice
    deriving (Eq)

data InputEntry = InputEntry
    { entryInput :: !TurnInput
    , entryBytes :: !Int
    , entryNoticeKind :: !(Maybe PendingNoticeKind)
    , entryBackgroundKey :: !(Maybe Text)
    , entryWake :: !Bool
    }

data InputQueueState = InputQueueState
    { queueEpoch :: !Word
    , queueEntries :: !(Seq.Seq InputEntry)
    , queueDeferredCompletions :: !(Seq.Seq InputEntry)
    -- Retained budgets include a drained batch until it is committed.
    , queueRetainedCount :: !Int
    , queueRetainedBytes :: !Int
    , queueOmissionReported :: !Bool
    , queueClosed :: !Bool
    , queueReservedCount :: !Int
    -- A batch owns its submission lifecycle, including across a reset.
    , queueBatchActive :: !Bool
    , queueCountLimit :: !Int
    , queueByteLimit :: !Int
    }

data PendingBatch = PendingBatch
    !Word
    !(Seq.Seq InputEntry)
    !Int
    !Int

emptySteeringState :: InputQueueState
emptySteeringState = emptyPendingState
    { queueCountLimit = steeringInputCountLimit
    , queueByteLimit = steeringInputByteLimit
    }

emptyPendingState :: InputQueueState
emptyPendingState = InputQueueState
    { queueEpoch = 0
    , queueEntries = Seq.empty
    , queueDeferredCompletions = Seq.empty
    , queueRetainedCount = 0
    , queueRetainedBytes = 0
    , queueOmissionReported = False
    , queueClosed = False
    , queueReservedCount = 0
    , queueBatchActive = False
    , queueCountLimit = pendingInputCountLimit
    , queueByteLimit = pendingInputByteLimit
    }

retainedCount :: InputQueueState -> Int
retainedCount state = state.queueRetainedCount

retainedBytes :: InputQueueState -> Int
retainedBytes state = state.queueRetainedBytes

clearPendingState :: InputQueueState -> (InputQueueState, ())
clearPendingState state =
    ( state
        { queueEpoch = state.queueEpoch + 1
        , queueEntries = Seq.empty
        , queueDeferredCompletions = Seq.empty
        , queueRetainedCount = 0
        , queueRetainedBytes = 0
        , queueOmissionReported = False
        , queueClosed = False
        , queueReservedCount = 0
        }
    , ()
    )

enqueueInput :: TurnInput -> InputQueueState -> (InputQueueState, Either Text ())
enqueueInput input state =
    case appendEntries (Seq.singleton (inputEntry Nothing input)) state of
        Nothing -> (state, Left pendingQueueFullMessage)
        Just next -> (next, Right ())

-- | MCP snapshots replace older queued snapshots. Rejected replacements keep
-- the previous snapshot and report omission once until the next drain/reset.
enqueueNotice
    :: PendingNoticeKind
    -> TurnInput
    -> InputQueueState
    -> (InputQueueState, Either Text ())
enqueueNotice kind input state =
    let withoutPrevious =
            if kind == PendingMcpNotice then removeNotice kind state else state
        appended = appendEntries
            (Seq.singleton (inputEntry (Just kind) input)) withoutPrevious
    in case appended of
        Just next -> (next, Right ())
        Nothing ->
            ( state { queueOmissionReported = True }
            , if state.queueOmissionReported then Right ()
                else Left "Root input queue is full; one or more background notices were omitted."
            )

inputEntry :: Maybe PendingNoticeKind -> TurnInput -> InputEntry
inputEntry kind input = InputEntry input (logicalTurnInputBytes input) kind Nothing True

-- | Admission and budget accounting are identical for both delivery modes.
appendEntries :: Seq.Seq InputEntry -> InputQueueState -> Maybe InputQueueState
appendEntries entries state
    | nextCount > state.queueCountLimit || nextBytes > state.queueByteLimit = Nothing
    | otherwise = Just state
        { queueEntries = state.queueEntries Seq.>< entries
        , queueRetainedCount = nextCount
        , queueRetainedBytes = nextBytes
        }
  where
    nextCount = state.queueRetainedCount + Seq.length entries
    nextBytes = state.queueRetainedBytes `saturatingAdd` entryBytesTotal entries

entryBytesTotal :: Seq.Seq InputEntry -> Int
entryBytesTotal = foldr (\entry total -> entry.entryBytes `saturatingAdd` total) 0

pendingQueueFullMessage :: Text
pendingQueueFullMessage =
    "Root input queue is full; wait for the root agent to consume pending messages."

removeNotice :: PendingNoticeKind -> InputQueueState -> InputQueueState
removeNotice kind state = state
    { queueEntries = kept
    , queueRetainedCount = state.queueRetainedCount - Seq.length removed
    , queueRetainedBytes = max 0 (state.queueRetainedBytes - entryBytesTotal removed)
    }
  where
    (removed, kept) = Seq.partition ((== Just kind) . (.entryNoticeKind)) state.queueEntries

-- | Completion callbacks retain overflow without blocking their delivery gate.
-- Promotion preserves the prefix already reserved for provider acknowledgement.
promoteDeferredCompletions :: InputQueueState -> InputQueueState
promoteDeferredCompletions state =
    case Seq.viewl state.queueDeferredCompletions of
        Seq.EmptyL -> state
        entry Seq.:< remaining ->
            case appendEntries (Seq.singleton entry { entryWake = True }) state of
                Nothing -> state
                Just next -> promoteDeferredCompletions next
                    { queueDeferredCompletions = remaining }

-- | The caller serializes drain/commit-or-requeue lifecycles. Enqueue and clear
-- may interleave; clear invalidates old batches without releasing new budgets.
drain :: InputQueueState -> (InputQueueState, PendingBatch)
drain state =
    ( state { queueEntries = Seq.empty, queueOmissionReported = False }
    , PendingBatch state.queueEpoch state.queueEntries
        (Seq.length state.queueEntries) (entryBytesTotal state.queueEntries)
    )

requeue :: PendingBatch -> InputQueueState -> (InputQueueState, ())
requeue (PendingBatch epoch queued _ _) state
    | state.queueEpoch /= epoch = (state, ())
    | otherwise =
        let (kept, removedCount, removedBytes) =
                if any ((== Just PendingMcpNotice) . (.entryNoticeKind)) state.queueEntries
                    then
                        let (removed, retained) = Seq.partition
                                ((== Just PendingMcpNotice) . (.entryNoticeKind)) queued
                        in (retained, Seq.length removed, entryBytesTotal removed)
                    else (queued, 0, 0)
        in ( state
                { queueEntries = kept Seq.>< state.queueEntries
                , queueRetainedCount = max 0 (state.queueRetainedCount - removedCount)
                , queueRetainedBytes = max 0 (state.queueRetainedBytes - removedBytes)
                }
           , ()
           )

commit :: PendingBatch -> InputQueueState -> (InputQueueState, ())
commit (PendingBatch epoch _ count bytes) state
    | state.queueEpoch /= epoch = (state, ())
    | otherwise =
        ( promoteDeferredCompletions state
            { queueRetainedCount = max 0 (state.queueRetainedCount - count)
            , queueRetainedBytes = max 0 (state.queueRetainedBytes - bytes)
            }
        , ()
        )

batchInputs :: PendingBatch -> [TurnInput]
batchInputs (PendingBatch _ entries _ _) = map (.entryInput) (toList entries)
