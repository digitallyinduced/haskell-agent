-- | Internal pure lifecycle for pending parent inputs.
--
-- The interpreter serializes drain/submit/commit-or-requeue lifecycles. Each
-- batch must be finished exactly once before draining again; enqueue and clear
-- may interleave. Clear invalidates old batches by epoch, not by submission ID.
module Agent.CLI.PendingInputs.Model
    ( PendingState
    , PendingBatch
    , PendingNoticeKind(..)
    , emptyPendingState
    , clearPendingState
    , enqueueInput
    , enqueueNotice
    , drain
    , requeue
    , commit
    , batchInputs
    , retainedCount
    , retainedBytes
    , pendingInputCountLimit
    , pendingInputByteLimit
    ) where

import Agent.CLI.InputBudget (logicalTurnInputBytes, saturatingAdd)
import Agent.Loop (TurnInput)
import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import Data.Text (Text)

pendingInputCountLimit :: Int
pendingInputCountLimit = 256

pendingInputByteLimit :: Int
pendingInputByteLimit = 8 * 1024 * 1024

data PendingNoticeKind
    = PendingMcpNotice
    | PendingSubagentNotice
    deriving (Eq)

data PendingEntry = PendingEntry
    { entryInput :: !TurnInput
    , entryBytes :: !Int
    , entryNoticeKind :: !(Maybe PendingNoticeKind)
    }

data PendingState = PendingState
    { pendingEpoch :: !Word
    , pendingQueue :: !(Seq.Seq PendingEntry)
    -- Includes the currently drained batch. That batch is still live and must
    -- continue to consume the budget until submission succeeds or it requeues.
    , pendingRetainedCount :: !Int
    , pendingRetainedBytes :: !Int
    , pendingOmissionReported :: !Bool
    }

data PendingBatch = PendingBatch
    !Word
    !(Seq.Seq PendingEntry)
    !Int
    !Int

emptyPendingState :: PendingState
emptyPendingState = PendingState 0 Seq.empty 0 0 False

retainedCount :: PendingState -> Int
retainedCount state = state.pendingRetainedCount

retainedBytes :: PendingState -> Int
retainedBytes state = state.pendingRetainedBytes

clearPendingState :: PendingState -> (PendingState, ())
clearPendingState state =
    (state { pendingEpoch = epochOf state + 1
           , pendingQueue = Seq.empty
           , pendingRetainedCount = 0
           , pendingRetainedBytes = 0
           , pendingOmissionReported = False
           }, ())

enqueueInput :: TurnInput -> PendingState -> (PendingState, Either Text ())
enqueueInput input state =
    appendEntry state (PendingEntry input (logicalTurnInputBytes input) Nothing)

-- | Queue a generated notice. MCP state is latest-only, so a newer settled
-- snapshot replaces an older queued one. Subagent completions are not
-- interchangeable and receive the same explicit bounded failure as messages.
enqueueNotice
    :: PendingNoticeKind
    -> TurnInput
    -> PendingState
    -> (PendingState, Either Text ())
enqueueNotice kind input state =
    let withoutPrevious =
            if kind == PendingMcpNotice
                then removeNotice kind state
                else state
        entry = PendingEntry input (logicalTurnInputBytes input) (Just kind)
        (next, result) = appendEntry withoutPrevious entry
    in case result of
        Right () -> (next, Right ())
        Left _ ->
            ( state
                { pendingOmissionReported = True
                }
            , if not state.pendingOmissionReported
                then Left pendingNoticeOmittedMessage
                else Right ()
            )

appendEntry :: PendingState -> PendingEntry -> (PendingState, Either Text ())
appendEntry state entry
    | nextCount > pendingInputCountLimit =
        (state, Left pendingQueueFullMessage)
    | nextBytes > pendingInputByteLimit =
        (state, Left pendingQueueFullMessage)
    | otherwise =
        ( state
            { pendingQueue = queueOf state Seq.|> entry
            , pendingRetainedCount = nextCount
            , pendingRetainedBytes = nextBytes
            }
        , Right ()
        )
  where
    nextCount = state.pendingRetainedCount + 1
    nextBytes = state.pendingRetainedBytes `saturatingAdd` entry.entryBytes

pendingQueueFullMessage :: Text
pendingQueueFullMessage =
    "Root input queue is full; wait for the root agent to consume pending messages."

removeNotice :: PendingNoticeKind -> PendingState -> PendingState
removeNotice kind state =
    state
        { pendingQueue = kept
        , pendingRetainedCount =
            max 0 (state.pendingRetainedCount - removedCount)
        , pendingRetainedBytes =
            max 0 (state.pendingRetainedBytes - removedBytes)
        }
  where
    (kept, removedCount, removedBytes) =
        foldr removeOne (Seq.empty, 0, 0) (queueOf state)
    removeOne entry (entries, count, bytes)
        | entry.entryNoticeKind == Just kind =
            ( entries
            , count + 1
            , bytes `saturatingAdd` entry.entryBytes
            )
        | otherwise = (entry Seq.<| entries, count, bytes)

drain :: PendingState -> (PendingState, PendingBatch)
drain state =
    let drained = queueOf state
    in
    (state
        { pendingQueue = Seq.empty
        , pendingOmissionReported = False
        }
    , PendingBatch
        (epochOf state)
        drained
        (Seq.length drained)
        (foldr
            (\entry total -> entry.entryBytes `saturatingAdd` total)
            0
            drained))

pendingNoticeOmittedMessage :: Text
pendingNoticeOmittedMessage =
    "Root input queue is full; one or more background notices were omitted."

requeue :: PendingBatch -> PendingState -> (PendingState, ())
requeue (PendingBatch epoch queued _ _) state =
    if epochOf state == epoch
        then
            let (requeued, removedCount, removedBytes) =
                    mergeRequeuedQueue queued (queueOf state)
            in
            ( state
                { pendingQueue = requeued
                , pendingRetainedCount =
                    max 0 (state.pendingRetainedCount - removedCount)
                , pendingRetainedBytes =
                    max 0 (state.pendingRetainedBytes - removedBytes)
                }
            , ()
            )
        else (state, ())

-- A newer MCP snapshot may arrive while an older snapshot is in the drained
-- in-flight batch. If that submission fails, requeue only the newest snapshot
-- rather than exposing both stale and current state on the next attempt.
mergeRequeuedQueue
    :: Seq.Seq PendingEntry
    -> Seq.Seq PendingEntry
    -> (Seq.Seq PendingEntry, Int, Int)
mergeRequeuedQueue drained current
    | any ((== Just PendingMcpNotice) . (.entryNoticeKind)) current =
        let (kept, removedCount, removedBytes) =
                foldr removeStaleMcp (Seq.empty, 0, 0) drained
        in (kept <> current, removedCount, removedBytes)
    | otherwise = (drained <> current, 0, 0)
  where
    removeStaleMcp entry (entries, count, bytes)
        | entry.entryNoticeKind == Just PendingMcpNotice =
            ( entries
            , count + 1
            , bytes `saturatingAdd` entry.entryBytes
            )
        | otherwise = (entry Seq.<| entries, count, bytes)

commit :: PendingBatch -> PendingState -> (PendingState, ())
commit (PendingBatch epoch _ count bytes) state =
    if epochOf state == epoch
        then
            ( state
                { pendingRetainedCount =
                    max 0 (state.pendingRetainedCount - count)
                , pendingRetainedBytes =
                    max 0 (state.pendingRetainedBytes - bytes)
                }
            , ()
            )
        else (state, ())

epochOf :: PendingState -> Word
epochOf state = state.pendingEpoch

queueOf :: PendingState -> Seq.Seq PendingEntry
queueOf state = state.pendingQueue

batchInputs :: PendingBatch -> [TurnInput]
batchInputs (PendingBatch _ entries _ _) = (.entryInput) <$> toList entries
