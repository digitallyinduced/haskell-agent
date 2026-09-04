-- | Queue parent inputs that must survive failed or interrupted submissions.
module Agent.CLI.PendingInputs
    ( PendingInputs
    , PendingNoticeKind(..)
    , newPendingInputs
    , clearPendingInputs
    , enqueuePendingInput
    , enqueuePendingNotice
    , pendingInputByteLimit
    , pendingInputCountLimit
    , withPendingInputs
    ) where

import Agent.CLI.InputBudget
    ( logicalTurnInputBytes
    , saturatingAdd
    )
import Agent.Loop (Backend(..), BackendMiddleware, TurnInput(..))
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception.Safe (mask, onException)
import Data.Foldable (toList)
import Data.IORef (IORef, atomicModifyIORef')
import qualified Data.IORef
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

data PendingInputs = PendingInputs
    (IORef PendingState)
    (MVar ())

newPendingInputs :: IO PendingInputs
newPendingInputs = PendingInputs
    <$> Data.IORef.newIORef (PendingState 0 Seq.empty 0 0 False)
    <*> newMVar ()

clearPendingInputs :: PendingInputs -> IO ()
clearPendingInputs (PendingInputs pending _) =
    atomicModifyIORef' pending \state ->
        (state { pendingEpoch = epochOf state + 1
               , pendingQueue = Seq.empty
               , pendingRetainedCount = 0
               , pendingRetainedBytes = 0
               , pendingOmissionReported = False
               }, ())

enqueuePendingInput
    :: PendingInputs
    -> TurnInput
    -> IO (Either Text ())
enqueuePendingInput (PendingInputs pending _) input =
    enqueueEntry pending (PendingEntry input (logicalTurnInputBytes input) Nothing)

-- | Queue a generated notice. MCP state is latest-only, so a newer settled
-- snapshot replaces an older queued one. Subagent completions are not
-- interchangeable and receive the same explicit bounded failure as messages.
enqueuePendingNotice
    :: PendingInputs
    -> PendingNoticeKind
    -> TurnInput
    -> IO (Either Text ())
enqueuePendingNotice (PendingInputs pending _) kind input =
    atomicModifyIORef' pending \state ->
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

enqueueEntry
    :: IORef PendingState
    -> PendingEntry
    -> IO (Either Text ())
enqueueEntry pending entry =
    atomicModifyIORef' pending \state ->
        appendEntry state entry

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

drainPendingInputs :: IORef PendingState -> IO PendingBatch
drainPendingInputs pending =
    atomicModifyIORef' pending \state ->
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

requeuePendingInputs :: IORef PendingState -> PendingBatch -> IO ()
requeuePendingInputs pending (PendingBatch epoch queued _ _) =
    atomicModifyIORef' pending \state ->
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

commitPendingInputs :: IORef PendingState -> PendingBatch -> IO ()
commitPendingInputs pending (PendingBatch epoch _ count bytes) =
    atomicModifyIORef' pending \state ->
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

withPendingInputs :: PendingInputs -> BackendMiddleware
withPendingInputs (PendingInputs pending lifecycle) (Backend submit) =
    Backend \state previous inputs onEvent ->
        mask \restore ->
            withMVar lifecycle \_ -> do
                batch <- drainPendingInputs pending
                let queued = case batch of
                        PendingBatch _ values _ _ -> values
                    requeue = requeuePendingInputs pending batch
                    prefixed
                        | Seq.null queued = inputs
                        | otherwise =
                            ((.entryInput) <$> toList queued) <> inputs
                result <- restore
                    (submit state previous prefixed onEvent)
                    `onException` requeue
                case result of
                    Left _ -> requeue
                    Right _ -> commitPendingInputs pending batch
                pure result
