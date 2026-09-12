-- | Tool admission, scheduling and scoped workers. The loop owns provider
-- checkpoints; this module owns tool execution and its recovery evidence.
module Agent.Loop.ToolManager
    ( ToolManager
    , ToolManagerConfig(..)
    , ToolRecoveryEvidence(..)
    , newToolManager
    , runToolManager
    , admitAsyncToolCall
    , runManagedToolCalls
    , acknowledgeManagedTools
    , readToolRecoveryEvidence
    , waitToolManagerFailure
    , readToolManagerFailure
    ) where

import Agent.Cancel (CancelFlag, isCancelled, waitCancel)
import Agent.Loop.Output (LoopEvent(..))
import Agent.ToolDispatch
    ( ToolCall(..), ToolCallMode(..), ToolCallResult(..), ToolOutcome(..)
    , ToolDispatchConfig(..), toolCallMode
    )
import Agent.Tools.Scheduling (ToolSchedulingPlan(..), schedulingPlansConflict)
import Agent.Tools.Types
    ( ToolRegistry, ToolApproval(..), dispatchApprovedRegisteredToolCall
    , toolSupportsAsync, toolSchedulingPlanFor
    )
import Control.Concurrent.Async (Async, race, waitCatch, withAsync)
import Control.Concurrent.STM
    ( STM
    , TMVar
    , TQueue
    , TVar
    , atomically
    , check
    , modifyTVar'
    , newEmptyTMVar
    , newEmptyTMVarIO
    , newTQueueIO
    , newTVar
    , newTVarIO
    , putTMVar
    , readTMVar
    , readTQueue
    , readTVar
    , retry
    , throwSTM
    , tryPutTMVar
    , tryReadTMVar
    , tryReadTQueue
    , writeTQueue
    , writeTVar
    )
import qualified Control.Exception.Safe as Exception
import Control.Exception.Safe (SomeException, displayException, tryAny)
import Control.Monad (when)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text

-- | Only tool-execution dependencies; no provider or conversation state.
data ToolManagerConfig = ToolManagerConfig
    { tools :: !ToolRegistry
    , dispatch :: !ToolDispatchConfig
    , approve :: !(ToolCall -> IO ToolApproval)
    , cancel :: !CancelFlag
    , onEvent :: !(LoopEvent -> IO ())
    }

-- | Immutable, atomic snapshot, read after the manager and its workers join.
data ToolRecoveryEvidence = ToolRecoveryEvidence
    { unacknowledgedTools :: ![(ToolCall, Maybe ToolCallResult)]
    , committedTools :: !(Map Text ToolCall)
    }

data ToolManager = ToolManager
    { asyncToolRequests :: !(TQueue ManagedToolCall)
    , asyncToolCalls :: !(TVar (Map Text ManagedToolCall))
    , asyncToolScheduled :: !(TVar (IntMap ToolSchedulingPlan))
    , asyncToolOutstanding :: !(TVar Int)
    , asyncToolCompleted :: !(TQueue ToolCallResult)
    , asyncToolFailure :: !(TMVar SomeException)
    , asyncToolAcknowledged :: !(TVar (Set.Set Text))
    , asyncToolCommittedCalls :: !(TVar (Map Text ToolCall))
    }

data ManagedToolCall = ManagedToolCall
    { managedCall :: !ToolCall
    , managedResult :: !(TMVar (Maybe ToolCallResult))
    -- Recorded by the worker before event delivery; unlike managedResult,
    -- this does not release scheduling barriers or normal result waiters.
    , managedTrustedResult :: !(TVar (Maybe ToolCallResult))
    , managedAdmissionSequence :: !Int
    }

data AsyncToolCallConflict = AsyncToolCallConflict !Text

instance Show AsyncToolCallConflict where
    show (AsyncToolCallConflict message) = Text.unpack message

instance Exception.Exception AsyncToolCallConflict

-- | Allocate state only. All workers are scoped to 'runToolManager'.
newToolManager :: IO ToolManager
newToolManager =
    ToolManager
        <$> newTQueueIO
        <*> newTVarIO Map.empty
        <*> newTVarIO IntMap.empty
        <*> newTVarIO 0
        <*> newTQueueIO
        <*> newEmptyTMVarIO
        <*> newTVarIO Set.empty
        <*> newTVarIO Map.empty

-- | A result is acknowledged only when the response consuming it commits,
-- not when a waiter drains it. Keep that fact across history compaction.
acknowledgeManagedTools :: ToolManager -> [ToolCallResult] -> [ToolCall] -> IO ()
acknowledgeManagedTools manager results calls = atomically do
    modifyTVar' manager.asyncToolAcknowledged $
        Set.union (Set.fromList [result.callId | result <- results])
    modifyTVar' manager.asyncToolCommittedCalls $
        Map.union (Map.fromList [(call.callId, call) | call <- calls])

readToolRecoveryEvidence :: ToolManager -> IO ToolRecoveryEvidence
readToolRecoveryEvidence manager = atomically do
    calls <- readTVar manager.asyncToolCalls
    records <- traverse
        (\record -> do
            result <- readTVar record.managedTrustedResult
            pure (record.managedCall, result))
        (sortOn (.managedAdmissionSequence) (Map.elems calls))
    acknowledged <- readTVar manager.asyncToolAcknowledged
    committedTools <- readTVar manager.asyncToolCommittedCalls
    pure ToolRecoveryEvidence
        { unacknowledgedTools =
            [(call, result) | (call, result) <- records, Set.notMember call.callId acknowledged]
        , committedTools
        }

readToolManagerFailure :: ToolManager -> IO (Maybe SomeException)
readToolManagerFailure manager = atomically (tryReadTMVar manager.asyncToolFailure)

admitAsyncToolCall :: ToolManager -> ToolCall -> IO ()
admitAsyncToolCall manager call
    | toolCallMode call /= AsyncToolCall =
        atomically $
            throwSTM $
                AsyncToolCallConflict
                    ("Backend announced a non-async tool call: " <> call.callId)
    | otherwise = do
        _ <- atomically (admitManagedToolCall manager call)
        pure ()

admitBlockingToolCall
    :: ToolManager
    -> ToolCall
    -> IO (TMVar (Maybe ToolCallResult))
admitBlockingToolCall manager call =
    atomically (admitManagedToolCall manager call)

admitManagedToolCall
    :: ToolManager
    -> ToolCall
    -> STM (TMVar (Maybe ToolCallResult))
admitManagedToolCall manager call = do
    calls <- readTVar manager.asyncToolCalls
    case Map.lookup call.callId calls of
        Just existing
            | existing.managedCall == call ->
                pure existing.managedResult
            | otherwise ->
                throwSTM $
                    AsyncToolCallConflict
                        ("Conflicting tool calls reused call_id " <> call.callId)
        Nothing -> do
            result <- newEmptyTMVar
            trustedResult <- newTVar Nothing
            -- The registry retains every admitted call for deduplication and
            -- recovery, so its size is also the next admission sequence.
            let record = ManagedToolCall
                    { managedCall = call
                    , managedResult = result
                    , managedTrustedResult = trustedResult
                    , managedAdmissionSequence = Map.size calls
                    }
            writeTVar
                manager.asyncToolCalls
                (Map.insert call.callId record calls)
            when (toolCallMode call == AsyncToolCall) $
                modifyTVar' manager.asyncToolOutstanding (+ 1)
            writeTQueue manager.asyncToolRequests record
            pure result

runManagedToolCalls :: ToolManager -> [ToolCall] -> IO [ToolCallResult]
runManagedToolCalls manager calls = do
    blocking <- catMaybes <$> traverse admit calls
    blockingResults <-
        catMaybes <$> traverse (atomically . readTMVar) blocking
    completedAsync <- atomically do
        ready <- takeAsyncToolCompletions manager
        admitted <- readTVar manager.asyncToolCalls
        committed <- readTVar manager.asyncToolCommittedCalls
        -- A restarted provider stream may omit a call that already executed.
        -- Keep its trusted evidence for host-attributed recovery, but never
        -- submit an unmatched native tool result on the normal path.
        let canonical =
                [ result
                | result <- ready
                , Just record <- [Map.lookup result.callId admitted]
                , Map.lookup result.callId committed == Just record.managedCall
                ]
        outstanding <- readTVar manager.asyncToolOutstanding
        -- An orphan-only batch is not the end of the turn while other
        -- asynchronous tools are still running. Retry also restores the queue.
        if null canonical && outstanding > 0
            then retry
            else pure canonical
    pure (blockingResults <> completedAsync)
  where
    admit call =
        case toolCallMode call of
            AsyncToolCall ->
                admitAsyncToolCall manager call >> pure Nothing
            BlockingToolCall ->
                Just <$> admitBlockingToolCall manager call

takeAsyncToolCompletions :: ToolManager -> STM [ToolCallResult]
takeAsyncToolCompletions manager = do
    ready <- drainTQueue manager.asyncToolCompleted
    case ready of
        _ : _ -> pure ready
        [] -> do
            outstanding <- readTVar manager.asyncToolOutstanding
            if outstanding == 0
                then pure []
                else do
                    first <- readTQueue manager.asyncToolCompleted
                    rest <- drainTQueue manager.asyncToolCompleted
                    pure (first : rest)

drainTQueue :: TQueue value -> STM [value]
drainTQueue queue =
    tryReadTQueue queue >>= \case
        Nothing -> pure []
        Just value -> (value :) <$> drainTQueue queue

runToolManager :: ToolManagerConfig -> ToolManager -> IO ()
runToolManager config manager = do
    request <- atomically (readTQueue manager.asyncToolRequests)
    cancelledBefore <- isCancelled config.cancel
    if cancelledBefore
        then completeCancelledRequest request
        else
            race
                (waitCancel config.cancel)
                (do
                    prepared <-
                        prepareManagedToolCall config request.managedCall
                    plan <- schedulingPlanForPrepared config prepared
                    pure (prepared, plan))
                >>= \case
                    Left () ->
                        completeCancelledRequest request
                    Right (prepared, plan) -> do
                        cancelledAfter <- isCancelled config.cancel
                        if cancelledAfter
                            then completeCancelledRequest request
                            else do
                                atomically $
                                    modifyTVar'
                                        manager.asyncToolScheduled
                                        (IntMap.insert
                                            request.managedAdmissionSequence
                                            plan)
                                withAsync
                                    (runManagedToolWorker
                                        config manager request prepared plan)
                                    \worker ->
                                        withAsync
                                            (waitCatch worker
                                                >>= completeManagedToolRequest
                                                    manager request)
                                            \_monitor ->
                                                runToolManager config manager
  where
    -- Approval and scheduling are allowed to perform IO. Complete cancelled
    -- requests so blocking result waiters cannot be stranded.
    completeCancelledRequest request = do
        completeManagedToolRequest manager request (Right Nothing)
        runToolManager config manager

prepareManagedToolCall :: ToolManagerConfig -> ToolCall -> IO PreparedToolCall
prepareManagedToolCall config call
    | toolCallMode call == AsyncToolCall
        && not (toolSupportsAsync config.tools call) =
            pure $
                PreparedToolCall call $
                    ToolApprovalDenied
                        ("Tool " <> call.name
                            <> " does not support asynchronous execution.")
    | otherwise =
        prepareToolCall config call

runManagedToolWorker
    :: ToolManagerConfig
    -> ToolManager
    -> ManagedToolCall
    -> PreparedToolCall
    -> ToolSchedulingPlan
    -> IO (Maybe ToolCallResult)
runManagedToolWorker config manager request prepared plan = do
    atomically do
        scheduled <- readTVar manager.asyncToolScheduled
        check $
            not $
                IntMap.foldrWithKey
                    (\sequenceNumber earlierPlan conflicts ->
                        conflicts
                            || ( sequenceNumber < request.managedAdmissionSequence
                                && schedulingPlansConflict earlierPlan plan
                               ))
                    False
                    scheduled
    race
        (waitCancel config.cancel)
        (runPreparedToolCallWithCompletion
            (atomically . writeTVar request.managedTrustedResult . Just)
            config
            prepared)
        >>= \case
            Left () -> pure Nothing
            Right result -> pure result

completeManagedToolRequest
    :: ToolManager
    -> ManagedToolCall
    -> Either SomeException (Maybe ToolCallResult)
    -> IO ()
completeManagedToolRequest manager request outcome =
    atomically do
        modifyTVar'
            manager.asyncToolScheduled
            (IntMap.delete request.managedAdmissionSequence)
        let call = request.managedCall
        case outcome of
            Left exception -> do
                -- Do not publish a synthetic empty completion for a crashed
                -- worker. Keeping any waiter blocked makes the manager-failure
                -- branch of the enclosing structured race authoritative.
                _ <- tryPutTMVar manager.asyncToolFailure exception
                pure ()
            Right result -> do
                putTMVar request.managedResult result
                when (toolCallMode call == AsyncToolCall) do
                    modifyTVar'
                        manager.asyncToolOutstanding
                        (\count -> count - 1)
                    case result of
                        Nothing -> pure ()
                        Just completed ->
                            writeTQueue manager.asyncToolCompleted completed

waitToolManagerFailure :: Async () -> ToolManager -> IO SomeException
waitToolManagerFailure managerWorker manager =
    race
        (waitCatch managerWorker)
        (atomically (readTMVar manager.asyncToolFailure))
        >>= \case
            Left (Left exception) -> pure exception
            Left (Right ()) ->
                pure $
                    Exception.toException $
                        AsyncToolCallConflict
                            "Async tool manager stopped unexpectedly."
            Right exception -> pure exception

data PreparedToolCall = PreparedToolCall !ToolCall !ToolApproval

schedulingPlanForPrepared
    :: ToolManagerConfig
    -> PreparedToolCall
    -> IO ToolSchedulingPlan
schedulingPlanForPrepared config (PreparedToolCall call approval) =
    case approval of
        ToolApprovalGranted ->
            toolSchedulingPlanFor config.tools call
        ToolApprovalDenied{} ->
            pure ToolUnconstrained
        ToolApprovalRejected ->
            pure ToolUnconstrained

-- | Approval may touch interactive or otherwise order-sensitive state, so it
-- is prepared serially even when the resulting handlers may run concurrently.
prepareToolCall :: ToolManagerConfig -> ToolCall -> IO PreparedToolCall
prepareToolCall config call = do
    approval <- tryAny (config.approve call)
    pure $
        PreparedToolCall call $
            case approval of
                Left exception ->
                    ToolApprovalDenied
                        ("Tool " <> call.name
                            <> " could not be prepared: "
                            <> exceptionSummary exception)
                Right decision -> decision

runPreparedToolCallWithCompletion
    :: (ToolCallResult -> IO ())
    -> ToolManagerConfig
    -> PreparedToolCall
    -> IO (Maybe ToolCallResult)
runPreparedToolCallWithCompletion completed config (PreparedToolCall call approval) = do
    cancelled <- isCancelled config.cancel
    if cancelled
        then pure Nothing
        else do
            config.onEvent (ToolStarted call)
            result <- case approval of
                ToolApprovalDenied denial ->
                    pure (deniedResult denial)
                ToolApprovalRejected ->
                    pure (deniedResult "Tool call rejected by user.")
                ToolApprovalGranted ->
                    dispatchApprovedRegisteredToolCall
                        config.dispatch
                            { toolDispatchOnOutput = \progressCall output ->
                                config.dispatch.toolDispatchOnOutput progressCall output
                                    >> config.onEvent
                                        (ToolOutputUpdated progressCall.callId output)
                            }
                        config.tools
                        call
            -- The trusted result must survive cancellation or a failing event
            -- consumer after the tool has returned. The manager's monitor is
            -- not authoritative: its own scope can be cancelled first.
            completed result
            config.onEvent (ToolFinished result)
            pure (Just result)
  where
    deniedResult message = ToolCallResult
        { callId = call.callId
        , output = message
        , callKind = call.callKind
        , toolResultMode = toolCallMode call
        , toolResultImages = []
        , toolResultOutcome = Just ToolDenied
        }

exceptionSummary :: SomeException -> Text
exceptionSummary =
    fst
        . Text.breakOn "\nHasCallStack backtrace:"
        . Text.pack
        . displayException
