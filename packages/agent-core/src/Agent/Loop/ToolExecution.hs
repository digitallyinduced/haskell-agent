-- | Tool admission, scheduling and scoped workers. The loop owns provider
-- checkpoints; this module owns tool execution and its recovery evidence.
module Agent.Loop.ToolExecution
    ( ToolScope
    , ToolExecutionConfig(..)
    , ToolRecoveryEvidence(..)
    , newToolScope
    , withToolScope
    , admitAsyncToolCall
    , runToolCalls
    , acknowledgeTools
    , readToolRecoveryEvidence
    , waitToolFailure
    , readToolFailure
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
data ToolExecutionConfig = ToolExecutionConfig
    { tools :: !ToolRegistry
    , dispatch :: !ToolDispatchConfig
    , approve :: !(ToolCall -> IO ToolApproval)
    , cancel :: !CancelFlag
    , onEvent :: !(LoopEvent -> IO ())
    }

-- | Immutable, atomic snapshot, read after the scope's workers join.
data ToolRecoveryEvidence = ToolRecoveryEvidence
    { unacknowledgedTools :: ![(ToolCall, Maybe ToolCallResult)]
    , committedTools :: !(Map Text ToolCall)
    }

data ToolScope = ToolScope
    { asyncToolRequests :: !(TQueue AdmittedToolCall)
    , asyncToolCalls :: !(TVar (Map Text AdmittedToolCall))
    , asyncToolScheduled :: !(TVar (IntMap ToolSchedulingPlan))
    , asyncToolOutstanding :: !(TVar Int)
    , asyncToolCompleted :: !(TQueue ToolCallResult)
    , asyncToolFailure :: !(TMVar SomeException)
    , asyncToolAcknowledged :: !(TVar (Set.Set Text))
    , asyncToolCommittedCalls :: !(TVar (Map Text ToolCall))
    }

data AdmittedToolCall = AdmittedToolCall
    { admittedCall :: !ToolCall
    , admittedResult :: !(TMVar (Maybe ToolCallResult))
    -- Recorded by the worker before event delivery; unlike admittedResult,
    -- this does not release scheduling barriers or normal result waiters.
    , admittedTrustedResult :: !(TVar (Maybe ToolCallResult))
    , admissionSequence :: !Int
    }

data AsyncToolCallConflict = AsyncToolCallConflict !Text

instance Show AsyncToolCallConflict where
    show (AsyncToolCallConflict message) = Text.unpack message

instance Exception.Exception AsyncToolCallConflict

-- | Allocate state only. All workers are scoped to 'withToolScope'.
newToolScope :: IO ToolScope
newToolScope =
    ToolScope
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
acknowledgeTools :: ToolScope -> [ToolCallResult] -> [ToolCall] -> IO ()
acknowledgeTools scope results calls = atomically do
    modifyTVar' scope.asyncToolAcknowledged $
        Set.union (Set.fromList [result.callId | result <- results])
    modifyTVar' scope.asyncToolCommittedCalls $
        Map.union (Map.fromList [(call.callId, call) | call <- calls])

readToolRecoveryEvidence :: ToolScope -> IO ToolRecoveryEvidence
readToolRecoveryEvidence scope = atomically do
    calls <- readTVar scope.asyncToolCalls
    records <- traverse
        (\record -> do
            result <- readTVar record.admittedTrustedResult
            pure (record.admittedCall, result))
        (sortOn (.admissionSequence) (Map.elems calls))
    acknowledged <- readTVar scope.asyncToolAcknowledged
    committedTools <- readTVar scope.asyncToolCommittedCalls
    pure ToolRecoveryEvidence
        { unacknowledgedTools =
            [(call, result) | (call, result) <- records, Set.notMember call.callId acknowledged]
        , committedTools
        }

readToolFailure :: ToolScope -> IO (Maybe SomeException)
readToolFailure scope = atomically (tryReadTMVar scope.asyncToolFailure)

admitAsyncToolCall :: ToolScope -> ToolCall -> IO ()
admitAsyncToolCall scope call
    | toolCallMode call /= AsyncToolCall =
        atomically $
            throwSTM $
                AsyncToolCallConflict
                    ("Backend announced a non-async tool call: " <> call.callId)
    | otherwise = do
        _ <- atomically (admitToolCall scope call)
        pure ()

admitBlockingToolCall
    :: ToolScope
    -> ToolCall
    -> IO (TMVar (Maybe ToolCallResult))
admitBlockingToolCall scope call =
    atomically (admitToolCall scope call)

admitToolCall
    :: ToolScope
    -> ToolCall
    -> STM (TMVar (Maybe ToolCallResult))
admitToolCall scope call = do
    calls <- readTVar scope.asyncToolCalls
    case Map.lookup call.callId calls of
        Just existing
            | existing.admittedCall == call ->
                pure existing.admittedResult
            | otherwise ->
                throwSTM $
                    AsyncToolCallConflict
                        ("Conflicting tool calls reused call_id " <> call.callId)
        Nothing -> do
            result <- newEmptyTMVar
            trustedResult <- newTVar Nothing
            -- The registry retains every admitted call for deduplication and
            -- recovery, so its size is also the next admission sequence.
            let record = AdmittedToolCall
                    { admittedCall = call
                    , admittedResult = result
                    , admittedTrustedResult = trustedResult
                    , admissionSequence = Map.size calls
                    }
            writeTVar
                scope.asyncToolCalls
                (Map.insert call.callId record calls)
            when (toolCallMode call == AsyncToolCall) $
                modifyTVar' scope.asyncToolOutstanding (+ 1)
            writeTQueue scope.asyncToolRequests record
            pure result

runToolCalls :: ToolScope -> [ToolCall] -> IO [ToolCallResult]
runToolCalls scope calls = do
    blocking <- catMaybes <$> traverse admit calls
    blockingResults <-
        catMaybes <$> traverse (atomically . readTMVar) blocking
    completedAsync <- atomically do
        ready <- takeAsyncToolCompletions scope
        admitted <- readTVar scope.asyncToolCalls
        committed <- readTVar scope.asyncToolCommittedCalls
        -- A restarted provider stream may omit a call that already executed.
        -- Keep its trusted evidence for host-attributed recovery, but never
        -- submit an unmatched native tool result on the normal path.
        let canonical =
                [ result
                | result <- ready
                , Just record <- [Map.lookup result.callId admitted]
                , Map.lookup result.callId committed == Just record.admittedCall
                ]
        outstanding <- readTVar scope.asyncToolOutstanding
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
                admitAsyncToolCall scope call >> pure Nothing
            BlockingToolCall ->
                Just <$> admitBlockingToolCall scope call

takeAsyncToolCompletions :: ToolScope -> STM [ToolCallResult]
takeAsyncToolCompletions scope = do
    ready <- drainTQueue scope.asyncToolCompleted
    case ready of
        _ : _ -> pure ready
        [] -> do
            outstanding <- readTVar scope.asyncToolOutstanding
            if outstanding == 0
                then pure []
                else do
                    first <- readTQueue scope.asyncToolCompleted
                    rest <- drainTQueue scope.asyncToolCompleted
                    pure (first : rest)

drainTQueue :: TQueue value -> STM [value]
drainTQueue queue =
    tryReadTQueue queue >>= \case
        Nothing -> pure []
        Just value -> (value :) <$> drainTQueue queue

-- | Run the scheduler and its children for the duration of the callback.
-- Recovery evidence remains available after every worker has been joined.
withToolScope :: ToolExecutionConfig -> ToolScope -> (Async () -> IO a) -> IO a
withToolScope config scope = withAsync (runToolScheduler config scope)

runToolScheduler :: ToolExecutionConfig -> ToolScope -> IO ()
runToolScheduler config scope = do
    request <- atomically (readTQueue scope.asyncToolRequests)
    cancelledBefore <- isCancelled config.cancel
    if cancelledBefore
        then completeCancelledRequest request
        else
            race
                (waitCancel config.cancel)
                (do
                    prepared <-
                        prepareAdmittedToolCall config request.admittedCall
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
                                        scope.asyncToolScheduled
                                        (IntMap.insert
                                            request.admissionSequence
                                            plan)
                                withAsync
                                    (runToolWorker
                                        config scope request prepared plan)
                                    \worker ->
                                        withAsync
                                            (waitCatch worker
                                                >>= completeToolRequest
                                                    scope request)
                                            \_monitor ->
                                                runToolScheduler config scope
  where
    -- Approval and scheduling are allowed to perform IO. Complete cancelled
    -- requests so blocking result waiters cannot be stranded.
    completeCancelledRequest request = do
        completeToolRequest scope request (Right Nothing)
        runToolScheduler config scope

prepareAdmittedToolCall :: ToolExecutionConfig -> ToolCall -> IO PreparedToolCall
prepareAdmittedToolCall config call
    | toolCallMode call == AsyncToolCall
        && not (toolSupportsAsync config.tools call) =
            pure $
                PreparedToolCall call $
                    ToolApprovalDenied
                        ("Tool " <> call.name
                            <> " does not support asynchronous execution.")
    | otherwise =
        prepareToolCall config call

runToolWorker
    :: ToolExecutionConfig
    -> ToolScope
    -> AdmittedToolCall
    -> PreparedToolCall
    -> ToolSchedulingPlan
    -> IO (Maybe ToolCallResult)
runToolWorker config scope request prepared plan = do
    atomically do
        scheduled <- readTVar scope.asyncToolScheduled
        check $
            not $
                IntMap.foldrWithKey
                    (\sequenceNumber earlierPlan conflicts ->
                        conflicts
                            || ( sequenceNumber < request.admissionSequence
                                && schedulingPlansConflict earlierPlan plan
                               ))
                    False
                    scheduled
    race
        (waitCancel config.cancel)
        (runPreparedToolCallWithCompletion
            (atomically . writeTVar request.admittedTrustedResult . Just)
            config
            prepared)
        >>= \case
            Left () -> pure Nothing
            Right result -> pure result

completeToolRequest
    :: ToolScope
    -> AdmittedToolCall
    -> Either SomeException (Maybe ToolCallResult)
    -> IO ()
completeToolRequest scope request outcome =
    atomically do
        modifyTVar'
            scope.asyncToolScheduled
            (IntMap.delete request.admissionSequence)
        let call = request.admittedCall
        case outcome of
            Left exception -> do
                -- Do not publish a synthetic empty completion for a crashed
                -- worker. Keeping any waiter blocked makes the tool-failure
                -- branch of the enclosing structured race authoritative.
                _ <- tryPutTMVar scope.asyncToolFailure exception
                pure ()
            Right result -> do
                putTMVar request.admittedResult result
                when (toolCallMode call == AsyncToolCall) do
                    modifyTVar'
                        scope.asyncToolOutstanding
                        (\count -> count - 1)
                    case result of
                        Nothing -> pure ()
                        Just completed ->
                            writeTQueue scope.asyncToolCompleted completed

waitToolFailure :: Async () -> ToolScope -> IO SomeException
waitToolFailure scheduler scope =
    race
        (waitCatch scheduler)
        (atomically (readTMVar scope.asyncToolFailure))
        >>= \case
            Left (Left exception) -> pure exception
            Left (Right ()) ->
                pure $
                    Exception.toException $
                        AsyncToolCallConflict
                            "Tool scheduler stopped unexpectedly."
            Right exception -> pure exception

data PreparedToolCall = PreparedToolCall !ToolCall !ToolApproval

schedulingPlanForPrepared
    :: ToolExecutionConfig
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
prepareToolCall :: ToolExecutionConfig -> ToolCall -> IO PreparedToolCall
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
    -> ToolExecutionConfig
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
            -- consumer after the tool has returned. The worker's monitor is
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
