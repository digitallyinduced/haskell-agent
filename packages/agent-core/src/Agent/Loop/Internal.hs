-- | Loop execution and scoped tool-worker ownership. Pure input/output,
-- checkpoint, accounting and display-journal concerns live in sibling modules.
module Agent.Loop.Internal
    ( LoopConfig(..)
    , LoopError(..)
    , LoopExecution(..)
    , LoopProgress(..)
    , LoopResult(..)
    , runLoop
    , runLoopInputs
    , runLoopInputsDetailed
    ) where

import Agent.Cancel (CancelFlag, isCancelled, waitCancel)
import Agent.Error (ApiError)
import Agent.Loop.Backend
import Agent.Loop.DisplayJournal
import Agent.Loop.EventPump
    ( EventPumpFailure(..)
    , flushEventPump
    , newEventPump
    , runEventPump
    , waitEventPumpFailure
    )
import Agent.Loop.Input
import Agent.Loop.InputItems (turnInputsToItems)
import Agent.Loop.Output
import Agent.Loop.TokenUsage
import Agent.Loop.VisibleState
import Agent.Responses.Types
import Agent.Telemetry (TurnTelemetry)
import Agent.ToolDispatch
    ( ToolCall(..)
    , ToolCallMode(..)
    , ToolOutcome(..)
    , ToolCallResult(..)
    , ToolDispatchConfig(..)
    , toolCallMode
    , withToolCallResultMode
    )
import Agent.Tools.Scheduling
    ( ToolSchedulingPlan(..)
    , schedulingPlansConflict
    )
import Agent.Tools.Types
    ( ToolRegistry
    , ToolApproval(..)
    , dispatchApprovedRegisteredToolCall
    , toolSupportsAsync
    , toolSchedulingPlanFor
    )
import Control.Concurrent.Async
    ( Async
    , race
    , waitCatch
    , withAsync
    )
import Control.Concurrent.MVar (modifyMVar, modifyMVar_, newMVar, withMVar)
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
import qualified Control.Exception as Exception
import Control.Exception.Safe
    ( SomeException
    , displayException
    , isAsyncException
    , mask
    , onException
    , tryAny
    )
import Control.Monad (when)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (IORef, atomicModifyIORef', atomicWriteIORef, modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes, fromMaybe)
import Data.List (sortOn)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import System.Timeout (timeout)

maxEmptyContinuations :: Int
maxEmptyContinuations = 2

normalizeBackendSnapshotImages :: BackendSnapshot -> BackendSnapshot
normalizeBackendSnapshotImages snapshot =
    snapshot
        { backendItems =
            map normalizeResponseItemImages snapshot.backendItems
        }

data LoopProgress
    = NoResponseCommitted
    -- Includes a provider's explicit recovery context. Display deltas alone
    -- never advance the committed checkpoint.
    | ResponseCommitted
    deriving (Eq, Show)

data LoopExecution = LoopExecution
    { executionState :: ![ResponseItem]
    -- | Inputs the loop accepted after the last committed response but never
    -- submitted successfully: the tool results (and any steering) queued for
    -- the next model step, or the initial inputs while nothing has committed.
    -- Callers that checkpoint an interrupted turn retain the tool results here
    -- next to 'executionState'. Steering stays unacknowledged until a later
    -- commit and must not be duplicated from this field.
    , executionPendingInputs :: ![TurnInput]
    , executionProgress :: !LoopProgress
    -- | Assistant text streamed since the last committed response: the sample
    -- that never committed plus restarted attempts of the same step. Text of
    -- committed samples is already represented by assistant messages in
    -- 'executionState'. This is display metadata only: callers must not add
    -- it to backend state.
    , executionUncommittedAssistantText :: !(Maybe Text)
    -- | Replayable live response events from the provider attempt that never
    -- committed. This is display metadata only: callers may project it into
    -- durable history, but must never append it to backend/model state.
    --
    -- Host tool execution happens after 'TurnFinished' and is deliberately
    -- excluded because its canonical calls/results are already retained in
    -- 'executionState' and 'executionPendingInputs'.
    , executionUncommittedDisplayEvents :: ![LoopEvent]
    -- | Rich provider metadata for every response committed during this loop.
    , executionProviderTelemetry :: ![TurnTelemetry]
    , executionResult :: !(Either LoopError LoopResult)
    } deriving (Eq, Show)

data LoopConfig = LoopConfig
    { loopBackend :: !Backend
    , loopBackendState :: !BackendStateStore
    , loopTools :: !ToolRegistry
    , loopDispatch :: !ToolDispatchConfig
    , loopMaxTurns :: !Int
    , loopOnEvent :: !(LoopEvent -> IO ())
    -- | Check or request permission before running a tool.
    , loopApprove :: !(ToolCall -> IO ToolApproval)
    -- | Read pending user guidance submitted while this loop is active.
    -- Guidance is acknowledged only after the model response commits, so a
    -- failed submission can be retried without losing it.
    , loopReadSteering :: !(IO [TurnInput])
    , loopCommitSteering :: !(Int -> IO ())
    -- | Ask the active provider to interrupt its turn in-band. The loop calls
    -- this before falling back to structured async teardown.
    , loopInterrupt :: !(IO ())
    -- | Soft-cancel latch. The caller owns resetting it before publishing
    -- the turn to input/interrupt handlers. When set, the loop stops after
    -- the current tool batch instead of asking the model for another step.
    , loopCancel :: !CancelFlag
    }

data LoopResult = LoopResult
    { finalResponseId :: !Text
    , finalText :: !(Maybe Text)
    , turnsUsed :: !Int
    , tokenUsage :: !TokenUsage
    } deriving (Eq, Show)

data LoopError
    = LoopTransport ApiError
    -- | The transport failed after text or reasoning was already exposed.
    -- Connection-recovery backends normally retry these with a visible stream
    -- boundary; this remains the terminal fallback for unwrapped backends.
    | LoopTransportAfterOutput ApiError
    | LoopMaxTurns TurnOutput
    | LoopIncomplete TurnOutput
    | LoopNoResponseId
    -- | An unexpected synchronous exception escaped a backend, approval
    -- callback, event sink, or other loop-owned IO action. Keeping it in-band
    -- lets interactive callers fail this turn without terminating the agent.
    | LoopUnexpected Text
    -- | Soft-cancel after tools ran. Carries the completed tool results for
    -- callers that retain the in-progress turn; callers may instead roll the
    -- whole turn back to its last committed response boundary.
    | LoopCancelled [ToolCallResult]
    deriving (Eq, Show)

hasVisibleAssistantText :: Maybe Text -> Bool
hasVisibleAssistantText =
    maybe False (not . Text.null . Text.strip)

emptyContinuationWarning :: Text
emptyContinuationWarning =
    "The model produced no assistant text or tool calls after reasoning; stopping."

-- | The loop's last owned checkpoint and the work following it. The state
-- store may publish a newer reset/compaction; recovery must still check ownership.
-- Normal execution and failure recovery read the same runtime-owned value.
data LoopState = LoopState
    { checkpoint :: !BackendSnapshot
    , progress :: !LoopProgress
    , pending :: !PendingInputs
    , previousResponseId :: !(Maybe Text)
    , turnsUsed :: !Int
    , lastOutput :: !(Maybe TurnOutput)
    , tokenUsage :: !TokenUsage
    , emptyContinuations :: !Int
    }

-- | A response can absorb its inputs before the caller's steering queue is
-- acknowledged. Clearing inputs must not erase that acknowledgement debt.
data PendingInputs = PendingInputs
    { inputs :: ![TurnInput]
    , steeringToAcknowledge :: !Int
    }

data CompletedTurnDecision
    = ContinueLoop { nextEmptyContinuations :: !Int }
    | FinishLoop !LoopResult
    | WarnAndFinishLoop !LoopResult

decideCompletedTurn
    :: LoopState
    -> TurnOutput
    -> [TurnInput]
    -> CompletedTurnDecision
decideCompletedTurn state turn continuation
    | not (null continuation) =
        ContinueLoop 0
    | hasVisibleAssistantText turn.assistantText =
        FinishLoop result
    | state.emptyContinuations >= maxEmptyContinuations =
        WarnAndFinishLoop result
    | otherwise =
        ContinueLoop (state.emptyContinuations + 1)
  where
    nextTurnsUsed = state.turnsUsed + 1
    usage = addTokenUsage state.tokenUsage turn.tokenUsage
    result = LoopResult
        { finalResponseId = turn.responseId
        , finalText = turn.assistantText
        , turnsUsed = nextTurnsUsed
        , tokenUsage = usage
        }

-- | Advance only continuation bookkeeping; the owned checkpoint is untouched.
advanceLoopState :: TurnOutput -> PendingInputs -> Int -> LoopState -> LoopState
advanceLoopState turn pending emptyContinuations state =
    state
        { previousResponseId = Just turn.responseId
        , turnsUsed = state.turnsUsed + 1
        , lastOutput = Just turn
        , tokenUsage = addTokenUsage state.tokenUsage turn.tokenUsage
        , pending
        , emptyContinuations
        }

runLoop
    :: LoopConfig
    -> Maybe Text
    -> Text
    -> IO (Either LoopError LoopResult)
runLoop config previousResponseId prompt =
    runLoopInputs config previousResponseId [UserMessage prompt]

-- | Same as 'runLoop', but the first turn may be multimodal.
runLoopInputs
    :: LoopConfig
    -> Maybe Text
    -> [TurnInput]
    -> IO (Either LoopError LoopResult)
runLoopInputs config previousResponseId firstInputs =
    (.executionResult)
        <$> runLoopInputsDetailed config previousResponseId firstInputs

-- | Run a loop while retaining the latest explicitly committed backend state.
runLoopInputsDetailed
    :: LoopConfig
    -> Maybe Text
    -> [TurnInput]
    -> IO LoopExecution
runLoopInputsDetailed config previousResponseId firstInputs = do
    initialState <- config.loopBackendState.readBackendState
    runLoopInputsUnsafe
        config initialState previousResponseId firstInputs

exceptionSummary :: SomeException -> Text
exceptionSummary =
    fst
        . Text.breakOn "\nHasCallStack backtrace:"
        . Text.pack
        . displayException

data LoopRuntime = LoopRuntime
    { loopRuntimeConfig :: LoopConfig
    , loopRuntimeEventPump :: LoopEventPump
    , loopRuntimeAsyncToolManager :: AsyncToolManager
    , loopRuntimeState :: IORef LoopState
    , loopRuntimeVisibleStateRef :: IORef VisibleLoopState
    , loopRuntimeProviderTelemetryRef :: IORef [TurnTelemetry]
    }

-- A returned response is not necessarily complete or a valid checkpoint.
data SubmissionOutcome
    = SubmissionReturned !BackendResult
    | SubmissionCancelled
    | SubmissionFailed !LoopError

-- Closing consumes the journal and rejects any callbacks retained by a backend.
data RecoveryJournal
    = RecoveryClosed
    | RecoveryOpen
        { recoverySummary :: !(Maybe Text)
        -- Newest first while the journal is open.
        , recoveryItems :: ![(ResponseItem, Maybe ToolCall)]
        }

data AttemptVisibility = AttemptVisibility
    { previousAttemptVisible :: !Bool
    , currentAttemptVisible :: !Bool
    }

runLoopInputsUnsafe
    :: LoopConfig
    -> BackendSnapshot
    -> Maybe Text
    -> [TurnInput]
    -> IO LoopExecution
runLoopInputsUnsafe config0 initialState previousResponseId firstInputs = do
    runtime <- initializeLoopRuntime config0 initialState previousResponseId firstInputs
    runLoopWithEventPump runtime

initializeLoopRuntime
    :: LoopConfig
    -> BackendSnapshot
    -> Maybe Text
    -> [TurnInput]
    -> IO LoopRuntime
initializeLoopRuntime config0 initialState previousResponseId firstInputs = do
    eventPump <- newEventPump config0.loopOnEvent
    -- Allocate only STM state here; the manager's workers remain scoped to
    -- runLoopWithEventPump.
    manager <- newAsyncToolManager
    eventAdmissionLock <- newMVar ()
    visibleStateRef <- newIORef emptyVisibleLoopState
    providerTelemetryRef <- newIORef []
    initialSteering <- config0.loopReadSteering
    stateRef <- newIORef LoopState
        { checkpoint = initialState
        , progress = NoResponseCommitted
        , pending = PendingInputs (firstInputs <> initialSteering) (length initialSteering)
        , previousResponseId
        , turnsUsed = 0
        , lastOutput = Nothing
        , tokenUsage = emptyTokenUsage
        , emptyContinuations = 0
        }
    let config = config0
            { loopOnEvent = \event ->
                -- Provider streaming and async host tools can publish at the
                -- same time. Keep journal mutation and event-pump admission
                -- in one total order. An atomic state update alone would not
                -- preserve admission order when the event pump backpressures.
                withMVar eventAdmissionLock \_ -> do
                    atomicModifyIORef' visibleStateRef \state ->
                        (recordVisibleLoopEvent event state, ())
                    emitLoopEvent eventPump event
            }
    pure LoopRuntime
        { loopRuntimeConfig = config
        , loopRuntimeEventPump = eventPump
        , loopRuntimeAsyncToolManager = manager
        , loopRuntimeState = stateRef
        , loopRuntimeVisibleStateRef = visibleStateRef
        , loopRuntimeProviderTelemetryRef = providerTelemetryRef
        }

-- Updates belong to the loop worker, or recovery after that worker is joined.
-- Never restore an entire pre-submission snapshot after running effects.
recordCheckpoint :: LoopRuntime -> BackendSnapshot -> IO ()
recordCheckpoint runtime checkpoint =
    modifyIORef' runtime.loopRuntimeState \state ->
        state { checkpoint, progress = ResponseCommitted }

setPendingInputs :: LoopRuntime -> [TurnInput] -> IO ()
setPendingInputs runtime inputs =
    modifyIORef' runtime.loopRuntimeState \state ->
        state { pending = state.pending { inputs } }

clearSteeringAcknowledgement :: LoopRuntime -> IO ()
clearSteeringAcknowledgement runtime =
    modifyIORef' runtime.loopRuntimeState \state ->
        state { pending = state.pending { steeringToAcknowledge = 0 } }

finishLoopExecution
    :: LoopRuntime
    -> Either LoopError LoopResult
    -> IO LoopExecution
finishLoopExecution runtime result = do
    state <- readIORef runtime.loopRuntimeState
    visibleState <- readIORef runtime.loopRuntimeVisibleStateRef
    providerTelemetry <-
        reverse <$> readIORef runtime.loopRuntimeProviderTelemetryRef
    pure LoopExecution
        { executionState = state.checkpoint.backendItems
        , executionPendingInputs = state.pending.inputs
        , executionProgress = state.progress
        , executionUncommittedAssistantText =
            visibleAssistantText visibleState
        , executionUncommittedDisplayEvents = visibleDisplayEvents visibleState
        , executionProviderTelemetry = providerTelemetry
        , executionResult = result
        }

unexpectedLoopExecution
    :: LoopRuntime
    -> SomeException
    -> IO LoopExecution
unexpectedLoopExecution runtime exception =
    finishLoopExecution runtime
        (Left (LoopUnexpected (exceptionSummary exception)))

protectLoop
    :: LoopRuntime
    -> IO LoopExecution
    -> IO LoopExecution
protectLoop runtime action =
    tryAny action >>= either (unexpectedLoopExecution runtime) pure

runLoopState :: LoopRuntime -> IO LoopExecution
runLoopState runtime = do
    let config = runtime.loopRuntimeConfig
    state <- readIORef runtime.loopRuntimeState
    if state.turnsUsed >= config.loopMaxTurns
        then finishLoopExecution runtime $ case state.lastOutput of
            Just turn -> Left (LoopMaxTurns turn)
            Nothing -> Left LoopNoResponseId
        else protectLoop runtime do
            cancelled <- isCancelled config.loopCancel
            if cancelled
                then finishLoopExecution runtime
                    (Left (LoopCancelled []))
                else do
                    config.loopOnEvent TurnStarted
                    submission <- submitLoopTurn runtime state
                    case submission of
                        SubmissionCancelled ->
                            finishLoopExecution runtime
                                (Left (LoopCancelled []))
                        SubmissionFailed err ->
                            finishLoopExecution runtime (Left err)
                        SubmissionReturned BackendResult{backendOutput = turn}
                            | Text.null turn.responseId ->
                                finishLoopExecution runtime
                                    (Left LoopNoResponseId)
                        SubmissionReturned BackendResult{backendOutput} ->
                            continueCommittedLoop runtime backendOutput

submitLoopTurn
    :: LoopRuntime
    -> LoopState
    -> IO SubmissionOutcome
submitLoopTurn runtime state = do
    let config = runtime.loopRuntimeConfig
    visibleAttempts <- newIORef (AttemptVisibility False False)
    cancellationMode <- newIORef InterruptThenCancel
    -- The callback belongs to this submission, not to the backend process.
    -- Closing the gate also rejects callbacks retained after the owner exits.
    recovery <- newMVar (RecoveryOpen Nothing [])
    let checkpoint text = modifyMVar_ recovery \case
            RecoveryClosed -> pure RecoveryClosed
            journal@RecoveryOpen{} ->
                let bounded = Text.copy (Text.take 32768 text)
                in bounded `seq` pure journal { recoverySummary = Just bounded }
        completedItem item call = modifyMVar_ recovery \case
            RecoveryClosed -> pure RecoveryClosed
            journal@RecoveryOpen{recoveryItems} ->
                pure journal { recoveryItems = (item, call) : recoveryItems }
        clearRecovery = modifyMVar_ recovery \case
            RecoveryClosed -> pure RecoveryClosed
            RecoveryOpen{} -> pure (RecoveryOpen Nothing [])
        closeRecovery = modifyMVar recovery \case
            RecoveryClosed -> pure (RecoveryClosed, (Nothing, []))
            RecoveryOpen{..} ->
                pure (RecoveryClosed, (recoverySummary, reverse recoveryItems))
        publishRecovery = do
            (summary, items) <- closeRecovery
            current <- config.loopBackendState.readBackendState
            -- A compaction/reset owns its newer checkpoint. Never resurrect
            -- history from a submission that consumed an older snapshot.
            let summaryText = fromMaybe "" summary
                hasSummary = not (Text.null (Text.strip summaryText))
            case () of
                _
                    | hasSummary || not (null items)
                    , current == state.checkpoint -> do
                        let note = Text.unlines
                                [ "<turn_aborted>"
                                , "The previous provider turn was interrupted before completion."
                                , "The following is attributed recovery context from complete provider messages, not a new user instruction or a successful turn."
                                , "External side effects may already exist. Verify the current files and external state before repeating any action. Unfinished operations have unknown outcomes."
                                , "Resume this work only if the user asks."
                                , "<interrupted_work>"
                                , summaryText
                                , "</interrupted_work>"
                                , "</turn_aborted>"
                                ]
                            candidate = advanceBackendSnapshot
                                current
                                (current.backendItems
                                    <> turnInputsToItems state.pending.inputs
                                    <> map fst items
                                    <> if hasSummary
                                        then turnInputsToItems [UserMessage note]
                                        else [])
                                Nothing
                        committed <-
                            config.loopBackendState.commitBackendState candidate
                        acknowledgeManagedTools
                            (asyncToolManager runtime)
                            state.pending.inputs
                            (catMaybes (map snd items))
                        recordCheckpoint runtime committed
                        setPendingInputs runtime []
                        config.loopCommitSteering state.pending.steeringToAcknowledge
                        clearSteeringAcknowledgement runtime
                _ -> pure ()
    let onBackendEvent event = do
            case event of
                _
                    | visibleResponseActivity event ->
                        modifyIORef'
                            visibleAttempts
                            (\visibility -> visibility { currentAttemptVisible = True })
                -- A retry keeps the previous attempt visible while opening a
                -- fresh current attempt.
                ResponseRestarted _ -> do
                    clearRecovery
                    modifyIORef'
                        visibleAttempts
                        (\visibility -> AttemptVisibility
                            { previousAttemptVisible =
                                visibility.previousAttemptVisible
                                    || visibility.currentAttemptVisible
                            , currentAttemptVisible = False
                            })
                -- The backend rolled that attempt back, but earlier restarted
                -- attempts remain visible.
                ResponseAttemptDiscarded -> do
                    clearRecovery
                    modifyIORef'
                        visibleAttempts
                        (\visibility -> visibility { currentAttemptVisible = False })
                _ -> pure ()
            config.loopOnEvent event
    -- Race the model call against cancel so Ctrl-C / Esc can stop reasoning
    -- mid-stream, not only between tools.
    raced <- mask \restore -> do
        normalized <- (withAsync
            (restore $
                config.loopBackend.submitTurnWithCallbacks
                    (normalizeBackendSnapshotImages state.checkpoint)
                    state.previousResponseId
                    (normalizeTurnInputs state.pending.inputs)
                    BackendCallbacks
                        { onLoopEvent = onBackendEvent
                        , onAsyncToolCall = \call ->
                            withMVar recovery \case
                                RecoveryClosed -> pure ()
                                RecoveryOpen{} ->
                                    admitAsyncToolCall
                                        (asyncToolManager runtime)
                                        call
                        , onRecoveryCheckpoint = checkpoint
                        , onCompletedResponseItem = completedItem
                        , onCancellationMode = \mode ->
                            withMVar recovery \case
                                RecoveryClosed -> pure ()
                                RecoveryOpen{} -> writeIORef cancellationMode mode
                        })
            \submission -> do
                result <- restore $ race
                    (waitCancel config.loopCancel)
                    (waitCatch submission)
                normalized <- case result of
                    Left () -> do
                        -- Give structured providers a chance to preserve their
                        -- subprocess/session invariants before withAsync
                        -- force-cancels an unresponsive submission.
                        mode <- readIORef cancellationMode
                        when (mode == InterruptThenCancel) do
                            _ <- restore $
                                timeout 2000000 (tryAny config.loopInterrupt)
                            _ <- restore $
                                timeout 2000000 (waitCatch submission)
                            pure ()
                        pure SubmissionCancelled
                    Right (Left exception) ->
                        -- Preserve the provider thread's asynchronous-exception
                        -- identity. Safe.throwIO would turn ThreadKilled into
                        -- LoopUnexpected.
                        Exception.throwIO exception
                    Right (Right completed) ->
                        pure (either (SubmissionFailed . LoopTransport) SubmissionReturned completed)
                case normalized of
                    SubmissionReturned backendResult@BackendResult{..}
                        | not (Text.null backendOutput.responseId) -> do
                            committed <-
                                config.loopBackendState.commitBackendState
                                    backendState
                            acknowledgeManagedTools
                                (asyncToolManager runtime)
                                state.pending.inputs
                                backendOutput.toolCalls
                            recordCheckpoint runtime committed
                            pure
                                (SubmissionReturned backendResult
                                    { backendState = committed
                                    })
                    _ -> pure normalized)
            `onException` publishRecovery
        case normalized of
            SubmissionReturned BackendResult{backendOutput}
                | not (Text.null backendOutput.responseId) -> do
                    _ <- closeRecovery
                    pure normalized
            _ -> publishRecovery >> pure normalized
    case raced of
        SubmissionCancelled -> pure SubmissionCancelled
        SubmissionFailed (LoopTransport err) -> do
            visibility <- readIORef visibleAttempts
            let emitted =
                    visibility.previousAttemptVisible || visibility.currentAttemptVisible
            when emitted $
                config.loopOnEvent ResponseAttemptFailed
            pure $ SubmissionFailed $
                if emitted
                    then LoopTransportAfterOutput err
                    else LoopTransport err
        outcome -> pure outcome

continueCommittedLoop
    :: LoopRuntime
    -> TurnOutput
    -> IO LoopExecution
continueCommittedLoop runtime turn = do
    let config = runtime.loopRuntimeConfig
    -- The committed response absorbed every input submitted with it, and its
    -- assistant text now lives in the committed state.
    setPendingInputs runtime []
    -- Async tool publishers may still be active. Reset the whole snapshot
    -- atomically without introducing a blocking checkpoint during commit.
    atomicWriteIORef runtime.loopRuntimeVisibleStateRef emptyVisibleLoopState
    -- Result metadata belongs to the response commit even when a cancellation
    -- lands before the completion event is painted.
    case turn.providerTelemetry of
        Nothing -> pure ()
        Just telemetry ->
            modifyIORef'
                runtime.loopRuntimeProviderTelemetryRef
                (telemetry :)
    protectLoop runtime do
        -- A cancel that landed during submitTurn after the race chose Right
        -- still counts, but its returned state is committed.
        cancelledMid <- isCancelled config.loopCancel
        if cancelledMid
            then finishLoopExecution runtime
                (Left (LoopCancelled []))
            else do
                config.loopOnEvent (TurnFinished turn)
                case turn.completion of
                    TurnIncomplete{} ->
                        -- An incomplete provider response is terminal rather
                        -- than an assistant completion. Leaving the enclosing
                        -- loop scope cancels and joins any async calls that
                        -- were announced before the incomplete response.
                        finishLoopExecution runtime
                            (Left (LoopIncomplete turn))
                    TurnCompleted -> do
                        completeLoopTurn runtime turn

-- | The response is committed before acknowledging its inputs or waiting for
-- tools. Keep these boundaries explicit: recovery observes each completed effect.
completeLoopTurn :: LoopRuntime -> TurnOutput -> IO LoopExecution
completeLoopTurn runtime turn = do
    let config = runtime.loopRuntimeConfig
    state <- readIORef runtime.loopRuntimeState
    config.loopCommitSteering state.pending.steeringToAcknowledge
    clearSteeringAcknowledgement runtime
    race
        (waitCancel config.loopCancel)
        (runManagedToolCalls (asyncToolManager runtime) turn.toolCalls)
        >>= \case
            Left () ->
                -- Leaving the enclosing manager scope cancels and joins
                -- unfinished handlers and approval callbacks.
                finishLoopExecution runtime (Left (LoopCancelled []))
            Right results -> do
                -- Retain completed results even if cancellation or reading
                -- steering interrupts the transition to the next turn.
                setPendingInputs runtime (map CompletedTool results)
                cancelled <- isCancelled config.loopCancel
                if cancelled
                    then finishLoopExecution runtime (Left (LoopCancelled results))
                    else advanceCompletedTurn runtime turn results

advanceCompletedTurn
    :: LoopRuntime
    -> TurnOutput
    -> [ToolCallResult]
    -> IO LoopExecution
advanceCompletedTurn runtime turn results = do
    let config = runtime.loopRuntimeConfig
    steering <- config.loopReadSteering
    let continuation = map CompletedTool results <> steering
    state <- readIORef runtime.loopRuntimeState
    case decideCompletedTurn state turn continuation of
        ContinueLoop{nextEmptyContinuations} -> do
            modifyIORef' runtime.loopRuntimeState $
                advanceLoopState turn
                    (PendingInputs continuation (length steering))
                    nextEmptyContinuations
            runLoopState runtime
        FinishLoop result ->
            finishLoopExecution runtime (Right result)
        WarnAndFinishLoop result -> do
            config.loopOnEvent (WarningRaised emptyContinuationWarning)
            finishLoopExecution runtime (Right result)

runLoopWithEventPump
    :: LoopRuntime
    -> IO LoopExecution
runLoopWithEventPump runtime =
    withAsync (runEventPump runtime.loopRuntimeEventPump) \eventWorker -> do
        let manager = asyncToolManager runtime
            handleManagerFailure exception
                | isAsyncException exception =
                    Exception.throwIO exception
                | otherwise =
                    unexpectedLoopExecution runtime exception
        execution <-
            withAsync
                (runAsyncToolManager
                    runtime.loopRuntimeConfig
                    manager)
                \managerWorker -> do
                    raced <-
                        race
                            (race
                                (waitEventPumpFailure
                                    eventWorker
                                    runtime.loopRuntimeEventPump)
                                (waitAsyncToolManagerFailure
                                    managerWorker
                                    manager))
                            (runLoopState runtime)
                    case raced of
                        Left (Left failure) ->
                            handleLoopEventFailure
                                (unexpectedLoopExecution runtime)
                                failure
                        Left (Right exception) ->
                            handleManagerFailure exception
                        Right completed ->
                            atomically
                                (tryReadTMVar manager.asyncToolFailure)
                                >>= maybe
                                    (pure completed)
                                    handleManagerFailure
        -- Only inspect outcomes once the manager scope has cancelled and
        -- joined every worker. A result waiter can lose its race while some
        -- of its sibling tools have already finished successfully.
        recovered <- tryAny (recoverManagedTools runtime manager execution) >>= \case
            Right recovered -> pure recovered
            Left exception ->
                unexpectedLoopExecution runtime exception
        flushEventPump runtime.loopRuntimeEventPump >>= \case
            Left failure ->
                handleLoopEventFailure
                    (unexpectedLoopExecution runtime)
                    failure
            Right () -> pure recovered

-- | A tool result is acknowledged only when the response consuming it commits,
-- not when a waiter drains it. Keep that fact across history compaction.
acknowledgeManagedTools :: AsyncToolManager -> [TurnInput] -> [ToolCall] -> IO ()
acknowledgeManagedTools manager inputs calls = atomically do
    modifyTVar' manager.asyncToolAcknowledged $
        Set.union (Set.fromList [result.callId | CompletedTool result <- inputs])
    modifyTVar' manager.asyncToolCommittedCalls $
        Map.union (Map.fromList [(call.callId, call) | call <- calls])

recoverManagedTools
    :: LoopRuntime
    -> AsyncToolManager
    -> LoopExecution
    -> IO LoopExecution
recoverManagedTools runtime manager execution = do
    (records, acknowledged, committedCalls) <- atomically do
        calls <- readTVar manager.asyncToolCalls
        records <- traverse
            (\record -> do
                result <- readTVar record.managedTrustedResult
                pure (record, result))
            (sortOn (.managedAdmissionSequence) (Map.elems calls))
        acknowledged <- readTVar manager.asyncToolAcknowledged
        committedCalls <- readTVar manager.asyncToolCommittedCalls
        pure (records, acknowledged, committedCalls)
    let unacknowledged =
            [ (record.managedCall, result)
            | (record, result) <- records
            , Set.notMember record.managedCall.callId acknowledged
            ]
        retainedCallIds = Set.fromList (concatMap retainedCallId execution.executionState)
        canonical call =
            Map.lookup call.callId committedCalls == Just call
                && Set.member call.callId retainedCallIds
        orphanIds = Set.fromList
            [call.callId | (call, _) <- unacknowledged, not (canonical call)]
        safePending = filter
            (\case
                CompletedTool completed -> Set.notMember completed.callId orphanIds
                _ -> True)
            execution.executionPendingInputs
        pendingIds = Set.fromList
            [ result.callId
            | CompletedTool result <- safePending
            ]
        salvaged =
            [ result
            | (call, Just result) <- unacknowledged
            , canonical call
            , Set.notMember result.callId pendingIds
            ]
        pending = safePending <> map CompletedTool salvaged
        orphans = [(call, result) | (call, result) <- unacknowledged, not (canonical call)]
        result = case execution.executionResult of
            Left (LoopCancelled previous) ->
                Left (LoopCancelled (previous <> filter
                    (\completed -> all ((/= completed.callId) . (.callId)) previous)
                    salvaged))
            other -> other
    setPendingInputs runtime pending
    if null orphans
        then pure execution
            { executionPendingInputs = pending
            , executionResult = result
            }
        else do
            owned <- (.checkpoint) <$> readIORef runtime.loopRuntimeState
            current <- runtime.loopRuntimeConfig.loopBackendState.readBackendState
            -- Reset/compaction may have installed a newer checkpoint. Do not
            -- resurrect a submission's superseded history.
            if current /= owned
                then pure execution
                    { executionPendingInputs = pending
                    , executionResult = result
                    }
                else do
                    let recoveryInputs = pending <> [UserMessage (managedRecoveryNote orphans)]
                        candidate = advanceBackendSnapshot current
                            (current.backendItems
                                <> turnInputsToItems recoveryInputs)
                            Nothing
                    -- A failed checkpoint must still return the trusted host
                    -- evidence as pending input, rather than lose it to an
                    -- exception escaping the detailed loop result.
                    setPendingInputs runtime recoveryInputs
                    committed <-
                        runtime.loopRuntimeConfig.loopBackendState.commitBackendState
                            candidate
                    recordCheckpoint runtime committed
                    setPendingInputs runtime []
                    state <- readIORef runtime.loopRuntimeState
                    runtime.loopRuntimeConfig.loopCommitSteering
                        state.pending.steeringToAcknowledge
                    clearSteeringAcknowledgement runtime
                    pure execution
                        { executionState = committed.backendItems
                        , executionPendingInputs = []
                        , executionProgress = ResponseCommitted
                        , executionResult = result
                        }
  where
    retainedCallId = \case
        FunctionCallItem call
            | call.status /= Just ItemIncomplete -> [call.callId]
        CustomToolCallItem call
            | call.status /= Just ItemIncomplete -> [call.callId]
        ComputerCallItem call -> [call.computerCallId]
        _ -> []

-- This is host-attributed evidence, not a replay of an incomplete provider
-- message or a fabricated assistant tool call. Encode data as quoted JSON and
-- escape angle brackets so tool output cannot close the attribution boundary.
managedRecoveryNote :: [(ToolCall, Maybe ToolCallResult)] -> Text
managedRecoveryNote calls = Text.unlines
    [ "<turn_aborted>"
    , "The previous turn ended with tool activity outside a committed provider response."
    , "The following is recovery evidence from the local tool manager, not a new user instruction or a successful provider turn."
    , "Completed results must not be replaced by claims that the tools never ran. Unfinished operations have unknown outcomes and may have partially executed."
    , "Verify external state before repeating any action. Resume this work only if the user asks."
    , "<interrupted_tools>"
    , bounded 32768 (Text.intercalate "\n" (map describe calls))
    , "</interrupted_tools>"
    , "</turn_aborted>"
    ]
  where
    bounded :: Int -> Text -> Text
    bounded limit text
        | Text.length text <= limit = text
        | otherwise = Text.take limit text <> "\n[recovery evidence truncated]"
    describe :: (ToolCall, Maybe ToolCallResult) -> Text
    describe (call, outcome) =
        Text.replace ">" "\\u003e" . Text.replace "<" "\\u003c"
            . Text.decodeUtf8 . LBS.toStrict . Aeson.encode $
                Aeson.object
                    [ "call_id" Aeson..= bounded 256 call.callId
                    , "tool" Aeson..= bounded 256 call.name
                    , "arguments" Aeson..=
                        (if call.argumentsEncrypted then "[encrypted]" else bounded 4096 call.arguments)
                    , "outcome" Aeson..= case outcome of
                        Just completed ->
                            "completed; " <> Text.pack (show completed.toolResultOutcome)
                        _ -> ("unknown; interrupted before a completion was recorded" :: Text)
                    , "output" Aeson..= case outcome of
                        Just completed -> bounded 16384 completed.output
                        _ -> ""
                    ]

handleLoopEventFailure
    :: (SomeException -> IO LoopExecution)
    -> EventPumpFailure
    -> IO LoopExecution
handleLoopEventFailure unexpected = \case
    EventPumpSyncFailure exception ->
        unexpected exception
    EventPumpAsyncFailure exception ->
        Exception.throwIO exception

data AsyncToolManager = AsyncToolManager
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

newAsyncToolManager :: IO AsyncToolManager
newAsyncToolManager =
    AsyncToolManager
        <$> newTQueueIO
        <*> newTVarIO Map.empty
        <*> newTVarIO IntMap.empty
        <*> newTVarIO 0
        <*> newTQueueIO
        <*> newEmptyTMVarIO
        <*> newTVarIO Set.empty
        <*> newTVarIO Map.empty

asyncToolManager :: LoopRuntime -> AsyncToolManager
asyncToolManager runtime = runtime.loopRuntimeAsyncToolManager

admitAsyncToolCall :: AsyncToolManager -> ToolCall -> IO ()
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
    :: AsyncToolManager
    -> ToolCall
    -> IO (TMVar (Maybe ToolCallResult))
admitBlockingToolCall manager call =
    atomically (admitManagedToolCall manager call)

admitManagedToolCall
    :: AsyncToolManager
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

runManagedToolCalls
    :: AsyncToolManager
    -> [ToolCall]
    -> IO [ToolCallResult]
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

takeAsyncToolCompletions
    :: AsyncToolManager
    -> STM [ToolCallResult]
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

runAsyncToolManager :: LoopConfig -> AsyncToolManager -> IO ()
runAsyncToolManager config manager = do
    request <- atomically (readTQueue manager.asyncToolRequests)
    cancelledBefore <- isCancelled config.loopCancel
    if cancelledBefore
        then completeCancelledRequest request
        else
            race
                (waitCancel config.loopCancel)
                (do
                    prepared <-
                        prepareManagedToolCall
                            config
                            request.managedCall
                    plan <- schedulingPlanForPrepared config prepared
                    pure (prepared, plan))
                >>= \case
                    Left () ->
                        completeCancelledRequest request
                    Right (prepared, plan) -> do
                        cancelledAfter <- isCancelled config.loopCancel
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
                                        config
                                        manager
                                        request
                                        prepared
                                        plan)
                                    \worker ->
                                        withAsync
                                            (waitCatch worker
                                                >>= completeManagedToolRequest
                                                    manager
                                                    request)
                                            \_monitor ->
                                                runAsyncToolManager
                                                    config
                                                    manager
  where
    -- Approval and scheduling are allowed to perform IO. Complete cancelled
    -- requests so blocking result waiters cannot be stranded.
    completeCancelledRequest request = do
        completeManagedToolRequest manager request (Right Nothing)
        runAsyncToolManager config manager

prepareManagedToolCall :: LoopConfig -> ToolCall -> IO PreparedToolCall
prepareManagedToolCall config call
    | toolCallMode call == AsyncToolCall
        && not (toolSupportsAsync config.loopTools call) =
            pure $
                PreparedToolCall call $
                    ToolApprovalDenied
                        ("Tool " <> call.name
                            <> " does not support asynchronous execution.")
    | otherwise =
        prepareToolCall config call

runManagedToolWorker
    :: LoopConfig
    -> AsyncToolManager
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
        (waitCancel config.loopCancel)
        (runPreparedToolCallWithCompletion
            (atomically . writeTVar request.managedTrustedResult . Just)
            config
            prepared)
        >>= \case
            Left () -> pure Nothing
            Right result -> pure result

completeManagedToolRequest
    :: AsyncToolManager
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
                            writeTQueue
                                manager.asyncToolCompleted
                                completed

waitAsyncToolManagerFailure
    :: Async ()
    -> AsyncToolManager
    -> IO SomeException
waitAsyncToolManagerFailure managerWorker manager =
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

data PreparedToolCall =
    PreparedToolCall !ToolCall !ToolApproval

schedulingPlanForPrepared
    :: LoopConfig
    -> PreparedToolCall
    -> IO ToolSchedulingPlan
schedulingPlanForPrepared config (PreparedToolCall call approval) =
    case approval of
        ToolApprovalGranted ->
            toolSchedulingPlanFor config.loopTools call
        ToolApprovalDenied{} ->
            pure ToolUnconstrained
        ToolApprovalRejected ->
            pure ToolUnconstrained

-- | Approval may touch interactive or otherwise order-sensitive state, so it
-- is prepared serially even when the resulting handlers may run concurrently.
prepareToolCall :: LoopConfig -> ToolCall -> IO PreparedToolCall
prepareToolCall config call = do
    approval <- tryAny (config.loopApprove call)
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
    -> LoopConfig
    -> PreparedToolCall
    -> IO (Maybe ToolCallResult)
runPreparedToolCallWithCompletion completed config (PreparedToolCall call approval) = do
    cancelled <- isCancelled config.loopCancel
    if cancelled
        then pure Nothing
        else do
            config.loopOnEvent (ToolStarted call)
            result <- case approval of
                ToolApprovalDenied denial ->
                    pure (deniedResult denial)
                ToolApprovalRejected ->
                    pure (deniedResult "Tool call rejected by user.")
                ToolApprovalGranted ->
                    dispatchApprovedRegisteredToolCall
                        config.loopDispatch
                            { toolDispatchOnOutput = \progressCall output ->
                                config.loopDispatch.toolDispatchOnOutput progressCall output
                                    >> config.loopOnEvent
                                        (ToolOutputUpdated progressCall.callId output)
                            }
                        config.loopTools
                        call
            -- The trusted result must survive cancellation or a failing event
            -- consumer after the tool has returned. The manager's monitor is
            -- not authoritative: its own scope can be cancelled first.
            completed result
            config.loopOnEvent (ToolFinished result)
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
