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
import Agent.Loop.Output
import Agent.Loop.TokenUsage
import Agent.Responses.Types (ResponseItem)
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
    , dispatchRegisteredToolCall
    , toolSupportsAsync
    , toolSchedulingPlanFor
    )
import Control.Concurrent.Async
    ( Async
    , mapConcurrently
    , race
    , waitCatch
    , withAsync
    )
import Control.Concurrent.MVar (newMVar, withMVar)
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
    , newTVarIO
    , putTMVar
    , readTMVar
    , readTQueue
    , readTVar
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
    , tryAny
    )
import Control.Monad (when)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
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
    -- | 'Left' denies with that tool-output message; 'Right True' runs the
    -- tool; 'Right False' uses the usual user-rejection string.
    , loopApprove :: !(ToolCall -> IO (Either Text Bool))
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

data LoopCursor = LoopCursor
    { cursorState :: !BackendSnapshot
    , cursorProgress :: !LoopProgress
    , cursorPreviousResponseId :: !(Maybe Text)
    , cursorTurnsUsed :: !Int
    , cursorInputs :: ![TurnInput]
    , cursorSteeringCount :: !Int
    , cursorLastOutput :: !(Maybe TurnOutput)
    , cursorTokenUsage :: !TokenUsage
    , cursorEmptyContinuations :: !Int
    }

data CompletedTurnDecision
    = ContinueLoop !LoopCursor
    | FinishLoop !LoopResult
    | WarnAndFinishLoop !LoopResult

decideCompletedTurn
    :: LoopCursor
    -> TurnOutput
    -> [TurnInput]
    -> Int
    -> CompletedTurnDecision
decideCompletedTurn cursor turn continuation steeringCount
    | not (null continuation) =
        ContinueLoop cursor
            { cursorPreviousResponseId = Just turn.responseId
            , cursorTurnsUsed = nextTurnsUsed
            , cursorInputs = continuation
            , cursorSteeringCount = steeringCount
            , cursorLastOutput = Just turn
            , cursorTokenUsage = usage
            , cursorEmptyContinuations = 0
            }
    | hasVisibleAssistantText turn.assistantText =
        FinishLoop result
    | cursor.cursorEmptyContinuations >= maxEmptyContinuations =
        WarnAndFinishLoop result
    | otherwise =
        ContinueLoop cursor
            { cursorPreviousResponseId = Just turn.responseId
            , cursorTurnsUsed = nextTurnsUsed
            , cursorInputs = []
            , cursorSteeringCount = 0
            , cursorLastOutput = Just turn
            , cursorTokenUsage = usage
            , cursorEmptyContinuations =
                cursor.cursorEmptyContinuations + 1
            }
  where
    nextTurnsUsed = cursor.cursorTurnsUsed + 1
    usage = addTokenUsage cursor.cursorTokenUsage turn.tokenUsage
    result = LoopResult
        { finalResponseId = turn.responseId
        , finalText = turn.assistantText
        , turnsUsed = nextTurnsUsed
        , tokenUsage = usage
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
    , loopRuntimeAsyncToolManager :: Maybe AsyncToolManager
    , loopRuntimeProgressRef :: IORef (BackendSnapshot, LoopProgress)
    , loopRuntimePendingRef :: IORef [TurnInput]
    , loopRuntimeUncommittedTextRef :: IORef ([[Text]], [Text])
    , loopRuntimeUncommittedDisplayEventsRef
        :: IORef [DisplayJournalEntry]
    , loopRuntimeProviderAttemptActiveRef :: IORef Bool
    , loopRuntimeProviderTelemetryRef :: IORef [TurnTelemetry]
    , loopRuntimeInitialSteering :: [TurnInput]
    }

type LoopSubmissionResult =
    Either () (Either LoopError BackendResult)

runLoopInputsUnsafe
    :: LoopConfig
    -> BackendSnapshot
    -> Maybe Text
    -> [TurnInput]
    -> IO LoopExecution
runLoopInputsUnsafe config0 initialState previousResponseId firstInputs = do
    runtime <- initializeLoopRuntime config0 initialState firstInputs
    runLoopWithEventPump runtime initialState previousResponseId firstInputs

initializeLoopRuntime
    :: LoopConfig
    -> BackendSnapshot
    -> [TurnInput]
    -> IO LoopRuntime
initializeLoopRuntime config0 initialState firstInputs = do
    eventPump <- newEventPump config0.loopOnEvent
    eventAdmissionLock <- newMVar ()
    progressRef <- newIORef (initialState, NoResponseCommitted)
    uncommittedTextRef <- newIORef ([], [])
    uncommittedDisplayEventsRef <- newIORef []
    providerAttemptActiveRef <- newIORef False
    providerTelemetryRef <- newIORef []
    initialSteering <- config0.loopReadSteering
    pendingRef <- newIORef (firstInputs <> initialSteering)
    let config = config0
            { loopOnEvent = \event ->
                -- Provider streaming and async host tools can publish at the
                -- same time. Keep journal mutation and event-pump admission
                -- in one total order instead of racing the IORefs below.
                withMVar eventAdmissionLock \_ -> do
                    recordVisibleLoopEvent
                        uncommittedTextRef
                        uncommittedDisplayEventsRef
                        providerAttemptActiveRef
                        event
                    emitLoopEvent eventPump event
            }
    pure LoopRuntime
        { loopRuntimeConfig = config
        , loopRuntimeEventPump = eventPump
        , loopRuntimeAsyncToolManager = Nothing
        , loopRuntimeProgressRef = progressRef
        , loopRuntimePendingRef = pendingRef
        , loopRuntimeUncommittedTextRef = uncommittedTextRef
        , loopRuntimeUncommittedDisplayEventsRef =
            uncommittedDisplayEventsRef
        , loopRuntimeProviderAttemptActiveRef = providerAttemptActiveRef
        , loopRuntimeProviderTelemetryRef = providerTelemetryRef
        , loopRuntimeInitialSteering = initialSteering
        }

recordVisibleLoopEvent
    :: IORef ([[Text]], [Text])
    -> IORef [DisplayJournalEntry]
    -> IORef Bool
    -> LoopEvent
    -> IO ()
recordVisibleLoopEvent
    uncommittedTextRef
    uncommittedDisplayEventsRef
    providerAttemptActiveRef
    event = do
        modifyIORef' uncommittedTextRef \(finished, current) ->
            case event of
                TextDelta delta -> (finished, delta : current)
                -- A restarted attempt stays visible, marked failed,
                -- until a later response commits or the turn ends.
                ResponseRestarted _ ->
                    finishCurrentTextAttempt finished current
                ResponseAttemptDiscarded -> (finished, [])
                _ -> (finished, current)
        case event of
            TurnStarted -> do
                writeIORef providerAttemptActiveRef True
                writeIORef uncommittedDisplayEventsRef []
            TurnFinished _ -> do
                writeIORef providerAttemptActiveRef False
                writeIORef uncommittedDisplayEventsRef []
            ResponseRestarted _ ->
                modifyIORef'
                    uncommittedDisplayEventsRef
                    (recordDisplayEvent event)
            ResponseAttemptDiscarded ->
                modifyIORef'
                    uncommittedDisplayEventsRef
                    discardCurrentDisplayAttempt
            _
                | replayableDisplayEvent event -> do
                    active <- readIORef providerAttemptActiveRef
                    when active $
                        modifyIORef'
                            uncommittedDisplayEventsRef
                            (recordDisplayEvent event)
                | otherwise -> pure ()

finishCurrentTextAttempt
    :: [[Text]]
    -> [Text]
    -> ([[Text]], [Text])
finishCurrentTextAttempt finished current
    | null current = (finished, [])
    | otherwise = (current : finished, [])

finishLoopExecution
    :: LoopRuntime
    -> BackendSnapshot
    -> LoopProgress
    -> Either LoopError LoopResult
    -> IO LoopExecution
finishLoopExecution runtime state progress result = do
    writeIORef runtime.loopRuntimeProgressRef (state, progress)
    pending <- readIORef runtime.loopRuntimePendingRef
    (finishedChunks, currentChunks) <-
        readIORef runtime.loopRuntimeUncommittedTextRef
    displayEvents <-
        displayEventsFromJournal
            <$> readIORef runtime.loopRuntimeUncommittedDisplayEventsRef
    providerTelemetry <-
        reverse <$> readIORef runtime.loopRuntimeProviderTelemetryRef
    let uncommittedText = Text.intercalate "\n\n" $
            filter (not . Text.null) $
                map (Text.concat . reverse)
                    (reverse finishedChunks <> [currentChunks])
    pure LoopExecution
        { executionState = state.backendItems
        , executionPendingInputs = pending
        , executionProgress = progress
        , executionUncommittedAssistantText =
            if Text.null uncommittedText
                then Nothing
                else Just uncommittedText
        , executionUncommittedDisplayEvents = displayEvents
        , executionProviderTelemetry = providerTelemetry
        , executionResult = result
        }

finishLoopCursor
    :: LoopRuntime
    -> LoopCursor
    -> Either LoopError LoopResult
    -> IO LoopExecution
finishLoopCursor runtime cursor =
    finishLoopExecution
        runtime
        cursor.cursorState
        cursor.cursorProgress

unexpectedLoopExecution
    :: LoopRuntime
    -> BackendSnapshot
    -> LoopProgress
    -> SomeException
    -> IO LoopExecution
unexpectedLoopExecution runtime state progress exception =
    finishLoopExecution runtime state progress
        (Left (LoopUnexpected (exceptionSummary exception)))

unexpectedLoopCursor
    :: LoopRuntime
    -> LoopCursor
    -> SomeException
    -> IO LoopExecution
unexpectedLoopCursor runtime cursor =
    unexpectedLoopExecution
        runtime
        cursor.cursorState
        cursor.cursorProgress

protectLoopCursor
    :: LoopRuntime
    -> LoopCursor
    -> IO LoopExecution
    -> IO LoopExecution
protectLoopCursor runtime cursor action =
    tryAny action >>= either (unexpectedLoopCursor runtime cursor) pure

runLoopCursor :: LoopRuntime -> LoopCursor -> IO LoopExecution
runLoopCursor runtime cursor = do
    let config = runtime.loopRuntimeConfig
    writeIORef runtime.loopRuntimeProgressRef
        (cursor.cursorState, cursor.cursorProgress)
    writeIORef runtime.loopRuntimePendingRef cursor.cursorInputs
    if cursor.cursorTurnsUsed >= config.loopMaxTurns
        then finishLoopCursor runtime cursor $ case cursor.cursorLastOutput of
            Just turn -> Left (LoopMaxTurns turn)
            Nothing -> Left LoopNoResponseId
        else protectLoopCursor runtime cursor do
            cancelled <- isCancelled config.loopCancel
            if cancelled
                then finishLoopCursor runtime cursor
                    (Left (LoopCancelled []))
                else do
                    config.loopOnEvent TurnStarted
                    submission <- submitLoopTurn runtime cursor
                    case submission of
                        Left () ->
                            finishLoopCursor runtime cursor
                                (Left (LoopCancelled []))
                        Right (Left err) ->
                            finishLoopCursor runtime cursor (Left err)
                        Right
                            (Right BackendResult{backendOutput = turn})
                            | Text.null turn.responseId ->
                                finishLoopCursor runtime cursor
                                    (Left LoopNoResponseId)
                        Right (Right BackendResult{..}) ->
                            continueCommittedLoop
                                runtime
                                cursor
                                    { cursorState = backendState
                                    , cursorProgress = ResponseCommitted
                                    }
                                backendOutput

submitLoopTurn
    :: LoopRuntime
    -> LoopCursor
    -> IO LoopSubmissionResult
submitLoopTurn runtime cursor = do
    let config = runtime.loopRuntimeConfig
    visibleAttempts <- newIORef (False, False)
    let onBackendEvent event = do
            case event of
                _
                    | visibleResponseActivity event ->
                        modifyIORef'
                            visibleAttempts
                            (\(prior, _) -> (prior, True))
                -- A retry keeps the previous attempt visible while opening a
                -- fresh current attempt.
                ResponseRestarted _ ->
                    modifyIORef'
                        visibleAttempts
                        (\(prior, current) -> (prior || current, False))
                -- The backend rolled that attempt back, but earlier restarted
                -- attempts remain visible.
                ResponseAttemptDiscarded ->
                    modifyIORef'
                        visibleAttempts
                        (\(prior, _) -> (prior, False))
                _ -> pure ()
            config.loopOnEvent event
    -- Race the model call against cancel so Ctrl-C / Esc can stop reasoning
    -- mid-stream, not only between tools.
    raced <- mask \restore ->
        withAsync
            (restore $
                config.loopBackend.submitTurnWithCallbacks
                    (normalizeBackendSnapshotImages cursor.cursorState)
                    cursor.cursorPreviousResponseId
                    (normalizeTurnInputs cursor.cursorInputs)
                    BackendCallbacks
                        { onLoopEvent = onBackendEvent
                        , onAsyncToolCall =
                            admitAsyncToolCall
                                (asyncToolManager runtime)
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
                        _ <- restore $
                            timeout 2000000 (tryAny config.loopInterrupt)
                        _ <- restore $
                            timeout 2000000 (waitCatch submission)
                        pure (Left ())
                    Right (Left exception) ->
                        -- Preserve the provider thread's asynchronous-exception
                        -- identity. Safe.throwIO would turn ThreadKilled into
                        -- LoopUnexpected.
                        Exception.throwIO exception
                    Right (Right completed) ->
                        pure (Right completed)
                case normalized of
                    Right (Right backendResult@BackendResult{..})
                        | not (Text.null backendOutput.responseId) -> do
                            committed <-
                                config.loopBackendState.commitBackendState
                                    backendState
                            writeIORef runtime.loopRuntimeProgressRef
                                (committed, ResponseCommitted)
                            pure
                                (Right
                                    (Right backendResult
                                        { backendState = committed
                                        }))
                    _ -> pure normalized
    case raced of
        Left () -> pure (Left ())
        Right (Left err) -> do
            (priorVisible, currentVisible) <- readIORef visibleAttempts
            let emitted = priorVisible || currentVisible
            when emitted $
                config.loopOnEvent ResponseAttemptFailed
            pure $ Right $ Left $
                if emitted
                    then LoopTransportAfterOutput err
                    else LoopTransport err
        Right (Right backendResult) ->
            pure (Right (Right backendResult))

continueCommittedLoop
    :: LoopRuntime
    -> LoopCursor
    -> TurnOutput
    -> IO LoopExecution
continueCommittedLoop runtime cursor turn = do
    let config = runtime.loopRuntimeConfig
    writeIORef runtime.loopRuntimeProgressRef
        (cursor.cursorState, ResponseCommitted)
    -- The committed response absorbed every input submitted with it, and its
    -- assistant text now lives in the committed state.
    writeIORef runtime.loopRuntimePendingRef []
    writeIORef runtime.loopRuntimeUncommittedTextRef ([], [])
    writeIORef runtime.loopRuntimeUncommittedDisplayEventsRef []
    writeIORef runtime.loopRuntimeProviderAttemptActiveRef False
    -- Result metadata belongs to the response commit even when a cancellation
    -- lands before the completion event is painted.
    case turn.providerTelemetry of
        Nothing -> pure ()
        Just telemetry ->
            modifyIORef'
                runtime.loopRuntimeProviderTelemetryRef
                (telemetry :)
    protectLoopCursor runtime cursor do
        -- A cancel that landed during submitTurn after the race chose Right
        -- still counts, but its returned state is committed.
        cancelledMid <- isCancelled config.loopCancel
        if cancelledMid
            then finishLoopCursor runtime cursor
                (Left (LoopCancelled []))
            else do
                config.loopOnEvent (TurnFinished turn)
                case turn.completion of
                    TurnIncomplete{} ->
                        -- An incomplete provider response is terminal rather
                        -- than an assistant completion. Leaving the enclosing
                        -- loop scope cancels and joins any async calls that
                        -- were announced before the incomplete response.
                        finishLoopCursor runtime cursor
                            (Left (LoopIncomplete turn))
                    TurnCompleted -> do
                        config.loopCommitSteering cursor.cursorSteeringCount
                        racedResults <-
                            race
                                (waitCancel config.loopCancel)
                                (runManagedToolCalls
                                    (asyncToolManager runtime)
                                    turn.toolCalls)
                        case racedResults of
                            Left () ->
                                -- The manager itself is scoped by
                                -- 'runLoopWithEventPump'. Returning here lets
                                -- that scope cancel and join a handler or an
                                -- approval callback that has not completed.
                                finishLoopCursor runtime cursor
                                    (Left (LoopCancelled []))
                            Right results -> do
                                writeIORef
                                    runtime.loopRuntimePendingRef
                                    (map CompletedTool results)
                                cancelledAfter <-
                                    isCancelled config.loopCancel
                                if cancelledAfter
                                    then finishLoopCursor runtime cursor
                                        (Left (LoopCancelled results))
                                    else do
                                        steering <- config.loopReadSteering
                                        let continuation =
                                                map CompletedTool results
                                                    <> steering
                                        case decideCompletedTurn
                                            cursor
                                            turn
                                            continuation
                                            (length steering) of
                                            ContinueLoop nextCursor ->
                                                runLoopCursor runtime nextCursor
                                            FinishLoop result ->
                                                finishLoopCursor runtime cursor
                                                    (Right result)
                                            WarnAndFinishLoop result -> do
                                                config.loopOnEvent
                                                    (WarningRaised
                                                        emptyContinuationWarning)
                                                finishLoopCursor runtime cursor
                                                    (Right result)

runLoopWithEventPump
    :: LoopRuntime
    -> BackendSnapshot
    -> Maybe Text
    -> [TurnInput]
    -> IO LoopExecution
runLoopWithEventPump runtime initialState previousResponseId firstInputs =
    withAsync (runEventPump runtime.loopRuntimeEventPump) \eventWorker -> do
        manager <- newAsyncToolManager
        let managedRuntime =
                runtime { loopRuntimeAsyncToolManager = Just manager }
            initialSteering = managedRuntime.loopRuntimeInitialSteering
            run =
                runLoopCursor managedRuntime LoopCursor
                    { cursorState = initialState
                    , cursorProgress = NoResponseCommitted
                    , cursorPreviousResponseId = previousResponseId
                    , cursorTurnsUsed = 0
                    , cursorInputs = firstInputs <> initialSteering
                    , cursorSteeringCount = length initialSteering
                    , cursorLastOutput = Nothing
                    , cursorTokenUsage = emptyTokenUsage
                    , cursorEmptyContinuations = 0
                    }
            handleManagerFailure exception
                | isAsyncException exception =
                    Exception.throwIO exception
                | otherwise = do
                    (state, progress) <-
                        readIORef managedRuntime.loopRuntimeProgressRef
                    unexpectedLoopExecution
                        managedRuntime
                        state
                        progress
                        exception
        execution <-
            withAsync
                (runAsyncToolManager
                    managedRuntime.loopRuntimeConfig
                    manager)
                \managerWorker -> do
                    raced <-
                        race
                            (race
                                (waitEventPumpFailure
                                    eventWorker
                                    managedRuntime.loopRuntimeEventPump)
                                (waitAsyncToolManagerFailure
                                    managerWorker
                                    manager))
                            run
                    case raced of
                        Left (Left failure) -> do
                            (state, progress) <-
                                readIORef
                                    managedRuntime.loopRuntimeProgressRef
                            handleLoopEventFailure
                                (unexpectedLoopExecution managedRuntime)
                                state
                                progress
                                failure
                        Left (Right exception) ->
                            handleManagerFailure exception
                        Right completed ->
                            atomically
                                (tryReadTMVar manager.asyncToolFailure)
                                >>= maybe
                                    (pure completed)
                                    handleManagerFailure
        flushEventPump runtime.loopRuntimeEventPump >>= \case
            Left failure ->
                readIORef runtime.loopRuntimeProgressRef
                    >>= \(state, progress) ->
                        handleLoopEventFailure
                            (unexpectedLoopExecution runtime)
                            state
                            progress
                            failure
            Right () -> pure execution

handleLoopEventFailure
    :: (BackendSnapshot -> LoopProgress -> SomeException -> IO LoopExecution)
    -> BackendSnapshot
    -> LoopProgress
    -> EventPumpFailure
    -> IO LoopExecution
handleLoopEventFailure unexpected state progress = \case
    EventPumpSyncFailure exception ->
        unexpected state progress exception
    EventPumpAsyncFailure exception ->
        Exception.throwIO exception

data AsyncToolManager = AsyncToolManager
    { asyncToolRequests :: !(TQueue ManagedToolRequest)
    , asyncToolCalls :: !(TVar (Map Text ManagedToolCall))
    , asyncToolNextSequence :: !(TVar Int)
    , asyncToolScheduled :: !(TVar (IntMap ToolSchedulingPlan))
    , asyncToolOutstanding :: !(TVar Int)
    , asyncToolCompleted :: !(TQueue ToolCallResult)
    , asyncToolFailure :: !(TMVar SomeException)
    }

data ManagedToolCall = ManagedToolCall
    { managedCall :: !ToolCall
    , managedResult :: !(TMVar (Maybe ToolCallResult))
    }

data ManagedToolRequest = ManagedToolRequest
    { managedSequence :: !Int
    , managedRecord :: !ManagedToolCall
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
        <*> newTVarIO 0
        <*> newTVarIO IntMap.empty
        <*> newTVarIO 0
        <*> newTQueueIO
        <*> newEmptyTMVarIO

asyncToolManager :: LoopRuntime -> AsyncToolManager
asyncToolManager runtime =
    case runtime.loopRuntimeAsyncToolManager of
        Just manager -> manager
        Nothing ->
            error "async tool manager used outside runLoopWithEventPump"

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
            sequenceNumber <- readTVar manager.asyncToolNextSequence
            let record = ManagedToolCall
                    { managedCall = call
                    , managedResult = result
                    }
            writeTVar
                manager.asyncToolCalls
                (Map.insert call.callId record calls)
            writeTVar
                manager.asyncToolNextSequence
                (sequenceNumber + 1)
            when (toolCallMode call == AsyncToolCall) $
                modifyTVar' manager.asyncToolOutstanding (+ 1)
            writeTQueue manager.asyncToolRequests ManagedToolRequest
                { managedSequence = sequenceNumber
                , managedRecord = record
                }
            pure result

runManagedToolCalls
    :: AsyncToolManager
    -> [ToolCall]
    -> IO [ToolCallResult]
runManagedToolCalls manager calls = do
    blocking <- catMaybes <$> traverse admit calls
    blockingResults <-
        catMaybes <$> traverse (atomically . readTMVar) blocking
    completedAsync <- atomically (takeAsyncToolCompletions manager)
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
                            request.managedRecord.managedCall
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
                                            request.managedSequence
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
    -> ManagedToolRequest
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
                            || ( sequenceNumber < request.managedSequence
                                && schedulingPlansConflict earlierPlan plan
                               ))
                    False
                    scheduled
    race
        (waitCancel config.loopCancel)
        (runPreparedToolCall config prepared)
        >>= \case
            Left () -> pure Nothing
            Right result -> pure result

completeManagedToolRequest
    :: AsyncToolManager
    -> ManagedToolRequest
    -> Either SomeException (Maybe ToolCallResult)
    -> IO ()
completeManagedToolRequest manager request outcome =
    atomically do
        modifyTVar'
            manager.asyncToolScheduled
            (IntMap.delete request.managedSequence)
        let call = request.managedRecord.managedCall
        case outcome of
            Left exception -> do
                -- Do not publish a synthetic empty completion for a crashed
                -- worker. Keeping any waiter blocked makes the manager-failure
                -- branch of the enclosing structured race authoritative.
                _ <- tryPutTMVar manager.asyncToolFailure exception
                pure ()
            Right result -> do
                putTMVar request.managedRecord.managedResult result
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

-- | Preserve model order between conflicting calls while allowing independent
-- calls from the same model turn to overlap. Results are returned in model
-- order regardless of completion order.
runToolCalls :: LoopConfig -> [ToolCall] -> IO [ToolCallResult]
runToolCalls config calls = do
    prepared <- prepareIndexedToolCalls config (zip [0..] calls)
    scheduled <- traverse schedule prepared
    go scheduled IntMap.empty
  where
    schedule
        :: IndexedPreparedToolCall
        -> IO PreparedScheduledToolCall
    schedule indexed = do
        plan <- schedulingPlanForPrepared config indexed.prepared
        pure PreparedScheduledToolCall
            { index = indexed.index
            , plan
            , prepared = indexed.prepared
            }

    go
        :: [PreparedScheduledToolCall]
        -> IntMap ToolCallResult
        -> IO [ToolCallResult]
    go [] completed =
        pure (IntMap.elems completed)
    go remaining completed = do
        let ready = readyCalls remaining
            readyIndexes = IntSet.fromList (map (.index) ready)
            pending =
                filter
                    (\scheduled ->
                        IntSet.notMember scheduled.index readyIndexes)
                    remaining
        raced <- race
            (waitCancel config.loopCancel)
            (mapConcurrently
                (\scheduled -> do
                    result <-
                        runPreparedToolCall config scheduled.prepared
                    pure (fmap (\value -> (scheduled.index, value)) result))
                ready)
        case raced of
            Left () ->
                -- 'race' cancels and joins the structured concurrent batch,
                -- so no tool handler survives the cancelled turn.
                pure (IntMap.elems completed)
            Right batchResults -> do
                let completed' =
                        foldr
                            (\result acc ->
                                maybe
                                    acc
                                    (\(index, value) ->
                                        IntMap.insert index value acc)
                                    result)
                            completed
                            batchResults
                go pending completed'

data IndexedPreparedToolCall = IndexedPreparedToolCall
    { index :: !Int
    , prepared :: !PreparedToolCall
    }

data PreparedScheduledToolCall = PreparedScheduledToolCall
    { index :: !Int
    , plan :: !ToolSchedulingPlan
    , prepared :: !PreparedToolCall
    }

readyCalls :: [PreparedScheduledToolCall] -> [PreparedScheduledToolCall]
readyCalls calls =
    [ call
    | call <- calls
    , not
        (any
            (\earlier ->
                earlier.index < call.index
                    && schedulingPlansConflict earlier.plan call.plan)
            calls)
    ]

prepareIndexedToolCalls
    :: LoopConfig
    -> [(Int, ToolCall)]
    -> IO [IndexedPreparedToolCall]
prepareIndexedToolCalls _ [] = pure []
prepareIndexedToolCalls config ((index, call) : rest) = do
    cancelled <- isCancelled config.loopCancel
    if cancelled
        then pure []
        else do
            prepared <- prepareToolCall config call
            cancelledAfter <- isCancelled config.loopCancel
            if cancelledAfter
                then pure []
                else
                    (IndexedPreparedToolCall
                        { index
                        , prepared
                        } :)
                        <$> prepareIndexedToolCalls config rest

data ToolApproval
    = ToolApprovalDenied !Text
    | ToolApprovalRejected
    | ToolApprovalGranted

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
                Right decision ->
                    normalizeApproval decision
  where
    normalizeApproval = \case
        Left denial -> ToolApprovalDenied denial
        Right False -> ToolApprovalRejected
        Right True -> ToolApprovalGranted

runPreparedToolCall
    :: LoopConfig
    -> PreparedToolCall
    -> IO (Maybe ToolCallResult)
runPreparedToolCall config (PreparedToolCall call approval) = do
    cancelled <- isCancelled config.loopCancel
    if cancelled
        then pure Nothing
        else do
            config.loopOnEvent (ToolStarted call)
            result <- case approval of
                ToolApprovalDenied denial ->
                    pure $
                        ToolCallResult
                            { callId = call.callId
                            , output = denial
                            , callKind = call.callKind
                            , toolResultMode = toolCallMode call
                            , toolResultImages = []
                            , toolResultOutcome = Just ToolDenied
                            }
                ToolApprovalRejected ->
                    pure $
                        ToolCallResult
                            { callId = call.callId
                            , output = "Tool call rejected by user."
                            , callKind = call.callKind
                            , toolResultMode = toolCallMode call
                            , toolResultImages = []
                            , toolResultOutcome = Just ToolDenied
                            }
                ToolApprovalGranted ->
                    dispatchRegisteredToolCall
                        config.loopDispatch
                            { toolDispatchOnOutput = \progressCall output ->
                                config.loopDispatch.toolDispatchOnOutput progressCall output
                                    >> config.loopOnEvent
                                        (ToolOutputUpdated progressCall.callId output)
                            }
                        config.loopTools
                        call
            config.loopOnEvent (ToolFinished result)
            pure (Just result)
