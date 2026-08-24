-- | Provider-neutral agent loop: submit a user turn, dispatch tool calls,
-- feed results back, repeat until the model answers in text or hits a cap.
--
-- Transports close over model, instructions, and tool schemas. This module
-- only sees 'ToolCall' / 'ToolCallResult' and a 'Backend' callback.
module Agent.Loop
    ( Backend(..)
    , BackendResult(..)
    , BackendStateStore(..)
    , FileAttachment(..)
    , ImageAttachment(..)
    , LoopConfig(..)
    , LoopExecution(..)
    , LoopEvent(..)
    , LoopError(..)
    , LoopProgress(..)
    , LoopResult(..)
    , TokenUsage(..)
    , TurnInput(..)
    , TurnOutput(..)
    , addTokenUsage
    , defaultLoopMaxTurns
    , defaultLoopDispatch
    , emptyTokenUsage
    , emptyTurnOutput
    , runLoop
    , runLoopInputs
    , runLoopInputsDetailed
    ) where

import Agent.Cancel (CancelFlag, isCancelled, waitCancel)
import Agent.Error (ApiError)
import Agent.InterAgentMessage (InterAgentMessage)
import Agent.Responses.Types (ResponseItem)
import Agent.TextBuffer
    ( TextBuffer
    , appendTextBuffer
    , emptyTextBuffer
    , textBufferToText
    )
import Agent.ToolDispatch
    ( ToolCall(..)
    , ToolCallResult(..)
    , ToolDispatchConfig(..)
    )
import Agent.Tools.Scheduling
    ( ToolSchedulingPlan(..)
    , schedulingPlansConflict
    )
import Agent.Tools.Types
    ( ToolRegistry
    , dispatchRegisteredToolCall
    , toolSchedulingPlanFor
    )
import Control.Concurrent.Async
    ( Async
    , mapConcurrently
    , race
    , waitCatch
    , withAsync
    )
import Control.Concurrent.STM
    ( TBQueue
    , TMVar
    , STM
    , TVar
    , atomically
    , newEmptyTMVarIO
    , newTVar
    , newTBQueueIO
    , modifyTVar'
    , orElse
    , putTMVar
    , readTBQueue
    , readTMVar
    , readTVar
    , retry
    , tryPutTMVar
    , writeTVar
    , writeTBQueue
    )
import Control.Exception (AsyncException, toException)
import qualified Control.Exception as Exception
import Control.Exception.Safe
    ( SomeException
    , catchAsync
    , displayException
    , isAsyncException
    , mask
    , throwIO
    , tryAny
    )
import Control.Monad (void)
import Data.Aeson (FromJSON(..), ToJSON(..), object, withObject, (.:), (.:?), (.!=), (.=))
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text

-- | Image bytes attached to a user turn (PNG/JPEG/…).
data ImageAttachment = ImageAttachment
    { imageMime :: !Text
    , imageBytes :: !ByteString
    } deriving (Eq)

instance Show ImageAttachment where
    show image =
        "ImageAttachment { imageMime = " <> show image.imageMime
            <> ", imageBytes = <redacted>"
            <> ", imageByteLength = " <> show (ByteString.length image.imageBytes)
            <> " }"

-- | File bytes attached to a user turn. Providers that cannot ingest files
-- natively should fall back to a local path or text summary.
data FileAttachment = FileAttachment
    { fileName :: !(Maybe Text)
    , fileMime :: !Text
    , fileBytes :: !ByteString
    } deriving (Eq)

instance Show FileAttachment where
    show file =
        "FileAttachment { fileName = " <> show file.fileName
            <> ", fileMime = " <> show file.fileMime
            <> ", fileBytes = <redacted>"
            <> ", fileByteLength = " <> show (ByteString.length file.fileBytes)
            <> " }"

data TurnInput
    = UserMessage Text
    | AgentMessage InterAgentMessage
    | UserMultimodal
        { userText :: !Text
        , userImages :: ![ImageAttachment]
        }
    | UserMultimodalFiles
        { userText :: !Text
        , userImages :: ![ImageAttachment]
        , userFiles :: ![FileAttachment]
        }
    | CompletedTool ToolCallResult
    deriving (Eq, Show)

-- | Provider-reported token counts for one model response. @inputTokens@
-- typically includes any cached prefix; @cachedTokens@ is that subset when
-- the provider reports it.
data TokenUsage = TokenUsage
    { inputTokens :: !Int
    , outputTokens :: !Int
    , cachedTokens :: !Int
    } deriving (Eq, Show)

instance Semigroup TokenUsage where
    left <> right = addTokenUsage left right

instance Monoid TokenUsage where
    mempty = emptyTokenUsage

emptyTokenUsage :: TokenUsage
emptyTokenUsage = TokenUsage
    { inputTokens = 0
    , outputTokens = 0
    , cachedTokens = 0
    }

instance ToJSON TokenUsage where
    toJSON usage = object
        [ "input" .= usage.inputTokens
        , "output" .= usage.outputTokens
        , "cached" .= usage.cachedTokens
        ]

instance FromJSON TokenUsage where
    parseJSON = withObject "TokenUsage" \o ->
        TokenUsage
            <$> o .: "input"
            <*> o .: "output"
            <*> (o .:? "cached" .!= 0)

addTokenUsage :: TokenUsage -> TokenUsage -> TokenUsage
addTokenUsage a b = TokenUsage
    { inputTokens = a.inputTokens + b.inputTokens
    , outputTokens = a.outputTokens + b.outputTokens
    , cachedTokens = a.cachedTokens + b.cachedTokens
    }

data TurnOutput = TurnOutput
    { responseId :: !Text
    , toolCalls :: ![ToolCall]
    , assistantText :: !(Maybe Text)
    , tokenUsage :: !TokenUsage
    } deriving (Eq, Show)

data LoopProgress
    = NoResponseCommitted
    | ResponseCommitted
    deriving (Eq, Show)

data LoopExecution = LoopExecution
    { executionState :: ![ResponseItem]
    , executionProgress :: !LoopProgress
    , executionResult :: !(Either LoopError LoopResult)
    } deriving (Eq, Show)

emptyTurnOutput :: Text -> [ToolCall] -> Maybe Text -> TurnOutput
emptyTurnOutput responseId toolCalls assistantText = TurnOutput
    { responseId
    , toolCalls
    , assistantText
    , tokenUsage = emptyTokenUsage
    }

data BackendResult = BackendResult
    { backendOutput :: !TurnOutput
    , backendState :: ![ResponseItem]
    } deriving (Eq, Show)

newtype Backend = Backend
    { submitTurn
        :: [ResponseItem]
        -> Maybe Text
        -> [TurnInput]
        -> (LoopEvent -> IO ())
        -> IO (Either ApiError BackendResult)
    }

data BackendStateStore = BackendStateStore
    { readBackendState :: !(IO [ResponseItem])
      -- | Publish a completed provider response for live observers and later
      -- tool continuations. Higher-level turn policy may still deliberately
      -- roll this state back after cancellation or terminal failure.
    , commitBackendState :: !([ResponseItem] -> IO ())
    }

data LoopEvent
    = TextDelta Text
    | ReasoningDelta Text
    -- | Ephemeral transport/tool activity for the live CLI status line.
    | ActivityUpdated Text
    -- | A persistent user-visible warning that must not replace live activity.
    | WarningRaised Text
    -- | A streamed response was interrupted and its provider submission is
    -- being retried. Renderers must close the partial stream before displaying
    -- output from the new attempt.
    | ResponseRestarted Text
    | TurnStarted
    | TurnFinished TurnOutput
    | ToolStarted ToolCall
    -- | Latest accumulated output snapshot for an in-flight tool call.
    | ToolOutputUpdated Text Text
    | ToolFinished ToolCallResult
    deriving (Eq, Show)

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

defaultLoopMaxTurns :: Int
defaultLoopMaxTurns = 500

-- | CLI-facing formatter: unknown tools, handler errors, and crashes stay
-- in-band as tool output so the model can continue.
defaultLoopDispatch :: ToolDispatchConfig
defaultLoopDispatch = ToolDispatchConfig
    { toolDispatchUnknownTool = \name -> "Unknown tool: " <> name
    , toolDispatchFormatResult = \case
        Left err -> "Error: " <> err
        Right output -> output
    , toolDispatchFormatException = \name exception ->
        "Tool " <> name <> " crashed: " <> Text.pack (show exception)
    , toolDispatchOnException = \_name (_ :: SomeException) -> pure ()
    , toolDispatchOnOutput = \_call _output -> pure ()
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

runLoopInputsUnsafe
    :: LoopConfig
    -> [ResponseItem]
    -> Maybe Text
    -> [TurnInput]
    -> IO LoopExecution
runLoopInputsUnsafe config0 initialState previousResponseId firstInputs = do
    eventPump <- newLoopEventPump config0.loopOnEvent
    progressRef <- newIORef (initialState, NoResponseCommitted)
    withAsync (runLoopEventPump eventPump) \eventWorker -> do
        let config = config0
                { loopOnEvent = emitLoopEvent eventPump
                }
            finish state progress result = do
                writeIORef progressRef (state, progress)
                pure LoopExecution
                    { executionState = state
                    , executionProgress = progress
                    , executionResult = result
                    }
            unexpected state progress exception =
                finish state progress
                    (Left (LoopUnexpected (exceptionSummary exception)))
            protect state progress action =
                tryAny action >>= either (unexpected state progress) pure
            go state progress prev turnsUsed inputs lastOutput usageAcc = do
                writeIORef progressRef (state, progress)
                if turnsUsed >= config.loopMaxTurns
                    then finish state progress $ case lastOutput of
                        Just turn -> Left (LoopMaxTurns turn)
                        Nothing -> Left LoopNoResponseId
                    else protect state progress do
                        cancelled <- isCancelled config.loopCancel
                        if cancelled
                            then finish state progress (Left (LoopCancelled []))
                            else do
                                config.loopOnEvent TurnStarted
                                outputSeen <- newIORef False
                                let onBackendEvent event = do
                                        case event of
                                            TextDelta _ -> writeIORef outputSeen True
                                            ReasoningDelta _ -> writeIORef outputSeen True
                                            _ -> pure ()
                                        config.loopOnEvent event
                                -- Race the model call against cancel so Ctrl-C / Esc
                                -- can stop reasoning mid-stream, not only between tools.
                                raced <- mask \restore -> do
                                    result <- restore $ race
                                        (waitCancel config.loopCancel)
                                        (config.loopBackend.submitTurn
                                            state prev inputs onBackendEvent)
                                    case result of
                                        Right (Right BackendResult{..})
                                            | not
                                                (Text.null
                                                    backendOutput.responseId) -> do
                                                config.loopBackendState.commitBackendState
                                                    backendState
                                                writeIORef progressRef
                                                    (backendState, ResponseCommitted)
                                        _ -> pure ()
                                    pure result
                                case raced of
                                    Left () ->
                                        finish state progress (Left (LoopCancelled []))
                                    Right (Left err) -> do
                                        emitted <- readIORef outputSeen
                                        finish state progress $ Left $
                                            if emitted
                                                then LoopTransportAfterOutput err
                                                else LoopTransport err
                                    Right (Right BackendResult{backendOutput = turn})
                                        | Text.null turn.responseId ->
                                            finish state progress (Left LoopNoResponseId)
                                    Right (Right BackendResult{..}) -> do
                                        continueCommitted
                                            backendState backendOutput turnsUsed usageAcc
            continueCommitted state turn turnsUsed usageAcc = do
                writeIORef progressRef (state, ResponseCommitted)
                protect state ResponseCommitted do
                    -- A cancel that landed during submitTurn after the race chose
                    -- Right still counts, but its returned state is committed.
                    cancelledMid <- isCancelled config.loopCancel
                    let usageAcc' = addTokenUsage usageAcc turn.tokenUsage
                    if cancelledMid
                        then finish state ResponseCommitted
                            (Left (LoopCancelled []))
                        else do
                            config.loopOnEvent (TurnFinished turn)
                            let nextTurnsUsed = turnsUsed + 1
                            if null turn.toolCalls
                                then finish state ResponseCommitted $
                                    Right LoopResult
                                        { finalResponseId = turn.responseId
                                        , finalText = turn.assistantText
                                        , turnsUsed = nextTurnsUsed
                                        , tokenUsage = usageAcc'
                                        }
                                else do
                                    results <- runToolCalls config turn.toolCalls
                                    cancelledAfter <-
                                        isCancelled config.loopCancel
                                    if cancelledAfter
                                        then finish state ResponseCommitted
                                            (Left (LoopCancelled results))
                                        else go
                                            state
                                            ResponseCommitted
                                            (Just turn.responseId)
                                            nextTurnsUsed
                                            (map CompletedTool results)
                                            (Just turn)
                                            usageAcc'
            run =
                go initialState NoResponseCommitted previousResponseId 0 firstInputs
                    Nothing emptyTokenUsage
        raced <- race (waitLoopEventFailure eventWorker eventPump) run
        execution <- case raced of
            Left failure -> do
                (state, progress) <- readIORef progressRef
                handleLoopEventFailure unexpected state progress failure
            Right completed -> pure completed
        flushLoopEvents eventPump >>= \case
            Left failure ->
                handleLoopEventFailure
                    unexpected
                    execution.executionState
                    execution.executionProgress
                    failure
            Right () -> pure execution

data LoopEventCommand
    = DeliverLoopEvent !LoopEvent
    | DeliverCoalescedLoopEvent !CoalescedLoopEvent
    | FlushLoopEvents !(TMVar ())

data CoalescedLoopEvent
    = CoalescedTextDelta !(TVar TextBuffer)
    | CoalescedReasoningDelta !(TVar TextBuffer)
    | CoalescedToolOutput !Text !(TVar Text)

data LoopEventFailure
    = LoopEventSyncFailure !SomeException
    | LoopEventAsyncFailure !AsyncException

data LoopEventPump = LoopEventPump
    { eventPumpQueue :: !(TBQueue LoopEventCommand)
    , eventPumpFailure :: !(TMVar LoopEventFailure)
    , eventPumpTail :: !(TVar (Maybe CoalescedLoopEvent))
    , eventPumpSink :: !(LoopEvent -> IO ())
    }

loopEventQueueCapacity :: Int
loopEventQueueCapacity = 256

newLoopEventPump :: (LoopEvent -> IO ()) -> IO LoopEventPump
newLoopEventPump sink = do
    queue <- newTBQueueIO (fromIntegral loopEventQueueCapacity)
    failure <- newEmptyTMVarIO
    tailEvent <- atomically (newTVar Nothing)
    pure LoopEventPump
        { eventPumpQueue = queue
        , eventPumpFailure = failure
        , eventPumpTail = tailEvent
        , eventPumpSink = sink
        }

runLoopEventPump :: LoopEventPump -> IO ()
runLoopEventPump pump = go
  where
    go =
        atomically (readTBQueue pump.eventPumpQueue) >>= \case
            DeliverLoopEvent event ->
                deliver event
            DeliverCoalescedLoopEvent pending -> do
                event <- atomically do
                    current <- readTVar pump.eventPumpTail
                    whenSTM (sameCoalescedEvent current pending) $
                        writeTVar pump.eventPumpTail Nothing
                    materializeCoalescedEvent pending
                deliver event
            FlushLoopEvents flushed -> do
                atomically (putTMVar flushed ())
                go
    deliver event = do
        (tryAny (pump.eventPumpSink event) >>= \case
            Right () -> go
            Left exception -> do
                atomically $
                    recordLoopEventFailure
                        pump
                        (LoopEventSyncFailure exception)
                atomically retry)
            `catchAsync` \(exception :: AsyncException) -> do
                atomically $
                    recordLoopEventFailure
                        pump
                        (LoopEventAsyncFailure exception)
                Exception.throwIO exception

emitLoopEvent :: LoopEventPump -> LoopEvent -> IO ()
emitLoopEvent pump = \case
    TextDelta text ->
        enqueueTextDelta pump False text
    ReasoningDelta text ->
        enqueueTextDelta pump True text
    ToolOutputUpdated callId output ->
        enqueueToolOutput pump callId output
    event ->
        enqueueLoopEventCommand pump (DeliverLoopEvent event)

flushLoopEvents :: LoopEventPump -> IO (Either LoopEventFailure ())
flushLoopEvents pump = do
    flushed <- newEmptyTMVarIO
    tryAny (enqueueLoopEventCommand pump (FlushLoopEvents flushed)) >>= \case
        Left exception
            | isAsyncException exception ->
                Exception.throwIO exception
            | otherwise ->
                pure (Left (LoopEventSyncFailure exception))
        Right () ->
            atomically $
                (Left <$> readTMVar pump.eventPumpFailure)
                    `orElse`
                (readTMVar flushed >> pure (Right ()))

enqueueLoopEventCommand :: LoopEventPump -> LoopEventCommand -> IO ()
enqueueLoopEventCommand pump command =
    atomically
        ( (Left <$> readTMVar pump.eventPumpFailure)
            `orElse`
          (do
            clearEventPumpTail pump
            writeTBQueue pump.eventPumpQueue command
            pure (Right ()))
        ) >>= either throwLoopEventFailure pure

enqueueTextDelta :: LoopEventPump -> Bool -> Text -> IO ()
enqueueTextDelta pump reasoning text =
    atomically
        ( (Left <$> readTMVar pump.eventPumpFailure)
            `orElse`
          (do
            current <- readTVar pump.eventPumpTail
            case current of
                Just (CoalescedTextDelta buffer)
                    | not reasoning -> do
                        modifyTVar' buffer (appendTextBuffer text)
                        pure (Right ())
                Just (CoalescedReasoningDelta buffer)
                    | reasoning -> do
                        modifyTVar' buffer (appendTextBuffer text)
                        pure (Right ())
                _ -> do
                    buffer <- newTVar
                        (appendTextBuffer text emptyTextBuffer)
                    let pending =
                            if reasoning
                                then CoalescedReasoningDelta buffer
                                else CoalescedTextDelta buffer
                    writeTVar pump.eventPumpTail (Just pending)
                    writeTBQueue pump.eventPumpQueue
                        (DeliverCoalescedLoopEvent pending)
                    pure (Right ()))
        ) >>= either throwLoopEventFailure pure

enqueueToolOutput :: LoopEventPump -> Text -> Text -> IO ()
enqueueToolOutput pump callId output =
    atomically
        ( (Left <$> readTMVar pump.eventPumpFailure)
            `orElse`
          (do
            current <- readTVar pump.eventPumpTail
            case current of
                Just (CoalescedToolOutput currentId snapshot)
                    | currentId == callId -> do
                        writeTVar snapshot output
                        pure (Right ())
                _ -> do
                    snapshot <- newTVar output
                    let pending = CoalescedToolOutput callId snapshot
                    writeTVar pump.eventPumpTail (Just pending)
                    writeTBQueue pump.eventPumpQueue
                        (DeliverCoalescedLoopEvent pending)
                    pure (Right ()))
        ) >>= either throwLoopEventFailure pure

sameCoalescedEvent
    :: Maybe CoalescedLoopEvent
    -> CoalescedLoopEvent
    -> Bool
sameCoalescedEvent current pending =
    case (current, pending) of
        (Just (CoalescedTextDelta left), CoalescedTextDelta right) ->
            left == right
        (Just (CoalescedReasoningDelta left), CoalescedReasoningDelta right) ->
            left == right
        ( Just (CoalescedToolOutput leftId left)
          , CoalescedToolOutput rightId right
          ) ->
            leftId == rightId && left == right
        _ -> False

materializeCoalescedEvent :: CoalescedLoopEvent -> STM LoopEvent
materializeCoalescedEvent = \case
    CoalescedTextDelta buffer ->
        TextDelta . textBufferToText <$> readTVar buffer
    CoalescedReasoningDelta buffer ->
        ReasoningDelta . textBufferToText <$> readTVar buffer
    CoalescedToolOutput callId snapshot ->
        ToolOutputUpdated callId <$> readTVar snapshot

clearEventPumpTail :: LoopEventPump -> STM ()
clearEventPumpTail pump =
    readTVar pump.eventPumpTail >>= \case
        Nothing -> pure ()
        Just _ -> writeTVar pump.eventPumpTail Nothing

whenSTM :: Bool -> STM () -> STM ()
whenSTM True action = action
whenSTM False _ = pure ()

waitLoopEventFailure :: Async () -> LoopEventPump -> IO LoopEventFailure
waitLoopEventFailure worker pump =
    race
        (atomically (readTMVar pump.eventPumpFailure))
        (waitCatch worker) >>= \case
            Left failure -> pure failure
            Right (Left exception)
                | isAsyncException exception ->
                    Exception.throwIO exception
                | otherwise ->
                    pure (LoopEventSyncFailure exception)
            Right (Right ()) ->
                pure . LoopEventSyncFailure . toException $
                    userError "loop event pump stopped unexpectedly"

throwLoopEventFailure :: LoopEventFailure -> IO a
throwLoopEventFailure = \case
    LoopEventSyncFailure exception -> throwIO exception
    LoopEventAsyncFailure exception -> Exception.throwIO exception

handleLoopEventFailure
    :: ([ResponseItem] -> LoopProgress -> SomeException -> IO LoopExecution)
    -> [ResponseItem]
    -> LoopProgress
    -> LoopEventFailure
    -> IO LoopExecution
handleLoopEventFailure unexpected state progress = \case
    LoopEventSyncFailure exception ->
        unexpected state progress exception
    LoopEventAsyncFailure exception ->
        Exception.throwIO exception

recordLoopEventFailure :: LoopEventPump -> LoopEventFailure -> STM ()
recordLoopEventFailure pump failure =
    void (tryPutTMVar pump.eventPumpFailure failure)

-- | Preserve model order between conflicting calls while allowing independent
-- calls from the same model turn to overlap. Results are returned in model
-- order regardless of completion order.
runToolCalls :: LoopConfig -> [ToolCall] -> IO [ToolCallResult]
runToolCalls config calls = do
    prepared <- prepareIndexedToolCalls config (zip [0..] calls)
    scheduled <- traverse schedule prepared
    go scheduled Map.empty
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
        -> Map.Map Int ToolCallResult
        -> IO [ToolCallResult]
    go [] completed =
        pure (map snd (Map.toAscList completed))
    go remaining completed = do
        let ready = readyCalls remaining
            readyIndexes = Map.fromList [(call.index, ()) | call <- ready]
            pending =
                filter
                    (\scheduled ->
                        Map.notMember scheduled.index readyIndexes)
                    remaining
        batchResults <-
            mapConcurrently
                (\scheduled -> do
                    result <-
                        runPreparedToolCall config scheduled.prepared
                    pure (fmap (\value -> (scheduled.index, value)) result))
                ready
        let completed' =
                foldr
                    (\result acc ->
                        maybe
                            acc
                            (\(index, value) -> Map.insert index value acc)
                            result)
                    completed
                    batchResults
        cancelled <- isCancelled config.loopCancel
        if cancelled
            then pure (map snd (Map.toAscList completed'))
            else go pending completed'

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
    approval <- config.loopApprove call
    pure (PreparedToolCall call (normalizeApproval approval))
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
                    pure ToolCallResult
                        { callId = call.callId
                        , output = denial
                        , callKind = call.callKind
                        }
                ToolApprovalRejected ->
                    pure ToolCallResult
                        { callId = call.callId
                        , output = "Tool call rejected by user."
                        , callKind = call.callKind
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
