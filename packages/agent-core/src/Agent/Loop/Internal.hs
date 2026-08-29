module Agent.Loop.Internal where

import Agent.Cancel (CancelFlag, isCancelled, waitCancel)
import qualified Agent.Json.Decode as Json
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
import Data.Aeson (ToJSON(..), object, (.=))
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)
import qualified Data.IntSet as IntSet
import Data.Text (Text)
import qualified Data.Text as Text

maxEmptyContinuations :: Int
maxEmptyContinuations = 2

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

tokenUsageDecoder :: Json.Decoder TokenUsage
tokenUsageDecoder = Json.object $
    TokenUsage
        <$> Json.atKey "input" Json.int
        <*> Json.atKey "output" Json.int
        <*> (maybe 0 id <$> Json.atKeyOptional "cached" Json.int)

addTokenUsage :: TokenUsage -> TokenUsage -> TokenUsage
addTokenUsage a b = TokenUsage
    { inputTokens = a.inputTokens + b.inputTokens
    , outputTokens = a.outputTokens + b.outputTokens
    , cachedTokens = a.cachedTokens + b.cachedTokens
    }

-- | Rough streamed-text estimate: about four Unicode scalars per token.
estimateTokensFromChars :: Int -> Int
estimateTokensFromChars chars = (max 0 chars + 3) `div` 4

-- | Output tokens per second from a token count and elapsed milliseconds.
tokensPerSecond :: Int -> Int -> Maybe Double
tokensPerSecond tokens millis
    | tokens <= 0 = Nothing
    | millis <= 0 = Nothing
    | otherwise =
        Just (fromIntegral tokens * 1000 / fromIntegral millis)

-- | Prefer provider-reported output tokens; fall back to the streamed-text
-- estimate when usage is missing.
generationTokensPerSecond :: Int -> Int -> Int -> Maybe Double
generationTokensPerSecond reportedOutput chars millis =
    tokensPerSecond
        ( if reportedOutput > 0
            then reportedOutput
            else estimateTokensFromChars chars
        )
        millis

-- | Live tok/s is noisy on the first few hundred milliseconds of a stream.
liveTokenRateMinMillis :: Int
liveTokenRateMinMillis = 400

liveTokensPerSecond :: Int -> Int -> Maybe Double
liveTokensPerSecond chars millis
    | millis < liveTokenRateMinMillis = Nothing
    | otherwise = tokensPerSecond (estimateTokensFromChars chars) millis

data TurnOutput = TurnOutput
    { responseId :: !Text
    , toolCalls :: ![ToolCall]
    , assistantText :: !(Maybe Text)
    , tokenUsage :: !TokenUsage
    , completion :: !TurnCompletion
    } deriving (Eq, Show)

data TurnCompletion
    = TurnCompleted
    | TurnIncomplete
        { incompleteReason :: !Text
        , incompleteReasoningTokens :: !(Maybe Int)
        }
    deriving (Eq, Show)

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
    , executionResult :: !(Either LoopError LoopResult)
    } deriving (Eq, Show)

emptyTurnOutput :: Text -> [ToolCall] -> Maybe Text -> TurnOutput
emptyTurnOutput responseId toolCalls assistantText = TurnOutput
    { responseId
    , toolCalls
    , assistantText
    , tokenUsage = emptyTokenUsage
    , completion = TurnCompleted
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
    -- | Replace the metadata for an already-visible in-flight tool call.
    -- Providers may learn canonical arguments after an early live start.
    | ToolUpdated ToolCall
    -- | Latest accumulated output snapshot for an in-flight tool call.
    | ToolOutputUpdated Text Text
    | ToolFinished ToolCallResult
    -- | Remove a provider-retracted tool call from the current attempt.
    | ToolRetracted Text
    -- | Discard all UI activity emitted by the current response attempt.
    -- This is distinct from ending the whole turn: a retry may follow.
    | ResponseAttemptDiscarded
    -- | Lifecycle/activity from a provider-managed child agent. These agents
    -- are display-only unless the provider exposes targeted controls.
    | NativeAgentStarted Text (Maybe Text) Text (Maybe Text)
    | NativeAgentOutput Text Text
    | NativeAgentFinished Text NativeAgentStatus
    deriving (Eq, Show)

data NativeAgentStatus
    = NativeAgentRunning
    | NativeAgentCompleted
    | NativeAgentFailed
    | NativeAgentCancelled
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
    -- | Read pending user guidance submitted while this loop is active.
    -- Guidance is acknowledged only after the model response commits, so a
    -- failed submission can be retried without losing it.
    , loopReadSteering :: !(IO [TurnInput])
    , loopCommitSteering :: !(Int -> IO ())
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
    initialSteering <- config0.loopReadSteering
    pendingRef <- newIORef (firstInputs <> initialSteering)
    withAsync (runLoopEventPump eventPump) \eventWorker -> do
        let config = config0
                { loopOnEvent = emitLoopEvent eventPump
                }
            finish state progress result = do
                writeIORef progressRef (state, progress)
                pending <- readIORef pendingRef
                pure LoopExecution
                    { executionState = state
                    , executionPendingInputs = pending
                    , executionProgress = progress
                    , executionResult = result
                    }
            unexpected state progress exception =
                finish state progress
                    (Left (LoopUnexpected (exceptionSummary exception)))
            protect state progress action =
                tryAny action >>= either (unexpected state progress) pure
            go state progress prev turnsUsed inputs steeringCount lastOutput
                    usageAcc emptyContinuations = do
                writeIORef progressRef (state, progress)
                writeIORef pendingRef inputs
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
                                            backendState backendOutput turnsUsed
                                            steeringCount usageAcc emptyContinuations
            continueCommitted state turn turnsUsed steeringCount usageAcc
                    emptyContinuations = do
                writeIORef progressRef (state, ResponseCommitted)
                -- The committed response absorbed every input submitted with it.
                writeIORef pendingRef []
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
                            case turn.completion of
                                TurnIncomplete{} ->
                                    finish state ResponseCommitted
                                        (Left (LoopIncomplete turn))
                                TurnCompleted -> do
                                    config.loopCommitSteering steeringCount
                                    results <-
                                        if null turn.toolCalls
                                            then pure []
                                            else runToolCalls config turn.toolCalls
                                    writeIORef pendingRef (map CompletedTool results)
                                    cancelledAfter <-
                                        isCancelled config.loopCancel
                                    if cancelledAfter
                                        then finish state ResponseCommitted
                                            (Left (LoopCancelled results))
                                        else do
                                            steering <- config.loopReadSteering
                                            let continuation =
                                                    map CompletedTool results
                                                        <> steering
                                            if not (null continuation)
                                                then go
                                                    state
                                                    ResponseCommitted
                                                    (Just turn.responseId)
                                                    nextTurnsUsed
                                                    continuation
                                                    (length steering)
                                                    (Just turn)
                                                    usageAcc'
                                                    0
                                                else if hasVisibleAssistantText
                                                    turn.assistantText
                                                    then finish state ResponseCommitted $
                                                        Right LoopResult
                                                            { finalResponseId =
                                                                turn.responseId
                                                            , finalText =
                                                                turn.assistantText
                                                            , turnsUsed =
                                                                nextTurnsUsed
                                                            , tokenUsage = usageAcc'
                                                            }
                                                    else if emptyContinuations
                                                            >= maxEmptyContinuations
                                                        then do
                                                            config.loopOnEvent
                                                                (WarningRaised
                                                                    emptyContinuationWarning)
                                                            finish state ResponseCommitted $
                                                                Right LoopResult
                                                                    { finalResponseId =
                                                                        turn.responseId
                                                                    , finalText =
                                                                        turn.assistantText
                                                                    , turnsUsed =
                                                                        nextTurnsUsed
                                                                    , tokenUsage = usageAcc'
                                                                    }
                                                        else
                                                            go
                                                                state
                                                                ResponseCommitted
                                                                (Just turn.responseId)
                                                                nextTurnsUsed
                                                                []
                                                                0
                                                                (Just turn)
                                                                usageAcc'
                                                                (emptyContinuations + 1)
            run =
                go initialState NoResponseCommitted previousResponseId 0
                    (firstInputs <> initialSteering)
                    (length initialSteering)
                    Nothing
                    emptyTokenUsage
                    0
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
                            (\(index, value) -> IntMap.insert index value acc)
                            result)
                    completed
                    batchResults
        cancelled <- isCancelled config.loopCancel
        if cancelled
            then pure (IntMap.elems completed')
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
