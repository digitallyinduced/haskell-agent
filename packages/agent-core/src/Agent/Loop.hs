-- | Provider-neutral agent loop: submit a user turn, dispatch tool calls,
-- feed results back, repeat until the model answers in text or hits a cap.
--
-- Transports close over model, instructions, and tool schemas. This module
-- only sees 'ToolCall' / 'ToolCallResult' and a 'Backend' callback.
module Agent.Loop
    ( Backend(..)
    , ImageAttachment(..)
    , LoopConfig(..)
    , LoopEvent(..)
    , LoopError(..)
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
    ) where

import Agent.Cancel (CancelFlag, isCancelled, waitCancel)
import Agent.Error (ApiError)
import Agent.InterAgentMessage (InterAgentMessage)
import Agent.Responses.Types
    ( Response
    , ResponseCreateParams
    )
import Agent.ToolDispatch
    ( ToolCall(..)
    , ToolCallResult(..)
    , ToolDispatchConfig(..)
    , ToolRuntime(..)
    )
import Agent.Tools.Types
    ( ToolExecutionPolicy(..)
    , ToolRegistry
    , dispatchRegisteredToolCall
    , toolExecutionPolicyFor
    )
import Control.Concurrent.Async (mapConcurrently, race)
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, withMVar)
import Control.Exception.Safe (SomeException, displayException, tryAny)
import Data.Aeson (FromJSON(..), ToJSON(..), object, withObject, (.:), (.:?), (.!=), (.=))
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
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

data TurnInput
    = UserMessage Text
    | AgentMessage InterAgentMessage
    | UserMultimodal
        { userText :: !Text
        , userImages :: ![ImageAttachment]
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

emptyTurnOutput :: Text -> [ToolCall] -> Maybe Text -> TurnOutput
emptyTurnOutput responseId toolCalls assistantText = TurnOutput
    { responseId
    , toolCalls
    , assistantText
    , tokenUsage = emptyTokenUsage
    }

newtype Backend = Backend
    { submitTurn
        :: Maybe Text
        -> [TurnInput]
        -> (LoopEvent -> IO ())
        -> IO (Either ApiError TurnOutput)
    }

data LoopEvent
    = TextDelta Text
    | ReasoningDelta Text
    -- | Ephemeral transport/tool activity for the live CLI status line.
    | ActivityUpdated Text
    | TurnStarted
    | TurnFinished TurnOutput
    | ToolStarted ToolCall
    -- | Latest accumulated output snapshot for an in-flight tool call.
    | ToolOutputUpdated Text Text
    | ToolFinished ToolCallResult
    deriving (Eq, Show)

data LoopConfig = LoopConfig
    { loopBackend :: !Backend
      -- | Tools callable directly by the parent model.
    , loopTools :: !ToolRegistry
      -- | Optional broader registry for calls made from inside tools such as
      -- run_haskell_program. This lets a harness hide direct shell access
      -- while retaining shell_command behind the programmatic boundary.
    , loopNestedTools :: !(Maybe ToolRegistry)
    , loopDispatch :: !ToolDispatchConfig
    , loopMaxTurns :: !Int
    , loopOnEvent :: !(LoopEvent -> IO ())
    -- | 'Left' denies with that tool-output message; 'Right True' runs the
    -- tool; 'Right False' uses the usual user-rejection string.
    , loopApprove :: !(ToolCall -> IO (Either Text Bool))
      -- | Optional approval callback paired with 'loopNestedTools'.
    , loopNestedApprove
        :: !(Maybe (ToolCall -> IO (Either Text Bool)))
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
    , toolDispatchRuntime = Nothing
    , toolDispatchCallResponses = Nothing
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
    tryAny (runLoopInputsUnsafe config previousResponseId firstInputs) >>= \case
        Left exception ->
            pure (Left (LoopUnexpected (exceptionSummary exception)))
        Right result ->
            pure result

exceptionSummary :: SomeException -> Text
exceptionSummary =
    fst
        . Text.breakOn "\nHasCallStack backtrace:"
        . Text.pack
        . displayException

runLoopInputsUnsafe
    :: LoopConfig
    -> Maybe Text
    -> [TurnInput]
    -> IO (Either LoopError LoopResult)
runLoopInputsUnsafe config0 previousResponseId firstInputs = do
    -- Parallel-safe tool batches run with mapConcurrently. Serialize onEvent
    -- so a printer (hPutStrLn on String is not atomic) cannot interleave
    -- characters.
    eventLock <- newMVar ()
    responseCache <- newMVar Map.empty
    let config = config0
            { loopOnEvent = \event ->
                withMVar eventLock \_ -> config0.loopOnEvent event
            , loopDispatch = config0.loopDispatch
                { toolDispatchCallResponses =
                    memoizedResponseCaller responseCache
                        <$> config0.loopDispatch.toolDispatchCallResponses
                }
            }
        go prev turnsUsed inputs lastOutput usageAcc
            | turnsUsed >= config.loopMaxTurns =
                pure $ case lastOutput of
                    Just turn -> Left (LoopMaxTurns turn)
                    Nothing -> Left LoopNoResponseId
            | otherwise = do
                cancelled <- isCancelled config.loopCancel
                if cancelled
                    then pure (Left (LoopCancelled []))
                    else do
                        config.loopOnEvent TurnStarted
                        -- Race the model call against cancel so Ctrl-C / Esc
                        -- can stop reasoning mid-stream, not only between tools.
                        raced <- race
                            (waitCancel config.loopCancel)
                            (config.loopBackend.submitTurn prev inputs config.loopOnEvent)
                        case raced of
                            Left () ->
                                pure (Left (LoopCancelled []))
                            Right (Left err) ->
                                pure (Left (LoopTransport err))
                            Right (Right turn)
                                | Text.null turn.responseId ->
                                    pure (Left LoopNoResponseId)
                                | otherwise -> do
                                    -- A cancel that landed during submitTurn
                                    -- after the race chose Right still counts.
                                    cancelledMid <- isCancelled config.loopCancel
                                    let usageAcc' = addTokenUsage usageAcc turn.tokenUsage
                                    if cancelledMid
                                        then pure (Left (LoopCancelled []))
                                        else do
                                            config.loopOnEvent (TurnFinished turn)
                                            let nextTurnsUsed = turnsUsed + 1
                                            if null turn.toolCalls
                                                then pure $ Right LoopResult
                                                    { finalResponseId = turn.responseId
                                                    , finalText = turn.assistantText
                                                    , turnsUsed = nextTurnsUsed
                                                    , tokenUsage = usageAcc'
                                                    }
                                                else do
                                                    results <- runToolCalls config turn.toolCalls
                                                    cancelledAfter <- isCancelled config.loopCancel
                                                    if cancelledAfter
                                                        then pure (Left (LoopCancelled results))
                                                        else go (Just turn.responseId) nextTurnsUsed
                                                            (map CompletedTool results) (Just turn) usageAcc'
    go previousResponseId 0 firstInputs Nothing emptyTokenUsage

-- | Reuse successful isolated model calls while one parent user turn is
-- running. A repaired run_haskell_program invocation often submits the same
-- requests again; replaying their complete lossless Responses avoids duplicate
-- provider work without retaining data across user turns. Failures are not
-- cached, so transient provider errors remain retryable.
memoizedResponseCaller
    :: MVar (Map.Map ByteString Response)
    -> ([ResponseCreateParams] -> IO [Either Text Response])
    -> [ResponseCreateParams]
    -> IO [Either Text Response]
memoizedResponseCaller cache invoke requests =
    modifyMVar cache \cached -> do
        let keyed = [(responseRequestKey request, request) | request <- requests]
            missing = uniqueMissing cached keyed
        fetched <- if null missing
            then pure []
            else invoke (map snd missing)
        let fetchedByKey = Map.fromList
                [ (key, result)
                | ((key, _), result) <-
                    zip missing
                        (fetched
                            <> repeat
                                (Left
                                    "Nested LLM bridge failed: missing result"))
                ]
            newlyCached = Map.fromList
                [ (key, response)
                | (key, Right response) <- Map.toList fetchedByKey
                ]
            cached' = Map.union cached newlyCached
            resultFor key =
                case Map.lookup key cached of
                    Just response -> Right response
                    Nothing -> fromMaybe
                        (Left "Nested LLM bridge failed: missing result")
                        (Map.lookup key fetchedByKey)
        pure (cached', map (resultFor . fst) keyed)
  where
    uniqueMissing cached =
        reverse . snd . foldl' collect (Map.empty, [])
      where
        collect (seen, values) pair@(key, _)
            | Map.member key cached || Map.member key seen =
                (seen, values)
            | otherwise =
                (Map.insert key () seen, pair : values)

responseRequestKey :: ResponseCreateParams -> ByteString
responseRequestKey =
    LBS.toStrict . Aeson.encode

-- | Preserve model order around stateful tools while retaining concurrency
-- for maximal consecutive runs of explicitly parallel-safe calls.
runToolCalls :: LoopConfig -> [ToolCall] -> IO [ToolCallResult]
runToolCalls config = go
  where
    go [] = pure []
    go calls@(call : rest) =
        case toolExecutionPolicyFor config.loopTools call of
            ParallelSafe -> do
                let (batch, remaining) =
                        span
                            ((== ParallelSafe)
                                . toolExecutionPolicyFor config.loopTools)
                            calls
                prepared <- traverse (prepareToolCall config) batch
                batchResults <-
                    mapConcurrently (runPreparedToolCall config) prepared
                continue batchResults remaining
            TurnSequential -> do
                result <- runOne config call
                continue [result] rest

    continue completed remaining = do
        cancelled <- isCancelled config.loopCancel
        if cancelled
            then pure completed
            else (completed <>) <$> go remaining

data ToolApproval
    = ToolApprovalDenied !Text
    | ToolApprovalRejected
    | ToolApprovalGranted

data PreparedToolCall =
    PreparedToolCall !ToolCall !ToolApproval

runOne :: LoopConfig -> ToolCall -> IO ToolCallResult
runOne config call =
    prepareToolCall config call >>= runPreparedToolCall config

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
    -> IO ToolCallResult
runPreparedToolCall config (PreparedToolCall call approval) = do
    config.loopOnEvent (ToolStarted call)
    let nestedConfig = config
            { loopTools =
                fromMaybe config.loopTools config.loopNestedTools
            , loopNestedTools = Nothing
            , loopApprove =
                fromMaybe config.loopApprove config.loopNestedApprove
            , loopNestedApprove = Nothing
            }
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
                    , toolDispatchRuntime = Just ToolRuntime
                        { invokeNestedTool = runOne nestedConfig
                        , invokeNestedTools = runToolCalls nestedConfig
                        , invokeNestedResponses =
                            case config.loopDispatch.toolDispatchCallResponses of
                                Just invoke -> invoke
                                Nothing -> \requests ->
                                    pure (replicate (length requests)
                                        (Left
                                            "callLLM is unavailable in this agent runtime"))
                        }
                    }
                config.loopTools
                call
    config.loopOnEvent (ToolFinished result)
    pure result
