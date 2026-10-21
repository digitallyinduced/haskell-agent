-- | Versioned local presentation events. These are decoded only by frontends;
-- a display projection must never be used as provider conversation state.
module Agent.Runtime.Session.Events (encodeDisplayEvent, decodeDisplayEvent) where

import Agent.Loop
import Agent.ToolDispatch
import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import Data.Text (Text)

encodeDisplayEvent :: LoopEvent -> Value
encodeDisplayEvent event = object ["version" .= (1 :: Int), "type" .= (kind :: Text), "data" .= payload]
  where
    (kind, payload) = case event of
        TextDelta text -> ("text", toJSON text)
        PlanDelta text -> ("plan", toJSON text)
        ReasoningDelta text -> ("reasoning", toJSON text)
        ActivityUpdated text -> ("activity", toJSON text)
        WarningRaised text -> ("warning", toJSON text)
        ResponseRestarted text -> ("restart", toJSON text)
        ProviderLimitUpdated text warning -> ("limit", object ["text" .= text, "warning" .= warning])
        TurnStarted -> ("turn_start", Null)
        TurnFinished output -> ("turn_finish", outputValue output)
        ToolStarted call -> ("tool_start", callValue call)
        ToolUpdated call -> ("tool_update", callValue call)
        ToolArgumentsUpdated call -> ("tool_arguments", callValue call)
        ToolOutputUpdated name output -> ("tool_output", object ["name" .= name, "output" .= output])
        ToolFinished result -> ("tool_finish", resultValue result)
        ToolRetracted identifier -> ("tool_retract", toJSON identifier)
        ResponseAttemptDiscarded -> ("attempt_discard", Null)
        ResponseAttemptFailed -> ("attempt_fail", Null)
        ModelContextReset -> ("context_reset", Null)
        NativeAgentStarted identifier parent label model -> ("agent_start", object
            ["id" .= identifier, "parent" .= parent, "label" .= label, "model" .= model])
        NativeAgentOutput identifier output -> ("agent_output", object ["id" .= identifier, "output" .= output])
        NativeAgentFinished identifier status -> ("agent_finish", object
            ["id" .= identifier, "status" .= case status of
                NativeAgentRunning -> ("running" :: Text)
                NativeAgentCompleted -> "completed"
                NativeAgentFailed -> "failed"
                NativeAgentCancelled -> "cancelled"])

decodeDisplayEvent :: Value -> Either String LoopEvent
decodeDisplayEvent = parseEither $ withObject "display event" \value -> do
    version <- value .: "version"
    if version /= (1 :: Int) then fail "unsupported display event version" else pure ()
    kind <- value .: "type"
    payload <- value .: "data"
    case (kind :: Text) of
        "text" -> TextDelta <$> parseJSON payload
        "plan" -> PlanDelta <$> parseJSON payload
        "reasoning" -> ReasoningDelta <$> parseJSON payload
        "activity" -> ActivityUpdated <$> parseJSON payload
        "warning" -> WarningRaised <$> parseJSON payload
        "restart" -> ResponseRestarted <$> parseJSON payload
        "limit" -> withObject "provider limit" (\fields -> ProviderLimitUpdated <$> fields .: "text" <*> fields .: "warning") payload
        "turn_start" -> pure TurnStarted
        "turn_finish" -> TurnFinished <$> parseOutput payload
        "tool_start" -> ToolStarted <$> parseCall payload
        "tool_update" -> ToolUpdated <$> parseCall payload
        "tool_arguments" -> ToolArgumentsUpdated <$> parseCall payload
        "tool_output" -> withObject "tool output" (\fields -> ToolOutputUpdated <$> fields .: "name" <*> fields .: "output") payload
        "tool_finish" -> ToolFinished <$> parseResult payload
        "tool_retract" -> ToolRetracted <$> parseJSON payload
        "attempt_discard" -> pure ResponseAttemptDiscarded
        "attempt_fail" -> pure ResponseAttemptFailed
        "context_reset" -> pure ModelContextReset
        "agent_start" -> withObject "agent start" (\fields -> NativeAgentStarted <$> fields .: "id" <*> fields .:? "parent" <*> fields .: "label" <*> fields .:? "model") payload
        "agent_output" -> withObject "agent output" (\fields -> NativeAgentOutput <$> fields .: "id" <*> fields .: "output") payload
        "agent_finish" -> withObject "agent finish" (\fields -> do
            identifier <- fields .: "id"
            status <- fields .: "status" >>= \case
                "running" -> pure NativeAgentRunning
                "completed" -> pure NativeAgentCompleted
                "failed" -> pure NativeAgentFailed
                "cancelled" -> pure NativeAgentCancelled
                (_ :: Text) -> fail "invalid agent status"
            pure (NativeAgentFinished identifier status)) payload
        _ -> fail "unknown display event"

callValue :: ToolCall -> Value
callValue call = object
    [ "id" .= call.callId, "name" .= call.name, "kind" .= kindValue call.callKind
    , "arguments" .= (if call.argumentsEncrypted then "" else call.arguments)
    , "encrypted" .= call.argumentsEncrypted, "async" .= (toolCallMode call == AsyncToolCall) ]

parseCall :: Value -> Parser ToolCall
parseCall = withObject "tool call" \fields -> do
    call <- ToolCall <$> fields .: "id" <*> fields .: "name" <*> fields .: "arguments"
        <*> (fields .: "kind" >>= parseKind) <*> fields .: "encrypted"
    asynchronous <- fields .: "async"
    pure (withToolCallMode (if asynchronous then AsyncToolCall else BlockingToolCall) call)

resultValue :: ToolCallResult -> Value
resultValue result = object
    [ "id" .= result.callId, "output" .= result.output, "kind" .= kindValue result.callKind
    , "async" .= (toolCallResultMode result == AsyncToolCall)
    , "images" .= [object ["url" .= image.imageUrl, "detail" .= image.imageDetail] | image <- toolCallResultImages result]
    , "structured" .= toolCallResultStructured result
    ]

parseResult :: Value -> Parser ToolCallResult
parseResult = withObject "tool result" \fields -> do
    identifier <- fields .: "id"
    output <- fields .: "output"
    kind <- fields .: "kind" >>= parseKind
    asynchronous <- fields .: "async"
    images <- fields .: "images" >>= traverse (withObject "tool image" \image ->
        ToolResultImage <$> image .: "url" <*> image .:? "detail")
    structured <- fields .:? "structured"
    let mode = if asynchronous then AsyncToolCall else BlockingToolCall
    pure $ case structured of
        Nothing -> ToolCallResult identifier output kind mode images Nothing
        Just value -> ToolCallResultWithStructured identifier output kind mode images Nothing value []

kindValue :: ToolCallKind -> Text
kindValue = \case
    FunctionCallKind -> "function"
    CustomCallKind -> "custom"
    ComputerCallKind -> "computer"
    ComputerFunctionCallKind -> "computer_function"
    ToolSearchCallKind -> "tool_search"

parseKind :: Text -> Parser ToolCallKind
parseKind = \case
    "function" -> pure FunctionCallKind
    "custom" -> pure CustomCallKind
    "computer" -> pure ComputerCallKind
    "computer_function" -> pure ComputerFunctionCallKind
    "tool_search" -> pure ToolSearchCallKind
    _ -> fail "unknown tool kind"

usageValue :: TokenUsage -> Value
usageValue usage = object
    ["input" .= usage.inputTokens, "output" .= usage.outputTokens, "cached" .= usage.cachedTokens]

parseUsage :: Value -> Parser TokenUsage
parseUsage = withObject "usage" \fields -> TokenUsage <$> fields .: "input" <*> fields .: "output" <*> fields .: "cached"

outputValue :: TurnOutput -> Value
outputValue output = object
    [ "id" .= output.responseId, "calls" .= map callValue output.toolCalls
    , "text" .= output.assistantText, "usage" .= usageValue output.tokenUsage
    , "contextUsage" .= (usageValue <$> output.contextUsage)
    , "incomplete" .= case output.completion of
        TurnCompleted -> Nothing
        TurnIncomplete reason tokens -> Just (object ["reason" .= reason, "tokens" .= tokens]) ]

parseOutput :: Value -> Parser TurnOutput
parseOutput = withObject "turn output" \fields -> do
    identifier <- fields .: "id"
    calls <- fields .: "calls" >>= traverse parseCall
    text <- fields .:? "text"
    usage <- fields .: "usage" >>= parseUsage
    context <- fields .:? "contextUsage" >>= traverse parseUsage
    incomplete <- fields .:? "incomplete"
    completion <- maybe (pure TurnCompleted)
        (withObject "incomplete response" \value -> TurnIncomplete <$> value .: "reason" <*> value .:? "tokens") incomplete
    pure (TurnOutput identifier calls text usage context Nothing completion)
