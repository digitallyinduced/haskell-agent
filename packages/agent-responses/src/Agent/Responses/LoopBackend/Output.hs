-- | Convert completed Responses output into provider-neutral loop values.
-- Incomplete-output classification and sensitive tool-argument handling are
-- shared by the backend and streaming projector.
module Agent.Responses.LoopBackend.Output
    ( responseToTurnOutput
    , hasRecoverableIncompleteOutput
    , responseTokenUsage
    , responseItemToToolCall
    , completedAsyncToolCall
    , assistantTextFromResponse
    , namespacedToolName
    ) where

import Agent.Loop
    ( TokenUsage(..)
    , TurnCompletion(..)
    , TurnOutput(..)
    , emptyTokenUsage
    )
import Agent.Responses.LoopBackend.Input (isLegacyComputerFunctionCall)
import Agent.Responses.Types
import Agent.ToolDispatch
    ( ToolCall(..)
    , ToolCallKind(..)
    , ToolCallMode(..)
    , canonicalToolName
    , toolCallMode
    , withToolCallMode
    )
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

responseToTurnOutput :: Response -> TurnOutput
responseToTurnOutput response = TurnOutput
    { responseId = response.responseId
    , toolCalls = mapMaybe responseItemToToolCall response.output
    , assistantText = assistantTextFromResponse response
    , tokenUsage = responseTokenUsage response
    , contextUsage = tokenUsageFromResponse . Just <$> response.usage
    , providerTelemetry = Nothing
    , completion = case response.status of
        ResponseIncomplete -> TurnIncomplete
            { incompleteReason =
                maybe "unknown" (.reason) response.incompleteDetails
            , incompleteReasoningTokens =
                response.usage
                    >>= (.outputTokensDetails)
                    >>= (.reasoningTokens)
            }
        _ -> TurnCompleted
    }

-- | Whether the transport should retain an incomplete response instead of
-- converting it to an @ApiError@. Partial tool/text output and reasoning cut
-- off by @max_output_tokens@ are retained so the committed response can be
-- reported without replaying it, but remain 'TurnIncomplete': executing or
-- continuing either could act on a truncated response. Reasons such as
-- @content_filter@ stay transport failures, as do completely empty incomplete
-- responses, so a replay-safe fallback can still run.
hasRecoverableIncompleteOutput :: Response -> Bool
hasRecoverableIncompleteOutput response =
    responseHasToolCalls response
        || responseHasVisibleAssistantText response
        || hasReasoningOnlyLengthStop response

hasReasoningOnlyLengthStop :: Response -> Bool
hasReasoningOnlyLengthStop response =
    not (null response.output)
        && all isReasoningOutput response.output
        && maybe False ((== "max_output_tokens") . (.reason))
            response.incompleteDetails

responseHasToolCalls :: Response -> Bool
responseHasToolCalls =
    not . null . mapMaybe responseItemToToolCall . (.output)

responseHasVisibleAssistantText :: Response -> Bool
responseHasVisibleAssistantText =
    maybe False (not . Text.null . Text.strip) . assistantTextFromResponse

isReasoningOutput :: ResponseItem -> Bool
isReasoningOutput = \case
    ReasoningItemValue{} -> True
    _ -> False

responseTokenUsage :: Response -> TokenUsage
responseTokenUsage response =
    tokenUsageFromResponse response.usage

tokenUsageFromResponse :: Maybe ResponseUsage -> TokenUsage
tokenUsageFromResponse = maybe emptyTokenUsage \usage ->
    TokenUsage
        { inputTokens = usage.inputTokens
        , outputTokens = usage.outputTokens
        , cachedTokens = fromMaybe 0 (usage.inputTokensDetails >>= (.cachedTokens))
        }

responseItemToToolCall :: ResponseItem -> Maybe ToolCall
responseItemToToolCall = \case
    FunctionCallItem call
        | isComputerFunctionCall call ->
            Just $ withToolCallMode (callModeFromField call.async) ToolCall
                { callId = call.callId
                , name = "computer"
                , arguments = call.arguments
                , callKind = ComputerFunctionCallKind
                -- Desktop input may contain typed secrets. Conservatively
                -- redact every reserved computer-function payload.
                , argumentsEncrypted = True
                }
    FunctionCallItem call ->
        let toolName = namespacedToolName call.namespace call.name
        in Just $ withToolCallMode (callModeFromField call.async) ToolCall
            { callId = call.callId
            , name = toolName
            , arguments = call.arguments
            , callKind = FunctionCallKind
            , argumentsEncrypted =
                encryptedCollaborationArguments
                    toolName
                    call.encryptedFunctionArgs
            }
    CustomToolCallItem call ->
        Just $ withToolCallMode (callModeFromField call.async) ToolCall
        { callId = call.callId
        , name = namespacedToolName call.namespace call.name
        , arguments = call.input
        , callKind = CustomCallKind
        , argumentsEncrypted = False
        }
    ComputerCallItem call -> Just ToolCall
        { callId = call.computerCallId
        , name = "computer"
        , arguments = Text.decodeUtf8 (LBS.toStrict (Aeson.encode call))
        , callKind = ComputerCallKind
        , argumentsEncrypted = any isSensitiveComputerAction call.computerActions
        }
    _ -> Nothing

callModeFromField :: Maybe Bool -> ToolCallMode
callModeFromField = \case
    Just True -> AsyncToolCall
    _ -> BlockingToolCall

completedAsyncToolCall :: ResponseStreamEvent -> Maybe ToolCall
completedAsyncToolCall = \case
    ResponseOutputItemDoneEvent { item } -> do
        call <- responseItemToToolCall item
        case toolCallMode call of
            AsyncToolCall -> Just call
            BlockingToolCall -> Nothing
    _ -> Nothing

isComputerFunctionCall :: FunctionCall -> Bool
isComputerFunctionCall call =
    ( call.name == computerFunctionName
        && call.namespace `elem` [Nothing, Just "functions"]
    )
        || isLegacyComputerFunctionCall call

isSensitiveComputerAction :: ComputerAction -> Bool
isSensitiveComputerAction = \case
    TypeAction{} -> True
    KeypressAction{} -> True
    _ -> False

encryptedCollaborationArguments :: Text -> Maybe [Text] -> Bool
encryptedCollaborationArguments toolName encryptedFunctionArgs =
    canonicalToolName toolName `elem`
        [ "spawn_agent"
        , "send_message"
        , "followup_task"
        ]
        && encryptedFunctionArgs /= Just []

namespacedToolName :: Maybe Text -> Text -> Text
namespacedToolName namespace name = case namespace of
    Just value
        | not (Text.null value) ->
            if Text.isSuffixOf "." value || Text.isSuffixOf "::" value
                then value <> name
                else value <> "." <> name
    _ -> name

assistantTextFromResponse :: Response -> Maybe Text
assistantTextFromResponse response = case
    [ value
    | MessageItem message <- response.output
    , message.role == RoleAssistant
    , value <- case message.content of
        MessageContentText text -> [text]
        MessageContentParts parts -> [text | OutputTextPart { text } <- parts]
    ] of
        [] -> Nothing
        values -> Just (Text.intercalate "\n" values)
