-- | Convert completed Responses output into provider-neutral loop values.
-- Continuation classification and sensitive tool-argument handling are shared
-- by the backend and streaming projector.
module Agent.Responses.LoopBackend.Output
    ( responseToTurnOutput
    , responseNeedsLoopContinuation
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
    , providerTelemetry = Nothing
    , completion = case response.status of
        ResponseIncomplete
            | hasContinuableReasoningOnlyOutput response -> TurnCompleted
            | otherwise -> TurnIncomplete
                { incompleteReason =
                    maybe "unknown" (.reason) response.incompleteDetails
                , incompleteReasoningTokens =
                    response.usage
                        >>= (.outputTokensDetails)
                        >>= (.reasoningTokens)
                }
        _ -> TurnCompleted
    }

-- | Whether this successful response requires an empty continuation on its
-- committed response chain.
responseNeedsLoopContinuation :: Response -> Bool
responseNeedsLoopContinuation response = case response.status of
    ResponseCompleted ->
        not (responseHasToolCalls response)
            && not (responseHasVisibleAssistantText response)
    ResponseIncomplete -> hasContinuableReasoningOnlyOutput response
    _ -> False

-- | Whether the transport should retain an incomplete response instead of
-- converting it to an @ApiError@. Partial tool/text output is retained so the
-- committed response can be reported without replaying it, but remains
-- 'TurnIncomplete'. A reasoning-only @max_output_tokens@ stop instead becomes
-- an empty completion so the loop can continue the response chain. Reasons
-- such as @content_filter@ stay transport failures, as do completely empty
-- incomplete responses, so a replay-safe fallback can still run.
hasRecoverableIncompleteOutput :: Response -> Bool
hasRecoverableIncompleteOutput response =
    responseHasToolCalls response
        || responseHasVisibleAssistantText response
        || hasContinuableReasoningOnlyOutput response

-- A reasoning-only max-output stop is an intermediate model sample. Mark it
-- completed at the loop boundary so the core loop continues from its committed
-- response id. Partial text or tool calls remain terminal incomplete output:
-- executing either could act on a truncated response.
hasContinuableReasoningOnlyOutput :: Response -> Bool
hasContinuableReasoningOnlyOutput response =
    not (null response.output)
        && all isReasoningOutput response.output
        && isContinuableIncompleteReason response

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

isContinuableIncompleteReason :: Response -> Bool
isContinuableIncompleteReason response =
    maybe False ((`elem` continuableIncompleteReasons) . (.reason))
        response.incompleteDetails

-- | Incomplete reasons where the model can still produce tools or text on a
-- follow-up sample. Safety/filter stops are not continuable.
continuableIncompleteReasons :: [Text]
continuableIncompleteReasons =
    ["max_output_tokens"]

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
    toolName `elem`
        [ "collaboration.spawn_agent"
        , "collaboration.send_message"
        , "collaboration.followup_task"
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
