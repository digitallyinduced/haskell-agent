-- | Safe, isolated model completions requested by an MCP server.
module Agent.CLI.McpSampling
    ( mcpSamplingHandler
    ) where

import Agent.CLI.Btw (BtwBackendFactory)
import Agent.Runtime.Error (formatApiErrorInline)
import Agent.Json (RawJson, rawJsonBytes, rawJsonFromEncoding)
import Agent.Loop
    ( Backend(..), BackendResult(..), TurnOutput(..), initialBackendSnapshot )
import qualified Agent.MCP as MCP
import Agent.Responses.Types
    ( MessageContent(..), ResponseCreateParams(..), ResponseItem(..)
    , ResponseMessage(..), ResponseRole(..), ToolChoice(..)
    , ToolChoiceMode(..), defaultResponseCreateParams )
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Vector as Vector
import Data.Scientific (fromFloatDigits)
import Data.Text (Text)
import qualified Data.Text as Text

-- | Construct a provider-neutral one-shot sampling handler.
--
-- The backend factory is the same fresh/private backend used for other side
-- calls. Request state is never shared with the main turn, tools are disabled,
-- and the completion is not stored by providers that support that option.
mcpSamplingHandler
    :: Text
    -> BtwBackendFactory
    -> MCP.McpSamplingRequest
    -> IO (Either Text MCP.McpSamplingResult)
mcpSamplingHandler activeModel makeBackend request =
    case traverse samplingMessageItem request.samplingMessages of
        Left err -> pure (Left err)
        Right [] -> pure (Left "MCP sampling requires at least one message")
        Right messages -> do
            let params = samplingParams request
                Backend submit = makeBackend params
            submit (initialBackendSnapshot messages) Nothing [] (\_ -> pure ())
                >>= pure . \case
                    Left err ->
                        Left ("MCP sampling failed: " <> formatApiErrorInline err)
                    Right BackendResult{backendOutput = output}
                        | not (null output.toolCalls) ->
                            Left "MCP sampling attempted a tool call"
                        | Just text <- output.assistantText
                        , not (Text.null (Text.strip text)) ->
                            Right MCP.McpSamplingResult
                                { MCP.samplingResultRole = "assistant"
                                , MCP.samplingResultContent =
                                    rawJsonFromEncoding $
                                        Aeson.toEncoding $
                                            Aeson.object
                                                [ "type" .= ("text" :: Text)
                                                , "text" .= text
                                                ]
                                , MCP.samplingResultModel = activeModel
                                , MCP.samplingResultStopReason = Just "endTurn"
                                }
                        | otherwise ->
                            Left "MCP sampling returned an empty response"

samplingParams :: MCP.McpSamplingRequest -> ResponseCreateParams
samplingParams request =
    defaultResponseCreateParams
        { instructions = request.samplingSystemPrompt
        , maxOutputTokens = Just (max 1 request.samplingMaxTokens)
        , previousResponseId = Nothing
        , store = Just False
        , temperature = fromFloatDigits <$> request.samplingTemperature
        , toolChoice = Just (ToolChoiceMode ToolChoiceNone)
        , tools = Just []
        , parallelToolCalls = Just False
        }

samplingMessageItem
    :: MCP.McpSamplingMessage
    -> Either Text ResponseItem
samplingMessageItem message = do
    role <- case message.samplingMessageRole of
        "user" -> Right RoleUser
        "assistant" -> Right RoleAssistant
        other -> Left ("unsupported MCP sampling message role: " <> other)
    text <- samplingContentText message.samplingMessageContent
    pure $ MessageItem ResponseMessage
        { messageId = Nothing
        , content = MessageContentText text
        , role
        , status = Nothing
        , phase = Nothing
        , passthrough = Nothing
        }

samplingContentText :: RawJson -> Either Text Text
samplingContentText raw =
    case Aeson.eitherDecodeStrict' (rawJsonBytes raw) of
        Left err -> Left ("invalid MCP sampling content: " <> Text.pack err)
        Right value -> contentValueText value

contentValueText :: Aeson.Value -> Either Text Text
contentValueText = \case
    Aeson.String text -> Right text
    Aeson.Object object
        | Just (Aeson.String "text") <- KeyMap.lookup "type" object
        , Just (Aeson.String text) <- KeyMap.lookup "text" object ->
            Right text
        | otherwise -> Left "MCP sampling only supports text content"
    Aeson.Array values ->
        Text.intercalate "\n" <$> traverse contentValueText (Vector.toList values)
    _ -> Left "MCP sampling only supports text content"
