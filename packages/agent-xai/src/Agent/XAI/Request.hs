-- | Pure projection from canonical Responses API parameters to the dialect
-- accepted by the xAI Grok subscription proxy.
module Agent.XAI.Request
    ( mapModel
    , buildRequest
    ) where

import Agent.Responses.Request
    ( forceStatelessStreaming
    , mapResponseTools
    , selectConfiguredModel
    )
import Agent.Responses.Types
import Agent.XAI.Options (ClientOptions(..))
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import qualified Data.Text as Text

-- | Apply an exact model override, preserve an existing Grok model name, or
-- use the configured default.
mapModel :: ClientOptions -> Text -> Text
mapModel options model =
    selectConfiguredModel
        options.modelOverrides
        (Text.isPrefixOf "grok")
        options.defaultModel
        (Just model)

-- | Build the typed Responses request sent to xAI.
--
-- The proxy differs from the public Responses API in a few known places:
-- instructions are represented as a leading system message, reasoning is
-- always enabled, ChatGPT-only tool knobs are omitted, and the transcript is
-- never stored server-side.
buildRequest :: ClientOptions -> ResponseCreateParams -> ResponseCreateParams
buildRequest options request =
    mapResponseTools xaiTool $
        forceStatelessStreaming defaultResponseCreateParams
            { model = Just $
                selectConfiguredModel
                    options.modelOverrides
                    (Text.isPrefixOf "grok")
                    options.defaultModel
                    request.model
            , input = Just (ResponseInputItems (systemItems <> requestInputItems request))
            , tools = request.tools
            , reasoning = Just ReasoningConfig
                { context = Nothing
                , effort = Just (xaiReasoningEffort (request.reasoning >>= (.effort)))
                , generateSummary = Nothing
                , reasoningMode = Nothing
                , summary = Just "concise"
                , extraFields = KeyMap.empty
                }
            , include = request.include
            , promptCacheKey = request.promptCacheKey
            }
  where
    systemItems = case request.instructions of
        Just instructions
            | not (Text.null (Text.strip instructions)) ->
                [ MessageItem ResponseMessage
                    { messageId = Nothing
                    , content = MessageContentParts
                        [InputTextPart instructions Nothing KeyMap.empty]
                    , role = RoleSystem
                    , status = Nothing
                    , phase = Nothing
                    , extraFields = KeyMap.empty
                    }
                ]
        _ -> []

    xaiTool tool = case tool of
        FunctionToolValue {} -> Just tool
        KnownResponseTool ToolWebSearch _ -> Just (KnownResponseTool ToolWebSearch TaggedObject
            { tag = "web_search"
            , fields = KeyMap.empty
            })
        KnownResponseTool ToolComputer _ -> Nothing
        _ -> Just tool

xaiReasoningEffort :: Maybe Text -> Text
xaiReasoningEffort = \case
    Nothing -> "high"
    Just "low" -> "low"
    Just "none" -> "low"
    Just "minimal" -> "low"
    Just "medium" -> "medium"
    Just "high" -> "high"
    Just "xhigh" -> "high"
    Just "max" -> "high"
    _ -> "high"

requestInputItems :: ResponseCreateParams -> [ResponseItem]
requestInputItems request = case request.input of
    Just (ResponseInputItems items) -> items
    Just (ResponseInputText inputText) ->
        [ MessageItem ResponseMessage
            { messageId = Nothing
            , content = MessageContentText inputText
            , role = RoleUser
            , status = Nothing
            , phase = Nothing
            , extraFields = KeyMap.empty
            }
        ]
    Nothing -> []
