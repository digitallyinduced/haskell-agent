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
import qualified Data.Maybe as Maybe
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
    withHostedXSearch $
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
                    , passthrough = Nothing
                    , extraFields = KeyMap.empty
                    }
                ]
        _ -> []

    xaiTool tool = case tool of
        FunctionToolValue {} -> Just tool
        KnownResponseTool ToolWebSearch _ -> Just hostedWebSearchTool
        KnownResponseTool ToolXSearch _ -> Just hostedXSearchTool
        KnownResponseTool ToolComputer _ -> Nothing
        _ -> Just tool

-- | Grok Build always splices hosted @x_search@ onto grok-4.6 Responses
-- requests. Keep a single empty-fields entry even when the caller omitted
-- tools or already listed web search.
withHostedXSearch :: ResponseCreateParams -> ResponseCreateParams
withHostedXSearch ResponseCreateParams { tools = existing, .. } =
    ResponseCreateParams
        { tools = Just (current <> extra)
        , ..
        }
  where
    current = Maybe.fromMaybe [] existing
    extra
        | any isHostedXSearch current = []
        | otherwise = [hostedXSearchTool]

isHostedXSearch :: ResponseTool -> Bool
isHostedXSearch = \case
    KnownResponseTool ToolXSearch _ -> True
    UnknownResponseTool tagged | tagged.tag == "x_search" -> True
    _ -> False

hostedWebSearchTool :: ResponseTool
hostedWebSearchTool =
    KnownResponseTool ToolWebSearch TaggedObject
        { tag = "web_search"
        , fields = KeyMap.empty
        }

hostedXSearchTool :: ResponseTool
hostedXSearchTool =
    KnownResponseTool ToolXSearch TaggedObject
        { tag = "x_search"
        , fields = KeyMap.empty
        }

xaiReasoningEffort :: Maybe Text -> Text
xaiReasoningEffort = \case
    Nothing -> "high"
    Just "low" -> "low"
    Just "none" -> "low"
    Just "minimal" -> "low"
    Just "medium" -> "medium"
    Just "high" -> "high"
    Just "xhigh" -> "xhigh"
    Just "max" -> "max"
    _ -> "high"

requestInputItems :: ResponseCreateParams -> [ResponseItem]
requestInputItems request = case request.input of
    Just (ResponseInputItems items) -> map normalizeInputItem items
    Just (ResponseInputText inputText) ->
        [ MessageItem ResponseMessage
            { messageId = Nothing
            , content = MessageContentText inputText
            , role = RoleUser
            , status = Nothing
            , phase = Nothing
            , passthrough = Nothing
            , extraFields = KeyMap.empty
            }
        ]
    Nothing -> []

-- OpenAI stores inter-agent collaboration messages as the provider-specific
-- @agent_message@ item. Resumed transcripts can retain those items when the
-- user switches to Grok, whose Responses union does not recognize that tag.
-- Preserve the readable collaboration context as an ordinary user message.
normalizeInputItem :: ResponseItem -> ResponseItem
normalizeInputItem = \case
    AgentMessageItem message ->
        MessageItem ResponseMessage
            { messageId = Nothing
            , content = MessageContentParts
                [InputTextPart (agentMessageText message) Nothing KeyMap.empty]
            , role = RoleUser
            , status = Nothing
            , phase = Nothing
            , passthrough = Nothing
            , extraFields = KeyMap.empty
            }
    item -> item

agentMessageText :: ResponseAgentMessage -> Text
agentMessageText message =
    case
        [ text
        | InputTextPart { text } <- message.content
        ]
    of
        texts@(_ : _) -> Text.intercalate "\n" texts
        [] -> case (message.author, message.recipient) of
            (Just author, Just recipient) ->
                "Agent message from " <> author <> " to " <> recipient
            (Just author, Nothing) -> "Agent message from " <> author
            (Nothing, Just recipient) -> "Agent message to " <> recipient
            (Nothing, Nothing) -> "Agent message"
