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
import Agent.ReasoningEffort
    ( ReasoningEffort(..)
    , grokReasoningEffort
    , grokReasoningEffortText
    , parseReasoningEffort
    )
import Agent.Responses.Types
import Agent.XAI.Options (ClientOptions(..))
import qualified Data.Aeson.Key as Key
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
    UnknownResponseTool tagged
        | tagged.tag == responseToolTypeText ToolXSearch -> True
    _ -> False

hostedWebSearchTool :: ResponseTool
hostedWebSearchTool = knownResponseTool ToolWebSearch KeyMap.empty

hostedXSearchTool :: ResponseTool
hostedXSearchTool = knownResponseTool ToolXSearch KeyMap.empty

xaiReasoningEffort :: Maybe Text -> Text
xaiReasoningEffort value =
    grokReasoningEffortText . grokReasoningEffort $
        case value of
            Nothing -> EffortHigh
            Just "minimal" -> EffortLow
            Just raw ->
                either (const EffortHigh) id (parseReasoningEffort raw)

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
normalizeInputItem =
    stripItemStatus . \case
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

-- The Grok proxy rejects OpenAI item @status@ as an unknown parameter
-- (@input[n].status@). Strip it from every input item at the wire boundary.
stripItemStatus :: ResponseItem -> ResponseItem
stripItemStatus = \case
    MessageItem message ->
        MessageItem message
            { status = Nothing
            , extraFields = withoutStatusFields message.extraFields
            }
    FunctionCallItem call ->
        FunctionCallItem call
            { status = Nothing
            , extraFields = withoutStatusFields call.extraFields
            }
    FunctionCallOutputItem output ->
        FunctionCallOutputItem output
            { status = Nothing
            , extraFields = withoutStatusFields output.extraFields
            }
    CustomToolCallItem call ->
        CustomToolCallItem call
            { status = Nothing
            , extraFields = withoutStatusFields call.extraFields
            }
    CustomToolCallOutputItem output ->
        CustomToolCallOutputItem output
            { status = Nothing
            , extraFields = withoutStatusFields output.extraFields
            }
    ReasoningItemValue item ->
        ReasoningItemValue item
            { status = Nothing
            , extraFields = withoutStatusFields item.extraFields
            }
    LocalShellCallItem item ->
        LocalShellCallItem item
            { status = Nothing
            , extraFields = withoutStatusFields item.extraFields
            }
    ToolSearchCallItem item ->
        ToolSearchCallItem item
            { status = Nothing
            , extraFields = withoutStatusFields item.extraFields
            }
    ToolSearchOutputItem item ->
        ToolSearchOutputItem item
            { status = Nothing
            , extraFields = withoutStatusFields item.extraFields
            }
    WebSearchCallItem item ->
        WebSearchCallItem item
            { status = Nothing
            , extraFields = withoutStatusFields item.extraFields
            }
    ImageGenerationCallItem item ->
        ImageGenerationCallItem item
            { status = Nothing
            , extraFields = withoutStatusFields item.extraFields
            }
    KnownResponseItem itemType tagged ->
        KnownResponseItem itemType tagged
            { fields = withoutStatusFields tagged.fields
            }
    UnknownResponseItem tagged ->
        UnknownResponseItem tagged
            { fields = withoutStatusFields tagged.fields
            }
    item -> item

withoutStatusFields = KeyMap.delete (Key.fromText "status")

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
