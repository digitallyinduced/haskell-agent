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
    , stripLocalCompactionMarker
    )
import Agent.ReasoningEffort
    ( ReasoningEffort(..)
    , parseReasoningEffort
    )
import Agent.XAI.ReasoningEffort
    ( grokReasoningEffort
    , grokReasoningEffortText
    )
import Agent.Responses.Types
import Agent.XAI.Options (ClientOptions(..))
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
    stripLocalCompactionMarker $
        (if options.hostedXSearchEnabled then withHostedXSearch else id) $
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
                    }
                }
  where
    systemItems = case request.instructions of
        Just instructions
            | not (Text.null (Text.strip instructions)) ->
                [ MessageItem ResponseMessage
                    { messageId = Nothing
                    , content = MessageContentParts
                        [InputTextPart instructions Nothing]
                    , role = RoleSystem
                    , status = Nothing
                    , phase = Nothing
                    , passthrough = Nothing
                    }
                ]
        _ -> []

    xaiTool tool = case tool of
        FunctionToolValue {} -> Just tool
        KnownResponseTool ToolWebSearch -> Just hostedWebSearchTool
        KnownResponseTool ToolXSearch -> Just hostedXSearchTool
        KnownResponseTool ToolComputer -> Nothing
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
    KnownResponseTool ToolXSearch -> True
    UnknownResponseTool tagged
        | tagged.tag == responseToolTypeText ToolXSearch -> True
    _ -> False

hostedWebSearchTool :: ResponseTool
hostedWebSearchTool = knownResponseTool ToolWebSearch

hostedXSearchTool :: ResponseTool
hostedXSearchTool = knownResponseTool ToolXSearch

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
                    [InputTextPart (agentMessageText message) Nothing]
                , role = RoleUser
                , status = Nothing
                , phase = Nothing
                , passthrough = Nothing
                }
        item -> item

-- The Grok proxy rejects OpenAI item @status@ as an unknown parameter
-- (@input[n].status@). Strip it from every input item at the wire boundary.
stripItemStatus :: ResponseItem -> ResponseItem
stripItemStatus = \case
    MessageItem (ResponseMessage itemId content role _ phase passthrough) ->
        MessageItem (ResponseMessage itemId content role Nothing phase passthrough)
    FunctionCallItem (FunctionCall itemId callId name namespace provider arguments encryptedArgs _) ->
        FunctionCallItem
            (FunctionCall itemId callId name namespace provider arguments encryptedArgs Nothing)
    FunctionCallOutputItem (FunctionCallOutput itemId callId name namespace provider output _) ->
        FunctionCallOutputItem
            (FunctionCallOutput itemId callId name namespace provider output Nothing)
    CustomToolCallItem (CustomToolCall itemId callId name namespace input _) ->
        CustomToolCallItem (CustomToolCall itemId callId name namespace input Nothing)
    CustomToolCallOutputItem (CustomToolCallOutput itemId callId name output _) ->
        CustomToolCallOutputItem (CustomToolCallOutput itemId callId name output Nothing)
    ReasoningItemValue (ReasoningItem itemId summary content encryptedContent _) ->
        ReasoningItemValue
            (ReasoningItem itemId summary content encryptedContent Nothing)
    LocalShellCallItem (LocalShellCall itemId callId _ action) ->
        LocalShellCallItem (LocalShellCall itemId callId Nothing action)
    ToolSearchCallItem (ToolSearchCall itemId callId _ execution arguments) ->
        ToolSearchCallItem
            (ToolSearchCall itemId callId Nothing execution arguments)
    ToolSearchOutputItem (ToolSearchOutput itemId callId _ execution tools) ->
        ToolSearchOutputItem
            (ToolSearchOutput itemId callId Nothing execution tools)
    WebSearchCallItem (WebSearchCall itemId _ action) ->
        WebSearchCallItem (WebSearchCall itemId Nothing action)
    ImageGenerationCallItem (ImageGenerationCall itemId _ revisedPrompt result) ->
        ImageGenerationCallItem
            (ImageGenerationCall itemId Nothing revisedPrompt result)
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
