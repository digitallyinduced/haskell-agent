-- | Request normalization for the ChatGPT Codex transport.
module Agent.OpenAI.Request
    ( sanitizeCodexRequest
    ) where

import Agent.OpenAI.ModelMetadata (isCodexResponsesLiteModel)
import Agent.Responses.Types
import Agent.Json
    ( Extensions
    , deleteExtension
    , emptyExtensions
    , extensionsToList
    , insertExtension
    , lookupExtension
    , rawJsonBytes
    )
import qualified Agent.Json.Decoder as JsonDecoder
import qualified Agent.Json.Encoder as JsonEncoder

-- | Keep Codex-incompatible fields out of the serialized request.
--
-- Retention is provider-managed for this transport. Sending a retained value
-- explicitly is model-dependent and can make an otherwise valid request fail
-- after the provider routes a response chain to a different model.
--
-- @content_item_kinds@ is a local prefix marker used to keep Responses Lite
-- instructions attached across request rebuilds. Codex only sends it when a
-- client feature flag is on; the current Responses Lite endpoint rejects it as
-- an unknown parameter, so strip it at the wire boundary and leave the
-- in-memory request params unchanged.
--
-- Responses Lite also requires @parallel_tool_calls=false@. Compaction and
-- other request rebuilds can flip that flag back to true, so restore the
-- Lite contract here for both HTTP and WebSocket.
sanitizeCodexRequest :: ResponseCreateParams -> ResponseCreateParams
sanitizeCodexRequest ResponseCreateParams
        { promptCacheRetention = _
        , input
        , parallelToolCalls
        , ..
        } =
    ResponseCreateParams
        { promptCacheRetention = Nothing
        , input = fmap stripContentItemKindsInput input
        , parallelToolCalls =
            if maybe False isCodexResponsesLiteModel model
                then Just False
                else parallelToolCalls
        , ..
        }

stripContentItemKindsInput :: ResponseInput -> ResponseInput
stripContentItemKindsInput = \case
    ResponseInputItems items ->
        ResponseInputItems (map stripContentItemKindsItem items)
    other -> other

stripContentItemKindsItem :: ResponseItem -> ResponseItem
stripContentItemKindsItem = \case
    MessageItem message ->
        MessageItem message
            { passthrough = stripItemPassthrough message.passthrough
            , extraFields = stripContentItemKindsFields message.extraFields
            }
    AgentMessageItem message ->
        AgentMessageItem message
            { passthrough = stripItemPassthrough message.passthrough
            , extraFields = stripContentItemKindsFields message.extraFields
            }
    FunctionCallItem value ->
        FunctionCallItem value
            { extraFields = stripContentItemKindsFields value.extraFields }
    FunctionCallOutputItem value ->
        FunctionCallOutputItem value
            { extraFields = stripContentItemKindsFields value.extraFields }
    CustomToolCallItem value ->
        CustomToolCallItem value
            { extraFields = stripContentItemKindsFields value.extraFields }
    CustomToolCallOutputItem value ->
        CustomToolCallOutputItem value
            { extraFields = stripContentItemKindsFields value.extraFields }
    ReasoningItemValue value ->
        ReasoningItemValue value
            { extraFields = stripContentItemKindsFields value.extraFields }
    ItemReferenceValue value ->
        ItemReferenceValue value
            { extraFields = stripContentItemKindsFields value.extraFields }
    AdditionalToolsItemValue value ->
        AdditionalToolsItemValue value
            { extraFields = stripContentItemKindsFields value.extraFields }
    LocalShellCallItem value ->
        LocalShellCallItem value
            { extraFields = stripContentItemKindsFields value.extraFields }
    ToolSearchCallItem value ->
        ToolSearchCallItem value
            { extraFields = stripContentItemKindsFields value.extraFields }
    ToolSearchOutputItem value ->
        ToolSearchOutputItem value
            { extraFields = stripContentItemKindsFields value.extraFields }
    WebSearchCallItem value ->
        WebSearchCallItem value
            { extraFields = stripContentItemKindsFields value.extraFields }
    ImageGenerationCallItem value ->
        ImageGenerationCallItem value
            { extraFields = stripContentItemKindsFields value.extraFields }
    CompactionItemValue value ->
        CompactionItemValue value
            { extraFields = stripContentItemKindsFields value.extraFields }
    CompactionTriggerItemValue value ->
        CompactionTriggerItemValue value
            { extraFields = stripContentItemKindsFields value.extraFields }
    ContextCompactionItemValue value ->
        ContextCompactionItemValue value
            { extraFields = stripContentItemKindsFields value.extraFields }
    KnownResponseItem itemType tagged ->
        KnownResponseItem itemType tagged
            { fields = stripContentItemKindsFields tagged.fields }
    UnknownResponseItem tagged ->
        UnknownResponseItem tagged
            { fields = stripContentItemKindsFields tagged.fields }

stripItemPassthrough
    :: Maybe InternalChatMetadata
    -> Maybe InternalChatMetadata
stripItemPassthrough = \case
    Nothing -> Nothing
    Just metadata ->
        let cleaned = metadata { contentItemKinds = Nothing }
        in if cleaned == InternalChatMetadata
                { turnId = Nothing
                , createTime = Nothing
                , contentItemKinds = Nothing
                , executedToolCalls = Nothing
                , extraFields = emptyExtensions
                }
            then Nothing
            else Just cleaned

stripContentItemKindsFields :: Extensions -> Extensions
stripContentItemKindsFields fields =
    case lookupExtension passthroughKey fields >>= decodeExtensions of
        Just metadata ->
            let cleaned = deleteExtension contentItemKindsKey metadata
            in if null (extensionsToList cleaned)
                then deleteExtension passthroughKey fields
                else insertExtension
                    passthroughKey
                    (encodeExtensions cleaned)
                    fields
        _ -> fields
  where
    passthroughKey = "internal_chat_message_metadata_passthrough"
    contentItemKindsKey = "content_item_kinds"

decodeExtensions raw =
    either (const Nothing) Just
        (JsonDecoder.decode
            (JsonDecoder.objectFields JsonDecoder.extensionFields)
            (rawJsonBytes raw))

encodeExtensions =
    validatedRaw
        . JsonEncoder.encode (JsonEncoder.objectWithExtensions id [])

validatedRaw bytes =
    case JsonDecoder.validateRawJson bytes of
        Right raw -> raw
        Left err -> error ("impossible invalid extension encoding: " <> show err)
