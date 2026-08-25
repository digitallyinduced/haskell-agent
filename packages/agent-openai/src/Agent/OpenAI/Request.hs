-- | Request normalization for the ChatGPT Codex transport.
module Agent.OpenAI.Request
    ( sanitizeCodexRequest
    ) where

import Agent.Responses.Types
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap

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
sanitizeCodexRequest :: ResponseCreateParams -> ResponseCreateParams
sanitizeCodexRequest ResponseCreateParams{promptCacheRetention = _, input, ..} =
    ResponseCreateParams
        { promptCacheRetention = Nothing
        , input = fmap stripContentItemKindsInput input
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
            { extraFields = stripContentItemKindsFields message.extraFields }
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
    KnownResponseItem itemType tagged ->
        KnownResponseItem itemType tagged
            { fields = stripContentItemKindsFields tagged.fields }
    UnknownResponseItem tagged ->
        UnknownResponseItem tagged
            { fields = stripContentItemKindsFields tagged.fields }

stripContentItemKindsFields :: Aeson.Object -> Aeson.Object
stripContentItemKindsFields fields =
    case KeyMap.lookup passthroughKey fields of
        Just (Aeson.Object metadata) ->
            let cleaned = KeyMap.delete contentItemKindsKey metadata
            in if KeyMap.null cleaned
                then KeyMap.delete passthroughKey fields
                else KeyMap.insert
                    passthroughKey
                    (Aeson.Object cleaned)
                    fields
        _ -> fields
  where
    passthroughKey =
        Key.fromText "internal_chat_message_metadata_passthrough"
    contentItemKindsKey = Key.fromText "content_item_kinds"
