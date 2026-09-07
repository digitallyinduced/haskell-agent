-- | Request normalization for the ChatGPT Codex transport.
module Agent.OpenAI.Request
    ( sanitizeCodexRequest
    ) where

import Agent.OpenAI.ModelMetadata (isCodexResponsesLiteModel)
import Agent.Responses.Request
    ( filterRequestCompactionCheckpointsByOrigin
    , stripLocalCompactionMarker
    , stripReplayedInputStatus
    )
import Agent.Responses.Types

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
--
-- Replayed transcript items must not carry their provider lifecycle @status@
-- either: Codex never sends it, and Responses Lite rejects it as an unknown
-- parameter on reasoning items. 'withRequestInput' already drops it for every
-- backend; repeating that here keeps the Codex wire contract independent of
-- how a caller assembled the request.
sanitizeCodexRequest :: ResponseCreateParams -> ResponseCreateParams
sanitizeCodexRequest ResponseCreateParams
        { promptCacheRetention = _
        , input
        , parallelToolCalls
        , ..
        } =
    stripLocalCompactionMarker $
        filterRequestCompactionCheckpointsByOrigin
            keepOpenAiOrLegacyCheckpoint $
            ResponseCreateParams
                { promptCacheRetention = Nothing
                , input =
                    fmap
                        (stripReplayedInputStatus . stripContentItemKindsInput)
                        input
                , parallelToolCalls =
                    if maybe False isCodexResponsesLiteModel model
                        then Just False
                        else parallelToolCalls
                , ..
                }
  where
    keepOpenAiOrLegacyCheckpoint = \case
        Nothing -> True
        Just origin -> origin == "openai"

stripContentItemKindsInput :: ResponseInput -> ResponseInput
stripContentItemKindsInput = \case
    ResponseInputItems items ->
        ResponseInputItems (map stripContentItemKindsItem items)
    other -> other

stripContentItemKindsItem :: ResponseItem -> ResponseItem
stripContentItemKindsItem = \case
    MessageItem (ResponseMessage itemId content role status phase passthrough) ->
        MessageItem
            (ResponseMessage
                itemId
                content
                role
                status
                phase
                (stripItemPassthrough passthrough))
    AgentMessageItem (ResponseAgentMessage itemId author recipient content passthrough) ->
        AgentMessageItem
            (ResponseAgentMessage
                itemId
                author
                recipient
                content
                (stripItemPassthrough passthrough))
    other -> other

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
                }
            then Nothing
            else Just cleaned
