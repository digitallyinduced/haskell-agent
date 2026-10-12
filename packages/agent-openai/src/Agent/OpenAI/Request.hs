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
import Data.Text (Text)
import qualified Data.Text as Text

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
-- Responses Lite rejects every request whose @reasoning.context@ is not
-- @all_turns@. Callers that assemble 'ResponseCreateParams' directly, rather
-- than through the runtime's provider request builder, leave the context
-- unset, so set it here for both transports too.
--
-- The Codex backend never stores responses and rejects a request whose
-- @store@ is not @false@. The WebSocket envelope already forces it; set it in
-- the shared request so the HTTP body carries it as well.
--
-- Codex only accepts message ids it issued itself (@msg…@). A caller may keep
-- a local identifier for a persisted input message in the same slot; replaying
-- it fails the whole request with @invalid_id_prefix@, so drop such ids.
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
        , reasoning
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
                    if responsesLite
                        then Just False
                        else parallelToolCalls
                , reasoning =
                    if responsesLite
                        then Just (withAllTurnsContext reasoning)
                        else reasoning
                , store = Just False
                , ..
                }
  where
    responsesLite = maybe False isCodexResponsesLiteModel model
    keepOpenAiOrLegacyCheckpoint = \case
        Nothing -> True
        Just origin -> origin == "openai"

withAllTurnsContext :: Maybe ReasoningConfig -> ReasoningConfig
withAllTurnsContext = \case
    Just ReasoningConfig{effort, generateSummary, reasoningMode, summary} ->
        ReasoningConfig
            { context = Just "all_turns"
            , effort
            , generateSummary
            , reasoningMode
            , summary
            }
    Nothing ->
        ReasoningConfig
            { context = Just "all_turns"
            , effort = Nothing
            , generateSummary = Nothing
            , reasoningMode = Nothing
            , summary = Nothing
            }

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
                (providerMessageId itemId)
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

providerMessageId :: Maybe Text -> Maybe Text
providerMessageId = \case
    Just itemId | "msg" `Text.isPrefixOf` itemId -> Just itemId
    _ -> Nothing

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
