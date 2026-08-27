module Agent.Responses.Types.Streaming
    ( ResponseStreamEvent(..)
    , ResponseStreamError(..)
    , CodexRateLimits(..)
    , StreamEventType(..)
    , responseStreamEventType
    , responseStreamEventSequenceNumber
    , streamEventTypeText
    , parseStreamEventWithType
    , responseStreamEventDecoder
    , responseStreamEventDecoderWithType
    , unparsedStreamEventTypeText
    ) where

import Agent.Responses.Types.Common
import Agent.Responses.Types.Items (ResponseItem, responseItemDecoder)
import Agent.Responses.Types.Response (Response, responseDecoder)
import Data.Aeson hiding (TaggedObject)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Hermes as Hermes
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import qualified Data.Text as Text

data StreamEventType
    = EventResponseCreated
    | EventResponseInProgress
    | EventResponseCompleted
    | EventResponseDone
    | EventResponseFailed
    | EventResponseIncomplete
    | EventOutputItemAdded
    | EventOutputItemDone
    | EventContentPartAdded
    | EventContentPartDone
    | EventOutputTextDelta
    | EventOutputTextDone
    | EventRefusalDelta
    | EventRefusalDone
    | EventFunctionCallArgumentsDelta
    | EventFunctionCallArgumentsDone
    | EventFileSearchInProgress
    | EventFileSearchSearching
    | EventFileSearchCompleted
    | EventWebSearchInProgress
    | EventWebSearchSearching
    | EventWebSearchCompleted
    | EventReasoningSummaryPartAdded
    | EventReasoningSummaryPartDone
    | EventReasoningSummaryTextDelta
    | EventReasoningSummaryTextDone
    | EventReasoningTextDelta
    | EventReasoningTextDone
    | EventImageGenerationCompleted
    | EventImageGenerationGenerating
    | EventImageGenerationInProgress
    | EventImageGenerationPartialImage
    | EventMcpCallArgumentsDelta
    | EventMcpCallArgumentsDone
    | EventMcpCallCompleted
    | EventMcpCallFailed
    | EventMcpCallInProgress
    | EventMcpListToolsCompleted
    | EventMcpListToolsFailed
    | EventMcpListToolsInProgress
    | EventCodeInterpreterInProgress
    | EventCodeInterpreterInterpreting
    | EventCodeInterpreterCompleted
    | EventCodeInterpreterCodeDelta
    | EventCodeInterpreterCodeDone
    | EventOutputTextAnnotationAdded
    | EventResponseQueued
    | EventCustomToolInputDelta
    | EventCustomToolInputDone
    | EventError
    | EventAudioDelta
    | EventAudioDone
    | EventAudioTranscriptDelta
    | EventAudioTranscriptDone
    | EventShellCommandAdded
    | EventShellCommandDelta
    | EventShellCommandDone
    | EventShellOutputDelta
    | EventShellOutputDone
    | EventCodexRateLimits
    | EventCodexResponseMetadata
    | EventResponseMetadata
    | EventResponsesApiWebSocketTiming
    | StreamEventUnknown !Text
    deriving stock (Eq, Show)

streamEventTypeText :: StreamEventType -> Text
streamEventTypeText = \case
    EventResponseCreated -> "response.created"
    EventResponseInProgress -> "response.in_progress"
    EventResponseCompleted -> "response.completed"
    EventResponseDone -> "response.done"
    EventResponseFailed -> "response.failed"
    EventResponseIncomplete -> "response.incomplete"
    EventOutputItemAdded -> "response.output_item.added"
    EventOutputItemDone -> "response.output_item.done"
    EventContentPartAdded -> "response.content_part.added"
    EventContentPartDone -> "response.content_part.done"
    EventOutputTextDelta -> "response.output_text.delta"
    EventOutputTextDone -> "response.output_text.done"
    EventRefusalDelta -> "response.refusal.delta"
    EventRefusalDone -> "response.refusal.done"
    EventFunctionCallArgumentsDelta -> "response.function_call_arguments.delta"
    EventFunctionCallArgumentsDone -> "response.function_call_arguments.done"
    EventFileSearchInProgress -> "response.file_search_call.in_progress"
    EventFileSearchSearching -> "response.file_search_call.searching"
    EventFileSearchCompleted -> "response.file_search_call.completed"
    EventWebSearchInProgress -> "response.web_search_call.in_progress"
    EventWebSearchSearching -> "response.web_search_call.searching"
    EventWebSearchCompleted -> "response.web_search_call.completed"
    EventReasoningSummaryPartAdded -> "response.reasoning_summary_part.added"
    EventReasoningSummaryPartDone -> "response.reasoning_summary_part.done"
    EventReasoningSummaryTextDelta -> "response.reasoning_summary_text.delta"
    EventReasoningSummaryTextDone -> "response.reasoning_summary_text.done"
    EventReasoningTextDelta -> "response.reasoning_text.delta"
    EventReasoningTextDone -> "response.reasoning_text.done"
    EventImageGenerationCompleted -> "response.image_generation_call.completed"
    EventImageGenerationGenerating -> "response.image_generation_call.generating"
    EventImageGenerationInProgress -> "response.image_generation_call.in_progress"
    EventImageGenerationPartialImage -> "response.image_generation_call.partial_image"
    EventMcpCallArgumentsDelta -> "response.mcp_call_arguments.delta"
    EventMcpCallArgumentsDone -> "response.mcp_call_arguments.done"
    EventMcpCallCompleted -> "response.mcp_call.completed"
    EventMcpCallFailed -> "response.mcp_call.failed"
    EventMcpCallInProgress -> "response.mcp_call.in_progress"
    EventMcpListToolsCompleted -> "response.mcp_list_tools.completed"
    EventMcpListToolsFailed -> "response.mcp_list_tools.failed"
    EventMcpListToolsInProgress -> "response.mcp_list_tools.in_progress"
    EventCodeInterpreterInProgress -> "response.code_interpreter_call.in_progress"
    EventCodeInterpreterInterpreting -> "response.code_interpreter_call.interpreting"
    EventCodeInterpreterCompleted -> "response.code_interpreter_call.completed"
    EventCodeInterpreterCodeDelta -> "response.code_interpreter_call_code.delta"
    EventCodeInterpreterCodeDone -> "response.code_interpreter_call_code.done"
    EventOutputTextAnnotationAdded -> "response.output_text.annotation.added"
    EventResponseQueued -> "response.queued"
    EventCustomToolInputDelta -> "response.custom_tool_call_input.delta"
    EventCustomToolInputDone -> "response.custom_tool_call_input.done"
    EventError -> "error"
    EventAudioDelta -> "response.audio.delta"
    EventAudioDone -> "response.audio.done"
    EventAudioTranscriptDelta -> "response.audio.transcript.delta"
    EventAudioTranscriptDone -> "response.audio.transcript.done"
    EventShellCommandAdded -> "response.shell_call_command.added"
    EventShellCommandDelta -> "response.shell_call_command.delta"
    EventShellCommandDone -> "response.shell_call_command.done"
    EventShellOutputDelta -> "response.shell_call_output_content.delta"
    EventShellOutputDone -> "response.shell_call_output_content.done"
    EventCodexRateLimits -> "codex.rate_limits"
    EventCodexResponseMetadata -> "codex.response.metadata"
    EventResponseMetadata -> "response.metadata"
    EventResponsesApiWebSocketTiming -> "responsesapi.websocket_timing"
    StreamEventUnknown value -> value

-- | Synthetic event type used when a WebSocket frame cannot be decoded.
-- This is not a provider wire type; the receiver invents it so dropped
-- frames remain visible in the agent loop.
unparsedStreamEventTypeText :: Text
unparsedStreamEventTypeText = "websocket.unparsed_frame"

parseStreamEventType :: Text -> StreamEventType
parseStreamEventType value = case value of
    "response.created" -> EventResponseCreated
    "response.in_progress" -> EventResponseInProgress
    "response.completed" -> EventResponseCompleted
    "response.done" -> EventResponseDone
    "response.failed" -> EventResponseFailed
    "response.incomplete" -> EventResponseIncomplete
    "response.output_item.added" -> EventOutputItemAdded
    "response.output_item.done" -> EventOutputItemDone
    "response.content_part.added" -> EventContentPartAdded
    "response.content_part.done" -> EventContentPartDone
    "response.output_text.delta" -> EventOutputTextDelta
    "response.output_text.done" -> EventOutputTextDone
    "response.refusal.delta" -> EventRefusalDelta
    "response.refusal.done" -> EventRefusalDone
    "response.function_call_arguments.delta" -> EventFunctionCallArgumentsDelta
    "response.function_call_arguments.done" -> EventFunctionCallArgumentsDone
    "response.file_search_call.in_progress" -> EventFileSearchInProgress
    "response.file_search_call.searching" -> EventFileSearchSearching
    "response.file_search_call.completed" -> EventFileSearchCompleted
    "response.web_search_call.in_progress" -> EventWebSearchInProgress
    "response.web_search_call.searching" -> EventWebSearchSearching
    "response.web_search_call.completed" -> EventWebSearchCompleted
    "response.reasoning_summary_part.added" -> EventReasoningSummaryPartAdded
    "response.reasoning_summary_part.done" -> EventReasoningSummaryPartDone
    "response.reasoning_summary_text.delta" -> EventReasoningSummaryTextDelta
    "response.reasoning_summary_text.done" -> EventReasoningSummaryTextDone
    "response.reasoning_text.delta" -> EventReasoningTextDelta
    "response.reasoning_text.done" -> EventReasoningTextDone
    "response.image_generation_call.completed" -> EventImageGenerationCompleted
    "response.image_generation_call.generating" -> EventImageGenerationGenerating
    "response.image_generation_call.in_progress" -> EventImageGenerationInProgress
    "response.image_generation_call.partial_image" -> EventImageGenerationPartialImage
    "response.mcp_call_arguments.delta" -> EventMcpCallArgumentsDelta
    "response.mcp_call_arguments.done" -> EventMcpCallArgumentsDone
    "response.mcp_call.completed" -> EventMcpCallCompleted
    "response.mcp_call.failed" -> EventMcpCallFailed
    "response.mcp_call.in_progress" -> EventMcpCallInProgress
    "response.mcp_list_tools.completed" -> EventMcpListToolsCompleted
    "response.mcp_list_tools.failed" -> EventMcpListToolsFailed
    "response.mcp_list_tools.in_progress" -> EventMcpListToolsInProgress
    "response.code_interpreter_call.in_progress" -> EventCodeInterpreterInProgress
    "response.code_interpreter_call.interpreting" -> EventCodeInterpreterInterpreting
    "response.code_interpreter_call.completed" -> EventCodeInterpreterCompleted
    "response.code_interpreter_call_code.delta" -> EventCodeInterpreterCodeDelta
    "response.code_interpreter_call_code.done" -> EventCodeInterpreterCodeDone
    "response.output_text.annotation.added" -> EventOutputTextAnnotationAdded
    "response.queued" -> EventResponseQueued
    "response.custom_tool_call_input.delta" -> EventCustomToolInputDelta
    "response.custom_tool_call_input.done" -> EventCustomToolInputDone
    "error" -> EventError
    "response.audio.delta" -> EventAudioDelta
    "response.audio.done" -> EventAudioDone
    "response.audio.transcript.delta" -> EventAudioTranscriptDelta
    "response.audio.transcript.done" -> EventAudioTranscriptDone
    "response.shell_call_command.added" -> EventShellCommandAdded
    "response.shell_call_command.delta" -> EventShellCommandDelta
    "response.shell_call_command.done" -> EventShellCommandDone
    "response.shell_call_output_content.delta" -> EventShellOutputDelta
    "response.shell_call_output_content.done" -> EventShellOutputDone
    "codex.rate_limits" -> EventCodexRateLimits
    "codex.response.metadata" -> EventCodexResponseMetadata
    "response.metadata" -> EventResponseMetadata
    "responsesapi.websocket_timing" -> EventResponsesApiWebSocketTiming
    other -> StreamEventUnknown other

data ResponseStreamError = ResponseStreamError
    { errorType   :: !(Maybe Text)
    , code        :: !(Maybe Text)
    , message     :: !Text
    , param       :: !(Maybe Text)
    , retryAfter  :: !(Maybe Int)

    } deriving stock (Eq, Show)

data CodexRateLimits = CodexRateLimits
    { allowed              :: !(Maybe Bool)
    , limitReached         :: !(Maybe Bool)
    , primaryUsedPercent   :: !(Maybe Double)
    , secondaryUsedPercent :: !(Maybe Double)
    } deriving stock (Eq, Show)

instance ToJSON ResponseStreamError where
    toJSON ResponseStreamError { errorType, code, message, param, retryAfter } =
        objectWith
            [ optionalField "type" errorType
            , optionalField "code" code
            , Just (field "message" message)
            , optionalField "param" param
            , optionalField "resets_in_seconds" retryAfter
            ]


data ResponseStreamEvent
    = ResponseCreatedEvent
        { responseValue     :: !Response
        , sequenceNumber    :: !(Maybe Int)

        }
    | ResponseInProgressEvent
        { responseValue     :: !Response
        , sequenceNumber    :: !(Maybe Int)

        }
    | ResponseCompletedEvent
        { responseValue     :: !Response
        , sequenceNumber    :: !(Maybe Int)

        }
    | ResponseDoneEvent
        { responseValue     :: !Response
        , sequenceNumber    :: !(Maybe Int)

        }
    | ResponseFailedEvent
        { responseValue     :: !Response
        , sequenceNumber    :: !(Maybe Int)

        }
    | ResponseIncompleteEvent
        { responseValue     :: !Response
        , sequenceNumber    :: !(Maybe Int)

        }
    | ResponseQueuedEvent
        { responseValue     :: !Response
        , sequenceNumber    :: !(Maybe Int)

        }
    | ResponseOutputItemAddedEvent
        { item             :: !ResponseItem
        , outputIndex      :: !(Maybe Int)
        , sequenceNumber   :: !(Maybe Int)

        }
    | ResponseOutputItemDoneEvent
        { item             :: !ResponseItem
        , outputIndex      :: !(Maybe Int)
        , sequenceNumber   :: !(Maybe Int)

        }
    | ResponseFunctionCallArgumentsDeltaEvent
        { delta             :: !(Maybe Text)
        , streamItemId      :: !(Maybe Text)
        , streamOutputIndex :: !(Maybe Int)
        , sequenceNumber    :: !(Maybe Int)

        }
    | ResponseFunctionCallArgumentsDoneEvent
        { arguments         :: !(Maybe Text)
        , functionName      :: !(Maybe Text)
        , streamItemId      :: !(Maybe Text)
        , streamOutputIndex :: !(Maybe Int)
        , sequenceNumber    :: !(Maybe Int)

        }
    | ResponseCustomToolInputDeltaEvent
        { delta             :: !(Maybe Text)
        , streamItemId      :: !(Maybe Text)
        , streamCallId      :: !(Maybe Text)
        , streamOutputIndex :: !(Maybe Int)
        , sequenceNumber    :: !(Maybe Int)

        }
    | ResponseCustomToolInputDoneEvent
        { inputText         :: !(Maybe Text)
        , streamItemId      :: !(Maybe Text)
        , streamCallId      :: !(Maybe Text)
        , streamOutputIndex :: !(Maybe Int)
        , sequenceNumber    :: !(Maybe Int)

        }
    | ResponseReasoningSummaryPartAddedEvent
        { streamItemId      :: !(Maybe Text)
        , streamOutputIndex :: !(Maybe Int)
        , summaryIndex      :: !(Maybe Int)
        , partValue         :: !(Maybe RawJson)
        , sequenceNumber    :: !(Maybe Int)

        }
    | ResponseReasoningSummaryTextDoneEvent
        { streamItemId      :: !(Maybe Text)
        , streamOutputIndex :: !(Maybe Int)
        , summaryIndex      :: !(Maybe Int)
        , text              :: !(Maybe Text)
        , sequenceNumber    :: !(Maybe Int)

        }
    | ResponseErrorEvent
        { streamError       :: !ResponseStreamError
        , sequenceNumber    :: !(Maybe Int)

        }
    | ResponseNestedErrorEvent
        { streamError       :: !ResponseStreamError
        , sequenceNumber    :: !(Maybe Int)

        }
    | ResponseCodexRateLimitsEvent
        { rateLimits     :: !CodexRateLimits
        , sequenceNumber :: !(Maybe Int)
        }
    | OtherResponseStreamEvent
        { otherEventType    :: !StreamEventType
        , sequenceNumber    :: !(Maybe Int)
        , eventDelta        :: !(Maybe Text)
        , streamItemId      :: !(Maybe Text)
        , streamOutputIndex :: !(Maybe Int)
        , summaryIndex      :: !(Maybe Int)
        }
    deriving stock (Eq, Show)

responseStreamEventType :: ResponseStreamEvent -> StreamEventType
responseStreamEventType = \case
    ResponseCreatedEvent{} -> EventResponseCreated
    ResponseInProgressEvent{} -> EventResponseInProgress
    ResponseCompletedEvent{} -> EventResponseCompleted
    ResponseDoneEvent{} -> EventResponseDone
    ResponseFailedEvent{} -> EventResponseFailed
    ResponseIncompleteEvent{} -> EventResponseIncomplete
    ResponseQueuedEvent{} -> EventResponseQueued
    ResponseOutputItemAddedEvent{} -> EventOutputItemAdded
    ResponseOutputItemDoneEvent{} -> EventOutputItemDone
    ResponseFunctionCallArgumentsDeltaEvent{} ->
        EventFunctionCallArgumentsDelta
    ResponseFunctionCallArgumentsDoneEvent{} ->
        EventFunctionCallArgumentsDone
    ResponseCustomToolInputDeltaEvent{} -> EventCustomToolInputDelta
    ResponseCustomToolInputDoneEvent{} -> EventCustomToolInputDone
    ResponseReasoningSummaryPartAddedEvent{} -> EventReasoningSummaryPartAdded
    ResponseReasoningSummaryTextDoneEvent{} -> EventReasoningSummaryTextDone
    ResponseErrorEvent{} -> EventError
    ResponseNestedErrorEvent{} -> EventError
    ResponseCodexRateLimitsEvent{} -> EventCodexRateLimits
    OtherResponseStreamEvent { otherEventType } -> otherEventType

responseStreamEventSequenceNumber :: ResponseStreamEvent -> Maybe Int
responseStreamEventSequenceNumber event = event.sequenceNumber

instance ToJSON ResponseStreamEvent where
    toJSON = \case
        ResponseCreatedEvent { responseValue, sequenceNumber } ->
            lifecycleEvent "response.created" responseValue sequenceNumber
        ResponseInProgressEvent { responseValue, sequenceNumber } ->
            lifecycleEvent "response.in_progress" responseValue sequenceNumber
        ResponseCompletedEvent { responseValue, sequenceNumber } ->
            lifecycleEvent "response.completed" responseValue sequenceNumber
        ResponseDoneEvent { responseValue, sequenceNumber } ->
            objectWith
                [ Just (field "type" ("response.done" :: Text))
                , optionalField "sequence_number" sequenceNumber
                , Just (field "response" responseValue)
                ]
        ResponseFailedEvent { responseValue, sequenceNumber } ->
            lifecycleEvent "response.failed" responseValue sequenceNumber
        ResponseIncompleteEvent { responseValue, sequenceNumber } ->
            lifecycleEvent "response.incomplete" responseValue sequenceNumber
        ResponseQueuedEvent { responseValue, sequenceNumber } ->
            lifecycleEvent "response.queued" responseValue sequenceNumber
        ResponseOutputItemAddedEvent { item, outputIndex, sequenceNumber } ->
            outputItemEvent "response.output_item.added" item outputIndex sequenceNumber
        ResponseOutputItemDoneEvent { item, outputIndex, sequenceNumber } ->
            outputItemEvent "response.output_item.done" item outputIndex sequenceNumber
        ResponseFunctionCallArgumentsDeltaEvent { delta, streamItemId, streamOutputIndex, sequenceNumber } ->
            indexedItemEvent "response.function_call_arguments.delta"
                streamItemId (Nothing :: Maybe Text) streamOutputIndex sequenceNumber
                [optionalField "delta" delta]
        ResponseFunctionCallArgumentsDoneEvent { arguments, functionName, streamItemId, streamOutputIndex, sequenceNumber } ->
            indexedItemEvent "response.function_call_arguments.done"
                streamItemId (Nothing :: Maybe Text) streamOutputIndex sequenceNumber
                [ optionalField "name" functionName
                , optionalField "arguments" arguments
                ]
        ResponseCustomToolInputDeltaEvent { delta, streamItemId, streamCallId, streamOutputIndex, sequenceNumber } ->
            indexedItemEvent "response.custom_tool_call_input.delta"
                streamItemId streamCallId streamOutputIndex sequenceNumber
                [optionalField "delta" delta]
        ResponseCustomToolInputDoneEvent { inputText, streamItemId, streamCallId, streamOutputIndex, sequenceNumber } ->
            indexedItemEvent "response.custom_tool_call_input.done"
                streamItemId streamCallId streamOutputIndex sequenceNumber
                [optionalField "input" inputText]
        ResponseReasoningSummaryPartAddedEvent { streamItemId, streamOutputIndex, summaryIndex, partValue, sequenceNumber } ->
            indexedItemEvent "response.reasoning_summary_part.added"
                streamItemId (Nothing :: Maybe Text) streamOutputIndex sequenceNumber
                [ optionalField "summary_index" summaryIndex
                , optionalField "part" partValue
                ]
        ResponseReasoningSummaryTextDoneEvent { streamItemId, streamOutputIndex, summaryIndex, text, sequenceNumber } ->
            indexedItemEvent "response.reasoning_summary_text.done"
                streamItemId (Nothing :: Maybe Text) streamOutputIndex sequenceNumber
                [ optionalField "summary_index" summaryIndex
                , optionalField "text" text
                ]
        ResponseErrorEvent { streamError, sequenceNumber } ->
            topLevelErrorEvent streamError sequenceNumber
        ResponseNestedErrorEvent { streamError, sequenceNumber } ->
            objectWith
                [ Just (field "type" ("error" :: Text))
                , optionalField "sequence_number" sequenceNumber
                , Just (field "error" streamError)
                ]
        ResponseCodexRateLimitsEvent { sequenceNumber } ->
            objectWith
                [ Just (field "type" ("codex.rate_limits" :: Text))
                , optionalField "sequence_number" sequenceNumber
                ]
        OtherResponseStreamEvent
            { otherEventType, sequenceNumber, eventDelta, streamItemId
            , streamOutputIndex, summaryIndex } ->
            objectWith
                [ Just (field "type" (streamEventTypeText otherEventType))
                , optionalField "sequence_number" sequenceNumber
                , optionalField "delta" eventDelta
                , optionalField "item_id" streamItemId
                , optionalField "output_index" streamOutputIndex
                , optionalField "summary_index" summaryIndex
                ]
      where
        lifecycleEvent eventType response sequenceNumber = objectWith
            [ Just (field "type" (eventType :: Text))
            , optionalField "sequence_number" sequenceNumber
            , Just (field "response" response)
            ]
        outputItemEvent eventType item outputIndex sequenceNumber = objectWith
            [ Just (field "type" (eventType :: Text))
            , optionalField "sequence_number" sequenceNumber
            , optionalField "output_index" outputIndex
            , Just (field "item" item)
            ]
        indexedItemEvent eventType itemId callId outputIndex sequenceNumber payloadFields =
            objectWith
                ( [ Just (field "type" (eventType :: Text))
                  , optionalField "sequence_number" sequenceNumber
                  , optionalField "item_id" itemId
                  , optionalField "call_id" callId
                  , optionalField "output_index" outputIndex
                  ]
                    <> payloadFields
                )
        topLevelErrorEvent streamError sequenceNumber =
            objectWith
                [ Just (field "type" ("error" :: Text))
                , optionalField "sequence_number" sequenceNumber
                , optionalField "code" streamError.code
                , Just (field "message" streamError.message)
                , optionalField "param" streamError.param
                , optionalField "resets_in_seconds" streamError.retryAfter
                ]


responseStreamEventDecoder :: Hermes.Decoder ResponseStreamEvent
responseStreamEventDecoder =
    Hermes.object do
        wireType <- Hermes.atKey "type" Hermes.text
        Hermes.liftObjectDecoder (decoderForType wireType)

responseStreamEventDecoderWithType
    :: Text
    -> Hermes.Decoder ResponseStreamEvent
responseStreamEventDecoderWithType suppliedType =
    Hermes.object do
        payloadType <- optionalAtKey "type" Hermes.text
        case payloadType of
            Just actual
                | actual /= suppliedType ->
                    fail
                        ( "SSE event type " <> Text.unpack suppliedType
                        <> " disagrees with JSON type " <> Text.unpack actual
                        )
            _ -> Hermes.liftObjectDecoder (decoderForType suppliedType)

-- Retained as a source-compatible name for callers which supplied the SSE
-- discriminator separately.
parseStreamEventWithType
    :: Text
    -> Hermes.Decoder ResponseStreamEvent
parseStreamEventWithType = responseStreamEventDecoderWithType

eventDecoder :: Text -> Hermes.Decoder ResponseStreamEvent
eventDecoder wireType = Hermes.object do
    sequenceNumber <- optionalAtKey "sequence_number" Hermes.int
    case parseStreamEventType wireType of
        EventResponseCreated ->
            lifecycle ResponseCreatedEvent sequenceNumber
        EventResponseInProgress ->
            lifecycle ResponseInProgressEvent sequenceNumber
        EventResponseCompleted ->
            lifecycle ResponseCompletedEvent sequenceNumber
        EventResponseDone ->
            lifecycle ResponseDoneEvent sequenceNumber
        EventResponseFailed ->
            lifecycle ResponseFailedEvent sequenceNumber
        EventResponseIncomplete ->
            lifecycle ResponseIncompleteEvent sequenceNumber
        EventResponseQueued ->
            lifecycle ResponseQueuedEvent sequenceNumber
        EventOutputItemAdded ->
            outputItem ResponseOutputItemAddedEvent sequenceNumber
        EventOutputItemDone ->
            outputItem ResponseOutputItemDoneEvent sequenceNumber
        EventFunctionCallArgumentsDelta ->
            ResponseFunctionCallArgumentsDeltaEvent
                <$> optionalAtKey "delta" Hermes.text
                <*> optionalAtKey "item_id" Hermes.text
                <*> optionalAtKey "output_index" Hermes.int
                <*> pure sequenceNumber
        EventFunctionCallArgumentsDone ->
            ResponseFunctionCallArgumentsDoneEvent
                <$> optionalAtKey "arguments" Hermes.text
                <*> optionalAtKey "name" Hermes.text
                <*> optionalAtKey "item_id" Hermes.text
                <*> optionalAtKey "output_index" Hermes.int
                <*> pure sequenceNumber
        EventCustomToolInputDelta ->
            ResponseCustomToolInputDeltaEvent
                <$> optionalAtKey "delta" Hermes.text
                <*> optionalAtKey "item_id" Hermes.text
                <*> optionalAtKey "call_id" Hermes.text
                <*> optionalAtKey "output_index" Hermes.int
                <*> pure sequenceNumber
        EventCustomToolInputDone ->
            ResponseCustomToolInputDoneEvent
                <$> optionalAtKey "input" Hermes.text
                <*> optionalAtKey "item_id" Hermes.text
                <*> optionalAtKey "call_id" Hermes.text
                <*> optionalAtKey "output_index" Hermes.int
                <*> pure sequenceNumber
        EventReasoningSummaryPartAdded ->
            ResponseReasoningSummaryPartAddedEvent
                <$> optionalAtKey "item_id" Hermes.text
                <*> optionalAtKey "output_index" Hermes.int
                <*> optionalAtKey "summary_index" Hermes.int
                <*> optionalAtKey "part" rawJsonDecoder
                <*> pure sequenceNumber
        EventReasoningSummaryTextDone ->
            ResponseReasoningSummaryTextDoneEvent
                <$> optionalAtKey "item_id" Hermes.text
                <*> optionalAtKey "output_index" Hermes.int
                <*> optionalAtKey "summary_index" Hermes.int
                <*> optionalAtKey "text" Hermes.text
                <*> pure sequenceNumber
        EventError -> do
            nested <- optionalAtKey "error" responseStreamErrorDecoder
            case nested of
                Just streamError -> pure ResponseNestedErrorEvent
                    { streamError
                    , sequenceNumber

                    }
                Nothing -> do
                    streamError <- ResponseStreamError
                        <$> pure Nothing
                        <*> optionalAtKey "code" Hermes.text
                        <*> (maybe "" id
                            <$> optionalAtKey "message" Hermes.text)
                        <*> optionalAtKey "param" Hermes.text
                        <*> optionalAtKey "resets_in_seconds" Hermes.int
                    pure ResponseErrorEvent
                        { streamError
                        , sequenceNumber

                        }
        eventType -> OtherResponseStreamEvent
            <$> pure eventType
            <*> pure sequenceNumber
            <*> optionalAtKey "delta" Hermes.text
            <*> optionalAtKey "item_id" Hermes.text
            <*> optionalAtKey "output_index" Hermes.int
            <*> optionalAtKey "summary_index" Hermes.int
  where
    lifecycle constructor sequenceNumber =
        constructor
            <$> Hermes.atKey "response" responseDecoder
            <*> pure sequenceNumber
    outputItem constructor sequenceNumber =
        constructor
            <$> Hermes.atKey "item" responseItemDecoder
            <*> optionalAtKey "output_index" Hermes.int
            <*> pure sequenceNumber

responseStreamErrorDecoder :: Hermes.Decoder ResponseStreamError
responseStreamErrorDecoder = Hermes.object $
    ResponseStreamError
        <$> optionalAtKey "type" Hermes.text
        <*> optionalAtKey "code" Hermes.text
        <*> (maybe "" id <$> optionalAtKey "message" Hermes.text)
        <*> optionalAtKey "param" Hermes.text
        <*> optionalAtKey "resets_in_seconds" Hermes.int

decoderForType :: Text -> Hermes.Decoder ResponseStreamEvent
decoderForType wireType =
    case parseStreamEventType wireType of
        EventResponseCreated -> eventDecoder wireType
        EventResponseInProgress -> eventDecoder wireType
        EventResponseCompleted -> eventDecoder wireType
        EventResponseDone -> eventDecoder wireType
        EventResponseFailed -> eventDecoder wireType
        EventResponseIncomplete -> eventDecoder wireType
        EventResponseQueued -> eventDecoder wireType
        EventOutputItemAdded -> eventDecoder wireType
        EventOutputItemDone -> eventDecoder wireType
        EventFunctionCallArgumentsDelta -> eventDecoder wireType
        EventFunctionCallArgumentsDone -> eventDecoder wireType
        EventCustomToolInputDelta -> eventDecoder wireType
        EventCustomToolInputDone -> eventDecoder wireType
        EventReasoningSummaryPartAdded -> eventDecoder wireType
        EventReasoningSummaryTextDone -> eventDecoder wireType
        EventError -> eventDecoder wireType
        EventCodexRateLimits -> Hermes.object $
            ResponseCodexRateLimitsEvent
                <$> Hermes.atKey "rate_limits" codexRateLimitsDecoder
                <*> optionalAtKey "sequence_number" Hermes.int
        eventType -> otherEventDecoder eventType

otherEventDecoder :: StreamEventType -> Hermes.Decoder ResponseStreamEvent
otherEventDecoder eventType = Hermes.object $
    OtherResponseStreamEvent
        <$> pure eventType
        <*> optionalAtKey "sequence_number" Hermes.int
        <*> optionalAtKey "delta" Hermes.text
        <*> optionalAtKey "item_id" Hermes.text
        <*> optionalAtKey "output_index" Hermes.int
        <*> optionalAtKey "summary_index" Hermes.int

codexRateLimitsDecoder :: Hermes.Decoder CodexRateLimits
codexRateLimitsDecoder = Hermes.object $
    CodexRateLimits
        <$> optionalAtKey "allowed" Hermes.bool
        <*> optionalAtKey "limit_reached" Hermes.bool
        <*> (fmap (.usedPercent)
            <$> optionalAtKey "primary" rateLimitWindowDecoder)
        <*> (fmap (.usedPercent)
            <$> optionalAtKey "secondary" rateLimitWindowDecoder)

newtype RateLimitWindow = RateLimitWindow { usedPercent :: Double }

rateLimitWindowDecoder :: Hermes.Decoder RateLimitWindow
rateLimitWindowDecoder = Hermes.object $
    RateLimitWindow <$> Hermes.atKey "used_percent" Hermes.double
