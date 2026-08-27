module Agent.Responses.Types.Streaming
    ( ResponseStreamEvent(..)
    , ResponseStreamError(..)
    , StreamEventType(..)
    , responseStreamEventType
    , responseStreamEventSequenceNumber
    , streamEventTypeText
    , parseStreamEventWithType
    , unparsedStreamEventTypeText
    ) where

import Agent.Responses.Types.Common
import Agent.Responses.Types.Items (ResponseItem)
import Data.Aeson hiding (TaggedObject)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (Parser)
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
    , extraFields :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON ResponseStreamError where
    toJSON ResponseStreamError { errorType, code, message, param, retryAfter, extraFields } =
        objectWith extraFields
            [ optionalField "type" errorType
            , optionalField "code" code
            , Just (field "message" message)
            , optionalField "param" param
            , optionalField "resets_in_seconds" retryAfter
            ]

instance FromJSON ResponseStreamError where
    parseJSON = withObject "ResponseStreamError" $ \o -> ResponseStreamError
        <$> o .:? "type"
        <*> o .:? "code"
        -- Some Responses gateways emit code/type without a human-readable
        -- message. Keep the wire shape permissive so those errors can still
        -- be classified instead of aborting event decoding.
        <*> o .:? "message" .!= ""
        <*> o .:? "param"
        <*> o .:? "resets_in_seconds"
        <*> pure (without ["message"] o)

data ResponseStreamEvent
    = ResponseCreatedEvent
        { responseValue     :: !Aeson.Value
        , sequenceNumber    :: !(Maybe Int)
        , eventExtraFields  :: !Aeson.Object
        }
    | ResponseInProgressEvent
        { responseValue     :: !Aeson.Value
        , sequenceNumber    :: !(Maybe Int)
        , eventExtraFields  :: !Aeson.Object
        }
    | ResponseCompletedEvent
        { responseValue     :: !Aeson.Value
        , sequenceNumber    :: !(Maybe Int)
        , eventExtraFields  :: !Aeson.Object
        }
    | ResponseDoneEvent
        { responseValue     :: !Aeson.Value
        , sequenceNumber    :: !(Maybe Int)
        , eventExtraFields  :: !Aeson.Object
        }
    | ResponseFailedEvent
        { responseValue     :: !Aeson.Value
        , sequenceNumber    :: !(Maybe Int)
        , eventExtraFields  :: !Aeson.Object
        }
    | ResponseIncompleteEvent
        { responseValue     :: !Aeson.Value
        , sequenceNumber    :: !(Maybe Int)
        , eventExtraFields  :: !Aeson.Object
        }
    | ResponseQueuedEvent
        { responseValue     :: !Aeson.Value
        , sequenceNumber    :: !(Maybe Int)
        , eventExtraFields  :: !Aeson.Object
        }
    | ResponseOutputItemAddedEvent
        { item             :: !ResponseItem
        , outputIndex      :: !(Maybe Int)
        , sequenceNumber   :: !(Maybe Int)
        , eventExtraFields :: !Aeson.Object
        }
    | ResponseOutputItemDoneEvent
        { item             :: !ResponseItem
        , outputIndex      :: !(Maybe Int)
        , sequenceNumber   :: !(Maybe Int)
        , eventExtraFields :: !Aeson.Object
        }
    | ResponseFunctionCallArgumentsDeltaEvent
        { delta             :: !(Maybe Text)
        , streamItemId      :: !(Maybe Text)
        , streamOutputIndex :: !(Maybe Int)
        , sequenceNumber    :: !(Maybe Int)
        , eventExtraFields  :: !Aeson.Object
        }
    | ResponseFunctionCallArgumentsDoneEvent
        { arguments         :: !(Maybe Text)
        , functionName      :: !(Maybe Text)
        , streamItemId      :: !(Maybe Text)
        , streamOutputIndex :: !(Maybe Int)
        , sequenceNumber    :: !(Maybe Int)
        , eventExtraFields  :: !Aeson.Object
        }
    | ResponseCustomToolInputDeltaEvent
        { delta             :: !(Maybe Text)
        , streamItemId      :: !(Maybe Text)
        , streamCallId      :: !(Maybe Text)
        , streamOutputIndex :: !(Maybe Int)
        , sequenceNumber    :: !(Maybe Int)
        , eventExtraFields  :: !Aeson.Object
        }
    | ResponseCustomToolInputDoneEvent
        { inputText         :: !(Maybe Text)
        , streamItemId      :: !(Maybe Text)
        , streamCallId      :: !(Maybe Text)
        , streamOutputIndex :: !(Maybe Int)
        , sequenceNumber    :: !(Maybe Int)
        , eventExtraFields  :: !Aeson.Object
        }
    | ResponseReasoningSummaryPartAddedEvent
        { streamItemId      :: !(Maybe Text)
        , streamOutputIndex :: !(Maybe Int)
        , summaryIndex      :: !(Maybe Int)
        , partValue         :: !(Maybe Aeson.Value)
        , sequenceNumber    :: !(Maybe Int)
        , eventExtraFields  :: !Aeson.Object
        }
    | ResponseReasoningSummaryTextDoneEvent
        { streamItemId      :: !(Maybe Text)
        , streamOutputIndex :: !(Maybe Int)
        , summaryIndex      :: !(Maybe Int)
        , text              :: !(Maybe Text)
        , sequenceNumber    :: !(Maybe Int)
        , eventExtraFields  :: !Aeson.Object
        }
    | ResponseErrorEvent
        { streamError       :: !ResponseStreamError
        , sequenceNumber    :: !(Maybe Int)
        , eventExtraFields  :: !Aeson.Object
        }
    | ResponseNestedErrorEvent
        { streamError       :: !ResponseStreamError
        , sequenceNumber    :: !(Maybe Int)
        , eventExtraFields  :: !Aeson.Object
        }
    | OtherResponseStreamEvent
        { otherEventType    :: !StreamEventType
        , sequenceNumber    :: !(Maybe Int)
        , eventExtraFields  :: !Aeson.Object
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
    OtherResponseStreamEvent { otherEventType } -> otherEventType

responseStreamEventSequenceNumber :: ResponseStreamEvent -> Maybe Int
responseStreamEventSequenceNumber event = event.sequenceNumber

instance ToJSON ResponseStreamEvent where
    toJSON = \case
        ResponseCreatedEvent { responseValue, sequenceNumber, eventExtraFields } ->
            lifecycleEvent "response.created" responseValue sequenceNumber eventExtraFields
        ResponseInProgressEvent { responseValue, sequenceNumber, eventExtraFields } ->
            lifecycleEvent "response.in_progress" responseValue sequenceNumber eventExtraFields
        ResponseCompletedEvent { responseValue, sequenceNumber, eventExtraFields } ->
            lifecycleEvent "response.completed" responseValue sequenceNumber eventExtraFields
        ResponseDoneEvent { responseValue, sequenceNumber, eventExtraFields } ->
            objectWith eventExtraFields
                [ Just (field "type" ("response.done" :: Text))
                , optionalField "sequence_number" sequenceNumber
                , Just (field "response" responseValue)
                ]
        ResponseFailedEvent { responseValue, sequenceNumber, eventExtraFields } ->
            lifecycleEvent "response.failed" responseValue sequenceNumber eventExtraFields
        ResponseIncompleteEvent { responseValue, sequenceNumber, eventExtraFields } ->
            lifecycleEvent "response.incomplete" responseValue sequenceNumber eventExtraFields
        ResponseQueuedEvent { responseValue, sequenceNumber, eventExtraFields } ->
            lifecycleEvent "response.queued" responseValue sequenceNumber eventExtraFields
        ResponseOutputItemAddedEvent { item, outputIndex, sequenceNumber, eventExtraFields } ->
            outputItemEvent "response.output_item.added" item outputIndex sequenceNumber eventExtraFields
        ResponseOutputItemDoneEvent { item, outputIndex, sequenceNumber, eventExtraFields } ->
            outputItemEvent "response.output_item.done" item outputIndex sequenceNumber eventExtraFields
        ResponseFunctionCallArgumentsDeltaEvent { delta, streamItemId, streamOutputIndex, sequenceNumber, eventExtraFields } ->
            indexedItemEvent "response.function_call_arguments.delta"
                streamItemId (Nothing :: Maybe Text) streamOutputIndex sequenceNumber eventExtraFields
                [optionalField "delta" delta]
        ResponseFunctionCallArgumentsDoneEvent { arguments, functionName, streamItemId, streamOutputIndex, sequenceNumber, eventExtraFields } ->
            indexedItemEvent "response.function_call_arguments.done"
                streamItemId (Nothing :: Maybe Text) streamOutputIndex sequenceNumber eventExtraFields
                [ optionalField "name" functionName
                , optionalField "arguments" arguments
                ]
        ResponseCustomToolInputDeltaEvent { delta, streamItemId, streamCallId, streamOutputIndex, sequenceNumber, eventExtraFields } ->
            indexedItemEvent "response.custom_tool_call_input.delta"
                streamItemId streamCallId streamOutputIndex sequenceNumber eventExtraFields
                [optionalField "delta" delta]
        ResponseCustomToolInputDoneEvent { inputText, streamItemId, streamCallId, streamOutputIndex, sequenceNumber, eventExtraFields } ->
            indexedItemEvent "response.custom_tool_call_input.done"
                streamItemId streamCallId streamOutputIndex sequenceNumber eventExtraFields
                [optionalField "input" inputText]
        ResponseReasoningSummaryPartAddedEvent { streamItemId, streamOutputIndex, summaryIndex, partValue, sequenceNumber, eventExtraFields } ->
            indexedItemEvent "response.reasoning_summary_part.added"
                streamItemId (Nothing :: Maybe Text) streamOutputIndex sequenceNumber eventExtraFields
                [ optionalField "summary_index" summaryIndex
                , optionalField "part" partValue
                ]
        ResponseReasoningSummaryTextDoneEvent { streamItemId, streamOutputIndex, summaryIndex, text, sequenceNumber, eventExtraFields } ->
            indexedItemEvent "response.reasoning_summary_text.done"
                streamItemId (Nothing :: Maybe Text) streamOutputIndex sequenceNumber eventExtraFields
                [ optionalField "summary_index" summaryIndex
                , optionalField "text" text
                ]
        ResponseErrorEvent { streamError, sequenceNumber, eventExtraFields } ->
            topLevelErrorEvent streamError sequenceNumber eventExtraFields
        ResponseNestedErrorEvent { streamError, sequenceNumber, eventExtraFields } ->
            objectWith eventExtraFields
                [ Just (field "type" ("error" :: Text))
                , optionalField "sequence_number" sequenceNumber
                , Just (field "error" streamError)
                ]
        OtherResponseStreamEvent { otherEventType, sequenceNumber, eventExtraFields } ->
            objectWith eventExtraFields
                [ Just (field "type" (streamEventTypeText otherEventType))
                , optionalField "sequence_number" sequenceNumber
                ]
      where
        lifecycleEvent eventType response sequenceNumber extras = objectWith extras
            [ Just (field "type" (eventType :: Text))
            , optionalField "sequence_number" sequenceNumber
            , Just (field "response" response)
            ]
        outputItemEvent eventType item outputIndex sequenceNumber extras = objectWith extras
            [ Just (field "type" (eventType :: Text))
            , optionalField "sequence_number" sequenceNumber
            , optionalField "output_index" outputIndex
            , Just (field "item" item)
            ]
        indexedItemEvent eventType itemId callId outputIndex sequenceNumber extras payloadFields =
            objectWith extras
                ( [ Just (field "type" (eventType :: Text))
                  , optionalField "sequence_number" sequenceNumber
                  , optionalField "item_id" itemId
                  , optionalField "call_id" callId
                  , optionalField "output_index" outputIndex
                  ]
                    <> payloadFields
                )
        topLevelErrorEvent streamError sequenceNumber extras =
            objectWith (KeyMap.union extras streamError.extraFields)
                [ Just (field "type" ("error" :: Text))
                , optionalField "sequence_number" sequenceNumber
                , optionalField "code" streamError.code
                , Just (field "message" streamError.message)
                , optionalField "param" streamError.param
                , optionalField "resets_in_seconds" streamError.retryAfter
                ]

instance FromJSON ResponseStreamEvent where
    parseJSON = withObject "ResponseStreamEvent" $ \o -> do
        tag <- o .: "type"
        sequenceNumber <- o .:? "sequence_number"
        case parseStreamEventType tag of
            EventResponseCreated -> lifecycle ResponseCreatedEvent sequenceNumber o
            EventResponseInProgress -> lifecycle ResponseInProgressEvent sequenceNumber o
            EventResponseCompleted -> lifecycle ResponseCompletedEvent sequenceNumber o
            EventResponseDone -> ResponseDoneEvent
                <$> o .: "response"
                <*> pure sequenceNumber
                <*> pure
                    ( without ["type", "response"]
                    $ withoutNonNull ["sequence_number"] o
                    )
            EventResponseFailed -> lifecycle ResponseFailedEvent sequenceNumber o
            EventResponseIncomplete -> lifecycle ResponseIncompleteEvent sequenceNumber o
            EventResponseQueued -> lifecycle ResponseQueuedEvent sequenceNumber o
            EventOutputItemAdded -> outputItem ResponseOutputItemAddedEvent sequenceNumber o
            EventOutputItemDone -> outputItem ResponseOutputItemDoneEvent sequenceNumber o
            EventFunctionCallArgumentsDelta ->
                ResponseFunctionCallArgumentsDeltaEvent
                    <$> o .:? "delta"
                    <*> o .:? "item_id"
                    <*> o .:? "output_index"
                    <*> pure sequenceNumber
                    <*> pure
                        ( without ["type"]
                        $ withoutNonNull
                            ["sequence_number", "delta", "item_id", "output_index"]
                            o
                        )
            EventFunctionCallArgumentsDone ->
                ResponseFunctionCallArgumentsDoneEvent
                    <$> o .:? "arguments"
                    <*> o .:? "name"
                    <*> o .:? "item_id"
                    <*> o .:? "output_index"
                    <*> pure sequenceNumber
                    <*> pure
                        ( without ["type"]
                        $ withoutNonNull
                            [ "sequence_number"
                            , "arguments"
                            , "name"
                            , "item_id"
                            , "output_index"
                            ]
                            o
                        )
            EventCustomToolInputDelta -> ResponseCustomToolInputDeltaEvent
                <$> o .:? "delta"
                <*> o .:? "item_id"
                <*> o .:? "call_id"
                <*> o .:? "output_index"
                <*> pure sequenceNumber
                <*> pure
                    ( without ["type"]
                    $ withoutNonNull
                        ["sequence_number", "delta", "item_id", "call_id", "output_index"]
                        o
                    )
            EventCustomToolInputDone -> ResponseCustomToolInputDoneEvent
                <$> o .:? "input"
                <*> o .:? "item_id"
                <*> o .:? "call_id"
                <*> o .:? "output_index"
                <*> pure sequenceNumber
                <*> pure
                    ( without ["type"]
                    $ withoutNonNull
                        ["sequence_number", "input", "item_id", "call_id", "output_index"]
                        o
                    )
            EventReasoningSummaryPartAdded -> ResponseReasoningSummaryPartAddedEvent
                <$> o .:? "item_id"
                <*> o .:? "output_index"
                <*> o .:? "summary_index"
                <*> o .:? "part"
                <*> pure sequenceNumber
                <*> pure
                    ( without ["type"]
                    $ withoutNonNull
                        ["sequence_number", "item_id", "output_index", "summary_index", "part"]
                        o
                    )
            EventReasoningSummaryTextDone -> ResponseReasoningSummaryTextDoneEvent
                <$> o .:? "item_id"
                <*> o .:? "output_index"
                <*> o .:? "summary_index"
                <*> o .:? "text"
                <*> pure sequenceNumber
                <*> pure
                    ( without ["type"]
                    $ withoutNonNull
                        ["sequence_number", "item_id", "output_index", "summary_index", "text"]
                        o
                    )
            EventError -> parseErrorEvent sequenceNumber o
            eventType -> pure OtherResponseStreamEvent
                { otherEventType = eventType
                , sequenceNumber
                , eventExtraFields =
                    without ["type"] (withoutNonNull ["sequence_number"] o)
                }
      where
        lifecycle constructor sequenceNumber object = constructor
            <$> object .: "response"
            <*> pure sequenceNumber
            <*> pure
                ( without ["type", "response"]
                $ withoutNonNull ["sequence_number"] object
                )
        outputItem constructor sequenceNumber object = constructor
            <$> object .: "item"
            <*> object .:? "output_index"
            <*> pure sequenceNumber
            <*> pure
                ( without ["type", "item"]
                $ withoutNonNull ["sequence_number", "output_index"] object
                )
        parseErrorEvent sequenceNumber object = do
            nestedError <- object .:? "error"
            case nestedError of
                Just streamError -> pure ResponseNestedErrorEvent
                    { streamError
                    , sequenceNumber
                    , eventExtraFields =
                        without ["type", "error"]
                            (withoutNonNull ["sequence_number"] object)
                    }
                Nothing -> do
                    streamError <- ResponseStreamError
                        <$> pure Nothing
                        <*> object .:? "code"
                        <*> object .:? "message" .!= ""
                        <*> object .:? "param"
                        <*> object .:? "resets_in_seconds"
                        <*> pure (without ["type", "sequence_number", "message"] object)
                    pure ResponseErrorEvent
                        { streamError
                        , sequenceNumber
                        , eventExtraFields =
                            without ["type", "code", "message", "param", "resets_in_seconds"]
                                (withoutNonNull ["sequence_number"] object)
                        }

parseStreamEventWithType :: Text -> Aeson.Value -> Parser ResponseStreamEvent
parseStreamEventWithType suppliedType = withObject "ResponseStreamEvent" $ \o -> do
    payloadType <- o .:? "type"
    case payloadType of
        Just actual | actual /= suppliedType ->
            fail ("SSE event type " <> Text.unpack suppliedType <> " disagrees with JSON type " <> Text.unpack actual)
        _ -> parseJSON (Aeson.Object (KeyMap.insert "type" (Aeson.String suppliedType) o))
