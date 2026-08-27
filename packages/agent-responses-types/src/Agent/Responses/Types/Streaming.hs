module Agent.Responses.Types.Streaming
    ( ResponseStreamEvent(..)
    , ResponseStreamError(..)
    , StreamEventType(..)
    , responseStreamEventType
    , responseStreamEventSequenceNumber
    , streamEventTypeText
    , responseStreamEventEncoder
    , responseStreamEventDecoder
    , responseStreamEventDecoderWithType
    , unparsedStreamEventTypeText
    ) where

import Agent.Json
    ( Extensions
    , RawJson
    , emptyExtensions
    , deleteExtension
    , extensionsToList
    , insertExtension
    , lookupExtension
    )
import qualified Agent.Json.Decoder as Decoder
import qualified Agent.Json.Encoder as Encoder
import Data.Scientific (toBoundedInteger)
import Agent.Responses.Types.Items
    ( ResponseItem
    , responseItemDecoder
    , responseItemEncoder
    )
import Agent.Responses.Types.Response
    ( Response
    , responseFragmentDecoder
    , responseFragmentEncoder
    )
import Data.Text (Text)

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
    , extraFields :: !Extensions
    } deriving stock (Eq, Show)

data ResponseStreamEvent
    = ResponseCreatedEvent
        { responseValue     :: !Response
        , sequenceNumber    :: !(Maybe Int)
        , eventExtraFields  :: !Extensions
        }
    | ResponseInProgressEvent
        { responseValue     :: !Response
        , sequenceNumber    :: !(Maybe Int)
        , eventExtraFields  :: !Extensions
        }
    | ResponseCompletedEvent
        { responseValue     :: !Response
        , sequenceNumber    :: !(Maybe Int)
        , eventExtraFields  :: !Extensions
        }
    | ResponseDoneEvent
        { responseValue     :: !Response
        , sequenceNumber    :: !(Maybe Int)
        , eventExtraFields  :: !Extensions
        }
    | ResponseFailedEvent
        { responseValue     :: !Response
        , sequenceNumber    :: !(Maybe Int)
        , eventExtraFields  :: !Extensions
        }
    | ResponseIncompleteEvent
        { responseValue     :: !Response
        , sequenceNumber    :: !(Maybe Int)
        , eventExtraFields  :: !Extensions
        }
    | ResponseQueuedEvent
        { responseValue     :: !Response
        , sequenceNumber    :: !(Maybe Int)
        , eventExtraFields  :: !Extensions
        }
    | ResponseOutputItemAddedEvent
        { item             :: !ResponseItem
        , outputIndex      :: !(Maybe Int)
        , sequenceNumber   :: !(Maybe Int)
        , eventExtraFields :: !Extensions
        }
    | ResponseOutputItemDoneEvent
        { item             :: !ResponseItem
        , outputIndex      :: !(Maybe Int)
        , sequenceNumber   :: !(Maybe Int)
        , eventExtraFields :: !Extensions
        }
    | ResponseOutputTextDeltaEvent
        { delta             :: !(Maybe Text)
        , streamItemId      :: !(Maybe Text)
        , streamOutputIndex :: !(Maybe Int)
        , contentIndex      :: !(Maybe Int)
        , logprobs          :: !(Maybe RawJson)
        , sequenceNumber    :: !(Maybe Int)
        , eventExtraFields  :: !Extensions
        }
    | ResponseOutputTextDoneEvent
        { text              :: !(Maybe Text)
        , streamItemId      :: !(Maybe Text)
        , streamOutputIndex :: !(Maybe Int)
        , contentIndex      :: !(Maybe Int)
        , sequenceNumber    :: !(Maybe Int)
        , eventExtraFields  :: !Extensions
        }
    | ResponseFunctionCallArgumentsDeltaEvent
        { delta             :: !(Maybe Text)
        , streamItemId      :: !(Maybe Text)
        , streamOutputIndex :: !(Maybe Int)
        , sequenceNumber    :: !(Maybe Int)
        , eventExtraFields  :: !Extensions
        }
    | ResponseFunctionCallArgumentsDoneEvent
        { arguments         :: !(Maybe Text)
        , functionName      :: !(Maybe Text)
        , streamItemId      :: !(Maybe Text)
        , streamOutputIndex :: !(Maybe Int)
        , sequenceNumber    :: !(Maybe Int)
        , eventExtraFields  :: !Extensions
        }
    | ResponseCustomToolInputDeltaEvent
        { delta             :: !(Maybe Text)
        , streamItemId      :: !(Maybe Text)
        , streamCallId      :: !(Maybe Text)
        , streamOutputIndex :: !(Maybe Int)
        , sequenceNumber    :: !(Maybe Int)
        , eventExtraFields  :: !Extensions
        }
    | ResponseCustomToolInputDoneEvent
        { inputText         :: !(Maybe Text)
        , streamItemId      :: !(Maybe Text)
        , streamCallId      :: !(Maybe Text)
        , streamOutputIndex :: !(Maybe Int)
        , sequenceNumber    :: !(Maybe Int)
        , eventExtraFields  :: !Extensions
        }
    | ResponseReasoningSummaryPartAddedEvent
        { streamItemId      :: !(Maybe Text)
        , streamOutputIndex :: !(Maybe Int)
        , summaryIndex      :: !(Maybe Int)
        , partValue         :: !(Maybe RawJson)
        , sequenceNumber    :: !(Maybe Int)
        , eventExtraFields  :: !Extensions
        }
    | ResponseReasoningSummaryPartDoneEvent
        { streamItemId      :: !(Maybe Text)
        , streamOutputIndex :: !(Maybe Int)
        , summaryIndex      :: !(Maybe Int)
        , partValue         :: !(Maybe RawJson)
        , sequenceNumber    :: !(Maybe Int)
        , eventExtraFields  :: !Extensions
        }
    | ResponseReasoningSummaryTextDeltaEvent
        { delta             :: !(Maybe Text)
        , streamItemId      :: !(Maybe Text)
        , streamOutputIndex :: !(Maybe Int)
        , summaryIndex      :: !(Maybe Int)
        , sequenceNumber    :: !(Maybe Int)
        , eventExtraFields  :: !Extensions
        }
    | ResponseReasoningSummaryTextDoneEvent
        { streamItemId      :: !(Maybe Text)
        , streamOutputIndex :: !(Maybe Int)
        , summaryIndex      :: !(Maybe Int)
        , text              :: !(Maybe Text)
        , sequenceNumber    :: !(Maybe Int)
        , eventExtraFields  :: !Extensions
        }
    | ResponseReasoningTextDeltaEvent
        { delta             :: !(Maybe Text)
        , streamItemId      :: !(Maybe Text)
        , streamOutputIndex :: !(Maybe Int)
        , contentIndex      :: !(Maybe Int)
        , sequenceNumber    :: !(Maybe Int)
        , eventExtraFields  :: !Extensions
        }
    | ResponseReasoningTextDoneEvent
        { text              :: !(Maybe Text)
        , streamItemId      :: !(Maybe Text)
        , streamOutputIndex :: !(Maybe Int)
        , contentIndex      :: !(Maybe Int)
        , sequenceNumber    :: !(Maybe Int)
        , eventExtraFields  :: !Extensions
        }
    | ResponseErrorEvent
        { streamError       :: !ResponseStreamError
        , sequenceNumber    :: !(Maybe Int)
        , eventExtraFields  :: !Extensions
        }
    | ResponseNestedErrorEvent
        { streamError       :: !ResponseStreamError
        , sequenceNumber    :: !(Maybe Int)
        , eventExtraFields  :: !Extensions
        }
    | OtherResponseStreamEvent
        { otherEventType    :: !StreamEventType
        , sequenceNumber    :: !(Maybe Int)
        , eventExtraFields  :: !Extensions
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
    ResponseOutputTextDeltaEvent{} -> EventOutputTextDelta
    ResponseOutputTextDoneEvent{} -> EventOutputTextDone
    ResponseFunctionCallArgumentsDeltaEvent{} ->
        EventFunctionCallArgumentsDelta
    ResponseFunctionCallArgumentsDoneEvent{} ->
        EventFunctionCallArgumentsDone
    ResponseCustomToolInputDeltaEvent{} -> EventCustomToolInputDelta
    ResponseCustomToolInputDoneEvent{} -> EventCustomToolInputDone
    ResponseReasoningSummaryPartAddedEvent{} -> EventReasoningSummaryPartAdded
    ResponseReasoningSummaryPartDoneEvent{} -> EventReasoningSummaryPartDone
    ResponseReasoningSummaryTextDeltaEvent{} -> EventReasoningSummaryTextDelta
    ResponseReasoningSummaryTextDoneEvent{} -> EventReasoningSummaryTextDone
    ResponseReasoningTextDeltaEvent{} -> EventReasoningTextDelta
    ResponseReasoningTextDoneEvent{} -> EventReasoningTextDone
    ResponseErrorEvent{} -> EventError
    ResponseNestedErrorEvent{} -> EventError
    OtherResponseStreamEvent { otherEventType } -> otherEventType

responseStreamEventSequenceNumber :: ResponseStreamEvent -> Maybe Int
responseStreamEventSequenceNumber event = event.sequenceNumber

-- | Encode a streaming event without constructing an intermediate JSON tree.
responseStreamEventEncoder :: Encoder.Encoder ResponseStreamEvent
responseStreamEventEncoder = Encoder.choose \case
    ResponseCreatedEvent{} -> lifecycleEncoder "response.created"
    ResponseInProgressEvent{} -> lifecycleEncoder "response.in_progress"
    ResponseCompletedEvent{} -> lifecycleEncoder "response.completed"
    ResponseDoneEvent{} -> lifecycleEncoder "response.done"
    ResponseFailedEvent{} -> lifecycleEncoder "response.failed"
    ResponseIncompleteEvent{} -> lifecycleEncoder "response.incomplete"
    ResponseQueuedEvent{} -> lifecycleEncoder "response.queued"
    ResponseOutputItemAddedEvent{} -> outputItemEncoder "response.output_item.added"
    ResponseOutputItemDoneEvent{} -> outputItemEncoder "response.output_item.done"
    ResponseOutputTextDeltaEvent{} ->
        indexedEncoder "response.output_text.delta"
            [ Encoder.optionalField "content_index" Encoder.int (.contentIndex)
            , Encoder.optionalField "delta" Encoder.text (.delta)
            , Encoder.optionalField "logprobs" Encoder.rawJson (.logprobs)
            ]
    ResponseOutputTextDoneEvent{} ->
        indexedEncoder "response.output_text.done"
            [ Encoder.optionalField "content_index" Encoder.int (.contentIndex)
            , Encoder.optionalField "text" Encoder.text (.text)
            ]
    ResponseFunctionCallArgumentsDeltaEvent{} ->
        indexedEncoder "response.function_call_arguments.delta"
            [Encoder.optionalField "delta" Encoder.text (.delta)]
    ResponseFunctionCallArgumentsDoneEvent{} ->
        indexedEncoder "response.function_call_arguments.done"
            [ Encoder.optionalField "name" Encoder.text (.functionName)
            , Encoder.optionalField "arguments" Encoder.text (.arguments)
            ]
    ResponseCustomToolInputDeltaEvent{} ->
        indexedCallEncoder "response.custom_tool_call_input.delta"
            [Encoder.optionalField "delta" Encoder.text (.delta)]
    ResponseCustomToolInputDoneEvent{} ->
        indexedCallEncoder "response.custom_tool_call_input.done"
            [Encoder.optionalField "input" Encoder.text (.inputText)]
    ResponseReasoningSummaryPartAddedEvent{} ->
        summaryPartEncoder "response.reasoning_summary_part.added"
    ResponseReasoningSummaryPartDoneEvent{} ->
        summaryPartEncoder "response.reasoning_summary_part.done"
    ResponseReasoningSummaryTextDeltaEvent{} ->
        summaryEncoder "response.reasoning_summary_text.delta"
            [Encoder.optionalField "delta" Encoder.text (.delta)]
    ResponseReasoningSummaryTextDoneEvent{} ->
        summaryEncoder "response.reasoning_summary_text.done"
            [Encoder.optionalField "text" Encoder.text (.text)]
    ResponseReasoningTextDeltaEvent{} ->
        indexedEncoder "response.reasoning_text.delta"
            [ Encoder.optionalField "content_index" Encoder.int (.contentIndex)
            , Encoder.optionalField "delta" Encoder.text (.delta)
            ]
    ResponseReasoningTextDoneEvent{} ->
        indexedEncoder "response.reasoning_text.done"
            [ Encoder.optionalField "content_index" Encoder.int (.contentIndex)
            , Encoder.optionalField "text" Encoder.text (.text)
            ]
    ResponseErrorEvent{} -> topLevelErrorEncoder
    ResponseNestedErrorEvent{} ->
        eventEncoder "error"
            [Encoder.field "error" responseStreamErrorEncoder (.streamError)]
    OtherResponseStreamEvent { otherEventType } ->
        eventEncoder (streamEventTypeText otherEventType) []
  where
    eventEncoder
        :: Text
        -> [Encoder.Field ResponseStreamEvent]
        -> Encoder.Encoder ResponseStreamEvent
    eventEncoder eventType fields =
        Encoder.object
            ( [ Encoder.field "type" Encoder.text (const eventType)
              , Encoder.optionalField
                    "sequence_number" Encoder.int (.sequenceNumber)
              ]
            <> fields
            <> [Encoder.extensionsField (.eventExtraFields)]
            )

    lifecycleEncoder eventType =
        eventEncoder eventType
            [ Encoder.field
                "response"
                responseFragmentEncoder
                (.responseValue)
            ]

    outputItemEncoder eventType =
        eventEncoder eventType
            [ Encoder.optionalField "output_index" Encoder.int (.outputIndex)
            , Encoder.field "item" responseItemEncoder (.item)
            ]

    indexedEncoder eventType fields =
        eventEncoder eventType
            ( [ Encoder.optionalField "item_id" Encoder.text (.streamItemId)
              , Encoder.optionalField
                    "output_index" Encoder.int (.streamOutputIndex)
              ]
            <> fields
            )

    indexedCallEncoder eventType fields =
        indexedEncoder eventType
            (Encoder.optionalField "call_id" Encoder.text (.streamCallId) : fields)

    summaryEncoder eventType fields =
        indexedEncoder eventType
            (Encoder.optionalField "summary_index" Encoder.int (.summaryIndex) : fields)

    summaryPartEncoder eventType =
        summaryEncoder eventType
            [Encoder.optionalField "part" Encoder.rawJson (.partValue)]

    topLevelErrorEncoder =
        Encoder.object
            [ Encoder.field "type" Encoder.text
                (const ("error" :: Text))
            , Encoder.optionalField
                "sequence_number" Encoder.int (.sequenceNumber)
            , Encoder.optionalField "code" Encoder.text
                (\event -> event.streamError.code)
            , Encoder.field "message" Encoder.text
                (\event -> event.streamError.message)
            , Encoder.optionalField "param" Encoder.text
                (\event -> event.streamError.param)
            , Encoder.optionalField "resets_in_seconds" Encoder.int
                (\event -> event.streamError.retryAfter)
            , Encoder.extensionsField
                (\event ->
                    mergeExtensions
                        event.eventExtraFields
                        event.streamError.extraFields)
            ]

responseStreamErrorEncoder :: Encoder.Encoder ResponseStreamError
responseStreamErrorEncoder =
    Encoder.object
        [ Encoder.optionalField "type" Encoder.text (.errorType)
        , Encoder.optionalField "code" Encoder.text (.code)
        , Encoder.optionalField
            "message"
            Encoder.text
            (\streamError ->
                if lookupExtension
                    "message"
                    streamError.extraFields
                    /= Nothing
                    then Nothing
                    else Just streamError.message)
        , Encoder.optionalField "param" Encoder.text (.param)
        , Encoder.optionalField "resets_in_seconds" Encoder.int (.retryAfter)
        , Encoder.extensionsField (.extraFields)
        ]

mergeExtensions :: Extensions -> Extensions -> Extensions
mergeExtensions preferred fallback =
    foldr (uncurry insertExtension) fallback (extensionsToList preferred)

data DirectEvent = DirectEvent
    { directType :: !(Maybe Text)
    , directSequenceNumber :: !(Maybe Int)
    , directResponse :: !(Maybe Response)
    , directItem :: !(Maybe ResponseItem)
    , directItemId :: !(Maybe Text)
    , directOutputIndex :: !(Maybe Int)
    , directContentIndex :: !(Maybe Int)
    , directSummaryIndex :: !(Maybe Int)
    , directDelta :: !(Maybe Text)
    , directText :: !(Maybe Text)
    , directLogprobs :: !(Maybe RawJson)
    , directArguments :: !(Maybe Text)
    , directName :: !(Maybe Text)
    , directCallId :: !(Maybe Text)
    , directInput :: !(Maybe Text)
    , directPart :: !(Maybe RawJson)
    , directNestedError :: !(Maybe ResponseStreamError)
    , directCode :: !(Maybe Text)
    , directMessage :: !(Maybe Text)
    , directParam :: !(Maybe Text)
    , directRetryAfter :: !(Maybe Int)
    , directExtensions :: !Extensions
    }

-- | Decode an event whose type must be present in the JSON object.
responseStreamEventDecoder :: Decoder.Decoder ResponseStreamEvent
responseStreamEventDecoder =
    Decoder.discriminatedObject "type" eventDecoderForType

-- | Decode an event in one object traversal. A supplied transport type fills
-- an omitted JSON type and must agree with it when both are present.
responseStreamEventDecoderWithType
    :: Maybe Text
    -> Decoder.Decoder ResponseStreamEvent
responseStreamEventDecoderWithType = \case
    Nothing -> responseStreamEventDecoder
    Just eventTypeText -> eventDecoderForType eventTypeText

eventDecoderForType :: Text -> Decoder.Decoder ResponseStreamEvent
eventDecoderForType eventTypeText =
    case eventType of
        EventOutputTextDelta -> textDeltaDecoder eventType
        EventReasoningTextDelta -> textDeltaDecoder eventType
        EventReasoningSummaryTextDelta -> textDeltaDecoder eventType
        _ ->
            Decoder.mapEither finish
                (directEventDecoderFor eventType)
  where
    eventType = parseStreamEventType eventTypeText
    finish event = do
        case event.directType of
            Just actual
                | actual /= eventTypeText ->
                    Left
                        ( "SSE event type " <> eventTypeText
                        <> " disagrees with JSON type " <> actual
                        )
            _ -> Right ()
        buildEvent eventType event

data TextDeltaState = TextDeltaState
    { textDeltaType :: !(Maybe Text)
    , textDeltaSequenceNumber :: !(Maybe Int)
    , textDeltaItemId :: !(Maybe Text)
    , textDeltaOutputIndex :: !(Maybe Int)
    , textDeltaContentIndex :: !(Maybe Int)
    , textDeltaSummaryIndex :: !(Maybe Int)
    , textDeltaValue :: !(Maybe Text)
    , textDeltaLogprobs :: !(Maybe RawJson)
    , textDeltaExtensions :: !Extensions
    }

data WireOptional value
    = WireNull !RawJson
    | WireValue !value

data WireOuter value
    = WireOuterValue !value
    | WireOuterRaw !RawJson

wireOptionalDecoder
    :: Decoder.Decoder value
    -> Decoder.Decoder (WireOptional value)
wireOptionalDecoder decoder =
    Decoder.byType \case
        Decoder.JsonNull ->
            WireNull <$> Decoder.rawJson
        _ ->
            WireValue <$> decoder

wireOuterTextDecoder :: Decoder.Decoder (WireOuter Text)
wireOuterTextDecoder =
    Decoder.byType \case
        Decoder.JsonString ->
            WireOuterValue <$> Decoder.text
        _ -> WireOuterRaw <$> Decoder.rawJson

wireOuterIntDecoder :: Decoder.Decoder (WireOuter Int)
wireOuterIntDecoder =
    Decoder.byType \case
        Decoder.JsonNumber ->
            Decoder.mapEither
                (\value -> maybe
                    (Left "integer is outside Int bounds")
                    (Right . WireOuterValue)
                    (toBoundedInteger value))
                Decoder.scientific
        _ -> WireOuterRaw <$> Decoder.rawJson

textDeltaDecoder :: StreamEventType -> Decoder.Decoder ResponseStreamEvent
textDeltaDecoder expectedType =
    Decoder.object
        (TextDeltaState
            Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
            emptyExtensions)
        ( [ field "type" (Decoder.nullable Decoder.text) \value state ->
            state { textDeltaType = value }
        , optionalField "sequence_number" Decoder.int \value state ->
            state { textDeltaSequenceNumber = value }
        , optionalField "item_id" Decoder.text \value state ->
            state { textDeltaItemId = value }
        , optionalField "output_index" Decoder.int \value state ->
            state { textDeltaOutputIndex = value }
        , optionalField "delta" Decoder.text \value state ->
            state { textDeltaValue = value }
          ] <> variantFields
        )
        (Decoder.unknownField Decoder.rawJson \key value state ->
            Right state
                { textDeltaExtensions =
                    insertExtension key value state.textDeltaExtensions
                })
        finish
  where
    expectedText = streamEventTypeText expectedType
    variantFields = case expectedType of
        EventOutputTextDelta -> [contentIndexField, logprobsField]
        EventReasoningTextDelta -> [contentIndexField]
        EventReasoningSummaryTextDelta -> [summaryIndexField]
        _ -> []
    contentIndexField =
        optionalField "content_index" Decoder.int \value state ->
            state { textDeltaContentIndex = value }
    summaryIndexField =
        optionalField "summary_index" Decoder.int \value state ->
            state { textDeltaSummaryIndex = value }
    logprobsField =
        optionalField "logprobs" Decoder.rawJson \value state ->
            state { textDeltaLogprobs = value }
    finish state = do
        case state.textDeltaType of
            Just actual
                | actual /= expectedText ->
                    Left
                        ( "SSE event type " <> expectedText
                        <> " disagrees with JSON type " <> actual
                        )
            _ -> Right ()
        case expectedType of
            EventOutputTextDelta ->
                Right ResponseOutputTextDeltaEvent
                    { delta = state.textDeltaValue
                    , streamItemId = state.textDeltaItemId
                    , streamOutputIndex = state.textDeltaOutputIndex
                    , contentIndex = state.textDeltaContentIndex
                    , logprobs = state.textDeltaLogprobs
                    , sequenceNumber = state.textDeltaSequenceNumber
                    , eventExtraFields = state.textDeltaExtensions
                    }
            EventReasoningTextDelta ->
                Right ResponseReasoningTextDeltaEvent
                    { delta = state.textDeltaValue
                    , streamItemId = state.textDeltaItemId
                    , streamOutputIndex = state.textDeltaOutputIndex
                    , contentIndex = state.textDeltaContentIndex
                    , sequenceNumber = state.textDeltaSequenceNumber
                    , eventExtraFields = state.textDeltaExtensions
                    }
            EventReasoningSummaryTextDelta ->
                Right ResponseReasoningSummaryTextDeltaEvent
                    { delta = state.textDeltaValue
                    , streamItemId = state.textDeltaItemId
                    , streamOutputIndex = state.textDeltaOutputIndex
                    , summaryIndex = state.textDeltaSummaryIndex
                    , sequenceNumber = state.textDeltaSequenceNumber
                    , eventExtraFields = state.textDeltaExtensions
                    }
            _ -> Left "internal text delta decoder mismatch"

    field key decoder update =
        Decoder.field key decoder \value state ->
            Right (update value state)
    optionalField key decoder update =
        Decoder.field key (wireOptionalDecoder decoder) \wire state ->
            Right $ case wire of
                WireNull raw ->
                    (update Nothing state)
                        { textDeltaExtensions =
                            insertExtension
                                key
                                raw
                                state.textDeltaExtensions
                        }
                WireValue value ->
                    update
                        (Just value)
                        state
                            { textDeltaExtensions =
                                deleteExtension
                                    key
                                    state.textDeltaExtensions
                            }

directEventDecoderFor
    :: StreamEventType
    -> Decoder.Decoder DirectEvent
directEventDecoderFor eventType =
    Decoder.object
        emptyDirectEvent
        ( [ field "type" (Decoder.nullable Decoder.text) \value state ->
            state { directType = value }
        , optionalField "sequence_number" Decoder.int \value state ->
            state { directSequenceNumber = value }
          ] <> eventFields eventType
        )
        (Decoder.unknownField Decoder.rawJson \key value state ->
            Right state
                { directExtensions =
                    insertExtension key value state.directExtensions
                })
        Right
  where
    field key decoder update =
        Decoder.field key decoder \value state ->
            Right (update value state)
    optionalField key decoder update =
        Decoder.field key (wireOptionalDecoder decoder) \wire state ->
            Right $ case wire of
                WireNull raw ->
                    (update Nothing state)
                        { directExtensions =
                            insertExtension
                                key
                                raw
                                state.directExtensions
                        }
                WireValue value ->
                    update
                        (Just value)
                        state
                            { directExtensions =
                                deleteExtension
                                    key
                                    state.directExtensions
                            }

    eventFields = \case
        EventResponseCreated -> lifecycleFields
        EventResponseInProgress -> lifecycleFields
        EventResponseCompleted -> lifecycleFields
        EventResponseDone -> lifecycleFields
        EventResponseFailed -> lifecycleFields
        EventResponseIncomplete -> lifecycleFields
        EventResponseQueued -> lifecycleFields
        EventOutputItemAdded -> outputItemFields
        EventOutputItemDone -> outputItemFields
        EventOutputTextDone ->
            textIdentityFields <> [textField]
        EventFunctionCallArgumentsDelta ->
            itemOutputFields <> [deltaField]
        EventFunctionCallArgumentsDone ->
            itemOutputFields <> [argumentsField, nameField]
        EventCustomToolInputDelta ->
            itemOutputFields <> [callIdField, deltaField]
        EventCustomToolInputDone ->
            itemOutputFields <> [callIdField, inputField]
        EventReasoningSummaryPartAdded -> summaryPartFields
        EventReasoningSummaryPartDone -> summaryPartFields
        EventReasoningSummaryTextDone ->
            summaryIdentityFields <> [textField]
        EventReasoningTextDone ->
            textIdentityFields <> [textField]
        EventError -> errorFields
        _ -> []

    lifecycleFields =
        [ optionalField "response" responseFragmentDecoder \value state ->
            state { directResponse = value }
        ]
    outputItemFields =
        [ optionalField "item" responseItemDecoder \value state ->
            state { directItem = value }
        , outputIndexField
        ]
    itemOutputFields = [itemIdField, outputIndexField]
    textIdentityFields =
        [itemIdField, outputIndexField, contentIndexField]
    summaryIdentityFields =
        [itemIdField, outputIndexField, summaryIndexField]
    summaryPartFields = summaryIdentityFields <> [partField]
    errorFields =
        [ optionalField "error" responseStreamErrorDecoder \value state ->
            state { directNestedError = value }
        , outerField "code" wireOuterTextDecoder
            (\value state -> state { directCode = value })
        , outerField "message" wireOuterTextDecoder
            (\value state -> state { directMessage = value })
        , outerField "param" wireOuterTextDecoder
            (\value state -> state { directParam = value })
        , outerField "resets_in_seconds" wireOuterIntDecoder
            (\value state -> state { directRetryAfter = value })
        ]
    outerField key decoder update =
        Decoder.field key decoder \wire state ->
            Right $ case wire of
                WireOuterValue value ->
                    update
                        (Just value)
                        state
                            { directExtensions =
                                deleteExtension
                                    key
                                    state.directExtensions
                            }
                WireOuterRaw raw ->
                    (update Nothing state)
                        { directExtensions =
                            insertExtension
                                key
                                raw
                                (update Nothing state).directExtensions
                        }
    itemIdField =
        optionalField "item_id" Decoder.text \value state ->
            state { directItemId = value }
    outputIndexField =
        optionalField "output_index" Decoder.int \value state ->
            state { directOutputIndex = value }
    contentIndexField =
        optionalField "content_index" Decoder.int \value state ->
            state { directContentIndex = value }
    summaryIndexField =
        optionalField "summary_index" Decoder.int \value state ->
            state { directSummaryIndex = value }
    deltaField =
        optionalField "delta" Decoder.text \value state ->
            state { directDelta = value }
    textField =
        optionalField "text" Decoder.text \value state ->
            state { directText = value }
    argumentsField =
        optionalField "arguments" Decoder.text \value state ->
            state { directArguments = value }
    nameField =
        optionalField "name" Decoder.text \value state ->
            state { directName = value }
    callIdField =
        optionalField "call_id" Decoder.text \value state ->
            state { directCallId = value }
    inputField =
        optionalField "input" Decoder.text \value state ->
            state { directInput = value }
    partField =
        optionalField "part" Decoder.rawJson \value state ->
            state { directPart = value }

emptyDirectEvent :: DirectEvent
emptyDirectEvent = DirectEvent
    { directType = Nothing
    , directSequenceNumber = Nothing
    , directResponse = Nothing
    , directItem = Nothing
    , directItemId = Nothing
    , directOutputIndex = Nothing
    , directContentIndex = Nothing
    , directSummaryIndex = Nothing
    , directDelta = Nothing
    , directText = Nothing
    , directLogprobs = Nothing
    , directArguments = Nothing
    , directName = Nothing
    , directCallId = Nothing
    , directInput = Nothing
    , directPart = Nothing
    , directNestedError = Nothing
    , directCode = Nothing
    , directMessage = Nothing
    , directParam = Nothing
    , directRetryAfter = Nothing
    , directExtensions = emptyExtensions
    }

buildEvent
    :: StreamEventType
    -> DirectEvent
    -> Either Text ResponseStreamEvent
buildEvent eventType event = case eventType of
    EventResponseCreated -> lifecycle ResponseCreatedEvent
    EventResponseInProgress -> lifecycle ResponseInProgressEvent
    EventResponseCompleted -> lifecycle ResponseCompletedEvent
    EventResponseDone -> lifecycle ResponseDoneEvent
    EventResponseFailed -> lifecycle ResponseFailedEvent
    EventResponseIncomplete -> lifecycle ResponseIncompleteEvent
    EventResponseQueued -> lifecycle ResponseQueuedEvent
    EventOutputItemAdded -> outputItem ResponseOutputItemAddedEvent
    EventOutputItemDone -> outputItem ResponseOutputItemDoneEvent
    EventOutputTextDelta -> Right ResponseOutputTextDeltaEvent
        { delta = event.directDelta
        , streamItemId = event.directItemId
        , streamOutputIndex = event.directOutputIndex
        , contentIndex = event.directContentIndex
        , logprobs = event.directLogprobs
        , sequenceNumber = event.directSequenceNumber
        , eventExtraFields = event.directExtensions
        }
    EventOutputTextDone -> Right ResponseOutputTextDoneEvent
        { text = event.directText
        , streamItemId = event.directItemId
        , streamOutputIndex = event.directOutputIndex
        , contentIndex = event.directContentIndex
        , sequenceNumber = event.directSequenceNumber
        , eventExtraFields = event.directExtensions
        }
    EventFunctionCallArgumentsDelta ->
        Right ResponseFunctionCallArgumentsDeltaEvent
            { delta = event.directDelta
            , streamItemId = event.directItemId
            , streamOutputIndex = event.directOutputIndex
            , sequenceNumber = event.directSequenceNumber
            , eventExtraFields = event.directExtensions
            }
    EventFunctionCallArgumentsDone ->
        Right ResponseFunctionCallArgumentsDoneEvent
            { arguments = event.directArguments
            , functionName = event.directName
            , streamItemId = event.directItemId
            , streamOutputIndex = event.directOutputIndex
            , sequenceNumber = event.directSequenceNumber
            , eventExtraFields = event.directExtensions
            }
    EventCustomToolInputDelta -> Right ResponseCustomToolInputDeltaEvent
        { delta = event.directDelta
        , streamItemId = event.directItemId
        , streamCallId = event.directCallId
        , streamOutputIndex = event.directOutputIndex
        , sequenceNumber = event.directSequenceNumber
        , eventExtraFields = event.directExtensions
        }
    EventCustomToolInputDone -> Right ResponseCustomToolInputDoneEvent
        { inputText = event.directInput
        , streamItemId = event.directItemId
        , streamCallId = event.directCallId
        , streamOutputIndex = event.directOutputIndex
        , sequenceNumber = event.directSequenceNumber
        , eventExtraFields = event.directExtensions
        }
    EventReasoningSummaryPartAdded ->
        summaryPart ResponseReasoningSummaryPartAddedEvent
    EventReasoningSummaryPartDone ->
        summaryPart ResponseReasoningSummaryPartDoneEvent
    EventReasoningSummaryTextDelta ->
        Right ResponseReasoningSummaryTextDeltaEvent
            { delta = event.directDelta
            , streamItemId = event.directItemId
            , streamOutputIndex = event.directOutputIndex
            , summaryIndex = event.directSummaryIndex
            , sequenceNumber = event.directSequenceNumber
            , eventExtraFields = event.directExtensions
            }
    EventReasoningSummaryTextDone ->
        Right ResponseReasoningSummaryTextDoneEvent
            { streamItemId = event.directItemId
            , streamOutputIndex = event.directOutputIndex
            , summaryIndex = event.directSummaryIndex
            , text = event.directText
            , sequenceNumber = event.directSequenceNumber
            , eventExtraFields = event.directExtensions
            }
    EventReasoningTextDelta -> Right ResponseReasoningTextDeltaEvent
        { delta = event.directDelta
        , streamItemId = event.directItemId
        , streamOutputIndex = event.directOutputIndex
        , contentIndex = event.directContentIndex
        , sequenceNumber = event.directSequenceNumber
        , eventExtraFields = event.directExtensions
        }
    EventReasoningTextDone -> Right ResponseReasoningTextDoneEvent
        { text = event.directText
        , streamItemId = event.directItemId
        , streamOutputIndex = event.directOutputIndex
        , contentIndex = event.directContentIndex
        , sequenceNumber = event.directSequenceNumber
        , eventExtraFields = event.directExtensions
        }
    EventError -> case event.directNestedError of
        Just streamError -> Right ResponseNestedErrorEvent
            { streamError
            , sequenceNumber = event.directSequenceNumber
            , eventExtraFields =
                insertOuterErrorFields event
            }
        Nothing -> Right ResponseErrorEvent
            { streamError = ResponseStreamError
                { errorType = Nothing
                , code = event.directCode
                , message = maybe "" id event.directMessage
                , param = event.directParam
                , retryAfter = event.directRetryAfter
                , extraFields = emptyExtensions
                }
            , sequenceNumber = event.directSequenceNumber
            , eventExtraFields = event.directExtensions
            }
    other -> Right OtherResponseStreamEvent
        { otherEventType = other
        , sequenceNumber = event.directSequenceNumber
        , eventExtraFields = event.directExtensions
        }
  where
    lifecycle constructor = constructor
        <$> maybe (Left "missing required field response") Right
            event.directResponse
        <*> pure event.directSequenceNumber
        <*> pure event.directExtensions
    outputItem constructor = constructor
        <$> maybe (Left "missing required field item") Right event.directItem
        <*> pure event.directOutputIndex
        <*> pure event.directSequenceNumber
        <*> pure event.directExtensions
    summaryPart constructor = Right (constructor
        event.directItemId
        event.directOutputIndex
        event.directSummaryIndex
        event.directPart
        event.directSequenceNumber
        event.directExtensions)

insertOuterErrorFields :: DirectEvent -> Extensions
insertOuterErrorFields event =
    insertEncoded "code" Encoder.text event.directCode
        $ insertEncoded "message" Encoder.text event.directMessage
        $ insertEncoded "param" Encoder.text event.directParam
        $ insertEncoded
            "resets_in_seconds"
            Encoder.int
            event.directRetryAfter
            event.directExtensions
  where
    insertEncoded key encoder value fields =
        case value of
            Nothing -> fields
            Just present ->
                case Decoder.validateRawJson
                    (Encoder.encode encoder present) of
                    Left _ -> fields
                    Right raw -> insertExtension key raw fields

responseStreamErrorDecoder :: Decoder.Decoder ResponseStreamError
responseStreamErrorDecoder =
    Decoder.objectFields $
        ResponseStreamError
            <$> Decoder.optionalField "type" Decoder.text
            <*> Decoder.optionalField "code" Decoder.text
            <*> Decoder.defaultField "" "message" Decoder.text
            <*> Decoder.optionalField "param" Decoder.text
            <*> Decoder.optionalField
                "resets_in_seconds"
                Decoder.int
            <*> Decoder.extensionFields
