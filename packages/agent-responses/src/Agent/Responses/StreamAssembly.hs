-- | Provider-neutral terminal response assembly for Responses streams.
module Agent.Responses.StreamAssembly
    ( StreamAssemblyConfig(..)
    , ResponseFailure(..)
    , StreamAssemblyState
    , emptyStreamAssemblyState
    , applyStreamEvent
    , finishStreamResponse
    , finishAssembledIncomplete
    , buildStreamResponse
    , buildStreamResponseWithModel
    , assembleDoneResponse
    , failedResponseMessage
    , failedStreamResponseMessage
    , responseFailureFromState
    , responseFragmentHasOutput
    ) where

import Agent.Error (ApiError(..))
import Agent.Json
    ( RawJson
    , emptyExtensions
    , extensionFieldWasPresent
    , lookupExtension
    , rawJsonBytes
    )
import qualified Agent.Json.Decoder as Decoder
import qualified Agent.Json.Encoder as JsonEncoder
import Agent.Responses.ResponseMerge
    ( mergeDoneResponse
    , mergeResponseFragments
    , responseItemIdentities
    , responseItemKind
    )
import Agent.Responses.Types
import Agent.Responses.Types.Items (reasoningSummaryPartDecoder)
import Control.Applicative ((<|>))
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

-- | Provider-specific failure classification around shared event assembly.
data StreamAssemblyConfig = StreamAssemblyConfig
    { missingCompletionMessage :: !Text
    , classifyStreamError :: !(ResponseStreamError -> ApiError)
    , classifyFailedResponse :: !(ResponseFailure -> ApiError)
    , incompleteAsFailure :: !Bool
    }

data ResponseFailure = ResponseFailure
    { failureStatus            :: !(Maybe Text)
    , failureErrorType         :: !(Maybe Text)
    , failureErrorCode         :: !(Maybe Text)
    , failureErrorMessage      :: !(Maybe Text)
    , failureIncompleteDetails :: !(Maybe IncompleteDetails)
    , failureResponseValue     :: !(Maybe Response)
    } deriving (Eq, Show)

data PartialResponseItem
    = ParsedItem !ResponseItem
    | PartialFunctionCall
        { partialItemId    :: !(Maybe Text)
        , partialName      :: !(Maybe Text)
        , partialArguments :: !Text
        }
    | PartialCustomToolCall
        { partialItemId :: !(Maybe Text)
        , partialCallId :: !(Maybe Text)
        , partialInput  :: !Text
        }
    | PartialReasoning
        { partialItemId  :: !(Maybe Text)
        , partialSummary :: !(Map Int ReasoningSummaryPart)
        , partialContent :: !(Map Int ResponseContentPart)
        }
    | PartialMessage
        { partialItemId :: !(Maybe Text)
        , partialContent :: !(Map Int ResponseContentPart)
        }
    deriving (Eq, Show)

data ItemProgress = ItemProgress
    { itemValue :: !PartialResponseItem
    , itemDone  :: !Bool
    }
    deriving (Eq, Show)

-- | Incremental state shared by HTTP/SSE and reusable WebSocket transports.
data StreamAssemblyState = StreamAssemblyState
    { lifecycleResponse :: !(Maybe Response)
    , outputItems       :: !(IntMap ItemProgress)
    }

emptyStreamAssemblyState :: StreamAssemblyState
emptyStreamAssemblyState = StreamAssemblyState
    { lifecycleResponse = Nothing
    , outputItems = IntMap.empty
    }

applyStreamEvent :: StreamAssemblyState -> ResponseStreamEvent -> StreamAssemblyState
applyStreamEvent state event = case event of
    ResponseCreatedEvent { responseValue } -> overlayLifecycle responseValue state
    ResponseInProgressEvent { responseValue } -> overlayLifecycle responseValue state
    ResponseQueuedEvent { responseValue } -> overlayLifecycle responseValue state
    ResponseCompletedEvent { responseValue } -> overlayLifecycle responseValue state
    ResponseDoneEvent { responseValue } -> overlayLifecycle responseValue state
    ResponseFailedEvent { responseValue } -> overlayLifecycle responseValue state
    ResponseIncompleteEvent { responseValue } -> overlayLifecycle responseValue state
    ResponseOutputItemAddedEvent { item, outputIndex } ->
        updateStreamItem outputIndex False item state
    ResponseOutputItemDoneEvent { item, outputIndex } ->
        updateStreamItem outputIndex True item state
    ResponseOutputTextDeltaEvent
        { delta, streamItemId, streamOutputIndex, contentIndex } ->
            updateTextEvent
                updateOutputText appendText
                delta streamItemId streamOutputIndex contentIndex state
    ResponseOutputTextDoneEvent
        { text, streamItemId, streamOutputIndex, contentIndex } ->
            updateTextEvent
                updateOutputText setText
                text streamItemId streamOutputIndex contentIndex state
    ResponseFunctionCallArgumentsDeltaEvent
        { delta, streamItemId, streamOutputIndex } ->
            case (delta, resolveOutputIndex streamOutputIndex [streamItemId] state) of
                (Just argumentsDelta, Just outputIndex) ->
                    updateFunctionCall outputIndex streamItemId Nothing
                        (appendText argumentsDelta) state
                _ -> state
    ResponseFunctionCallArgumentsDoneEvent
        { arguments, functionName, streamItemId, streamOutputIndex } ->
            case resolveOutputIndex streamOutputIndex [streamItemId] state of
                Just outputIndex ->
                    updateFunctionCall outputIndex streamItemId functionName
                        (maybe id setText arguments) state
                Nothing -> state
    ResponseCustomToolInputDeltaEvent
        { delta, streamItemId, streamCallId, streamOutputIndex } ->
            case ( delta
                 , resolveOutputIndex
                    streamOutputIndex [streamItemId, streamCallId] state
                 ) of
                (Just inputDelta, Just outputIndex) ->
                    updateCustomToolInput outputIndex streamItemId streamCallId
                        (appendText inputDelta) state
                _ -> state
    ResponseCustomToolInputDoneEvent
        { inputText, streamItemId, streamCallId, streamOutputIndex } ->
            case resolveOutputIndex
                    streamOutputIndex [streamItemId, streamCallId] state of
                Just outputIndex ->
                    updateCustomToolInput outputIndex streamItemId streamCallId
                        (maybe id setText inputText) state
                Nothing -> state
    ResponseReasoningSummaryPartAddedEvent
        { streamItemId, streamOutputIndex, summaryIndex, partValue } ->
            updateReasoningPartEvent
                partValue streamItemId streamOutputIndex summaryIndex state
    ResponseReasoningSummaryPartDoneEvent
        { streamItemId, streamOutputIndex, summaryIndex, partValue } ->
            updateReasoningPartEvent
                partValue streamItemId streamOutputIndex summaryIndex state
    ResponseReasoningSummaryTextDeltaEvent
        { streamItemId, streamOutputIndex, summaryIndex, delta } ->
            updateTextEvent
                updateReasoningSummary appendSummaryText
                delta streamItemId streamOutputIndex summaryIndex state
    ResponseReasoningSummaryTextDoneEvent
        { streamItemId, streamOutputIndex, summaryIndex, text } ->
            updateTextEvent
                updateReasoningSummary setSummaryText
                text streamItemId streamOutputIndex summaryIndex state
    ResponseReasoningTextDeltaEvent
        { streamItemId, streamOutputIndex, contentIndex, delta } ->
            updateTextEvent
                updateReasoningText appendText
                delta streamItemId streamOutputIndex contentIndex state
    ResponseReasoningTextDoneEvent
        { streamItemId, streamOutputIndex, contentIndex, text } ->
            updateTextEvent
                updateReasoningText setText
                text streamItemId streamOutputIndex contentIndex state
    _ -> state

finishStreamResponse
    :: Maybe Text
    -> StreamAssemblyState
    -> ResponseStreamEvent
    -> Either ApiError Response
finishStreamResponse modelHint state terminalEvent =
    case terminalEvent of
        ResponseCompletedEvent { responseValue } ->
            finishState modelHint
                (terminalStatus ResponseCompleted responseValue)
                state
        ResponseDoneEvent { responseValue }
            | extensionFieldWasPresent
                "status"
                responseValue.extraFields ->
                    finishState modelHint responseValue.status state
            | responseValue.status /= ResponseInProgress ->
                finishState modelHint responseValue.status state
            | otherwise ->
                finishState modelHint ResponseCompleted state
        ResponseIncompleteEvent { responseValue } ->
            finishState modelHint
                (terminalStatus ResponseIncomplete responseValue)
                state
        ResponseFailedEvent { responseValue } ->
            finishState modelHint
                (terminalStatus ResponseFailed responseValue)
                state
        _ -> Left $ JsonDecodeError
            "Cannot assemble a non-terminal response event"
            (preview terminalEvent)
  where
    terminalStatus fallback responseValue
        | extensionFieldWasPresent
            "status"
            responseValue.extraFields =
                responseValue.status
        | otherwise = fallback

finishState
    :: Maybe Text
    -> ResponseStatus
    -> StreamAssemblyState
    -> Either ApiError Response
finishState modelHint terminalStatus state =
    case state.lifecycleResponse of
        Nothing -> Left $ JsonDecodeError
            "Streamed response did not contain a lifecycle response"
            (preview state.outputItems)
        Just response ->
            let assembled = response
                    { responseId = response.responseId
                    , model =
                        if Text.null response.model
                            then fromMaybe "" modelHint
                            else response.model
                    , object =
                        if Text.null response.object
                            then "response"
                            else response.object
                    , status = terminalStatus
                    , output = assembledOutput state
                    }
            in if Text.null assembled.responseId
                then Left $ JsonDecodeError
                    "Streamed response did not contain a response id"
                    (preview assembled)
                else Right assembled

finishAssembledIncomplete
    :: Maybe Text
    -> StreamAssemblyState
    -> Either ApiError Response
finishAssembledIncomplete modelHint =
    finishState modelHint ResponseIncomplete

buildStreamResponse
    :: StreamAssemblyConfig
    -> [ResponseStreamEvent]
    -> Either ApiError Response
buildStreamResponse config =
    buildStreamResponseWithModel config Nothing

buildStreamResponseWithModel
    :: StreamAssemblyConfig
    -> Maybe Text
    -> [ResponseStreamEvent]
    -> Either ApiError Response
buildStreamResponseWithModel config modelHint events =
    go emptyStreamAssemblyState events
  where
    go _ [] = Left $ JsonDecodeError
        config.missingCompletionMessage
        (preview events)
    go state (event : rest) =
        let nextState = applyStreamEvent state event
        in case event of
            ResponseCompletedEvent{} ->
                finishTerminal nextState event
            ResponseDoneEvent{} ->
                finishTerminal nextState event
            ResponseIncompleteEvent{} ->
                finishTerminal nextState event
            ResponseFailedEvent{} ->
                Left (config.classifyFailedResponse
                    ((responseFailureFromState nextState)
                        { failureStatus = Just "failed" }))
            ResponseErrorEvent { streamError } ->
                Left (config.classifyStreamError streamError)
            ResponseNestedErrorEvent { streamError } ->
                Left (config.classifyStreamError streamError)
            _ -> go nextState rest

    finishTerminal state event = do
        response <- finishStreamResponse modelHint state event
        case response.status of
            ResponseFailed ->
                Left (config.classifyFailedResponse
                    ((responseFailureFromState state)
                        { failureStatus = Just "failed" }))
            ResponseIncomplete
                | config.incompleteAsFailure ->
                    Left (config.classifyFailedResponse
                        ((responseFailureFromState state)
                            { failureStatus = Just "incomplete" }))
            _ -> Right response

assembleDoneResponse
    :: Maybe Response
    -> [ResponseItem]
    -> Response
    -> Either ApiError Response
assembleDoneResponse baseResponse doneItems doneResponse =
    let merged =
            mergeDoneResponse baseResponse doneItems doneResponse
    in if Text.null merged.responseId
        then Left $ JsonDecodeError
            "Streamed response did not contain a response id"
            (preview merged)
        else if Text.null merged.model
            then Left $ JsonDecodeError
                "Streamed response did not contain a model"
                (preview merged)
            else Right merged

responseFragmentHasOutput :: Response -> Bool
responseFragmentHasOutput = not . null . (.output)

failedResponseMessage :: Response -> Text
failedResponseMessage response =
    case response.error >>= bestErrorDetail of
        Just detail -> detail
        Nothing -> case response.incompleteDetails of
            Just details -> responseLabel <> ": " <> details.reason
            Nothing -> responseLabel <> " (no details)"
  where
    bestErrorDetail responseError =
        nonEmpty (Just responseError.message)
            <|> nonEmpty (Just responseError.code)
    responseLabel = case response.status of
        ResponseIncomplete -> "response.incomplete"
        _ -> "response.failed"

failedStreamResponseMessage :: ResponseFailure -> Text
failedStreamResponseMessage failure =
    fromMaybe fallback (nonEmpty failure.failureErrorMessage)
  where
    fallback = case failure.failureIncompleteDetails of
        Just details -> responseLabel <> ": " <> details.reason
        Nothing -> case nonEmpty failure.failureErrorCode of
            Just code -> responseLabel <> ": " <> code
            Nothing -> responseLabel <> " (no details)"
    responseLabel
        | failure.failureStatus == Just "incomplete" = "response.incomplete"
        | otherwise = "response.failed"

responseFailureFromState :: StreamAssemblyState -> ResponseFailure
responseFailureFromState state =
    let responseError = state.lifecycleResponse >>= (.error)
    in ResponseFailure
        { failureStatus =
            responseStatusText . (.status) <$> state.lifecycleResponse
        , failureErrorType = responseError >>= \err ->
            lookupExtension "type" err.extraFields >>= decodeRawText
        , failureErrorCode = responseError >>= nonEmpty . Just . (.code)
        , failureErrorMessage =
            responseError >>= nonEmpty . Just . (.message)
        , failureIncompleteDetails =
            state.lifecycleResponse >>= (.incompleteDetails)
        , failureResponseValue = state.lifecycleResponse
        }

overlayLifecycle :: Response -> StreamAssemblyState -> StreamAssemblyState
overlayLifecycle response state =
    state
        { lifecycleResponse =
            mergeResponseFragments
                (maybe [] pure state.lifecycleResponse <> [response])
        }

updateItem
    :: Int
    -> Bool
    -> ResponseItem
    -> StreamAssemblyState
    -> StreamAssemblyState
updateItem outputIndex done newValue state =
    state
        { outputItems =
            IntMap.alter (Just . mergeProgress) outputIndex state.outputItems
        }
  where
    mergeProgress Nothing = ItemProgress (ParsedItem newValue) done
    mergeProgress (Just old) = ItemProgress
        { itemValue =
            if done
                then case old.itemValue of
                    ParsedItem oldValue ->
                        ParsedItem
                            (mergeParsedItems oldValue newValue)
                    partial ->
                        ParsedItem
                            (mergePartialIntoDone partial newValue)
                else case old.itemValue of
                    ParsedItem oldValue ->
                        ParsedItem
                            (mergeParsedItems oldValue newValue)
                    partial ->
                        mergePartialItem partial newValue
        , itemDone = old.itemDone || done
        }

mergePartialIntoDone
    :: PartialResponseItem
    -> ResponseItem
    -> ResponseItem
mergePartialIntoDone partial done =
    case (partial, done) of
        (PartialFunctionCall { partialItemId }, FunctionCallItem value) ->
            FunctionCallItem value
                { itemId =
                    if extensionFieldWasPresent
                        "id"
                        value.extraFields
                        then value.itemId
                        else value.itemId <|> partialItemId
                }
        ( PartialCustomToolCall
            { partialItemId, partialCallId }
          , CustomToolCallItem value
          ) ->
            CustomToolCallItem value
                { itemId =
                    if extensionFieldWasPresent
                        "id"
                        value.extraFields
                        then value.itemId
                        else value.itemId <|> partialItemId
                , callId =
                    if extensionFieldWasPresent
                        "call_id"
                        value.extraFields
                        then value.callId
                        else fromMaybe value.callId partialCallId
                }
        (PartialReasoning { partialItemId }, ReasoningItemValue value) ->
            let merged = case applyPartialToItem partial done of
                    ParsedItem (ReasoningItemValue candidate) ->
                        candidate
                    _ -> value
            in ReasoningItemValue value
                { itemId =
                    if extensionFieldWasPresent
                        "id"
                        value.extraFields
                        then value.itemId
                        else value.itemId <|> partialItemId
                , summary = value.summary
                , content =
                    if extensionFieldWasPresent
                        "content"
                        value.extraFields
                        then value.content
                        else merged.content
                }
        (PartialMessage { partialItemId }, MessageItem value) ->
            MessageItem value
                { messageId =
                    if extensionFieldWasPresent
                        "id"
                        value.extraFields
                        then value.messageId
                        else value.messageId <|> partialItemId
                }
        _ -> done

mergeParsedItems :: ResponseItem -> ResponseItem -> ResponseItem
mergeParsedItems oldValue newValue =
    case (oldValue, newValue) of
        (MessageItem old, MessageItem new) ->
            MessageItem ResponseMessage
                { messageId = pick "id" new.extraFields
                    new.messageId old.messageId
                , content = new.content
                , role = new.role
                , status = pick "status" new.extraFields
                    new.status old.status
                , phase = pick "phase" new.extraFields
                    new.phase old.phase
                , passthrough =
                    pick "internal_chat_message_metadata_passthrough"
                        new.extraFields
                        new.passthrough
                        old.passthrough
                , extraFields = old.extraFields <> new.extraFields
                }
        (ItemReferenceValue old, ItemReferenceValue new) ->
            ItemReferenceValue new
                { extraFields = old.extraFields <> new.extraFields }
        (AdditionalToolsItemValue old, AdditionalToolsItemValue new) ->
            AdditionalToolsItemValue new
                { itemId = pick "id" new.extraFields
                    new.itemId old.itemId
                , role =
                    if present "role" new.extraFields
                        || new.role /= "developer"
                        then new.role
                        else old.role
                , tools =
                    if present "tools" new.extraFields
                        || not (null new.tools)
                        then new.tools
                        else old.tools
                , extraFields = old.extraFields <> new.extraFields
                }
        (LocalShellCallItem old, LocalShellCallItem new) ->
            LocalShellCallItem new
                { itemId = pick "id" new.extraFields
                    new.itemId old.itemId
                , callId = pick "call_id" new.extraFields
                    new.callId old.callId
                , status = pick "status" new.extraFields
                    new.status old.status
                , action = pick "action" new.extraFields
                    new.action old.action
                , extraFields = old.extraFields <> new.extraFields
                }
        (ToolSearchCallItem old, ToolSearchCallItem new) ->
            ToolSearchCallItem new
                { itemId = pick "id" new.extraFields
                    new.itemId old.itemId
                , callId = pick "call_id" new.extraFields
                    new.callId old.callId
                , status = pick "status" new.extraFields
                    new.status old.status
                , execution = pick "execution" new.extraFields
                    new.execution old.execution
                , arguments = pick "arguments" new.extraFields
                    new.arguments old.arguments
                , extraFields = old.extraFields <> new.extraFields
                }
        (ToolSearchOutputItem old, ToolSearchOutputItem new) ->
            ToolSearchOutputItem new
                { itemId = pick "id" new.extraFields
                    new.itemId old.itemId
                , callId = pick "call_id" new.extraFields
                    new.callId old.callId
                , status = pick "status" new.extraFields
                    new.status old.status
                , execution = pick "execution" new.extraFields
                    new.execution old.execution
                , tools =
                    if present "tools" new.extraFields
                        || not (null new.tools)
                        then new.tools
                        else old.tools
                , extraFields = old.extraFields <> new.extraFields
                }
        (WebSearchCallItem old, WebSearchCallItem new) ->
            WebSearchCallItem new
                { itemId = pick "id" new.extraFields
                    new.itemId old.itemId
                , status = pick "status" new.extraFields
                    new.status old.status
                , action = pick "action" new.extraFields
                    new.action old.action
                , extraFields = old.extraFields <> new.extraFields
                }
        (ImageGenerationCallItem old, ImageGenerationCallItem new) ->
            ImageGenerationCallItem new
                { itemId = pick "id" new.extraFields
                    new.itemId old.itemId
                , status = pick "status" new.extraFields
                    new.status old.status
                , revisedPrompt = pick "revised_prompt" new.extraFields
                    new.revisedPrompt old.revisedPrompt
                , result = pick "result" new.extraFields
                    new.result old.result
                , extraFields = old.extraFields <> new.extraFields
                }
        (CompactionItemValue old, CompactionItemValue new) ->
            CompactionItemValue new
                { itemId = pick "id" new.extraFields
                    new.itemId old.itemId
                , encryptedContent =
                    pick "encrypted_content" new.extraFields
                        new.encryptedContent old.encryptedContent
                , extraFields = old.extraFields <> new.extraFields
                }
        (CompactionTriggerItemValue old, CompactionTriggerItemValue new) ->
            CompactionTriggerItemValue new
                { extraFields = old.extraFields <> new.extraFields }
        (ContextCompactionItemValue old, ContextCompactionItemValue new) ->
            ContextCompactionItemValue new
                { itemId = pick "id" new.extraFields
                    new.itemId old.itemId
                , encryptedContent =
                    pick "encrypted_content" new.extraFields
                        new.encryptedContent old.encryptedContent
                , extraFields = old.extraFields <> new.extraFields
                }
        (FunctionCallItem old, FunctionCallItem new) ->
            FunctionCallItem new
                { itemId = pick "id" new.extraFields
                    new.itemId old.itemId
                , namespace = pick "namespace" new.extraFields
                    new.namespace old.namespace
                , encryptedFunctionArgs =
                    pick "encrypted_function_args" new.extraFields
                        new.encryptedFunctionArgs
                        old.encryptedFunctionArgs
                , status = pick "status" new.extraFields
                    new.status old.status
                , extraFields = old.extraFields <> new.extraFields
                }
        (FunctionCallOutputItem old, FunctionCallOutputItem new) ->
            FunctionCallOutputItem new
                { itemId = pick "id" new.extraFields
                    new.itemId old.itemId
                , name = pick "name" new.extraFields
                    new.name old.name
                , namespace = pick "namespace" new.extraFields
                    new.namespace old.namespace
                , status = pick "status" new.extraFields
                    new.status old.status
                , extraFields = old.extraFields <> new.extraFields
                }
        (CustomToolCallItem old, CustomToolCallItem new) ->
            CustomToolCallItem new
                { itemId = pick "id" new.extraFields
                    new.itemId old.itemId
                , namespace = pick "namespace" new.extraFields
                    new.namespace old.namespace
                , status = pick "status" new.extraFields
                    new.status old.status
                , extraFields = old.extraFields <> new.extraFields
                }
        (CustomToolCallOutputItem old, CustomToolCallOutputItem new) ->
            CustomToolCallOutputItem new
                { itemId = pick "id" new.extraFields
                    new.itemId old.itemId
                , name = pick "name" new.extraFields
                    new.name old.name
                , status = pick "status" new.extraFields
                    new.status old.status
                , extraFields = old.extraFields <> new.extraFields
                }
        (ReasoningItemValue old, ReasoningItemValue new) ->
            ReasoningItemValue new
                { itemId = pick "id" new.extraFields
                    new.itemId old.itemId
                , summary =
                    if present "summary" new.extraFields
                        || not (null new.summary)
                        then new.summary
                        else old.summary
                , content = pick "content" new.extraFields
                    new.content old.content
                , encryptedContent =
                    pick "encrypted_content" new.extraFields
                        new.encryptedContent
                        old.encryptedContent
                , status = pick "status" new.extraFields
                    new.status old.status
                , extraFields = old.extraFields <> new.extraFields
                }
        (AgentMessageItem old, AgentMessageItem new) ->
            AgentMessageItem ResponseAgentMessage
                { messageId = pick "id" new.extraFields
                    new.messageId old.messageId
                , author = pick "author" new.extraFields
                    new.author old.author
                , recipient = pick "recipient" new.extraFields
                    new.recipient old.recipient
                , content =
                    if present "content" new.extraFields
                        || not (null new.content)
                        then new.content
                        else old.content
                , passthrough =
                    pick "internal_chat_message_metadata_passthrough"
                        new.extraFields
                        new.passthrough
                        old.passthrough
                , extraFields = old.extraFields <> new.extraFields
                }
        (UnknownResponseItem old, UnknownResponseItem new)
            | old.tag == new.tag ->
                UnknownResponseItem new
                    { fields = old.fields <> new.fields }
        (KnownResponseItem oldType old, KnownResponseItem newType new)
            | oldType == newType && old.tag == new.tag ->
                KnownResponseItem newType new
                    { fields = old.fields <> new.fields }
        _ -> mergeEncodedItems oldValue newValue
  where
    present = extensionFieldWasPresent
    pick key fields new old
        | present key fields = new
        | otherwise = new <|> old

mergeEncodedItems :: ResponseItem -> ResponseItem -> ResponseItem
mergeEncodedItems oldValue newValue =
    fromMaybe newValue do
        oldFields <- either (const Nothing) Just $
            Decoder.decode extensionObjectDecoder
                (JsonEncoder.encode responseItemEncoder oldValue)
        newFields <- either (const Nothing) Just $
            Decoder.decode extensionObjectDecoder
                (JsonEncoder.encode responseItemEncoder newValue)
        either (const Nothing) Just $
            Decoder.decode responseItemDecoder $
                JsonEncoder.encode
                    (JsonEncoder.objectWithExtensions id [])
                    (oldFields <> newFields)
  where
    extensionObjectDecoder =
        Decoder.objectFields Decoder.extensionFields

updateStreamItem
    :: Maybe Int
    -> Bool
    -> ResponseItem
    -> StreamAssemblyState
    -> StreamAssemblyState
updateStreamItem explicitIndex done newValue state =
    updateItem outputIndex done newValue state
  where
    outputIndex =
        fromMaybe (nextOutputIndex state) $
            explicitIndex
                <|> findItemIndex newValue state
                <|> if done then findPendingItemIndex newValue state else Nothing

resolveOutputIndex
    :: Maybe Int
    -> [Maybe Text]
    -> StreamAssemblyState
    -> Maybe Int
resolveOutputIndex explicitIndex identities state =
    explicitIndex
        <|> firstJust
            [ findIdentityIndex wanted state
            | Just wanted <- identities
            ]
        <|> case [wanted | Just wanted <- identities] of
            [] -> Nothing
            _ -> Just (nextOutputIndex state)

findIdentityIndex :: Text -> StreamAssemblyState -> Maybe Int
findIdentityIndex wanted state =
    fst <$> IntMap.lookupMin
        (IntMap.filter
            (elem wanted . map snd . partialItemIdentities . (.itemValue))
            state.outputItems)

findItemIndex :: ResponseItem -> StreamAssemblyState -> Maybe Int
findItemIndex value state =
    firstJust
        [ findIdentityIndex identity state
        | (_, identity) <- responseItemIdentities value
        ]

findPendingItemIndex :: ResponseItem -> StreamAssemblyState -> Maybe Int
findPendingItemIndex value state =
    fst <$> IntMap.lookupMin
        (IntMap.filter matchesPending state.outputItems)
  where
    matchesPending progress =
        not progress.itemDone
            && partialItemKind progress.itemValue == responseItemKind value

nextOutputIndex :: StreamAssemblyState -> Int
nextOutputIndex state =
    maybe 0 ((+ 1) . fst) (IntMap.lookupMax state.outputItems)

updateFunctionCall
    :: Int
    -> Maybe Text
    -> Maybe Text
    -> (Text -> Text)
    -> StreamAssemblyState
    -> StreamAssemblyState
updateFunctionCall outputIndex itemId functionName update state =
    alterProgress outputIndex state \case
        Nothing ->
            ItemProgress
                (PartialFunctionCall itemId functionName (update ""))
                False
        Just progress -> progress
            { itemValue =
                updateFunctionPartial
                    itemId functionName update progress.itemValue
            }

updateCustomToolInput
    :: Int
    -> Maybe Text
    -> Maybe Text
    -> (Text -> Text)
    -> StreamAssemblyState
    -> StreamAssemblyState
updateCustomToolInput outputIndex itemId callId update state =
    alterProgress outputIndex state \case
        Nothing ->
            ItemProgress
                (PartialCustomToolCall itemId callId (update ""))
                False
        Just progress -> progress
            { itemValue =
                updateCustomPartial itemId callId update progress.itemValue
            }

updateTextEvent
    :: (Int
        -> Text
        -> Int
        -> (part -> part)
        -> StreamAssemblyState
        -> StreamAssemblyState)
    -> (Text -> part -> part)
    -> Maybe Text
    -> Maybe Text
    -> Maybe Int
    -> Maybe Int
    -> StreamAssemblyState
    -> StreamAssemblyState
updateTextEvent updateItemPart updatePart value itemId outputIndex partIndex state =
    case (value, partIndex, resolveOutputIndex outputIndex [itemId] state) of
        (Just text, Just index, Just resolvedIndex) ->
            updateItemPart resolvedIndex (fromMaybe "" itemId) index
                (updatePart text) state
        _ -> state

updateReasoningPartEvent
    :: Maybe RawJson
    -> Maybe Text
    -> Maybe Int
    -> Maybe Int
    -> StreamAssemblyState
    -> StreamAssemblyState
updateReasoningPartEvent partValue itemId outputIndex summaryIndex state =
    case ( partValue >>= decodeReasoningSummaryPart
         , summaryIndex
         , resolveOutputIndex outputIndex [itemId] state
         ) of
        (Just part, Just index, Just resolvedIndex) ->
            updateReasoningSummary resolvedIndex (fromMaybe "" itemId)
                index (const part) state
        _ -> state

updateReasoningSummary
    :: Int
    -> Text
    -> Int
    -> (ReasoningSummaryPart -> ReasoningSummaryPart)
    -> StreamAssemblyState
    -> StreamAssemblyState
updateReasoningSummary outputIndex itemId index update state =
    alterProgress outputIndex state \case
        Nothing ->
            ItemProgress
                (PartialReasoning
                    (nonEmpty (Just itemId))
                    (Map.singleton index (update emptySummaryPart))
                    Map.empty)
                False
        Just progress -> progress
            { itemValue =
                updateReasoningSummaryPartial
                    itemId index update progress.itemValue
            }

updateReasoningText
    :: Int
    -> Text
    -> Int
    -> (Text -> Text)
    -> StreamAssemblyState
    -> StreamAssemblyState
updateReasoningText outputIndex itemId index update state =
    alterProgress outputIndex state \case
        Nothing ->
            ItemProgress
                (PartialReasoning
                    (nonEmpty (Just itemId))
                    Map.empty
                    (Map.singleton index
                        (ReasoningTextPart (update "") emptyExtensions)))
                False
        Just progress -> progress
            { itemValue =
                updateReasoningTextPartial
                    itemId index update progress.itemValue
            }

updateOutputText
    :: Int
    -> Text
    -> Int
    -> (Text -> Text)
    -> StreamAssemblyState
    -> StreamAssemblyState
updateOutputText outputIndex itemId index update state =
    alterProgress outputIndex state \case
        Nothing ->
            ItemProgress
                (PartialMessage
                    (nonEmpty (Just itemId))
                    (Map.singleton index
                        (OutputTextPart
                            (update "") Nothing Nothing emptyExtensions)))
                False
        Just progress -> progress
            { itemValue =
                updateOutputTextPartial
                    itemId index update progress.itemValue
            }

alterProgress
    :: Int
    -> StreamAssemblyState
    -> (Maybe ItemProgress -> ItemProgress)
    -> StreamAssemblyState
alterProgress index state update =
    state
        { outputItems =
            IntMap.alter (Just . update) index state.outputItems
        }

assembledOutput :: StreamAssemblyState -> [ResponseItem]
assembledOutput state =
    foldMap (partialToList . (.itemValue)) . IntMap.elems $
        IntMap.unionWith combine state.outputItems terminalItems
  where
    finalItems = maybe [] (.output) state.lifecycleResponse
    terminalItems = IntMap.fromList
        [ (index, ItemProgress (ParsedItem value) True)
        | (index, value) <- zip [0 ..] finalItems
        ]
    combine streamed terminal
        | streamed.itemDone =
            ItemProgress
                (case (terminal.itemValue, streamed.itemValue) of
                    (ParsedItem terminalValue, ParsedItem streamedValue) ->
                        ParsedItem
                            (mergeParsedItems terminalValue streamedValue)
                    _ ->
                        mergeProgressItems
                            terminal.itemValue
                            streamed.itemValue)
                True
        | otherwise =
            -- A terminal response is authoritative when no item-level done
            -- frame established a stronger value. Partial deltas must not
            -- overwrite a complete terminal item.
            case (streamed.itemValue, terminal.itemValue) of
                (ParsedItem streamedValue, ParsedItem terminalValue) ->
                    ItemProgress
                        (ParsedItem
                            (mergeParsedItems
                                streamedValue
                                terminalValue))
                        True
                _ -> terminal

mergePartialItem :: PartialResponseItem -> ResponseItem -> PartialResponseItem
mergePartialItem partial item = mergeProgressItems partial (ParsedItem item)

mergeProgressItems
    :: PartialResponseItem
    -> PartialResponseItem
    -> PartialResponseItem
mergeProgressItems base overlay = case overlay of
    ParsedItem item -> applyPartialToItem base item
    PartialFunctionCall itemId functionName arguments ->
        updateFunctionPartial itemId functionName (const arguments) base
    PartialCustomToolCall itemId callId input ->
        updateCustomPartial itemId callId (const input) base
    PartialReasoning itemId summaries content ->
        let withSummaries = Map.foldlWithKey'
                (\current index part ->
                    updateReasoningSummaryPartial
                        (fromMaybe "" itemId) index (const part) current)
                base summaries
        in Map.foldlWithKey'
            (\current index part ->
                updateReasoningTextPartPartial
                    (fromMaybe "" itemId) index (const part) current)
            withSummaries content
    PartialMessage itemId content ->
        Map.foldlWithKey'
            (\current index part ->
                updateOutputTextPartPartial
                    (fromMaybe "" itemId) index (const part) current)
            base content

applyPartialToItem :: PartialResponseItem -> ResponseItem -> PartialResponseItem
applyPartialToItem partial item = case (partial, item) of
    (PartialFunctionCall _ functionName arguments, FunctionCallItem call) ->
        ParsedItem (FunctionCallItem call
            { name = fromMaybe call.name functionName
            , arguments
            })
    (PartialCustomToolCall _ _ input, CustomToolCallItem call) ->
        ParsedItem (CustomToolCallItem call { input })
    (PartialReasoning _ summaries content, ReasoningItemValue reasoning) ->
        ParsedItem (ReasoningItemValue reasoning
            { summary =
                if Map.null summaries
                    then reasoning.summary
                    else indexedValues emptySummaryPart summaries
            , content =
                if Map.null content
                    then reasoning.content
                    else Just (indexedValues emptyReasoningTextPart content)
            })
    (PartialMessage _ content, MessageItem message) ->
        ParsedItem (MessageItem message
            { content =
                if Map.null content
                    then message.content
                    else MessageContentParts
                        (indexedValues emptyOutputTextPart content)
            })
    _ -> ParsedItem item

updateFunctionPartial
    :: Maybe Text
    -> Maybe Text
    -> (Text -> Text)
    -> PartialResponseItem
    -> PartialResponseItem
updateFunctionPartial itemId functionName update = \case
    ParsedItem (FunctionCallItem call) ->
        ParsedItem (FunctionCallItem call
            { name = fromMaybe call.name functionName
            , arguments = update call.arguments
            })
    PartialFunctionCall oldItemId oldName arguments ->
        PartialFunctionCall
            (itemId <|> oldItemId)
            (functionName <|> oldName)
            (update arguments)
    other -> other

updateCustomPartial
    :: Maybe Text
    -> Maybe Text
    -> (Text -> Text)
    -> PartialResponseItem
    -> PartialResponseItem
updateCustomPartial itemId callId update = \case
    ParsedItem (CustomToolCallItem call) ->
        ParsedItem (CustomToolCallItem call { input = update call.input })
    PartialCustomToolCall oldItemId oldCallId input ->
        PartialCustomToolCall
            (itemId <|> oldItemId)
            (callId <|> oldCallId)
            (update input)
    other -> other

updateReasoningSummaryPartial
    :: Text
    -> Int
    -> (ReasoningSummaryPart -> ReasoningSummaryPart)
    -> PartialResponseItem
    -> PartialResponseItem
updateReasoningSummaryPartial itemId index update = \case
    ParsedItem (ReasoningItemValue reasoning) ->
        ParsedItem (ReasoningItemValue reasoning
            { summary =
                updateListAt index update emptySummaryPart reasoning.summary
            })
    PartialReasoning oldItemId summaries content ->
        PartialReasoning
            (nonEmpty (Just itemId) <|> oldItemId)
            (Map.alter
                (Just . update . fromMaybe emptySummaryPart)
                index summaries)
            content
    other -> other

updateReasoningTextPartial
    :: Text
    -> Int
    -> (Text -> Text)
    -> PartialResponseItem
    -> PartialResponseItem
updateReasoningTextPartial itemId index update =
    updateReasoningTextPartPartial itemId index \part ->
        case part of
            ReasoningTextPart text fields ->
                ReasoningTextPart (update text) fields
            _ -> ReasoningTextPart (update "") emptyExtensions

updateReasoningTextPartPartial
    :: Text
    -> Int
    -> (ResponseContentPart -> ResponseContentPart)
    -> PartialResponseItem
    -> PartialResponseItem
updateReasoningTextPartPartial itemId index update = \case
    ParsedItem (ReasoningItemValue reasoning) ->
        ParsedItem (ReasoningItemValue reasoning
            { content = Just (updateListAt index update
                emptyReasoningTextPart (fromMaybe [] reasoning.content))
            })
    PartialReasoning oldItemId summaries content ->
        PartialReasoning
            (nonEmpty (Just itemId) <|> oldItemId)
            summaries
            (Map.alter
                (Just . update . fromMaybe emptyReasoningTextPart)
                index content)
    other -> other

updateOutputTextPartial
    :: Text
    -> Int
    -> (Text -> Text)
    -> PartialResponseItem
    -> PartialResponseItem
updateOutputTextPartial itemId index update =
    updateOutputTextPartPartial itemId index \part ->
        case part of
            OutputTextPart text annotations logprobs fields ->
                OutputTextPart (update text) annotations logprobs fields
            _ -> OutputTextPart (update "") Nothing Nothing emptyExtensions

updateOutputTextPartPartial
    :: Text
    -> Int
    -> (ResponseContentPart -> ResponseContentPart)
    -> PartialResponseItem
    -> PartialResponseItem
updateOutputTextPartPartial itemId index update = \case
    ParsedItem (MessageItem message) ->
        let parts = case message.content of
                MessageContentParts values -> values
                MessageContentText value ->
                    [OutputTextPart value Nothing Nothing emptyExtensions]
        in ParsedItem (MessageItem message
            { content = MessageContentParts
                (updateListAt index update emptyOutputTextPart parts)
            })
    PartialMessage oldItemId content ->
        PartialMessage
            (nonEmpty (Just itemId) <|> oldItemId)
            (Map.alter
                (Just . update . fromMaybe emptyOutputTextPart)
                index content)
    other -> other

partialToList :: PartialResponseItem -> [ResponseItem]
partialToList = \case
    ParsedItem item -> [item]
    PartialFunctionCall{} -> []
    PartialCustomToolCall{} -> []
    PartialReasoning itemId summaries content ->
        [ReasoningItemValue ReasoningItem
            { itemId
            , summary = indexedValues emptySummaryPart summaries
            , content =
                if Map.null content
                    then Nothing
                    else Just (indexedValues emptyReasoningTextPart content)
            , encryptedContent = Nothing
            , status = Nothing
            , extraFields = emptyExtensions
            }]
    PartialMessage itemId content ->
        [MessageItem ResponseMessage
            { messageId = itemId
            , content = MessageContentParts
                (indexedValues emptyOutputTextPart content)
            , role = RoleAssistant
            , status = Nothing
            , phase = Nothing
            , passthrough = Nothing
            , extraFields = emptyExtensions
            }]

partialItemIdentities :: PartialResponseItem -> [(Text, Text)]
partialItemIdentities = \case
    ParsedItem item -> responseItemIdentities item
    PartialFunctionCall itemId _ _ -> optionalIdentity "id" itemId
    PartialCustomToolCall itemId callId _ ->
        optionalIdentity "id" itemId <> optionalIdentity "call_id" callId
    PartialReasoning itemId _ _ -> optionalIdentity "id" itemId
    PartialMessage itemId _ -> optionalIdentity "id" itemId

partialItemKind :: PartialResponseItem -> Text
partialItemKind = \case
    ParsedItem item -> responseItemKind item
    PartialFunctionCall{} -> "function_call"
    PartialCustomToolCall{} -> "custom_tool_call"
    PartialReasoning{} -> "reasoning"
    PartialMessage{} -> "message"

emptySummaryPart :: ReasoningSummaryPart
emptySummaryPart = ReasoningSummaryPart
    { partType = "summary_text"
    , text = Nothing
    , extraFields = emptyExtensions
    }

emptyOutputTextPart :: ResponseContentPart
emptyOutputTextPart =
    OutputTextPart "" Nothing Nothing emptyExtensions

emptyReasoningTextPart :: ResponseContentPart
emptyReasoningTextPart =
    ReasoningTextPart "" emptyExtensions

setSummaryText :: Text -> ReasoningSummaryPart -> ReasoningSummaryPart
setSummaryText value part = part { text = Just value }

appendSummaryText :: Text -> ReasoningSummaryPart -> ReasoningSummaryPart
appendSummaryText delta part =
    part { text = Just (fromMaybe "" part.text <> delta) }

appendText :: Text -> Text -> Text
appendText delta current = current <> delta

setText :: Text -> Text -> Text
setText = const

indexedValues :: value -> Map Int value -> [value]
indexedValues defaultValue values
    | Map.null values = []
    | otherwise =
        [ Map.findWithDefault defaultValue index values
        | index <- [0 .. fst (Map.findMax values)]
        ]

updateListAt :: Int -> (value -> value) -> value -> [value] -> [value]
updateListAt index update defaultValue values
    | index < 0 = values
    | otherwise =
        indexedValues defaultValue $
            Map.alter
                (Just . update . fromMaybe defaultValue)
                index
                (Map.fromList (zip [0 ..] values))

decodeReasoningSummaryPart :: RawJson -> Maybe ReasoningSummaryPart
decodeReasoningSummaryPart raw =
    either (const Nothing) Just
        (Decoder.decode reasoningSummaryPartDecoder (rawJsonBytes raw))

decodeRawText :: RawJson -> Maybe Text
decodeRawText raw =
    either (const Nothing) Just
        (Decoder.decode Decoder.text (rawJsonBytes raw))

optionalIdentity :: Text -> Maybe Text -> [(Text, Text)]
optionalIdentity field = maybe [] (pure . (field,))

firstJust :: [Maybe value] -> Maybe value
firstJust = foldr (<|>) Nothing

nonEmpty :: Maybe Text -> Maybe Text
nonEmpty value = value >>= \text ->
    if Text.null text then Nothing else Just text

responseStatusText :: ResponseStatus -> Text
responseStatusText = \case
    ResponseCompleted -> "completed"
    ResponseFailed -> "failed"
    ResponseInProgress -> "in_progress"
    ResponseCancelled -> "cancelled"
    ResponseQueued -> "queued"
    ResponseIncomplete -> "incomplete"
    ResponseStatusUnknown value -> value

preview :: Show value => value -> Text
preview = Text.take 2000 . Text.pack . show
