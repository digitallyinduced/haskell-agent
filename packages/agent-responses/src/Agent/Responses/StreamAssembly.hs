-- | Typed terminal response assembly for Responses streams.
module Agent.Responses.StreamAssembly
    ( StreamAssemblyConfig(..)
    , ResponseFailure(..)
    , StreamAssemblyStep(..)
    , StreamAssemblyState
    , emptyStreamAssemblyState
    , applyStreamEvent
    , stepStreamResponse
    , finishStreamWithoutTerminal
    , finishStreamResponse
    , buildStreamResponse
    , buildStreamResponseWithModel
    , assembleDoneResponse
    , failedResponseMessage
    , failedStreamResponseMessage
    , responseFailureFromState
    , responseFragmentHasOutput
    ) where

import Agent.Error (ApiError(..))
import Agent.TextBuffer
    ( TextBuffer
    , appendTextBuffer
    , emptyTextBuffer
    , textBufferToText
    )
import Agent.Responses.ResponseMerge
    ( mergeDoneResponse
    , mergeResponseFragments
    , responseItemIdentities
    )
import Agent.Responses.Types
import Control.Applicative ((<|>))
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

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

data ItemProgress = ItemProgress
    { itemValue              :: !ResponseItem
    , itemDone               :: !Bool
    , functionArgumentChunks :: !(Maybe TextBuffer)
    , customInputChunks      :: !(Maybe TextBuffer)
    , reasoningTextChunks    :: !(IntMap TextBuffer)
    }

data StreamAssemblyState = StreamAssemblyState
    { lifecycleResponse :: !(Maybe Response)
    , outputItems       :: !(IntMap ItemProgress)
    }

-- | Result of consuming one event. A terminal event always produces either a
-- completed response or the provider-classified failure, so callers never need
-- to retain the event stream to assemble it later.
data StreamAssemblyStep
    = StreamContinue !StreamAssemblyState
    | StreamFinished !(Either ApiError Response)

emptyStreamAssemblyState :: StreamAssemblyState
emptyStreamAssemblyState = StreamAssemblyState Nothing IntMap.empty

applyStreamEvent :: StreamAssemblyState -> ResponseStreamEvent -> StreamAssemblyState
applyStreamEvent state = \case
    ResponseCreatedEvent { responseValue } -> lifecycle responseValue
    ResponseInProgressEvent { responseValue } -> lifecycle responseValue
    ResponseQueuedEvent { responseValue } -> lifecycle responseValue
    ResponseCompletedEvent { responseValue } -> lifecycle responseValue
    ResponseDoneEvent { responseValue } -> lifecycle responseValue
    ResponseFailedEvent { responseValue } -> lifecycle responseValue
    ResponseIncompleteEvent { responseValue } -> lifecycle responseValue
    ResponseOutputItemAddedEvent { item, outputIndex } ->
        updateItem outputIndex False item state
    ResponseOutputItemDoneEvent { item, outputIndex } ->
        updateItem outputIndex True item state
    ResponseFunctionCallArgumentsDeltaEvent
        { delta = Just value, streamItemId, streamOutputIndex } ->
            updateResolvedProgress streamOutputIndex [streamItemId]
                (\progress -> progress
                    { functionArgumentChunks = Just
                        (appendTextBuffer value
                            (fromMaybe emptyTextBuffer
                                progress.functionArgumentChunks))
                    })
                state
    ResponseFunctionCallArgumentsDoneEvent
        { arguments, functionName, streamItemId, streamOutputIndex } ->
            updateResolvedProgress streamOutputIndex [streamItemId]
                (\progress -> progress
                    { itemValue = mapFunctionCall streamItemId
                        (\call -> call
                            { arguments =
                                fromMaybe
                                    (materializedFunctionArguments progress)
                                    arguments
                            , name = fromMaybe call.name functionName
                            })
                        progress.itemValue
                    , functionArgumentChunks = Nothing
                    })
                state
    ResponseCustomToolInputDeltaEvent
        { delta = Just value, streamItemId, streamCallId
        , streamOutputIndex } ->
            updateResolvedProgress streamOutputIndex
                [streamItemId, streamCallId]
                (\progress -> progress
                    { customInputChunks = Just
                        (appendTextBuffer value
                            (fromMaybe emptyTextBuffer
                                progress.customInputChunks))
                    })
                state
    ResponseCustomToolInputDoneEvent
        { inputText, streamItemId, streamCallId, streamOutputIndex } ->
            updateResolvedProgress streamOutputIndex
                [streamItemId, streamCallId]
                (\progress -> progress
                    { itemValue = mapCustomCall streamItemId streamCallId
                        (\call -> call
                            { input = fromMaybe
                                (materializedCustomInput progress)
                                inputText
                            })
                        progress.itemValue
                    , customInputChunks = Nothing
                    })
                state
    ResponseReasoningSummaryTextDoneEvent
        { text = Just value, streamItemId, streamOutputIndex
        , summaryIndex = Just index } ->
            updateResolvedProgress streamOutputIndex [streamItemId]
                (\progress -> progress
                    { itemValue = mapReasoning streamItemId index
                        (\part -> part { text = Just value })
                        progress.itemValue
                    , reasoningTextChunks =
                        IntMap.delete index progress.reasoningTextChunks
                    })
                state
    OtherResponseStreamEvent
        { otherEventType = EventReasoningSummaryTextDelta
        , eventDelta = Just value, streamItemId, streamOutputIndex
        , summaryIndex = Just index } ->
            updateResolvedProgress streamOutputIndex [streamItemId]
                (\progress -> progress
                    { reasoningTextChunks = IntMap.alter
                        (Just . appendTextBuffer value
                            . fromMaybe emptyTextBuffer)
                        index
                        progress.reasoningTextChunks
                    })
                state
    _ -> state
  where
    lifecycle response =
        state
            { lifecycleResponse = Just $ case state.lifecycleResponse of
                Nothing -> response
                Just previous ->
                    fromMaybe response
                        (mergeResponseFragments [previous, response])
            }

updateResolvedProgress
    :: Maybe Int
    -> [Maybe Text]
    -> (ItemProgress -> ItemProgress)
    -> StreamAssemblyState
    -> StreamAssemblyState
updateResolvedProgress explicitIndex identities update state =
    case explicitIndex <|> findIdentityIndex identities state of
        Nothing -> state
        Just index -> state
            { outputItems = IntMap.adjust update index state.outputItems }

-- | Incrementally consume one stream event using the same terminal semantics
-- as 'buildStreamResponseWithModel'.
stepStreamResponse
    :: StreamAssemblyConfig
    -> Maybe Text
    -> StreamAssemblyState
    -> ResponseStreamEvent
    -> StreamAssemblyStep
stepStreamResponse config modelHint state event =
    let next = applyStreamEvent state event
    in case event of
        ResponseCompletedEvent{} ->
            StreamFinished (finishStreamResponse modelHint next event)
        ResponseDoneEvent{} ->
            StreamFinished (finishStreamResponse modelHint next event)
        ResponseIncompleteEvent { responseValue }
            | config.incompleteAsFailure ->
                StreamFinished $ Left $ config.classifyFailedResponse
                    (responseFailure
                        (setResponseStatus ResponseIncomplete responseValue))
            | otherwise ->
                StreamFinished (finishStreamResponse modelHint next event)
        ResponseFailedEvent{} ->
            StreamFinished $ Left $ config.classifyFailedResponse
                (responseFailureFromState next)
        ResponseErrorEvent { streamError } ->
            StreamFinished (Left (config.classifyStreamError streamError))
        ResponseNestedErrorEvent { streamError } ->
            StreamFinished (Left (config.classifyStreamError streamError))
        _ -> StreamContinue next

-- | Finish a stream that reached EOF without a terminal event.
finishStreamWithoutTerminal
    :: StreamAssemblyConfig
    -> StreamAssemblyState
    -> Either ApiError Response
finishStreamWithoutTerminal config state =
    case state.lifecycleResponse of
        Just response
            | response.status `elem` [ResponseFailed, ResponseIncomplete] ->
                Left (config.classifyFailedResponse (responseFailure response))
        _ -> Left $ JsonDecodeError config.missingCompletionMessage ""

finishStreamResponse
    :: Maybe Text
    -> StreamAssemblyState
    -> ResponseStreamEvent
    -> Either ApiError Response
finishStreamResponse modelHint state terminalEvent = do
    terminal <- case terminalEvent of
        ResponseCompletedEvent { responseValue } ->
            Right (setResponseStatus ResponseCompleted responseValue)
        ResponseDoneEvent { responseValue } ->
            Right (setResponseStatus
                (if responseValue.status == ResponseInProgress
                    then ResponseCompleted
                    else responseValue.status)
                responseValue)
        ResponseIncompleteEvent { responseValue } ->
            Right (setResponseStatus ResponseIncomplete responseValue)
        ResponseFailedEvent { responseValue } ->
            Right (setResponseStatus ResponseFailed responseValue)
        _ -> Left $ JsonDecodeError
            "Cannot assemble a non-terminal response event"
            (Text.pack (show terminalEvent))
    let base = state.lifecycleResponse
        terminalWithOutput =
            setResponseOutput (assembledTerminalOutput terminal state) terminal
        response = mergeDoneResponse base [] terminalWithOutput
        withModel
            | Text.null response.model =
                setResponseModel (fromMaybe "" modelHint) response
            | otherwise = response
    if Text.null withModel.responseId
        then Left $ JsonDecodeError
            "Streamed response did not contain a response id"
            (Text.pack (show terminalEvent))
        else Right withModel

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
buildStreamResponseWithModel config modelHint = go emptyStreamAssemblyState
  where
    go state [] = finishStreamWithoutTerminal config state
    go state (event : rest) =
        case stepStreamResponse config modelHint state event of
            StreamContinue next -> go next rest
            StreamFinished result -> result

assembleDoneResponse
    :: Maybe Response
    -> [ResponseItem]
    -> Response
    -> Either ApiError Response
assembleDoneResponse base doneItems doneResponse =
    Right (mergeDoneResponse base doneItems doneResponse)

responseFragmentHasOutput :: Response -> Bool
responseFragmentHasOutput = not . null . (.output)

failedResponseMessage :: Response -> Text
failedResponseMessage response =
    case response.error >>= bestErrorDetail of
        Just detail -> detail
        Nothing -> case response.incompleteDetails of
            Just details -> label <> ": " <> details.reason
            Nothing -> label <> " (no details)"
  where
    bestErrorDetail err =
        nonEmpty (Just err.message) <|> nonEmpty (Just err.code)
    label
        | response.status == ResponseIncomplete = "response.incomplete"
        | otherwise = "response.failed"

failedStreamResponseMessage :: ResponseFailure -> Text
failedStreamResponseMessage failure =
    fromMaybe fallback (nonEmpty failure.failureErrorMessage)
  where
    fallback = case failure.failureIncompleteDetails of
        Just details -> label <> ": " <> details.reason
        Nothing -> case nonEmpty failure.failureErrorCode of
            Just code -> label <> ": " <> code
            Nothing -> label <> " (no details)"
    label
        | failure.failureStatus == Just "incomplete" = "response.incomplete"
        | otherwise = "response.failed"

responseFailureFromState :: StreamAssemblyState -> ResponseFailure
responseFailureFromState state =
    maybe emptyFailure responseFailure
        state.lifecycleResponse

responseFailure :: Response -> ResponseFailure
responseFailure response = ResponseFailure
    { failureStatus = Just (statusText response.status)
    , failureErrorType = Nothing
    , failureErrorCode = (.code) <$> response.error
    , failureErrorMessage = (.message) <$> response.error
    , failureIncompleteDetails = response.incompleteDetails
    , failureResponseValue = Just response
    }

emptyFailure :: ResponseFailure
emptyFailure = ResponseFailure Nothing Nothing Nothing Nothing Nothing Nothing

statusText :: ResponseStatus -> Text
statusText = \case
    ResponseCompleted -> "completed"
    ResponseFailed -> "failed"
    ResponseInProgress -> "in_progress"
    ResponseCancelled -> "cancelled"
    ResponseQueued -> "queued"
    ResponseIncomplete -> "incomplete"
    ResponseStatusUnknown value -> value

updateItem
    :: Maybe Int
    -> Bool
    -> ResponseItem
    -> StreamAssemblyState
    -> StreamAssemblyState
updateItem explicitIndex done item state =
    state
        { outputItems = IntMap.alter merge index state.outputItems }
  where
    index = fromMaybe (nextOutputIndex state) $
        explicitIndex
            <|> findItemIndex item state
            <|> if done then findPendingIndex state else Nothing
    merge Nothing = Just (ItemProgress
        item
        done
        Nothing
        Nothing
        IntMap.empty)
    merge (Just old) = Just
        old
            { itemValue = mergeResponseItem old.itemValue item
            , itemDone = old.itemDone || done
            , functionArgumentChunks =
                if done then Nothing else old.functionArgumentChunks
            , customInputChunks =
                if done then Nothing else old.customInputChunks
            , reasoningTextChunks =
                if done then IntMap.empty else old.reasoningTextChunks
            }

mergeResponseItem :: ResponseItem -> ResponseItem -> ResponseItem
mergeResponseItem old new =
    case (old, new) of
        (FunctionCallItem previous, FunctionCallItem next) ->
            FunctionCallItem next
                { itemId = next.itemId <|> previous.itemId
                , namespace = next.namespace <|> previous.namespace
                , status = next.status <|> previous.status
                }
        (CustomToolCallItem previous, CustomToolCallItem next) ->
            CustomToolCallItem next
                { itemId = next.itemId <|> previous.itemId
                , namespace = next.namespace <|> previous.namespace
                , status = next.status <|> previous.status
                }
        _ -> new

findItemIndex :: ResponseItem -> StreamAssemblyState -> Maybe Int
findItemIndex item =
    findIdentityIndex (map (Just . snd) (responseItemIdentities item))

findIdentityIndex :: [Maybe Text] -> StreamAssemblyState -> Maybe Int
findIdentityIndex identities state =
    foldr (<|>) Nothing (map findValue values)
  where
    values = [value | Just value <- identities, not (Text.null value)]
    findValue value =
        fst <$> find
            (elem value . map snd
                . responseItemIdentities . (.itemValue) . snd)
            (IntMap.toList state.outputItems)

nextOutputIndex :: StreamAssemblyState -> Int
nextOutputIndex state =
    maybe 0 ((+ 1) . fst) (IntMap.lookupMax state.outputItems)

findPendingIndex :: StreamAssemblyState -> Maybe Int
findPendingIndex state =
    fst <$> find (not . (.itemDone) . snd)
        (IntMap.toList state.outputItems)

assembledTerminalOutput :: Response -> StreamAssemblyState -> [ResponseItem]
assembledTerminalOutput terminal state =
    IntMap.elems (IntMap.unionWith
        mergeResponseItem
        finalItems
        (fmap materializeItemProgress state.outputItems))
  where
    finalItems = IntMap.fromList (zip [0..] terminal.output)

materializeItemProgress :: ItemProgress -> ResponseItem
materializeItemProgress progress =
    materializeReasoning
        $ materializeCustom
        $ materializeFunction progress.itemValue
  where
    materializeFunction = case progress.functionArgumentChunks of
        Nothing -> id
        Just chunks -> mapFunctionCall Nothing \call -> call
            { arguments = call.arguments <> textBufferToText chunks }
    materializeCustom = case progress.customInputChunks of
        Nothing -> id
        Just chunks -> mapCustomCall Nothing Nothing \call -> call
            { input = call.input <> textBufferToText chunks }
    materializeReasoning item =
        IntMap.foldlWithKey'
            (\current index chunks ->
                mapReasoning Nothing index
                    (\part -> part
                        { text = Just
                            (fromMaybe "" part.text <> textBufferToText chunks)
                        })
                    current)
            item
            progress.reasoningTextChunks

materializedFunctionArguments :: ItemProgress -> Text
materializedFunctionArguments progress =
    case materializeItemProgress progress of
        FunctionCallItem call -> call.arguments
        _ -> ""

materializedCustomInput :: ItemProgress -> Text
materializedCustomInput progress =
    case materializeItemProgress progress of
        CustomToolCallItem call -> call.input
        _ -> ""

mapFunctionCall
    :: Maybe Text
    -> (FunctionCall -> FunctionCall)
    -> ResponseItem
    -> ResponseItem
mapFunctionCall itemId update = \case
    FunctionCallItem call -> FunctionCallItem (update call)
    item -> case itemId of
        Nothing -> item
        Just identifier -> FunctionCallItem (update FunctionCall
            { itemId = Just identifier
            , callId = identifier
            , name = ""
            , namespace = Nothing
            , provider = Nothing
            , arguments = ""
            , encryptedFunctionArgs = Nothing
            , status = Nothing
            })

mapCustomCall
    :: Maybe Text
    -> Maybe Text
    -> (CustomToolCall -> CustomToolCall)
    -> ResponseItem
    -> ResponseItem
mapCustomCall itemId callId update = \case
    CustomToolCallItem call -> CustomToolCallItem (update call)
    item -> case callId <|> itemId of
        Nothing -> item
        Just identifier -> CustomToolCallItem (update CustomToolCall
            { itemId
            , callId = identifier
            , name = ""
            , namespace = Nothing
            , input = ""
            , status = Nothing
            })

mapReasoning
    :: Maybe Text
    -> Int
    -> (ReasoningSummaryPart -> ReasoningSummaryPart)
    -> ResponseItem
    -> ResponseItem
mapReasoning itemId index update = \case
    ReasoningItemValue reasoning ->
        ReasoningItemValue reasoning
            { summary = updateAt index update reasoning.summary }
    item -> case itemId of
        Nothing -> item
        Just identifier -> ReasoningItemValue ReasoningItem
            { itemId = Just identifier
            , summary = updateAt index update []
            , content = Nothing
            , encryptedContent = Nothing
            , status = Nothing
            }

updateAt
    :: Int
    -> (ReasoningSummaryPart -> ReasoningSummaryPart)
    -> [ReasoningSummaryPart]
    -> [ReasoningSummaryPart]
updateAt index update values =
    take index padded <> [update (padded !! index)] <> drop (index + 1) padded
  where
    emptyPart = ReasoningSummaryPart "summary_text" Nothing
    padded = values <> replicate (max 0 (index + 1 - length values)) emptyPart

nonEmpty :: Maybe Text -> Maybe Text
nonEmpty (Just value) | not (Text.null value) = Just value
nonEmpty _ = Nothing

setResponseStatus :: ResponseStatus -> Response -> Response
setResponseStatus value response =
    response { status = value }

setResponseModel :: Text -> Response -> Response
setResponseModel value response =
    response { model = value }

setResponseOutput :: [ResponseItem] -> Response -> Response
setResponseOutput value response =
    response { output = value }
