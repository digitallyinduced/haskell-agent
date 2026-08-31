{-# LANGUAGE RecordWildCards #-}

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
    , mergeResponseFragment
    , responseItemIdentities
    , responseItemKind
    )
import Agent.Responses.Types
import Control.Applicative ((<|>))
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
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

data StreamAssemblyStep
    = StreamContinue !StreamAssemblyState
    | StreamFinished !(Either ApiError Response)

data ItemProgress = ItemProgress
    { itemValue              :: !ResponseItem
    , itemDone               :: !Bool
    , functionArgumentChunks :: !(Maybe TextBuffer)
    , customInputChunks      :: !(Maybe TextBuffer)
    , reasoningTextChunks    :: !(IntMap TextBuffer)
    }

-- | Provider item ids and tool call ids occupy separate namespaces, as do
-- identities belonging to different output item kinds.
data ItemIdentity
    = ProviderItemId !Text !Text
    | ToolCallId !Text !Text
    deriving (Eq, Ord)

-- | Output items retain provider order while supporting the alternate
-- identities used by streaming events. An identity may occur in more than one
-- slot in a malformed stream; retaining every slot preserves the historical
-- rule that the lowest output index for that identity wins.
data OutputItemStore = OutputItemStore
    { outputSlots     :: !(IntMap ItemProgress)
    , outputAliases   :: !(Map ItemIdentity IntSet)
    , outputPending   :: !IntSet
    }

data StreamAssemblyState = StreamAssemblyState
    { lifecycleResponse :: !(Maybe Response)
    , outputItems       :: !OutputItemStore
    }

emptyStreamAssemblyState :: StreamAssemblyState
emptyStreamAssemblyState =
    StreamAssemblyState Nothing emptyOutputItemStore

emptyOutputItemStore :: OutputItemStore
emptyOutputItemStore = OutputItemStore
    { outputSlots = IntMap.empty
    , outputAliases = Map.empty
    , outputPending = IntSet.empty
    }

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
            updateResolvedProgress streamOutputIndex
                [streamItemIdentity "function_call" streamItemId]
                (\progress -> progress
                    { functionArgumentChunks = Just
                        (appendTextBuffer value
                            (fromMaybe emptyTextBuffer
                                progress.functionArgumentChunks))
                    })
                state
    ResponseFunctionCallArgumentsDoneEvent
        { arguments, functionName, streamItemId, streamOutputIndex } ->
            updateResolvedProgress streamOutputIndex
                [streamItemIdentity "function_call" streamItemId]
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
                [ streamItemIdentity "custom_tool_call" streamItemId
                , streamCallIdentity "custom_tool_call" streamCallId
                ]
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
                [ streamItemIdentity "custom_tool_call" streamItemId
                , streamCallIdentity "custom_tool_call" streamCallId
                ]
                (\progress -> progress
                    { itemValue = mapCustomCall streamItemId streamCallId
                        (setCustomToolInput
                            (fromMaybe
                                (materializedCustomInput progress)
                                inputText))
                        progress.itemValue
                    , customInputChunks = Nothing
                    })
                state
    ResponseReasoningSummaryTextDoneEvent
        { text = Just value, streamItemId, streamOutputIndex
        , summaryIndex = Just index } ->
            updateResolvedProgress streamOutputIndex
                [streamItemIdentity "reasoning" streamItemId]
                (\progress -> progress
                    { itemValue = mapReasoning streamItemId index
                        (setReasoningPartText (Just value))
                        progress.itemValue
                    , reasoningTextChunks =
                        IntMap.delete index progress.reasoningTextChunks
                    })
                state
    OtherResponseStreamEvent
        { otherEventType = EventReasoningSummaryTextDelta
        , eventDelta = Just value, streamItemId, streamOutputIndex
        , summaryIndex = Just index } ->
            updateResolvedProgress streamOutputIndex
                [streamItemIdentity "reasoning" streamItemId]
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
        merged `seq` state { lifecycleResponse = Just merged }
      where
        merged =
            maybe response
                (`mergeResponseFragment` response)
                state.lifecycleResponse

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
        { outputItems =
            setOutputSlot index progress state.outputItems
        }
  where
    index = fromMaybe (nextOutputIndex state.outputItems) $
        explicitIndex
            <|> findItemIndex item state
            <|> if done then findPendingIndex state else Nothing
    progress =
        case IntMap.lookup index state.outputItems.outputSlots of
            Nothing -> ItemProgress
                item
                done
                Nothing
                Nothing
                IntMap.empty
            Just old ->
                old
                    { itemValue = mergeResponseItem old.itemValue
                        (if done
                            then preserveBufferedProgress old item
                            else item)
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
            FunctionCallItem (mergeFunctionCall previous next)
        (CustomToolCallItem previous, CustomToolCallItem next) ->
            CustomToolCallItem (mergeCustomToolCall previous next)
        _ -> new

mergeFunctionCall :: FunctionCall -> FunctionCall -> FunctionCall
mergeFunctionCall previous next =
    let FunctionCall {..} = next
    in FunctionCall
        { itemId = itemId <|> previous.itemId
        , namespace = namespace <|> previous.namespace
        , status = status <|> previous.status
        , ..
        }

mergeCustomToolCall :: CustomToolCall -> CustomToolCall -> CustomToolCall
mergeCustomToolCall previous next =
    let CustomToolCall {..} = next
    in CustomToolCall
        { itemId = itemId <|> previous.itemId
        , namespace = namespace <|> previous.namespace
        , status = status <|> previous.status
        , ..
        }

setFunctionArguments :: Text -> FunctionCall -> FunctionCall
setFunctionArguments value call =
    let FunctionCall {..} = call
    in FunctionCall {arguments = value, ..}

setCustomToolInput :: Text -> CustomToolCall -> CustomToolCall
setCustomToolInput value call =
    let CustomToolCall {..} = call
    in CustomToolCall {input = value, ..}

setReasoningSummary :: [ReasoningSummaryPart] -> ReasoningItem -> ReasoningItem
setReasoningSummary value reasoning =
    let ReasoningItem {..} = reasoning
    in ReasoningItem {summary = value, ..}

setReasoningPartText :: Maybe Text -> ReasoningSummaryPart -> ReasoningSummaryPart
setReasoningPartText value part =
    let ReasoningSummaryPart {..} = part
    in ReasoningSummaryPart {text = value, ..}

-- A sparse output-item.done is authoritative for normal item fields, but it
-- must not erase deltas buffered since output-item.added. Only fields with
-- pending delta buffers receive a fallback, preserving the existing
-- last-event-wins semantics for every other field.
preserveBufferedProgress :: ItemProgress -> ResponseItem -> ResponseItem
preserveBufferedProgress progress = \case
    FunctionCallItem call
        | Just _ <- progress.functionArgumentChunks
        , Text.null call.arguments ->
            FunctionCallItem
                (setFunctionArguments
                    (materializedFunctionArguments progress)
                    call)
    CustomToolCallItem call
        | Just _ <- progress.customInputChunks
        , Text.null call.input ->
            CustomToolCallItem
                (setCustomToolInput (materializedCustomInput progress) call)
    ReasoningItemValue reasoning
        | not (IntMap.null progress.reasoningTextChunks) ->
            ReasoningItemValue
                (setReasoningSummary
                    (mergeReasoningSummary
                        (materializedReasoningSummary progress)
                        reasoning.summary)
                    reasoning)
    item -> item

materializedReasoningSummary :: ItemProgress -> [ReasoningSummaryPart]
materializedReasoningSummary progress =
    case materializeItemProgress progress of
        ReasoningItemValue reasoning -> reasoning.summary
        _ -> []

preferNonEmpty :: Text -> Text -> Text
preferNonEmpty preferred fallback
    | Text.null preferred = fallback
    | otherwise = preferred

mergeReasoningSummary
    :: [ReasoningSummaryPart]
    -> [ReasoningSummaryPart]
    -> [ReasoningSummaryPart]
mergeReasoningSummary (previous : previousRest) (next : nextRest) =
    next
        { partType = preferNonEmpty next.partType previous.partType
        , text = nonEmpty next.text <|> previous.text
        }
        : mergeReasoningSummary previousRest nextRest
mergeReasoningSummary previous [] = previous
mergeReasoningSummary [] next = next

updateResolvedProgress
    :: Maybe Int
    -> [Maybe ItemIdentity]
    -> (ItemProgress -> ItemProgress)
    -> StreamAssemblyState
    -> StreamAssemblyState
updateResolvedProgress explicitIndex identities update state =
    case explicitIndex <|> findIdentityIndex identities state of
        Nothing -> state
        Just index -> state
            { outputItems =
                adjustOutputProgress index update state.outputItems
            }

findItemIndex :: ResponseItem -> StreamAssemblyState -> Maybe Int
findItemIndex item =
    findIdentityIndex (map Just (itemAliases item))

findIdentityIndex
    :: [Maybe ItemIdentity]
    -> StreamAssemblyState
    -> Maybe Int
findIdentityIndex identities state =
    foldr (<|>) Nothing (map findValue values)
  where
    values = [identity | Just identity <- identities]
    findValue identity =
        Map.lookup identity state.outputItems.outputAliases
            >>= minimumIndex

findPendingIndex :: StreamAssemblyState -> Maybe Int
findPendingIndex state =
    minimumIndex state.outputItems.outputPending

minimumIndex :: IntSet -> Maybe Int
minimumIndex = fmap fst . IntSet.minView

assembledTerminalOutput :: Response -> StreamAssemblyState -> [ResponseItem]
assembledTerminalOutput terminal state =
    IntMap.elems (IntMap.unionWith
        mergeResponseItem
        finalItems
        (fmap materializeItemProgress state.outputItems.outputSlots))
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
        Just chunks -> mapFunctionCall Nothing \call ->
            setFunctionArguments
                (call.arguments <> textBufferToText chunks)
                call
    materializeCustom = case progress.customInputChunks of
        Nothing -> id
        Just chunks -> mapCustomCall Nothing Nothing \call ->
            setCustomToolInput
                (call.input <> textBufferToText chunks)
                call
    materializeReasoning item =
        IntMap.foldlWithKey'
            (\current index chunks ->
                mapReasoning Nothing index
                    (\part -> setReasoningPartText
                        (Just (fromMaybe "" part.text <> textBufferToText chunks))
                        part)
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

adjustOutputProgress
    :: Int
    -> (ItemProgress -> ItemProgress)
    -> OutputItemStore
    -> OutputItemStore
adjustOutputProgress index update store =
    case IntMap.lookup index store.outputSlots of
        Nothing -> store
        Just progress ->
            setOutputSlot
                index
                (update progress)
                store

-- Keep all three projections synchronized in this sole store mutation point.
setOutputSlot
    :: Int
    -> ItemProgress
    -> OutputItemStore
    -> OutputItemStore
setOutputSlot index progress store =
    OutputItemStore
        { outputSlots = IntMap.insert index progress store.outputSlots
        , outputAliases =
            addItemAliases index progress.itemValue aliasesWithoutOld
        , outputPending =
            if progress.itemDone
                then IntSet.delete index store.outputPending
                else IntSet.insert index store.outputPending
        }
  where
    aliasesWithoutOld =
        case IntMap.lookup index store.outputSlots of
            Nothing -> store.outputAliases
            Just old -> removeItemAliases index old.itemValue store.outputAliases

addItemAliases
    :: Int
    -> ResponseItem
    -> Map ItemIdentity IntSet
    -> Map ItemIdentity IntSet
addItemAliases index item aliases =
    List.foldl'
        (\current identity ->
            Map.insertWith IntSet.union
                identity
                (IntSet.singleton index)
                current)
        aliases
        (itemAliases item)

removeItemAliases
    :: Int
    -> ResponseItem
    -> Map ItemIdentity IntSet
    -> Map ItemIdentity IntSet
removeItemAliases index item aliases =
    List.foldl'
        (\current identity ->
            Map.update
                (\indexes ->
                    let remaining = IntSet.delete index indexes
                    in if IntSet.null remaining
                        then Nothing
                        else Just remaining)
                identity
                current)
        aliases
        (itemAliases item)

itemAliases :: ResponseItem -> [ItemIdentity]
itemAliases item =
    [ identity
    | (field, value) <- responseItemIdentities item
    , Just identity <- [itemIdentity (responseItemKind item) field value]
    ]

itemIdentity
    :: Text
    -> Text
    -> Text
    -> Maybe ItemIdentity
itemIdentity kind field value
    | Text.null value = Nothing
    | field == "id" = Just (ProviderItemId kind value)
    | field == "call_id" = Just (ToolCallId kind value)
    | otherwise = Nothing

streamItemIdentity :: Text -> Maybe Text -> Maybe ItemIdentity
streamItemIdentity kind = fmap (ProviderItemId kind) . nonEmpty

streamCallIdentity :: Text -> Maybe Text -> Maybe ItemIdentity
streamCallIdentity kind = fmap (ToolCallId kind) . nonEmpty

nextOutputIndex :: OutputItemStore -> Int
nextOutputIndex store =
    maybe 0 ((+ 1) . fst) (IntMap.lookupMax store.outputSlots)

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
        ReasoningItemValue
            (setReasoningSummary
                (updateAt index update reasoning.summary)
                reasoning)
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
    let Response {..} = response
    in Response {status = value, ..}

setResponseModel :: Text -> Response -> Response
setResponseModel value response =
    let Response {..} = response
    in Response {model = value, ..}

setResponseOutput :: [ResponseItem] -> Response -> Response
setResponseOutput value response =
    let Response {..} = response
    in Response {output = value, ..}
