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
import Agent.Responses.ResponseMerge (mergeDoneResponse)
import Agent.Responses.Types
import Agent.TextBuffer
    ( TextBuffer
    , appendTextBuffer
    , textBufferFromText
    , textBufferToText
    )
import Control.Applicative ((<|>))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Encoding.Error as TextEncoding (lenientDecode)
import qualified Data.Vector as Vector

-- | Provider-specific failure classification around shared event assembly.
data StreamAssemblyConfig = StreamAssemblyConfig
    { missingCompletionMessage :: !Text
      -- ^ Error message when no terminal response or stream failure is present.
    , classifyStreamError :: !(ResponseStreamError -> ApiError)
      -- ^ Classify top-level and nested stream error events.
    , classifyFailedResponse :: !(ResponseFailure -> ApiError)
      -- ^ Classify a permissive terminal @response.failed@ payload.
    , incompleteAsFailure :: !Bool
      -- ^ Whether @response.incomplete@ should be returned as an error.
    }

data ResponseFailure = ResponseFailure
    { failureStatus            :: !(Maybe Text)
    , failureErrorType         :: !(Maybe Text)
    , failureErrorCode         :: !(Maybe Text)
    , failureErrorMessage      :: !(Maybe Text)
    , failureIncompleteDetails :: !(Maybe IncompleteDetails)
    , failureResponseValue     :: !Aeson.Value
    } deriving (Eq, Show)

data ItemProgress = ItemProgress
    { itemValue :: !Aeson.Value
    , itemDone  :: !Bool
    , itemBuffers :: !ItemBuffers
    }

data ItemBuffers = ItemBuffers
    { inputBuffer :: !(Maybe TextBuffer)
    , argumentsBuffer :: !(Maybe TextBuffer)
    , summaryBuffers :: !(IntMap TextBuffer)
    }

emptyItemBuffers :: ItemBuffers
emptyItemBuffers = ItemBuffers Nothing Nothing IntMap.empty

-- | Incremental state shared by HTTP/SSE and reusable WebSocket transports.
-- Lifecycle response objects are overlaid in wire order; output items are
-- indexed so an @output_item.done@ replaces its earlier @added@ form.
data StreamAssemblyState = StreamAssemblyState
    { responseObject :: !Aeson.Object
    , outputItems    :: !(IntMap ItemProgress)
    , identityIndexes :: !(Map Text IntSet)
    , pendingTypeIndexes :: !(Map Text IntSet)
    }

emptyStreamAssemblyState :: StreamAssemblyState
emptyStreamAssemblyState = StreamAssemblyState
    { responseObject = KeyMap.empty
    , outputItems = IntMap.empty
    , identityIndexes = Map.empty
    , pendingTypeIndexes = Map.empty
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
        updateStreamItem outputIndex False (Aeson.toJSON item) state
    ResponseOutputItemDoneEvent { item, outputIndex } ->
        updateStreamItem outputIndex True (Aeson.toJSON item) state
    ResponseCustomToolInputDeltaEvent
        { delta, streamItemId, streamCallId, streamOutputIndex } ->
            case ( delta
                 , resolveOutputIndex
                    streamOutputIndex [streamItemId, streamCallId] state
                 ) of
                (Just inputDelta, Just outputIndex) ->
                    updateCustomToolInput outputIndex
                        streamItemId
                        streamCallId
                        (AppendInput inputDelta)
                        state
                _ -> state
    ResponseCustomToolInputDoneEvent
        { inputText, streamItemId, streamCallId, streamOutputIndex } ->
            case ( inputText
                 , resolveOutputIndex
                    streamOutputIndex [streamItemId, streamCallId] state
                 ) of
                (Just finalInput, Just outputIndex) ->
                    updateCustomToolInput outputIndex
                        streamItemId
                        streamCallId
                        (SetInput finalInput)
                        state
                _ -> state
    ResponseFunctionCallArgumentsDeltaEvent
        { delta, streamItemId, streamOutputIndex } ->
            case ( delta
                 , resolveOutputIndex streamOutputIndex [streamItemId] state
                 ) of
                (Just argumentsDelta, Just outputIndex) ->
                    updateFunctionCallArguments outputIndex
                        streamItemId
                        (AppendArguments argumentsDelta)
                        state
                _ -> state
    ResponseFunctionCallArgumentsDoneEvent
        { arguments, functionName, streamItemId, streamOutputIndex } ->
            case resolveOutputIndex streamOutputIndex [streamItemId] state of
                Just outputIndex ->
                    updateFunctionCallArguments outputIndex
                        streamItemId
                        (SetArguments arguments functionName)
                        state
                Nothing -> state
    ResponseReasoningSummaryPartAddedEvent
        { streamItemId, streamOutputIndex, summaryIndex, partValue } ->
            case ( summaryIndex
                 , resolveOutputIndex streamOutputIndex [streamItemId] state
                 ) of
                (Just index, Just outputIndex) ->
                    updateReasoningSummary outputIndex
                        (fromMaybe "" streamItemId)
                        index
                        (maybe KeepSummary ReplaceSummary partValue)
                        state
                _ -> state
    ResponseReasoningSummaryTextDoneEvent
        { streamItemId, streamOutputIndex, summaryIndex, text } ->
            case ( summaryIndex
                 , text
                 , resolveOutputIndex streamOutputIndex [streamItemId] state
                 ) of
                (Just index, Just finalText, Just outputIndex) ->
                    updateReasoningSummary outputIndex
                        (fromMaybe "" streamItemId)
                        index
                        (SetSummaryText finalText)
                        state
                _ -> state
    OtherResponseStreamEvent
        { otherEventType = EventReasoningSummaryTextDelta
        , eventExtraFields
        } ->
            case ( intField "summary_index" eventExtraFields
                 , textField "delta" eventExtraFields
                 ) of
                (Just summaryIndex, Just delta) ->
                    case resolveOutputIndex
                            (intField "output_index" eventExtraFields)
                            [textField "item_id" eventExtraFields]
                            state of
                        Just outputIndex ->
                            updateReasoningSummary outputIndex
                                (fromMaybe ""
                                    (textField "item_id" eventExtraFields))
                                summaryIndex
                                (AppendSummaryText delta)
                                state
                        Nothing -> state
                _ -> state
    _ -> state

-- | Assemble one terminal lifecycle event into the canonical strict response.
-- Codex lifecycle frames can omit fields unrelated to that event, so only the
-- response id remains mandatory here.
finishStreamResponse
    :: Maybe Text
    -> StreamAssemblyState
    -> ResponseStreamEvent
    -> Either ApiError Response
finishStreamResponse modelHint state terminalEvent = do
    terminalStatus <- case terminalEvent of
        ResponseCompletedEvent{} -> Right "completed"
        ResponseDoneEvent{} -> Right "completed"
        ResponseIncompleteEvent{} -> Right "incomplete"
        ResponseFailedEvent{} -> Right "failed"
        _ -> Left $ JsonDecodeError
            "Cannot assemble a non-terminal response event"
            (jsonPreview terminalEvent)
    let terminalFragment = responseValueFor terminalEvent
        -- Only trust a status carried by the terminal fragment when it is
        -- itself a terminal status. The socket-death recovery path
        -- (finishAssembledIncomplete) feeds the accumulated stream object back
        -- in as the fragment, and that object still holds the non-terminal
        -- "in_progress"/"queued" status copied from the response.created frame.
        -- Preserving it there would leak a running status into a finished
        -- response and make a dropped connection look like a completed turn.
        withStatus = case terminalFragment of
            Just (Aeson.Object object)
                | fragmentHasTerminalStatus object -> state.responseObject
            _ -> KeyMap.insert "status" (Aeson.String terminalStatus)
                    state.responseObject
        withDefaults =
            insertMissing "object" (Aeson.String "response")
                $ insertMissing "model" (Aeson.String (fromMaybe "" modelHint))
                $ insertMissing "created_at" (Aeson.Number 0)
                $ withStatus
        withOutput =
            KeyMap.insert "output"
                (Aeson.Array (Vector.fromList (assembledOutput state withDefaults)))
                withDefaults
        assembled = Aeson.Object withOutput
    case KeyMap.lookup "id" withOutput of
        Just (Aeson.String responseId) | not (Text.null responseId) ->
            case Aeson.fromJSON assembled of
                Aeson.Success response -> Right response
                Aeson.Error err -> Left $ JsonDecodeError
                    (Text.pack err)
                    (jsonPreview assembled)
        _ -> Left $ JsonDecodeError
            "Streamed response did not contain a response id"
            (jsonPreview assembled)

-- | Assemble the current stream state as an incomplete response. Used when the
-- provider emits @response.incomplete@ or the socket dies after output items
-- were already collected.
finishAssembledIncomplete
    :: Maybe Text
    -> StreamAssemblyState
    -> Either ApiError Response
finishAssembledIncomplete modelHint state =
    finishStreamResponse modelHint state
        (ResponseIncompleteEvent
            (Aeson.Object state.responseObject)
            Nothing
            KeyMap.empty)

-- | Build a response without a request-model hint. This remains the shared
-- entry point used by provider SSE transports.
buildStreamResponse
    :: StreamAssemblyConfig
    -> [ResponseStreamEvent]
    -> Either ApiError Response
buildStreamResponse config =
    buildStreamResponseWithModel config Nothing

-- | Build a response while using the originating request model if the
-- provider's partial lifecycle frames omit it.
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
        (jsonPreview events)
    go state (event : rest) =
        let nextState = applyStreamEvent state event
        in case event of
            ResponseCompletedEvent{} ->
                finishStreamResponse modelHint nextState event
            ResponseDoneEvent{} ->
                finishStreamResponse modelHint nextState event
            ResponseIncompleteEvent{} ->
                if config.incompleteAsFailure
                    then Left (config.classifyFailedResponse
                        ((responseFailureFromState nextState)
                            { failureStatus = Just "incomplete" }))
                    else finishStreamResponse modelHint nextState event
            ResponseFailedEvent{} ->
                Left (config.classifyFailedResponse
                    ((responseFailureFromState nextState)
                        { failureStatus = Just "failed" }))
            ResponseErrorEvent { streamError } ->
                Left (config.classifyStreamError streamError)
            ResponseNestedErrorEvent { streamError } ->
                Left (config.classifyStreamError streamError)
            _ -> go nextState rest

-- | Compatibility wrapper for callers that already hold a complete base
-- response plus streamed done items.
assembleDoneResponse
    :: Maybe Response
    -> [Aeson.Value]
    -> Aeson.Value
    -> Either ApiError Response
assembleDoneResponse baseResponse doneItems doneResponse =
    let merged = mergeDoneResponse
            (Aeson.toJSON <$> baseResponse)
            doneItems
            doneResponse
    in case Aeson.fromJSON merged of
        Aeson.Success response -> Right response
        Aeson.Error err -> Left $ JsonDecodeError
            (Text.pack err)
            (jsonPreview merged)

responseFragmentHasOutput :: Aeson.Value -> Bool
responseFragmentHasOutput (Aeson.Object object) =
    case KeyMap.lookup "output" object of
        Just (Aeson.Array output) -> not (Vector.null output)
        _ -> False
responseFragmentHasOutput _ = False

-- | Best available text from a failed terminal response.
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
    let value = Aeson.Object state.responseObject
        parseOptional fieldName = case KeyMap.lookup fieldName state.responseObject of
            Just fieldValue -> case Aeson.fromJSON fieldValue of
                Aeson.Success parsed -> Just parsed
                Aeson.Error _ -> Nothing
            Nothing -> Nothing
        nestedText objectName fieldName = do
            Aeson.Object object <-
                KeyMap.lookup objectName state.responseObject
            textField fieldName object
    in ResponseFailure
        { failureStatus = parseOptional "status"
        , failureErrorType = nestedText "error" "type"
        , failureErrorCode = nestedText "error" "code"
        , failureErrorMessage = nestedText "error" "message"
        , failureIncompleteDetails = parseOptional "incomplete_details"
        , failureResponseValue = value
        }

overlayLifecycle :: Aeson.Value -> StreamAssemblyState -> StreamAssemblyState
overlayLifecycle (Aeson.Object fragment) state =
    state
        { responseObject =
            KeyMap.foldrWithKey KeyMap.insert state.responseObject fragment
        }
overlayLifecycle _ state = state

updateItem
    :: Int
    -> Bool
    -> Aeson.Value
    -> StreamAssemblyState
    -> StreamAssemblyState
updateItem outputIndex done newValue state =
    alterProgress outputIndex (Just . mergeProgress) state
  where
    mergeProgress Nothing = ItemProgress newValue done emptyItemBuffers
    mergeProgress (Just old) = ItemProgress
        { itemValue = mergeObjects old.itemValue newValue
        , itemDone = old.itemDone || done
        , itemBuffers = clearOverlaidBuffers newValue old.itemBuffers
        }

alterProgress
    :: Int
    -> (Maybe ItemProgress -> Maybe ItemProgress)
    -> StreamAssemblyState
    -> StreamAssemblyState
alterProgress outputIndex update state =
    let oldProgress = IntMap.lookup outputIndex state.outputItems
        newProgress = update oldProgress
        newItems = IntMap.alter (const newProgress) outputIndex state.outputItems
        withNew = updateProgressIndexes
            outputIndex oldProgress newProgress state
    in withNew { outputItems = newItems }

-- Content deltas do not change an existing object's identity, type, or done
-- status. Avoid rebuilding those secondary indexes on the hot path; creation
-- and recovery from a non-object value still go through the checked updater.
alterItemContent
    :: Int
    -> (Maybe ItemProgress -> ItemProgress)
    -> StreamAssemblyState
    -> StreamAssemblyState
alterItemContent outputIndex update state =
    case IntMap.lookup outputIndex state.outputItems of
        Just old@ItemProgress { itemValue = Aeson.Object{} } ->
            state
                { outputItems =
                    IntMap.insert outputIndex (update (Just old)) state.outputItems
                }
        _ ->
            alterProgress outputIndex (Just . update) state

updateProgressIndexes
    :: Int
    -> Maybe ItemProgress
    -> Maybe ItemProgress
    -> StreamAssemblyState
    -> StreamAssemblyState
updateProgressIndexes outputIndex oldProgress newProgress state =
    state
        { identityIndexes =
            foldl'
                (\indexes identity -> addIndex identity outputIndex indexes)
                (foldl'
                    (\indexes identity ->
                        removeIndex identity outputIndex indexes)
                    state.identityIndexes
                    removedIdentities)
                addedIdentities
        , pendingTypeIndexes =
            maybe id
                (\itemType -> addIndex itemType outputIndex)
                newPending
            $ maybe id
                (\itemType -> removeIndex itemType outputIndex)
                oldPending
            $ state.pendingTypeIndexes
        }
  where
    oldIdentities = maybe [] (itemIdentities . (.itemValue)) oldProgress
    newIdentities = maybe [] (itemIdentities . (.itemValue)) newProgress
    removedIdentities =
        [identity | identity <- oldIdentities, identity `notElem` newIdentities]
    addedIdentities =
        [identity | identity <- newIdentities, identity `notElem` oldIdentities]
    oldPendingCandidate = pendingType =<< oldProgress
    newPendingCandidate = pendingType =<< newProgress
    (oldPending, newPending)
        | oldPendingCandidate == newPendingCandidate = (Nothing, Nothing)
        | otherwise = (oldPendingCandidate, newPendingCandidate)
    pendingType progress
        | progress.itemDone = Nothing
        | otherwise = objectTextField "type" progress.itemValue

addIndex :: Ord key => key -> Int -> Map key IntSet -> Map key IntSet
addIndex key outputIndex =
    Map.insertWith IntSet.union key (IntSet.singleton outputIndex)

removeIndex :: Ord key => key -> Int -> Map key IntSet -> Map key IntSet
removeIndex key outputIndex =
    Map.update
        (\indexes ->
            let remaining = IntSet.delete outputIndex indexes
            in if IntSet.null remaining then Nothing else Just remaining)
        key

clearOverlaidBuffers :: Aeson.Value -> ItemBuffers -> ItemBuffers
clearOverlaidBuffers (Aeson.Object overlay) buffers =
    buffers
        { inputBuffer =
            if KeyMap.member "input" overlay
                then Nothing
                else buffers.inputBuffer
        , argumentsBuffer =
            if KeyMap.member "arguments" overlay
                then Nothing
                else buffers.argumentsBuffer
        , summaryBuffers =
            if KeyMap.member "summary" overlay
                then IntMap.empty
                else buffers.summaryBuffers
        }
clearOverlaidBuffers _ _ = emptyItemBuffers

updateStreamItem
    :: Maybe Int
    -> Bool
    -> Aeson.Value
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
    setMinimum =<< Map.lookup wanted state.identityIndexes

findItemIndex :: Aeson.Value -> StreamAssemblyState -> Maybe Int
findItemIndex value state =
    firstJust
        [ findIdentityIndex identity state
        | identity <- itemIdentities value
        ]

findPendingItemIndex :: Aeson.Value -> StreamAssemblyState -> Maybe Int
findPendingItemIndex value state =
    objectTextField "type" value
        >>= (\wantedType ->
            setMinimum =<< Map.lookup wantedType state.pendingTypeIndexes)

setMinimum :: IntSet -> Maybe Int
setMinimum indexes = fst <$> IntSet.minView indexes

nextOutputIndex :: StreamAssemblyState -> Int
nextOutputIndex state =
    maybe 0 ((+ 1) . fst) (IntMap.lookupMax state.outputItems)

itemIdentities :: Aeson.Value -> [Text]
itemIdentities = \case
    Aeson.Object object ->
        foldMap
            (\fieldName -> maybe [] pure (textField fieldName object))
            ["id", "call_id"]
    _ -> []

objectTextField :: Text -> Aeson.Value -> Maybe Text
objectTextField fieldName = \case
    Aeson.Object object -> textField fieldName object
    _ -> Nothing

firstJust :: [Maybe value] -> Maybe value
firstJust = foldr (<|>) Nothing

nonEmpty :: Maybe Text -> Maybe Text
nonEmpty value = value >>= \text ->
    if Text.null text then Nothing else Just text

data InputUpdate
    = AppendInput !Text
    | SetInput !Text

updateCustomToolInput
    :: Int
    -> Maybe Text
    -> Maybe Text
    -> InputUpdate
    -> StreamAssemblyState
    -> StreamAssemblyState
updateCustomToolInput outputIndex itemId callId updateInput state =
    alterItemContent outputIndex updateProgress state
  where
    baseObject =
        maybe id
            (KeyMap.insert "id" . Aeson.String)
            itemId
        $ maybe id
            (KeyMap.insert "call_id" . Aeson.String)
            callId
        $ KeyMap.fromList
            [ ("type", Aeson.String "custom_tool_call")
            , ("input", Aeson.String "")
            ]
    updateProgress Nothing =
        applyInputUpdate updateInput
            (ItemProgress (Aeson.Object baseObject) False emptyItemBuffers)
    updateProgress (Just progress) =
        applyInputUpdate updateInput
            progress
                { itemValue = case progress.itemValue of
                    Aeson.Object object -> Aeson.Object object
                    _ -> Aeson.Object baseObject
                }

applyInputUpdate :: InputUpdate -> ItemProgress -> ItemProgress
applyInputUpdate inputUpdate progress =
    case inputUpdate of
        AppendInput delta ->
            let current = case progress.itemBuffers.inputBuffer of
                    Just buffered -> buffered
                    Nothing ->
                        textBufferFromText
                            (fromMaybe "" (objectTextField "input" progress.itemValue))
            in progress
                { itemBuffers = progress.itemBuffers
                    { inputBuffer = Just (appendTextBuffer delta current) }
                }
        SetInput input ->
            progress
                { itemValue = mapObject
                    (KeyMap.insert "input" (Aeson.String input))
                    progress.itemValue
                , itemBuffers = progress.itemBuffers { inputBuffer = Nothing }
                }

data SummaryUpdate
    = KeepSummary
    | ReplaceSummary !Aeson.Value
    | SetSummaryText !Text
    | AppendSummaryText !Text

updateReasoningSummary
    :: Int
    -> Text
    -> Int
    -> SummaryUpdate
    -> StreamAssemblyState
    -> StreamAssemblyState
updateReasoningSummary outputIndex itemId summaryIndex updatePart state =
    alterItemContent outputIndex updateProgress state
  where
    baseObject = KeyMap.fromList
        [ ("type", Aeson.String "reasoning")
        , ("id", Aeson.String itemId)
        , ("summary", Aeson.Array Vector.empty)
        ]
    updateProgress Nothing =
        applySummaryUpdate summaryIndex updatePart
            (ItemProgress
                (Aeson.Object (ensureSummaryIndex summaryIndex baseObject))
                False
                emptyItemBuffers)
    updateProgress (Just progress) =
        applySummaryUpdate summaryIndex updatePart
            progress
                { itemValue = case progress.itemValue of
                    Aeson.Object object ->
                        Aeson.Object (ensureSummaryIndex summaryIndex object)
                    _ ->
                        Aeson.Object (ensureSummaryIndex summaryIndex baseObject)
                }

ensureSummaryIndex :: Int -> Aeson.Object -> Aeson.Object
ensureSummaryIndex summaryIndex object =
    let current = case KeyMap.lookup "summary" object of
            Just (Aeson.Array values) -> values
            _ -> Vector.empty
        updated = updateVectorAt summaryIndex id current
    in KeyMap.insert "summary" (Aeson.Array updated) object

applySummaryUpdate :: Int -> SummaryUpdate -> ItemProgress -> ItemProgress
applySummaryUpdate summaryIndex summaryUpdate progress
    | summaryIndex < 0 = progress
    | otherwise =
        case summaryUpdate of
            KeepSummary -> progress
            ReplaceSummary value ->
                setSummaryPart value progress
            SetSummaryText text ->
                setSummaryPart
                    (setObjectText text (summaryPart summaryIndex progress.itemValue))
                    progress
            AppendSummaryText delta ->
                let existingBuffer =
                        fromMaybe
                            (textBufferFromText
                                (fromMaybe ""
                                    (objectTextField "text"
                                        (summaryPart summaryIndex progress.itemValue))))
                            (IntMap.lookup summaryIndex
                                progress.itemBuffers.summaryBuffers)
                in progress
                    { itemBuffers = progress.itemBuffers
                        { summaryBuffers =
                            IntMap.insert
                                summaryIndex
                                (appendTextBuffer delta existingBuffer)
                                progress.itemBuffers.summaryBuffers
                        }
                    }
  where
    setSummaryPart value current =
        current
            { itemValue = mapObject
                (\object ->
                    let summary = case KeyMap.lookup "summary" object of
                            Just (Aeson.Array values) -> values
                            _ -> Vector.empty
                    in KeyMap.insert "summary"
                        (Aeson.Array
                            (updateVectorAt summaryIndex (const value) summary))
                        object)
                current.itemValue
            , itemBuffers = current.itemBuffers
                { summaryBuffers =
                    IntMap.delete summaryIndex current.itemBuffers.summaryBuffers
                }
            }

summaryPart :: Int -> Aeson.Value -> Aeson.Value
summaryPart summaryIndex = \case
    Aeson.Object object ->
        case KeyMap.lookup "summary" object of
            Just (Aeson.Array summary)
                | summaryIndex >= 0
                , summaryIndex < Vector.length summary ->
                    summary Vector.! summaryIndex
            _ -> emptySummaryPart
    _ -> emptySummaryPart
  where
    emptySummaryPart =
        Aeson.object ["type" Aeson..= ("summary_text" :: Text)]

assembledOutput :: StreamAssemblyState -> Aeson.Object -> [Aeson.Value]
assembledOutput state response =
    map (.itemValue) . IntMap.elems $
        IntMap.unionWith combine
            (materializeProgress <$> state.outputItems)
            terminalItems
  where
    terminalItems = IntMap.fromList
        [ (index, ItemProgress value True emptyItemBuffers)
        | (index, value) <- zip [0 ..] finalItems
        ]
    finalItems = case KeyMap.lookup "output" response of
        Just (Aeson.Array values) -> Vector.toList values
        _ -> []
    combine streamed terminal
        | streamed.itemDone =
            ItemProgress
                (mergeObjects terminal.itemValue streamed.itemValue)
                True
                emptyItemBuffers
        | otherwise =
            ItemProgress
                (mergeObjects streamed.itemValue terminal.itemValue)
                True
                emptyItemBuffers

materializeProgress :: ItemProgress -> ItemProgress
materializeProgress progress =
    progress
        { itemValue =
            materializeSummaries progress.itemBuffers.summaryBuffers
                $ maybe id
                    (setTextField "arguments" . textBufferToText)
                    progress.itemBuffers.argumentsBuffer
                $ maybe id
                    (setTextField "input" . textBufferToText)
                    progress.itemBuffers.inputBuffer
                $ progress.itemValue
        , itemBuffers = emptyItemBuffers
        }
  where
    setTextField fieldName text =
        mapObject (KeyMap.insert fieldName (Aeson.String text))

    materializeSummaries buffers value =
        IntMap.foldlWithKey'
            (\value summaryIndex buffer ->
                mapObject
                    (\object ->
                        let summary = case KeyMap.lookup "summary" object of
                                Just (Aeson.Array values) -> values
                                _ -> Vector.empty
                        in KeyMap.insert "summary"
                            (Aeson.Array
                                (updateVectorAt summaryIndex
                                    (setObjectText (textBufferToText buffer))
                                    summary))
                            object)
                    value)
            value
            buffers

-- | Does a response fragment already carry a terminal lifecycle status? Only
-- @completed@/@incomplete@/@failed@/@cancelled@ count; a leftover
-- @in_progress@/@queued@ status from an earlier streaming frame must be
-- overwritten with the status implied by the terminal event.
fragmentHasTerminalStatus :: Aeson.Object -> Bool
fragmentHasTerminalStatus object =
    case KeyMap.lookup "status" object of
        Just (Aeson.String status) ->
            status `elem` ["completed", "incomplete", "failed", "cancelled"]
        _ -> False

responseValueFor :: ResponseStreamEvent -> Maybe Aeson.Value
responseValueFor = \case
    ResponseCreatedEvent { responseValue } -> Just responseValue
    ResponseInProgressEvent { responseValue } -> Just responseValue
    ResponseQueuedEvent { responseValue } -> Just responseValue
    ResponseCompletedEvent { responseValue } -> Just responseValue
    ResponseDoneEvent { responseValue } -> Just responseValue
    ResponseFailedEvent { responseValue } -> Just responseValue
    ResponseIncompleteEvent { responseValue } -> Just responseValue
    _ -> Nothing

mergeObjects :: Aeson.Value -> Aeson.Value -> Aeson.Value
mergeObjects (Aeson.Object base) (Aeson.Object overlay) =
    Aeson.Object (KeyMap.foldrWithKey KeyMap.insert base overlay)
mergeObjects _ overlay = overlay

insertMissing :: Key.Key -> Aeson.Value -> Aeson.Object -> Aeson.Object
insertMissing key value object
    | KeyMap.member key object = object
    | otherwise = KeyMap.insert key value object

data ArgumentsUpdate
    = AppendArguments !Text
    | SetArguments !(Maybe Text) !(Maybe Text)

updateFunctionCallArguments
    :: Int
    -> Maybe Text
    -> ArgumentsUpdate
    -> StreamAssemblyState
    -> StreamAssemblyState
updateFunctionCallArguments outputIndex itemId updateArgs state =
    alterItemContent outputIndex updateProgress state
  where
    baseObject =
        maybe id
            (KeyMap.insert "id" . Aeson.String)
            itemId
        $ KeyMap.fromList
            [ ("type", Aeson.String "function_call")
            , ("arguments", Aeson.String "")
            ]
    updateProgress Nothing =
        applyArgumentsUpdate updateArgs
            (ItemProgress (Aeson.Object baseObject) False emptyItemBuffers)
    updateProgress (Just progress) =
        applyArgumentsUpdate updateArgs
            progress
                { itemValue = case progress.itemValue of
                    Aeson.Object object -> Aeson.Object object
                    _ -> Aeson.Object baseObject
                }

applyArgumentsUpdate :: ArgumentsUpdate -> ItemProgress -> ItemProgress
applyArgumentsUpdate argumentsUpdate progress =
    case argumentsUpdate of
        AppendArguments delta ->
            let current = case progress.itemBuffers.argumentsBuffer of
                    Just buffered -> buffered
                    Nothing ->
                        textBufferFromText
                            (fromMaybe ""
                                (objectTextField "arguments" progress.itemValue))
            in progress
                { itemBuffers = progress.itemBuffers
                    { argumentsBuffer = Just (appendTextBuffer delta current) }
                }
        SetArguments arguments functionName ->
            progress
                { itemValue =
                    mapObject
                        (maybe id
                            (KeyMap.insert "name" . Aeson.String)
                            functionName
                        . maybe id
                            (KeyMap.insert "arguments" . Aeson.String)
                            arguments)
                        progress.itemValue
                , itemBuffers = progress.itemBuffers
                    { argumentsBuffer =
                        case arguments of
                            Just _ -> Nothing
                            Nothing -> progress.itemBuffers.argumentsBuffer
                    }
                }

mapObject :: (Aeson.Object -> Aeson.Object) -> Aeson.Value -> Aeson.Value
mapObject update = \case
    Aeson.Object object -> Aeson.Object (update object)
    value -> value

setObjectText :: Text -> Aeson.Value -> Aeson.Value
setObjectText text = \case
    Aeson.Object object ->
        Aeson.Object (KeyMap.insert "text" (Aeson.String text) object)
    _ -> Aeson.object
        [ "type" Aeson..= ("summary_text" :: Text)
        , "text" Aeson..= text
        ]

updateVectorAt
    :: Int
    -> (Aeson.Value -> Aeson.Value)
    -> Vector.Vector Aeson.Value
    -> Vector.Vector Aeson.Value
updateVectorAt index update values
    | index < 0 = values
    | index < Vector.length values =
        values Vector.// [(index, update (values Vector.! index))]
    | otherwise =
        let padding = Vector.replicate
                (index - Vector.length values)
                (Aeson.object ["type" Aeson..= ("summary_text" :: Text)])
        in values
            <> padding
            <> Vector.singleton
                (update (Aeson.object ["type" Aeson..= ("summary_text" :: Text)]))

textField :: Text -> Aeson.Object -> Maybe Text
textField name object =
    case KeyMap.lookup (Key.fromText name) object of
        Just (Aeson.String value) -> Just value
        _ -> Nothing

intField :: Text -> Aeson.Object -> Maybe Int
intField name object =
    case KeyMap.lookup (Key.fromText name) object of
        Just value -> case Aeson.fromJSON value of
            Aeson.Success int -> Just int
            Aeson.Error _ -> Nothing
        Nothing -> Nothing

jsonPreview :: Aeson.ToJSON value => value -> Text
jsonPreview =
    Text.take 2000
        . TextEncoding.decodeUtf8With TextEncoding.lenientDecode
        . LBS.toStrict
        . Aeson.encode
