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
import qualified Agent.Responses.Codec as ResponsesCodec
import Agent.Responses.Types
import Control.Applicative ((<|>))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)
import Data.Maybe (fromMaybe)
import Data.Scientific (floatingOrInteger)
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
    }

-- | Incremental state shared by HTTP/SSE and reusable WebSocket transports.
-- Lifecycle response objects are overlaid in wire order; output items are
-- indexed so an @output_item.done@ replaces its earlier @added@ form.
data StreamAssemblyState = StreamAssemblyState
    { responseObject :: !Aeson.Object
    , outputItems    :: !(IntMap ItemProgress)
    }

emptyStreamAssemblyState :: StreamAssemblyState
emptyStreamAssemblyState = StreamAssemblyState
    { responseObject = KeyMap.empty
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
                        (appendInput inputDelta)
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
                        (setInput finalInput)
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
                        (appendArguments argumentsDelta)
                        state
                _ -> state
    ResponseFunctionCallArgumentsDoneEvent
        { arguments, functionName, streamItemId, streamOutputIndex } ->
            case resolveOutputIndex streamOutputIndex [streamItemId] state of
                Just outputIndex ->
                    updateFunctionCallArguments outputIndex
                        streamItemId
                        (setArguments arguments functionName)
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
                        (maybe id const partValue)
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
                        (setObjectText finalText)
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
                                (appendObjectText delta)
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
            case ResponsesCodec.decodeResponse
                    (LBS.toStrict (Aeson.encode assembled)) of
                Right response -> Right response
                Left err -> Left $ JsonDecodeError
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
    in case ResponsesCodec.decodeResponse
            (LBS.toStrict (Aeson.encode merged)) of
        Right response -> Right response
        Left err -> Left $ JsonDecodeError
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
        nestedText objectName fieldName = do
            Aeson.Object object <-
                KeyMap.lookup objectName state.responseObject
            textField fieldName object
    in ResponseFailure
        { failureStatus = textField "status" state.responseObject
        , failureErrorType = nestedText "error" "type"
        , failureErrorCode = nestedText "error" "code"
        , failureErrorMessage = nestedText "error" "message"
        , failureIncompleteDetails = do
            Aeson.Object details <-
                KeyMap.lookup "incomplete_details" state.responseObject
            reason <- textField "reason" details
            pure IncompleteDetails
                { reason
                , extraFields = KeyMap.empty
                }
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
    state
        { outputItems =
            IntMap.alter
                (Just . mergeProgress)
                outputIndex
                state.outputItems
        }
  where
    mergeProgress Nothing = ItemProgress newValue done
    mergeProgress (Just old) = ItemProgress
        { itemValue = mergeObjects old.itemValue newValue
        , itemDone = old.itemDone || done
        }

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
    fst <$> IntMap.lookupMin
        (IntMap.filter (matchesIdentity wanted . (.itemValue)) state.outputItems)
  where
    matchesIdentity wanted = \case
        Aeson.Object object ->
            textField "id" object == Just wanted
                || textField "call_id" object == Just wanted
        _ -> False

findItemIndex :: Aeson.Value -> StreamAssemblyState -> Maybe Int
findItemIndex value state =
    firstJust
        [ findIdentityIndex identity state
        | identity <- itemIdentities value
        ]

findPendingItemIndex :: Aeson.Value -> StreamAssemblyState -> Maybe Int
findPendingItemIndex value state =
    case wantedType of
        Nothing -> Nothing
        Just _ ->
            fst <$> IntMap.lookupMin
                (IntMap.filter matchesPending state.outputItems)
  where
    wantedType = objectTextField "type" value
    matchesPending progress =
        not progress.itemDone
            && objectTextField "type" progress.itemValue == wantedType

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

updateCustomToolInput
    :: Int
    -> Maybe Text
    -> Maybe Text
    -> (Aeson.Object -> Aeson.Object)
    -> StreamAssemblyState
    -> StreamAssemblyState
updateCustomToolInput outputIndex itemId callId updateInput state =
    state
        { outputItems =
            IntMap.alter
                (Just . updateProgress)
                outputIndex
                state.outputItems
        }
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
        ItemProgress (Aeson.Object (updateInput baseObject)) False
    updateProgress (Just progress) =
        progress
            { itemValue = case progress.itemValue of
                Aeson.Object object -> Aeson.Object (updateInput object)
                _ -> Aeson.Object (updateInput baseObject)
            }

updateReasoningSummary
    :: Int
    -> Text
    -> Int
    -> (Aeson.Value -> Aeson.Value)
    -> StreamAssemblyState
    -> StreamAssemblyState
updateReasoningSummary outputIndex itemId summaryIndex updatePart state =
    state
        { outputItems =
            IntMap.alter
                (Just . updateProgress)
                outputIndex
                state.outputItems
        }
  where
    baseObject = KeyMap.fromList
        [ ("type", Aeson.String "reasoning")
        , ("id", Aeson.String itemId)
        , ("summary", Aeson.Array Vector.empty)
        ]
    updateProgress Nothing =
        ItemProgress (Aeson.Object (updateSummary baseObject)) False
    updateProgress (Just progress) =
        progress
            { itemValue = case progress.itemValue of
                Aeson.Object object -> Aeson.Object (updateSummary object)
                _ -> Aeson.Object (updateSummary baseObject)
            }
    updateSummary object =
        let current = case KeyMap.lookup "summary" object of
                Just (Aeson.Array values) -> values
                _ -> Vector.empty
            updated = updateVectorAt summaryIndex updatePart current
        in KeyMap.insert "summary" (Aeson.Array updated) object

assembledOutput :: StreamAssemblyState -> Aeson.Object -> [Aeson.Value]
assembledOutput state response =
    map (.itemValue) . IntMap.elems $
        IntMap.unionWith combine
            state.outputItems
            terminalItems
  where
    terminalItems = IntMap.fromList
        [ (index, ItemProgress value True)
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
        | otherwise =
            ItemProgress
                (mergeObjects streamed.itemValue terminal.itemValue)
                True

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

appendInput :: Text -> Aeson.Object -> Aeson.Object
appendInput delta object =
    let current = fromMaybe "" (textField "input" object)
    in KeyMap.insert "input" (Aeson.String (current <> delta)) object

setInput :: Text -> Aeson.Object -> Aeson.Object
setInput input =
    KeyMap.insert "input" (Aeson.String input)

updateFunctionCallArguments
    :: Int
    -> Maybe Text
    -> (Aeson.Object -> Aeson.Object)
    -> StreamAssemblyState
    -> StreamAssemblyState
updateFunctionCallArguments outputIndex itemId updateArgs state =
    state
        { outputItems =
            IntMap.alter
                (Just . updateProgress)
                outputIndex
                state.outputItems
        }
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
        ItemProgress (Aeson.Object (updateArgs baseObject)) False
    updateProgress (Just progress) =
        progress
            { itemValue = case progress.itemValue of
                Aeson.Object object -> Aeson.Object (updateArgs object)
                _ -> Aeson.Object (updateArgs baseObject)
            }

appendArguments :: Text -> Aeson.Object -> Aeson.Object
appendArguments delta object =
    let current = fromMaybe "" (textField "arguments" object)
    in KeyMap.insert "arguments" (Aeson.String (current <> delta)) object

setArguments :: Maybe Text -> Maybe Text -> Aeson.Object -> Aeson.Object
setArguments arguments functionName object =
    maybe id (KeyMap.insert "name" . Aeson.String) functionName $
        maybe id (KeyMap.insert "arguments" . Aeson.String) arguments
            object

setObjectText :: Text -> Aeson.Value -> Aeson.Value
setObjectText text = \case
    Aeson.Object object ->
        Aeson.Object (KeyMap.insert "text" (Aeson.String text) object)
    _ -> Aeson.object
        [ "type" Aeson..= ("summary_text" :: Text)
        , "text" Aeson..= text
        ]

appendObjectText :: Text -> Aeson.Value -> Aeson.Value
appendObjectText delta = \case
    Aeson.Object object ->
        let current = fromMaybe "" (textField "text" object)
        in Aeson.Object
            (KeyMap.insert "text" (Aeson.String (current <> delta)) object)
    _ -> Aeson.object
        [ "type" Aeson..= ("summary_text" :: Text)
        , "text" Aeson..= delta
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
        Just (Aeson.Number value) ->
            either
                (const Nothing)
                Just
                (floatingOrInteger value :: Either Double Int)
        _ -> Nothing

jsonPreview :: Aeson.ToJSON value => value -> Text
jsonPreview =
    Text.take 2000
        . TextEncoding.decodeUtf8With TextEncoding.lenientDecode
        . LBS.toStrict
        . Aeson.encode
