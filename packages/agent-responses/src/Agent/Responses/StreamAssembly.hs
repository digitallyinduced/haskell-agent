-- | Provider-neutral terminal response assembly for Responses SSE streams.
module Agent.Responses.StreamAssembly
    ( StreamAssemblyConfig(..)
    , assembleDoneResponse
    , buildStreamResponse
    , failedResponseMessage
    ) where

import Agent.Error (ApiError(..))
import Agent.Responses.ResponseMerge
    ( mergeCompletedResponseOutput
    , mergeDoneResponse
    )
import Agent.Responses.Types
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Encoding.Error as TextEncoding (lenientDecode)

-- | Provider-specific failure classification around shared event assembly.
data StreamAssemblyConfig = StreamAssemblyConfig
    { missingCompletionMessage :: !Text
      -- ^ Error message when no terminal response or stream failure is present.
    , classifyStreamError :: !(ResponseStreamError -> ApiError)
      -- ^ Classify top-level and nested stream error events.
    , classifyFailedResponse :: !(Response -> ApiError)
      -- ^ Classify a terminal @response.failed@ event.
    }

-- | Merge streamed output items into the last terminal response. If no
-- completed or incomplete response is present, return the first typed stream
-- failure, falling back to a decode error containing a bounded event preview.
buildStreamResponse
    :: StreamAssemblyConfig
    -> [ResponseStreamEvent]
    -> Either ApiError Response
buildStreamResponse config events = case lastMaybe terminalEvents of
    Just ResponseCompletedEvent { response } -> decodeMerged response
    Just ResponseIncompleteEvent { response } -> decodeMerged response
    Just ResponseDoneEvent { responseValue } ->
        assembleDoneResponse latestLifecycleResponse doneItems responseValue
    Just _ -> Left missingCompletion
    Nothing -> Left (Maybe.fromMaybe missingCompletion (firstFailure config events))
  where
    terminalEvents = filter isTerminalEvent events
    latestLifecycleResponse =
        lastMaybe (Maybe.mapMaybe lifecycleResponse events)
    doneItems =
        [ Aeson.toJSON item
        | ResponseOutputItemDoneEvent { item } <- events
        ]

    decodeMerged completed =
        let merged = mergeCompletedResponseOutput doneItems (Aeson.toJSON completed)
        in case Aeson.fromJSON merged of
            Aeson.Success response -> Right response
            Aeson.Error err -> Left $ JsonDecodeError
                (Text.pack err)
                (jsonPreview merged)

    missingCompletion = JsonDecodeError
        config.missingCompletionMessage
        (jsonPreview events)

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

firstFailure
    :: StreamAssemblyConfig
    -> [ResponseStreamEvent]
    -> Maybe ApiError
firstFailure config = Maybe.listToMaybe . Maybe.mapMaybe failure
  where
    failure = \case
        ResponseErrorEvent { streamError } ->
            Just (config.classifyStreamError streamError)
        ResponseNestedErrorEvent { streamError } ->
            Just (config.classifyStreamError streamError)
        ResponseFailedEvent { response } ->
            Just (config.classifyFailedResponse response)
        _ -> Nothing

isTerminalEvent :: ResponseStreamEvent -> Bool
isTerminalEvent = \case
    ResponseCompletedEvent{} -> True
    ResponseDoneEvent{} -> True
    ResponseIncompleteEvent{} -> True
    _ -> False

lifecycleResponse :: ResponseStreamEvent -> Maybe Response
lifecycleResponse = \case
    ResponseCreatedEvent { response } -> Just response
    ResponseInProgressEvent { response } -> Just response
    ResponseQueuedEvent { response } -> Just response
    _ -> Nothing

-- | Best available text from a failed terminal response.
failedResponseMessage :: Response -> Text
failedResponseMessage response = case response.error of
    Just responseError -> responseError.message
    Nothing -> case response.incompleteDetails of
        Just details -> "response.failed: " <> details.reason
        Nothing -> "response.failed (no details)"

jsonPreview :: Aeson.ToJSON value => value -> Text
jsonPreview =
    Text.take 2000
        . TextEncoding.decodeUtf8With TextEncoding.lenientDecode
        . LBS.toStrict
        . Aeson.encode

lastMaybe :: [value] -> Maybe value
lastMaybe [] = Nothing
lastMaybe [value] = Just value
lastMaybe (_ : rest) = lastMaybe rest
