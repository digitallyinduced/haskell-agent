-- | Typed decoding and terminal-response assembly for xAI Responses SSE.
module Agent.XAI.Stream
    ( SseDecoder
    , newSseDecoder
    , feedSseDecoder
    , finishSseDecoder
    , parseSseEvents
    , buildResponse
    ) where

import Agent.Error (ApiError(..), errorTypeFromText)
import Agent.Responses.Error (mkOpenAIError)
import Agent.Responses.ResponseMerge (mergeCompletedResponseOutput)
import Agent.Responses.SSE
    ( SseDecoder
    , feedSseDecoder
    , finishSseDecoder
    , newSseDecoder
    , parseSseEvents
    )
import Agent.Responses.Types
import Agent.XAI.Error (classifyStreamError)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text (lenientDecode)

-- | Merge streamed output-item events into the terminal completed response.
buildResponse :: [ResponseStreamEvent] -> Either ApiError Response
buildResponse events = case lastMaybe terminalResponses of
    Just terminal -> decodeMerged terminal
    Nothing -> Left (Maybe.fromMaybe missingCompletion (firstFailure events))
  where
    terminalResponses = Maybe.mapMaybe terminalResponse events
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
                (Text.take 2000 (Text.decodeUtf8With Text.lenientDecode
                    (LBS.toStrict (Aeson.encode merged))))

    missingCompletion = JsonDecodeError
        "No terminal response event found in xAI SSE stream"
        (Text.take 2000 (Text.decodeUtf8With Text.lenientDecode
            (LBS.toStrict (Aeson.encode events))))

    terminalResponse = \case
        ResponseCompletedEvent { response } -> Just response
        ResponseIncompleteEvent { response } -> Just response
        _ -> Nothing

firstFailure :: [ResponseStreamEvent] -> Maybe ApiError
firstFailure = Maybe.listToMaybe . Maybe.mapMaybe failure
  where
    failure = \case
        ResponseErrorEvent { streamError } -> Just (classifyStreamError streamError)
        ResponseNestedErrorEvent { streamError } -> Just (classifyStreamError streamError)
        ResponseFailedEvent { response } -> Just (failedResponseError response)
        _ -> Nothing

failedResponseError :: Response -> ApiError
failedResponseError response = case response.error of
    Just responseError ->
        mkOpenAIError
            (errorTypeFromText responseError.code)
            responseError.message
            (Just responseError.code)
            Nothing
    Nothing -> ConnectionError (failedResponseMessage response)

failedResponseMessage :: Response -> Text
failedResponseMessage response = case response.error of
    Just responseError -> responseError.message
    Nothing -> case response.incompleteDetails of
        Just details -> "response.failed: " <> details.reason
        Nothing -> "response.failed (no details)"

lastMaybe :: [value] -> Maybe value
lastMaybe [] = Nothing
lastMaybe [value] = Just value
lastMaybe (_ : rest) = lastMaybe rest
