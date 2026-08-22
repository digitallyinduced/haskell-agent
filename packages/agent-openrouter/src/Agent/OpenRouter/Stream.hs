-- | Typed decoding and terminal-response assembly for OpenRouter Responses SSE.
module Agent.OpenRouter.Stream
    ( SseDecoder
    , newSseDecoder
    , feedSseDecoder
    , finishSseDecoder
    , parseSseEvents
    , buildResponse
    ) where

import Agent.Error (ApiError(..))
import Agent.Responses.SSE
    ( SseDecoder
    , feedSseDecoder
    , finishSseDecoder
    , newSseDecoder
    , parseSseEvents
    )
import Agent.Responses.ResponseMerge (mergeCompletedResponseOutput)
import Agent.Responses.Types
import Agent.OpenRouter.Error (classifyStreamError)
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
        "No terminal response event found in OpenRouter SSE stream"
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
        ResponseFailedEvent { response } -> Just (ConnectionError (failedResponseMessage response))
        _ -> Nothing

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
