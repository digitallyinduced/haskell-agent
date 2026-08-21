-- | Typed decoding and terminal-response assembly for xAI Responses SSE.
module Agent.XAI.Stream
    ( parseSseEvents
    , buildResponse
    ) where

import Agent.Error (ApiError(..), errorTypeFromText)
import Agent.Responses.Error (mkOpenAIError)
import Agent.Responses.ResponseMerge (mergeCompletedResponseOutput)
import qualified Agent.Responses.Codec as ResponsesCodec
import Agent.Responses.Types
import Agent.XAI.Error (classifyStreamError)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text (lenientDecode)

-- | Decode an SSE body into the canonical typed Responses event union.
-- The discriminator may be supplied by the SSE @event:@ line, by the JSON
-- @type@ member, or by both when they agree.
parseSseEvents :: Text -> Either ApiError [ResponseStreamEvent]
parseSseEvents sseText = Maybe.catMaybes <$> traverse parseBlock blocks
  where
    normalized = Text.replace "\r\n" "\n" sseText
    blocks = filter (not . Text.null . Text.strip) (Text.splitOn "\n\n" normalized)

    parseBlock block
        | Text.null dataText = Right Nothing
        | Text.strip dataText == "[DONE]" = Right Nothing
        | otherwise = Just <$> decodeEvent eventType dataText
      where
        blockLines = Text.lines block
        eventType = Maybe.listToMaybe
            [ Text.strip (Text.drop 6 line)
            | line <- blockLines
            , "event:" `Text.isPrefixOf` line
            ]
        dataText = Text.intercalate "\n"
            [ Text.strip (Text.drop 5 line)
            | line <- blockLines
            , "data:" `Text.isPrefixOf` line
            ]

    decodeEvent eventType dataText = do
        value <- case Aeson.eitherDecodeStrict' (Text.encodeUtf8 dataText) of
            Left err -> Left (decodeError (Text.pack err) dataText)
            Right value -> Right value
        let decoded = case eventType of
                Just suppliedType ->
                    ResponsesCodec.decodeResponseStreamEventWithType suppliedType value
                Nothing -> case ResponsesCodec.decodeResponseStreamEventValue value of
                    Aeson.Success event -> Right event
                    Aeson.Error err -> Left err
        case decoded of
            Left err -> Left (decodeError (Text.pack err) dataText)
            Right event -> Right event

    decodeError message body = JsonDecodeError message (Text.take 2000 body)

-- | Merge streamed output-item events into the terminal completed response.
buildResponse :: [ResponseStreamEvent] -> Either ApiError Response
buildResponse events = case lastMaybe completedResponses of
    Just completed -> decodeMerged completed
    Nothing -> Left (Maybe.fromMaybe missingCompletion (firstFailure events))
  where
    completedResponses =
        [ response
        | ResponseCompletedEvent { response } <- events
        ]
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
        "No response.completed event found in xAI SSE stream"
        (Text.take 2000 (Text.decodeUtf8With Text.lenientDecode
            (LBS.toStrict (Aeson.encode events))))

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
