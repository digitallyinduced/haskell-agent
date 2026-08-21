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
import Agent.Responses.ResponseMerge (mergeCompletedResponseOutput)
import qualified Agent.Responses.Codec as ResponsesCodec
import Agent.Responses.Types
import Agent.OpenRouter.Error (classifyStreamError)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text (lenientDecode)
import Data.Word (Word8)

-- | Incremental SSE decoder. Only bytes belonging to the current incomplete
-- event block are retained between HTTP chunks.
newtype SseDecoder = SseDecoder BS.ByteString

newSseDecoder :: SseDecoder
newSseDecoder = SseDecoder BS.empty

-- | Feed an arbitrary HTTP body chunk. Completed events are returned in wire
-- order as soon as their terminating blank line arrives.
feedSseDecoder
    :: SseDecoder
    -> BS.ByteString
    -> Either ApiError (SseDecoder, [ResponseStreamEvent])
feedSseDecoder (SseDecoder buffered) chunk =
    decodeAvailable [] (buffered <> chunk)
  where
    decodeAvailable events bytes = case takeBlock bytes of
        Nothing -> Right (SseDecoder bytes, reverse events)
        Just (block, rest) -> do
            decoded <- parseBlockBytes block
            decodeAvailable (maybe events (: events) decoded) rest

-- | Finish an SSE stream, accepting a final event without a trailing blank
-- line.
finishSseDecoder :: SseDecoder -> Either ApiError [ResponseStreamEvent]
finishSseDecoder (SseDecoder buffered)
    | BS.null (BS.dropWhile isSseWhitespace buffered) = Right []
    | otherwise = maybe [] pure <$> parseBlockBytes buffered

-- | Decode a complete SSE body into the canonical typed Responses event union.
parseSseEvents :: Text -> Either ApiError [ResponseStreamEvent]
parseSseEvents sseText = do
    (decoder, events) <- feedSseDecoder newSseDecoder (Text.encodeUtf8 sseText)
    trailing <- finishSseDecoder decoder
    pure (events <> trailing)

parseBlockBytes :: BS.ByteString -> Either ApiError (Maybe ResponseStreamEvent)
parseBlockBytes bytes = case Text.decodeUtf8' bytes of
    Left err -> Left $ JsonDecodeError
        ("Invalid UTF-8 in OpenRouter SSE event: " <> Text.pack (show err))
        (Text.take 2000 (Text.decodeUtf8With Text.lenientDecode bytes))
    Right block -> parseBlock (Text.replace "\r\n" "\n" block)

parseBlock :: Text -> Either ApiError (Maybe ResponseStreamEvent)
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
        [ stripOptionalSpace (Text.drop 5 line)
        | line <- blockLines
        , "data:" `Text.isPrefixOf` line
        ]

    stripOptionalSpace line = Maybe.fromMaybe line (Text.stripPrefix " " line)

decodeEvent :: Maybe Text -> Text -> Either ApiError ResponseStreamEvent
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
  where
    decodeError message body = JsonDecodeError message (Text.take 2000 body)

takeBlock :: BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
takeBlock bytes = do
    (offset, delimiterLength) <- earliestDelimiter bytes
    pure
        ( BS.take offset bytes
        , BS.drop (offset + delimiterLength) bytes
        )

earliestDelimiter :: BS.ByteString -> Maybe (Int, Int)
earliestDelimiter bytes = case
    ( findSubstring "\n\n" bytes
    , findSubstring "\r\n\r\n" bytes
    ) of
        (Nothing, Nothing) -> Nothing
        (Just offset, Nothing) -> Just (offset, 2)
        (Nothing, Just offset) -> Just (offset, 4)
        (Just lfOffset, Just crlfOffset)
            | lfOffset <= crlfOffset -> Just (lfOffset, 2)
            | otherwise -> Just (crlfOffset, 4)

findSubstring :: BS.ByteString -> BS.ByteString -> Maybe Int
findSubstring needle haystack
    | BS.null needle = Just 0
    | otherwise =
        let (prefix, suffix) = BS.breakSubstring needle haystack
        in if BS.null suffix then Nothing else Just (BS.length prefix)

isSseWhitespace :: Word8 -> Bool
isSseWhitespace byte =
    byte == 0x20 || byte == 0x09 || byte == 0x0a || byte == 0x0d

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
        "No response.completed event found in OpenRouter SSE stream"
        (Text.take 2000 (Text.decodeUtf8With Text.lenientDecode
            (LBS.toStrict (Aeson.encode events))))

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
