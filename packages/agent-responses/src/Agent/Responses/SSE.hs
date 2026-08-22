-- | Incremental decoding for provider-neutral Responses SSE events.
module Agent.Responses.SSE
    ( SseDecoder
    , newSseDecoder
    , feedSseDecoder
    , finishSseDecoder
    , parseSseEvents
    ) where

import Agent.Error (ApiError(..))
import qualified Agent.Responses.Codec as ResponsesCodec
import Agent.Responses.Types (ResponseStreamEvent)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text (lenientDecode)
import Data.Word (Word8)

-- | Incremental SSE decoder. Incomplete lines and event blocks stay chunked
-- until their terminating blank line arrives, avoiding repeated copies and
-- rescans when one large event spans many HTTP chunks.
data SseDecoder = SseDecoder
    { decoderLineChunksRev :: ![BS.ByteString]
    , decoderBlockLinesRev :: ![BS.ByteString]
    , decoderEventBytes :: !Int
    }

newSseDecoder :: SseDecoder
newSseDecoder = SseDecoder
    { decoderLineChunksRev = []
    , decoderBlockLinesRev = []
    , decoderEventBytes = 0
    }

-- | Feed an arbitrary HTTP body chunk. Completed events are returned in wire
-- order as soon as their terminating blank line arrives.
feedSseDecoder
    :: SseDecoder
    -> BS.ByteString
    -> Either ApiError (SseDecoder, [ResponseStreamEvent])
feedSseDecoder decoder chunk =
    decodeAvailable decoder chunk []
  where
    decodeAvailable current bytes events
        | BS.null bytes = Right (current, reverse events)
        | otherwise =
            let (linePart, restWithNewline) = BS.break (== lineFeed) bytes
            in if BS.null restWithNewline
                then do
                    next <- appendLinePart current linePart
                    Right (next, reverse events)
                else do
                    withPart <- appendLinePart current linePart
                    withNewline <- addEventBytes 1 withPart
                    let line = completeLine withNewline
                        afterLine = withNewline { decoderLineChunksRev = [] }
                    (next, decoded) <- consumeLine afterLine line
                    decodeAvailable
                        next
                        (BS.tail restWithNewline)
                        (maybe events (: events) decoded)

-- | Finish an SSE stream, accepting a final event without a trailing blank
-- line.
finishSseDecoder :: SseDecoder -> Either ApiError [ResponseStreamEvent]
finishSseDecoder decoder = do
    (afterLine, lineEvent) <-
        if null decoder.decoderLineChunksRev
            then Right (decoder, Nothing)
            else
                let line = completeLine decoder
                    withoutLine = decoder { decoderLineChunksRev = [] }
                in consumeLine withoutLine line
    blockEvent <- parseBlockLines (reverse afterLine.decoderBlockLinesRev)
    pure (Maybe.catMaybes [lineEvent, blockEvent])

-- | Decode a complete SSE body into the canonical typed Responses event union.
parseSseEvents :: Text -> Either ApiError [ResponseStreamEvent]
parseSseEvents sseText = do
    (decoder, events) <- feedSseDecoder newSseDecoder (Text.encodeUtf8 sseText)
    trailing <- finishSseDecoder decoder
    pure (events <> trailing)

appendLinePart :: SseDecoder -> BS.ByteString -> Either ApiError SseDecoder
appendLinePart decoder chunk
    | BS.null chunk = Right decoder
    | otherwise = do
        withBytes <- addEventBytes (BS.length chunk) decoder
        pure withBytes
            { decoderLineChunksRev =
                chunk : withBytes.decoderLineChunksRev
            }

addEventBytes :: Int -> SseDecoder -> Either ApiError SseDecoder
addEventBytes amount decoder
    | amount <= maxSseEventBytes - decoder.decoderEventBytes =
        Right decoder
            { decoderEventBytes = decoder.decoderEventBytes + amount }
    | otherwise = Left $ JsonDecodeError
        ( "Responses SSE event exceeds "
            <> Text.pack (show maxSseEventBytes)
            <> " bytes"
        )
        (decoderPreview decoder)

completeLine :: SseDecoder -> BS.ByteString
completeLine =
    dropTrailingCarriageReturn
        . BS.concat
        . reverse
        . (.decoderLineChunksRev)

consumeLine
    :: SseDecoder
    -> BS.ByteString
    -> Either ApiError (SseDecoder, Maybe ResponseStreamEvent)
consumeLine decoder line
    | BS.null line = do
        decoded <- parseBlockLines (reverse decoder.decoderBlockLinesRev)
        pure (newSseDecoder, decoded)
    | otherwise =
        Right
            ( decoder
                { decoderBlockLinesRev =
                    line : decoder.decoderBlockLinesRev
                }
            , Nothing
            )

parseBlockLines :: [BS.ByteString] -> Either ApiError (Maybe ResponseStreamEvent)
parseBlockLines [] = Right Nothing
parseBlockLines lines = parseBlockBytes (BS.intercalate "\n" lines)

parseBlockBytes :: BS.ByteString -> Either ApiError (Maybe ResponseStreamEvent)
parseBlockBytes bytes = case Text.decodeUtf8' bytes of
    Left err -> Left $ JsonDecodeError
        ("Invalid UTF-8 in Responses SSE event: " <> Text.pack (show err))
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

dropTrailingCarriageReturn :: BS.ByteString -> BS.ByteString
dropTrailingCarriageReturn bytes = case BS.unsnoc bytes of
    Just (prefix, byte) | byte == carriageReturn -> prefix
    _ -> bytes

decoderPreview :: SseDecoder -> Text
decoderPreview decoder =
    Text.decodeUtf8With Text.lenientDecode $
        BS.concat $
            takeByteChunks 2000 $
                reverse decoder.decoderBlockLinesRev
                    <> reverse decoder.decoderLineChunksRev

takeByteChunks :: Int -> [BS.ByteString] -> [BS.ByteString]
takeByteChunks remaining _
    | remaining <= 0 = []
takeByteChunks _ [] = []
takeByteChunks remaining (chunk : rest) =
    let kept = BS.take remaining chunk
    in kept : takeByteChunks (remaining - BS.length kept) rest

lineFeed :: Word8
lineFeed = 0x0a

carriageReturn :: Word8
carriageReturn = 0x0d

maxSseEventBytes :: Int
maxSseEventBytes = 64 * 1024 * 1024
