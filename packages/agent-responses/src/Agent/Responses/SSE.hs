-- | Incremental decoding for provider-neutral Responses SSE events.
module Agent.Responses.SSE
    ( SseDecoder
    , newSseDecoder
    , feedSseDecoder
    , finishSseDecoder
    , parseSseEvents
    , parseSseEventsBytes
    ) where

import Agent.Error (ApiError(..))
import qualified Agent.Responses.Codec as ResponsesCodec
import Agent.Responses.Types (ResponseStreamEvent)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
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
parseSseEvents = parseSseEventsBytes . Text.encodeUtf8

-- | Decode a complete SSE body without converting its validated wire bytes
-- through 'Text' first.
parseSseEventsBytes :: BS.ByteString -> Either ApiError [ResponseStreamEvent]
parseSseEventsBytes bytes = do
    (decoder, events) <- feedSseDecoder newSseDecoder bytes
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
    Right _ -> parseBlock bytes

parseBlock :: BS.ByteString -> Either ApiError (Maybe ResponseStreamEvent)
parseBlock block
    | BS.null dataBytes = Right Nothing
    | isDonePayload dataBytes = Right Nothing
    | otherwise = decodeEvent eventType dataBytes
  where
    blockLines = map dropTrailingCarriageReturn (BS8.lines block)
    eventType = Maybe.listToMaybe
        [ Text.strip (Text.decodeUtf8 (BS.drop 6 line))
        | line <- blockLines
        , "event:" `BS.isPrefixOf` line
        ]
    dataBytes = BS.intercalate "\n"
        [ stripOptionalSpace (BS.drop 5 line)
        | line <- blockLines
        , "data:" `BS.isPrefixOf` line
        ]

    stripOptionalSpace line = Maybe.fromMaybe line (BS.stripPrefix " " line)

isDonePayload :: BS.ByteString -> Bool
isDonePayload bytes =
    "[DONE]" `BS.isInfixOf` bytes
        && Text.strip (Text.decodeUtf8 bytes) == "[DONE]"

-- A malformed JSON payload should not tear down an otherwise healthy stream.
-- Codex skips such frames (notably partial/unparseable output_item events)
-- and continues decoding subsequent events. Framing/UTF-8 failures remain
-- hard errors because there is no safe way to recover their boundaries.
decodeEvent :: Maybe Text -> BS.ByteString -> Either ApiError (Maybe ResponseStreamEvent)
decodeEvent eventType dataBytes =
    case Aeson.eitherDecodeStrict' dataBytes of
        Left _ -> Right Nothing
        Right value ->
            let decoded = case eventType of
                    Just suppliedType ->
                        ResponsesCodec.decodeResponseStreamEventWithType
                            suppliedType
                            value
                    Nothing ->
                        case ResponsesCodec.decodeResponseStreamEventValue value of
                            Aeson.Success event -> Right event
                            Aeson.Error err -> Left err
            in case decoded of
                -- A valid JSON object with an invalid/partial event payload
                -- is also skippable. Unknown event types still decode to
                -- OtherResponseStreamEvent and are therefore preserved.
                Left _ -> Right Nothing
                Right event -> Right (Just event)

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
