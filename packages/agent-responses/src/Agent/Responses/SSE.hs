-- | Incremental decoding for provider-neutral Responses SSE events.
module Agent.Responses.SSE
    ( SseDecoder
    , SseFrame(..)
    , newSseDecoder
    , feedSseFrameDecoder
    , finishSseFrameDecoder
    , feedSseDecoderWith
    , finishSseDecoderWith
    , feedSseDecoder
    , finishSseDecoder
    , parseSseEvents
    ) where

import Agent.Error (ApiError(..))
import qualified Agent.Json.Decoder as Decoder
import Agent.Responses.Types
    ( ResponseStreamEvent
    , responseStreamEventDecoderWithType
    )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Functor.Identity (Identity(..))
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

-- | A complete SSE event payload, retaining the optional transport-level
-- event type and strict JSON bytes for the caller's chosen decoder backend.
data SseFrame = SseFrame
    { sseFrameEventType :: !(Maybe Text)
    , sseFrameData :: !BS.ByteString
    }
    deriving stock (Eq, Show)

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
    runIdentity $
        feedSseDecoderWith (pure . decodeFrame) decoder chunk

-- | Feed an arbitrary HTTP body chunk and return complete strict frames
-- without selecting a JSON backend.
feedSseFrameDecoder
    :: SseDecoder
    -> BS.ByteString
    -> Either ApiError (SseDecoder, [SseFrame])
feedSseFrameDecoder decoder chunk =
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

-- | Feed bytes and decode complete frames with a caller-supplied backend.
-- The callback can close over a decoder session scoped by the caller.
feedSseDecoderWith
    :: Applicative f
    => (SseFrame -> f (Maybe event))
    -> SseDecoder
    -> BS.ByteString
    -> f (Either ApiError (SseDecoder, [event]))
feedSseDecoderWith decode decoder chunk =
    case feedSseFrameDecoder decoder chunk of
        Left err -> pure (Left err)
        Right (next, frames) ->
            fmap
                (Right . (next,) . Maybe.catMaybes)
                (traverse decode frames)

-- | Finish an SSE stream, accepting a final event without a trailing blank
-- line.
finishSseDecoder :: SseDecoder -> Either ApiError [ResponseStreamEvent]
finishSseDecoder decoder =
    runIdentity $
        finishSseDecoderWith (pure . decodeFrame) decoder

-- | Finish framing an SSE stream, accepting a final event without a trailing
-- blank line.
finishSseFrameDecoder :: SseDecoder -> Either ApiError [SseFrame]
finishSseFrameDecoder decoder = do
    (afterLine, lineEvent) <-
        if null decoder.decoderLineChunksRev
            then Right (decoder, Nothing)
            else
                let line = completeLine decoder
                    withoutLine = decoder { decoderLineChunksRev = [] }
                in consumeLine withoutLine line
    blockEvent <- frameBlockLines (reverse afterLine.decoderBlockLinesRev)
    pure (Maybe.catMaybes [lineEvent, blockEvent])

-- | Finish framing and decode trailing frames with a caller-supplied backend.
finishSseDecoderWith
    :: Applicative f
    => (SseFrame -> f (Maybe event))
    -> SseDecoder
    -> f (Either ApiError [event])
finishSseDecoderWith decode decoder =
    case finishSseFrameDecoder decoder of
        Left err -> pure (Left err)
        Right frames ->
            fmap
                (Right . Maybe.catMaybes)
                (traverse decode frames)

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
    -> Either ApiError (SseDecoder, Maybe SseFrame)
consumeLine decoder line
    | BS.null line = do
        decoded <- frameBlockLines (reverse decoder.decoderBlockLinesRev)
        pure (newSseDecoder, decoded)
    | otherwise =
        Right
            ( decoder
                { decoderBlockLinesRev =
                    line : decoder.decoderBlockLinesRev
                }
            , Nothing
            )

frameBlockLines :: [BS.ByteString] -> Either ApiError (Maybe SseFrame)
frameBlockLines [] = Right Nothing
frameBlockLines lines = frameBlockBytes (BS.intercalate "\n" lines)

frameBlockBytes :: BS.ByteString -> Either ApiError (Maybe SseFrame)
frameBlockBytes bytes = case Text.decodeUtf8' bytes of
    Left err -> Left $ JsonDecodeError
        ("Invalid UTF-8 in Responses SSE event: " <> Text.pack (show err))
        (Text.take 2000 (Text.decodeUtf8With Text.lenientDecode bytes))
    Right _ -> Right (frameBlock bytes)

frameBlock :: BS.ByteString -> Maybe SseFrame
frameBlock block
    | BS.null dataBytes = Nothing
    | Text.strip (Text.decodeUtf8 dataBytes) == "[DONE]" = Nothing
    | otherwise = Just SseFrame
        { sseFrameEventType = eventType
        , sseFrameData = dataBytes
        }
  where
    blockLines = BS8.lines block
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

-- A malformed JSON payload should not tear down an otherwise healthy stream.
-- Codex skips such frames (notably partial/unparseable output_item events)
-- and continues decoding subsequent events. Framing/UTF-8 failures remain
-- hard errors because there is no safe way to recover their boundaries.
decodeFrame :: SseFrame -> Maybe ResponseStreamEvent
decodeFrame SseFrame{sseFrameEventType, sseFrameData} =
    -- Both malformed JSON and valid but invalid/partial event payloads are
    -- skippable. Unknown event types decode to OtherResponseStreamEvent.
    either (const Nothing) Just $
        Decoder.decode
            (responseStreamEventDecoderWithType sseFrameEventType)
            sseFrameData

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
