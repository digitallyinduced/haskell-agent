-- | Incremental decoding of Gemini's server-sent GenerateContent responses.
module Agent.Gemini.Stream
    ( SseDecoder
    , newSseDecoder
    , feedSseDecoder
    , finishSseDecoder
    , parseSseResponses
    , parseSseResponsesBytes
    ) where

import Agent.Error (ApiError(..))
import Agent.Gemini.Types (GenerateContentResponse)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Encoding.Error as TextEncoding (lenientDecode)
import Data.Word (Word8)

-- | Incomplete lines and event blocks remain chunked until a terminating
-- newline arrives. This avoids repeatedly copying a large JSON frame when the
-- HTTP client splits it across many small chunks.
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

feedSseDecoder
    :: SseDecoder
    -> BS.ByteString
    -> Either ApiError (SseDecoder, [GenerateContentResponse])
feedSseDecoder decoder chunk =
    decodeAvailable decoder chunk []
  where
    decodeAvailable current bytes responses
        | BS.null bytes = Right (current, reverse responses)
        | otherwise =
            let (linePart, restWithNewline) = BS.break (== lineFeed) bytes
            in if BS.null restWithNewline
                then do
                    next <- appendLinePart current linePart
                    Right (next, reverse responses)
                else do
                    withPart <- appendLinePart current linePart
                    withNewline <- addEventBytes 1 withPart
                    let line = completeLine withNewline
                        afterLine =
                            withNewline { decoderLineChunksRev = [] }
                    (next, decoded) <- consumeLine afterLine line
                    decodeAvailable
                        next
                        (BS.tail restWithNewline)
                        (maybe responses (: responses) decoded)

-- | Finish a stream, accepting a final event without a blank-line delimiter.
finishSseDecoder
    :: SseDecoder
    -> Either ApiError [GenerateContentResponse]
finishSseDecoder decoder = do
    (afterLine, lineResponse) <-
        if null decoder.decoderLineChunksRev
            then Right (decoder, Nothing)
            else
                let line = completeLine decoder
                    withoutLine = decoder { decoderLineChunksRev = [] }
                in consumeLine withoutLine line
    blockResponse <-
        parseBlockLines (reverse afterLine.decoderBlockLinesRev)
    pure (Maybe.catMaybes [lineResponse, blockResponse])

parseSseResponses
    :: Text
    -> Either ApiError [GenerateContentResponse]
parseSseResponses =
    parseSseResponsesBytes . TextEncoding.encodeUtf8

parseSseResponsesBytes
    :: BS.ByteString
    -> Either ApiError [GenerateContentResponse]
parseSseResponsesBytes bytes = do
    (decoder, responses) <- feedSseDecoder newSseDecoder bytes
    trailing <- finishSseDecoder decoder
    pure (responses <> trailing)

appendLinePart
    :: SseDecoder
    -> BS.ByteString
    -> Either ApiError SseDecoder
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
        ( "Gemini SSE event exceeds "
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
    -> Either
        ApiError
        (SseDecoder, Maybe GenerateContentResponse)
consumeLine decoder line
    | BS.null line = do
        decoded <-
            parseBlockLines (reverse decoder.decoderBlockLinesRev)
        pure (newSseDecoder, decoded)
    | otherwise =
        Right
            ( decoder
                { decoderBlockLinesRev =
                    line : decoder.decoderBlockLinesRev
                }
            , Nothing
            )

parseBlockLines
    :: [BS.ByteString]
    -> Either ApiError (Maybe GenerateContentResponse)
parseBlockLines [] = Right Nothing
parseBlockLines lines =
    parseBlockBytes (BS.intercalate "\n" lines)

parseBlockBytes
    :: BS.ByteString
    -> Either ApiError (Maybe GenerateContentResponse)
parseBlockBytes bytes =
    case TextEncoding.decodeUtf8' bytes of
        Left err -> Left $ JsonDecodeError
            ("Invalid UTF-8 in Gemini SSE event: " <> Text.pack (show err))
            (Text.take 2000
                (TextEncoding.decodeUtf8With TextEncoding.lenientDecode bytes))
        Right _ -> parseBlock bytes

parseBlock
    :: BS.ByteString
    -> Either ApiError (Maybe GenerateContentResponse)
parseBlock block
    | BS.null dataBytes = Right Nothing
    | isDonePayload dataBytes = Right Nothing
    | otherwise =
        case Aeson.eitherDecodeStrict' dataBytes of
            Left err -> Left $ JsonDecodeError
                ("Invalid Gemini SSE JSON: " <> Text.pack err)
                (Text.take 2000
                    (TextEncoding.decodeUtf8With
                        TextEncoding.lenientDecode
                        dataBytes))
            Right response -> Right (Just response)
  where
    blockLines = map dropTrailingCarriageReturn (BS8.lines block)
    dataBytes = BS.intercalate "\n"
        [ stripOptionalSpace (BS.drop 5 line)
        | line <- blockLines
        , "data:" `BS.isPrefixOf` line
        ]
    stripOptionalSpace line =
        Maybe.fromMaybe line (BS.stripPrefix " " line)

isDonePayload :: BS.ByteString -> Bool
isDonePayload bytes =
    Text.strip
        (TextEncoding.decodeUtf8With TextEncoding.lenientDecode bytes)
        == "[DONE]"

dropTrailingCarriageReturn :: BS.ByteString -> BS.ByteString
dropTrailingCarriageReturn bytes =
    case BS.unsnoc bytes of
        Just (prefix, byte)
            | byte == carriageReturn -> prefix
        _ -> bytes

decoderPreview :: SseDecoder -> Text
decoderPreview decoder =
    TextEncoding.decodeUtf8With TextEncoding.lenientDecode
        $ BS.concat
        $ takeByteChunks 2000
        $ reverse decoder.decoderBlockLinesRev
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
