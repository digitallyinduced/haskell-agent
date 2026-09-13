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
import qualified Agent.Transport.SSE as SSE
import Agent.Transport.SSE (dropTrailingCarriageReturn)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Encoding.Error as TextEncoding (lenientDecode)

-- | Gemini event decoding over the shared incremental SSE framer.
newtype SseDecoder = SseDecoder SSE.SseFramer

newSseDecoder :: SseDecoder
newSseDecoder = SseDecoder SSE.newSseFramer

feedSseDecoder
    :: SseDecoder
    -> BS.ByteString
    -> Either ApiError (SseDecoder, [GenerateContentResponse])
feedSseDecoder (SseDecoder framer) chunk = do
    (next, responses) <- SSE.feedSseFramer "Gemini" parseBlockBytes framer chunk
    pure (SseDecoder next, responses)

-- | Finish a stream, accepting a final event without a blank-line delimiter.
finishSseDecoder
    :: SseDecoder
    -> Either ApiError [GenerateContentResponse]
finishSseDecoder (SseDecoder framer) =
    SSE.finishSseFramer parseBlockBytes framer

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
