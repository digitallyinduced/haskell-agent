-- | Incremental decoding for provider-neutral Responses SSE events.
module Agent.Responses.SSE
    ( SseDecoder
    , newSseDecoder
    , feedSseDecoder
    , finishSseDecoder
    , decodeSseC
    , parseSseEvents
    , parseSseEventsBytes
    ) where

import Agent.Error (ApiError(..))
import qualified Agent.Responses.Codec as ResponsesCodec
import Agent.Responses.Types (ResponseStreamEvent)
import qualified Agent.Transport.SSE as SSE
import Agent.Transport.SSE (dropTrailingCarriageReturn)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, except)
import Data.Conduit (ConduitT, await, yield)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text (lenientDecode)

-- | Responses event decoding over the shared incremental SSE framer.
newtype SseDecoder = SseDecoder SSE.SseFramer

newSseDecoder :: SseDecoder
newSseDecoder = SseDecoder SSE.newSseFramer

-- | Decode chunks under downstream demand, flushing only on normal EOF.
-- Keep the existing per-chunk validation boundary: a framing/UTF-8 error in
-- a chunk is reported before any events from that chunk are delivered.
-- Empty chunks here are harmless; the transport source owns EOF detection.
decodeSseC
    :: Monad m
    => ConduitT BS.ByteString ResponseStreamEvent (ExceptT ApiError m) ()
decodeSseC = go newSseDecoder
  where
    go decoder = await >>= \case
        Nothing -> do
            trailing <- lift (except (finishSseDecoder decoder))
            mapM_ yield trailing
        Just chunk -> do
            (next, events) <- lift (except (feedSseDecoder decoder chunk))
            mapM_ yield events
            go next

-- | Feed an arbitrary HTTP body chunk. Completed events are returned in wire
-- order as soon as their terminating blank line arrives.
feedSseDecoder
    :: SseDecoder
    -> BS.ByteString
    -> Either ApiError (SseDecoder, [ResponseStreamEvent])
feedSseDecoder (SseDecoder framer) chunk = do
    (next, events) <- SSE.feedSseFramer "Responses" parseBlockBytes framer chunk
    pure (SseDecoder next, events)

-- | Finish an SSE stream, accepting a final event without a trailing blank
-- line.
finishSseDecoder :: SseDecoder -> Either ApiError [ResponseStreamEvent]
finishSseDecoder (SseDecoder framer) =
    SSE.finishSseFramer parseBlockBytes framer

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
    case decoded of
        -- An invalid/partial event payload is skippable. Unknown event types
        -- still decode to OtherResponseStreamEvent and are preserved.
        Left _ -> Right Nothing
        Right event -> Right (Just event)
  where
    decoded = case eventType of
        Just suppliedType ->
            ResponsesCodec.decodeResponseStreamEventWithType
                suppliedType
                dataBytes
        Nothing ->
            ResponsesCodec.decodeResponseStreamEvent dataBytes
