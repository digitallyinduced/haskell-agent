-- | Provider-neutral incremental SSE framing. Field interpretation, UTF-8
-- validation, and event decoding belong to the caller.
module Agent.Transport.SSE
    ( SseFramer
    , newSseFramer
    , feedSseFramer
    , finishSseFramer
    , dropTrailingCarriageReturn
    , maxSseEventBytes
    ) where

import Agent.Error (ApiError(..))
import qualified Data.ByteString as BS
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text (lenientDecode)

-- | Incomplete lines and blocks remain chunked until their delimiter arrives.
-- The byte count includes all wire bytes, including comments, CRs, and LFs,
-- and resets after each blank line.
data SseFramer = SseFramer
    { lineChunksRev :: ![BS.ByteString]
    , blockLinesRev :: ![BS.ByteString]
    , eventBytes :: !Int
    }

newSseFramer :: SseFramer
newSseFramer = SseFramer [] [] 0

-- | Feed an arbitrary HTTP chunk, decoding nonempty blocks in wire order.
-- The callback runs before framing subsequent bytes so its errors take
-- precedence over framing errors in later blocks of the same chunk.
feedSseFramer
    :: Text
    -- ^ Provider name used in limit errors.
    -> (BS.ByteString -> Either ApiError (Maybe event))
    -> SseFramer
    -> BS.ByteString
    -> Either ApiError (SseFramer, [event])
feedSseFramer provider decodeBlock framer chunk =
    decodeAvailable framer chunk []
  where
    decodeAvailable current bytes events
        | BS.null bytes = Right (current, reverse events)
        | otherwise =
            let (linePart, restWithNewline) = BS.break (== 0x0a) bytes
            in if BS.null restWithNewline
                then do
                    next <- appendLinePart provider current linePart
                    Right (next, reverse events)
                else do
                    withPart <- appendLinePart provider current linePart
                    withNewline <- addEventBytes provider 1 withPart
                    let line = completeLine withNewline
                        afterLine = withNewline { lineChunksRev = [] }
                    (next, decoded) <- consumeLine decodeBlock afterLine line
                    decodeAvailable next (BS.tail restWithNewline)
                        (maybe events (: events) decoded)

-- | Accept a final event without a trailing newline or blank-line delimiter.
-- Finishing does not add synthetic wire bytes to the size limit.
finishSseFramer
    :: (BS.ByteString -> Either ApiError (Maybe event))
    -> SseFramer
    -> Either ApiError [event]
finishSseFramer decodeBlock framer = do
    (afterLine, lineEvent) <-
        if null framer.lineChunksRev
            then Right (framer, Nothing)
            else consumeLine decodeBlock
                (framer { lineChunksRev = [] }) (completeLine framer)
    blockEvent <- decodeBlockLines decodeBlock (reverse afterLine.blockLinesRev)
    pure (Maybe.catMaybes [lineEvent, blockEvent])

appendLinePart :: Text -> SseFramer -> BS.ByteString -> Either ApiError SseFramer
appendLinePart provider framer chunk
    | BS.null chunk = Right framer
    | otherwise = do
        withBytes <- addEventBytes provider (BS.length chunk) framer
        pure withBytes { lineChunksRev = chunk : withBytes.lineChunksRev }

addEventBytes :: Text -> Int -> SseFramer -> Either ApiError SseFramer
addEventBytes provider amount framer
    | amount <= maxSseEventBytes - framer.eventBytes =
        Right framer { eventBytes = framer.eventBytes + amount }
    | otherwise = Left $ JsonDecodeError
        (provider <> " SSE event exceeds "
            <> Text.pack (show maxSseEventBytes) <> " bytes")
        (framerPreview framer)

completeLine :: SseFramer -> BS.ByteString
completeLine =
    dropTrailingCarriageReturn . BS.concat . reverse . (.lineChunksRev)

consumeLine
    :: (BS.ByteString -> Either ApiError (Maybe event))
    -> SseFramer
    -> BS.ByteString
    -> Either ApiError (SseFramer, Maybe event)
consumeLine decodeBlock framer line
    | BS.null line = do
        decoded <- decodeBlockLines decodeBlock (reverse framer.blockLinesRev)
        pure (newSseFramer, decoded)
    | otherwise = Right
        (framer { blockLinesRev = line : framer.blockLinesRev }, Nothing)

decodeBlockLines
    :: (BS.ByteString -> Either ApiError (Maybe event))
    -> [BS.ByteString]
    -> Either ApiError (Maybe event)
decodeBlockLines _ [] = Right Nothing
decodeBlockLines decodeBlock lines = decodeBlock (BS.intercalate "\n" lines)

-- | Strip exactly one CR. A bare CR is not a line delimiter.
dropTrailingCarriageReturn :: BS.ByteString -> BS.ByteString
dropTrailingCarriageReturn bytes = case BS.unsnoc bytes of
    Just (prefix, 0x0d) -> prefix
    _ -> bytes

framerPreview :: SseFramer -> Text
framerPreview framer =
    Text.decodeUtf8With Text.lenientDecode $
        BS.concat $
            takeByteChunks 2000 $
                reverse framer.blockLinesRev <> reverse framer.lineChunksRev

takeByteChunks :: Int -> [BS.ByteString] -> [BS.ByteString]
takeByteChunks remaining _
    | remaining <= 0 = []
takeByteChunks _ [] = []
takeByteChunks remaining (chunk : rest) =
    let kept = BS.take remaining chunk
    in kept : takeByteChunks (remaining - BS.length kept) rest

maxSseEventBytes :: Int
maxSseEventBytes = 64 * 1024 * 1024
