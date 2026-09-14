-- | Bounded framing for the local computer service. This module does not
-- authenticate peers or retry requests. A connection owner must discard the
-- stream after any framing failure or interrupted read/write: its position is
-- then unknown, and replaying a computer action is not safe.
module Agent.ComputerUse.Transport
    ( FrameLimit
    , frameLimit
    , FrameFailure(..)
    , encodeFrame
    , receiveFrame
    ) where

import Data.Bits (shiftL, shiftR, (.|.))
import qualified Data.ByteString as BS
import Data.Word (Word32)

-- | The configured bound is checked before reading a payload. Separate
-- request and response limits can be used without changing the framing.
newtype FrameLimit = FrameLimit Int
    deriving (Eq, Show)

frameLimit :: Int -> Maybe FrameLimit
frameLimit size
    | size > 0 && toInteger size <= toInteger (maxBound :: Word32) =
        Just (FrameLimit size)
    | otherwise = Nothing

data FrameFailure
    = EmptyFrame
    | FrameExceedsLimit
    | TruncatedFrame
    | ReaderExceedsRequestedLength
    deriving (Eq, Show)

-- | Four-byte unsigned little-endian payload length followed by the payload.
-- Empty payloads are not protocol messages; EOF is represented separately.
encodeFrame :: FrameLimit -> BS.ByteString -> Either FrameFailure BS.ByteString
encodeFrame (FrameLimit limit) payload
    | BS.null payload = Left EmptyFrame
    | BS.length payload > limit = Left FrameExceedsLimit
    | otherwise = Right (header <> payload)
  where
    size = fromIntegral (BS.length payload) :: Word32
    header = BS.pack [fromIntegral (size `shiftR` offset) | offset <- [0, 8, 16, 24]]

-- | Read one frame. The supplied reader must return at most the requested
-- byte count, and an empty chunk only at EOF. Exceptions (including async
-- cancellation) propagate to the connection owner. No worker is detached.
-- 'Right Nothing' means clean EOF before the first header byte only.
receiveFrame
    :: FrameLimit
    -> (Int -> IO BS.ByteString)
    -> IO (Either FrameFailure (Maybe BS.ByteString))
receiveFrame (FrameLimit limit) receive = do
    headerResult <- readExactly receive 4
    case headerResult of
        Left failure -> pure (Left failure)
        Right Nothing -> pure (Right Nothing)
        Right (Just header) -> do
            let size = decodeLength header
            if size == 0
                then pure (Left EmptyFrame)
                else if toInteger size > toInteger limit
                    then pure (Left FrameExceedsLimit)
                    else readExactly receive (fromIntegral size) >>= \case
                        Right Nothing -> pure (Left TruncatedFrame)
                        result -> pure result

decodeLength :: BS.ByteString -> Word32
decodeLength = BS.foldr (\byte rest -> fromIntegral byte .|. (rest `shiftL` 8)) 0

readExactly
    :: (Int -> IO BS.ByteString)
    -> Int
    -> IO (Either FrameFailure (Maybe BS.ByteString))
readExactly receive count = go count []
  where
    go remaining chunks
        | remaining == 0 = pure (Right (Just (BS.concat (reverse chunks))))
        | otherwise = do
            chunk <- receive remaining
            if BS.length chunk > remaining
                then pure (Left ReaderExceedsRequestedLength)
                else if BS.null chunk
                    then pure (if null chunks then Right Nothing else Left TruncatedFrame)
                    else go (remaining - BS.length chunk) (chunk : chunks)
