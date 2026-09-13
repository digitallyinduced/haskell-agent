-- | Private session IPC framing, independent of JSON and protocol error text.
-- The daemon deliberately does not depend on this CLI runtime package: it has
-- its own frame limit, exception type, and empty-frame policy.
module Agent.Runtime.Session.Framing
    ( maximumFrameBytes
    , sendFrame
    , receiveFrame
    ) where

import Control.Exception.Safe (throwIO)
import Control.Monad (when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import Network.Socket (Socket)
import qualified Network.Socket.ByteString as Socket

maximumFrameBytes :: Int
maximumFrameBytes = 32 * 1024 * 1024

-- | Write the existing Word32 big-endian wire format. Outbound size policy
-- belongs to the caller: Inbox bounds sends, while Observation historically
-- only bounds receives.
sendFrame :: Socket -> BS.ByteString -> IO ()
sendFrame client bytes =
    Socket.sendAll client (prefix <> bytes)
  where
    prefix = LBS.toStrict
        (Builder.toLazyByteString (Builder.word32BE (fromIntegral (BS.length bytes))))

-- | Read one nonempty frame, rejecting its length before reading its body.
-- The supplied reader must have 'Socket.recv' semantics: return at most the
-- requested bytes, with an empty result indicating EOF. Taking the reader as
-- an argument also lets tests exercise short reads without socket scheduling.
receiveFrame :: String -> String -> (Int -> IO BS.ByteString) -> IO BS.ByteString
receiveFrame invalidSize disconnected receive = do
    prefix <- receiveExactly 4
    let count = BS.foldl' (\size byte -> size * 256 + fromIntegral byte) (0 :: Int) prefix
    when (count <= 0 || count > maximumFrameBytes) $
        throwIO (userError invalidSize)
    receiveExactly count
  where
    receiveExactly count = BS.concat . reverse <$> go count []
    go 0 chunks = pure chunks
    go remaining chunks = do
        bytes <- receive (min remaining 65536)
        when (BS.null bytes) (throwIO (userError disconnected))
        go (remaining - BS.length bytes) (bytes : chunks)
