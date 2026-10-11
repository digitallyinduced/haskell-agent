-- Host-side limits apply even if evaluated code escapes its JavaScript realm.
module Agent.Tools.CodeMode.IsolatedHost.Limits (boundedProtocolReader) where

import Prelude
import Control.Monad (when)
import qualified Data.ByteString as BS
import Data.IORef
import System.IO (Handle)

-- One reader owns a handle. Retain at most one bounded frame plus a 4KiB
-- read-ahead chunk, and bound total traffic and notification/tool counts.
boundedProtocolReader :: Handle -> IO (IO BS.ByteString)
boundedProtocolReader handle = do
    pending <- newIORef BS.empty
    budget <- newIORef (16 * 1024 * 1024 :: Int, 2048 :: Int)
    let next = do
            bytes <- readIORef pending
            collect [] 0 bytes
        collect chunks size bytes =
            case BS.elemIndex 10 bytes of
                Just index -> do
                    let (prefix, suffix) = BS.splitAt index bytes
                        total = size + BS.length prefix
                    checkFrame total
                    writeIORef pending (BS.drop 1 suffix)
                    (remaining, messages) <- readIORef budget
                    when (total > remaining || messages <= 0) $
                        ioError (userError "Code-mode protocol budget exceeded")
                    writeIORef budget (remaining - total, messages - 1)
                    pure (BS.concat (reverse (prefix : chunks)))
                Nothing -> do
                    let total = size + BS.length bytes
                    checkFrame total
                    chunk <- BS.hGetSome handle 4096
                    when (BS.null chunk) $
                        ioError (userError "Code-mode worker closed its output")
                    collect (bytes : chunks) total chunk
        checkFrame size = when (size > 8 * 1024 * 1024) $
            ioError (userError "Code-mode protocol frame exceeds byte limit")
    pure next
