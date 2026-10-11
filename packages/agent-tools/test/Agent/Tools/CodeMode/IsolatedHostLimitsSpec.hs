module Agent.Tools.CodeMode.IsolatedHostLimitsSpec (spec) where

import Agent.Tools.CodeMode.IsolatedHost.Limits
import Control.Concurrent.Async (withAsync)
import Control.Exception.Safe (bracket)
import Control.Monad (replicateM_)
import qualified Data.ByteString as BS
import System.IO (hClose, hFlush)
import System.Posix.IO (createPipe, fdToHandle)
import Test.Hspec

spec :: Spec
spec = describe "isolated code-mode protocol resource limits" do
    it "preserves multiple frames from the same chunk" $
        withInput "one\ntwo\n" \next -> do
            next `shouldReturn` "one"
            next `shouldReturn` "two"
    it "rejects an unterminated oversized frame" $
        withInput (BS.replicate (8 * 1024 * 1024 + 4096) 120) \next ->
            next `shouldThrow` anyIOException
    it "bounds message count even for empty frames" $
        withInput (BS.replicate 2049 10) \next -> do
            replicateM_ 2048 next
            next `shouldThrow` anyIOException
    it "bounds cumulative bytes across otherwise valid frames" $
        withInput (BS.concat (replicate 3 (BS.replicate (8 * 1024 * 1024) 120 <> "\n"))) \next -> do
            replicateM_ 2 next
            next `shouldThrow` anyIOException

withInput :: BS.ByteString -> (IO BS.ByteString -> IO a) -> IO a
withInput bytes action =
    bracket
        (do
            (input, output) <- createPipe
            (,) <$> fdToHandle input <*> fdToHandle output)
        (\(input, output) -> hClose input >> hClose output)
        \(input, output) ->
            withAsync (BS.hPut output bytes >> hFlush output) \_ ->
                boundedProtocolReader input >>= action
