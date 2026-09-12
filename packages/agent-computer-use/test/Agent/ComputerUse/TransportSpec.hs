module Agent.ComputerUse.TransportSpec (spec) where

import Agent.ComputerUse.Transport
import Control.Exception.Safe (throwIO)
import qualified Data.ByteString as BS
import Data.IORef
import Test.Hspec

spec :: Spec
spec = describe "local computer service framing" do
    it "rejects non-positive limits" do
        frameLimit 0 `shouldBe` Nothing
        frameLimit (-1) `shouldBe` Nothing
    it "encodes an unsigned little-endian length" do
        encodeFrame limit "abc" `shouldBe` Right (BS.pack [3, 0, 0, 0] <> "abc")
    it "rejects empty and oversized outgoing frames" do
        encodeFrame limit "" `shouldBe` Left EmptyFrame
        encodeFrame limit (BS.replicate 1025 0) `shouldBe` Left FrameExceedsLimit
    it "reassembles one-byte reads" do
        reader <- fragmentedReader (BS.pack [3, 0, 0, 0] <> "abc")
        receiveFrame limit reader `shouldReturn` Right (Just "abc")
    it "accepts the exact limit and decodes multi-byte lengths" do
        let payload = BS.replicate 1024 65
        case encodeFrame limit payload of
            Left failure -> expectationFailure (show failure)
            Right encoded -> do
                BS.take 4 encoded `shouldBe` BS.pack [0, 4, 0, 0]
                reader <- fragmentedReader encoded
                receiveFrame limit reader `shouldReturn` Right (Just payload)
    it "does not consume the next frame" do
        reader <- fragmentedReader (BS.pack [1, 0, 0, 0, 65, 1, 0, 0, 0, 66])
        receiveFrame limit reader `shouldReturn` Right (Just "A")
        receiveFrame limit reader `shouldReturn` Right (Just "B")
        receiveFrame limit reader `shouldReturn` Right Nothing
    it "distinguishes clean EOF from a truncated header" do
        receiveFrame limit (const (pure BS.empty)) `shouldReturn` Right Nothing
        reader <- fragmentedReader (BS.pack [1, 0])
        receiveFrame limit reader `shouldReturn` Left TruncatedFrame
    it "rejects EOF at the start or middle of a payload" do
        mapM_ (\bytes -> do
            reader <- fragmentedReader bytes
            receiveFrame limit reader `shouldReturn` Left TruncatedFrame)
            [BS.pack [2, 0, 0, 0], BS.pack [2, 0, 0, 0, 65]]
    it "rejects empty incoming messages" do
        reader <- fragmentedReader (BS.replicate 4 0)
        receiveFrame limit reader `shouldReturn` Left EmptyFrame
    it "checks the bound before requesting payload bytes" do
        calls <- newIORef (0 :: Int)
        let reader _ = do
                call <- atomicModifyIORef' calls (\n -> (n + 1, n))
                if call == 0 then pure (BS.replicate 4 255)
                    else expectationFailure "payload read after oversized header" >> pure BS.empty
        receiveFrame limit reader `shouldReturn` Left FrameExceedsLimit
        readIORef calls `shouldReturn` 1
    it "rejects a reader violating its byte bound" do
        receiveFrame limit (const (pure (BS.replicate 5 0)))
            `shouldReturn` Left ReaderExceedsRequestedLength
    it "propagates IO exceptions rather than retrying" do
        receiveFrame limit (const (throwIO (userError "connection lost")))
            `shouldThrow` anyIOException
  where
    limit = case frameLimit 1024 of
        Just value -> value
        Nothing -> error "invalid test frame limit"

fragmentedReader :: BS.ByteString -> IO (Int -> IO BS.ByteString)
fragmentedReader bytes = do
    remaining <- newIORef bytes
    pure \requested -> atomicModifyIORef' remaining \current ->
        let (chunk, rest) = BS.splitAt (min 1 requested) current
        in (rest, chunk)
