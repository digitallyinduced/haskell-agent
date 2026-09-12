module Main (main) where

import Agent.CLI.Session.Framing
import Control.Concurrent.Async (concurrently)
import Control.Exception.Safe (bracket, throwIO)
import Control.Monad (forM_, when)
import Data.Aeson (Value, eitherDecodeStrict', encode, object, (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef')
import Network.Socket
import qualified Network.Socket.ByteString as Socket
import System.IO.Error (ioeGetErrorString)
import System.Timeout (timeout)
import Test.Hspec

main :: IO ()
main = hspec do
    describe "session framing" do
        it "retains the 32 MiB receive limit" $
            maximumFrameBytes `shouldBe` 33554432

        forM_ [("inbox", "Inbox"), ("observation", "Observation")] \(name, owner) -> do
            let invalid = "Invalid " <> name <> " frame size."
                disconnected = owner <> " owner disconnected."
                receive = receiveFrame invalid disconnected

            describe name do
                it "assembles one-byte header and body fragments" do
                    reader <- fragmentedReader (map BS.singleton [0, 0, 0, 5, 104, 101, 108, 108, 111])
                    receive reader `shouldReturn` "hello"

                forM_ [[], ["\0"], ["\0\0\0"], ["\0\0\0\5"], ["\0\0\0\5", "hel"]] \chunks ->
                    it ("reports disconnect for truncated input " <> show chunks) do
                        reader <- fragmentedReader chunks
                        receive reader `shouldThrow` ((== disconnected) . ioeGetErrorString)

                forM_ [BS.pack [0,0,0,0], BS.pack [2,0,0,1], BS.pack [128,0,0,0], BS.pack [255,255,255,255]] \header ->
                    it ("rejects invalid/oversized length before a body read " <> show header) do
                        calls <- newIORef (0 :: Int)
                        let reader requested = do
                                modifyIORef' calls (+ 1)
                                requested `shouldBe` 4
                                count <- readIORef calls
                                when (count > 1) (expectationFailure "read body after invalid length")
                                pure header
                        receive reader `shouldThrow` ((== invalid) . ioeGetErrorString)
                        readIORef calls `shouldReturn` 1

                it "accepts the exact limit and caps all body reads at 64 KiB" do
                    requests <- newIORef []
                    first <- newIORef True
                    let reader requested = do
                            modifyIORef' requests (requested :)
                            isFirst <- readIORef first
                            writeIORef first False
                            pure (if isFirst then BS.pack [2,0,0,0] else BS.replicate requested 120)
                    body <- receive reader
                    BS.length body `shouldBe` maximumFrameBytes
                    reverse <$> readIORef requests `shouldReturn` (4 : replicate 512 65536)

                it "does not consume the next frame" do
                    reader <- fragmentedReader ["\0\0\0\1a\0\0\0\1b"]
                    receive reader `shouldReturn` "a"
                    receive reader `shouldReturn` "b"

                it "preserves JSON round trips on real sockets" $
                    withPair \sender receiver -> withinTimeout do
                        let value = object ["version" .= (1 :: Int), "message" .= ("hello \955" :: String)]
                            body = LBS.toStrict (encode value)
                        (_, received) <- concurrently (sendFrame sender body) (receive (Socket.recv receiver))
                        (eitherDecodeStrict' received :: Either String Value) `shouldBe` Right value

                it "leaves JSON parsing and its errors to the caller" $
                    withPair \sender receiver -> withinTimeout do
                        (_, body) <- concurrently (sendFrame sender "{invalid") (receive (Socket.recv receiver))
                        body `shouldBe` "{invalid"
                        (eitherDecodeStrict' body :: Either String Value) `shouldSatisfy` either (const True) (const False)

                it "reports real socket EOF midway through the body" $
                    withPair \sender receiver -> withinTimeout do
                        Socket.sendAll sender "\0\0\0\5hi"
                        shutdown sender ShutdownSend
                        receive (Socket.recv receiver) `shouldThrow` ((== disconnected) . ioeGetErrorString)

        it "writes the existing big-endian prefix" $
            withPair \sender receiver -> withinTimeout do
                let body = BS.replicate 256 120
                (_, received) <- concurrently
                    (sendFrame sender body >> shutdown sender ShutdownSend)
                    (readToEOF receiver)
                received `shouldBe` BS.pack [0,0,1,0] <> body

        it "retains the raw sender's empty-frame policy" $
            withPair \sender receiver -> withinTimeout do
                sendFrame sender BS.empty
                shutdown sender ShutdownSend
                readToEOF receiver `shouldReturn` BS.replicate 4 0

-- Deterministically simulate recv's short-read contract, including splitting a
-- supplied chunk at the requested boundary and returning empty only at EOF.
fragmentedReader :: [BS.ByteString] -> IO (Int -> IO BS.ByteString)
fragmentedReader chunks = do
    remaining <- newIORef chunks
    pure \requested -> do
        queued <- readIORef remaining
        case queued of
            [] -> pure BS.empty
            chunk : rest -> do
                let (bytes, leftover) = BS.splitAt requested chunk
                writeIORef remaining (if BS.null leftover then rest else leftover : rest)
                pure bytes

withPair :: (Socket -> Socket -> IO a) -> IO a
withPair action =
    bracket (socketPair AF_UNIX Stream defaultProtocol)
        (\(sender, receiver) -> close sender >> close receiver)
        (uncurry action)

withinTimeout :: IO () -> IO ()
withinTimeout action = do
    result <- timeout 5000000 action
    when (result == Nothing) (throwIO (userError "socket test timed out"))

readToEOF :: Socket -> IO BS.ByteString
readToEOF socket = do
    bytes <- Socket.recv socket 65536
    if BS.null bytes then pure BS.empty else (bytes <>) <$> readToEOF socket
