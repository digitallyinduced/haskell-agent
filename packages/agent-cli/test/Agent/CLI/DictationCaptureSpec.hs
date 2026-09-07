module Agent.CLI.DictationCaptureSpec (spec) where

import Agent.CLI.Dictation.Capture (withBufferedCapture)
import Control.Concurrent.Async (cancel, withAsync)
import Control.Concurrent.MVar
    ( newEmptyMVar, putMVar, readMVar, takeMVar, tryReadMVar )
import Control.Exception.Safe (finally)
import qualified Data.ByteString as BS
import Data.IORef (modifyIORef', newIORef, readIORef)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = around_ within $ describe "buffered dictation capture" do
    it "captures opening words while provider startup is blocked" do
        captured <- newEmptyMVar
        stop <- newEmptyMVar
        let capture send = do
                () <- send "opening words"
                putMVar captured ()
                takeMVar stop
                send " ending"
        result <- withBufferedCapture 1024 (pure ()) capture \produce -> do
            -- Simulate provider startup: do not request audio until capture
            -- has already happened, then stop before the provider is ready.
            takeMVar captured
            putMVar stop ()
            collect produce
        result `shouldBe` "opening words ending"

    it "announces recording once and only after nonempty PCM arrives" do
        notices <- newIORef (0 :: Int)
        let capture send = do
                readIORef notices `shouldReturn` 0
                () <- send BS.empty
                readIORef notices `shouldReturn` 0
                () <- send "first"
                readIORef notices `shouldReturn` 1
                send "second"
        result <- withBufferedCapture 1024
            (modifyIORef' notices (+ 1)) capture collect
        result `shouldBe` "firstsecond"
        readIORef notices `shouldReturn` 1

    it "replays the same recording for retries without reopening capture" do
        starts <- newIORef (0 :: Int)
        let capture send = do
                modifyIORef' starts (+ 1)
                send "whole recording"
        result <- withBufferedCapture 1024 (pure ()) capture \produce -> do
            first <- collect produce
            second <- collect produce
            pure (first, second)
        result `shouldBe` ("whole recording", "whole recording")
        readIORef starts `shouldReturn` 1

    it "propagates capture failure and joins a blocked provider" do
        providerStarted <- newEmptyMVar
        providerClosed <- newEmptyMVar
        never <- newEmptyMVar
        let capture _ =
                takeMVar providerStarted >> fail "microphone failed"
            consume _ =
                (putMVar providerStarted () >> takeMVar never)
                    `finally` putMVar providerClosed ()
        withBufferedCapture 1024 (pure ()) capture consume
            `shouldThrow` anyIOException
        tryReadMVar providerClosed `shouldReturn` Just ()

    it "joins microphone capture when authentication fails" do
        captureStarted <- newEmptyMVar
        captureClosed <- newEmptyMVar
        never <- newEmptyMVar
        let capture _ =
                (putMVar captureStarted () >> takeMVar never)
                    `finally` putMVar captureClosed ()
            consume _ =
                takeMVar captureStarted >> fail "authentication failed"
        withBufferedCapture 1024 (pure ()) capture consume
            `shouldThrow` anyIOException
        tryReadMVar captureClosed `shouldReturn` Just ()

    it "joins capture and provider on cancellation during startup" do
        captureStarted <- newEmptyMVar
        captureClosed <- newEmptyMVar
        providerStarted <- newEmptyMVar
        providerClosed <- newEmptyMVar
        never <- newEmptyMVar
        let capture _ =
                (putMVar captureStarted () >> readMVar never)
                    `finally` putMVar captureClosed ()
            consume _ =
                (putMVar providerStarted () >> readMVar never)
                    `finally` putMVar providerClosed ()
        withAsync (withBufferedCapture 1024 (pure ()) capture consume) \worker -> do
            takeMVar captureStarted
            takeMVar providerStarted
            cancel worker
        tryReadMVar captureClosed `shouldReturn` Just ()
        tryReadMVar providerClosed `shouldReturn` Just ()

    it "fails explicitly rather than losing speech when the buffer is full" do
        never <- newEmptyMVar
        let capture send = send "1234" >> send "5"
        withBufferedCapture 4 (pure ()) capture (const (takeMVar never))
            `shouldThrow` anyIOException

    it "completes an empty recording without announcing readiness" do
        notices <- newIORef (0 :: Int)
        result <- withBufferedCapture 4 (modifyIORef' notices (+ 1))
            (\send -> send BS.empty) collect
        result `shouldBe` BS.empty
        readIORef notices `shouldReturn` 0

collect :: ((BS.ByteString -> IO ()) -> IO ()) -> IO BS.ByteString
collect produce = do
    chunks <- newIORef []
    produce \bytes -> modifyIORef' chunks (bytes :)
    BS.concat . reverse <$> readIORef chunks

within :: IO () -> IO ()
within action = timeout 2_000_000 action >>= (`shouldBe` Just ())
