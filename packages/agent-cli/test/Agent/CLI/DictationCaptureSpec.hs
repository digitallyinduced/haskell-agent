module Agent.CLI.DictationCaptureSpec (spec) where

import Agent.CLI.Dictation.Capture (withBufferedCapture, withDictationCancellation)
import Agent.CLI.Dictation (saveFailedDictationRecording)
import Control.Concurrent.Async (cancel, withAsync)
import Control.Concurrent.MVar
    ( newEmptyMVar, putMVar, readMVar, takeMVar, tryReadMVar )
import Control.Exception.Safe (finally)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (isSuffixOf)
import System.Directory (listDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = around_ within $ describe "buffered dictation capture" do
    it "cancels blocked final transcription and joins both resources" do
        cancellation <- newEmptyMVar
        finalizing <- newEmptyMVar
        captureClosed <- newEmptyMVar
        providerClosed <- newEmptyMVar
        never <- newEmptyMVar
        let capture send =
                send "speech"
                    `finally` putMVar captureClosed ()
            consume produce =
                (produce (const (pure ())) >> putMVar finalizing () >> readMVar never)
                    `finally` putMVar providerClosed ()
        withAsync
            (takeMVar finalizing >> putMVar cancellation ())
            \_ -> do
                withDictationCancellation (readMVar cancellation)
                    (withBufferedCapture 1024 (pure ()) capture consume)
                    `shouldReturn` (Nothing :: Maybe ())
        tryReadMVar captureClosed `shouldReturn` Just ()
        tryReadMVar providerClosed `shouldReturn` Just ()

    it "cancels provider startup and joins an active microphone" do
        cancellation <- newEmptyMVar
        captured <- newEmptyMVar
        providerStarted <- newEmptyMVar
        captureClosed <- newEmptyMVar
        providerClosed <- newEmptyMVar
        never <- newEmptyMVar
        let capture send =
                (send "speech" >> putMVar captured () >> readMVar never)
                    `finally` putMVar captureClosed ()
            consume _ =
                (putMVar providerStarted () >> readMVar never)
                    `finally` putMVar providerClosed ()
        withAsync
            (takeMVar captured >> takeMVar providerStarted >> putMVar cancellation ())
            \_ -> do
                withDictationCancellation (readMVar cancellation)
                    (withBufferedCapture 1024 (pure ()) capture consume)
                    `shouldReturn` (Nothing :: Maybe ())
        tryReadMVar captureClosed `shouldReturn` Just ()
        tryReadMVar providerClosed `shouldReturn` Just ()

    it "preserves completed transcription when no cancellation is requested" do
        cancellation <- newEmptyMVar
        withDictationCancellation (readMVar cancellation) (pure ("speech" :: BS.ByteString))
            `shouldReturn` Just "speech"

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

    it "saves the original buffered audio as WAV after transcription fails" do
        withSystemTempDirectory "dictation-recovery" \root -> do
            starts <- newIORef (0 :: Int)
            let capture send = do
                    modifyIORef' starts (+ 1)
                    send "\x01\x02"
                    send "\x03\x04"
                directory = root </> "recordings"
            saved <- withBufferedCapture 1024 (pure ()) capture \produce -> do
                collect produce `shouldReturn` "\x01\x02\x03\x04"
                saveFailedDictationRecording directory 24_000 produce
            path <- case saved of
                Just path -> pure path
                Nothing -> expectationFailure "recording was not saved" >> pure ""
            (".wav" `isSuffixOf` path) `shouldBe` True
            wav <- LBS.readFile path
            LBS.take 4 wav `shouldBe` "RIFF"
            LBS.take 4 (LBS.drop 8 wav) `shouldBe` "WAVE"
            LBS.drop 44 wav `shouldBe` "\x01\x02\x03\x04"
            readIORef starts `shouldReturn` 1

    it "does not create an empty fallback recording" do
        withSystemTempDirectory "dictation-recovery" \root -> do
            let directory = root </> "recordings"
            saveFailedDictationRecording directory 24_000 (\_ -> pure ())
                `shouldReturn` Nothing
            listDirectory root `shouldReturn` []

collect :: ((BS.ByteString -> IO ()) -> IO ()) -> IO BS.ByteString
collect produce = do
    chunks <- newIORef []
    produce \bytes -> modifyIORef' chunks (bytes :)
    BS.concat . reverse <$> readIORef chunks

within :: IO () -> IO ()
within action = timeout 2_000_000 action >>= (`shouldBe` Just ())
