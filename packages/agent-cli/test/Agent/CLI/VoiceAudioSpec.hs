module Agent.CLI.VoiceAudioSpec (spec) where

import Agent.CLI.Voice.Audio
import Agent.OpenAI.Live
import Agent.OpenAI.Live.Call
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception.Safe (bracket_, finally)
import Data.ByteString qualified as BS
import Data.IORef
import Data.Either (isLeft)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "In-process voice devices" do
    it "keeps capture alive while joining and replacing interrupted playback" do
        captureInput <- newTQueueIO
        written <- newTQueueIO
        opened <- newTQueueIO
        captureClosed <- newIORef False
        playbackCount <- newIORef (0 :: Int)
        playbackClosed <- newIORef ([] :: [Int])
        let devices = VoiceDevices
                { captureDevice = \use -> use (atomically (readTQueue captureInput))
                    `finally` writeIORef captureClosed True
                , playbackDevice = \use -> do
                    index <- atomicModifyIORef' playbackCount (\n -> (n + 1, n + 1))
                    bracket_ (atomically (writeTQueue opened index))
                        (modifyIORef' playbackClosed (<> [index]))
                        (use (\bytes -> atomically (writeTQueue written (index, bytes))))
                }
            connect next receive = do
                receive LiveStarted
                atomically (writeTQueue captureInput (BS.pack [1,0]))
                next `shouldReturn` LiveInputAudio (BS.pack [1,0])
                atomically (readTQueue opened) `shouldReturn` 1
                receive (LiveTranscript LiveAssistant False "first")
                receive (LiveAudio (BS.pack [2,0]))
                atomically (readTQueue written) `shouldReturn` (1, BS.pack [2,0])
                receive (LiveTranscript LiveUser False "interrupt")
                atomically (readTQueue opened) `shouldReturn` 2
                readIORef playbackClosed `shouldReturn` [1]
                readIORef captureClosed `shouldReturn` False
                atomically (writeTQueue captureInput (BS.pack [3,0]))
                next `shouldReturn` LiveInputAudio (BS.pack [3,0])
                receive (LiveTranscript LiveAssistant True "first")
                receive (LiveTranscript LiveUser True "interrupt")
                receive (LiveTranscript LiveAssistant False "second")
                receive (LiveAudio (BS.pack [4,0]))
                atomically (readTQueue written) `shouldReturn` (2, BS.pack [4,0])
                pure (Right ())
        timeout 2_000_000 (runLiveCallWith connect (\_ _ -> pure "") (const (pure ())) (runVoiceAudioWith devices))
            `shouldReturn` Just (Right ())
        readIORef captureClosed `shouldReturn` True
        readIORef playbackClosed `shouldReturn` [1,2]

    it "joins capture when opening playback fails" do
        first <- newMVar (BS.pack [1,0])
        closed <- newIORef False
        let devices = VoiceDevices
                { captureDevice = \use -> use (takeMVar first) `finally` writeIORef closed True
                , playbackDevice = \_ -> fail "device unavailable"
                }
            connect next receive = receive LiveStarted >> next >> next >> pure (Right ())
        result <- timeout 2_000_000 (runLiveCallWith connect (\_ _ -> pure "") (const (pure ())) (runVoiceAudioWith devices))
        result `shouldSatisfy` maybe False isLeft
        readIORef closed `shouldReturn` True
