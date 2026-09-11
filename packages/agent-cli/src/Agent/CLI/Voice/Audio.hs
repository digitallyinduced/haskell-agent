{-# LANGUAGE RankNTypes #-}
-- | Scoped in-process audio. Use headphones: no acoustic echo cancellation.
module Agent.CLI.Voice.Audio (runVoiceAudio, runVoiceAudioWith, VoiceDevices(..)) where

import Agent.OpenAI.Live.Call
import Agent.WebRTC.Audio qualified as Audio
import Control.Concurrent.Async (race, race_)
import Control.Exception.Safe qualified as Safe
import Control.Monad (unless, when)
import Data.ByteString qualified as BS

-- | Scoped operations also permit device-independent lifecycle tests.
data VoiceDevices = VoiceDevices
    { captureDevice :: forall a. (IO BS.ByteString -> IO a) -> IO a
    , playbackDevice :: forall a. ((BS.ByteString -> IO ()) -> IO a) -> IO a
    }

runVoiceAudio :: LiveCall -> IO ()
runVoiceAudio = runVoiceAudioWith VoiceDevices
    { captureDevice = \use -> Audio.withCapture (use . Audio.readCapture)
    , playbackDevice = \use -> Audio.withPlayback \playback ->
        race (Audio.monitorPlayback playback) (use (Audio.writePlayback playback)) >>= \case
            Left () -> Safe.throwString "Voice playback device stopped."
            Right result -> pure result
    }

runVoiceAudioWith :: VoiceDevices -> LiveCall -> IO ()
runVoiceAudioWith VoiceDevices{captureDevice, playbackDevice} call = captureDevice \readSamples -> do
    -- Bluetooth capture changes the device's duplex format. Open playback only
    -- after receiving the first capture buffer, not merely creating the device.
    first <- readSamples
    submit first
    race_ (captureLoop readSamples) do
        ready <- awaitLiveStarted call
        when ready (playbackGenerationLoop 0)
  where
    submit bytes = do
        accepted <- submitLiveAudioWhenReady call bytes
        unless accepted $ Safe.throwString "Voice microphone queue closed or overran."
    captureLoop readSamples = readSamples >>= submit >> captureLoop readSamples
    playbackGenerationLoop generation = do
        _ <- race (awaitLivePlaybackReset call generation) $
            playbackDevice \writeSamples -> do
                playbackLoop generation writeSamples
        -- Closing the old device flushes its buffers and joins its streaming
        -- threads before a replacement opens. Capture has a separate scope.
        awaitLivePlaybackReset call generation >>= maybe (pure ()) playbackGenerationLoop
    playbackLoop generation writeSamples = readLiveAudioGeneration call generation >>= \case
        Nothing -> pure ()
        Just bytes -> writeSamples bytes >> playbackLoop generation writeSamples
