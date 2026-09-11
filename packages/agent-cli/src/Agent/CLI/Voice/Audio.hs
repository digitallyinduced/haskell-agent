-- | Scoped desktop audio for Live calls. Requires ffmpeg and ffplay on PATH.
-- Use headphones: this adapter does not provide acoustic echo cancellation.
module Agent.CLI.Voice.Audio (runVoiceAudio, playbackHeader, playbackSamples) where

import Agent.OpenAI.Live.Call
import Agent.Process (terminateProcessGroup)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race, race_, withAsyncWithUnmask, wait)
import Control.Exception.Safe qualified as Safe
import Control.Monad (unless, when, void)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Data.Int (Int16)
import Data.Maybe (catMaybes)
import Debug.Trace (traceIO)
import System.Info qualified as System
import System.IO (Handle, hClose, hSetBinaryMode, hSetBuffering, BufferMode(NoBuffering))
import System.Process qualified as Process
import System.Timeout qualified as Timeout

-- | Warm up capture concurrently with connection setup. Returning
-- from either device stops its peer. The call owner must scope this action
-- with runLiveCall; cancellation then stops both child process groups.
runVoiceAudio :: LiveCall -> IO ()
runVoiceAudio call = do
        input <- case System.os of
            "darwin" -> pure ["-f", "avfoundation", "-i", ":default"]
            "linux" -> pure ["-f", "pulse", "-i", "default"]
            _ -> Safe.throwString "Voice audio is unsupported on this platform."
        withAudioProcess "ffmpeg"
            (["-nostdin", "-hide_banner", "-loglevel", "error"] <> input <>
             ["-ac", "1", "-ar", "24000", "-c:a", "pcm_s16le", "-f", "s16le", "-flush_packets", "1", "pipe:1"])
            False $ \capture -> do
                -- Opening a Bluetooth microphone switches the device into its
                -- duplex format. Wait for actual capture, not merely process
                -- creation, before opening playback against that device.
                first <- BS.hGetSome capture 1920
                traceIO "Voice: microphone capture started."
                when (BS.null first) $ Safe.throwString "Voice microphone disconnected."
                race_ (captureLoop capture first) do
                    ready <- awaitLiveStarted call
                    when ready (playbackGenerationLoop 0)
  where
    playbackGenerationLoop generation = do
        _ <- race (awaitLivePlaybackReset call generation) $
            withAudioProcess "ffplay"
                    ["-nodisp", "-autoexit", "-loglevel", "error", "-f", "wav",
                     "-ignore_length", "1", "-max_size", "1920", "-probesize", "32",
                     "-analyzeduration", "1", "-i", "pipe:0"]
                    True $ \playback -> do
                        traceIO "Voice: playback process started."
                        BS.hPut playback playbackHeader
                        playbackLoop generation playback
        -- The old process and its writer have been joined before reopening.
        awaitLivePlaybackReset call generation >>= maybe (pure ()) playbackGenerationLoop
    captureLoop handle carry = do
        bytes <- BS.hGetSome handle 1920
        when (BS.null bytes) $ Safe.throwString "Voice microphone disconnected."
        let combined = carry <> bytes
            sampleBytes = BS.length combined - BS.length combined `mod` 2
            (complete, remainder) = BS.splitAt sampleBytes combined
        unless (BS.null complete) do
            accepted <- submitLiveAudioWhenReady call complete
            unless accepted $ Safe.throwString "Voice microphone queue closed or overran."
        captureLoop handle remainder
    playbackLoop generation handle = readLiveAudioGeneration call generation >>= \case
        Nothing -> pure ()
        Just bytes -> case playbackSamples bytes of
            Nothing -> Safe.throwString "Voice playback received an incomplete sample."
            Just samples -> BS.hPut handle samples >> playbackLoop generation handle

-- Raw PCM's FFmpeg demuxer batches 2048 samples (85 ms at 24 kHz).
-- WAV permits 20 ms packets, but integer WAV probes 64 KiB for SPDIF.
-- IEEE float WAV avoids both waits. PCM16 -> Float is exact; no resampling.
-- Unknown lengths plus -ignore_length keep the pipe open until hangup.
playbackHeader :: BS.ByteString
playbackHeader = LBS.toStrict $ Builder.toLazyByteString $
    Builder.string8 "RIFF" <> Builder.word32LE maxBound <> Builder.string8 "WAVEfmt " <>
    Builder.word32LE 16 <> Builder.word16LE 3 <> Builder.word16LE 1 <>
    Builder.word32LE 24_000 <> Builder.word32LE 96_000 <>
    Builder.word16LE 4 <> Builder.word16LE 32 <>
    Builder.string8 "data" <> Builder.word32LE maxBound

playbackSamples :: BS.ByteString -> Maybe BS.ByteString
playbackSamples pcm
    | odd (BS.length pcm) = Nothing
    | otherwise = Just $ LBS.toStrict $ Builder.toLazyByteString $ go 0
  where
    go offset
        | offset == BS.length pcm = mempty
        | otherwise =
            let sample = fromIntegral (BS.index pcm offset) +
                    256 * fromIntegral (BS.index pcm (offset + 1)) :: Int16
            in Builder.floatLE (fromIntegral sample / 32768) <> go (offset + 2)

withAudioProcess :: FilePath -> [String] -> Bool -> (Handle -> IO a) -> IO a
withAudioProcess executable arguments writing use =
    Safe.bracket acquire release $ \(input, output, _, process) ->
        case if writing then input else output of
            Nothing -> Safe.throwString "Could not open voice audio pipe."
            Just handle -> do
                hSetBinaryMode handle True
                hSetBuffering handle NoBuffering
                race (monitor process) (use handle) >>= \case
                    Left _ -> Safe.throwString "Voice audio device process exited."
                    Right result -> pure result
  where
    acquire = Process.createProcess (Process.proc executable arguments)
        { Process.std_in = if writing then Process.CreatePipe else Process.NoStream
        , Process.std_out = if writing then Process.NoStream else Process.CreatePipe
        , Process.std_err = Process.Inherit
        , Process.create_group = True
        }
    -- Never cancel a blocking process waiter before terminating its child:
    -- joining that waiter can prevent the owning bracket from reaching release.
    monitor process = Process.getProcessExitCode process >>= \case
        Nothing -> threadDelay 50_000 >> monitor process
        Just result -> pure result
    -- safe-exceptions runs release masked. Use one unmasked, joined child so
    -- the deadline also applies to pipe close and process reaping.
    release (input, output, err, process) =
        withAsyncWithUnmask (\unmask -> unmask $ void $ Timeout.timeout 4_000_000 do
            group <- Process.getPid process
            terminateProcessGroup group process
            mapM_ (void . Safe.tryAny . hClose) (catMaybes [input, output, err])
            void (Process.waitForProcess process)) wait
