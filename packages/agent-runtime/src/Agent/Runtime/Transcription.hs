-- | Non-interactive voice-message transcription for gateway frontends.
module Agent.Runtime.Transcription
    ( transcribeAudio
    , streamGatewayDictationAudio
    ) where

import Agent.Accounts.Auth (LoadedAuth(..), loadAuth)
import Agent.OpenAI.Transcription (openAITranscriptionSampleRate)
import Agent.Provider (Provider(XAIProvider))
import Agent.XAI.Transcription (transcribeAudioWithXAI)
import Control.Exception.Safe (bracket, tryAny)
import Control.Monad (unless, void)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory (findExecutable)
import System.Exit (ExitCode(..))
import System.IO (hClose)
import System.Process
    ( CreateProcess(..)
    , StdStream(..)
    , createProcess
    , proc
    , terminateProcess
    , waitForProcess
    )

transcribeAudio :: FilePath -> IO Text
transcribeAudio path =
    loadAuth (Just XAIProvider) >>= \case
        Left err -> fail (Text.unpack err)
        Right loaded ->
            transcribeAudioWithXAI loaded.loadedTokenProvider path >>= \case
                Left err -> fail (show err)
                Right transcript -> pure transcript

-- | Convert an audio file to the PCM16 sample rate used by organization-gateway
-- dictation. Telegram voice notes are typically OGG/Opus; ffmpeg is the same
-- converter the CLI microphone path already requires.
streamGatewayDictationAudio
    :: FilePath
    -> (BS.ByteString -> IO ())
    -> IO ()
streamGatewayDictationAudio audioPath sendAudio = do
    findExecutable "ffmpeg" >>= \case
        Nothing ->
            fail
                "ffmpeg is required to transcribe voice messages but was not found on PATH"
        Just _ ->
            bracket start stop \(output, processHandle) -> do
                let loop = do
                        bytes <- BS.hGetSome output chunkBytes
                        unless (BS.null bytes) do
                            sendAudio bytes
                            loop
                loop
                waitForProcess processHandle >>= \case
                    ExitSuccess -> pure ()
                    ExitFailure code ->
                        fail $
                            "ffmpeg audio conversion failed (exit "
                                <> show code
                                <> ")"
  where
    -- 100 ms of mono signed PCM16 at the gateway dictation sample rate.
    chunkBytes = openAITranscriptionSampleRate * 2 `div` 10
    start = do
        (_, Just output, _, processHandle) <-
            createProcess
                (proc "ffmpeg"
                    [ "-hide_banner"
                    , "-loglevel", "error"
                    , "-i", audioPath
                    , "-f", "s16le"
                    , "-acodec", "pcm_s16le"
                    , "-ac", "1"
                    , "-ar", show openAITranscriptionSampleRate
                    , "pipe:1"
                    ])
                { std_out = CreatePipe
                , std_err = Inherit
                }
        pure (output, processHandle)
    stop (output, processHandle) = do
        void (tryAny (hClose output))
        void (tryAny (terminateProcess processHandle))
        void (tryAny (waitForProcess processHandle))
