-- | Non-interactive voice-message transcription for gateway frontends.
module Agent.Runtime.Transcription
    ( transcribeAudio
    , transcribeAudioFile
    ) where

import Agent.Accounts.Auth (LoadedAuth(..), loadAuth, loadOpenAiDictationAuth)
import Agent.Provider (Provider(..), providerSlug)
import Agent.OpenAI.Transcription (openAITranscriptionSampleRate, transcribePcmWithOpenAI)
import Agent.Runtime.Gateway.Catalog (GatewayModelAccess, transcribeGatewayPcm)
import Agent.XAI.Transcription (transcribeAudioWithXAI, transcribePcmWithXAI)
import Control.Concurrent.Async (concurrently)
import Control.Exception.Safe (finally, throwString, tryAny, displayException)
import Control.Monad (when)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as Text
import System.Exit (ExitCode(..))
import System.IO (hClose)
import System.Process (CreateProcess(..), StdStream(..), proc, waitForProcess, withCreateProcess)
import System.Timeout (timeout)

transcribeAudio :: FilePath -> IO Text
transcribeAudio path =
    loadAuth (Just XAIProvider) >>= \case
        Left err -> fail (Text.unpack err)
        Right loaded ->
            transcribeAudioWithXAI loaded.loadedTokenProvider path >>= \case
                Left err -> fail (show err)
                Right transcript -> pure transcript

-- | Transcribe only the bytes already admitted by the filesystem tool. The
-- filename is descriptive, never a path opened by this function.
-- Organization sessions remain inside their gateway even on failure.
transcribeAudioFile
    :: Provider
    -> Maybe GatewayModelAccess
    -> Text
    -> Text
    -> BS.ByteString
    -> IO (Either Text Text)
transcribeAudioFile provider gateway _filename mime bytes
    | mime `notElem` ["audio/wav", "audio/mpeg"] =
        pure (Left "Audio transcription supports WAV and MP3 recordings.")
    | BS.null bytes = pure (Left "Audio recording is empty.")
    | BS.length bytes > 20 * 1024 * 1024 =
        pure (Left "Audio recording exceeds the 20 MiB limit.")
    | provider `notElem` [OpenAIProvider, XAIProvider] =
        pure (Left ("Audio transcription is not supported for " <> providerSlug provider <> " models."))
    | otherwise = do
        result <- tryAny $ timeout (10 * 60 * 1_000_000) do
            converted <- convertAudioBytes rate mime bytes
            case converted of
                Left err -> pure (Left err)
                Right pcm -> case gateway of
                    Just access -> transcribeSegments
                        (\segment -> transcribeGatewayPcm access provider (produce segment) ignoreTranscript)
                        pcm
                    Nothing -> do
                        loaded <- if provider == OpenAIProvider
                            then loadOpenAiDictationAuth
                            else loadAuth (Just XAIProvider)
                        case loaded of
                            Left err -> pure (Left err)
                            Right auth
                                | auth.loadedProvider /= provider ->
                                    pure (Left "Audio transcription requires credentials for the active provider.")
                                | otherwise -> transcribeSegments
                                    (\segment -> fmap (either (Left . Text.pack . show) Right) $
                                        if provider == OpenAIProvider
                                            then transcribePcmWithOpenAI auth.loadedTokenProvider (produce segment) ignoreTranscript
                                            else transcribePcmWithXAI auth.loadedTokenProvider (produce segment) ignoreTranscript)
                                    pcm
        pure $ case result of
            Left err -> Left (Text.pack (displayException err))
            Right Nothing -> Left "Audio transcription timed out."
            Right (Just transcript) -> transcript
  where
    rate = if provider == XAIProvider then 16_000 else openAITranscriptionSampleRate
    ignoreTranscript _ = pure ()
    produce pcm send
        | BS.null pcm = pure ()
        | otherwise = do
            let (chunk, remaining) = BS.splitAt (rate * 2 `div` 10) pcm
            send chunk
            produce remaining send
    -- A gateway request admits less than 4 MiB of PCM. Sixty-second
    -- segments also bound individual provider requests for longer recordings.
    transcribeSegments transcribe = go []
      where
        go transcripts pcm
            | BS.null pcm = pure $ Right $ Text.unwords (reverse transcripts)
            | otherwise = do
                let (segment, remaining) = BS.splitAt (rate * 2 * 60) pcm
                transcribe segment >>= \case
                    Left err -> pure (Left err)
                    Right transcript -> go (Text.strip transcript : transcripts) remaining

convertAudioBytes :: Int -> Text -> BS.ByteString -> IO (Either Text BS.ByteString)
convertAudioBytes rate mime bytes =
    withCreateProcess configuration \input output _ process ->
        case (input, output) of
            (Just inputHandle, Just outputHandle) -> do
                (_, pcm) <- concurrently
                    (BS.hPut inputHandle bytes `finally` hClose inputHandle)
                    do
                        result <- BS.hGet outputHandle (maxPcmBytes + 1)
                        when (BS.length result > maxPcmBytes) $
                            throwString "Decoded audio exceeds the 32 MiB PCM limit."
                        pure result
                exit <- waitForProcess process
                pure $ case exit of
                    ExitSuccess | not (BS.null pcm) -> Right pcm
                    _ -> Left "Could not decode the audio recording with ffmpeg."
            _ -> pure (Left "Could not open ffmpeg audio conversion pipes.")
  where
    maxPcmBytes = 32 * 1024 * 1024
    configuration = (proc "ffmpeg"
        [ "-nostdin", "-v", "error"
        , "-protocol_whitelist", "pipe"
        , "-f", if mime == "audio/wav" then "wav" else "mp3"
        , "-i", "pipe:0", "-map", "0:a:0", "-vn"
        , "-ar", show rate, "-ac", "1", "-f", "s16le", "pipe:1"
        ])
        { std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = NoStream
        }
