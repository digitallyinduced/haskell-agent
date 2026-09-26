-- | Voice-message transcription for the Telegram gateway.
--
-- A connected organization gateway is the only allowed STT transport. Local
-- xAI credentials are used only when no gateway credential is present.
module Agent.Telegram.Voice
    ( TelegramVoiceTranscriptionTarget(..)
    , telegramVoiceTranscriptionTarget
    , transcribeTelegramVoiceAudio
    , transcribeTelegramVoiceAudioWith
    , transcribeWithXAI
    ) where

import Agent.Runtime.GatewayClient
    ( GatewayModelAccess
    , loadGatewayCredential
    , newGatewayModelAccess
    , transcribeGatewayPcm
    )
import Agent.Runtime.Transcription
    ( streamGatewayDictationAudio
    , transcribeAudio
    )
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as Text

-- | Voice STT is either the historical direct-xAI path or routed entirely
-- through the organization gateway. Keeping these constructors distinct
-- prevents a gateway session from falling back to local credentials.
data TelegramVoiceTranscriptionTarget
    = DirectXAIVoiceTranscription
    | GatewayVoiceTranscription !GatewayModelAccess

-- | Select the only transcription transport allowed by the current credential
-- boundary. A connected gateway is authoritative regardless of local xAI,
-- OpenAI, or Codex logins.
telegramVoiceTranscriptionTarget
    :: Maybe GatewayModelAccess
    -> TelegramVoiceTranscriptionTarget
telegramVoiceTranscriptionTarget =
    maybe DirectXAIVoiceTranscription GatewayVoiceTranscription

transcribeWithXAI :: FilePath -> FilePath -> IO Text
transcribeWithXAI = transcribeTelegramVoiceAudio

transcribeTelegramVoiceAudio :: FilePath -> FilePath -> IO Text
transcribeTelegramVoiceAudio _cwd audioPath = do
    target <- loadGatewayCredential >>= \case
        Left err ->
            fail (Text.unpack ("Could not load gateway credentials: " <> err))
        Right Nothing ->
            pure DirectXAIVoiceTranscription
        Right (Just credential) ->
            GatewayVoiceTranscription <$> newGatewayModelAccess credential
    transcribeTelegramVoiceAudioWith
        target
        transcribeAudio
        streamGatewayDictationAudio
        audioPath

-- | Injectable transcription used by Voice/Turn tests. Production always
-- supplies the real xAI fallback and ffmpeg PCM conversion; tests assert that
-- a gateway target never invokes that fallback.
transcribeTelegramVoiceAudioWith
    :: TelegramVoiceTranscriptionTarget
    -> (FilePath -> IO Text)
    -> (FilePath -> ((BS.ByteString -> IO ()) -> IO ()))
    -> FilePath
    -> IO Text
transcribeTelegramVoiceAudioWith target transcribeDirect streamPcm audioPath =
    case target of
        DirectXAIVoiceTranscription ->
            transcribeDirect audioPath
        GatewayVoiceTranscription gateway ->
            transcribeGatewayPcm
                gateway
                (streamPcm audioPath)
                (const (pure ())) >>= \case
                    Left err -> fail (Text.unpack err)
                    Right transcript -> pure transcript

