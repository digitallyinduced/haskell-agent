-- | Voice resource lifetime and limits, with caller-owned transcription.
module Agent.Telegram.VoicePreparation (withTelegramVoiceTranscript) where

import Agent.Telegram.Types.Wire (TelegramVoice(..))
import Control.Exception.Safe (bracket)
import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as Text

-- | The acquisition callback must enforce the byte limit during download and
-- clean up partially acquired files on failure. Successful resources are always
-- released, including on transcription failure or cancellation. No credentials
-- or transcription provider are selected by this library.
withTelegramVoiceTranscript
    :: TelegramVoice -> IO resource -> (resource -> IO ())
    -> (resource -> IO Text) -> IO Text
withTelegramVoiceTranscript voice acquire release transcribe = do
    when (voice.voiceDuration < 0 || voice.voiceDuration > 600) $
        fail "Telegram voice message exceeds the 10-minute limit"
    when (maybe False (\size -> size < 0 || size > 20 * 1024 * 1024) voice.voiceFileSize) $
        fail "Telegram voice message exceeds the 20 MB limit"
    bracket acquire release \resource -> do
        transcript <- Text.strip <$> transcribe resource
        when (Text.null transcript) (fail "Voice transcription was empty")
        pure transcript
