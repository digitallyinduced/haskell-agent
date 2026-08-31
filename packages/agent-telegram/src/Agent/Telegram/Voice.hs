-- | xAI-backed voice-message transcription for the Telegram gateway.
module Agent.Telegram.Voice
    ( transcribeWithXAI
    ) where

import Agent.CLI.Transcription (transcribeAudio)
import Data.Text (Text)

transcribeWithXAI :: FilePath -> FilePath -> IO Text
transcribeWithXAI _cwd = transcribeAudio
