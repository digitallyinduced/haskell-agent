-- | xAI-backed voice-message transcription for the Telegram gateway.
module Agent.Telegram.Voice
    ( transcribeWithXAI
    ) where

import Agent.CLI.Dictation (transcribeAudio)
import Data.Text (Text)

transcribeWithXAI :: FilePath -> FilePath -> IO Text
transcribeWithXAI _cwd = transcribeAudio
