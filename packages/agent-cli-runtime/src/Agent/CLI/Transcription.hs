-- | Non-interactive voice-message transcription for gateway frontends.
module Agent.CLI.Transcription
    ( transcribeAudio
    ) where

import Agent.CLI.Auth (LoadedAuth(..), loadAuth)
import Agent.Provider (Provider(XAIProvider))
import Agent.XAI.Transcription (transcribeAudioWithXAI)
import Data.Text (Text)
import qualified Data.Text as Text

transcribeAudio :: FilePath -> IO Text
transcribeAudio path =
    loadAuth (Just XAIProvider) >>= \case
        Left err -> fail (Text.unpack err)
        Right loaded ->
            transcribeAudioWithXAI loaded.loadedTokenProvider path >>= \case
                Left err -> fail (show err)
                Right transcript -> pure transcript
