-- | Trusted, non-persisted secret entry for the line-oriented CLI.
module Agent.CLI.Secret
    ( promptSecretLine
    , sanitizeSecretPromptText
    , secretPromptMessage
    ) where

import Agent.CLI.CancelWatch (StdinControl, withStdinPaused)
import Agent.CLI.Notification
    ( AttentionRequest(SecretRequested)
    , notifyAttention
    )
import Control.Exception.Safe (bracket_, finally, tryIO)
import Data.Char (isControl)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.IO
    ( hFlush
    , hGetEcho
    , hIsTerminalDevice
    , hSetEcho
    , stderr
    , stdin
    )

-- | Ask for one line of secret text without echoing it.
--
-- The value is read directly from the terminal and never passes through the
-- ordinary REPL input path or its history. An unavailable terminal or empty
-- value returns 'Nothing'.
promptSecretLine
    :: StdinControl
    -> Text
    -> Maybe Text
    -> IO (Maybe Text)
promptSecretLine stdinControl prompt purpose =
    withStdinPaused stdinControl do
        tty <- hIsTerminalDevice stdin
        if not tty
            then pure Nothing
            else do
                notifyAttention stderr SecretRequested
                Text.hPutStr stderr (secretPromptMessage prompt purpose)
                hFlush stderr
                result <-
                    tryIO do
                        oldEcho <- hGetEcho stdin
                        bracket_
                            (hSetEcho stdin False)
                            (hSetEcho stdin oldEcho)
                            Text.getLine
                        `finally` Text.hPutStrLn stderr ""
                pure case result of
                    Left _ -> Nothing
                    Right value
                        | Text.null value -> Nothing
                        | otherwise -> Just value

-- | Trusted chrome shown before the no-echo input field.
secretPromptMessage :: Text -> Maybe Text -> Text
secretPromptMessage prompt purpose =
    Text.unlines
        ( [ "Secret requested by agent"
          ]
            <> [ "Purpose: " <> sanitizeDisplayText value
               | Just value <- [purpose]
               , not (Text.null (Text.strip value))
               ]
            <> [ sanitizeDisplayText prompt
               , "Input is hidden and is not added to conversation history."
               ]
        )
        <> "secret> "

sanitizeDisplayText :: Text -> Text
sanitizeDisplayText = sanitizeSecretPromptText

-- | Constrain model-controlled prompt chrome to one terminal-safe line.
--
-- This replaces every control character, including line endings, tabs, escape
-- sequences, and bell characters. Callers remain responsible for adding any
-- trusted layout around the sanitized text.
sanitizeSecretPromptText :: Text -> Text
sanitizeSecretPromptText =
    Text.map \character ->
        if isControl character
            then ' '
            else character
