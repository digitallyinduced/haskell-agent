-- | Error values corresponding to the public error family in Anthropic's
-- Claude Agent SDKs.
module Claude.Agent.SDK.Errors
    ( ClaudeSDKError(..)
    , renderClaudeSDKError
    ) where

import Control.Exception (Exception)
import Data.Aeson (Value)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Exit (ExitCode)

data ClaudeSDKError
    = CLIConnectionError !Text
    | CLIProtocolError !Text
    | CLINotFoundError !FilePath
    | ProcessError
        { message :: !Text
        , exitCode :: !(Maybe ExitCode)
        , stderr :: !Text
        }
    | ResultError
        { subtype :: !Text
        , apiErrorStatus :: !(Maybe Int)
        , errors :: ![Text]
        , result :: !(Maybe Text)
        }
    | CLIJSONDecodeError
        { decodeError :: !Text
        , rawBody :: !Text
        }
    | MessageParseError
        { parseError :: !Text
        , rawMessage :: !(Maybe Value)
        }
    deriving (Eq, Show)

instance Exception ClaudeSDKError

renderClaudeSDKError :: ClaudeSDKError -> Text
renderClaudeSDKError = \case
    CLIConnectionError message ->
        message
    CLIProtocolError message ->
        message
    CLINotFoundError executable ->
        "Claude Code executable not found: " <> Text.pack executable
    ProcessError{message, exitCode, stderr} ->
        message
            <> maybe "" ((" (" <>) . (<> ")") . Text.pack . show) exitCode
            <> if Text.null (Text.strip stderr)
                then ""
                else "\nClaude Code stderr:\n" <> Text.takeEnd 2_000 stderr
    ResultError{subtype, apiErrorStatus, errors, result} ->
        "Claude Code "
            <> subtype
            <> maybe
                ""
                (\status -> " (HTTP " <> Text.pack (show status) <> ")")
                apiErrorStatus
            <> ": "
            <> case errors of
                first : rest -> Text.intercalate "; " (first : rest)
                [] -> maybe "request failed" id result
    CLIJSONDecodeError{decodeError} ->
        "Invalid Claude Code stream JSON: " <> decodeError
    MessageParseError{parseError} ->
        "Invalid Claude Code message: " <> parseError
