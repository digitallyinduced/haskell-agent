module Agent.GrokBuild.Dialect.Terminal (runTerminalCmdTool) where

import qualified Agent.Json.Decode as Json
import Agent.ToolDSL (PropertySchema(..), PropertyType(..))
import Agent.ToolDispatch
    ( ToolCall(..)
    , decodeToolArguments
    , typedStreamingTool
    )
import Agent.GrokBuild.Dialect.Common (jsonTool, stripAnsi)
import Agent.GrokBuild.Dialect.Json
    ( optionalBool
    , optionalIntOrString
    )
import Agent.Tools.Scheduling
    ( ToolAccess(..)
    , ToolResource(..)
    , ToolResourceClaim(..)
    )
import Agent.Tools.ShellReadOnly (shellCommandIsReadOnly)
import Agent.GrokBuild.Dialect.Shell
    ( GrokSession
    , hasUnwaitedBackgroundOp
    , runForegroundStreaming
    , startBackground
    )
import Agent.Tools.IO
    ( combineCommandOutput
    , formatCommandResult
    )
import Agent.Tools.Types
    ( AppTool
    , ToolExecutionPolicy(..)
    , withToolResourceClaims
    )
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

data TerminalArgs = TerminalArgs
    { command :: Text
    , timeout :: Maybe Int
    , description :: Text
    , background :: Bool
    }

terminalArgsDecoder :: Json.Decoder TerminalArgs
terminalArgsDecoder = Json.object $
    TerminalArgs
        <$> Json.atKey "command" Json.text
        <*> optionalIntOrString "timeout"
        <*> Json.atKey "description" Json.text
        <*> (fromMaybe False <$> optionalBool "background")

runTerminalCmdTool :: GrokSession -> AppTool
runTerminalCmdTool session =
    withToolResourceClaims terminalResourceClaims $
    jsonTool "run_terminal_cmd" terminalDescription
    [ PropertySchema "command" PropertyString True $ Just
        "The bash command to run."
    , PropertySchema "timeout" PropertyInteger False $ Just
        "Optional timeout in milliseconds (max 300000). Default: 120000 (2 minutes), enforced for foreground commands only."
    , PropertySchema "description" PropertyString True $ Just
        "One sentence explanation as to why this command needs to be run and how it contributes to the goal."
    , PropertySchema "background" PropertyBoolean False $ Just
        "Set to true for long-running commands that should run in the background (e.g., dev servers, long builds). Returns a task id immediately while the command keeps running in the background; you are notified on completion, so do not poll or sleep-wait for it."
    ]
    False
    TurnSequential
    (typedStreamingTool "run_terminal_cmd" terminalArgsDecoder (runTerminal session))

terminalResourceClaims
    :: ToolCall
    -> IO (Either Text [ToolResourceClaim])
terminalResourceClaims call =
    pure $ do
        args <- decodeToolArguments terminalArgsDecoder call.arguments
        if args.background
            then Left "background terminal commands remain exclusive"
            else if not (shellCommandIsReadOnly args.command)
                then Left "shell command is not in the read-only allowlist"
                else Right
                    [ ToolResourceClaim
                        ToolRead
                        ToolAllPaths
                    ]

terminalDescription :: Text
terminalDescription =
    "Run a bash command and return its output.\n\
    \- Always set a timeout for commands that may hang.\n\
    \- Use `$TMPDIR` for temporary files; literal `/tmp` and `/private/tmp` paths are rejected.\n\
    \- Prefer dedicated tools (read_file, grep, list_dir, search_replace) over shell equivalents when they exist."

runTerminal
    :: GrokSession
    -> (Text -> IO ())
    -> TerminalArgs
    -> IO (Either Text Text)
runTerminal session emitOutput args
    | Text.null args.description =
        pure (Left "Missing parameter: description")
    | not args.background
    , hasUnwaitedBackgroundOp args.command =
        pure $ Left
            "The command contains a background '&'. Set background=true to run it as a background task, or append `wait` if you meant to wait for the children."
    | args.background =
        startBackground session args.command
    | otherwise = do
        let timeoutMs =
                min 300000
                    (max 1 (fromMaybe 120000 args.timeout))
        result <- runForegroundStreaming
            session
            args.command
            timeoutMs
            (\out err ->
                emitOutput
                    (stripAnsi (combineCommandOutput out err)))
        pure $ stripAnsi . formatCommandResult <$> result
