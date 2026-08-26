module Agent.GrokBuild.Dialect.Terminal (runTerminalCmdTool) where

import Agent.ToolArgs (objectArgs, optBool, reqText)
import Agent.ToolDSL (PropertySchema(..), PropertyType(..))
import Agent.ToolDispatch
    ( ToolCall(..)
    , decodeToolArguments
    , toolArgumentsValue
    , typedStreamingTool
    )
import Agent.Tools.Dangerous (commandLooksLikeRmRf, forbiddenRmRfReason)
import Agent.GrokBuild.Dialect.Common (jsonTool, optionalTimeout, stripAnsi)
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
import Data.Aeson (FromJSON(..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

data TerminalArgs = TerminalArgs
    { command :: Text
    , timeout :: Maybe Int
    , description :: Text
    , background :: Bool
    }

instance FromJSON TerminalArgs where
    parseJSON = objectArgs \object -> TerminalArgs
        <$> reqText object "command"
        <*> optionalTimeout object
        <*> reqText object "description"
        <*> (fromMaybe False <$> optBool object "background")

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
    (typedStreamingTool "run_terminal_cmd" (runTerminal session))

terminalResourceClaims
    :: ToolCall
    -> IO (Either Text [ToolResourceClaim])
terminalResourceClaims call =
    pure $ do
        args <-
            decodeToolArguments (toolArgumentsValue call.arguments)
                :: Either Text TerminalArgs
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
    \- Prefer dedicated tools (read_file, grep, list_dir, search_replace) over shell equivalents when they exist."

runTerminal
    :: GrokSession
    -> (Text -> IO ())
    -> TerminalArgs
    -> IO (Either Text Text)
runTerminal session emitOutput args
    | Text.null args.description =
        pure (Left "Missing parameter: description")
    | commandLooksLikeRmRf args.command =
        pure (Left (forbiddenRmRfReason args.command))
    | not args.background && hasUnwaitedBackgroundOp args.command =
        pure $ Left
            "The command contains a background '&'. Set background=true to run it as a background task, or append `wait` if you meant to wait for the children."
    | args.background = startBackground session args.command
    | otherwise = do
        let timeoutMs = min 300000 (max 1 (fromMaybe 120000 args.timeout))
        result <- runForegroundStreaming
            session
            (Text.unpack args.command)
            timeoutMs
            (\out err ->
                emitOutput (stripAnsi (combineCommandOutput out err)))
        pure $ Right $ stripAnsi (formatCommandResult result)
