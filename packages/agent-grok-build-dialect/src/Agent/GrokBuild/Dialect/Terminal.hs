module Agent.GrokBuild.Dialect.Terminal (runTerminalCmdTool) where

import qualified Agent.Json.Decode as Json
import Agent.ToolDSL (PropertySchema(..), PropertyType(..))
import Agent.ToolDispatch
    ( ToolCall(..)
    , ToolHandlerResult(..)
    , ToolInvocationAuthorization
    , decodeToolArguments
    , typedAuthorizedStreamingRichTool
    )
import Agent.GrokBuild.Dialect.Common (stripAnsi)
import Agent.Tools.ShellPermission
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
    , runForegroundStreamingAuthorized
    , startBackgroundAuthorized
    )
import Agent.Tools.IO
    ( combineCommandOutput
    , formatCommandResult
    )
import Agent.Tools.Types
    ( AppTool
    , ApprovalRule(..)
    , ToolExecutionPolicy(..)
    , jsonAppToolWithExecution
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
    , sandboxPermission :: ShellPermissionRequest
    }

terminalArgsDecoder :: Json.Decoder TerminalArgs
terminalArgsDecoder = Json.object $
    TerminalArgs
        <$> Json.atKey "command" Json.text
        <*> optionalIntOrString "timeout"
        <*> Json.atKey "description" Json.text
        <*> (fromMaybe False <$> optionalBool "background")
        <*> shellPermissionFieldsDecoder

runTerminalCmdTool :: GrokSession -> AppTool
runTerminalCmdTool session =
    withToolResourceClaims terminalResourceClaims $
    jsonAppToolWithExecution "run_terminal_cmd" terminalDescription
    [ PropertySchema "command" PropertyString True $ Just
        "The bash command to run."
    , PropertySchema "timeout" PropertyInteger False $ Just
        "Optional timeout in milliseconds (max 300000). Default: 120000 (2 minutes), enforced for foreground commands only."
    , PropertySchema "description" PropertyString True $ Just
        "One sentence explanation as to why this command needs to be run and how it contributes to the goal."
    , PropertySchema "background" PropertyBoolean False $ Just
        "Set to true for long-running commands that should run in the background (e.g., dev servers, long builds). Returns a task id immediately while the command keeps running in the background; you are notified on completion, so do not poll or sleep-wait for it."
    , PropertySchema "sandbox_permissions" PropertyString False $ Just
        "use_default (default) or require_escalated. Full access (--yolo) auto-approves escalation; otherwise fresh user approval is required for this exact command."
    , PropertySchema "justification" PropertyString False $ Just
        "Required nonblank explanation when sandbox_permissions is require_escalated."
    ]
    (ClassifyApproval shellPermissionApproval)
    TurnSequential
    (typedAuthorizedStreamingRichTool "run_terminal_cmd" terminalArgsDecoder
        (\authorization emit args ->
            fmap (\text -> ToolHandlerResult text []) <$>
                runTerminal session authorization emit args))

terminalResourceClaims
    :: ToolCall
    -> IO (Either Text [ToolResourceClaim])
terminalResourceClaims call =
    pure $ do
        args <- decodeToolArguments terminalArgsDecoder call.arguments
        if args.sandboxPermission /= UseDefaultSandbox
            then Left "escalated terminal commands remain exclusive"
            else if args.background
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
    \- If a command fails because of the sandbox (for example SwiftPM sandbox_apply: Operation not permitted), request require_escalated with a justification; never silently retry outside the sandbox. Full access (--yolo) auto-approves this request; otherwise fresh user approval is required per command.\n\
    \- Escalated commands run from the turn cwd with a fresh shell, without replaying or updating persistent terminal cwd/environment. Use an explicit cd in the approved command if needed.\n\
    \- Always set a timeout for commands that may hang.\n\
    \- Use `$TMPDIR` as the only temporary-file root. Put task-specific subdirectories there; do not create alternate scratch directories in the home directory or workspace. Literal `/tmp` and `/private/tmp` paths are rejected.\n\
    \- Prefer dedicated tools (read_file, grep, list_dir, search_replace) over shell equivalents when they exist."

runTerminal
    :: GrokSession
    -> Maybe ToolInvocationAuthorization
    -> (Text -> IO ())
    -> TerminalArgs
    -> IO (Either Text Text)
runTerminal session invocationAuthorization emitOutput args
    | Left err <- authorizationResult = pure (Left err)
    | Text.null args.description =
        pure (Left "Missing parameter: description")
    | not args.background
    , hasUnwaitedBackgroundOp args.command =
        pure $ Left
            "The command contains a background '&'. Set background=true to run it as a background task, or append `wait` if you meant to wait for the children."
    | args.background =
        either (pure . Left)
            (\authorization -> startBackgroundAuthorized authorization session args.command)
            authorizationResult
    | otherwise = do
        let timeoutMs =
                min 300000
                    (max 1 (fromMaybe 120000 args.timeout))
        result <- either (pure . Left) (\authorization -> runForegroundStreamingAuthorized
            authorization
            session
            args.command
            timeoutMs
            (\out err ->
                emitOutput
                    (stripAnsi (combineCommandOutput out err)))) authorizationResult
        pure $ stripAnsi . formatCommandResult <$> result
  where
    authorizationResult =
        shellExecutionAuthorization invocationAuthorization args.sandboxPermission
