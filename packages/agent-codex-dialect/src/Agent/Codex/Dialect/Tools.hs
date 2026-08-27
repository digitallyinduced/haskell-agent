-- | OpenAI Codex coding tools.
--
-- Wire names and schemas are copied from openai/codex
-- @codex-rs/core/src/tools/handlers for the Codex-native tools.
-- run_ghci is a local extension shared with Grok/OpenRouter.
-- Multi-agent v1 tools are optional and registered when a registry is supplied.
module Agent.Codex.Dialect.Tools
    ( codexTools
    , shellCommandIsReadOnly
    ) where

import Agent.OsPath (fromText)
import qualified Agent.Json.Decode as Json
import Agent.ToolDSL
    ( PropertySchema(..)
    , PropertyType(..)
    )
import Agent.ToolDispatch
    ( ToolCall(..)
    , ToolCallKind(..)
    , decodeToolArguments
    , typedStreamingTool
    , typedTool
    )
import Agent.Codex.Dialect.ApplyPatch
    ( Hunk(..)
    , applyPatch
    , parsePatch
    )
import Agent.Codex.Dialect.Shell
    ( CodexShellResult(..)
    , CodexShellSession
    , continueCodexShellCommand
    , startCodexShellCommand
    )
import Agent.Tools.Ghci (GhciSession, runGhciTool)
import Agent.Tools.Dangerous (forbiddenRmRfReason, commandLooksLikeRmRf)
import Agent.Tools.FileSystem.Grep (grepTool)
import Agent.Tools.FileSystem.ListDir (listDirTool)
import Agent.Tools.FileSystem.ReadFile (readFileTool)
import Agent.Tools.IO
    ( CommandResult(..)
    , commandResultArtifacts
    , resolveUnderCwd
    , runShellCommandStreaming
    )
import Agent.Tools.MultiAgents (MultiAgentContext, multiAgentTools)
import Agent.Tools.PlanMode
    ( PlanModeEnv
    , askUserQuestionTool
    , enterCodexPlanModeTool
    , isPlanModeActive
    , writePlanTool
    )
import Agent.Tools.Scheduling
    ( ToolAccess(..)
    , ToolResource(..)
    , ToolResourceClaim(..)
    )
import Agent.Tools.ShellReadOnly (shellCommandIsReadOnly)
import Agent.Tools.Types
    ( AppTool
    , ApprovalRule(..)
    , ToolExecutionPolicy(..)
    , ToolEnv(..)
    , freeformApplyPatchAppToolWithExecution
    , jsonAppToolWithExecution
    , jsonTool
    , withToolResourceClaims
    )
import Control.Monad (join)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as Text
import System.OsPath (unsafeEncodeUtf)

codexTools
    :: ToolEnv
    -> CodexShellSession
    -> GhciSession
    -> PlanModeEnv
    -> Maybe MultiAgentContext
    -> IO [AppTool]
codexTools env shellSession ghci planMode multi =
    pure $
        [ runGhciTool ghci
        , readFileTool env
        , grepTool env
        , listDirTool env
        , applyPatchTool env
        , updatePlanTool planMode
        , enterCodexPlanModeTool planMode
        , writePlanTool planMode
        , askUserQuestionTool planMode
        , shellCommandTool env shellSession
        , writeStdinTool shellSession
        ]
        ++ maybe [] multiAgentTools multi

--------------------------------------------------------------------------------
-- shell_command
--------------------------------------------------------------------------------

data ShellCommandArgs = ShellCommandArgs
    { command :: Text
    , workdir :: Maybe Text
    , timeoutMs :: Maybe Int
    , yieldTimeMs :: Maybe Int
    }

shellCommandArgsDecoder :: Json.Decoder ShellCommandArgs
shellCommandArgsDecoder = Json.object $
    ShellCommandArgs
        <$> Json.atKey "command" Json.text
        <*> optionalText "workdir"
        <*> optionalIntOrString "timeout_ms"
        <*> optionalIntOrString "yield_time_ms"

shellCommandTool :: ToolEnv -> CodexShellSession -> AppTool
shellCommandTool env session =
    withToolResourceClaims (shellCommandResourceClaims env) $
    jsonTool "shell_command" shellDescription
    [ PropertySchema "command" PropertyString True $ Just
        "Shell script to run in the user's default shell."
    , PropertySchema "workdir" PropertyString False $ Just
        "Working directory for the command. Defaults to the turn cwd."
    , PropertySchema "timeout_ms" PropertyInteger False $ Just
        "Maximum foreground command runtime. Defaults to 10000 ms. Mutually exclusive with yield_time_ms."
    , PropertySchema "yield_time_ms" PropertyInteger False $ Just
        "Return a session_id if the command is still running after this many milliseconds. Mutually exclusive with timeout_ms."
    ]
    False
    TurnSequential
    (typedStreamingTool "shell_command" shellCommandArgsDecoder (runShell env session))

shellCommandResourceClaims
    :: ToolEnv
    -> ToolCall
    -> IO (Either Text [ToolResourceClaim])
shellCommandResourceClaims env call =
    case decodeToolArguments shellCommandArgsDecoder call.arguments of
        Left err -> pure (Left err)
        Right args
            | args.yieldTimeMs /= Nothing ->
                pure (Left "yielding shell commands remain exclusive")
            | not (shellCommandIsReadOnly args.command) ->
                pure (Left "shell command is not in the read-only allowlist")
            | otherwise -> do
                let requested = maybe env.toolCwd fromText args.workdir
                resolveUnderCwd env requested
                    >>= pure . fmap
                        (\_ ->
                            [ ToolResourceClaim
                                ToolRead
                                ToolAllPaths
                            ])

shellDescription :: Text
shellDescription =
    "Runs a shell command and returns its output.\n\
    \- Always set the `workdir` param when using the shell_command function. Do not use `cd` unless absolutely necessary.\n\
    \- For a long-running command, set `yield_time_ms`; if it is still running, use `write_stdin` with the returned session_id to poll or send input."

runShell
    :: ToolEnv
    -> CodexShellSession
    -> (Text -> IO ())
    -> ShellCommandArgs
    -> IO (Either Text Text)
runShell env session emitOutput args
    | commandLooksLikeRmRf args.command =
        pure (Left (forbiddenRmRfReason args.command))
    | args.timeoutMs /= Nothing && args.yieldTimeMs /= Nothing =
        pure (Left "timeout_ms and yield_time_ms are mutually exclusive")
    | otherwise = do
        workdir <- case args.workdir of
            Nothing -> pure (Right env.toolCwd)
            Just dir -> resolveUnderCwd env (fromText dir)
        case workdir of
            Left err -> pure (Left err)
            Right dir -> case args.yieldTimeMs of
                Just requestedYield -> do
                    let yieldMs = clampMs requestedYield
                    startCodexShellCommand
                        session
                        dir
                        (Text.unpack args.command)
                        yieldMs
                        (\out err -> emitOutput (commandBody out err))
                        >>= pure . fmap renderShellResult
                Nothing -> do
                    let timeoutMs = clampMs (fromMaybe 10000 args.timeoutMs)
                    result <- runShellCommandStreaming
                        env
                        dir
                        (Text.unpack args.command)
                        timeoutMs
                        (\out err -> emitOutput (commandBody out err))
                    if result.commandCancelled
                        then pure $ Left "Error: Command cancelled"
                        else if result.commandTimedOut
                        then pure $ Left $
                            "Error: Command timed out after " <> Text.pack (show timeoutMs) <> "ms"
                        else pure $ Right $ renderFinished result

data WriteStdinArgs = WriteStdinArgs
    { sessionId :: Int
    , chars :: Maybe Text
    , yieldTimeMs :: Maybe Int
    }

writeStdinArgsDecoder :: Json.Decoder WriteStdinArgs
writeStdinArgsDecoder = Json.object $
    WriteStdinArgs
        <$> Json.atKey "session_id" Json.int
        <*> optionalText "chars"
        <*> optionalIntOrString "yield_time_ms"

writeStdinTool :: CodexShellSession -> AppTool
writeStdinTool session =
    withToolResourceClaims writeStdinResourceClaims $
    jsonAppToolWithExecution "write_stdin" writeStdinDescription
        [ PropertySchema "session_id" PropertyInteger True $ Just
            "Identifier returned by shell_command for a running command."
        , PropertySchema "chars" PropertyString False $ Just
            "Text to write to stdin. Omit or use an empty string to poll without writing. Use \\u0003 to interrupt."
        , PropertySchema "yield_time_ms" PropertyInteger False $ Just
            "Wait before returning output. Defaults to 5000 ms; maximum 300000 ms."
        ]
        (ClassifyReadOnly writeStdinIsReadOnly)
        TurnSequential
        (typedTool "write_stdin" writeStdinArgsDecoder (runWriteStdin session))

writeStdinResourceClaims
    :: ToolCall
    -> IO (Either Text [ToolResourceClaim])
writeStdinResourceClaims call =
    pure $ do
        args <- decodeToolArguments writeStdinArgsDecoder call.arguments
        Right
            [ ToolResourceClaim ToolWrite $
                ToolNamedResource
                    ("shell-session:" <> Text.pack (show args.sessionId))
            ]

writeStdinDescription :: Text
writeStdinDescription =
    "Writes text to or polls an existing shell_command session and returns newly produced output."

writeStdinIsReadOnly :: ToolCall -> IO Bool
writeStdinIsReadOnly call =
    pure $ case decodeToolArguments writeStdinArgsDecoder call.arguments of
        Right args -> maybe True Text.null args.chars
        Left _ -> True

runWriteStdin
    :: CodexShellSession
    -> WriteStdinArgs
    -> IO (Either Text Text)
runWriteStdin session args = do
    let yieldMs = clampMs (fromMaybe 5000 args.yieldTimeMs)
    fmap renderShellResult <$> continueCodexShellCommand
        session
        args.sessionId
        (fromMaybe "" args.chars)
        yieldMs

clampMs :: Int -> Int
clampMs = min 300000 . max 1

renderShellResult :: CodexShellResult -> Text
renderShellResult = \case
    CodexShellFinished result -> renderFinished result
    CodexShellRunning sessionId out err ->
        "Process still running.\n\
        \session_id: " <> Text.pack (show sessionId) <> "\n"
            <> commandBody out err

renderFinished :: CommandResult -> Text
renderFinished result =
    let output = commandBody result.commandStdout result.commandStderr
        artifacts = commandResultArtifacts result
        body
            | Text.null output = artifacts
            | Text.null artifacts = output
            | otherwise = output <> "\n" <> artifacts
    in if result.commandCancelled
        then "Error: Command cancelled\n" <> body
        else if result.commandTimedOut
            then "Error: Command timed out\n" <> body
            else
                let code = fromMaybe 1 result.commandExitCode
                in "Exit code: " <> Text.pack (show code) <> "\n" <> body

commandBody :: Text -> Text -> Text
commandBody out err
    | Text.null err = out
    | Text.null out = err
    | otherwise = out <> "\nstderr:\n" <> err


--------------------------------------------------------------------------------
-- apply_patch
--------------------------------------------------------------------------------

newtype ApplyPatchArgs = ApplyPatchArgs { patch :: Text }

applyPatchArgsDecoder :: Json.Decoder ApplyPatchArgs
applyPatchArgsDecoder = Json.withType \case
    Json.VString -> ApplyPatchArgs <$> Json.text
    Json.VObject -> Json.object $
        ApplyPatchArgs <$> firstPresentText ["input", "patch", "command"]
    _ -> fail "apply_patch expects freeform patch text"

applyPatchTool :: ToolEnv -> AppTool
applyPatchTool env =
    withToolResourceClaims (applyPatchResourceClaims env) $
    freeformApplyPatchAppToolWithExecution
        "apply_patch" applyPatchDescription AlwaysPrompt TurnSequential
        (typedTool "apply_patch" applyPatchArgsDecoder (runApplyPatch env))

applyPatchResourceClaims
    :: ToolEnv
    -> ToolCall
    -> IO (Either Text [ToolResourceClaim])
applyPatchResourceClaims env call =
    case decodeApplyPatchArguments call of
        Left err -> pure (Left err)
        Right args ->
            case parsePatch args.patch of
                Left err -> pure (Left err)
                Right hunks -> do
                    claims <- traverse (claimsForHunk env) hunks
                    pure (concat <$> sequence claims)

claimsForHunk
    :: ToolEnv
    -> Hunk
    -> IO (Either Text [ToolResourceClaim])
claimsForHunk env = \case
    AddFile path _ -> one path
    DeleteFile path -> one path
    UpdateFile path moveTo _ -> do
        source <- one path
        destination <- maybe
            (pure (Right []))
            one
            moveTo
        pure ((<>) <$> source <*> destination)
  where
    one path =
        resolveUnderCwd env (unsafeEncodeUtf path)
            >>= pure . fmap
                (\resolved ->
                    [ToolResourceClaim ToolWrite (ToolPath resolved)])

applyPatchDescription :: Text
applyPatchDescription =
    "The `apply_patch` tool can be used to edit files. This is a FREEFORM tool, so do not wrap the patch in JSON.\n\
    \Use the `apply_patch` tool to edit files (NEVER try applypatch or apply-patch, only apply_patch).\n\
    \Your patch language is a stripped-down, file-oriented diff format:\n\
    \*** Begin Patch\n\
    \*** Add File: path\n\
    \+contents\n\
    \*** Update File: path\n\
    \@@\n\
    \-old\n\
    \+new\n\
    \*** Delete File: path\n\
    \*** End Patch"

runApplyPatch :: ToolEnv -> ApplyPatchArgs -> IO (Either Text Text)
runApplyPatch env args = applyPatch env args.patch

decodeApplyPatchArguments :: ToolCall -> Either Text ApplyPatchArgs
decodeApplyPatchArguments call = case call.callKind of
    CustomCallKind -> Right (ApplyPatchArgs call.arguments)
    FunctionCallKind -> decodeToolArguments applyPatchArgsDecoder call.arguments

--------------------------------------------------------------------------------
-- update_plan
--------------------------------------------------------------------------------

data PlanItem = PlanItem
    { step :: Text
    , status :: Text
    } deriving (Eq, Show)

planItemDecoder :: Json.Decoder PlanItem
planItemDecoder = Json.object $
    PlanItem
        <$> Json.atKey "step" Json.text
        <*> Json.atKey "status" Json.text

data UpdatePlanArgs = UpdatePlanArgs
    { explanation :: Maybe Text
    , plan :: [PlanItem]
    }

updatePlanArgsDecoder :: Json.Decoder UpdatePlanArgs
updatePlanArgsDecoder = Json.object $
    UpdatePlanArgs
        <$> optionalText "explanation"
        <*> Json.atKey "plan" (Json.list planItemDecoder)

updatePlanTool :: PlanModeEnv -> AppTool
updatePlanTool planMode = jsonTool "update_plan" updatePlanDescription
    [ PropertySchema "explanation" PropertyString False $ Just
        "Optional explanation for this plan update."
    , PropertySchema "plan" (PropertyArray (PropertyObject
        [ PropertySchema "step" PropertyString True $ Just "Task step text."
        , PropertySchema "status" (PropertyEnum ["pending", "in_progress", "completed"]) True $
            Just "Step status."
        ])) True $ Just "The list of steps"
    ]
    True
    TurnSequential
    (typedTool "update_plan" updatePlanArgsDecoder (runUpdatePlan planMode))

updatePlanDescription :: Text
updatePlanDescription =
    "Updates the task plan.\n\
    \Provide an optional explanation and a list of plan items, each with a step and status.\n\
    \At most one step can be in_progress at a time.\n\
    \This is a progress checklist, not Plan Mode. It errors while Plan Mode is active."

runUpdatePlan :: PlanModeEnv -> UpdatePlanArgs -> IO (Either Text Text)
runUpdatePlan planMode args = do
    active <- isPlanModeActive planMode
    if active
        then pure $ Left
            "update_plan is unavailable in Plan Mode. Write the design to plan.md \
            \and present it with a <proposed_plan> block when ready."
        else pure (runUpdatePlanBody args)

runUpdatePlanBody :: UpdatePlanArgs -> Either Text Text
runUpdatePlanBody args
    | any (\item -> item.status `notElem` ["pending", "in_progress", "completed"]) args.plan =
        Left "Each plan status must be pending, in_progress, or completed."
    | length (filter (\item -> item.status == "in_progress") args.plan) > 1 =
        Left "At most one step can be in_progress at a time."
    | otherwise =
        let rendered = Text.unlines (map renderItem args.plan)
            header = case args.explanation of
                Nothing -> "Plan updated:\n"
                Just explanation -> explanation <> "\nPlan updated:\n"
        in Right (header <> rendered)
  where
    renderItem :: PlanItem -> Text
    renderItem item = "- [" <> item.status <> "] " <> item.step

optionalText :: Text -> Json.FieldsDecoder (Maybe Text)
optionalText key =
    fmap (>>= nonEmpty) $
        join <$> Json.atKeyOptional key (Json.nullable Json.text)
  where
    nonEmpty value
        | Text.null value = Nothing
        | otherwise = Just value

optionalIntOrString :: Text -> Json.FieldsDecoder (Maybe Int)
optionalIntOrString key =
    join <$> Json.atKeyOptional key (Json.nullable intOrString)

intOrString :: Json.Decoder Int
intOrString = Json.withType \case
    Json.VNumber -> Json.int
    Json.VString -> Json.withText \value ->
        case Text.signed Text.decimal (Text.strip value) of
            Right (number, rest)
                | Text.null rest -> pure number
            _ -> fail "expected integer"
    _ -> fail "expected integer"

firstPresentText :: [Text] -> Json.FieldsDecoder Text
firstPresentText keys = do
    values <- traverse (`Json.atKeyOptional` Json.text) keys
    case [value | Just value <- values] of
        value : _ -> pure value
        [] -> fail "missing patch text"
