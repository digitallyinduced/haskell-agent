-- | OpenAI Codex coding tools.
--
-- Wire names and schemas are copied from openai/codex
-- @codex-rs/core/src/tools/handlers for the Codex-native tools.
-- run_ghci is a local extension shared with Grok/OpenRouter.
-- Multi-agent v1 tools are optional and registered when a registry is supplied.
module Agent.Tools.Codex
    ( codexTools
    ) where

import Agent.OsPath (fromText)
import Agent.ToolArgs
    ( objectArgs
    , optIntOrString
    , optText
    , reqText
    )
import Agent.ToolDSL
    ( PropertySchema(..)
    , PropertyType(..)
    )
import Agent.ToolDispatch (ToolHandler, typedStreamingTool, typedTool)
import Agent.Tools.ApplyPatch (applyPatch)
import Agent.Tools.Ghci (GhciSession, runGhciTool)
import Agent.Tools.Dangerous (forbiddenRmRfReason, commandLooksLikeRmRf)
import Agent.Tools.IO
    ( CommandResult(..)
    , resolveUnderCwd
    , runShellCommandStreaming
    )
import Agent.Tools.MultiAgents (MultiAgentContext, multiAgentTools)
import Agent.Tools.PlanMode
    ( PlanModeEnv
    , askUserQuestionTool
    , enterPlanModeTool
    , isPlanModeActive
    , planFilePath
    , planModeBlockedEditMessage
    , withPlanModeLock
    )
import Agent.Tools.Types
    ( AppTool
    , ApprovalRule(..)
    , ToolExecutionPolicy(..)
    , ToolEnv(..)
    , freeformApplyPatchAppToolWithExecution
    , jsonAppToolWithExecution
    )
import Control.Applicative ((<|>))
import Data.Aeson (FromJSON(..), Value(..), withObject)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (parseFail)
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

codexTools
    :: ToolEnv
    -> GhciSession
    -> PlanModeEnv
    -> Maybe MultiAgentContext
    -> IO [AppTool]
codexTools env ghci planMode multi = do
    planRef <- newIORef []
    pure $
        [ shellCommandTool env
        , applyPatchTool env planMode
        , updatePlanTool planMode planRef
        , runGhciTool ghci
        , enterPlanModeTool planMode
        , askUserQuestionTool planMode
        ]
        ++ maybe [] multiAgentTools multi

jsonTool
    :: Text
    -> Text
    -> [PropertySchema]
    -> Bool
    -> ToolExecutionPolicy
    -> ToolHandler
    -> AppTool
jsonTool name description parameters readOnly execution =
    jsonAppToolWithExecution name description parameters
        (if readOnly then AlwaysReadOnly else AlwaysPrompt)
        execution

--------------------------------------------------------------------------------
-- shell_command
--------------------------------------------------------------------------------

data ShellCommandArgs = ShellCommandArgs
    { command :: Text
    , workdir :: Maybe Text
    , timeoutMs :: Maybe Int
    }

instance FromJSON ShellCommandArgs where
    parseJSON = objectArgs \object -> ShellCommandArgs
        <$> reqText object "command"
        <*> optText object "workdir"
        <*> optIntOrString object "timeout_ms"

shellCommandTool :: ToolEnv -> AppTool
shellCommandTool env = jsonTool "shell_command" shellDescription
    [ PropertySchema "command" PropertyString True $ Just
        "Shell script to run in the user's default shell."
    , PropertySchema "workdir" PropertyString False $ Just
        "Working directory for the command. Defaults to the turn cwd."
    , PropertySchema "timeout_ms" PropertyInteger False $ Just
        "Maximum command runtime. Defaults to 10000 ms."
    ]
    False
    TurnSequential
    (typedStreamingTool "shell_command" (runShell env))

shellDescription :: Text
shellDescription =
    "Runs a shell command and returns its output.\n\
    \- Always set the `workdir` param when using the shell_command function. Do not use `cd` unless absolutely necessary."

runShell
    :: ToolEnv
    -> (Text -> IO ())
    -> ShellCommandArgs
    -> IO (Either Text Text)
runShell env emitOutput args
    | commandLooksLikeRmRf args.command =
        pure (Left (forbiddenRmRfReason args.command))
    | otherwise = do
        let timeoutMs = min 300000 (max 1 (fromMaybe 10000 args.timeoutMs))
        workdir <- case args.workdir of
            Nothing -> pure (Right env.toolCwd)
            Just dir -> resolveUnderCwd env (fromText dir)
        case workdir of
            Left err -> pure (Left err)
            Right dir -> do
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
                    else
                        let code = fromMaybe 1 result.commandExitCode
                            body = commandBody
                                result.commandStdout
                                result.commandStderr
                        in pure $ Right $
                            "Exit code: " <> Text.pack (show code) <> "\n" <> body

commandBody :: Text -> Text -> Text
commandBody out err
    | Text.null err = out
    | Text.null out = err
    | otherwise = out <> "\nstderr:\n" <> err


--------------------------------------------------------------------------------
-- apply_patch
--------------------------------------------------------------------------------

newtype ApplyPatchArgs = ApplyPatchArgs { patch :: Text }

instance FromJSON ApplyPatchArgs where
    parseJSON (String text) = pure (ApplyPatchArgs text)
    parseJSON (Object object) =
        ApplyPatchArgs <$> (reqText object "input" <|> reqText object "patch" <|> reqText object "command")
    parseJSON _ = parseFail "apply_patch expects freeform patch text"

applyPatchTool :: ToolEnv -> PlanModeEnv -> AppTool
applyPatchTool env planMode =
    freeformApplyPatchAppToolWithExecution
        "apply_patch" applyPatchDescription AlwaysPrompt TurnSequential
        (typedTool "apply_patch" (runApplyPatch env planMode))

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

runApplyPatch
    :: ToolEnv
    -> PlanModeEnv
    -> ApplyPatchArgs
    -> IO (Either Text Text)
runApplyPatch env planMode args = withPlanModeLock planMode do
    active <- isPlanModeActive planMode
    if active
        then Left . planModeBlockedEditMessage <$> planFilePath planMode
        else applyPatch env args.patch

--------------------------------------------------------------------------------
-- update_plan
--------------------------------------------------------------------------------

data PlanItem = PlanItem
    { step :: Text
    , status :: Text
    } deriving (Eq, Show)

instance FromJSON PlanItem where
    parseJSON = withObject "plan item" \object -> PlanItem
        <$> reqText object "step"
        <*> reqText object "status"

data UpdatePlanArgs = UpdatePlanArgs
    { explanation :: Maybe Text
    , plan :: [PlanItem]
    }

instance FromJSON UpdatePlanArgs where
    parseJSON = withObject "update_plan" \object -> do
        explanation <- optText object "explanation"
        plan <- case KeyMap.lookup "plan" object of
            Nothing -> parseFail "Missing parameter: plan"
            Just value -> parseJSON value
        pure UpdatePlanArgs { explanation, plan }

updatePlanTool :: PlanModeEnv -> IORef [PlanItem] -> AppTool
updatePlanTool planMode planRef = jsonTool "update_plan" updatePlanDescription
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
    (typedTool "update_plan" (runUpdatePlan planMode planRef))

updatePlanDescription :: Text
updatePlanDescription =
    "Updates the task plan.\n\
    \Provide an optional explanation and a list of plan items, each with a step and status.\n\
    \At most one step can be in_progress at a time.\n\
    \This is a progress checklist, not Plan Mode. It errors while Plan Mode is active."

runUpdatePlan :: PlanModeEnv -> IORef [PlanItem] -> UpdatePlanArgs -> IO (Either Text Text)
runUpdatePlan planMode planRef args = withPlanModeLock planMode do
    active <- isPlanModeActive planMode
    if active
        then pure $ Left
            "update_plan is unavailable in Plan Mode. Write the design to plan.md \
            \and present it with a <proposed_plan> block when ready."
        else runUpdatePlanBody planRef args

runUpdatePlanBody :: IORef [PlanItem] -> UpdatePlanArgs -> IO (Either Text Text)
runUpdatePlanBody planRef args
    | any (\item -> item.status `notElem` ["pending", "in_progress", "completed"]) args.plan =
        pure (Left "Each plan status must be pending, in_progress, or completed.")
    | length (filter (\item -> item.status == "in_progress") args.plan) > 1 =
        pure (Left "At most one step can be in_progress at a time.")
    | otherwise = do
        writeIORef planRef args.plan
        let rendered = Text.unlines (map renderItem args.plan)
            header = case args.explanation of
                Nothing -> "Plan updated:\n"
                Just explanation -> explanation <> "\nPlan updated:\n"
        pure $ Right (header <> rendered)
  where
    renderItem :: PlanItem -> Text
    renderItem item = "- [" <> item.status <> "] " <> item.step
