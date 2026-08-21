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
    , optInt
    , optText
    , reqText
    )
import Agent.ToolDSL
    ( PropertySchema(..)
    , PropertyType(..)
    )
import Agent.ToolDispatch (ToolHandler, typedTool)
import Agent.Tools.ApplyPatch (applyPatch)
import Agent.Tools.Ghci (GhciSession, runGhciTool)
import Agent.Tools.Dangerous (forbiddenRmRfReason, commandLooksLikeRmRf)
import Agent.Tools.IO (CommandResult(..), resolveUnderCwd, runShellCommand)
import Agent.Tools.MultiAgents (MultiAgentContext, multiAgentTools)
import Agent.Tools.PlanMode
    ( PlanModeEnv
    , isPlanModeActive
    )
import Agent.Tools.Types
    ( AppTool(..)
    , AppToolKind(..)
    , ToolEnv(..)
    )
import Control.Applicative ((<|>))
import Data.Aeson (FromJSON(..), Object, Value(..), withObject)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (Parser, parseFail)
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
        , applyPatchTool env
        , updatePlanTool planMode planRef
        , runGhciTool ghci
        ]
        ++ maybe [] multiAgentTools multi

jsonTool
    :: Text
    -> Text
    -> [PropertySchema]
    -> Bool
    -> ToolHandler
    -> AppTool
jsonTool name description parameters readOnly handler = AppTool
    { appToolName = name
    , appToolDescription = description
    , appToolParameters = parameters
    , appToolHandler = handler
    , appToolKind = JsonFunction
    , appToolReadOnly = readOnly
    , appToolIsReadOnlyCall = Nothing
    }

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
        <*> (optInt object "timeout_ms" <|> optionalIntString object "timeout_ms")

optionalIntString :: Object -> Text -> Parser (Maybe Int)
optionalIntString object key = do
    value <- optText object key
    pure (value >>= readInt)

readInt :: Text -> Maybe Int
readInt text = case reads (Text.unpack text) of
    [(n, "")] -> Just n
    _ -> Nothing

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
    (typedTool "shell_command" (runShell env))

shellDescription :: Text
shellDescription =
    "Runs a shell command and returns its output.\n\
    \- Always set the `workdir` param when using the shell_command function. Do not use `cd` unless absolutely necessary."

runShell :: ToolEnv -> ShellCommandArgs -> IO (Either Text Text)
runShell env args
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
                result <- runShellCommand env dir (Text.unpack args.command) timeoutMs
                if result.commandCancelled
                    then pure $ Left "Error: Command cancelled"
                    else if result.commandTimedOut
                    then pure $ Left $
                        "Error: Command timed out after " <> Text.pack (show timeoutMs) <> "ms"
                    else
                        let code = fromMaybe 1 result.commandExitCode
                            body
                                | Text.null result.commandStderr = result.commandStdout
                                | Text.null result.commandStdout = result.commandStderr
                                | otherwise =
                                    result.commandStdout <> "\nstderr:\n" <> result.commandStderr
                        in pure $ Right $
                            "Exit code: " <> Text.pack (show code) <> "\n" <> body


--------------------------------------------------------------------------------
-- apply_patch
--------------------------------------------------------------------------------

newtype ApplyPatchArgs = ApplyPatchArgs { patch :: Text }

instance FromJSON ApplyPatchArgs where
    parseJSON (String text) = pure (ApplyPatchArgs text)
    parseJSON (Object object) =
        ApplyPatchArgs <$> (reqText object "input" <|> reqText object "patch" <|> reqText object "command")
    parseJSON _ = parseFail "apply_patch expects freeform patch text"

applyPatchTool :: ToolEnv -> AppTool
applyPatchTool env = AppTool
    { appToolName = "apply_patch"
    , appToolDescription = applyPatchDescription
    , appToolParameters = []
    , appToolHandler = typedTool "apply_patch" (runApplyPatch env)
    , appToolKind = FreeformApplyPatch
    , appToolReadOnly = False
    , appToolIsReadOnlyCall = Nothing
    }

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
    (typedTool "update_plan" (runUpdatePlan planMode planRef))

updatePlanDescription :: Text
updatePlanDescription =
    "Updates the task plan.\n\
    \Provide an optional explanation and a list of plan items, each with a step and status.\n\
    \At most one step can be in_progress at a time.\n\
    \This is a progress checklist, not Plan Mode. It errors while Plan Mode is active."

runUpdatePlan :: PlanModeEnv -> IORef [PlanItem] -> UpdatePlanArgs -> IO (Either Text Text)
runUpdatePlan planMode planRef args = do
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
