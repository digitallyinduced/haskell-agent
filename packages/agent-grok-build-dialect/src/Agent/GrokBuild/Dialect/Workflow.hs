-- | Bounded named workflow support.
--
-- This core-parity slice intentionally supports only the registered
-- @deep-research@ workflow. It launches one tracked background subagent;
-- arbitrary Rhai, script paths, resumption, and budget semantics are rejected
-- rather than falsely advertised as implemented.
module Agent.GrokBuild.Dialect.Workflow
    ( WorkflowRuntime
    , WorkflowRunSnapshot(..)
    , newWorkflowRuntime
    , workflowTool
    , workflowRunSnapshots
    , formatWorkflowRuns
    ) where

import Agent.GrokBuild.Dialect.Common (jsonTool)
import Agent.GrokBuild.Dialect.Task
    ( GrokSubagentSpec(..)
    , GrokSubagentSpecs
    , runtimeSubagentType
    , spawnManagedGrokSubagent
    )
import Agent.Subagents
    ( SubagentId
    , SubagentStatus(..)
    , getStatus
    )
import Agent.ToolDSL (PropertySchema(..), PropertyType(..))
import Agent.ToolDispatch (typedTool)
import Agent.Tools.MultiAgents (MultiAgentContext(..))
import Agent.Tools.Types (AppTool, ToolExecutionPolicy(..))
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar
    , newMVar
    , readMVar
    )
import Data.Aeson
    ( FromJSON(..)
    , Value(..)
    , object
    , withObject
    , (.:?)
    , (.=)
    )
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Text as Aeson
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.OsPath (OsPath)

data WorkflowRun = WorkflowRun
    { workflowInternalRunId :: !Text
    , workflowDisplayName :: !Text
    , workflowObjective :: !Text
    , workflowAgentId :: !SubagentId
    , workflowStartedAt :: !UTCTime
    }

data WorkflowStore = WorkflowStore
    { workflowNextRun :: !Int
    , workflowNameCounts :: !(Map Text Int)
    , workflowRuns :: !(Map Text WorkflowRun)
    }

data WorkflowRuntime = WorkflowRuntime
    { workflowCwd :: !OsPath
    , workflowMulti :: !MultiAgentContext
    , workflowSpecs :: !GrokSubagentSpecs
    , workflowStore :: !(MVar WorkflowStore)
    }

data WorkflowRunSnapshot = WorkflowRunSnapshot
    { workflowRunId :: !Text
    , workflowRunName :: !Text
    , workflowRunObjective :: !Text
    , workflowRunStatus :: !Text
    , workflowRunAgentId :: !SubagentId
    , workflowRunStartedAt :: !UTCTime
    } deriving (Eq, Show)

newWorkflowRuntime
    :: OsPath
    -> MultiAgentContext
    -> GrokSubagentSpecs
    -> IO WorkflowRuntime
newWorkflowRuntime cwd multi specs = do
    store <- newMVar WorkflowStore
        { workflowNextRun = 0
        , workflowNameCounts = Map.empty
        , workflowRuns = Map.empty
        }
    pure WorkflowRuntime
        { workflowCwd = cwd
        , workflowMulti = multi
        , workflowSpecs = specs
        , workflowStore = store
        }

data WorkflowArgs = WorkflowArgs
    { agentBudget :: !(Maybe Int)
    , name :: !(Maybe Text)
    , script :: !(Maybe Text)
    , scriptPath :: !(Maybe Text)
    , workflowInputArgs :: !(Maybe Value)
    , resumeFromRunId :: !(Maybe Text)
    , validateOnly :: !Bool
    }

instance FromJSON WorkflowArgs where
    parseJSON = withObject "workflow" \object_ -> do
        agentBudget <- object_ .:? "agent_budget"
        name <- object_ .:? "name"
        script <- object_ .:? "script"
        scriptPath <- object_ .:? "script_path"
        workflowInputArgs <- object_ .:? "args"
        resumeFromRunId <- object_ .:? "resume_from_run_id"
        validateOnly <- maybe False id <$> object_ .:? "validate_only"
        pure WorkflowArgs
            { agentBudget
            , name
            , script
            , scriptPath
            , workflowInputArgs
            , resumeFromRunId
            , validateOnly
            }

workflowTool :: WorkflowRuntime -> AppTool
workflowTool runtime =
    jsonTool
        "workflow"
        workflowDescription
        [ PropertySchema "agent_budget" PropertyInteger False $ Just
            "Absolute cumulative cap on logical child-agent calls. This host's bounded named workflow does not support custom budgets."
        , PropertySchema "name" PropertyString False $ Just
            "Name of a registered workflow. This host currently provides deep-research."
        , PropertySchema "script" PropertyString False $ Just
            "Inline Rhai workflow script. Not supported by this host."
        , PropertySchema "script_path" PropertyString False $ Just
            "Path to a .rhai workflow script. Not supported by this host."
        , PropertySchema "args" (PropertyRaw (object [])) False $ Just
            "JSON value passed to the named workflow. deep-research accepts a string or an object with query or objective."
        , PropertySchema "resume_from_run_id" PropertyString False $ Just
            "Resume a same-process paused workflow run. Not supported by this host."
        , PropertySchema "validate_only" PropertyBoolean False $ Just
            "Validate the selected registered workflow and arguments without launching."
        ]
        False
        TurnSequential
        (typedTool "workflow" (runWorkflow runtime))

workflowDescription :: Text
workflowDescription =
    "Launch a registered workflow as one tracked background run. The call returns immediately and completion is reported by the subagent runtime.\n\n\
    \This host currently supports the bounded deep-research workflow. Arbitrary Rhai scripts, script paths, workflow resumption, and custom agent budgets are not implemented and return explicit errors."

runWorkflow
    :: WorkflowRuntime
    -> WorkflowArgs
    -> IO (Either Text Text)
runWorkflow runtime rawArgs =
    case validateWorkflowInput (normalizeWorkflowArgs rawArgs) of
        Left err -> pure (Left err)
        Right (workflowName, objective)
            | rawArgs.validateOnly ->
                pure $ Right $ jsonText $ object
                    [ "run_id" .= ("" :: Text)
                    , "task_id" .= ("" :: Text)
                    , "name" .= workflowName
                    , "message" .=
                        ("Smoke check passed for workflow '"
                            <> workflowName
                            <> "'. This did not launch the workflow or exercise live dependencies.")
                    ]
            | otherwise -> launchWorkflow runtime workflowName objective

normalizeWorkflowArgs :: WorkflowArgs -> WorkflowArgs
normalizeWorkflowArgs args = args
    { name = nonBlank args.name
    , script = nonBlank args.script
    , scriptPath = nonBlank args.scriptPath
    , resumeFromRunId = nonBlank args.resumeFromRunId
    }

validateWorkflowInput :: WorkflowArgs -> Either Text (Text, Text)
validateWorkflowInput args
    | Just _ <- args.resumeFromRunId =
        Left
            "workflow_resume_unsupported: resume_from_run_id is not supported by this host."
    | sourceCount == 0 =
        Left "workflow_invalid_input: provide one of name, script, or script_path"
    | sourceCount > 1 =
        Left
            "workflow_invalid_input: name, script, and script_path are mutually exclusive"
    | Just _ <- args.script =
        Left
            "workflow_inline_unsupported: inline Rhai workflow scripts are not supported by this host."
    | Just _ <- args.scriptPath =
        Left
            "workflow_script_path_unsupported: workflow script paths are not supported by this host."
    | Just budget <- args.agentBudget =
        Left
            ("workflow_budget_unsupported: agent_budget="
                <> Text.pack (show budget)
                <> " is not supported by this bounded workflow implementation.")
    | Just suppliedName <- args.name
    , Text.toLower suppliedName /= "deep-research" =
        Left
            ("workflow_not_found: Unknown registered workflow: "
                <> suppliedName
                <> ". Available workflows: deep-research")
    | otherwise = do
        objective <- extractDeepResearchObjective args.workflowInputArgs
        pure ("deep-research", objective)
  where
    sourceCount =
        length (catMaybes [args.name, args.script, args.scriptPath])

extractDeepResearchObjective :: Maybe Value -> Either Text Text
extractDeepResearchObjective = \case
    Nothing ->
        Left
            "workflow_invalid_args: deep-research requires a non-empty query."
    Just (String query) ->
        requireQuery query
    Just (Object object_) ->
        case firstText ["query", "objective"] object_ of
            Nothing ->
                Left
                    "workflow_invalid_args: deep-research args must include query or objective."
            Just query -> requireQuery query
    Just _ ->
        Left
            "workflow_invalid_args: deep-research args must be a string or an object with query or objective."
  where
    requireQuery query =
        let stripped = Text.strip query
        in if Text.null stripped
            then
                Left
                    "workflow_invalid_args: deep-research requires a non-empty query."
            else Right stripped

firstText :: [Text] -> KeyMap.KeyMap Value -> Maybe Text
firstText keys object_ =
    foldr choose Nothing keys
  where
    choose key rest =
        case KeyMap.lookup (Key.fromText key) object_ of
            Just (String value) -> Just value
            _ -> rest

launchWorkflow
    :: WorkflowRuntime
    -> Text
    -> Text
    -> IO (Either Text Text)
launchWorkflow runtime workflowName objective = do
    spawned <-
        spawnManagedGrokSubagent
            runtime.workflowCwd
            runtime.workflowMulti
            runtime.workflowSpecs
            GrokSubagentSpec
                { agentType = runtimeSubagentType
                , modelOverride = Nothing
                , reasoningEffortOverride = Nothing
                }
            (deepResearchPrompt objective)
            (Just workflowName)
    case spawned of
        Left err ->
            pure (Left ("workflow_launch_failed: " <> err))
        Right agentId -> do
            now <- getCurrentTime
            run <-
                modifyMVar runtime.workflowStore \store -> do
                    let next = store.workflowNextRun + 1
                        count =
                            Map.findWithDefault
                                0
                                workflowName
                                store.workflowNameCounts
                                + 1
                        runId = "wf_" <> Text.pack (show next)
                        displayName =
                            if count == 1
                                then workflowName
                                else
                                    workflowName
                                        <> "-"
                                        <> Text.pack (show count)
                        created = WorkflowRun
                            { workflowInternalRunId = runId
                            , workflowDisplayName = displayName
                            , workflowObjective = objective
                            , workflowAgentId = agentId
                            , workflowStartedAt = now
                            }
                        updated = store
                            { workflowNextRun = next
                            , workflowNameCounts =
                                Map.insert
                                    workflowName
                                    count
                                    store.workflowNameCounts
                            , workflowRuns =
                                Map.insert runId created store.workflowRuns
                            }
                    pure (updated, created)
            pure $ Right $ jsonText $ object
                [ "run_id" .= run.workflowInternalRunId
                , "task_id" .= run.workflowInternalRunId
                , "name" .= run.workflowDisplayName
                , "message" .=
                    ("Workflow '"
                        <> run.workflowDisplayName
                        <> "' started in the background. Completion is reported automatically. The display name is user-facing; keep the structured run id internal.")
                ]

workflowRunSnapshots
    :: WorkflowRuntime
    -> IO [WorkflowRunSnapshot]
workflowRunSnapshots runtime = do
    store <- readMVar runtime.workflowStore
    mapM snapshotRun $
        sortOn
            (\run ->
                ( run.workflowStartedAt
                , run.workflowInternalRunId
                ))
            (Map.elems store.workflowRuns)
  where
    snapshotRun :: WorkflowRun -> IO WorkflowRunSnapshot
    snapshotRun run = do
        status <-
            getStatus
                runtime.workflowMulti.multiRegistry
                run.workflowAgentId
        pure WorkflowRunSnapshot
            { workflowRunId = run.workflowInternalRunId
            , workflowRunName = run.workflowDisplayName
            , workflowRunObjective = run.workflowObjective
            , workflowRunStatus = workflowStatusName status
            , workflowRunAgentId = run.workflowAgentId
            , workflowRunStartedAt = run.workflowStartedAt
            }

formatWorkflowRuns :: [WorkflowRunSnapshot] -> Text
formatWorkflowRuns [] =
    "No workflow runs in this session yet."
formatWorkflowRuns runs =
    Text.intercalate "\n"
        [ run.workflowRunName
            <> " ("
            <> run.workflowRunId
            <> "): "
            <> run.workflowRunStatus
            <> "\n  objective: "
            <> run.workflowRunObjective
            <> "\n  started_at: "
            <> formatTimestamp run.workflowRunStartedAt
        | run <- runs
        ]

workflowStatusName :: SubagentStatus -> Text
workflowStatusName = \case
    Pending -> "pending"
    Running -> "active"
    Completed{} -> "complete"
    Errored{} -> "failed"
    Interrupted -> "interrupted"
    Closed -> "closed"
    NotFound -> "unknown"

deepResearchPrompt :: Text -> Text
deepResearchPrompt objective =
    Text.unlines
        [ "# Deep research workflow"
        , ""
        , "Research the following query with bounded effort:"
        , objective
        , ""
        , "1. Break it into a small set of independent research questions."
        , "2. Gather evidence from authoritative primary sources where possible."
        , "3. Cross-check every material claim against the cited source."
        , "4. Separate verified findings, uncertainty, and unresolved gaps."
        , "5. Return a concise cited report with precise source locators."
        , ""
        , "Do not delegate another workflow and do not broaden the requested scope."
        ]

formatTimestamp :: UTCTime -> Text
formatTimestamp =
    Text.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

nonBlank :: Maybe Text -> Maybe Text
nonBlank = (>>= keep)
  where
    keep value =
        let stripped = Text.strip value
        in if Text.null stripped then Nothing else Just stripped

jsonText :: Value -> Text
jsonText = LazyText.toStrict . Aeson.encodeToLazyText
