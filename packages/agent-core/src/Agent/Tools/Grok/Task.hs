-- | Grok-build @task@ tool — spawn nestable subagents.
--
-- Wire names and defaults follow xai-org/grok-build TaskToolInput /
-- build_task_description. Runtime reuses 'Agent.Subagents'.
module Agent.Tools.Grok.Task
    ( taskTool
    , filterGrokToolsForType
    , knownSubagentTypes
    , defaultSubagentType
    , isSubagentIdText
    , formatTaskStarted
    , formatTaskCompleted
    , recordAgentType
    , lookupAgentType
    ) where

import Agent.Subagents
    ( SubagentId(..)
    , SubagentStatus(..)
    , defaultWaitTimeoutMs
    , getStatus
    , sendInput
    , spawnSubagent
    , waitSubagents
    )
import Agent.ToolArgs
    ( objectArgs
    , optBool
    , optText
    , reqText
    )
import Agent.ToolDSL
    ( PropertySchema(..)
    , PropertyType(..)
    )
import Agent.ToolDispatch (ToolHandler, typedTool)
import Agent.Tools.MultiAgents (MultiAgentContext(..))
import Agent.Tools.Types
    ( AppTool(..)
    , AppToolKind(..)
    )
import Data.Aeson (FromJSON(..))
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

defaultSubagentType :: Text
defaultSubagentType = "general-purpose"

knownSubagentTypes :: [Text]
knownSubagentTypes = ["general-purpose", "explore", "plan"]

-- | True when the id looks like a registry subagent id (not a shell @tN@ id).
isSubagentIdText :: Text -> Bool
isSubagentIdText text = "agent-" `Text.isPrefixOf` text

data TaskArgs = TaskArgs
    { prompt :: Text
    , description :: Text
    , subagentType :: Text
    , runInBackground :: Bool
    , resumeFrom :: Maybe Text
    , cwd :: Maybe Text
    , model :: Maybe Text
    , isolation :: Maybe Text
    }

instance FromJSON TaskArgs where
    parseJSON = objectArgs \object -> do
        prompt <- reqText object "prompt"
        description <- reqText object "description"
        subagentType <- fromMaybe defaultSubagentType <$> optText object "subagent_type"
        runInBackground <- fromMaybe True <$> optBool object "run_in_background"
        resumeFrom <- optText object "resume_from"
        cwd <- optText object "cwd"
        model <- optText object "model"
        isolation <- optText object "isolation"
        pure TaskArgs
            { prompt
            , description
            , subagentType
            , runInBackground
            , resumeFrom
            , cwd
            , model
            , isolation
            }

taskTool :: MultiAgentContext -> IORef (Map SubagentId Text) -> AppTool
taskTool ctx typesRef = AppTool
    { appToolName = "task"
    , appToolDescription = taskDescription
    , appToolParameters =
        [ PropertySchema "prompt" PropertyString True $ Just
            "The full task prompt for the subagent to execute."
        , PropertySchema "description" PropertyString True $ Just
            "Short description of the task (3-5 words)."
        , PropertySchema "subagent_type" PropertyString False $ Just
            "Name of the subagent type to launch. Built-in types: \"general-purpose\", \"explore\", \"plan\"."
        , PropertySchema "run_in_background" PropertyBoolean False $ Just
            "Returns immediately with a subagent_id. Use get_task_output to retrieve results. Defaults to true."
        , PropertySchema "resume_from" PropertyString False $ Just
            "Resume from a previously completed subagent's conversation. Pass the subagent_id from a prior task call."
        , PropertySchema "cwd" PropertyString False $ Just
            "Explicit working directory for the subagent. Mutually exclusive with isolation=\"worktree\"."
        , PropertySchema "model" PropertyString False $ Just
            "Optional model slug. Omit to inherit the parent model."
        , PropertySchema "isolation" PropertyString False $ Just
            "Isolation mode: \"none\" (default) or \"worktree\". Worktree isolation is not supported yet."
        ]
    , appToolHandler = typedTool "task" (runTask ctx typesRef)
    , appToolKind = JsonFunction
    , appToolReadOnly = False
    , appToolIsReadOnlyCall = Nothing
    }

taskDescription :: Text
taskDescription =
    "Start a subagent that works on a task independently and reports back.\n\n\
    \Agent types:\n\n\
    \- **general-purpose**: General purpose agent for multi-step tasks. Has access to all tools including task.\n\
    \- **explore**: Fast, read-only agent specialized for codebase exploration. Read-only — has access to: read_file, list_dir, grep.\n\
    \- **plan**: Software architect for planning implementation strategies. Read-only — has access to: read_file, list_dir, grep, enter_plan_mode, exit_plan_mode, ask_user_question.\n\n\
    \## Usage notes\n\
    \- When the agent is done, it returns a single message with its agent ID. Use that ID to resume the agent later for follow-up work.\n\
    \- run_in_background: Returns immediately with a subagent_id. Use get_task_output to retrieve results. This is set to true by default.\n\
    \- When using the task tool, you must specify a subagent_type parameter to select which agent type to use.\n\
    \- When launching independent subagents, you MUST incorporate the results into the task based on requirements BEFORE concluding.\n\n\
    \Resuming a previous agent (resume_from):\n\
    \- Use resume_from to continue a previously completed subagent's conversation. Pass the subagent_id returned by a prior task call.\n\
    \- The resumed agent must use the same subagent_type as the source.\n\n\
    \Isolation mode:\n\
    \- isolation=\"worktree\" is not supported yet; use the default shared workspace."

runTask
    :: MultiAgentContext
    -> IORef (Map SubagentId Text)
    -> TaskArgs
    -> IO (Either Text Text)
runTask ctx typesRef args
    | Text.null (Text.strip args.prompt) =
        pure (Left "task requires a non-empty prompt")
    | Text.null (Text.strip args.description) =
        pure (Left "task requires a non-empty description")
    | args.subagentType `notElem` knownSubagentTypes =
        pure $ Left $
            "Unknown subagent_type: "
                <> args.subagentType
                <> ". Known types: "
                <> Text.intercalate ", " knownSubagentTypes
    | Just iso <- args.isolation
    , Text.toLower (Text.strip iso) `elem` ["worktree", "work_tree", "work-tree"] =
        pure (Left "isolation=\"worktree\" is not supported yet. Omit isolation or use \"none\".")
    | Just _ <- args.cwd
    , Just iso <- args.isolation
    , Text.toLower (Text.strip iso) `notElem` ["none", ""] =
        pure (Left "cwd and isolation are mutually exclusive.")
    | Just _ <- args.cwd =
        -- Explicit cwd overrides are accepted later; for v1 reject to avoid
        -- surprising ToolEnv mismatches until child cwd wiring exists.
        pure (Left "task cwd overrides are not supported yet. Omit cwd to use the parent working directory.")
    | Just resumeId <- args.resumeFrom = resumeTask ctx typesRef args resumeId
    | otherwise = spawnFresh ctx typesRef args

spawnFresh
    :: MultiAgentContext
    -> IORef (Map SubagentId Text)
    -> TaskArgs
    -> IO (Either Text Text)
spawnFresh ctx typesRef args = do
    -- model override accepted but unused until child runners plumb it.
    _ <- pure args.model
    result <- spawnSubagent
        ctx.multiRegistry
        ctx.multiSelfId
        ctx.multiDepth
        args.prompt
        (Just args.description)
    case result of
        Left err -> pure (Left err)
        Right agentId -> do
            recordAgentType typesRef agentId args.subagentType
            if args.runInBackground
                then pure $ Right $ formatTaskStarted agentId args
                else do
                    (statuses, timedOut) <-
                        waitSubagents ctx.multiRegistry [agentId] defaultWaitTimeoutMs
                    pure $ Right $ formatTaskCompleted agentId args timedOut
                        (Map.lookup agentId statuses)

resumeTask
    :: MultiAgentContext
    -> IORef (Map SubagentId Text)
    -> TaskArgs
    -> Text
    -> IO (Either Text Text)
resumeTask ctx typesRef args resumeId = do
    let agentId = SubagentId resumeId
    status <- getStatus ctx.multiRegistry agentId
    case status of
        NotFound -> pure (Left ("unknown subagent id: " <> resumeId))
        Closed -> pure (Left "cannot resume a closed subagent; it must still be open (completed).")
        Running -> pure (Left "cannot resume_from a running subagent; wait for it to finish first.")
        Pending -> pure (Left "cannot resume_from a pending subagent; wait for it to finish first.")
        _ -> do
            mtype <- lookupAgentType typesRef agentId
            case mtype of
                Just prior
                    | prior /= args.subagentType ->
                        pure $ Left $
                            "resume_from subagent_type mismatch: source is "
                                <> prior
                                <> ", requested "
                                <> args.subagentType
                _ -> do
                    recordAgentType typesRef agentId args.subagentType
                    sent <- sendInput ctx.multiRegistry agentId args.prompt False
                    case sent of
                        Left err -> pure (Left err)
                        Right _ ->
                            if args.runInBackground
                                then pure $ Right $ formatTaskStarted agentId args
                                else do
                                    (statuses, timedOut) <-
                                        waitSubagents ctx.multiRegistry [agentId] defaultWaitTimeoutMs
                                    pure $ Right $ formatTaskCompleted agentId args timedOut
                                        (Map.lookup agentId statuses)

formatTaskStarted :: SubagentId -> TaskArgs -> Text
formatTaskStarted agentId args =
    "Subagent started in background.\n\
    \subagent_id: "
        <> agentId.unSubagentId
        <> "\n\
        \type: "
        <> args.subagentType
        <> "\n\
        \description: "
        <> args.description
        <> "\n\
        \Use get_task_output with this subagent_id to retrieve results. Do not poll in a loop."

formatTaskCompleted
    :: SubagentId
    -> TaskArgs
    -> Bool
    -> Maybe SubagentStatus
    -> Text
formatTaskCompleted agentId args timedOut mstatus =
    let header =
            "subagent_id: "
                <> agentId.unSubagentId
                <> "\ntype: "
                <> args.subagentType
                <> "\ndescription: "
                <> args.description
                <> "\n"
        body = case mstatus of
            Just (Completed (Just text)) -> "status: completed\nfinal:\n" <> text
            Just (Completed Nothing) -> "status: completed"
            Just (Errored err) -> "status: errored\nerror: " <> err
            Just Interrupted -> "status: interrupted"
            Just Closed -> "status: shutdown"
            Just Running
                | timedOut -> "status: running (wait timed out)"
                | otherwise -> "status: running"
            Just Pending -> "status: pending"
            Just NotFound -> "status: not_found"
            Nothing -> "status: unknown"
    in header <> body

recordAgentType :: IORef (Map SubagentId Text) -> SubagentId -> Text -> IO ()
recordAgentType typesRef agentId agentType =
    atomicModifyIORef' typesRef \m -> (Map.insert agentId agentType m, ())

lookupAgentType :: IORef (Map SubagentId Text) -> SubagentId -> IO (Maybe Text)
lookupAgentType typesRef agentId = Map.lookup agentId <$> readIORef typesRef

-- | Restrict the child tool surface by subagent type.
filterGrokToolsForType :: Text -> [AppTool] -> [AppTool]
filterGrokToolsForType agentType tools = case agentType of
    "explore" -> filter ((`elem` exploreNames) . (.appToolName)) tools
    "plan" -> filter ((`elem` planNames) . (.appToolName)) tools
    _ -> tools
  where
    exploreNames =
        [ "read_file"
        , "list_dir"
        , "grep"
        , "run_terminal_cmd"
        , "run_ghci"
        , "get_task_output"
        , "kill_task"
        ]
    planNames =
        [ "read_file"
        , "list_dir"
        , "grep"
        , "run_terminal_cmd"
        , "run_ghci"
        , "get_task_output"
        , "kill_task"
        , "enter_plan_mode"
        , "exit_plan_mode"
        , "ask_user_question"
        ]
