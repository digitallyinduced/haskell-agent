-- | Grok-build @task@ tool — spawn one level of subagents.
--
-- Wire names and defaults follow xai-org/grok-build TaskToolInput /
-- build_task_description. Runtime reuses 'Agent.Subagents'.
module Agent.Tools.Grok.Task
    ( taskTool
    , filterGrokToolsForType
    , knownSubagentTypes
    , defaultSubagentType
    , GrokSubagentSpec(..)
    , GrokSubagentSpecs
    , isSubagentIdText
    , formatTaskStarted
    , formatTaskCompleted
    , recordAgentSpec
    , recordAgentType
    , lookupAgentType
    , lookupAgentModel
    ) where

import Agent.InterAgentMessage (plainInterAgentContent)
import Agent.OsPath (OsPath, fromText, toText)
import Agent.Subagents
    ( SubagentId(..)
    , SubagentStatus(..)
    , defaultWaitTimeoutMs
    , getStatus
    , sendInputMessageForTurn
    , spawnSubagentWithCwdPreparedForTurn
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
import Agent.ToolDispatch (typedTool)
import Agent.Tools.MultiAgents (MultiAgentContext(..), SubagentWorktree(..))
import Agent.Tools.Types
    ( AppTool(..)
    , ApprovalRule(..)
    , jsonAppTool
    )
import Control.Exception.Safe (mask, onException)
import Data.Aeson (FromJSON(..))
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory.OsPath (doesDirectoryExist)
import System.OsPath (isAbsolute, normalise, (</>))

defaultSubagentType :: Text
defaultSubagentType = "general-purpose"

knownSubagentTypes :: [Text]
knownSubagentTypes = ["general-purpose", "explore", "plan"]

data GrokSubagentSpec = GrokSubagentSpec
    { agentType :: !Text
    , modelOverride :: !(Maybe Text)
    } deriving (Eq, Show)

type GrokSubagentSpecs = IORef (Map SubagentId GrokSubagentSpec)

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
        resumeFrom <- sanitizeOptional <$> optText object "resume_from"
        cwd <- sanitizeOptional <$> optText object "cwd"
        model <- sanitizeOptional <$> optText object "model"
        isolation <- sanitizeOptional <$> optText object "isolation"
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

sanitizeOptional :: Maybe Text -> Maybe Text
sanitizeOptional value = value >>= \raw ->
    let stripped = Text.strip raw
        lowered = Text.toLower stripped
    in if Text.null stripped || lowered `elem` ["null", "none", "undefined"]
        then Nothing
        else Just stripped

taskTool :: OsPath -> MultiAgentContext -> GrokSubagentSpecs -> AppTool
taskTool baseCwd ctx specsRef =
    jsonAppTool "task" taskDescription
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
            "Isolation mode: \"none\" (default) or \"worktree\". Worktree mode prevents child edits from affecting the parent workspace until explicitly applied."
        ]
        AlwaysPrompt
        (typedTool "task" (runTask baseCwd ctx specsRef))

taskDescription :: Text
taskDescription =
    "Start a subagent that works on a task independently and reports back.\n\n\
    \Agent types:\n\n\
    \- **general-purpose**: General purpose agent for multi-step tasks. Has access to all parent tools except task.\n\
    \- **explore**: Fast, read-only agent specialized for codebase exploration. Read-only — has access to: read_file, list_dir, grep.\n\
    \- **plan**: Software architect for planning implementation strategies. Read-only — has access to: read_file, list_dir, grep, web_search, enter_plan_mode, exit_plan_mode, ask_user_question.\n\n\
    \## Usage notes\n\
    \- When the agent is done, it returns a single message with its agent ID. Use that ID to resume the agent later for follow-up work.\n\
    \- run_in_background: Returns immediately with a subagent_id. Use get_task_output to retrieve results. This is set to true by default.\n\
    \- Subagents receive project instructions from AGENTS.md. Include any especially important task-specific conventions directly in the prompt.\n\
    \- When using the task tool, you must specify a subagent_type parameter to select which agent type to use.\n\
    \- When launching independent subagents, you MUST incorporate the results into the task based on requirements BEFORE concluding.\n\n\
    \Resuming a previous agent (resume_from):\n\
    \- Use resume_from to continue a previously completed subagent's conversation. Pass the subagent_id returned by a prior task call. A resumed agent keeps its transcript, so only describe what changed since the last run.\n\
    \- The resumed agent must use the same subagent_type as the source.\n\n\
    \Isolation mode:\n\
    \- Use isolation=\"worktree\" to run the child in an isolated git worktree. The worktree is preserved after completion and its path is returned in the output."

runTask
    :: OsPath
    -> MultiAgentContext
    -> GrokSubagentSpecs
    -> TaskArgs
    -> IO (Either Text Text)
runTask baseCwd ctx typesRef args
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
    | Just resumeId <- args.resumeFrom = resumeTask ctx typesRef args resumeId
    | Just _ <- args.cwd
    , Just iso <- args.isolation
    , isWorktreeIsolation iso =
        pure (Left "cwd and isolation are mutually exclusive.")
    | otherwise = mask \restore ->
        resolveTaskWorkspace baseCwd ctx args >>= \case
            Left err -> pure (Left err)
            Right (childCwd, worktree) ->
                restore (spawnFresh childCwd worktree ctx typesRef args)

spawnFresh
    :: OsPath
    -> Maybe SubagentWorktree
    -> MultiAgentContext
    -> GrokSubagentSpecs
    -> TaskArgs
    -> IO (Either Text Text)
spawnFresh childCwd worktree ctx typesRef args = mask \restore -> do
    rootTurnId <- ctx.multiRootTurnId
    let spec = GrokSubagentSpec
            { agentType = args.subagentType
            , modelOverride = args.model
            }
        worktreePath = (.subagentWorktreePath) <$> worktree
    result <- restore
        (spawnSubagentWithCwdPreparedForTurn
            ctx.multiRegistry
            rootTurnId
            childCwd
            (\agentId -> recordAgentSpec typesRef agentId spec)
            ctx.multiSelfId
            ctx.multiDepth
            args.prompt
            (Just args.description))
        `onException` cleanupWorktreeQuietly worktree
    case result of
        Left err -> cleanupFailedWorktree worktree err
        Right agentId -> restore do
            if args.runInBackground
                then pure $ Right $ formatTaskStarted agentId args worktreePath
                else do
                    (statuses, timedOut) <-
                        waitSubagents ctx.multiRegistry [agentId] defaultWaitTimeoutMs
                    pure $ Right $ formatTaskCompleted agentId args worktreePath timedOut
                        (Map.lookup agentId statuses)

cleanupWorktreeQuietly :: Maybe SubagentWorktree -> IO ()
cleanupWorktreeQuietly Nothing = pure ()
cleanupWorktreeQuietly (Just worktree) = do
    _ <- worktree.subagentWorktreeCleanup
    pure ()

cleanupFailedWorktree
    :: Maybe SubagentWorktree
    -> Text
    -> IO (Either Text Text)
cleanupFailedWorktree Nothing spawnError = pure (Left spawnError)
cleanupFailedWorktree (Just worktree) spawnError =
    worktree.subagentWorktreeCleanup >>= \case
        Right () -> pure (Left spawnError)
        Left cleanupError ->
            pure $ Left $
                spawnError <> "\nAdditionally, worktree cleanup failed: " <> cleanupError

resolveTaskWorkspace
    :: OsPath
    -> MultiAgentContext
    -> TaskArgs
    -> IO (Either Text (OsPath, Maybe SubagentWorktree))
resolveTaskWorkspace baseCwd ctx args
    | maybe False isWorktreeIsolation args.isolation =
        case ctx.multiCreateWorktree of
            Nothing -> pure (Left "isolation=\"worktree\" is unavailable in this host.")
            Just create ->
                create baseCwd >>= \case
                    Left err -> pure (Left err)
                    Right worktree ->
                        pure (Right (worktree.subagentWorktreePath, Just worktree))
    | otherwise = do
        resolved <- resolveTaskCwd baseCwd args.cwd
        pure $ case resolved of
            Left err -> Left err
            Right path -> Right (path, Nothing)

isWorktreeIsolation :: Text -> Bool
isWorktreeIsolation isolation =
    Text.toLower (Text.strip isolation)
        `elem` ["worktree", "work_tree", "work-tree"]

resolveTaskCwd :: OsPath -> Maybe Text -> IO (Either Text OsPath)
resolveTaskCwd baseCwd requested = do
    let path = case requested of
            Nothing -> baseCwd
            Just raw ->
                let supplied = fromText (Text.strip raw)
                in if isAbsolute supplied
                    then normalise supplied
                    else normalise (baseCwd </> supplied)
    exists <- doesDirectoryExist path
    pure $ if exists
        then Right path
        else Left ("task cwd is not an existing directory: " <> toText path)

resumeTask
    :: MultiAgentContext
    -> GrokSubagentSpecs
    -> TaskArgs
    -> Text
    -> IO (Either Text Text)
resumeTask ctx typesRef args resumeId = do
    let agentId = SubagentId resumeId
    _ <- case ctx.multiResumeFromDisk of
        Just restore -> restore agentId
        Nothing -> pure (Right ())
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
                    rootTurnId <- ctx.multiRootTurnId
                    sent <- sendInputMessageForTurn ctx.multiRegistry rootTurnId
                        ctx.multiTaskPath agentId
                        (plainInterAgentContent args.prompt) False
                    case sent of
                        Left err -> pure (Left err)
                        Right _ ->
                            if args.runInBackground
                                then pure $ Right $ formatTaskStarted agentId args Nothing
                                else do
                                    (statuses, timedOut) <-
                                        waitSubagents ctx.multiRegistry [agentId] defaultWaitTimeoutMs
                                    pure $ Right $ formatTaskCompleted agentId args Nothing timedOut
                                        (Map.lookup agentId statuses)

formatTaskStarted :: SubagentId -> TaskArgs -> Maybe OsPath -> Text
formatTaskStarted agentId args worktreePath =
    "Subagent started in background.\n\
    \subagent_id: "
        <> agentId.unSubagentId
        <> "\n\
        \type: "
        <> args.subagentType
        <> "\n\
        \description: "
        <> args.description
        <> maybe "" (\path -> "\nworktree_path: " <> toText path) worktreePath
        <> "\n\n\
        \When you need its result, use get_task_output with task_ids=[\""
        <> agentId.unSubagentId
        <> "\"] and a positive timeout_ms."

formatTaskCompleted
    :: SubagentId
    -> TaskArgs
    -> Maybe OsPath
    -> Bool
    -> Maybe SubagentStatus
    -> Text
formatTaskCompleted agentId args worktreePath timedOut mstatus =
    let header =
            "subagent_id: "
                <> agentId.unSubagentId
                <> "\ntype: "
                <> args.subagentType
                <> "\ndescription: "
                <> args.description
                <> maybe "" (\path -> "\nworktree_path: " <> toText path) worktreePath
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

recordAgentSpec :: GrokSubagentSpecs -> SubagentId -> GrokSubagentSpec -> IO ()
recordAgentSpec specsRef agentId spec =
    atomicModifyIORef' specsRef update
  where
    update specs = (Map.insert agentId spec specs, ())

recordAgentType :: GrokSubagentSpecs -> SubagentId -> Text -> IO ()
recordAgentType specsRef agentId agentType = do
    specs <- readIORef specsRef
    let model = maybe Nothing (\spec -> spec.modelOverride) (Map.lookup agentId specs)
    recordAgentSpec specsRef agentId (GrokSubagentSpec agentType model)

lookupAgentType :: GrokSubagentSpecs -> SubagentId -> IO (Maybe Text)
lookupAgentType specsRef agentId = do
    specs <- readIORef specsRef
    pure ((\spec -> spec.agentType) <$> Map.lookup agentId specs)

lookupAgentModel :: GrokSubagentSpecs -> SubagentId -> IO (Maybe Text)
lookupAgentModel specsRef agentId = do
    specs <- readIORef specsRef
    pure (Map.lookup agentId specs >>= \spec -> spec.modelOverride)

-- | Restrict the child tool surface by subagent type.
filterGrokToolsForType :: Text -> [AppTool] -> [AppTool]
filterGrokToolsForType agentType tools = case agentType of
    "explore" -> filter ((`elem` exploreNames) . (.appToolName)) tools
    "plan" -> filter ((`elem` planNames) . (.appToolName)) tools
    _ -> filter ((/= "task") . (.appToolName)) tools
  where
    exploreNames :: [Text]
    exploreNames =
        [ "read_file"
        , "list_dir"
        , "grep"
        ]
    planNames :: [Text]
    planNames =
        [ "read_file"
        , "list_dir"
        , "grep"
        , "enter_plan_mode"
        , "exit_plan_mode"
        , "ask_user_question"
        ]
