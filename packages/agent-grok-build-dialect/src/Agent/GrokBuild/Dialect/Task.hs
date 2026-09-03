-- | Grok-build @task@ tool with bounded nested subagents.
--
-- Wire names and defaults follow xai-org/grok-build TaskToolInput /
-- build_task_description. Runtime reuses 'Agent.Subagents'.
module Agent.GrokBuild.Dialect.Task
    ( taskTool
    , filterGrokToolsForType
    , knownSubagentTypes
    , defaultSubagentType
    , runtimeSubagentType
    , GrokSubagentSpec(..)
    , GrokSubagentSpecs
    , isSubagentIdText
    , formatTaskStarted
    , formatTaskCompleted
    , recordAgentSpec
    , recordAgentType
    , lookupAgentType
    , lookupAgentModel
    , lookupAgentReasoningEffort
    , spawnManagedGrokSubagent
    , lunaSubagentModel
    , lunaSubagentEffort
    , grokRootChildModels
    , canonicalizeGrokChildModel
    , resolveRequestedGrokChildModel
    , isLunaSubagentModel
    ) where

import Agent.InterAgentMessage (plainInterAgentContent)
import Agent.OsPath (fromText, toText)
import Agent.Subagents
    ( SubagentId(..)
    , SubagentStatus(..)
    , defaultMaxDepth
    , defaultWaitTimeoutMs
    , getStatus
    , sendInputMessageForTurn
    , spawnSubagentWithCwdPreparedForTurn
    , subagentLease
    , waitSubagents
    )
import qualified Agent.Json.Decode as Json
import Agent.GrokBuild.Dialect.Json (optionalBool, optionalText)
import Agent.ToolDSL
    ( PropertySchema(..)
    , PropertyType(..)
    )
import Agent.ToolDispatch (typedTool)
import Agent.Tools.MultiAgents
    ( CollaborationModelTarget
    , CollaborationSpawnOptions(..)
    , MultiAgentContext(..)
    , SubagentWorktree(..)
    )
import Agent.Tools.Types
    ( AppTool(..)
    , ApprovalRule(..)
    , ToolExecutionPolicy(..)
    , jsonAppToolWithExecution
    )
import Control.Concurrent.MVar (modifyMVar, newMVar)
import Control.Exception.Safe (mask, onException)
import Control.Monad (void)
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory.OsPath (doesDirectoryExist)
import System.OsPath (OsPath, isAbsolute, normalise, (</>))

defaultSubagentType :: Text
defaultSubagentType = "general-purpose"

-- | Internal type for scheduler/workflow children. It retains the normal
-- coding surface but cannot recursively delegate or access root runtimes.
runtimeSubagentType :: Text
runtimeSubagentType = "runtime-bounded"

knownSubagentTypes :: [Text]
knownSubagentTypes = ["general-purpose", "explore", "plan"]

-- | OpenAI child slug allowed from a Grok root when OpenAI auth is present.
lunaSubagentModel :: Text
lunaSubagentModel = "gpt-5.6-luna"

-- | Grok-root Luna children always run at high reasoning effort.
lunaSubagentEffort :: Text
lunaSubagentEffort = "high"

-- | Models a Grok-root spawn_subagent call may select.
grokRootChildModels :: Bool -> [Text]
grokRootChildModels openaiAvailable =
    "grok-4.6" : "grok-4.5" : [lunaSubagentModel | openaiAvailable]

isLunaSubagentModel :: Text -> Bool
isLunaSubagentModel = (== lunaSubagentModel)

-- | Map a model-facing slug onto the Grok-root allowlist, if it is one.
canonicalizeGrokChildModel :: Text -> Maybe Text
canonicalizeGrokChildModel raw =
    case folded of
        "grok-4.6" -> Just "grok-4.6"
        "grok-4-6" -> Just "grok-4.6"
        "grok4.6" -> Just "grok-4.6"
        "grok4-6" -> Just "grok-4.6"
        "grok-4.5" -> Just "grok-4.5"
        "grok-4-5" -> Just "grok-4.5"
        "grok4.5" -> Just "grok-4.5"
        "grok4-5" -> Just "grok-4.5"
        "gpt-5.6-luna" -> Just lunaSubagentModel
        "gpt-5-6-luna" -> Just lunaSubagentModel
        "gpt5.6luna" -> Just lunaSubagentModel
        "gpt5-6luna" -> Just lunaSubagentModel
        "luna" -> Just lunaSubagentModel
        "openai/gpt-5.6-luna" -> Just lunaSubagentModel
        "openai/gpt-5-6-luna" -> Just lunaSubagentModel
        _ -> Nothing
  where
    folded =
        Text.toLower
            $ Text.replace "_" "-"
            $ Text.filter (/= ' ')
            $ Text.strip raw

-- | 'Nothing' policy keeps historical passthrough. A Just allowlist rejects
-- unknown slugs and canonicalizes aliases. Omitting model still inherits.
resolveRequestedGrokChildModel
    :: Maybe [Text]
    -> Maybe Text
    -> Either Text (Maybe Text)
resolveRequestedGrokChildModel Nothing requested = Right requested
resolveRequestedGrokChildModel _ Nothing = Right Nothing
resolveRequestedGrokChildModel (Just allowed) (Just raw) =
    case canonicalizeGrokChildModel raw of
        Just canonical
            | canonical `elem` allowed -> Right (Just canonical)
            | otherwise -> Left (unknownChildModelMessage raw allowed)
        Nothing -> Left (unknownChildModelMessage raw allowed)

unknownChildModelMessage :: Text -> [Text] -> Text
unknownChildModelMessage raw allowed =
    "Unknown spawn_subagent model '"
        <> raw
        <> "'. Valid model slugs: "
        <> Text.intercalate ", " allowed
        <> ". Omit `model` to inherit the parent model."

grokChildModelGuidance :: [Text] -> Text
grokChildModelGuidance slugs =
    "If you choose an explicit model, you may ONLY use model slugs from this list:\n"
        <> Text.intercalate "\n" (map ("- " <>) slugs)
        <> "\n\nIf you do not explicitly request a model, omit `model` to inherit the parent model."
        <> lunaNote
  where
    lunaNote
        | lunaSubagentModel `elem` slugs =
            " Use `"
                <> lunaSubagentModel
                <> "` only for mechanical, bounded, easily verified work; it runs"
                <> " at high reasoning effort. Do not use Luna as a blanket default."
                <> " Inherit the parent for ambiguous, high-risk, or"
                <> " judgment-heavy work."
        | otherwise = ""

taskModelProperty :: Maybe [Text] -> Text
taskModelProperty = \case
    Just slugs -> grokChildModelGuidance slugs
    Nothing -> "Optional model slug. Omit to inherit the parent model."

data GrokSubagentSpec = GrokSubagentSpec
    { agentType :: !Text
    , modelOverride :: !(Maybe Text)
    , reasoningEffortOverride :: !(Maybe Text)
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

taskArgsDecoder :: Json.Decoder TaskArgs
taskArgsDecoder = Json.object do
        prompt <- Json.atKey "prompt" Json.text
        description <- Json.atKey "description" Json.text
        subagentType <- fromMaybe defaultSubagentType <$> optionalText "subagent_type"
        canonicalBackground <- optionalBool "run_in_background"
        legacyBackground <- optionalBool "background"
        let runInBackground =
                fromMaybe True $ case canonicalBackground of
                    Just value -> Just value
                    Nothing -> legacyBackground
        resumeFrom <- sanitizeOptional <$> optionalText "resume_from"
        cwd <- sanitizeOptional <$> optionalText "cwd"
        model <- sanitizeOptional <$> optionalText "model"
        isolation <- sanitizeOptional <$> optionalText "isolation"
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
    jsonAppToolWithExecution "task" (taskDescription advertisedModels)
        [ PropertySchema "prompt" PropertyString True $ Just
            "The full task prompt for the subagent to execute."
        , PropertySchema "description" PropertyString True $ Just
            "Short description of the task (3-5 words)."
        , PropertySchema "subagent_type" PropertyString False $ Just
            "Name of the subagent type to launch. Built-in types: \"general-purpose\", \"explore\", \"plan\"."
        , PropertySchema "run_in_background" PropertyBoolean False $ Just
            "Returns immediately with a subagent_id. Use get_command_or_subagent_output to retrieve results. Defaults to true."
        , PropertySchema "resume_from" PropertyString False $ Just
            "Resume from a previously completed subagent's conversation. Pass the subagent_id from a prior spawn_subagent call."
        , PropertySchema "cwd" PropertyString False $ Just
            "Explicit working directory for the subagent. Mutually exclusive with isolation=\"worktree\"."
        , PropertySchema "model" PropertyString False $ Just
            (taskModelProperty advertisedModels)
        , PropertySchema "isolation" (PropertyEnum ["none", "worktree"]) False $ Just
            "Isolation mode: \"none\" (default, shared workspace) or \"worktree\" (isolated git worktree). Worktree mode prevents child edits from affecting the parent workspace until explicitly applied."
        ]
        (taskApproval ctx)
        TurnSequential
        (typedTool "task" taskArgsDecoder (runTask baseCwd ctx specsRef))
  where
    advertisedModels =
        case ctx.multiChildModelAllowed of
            Just _ -> Nothing
            Nothing -> ctx.multiAllowedChildModels

taskApproval :: MultiAgentContext -> ApprovalRule
taskApproval ctx = case ctx.multiSelfId of
    Nothing -> AlwaysPrompt
    Just _ -> AlwaysReadOnly

taskDescription :: Maybe [Text] -> Text
taskDescription allowedModels =
    "Start a subagent that works on a task independently and reports back.\n\n\
    \Agent types:\n\n\
    \- **general-purpose**: General purpose agent for multi-step tasks. Can delegate further work with spawn_subagent while below the harness nesting limit.\n\
    \- **explore**: Fast, read-only agent specialized for codebase exploration. Read-only — has access to: read_file, list_dir, grep.\n\
    \- **plan**: Software architect for planning implementation strategies. Read-only — has access to: read_file, list_dir, grep, todo_write, web_search, enter_plan_mode, exit_plan_mode, ask_user_question.\n\n\
    \## Usage notes\n\
    \- When the agent is done, it returns a single message with its agent ID. Use that ID to resume the agent later for follow-up work.\n\
    \- background: Returns immediately with a subagent_id. Use get_command_or_subagent_output to retrieve results. This is set to true by default.\n\
    \- Subagents receive project instructions from AGENTS.md. Include any especially important task-specific conventions directly in the prompt.\n\
    \- subagent_type defaults to general-purpose when omitted.\n\
    \- Nested delegation is limited to "
    <> Text.pack (show defaultMaxDepth)
    <> (if defaultMaxDepth == 1 then " level" else " levels")
    <> " below the root agent. At the limit, complete the assigned task directly.\n\
    \- When launching independent subagents, you MUST incorporate the results into the task based on requirements BEFORE concluding.\n\n\
    \Resuming a previous agent (resume_from):\n\
    \- Use resume_from to continue a previously completed subagent's conversation. Pass the subagent_id returned by a prior spawn_subagent call. A resumed agent keeps its transcript, so only describe what changed since the last run.\n\
    \- The resumed agent must use the same subagent_type as the source.\n\n\
    \Isolation mode:\n\
    \- Use isolation=\"worktree\" to run the child in an isolated git worktree. The worktree is preserved after completion and its path is returned in the output."
    <> maybe "" (\slugs -> "\n\n" <> grokChildModelGuidance slugs) allowedModels

runTask
    :: OsPath
    -> MultiAgentContext
    -> GrokSubagentSpecs
    -> TaskArgs
    -> IO (Either Text Text)
runTask baseCwd ctx typesRef args
    | Text.null (Text.strip args.prompt) =
        pure (Left "spawn_subagent requires a non-empty prompt")
    | Text.null (Text.strip args.description) =
        pure (Left "spawn_subagent requires a non-empty description")
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
    | otherwise = do
        resolveTaskChildModel ctx args.model >>= \case
            Left err -> pure (Left err)
            Right (model, resolvedModel) -> mask \restore ->
                resolveTaskWorkspace baseCwd ctx args >>= \case
                    Left err -> pure (Left err)
                    Right (childCwd, worktree) ->
                        restore
                            (spawnFresh
                                childCwd
                                worktree
                                ctx
                                typesRef
                                args { model }
                                resolvedModel)

resolveTaskChildModel
    :: MultiAgentContext
    -> Maybe Text
    -> IO (Either Text (Maybe Text, Maybe CollaborationModelTarget))
resolveTaskChildModel ctx requested =
    case sanitizeOptional requested of
        Nothing -> pure (Right (Nothing, Nothing))
        Just model ->
            case ctx.multiResolveChildModel of
                Just resolve ->
                    resolve model >>= \case
                        Nothing -> pure (Left organizationModelDenied)
                        Just target ->
                            pure (Right (Just model, Just target))
                Nothing ->
                    case ctx.multiChildModelAllowed of
                        Nothing ->
                            pure
                                (fmap
                                    (\modelOverride ->
                                        (modelOverride, Nothing))
                                    (resolveRequestedGrokChildModel
                                        ctx.multiAllowedChildModels
                                        (Just model)))
                        Just isAllowed ->
                            isAllowed model >>= \allowed ->
                                pure
                                    (if allowed
                                        then Right (Just model, Nothing)
                                        else Left organizationModelDenied)
  where
    organizationModelDenied =
        "The requested child model is not allowed by this organization."

spawnFresh
    :: OsPath
    -> Maybe SubagentWorktree
    -> MultiAgentContext
    -> GrokSubagentSpecs
    -> TaskArgs
    -> Maybe CollaborationModelTarget
    -> IO (Either Text Text)
spawnFresh childCwd worktree ctx typesRef args resolvedModel = mask \restore -> do
    ownedWorktree <- traverse makeIdempotentWorktree worktree
    rootTurnId <- ctx.multiRootTurnId
    let spec = GrokSubagentSpec
            { agentType = args.subagentType
            , modelOverride = args.model
            , reasoningEffortOverride =
                if maybe False isLunaSubagentModel args.model
                    then Just lunaSubagentEffort
                    else Nothing
            }
        worktreePath = (.subagentWorktreePath) <$> ownedWorktree
        prepare agentId = do
            prepareGrokSpawn ctx typesRef spec resolvedModel agentId
            pure $ case ownedWorktree of
                Nothing -> mempty
                Just lease ->
                    subagentLease (void lease.subagentWorktreeCleanup)
    result <- restore
        (spawnSubagentWithCwdPreparedForTurn
            ctx.multiRegistry
            rootTurnId
            childCwd
            prepare
            ctx.multiSelfId
            ctx.multiDepth
            args.prompt
            (Just args.description))
        `onException` cleanupWorktreeQuietly ownedWorktree
    case result of
        Left err -> cleanupFailedWorktree ownedWorktree err
        Right agentId -> restore do
            if args.runInBackground
                then pure $ Right $ formatTaskStarted agentId args worktreePath
                else do
                    (statuses, timedOut) <-
                        waitSubagents ctx.multiRegistry [agentId] defaultWaitTimeoutMs
                    pure $ Right $ formatTaskCompleted agentId args worktreePath timedOut
                        (Map.lookup agentId statuses)

makeIdempotentWorktree :: SubagentWorktree -> IO SubagentWorktree
makeIdempotentWorktree worktree = do
    resultVar <- newMVar Nothing
    let cleanup = modifyMVar resultVar \case
            Just result -> pure (Just result, result)
            Nothing -> do
                result <- worktree.subagentWorktreeCleanup
                pure (Just result, result)
    pure worktree { subagentWorktreeCleanup = cleanup }

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
    restored <- case ctx.multiResumeFromDisk of
        Just restore -> restore agentId
        Nothing -> pure (Right ())
    case restored of
        Left err -> pure (Left err)
        Right () -> do
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
                                                waitSubagents
                                                    ctx.multiRegistry
                                                    [agentId]
                                                    defaultWaitTimeoutMs
                                            pure $ Right $
                                                formatTaskCompleted
                                                    agentId
                                                    args
                                                    Nothing
                                                    timedOut
                                                    (Map.lookup agentId statuses)

-- | Spawn a detached Grok-managed child without going through the public
-- @task@ argument parser. Session-owned runtimes such as the scheduler and
-- workflow launcher use this so their child is not cancelled when the
-- currently active root turn aborts, and so they retain the real subagent id
-- rather than parsing model-facing formatted output.
spawnManagedGrokSubagent
    :: OsPath
    -> MultiAgentContext
    -> GrokSubagentSpecs
    -> GrokSubagentSpec
    -> Text
    -> Maybe Text
    -> IO (Either Text SubagentId)
spawnManagedGrokSubagent childCwd ctx typesRef spec prompt nickname
    | Text.null (Text.strip prompt) =
        pure (Left "subagent prompt must be non-empty")
    | otherwise = do
        resolvedSpec <-
            case ctx.multiResolveChildModel of
                Nothing -> pure (Right (spec, Nothing))
                Just _ ->
                    resolveTaskChildModel ctx spec.modelOverride >>= \case
                        Left err -> pure (Left err)
                        Right (model, resolvedModel) ->
                            pure
                                (Right
                                    ( spec { modelOverride = model }
                                    , resolvedModel
                                    ))
        case resolvedSpec of
            Left err -> pure (Left err)
            Right (preparedSpec, resolvedModel) ->
                spawnSubagentWithCwdPreparedForTurn
                    ctx.multiRegistry
                    Nothing
                    childCwd
                    (\agentId -> do
                        prepareGrokSpawn
                            ctx typesRef preparedSpec resolvedModel agentId
                        pure mempty)
                    ctx.multiSelfId
                    ctx.multiDepth
                    prompt
                    nickname

prepareGrokSpawn
    :: MultiAgentContext
    -> GrokSubagentSpecs
    -> GrokSubagentSpec
    -> Maybe CollaborationModelTarget
    -> SubagentId
    -> IO ()
prepareGrokSpawn ctx typesRef spec resolvedModel agentId = do
    case ctx.multiPrepareSpawn of
        Just prepare
            | isNothing spec.modelOverride || isJust resolvedModel ->
                prepare agentId CollaborationSpawnOptions
                    { collaborationModel = spec.modelOverride
                    , collaborationResolvedModel = resolvedModel
                    , collaborationReasoningEffort =
                        spec.reasoningEffortOverride
                    , collaborationForkTurns = Just "none"
                    }
        _ -> pure ()
    -- The host hook records collaboration defaults; retain the Grok task's
    -- actual type and effort after it installs the authoritative target.
    recordAgentSpec typesRef agentId spec

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
recordAgentType specsRef agentId agentType =
    atomicModifyIORef' specsRef \specs ->
        let existing = Map.lookup agentId specs
            model = existing >>= \spec -> spec.modelOverride
            effort = existing >>= \spec -> spec.reasoningEffortOverride
            updated = GrokSubagentSpec agentType model effort
        in (Map.insert agentId updated specs, ())

lookupAgentType :: GrokSubagentSpecs -> SubagentId -> IO (Maybe Text)
lookupAgentType specsRef agentId =
    fmap (\spec -> spec.agentType) <$> lookupAgentSpec specsRef agentId

lookupAgentModel :: GrokSubagentSpecs -> SubagentId -> IO (Maybe Text)
lookupAgentModel specsRef agentId =
    (>>= \spec -> spec.modelOverride) <$> lookupAgentSpec specsRef agentId

lookupAgentReasoningEffort :: GrokSubagentSpecs -> SubagentId -> IO (Maybe Text)
lookupAgentReasoningEffort specsRef agentId =
    (>>= \spec -> spec.reasoningEffortOverride)
        <$> lookupAgentSpec specsRef agentId

lookupAgentSpec
    :: GrokSubagentSpecs
    -> SubagentId
    -> IO (Maybe GrokSubagentSpec)
lookupAgentSpec specsRef agentId =
    Map.lookup agentId <$> readIORef specsRef

-- | Restrict the child tool surface by subagent type.
filterGrokToolsForType :: Text -> [AppTool] -> [AppTool]
filterGrokToolsForType agentType tools = case agentType of
    "explore" -> filter ((`elem` exploreNames) . (.appToolName)) tools
    "plan" -> filter ((`elem` planNames) . (.appToolName)) tools
    "runtime-bounded" ->
        filter ((`notElem` runtimeHiddenNames) . (.appToolName)) tools
    _ -> filter ((`notElem` rootOnlyNames) . (.appToolName)) tools
  where
    rootOnlyNames :: [Text]
    rootOnlyNames =
        [ "scheduler_create"
        , "scheduler_delete"
        , "scheduler_list"
        , "update_goal"
        , "workflow"
        ]
    runtimeHiddenNames :: [Text]
    runtimeHiddenNames = "task" : rootOnlyNames
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
        , "todo_write"
        , "enter_plan_mode"
        , "exit_plan_mode"
        , "ask_user_question"
        ]
