-- | Spawning delegated child agents from a tool call.
--
-- A 'MultiAgentContext' is the calling agent's view of its conversation's
-- registry. 'spawnSubagentFromCall' validates a spawn request, resolves its
-- model override, creates an isolated worktree when asked and admits the
-- child. Dialects build their model-facing tools on top of this, such as the
-- Codex collaboration tools and the Grok @task@ tool.
module Agent.Subagents.Spawn
    ( MultiAgentContext(..)
    , CollaborationModelTarget(..)
    , CollaborationSpawnOptions(..)
    , SubagentWorktree(..)
    , SpawnWorkspace(..)
    , SpawnRequest(..)
    , rootMultiAgentContext
    , spawnSubagentFromCall
    , spawnSharedSubagent
    , callMessageContent
    , makeIdempotentWorktree
    , cleanupWorktreeQuietly
    , cleanupFailedWorktree
    ) where

import Agent.Dialect (DialectId)
import Agent.InterAgentMessage
    ( InterAgentMessage
    , InterAgentMessageContent
    , encryptedInterAgentContent
    , plainInterAgentContent
    )
import Agent.OsPath (toText)
import Agent.Provider (Provider)
import Agent.Subagents.Loop
    ( RootSubagents
    , currentRootTurn
    , rootSubagentsRegistry
    )
import Agent.Subagents.Registry
    ( SubagentRegistry
    , spawnSubagentAtWithCwdPreparedForTurn
    , subagentLease
    )
import Agent.Subagents.TaskPath (TaskPath, taskPathRoot, taskPathText)
import Agent.Subagents.Types (RootTurnId, SubagentId)
import Agent.ToolArgs (readExactInt)
import Agent.ToolDispatch (ToolCall(..), canonicalToolName)
import Control.Concurrent.MVar (modifyMVar, newMVar)
import Control.Exception.Safe (mask, onException)
import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as Text
import System.OsPath (OsPath)

data SubagentWorktree = SubagentWorktree
    { subagentWorktreePath :: !OsPath
    , subagentWorktreeCleanup :: !(IO (Either Text ()))
    }

data CollaborationModelTarget = CollaborationModelTarget
    { collaborationTargetProvider :: !Provider
    , collaborationTargetConnection :: !Text
    , collaborationTargetEffectiveModel :: !Text
    , collaborationTargetDialect :: !DialectId
    } deriving (Eq, Show)

data CollaborationSpawnOptions = CollaborationSpawnOptions
    { collaborationModel :: !(Maybe Text)
    , collaborationResolvedModel :: !(Maybe CollaborationModelTarget)
    , collaborationReasoningEffort :: !(Maybe Text)
    , collaborationForkTurns :: !(Maybe Text)
    } deriving (Eq, Show)

-- | Per-agent identity for nesting depth / parent linkage / task path.
data MultiAgentContext = MultiAgentContext
    { multiRegistry :: !SubagentRegistry
    , multiCwd :: !OsPath
    , multiSelfId :: !(Maybe SubagentId)
    , multiDepth :: !Int
    , multiTaskPath :: !TaskPath
    , multiRootTurnId :: !(IO (Maybe RootTurnId))
      -- | Optional host hook to rehydrate a closed/missing agent from disk
      -- before follow-ups. 'Nothing' means in-memory only.
    , multiResumeFromDisk :: !(Maybe (SubagentId -> IO (Either Text ())))
      -- | Optional host hook for isolated worktree children.
    , multiCreateWorktree :: !(Maybe (OsPath -> IO (Either Text SubagentWorktree)))
      -- | Record model/effort overrides and seed the child transcript before
      -- its worker starts. Without it, model, effort and history-fork
      -- options have no effect.
    , multiPrepareSpawn
        :: !(Maybe (SubagentId -> CollaborationSpawnOptions -> IO ()))
      -- | Deliver a child message to the root agent's next model turn.
    , multiSendToRoot :: !(Maybe (InterAgentMessage -> IO (Either Text Text)))
      -- | Optional provider/billing-specific guidance for model overrides.
    , multiSpawnModelGuidance :: !(Maybe Text)
      -- | When set, spawn_subagent may only use these model slugs (or inherit).
    , multiAllowedChildModels :: !(Maybe [Text])
      -- | Optional live resolver for organization-scoped model aliases.
      -- Unknown explicit aliases are denied and resolved transport metadata is
      -- carried atomically into spawn preparation.
    , multiResolveChildModel
        :: !(Maybe (Text -> IO (Maybe CollaborationModelTarget)))
      -- | Optional live policy used instead of the advertised snapshot above.
      -- This lets hosts enforce organization catalog refreshes without
      -- rebuilding every tool definition in an active session.
    , multiChildModelAllowed :: !(Maybe (Text -> IO Bool))
    }

-- | The root agent's context for a loop that runs its children through a
-- 'RootSubagents' ('Agent.Subagents.subagentLoop'). Optional host hooks
-- are off; set them with a record update.
rootMultiAgentContext :: RootSubagents -> OsPath -> MultiAgentContext
rootMultiAgentContext session cwd = MultiAgentContext
    { multiRegistry = rootSubagentsRegistry session
    , multiCwd = cwd
    , multiSelfId = Nothing
    , multiDepth = 0
    , multiTaskPath = taskPathRoot
    , multiRootTurnId = currentRootTurn session
    , multiResumeFromDisk = Nothing
    , multiCreateWorktree = Nothing
    , multiPrepareSpawn = Nothing
    , multiSendToRoot = Nothing
    , multiSpawnModelGuidance = Nothing
    , multiAllowedChildModels = Nothing
    , multiResolveChildModel = Nothing
    , multiChildModelAllowed = Nothing
    }

-- | Where a spawned child works.
data SpawnWorkspace
    = SharedWorkspace
    -- | A worktree from 'multiCreateWorktree', owned by the child.
    | IsolatedWorktree

-- | A spawn request as the model made it.
data SpawnRequest = SpawnRequest
    { taskName :: Text
    , message :: Text
    , model :: Maybe Text
    , reasoningEffort :: Maybe Text
    , forkTurns :: Maybe Text
    }

-- | Validate a spawn request, resolve its model override and admit the
-- child. Returns the child's canonical task name, and its worktree, as
-- labeled text. Errors name the tool that was called.
spawnSubagentFromCall
    :: MultiAgentContext
    -> SpawnWorkspace
    -> ToolCall
    -> SpawnRequest
    -> IO (Either Text Text)
spawnSubagentFromCall ctx workspace call args
    | Text.null (Text.strip args.message) =
        pure (Left (canonicalToolName call.name <> " requires a non-empty message"))
    | not (validForkTurns args.forkTurns) =
        pure (Left "fork_turns must be none, all, or a positive integer string")
    | hasSpawnOverride args && usesFullHistory args.forkTurns =
        pure (Left
            "full-history forks inherit the parent model and reasoning effort; \
            \set fork_turns to none or a positive integer when overriding \
            \model or reasoning_effort")
    | otherwise = do
        resolveChildModelOverride ctx args.model >>= \case
            Left err -> pure (Left err)
            Right resolvedModel -> mask \restore ->
                resolveSpawnWorkspace ctx call workspace >>= \case
                    Left err -> pure (Left err)
                    Right (childCwd, worktree) ->
                        restore
                            (spawnInWorkspace
                                ctx call args resolvedModel childCwd worktree)

-- | Spawn a tracked child in the caller's shared workspace.
--
-- This is the programmatic counterpart of @spawn_agent@: hosts may use it
-- from another built-in tool without round-tripping through the JSON tool
-- dispatcher.  Validation, model sanitization, preparation hooks, task-path
-- accounting, and lifecycle ownership remain exactly the same as for the
-- public collaboration tool.
spawnSharedSubagent
    :: MultiAgentContext
    -> ToolCall
    -> Text
    -> Text
    -> Maybe Text
    -> Maybe Text
    -> Maybe Text
    -> IO (Either Text Text)
spawnSharedSubagent ctx call taskName message model reasoningEffort forkTurns =
    spawnSubagentFromCall
        ctx
        SharedWorkspace
        call
        SpawnRequest
            { taskName
            , message
            , model
            , reasoningEffort
            , forkTurns
            }

spawnInWorkspace
    :: MultiAgentContext
    -> ToolCall
    -> SpawnRequest
    -> Maybe CollaborationModelTarget
    -> OsPath
    -> Maybe SubagentWorktree
    -> IO (Either Text Text)
spawnInWorkspace ctx call args resolvedModel childCwd worktree = mask \restore -> do
    ownedWorktree <- traverse makeIdempotentWorktree worktree
    rootTurnId <- ctx.multiRootTurnId
    let spawnOptions = CollaborationSpawnOptions
            { collaborationModel = sanitizeOverride args.model
            , collaborationResolvedModel = resolvedModel
            , collaborationReasoningEffort =
                sanitizeOverride args.reasoningEffort
            , collaborationForkTurns = normalizeForkTurns args.forkTurns
            }
        prepare agentId = do
            maybe (pure ()) (\hook -> hook agentId spawnOptions)
                ctx.multiPrepareSpawn
            pure $ case ownedWorktree of
                Nothing -> mempty
                Just lease ->
                    subagentLease (void lease.subagentWorktreeCleanup)
    result <- restore
        (spawnSubagentAtWithCwdPreparedForTurn
            ctx.multiRegistry
            rootTurnId
            childCwd
            prepare
            ctx.multiSelfId
            ctx.multiTaskPath
            ctx.multiDepth
            args.taskName
            (callMessageContent call args.message)
            Nothing)
        `onException` cleanupWorktreeQuietly ownedWorktree
    case result of
        Left err -> cleanupFailedWorktree ownedWorktree err
        Right (_agentId, path) ->
            pure $ Right $ renderSpawnResult path ownedWorktree

renderSpawnResult :: TaskPath -> Maybe SubagentWorktree -> Text
renderSpawnResult path worktree =
    Text.intercalate "\n" $
        ("Agent: " <> taskPathText path)
            : maybe
                []
                (\lease -> ["Worktree: " <> toText lease.subagentWorktreePath])
                worktree

resolveSpawnWorkspace
    :: MultiAgentContext
    -> ToolCall
    -> SpawnWorkspace
    -> IO (Either Text (OsPath, Maybe SubagentWorktree))
resolveSpawnWorkspace ctx call = \case
    SharedWorkspace -> pure (Right (ctx.multiCwd, Nothing))
    IsolatedWorktree ->
        case ctx.multiCreateWorktree of
            Nothing ->
                pure (Left (canonicalToolName call.name <> " is unavailable in this host."))
            Just create ->
                create ctx.multiCwd >>= \case
                    Left err -> pure (Left err)
                    Right worktree ->
                        pure (Right (worktree.subagentWorktreePath, Just worktree))

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
cleanupWorktreeQuietly (Just worktree) =
    void worktree.subagentWorktreeCleanup

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

validForkTurns :: Maybe Text -> Bool
validForkTurns = \case
    Nothing -> True
    Just turns ->
        let stripped = Text.toLower (Text.strip turns)
        in stripped `elem` ["none", "all", ""]
            || case readExactInt stripped of
                Just count -> count > 0
                _ -> False

hasSpawnOverride :: SpawnRequest -> Bool
hasSpawnOverride args =
    any (/= Nothing)
        [ sanitizeOverride args.model
        , sanitizeOverride args.reasoningEffort
        ]

usesFullHistory :: Maybe Text -> Bool
usesFullHistory forkTurns =
    normalizeForkTurns forkTurns == Just "all"

sanitizeOverride :: Maybe Text -> Maybe Text
sanitizeOverride value = value >>= \raw ->
    let stripped = Text.strip raw
    in if Text.null stripped then Nothing else Just stripped

allowedModelOverride :: Maybe [Text] -> Maybe Text -> Bool
allowedModelOverride allowed override =
    case sanitizeOverride override of
        Nothing -> True
        Just requested ->
            maybe
                True
                (elem requested . map Text.strip)
                allowed

resolveChildModelOverride
    :: MultiAgentContext
    -> Maybe Text
    -> IO (Either Text (Maybe CollaborationModelTarget))
resolveChildModelOverride ctx override =
    case sanitizeOverride override of
        Nothing -> pure (Right Nothing)
        Just requested ->
            case ctx.multiResolveChildModel of
                Just resolve ->
                    resolve requested >>= \case
                        Nothing -> pure (Left notAllowed)
                        Just target -> pure (Right (Just target))
                Nothing -> do
                    allowed <- case ctx.multiChildModelAllowed of
                        Just isAllowed -> isAllowed requested
                        Nothing ->
                            pure
                                (allowedModelOverride
                                    ctx.multiAllowedChildModels
                                    (Just requested))
                    pure
                        (if allowed
                            then Right Nothing
                            else Left notAllowed)
  where
    notAllowed =
        "The requested child model is not allowed by this organization."

normalizeForkTurns :: Maybe Text -> Maybe Text
normalizeForkTurns value =
    case Text.toLower . Text.strip <$> value of
        Nothing -> Just "all"
        Just "" -> Just "all"
        normalized -> normalized

-- | A message the model wrote in a tool call, encrypted when the call's
-- arguments were.
callMessageContent :: ToolCall -> Text -> InterAgentMessageContent
callMessageContent call
    | call.argumentsEncrypted = encryptedInterAgentContent
    | otherwise = plainInterAgentContent
