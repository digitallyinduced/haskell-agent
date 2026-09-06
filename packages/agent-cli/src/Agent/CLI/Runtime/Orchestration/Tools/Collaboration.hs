-- | Root collaboration setup and callbacks. Registry ownership remains with
-- the session's existing close action.
module Agent.CLI.Runtime.Orchestration.Tools.Collaboration
    ( CollaborationRuntime(..)
    , newCollaborationRuntime
    , installCollaborationCallbacks
    ) where

import Agent.CLI.Auth (LoadedAuth(..), hasOpenAiAuth, loadAuth)
import Agent.CLI.Config (HarnessConfig(..))
import Agent.CLI.GatewayClient (cachedGatewayModels)
import Agent.CLI.GatewayModels (modelOptionsForGatewayModels)
import Agent.CLI.Models (ModelOption(..), ModelTarget(..), resolveModelOptionById)
import Agent.CLI.Options (CliOptions(..))
import Agent.CLI.PendingInputs
    ( PendingInputs, PendingNoticeKind(..), enqueuePendingInput
    , enqueuePendingNotice, newPendingInputs )
import Agent.CLI.Project (ProjectSettings(..))
import Agent.CLI.Prompt (subscriptionSubagentModelGuidance)
import Agent.CLI.Runtime.Orchestration.Startup (reportStartupWarning)
import Agent.CLI.Runtime.Orchestration.Tools.Model
import Agent.CLI.Runtime.Orchestration.Tools.Request
import Agent.CLI.Runtime.Orchestration.Types (NativeRunCapabilities(..))
import Agent.CLI.Session (Persistence(..))
import Agent.CLI.SessionLock (SessionLock)
import Agent.CLI.Subagents.Runtime
    ( flushAllSubagentSnapshots, persistAndEvictSubagentSessionWithStatus
    , prepareCollaborationSpawn, restoreAgentFromDisk )
import Agent.CLI.Subagents.Runtime.Types (SubagentSession, SubagentStoreRoot)
import Agent.CLI.Worktree (createManagedWorktree, removeWorktree)
import Agent.GrokBuild.Dialect.Task (GrokSubagentSpecs, grokRootChildModels)
import Agent.Loop (TurnInput(..), LoopError(..))
import Agent.Provider (Provider(..), TokenProvider, tokenProviderBillingMode)
import Agent.Responses.Types (ResponseItem)
import Agent.Subagents
    ( RootTurnId, SubagentId, SubagentRegistry, SubagentConfig(..)
    , closeSubagentRegistry, defaultMaxConcurrent, defaultSubagentConfig
    , formatCompletionNotice, interruptActiveSubagents, newSubagentRegistry
    , setSubagentOnComplete, setSubagentOnSettled )
import Agent.Subagents.TaskPath (taskPathRoot)
import Agent.Tools.MultiAgents
    ( CollaborationModelTarget(..), MultiAgentContext(..), SubagentWorktree(..) )
import Control.Applicative ((<|>))
import Data.IORef (IORef, newIORef, readIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as Text
import System.OsPath (OsPath)

data CollaborationRuntime = CollaborationRuntime
    { collaborationActiveSessionLock :: IORef (Maybe SessionLock)
    , collaborationPersistSlotRef :: IORef Persistence
    , collaborationSubagentSessions
        :: IORef (Map SubagentId SubagentSession)
    , collaborationSubagentStoreRoot :: SubagentStoreRoot
    , collaborationSubagentForkSource
        :: IORef (Maybe (IO [ResponseItem]))
    , collaborationPendingNotices :: PendingInputs
    , collaborationRegistry :: SubagentRegistry
    , collaborationRootTurnRef :: IORef (Maybe RootTurnId)
    , collaborationAgentTypes :: GrokSubagentSpecs
    , collaborationOpenAiChild :: Maybe TokenProvider
    , collaborationAllowedChildModels :: Maybe [Text]
    , collaborationChildModelAllowed :: Maybe (Text -> IO Bool)
    , collaborationResolveChildModel
        :: Maybe (Text -> IO (Maybe CollaborationModelTarget))
    , collaborationGatewayChildModelOption
        :: Maybe (Text -> IO (Maybe ModelOption))
    , collaborationCreateWorktree
        :: OsPath -> IO (Either Text SubagentWorktree)
    , collaborationContext :: Maybe MultiAgentContext
    , collaborationCloseAgents :: IO ()
    }

newCollaborationRuntime
    :: AgentToolsRequest windowTitleResult
    -> ToolStartup
    -> ToolModelRuntime
    -> IO CollaborationRuntime
newCollaborationRuntime AgentToolsRequest
    { resumeLock
    , options
    , projectSettings
    , cwd
    , gatewayModelsRef
    , catalog
    , home
    , tokenProvider
    } ToolStartup
    { toolNativeCapabilities = nativeCapabilities
    , toolHarnessConfig = harnessConfig
    , toolGatewayAllowedChildModels = gatewayAllowedChildModels
    } ToolModelRuntime
    { toolProvider = provider
    , toolTransportModel = transportModel
    , toolInferredTarget = inferredTarget
    , toolDialectId = dialectId
    , toolLegacySubagentTarget = legacySubagentTarget
    , toolEffortText = effortText
    } = do
    -- Plan mode itself is process-local, while the assistant's proposed plan
    -- is durable in the session transcript. Reconstruct the approval phase
    -- before entering the REPL so a resumed Codex session cannot interpret
    -- the user's approval as ordinary steering input.
    -- Keep inferred startup, resume, and delegated-agent targets session-local.
    -- Live top-level model/provider switches persist their selection in
    -- Agent.CLI.Provider.Switch instead.
    collaborationActiveSessionLock <- newIORef resumeLock
    collaborationPersistSlotRef <- newIORef PersistenceDisabled
    -- Per-subagent transcripts / previous ids, shared across send_input / task.
    collaborationSubagentSessions <- newIORef Map.empty
    collaborationSubagentStoreRoot <- newIORef Nothing
    collaborationSubagentForkSource <-
        newIORef (Nothing :: Maybe (IO [ResponseItem]))
    collaborationPendingNotices <- newPendingInputs
    let maxConcurrentAgents =
            fromMaybe defaultMaxConcurrent $
                options.optMaxConcurrentAgents
                    <|> projectSettings.settingsMaxConcurrentAgents
                    <|> harnessConfig.configMaxConcurrentAgents
    collaborationRegistry <- newSubagentRegistry
        defaultSubagentConfig { maxConcurrent = maxConcurrentAgents }
        cwd
        (\_ _ _ _ -> pure $ Left LoopNoResponseId)
        (\_ _ -> pure ())
    collaborationRootTurnRef <- newIORef (Nothing :: Maybe RootTurnId)
    collaborationAgentTypes <- newIORef Map.empty
    collaborationOpenAiChild <-
        if not nativeCapabilities.nativeCollaboration
            then pure Nothing
            else case provider of
                XAIProvider -> do
                    available <- hasOpenAiAuth
                    if not available
                        then pure Nothing
                        else loadAuth (Just OpenAIProvider) >>= \case
                            Left _ -> pure Nothing
                            Right openaiLoaded ->
                                pure (Just openaiLoaded.loadedTokenProvider)
                _ -> pure Nothing
    let collaborationAllowedChildModels =
            case gatewayAllowedChildModels of
                Just modelIds -> Just modelIds
                Nothing -> case provider of
                    XAIProvider ->
                        Just
                            (grokRootChildModels
                                (isJust collaborationOpenAiChild))
                    _ -> Nothing
        collaborationChildModelAllowed
            | Just resolve <- collaborationGatewayChildModelOption =
                Just \modelId -> isJust <$> resolve modelId
            | otherwise = Nothing
        collaborationResolveChildModel
            | Just resolve <- collaborationGatewayChildModelOption =
                Just \modelId ->
                    fmap toCollaborationTarget <$> resolve modelId
            | otherwise = Nothing
        toCollaborationTarget option =
            let target = option.modelTarget
            in CollaborationModelTarget
                { collaborationTargetProvider = target.targetProvider
                , collaborationTargetConnection = target.targetConnectionId
                , collaborationTargetEffectiveModel =
                    target.targetWireModelId
                , collaborationTargetDialect = target.targetDialect
                }
        collaborationGatewayChildModelOption
            | isNothing gatewayAllowedChildModels = Nothing
            | otherwise =
                Just \requested ->
                    readIORef gatewayModelsRef >>= \case
                        Nothing -> pure Nothing
                        Just access ->
                            cachedGatewayModels access >>= \case
                                Nothing -> pure Nothing
                                Just models ->
                                    pure
                                        (resolveModelOptionById
                                            (modelOptionsForGatewayModels
                                                catalog models)
                                            (Text.strip requested))
        sendToRoot message = do
            enqueuePendingInput
                collaborationPendingNotices
                (AgentMessage message) >>= \case
                    Left err -> pure (Left err)
                    Right () -> pure (Right "queued")
        collaborationCreateWorktree source =
            createManagedWorktree home source >>= \case
                Left err -> pure (Left err)
                Right path -> pure $ Right SubagentWorktree
                    { subagentWorktreePath = path
                    , subagentWorktreeCleanup =
                        removeWorktree source path >>= \case
                            Left err -> pure (Left err)
                            Right () -> pure (Right ())
                    }
        collaborationContext
            | not nativeCapabilities.nativeCollaboration = Nothing
            | otherwise = Just MultiAgentContext
                { multiRegistry = collaborationRegistry
                , multiCwd = cwd
                , multiSelfId = Nothing
                , multiDepth = 0
                , multiTaskPath = taskPathRoot
                , multiRootTurnId = readIORef collaborationRootTurnRef
                , multiResumeFromDisk = Just
                    (restoreAgentFromDisk
                        provider
                        inferredTarget.targetConnectionId
                        transportModel
                        inferredTarget.targetWireModelId
                        dialectId
                        legacySubagentTarget
                        collaborationSubagentStoreRoot
                        collaborationRegistry
                        collaborationSubagentSessions
                        collaborationResolveChildModel
                        collaborationAgentTypes)
                , multiCreateWorktree = Just collaborationCreateWorktree
                , multiPrepareSpawn = Just
                    (prepareCollaborationSpawn
                        provider
                        inferredTarget.targetConnectionId
                        transportModel
                        inferredTarget.targetWireModelId
                        effortText
                        dialectId
                        legacySubagentTarget
                        collaborationSubagentSessions
                        collaborationSubagentStoreRoot
                        collaborationAgentTypes
                        collaborationSubagentForkSource)
                , multiSendToRoot = Just sendToRoot
                , multiSpawnModelGuidance =
                    if isJust gatewayAllowedChildModels
                        then Nothing
                        else
                            subscriptionSubagentModelGuidance
                                provider
                                (tokenProviderBillingMode tokenProvider)
                , multiAllowedChildModels =
                    collaborationAllowedChildModels
                , multiResolveChildModel =
                    collaborationResolveChildModel
                , multiChildModelAllowed =
                    collaborationChildModelAllowed
                }
        collaborationCloseAgents =
            case collaborationContext of
                Just ctx -> do
                    interruptActiveSubagents ctx.multiRegistry
                    flushAllSubagentSnapshots
                        collaborationSubagentStoreRoot
                        ctx.multiRegistry
                        collaborationSubagentSessions
                        collaborationAgentTypes
                    closeSubagentRegistry ctx.multiRegistry
                Nothing -> pure ()
    pure CollaborationRuntime{..}

installCollaborationCallbacks
    :: AgentToolsRequest windowTitleResult
    -> CollaborationRuntime
    -> IO ()
installCollaborationCallbacks AgentToolsRequest
    { startup
    } CollaborationRuntime
    { collaborationContext = multiCtx
    , collaborationPendingNotices = pendingNotices
    , collaborationSubagentSessions = subagentSessions
    , collaborationSubagentStoreRoot = subagentStoreRoot
    , collaborationAgentTypes = agentTypesRef
    } =
    case multiCtx of
        Just ctx -> do
            setSubagentOnComplete ctx.multiRegistry \agentId status -> do
                enqueuePendingNotice pendingNotices PendingSubagentNotice
                    (UserMessage (formatCompletionNotice agentId status))
                    >>= either (reportStartupWarning startup) pure
            setSubagentOnSettled ctx.multiRegistry \agentId status -> do
                sessions <- readIORef subagentSessions
                case Map.lookup agentId sessions of
                    Just session -> do
                        _ <-
                            persistAndEvictSubagentSessionWithStatus
                                subagentStoreRoot
                                ctx.multiRegistry
                                agentTypesRef
                                agentId
                                status
                                session
                        pure ()
                    Nothing -> pure ()
        Nothing -> pure ()
