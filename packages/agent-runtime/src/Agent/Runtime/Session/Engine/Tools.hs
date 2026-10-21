-- | Scoped tool composition for a headless session owner.
module Agent.Runtime.Session.Engine.Tools
    ( SessionCallbacks(..), defaultSessionCallbacks
    , EngineTools(..), McpSession(..), withEngineTools, createEngineWorktree
    ) where

import Agent.Accounts.Auth (LoadedAuth(..), isGatewayLoadedAuth, hasOpenAiAuth, loadAuth)
import Agent.ExternalSession (defaultExternalSessionEnv, externalSessionTool)
import Agent.ComputerUse qualified as ComputerUse
import Agent.Dialect (DialectId(..))
import Agent.Integration.API (IntegrationRuntime(..))
import Agent.Loop (TurnInput(..))
import Agent.Loop.InputQueue qualified as Queue
import Agent.MCP qualified as MCP
import Agent.OsPath (toText, unsafeToFilePath)
import Agent.Provider (Provider(..), TokenProvider)
import Agent.Responses.Types qualified
import Agent.ResourceScope (allocateAcquire, allocateResource)
import Agent.Runtime.Collaboration
import Agent.Runtime.AgentSessions (AgentSessionActions(..), AgentSessionToolsEnv(..), agentSessionTools)
import Agent.Runtime.Config (HarnessConfig(..))
import Agent.Runtime.Database (databaseTools)
import Agent.Runtime.Database.Store
import Agent.Runtime.FileUri (fileUri)
import Agent.Runtime.GatewayClient (gatewayCredentialIdentity)
import Agent.Runtime.Gateway.Catalog (GatewayModelAccess, newGatewayModelAccessWithStore)
import Agent.Runtime.Worktree (createManagedWorktree, removeWorktree, acquireWorktreeLease, releaseWorktreeLease, worktreeRoot)
import Agent.Runtime.Gateway.Models (loadGatewayModelOptionsWithCredentialAt)
import Agent.Runtime.Models (ModelOption(..))
import Agent.Runtime.Host
import Agent.Runtime.IntegrationGateway
import Agent.Runtime.Lsp (LspStartup(..), lspRuntimeTool)
import Agent.Runtime.Mcp.Startup
import Agent.Runtime.Models (ModelTarget(..))
import Agent.Runtime.Request
import Agent.Runtime.Session
import Agent.Runtime.Session.Engine.Context
import Agent.Runtime.Session.Preparation
import Agent.Runtime.Session.Resources
import Agent.Runtime.Session.Settings
import Agent.Runtime.Skills.Content (loadMcpSkillsCatalog)
import Agent.Runtime.Skills.LearnedSkills (learnedSkillTools)
import Agent.Runtime.Skills.LearnedSkills.Store (learnedSkillToolsEnvForStore)
import Agent.Runtime.Startup.Context (mergeSkillCatalogs)
import Agent.Runtime.Subagents.Runtime
import Agent.Runtime.Tools.Dialects
import Agent.Runtime.Tools.LocalStartup
import Agent.Runtime.Tools.Resources
import Agent.Runtime.Tools.Startup
import Agent.Runtime.Transcription (transcribeAudioFile)
import Agent.Runtime.WebFetch (webFetchToolGroup)
import Agent.Skills
import Agent.Store.Postgres (trustedPool)
import Agent.Subagents
import Agent.Tools.MultiAgents
import Agent.Tools.PlanMode (PlanModeEnv(..))
import Agent.Tools.Secret (SecretPromptHooks)
import Agent.Tools.ShowImage (ImageDisplayHooks)
import Agent.Tools.RenderChart (renderChartTool)
import Agent.OpenAI.ImageGeneration (imageGenerationTool)
import Agent.Runtime.ModelConfig (builtinConnectionId)
import Agent.Tools.Types
import Control.Concurrent.STM (atomically)
import Control.Exception.Safe (bracket, finally)
import Control.Monad (forM_, void)
import Data.Acquire (mkAcquire)
import Data.IORef
import Data.List (find)
import Data.Aeson qualified as Aeson
import Data.Aeson ((.=))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified
import System.Environment (lookupEnv)
import System.Info (os)
import System.OsPath (OsPath, unsafeEncodeUtf)

data SessionCallbacks = SessionCallbacks
    { sessionElicit :: !(Maybe (MCP.McpElicitRequest -> IO MCP.McpElicitResult))
    , sessionSecret :: !(Maybe SecretPromptHooks)
    , sessionImage :: !(Maybe ImageDisplayHooks)
    , sessionNotice :: !(Text -> IO ())
    , sessionAnswer :: !(Text -> IO ())
    , sessionMcpStatus :: !([MCP.McpServerStatus] -> IO ())
    , sessionAgents :: !(Maybe AgentSessionActions)
    , sessionCatalog :: !(Aeson.Value -> IO ())
    , sessionPreferences :: !(Aeson.Value -> IO ())
    }

defaultSessionCallbacks :: SessionCallbacks
defaultSessionCallbacks = SessionCallbacks Nothing Nothing Nothing
    (const (pure ())) (const (pure ())) (const (pure ())) Nothing (const (pure ())) (const (pure ()))

data McpSession = McpSession
    { sessionFleet :: !MCP.McpFleet
    , sessionSampling :: !(IORef (Maybe (MCP.McpSamplingRequest -> IO (Either Text MCP.McpSamplingResult))))
    , sessionReleaseMcp :: !(IO ())
    }

data EngineTools = EngineTools
    { engineToolEnv :: !ToolEnv
    , enginePersistence :: !Persistence
    , engineScratch :: !(SessionResources ())
    , engineCoding :: !CodingTools
    , engineMcp :: !McpSession
    , engineAllTools :: ![AppTool]
    , engineReadTools :: !(IO [AppTool])
    , engineSkillCatalog :: !SkillCatalog
    , engineRegistry :: !SubagentRegistry
    , engineRootSubagents :: !RootSubagents
    , engineMultiContext :: !(Maybe MultiAgentContext)
    , engineChildSessions :: !(IORef (Map.Map SubagentId SubagentSession))
    , engineChildStoreRoot :: !SubagentStoreRoot
    , engineChildForkSource :: !(IORef (Maybe (IO [Agent.Responses.Types.ResponseItem])))
    , engineParentEffort :: !(IORef (IO Text))
    , engineChildModels :: !CollaborationModels
    , engineOpenAiChild :: !(Maybe TokenProvider)
    , enginePendingInputs :: !Queue.PendingInputs
    , engineGatewayAccess :: !(Maybe GatewayModelAccess)
    }

withEngineTools :: EngineContext -> SessionCallbacks -> (EngineTools -> IO a) -> IO a
withEngineTools context@EngineContext{..} callbacks use =
  withSessionResourceScopes \scopes -> do
    baseEnv <- defaultToolEnv engineCwd
    engineGatewayAccess <- traverse (newGatewayModelAccessWithStore (trustedPool engineStore)) engineGateway
    let engineToolEnv = baseEnv
            { toolResourceArbiter = engineProcess.processToolResourceArbiter
            , toolTranscribeAudio = Just \name mime bytes ->
                transcribeAudioFile engineTarget.targetProvider engineGatewayAccess name mime bytes
            }
        capabilities = engineHooks.nativeCapabilities
        loadsWorkspace = nativeLoadsHostWorkspaceContext engineHooks.nativeWorkspaceDiscovery
    writeIORef engineToolEnv.toolRootAccessRequest (Just engineHooks.nativeRequestRootAccess)
    (_, engineScratch) <- allocateResource scopes.scratchResources
        (prepareSessionResources SessionResourcesRequest
            { resourcePersistenceRequest = PersistenceRequest
                { persistencePool = trustedPool engineStore
                , persistenceRoot = engineRoot
                , persistenceTarget = engineTarget
                , persistenceGatewayIdentity = gatewayCredentialIdentity <$> engineGateway
                , persistenceRetargetResumed = True
                , persistenceCwd = engineCwd
                , persistenceEffort = engineEffort
                , persistencePrompt = Just engineRequest.nativeTurnPrompt
                , persistenceResumed = fst <$> engineResumed
                , persistenceEnabled = True
                , persistenceHeadless = False
                }
            , resourceToolEnv = engineToolEnv
            , resourceResumedTurns = maybe [] snd engineResumed
            } SessionResourceHooks
                { resourcePersistenceReady = const (pure ())
                , resourceTaskPlanReady = const (pure ())
                , resourcePrepareHost = const (pure ())
                , resourceAcquireWorktreeLease =
                    if nativeAllowsWorkspaceCreation (Just engineHooks)
                    then acquireWorktreeLease (worktreeRoot engineHome) engineCwd >>= requireEngine
                        >>= pure . mapM_ releaseWorktreeLease
                    else pure (pure ())
                }) (.resourceCleanup)
    let enginePersistence = engineScratch.resourcePersistence
    identifier <- ensurePersistenceSessionId enginePersistence
    forM_ identifier engineHooks.nativeOnSessionId
    engineChildSessions <- newIORef Map.empty
    engineChildStoreRoot <- newIORef (identifier >>= either (const Nothing) Just . sessionDirForId engineRoot)
    engineChildForkSource <- newIORef Nothing
    engineParentEffort <- newIORef (pure engineEffort)
    enginePendingInputs <- Queue.newPendingInputs
    types <- newIORef Map.empty
    (_, engineRegistry) <- allocateAcquire scopes.activityResources
        (acquireCollaborationRegistry
            (resolveMaxConcurrentAgents engineSettings.sessionMaxConcurrentAgents Nothing
                engineHarness.configMaxConcurrentAgents) engineCwd
            (\registry -> flushAllSubagentSnapshots engineChildStoreRoot registry engineChildSessions types))
    engineRootSubagents <- newRootSubagents NoticesToLoop engineRegistry
    -- Child completion and messages are consumed by the root loop. They never
    -- open a terminal or create an independently owned foreground session.
    engineOpenAiChild <-
        if shouldLoadOpenAiChild capabilities.nativeCollaboration
            (map (.targetModelId) . map (.modelTarget) <$> engineGatewayModels) engineTarget.targetProvider
        then hasOpenAiAuth >>= \available -> if not available then pure Nothing
            else loadAuth (Just OpenAIProvider) >>= pure . either (const Nothing) (Just . (.loadedTokenProvider))
        else pure Nothing
    let engineChildModels = resolveCollaborationModels engineTarget.targetProvider
            (map (.targetModelId) . map (.modelTarget) <$> engineGatewayModels)
            (isJust engineOpenAiChild)
            (loadGatewayModelOptionsWithCredentialAt engineHome engineCwd engineGateway
                >>= pure . either (const Nothing) snd)
        sendToRoot message =
            Queue.enqueuePendingInput enginePendingInputs (AgentMessage message)
                >>= pure . fmap (const "queued")
        engineMultiContext
            | not capabilities.nativeCollaboration = Nothing
            | otherwise = Just (rootMultiAgentContext engineRootSubagents engineCwd)
                { multiSendToRoot = Just sendToRoot
                , multiPrepareSpawn = Just (prepareCollaborationSpawn
                    engineTarget.targetProvider engineTarget.targetConnectionId id
                    engineTarget.targetWireModelId (readIORef engineParentEffort >>= id) engineTarget.targetDialect
                    (sessionLegacySubagentTarget . fst <$> engineResumed)
                    engineChildSessions engineChildStoreRoot types engineChildForkSource)
                , multiResumeFromDisk = Just (restoreAgentFromDisk
                    engineTarget.targetProvider engineTarget.targetConnectionId id
                    engineTarget.targetWireModelId engineTarget.targetDialect
                    (sessionLegacySubagentTarget . fst <$> engineResumed)
                    engineChildStoreRoot engineRegistry engineChildSessions
                    engineChildModels.childResolveModel types)
                , multiAllowedChildModels = engineChildModels.childAllowedModels
                , multiResolveChildModel = engineChildModels.childResolveModel
                , multiChildModelAllowed = engineChildModels.childModelAllowed
                , multiCreateWorktree = if nativeAllowsWorkspaceCreation (Just engineHooks)
                    then Just (createEngineWorktree context) else Nothing
                }
        settings = LocalToolSettings
            { localHostExtensions = capabilities.nativeHostExtensions
            , localHostWebFetch = capabilities.nativeHostWebFetch
            , localDialectId = engineTarget.targetDialect
            , localProvider = engineTarget.targetProvider
            , localPlatform = fromString os
            , localComputerUseEnabled = engineSettings.sessionComputerUse
            }
        local = localToolAcquisitions settings engineHarness.configWebFetch engineHarness.configLsp
            engineToolEnv (\err -> callbacks.sessionNotice err >> pure Nothing)
    acquired <- acquireToolStartup scopes ToolAcquisitions
        { acquireMcp = mkAcquire (acquireEngineMcp context callbacks engineScratch.resourceSessionTmp)
            (.sessionReleaseMcp)
        , acquireCoding = mkAcquire
            (codingToolsForWithTypes engineDialect engineToolEnv
                (Just engineHooks.nativePlanHooks) (Just engineScratch.resourceTaskPlan)
                callbacks.sessionSecret callbacks.sessionImage engineMultiContext types)
            (.codingClose)
        , acquireWebFetch = local.localAcquireWebFetch
        , acquireLsp = local.localAcquireLsp
        , acquireComputerUse = local.localAcquireComputerUse
        , preloadContext = pure ()
        }
    let engineCoding = acquired.startupLocalTools
        engineMcp = acquired.startupMcp
    writeIORef engineCoding.codingPlanMode.planSessionDir =<< readIORef engineChildStoreRoot
    forM_ acquired.startupLsp.lspStartupWarnings callbacks.sessionNotice
    engineSkillCatalog <- discoverEngineSkills context engineMcp.sessionFleet
    external <- if capabilities.nativeHostExtensions
        then do
            environment <- defaultExternalSessionEnv engineToolEnv (unsafeToFilePath engineCwd)
                (unsafeToFilePath engineScratch.resourceSessionTmp) (unsafeToFilePath engineHome)
            pure [externalSessionTool environment]
        else pure []
    setToolSkillRoots engineToolEnv
        [ directory
        | skill <- engineSkillCatalog.catalogSkills
        , FilesystemSkillSource{skillDirectory = directory} <- [skill.skillSource]
        ]
    scopesForData <- deriveDatabaseScopesWithNamespace engineHooks.nativeDatabaseScopeNamespace
        (unsafeToFilePath engineHome) (unsafeToFilePath engineProjectRoot) >>= requireEngine
    invocations <- newIORef []
    discovery <- MCP.newMcpToolDiscovery
    let artifactDir = Just (unsafeToFilePath engineScratch.resourceSessionTmp)
        readMcp = if engineTarget.targetDialect == CodexDialect
            then MCP.mcpFleetCodexToolsForArtifactDirectory artifactDir engineMcp.sessionFleet discovery
            else pure $ if engineTarget.targetDialect == GrokBuildDialect
                then MCP.mcpFleetGrokMetaToolsForArtifactDirectory artifactDir engineMcp.sessionFleet
                else MCP.mcpFleetMetaToolsForArtifactDirectory artifactDir engineMcp.sessionFleet
        nativeGroups = engineHooks.nativeToolGroups
        independent = case callbacks.sessionAgents of
            Just actions | capabilities.nativeCollaboration && capabilities.nativeHostExtensions ->
                agentSessionTools AgentSessionToolsEnv
                    { toolsPool = trustedPool engineStore, toolsRoot = engineRoot
                    , toolsProvider = engineTarget.targetProvider
                    , toolsConnection = engineTarget.targetConnectionId
                    , toolsModel = engineTarget.targetModelId
                    , toolsTransportModel = engineTarget.targetWireModelId
                    , toolsDialect = engineTarget.targetDialect
                    , toolsAllowedModels = engineChildModels.childAllowedModels
                    , toolsResolveModelOption = case engineGatewayModels of
                        Nothing -> Nothing
                        Just _ -> Just \name ->
                            loadGatewayModelOptionsWithCredentialAt engineHome engineCwd engineGateway
                                >>= pure . either (const Nothing)
                                    (\(_, models) -> models >>= find ((== name) . (.targetModelId) . (.modelTarget)))
                    , toolsGatewayIdentity = gatewayCredentialIdentity <$> engineGateway
                    , toolsCwd = engineCwd, toolsEffort = engineEffort
                    , toolsCurrentSessionId = pure identifier
                    , toolsLaunchTurn = actions.agentLaunch engineSettings
                    , toolsSessionStatus = actions.agentStatus
                    , toolsPrepareSessionWait = actions.agentPrepareWait
                    , toolsDeliverToOwner = actions.agentDeliver }
            _ -> []
        database = databaseTools $ databaseToolsEnvForStore engineStore scopesForData
            (pure identifier) (gatewayCredentialIdentity <$> engineGateway)
            engineHooks.nativeExposeHarnessCatalog identifier
        learned = learnedSkillTools invocations (Just engineMcp.sessionFleet)
            (learnedSkillToolsEnvForStore engineStore scopesForData (pure identifier))
        groups mcp = map filterExecution engineCoding.codingAppToolGroups
            <> [ HostToolGroup (mcp <> MCP.mcpFleetResourceTools engineMcp.sessionFleet)
               , HostToolGroup database, HostToolGroup learned, HostToolGroup independent, HostToolGroup external
               , HostToolGroup [renderChartTool | capabilities.nativeHostExtensions]
               , HostToolGroup
                    [ imageGenerationTool engineLoaded.loadedTokenProvider engineToolEnv
                        engineScratch.resourceImageGenerationHistory callbacks.sessionImage
                    | engineTarget.targetProvider == OpenAIProvider
                    , engineTarget.targetDialect == CodexDialect, capabilities.nativeHostExtensions
                    , not (isGatewayLoadedAuth engineLoaded)
                    , engineTarget.targetConnectionId == builtinConnectionId OpenAIProvider ] ]
            <> nativeGroups
            <> [ webFetchToolGroup acquired.startupWebFetch
               , ExecutionToolGroup (maybe [] (pure . lspRuntimeTool) acquired.startupLsp.lspStartupRuntime)
               , ExecutionToolGroup
                    [ComputerUse.computerUseRuntimeTool computer
                    | computer <- maybe [] pure acquired.startupComputerUse
                    , (resolveLocalToolPolicy settings).localComputerToolExposed]
               ]
        filterExecution = \case
            ExecutionToolGroup tools -> ExecutionToolGroup
                (filterGhciTools (engineRequest.nativeTurnShellMode `elem` [NativeShellGhci, NativeShellBoth])
                    (filterBashTools (engineRequest.nativeTurnShellMode `elem` [NativeShellBash, NativeShellBoth]) tools))
            group -> group
        engineReadTools = engineHooks.nativeComposeTools . groups <$> readMcp
    engineAllTools <- engineReadTools
    callbacks.sessionCatalog (Aeson.object
        [ "tools" .= map (.appToolName) engineAllTools
        , "skills" .= [Aeson.object
            ["name" .= skill.skillName, "summary" .= skill.skillDescription
            , "argumentHint" .= skill.skillArgumentHint, "source" .= skill.skillName]
            | skill <- engineSkillCatalog.catalogSkills] ])
    use EngineTools{..}
  where
    fromString = Data.Text.pack

createEngineWorktree :: EngineContext -> OsPath -> IO (Either Text SubagentWorktree)
createEngineWorktree context source = createManagedWorktree context.engineHome source >>= \case
    Left err -> pure (Left err)
    Right path -> pure $ Right SubagentWorktree
        { subagentWorktreePath = path, subagentWorktreeCleanup = removeWorktree source path }

acquireEngineMcp :: EngineContext -> SessionCallbacks -> OsPath -> IO McpSession
acquireEngineMcp EngineContext{..} callbacks temp = do
    let capabilities = engineHooks.nativeCapabilities
        (configured, progressive) = resolveMcpConfiguration McpConfigurationRequest
            { mcpConfigCwd = engineCwd, mcpConfigHome = engineHome, mcpConfigHarness = engineHarness
            , mcpConfigToolsEnabled = capabilities.nativeMcpTools
            , mcpConfigHostExtensions = capabilities.nativeHostExtensions
            , mcpConfigOneShot = engineSettings.sessionOneShot
            }
    integration <- prepareSessionIntegrationRuntime capabilities.nativeGatewayIntegrations
        engineGateway engineProcess.processIntegrationSupervisor >>= \case
            Left err -> callbacks.sessionNotice err >> pure Nothing
            Right runtime -> pure runtime
    let (remote, inMemory) = maybe ([], [])
            (integrationEndpointServers (map (.mcpServerName) configured) . integrationRuntimeEndpoint)
            integration
        request = McpStartupRequest (configured <> remote)
            [(integrationsMcpConfig name, server) | (name, server) <- inMemory] progressive
        samplingEnabled = any (.mcpServerSamplingEnabled) (startupServerConfigs request)
    sessionSampling <- newIORef Nothing
    let hooks = MCP.McpSessionHooks
            { MCP.mcpSessionIdentity = toText temp
            , MCP.mcpSessionElicit = callbacks.sessionElicit
            , MCP.mcpSessionRoots = Just (const (pure [MCP.McpRoot (fileUri (unsafeToFilePath engineCwd)) Nothing]))
            , MCP.mcpSessionSample = if samplingEnabled
                then Just \value -> readIORef sessionSampling >>= maybe
                    (pure (Left "MCP sampling is unavailable until the provider is ready")) ($ value)
                else Nothing
            }
    acquireMcpSessionStartup engineProcess.processMcpSupervisor hooks request callbacks.sessionMcpStatus
        \fleet sessionReleaseMcp -> pure McpSession
            { sessionFleet = MCP.withMcpFleetCallContext engineHooks.nativeMcpCallContext fleet
            , sessionSampling, sessionReleaseMcp }

discoverEngineSkills :: EngineContext -> MCP.McpFleet -> IO SkillCatalog
discoverEngineSkills EngineContext{..} fleet = do
    let loadsWorkspace = nativeLoadsHostWorkspaceContext engineHooks.nativeWorkspaceDiscovery
    builtin <- lookupEnv "HASKELL_AGENT_SKILLS_DIR"
    local <- if engineSettings.sessionSkills && loadsWorkspace
        then discoverSkills SkillDiscoverOptions
            { skillsHome = engineHome, skillsProjectRoot = engineProjectRoot, skillsCwd = engineCwd
            , skillsMaxDepth = 6, skillsBuiltinRoots =
                [(AgentSkills, unsafeEncodeUtf path) | path <- maybe [] pure builtin]
            }
        else pure (SkillCatalog [] [])
    operator <- discoverBuiltinSkills 6 [(AgentSkills, root) | root <- engineHooks.nativeOperatorSkillRoots]
    remote <- if engineSettings.sessionSkills then loadMcpSkillsCatalog fleet else pure (SkillCatalog [] [])
    pure (mergeSkillCatalogs operator (mergeSkillCatalogs local remote))
