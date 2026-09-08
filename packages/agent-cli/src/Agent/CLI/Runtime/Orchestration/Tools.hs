module Agent.CLI.Runtime.Orchestration.Tools
    ( AgentToolsRequest(..)
    , runAgentTools
    ) where

import Agent.CLI.Runtime.Orchestration.Tools.Request
import Agent.CLI.Runtime.Orchestration.Tools.Model
import Agent.CLI.Runtime.Orchestration.Tools.Collaboration
import Agent.CLI.Runtime.Orchestration.Tools.Scratch
import Agent.CLI.Runtime.Orchestration.Tools.Mcp
import Agent.CLI.Runtime.Orchestration.Tools.HostHooks
import Agent.CLI.AgentSessions
    ( agentSessionTools,
      launchSessionThreadNotifying,
      formatSessionCompletionNotice,
      sessionThreadStatus,
      prepareSessionThreadWait,
      AgentSessionToolsEnv(toolsSessionStatus, AgentSessionToolsEnv,
                           toolsPool, toolsRoot, toolsProvider, toolsConnection, toolsModel,
                           toolsTransportModel, toolsDialect, toolsAllowedModels,
                           toolsResolveModelOption,
                           toolsGatewayIdentity, toolsCwd, toolsEffort,
                           toolsCurrentSessionId, toolsLaunchTurn, toolsPrepareSessionWait) )
import Agent.CLI.Auth (isGatewayLoadedAuth)
import qualified Agent.CLI.ComputerUse as ComputerUse
import Agent.CLI.Config (HarnessConfig(..))
import Agent.CLI.Database ( databaseTools )
import Agent.CLI.Database.Store
    (databaseToolsEnvForStore)
import Agent.CLI.Dialects
    ( CodingTools(..),
      codingToolsForWithTypes,
      filterBashTools,
      filterGhciTools )
import Agent.CLI.Error ( formatException )
import Agent.CLI.GatewayBridge ( managedGatewayTools )
import Agent.CLI.LearnedSkills ( learnedSkillTools )
import Agent.CLI.LearnedSkills.Store
    ( learnedSkillToolsEnvForStore
    , loadApplicableLearnedSkillsForStore
    , successfulLearnedSkillsPreload
    )
import Agent.CLI.Lsp
    ( LspStartup(..), closeLspRuntime, lspRuntimeTool, newLspRuntime )
import Agent.CLI.IntegrationGateway (gatewayIntegrationAuthority)
import Agent.Integration.API
    ( IntegrationRuntime
    , acquireIntegrationRuntime
    )
import Agent.OsPath (unsafeToFilePath)
import Agent.CLI.ModelConfig (builtinConnectionId)
import Agent.CLI.Models (ModelTarget(targetConnectionId, targetWireModelId))
import Agent.CLI.Options
    ( isOneShot, resolveComputerUseEnabled
    , CliOptions(optGhci, optBash, optSkills)
    )
import Agent.CLI.Plan (resumedPlanNeedsApproval)
import Agent.CLI.Runtime.Orchestration.Background
    ( runInProcessSessionTurn )
import Agent.CLI.Runtime.Orchestration.Types
    ( AgentProcessRuntime(..)
    , NativeDiscoveryContext(..)
    , NativeInteractionMode(NativePlan)
    , NativeRunCapabilities(..)
    , NativeRunHooks(..)
    , nativeLoadsHostWorkspaceContext
    , nativePreparedDiscovery
    )
import Agent.CLI.Resume
    ( SessionInitialContext
        ( initialContextMayRestoreSnapshot
        , initialContextNeeded
        )
    , resolveSessionInitialContext
    )
import Agent.CLI.Session.Workspace (WorkspaceContext(..))
import Agent.CLI.Runtime.Orchestration.Session ( AgentSessionRequest(..)
    , runAgentSession
    )
import Agent.CLI.Runtime.Orchestration.Startup
    ( reportStartupWarning )
import Agent.CLI.Runtime.Types (RunResult)
import Agent.CLI.Session
    ( SessionHandle(sessionDir, sessionMeta), SessionMeta(metaId)
    , SessionTurn(turnAssistantText) )
import Agent.CLI.Session.Runtime.Types
    ( InitialContextPreload(..)
    , StartupRuntime(startupDatabaseStore, startupNativeHooks, startupStdinTty) )
import Agent.CLI.Session.Selection
    ( currentSessionId, reservedSessionId )
import Agent.CLI.SessionLock
    ( acquireSessionLock,
      releaseSessionLock,
      sessionLockFilePath,
      sessionLockPath )
import Agent.CLI.Startup.Auth (startupDie)
import Agent.CLI.StartupContext ( preloadAgentsContext )
import Agent.CLI.Skills (loadMcpSkillsCatalog, mergeSkillCatalogs)
import Agent.CLI.WebFetch
    ( WebFetchRuntime
    , closeWebFetchRuntime
    , newWebFetchRuntime
    , webFetchRuntimeTool
    )
import Agent.Dialect (DialectId(CodexDialect, GrokBuildDialect))
import Agent.OpenAI.ImageGeneration
    ( clearImageGenerationHistory
    , imageGenerationTool
    , recordImageGenerationImages
    )
import Agent.Provider (Provider(OpenAIProvider))
import Agent.ResourceScope
    ( allocateResource
    , allocateFourResourcesConcurrently
    , logSlowCleanup
    , releaseResource
    , withResourceScope
    )
import Agent.Skills
    ( SkillCatalog(..)
    , SkillInvocation
    )
import Agent.Store.Postgres ( trustedPool )
import Agent.Tools.PlanMode
    ( PlanModeEnv(planSessionDir, planStateRef),
      activatePlanMode,
      PlanModeState(PlanPending) )
import Agent.Tools.Types
    ( AppTool
    , AppToolGroup(..)
    , BackgroundTaskHooks(..)
    , BackgroundTaskNotice(..)
    , ToolEnv(..)
    , appToolsFromGroups
    )
import Control.Concurrent.Async ( concurrently, concurrently_ )
import Control.Exception.Safe
    ( SomeException, bracketOnError, finally, mask_, throwIO, try )
import Control.Monad ( forM_, join, when, void )
import Data.IORef
    (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (isJust)
import Data.Unique (newUnique, hashUnique)
import System.Info (os)
import System.OsPath (OsPath)
import qualified Agent.MCP as MCP
    ( mcpFleetGrokMetaToolsForArtifactDirectory,
      mcpFleetMetaToolsForArtifactDirectory,
      mcpFleetRegistrationsForArtifactDirectory,
      mcpFleetResourceTools,
      mcpFleetToolsForArtifactDirectory )
import qualified Data.Text as Text (unpack, pack)

data LocalToolRuntime = LocalToolRuntime
    { localCoding :: CodingTools
    , localInitialSkills :: SkillCatalog
    }

data CodingRuntime = CodingRuntime
    { runtimeCoding :: CodingTools
    , runtimeExtraTools :: [AppTool]
    , runtimeComputerUse :: Maybe ComputerUse.ComputerUseRuntime
    , runtimeCloseExtraTools :: IO ()
    }

data SessionControlRuntime = SessionControlRuntime
    { controlGhciEnabledRef :: IORef Bool
    , controlBashEnabledRef :: IORef Bool
    , controlSkillsRef :: IORef SkillCatalog
    , controlSkillInvocationsRef :: IORef [SkillInvocation]
    , controlCodeModeCloseRef :: IORef (IO ())
    , controlClaimCurrentSession :: SessionHandle -> IO ()
    , controlSessionTools :: [AppTool]
    }

data SessionToolsRuntime = SessionToolsRuntime
    { sessionAllTools :: [AppTool]
    , sessionTools :: [AppTool]
    , sessionMcpTools :: [AppTool]
    , sessionDatabaseTools :: [AppTool]
    , sessionLearnedSkillTools :: [AppTool]
    , sessionGatewayTools :: [AppTool]
    , sessionPlanMode :: PlanModeEnv
    , sessionNoteDirectory :: OsPath -> IO ()
    , sessionCloseAll :: IO ()
    , sessionResumedPlanPending :: Bool
    }

runAgentTools
    :: AgentToolsRequest windowTitleResult
    -> IO RunResult
runAgentTools request = withResourceScope \resourceScope -> do
    toolStartup <- loadToolStartup request
    let toolModelRuntime = resolveToolModel request toolStartup
        toolHostHooks =
            buildToolHostHooks request toolStartup toolModelRuntime
    collaborationRuntime <-
        newCollaborationRuntime request toolStartup toolModelRuntime
    (scratchKey, acquiredScratchRuntime) <-
        allocateResource
            resourceScope
            (prepareScratchRuntime
                request
                toolStartup
                toolModelRuntime
                collaborationRuntime)
            (logSlowCleanup "session temporary resources" . (.scratchCleanup))
    let scratchRuntime =
            acquiredScratchRuntime
                { scratchCleanup = releaseResource scratchKey }
    integrationRuntime <- acquireSessionIntegrationRuntime request
    ( (acquiredResources, (computerUseKey, runtimeComputerUse))
      , (initialContext, initialContextPreload)
      ) <-
        concurrently
            ( concurrently
                ( allocateFourResourcesConcurrently
                    resourceScope
                    (acquireMcpRuntime
                        request
                        toolStartup
                        toolModelRuntime
                        collaborationRuntime
                        scratchRuntime
                        integrationRuntime)
                    (logSlowCleanup "session MCP resources" . (.runtimeCloseMcp))
                    (acquireLocalToolRuntime
                        request
                        toolModelRuntime
                        toolHostHooks
                        collaborationRuntime
                        scratchRuntime)
                    (logSlowCleanup "local coding tools" . (.localCoding.codingClose))
                    (acquireWebFetchRuntime
                        request
                        toolStartup
                        toolModelRuntime)
                    (logSlowCleanup "web fetch runtime" . mapM_ closeWebFetchRuntime)
                    (acquireLspStartup
                        request
                        toolStartup
                        toolModelRuntime)
                    (logSlowCleanup "language server runtime"
                        . mapM_ closeLspRuntime . (.lspStartupRuntime))
                )
                ( allocateResource
                    resourceScope
                    (acquireComputerUseRuntime toolModelRuntime)
                    (logSlowCleanup "computer use runtime"
                        . mapM_ ComputerUse.closeComputerUseRuntime)
                )
            )
            (prepareInitialContextPreload request toolModelRuntime)
    let ( (mcpKey, acquiredMcpRuntime)
          , (localToolKey, acquiredLocalToolRuntime)
          , (webFetchKey, webFetchRuntime)
          , (lspKey, lspStartup)
          ) = acquiredResources
        mcpRuntime =
            acquiredMcpRuntime
                { runtimeCloseMcp =
                    releaseResource mcpKey
                }
        localToolRuntime =
            acquiredLocalToolRuntime
                { localCoding =
                    acquiredLocalToolRuntime.localCoding
                        { codingClose = releaseResource localToolKey }
                }
        lspRuntime = lspStartup.lspStartupRuntime
        runtimeCoding = localToolRuntime.localCoding
        runtimeExtraTools =
            maybe [] (pure . webFetchRuntimeTool) webFetchRuntime
                <> maybe [] (pure . lspRuntimeTool) lspRuntime
        runtimeCloseExtraTools =
            releaseResource computerUseKey
                `finally` concurrently_
                    (releaseResource lspKey)
                    (releaseResource webFetchKey)
        codingRuntime = CodingRuntime{..}
    mapM_
        (reportStartupWarning request.startup)
        lspStartup.lspStartupWarnings
    installCollaborationCallbacks request collaborationRuntime
    remoteInitialSkills <-
        if request.options.optSkills
            then loadMcpSkillsCatalog mcpRuntime.runtimeMcpFleet
            else pure (SkillCatalog [] [])
    let initialSkills =
            mergeSkillCatalogs
                localToolRuntime.localInitialSkills
                remoteInitialSkills
    sessionControlRuntime <-
        newSessionControlRuntime
            request
            toolStartup
            toolModelRuntime
            collaborationRuntime
            initialSkills
    sessionToolsRuntime <-
        assembleSessionToolsRuntime
            request
            toolStartup
            toolModelRuntime
            toolHostHooks
            collaborationRuntime
            scratchRuntime
            mcpRuntime
            codingRuntime
            sessionControlRuntime
    launchAgentToolsSession
        request
        toolStartup
        toolModelRuntime
        toolHostHooks
        collaborationRuntime
        scratchRuntime
        mcpRuntime
        codingRuntime
        initialContext
        initialContextPreload
        sessionControlRuntime
        sessionToolsRuntime

-- The connected gateway is authoritative: an unavailable organization
-- integration does not fall back to local account data.
acquireSessionIntegrationRuntime
    :: AgentToolsRequest windowTitleResult
    -> IO (Maybe IntegrationRuntime)
acquireSessionIntegrationRuntime request =
    acquireIntegrationRuntime
        request.processRuntime.processIntegrationSupervisor
        authority >>= \case
        Left err -> do
            reportStartupWarning request.startup err
            pure Nothing
        Right runtime -> pure (Just runtime)
  where
    authority = gatewayIntegrationAuthority request.connectedGateway

acquireLocalToolRuntime
    :: AgentToolsRequest windowTitleResult
    -> ToolModelRuntime
    -> ToolHostHooks
    -> CollaborationRuntime
    -> ScratchRuntime
    -> IO LocalToolRuntime
acquireLocalToolRuntime AgentToolsRequest
    { startup
    , baseToolEnv
    , initialSkills
    } ToolModelRuntime
    { toolDialect = dialect
    } ToolHostHooks
    { toolPlanHooks = planHooks
    , toolSecretHooks = secretHooks
    , toolImageHooks = imageHooks
    } CollaborationRuntime
    { collaborationContext = multiCtx
    , collaborationAgentTypes = agentTypesRef
    } ScratchRuntime
    { scratchTaskPlan = taskPlan
    } = do
    let preparedDiscovery =
            startup.startupNativeHooks
                >>= nativePreparedDiscovery . (.nativeWorkspaceDiscovery)
        codingToolEnv =
            case preparedDiscovery of
                Just context ->
                    baseToolEnv
                        { toolCwd = context.nativeDiscoveryProjectRoot }
                Nothing -> baseToolEnv
    bracketOnError
        (codingToolsForWithTypes
            dialect
            codingToolEnv
            (Just planHooks)
            (Just taskPlan)
            secretHooks
            imageHooks
            multiCtx
            agentTypesRef)
        (.codingClose)
        \localCoding -> do
            let localInitialSkills = initialSkills
            pure LocalToolRuntime{..}

acquireWebFetchRuntime
    :: AgentToolsRequest windowTitleResult
    -> ToolStartup
    -> ToolModelRuntime
    -> IO (Maybe WebFetchRuntime)
acquireWebFetchRuntime AgentToolsRequest
    { startup
    , baseToolEnv
    } ToolStartup
    { toolNativeCapabilities = nativeCapabilities
    , toolHarnessConfig = harnessConfig
    } ToolModelRuntime
    { toolDialectId = dialectId
    }
    | not nativeCapabilities.nativeHostExtensions
        || dialectId /= GrokBuildDialect =
        pure Nothing
    | otherwise =
        newWebFetchRuntime
            harnessConfig.configWebFetch
            baseToolEnv >>= \case
                Left err ->
                    startupDie startup
                        ("Failed to initialize web_fetch: " <> err)
                Right runtime -> pure runtime

acquireLspStartup
    :: AgentToolsRequest windowTitleResult
    -> ToolStartup
    -> ToolModelRuntime
    -> IO LspStartup
acquireLspStartup AgentToolsRequest
    { baseToolEnv
    } ToolStartup
    { toolNativeCapabilities = nativeCapabilities
    , toolHarnessConfig = harnessConfig
    } ToolModelRuntime
    { toolDialectId = dialectId
    }
    | not nativeCapabilities.nativeHostExtensions
        || dialectId /= GrokBuildDialect =
        pure LspStartup
            { lspStartupRuntime = Nothing
            , lspStartupWarnings = []
            }
    | otherwise =
        newLspRuntime harnessConfig.configLsp baseToolEnv

acquireComputerUseRuntime
    :: ToolModelRuntime
    -> IO (Maybe ComputerUse.ComputerUseRuntime)
acquireComputerUseRuntime ToolModelRuntime
    { toolProvider = provider
    }
    | provider == OpenAIProvider
        && os `elem` ["darwin", "linux"] =
        Just <$> ComputerUse.newComputerUseRuntime
    | otherwise = pure Nothing

prepareInitialContextPreload
    :: AgentToolsRequest windowTitleResult
    -> ToolModelRuntime
    -> IO (SessionInitialContext, InitialContextPreload)
prepareInitialContextPreload AgentToolsRequest
    { options
    , startup
    , databaseScopes
    , home
    , cwd
    , resumed
    , transition
    } ToolModelRuntime
    { toolDialect = dialect
    , toolResumeTargetChanged = resumeTargetChanged
    , toolRefreshDialectContext = refreshDialectContext
    } = do
    (preloadedAgentsContext, preloadedLearnedSkills) <-
        concurrently preloadAgents preloadLearnedSkills
    pure (contextRequirements, InitialContextPreload{..})
  where
    contextRequirements =
        resolveSessionInitialContext
            (isJust transition)
            resumeTargetChanged
            resumed
    loadsHostWorkspaceContext =
        maybe
            True
            ( nativeLoadsHostWorkspaceContext
                . (.nativeWorkspaceDiscovery)
            )
            startup.startupNativeHooks
    preloadAgents
        | loadsHostWorkspaceContext
            && ( contextRequirements.initialContextNeeded
                || refreshDialectContext
               ) =
            if refreshDialectContext
                || not contextRequirements.initialContextMayRestoreSnapshot
                then preloadAgentsContext options dialect home cwd
                else pure Nothing
        | otherwise = pure Nothing
    preloadLearnedSkills
        | contextRequirements.initialContextNeeded =
            successfulLearnedSkillsPreload
                <$> loadApplicableLearnedSkillsForStore
                    startup.startupDatabaseStore
                    databaseScopes
        | otherwise = pure Nothing

newSessionControlRuntime
    :: AgentToolsRequest windowTitleResult
    -> ToolStartup
    -> ToolModelRuntime
    -> CollaborationRuntime
    -> SkillCatalog
    -> IO SessionControlRuntime
newSessionControlRuntime AgentToolsRequest
    { options
    , baseToolEnv
    , startup
    , root
    , gatewayIdentity
    , cwd
    , runAgentChild
    , processRuntime
    } ToolStartup
    { toolNativeCapabilities = nativeCapabilities
    , toolGatewayAllowedChildModels = gatewayAllowedChildModels
    } ToolModelRuntime
    { toolProvider = provider
    , toolInferredTarget = inferredTarget
    , toolModel = model
    , toolDialectId = dialectId
    , toolEffortText = effortText
    , toolPolicy = policy
    } CollaborationRuntime
    { collaborationActiveSessionLock = activeSessionLock
    , collaborationPersistSlotRef = persistSlotRef
    , collaborationGatewayChildModelOption = gatewayChildModelOption
    } initialSkills = do
    controlGhciEnabledRef <- newIORef options.optGhci
    controlBashEnabledRef <- newIORef options.optBash
    controlSkillsRef <- newIORef initialSkills
    controlSkillInvocationsRef <- newIORef []
    controlCodeModeCloseRef <- newIORef (pure ())
    let controlClaimCurrentSession handle = mask_ do
            let desired = sessionLockPath handle.sessionDir
            readIORef activeSessionLock >>= \case
                Just current
                    | sessionLockFilePath current == desired -> pure ()
                previous ->
                    acquireSessionLock
                        handle.sessionDir
                        handle.sessionMeta.metaId >>= \case
                            Left err ->
                                throwIO (userError (Text.unpack err))
                            Right lock -> do
                                writeIORef activeSessionLock (Just lock)
                                mapM_ releaseSessionLock previous
        sessionToolsEnv = AgentSessionToolsEnv
            { toolsPool = trustedPool startup.startupDatabaseStore
            , toolsRoot = root
            , toolsProvider = provider
            , toolsConnection = inferredTarget.targetConnectionId
            , toolsModel = model
            , toolsTransportModel = inferredTarget.targetWireModelId
            , toolsDialect = dialectId
            , toolsAllowedModels = gatewayAllowedChildModels
            , toolsResolveModelOption = gatewayChildModelOption
            , toolsGatewayIdentity = gatewayIdentity
            , toolsCwd = cwd
            , toolsEffort = effortText
            , toolsCurrentSessionId =
                readIORef persistSlotRef >>= currentSessionId
            , toolsLaunchTurn = \handle message -> do
                ghciEnabled <- readIORef controlGhciEnabledRef
                bashEnabled <- readIORef controlBashEnabledRef
                let action =
                        runInProcessSessionTurn
                            runAgentChild
                            options
                            policy
                            ghciEnabled
                            bashEnabled
                            handle
                            message
                if isOneShot options
                    then
                        try @_ @SomeException action >>= \case
                            Left err -> pure (Left (formatException err))
                            Right (Left err) -> pure (Left err)
                            Right (Right ()) ->
                                pure
                                    (Right
                                        ("completed session "
                                            <> handle.sessionMeta.metaId))
                    else do
                        -- Capture the launching conversation's sink, rather
                        -- than looking up whichever session is open later.
                        hooks <- readIORef baseToolEnv.toolBackgroundTaskHooks
                        turnKey <- Text.pack . show . hashUnique <$> newUnique
                        launchSessionThreadNotifying
                            processRuntime.processSessionThreads
                            handle.sessionMeta.metaId
                            (\status -> void $ hooks.backgroundTaskCompleted
                                BackgroundTaskNotice
                                    { noticeKey = "agent-session:" <> turnKey
                                    , noticeBody = formatSessionCompletionNotice
                                        handle.sessionMeta.metaId status
                                    })
                            action
            , toolsSessionStatus =
                sessionThreadStatus processRuntime.processSessionThreads
            , toolsPrepareSessionWait =
                prepareSessionThreadWait processRuntime.processSessionThreads
            }
        -- Persisted agent-session tools recursively start another native
        -- runtime, so they require an explicit collaboration capability from
        -- the embedding.
        controlSessionTools
            | not nativeCapabilities.nativeCollaboration = []
            | otherwise = agentSessionTools sessionToolsEnv
    pure SessionControlRuntime{..}

assembleSessionToolsRuntime
    :: AgentToolsRequest windowTitleResult
    -> ToolStartup
    -> ToolModelRuntime
    -> ToolHostHooks
    -> CollaborationRuntime
    -> ScratchRuntime
    -> McpRuntime
    -> CodingRuntime
    -> SessionControlRuntime
    -> IO SessionToolsRuntime
assembleSessionToolsRuntime AgentToolsRequest
    { startup
    , databaseScopes
    , gatewayIdentity
    , options
    , loaded
    , tokenProvider
    , baseToolEnv
    , resumed
    } ToolStartup
    { toolNativeCapabilities = nativeCapabilities
    } ToolModelRuntime
    { toolProvider = provider
    , toolDialectId = dialectId
    , toolInferredTarget = inferredTarget
    } ToolHostHooks
    { toolImageHooks = imageHooks
    } CollaborationRuntime
    { collaborationPersistSlotRef = persistSlotRef
    , collaborationSubagentStoreRoot = subagentStoreRoot
    , collaborationActiveSessionLock = activeSessionLock
    , collaborationCloseAgents = closeAgents
    } ScratchRuntime
    { scratchPromptRequest = promptRequest
    , scratchImageGenerationHistory = imageGenerationHistory
    , scratchExternalSessionTools = externalSessionAppTools
    , scratchSessionTmp = sessionTmp
    , scratchCleanup = cleanupScratch
    } McpRuntime
    { runtimeMcpServerConfigs = mcpServerConfigs
    , runtimeProgressiveMcp = progressiveMcp
    , runtimeMcpFleet = mcpFleet
    , runtimeCloseMcp = closeMcp
    } CodingRuntime
    { runtimeCoding = coding
    , runtimeExtraTools = extraTools
    , runtimeComputerUse = computerUseRuntime
    , runtimeCloseExtraTools = closeExtraTools
    } SessionControlRuntime
    { controlSkillInvocationsRef = skillInvocationsRef
    , controlCodeModeCloseRef = codeModeCloseRef
    , controlSessionTools = persistedSessionTools
    } = do
    let artifactDirectory = Just (unsafeToFilePath sessionTmp)
        sessionMcpTools =
            if null mcpServerConfigs
                then []
                else
                    (if dialectId == GrokBuildDialect
                        then
                            MCP.mcpFleetGrokMetaToolsForArtifactDirectory
                                artifactDirectory
                                mcpFleet
                        else if progressiveMcp
                            then
                                MCP.mcpFleetMetaToolsForArtifactDirectory
                                    artifactDirectory
                                    mcpFleet
                            else
                                MCP.mcpFleetToolsForArtifactDirectory
                                    artifactDirectory
                                    mcpFleet)
                        <> MCP.mcpFleetResourceTools mcpFleet
        databaseToolsEnv =
            databaseToolsEnvForStore
                startup.startupDatabaseStore
                databaseScopes
                (readIORef persistSlotRef >>= currentSessionId)
                gatewayIdentity
        learnedSkillToolsEnv =
            learnedSkillToolsEnvForStore
                startup.startupDatabaseStore
                databaseScopes
                (readIORef persistSlotRef >>= reservedSessionId)
        sessionGatewayTools = maybe [] managedGatewayTools promptRequest
        sessionDatabaseTools = databaseTools databaseToolsEnv
        sessionLearnedSkillTools =
            learnedSkillTools
                skillInvocationsRef
                (if null mcpServerConfigs then Nothing else Just mcpFleet)
                learnedSkillToolsEnv
        nativeToolGroups =
            maybe [] (.nativeToolGroups) startup.startupNativeHooks
        terminalChartTools =
            case (startup.startupNativeHooks, imageHooks) of
                (Nothing, Just hooks) -> [terminalChartTool hooks]
                _ -> []
        computerTools =
            maybe [] (pure . ComputerUse.computerUseRuntimeTool)
                computerUseRuntime
        activeComputerTools =
            [ tool
            | resolveComputerUseEnabled options startup.startupStdinTty
            , tool <- computerTools
            ]
        imageGenerationTools =
            [ imageGenerationTool
                tokenProvider
                baseToolEnv
                imageGenerationHistory
                imageHooks
            | provider == OpenAIProvider
            , dialectId == CodexDialect
            , nativeCapabilities.nativeHostExtensions
            , not (isGatewayLoadedAuth loaded)
            , inferredTarget.targetConnectionId
                == builtinConnectionId OpenAIProvider
            ]
        surroundingToolGroupsFor selectedComputerTools =
            [ ExecutionToolGroup extraTools
            -- MCP clients and their tenant credentials stay in the
            -- orchestrator process; only execution tools cross a sandbox.
            , HostToolGroup sessionMcpTools
            , HostToolGroup persistedSessionTools
            , HostToolGroup sessionGatewayTools
            , HostToolGroup sessionDatabaseTools
            , HostToolGroup sessionLearnedSkillTools
            , HostToolGroup externalSessionAppTools
            , HostToolGroup terminalChartTools
            ]
                <> nativeToolGroups
                <> [ HostToolGroup imageGenerationTools
                   , ExecutionToolGroup selectedComputerTools
                   ]
        allToolGroups =
            coding.codingAppToolGroups
                <> surroundingToolGroupsFor computerTools
        activeCodingGroups =
            map filterCodingExecution coding.codingAppToolGroups
        activeToolGroups =
            activeCodingGroups
                <> surroundingToolGroupsFor activeComputerTools
        filterCodingExecution = \case
            ExecutionToolGroup appTools ->
                ExecutionToolGroup $
                    filterGhciTools options.optGhci
                        (filterBashTools options.optBash appTools)
            hostGroup@(HostToolGroup _) -> hostGroup
        composeToolGroups groups =
            case startup.startupNativeHooks of
                Nothing -> appToolsFromGroups groups
                Just hooks -> hooks.nativeComposeTools groups
        sessionPlanMode = coding.codingPlanMode
        sessionResumedPlanPending =
            case resumed of
                Just (_, turns) ->
                    resumedPlanNeedsApproval
                        (map (.turnAssistantText) turns)
                Nothing -> False
        -- Keep planSessionDir and subagent store root in sync.
        sessionNoteDirectory dir = do
            writeIORef sessionPlanMode.planSessionDir (Just dir)
            writeIORef subagentStoreRoot (Just dir)
        sessionCloseAll =
            logSlowCleanup "collaboration agents" closeAgents
                `finally`
                    (logSlowCleanup "session lock"
                        (readIORef activeSessionLock
                            >>= mapM_ releaseSessionLock)
                        `finally`
                            (closeExtraTools
                                `finally`
                                    (closeMcp
                                        `finally`
                                            (coding.codingClose
                                                `finally`
                                                    (logSlowCleanup "code mode runtime"
                                                        (join
                                                            (readIORef
                                                                codeModeCloseRef))
                                                        `finally`
                                                            cleanupScratch)))))
        sessionAllTools = composeToolGroups allToolGroups
        sessionTools = composeToolGroups activeToolGroups
    pure SessionToolsRuntime{..}

launchAgentToolsSession
    :: AgentToolsRequest windowTitleResult
    -> ToolStartup
    -> ToolModelRuntime
    -> ToolHostHooks
    -> CollaborationRuntime
    -> ScratchRuntime
    -> McpRuntime
    -> CodingRuntime
    -> SessionInitialContext
    -> InitialContextPreload
    -> SessionControlRuntime
    -> SessionToolsRuntime
    -> IO RunResult
launchAgentToolsSession AgentToolsRequest{..} ToolStartup
    { toolOpenRouterOptions = openRouterOptions
    } ToolModelRuntime
    { toolProvider = provider
    , toolModel = model
    , toolTransportModel = transportModel
    , toolInferredTarget = inferredTarget
    , toolCustomGenericOptions = customGenericOptions
    , toolDialect = dialect
    , toolResumeTargetChanged = resumeTargetChanged
    , toolRefreshDialectContext = refreshDialectContext
    , toolLegacySubagentTarget = legacySubagentTarget
    , toolEffortText = effortText
    , toolPolicy = policy
    , toolClaudeBypassEnabled = claudeBypassEnabled
    } ToolHostHooks
    { toolPlanHooks = planHooks
    } CollaborationRuntime
    { collaborationSubagentSessions = subagentSessions
    , collaborationSubagentStoreRoot = subagentStoreRoot
    , collaborationSubagentForkSource = subagentForkSource
    , collaborationPendingNotices = pendingNotices
    , collaborationRegistry = registry
    , collaborationRootTurnRef = rootTurnRef
    , collaborationAgentTypes = agentTypesRef
    , collaborationOpenAiChild = openaiChild
    , collaborationAllowedChildModels = allowedChildModels
    , collaborationChildModelAllowed = childModelAllowed
    , collaborationResolveChildModel = resolveCollaborationChildModel
    , collaborationCreateWorktree = createSubagentWorktree
    , collaborationContext = multiCtx
    } ScratchRuntime
    { scratchPromptRequest = promptRequest
    , scratchPersistence = persist
    , scratchSessionTmp = sessionTmp
    , scratchImageGenerationHistory = imageGenerationHistory
    } McpRuntime
    { runtimeMcpFleet = mcpFleet
    , runtimeMcpInstructions = mcpInstructions
    } CodingRuntime
    { runtimeCoding = coding
    , runtimeExtraTools = extraTools
    } initialContext initialContextPreload SessionControlRuntime
    { controlGhciEnabledRef = ghciEnabledRef
    , controlBashEnabledRef = bashEnabledRef
    , controlSkillsRef = skillsRef
    , controlSkillInvocationsRef = skillInvocationsRef
    , controlCodeModeCloseRef = codeModeCloseRef
    , controlClaimCurrentSession = claimCurrentSession
    , controlSessionTools = agentSessionAppTools
    } SessionToolsRuntime
    { sessionAllTools = allTools
    , sessionTools = tools
    , sessionMcpTools = mcpTools
    , sessionDatabaseTools = databaseAppTools
    , sessionLearnedSkillTools = learnedSkillAppTools
    , sessionGatewayTools = gatewayTools
    , sessionPlanMode = planMode
    , sessionNoteDirectory = noteSessionDir
    , sessionCloseAll = closeAll
    , sessionResumedPlanPending = resumedPlanPending
    } = do
    when resumedPlanPending (activatePlanMode planMode)
    forM_ startup.startupNativeHooks \hooks ->
        when (hooks.nativeInteractionMode == NativePlan) $
            writeIORef planMode.planStateRef PlanPending
    let mcpRegistrations =
            MCP.mcpFleetRegistrationsForArtifactDirectory
                (Just (unsafeToFilePath sessionTmp))
                mcpFleet
    runAgentSession AgentSessionRequest
        { loaded
        , connectedGateway
        , learnAboutUserRequested
        , sessionTmp
        , activeAccountRef
        , agentTypesRef
        , allTools
        , recordImageGenerationInputs =
            recordImageGenerationImages imageGenerationHistory
        , clearImageGenerationHistory =
            clearImageGenerationHistory imageGenerationHistory
        , bashEnabledRef
        , catalog
        , gatewayModelsRef
        , checkStartupUsageInBackground
        , claimCurrentSession
        , claudeBypassEnabled
        , closeAll
        , codeModeCloseRef
        , coding
        , createSubagentWorktree
        , customGenericOptions
        , workspace = WorkspaceContext{projectRoot, cwd, home}
        , databaseAppTools
        , databaseScopes
        , initialContext
        , initialContextPreload
        , dialect
        , effortText
        , stdinControl
        , extraTools
        , fullscreen
        , gatewayTools
        , ghciEnabledRef
        , allowedChildModels
        , resolveChildModel = resolveCollaborationChildModel
        , childModelAllowed
        , inferredTarget
        , interrupt
        , learnedSkillAppTools
        , legacySubagentTarget
        , mcpFleet
        , mcpRegistrations
        , mcpInstructions
        , mcpTools
        , mcpSamplingRef = processRuntime.processMcpSampling
        , model
        , multiCtx
        , noteSessionDir
        , openRouterOptions
        , openaiChild
        , options
        , pendingNotices
        , pendingTurn
        , persist
        , planHooks
        , planMode
        , policy
        , preferredOpenAiAccountRef
        , promptRequest
        , provider
        , refreshDialectContext
        , registry
        , resolveActiveAccountLabel
        , resumeTargetChanged
        , resumed
        , root
        , rootTurnRef
        , selectHttpAccount
        , selectableTokenProvider
        , sessionTools = agentSessionAppTools
        , setWindowTitle
        , skillInvocationsRef
        , skillsRef
        , startup
        , stateDirectory
        , stderrHandle
        , subagentForkSource
        , subagentSessions
        , subagentStoreRoot
        , tokenProvider
        , toolEnv = baseToolEnv
        , tools
        , transition
        , transportModel
        , unavailableProviders
        }
