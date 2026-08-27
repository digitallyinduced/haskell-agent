module Agent.CLI.Runtime.Orchestration.Tools (runAgentTools) where

import Agent.CLI.AccountPicker ()
import Agent.CLI.AccountSelection ()
import Agent.CLI.Afk ()
import Agent.CLI.AgentSessions
    ( agentSessionTools,
      launchSessionThread,
      sessionThreadStatus,
      AgentSessionToolsEnv(toolsSessionStatus, AgentSessionToolsEnv,
                           toolsPool, toolsRoot, toolsProvider, toolsConnection, toolsModel,
                           toolsTransportModel, toolsDialect, toolsCwd, toolsEffort,
                           toolsCurrentSessionId, toolsLaunchTurn) )
import Agent.CLI.AgentViewport ()
import Agent.CLI.Approval ()
import Agent.CLI.Artifact ()
import Agent.CLI.Auth
    ( LoadedAuth(loadedProvider, loadedTokenProvider),
      hasOpenAiAuth,
      loadAuth )
import Agent.CLI.Clipboard ()
import Agent.CLI.CodeModeRuntime ()
import Agent.CLI.Command ()
import Agent.CLI.Compaction ()
import Agent.CLI.Config
    ( HarnessConfig(..),
      McpServerConfig(..),
      loadHarnessConfig,
      useProgressiveMcp )
import Agent.CLI.Connectivity ()
import Agent.CLI.Database ( databaseTools )
import Agent.CLI.Database.Store ( databaseToolsEnvForStore )
import Agent.CLI.Dialects
    ( CodingTools(..),
      codingToolsForWithTypes,
      filterBashTools,
      filterGhciTools )
import Agent.CLI.Error ( formatException )
import Agent.CLI.GatewayBridge ( managedGatewayTools )
import Agent.CLI.Input ()
import Agent.CLI.Interrupt ()
import Agent.CLI.LearnedSkills ( learnedSkillTools )
import Agent.CLI.LearnedSkills.Store
    ( learnedSkillToolsEnvForStore )
import Agent.CLI.Login ()
import Agent.CLI.Lsp
    ( LspStartup(..), closeLspRuntime, lspRuntimeTool, newLspRuntime )
import Agent.CLI.ManagedTurn ( ManagedTurnRequest(..) )
import Agent.CLI.McpManager ()
import Agent.CLI.McpStatus
    ( formatMcpModelNoticeFor,
      formatMcpProgress,
      summarizeMcpStatuses )
import Agent.CLI.ModelConfig ( builtinConnectionId )
import Agent.CLI.Models
    ( defaultModelFor,
      rawModelOption,
      resolveConfiguredModel,
      resolvePersistedDialect,
      ModelOption(modelTarget),
      ModelTarget(targetConnectionId, targetModelId, targetDialect,
                  targetWireModelId) )
import Agent.CLI.Options
    ( defaultEffortFor,
      isOneShot,
      normalizeReasoningEffortForDialect,
      resolveApprovalPolicy,
      CliOptions(optYolo, optModel, optEffort, optMaxConcurrentAgents,
                 optGhci, optBash, optNoYolo) )
import Agent.CLI.PendingInputs
    ( enqueuePendingInput, newPendingInputs )
import Agent.CLI.Plan ( cliPlanHooks )
import Agent.CLI.Project ( saveRememberedModel )
import Agent.CLI.Prompt ( subscriptionSubagentModelGuidance )
import Agent.CLI.PromptHooks
    ( fullscreenAwarePlanHooks, fullscreenAwareSecretHooks )
import Agent.CLI.Provider.OpenAI ()
import Agent.CLI.Provider.Switch ()
import Agent.CLI.ProviderAvailability ()
import Agent.CLI.ProviderFallback ()
import Agent.CLI.ProviderTransition ()
import Agent.CLI.Recap ()
import Agent.CLI.Render ()
import Agent.CLI.ReplMode ()
import Agent.CLI.Request ()
import Agent.CLI.Resume ()
import Agent.CLI.Runtime.HistorySource
    ( emptyFullscreenHistoryPage, loadFullscreenHistoryPage )
import Agent.CLI.Runtime.Orchestration.Background
    ( runInProcessSessionTurn )
import Agent.CLI.Runtime.Orchestration.Concurrent
    ( concurrentlyAcquire )
import Agent.CLI.Runtime.Orchestration.Restart ()
import Agent.CLI.Runtime.Orchestration.Session ( runAgentSession )
import Agent.CLI.Runtime.Orchestration.Startup
    ( reportStartupWarning )
import Agent.CLI.Runtime.Orchestration.Types ()
import Agent.CLI.Runtime.Persistence ( preparePersistence )
import Agent.CLI.Runtime.Recap ()
import Agent.CLI.Runtime.Repl ()
import Agent.CLI.Runtime.Types ()
import Agent.CLI.Secret ( promptSecretLine )
import Agent.CLI.Session
    ( allocateSessionTemp,
      cleanupPendingPersistence,
      persistenceTempDir,
      removeSessionTemp,
      sessionLegacySubagentTarget,
      Persistence(PersistenceDisabled),
      SessionHandle(sessionDir, sessionMeta),
      SessionMeta(metaId, metaTransportModel, metaProvider,
                  metaConnection, metaModel, metaDialect, metaEffort) )
import Agent.CLI.Session.Attachments ()
import Agent.CLI.Session.Choices ()
import Agent.CLI.Session.History ()
import Agent.CLI.Session.Lifecycle ()
import Agent.CLI.Session.Runtime.Types
    ( StartupRuntime(startupFullscreen, startupBackground,
                     startupFinished, startupDatabaseStore) )
import Agent.CLI.Session.Selection
    ( currentSessionId, loadPrompt, reservedSessionId )
import Agent.CLI.SessionAdmin ()
import Agent.CLI.SessionEnv ()
import Agent.CLI.SessionLock
    ( acquireSessionLock,
      releaseSessionLock,
      sessionLockFilePath,
      sessionLockPath )
import Agent.CLI.SessionState ()
import Agent.CLI.SessionTitle ()
import Agent.CLI.Skills ()
import Agent.CLI.Startup.Auth
    ( markStartupStage, setStartupNotice, startupDie )
import Agent.CLI.StartupContext ()
import Agent.CLI.Style ()
import Agent.CLI.Subagents.Runtime
    ( flushAllSubagentSnapshots,
      persistAndEvictSubagentSessionWithStatus,
      prepareCollaborationSpawn,
      restoreAgentFromDisk )
import Agent.CLI.TUI.App
    ( clearFullscreenHistorySource,
      emitUiEvent,
      setFullscreenHistorySource )
import Agent.CLI.TUI.History
    ( HistoryGeneration(HistoryGeneration) )
import Agent.CLI.TUI.SessionHistory ()
import Agent.CLI.Terminal ( resolveColor )
import Agent.CLI.Tools ()
import Agent.CLI.Turn ()
import Agent.CLI.Usage ()
import Agent.CLI.WebFetch
    ( closeWebFetchRuntime, newWebFetchRuntime, webFetchRuntimeTool )
import Agent.CLI.Worktree
    ( createWorktree, removeWorktree, worktreeRoot )
import Agent.Cancel ()
import Agent.Claude ()
import Agent.Dialect ( dialectForId, DialectId(GrokBuildDialect) )
import Agent.Error ()
import Agent.GrokBuild.Dialect.Goal ()
import Agent.GrokBuild.Dialect.Runtime ()
import Agent.GrokBuild.Dialect.Task ( grokRootChildModels )
import Agent.GrokBuild.Dialect.Workflow ()
import Agent.Loop
    ( TurnInput(UserMessage, AgentMessage),
      LoopError(LoopNoResponseId) )
import Agent.OpenAI.Compaction ()
import Agent.OpenAI.Usage ()
import Agent.OpenAI.WebSocketClient ()
import Agent.OpenRouter.LoopBackend ()
import Agent.OsPath ( unsafeToFilePath )
import Agent.Provider
    ( Provider(XAIProvider, OpenRouterProvider, OpenAIProvider),
      tokenProviderBillingMode )
import Agent.ReasoningEffort
    ( parseReasoningEffort, reasoningEffortText )
import Agent.Responses.GenericBackend ()
import Agent.Responses.GenericClient ( GenericClientOptions(..) )
import Agent.Responses.Types ( ResponseItem )
import Agent.Skills ( SkillCatalog(SkillCatalog) )
import Agent.Store.Postgres ( trustedPool )
import Agent.Store.Types ()
import Agent.Subagents
    ( RootTurnId,
      formatCompletionNotice,
      closeSubagentRegistry,
      newSubagentRegistry,
      setSubagentOnComplete,
      setSubagentOnSettled,
      interruptActiveSubagents,
      defaultMaxConcurrent,
      defaultSubagentConfig,
      SubagentConfig(maxConcurrent) )
import Agent.Subagents.TaskPath ( taskPathRoot )
import Agent.TUI.Model ( UiEvent(UiSetNotice) )
import Agent.TUI.Motion ()
import Agent.Tools.MultiAgents
    ( MultiAgentContext(..), SubagentWorktree(..) )
import Agent.Tools.PlanMode
    ( PlanModeEnv(planSessionDir),
      PlanModeHooks(planAskQuestion, PlanModeHooks, planConfirmEnter,
                    planDecideExit),
      PlanDecision(PlanCancel) )
import Agent.Tools.Secret
    ( SecretPrompt(..), SecretPromptHooks(..) )
import Agent.Tools.Types ( setToolSessionTmp )
import Agent.XAI.LoopBackend ()
import Control.Applicative ( (<|>) )
import Control.Concurrent.Async ( concurrently_ )
import Control.Concurrent.Chan ()
import Control.Concurrent.MVar ()
import Control.Concurrent.STM ()
import Control.Exception ()
import Control.Exception.Safe
    ( SomeException, finally, onException, throwIO, try )
import Control.Monad ( join, when, forM_, unless )
import Data.Functor ()
import Data.IORef
    ( atomicModifyIORef', newIORef, readIORef, writeIORef )
import Data.List ()
import Data.Maybe ( isNothing, fromMaybe, isJust )
import Data.Text ()
import Data.Time.Clock ()
import System.Console.ANSI ()
import System.Console.ANSI.Codes ()
import System.Directory.OsPath ()
import System.Environment ()
import System.Exit ()
import System.IO ()
import System.OsPath ()
import qualified Data.ByteString as BS ()
import qualified Agent.Responses.GenericClient as GenericResponses
    ()
import qualified Agent.MCP as MCP
    ( acquireMcpFleetProgressive,
      acquireMcpFleetWithProgress,
      mcpFleetGrokMetaTools,
      mcpFleetMetaTools,
      mcpFleetTools,
      releaseMcpFleetLease,
      McpFleet(mcpFleetRegistrations, mcpFleetWarnings),
      McpFleetLease(mcpLeaseFleet),
      McpServerConfig(mcpServerRequestTimeoutSeconds, McpServerConfig,
                      mcpServerName, mcpServerCommand, mcpServerArgs, mcpServerCwd,
                      mcpServerEnv, mcpServerStartupTimeoutSeconds) )
import qualified Data.Map.Strict as Map
    ( toAscList, empty, lookup )
import qualified Agent.OpenAI.Auth as OpenAI ()
import qualified Agent.OpenRouter as OpenRouter
    ( clientOptionsFromEnv, mapModel )
import qualified Agent.OpenRouter.Usage as OpenRouterUsage ()
import qualified Agent.Provider as Provider ()
import qualified Agent.CLI.Session.Lifecycle as SessionLifecycle ()
import qualified Agent.CLI.Session.Runner as SessionRunner ()
import qualified Data.Set as Set ()
import qualified Data.Text as Text ( intercalate, unpack )
import qualified Data.Text.IO as Text ()
import qualified Agent.XAI.Options as XAI ()
import qualified Agent.XAI.Client as XAIClient ()
import qualified Agent.XAI.Request as XAIRequest ()
import qualified Agent.XAI.Usage as XAIUsage ()

runAgentTools
    runAgentChild
    loaded
    learnAboutUserRequested
    customBearerToken
    activeAccountIdRef
    activeAccountRef
    activeSelectionRef
    baseToolEnv
    catalog
    checkStartupUsageInBackground
    configuredOptionTarget
    customResponses
    cwd
    databaseScopes
    escPaused
    fullscreen
    home
    interrupt
    isTty
    mcpSupervisor
    options
    pendingTurn
    preferredOpenAiAccountRef
    processRuntime
    projectRoot
    projectSettings
    projectTarget
    resolveActiveAccountLabel
    resumeLock
    resumed
    resumedTarget
    root
    selectHttpAccount
    selectableTokenProvider
    setWindowTitle
    startup
    stateDirectory
    stderrHandle
    targetHint
    tokenProvider
    transition
    transitionTarget
    uiRuntimeRef
    unavailableProviders
    = do
    openRouterOptions <- OpenRouter.clientOptionsFromEnv
    markStartupStage startup "Loading tools…"
    harnessConfig <-
        loadHarnessConfig home >>= \case
            Left err -> startupDie startup (Text.unpack err)
            Right config -> pure config
    let basePlanHooks
            | startup.startupBackground =
                PlanModeHooks
                    { planConfirmEnter = \_ -> pure False
                    , planDecideExit = \_ -> pure PlanCancel
                    , planAskQuestion = \_ _ -> pure Nothing
                    }
            | otherwise =
                cliPlanHooks interrupt escPaused (resolveColor stderrHandle)
        planHooks = fullscreenAwarePlanHooks uiRuntimeRef basePlanHooks
        baseSecretHooks = SecretPromptHooks \request ->
            Right <$> promptSecretLine
                escPaused
                request.secretPromptMessage
                request.secretPromptPurpose
        secretHooks
            | isOneShot options || not isTty = Nothing
            | otherwise =
                Just (fullscreenAwareSecretHooks uiRuntimeRef baseSecretHooks)
        provider = loaded.loadedProvider
        fallbackModel =
            fromMaybe
                (error "validated default model is missing")
                (defaultModelFor catalog provider)
        model = fromMaybe
            (maybe fallbackModel (.targetModelId) targetHint)
            options.optModel
        rawTarget = (rawModelOption provider model).modelTarget
        inferredTarget0 =
            fromMaybe rawTarget $
                transitionTarget
                    <|> configuredOptionTarget
                    <|> resumedTarget
                    <|> if isNothing options.optModel
                        then projectTarget
                        else Nothing
        transportModel = case customResponses of
            Just _ ->
                \name ->
                    case resolveConfiguredModel catalog name of
                        Just option
                            | option.modelTarget.targetConnectionId
                                == inferredTarget0.targetConnectionId ->
                                option.modelTarget.targetWireModelId
                        _
                            | name == model ->
                                inferredTarget0.targetWireModelId
                            | otherwise -> name
            _ -> case provider of
                OpenRouterProvider -> OpenRouter.mapModel openRouterOptions
                _ -> id
        inferredTarget =
            inferredTarget0
                { targetWireModelId =
                    if inferredTarget0.targetConnectionId
                        == builtinConnectionId OpenRouterProvider
                        && inferredTarget0.targetWireModelId
                            == inferredTarget0.targetModelId
                        then transportModel model
                        else inferredTarget0.targetWireModelId
                }
        customGenericOptions = do
            (_, responses) <- customResponses
            pure GenericClientOptions
                { baseUrl = Text.unpack responses.responsesBaseUrl
                , model = inferredTarget.targetWireModelId
                , bearerToken = customBearerToken
                , requestTimeoutSeconds =
                    responses.responsesRequestTimeoutSeconds
                }
        persistedTarget = case fst <$> resumed of
            Just meta ->
                Just
                    ( meta.metaDialect
                    , meta.metaTransportModel
                    )
            Nothing -> do
                remembered <- projectSettings.settingsLastModel
                let target = remembered.projectModelTarget
                if target.targetProvider == provider
                    then Just
                        ( target.targetDialect
                        , Just target.targetWireModelId
                        )
                    else Nothing
        resolvedPersistedTarget =
            (\(storedDialect, storedTransportModel) ->
                resolvePersistedDialect
                    storedDialect
                    storedTransportModel
                    inferredTarget)
                <$> persistedTarget
        mappedTargetChanged =
            maybe False snd resolvedPersistedTarget
        dialectId = case transitionTarget of
            Just target -> target.targetDialect
            Nothing -> case options.optModel of
                Just _ -> inferredTarget.targetDialect
                Nothing
                    | mappedTargetChanged -> inferredTarget.targetDialect
                    | otherwise ->
                        maybe
                            inferredTarget.targetDialect
                            fst
                            resolvedPersistedTarget
        dialect = dialectForId dialectId
        resumeTargetChanged = case fst <$> resumed of
            Just meta ->
                provider /= meta.metaProvider
                    || inferredTarget.targetConnectionId /= meta.metaConnection
                    || model /= meta.metaModel
                    || mappedTargetChanged
                    || dialectId /= meta.metaDialect
            Nothing -> False
        refreshDialectContext = case fst <$> resumed of
            Just meta -> dialectId /= meta.metaDialect
            Nothing -> False
        legacySubagentTarget =
            sessionLegacySubagentTarget . fst <$> resumed
        effort =
            normalizeReasoningEffortForDialect dialectId $
                fromMaybe
                    (maybe
                        (defaultEffortFor provider)
                        (either
                            (const (defaultEffortFor provider))
                            id
                            . parseReasoningEffort
                            . (.metaEffort))
                        (fst <$> resumed))
                    options.optEffort
        effortText = reasoningEffortText effort
        policy = resolveApprovalPolicy options isTty
            projectSettings.settingsAutoApprove
        claudeBypassEnabled =
            not options.optNoYolo
                && (options.optYolo || projectSettings.settingsAutoApprove)
    -- Provider transitions commit their selection separately: manual switches
    -- immediately, automatic fallbacks only after the replacement succeeds.
    when (isNothing transition) $
        saveRememberedModel home projectRoot
            inferredTarget { targetDialect = dialectId }
    activeSessionLock <- newIORef resumeLock
    persistSlotRef <- newIORef PersistenceDisabled
    -- Per-subagent transcripts / previous ids, shared across send_input / task.
    subagentSessions <- newIORef Map.empty
    subagentStoreRoot <- newIORef Nothing
    subagentForkSource <- newIORef (Nothing :: Maybe (IO [ResponseItem]))
    pendingNotices <- newPendingInputs
    let maxConcurrentAgents =
            fromMaybe defaultMaxConcurrent $
                options.optMaxConcurrentAgents
                    <|> projectSettings.settingsMaxConcurrentAgents
                    <|> harnessConfig.configMaxConcurrentAgents
    registry <- newSubagentRegistry
        defaultSubagentConfig { maxConcurrent = maxConcurrentAgents }
        cwd
        (\_ _ _ _ -> pure $ Left LoopNoResponseId)
        (\_ _ -> pure ())
    rootTurnRef <- newIORef (Nothing :: Maybe RootTurnId)
    agentTypesRef <- newIORef Map.empty
    openaiChild <- case provider of
        XAIProvider -> do
            available <- hasOpenAiAuth
            if not available
                then pure Nothing
                else loadAuth (Just OpenAIProvider) >>= \case
                    Left _ -> pure Nothing
                    Right openaiLoaded ->
                        pure (Just openaiLoaded.loadedTokenProvider)
        _ ->
            pure Nothing
    let grokAllowedChildModels = case provider of
            XAIProvider ->
                Just (grokRootChildModels (isJust openaiChild))
            _ ->
                Nothing
        sendToRoot message = do
            enqueuePendingInput pendingNotices (AgentMessage message)
            pure (Right "queued")
        createSubagentWorktree source =
            createWorktree source (worktreeRoot home) >>= \case
                Left err -> pure (Left err)
                Right path -> pure $ Right SubagentWorktree
                    { subagentWorktreePath = path
                    , subagentWorktreeCleanup =
                        removeWorktree source path >>= \case
                            Left err -> pure (Left err)
                            Right () -> pure (Right ())
                    }
        multiCtx = Just MultiAgentContext
            { multiRegistry = registry
            , multiCwd = cwd
            , multiSelfId = Nothing
            , multiDepth = 0
            , multiTaskPath = taskPathRoot
            , multiRootTurnId = readIORef rootTurnRef
            , multiResumeFromDisk = Just
                (restoreAgentFromDisk
                    provider
                    inferredTarget.targetConnectionId
                    transportModel
                    inferredTarget.targetWireModelId
                    dialectId
                    legacySubagentTarget
                    subagentStoreRoot
                    registry
                    subagentSessions
                    agentTypesRef)
            , multiCreateWorktree = Just createSubagentWorktree
            , multiPrepareSpawn = Just
                (prepareCollaborationSpawn
                    provider
                    inferredTarget.targetConnectionId
                    transportModel
                    inferredTarget.targetWireModelId
                    dialectId
                    legacySubagentTarget
                    subagentSessions subagentStoreRoot agentTypesRef
                    subagentForkSource)
            , multiSendToRoot = Just sendToRoot
            , multiSpawnModelGuidance =
                subscriptionSubagentModelGuidance
                    provider
                    (tokenProviderBillingMode tokenProvider)
            , multiAllowedChildModels = grokAllowedChildModels
            }
    promptRequest <- loadPrompt options
    let promptText = fmap (\request -> request.managedTurnText) promptRequest
    persist <-
        preparePersistence
            (trustedPool startup.startupDatabaseStore)
            startup options root
                inferredTarget { targetDialect = dialectId }
                (isNothing transition) cwd effortText promptText resumed
    writeIORef persistSlotRef persist
    forM_ fullscreen \runtime ->
        reservedSessionId persist >>= \case
            Nothing ->
                clearFullscreenHistorySource runtime
            Just sessionId ->
                setFullscreenHistorySource
                    runtime
                    sessionId
                    (loadFullscreenHistoryPage
                        (trustedPool startup.startupDatabaseStore)
                        root
                        sessionId)
                    (emptyFullscreenHistoryPage
                        (HistoryGeneration 0))
    (sessionTmp, ephemeralSessionId) <-
        persistenceTempDir persist >>= \case
            Just tempDir -> pure (tempDir, Nothing)
            Nothing -> do
                (sessionId, tempDir) <- allocateSessionTemp root
                pure (tempDir, Just sessionId)
    setToolSessionTmp baseToolEnv (Just sessionTmp)
    let cleanupScratch = do
            cleanupPendingPersistence persist
            forM_ ephemeralSessionId \sessionId -> do
                _ <- removeSessionTemp root sessionId
                pure ()
        toolEnv = baseToolEnv
        mcpServerConfigs =
            [ MCP.McpServerConfig
                { MCP.mcpServerName = label
                , MCP.mcpServerCommand = Text.unpack config.mcpCommand
                , MCP.mcpServerArgs = map Text.unpack config.mcpArgs
                , MCP.mcpServerCwd =
                    Just $
                        maybe (unsafeToFilePath cwd) Text.unpack config.mcpCwd
                , MCP.mcpServerEnv =
                    [ (Text.unpack name, Text.unpack value)
                    | (name, value) <- Map.toAscList config.mcpEnv
                    ]
                , MCP.mcpServerStartupTimeoutSeconds =
                    config.mcpStartupTimeoutSeconds
                , MCP.mcpServerRequestTimeoutSeconds =
                    config.mcpRequestTimeoutSeconds
                }
            | (label, config) <-
                Map.toAscList harnessConfig.configMcpServers
            , config.mcpEnabled
            ]
        progressiveMcp =
            useProgressiveMcp
                harnessConfig.configMcpInitStrategy
                (isOneShot options)
    mcpStatusPhaseRef <- newIORef (Nothing :: Maybe Bool)
    let reportProgressiveMcp statuses = do
            finished <- readIORef startup.startupFinished
            unless finished do
                setStartupNotice startup.startupFullscreen
                    (formatMcpProgress statuses)
                -- A callback can race with finishStartup between the read and
                -- the UI update. Clear a late notice if startup won the race.
                readIORef startup.startupFinished >>= \nowFinished ->
                    when nowFinished $
                        forM_ startup.startupFullscreen \runtime ->
                            emitUiEvent runtime (UiSetNotice Nothing)
            let (connecting, _, _) = summarizeMcpStatuses statuses
                isConnecting = connecting > 0
            settled <-
                atomicModifyIORef' mcpStatusPhaseRef \previous ->
                    (Just isConnecting, previous == Just True && not isConnecting)
            when (settled && not (null statuses)) $
                enqueuePendingInput pendingNotices
                    (UserMessage (formatMcpModelNoticeFor dialectId statuses))
    mcpLease <-
        try @_ @SomeException
            (if progressiveMcp
                then
                    MCP.acquireMcpFleetProgressive
                        mcpSupervisor
                        reportProgressiveMcp
                        mcpServerConfigs
                else
                    MCP.acquireMcpFleetWithProgress
                        mcpSupervisor
                        (\names ->
                            setStartupNotice startup.startupFullscreen
                                (if null names
                                    then "Loading built-in tools…"
                                    else
                                        "Loading tools: "
                                            <> Text.intercalate ", " names
                                            <> "…"))
                        mcpServerConfigs)
            >>= \case
            Left exception ->
                startupDie startup
                    ("Failed to initialize MCP tools: " <> show exception)
            Right lease -> pure lease
    let mcpFleet = mcpLease.mcpLeaseFleet
    mapM_ (reportStartupWarning startup) mcpFleet.mcpFleetWarnings
    setStartupNotice startup.startupFullscreen "Loading built-in tools…"
    coding <-
        codingToolsForWithTypes
            dialect
            toolEnv
            (Just planHooks)
            secretHooks
            multiCtx
            agentTypesRef
            `onException`
                (MCP.releaseMcpFleetLease mcpLease >> cleanupScratch)
    let closeBeforeSession =
            coding.codingClose
                `finally`
                    (MCP.releaseMcpFleetLease mcpLease
                        `finally` cleanupScratch)
        acquireGrokExtras
            | dialectId /= GrokBuildDialect =
                pure
                    ( Nothing
                    , LspStartup
                        { lspStartupRuntime = Nothing
                        , lspStartupWarnings = []
                        }
                    )
            | otherwise =
                concurrentlyAcquire
                    (newWebFetchRuntime
                        harnessConfig.configWebFetch
                        toolEnv >>= \case
                            Left err ->
                                startupDie startup
                                    ("Failed to initialize web_fetch: "
                                        <> Text.unpack err)
                            Right runtime -> pure runtime)
                    (mapM_ closeWebFetchRuntime)
                    (newLspRuntime harnessConfig.configLsp toolEnv)
                    (mapM_ closeLspRuntime . (.lspStartupRuntime))
    (webFetchRuntime, lspStartup) <-
        acquireGrokExtras `onException` closeBeforeSession
    mapM_ (reportStartupWarning startup) lspStartup.lspStartupWarnings
    let lspRuntime = lspStartup.lspStartupRuntime
        extraTools =
            maybe [] (pure . webFetchRuntimeTool) webFetchRuntime
                <> maybe [] (pure . lspRuntimeTool) lspRuntime
        closeExtraTools =
            concurrently_
                (mapM_ closeLspRuntime lspRuntime)
                (mapM_ closeWebFetchRuntime webFetchRuntime)
    case multiCtx of
        Just ctx -> do
            setSubagentOnComplete ctx.multiRegistry \agentId status -> do
                enqueuePendingInput pendingNotices
                    (UserMessage (formatCompletionNotice agentId status))
            setSubagentOnSettled ctx.multiRegistry \agentId status -> do
                sessions <- readIORef subagentSessions
                case Map.lookup agentId sessions of
                    Just session -> do
                        _ <-
                            persistAndEvictSubagentSessionWithStatus
                                subagentStoreRoot ctx.multiRegistry agentTypesRef
                                agentId status session
                        pure ()
                    Nothing -> pure ()
        Nothing -> pure ()
    ghciEnabledRef <- newIORef options.optGhci
    bashEnabledRef <- newIORef options.optBash
    skillsRef <- newIORef (SkillCatalog [] [])
    skillInvocationsRef <- newIORef []
    codeModeCloseRef <- newIORef (pure ())
    let claimCurrentSession handle = do
            let desired = sessionLockPath handle.sessionDir
            readIORef activeSessionLock >>= \case
                Just current
                    | sessionLockFilePath current == desired -> pure ()
                previous ->
                    acquireSessionLock
                        handle.sessionDir
                        handle.sessionMeta.metaId >>= \case
                            Left err -> throwIO (userError (Text.unpack err))
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
            , toolsCwd = cwd
            , toolsEffort = effortText
            , toolsCurrentSessionId =
                readIORef persistSlotRef >>= currentSessionId
            , toolsLaunchTurn = \handle message -> do
                ghciEnabled <- readIORef ghciEnabledRef
                bashEnabled <- readIORef bashEnabledRef
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
                    else
                        launchSessionThread
                            processRuntime.processSessionThreads
                            handle.sessionMeta.metaId
                            action
            , toolsSessionStatus =
                sessionThreadStatus processRuntime.processSessionThreads
            }
        mcpTools =
            if null mcpServerConfigs
                then []
                else if dialectId == GrokBuildDialect
                    then MCP.mcpFleetGrokMetaTools mcpFleet
                    else if progressiveMcp
                        then MCP.mcpFleetMetaTools mcpFleet
                        else MCP.mcpFleetTools mcpFleet
        databaseToolsEnv =
            databaseToolsEnvForStore
                startup.startupDatabaseStore
                databaseScopes
                (readIORef persistSlotRef >>= currentSessionId)
        learnedSkillToolsEnv =
            learnedSkillToolsEnvForStore
                startup.startupDatabaseStore
                databaseScopes
                (readIORef persistSlotRef >>= reservedSessionId)
        sessionTools = agentSessionTools sessionToolsEnv
        gatewayTools = maybe [] managedGatewayTools promptRequest
        databaseAppTools = databaseTools databaseToolsEnv
        learnedSkillAppTools =
            learnedSkillTools skillInvocationsRef learnedSkillToolsEnv
        allTools =
            coding.codingAppTools
                ++ extraTools
                ++ mcpTools
                ++ sessionTools
                ++ gatewayTools
                ++ databaseAppTools
                ++ learnedSkillAppTools
        tools =
            filterGhciTools options.optGhci
                (filterBashTools options.optBash coding.codingAppTools)
                ++ extraTools
                ++ mcpTools
                ++ sessionTools
                ++ gatewayTools
                ++ databaseAppTools
                ++ learnedSkillAppTools
        planMode = coding.codingPlanMode
        -- Keep planSessionDir and subagent store root in sync.
        noteSessionDir dir = do
            writeIORef planMode.planSessionDir (Just dir)
            writeIORef subagentStoreRoot (Just dir)
        closeAgents =
            case multiCtx of
                Just ctx -> do
                    interruptActiveSubagents ctx.multiRegistry
                    flushAllSubagentSnapshots subagentStoreRoot ctx.multiRegistry
                        subagentSessions agentTypesRef
                    closeSubagentRegistry ctx.multiRegistry
                Nothing -> pure ()
        closeAll =
            closeAgents
                `finally`
                    ((readIORef activeSessionLock
                        >>= mapM_ releaseSessionLock)
                        `finally`
                            (closeExtraTools
                                `finally`
                                    (MCP.releaseMcpFleetLease mcpLease
                                        `finally`
                                            (coding.codingClose
                                                `finally`
                                                    (join (readIORef codeModeCloseRef)
                                                        `finally`
                                                            cleanupScratch)))))
    runAgentSession
        loaded
        learnAboutUserRequested
        sessionTmp
        activeAccountIdRef
        activeAccountRef
        activeSelectionRef
        agentTypesRef
        allTools
        bashEnabledRef
        catalog
        checkStartupUsageInBackground
        claimCurrentSession
        claudeBypassEnabled
        closeAll
        codeModeCloseRef
        coding
        createSubagentWorktree
        customGenericOptions
        cwd
        databaseAppTools
        databaseScopes
        dialect
        effortText
        escPaused
        extraTools
        fullscreen
        gatewayTools
        ghciEnabledRef
        grokAllowedChildModels
        home
        inferredTarget
        interrupt
        learnedSkillAppTools
        legacySubagentTarget
        mcpFleet
        mcpTools
        model
        multiCtx
        noteSessionDir
        openRouterOptions
        openaiChild
        options
        pendingNotices
        pendingTurn
        persist
        planHooks
        planMode
        policy
        preferredOpenAiAccountRef
        projectRoot
        promptRequest
        provider
        refreshDialectContext
        registry
        resolveActiveAccountLabel
        resumeTargetChanged
        resumed
        rootTurnRef
        selectHttpAccount
        selectableTokenProvider
        sessionTools
        setWindowTitle
        skillInvocationsRef
        skillsRef
        startup
        stateDirectory
        stderrHandle
        subagentForkSource
        subagentSessions
        subagentStoreRoot
        tokenProvider
        toolEnv
        tools
        transition
        transportModel
        unavailableProviders
