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
                           toolsTransportModel, toolsDialect, toolsAllowedModels,
                           toolsResolveModelOption,
                           toolsCwd, toolsEffort,
                           toolsCurrentSessionId, toolsLaunchTurn) )
import Agent.CLI.AgentViewport ()
import Agent.CLI.Approval ()
import Agent.CLI.Artifact ()
import Agent.CLI.Auth
    ( LoadedAuth(loadedProvider, loadedTokenProvider),
      hasOpenAiAuth,
      isGatewayLoadedAuth,
      loadAuth )
import Agent.CLI.Clipboard ()
import Agent.CLI.CodeModeRuntime ()
import Agent.CLI.Command ()
import qualified Agent.CLI.ComputerUse as ComputerUse
import Agent.CLI.Compaction ()
import Agent.CLI.Config
    ( HarnessConfig(..),
      McpServerConfig(..),
      loadHarnessConfig,
      useProgressiveMcp )
import Agent.CLI.Connectivity ()
import Agent.CLI.Database ( databaseTools )
import Agent.CLI.Database.Store
    (DatabaseScopes, databaseToolsEnvForStore)
import Agent.CLI.Dialects
    ( CodingTools(..),
      codingToolsForWithTypes,
      filterBashTools,
      filterGhciTools )
import Agent.CLI.Error ( formatException )
import Agent.CLI.GatewayClient
    ( GatewayModelAccess
    , cachedGatewayModels
    )
import Agent.CLI.GatewayBridge ( managedGatewayTools )
import Agent.CLI.Input ()
import Agent.CLI.Interrupt (InterruptState)
import Agent.CLI.LearnedSkills ( learnedSkillTools )
import Agent.CLI.LearnedSkills.Store
    ( learnedSkillToolsEnvForStore )
import Agent.CLI.Login ()
import Agent.CLI.Lsp
    ( LspStartup(..), closeLspRuntime, lspRuntimeTool, newLspRuntime )
import Agent.CLI.ManagedTurn ( ManagedTurnRequest(..) )
import Agent.CLI.McpManager ()
import Agent.CLI.McpOAuthStore (mcpOAuthStorePath)
import Agent.CLI.McpElicitation (cliMcpElicitation)
import Agent.CLI.McpStatus
    ( formatMcpInstructionsNotice,
      formatMcpModelNoticeFor,
      formatMcpProgress,
      summarizeMcpStatuses )
import Agent.CLI.ModelConfig
    (ModelCatalog, ResponsesConnection(..), builtinConnectionId)
import Agent.CLI.Models
    ( defaultModelFor,
      gatewayModelOptions,
      rawModelOption,
      resolveConfiguredModel,
      resolveModelOptionById,
      resolvePersistedDialect,
      ModelOption(modelTarget),
      ModelTarget(targetProvider, targetConnectionId, targetModelId, targetDialect,
                  targetWireModelId) )
import Agent.CLI.Options
    ( defaultEffortFor,
      isOneShot,
      normalizeReasoningEffortForDialect,
      resolveApprovalPolicy,
      CliOptions(optYolo, optModel, optEffort, optMaxConcurrentAgents,
                 optGhci, optBash, optComputerUse, optNoYolo) )
import Agent.CLI.PendingInputs
    ( PendingNoticeKind(..)
    , enqueuePendingInput
    , enqueuePendingNotice
    , newPendingInputs
    )
import Agent.CLI.Project (ProjectModel(..), ProjectSettings(..))
import Agent.CLI.Plan
    ( cliPlanHooks
    , resumedPlanNeedsApproval
    )
import Agent.CLI.Prompt
    ( mcpInstructionsForRequest
    , subscriptionSubagentModelGuidance
    )
import Agent.CLI.PromptHooks
    ( fullscreenAwareImageHooks, fullscreenAwarePlanHooks, fullscreenAwareSecretHooks )
import Agent.CLI.Provider.OpenAI ()
import Agent.CLI.Provider.Switch ()
import Agent.CLI.ProviderAvailability ()
import Agent.CLI.ProviderFallback ()
import Agent.CLI.ProviderTransition (PendingTurn, ProviderTransition)
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
import Agent.CLI.Runtime.Orchestration.Types
    (AgentProcessRuntime(..), AgentRunMode)
import Agent.CLI.Runtime.Orchestration.Restart ()
import Agent.CLI.Runtime.Orchestration.Session ( runAgentSession )
import Agent.CLI.Runtime.Orchestration.Startup
    ( reportStartupWarning )
import Agent.CLI.Runtime.Persistence ( preparePersistence )
import Agent.CLI.Runtime.Recap ()
import Agent.CLI.Runtime.Repl ()
import Agent.CLI.Runtime.Types (DevResult, RunResult)
import Agent.CLI.Secret ( promptSecretLine )
import Agent.CLI.Session
    ( allocateSessionTemp,
      acquireSessionTempLease,
      cleanupStaleSessionTemps,
      cleanupPendingPersistence,
      defaultSessionTempKeepCount,
      listSessions,
      persistenceTempDir,
      releaseSessionTempLease,
      removeSessionTemp,
      sessionLegacySubagentTarget,
      SessionTempCleanupReport(..),
      Persistence(PersistenceDisabled),
      SessionHandle(sessionDir, sessionMeta),
      SessionMeta(metaId, metaTransportModel, metaProvider,
                  metaConnection, metaModel, metaDialect, metaEffort, metaCwd),
      SessionTurn(turnAssistantText) )
import Agent.CLI.Session.Attachments ( putImagePreview )
import Agent.CLI.Session.Choices ()
import Agent.CLI.Session.History (foldSessionItems)
import Agent.CLI.Session.Lifecycle ()
import Agent.CLI.Session.Runtime.Types
    ( StartupRuntime(startupFullscreen, startupBackground,
                     startupFinished, startupDatabaseStore,
                     startupSessionState) )
import Agent.CLI.Session.Selection
    ( currentSessionId, loadPrompt, reservedSessionId )
import Agent.CLI.SessionAdmin ()
import Agent.CLI.SessionEnv ()
import Agent.CLI.SessionLock
    ( SessionLock,
      acquireSessionLock,
      releaseSessionLock,
      sessionLockFilePath,
      sessionLockPath )
import Agent.CLI.SessionState ( SessionState(sessionPreviewId) )
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
    ( FullscreenRuntime,
      clearFullscreenHistorySource,
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
    ( acquireWorktreeLease,
      cleanupStaleWorktrees,
      createManagedWorktree,
      defaultWorktreeKeepCount,
      releaseWorktreeLease,
      removeWorktree,
      worktreeRoot,
      WorktreeCleanupReport(..) )
import Agent.Cancel ()
import Agent.Claude ()
import Agent.Dialect
    ( dialectForId
    , DialectId(CodexDialect, GrokBuildDialect)
    )
import Agent.Error (ApiError)
import Agent.GrokBuild.Dialect.Goal ()
import Agent.GrokBuild.Dialect.Runtime ()
import Agent.GrokBuild.Dialect.Task ( grokRootChildModels )
import Agent.GrokBuild.Dialect.Workflow ()
import Agent.Loop
    ( TurnInput(UserMessage, AgentMessage),
      LoopError(LoopNoResponseId) )
import Agent.OpenAI.Compaction ()
import Agent.OpenAI.ImageGeneration
    ( clearImageGenerationHistory
    , imageGenerationTool
    , newImageGenerationHistory
    , recordImageGenerationImages
    , recordImageGenerationResponseItems
    )
import Agent.OpenAI.Usage ()
import Agent.OpenAI.WebSocketClient ()
import Agent.OpenRouter.LoopBackend ()
import Agent.OsPath ( unsafeToFilePath )
import Agent.Provider
    ( Credential, TokenProvider,
      Provider(XAIProvider, OpenRouterProvider, OpenAIProvider)
    , tokenProviderBillingMode
    )
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
    ( CollaborationModelTarget(..)
    , MultiAgentContext(..)
    , SubagentWorktree(..)
    )
import Agent.Tools.PlanMode
    ( PlanModeEnv(planSessionDir),
      activatePlanMode,
      PlanModeHooks(planAskQuestion, PlanModeHooks, planConfirmEnter,
                    planDecideExit),
      PlanDecision(PlanCancel) )
import Agent.Tools.Secret
    ( SecretPrompt(..), SecretPromptHooks(..) )
import Agent.Tools.ShowImage
    ( ImageDisplayHooks(..), ImageDisplayRequest(..) )
import Agent.Tools.Types (ToolEnv, setToolSessionTmp)
import Agent.XAI.LoopBackend ()
import Control.Applicative ( (<|>) )
import Control.Concurrent.Async ( concurrently, concurrently_ )
import Control.Concurrent.Chan ()
import Control.Concurrent.MVar ()
import Control.Concurrent.STM ()
import Control.Exception ()
import Control.Exception.Safe
    ( SomeException, finally, onException, throwIO, try )
import Control.Monad ( forM_, join, unless, when )
import Data.Functor ()
import Data.IORef
    (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List ()
import Data.Maybe ( isNothing, fromMaybe, isJust )
import Data.Set (Set)
import Data.Text (Text)
import Data.Time.Clock ()
import System.Console.ANSI ()
import System.Console.ANSI.Codes ()
import System.Directory.OsPath (getHomeDirectory)
import System.Environment ()
import System.Exit ()
import System.IO (Handle)
import System.OsPath (OsPath)
import qualified Data.ByteString as BS ()
import qualified Agent.Responses.GenericClient as GenericResponses
    ()
import qualified Agent.MCP as MCP
    ( McpSupervisor,
      acquireMcpFleetProgressive,
      acquireMcpFleetWithProgress,
      mcpFleetGrokMetaTools,
      mcpFleetInstructions,
      mcpFleetMetaTools,
      mcpFleetResourceTools,
      mcpFleetStatuses,
      mcpFleetTools,
      releaseMcpFleetLease,
      McpFleet(mcpFleetWarnings),
      McpFleetLease(mcpLeaseFleet),
      McpServerConfig(mcpServerRequestTimeoutSeconds, McpServerConfig,
                      mcpServerName, mcpServerUrl, mcpServerCommand, mcpServerArgs, mcpServerCwd,
                      mcpServerEnv, mcpServerStartupTimeoutSeconds, mcpServerProtocol) )
import qualified Data.Map.Strict as Map
    ( toAscList, empty, lookup, notMember )
import qualified Agent.OpenAI.Auth as OpenAI ()
import qualified Agent.OpenRouter as OpenRouter
    ( clientOptionsFromEnv, mapModel )
import qualified Agent.OpenRouter.Usage as OpenRouterUsage ()
import qualified Agent.Provider as Provider ()
import qualified Agent.CLI.Session.Lifecycle as SessionLifecycle ()
import qualified Agent.CLI.Session.Runner as SessionRunner ()
import qualified Data.Set as Set ()
import qualified Data.Text as Text ( intercalate, pack, strip, unpack )
import qualified Data.Text.IO as Text ()
import qualified Agent.XAI.Options as XAI ()
import qualified Agent.XAI.Client as XAIClient ()
import qualified Agent.XAI.Request as XAIRequest ()
import qualified Agent.XAI.Usage as XAIUsage ()

runAgentTools
    :: (AgentRunMode -> CliOptions -> IO DevResult)
    -> LoadedAuth
    -> Bool
    -> Maybe Text
    -> IORef Text
    -> IORef Text
    -> IORef Text
    -> ToolEnv
    -> ModelCatalog
    -> IORef (Maybe GatewayModelAccess)
    -> Bool
    -> Maybe ModelTarget
    -> Maybe (Text, ResponsesConnection)
    -> OsPath
    -> DatabaseScopes
    -> IORef Bool
    -> Maybe FullscreenRuntime
    -> OsPath
    -> InterruptState
    -> Bool
    -> MCP.McpSupervisor
    -> CliOptions
    -> Maybe PendingTurn
    -> IORef (Maybe Text)
    -> AgentProcessRuntime
    -> OsPath
    -> ProjectSettings
    -> Maybe ModelTarget
    -> (Credential -> IO Text)
    -> Maybe SessionLock
    -> Maybe (SessionMeta, [SessionTurn])
    -> Maybe ModelTarget
    -> OsPath
    -> (Text -> IO (Either ApiError Text))
    -> TokenProvider
    -> (Text -> IO windowTitleResult)
    -> StartupRuntime
    -> FilePath
    -> Handle
    -> Maybe ModelTarget
    -> TokenProvider
    -> Maybe ProviderTransition
    -> Maybe ModelTarget
    -> IORef (Maybe FullscreenRuntime)
    -> Set Provider
    -> IO RunResult
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
    gatewayModelsRef
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
    (gatewaySelection, gatewayAllowedChildModels) <-
        if not (isGatewayLoadedAuth loaded)
            then pure (Nothing, Nothing)
            else do
                access <-
                    readIORef gatewayModelsRef >>= \case
                        Nothing ->
                            startupDie startup
                                "The organization gateway model catalog is unavailable."
                        Just value -> pure value
                cachedGatewayModels access >>= \case
                    Nothing ->
                        startupDie startup
                            "The organization gateway model catalog is unavailable."
                    Just modelIds ->
                        case
                            gatewayModelOptions
                                catalog
                                (builtinConnectionId OpenAIProvider)
                                OpenAIProvider
                                modelIds
                            of
                            [] ->
                                startupDie startup
                                    "The organization gateway does not offer any models."
                            firstAvailable : remainingAvailable -> do
                                let available =
                                        firstAvailable : remainingAvailable
                                    resolveTarget target =
                                        resolveModelOptionById
                                            available
                                            target.targetModelId
                                selected <- case options.optModel of
                                    Just requested ->
                                        case
                                            resolveModelOptionById
                                                available
                                                requested
                                            of
                                            Nothing ->
                                                startupDie startup $
                                                    "Model '"
                                                        <> Text.unpack requested
                                                        <> "' is not available through your organization gateway."
                                            Just selected ->
                                                pure selected
                                    Nothing ->
                                        pure $
                                            fromMaybe firstAvailable $
                                                ( transitionTarget
                                                    >>= resolveTarget
                                                )
                                                    <|> ( configuredOptionTarget
                                                            >>= resolveTarget
                                                        )
                                                    <|> ( resumedTarget
                                                            >>= resolveTarget
                                                        )
                                                    <|> ( projectTarget
                                                            >>= resolveTarget
                                                        )
                                                    <|> ( targetHint
                                                            >>= resolveTarget
                                                        )
                                pure
                                    ( Just selected
                                    , Just
                                        (map
                                            (.modelTarget.targetModelId)
                                            available)
                                    )
    let basePlanHooks
            | startup.startupBackground =
                PlanModeHooks
                    { planConfirmEnter = \_ -> pure False
                    , planDecideExit = \_ -> pure PlanCancel
                    , planAskQuestion = \_ _ -> pure Nothing
                    }
            | otherwise =
                cliPlanHooks
                    provider interrupt escPaused (resolveColor stderrHandle)
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
        -- Outside the retained TUI, agent-displayed images print inline with
        -- the same graphics path as pasted attachments.
        baseImageHooks = ImageDisplayHooks \request -> do
            color <- resolveColor stderrHandle
            putImagePreview
                startup.startupSessionState.sessionPreviewId
                color
                [request.displayImage]
            pure (Right ())
        imageHooks
            | not isTty = Nothing
            | otherwise =
                Just (fullscreenAwareImageHooks uiRuntimeRef baseImageHooks)
        provider = loaded.loadedProvider
        fallbackModel =
            fromMaybe
                (error "validated default model is missing")
                (defaultModelFor catalog provider)
        unrestrictedModel =
            fromMaybe
                (maybe fallbackModel (.targetModelId) targetHint)
                options.optModel
        model =
            maybe
                unrestrictedModel
                (.modelTarget.targetModelId)
                gatewaySelection
        rawTarget = (rawModelOption provider model).modelTarget
        inferredTarget0 =
            maybe
                ( fromMaybe rawTarget $
                    transitionTarget
                        <|> configuredOptionTarget
                        <|> resumedTarget
                        <|> if isNothing options.optModel
                            then projectTarget
                            else Nothing
                )
                (.modelTarget)
                gatewaySelection
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
        dialectId = case gatewaySelection of
            Just selected -> selected.modelTarget.targetDialect
            Nothing -> case transitionTarget of
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
    -- Plan mode itself is process-local, while the assistant's proposed plan
    -- is durable in the session transcript. Reconstruct the approval phase
    -- before entering the REPL so a resumed Codex session cannot interpret
    -- the user's approval as ordinary steering input.
    -- Keep inferred startup, resume, and delegated-agent targets session-local.
    -- Live top-level model/provider switches persist their selection in
    -- Agent.CLI.Provider.Switch instead.
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
    let allowedChildModels =
            case gatewayAllowedChildModels of
                Just modelIds -> Just modelIds
                Nothing -> case provider of
                    XAIProvider ->
                        Just (grokRootChildModels (isJust openaiChild))
                    _ ->
                        Nothing
        childModelAllowed
            | Just resolve <- gatewayChildModelOption =
                Just \modelId -> isJust <$> resolve modelId
            | otherwise = Nothing
        resolveCollaborationChildModel
            | Just resolve <- gatewayChildModelOption =
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
        gatewayChildModelOption
            | isNothing gatewayAllowedChildModels = Nothing
            | otherwise =
                Just \requested ->
                    readIORef gatewayModelsRef >>= \case
                        Nothing -> pure Nothing
                        Just access ->
                            cachedGatewayModels access >>= \case
                                Nothing -> pure Nothing
                                Just modelIds ->
                                    pure
                                        (resolveModelOptionById
                                            (gatewayModelOptions
                                                catalog
                                                (builtinConnectionId
                                                    OpenAIProvider)
                                                OpenAIProvider
                                                modelIds)
                                            (Text.strip requested))
        sendToRoot message = do
            enqueuePendingInput pendingNotices (AgentMessage message) >>= \case
                Left err -> pure (Left err)
                Right () -> pure (Right "queued")
        createSubagentWorktree source =
            createManagedWorktree home source >>= \case
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
                    resolveCollaborationChildModel
                    agentTypesRef)
            , multiCreateWorktree = Just createSubagentWorktree
            , multiPrepareSpawn = Just
                (prepareCollaborationSpawn
                    provider
                    inferredTarget.targetConnectionId
                    transportModel
                    inferredTarget.targetWireModelId
                    effortText
                    dialectId
                    legacySubagentTarget
                    subagentSessions subagentStoreRoot agentTypesRef
                    subagentForkSource)
            , multiSendToRoot = Just sendToRoot
            , multiSpawnModelGuidance =
                if isJust gatewayAllowedChildModels
                    then Nothing
                    else
                        subscriptionSubagentModelGuidance
                            provider
                            (tokenProviderBillingMode tokenProvider)
            , multiAllowedChildModels = allowedChildModels
            , multiResolveChildModel = resolveCollaborationChildModel
            , multiChildModelAllowed = childModelAllowed
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
    imageGenerationHistory <- newImageGenerationHistory
    forM_ resumed \(_, turns) ->
        recordImageGenerationResponseItems
            imageGenerationHistory
            (foldSessionItems turns)
    home <- getHomeDirectory
    let cleanupAllocatedScratch = do
            cleanupPendingPersistence persist
            forM_ ephemeralSessionId \sessionId -> do
                _ <- removeSessionTemp root sessionId
                pure ()
    worktreeLease <-
        acquireWorktreeLease (worktreeRoot home) cwd >>= \case
            Left err -> do
                cleanupAllocatedScratch
                startupDie startup (Text.unpack err)
            Right lease -> pure lease
    sessionTempLease <-
        (acquireSessionTempLease root sessionTmp
            `onException`
                (mapM_ releaseWorktreeLease worktreeLease
                    >> cleanupAllocatedScratch)) >>= \case
                Left err -> do
                    mapM_ releaseWorktreeLease worktreeLease
                    cleanupAllocatedScratch
                    startupDie startup (Text.unpack err)
                Right lease -> pure lease
    let cleanupScratch =
            mapM_ releaseSessionTempLease sessionTempLease
                `finally`
                    (mapM_ releaseWorktreeLease worktreeLease
                        `finally` cleanupAllocatedScratch)
        toolEnv = baseToolEnv
        mcpServerConfigs =
            [ MCP.McpServerConfig
                { MCP.mcpServerName = label
                , MCP.mcpServerUrl = config.mcpUrl
                , MCP.mcpServerCommand = Text.unpack config.mcpCommand
                , MCP.mcpServerArgs = map Text.unpack config.mcpArgs
                , MCP.mcpServerCwd =
                    Just $
                        maybe (unsafeToFilePath cwd) Text.unpack config.mcpCwd
                , MCP.mcpServerEnv =
                    [ (Text.unpack name, Text.unpack value)
                    | (name, value) <- Map.toAscList config.mcpEnv
                    ] <> case config.mcpUrl of
                        Just url
                            | Map.notMember "MCP_OAUTH_TOKEN_FILE" config.mcpEnv ->
                                [("MCP_OAUTH_TOKEN_FILE", unsafeToFilePath (mcpOAuthStorePath home url))]
                        _ -> []
                , MCP.mcpServerStartupTimeoutSeconds =
                    config.mcpStartupTimeoutSeconds
                , MCP.mcpServerRequestTimeoutSeconds =
                    config.mcpRequestTimeoutSeconds
                , MCP.mcpServerProtocol = config.mcpProtocol
                }
            | (label, config) <-
                Map.toAscList harnessConfig.configMcpServers
            , config.mcpEnabled
            ]
        progressiveMcp =
            useProgressiveMcp
                harnessConfig.configMcpInitStrategy
                (isOneShot options)
    -- Housekeeping may inspect hundreds of worktrees and invoke Git for each
    -- candidate. It must never delay interactive startup.
    _ <- processRuntime.processStartCleanup do
        cleanupResult <- try @_ @SomeException do
            (sessions, sessionWarnings) <-
                listSessions
                    (trustedPool startup.startupDatabaseStore)
                    root
            let protectedWorktrees =
                    -- Persisted sessions must remain resumable. A worktree
                    -- becomes collectible after its session is deleted.
                    cwd : map (.metaCwd) sessions
            (worktreeReport, tempReport) <- concurrently
                (if null sessionWarnings
                    then cleanupStaleWorktrees
                        (worktreeRoot home)
                        defaultWorktreeKeepCount
                        protectedWorktrees
                    -- A partial session catalog cannot prove that an old
                    -- checkout is unreferenced.
                    else pure mempty)
                (cleanupStaleSessionTemps
                    root
                    defaultSessionTempKeepCount
                    [sessionTmp])
            pure (sessionWarnings, worktreeReport, tempReport)
        case cleanupResult of
            Left exception ->
                reportStartupWarning startup
                    ("stale resource cleanup failed: "
                        <> formatException exception)
            Right (sessionWarnings, worktreeReport, tempReport) -> do
                mapM_ (reportStartupWarning startup) sessionWarnings
                forM_ worktreeReport.cleanupFailures \(path, err) ->
                    reportStartupWarning startup
                        ("could not clean stale worktree "
                            <> Text.pack (unsafeToFilePath path)
                            <> ": "
                            <> err)
                forM_ tempReport.tempCleanupFailures \(path, err) ->
                    reportStartupWarning startup
                        ("could not clean stale session scratch directory "
                            <> Text.pack (unsafeToFilePath path)
                            <> ": "
                            <> err)
    mcpStatusPhaseRef <- newIORef (Nothing :: Maybe Bool)
    mcpFleetRef <- newIORef (Nothing :: Maybe MCP.McpFleet)
    writeIORef processRuntime.processMcpElicitation
        (if isOneShot options || not isTty
            then Nothing
            else Just (cliMcpElicitation escPaused uiRuntimeRef))
    let enqueueMcpSnapshot statuses =
            unless (null statuses) do
                instructions <-
                    readIORef mcpFleetRef
                        >>= maybe (pure []) MCP.mcpFleetInstructions
                enqueuePendingNotice pendingNotices PendingMcpNotice
                    (UserMessage
                        (formatMcpModelNoticeFor dialectId statuses
                            <> formatMcpInstructionsNotice instructions))
                    >>= either (reportStartupWarning startup) pure
        reportProgressiveMcp statuses = do
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
            when settled (enqueueMcpSnapshot statuses)
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
            Left exception -> do
                cleanupScratch
                startupDie startup
                    ("Failed to initialize MCP tools: " <> show exception)
            Right lease -> pure lease
    let mcpFleet = mcpLease.mcpLeaseFleet
    writeIORef mcpFleetRef (Just mcpFleet)
    when progressiveMcp $
        MCP.mcpFleetStatuses mcpFleet >>= enqueueMcpSnapshot
    currentMcpInstructions <- MCP.mcpFleetInstructions mcpFleet
    let mcpInstructions =
            mcpInstructionsForRequest
                progressiveMcp
                currentMcpInstructions
    mapM_ (reportStartupWarning startup) mcpFleet.mcpFleetWarnings
    setStartupNotice startup.startupFullscreen "Loading built-in tools…"
    coding <-
        codingToolsForWithTypes
            dialect
            toolEnv
            (Just planHooks)
            secretHooks
            imageHooks
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
                enqueuePendingNotice pendingNotices PendingSubagentNotice
                    (UserMessage (formatCompletionNotice agentId status))
                    >>= either (reportStartupWarning startup) pure
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
            , toolsAllowedModels = gatewayAllowedChildModels
            , toolsResolveModelOption = gatewayChildModelOption
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
                else
                    (if dialectId == GrokBuildDialect
                        then MCP.mcpFleetGrokMetaTools mcpFleet
                        else if progressiveMcp
                            then MCP.mcpFleetMetaTools mcpFleet
                            else MCP.mcpFleetTools mcpFleet)
                        <> MCP.mcpFleetResourceTools mcpFleet
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
        computerTools =
            [ComputerUse.computerUseTool | options.optComputerUse, provider == OpenAIProvider]
        imageGenerationTools =
            [ imageGenerationTool
                tokenProvider
                toolEnv
                imageGenerationHistory
                imageHooks
            | provider == OpenAIProvider
            , dialectId == CodexDialect
            , not (isGatewayLoadedAuth loaded)
            , inferredTarget.targetConnectionId
                == builtinConnectionId OpenAIProvider
            ]
        allTools =
            coding.codingAppTools
                ++ extraTools
                ++ mcpTools
                ++ sessionTools
                ++ gatewayTools
                ++ databaseAppTools
                ++ learnedSkillAppTools
                ++ imageGenerationTools
                ++ computerTools
        tools =
            filterGhciTools options.optGhci
                (filterBashTools options.optBash coding.codingAppTools)
                ++ extraTools
                ++ mcpTools
                ++ sessionTools
                ++ gatewayTools
                ++ databaseAppTools
                ++ learnedSkillAppTools
                ++ imageGenerationTools
        planMode = coding.codingPlanMode
        resumedPlanPending =
            case resumed of
                Just (_, turns) ->
                    resumedPlanNeedsApproval
                        (map (.turnAssistantText) turns)
                Nothing -> False
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
    when resumedPlanPending (activatePlanMode planMode)
    runAgentSession
        loaded
        learnAboutUserRequested
        sessionTmp
        activeAccountIdRef
        activeAccountRef
        activeSelectionRef
        agentTypesRef
        allTools
        (recordImageGenerationImages imageGenerationHistory)
        (clearImageGenerationHistory imageGenerationHistory)
        bashEnabledRef
        catalog
        gatewayModelsRef
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
        allowedChildModels
        resolveCollaborationChildModel
        childModelAllowed
        home
        inferredTarget
        interrupt
        learnedSkillAppTools
        legacySubagentTarget
        mcpFleet
        mcpInstructions
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
        root
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
