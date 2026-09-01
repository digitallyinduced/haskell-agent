module Agent.CLI.Runtime.Orchestration.Session (runAgentSession) where

import Agent.CLI.AccountPicker ()
import Agent.CLI.AccountSelection ()
import Agent.CLI.Afk ()
import Agent.CLI.AgentSessions ()
import Agent.CLI.AgentViewport ()
import Agent.CLI.Approval ()
import Agent.CLI.Artifact ()
import Agent.CLI.Auth
    ( LoadedAuth(loadedTokenProvider)
    , isGatewayLoadedAuth
    )
import Agent.CLI.Clipboard ()
import Agent.CLI.Claude (newClaudeSessionRuntimeSlot)
import Agent.CLI.CodeModeRuntime
    ( CodeModeSessionRuntime(..),
      CodexCatalogSession(..),
      codeModeSessionRuntimeFor,
      imageGenerationCodeModeRuntimeFor,
      loadCodexCatalogModelInfo )
import Agent.CLI.Command ()
import Agent.CLI.Compaction
    ( CompactionInstall(CompactionNotInstalled) )
import Agent.CLI.Config ()
import Agent.CLI.Connectivity ()
import Agent.CLI.Database ()
import Agent.CLI.Database.Store (DatabaseScopes)
import Agent.CLI.Dialects (CodingTools(..))
import Agent.CLI.Error ()
import Agent.CLI.GatewayClient (GatewayCredential, GatewayModelAccess)
import Agent.CLI.GatewayBridge ()
import Agent.CLI.Input ()
import Agent.CLI.Interrupt
    ( InterruptState
    , catchUserInterrupt
    , retryUserInterruptOnce
    , withCtrlCHandler
    )
import Agent.CLI.LearnedSkills ()
import Agent.CLI.LearnedSkills.Store ()
import Agent.CLI.Login ()
import Agent.CLI.Lsp ()
import Agent.CLI.ManagedTurn ( ManagedTurnRequest(..) )
import Agent.CLI.McpManager ()
import Agent.CLI.McpStatus ()
import Agent.CLI.ModelConfig
    (ModelCatalog, catalogContextWindowForTransport)
import Agent.CLI.Models (ModelTarget(targetConnectionId))
import Agent.CLI.Options
    ( ApprovalPolicy
    , isOneShot
    , CliOptions(optCodeMode)
    )
import Agent.CLI.PendingInputs (PendingInputs)
import Agent.CLI.Plan ()
import Agent.CLI.Project ( ModelSwitchScope(..) )
import Agent.CLI.Prompt
    ( codexEnvironmentContext,
      subscriptionSubagentModelGuidance,
      appendMcpInstructions,
      systemPromptForCatalogModel,
      systemPromptForTools )
import Agent.CLI.PromptHooks ()
import Agent.CLI.Provider.OpenAI ()
import Agent.CLI.Provider.Switch ()
import Agent.CLI.ProviderAvailability ( probeLoadedAvailability )
import Agent.CLI.ProviderFallback ( isProviderUnavailable )
import Agent.CLI.ProviderTransition (PendingTurn, ProviderTransition)
import Agent.CLI.Recap ()
import Agent.CLI.Render ( putTextLn )
import Agent.CLI.ReplMode ()
import Agent.CLI.Request
    ( requestParams
    , setRequestInstructionsAndTools
    , setRequestPromptCacheKey
    )
import Agent.CLI.Resume ( resumeNeedsGeneratedContext )
import Agent.CLI.Runtime.HistorySource ()
import Agent.CLI.Runtime.Orchestration.Background ()
import Agent.CLI.Runtime.Orchestration.Concurrent ()
import Agent.CLI.Runtime.Orchestration.Providers
    ( runAgentProviders )
import Agent.CLI.Runtime.Orchestration.Restart ()
import Agent.CLI.Runtime.Orchestration.Startup
    ( clearNativeProgress, mcpToolCollision, reportStartupWarning )
import Agent.CLI.Runtime.Orchestration.Types ()
import Agent.CLI.Runtime.Persistence ()
import Agent.CLI.Runtime.Recap ()
import Agent.CLI.Runtime.Repl ()
import Agent.CLI.Runtime.Types ( RunResult(RunQuit) )
import Agent.CLI.Secret ()
import Agent.CLI.Session
    ( addSessionUsage,
      ensureSession,
      ensurePersistenceSessionId,
      compatibleSessionPromptSnapshot,
      resumeHint,
      sessionTitleFromPrompt,
      sessionUsageFromTurns,
      Persistence(..),
      PersistenceState(PersistenceActive, PersistencePending),
      LegacySubagentTarget,
      SessionHandle(sessionDir),
      SessionMeta(metaId, metaLastResponseId, metaPromptSnapshot, metaTitle),
      SessionTurn,
      SessionPromptSnapshot(..) )
import Agent.CLI.Session.Attachments ()
import Agent.CLI.Session.Choices ()
import Agent.CLI.Session.History
    ( currentLiveTranscriptGeneration,
      durableTranscriptCheckpoint,
      evictLiveTranscript,
      foldSessionItems,
      readLiveTranscript,
      replaceLiveConversation )
import Agent.CLI.Session.Lifecycle ()
import Agent.CLI.Session.Runtime.Types
    ( SessionRequest(codexCatalogSession, SessionRequest, catalog,
                     gatewayModelsRef, modelInfo,
                     connectionId, options, provider, dialect, policyRef, allTools,
                     claudeRuntimeSlot, claudeBridgeTools,
                     recordImageGenerationInputs, clearImageGenerationHistory,
                     suspendGhci, resetToolSessionTemp, grokRuntime,
                     mcpRegistrations, mcpWarnings,
                     mcpInstructions, mcpFleet,
                     ghciEnabledRef, bashEnabledRef, toolEnv, planMode, startup,
                     learnAboutUserRequested, databaseScopes, promptRequest,
                     pendingTurn, unavailableProviders, startupUnavailable, paramsRef,
                     conversationRef, needsInitialContext, queueInitialContext,
                     initialGrokContext, persist,
                     contextOccupancyRef, currentContextWindow,
                     startupWindowTitle, automaticCompactionRef,
                     projectRoot, home, cwd, tokenProvider, openAiPool, startupContext,
                     automaticCompactionHookRef, skillsRef, skillInvocationsRef,
                     escPaused, interrupt, multiCtx, rootTurnRef, subagentSessions,
                     pendingNotices, storeRoot, agentTypes, legacyTarget, usageRef,
                     accountRef, accountIdRef, selectionRef, accountLabel,
                     selectAccount, onPersisted, compactRunner, codeModeNestedSlot),
      StartupRuntime(startupBackground, startupDatabaseStore,
                     startupSessionState) )
import Agent.CLI.Session.Selection ( reservedSessionId )
import Agent.CLI.SessionAdmin ()
import Agent.CLI.SessionEnv ()
import Agent.CLI.SessionLock ()
import Agent.CLI.SessionState ( SessionState(sessionConversation) )
import Agent.CLI.SessionTitle ()
import Agent.CLI.Skills ()
import Agent.CLI.Startup.Auth ( markStartupStage, startupDie )
import Agent.CLI.StartupContext
    ( AgentsContextNotice(..), loadAgentsContext )
import Agent.CLI.Style ( cliWindowTitle, roleMuted )
import Agent.CLI.Subagents.Runtime
    ( SubagentRuntime(subagentOpenAiChild, SubagentRuntime,
                      subagentOptions, subagentGhciEnabled, subagentBashEnabled,
                      subagentPolicy, subagentPlanHooks, subagentSkillRoots,
                      subagentAllowedRoots, subagentRootAccessRequest,
                      subagentParams, subagentMcpTools, subagentRegistry,
                      subagentSessions, subagentStoreRoot, subagentTypes,
                      subagentLegacyTarget, subagentConnection, subagentMapModel,
                      subagentCreateWorktree, subagentSessionTmp,
                      subagentSpawnModelGuidance, subagentAllowedChildModels,
                      subagentResolveChildModel, subagentChildModelAllowed) )
import Agent.CLI.Subagents.Runtime.Types
    (SubagentSession, SubagentStoreRoot)
import Agent.CLI.TUI.App
    ( FullscreenRuntime, withFullscreenSuspended )
import Agent.CLI.TUI.History ()
import Agent.CLI.TUI.SessionHistory ()
import Agent.CLI.Terminal ( resolveColor )
import Agent.CLI.Tools
    ( schemasFromAppTools, schemasFromAppToolsCodeMode )
import Agent.CLI.Turn ()
import Agent.CLI.Usage ()
import Agent.CLI.WebFetch ()
import Agent.CLI.Worktree ()
import Agent.Cancel ()
import Agent.Claude ()
import Agent.Dialect (Dialect, dialectId)
import Agent.Error (ApiError)
import Agent.GrokBuild.Dialect.Goal ()
import Agent.GrokBuild.Dialect.Runtime ()
import Agent.GrokBuild.Dialect.Task (GrokSubagentSpecs)
import Agent.GrokBuild.Dialect.Workflow ()
import Agent.Loop (ImageAttachment, addTokenUsage, emptyTokenUsage)
import Agent.OpenAI.Compaction ()
import Agent.OpenAI.ImageGeneration ( imageGenerationToolName )
import Agent.OpenAI.Usage ()
import Agent.OpenAI.WebSocketClient ()
import Agent.OpenAI.Models.Types ( resolvedContextWindow )
import Agent.OpenRouter.LoopBackend ()
import Agent.OpenRouter.Options (ClientOptions)
import Agent.OsPath ()
import Agent.Provider
    (Credential, Provider(OpenAIProvider), TokenProvider,
     tokenProviderBillingMode)
import Agent.ReasoningEffort ()
import Agent.Responses.GenericBackend ()
import Agent.Responses.GenericClient (GenericClientOptions)
import Agent.Responses.Types
    (ResponseItem, ResponseCreateParams(model))
import Agent.Skills (SkillCatalog, SkillInvocation)
import Agent.Store.Postgres ( trustedPool )
import Agent.Store.Types ()
import Agent.Subagents (SubagentRegistry)
import Agent.Subagents.Types (RootTurnId, SubagentId)
import Agent.Subagents.TaskPath ()
import Agent.TUI.Model ()
import Agent.TUI.Motion ()
import Agent.Tools.MultiAgents
    (CollaborationModelTarget, MultiAgentContext, SubagentWorktree)
import Agent.Tools.PlanMode (PlanModeEnv, PlanModeHooks)
import Agent.Tools.Secret ()
import Agent.ToolDispatch (canonicalToolName)
import Agent.Tools.Types
    ( AppTool(..)
    , ToolSchema(..)
    , ToolEnv(toolAllowedRoots, toolRootAccessRequest, toolSkillRoots, toolSessionTmp)
    )
import Agent.XAI.LoopBackend ()
import Control.Applicative ( (<|>) )
import Control.Concurrent.Async ( waitSTM, withAsync )
import Control.Concurrent.Chan ()
import Control.Concurrent.MVar ()
import Control.Concurrent.STM ( retry )
import Control.Exception ()
import Control.Exception.Safe ( mask_, finally )
import Control.Monad ( forM_, void, when )
import Data.Functor ( (<&>) )
import Data.IORef
    ( IORef,
      modifyIORef',
      newIORef,
      readIORef,
      writeIORef )
import Data.List ()
import Data.Map.Strict (Map)
import Data.Maybe ( isJust, isNothing, fromMaybe )
import Data.Set (Set)
import Data.Text (Text)
import Data.Time.Clock ( getCurrentTime, utctDay )
import System.Console.ANSI ()
import System.Console.ANSI.Codes ()
import System.Directory.OsPath ()
import System.Environment ( getProgName )
import System.Exit ()
import System.IO (Handle, stderr)
import System.Mem ( performMajorGC )
import System.OsPath (OsPath)
import qualified Data.ByteString as BS ()
import qualified Agent.Responses.GenericClient as GenericResponses
    ()
import qualified Agent.MCP as MCP
import qualified Data.Map.Strict as Map ()
import qualified Agent.OpenAI.Auth as OpenAI ()
import qualified Agent.OpenRouter as OpenRouter ()
import qualified Agent.OpenRouter.Usage as OpenRouterUsage ()
import qualified Agent.Provider as Provider ()
import qualified Agent.CLI.Session.Lifecycle as SessionLifecycle ()
import qualified Agent.CLI.Session.Runner as SessionRunner ()
import qualified Data.Set as Set ()
import qualified Data.Text as Text ( unpack )
import qualified Data.Text.IO as Text ( hPutStr )
import qualified Agent.XAI.Options as XAI ()
import qualified Agent.XAI.Client as XAIClient ()
import qualified Agent.XAI.Request as XAIRequest ()
import qualified Agent.XAI.Usage as XAIUsage ()

runAgentSession
    :: LoadedAuth
    -> Maybe GatewayCredential
    -> Bool
    -> OsPath
    -> IORef Text
    -> IORef Text
    -> IORef Text
    -> GrokSubagentSpecs
    -> [AppTool]
    -> ([ImageAttachment] -> IO ())
    -> IO ()
    -> IORef Bool
    -> ModelCatalog
    -> IORef (Maybe GatewayModelAccess)
    -> Bool
    -> (SessionHandle -> IO ())
    -> Bool
    -> IO closeResult
    -> IORef (IO ())
    -> CodingTools
    -> (OsPath -> IO (Either Text SubagentWorktree))
    -> Maybe GenericClientOptions
    -> OsPath
    -> [AppTool]
    -> DatabaseScopes
    -> Dialect
    -> Text
    -> IORef Bool
    -> [AppTool]
    -> Maybe FullscreenRuntime
    -> [AppTool]
    -> IORef Bool
    -> Maybe [Text]
    -> Maybe (Text -> IO (Maybe CollaborationModelTarget))
    -> Maybe (Text -> IO Bool)
    -> OsPath
    -> ModelTarget
    -> InterruptState
    -> [AppTool]
    -> Maybe LegacySubagentTarget
    -> MCP.McpFleet
    -> [(Text, Text)]
    -> [AppTool]
    -> Text
    -> Maybe MultiAgentContext
    -> (OsPath -> IO ())
    -> ClientOptions
    -> Maybe TokenProvider
    -> CliOptions
    -> PendingInputs
    -> Maybe PendingTurn
    -> Persistence
    -> PlanModeHooks
    -> PlanModeEnv
    -> ApprovalPolicy
    -> IORef (Maybe Text)
    -> OsPath
    -> Maybe ManagedTurnRequest
    -> Provider
    -> Bool
    -> SubagentRegistry
    -> (Credential -> IO Text)
    -> Bool
    -> Maybe (SessionMeta, [SessionTurn])
    -> OsPath
    -> IORef (Maybe RootTurnId)
    -> (Text -> IO (Either ApiError Text))
    -> TokenProvider
    -> [AppTool]
    -> (Text -> IO titleResult)
    -> IORef [SkillInvocation]
    -> IORef SkillCatalog
    -> StartupRuntime
    -> FilePath
    -> Handle
    -> IORef (Maybe (IO [ResponseItem]))
    -> IORef (Map SubagentId SubagentSession)
    -> SubagentStoreRoot
    -> TokenProvider
    -> ToolEnv
    -> [AppTool]
    -> Maybe ProviderTransition
    -> (Text -> Text)
    -> Set Provider
    -> IO RunResult
runAgentSession
    loaded
    connectedGateway
    learnAboutUserRequested
    sessionTmp
    activeAccountIdRef
    activeAccountRef
    activeSelectionRef
    agentTypesRef
    allTools
    recordImageGenerationInputs
    clearImageGenerationHistory
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
    resolveChildModel
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
    = do
    flip finally closeAll do
        case
                mcpToolCollision
                    ( coding.codingAppTools
                        ++ extraTools
                        ++ sessionTools
                        ++ gatewayTools
                        ++ databaseAppTools
                        ++ learnedSkillAppTools
                    )
                    mcpFleet.mcpFleetRegistrations
            of
                Just err ->
                    startupDie startup
                        ("Failed to initialize MCP tools: " <> Text.unpack err)
                Nothing -> pure ()
        today <- utctDay <$> getCurrentTime
        -- Catalog models provide the per-model instructions template. Full
        -- code mode remains opt-in, but code_mode_only models still route the
        -- reserved image-generation tool through exec because Responses Lite
        -- rejects its direct namespace. The offline lookup never blocks
        -- startup on the network.
        codexModelInfo <-
            loadCodexCatalogModelInfo
                stateDirectory
                provider
                dialect
                (if isGatewayLoadedAuth loaded
                    then Nothing
                    else Just selectableTokenProvider)
                model
        let initializeCodeMode =
                if options.optCodeMode
                    then codeModeSessionRuntimeFor codexModelInfo tools
                    else imageGenerationCodeModeRuntimeFor codexModelInfo tools
            codeModeFallbackWarning
                | options.optCodeMode =
                    "code mode unavailable; falling back to compatible \
                    \direct tools: "
                | otherwise =
                    "image generation code mode unavailable; \
                    \disabling image generation: "
        (codeModeRuntime, suppressDirectImageGeneration) <-
            initializeCodeMode >>= \case
                Left err -> do
                    reportStartupWarning startup
                        (codeModeFallbackWarning <> err)
                    pure (Nothing, True)
                Right runtime -> pure (runtime, False)
        writeIORef codeModeCloseRef
            (maybe (pure ()) (.codeModeClose) codeModeRuntime)
        sessionId <- reservedSessionId persist
        let providerTools
                | suppressDirectImageGeneration =
                    filter
                        ((/= imageGenerationToolName) . (.appToolName))
                        tools
                | otherwise = tools
            catalogSession = codexModelInfo <&> \info ->
                CodexCatalogSession
                    { catalogInstructionsFor = \toolNames sessionTmpDir ->
                        systemPromptForCatalogModel
                            dialect
                            info
                            toolNames
                            sessionTmpDir
                    , catalogEnvironmentContext =
                        codexEnvironmentContext cwd today Nothing Nothing
                    }
            instructions =
                appendMcpInstructions mcpInstructions case catalogSession of
                    Just catalog ->
                        catalog.catalogInstructionsFor
                            (map (.appToolName) providerTools)
                            (Just sessionTmp)
                    Nothing ->
                        systemPromptForTools
                            dialect
                            (map (.appToolName) providerTools)
                            cwd
                            (Just sessionTmp)
                            today
                            (isOneShot options)
            wireSchemas = case codeModeRuntime of
                Just codeMode ->
                    schemasFromAppToolsCodeMode
                        dialect
                        ( codeMode.codeModeWireTools
                            <> codeMode.codeModeDirectTools
                        )
                Nothing -> schemasFromAppTools dialect providerTools
            environmentContextBlock =
                (.catalogEnvironmentContext) <$> catalogSession
            registryTools =
                allTools
                    <> maybe [] (.codeModeWireTools) codeModeRuntime
            baseParams = requestParams provider model instructions
                wireSchemas effortText
            compatiblePromptSnapshot =
                compatibleSessionPromptSnapshot
                    provider
                    inferredTarget.targetConnectionId
                    (dialectId dialect)
                    cwd
                    sessionId
                    baseParams
                    (resumed >>= \(meta, _) -> meta.metaPromptSnapshot)
            params = case compatiblePromptSnapshot of
                Just snapshot ->
                    setRequestPromptCacheKey
                        snapshot.promptSnapshotCacheKey
                        (setRequestInstructionsAndTools
                            snapshot.promptSnapshotInstructions
                            (Just snapshot.promptSnapshotTools)
                            baseParams)
                Nothing ->
                    maybe baseParams
                        (`setRequestPromptCacheKey` baseParams)
                        sessionId
            initialItems = maybe [] (foldSessionItems . snd) resumed
            initialTurns = maybe [] snd resumed
            resumeNeedsFreshContext =
                resumeNeedsGeneratedContext initialTurns
            initialPrevious = case transition of
                Just _ -> Nothing
                Nothing
                    | resumeTargetChanged -> Nothing
                    | otherwise ->
                        resumed >>= \(meta, _) -> meta.metaLastResponseId
            needsInitialContext =
                resumeNeedsFreshContext
                    || (null initialTurns && isNothing initialPrevious)
            restoredInitialPromptSnapshot
                | null initialTurns && isNothing initialPrevious =
                    compatiblePromptSnapshot
                | otherwise = Nothing
            queueInitialContext =
                needsInitialContext && isNothing restoredInitialPromptSnapshot
        paramsRef <- newIORef params
        policyRef <- newIORef policy
        claudeRuntimeSlot <- newClaudeSessionRuntimeSlot
        let claudeBridgeTools =
                filter isClaudeBridgeTool allTools
            isClaudeBridgeTool tool =
                canonicalToolName tool.appToolName
                    `notElem`
                        [ "shell_command", "run_terminal_cmd"
                        , "read_file", "write_file", "grep", "glob"
                        , "search_replace", "apply_patch", "write_stdin"
                        , "web_fetch", "web_search"
                        ]
                    && case tool.appToolSchema of
                        JsonFunctionSchema{} -> True
                        RawJsonFunctionSchema{} -> True
                        _ -> False
        automaticCompactionRef <- newIORef Nothing
        automaticCompactionHookRef <-
            newIORef
                (\_outcome _inputs -> pure CompactionNotInstalled)
        let currentModelContextWindow mapTransportModel = do
                currentParams <- readIORef paramsRef
                pure $
                    catalogContextWindowForParams
                        mapTransportModel
                        currentParams
            catalogContextWindowForParams mapTransportModel params = do
                currentModel <- params.model
                catalogContextWindowForTransport
                    catalog
                    inferredTarget.targetConnectionId
                    currentModel
                    (mapTransportModel currentModel)
            contextWindowForParams mapTransportModel fallback params =
                fromMaybe fallback
                    (catalogContextWindowForParams mapTransportModel params)
            subagentRuntime = SubagentRuntime
                { subagentOptions = options
                , subagentGhciEnabled = ghciEnabledRef
                , subagentBashEnabled = bashEnabledRef
                , subagentPolicy = policyRef
                , subagentPlanHooks = planHooks
                , subagentSkillRoots = toolEnv.toolSkillRoots
                , subagentAllowedRoots = toolEnv.toolAllowedRoots
                , subagentRootAccessRequest = toolEnv.toolRootAccessRequest
                , subagentParams = paramsRef
                , subagentMcpTools = mcpTools
                , subagentRegistry = registry
                , subagentSessions = subagentSessions
                , subagentStoreRoot = subagentStoreRoot
                , subagentTypes = agentTypesRef
                , subagentLegacyTarget = legacySubagentTarget
                , subagentConnection = inferredTarget.targetConnectionId
                , subagentMapModel = transportModel
                , subagentCreateWorktree = Just createSubagentWorktree
                , subagentSessionTmp = toolEnv.toolSessionTmp
                , subagentSpawnModelGuidance =
                    if provider == OpenAIProvider
                        && isJust allowedChildModels
                        then Nothing
                        else
                            subscriptionSubagentModelGuidance
                                provider
                                (tokenProviderBillingMode tokenProvider)
                , subagentAllowedChildModels = allowedChildModels
                , subagentResolveChildModel = resolveChildModel
                , subagentChildModelAllowed = childModelAllowed
                , subagentOpenAiChild = openaiChild
                }
        let conversationRef = startup.startupSessionState.sessionConversation
        void $
            replaceLiveConversation
                conversationRef initialPrevious initialItems
        contextTokensRef <- newIORef Nothing
        writeIORef subagentForkSource (Just (readLiveTranscript conversationRef))
        let titleHint = case resumed of
                Just (meta, _) -> Just meta.metaTitle
                Nothing ->
                    fmap (\request -> sessionTitleFromPrompt request.managedTurnText)
                        promptRequest
            startupWindowTitle = cliWindowTitle cwd titleHint
        _ <- setWindowTitle startupWindowTitle
        markStartupStage startup "Loading instructions…"
        let agentsContextNotice
                | isNothing resumed && isNothing transition =
                    ReportAgentsContextLoaded
                | otherwise =
                    SuppressAgentsContextLoaded
        startupContext <- case restoredInitialPromptSnapshot of
            Just snapshot ->
                newIORef snapshot.promptSnapshotGeneratedContext
            Nothing ->
                loadAgentsContext
                    stderrHandle
                    fullscreen
                    agentsContextNotice
                    options
                    dialect
                    home
                    cwd
                    (if refreshDialectContext || resumeNeedsFreshContext
                        then []
                        else initialItems)
                    (if refreshDialectContext || resumeNeedsFreshContext
                        then Nothing
                        else initialPrevious)
                    environmentContextBlock
        -- Fullscreen sessions load skills after Brick has taken over the
        -- terminal, so filesystem discovery cannot delay the first frame.
        -- Minimal and one-shot sessions still initialize them synchronously
        -- before their first prompt/turn below.
        usageRef <- newIORef $ case resumed of
            Just (meta, turns) -> sessionUsageFromTurns meta turns
            Nothing -> emptyTokenUsage
        forM_ resumed \(meta, _) -> do
            generation <-
                currentLiveTranscriptGeneration conversationRef
            evicted <-
                evictLiveTranscript
                    conversationRef
                    generation
                    (durableTranscriptCheckpoint
                        (trustedPool startup.startupDatabaseStore)
                        root
                        meta.metaId)
            when evicted performMajorGC
        let recordCompactionUsage usage =
                when (usage /= emptyTokenUsage) $
                    mask_ do
                        case persist of
                            PersistenceDisabled -> pure ()
                            PersistenceEnabled slotRef -> do
                                handle <- ensureSession slotRef
                                claimCurrentSession handle
                                updated <- addSessionUsage usage handle
                                writeIORef slotRef (PersistenceActive updated)
                        modifyIORef' usageRef (`addTokenUsage` usage)
        case persist of
            PersistenceEnabled slotRef -> do
                slot <- readIORef slotRef
                case slot of
                    PersistenceActive handle -> do
                        claimCurrentSession handle
                        noteSessionDir handle.sessionDir
                    PersistencePending _ _ _ -> pure ()
            PersistenceDisabled -> pure ()
        progName <- getProgName
        markStartupStage startup "Connecting to provider…"
        let runWithInterruptHandling action
                | startup.startupBackground = action
                | otherwise =
                    withCtrlCHandler interrupt $
                        withResumeHintOnQuit
                            fullscreen progName persist action
        runWithInterruptHandling do
                let shouldProbeAtStartup =
                        checkStartupUsageInBackground
                            && isNothing promptRequest
                    sessionRequest
                        startupUnavailable
                        sessionTokenProvider
                        sessionOpenAiPool
                        sessionSelectAccount
                        sessionContextWindow
                        sessionCompactRunner =
                            SessionRequest
                                { catalog
                                , gatewayModelsRef
                                , claudeRuntimeSlot
                                , claudeBridgeTools
                                , modelInfo = codexModelInfo
                                , connectionId =
                                    inferredTarget.targetConnectionId
                                , options
                                , provider
                                , dialect
                                , policyRef
                                , allTools = registryTools
                                , recordImageGenerationInputs
                                , clearImageGenerationHistory
                                , suspendGhci = coding.codingSuspendGhci
                                , resetToolSessionTemp =
                                    coding.codingResetSessionTemp
                                , grokRuntime = coding.codingGrokRuntime
                                , mcpRegistrations =
                                    mcpFleet.mcpFleetRegistrations
                                , mcpWarnings = mcpFleet.mcpFleetWarnings
                                , mcpInstructions
                                , mcpFleet = Just mcpFleet
                                , ghciEnabledRef
                                , bashEnabledRef
                                , toolEnv
                                , planMode
                                , startup
                                , learnAboutUserRequested
                                , databaseScopes
                                , promptRequest
                                , pendingTurn
                                , unavailableProviders
                                , startupUnavailable
                                , paramsRef
                                , conversationRef
                                , contextOccupancyRef = contextTokensRef
                                , currentContextWindow = do
                                    configured <- sessionContextWindow
                                    pure $
                                        configured
                                            <|> (resolvedContextWindow
                                                =<< codexModelInfo)
                                , automaticCompactionRef
                                , needsInitialContext
                                , queueInitialContext
                                , initialGrokContext =
                                    restoredInitialPromptSnapshot
                                        >>= (.promptSnapshotGrokContext)
                                , persist
                                , startupWindowTitle
                                , projectRoot
                                , home
                                , cwd
                                , tokenProvider = sessionTokenProvider
                                , openAiPool = sessionOpenAiPool
                                , startupContext
                                , automaticCompactionHookRef
                                , skillsRef
                                , skillInvocationsRef
                                , escPaused
                                , interrupt
                                , multiCtx
                                , rootTurnRef
                                , subagentSessions
                                , pendingNotices
                                , storeRoot = subagentStoreRoot
                                , agentTypes = agentTypesRef
                                , legacyTarget = legacySubagentTarget
                                , usageRef
                                , accountRef = activeAccountRef
                                , accountIdRef = activeAccountIdRef
                                , selectionRef = activeSelectionRef
                                , accountLabel = resolveActiveAccountLabel
                                , selectAccount = sessionSelectAccount
                                , onPersisted = claimCurrentSession
                                , compactRunner = sessionCompactRunner
                                , codeModeNestedSlot =
                                    (.codeModeNestedSlot) <$> codeModeRuntime
                                , codexCatalogSession = catalogSession
                                }
                    withStartupAvailability action
                        | shouldProbeAtStartup
                        , not (isGatewayLoadedAuth loaded) =
                            withAsync
                                (probeLoadedAvailability
                                    loaded
                                        { loadedTokenProvider =
                                            tokenProvider
                                        })
                                \availability -> do
                                    let startupUnavailable =
                                            waitSTM availability >>= \case
                                                Left err
                                                    | isProviderUnavailable err ->
                                                        pure err
                                                _ -> retry
                                    action (Just startupUnavailable)
                        | otherwise = action Nothing
                withStartupAvailability \startupUnavailable ->
                    runAgentProviders
                        (if startup.startupBackground
                            then SessionLocalSwitch
                            else TopLevelSwitch)
                        loaded
                        connectedGateway
                        sessionRequest
                        activeAccountIdRef
                        activeAccountRef
                        activeSelectionRef
                        catalog
                        claudeBypassEnabled
                        contextTokensRef
                        contextWindowForParams
                        conversationRef
                        currentModelContextWindow
                        customGenericOptions
                        cwd
                        dialect
                        fullscreen
                        automaticCompactionHookRef
                        home
                        initialPrevious
                        model
                        multiCtx
                        openRouterOptions
                        options
                        params
                        paramsRef
                        pendingNotices
                        persist
                        preferredOpenAiAccountRef
                        projectRoot
                        provider
                        recordCompactionUsage
                        resolveActiveAccountLabel
                        selectHttpAccount
                        selectableTokenProvider
                        shouldProbeAtStartup
                        startup
                        startupUnavailable
                        stderrHandle
                        subagentRuntime
                        tokenProvider
                        transition
                        transportModel
                        unavailableProviders

-- | Print a copy-pasteable --resume line whenever the CLI session quits.
-- Ctrl-C is normalized to the same graceful 'RunQuit' result as :q/Ctrl-D so
-- every exit path reports the persisted session exactly once.
withResumeHintOnQuit
    :: Maybe FullscreenRuntime
    -> String
    -> Persistence
    -> IO RunResult
    -> IO RunResult
withResumeHintOnQuit fullscreen progName persist action = do
    result <- catchUserInterrupt action (pure RunQuit)
    case result of
        RunQuit -> do
            case fullscreen of
                Nothing -> printResumeHint progName persist
                Just runtime ->
                    withFullscreenSuspended runtime
                        (printResumeHint progName persist)
        _ -> pure ()
    -- An interrupt is the requested, graceful end of the CLI session.
    -- Returning lets the surrounding brackets restore the SIGINT handler
    -- and close tools without GHC's top-level exception handler printing
    -- "user interrupt" and a backtrace.
    pure result

printResumeHint
    :: String
    -> Persistence
    -> IO ()
printResumeHint progName persist = do
    -- A commit interrupted before publication is safe to adopt once. Keep the
    -- retry bounded so a later double Ctrl-C can still force a hung exit.
    sessionId <- retryUserInterruptOnce (ensurePersistenceSessionId persist)
    case sessionId of
        Nothing -> pure ()
        Just sessionId -> do
            -- Drop an in-place "Thinking…" status so the hint is its own line.
            Text.hPutStr stderr "\r\ESC[K"
            clearNativeProgress stderr
            color <- resolveColor stderr
            putTextLn stderr
                (roleMuted color (resumeHint progName sessionId))
