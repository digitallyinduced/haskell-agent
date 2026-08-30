module Agent.CLI.Runtime.Orchestration.Session (runAgentSession) where

import Agent.CLI.AccountPicker ()
import Agent.CLI.AccountSelection ()
import Agent.CLI.Afk ()
import Agent.CLI.AgentSessions ()
import Agent.CLI.AgentViewport ()
import Agent.CLI.Approval ()
import Agent.CLI.Artifact ()
import Agent.CLI.Auth
    ( LoadedAuth(loadedOpenAiPool, loadedTokenProvider) )
import Agent.CLI.Clipboard ()
import Agent.CLI.CodeModeRuntime
    ( CodeModeSessionRuntime(..),
      CodexCatalogSession(..),
      codeModeSessionRuntimeFor,
      loadCodexCatalogModelInfo )
import Agent.CLI.Command ()
import Agent.CLI.Compaction
    ( CompactionInstall(CompactionNotInstalled) )
import Agent.CLI.Config ()
import Agent.CLI.Connectivity ()
import Agent.CLI.Database ()
import Agent.CLI.Database.Store ()
import Agent.CLI.Dialects ()
import Agent.CLI.Error ()
import Agent.CLI.GatewayBridge ()
import Agent.CLI.Input ()
import Agent.CLI.Interrupt ( catchUserInterrupt, withCtrlCHandler )
import Agent.CLI.LearnedSkills ()
import Agent.CLI.LearnedSkills.Store ()
import Agent.CLI.Login ()
import Agent.CLI.Lsp ()
import Agent.CLI.ManagedTurn ( ManagedTurnRequest(..) )
import Agent.CLI.McpManager ()
import Agent.CLI.McpStatus ()
import Agent.CLI.ModelConfig ( catalogContextWindowForTransport )
import Agent.CLI.Models ()
import Agent.CLI.Options
    ( isOneShot
    , CliOptions(optCodeMode, optShowRawReasoning, optCompactThreshold)
    )
import Agent.CLI.PendingInputs ()
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
import Agent.CLI.ProviderTransition ()
import Agent.CLI.Recap ()
import Agent.CLI.Render ( putTextLn )
import Agent.CLI.ReplMode ()
import Agent.CLI.Request ( requestParams )
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
      resumeHint,
      sessionTitleFromPrompt,
      sessionUsageFromTurns,
      Persistence(..),
      PersistenceState(PersistenceActive, PersistencePending),
      SessionHandle(sessionMeta, sessionDir),
      SessionMeta(metaId, metaLastResponseId, metaTitle) )
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
    ( SessionRequest(codexCatalogSession, SessionRequest, catalog, modelInfo,
                     connectionId, options, provider, dialect, policy, allTools,
                     suspendGhci, grokRuntime, mcpRegistrations, mcpWarnings,
                     mcpInstructions, mcpFleet,
                     ghciEnabledRef, bashEnabledRef, toolEnv, planMode,
                     interactionDeliveriesRef, startup,
                     learnAboutUserRequested, databaseScopes, promptRequest,
                     pendingTurn, unavailableProviders, startupUnavailable, paramsRef,
                     conversationRef, needsInitialContext, persist,
                     startupWindowTitle, automaticCompactionRef,
                     projectRoot, home, cwd, tokenProvider, openAiPool, startupContext,
                     automaticCompactionHookRef, skillsRef, skillInvocationsRef,
                     escPaused, interrupt, multiCtx, rootTurnRef, subagentSessions,
                     pendingNotices, storeRoot, agentTypes, legacyTarget, usageRef,
                     accountRef, accountIdRef, selectionRef, accountLabel,
                     selectAccount, onPersisted, compactRunner, codeModeNestedSlot),
      StartupRuntime(startupBackground, startupDatabaseStore,
                     startupSessionState) )
import Agent.CLI.Session.Selection ()
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
                      subagentSpawnModelGuidance, subagentAllowedChildModels) )
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
import Agent.Dialect ()
import Agent.Error ()
import Agent.GrokBuild.Dialect.Goal ()
import Agent.GrokBuild.Dialect.Runtime ()
import Agent.GrokBuild.Dialect.Task ()
import Agent.GrokBuild.Dialect.Workflow ()
import Agent.Loop ( addTokenUsage, emptyTokenUsage )
import Agent.OpenAI.Compaction ()
import Agent.OpenAI.Usage ()
import Agent.OpenAI.WebSocketClient ()
import Agent.OpenRouter.LoopBackend ()
import Agent.OsPath ()
import Agent.Provider ( tokenProviderBillingMode )
import Agent.ReasoningEffort ()
import Agent.Responses.GenericBackend ()
import Agent.Responses.GenericClient ()
import Agent.Responses.Types ( ResponseCreateParams(model) )
import Agent.Skills ()
import Agent.Store.Postgres ( trustedPool )
import Agent.Store.Types ()
import Agent.Subagents ()
import Agent.Subagents.TaskPath ()
import Agent.TUI.Model ()
import Agent.TUI.Motion ()
import Agent.Tools.MultiAgents
    ( MultiAgentContext(multiSendToRoot, multiRegistry) )
import Agent.Tools.PlanMode ()
import Agent.Tools.Secret ()
import Agent.Tools.Types
    ( AppTool(appToolName)
    , ToolEnv(toolAllowedRoots, toolRootAccessRequest, toolSkillRoots, toolSessionTmp)
    )
import Agent.XAI.LoopBackend ()
import Control.Applicative ()
import Control.Concurrent.Async ( waitSTM, withAsync )
import Control.Concurrent.Chan ()
import Control.Concurrent.MVar ()
import Control.Concurrent.STM ( retry )
import Control.Exception ()
import Control.Exception.Safe ( mask_, finally )
import Control.Monad ( forM_, void, when )
import Data.Functor ( (<&>) )
import Data.IORef
    ( modifyIORef',
      newIORef,
      readIORef,
      writeIORef )
import Data.List ()
import Data.Maybe ( isNothing, fromMaybe )
import Data.Text ()
import Data.Time.Clock ( getCurrentTime, utctDay )
import System.Console.ANSI ()
import System.Console.ANSI.Codes ()
import System.Directory.OsPath ()
import System.Environment ( getProgName )
import System.Exit ()
import System.IO ( stderr )
import System.Mem ( performMajorGC )
import System.OsPath ()
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
    interactionDeliveriesRef
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
        mcpInstructions <- MCP.mcpFleetInstructions mcpFleet
        -- Catalog models provide the per-model instructions template. Code
        -- mode is opt-in even when the catalog selects code_mode_only; normal
        -- provider tool calling remains the default. The offline lookup never
        -- blocks startup on the network.
        codexModelInfo <-
            loadCodexCatalogModelInfo
                stateDirectory
                provider
                dialect
                (Just loaded.loadedTokenProvider)
                model
        codeModeRuntime <- if options.optCodeMode
            then codeModeSessionRuntimeFor codexModelInfo tools >>= \case
                Left err -> do
                    reportStartupWarning startup
                        ("code mode unavailable; falling back to direct tools: "
                            <> err)
                    pure Nothing
                Right runtime -> pure runtime
            else pure Nothing
        writeIORef codeModeCloseRef
            (maybe (pure ()) (.codeModeClose) codeModeRuntime)
        let catalogSession = codexModelInfo <&> \info ->
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
                            (map (.appToolName) tools)
                            (Just sessionTmp)
                    Nothing ->
                        systemPromptForTools
                            dialect
                            (map (.appToolName) tools)
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
                Nothing -> schemasFromAppTools dialect tools
            environmentContextBlock =
                (.catalogEnvironmentContext) <$> catalogSession
            registryTools =
                allTools
                    <> maybe [] (.codeModeWireTools) codeModeRuntime
            params = requestParams provider model instructions
                wireSchemas effortText
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
        paramsRef <- newIORef params
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
                , subagentPolicy = policy
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
                    subscriptionSubagentModelGuidance
                        provider
                        (tokenProviderBillingMode tokenProvider)
                , subagentAllowedChildModels = grokAllowedChildModels
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
        setWindowTitle startupWindowTitle
        markStartupStage startup "Loading instructions…"
        let agentsContextNotice
                | isNothing resumed && isNothing transition =
                    ReportAgentsContextLoaded
                | otherwise =
                    SuppressAgentsContextLoaded
        startupContext <-
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
                        noteSessionDir handle.sessionDir
                        claimCurrentSession handle
                    PersistencePending _ _ _ -> pure ()
            PersistenceDisabled -> pure ()
        progName <- getProgName
        markStartupStage startup "Connecting to provider…"
        let runWithInterruptHandling action
                | startup.startupBackground = action
                | otherwise =
                    withCtrlCHandler interrupt $
                        withInterruptResume
                            fullscreen progName persist RunQuit action
        runWithInterruptHandling do
                let shouldProbeAtStartup =
                        checkStartupUsageInBackground
                            && isNothing promptRequest
                    sessionRequest
                        startupUnavailable
                        sessionTokenProvider
                        sessionOpenAiPool
                        sessionSelectAccount
                        sessionCompactRunner =
                            SessionRequest
                                { catalog
                                , modelInfo = codexModelInfo
                                , connectionId =
                                    inferredTarget.targetConnectionId
                                , options
                                , provider
                                , dialect
                                , policy
                                , allTools = registryTools
                                , suspendGhci = coding.codingSuspendGhci
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
                                , interactionDeliveriesRef
                                , startup
                                , learnAboutUserRequested
                                , databaseScopes
                                , promptRequest
                                , pendingTurn
                                , unavailableProviders
                                , startupUnavailable
                                , paramsRef
                                , conversationRef
                                , automaticCompactionRef
                                , needsInitialContext =
                                    resumeNeedsFreshContext
                                        || (null initialTurns
                                            && isNothing initialPrevious)
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
                                , onPersisted = \handle -> do
                                    noteSessionDir handle.sessionDir
                                    claimCurrentSession handle
                                , compactRunner = sessionCompactRunner
                                , codeModeNestedSlot =
                                    (.codeModeNestedSlot) <$> codeModeRuntime
                                , codexCatalogSession = catalogSession
                                }
                    withStartupAvailability action
                        | shouldProbeAtStartup =
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

-- | On Ctrl-C, print a copy-pasteable --resume line when a session exists.
withInterruptResume
    :: Maybe FullscreenRuntime
    -> String
    -> Persistence
    -> a
    -> IO a
    -> IO a
withInterruptResume fullscreen progName persist interrupted action =
    catchUserInterrupt action finishInterrupt
  where
    finishInterrupt = do
        case fullscreen of
            Nothing -> printResumeHint progName persist
            Just runtime ->
                withFullscreenSuspended runtime
                    (printResumeHint progName persist)
        -- The interrupt is the requested, graceful end of the CLI session.
        -- Returning lets the surrounding brackets restore the SIGINT handler
        -- and close tools without GHC's top-level exception handler printing
        -- "user interrupt" and a backtrace.
        pure interrupted

printResumeHint
    :: String
    -> Persistence
    -> IO ()
printResumeHint progName = \case
    PersistenceDisabled -> pure ()
    PersistenceEnabled slotRef -> do
        slot <- readIORef slotRef
        case slot of
            PersistencePending _ _ _ -> pure ()
            PersistenceActive handle -> do
                -- Drop an in-place "Thinking…" status so the hint is its own line.
                Text.hPutStr stderr "\r\ESC[K"
                clearNativeProgress stderr
                color <- resolveColor stderr
                putTextLn stderr
                    (roleMuted color (resumeHint progName handle.sessionMeta.metaId))
