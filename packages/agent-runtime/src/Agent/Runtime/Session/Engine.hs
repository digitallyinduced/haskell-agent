-- | Headless session execution shared by local and hosted frontends.
-- The caller owns scheduling. This module owns provider/tool scopes, request
-- preparation, conversation checkpoints and durable turn finalization.
module Agent.Runtime.Session.Engine
    ( runNativeSessionTurn
    , SessionCallbacks(..), defaultSessionCallbacks
    , SessionSettings(..), defaultSessionSettings
    ) where

import Agent.Accounts.Auth (LoadedAuth(..), isGatewayLoadedAuth)
import Agent.Cancel (requestCancel)
import Agent.Claude.Auth (loadClaudeCodeAuth, loadClaudeCodeGatewayAuth)
import Agent.Claude.Control (ClaudeCodeHostHandlers(..), ClaudeCodeMcpRequest(..), defaultClaudeCodeHostHandlers)
import Agent.Dialect (codexDialect)
import Agent.GrokBuild.Dialect.Runtime (GrokRuntimeControl(..))
import Agent.GrokBuild.Dialect.Goal
import Agent.Loop hiding (TurnCompleted)
import Agent.Loop.InputQueue qualified as Queue
import Agent.MCP qualified as MCP
import Agent.OpenAI.ImageGeneration (recordImageGenerationImages)
import Agent.OpenRouter qualified as OpenRouter
import Agent.OsPath (unsafeToFilePath)
import Agent.Provider (Provider(..), Credential(..))
import Agent.ResourceScope (allocateResource, closeResourceScope, newResourceScope)
import Agent.Responses.GenericClient (GenericClientOptions(..))
import Agent.Responses.LoopBackend (turnInputsToItems)
import Agent.Responses.Types (ResponseCreateParams(..), ResponseItem, ReasoningConfig(effort))
import Agent.Runtime.AgentSnapshot
import Agent.Runtime.CodeMode
import Agent.Runtime.Collaboration (CollaborationModels(..))
import Agent.Runtime.Compaction.Provider
import Agent.Runtime.ConversationStore (newConversationStore)
import Agent.Runtime.Database.Store
import Agent.Runtime.Error (formatApiError, formatException)
import Agent.Runtime.GatewayClient (gatewayCredentialIdentity)
import Agent.Runtime.Host
import Agent.Runtime.Mcp.Sampling (mcpSamplingHandler)
import Agent.Runtime.ModelConfig
import Agent.Runtime.Models (ModelTarget(..))
import Agent.Runtime.Options (ApprovalPolicy(..), SubagentSettings(..))
import Agent.Runtime.Permission.Approval
import Agent.Runtime.Permission.Decision (ApprovalNotice(..))
import Agent.Runtime.Process (NativeProcessRuntime, nativeAgentProcessRuntime)
import Agent.Runtime.Project (saveProjectAutoApprove)
import Agent.Runtime.Prompt
import Agent.Runtime.ProviderRequest
import Agent.Runtime.Providers
import Agent.Runtime.Providers.ClaudeControl
import Agent.Runtime.Providers.ClaudeGatewayProxy (withClaudeGatewayProxy)
import Agent.Runtime.Request
import Agent.Runtime.Session
import Agent.Runtime.Session.Backend (SessionBackend(..))
import Agent.Runtime.Session.Engine.Context
import Agent.Runtime.Session.Engine.Tools
import Agent.Runtime.Session.Engine.Commands (runHarnessCommand)
import Agent.Runtime.Session.History
import Agent.Runtime.Session.PendingInputs (withPendingInputs)
import Agent.Runtime.Session.Request
import Agent.Runtime.Session.Resources (SessionResources(..))
import Agent.Runtime.Session.Settings
import Agent.Runtime.Session.SideQuestion
import Agent.Runtime.Session.TurnRecord (sessionTurnFromRecord)
import Agent.Runtime.Skills.LearnedSkills (formatLearnedSkillContext, defaultLearnedSkillContextMaxChars)
import Agent.Runtime.Skills.LearnedSkills.Store (loadApplicableLearnedSkillsForStore)
import Agent.Runtime.Startup.Context
import Agent.Runtime.Subagents.Runtime
import Agent.Runtime.Timestamp
import Agent.Runtime.Transcription (transcribeAudioFile)
import Agent.Runtime.Tools.Dialects (CodingTools(..), formatAgentsMdForDialect)
import Agent.Runtime.Tools.Schema
import Agent.Runtime.TurnEngine
import Agent.Runtime.TurnExecution
import Agent.Runtime.TurnRecord qualified as Record
import Agent.Runtime.TurnState
import Agent.Skills
import Agent.Subagents
import Agent.Subagents.TaskPath (taskPathText)
import Agent.ToolDispatch
import Agent.Tools.CodeMode.Tool (ToolMode(..))
import Agent.Tools.MultiAgents (MultiAgentContext(..))
import Agent.Tools.OutputArtifact (finalizeToolOutput)
import Agent.Tools.PlanMode
import Agent.Tools.TaskPlan (readTaskPlan, taskPlanContextText)
import Agent.Tools.Types
import Agent.Tools.Background (setBackgroundTaskHooks)
import Control.Concurrent.STM (atomically)
import Control.Exception.Safe (bracket, catch, finally, fromException, mask_, onException, tryAny)
import Control.Monad (forM_, unless, void, when)
import Data.Aeson qualified as Aeson
import Data.IORef
import Data.Functor ((<&>))
import Data.Maybe (catMaybes, fromMaybe, isNothing)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock (getCurrentTime, utctDay)

runNativeSessionTurn
    :: NativeProcessRuntime -> NativeRunHooks -> SessionCallbacks -> SessionSettings
    -> NativeTurnRequest -> IO (Either Text ())
runNativeSessionTurn process hooks callbacks settings request =
    tryAny (withEngineContext (nativeAgentProcessRuntime process) hooks settings request \context ->
        withEngineTools context callbacks \tools ->
            runPreparedSession context callbacks tools) >>= \case
        Right result -> pure result
        Left exception -> pure $ Left $ case fromException exception of
            Just (EngineError message) -> message
            Nothing -> formatException exception

runPreparedSession :: EngineContext -> SessionCallbacks -> EngineTools -> IO (Either Text ())
runPreparedSession context@EngineContext{..} callbacks tools@EngineTools{..} =
  bracket newResourceScope closeResourceScope \scope -> do
    let provider = engineTarget.targetProvider
        model = engineTarget.targetModelId
        mapModel value = if value == model then engineTarget.targetWireModelId else value
        capabilities = engineHooks.nativeCapabilities
        initial = resolveSessionInitialContext False False engineResumed
        storeDirectory = unsafeToFilePath engineHome
    info <- loadCodexCatalogModelInfo storeDirectory provider engineDialect
        (if isGatewayLoadedAuth engineLoaded then Nothing else Just engineLoaded.loadedTokenProvider) model
    forM_ (codexCatalogFallbackWarning provider engineDialect model info) callbacks.sessionNotice
    let selectedTools = codexToolsForModel info engineAllTools
        codePlan = codeModeRuntimePlan
            (engineSettings.sessionCodeMode /= "disabled")
            (if engineSettings.sessionCodeMode == "enabled" then CodeOnlyToolMode else ConventionalToolMode)
            info
    (_, codeResult) <- allocateResource scope
        (if capabilities.nativeHostExtensions
            then codeModeRuntimeForBackend engineSettings.sessionCodeModeBackend codePlan info selectedTools
            else pure (Right Nothing))
        (either (const (pure ())) (mapM_ (.codeModeClose)))
    code <- case codeResult of
        Left err -> callbacks.sessionNotice ("Code mode unavailable: " <> err) >> pure Nothing
        Right value -> pure value
    now <- getCurrentTime
    identifier <- ensurePersistenceSessionId enginePersistence
        >>= maybe (requireEngine (Left "persistent session has no identity")) pure
    conversation <- newIORef =<< newConversationStore initial.initialContextPrevious
        initial.initialContextItems []
    forM_ engineResumed \(meta, _) ->
        withLiveBackendState conversation \snapshot ->
            void (commitLiveBackendState conversation snapshot{backendProviderState = meta.metaProviderState})
    writeIORef engineChildForkSource (Just (readLiveTranscript conversation))
    contextTokens <- newIORef Nothing
    automatic <- newIORef Nothing
    policy <- newIORef PromptMutating
    remembered <- newIORef (Set.fromList engineSettings.sessionAllowedTools)
    let installInteractionMode mode = do
            applyInteractionMode policy engineCoding.codingPlanMode mode
            when engineSettings.sessionReadOnly (writeIORef policy DenyMutating)
    installInteractionMode engineRequest.nativeTurnInteractionMode
    forM_ engineCoding.codingGrokRuntime \control ->
        forM_ engineSettings.sessionGoal \(objective, budget, paused) -> do
            void (activateGoal control.grokGoalRuntime objective budget)
            when paused (void (pauseGoal control.grokGoalRuntime))
    forM_ engineHooks.nativeRegisterInteractionMode ($ installInteractionMode)
    inputQueue <- Queue.newInputQueue
    setToolSteeringWait engineToolEnv (Queue.awaitSteeringInputReady inputQueue)
    enqueueCompletion <- Queue.prepareBackgroundCompletion inputQueue
    setBackgroundTaskHooks engineToolEnv BackgroundTaskHooks
        { backgroundTaskCompleted = \notice ->
            enqueueCompletion notice.noticeKey (UserMessage notice.noticeBody)
                >>= pure . either (const False) id
        , backgroundTaskDismissed = Queue.dismissBackgroundCompletion inputQueue }
    setAsyncQuestionDelivery engineCoding.codingPlanMode
        (\question -> callbacks.sessionNotice question >> pure (Right ()))
    forM_ engineHooks.nativeRegisterSteering \register ->
        register (\message -> Queue.enqueueSteeringInputsSTM inputQueue [UserMessage message])
    let taskPlan = engineScratch.resourceTaskPlan
        artifactDispatch = defaultLoopDispatch
            { toolDispatchFinalizeOutput = finalizeToolOutput engineToolEnv }
    mcpInstructions <- MCP.mcpFleetInstructions engineMcp.sessionFleet
    let promptFor instructions appTools = appendMcpInstructions instructions $ case info of
            Just catalog -> systemPromptForCatalogModelWithHostedSearch
                capabilities.nativeProviderHostedTools engineDialect model engineEffort catalog
                (map (.appToolName) appTools) (Just engineScratch.resourceSessionTmp)
            Nothing -> systemPromptForToolsWithHostedSearch capabilities.nativeProviderHostedTools
                engineDialect model engineEffort (map (.appToolName) appTools) engineCwd
                (Just engineScratch.resourceSessionTmp) (utctDay now) engineSettings.sessionOneShot
        instructionsFor instructions appTools = promptFor instructions appTools <>
            maybe "" (const (codeModeBackendInstructions engineSettings.sessionCodeModeBackend)) code
        wireTools appTools = case code of
            Nothing -> appTools
            Just runtime -> runtime.codeModeWireTools
                <> (projectCodeModeToolsFor runtime.codeModeProjectionStrategy appTools).directCodeModeTools
        schemasFor appTools codeTools = (case code of
            Nothing -> schemasFromAppToolsWithHostedSearchAndAsyncCapability
            Just _ -> schemasFromAppToolsCodeModeWithHostedSearchAndAsyncCapability)
            capabilities.nativeProviderHostedTools
            (catalogSupportsAsyncToolCallsForTransport engineCatalog engineTarget.targetConnectionId model)
            engineDialect (case code of
                Nothing -> appTools
                Just runtime -> codeTools <>
                    (projectCodeModeToolsFor runtime.codeModeProjectionStrategy appTools).directCodeModeTools)
        initialCodeTools = maybe [] (.codeModeWireTools) code
        initialParams = (setRequestPromptCacheKey identifier $ requestParams provider model
            (instructionsFor mcpInstructions selectedTools) (schemasFor selectedTools initialCodeTools) engineEffort)
            {serviceTier = if engineSettings.sessionFast then Just "priority" else Nothing}
        restored = compatibleSessionPromptSnapshot provider engineTarget.targetConnectionId
            engineTarget.targetDialect engineCwd (Just identifier) initialParams
            (engineResumed >>= \(meta, _) -> meta.metaPromptSnapshot)
        params = maybe initialParams (\snapshot ->
            setRequestPromptCacheKey snapshot.promptSnapshotCacheKey $
                setRequestInstructionsAndTools snapshot.promptSnapshotInstructions
                    (Just snapshot.promptSnapshotTools) initialParams) restored
    requestState <- newSessionRequestState enginePersistence params >>= requireEngine
    writeIORef engineParentEffort do
        currentParams <- readSessionRequestParams requestState
        pure (fromMaybe engineEffort (currentParams.reasoning >>= (.effort)))
    generated <- if initial.initialContextNeeded
        then case restored of
            Just snapshot -> pure snapshot.promptSnapshotGeneratedContext
            Nothing -> generatedContext context engineSkillCatalog callbacks
                (if isNothing info then Nothing else Just (codexEnvironmentContext engineCwd (utctDay now) Nothing Nothing))
        else pure Nothing
    let (promptInstructions, promptSchemas) = requestPromptParts params
        snapshot = SessionPromptSnapshot
            { promptSnapshotVersion = 1, promptSnapshotCreatedAt = now
            , promptSnapshotProvider = provider, promptSnapshotConnection = engineTarget.targetConnectionId
            , promptSnapshotModel = model, promptSnapshotDialect = engineTarget.targetDialect
            , promptSnapshotCwd = engineCwd, promptSnapshotInstructions = promptInstructions
            , promptSnapshotTools = promptSchemas, promptSnapshotGeneratedContext = generated
            , promptSnapshotGrokContext = Nothing, promptSnapshotCacheKey = identifier
            }
    case enginePersistence of
        PersistenceDisabled -> pure ()
        PersistenceEnabled slot -> void (ensureSessionWithPromptSnapshot slot snapshot)
    registry <- requireToolRegistry (selectedTools <> maybe [] (.codeModeWireTools) code)
    registryRef <- newIORef registry
    surfaceRef <- newIORef (mcpInstructions, schemasFor selectedTools initialCodeTools)
    let approve classified current call =
            approveToolDecisionWithReporterAndPersistenceClassifiedWithPrompt
                (const (pure classified))
                (\fresh -> if fresh then engineHooks.nativeRequestFreshApproval else engineHooks.nativeRequestApproval)
                (\case ApprovalWarning text -> callbacks.sessionNotice text; ApprovalSuccess text -> callbacks.sessionNotice text)
                (when (nativeAllowsWorkspaceCreation (Just engineHooks))
                    (saveProjectAutoApprove engineProjectRoot True))
                policy remembered current engineCoding.codingPlanMode call
        approveCurrent call = readIORef registryRef >>= \current -> approve Nothing current call
        refreshTools = do
            available <- codexToolsForModel info <$> engineReadTools
            refreshedCode <- case code of
                Nothing -> pure []
                Just runtime -> runtime.codeModeRefreshTools available >>= requireEngine
            instructions <- MCP.mcpFleetInstructions engineMcp.sessionFleet
            let schemas = schemasFor available refreshedCode
                surface = (instructions, schemas)
            previousSurface <- readIORef surfaceRef
            current <- requireToolRegistry (available <> refreshedCode)
            -- Requests and their dispatch registry advance together, before
            -- admission of the next provider response.
            when (surface /= previousSurface) do
                modifySessionRequestOptions requestState
                    (setRequestInstructionsAndTools (instructionsFor instructions available) (Just schemas))
                writeIORef surfaceRef surface
            writeIORef registryRef current
            pure current
    claudeSlot <- newClaudeSessionRuntimeSlot
    installClaudeSessionRuntime claudeSlot ClaudeSessionRuntime
        { approveNativeTool = \call classified -> readIORef registryRef >>= \current -> approve classified current call
        , approveRegisteredTool = approveCurrent, planMode = engineCoding.codingPlanMode
        , providerNativeToolsEnabled = capabilities.nativeProviderNativeTools }
    let compaction = ProviderCompaction
            { paramsRef = requestState, contextTokensRef = contextTokens
            , contextWindowForParams = \mapTransport fallback current ->
                fromMaybe fallback $ current.model >>= \currentModel ->
                    catalogContextWindowForTransport engineCatalog engineTarget.targetConnectionId
                        currentModel (mapTransport currentModel)
            , currentModelContextWindow = \mapTransport -> do
                current <- readSessionRequestModel requestState
                pure (catalogContextWindowForTransport engineCatalog engineTarget.targetConnectionId current (mapTransport current))
            , conversationRef = conversation
            , installAutomaticCompact = commitAutomaticCompaction enginePersistence conversation automatic
            , taskPlan = Just taskPlan, recordCompactionUsage = recordExtraUsage enginePersistence
            , compactThreshold = engineSettings.sessionCompactThreshold
            }
        host = ProviderHost compaction engineProcess.processNetworkRecovery
    config <- providerConfig context tools claudeSlot approveCurrent mapModel initial.initialContextPrevious
    withProviderRuntime config host \runtime -> do
        installChildren context tools requestState policy runtime.subagents mapModel
        engineHooks.nativeRegisterAgentSnapshot do
            entries <- listAgents engineRegistry Nothing
            pure (AgentSnapshot "root" "running" (Just model) [] :
                [AgentSnapshot (taskPathText path) (agentStatusText status) Nothing []
                | (path, _, status) <- entries])
        writeIORef engineMcp.sessionSampling (Just (mcpSamplingHandler model runtime.sessionBackend.btwBackend))
        when engineSettings.sessionCodeModeRepair $
            forM_ code \worker -> worker.codeModeSetRepair
                (Just (codeModeRepairHandlerWithUsage (recordExtraUsage enginePersistence) runtime.sessionBackend.btwBackend))
        let backend = runtime.sessionBackend
            emit = engineHooks.nativeOnLoopEvent
            configLoop = LoopConfig
                { loopBackend = withPendingInputs enginePendingInputs backend.backend
                , loopBackendState = BackendStateStore
                    (withLiveBackendState conversation pure) (commitLiveBackendState conversation)
                , loopTools = registry, loopReadTools = Just refreshTools
                , loopDispatch = artifactDispatch, loopMaxTurns = engineSettings.sessionMaxTurns
                , loopOnEvent = emit, loopApprove = approveCurrent
                , loopWithToolExecution = \_ action -> action
                , loopReadSteering = Queue.readSteeringInputs inputQueue
                , loopWaitSteering = atomically . Queue.awaitUserSteeringAfter inputQueue
                , loopCommitSteering = Queue.commitSteeringInputs inputQueue
                , loopCloseSteering = Queue.closeSteeringInputs inputQueue
                , loopInterrupt = backend.interruptBackend, loopCancel = engineToolEnv.toolCancel
                , loopSubagents = if capabilities.nativeCollaboration
                    then Just (subagentLoop (AwaitChildren 30000) engineRootSubagents) else Nothing
                }
        engineHooks.nativeRegisterCancel (requestCancel engineToolEnv.toolCancel >> backend.interruptBackend)
        forM_ code \worker -> setCodeModeNestedInvoke worker.codeModeNestedSlot \tool call -> do
            single <- requireToolRegistry [tool]
            approve Nothing single call >>= \case
                ToolApprovalDenied reason -> pure (Left reason)
                ToolApprovalRejected -> pure (Left "Tool call rejected by user.")
                ToolApprovalGranted -> do
                    emit (ToolStarted call)
                    result <- dispatchApprovedRegisteredToolCall artifactDispatch single call
                    emit (ToolFinished result)
                    pure (Right result)
        case engineSettings.sessionOperation of
            HarnessCommand -> runHarnessCommand context tools engineRequest.nativeTurnPrompt >>= \case
                Left err -> pure (Left err)
                Right answer -> callbacks.sessionAnswer answer >> pure (Right ())
            CompactContext -> runtime.compactRunner (nonEmpty engineRequest.nativeTurnPrompt) >>= \case
                Left err -> pure (Left err)
                Right outcome -> do
                    void (commitAutomaticCompaction enginePersistence conversation automatic outcome [])
                    callbacks.sessionNotice ("Context compacted: " <> Text.pack (show outcome.compactBeforeTokens)
                        <> " → " <> Text.pack (show outcome.compactAfterTokens) <> " tokens.")
                    pure (Right ())
            operation | operation `elem` [SideQuestion, PrivateQuestion] -> do
                current <- readSessionRequestParams requestState
                history <- if operation == PrivateQuestion then pure [] else readLiveTranscript conversation
                let privateParams = if operation == PrivateQuestion
                        then current{instructions = Just "Answer the private request independently. Do not call tools.", tools = Just []}
                        else current
                runBtwWithCancel
                    (\cancel action -> engineHooks.nativeRegisterCancel (requestCancel cancel) >> action)
                    runtime.sessionBackend.btwBackend (sideCallSnapshot privateParams history)
                    engineRequest.nativeTurnPrompt >>= \case
                        Left err -> pure (Left (formatBtwError err))
                        Right answer -> callbacks.sessionAnswer answer >> pure (Right ())
            ConversationTurn -> do
                plan <- readTaskPlan taskPlan
                inputs <- requestInputs context
                recordImageGenerationImages engineScratch.resourceImageGenerationHistory (concatMap turnInputImages inputs)
                before <- readLiveTranscript conversation
                previous <- readLivePreviousResponseId conversation
                let prepared = PreparedTurn before generated Nothing
                        (turnInputsWithContext Nothing (taskPlanContextText <$> plan) generated inputs)
                    execute = PreparedExecution configLoop previous prepared
                    persistExceptional exceptional = do
                        -- Retain a replayable interruption record even when the owner
                        -- cancels this worker while a provider/tool scope is unwinding.
                        -- Exceptions have no completed LoopExecution. The typed patch
                        -- contains just the accepted inputs and their abort marker.
                        persistInterrupted enginePersistence engineRequest.nativeTurnPrompt prepared exceptional.exceptionalCompaction
                execution <- executePreparedTurn execute (readIORef automatic) persistExceptional
                let finalized = finalizeTurn (TurnPolicy (const False) stripBracketedTimestamps)
                        Nothing execution.executedCompaction prepared execution.executedLoop
                persistFinalized enginePersistence engineRequest.nativeTurnPrompt
                    conversation finalized execution.executedLoop
                case finalized.finalizedDisposition of
                    TurnCompleted _ -> pure (Right ())
                    TurnCancelled _ -> pure (Left "Turn cancelled.")
                    TurnFailed err -> pure (Left (loopErrorText err))
                    TurnProviderUnavailable err -> pure (Left (formatApiError err))
                    TurnRestarted _ -> pure (Left "Turn restarted.")
            _ -> pure (Left "unsupported session operation")
        `finally` do
            writeIORef engineMcp.sessionSampling Nothing
            allowed <- Set.toList <$> readIORef remembered
            currentPolicy <- readIORef policy
            planning <- isPlanModeActive engineCoding.codingPlanMode
            goal <- maybe (pure Nothing) (readGoal . (.grokGoalRuntime)) engineCoding.codingGrokRuntime
            callbacks.sessionPreferences (Aeson.object
                [ "allowedTools" Aeson..= allowed
                , "interactionMode" Aeson..= (if planning then ("plan" :: Text)
                    else if currentPolicy == ApproveAll then "yolo" else "ask")
                , "goal" Aeson..= fmap (\value -> (value.goalObjective, value.goalTokenBudget, value.goalStatus == GoalUserPaused)) goal ])

applyInteractionMode :: IORef ApprovalPolicy -> PlanModeEnv -> NativeInteractionMode -> IO ()
applyInteractionMode policy plan = \case
    NativeAsk -> writeIORef policy PromptMutating >> deactivatePlanMode plan
    NativeYolo -> writeIORef policy ApproveAll >> deactivatePlanMode plan
    NativePlan -> writeIORef policy PromptMutating >> activatePlanMode plan

nonEmpty :: Text -> Maybe Text
nonEmpty text = if Text.null (Text.strip text) then Nothing else Just text

generatedContext :: EngineContext -> SkillCatalog -> SessionCallbacks -> Maybe Text -> IO (Maybe Text)
generatedContext EngineContext{..} catalog callbacks environment = do
    agents <- preloadAgentsContext
        (engineSettings.sessionAgentsMd && nativeLoadsHostWorkspaceContext engineHooks.nativeWorkspaceDiscovery)
        engineDialect engineHome engineCwd
    scopes <- deriveDatabaseScopesWithNamespace engineHooks.nativeDatabaseScopeNamespace
        (unsafeToFilePath engineHome) (unsafeToFilePath engineProjectRoot) >>= requireEngine
    learned <- loadApplicableLearnedSkillsForStore engineStore scopes >>= \case
        Left err -> callbacks.sessionNotice err >> pure []
        Right values -> pure values
    memory <- loadDatabaseMemoryContext engineStore scopes >>= \case
        Left err -> callbacks.sessionNotice err >> pure Nothing
        Right value -> pure (Just value)
    let parts = catMaybes
            [ agents >>= formatAgentsMdForDialect engineDialect engineCwd
            , fst (formatSkillCatalogContext defaultSkillCatalogMaxChars catalog)
            , fst (formatLearnedSkillContext defaultLearnedSkillContextMaxChars learned)
            , memory, environment ]
    pure $ if null parts then Nothing else Just (Text.intercalate "\n\n" parts)

requestInputs :: EngineContext -> IO [TurnInput]
requestInputs EngineContext{..} = do
    clock <- traverse (\value -> requireEngine (parseMessageClock
        value.nativeHourCycle value.nativeTimeZoneName value.nativeTimeZoneOffsetMinutes))
        engineRequest.nativeTurnMessageClock
    now <- getCurrentTime
    let base = fromMaybe
            [userMessageWithAttachments engineRequest.nativeTurnPrompt
                (map ImageAttachmentItem engineRequest.nativeTurnImages <> map FileAttachmentItem engineRequest.nativeTurnFiles)]
            engineHooks.nativeInitialTurnInputs
        started = maybe now (\(meta, _) -> meta.metaCreatedAt) engineResumed
        previous = engineResumed >>= \(_, turns) -> case reverse turns of
            turn : _ -> Just turn.turnAt
            [] -> Nothing
    stampTurnInputsSinceWith clock started previous base

providerConfig
    :: EngineContext -> EngineTools -> ClaudeSessionRuntimeSlot
    -> (ToolCall -> IO ToolApproval) -> (Text -> Text) -> Maybe Text -> IO ProviderConfig
providerConfig EngineContext{..} EngineTools{..} claude approve mapModel previous = do
    let tokens = engineLoaded.loadedTokenProvider
        provider = engineTarget.targetProvider
    case provider of
        OpenAIProvider -> pure $ OpenAiProviderConfig OpenAiConfig
            { tokenProvider = tokens, showRawReasoning = engineSettings.sessionShowRawReasoning
            , transportModel = mapModel, accounts = OpenAiAccounts
                { selectablePool = engineLoaded.loadedOpenAiPool
                , readActiveAccountId = pure "", resolveAccountLabel = engineLoaded.loadedAccountLabel
                , installAccount = \_ _ -> pure (), preferAccount = const (pure ()) } }
        XAIProvider -> pure (XaiProviderConfig tokens engineHooks.nativeCapabilities.nativeProviderHostedTools engineGateway)
        GeminiProvider -> pure (GeminiProviderConfig tokens)
        OpenRouterProvider -> do
            options <- OpenRouter.clientOptionsFromEnv
            pure $ OpenRouterProviderConfig OpenRouterConfig
                { tokenProvider = tokens, clientOptions = options
                , genericOptions = engineCustomConnection <&> \connection -> GenericClientOptions
                    { baseUrl = Text.unpack connection.responsesBaseUrl
                    , model = engineTarget.targetWireModelId, bearerToken = engineCustomBearer
                    , requestTimeoutSeconds = connection.responsesRequestTimeoutSeconds }
                , model = engineTarget.targetModelId, transportModel = mapModel }
        ClaudeCodeProvider -> do
            unless engineHooks.nativeCapabilities.nativeProviderNativeTools $
                requireEngine (Left "Claude Code is unavailable in this runtime")
            server <- requireEngine $ MCP.createInProcessMcpServer "haskell-agent" "0.1.0"
                defaultLoopDispatch (approveClaudeRegisteredTool claude)
                    (filter isClaudeBridgeTool engineAllTools)
            pure $ ClaudeProviderConfig ClaudeConfig
                { withAuth = \action -> case engineGateway of
                    Nothing -> loadClaudeCodeAuth >>= requireEngine >>= action
                    Just credential -> withClaudeGatewayProxy credential
                        (\transport -> loadClaudeCodeGatewayAuth transport >>= requireEngine >>= action)
                            >>= requireEngine
                , cwd = engineCwd, initialPrevious = previous, transportModel = mapModel
                , hostHandlers = defaultClaudeCodeHostHandlers
                    { canUseTool = Just (handleClaudePermissionRequest claude)
                    , handleMcpMessage = Just \request ->
                        if request.serverName /= "haskell-agent" then pure Aeson.Null
                        else fromMaybe (Aeson.object []) <$> MCP.handleInProcessMcpMessage server request.message
                    , mcpToolNames = MCP.inProcessMcpToolNames server
                    , nativeToolsEnabled = engineHooks.nativeCapabilities.nativeProviderNativeTools }
                , onConnected = const (pure ()) }

installChildren
    :: EngineContext -> EngineTools -> SessionRequestState -> IORef ApprovalPolicy
    -> ProviderSubagents -> (Text -> Text) -> IO ()
installChildren EngineContext{..} EngineTools{..} params policy capabilities mapModel = do
    ghci <- newIORef (engineRequest.nativeTurnShellMode `elem` [NativeShellGhci, NativeShellBoth])
    bash <- newIORef (engineRequest.nativeTurnShellMode `elem` [NativeShellBash, NativeShellBoth])
    let runtime = SubagentRuntime
            { subagentOptions = SubagentSettings engineSettings.sessionShowRawReasoning
                engineSettings.sessionCompactThreshold engineSettings.sessionMaxTurns
            , subagentCodexModelInfo = \tokens -> loadCodexCatalogModelInfo (unsafeToFilePath engineHome)
                OpenAIProvider codexDialect (Just tokens)
            , subagentNetworkRecovery = engineProcess.processNetworkRecovery
            , subagentGhciEnabled = ghci, subagentBashEnabled = bash, subagentPolicy = policy
            , subagentPlanHooks = engineHooks.nativePlanHooks
            , subagentSkillRoots = engineToolEnv.toolSkillRoots, subagentAllowedRoots = engineToolEnv.toolAllowedRoots
            , subagentToolResourceArbiter = engineToolEnv.toolResourceArbiter
            , subagentRootAccessRequest = engineToolEnv.toolRootAccessRequest
            , subagentSessionTmp = engineToolEnv.toolSessionTmp
            , subagentShellEnvironment = engineToolEnv.toolShellEnvironment
            , subagentShellEnvironmentLock = engineToolEnv.toolShellEnvironmentLock
            , subagentOutputMemoryStore = engineToolEnv.toolOutputMemoryStore
            , subagentOutputMemoryCap = engineToolEnv.toolOutputMemoryCap
            , subagentMcpTools = MCP.mcpFleetMetaToolsForArtifactDirectory
                (Just (unsafeToFilePath engineScratch.resourceSessionTmp)) engineMcp.sessionFleet
                <> MCP.mcpFleetResourceTools engineMcp.sessionFleet
            , subagentComposeTools = engineHooks.nativeComposeTools
            , subagentParams = params, subagentRegistry = engineRegistry
            , subagentSessions = engineChildSessions, subagentStoreRoot = engineChildStoreRoot
            , subagentTypes = engineCoding.codingAgentTypes
            , subagentLegacyTarget = sessionLegacySubagentTarget . fst <$> engineResumed
            , subagentConnection = engineTarget.targetConnectionId, subagentMapModel = mapModel
            , subagentCreateWorktree = if nativeAllowsWorkspaceCreation (Just engineHooks)
                then Just (createEngineWorktree EngineContext{..}) else Nothing
            , subagentSpawnModelGuidance = Nothing
            , subagentAllowedChildModels = engineChildModels.childAllowedModels
            , subagentResolveChildModel = engineChildModels.childResolveModel
            , subagentChildModelAllowed = engineChildModels.childModelAllowed
            , subagentOpenAiChild = engineOpenAiChild
            , subagentTranscribeAudio = \provider -> transcribeAudioFile provider engineGatewayAccess
            }
        send = engineMultiContext >>= (.multiSendToRoot)
    case capabilities of
        NoProviderSubagents -> pure ()
        CodexSubagents gatewayOnly -> setSubagentRunner engineRegistry
            (runCodexSubagent gatewayOnly runtime engineLoaded.loadedTokenProvider send)
        HttpSubagents makeBackend -> setSubagentRunner engineRegistry
            (runHttpSubagent runtime engineDialect engineTarget.targetProvider send makeBackend)
        XaiSubagents contextWindow threshold makeBackend -> setSubagentRunner engineRegistry
            (runXaiParentSubagent runtime engineDialect send contextWindow threshold makeBackend)

persistFinalized :: Persistence -> Text -> IORef LiveConversation -> FinalizedTurn -> LoopExecution -> IO ()
persistFinalized persistence prompt conversation finalized execution = do
    now <- getCurrentTime
    state <- withLiveBackendState conversation pure
    let (assistant, failure, response, usage) = case finalized.finalizedDisposition of
            TurnCompleted result -> (stripBracketedTimestamps <$> result.finalText, Nothing, Just result.finalResponseId, Just result.tokenUsage)
            TurnFailed err -> (Nothing, Just (loopErrorText err), Nothing, Nothing)
            TurnCancelled _ -> (Nothing, Just "Turn cancelled.", Nothing, Nothing)
            TurnProviderUnavailable err -> (Nothing, Just (formatApiError err), Nothing, Nothing)
            TurnRestarted _ -> (Nothing, Just "Turn restarted.", Nothing, Nothing)
        effect = if turnReplacesTranscript finalized.finalizedPrepared.preparedBeforeItems execution.executionState
            then TranscriptReplace else TranscriptAppend
        record = Record.TurnRecord now prompt assistant failure response effect
            finalized.finalizedModelItems finalized.finalizedDisplayItems usage execution.executionProviderTelemetry
    persistTurn persistence (sessionTurnFromRecord record)
        (\meta -> meta { metaProviderState = if failure == Nothing then state.backendProviderState else Nothing })

persistInterrupted :: Persistence -> Text -> PreparedTurn -> Maybe AutomaticCompactionBoundary -> IO ()
persistInterrupted persistence prompt original boundary = do
    now <- getCurrentTime
    let prepared = rebasePreparedTurn boundary original
    persistTurn persistence SessionTurn
        { turnAt = now, turnUserText = prompt, turnAssistantText = Nothing
        , turnError = Just "Turn interrupted.", turnResponseId = Nothing
        , turnEffect = TranscriptAppend, turnItems = inputOnlyTurnItems prepared <> turnInputsToItems [UserMessage turnAbortedNote]
        , turnDisplayItems = [], turnUsage = Nothing, turnProviderTelemetry = [] }
        (\meta -> meta { metaLastResponseId = Nothing, metaProviderState = Nothing })

persistTurn :: Persistence -> SessionTurn -> (SessionMeta -> SessionMeta) -> IO ()
persistTurn PersistenceDisabled _ _ = pure ()
persistTurn (PersistenceEnabled slot) turn update = do
    handle <- ensureSession slot
    updated <- appendTurnWithMetaUpdate handle turn update
    writeIORef slot (PersistenceActive updated)

commitAutomaticCompaction
    :: Persistence -> IORef LiveConversation -> IORef (Maybe AutomaticCompactionBoundary)
    -> CompactOutcome -> [TurnInput] -> IO CompactionInstall
commitAutomaticCompaction persistence conversation automatic outcome pending = do
    now <- getCurrentTime
    let history = outcome.compactHistory <> turnInputsToItems pending
    mask_ do
        persistTurn persistence SessionTurn
            { turnAt = now, turnUserText = "", turnAssistantText = Nothing, turnError = Nothing
            , turnResponseId = Nothing, turnEffect = TranscriptReplace, turnItems = history
            , turnDisplayItems = [], turnUsage = Nothing, turnProviderTelemetry = [] }
            (\meta -> meta { metaLastResponseId = Nothing, metaProviderState = Nothing })
        writeIORef automatic (Just (AutomaticCompactionBoundary history []))
        void (replaceLiveConversation conversation Nothing history)
    pure CompactionInstalled

recordExtraUsage :: Persistence -> TokenUsage -> IO ()
recordExtraUsage PersistenceDisabled _ = pure ()
recordExtraUsage (PersistenceEnabled slot) usage = do
    handle <- ensureSession slot
    updated <- addSessionUsage usage handle
    writeIORef slot (PersistenceActive updated)

loopErrorText :: LoopError -> Text
loopErrorText = \case
    LoopTransport err -> formatApiError err
    LoopTransportAfterOutput err -> formatApiError err
    LoopCancelled _ -> "Turn cancelled."
    LoopMaxTurns _ -> "The turn reached the maximum number of model steps."
    LoopIncomplete _ -> "The model response was incomplete."
    LoopNoResponseId -> "The provider returned no response id."
    LoopUnexpected message -> message

agentStatusText :: SubagentStatus -> Text
agentStatusText = \case
    Pending -> "pending"
    Running -> "running"
    Completed _ -> "completed"
    Errored _ -> "failed"
    Interrupted -> "interrupted"
    Suspended _ _ -> "suspended"
    Closed -> "closed"
    NotFound -> "not_found"
