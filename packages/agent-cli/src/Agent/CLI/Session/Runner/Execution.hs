-- | Construction and execution of a CLI agent session.
module Agent.CLI.Session.Runner.Execution
    ( AgentStepCache(..)
    , SessionRunnerContinuation(..)
    , runSession
    ) where
import qualified Agent.CLI.Session.Activity as Activity
import Agent.CLI.Session.Request
    ( readSessionRequestParams
    , readSessionRequestModel
    , modifySessionRequestOptions
    )
import Agent.CLI.CodeModeRuntime
import Agent.CLI.Claude
    ( ClaudeSessionRuntime(..)
    , installClaudeSessionRuntime
    )
import Agent.CLI.Compaction
    ( AutomaticCompactionBoundary(..)
    , CompactOutcome(..)
    , CompactionInstall(CompactionInstalled)
    )
import Agent.CLI.Compaction.Projection (occupancyOnTurnFinished)
import Agent.CLI.Artifact (fencedCodeBlock, lastDiffBlock)
import Agent.CLI.Context (contextUsageTokens, formatContextReport)
import Agent.Responses.LoopBackend (turnInputsToItems)
import Agent.Responses.Types (ResponseCreateParams(model))
import Agent.CLI.ComputerUse (computerToolName)
import Agent.CLI.Session.Runner.Types
    ( SessionRunnerContinuation(..) )
import Agent.CLI.AgentViewport (AgentViewportEnv)
import Agent.CLI.AgentViewport.Runtime
import Agent.Tools.OutputArtifact
import Agent.Tools.Background
    ( setBackgroundTaskHooks )
import Agent.CLI.SessionTitle
import Agent.CLI.ManagedTurn
import Agent.CLI.GatewayBridge
import Agent.CLI.Notification
    ( AttentionRequest(PermissionRequested)
    , notifyAttention
    )
import Agent.CLI.Approval
import Agent.CLI.Permission (promptPermission, promptPermissionOnce, promptRootAccess)
import Agent.CLI.Plan
    ( ProposedPlanSegment(..)
    , ProposedPlanStream
    , feedProposedPlanStream
    , finishProposedPlanStream
    , initialProposedPlanStream
    , stripProposedPlan
    )
import Agent.CLI.ProviderTransition (PendingTurn)
import Agent.CLI.Recap
import Agent.CLI.CancelWatch
import Agent.CLI.Clipboard
import Agent.CLI.Command
import Agent.CLI.LearnedSkills
import Agent.CLI.LearnedSkills.Store
import Agent.CLI.Options
import Agent.CLI.PendingInputs
import Agent.CLI.SteeringInputs
import Agent.CLI.Runtime.Types
import Agent.CLI.Runtime.Orchestration.Types
    ( NativeDiscoveryContext(..)
    , NativeRunCapabilities(..)
    , NativeRunHooks(..)
    , fullNativeRunCapabilities
    , nativePreparedDiscovery
    )
import Agent.CLI.Session.Runtime.Types
import Agent.CLI.Interrupt
import Agent.Store.Postgres
import Agent.CLI.Project
import Agent.CLI.Prompt
import Agent.CLI.SessionState
import Agent.CLI.Render
import Agent.CLI.Session
import Agent.CLI.Session.History
import Agent.CLI.Session.Workspace (WorkspaceContext(..))
import qualified Agent.CLI.Session.Observation as Observation
import Agent.CLI.Session.Inbox
    ( newSessionInbox
    , releaseInboxPending
    , takeInboxMessage
    , withOptionalSessionInboxServer
    )
import Agent.CLI.SessionEnv
import Agent.Runtime.SessionState qualified as RuntimeState
import Agent.CLI.SessionLock
    ( acquireSessionActivityLock
    , releaseSessionLock
    )
import Agent.CLI.Session.Interaction
import Agent.CLI.Session.Selection (currentSessionId)
import Agent.CLI.Skills
import Agent.CLI.StartupContext
import Agent.CLI.Startup.Auth
import Agent.CLI.Subagents.Runtime
import Agent.CLI.Style
import Agent.CLI.Terminal
import Agent.CLI.Request
import Agent.CLI.Tools
import Agent.CLI.ModelConfig
    ( catalogSupportsAsyncToolCallsForTransport
    )
import Agent.CLI.Error
import Agent.CLI.Dialects
import Agent.CLI.Dictation (dictationTargetForSession)
import Agent.CLI.TUI.App
import Agent.CLI.TUI.Composer (appendFullscreenInput)
import Agent.CLI.TUI.Types (FullscreenInput(..), FullscreenRuntime(..))
import Agent.CLI.Input (ReplLine(ReplText))
import Agent.TUI.Model
import Agent.TUI.Motion
import Agent.CLI.WindowTitle
import Agent.CLI.Turn
import Agent.Cancel
import Agent.Loop
import qualified Agent.MCP.Fleet as MCP
import Agent.Dialect
import Agent.Error (ApiError)
import Agent.Provider (Provider)
import Agent.Skills
import Agent.Store.Postgres.Skill (LearnedSkill)
import Agent.Subagents
import Agent.Subagents.TaskPath
import Agent.ToolDispatch
import Agent.Tools.MultiAgents
import Agent.Tools.PlanMode
import Agent.Tools.Types
import Agent.OsPath
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, withAsync)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.MVar
    ( MVar, modifyMVar_, newEmptyMVar, newMVar, takeMVar, tryPutMVar, withMVar )
import Control.Concurrent.STM
    ( STM, atomically, newEmptyTMVarIO, putTMVar )
import Control.Exception.Safe
    ( catchAny
    , mask_
    , onException
    , uninterruptibleMask_
    )
import Control.Monad (forM_, forever, unless, void, when)
import Data.IORef
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing)
import qualified Data.Text as Text
import qualified Data.Set as Set
import Data.Time.Clock (getCurrentTime, utctDay)
import System.IO (Handle)
import System.OsPath (OsPath)

formatQueuedPrompts :: [Text.Text] -> Text.Text
formatQueuedPrompts [] = "No prompts are queued."
formatQueuedPrompts prompts =
    "Queued prompts (" <> Text.pack (show (length prompts)) <> "):\n"
        <> Text.intercalate
            "\n"
            (zipWith formatPrompt [1 :: Int ..] prompts)
  where
    formatPrompt index prompt =
        Text.pack (show index)
            <> ". "
            <> Text.replace "\n" "\n   " prompt

sessionDirectTools
    :: [AppTool]
    -> Maybe CodeModeSessionRuntime
    -> [AppTool]
sessionDirectTools allTools codeModeRuntime =
    filter
        (\tool ->
            canonicalToolName tool.appToolName
                `Set.notMember` codeModeWireNames)
        allTools
  where
    codeModeWireNames =
        Set.fromList $
            map
                (canonicalToolName . (.appToolName))
                (maybe [] (.codeModeWireTools) codeModeRuntime)

-- | Host-facing session resources that are established before the title and
-- turn runtimes. Keeping these together makes the outer session runner a
-- lifecycle coordinator instead of a second implementation of each host
-- concern.
data SessionHostRuntime = SessionHostRuntime
    { hostInitialPrevious :: !(Maybe Text.Text)
    , hostSessionState :: !RuntimeState.SessionState
    , hostIoLock :: !(MVar ())
    , hostApprovalLock :: !(MVar ())
    , hostObservationPublisher
        :: !(IORef (Maybe Observation.SessionObservationPublisher))
    , hostInboxRuntime :: !SessionInboxRuntime
    , hostNativeCapabilities :: !NativeRunCapabilities
    , hostLoadsWorkspaceContext :: !Bool
    , hostPreparedWorkspaceEnvironment
        :: !(Maybe PreparedWorkspaceEnvironment)
    , hostTerminal :: !TerminalCapabilities
    , hostStdoutHandle :: !Handle
    , hostStderrHandle :: !Handle
    , hostUseColor :: !Bool
    , hostStderrTty :: !Bool
    , hostReportSessionError :: !(Text.Text -> IO ())
    , hostWindowTitle :: !WindowTitleController
    , hostTitleEvent :: !(SessionTitleEvent -> IO ())
    , hostFullscreen :: !(Maybe FullscreenRuntime)
    }

newSessionHostRuntime :: SessionRequest -> IO SessionHostRuntime
newSessionHostRuntime SessionRequest{..} = do
    initialPrevious <- readLivePreviousResponseId conversationRef
    runtimeState <- RuntimeState.newSessionStateWith
        conversationRef startupContext usageRef automaticCompactionRef
        initialGrokContext
    ioLock <- newMVar ()
    approvalLock <- newMVar ()
    observationPublisher <- newIORef Nothing
    observationInputWaitCount <- newMVar (0 :: Int)
    inboxRuntime <- SessionInboxRuntime
        <$> newSessionInbox
        <*> newEmptyMVar
        <*> newIORef False
        <*> newIORef False
        <*> newEmptyTMVarIO
    let fullscreen = startup.startupFullscreen
        nativeCapabilities =
            maybe
                fullNativeRunCapabilities
                (.nativeCapabilities)
                startup.startupNativeHooks
        preparedDiscovery =
            startup.startupNativeHooks
                >>= nativePreparedDiscovery . (.nativeWorkspaceDiscovery)
        loadsHostWorkspaceContext = isNothing preparedDiscovery
        preparedWorkspaceEnvironment =
            (\context ->
                PreparedWorkspaceEnvironment
                    context.nativeDiscoveryOperatingSystem
                    context.nativeDiscoveryShell)
                <$> preparedDiscovery
        terminal = startup.startupTerminal
        stdoutHandle = startup.startupStdout
        stderrHandle = startup.startupStderr
        useColor = startup.startupUseColor
        stderrTty = startup.startupStderrTty
        stdoutTty = startup.startupStdoutTty
        writeWindowTitle title =
            case fullscreen of
                Just runtime -> setFullscreenWindowTitle runtime title
                Nothing -> setCliWindowTitle stdoutTty stdoutHandle title
        withIoLock action = withMVar ioLock (const action)
        requestRootAccess root =
            approveFilesystemRootAccess policyRef $
                withToolHumanInputWait toolEnv $
                    case startup.startupNativeHooks of
                        Just hooks -> hooks.nativeRequestRootAccess root
                        Nothing -> withMVar ioLock \_ ->
                            case promptRequest of
                                Just request
                                    | isJust request.managedTurnBridgeDirectory ->
                                        requestManagedRootAccess request root
                                _ -> case fullscreen of
                                    Just runtime -> do
                                        notifyAttention
                                            stderrHandle
                                            PermissionRequested
                                        maybe False (== 0)
                                            <$> requestFullscreenChoiceWithBody
                                                runtime
                                                "Filesystem access requested"
                                                ("Allow access to " <> toText root
                                                    <> " for this session?")
                                                0
                                                [ ( "Allow directory for this session"
                                                  , ""
                                                  )
                                                , ("Deny", "")
                                                ]
                                    Nothing ->
                                        withStdinPaused stdinControl
                                            (promptRootAccess useColor root)
        reportSessionError message =
            case fullscreen of
                Just runtime ->
                    emitUiEvent runtime (UiErrorMessage message)
                Nothing -> do
                    color <- resolveColor stderrHandle
                    putTextLn stderrHandle
                        (roleWarn color (glyphWarn <> message))
    windowTitle <- newWindowTitleController
        options.optMotionMode
        startupWindowTitle
        withIoLock
        writeWindowTitle
    let beginInputWait = do
            windowTitle.windowTitleBeginInputWait
            modifyMVar_ observationInputWaitCount \count -> do
                readIORef observationPublisher >>= mapM_
                    (\publisher -> Observation.setObservedWaiting publisher True)
                pure (count + 1)
        endInputWait = do
            windowTitle.windowTitleEndInputWait
            modifyMVar_ observationInputWaitCount \count -> do
                let remaining = max 0 (count - 1)
                readIORef observationPublisher >>= mapM_
                    (\publisher -> Observation.setObservedWaiting publisher (remaining > 0))
                pure remaining
    setPlanModeInputWaitHooks
        planMode
        beginInputWait
        endInputWait
    setToolHumanInputWaitHooks
        toolEnv
        beginInputWait
        endInputWait
    setToolRootAccessRequest toolEnv (Just requestRootAccess)
    let showTitleEvent = \case
            SessionTitleGenerated SessionTitleResult{..} ->
                case persist of
                    PersistenceDisabled -> pure ()
                    PersistenceEnabled slotRef ->
                        readIORef slotRef >>= \case
                            PersistenceActive handle
                                | handle.sessionMeta.metaId == resultSessionId
                                , not handle.sessionMeta.metaTitleIsManual ->
                                    windowTitle.windowTitleSet
                                        (cliWindowTitle
                                            handle.sessionMeta.metaCwd
                                            (Just resultTitle))
                            _ -> pure ()
            SessionTitleFailed SessionTitleFailure{..} ->
                case persist of
                    PersistenceDisabled -> pure ()
                    PersistenceEnabled slotRef ->
                        readIORef slotRef >>= \case
                            PersistenceActive handle
                                | handle.sessionMeta.metaId == failureSessionId
                                , not handle.sessionMeta.metaTitleIsManual ->
                                    withMVar ioLock \_ -> do
                                        let message =
                                                "session title generation failed: "
                                                    <> failureMessage
                                        case fullscreen of
                                            Just runtime ->
                                                emitUiEvent runtime
                                                    (UiErrorMessage message)
                                            Nothing -> do
                                                color <-
                                                    resolveColor stderrHandle
                                                putTextLn stderrHandle
                                                    (roleWarn color
                                                        (glyphWarn <> message))
                            _ -> pure ()
    pure SessionHostRuntime
        { hostInitialPrevious = initialPrevious
        , hostSessionState = runtimeState
        , hostIoLock = ioLock
        , hostApprovalLock = approvalLock
        , hostObservationPublisher = observationPublisher
        , hostInboxRuntime = inboxRuntime
        , hostNativeCapabilities = nativeCapabilities
        , hostLoadsWorkspaceContext = loadsHostWorkspaceContext
        , hostPreparedWorkspaceEnvironment = preparedWorkspaceEnvironment
        , hostTerminal = terminal
        , hostStdoutHandle = stdoutHandle
        , hostStderrHandle = stderrHandle
        , hostUseColor = useColor
        , hostStderrTty = stderrTty
        , hostReportSessionError = reportSessionError
        , hostWindowTitle = windowTitle
        , hostTitleEvent = showTitleEvent
        , hostFullscreen = fullscreen
        }

withSessionTitleRuntime
    :: SessionHostRuntime
    -> SessionRequest
    -> SessionBackend
    -> (SessionTitleManager -> IO a)
    -> IO a
withSessionTitleRuntime host SessionRequest{..} SessionBackend{..} =
    withSessionTitleManager
        btwBackend
        (readSessionRequestParams paramsRef)
        host.hostTitleEvent

-- | Mutable controls shared by rendering, tools, persistence, and the agent
-- viewport. Allocation and viewport registration form one startup phase.
data SessionControlRuntime = SessionControlRuntime
    { controlToolRegistry :: !ToolRegistry
    , controlSteeringInputs :: !SteeringInputs
    , controlSpinnerRef :: !(IORef (Maybe (Async ())))
    , controlRenderStateRef :: !(IORef RenderState)
    , controlAllowedToolsRef :: !(IORef (Set.Set Text.Text))
    , controlComputerUseEnabledRef :: !(IORef Bool)
    , controlReadLastAssistant :: !(IO (Maybe Text.Text))
    , controlUnavailableProvidersRef :: !(IORef (Set.Set Provider))
    , controlStartupUnavailableRef :: !(IORef (Maybe (STM ApiError)))
    , controlRestartEffortRef :: !(IORef (Maybe Text.Text))
    , controlLastFailedTurnRef :: !(IORef (Maybe PendingTurn))
    , controlTitleTurnCount :: !(IORef Int)
    , controlAgentViewportRuntime :: !AgentViewportRuntime
    , controlAgentViewport :: !AgentViewportEnv
    }

installBackgroundTaskSteering :: ToolEnv -> SteeringInputs -> IO ()
installBackgroundTaskSteering toolEnv steeringInputs = do
    enqueueCompletion <- prepareBackgroundCompletion steeringInputs
    setBackgroundTaskHooks toolEnv BackgroundTaskHooks
        { backgroundTaskCompleted = \notice ->
            enqueueCompletion
                notice.noticeKey
                (UserMessage notice.noticeBody) >>= \case
                    -- Completion callbacks hold a delivery gate. Keep this
                    -- non-blocking; UI reporting can backpressure.
                    Left _ -> pure False
                    Right inserted -> pure inserted
        , backgroundTaskDismissed =
            dismissBackgroundCompletion steeringInputs
        }

newSessionControlRuntime
    :: SessionHostRuntime
    -> SessionRequest
    -> IO SessionControlRuntime
newSessionControlRuntime host SessionRequest{..} = do
    toolRegistry <- requireToolRegistry allTools
    steeringInputs <- newSteeringInputs
    installBackgroundTaskSteering toolEnv steeringInputs
    spinnerRef <- newIORef Nothing
    renderStateRef <- newIORef emptyRenderState
    allowedToolsRef <- newIORef Set.empty
    computerUseEnabledRef <- newIORef $
        resolveComputerUseEnabled options startup.startupStdinTty
            && isJust
                (lookupAppTool
                    computerToolName
                    (sessionDirectTools refreshTools codeModeRuntime))
    unavailableProvidersRef <- newIORef unavailableProviders
    startupUnavailableRef <- newIORef startupUnavailable
    restartEffortRef <- newIORef Nothing
    lastFailedTurnRef <- newIORef Nothing
    titleTurnCount <- newIORef =<< sessionTitleTurnCountFromSlot persist
    let loadSelectedAgent agentId = do
            effectiveModel <- readSessionRequestModel paramsRef
            lookupOrCreateSubagentSession
                subagentSessions
                storeRoot
                agentTypes
                provider
                connectionId
                legacyTarget
                effectiveModel
                (dialectId dialect)
                agentId
        selectChild agentId =
            (do
                session <- loadSelectedAgent agentId
                pinSubagentSession
                    storeRoot agentTypes legacyTarget agentId session)
                `catchAny` \err ->
                    host.hostReportSessionError
                        ("failed to select agent: "
                            <> formatException err)
        releaseChild agentId = do
            sessions <- readIORef subagentSessions
            forM_ (Map.lookup agentId sessions) \session -> do
                unpinSubagentSession session
                case multiCtx of
                    Nothing -> pure ()
                    Just ctx -> do
                        status <- getStatus ctx.multiRegistry agentId
                        void $
                            persistAndEvictSubagentSessionWithStatus
                                storeRoot ctx.multiRegistry agentTypes
                                agentId status session
        listChildAgents =
            case multiCtx of
                Nothing -> pure []
                Just ctx ->
                    map
                        (\(path, agentId, status) -> AgentChildListing
                            { childListingPath = taskPathText path
                            , childListingId = agentId
                            , childListingStatus = status
                            })
                        <$> listAgents ctx.multiRegistry Nothing
        readChildSources =
            Map.map
                (\session -> AgentChildSource
                    { childSourceModel =
                        session.subSessionEffectiveModel
                    , childSourceTranscript =
                        (.backendItems) <$>
                            readIORef session.subSessionTranscript
                    })
                <$> readIORef subagentSessions
    agentViewportRuntime <-
        newAgentViewportRuntime AgentViewportRuntimeConfig
            { viewportConfigShowRawReasoning =
                options.optShowRawReasoning
            , viewportConfigWorkspace = toText workspace.cwd
            , viewportConfigReadRootTranscript =
                readLiveTranscript conversationRef
            , viewportConfigListChildren = listChildAgents
            , viewportConfigReadChildSources = readChildSources
            , viewportConfigSelectChild = selectChild
            , viewportConfigReleaseChild = releaseChild
            }
    let agentViewport =
            agentViewportEnvironment agentViewportRuntime
    writeIORef startup.startupAgentSnapshot
        (loadAgentSnapshot agentViewportRuntime False)
    forM_ startup.startupNativeHooks \hooks ->
        hooks.nativeRegisterAgentSnapshot
            (snd <$> loadAgentSnapshot agentViewportRuntime False)
    writeIORef startup.startupAgentSelect
        (selectAgentViewport agentViewportRuntime)
    pure SessionControlRuntime
        { controlToolRegistry = toolRegistry
        , controlSteeringInputs = steeringInputs
        , controlSpinnerRef = spinnerRef
        , controlRenderStateRef = renderStateRef
        , controlAllowedToolsRef = allowedToolsRef
        , controlComputerUseEnabledRef = computerUseEnabledRef
        , controlReadLastAssistant = RuntimeState.readLastAssistant host.hostSessionState
        , controlUnavailableProvidersRef = unavailableProvidersRef
        , controlStartupUnavailableRef = startupUnavailableRef
        , controlRestartEffortRef = restartEffortRef
        , controlLastFailedTurnRef = lastFailedTurnRef
        , controlTitleTurnCount = titleTurnCount
        , controlAgentViewportRuntime = agentViewportRuntime
        , controlAgentViewport = agentViewport
        }

data SkillContextRuntime = SkillContextRuntime
    { skillReloadGeneratedContext :: !(IO ())
    , skillResetSession :: !(IO ())
    , skillRefresh :: !(Bool -> IO ())
    , skillInitialize :: !(IO [LearnedSkill])
    }

buildSkillContextRuntime
    :: SessionRunnerContinuation
    -> SessionHostRuntime
    -> SessionControlRuntime
    -> SessionRequest
    -> SessionBackend
    -> SkillContextRuntime
buildSkillContextRuntime
        callbacks host controls SessionRequest{..} SessionBackend{..} =
    SkillContextRuntime
        { skillReloadGeneratedContext = reloadGeneratedContext
        , skillResetSession = sessionReset
        , skillRefresh = refreshSkills
        , skillInitialize = initializeSkills
        }
  where
    fullscreen = host.hostFullscreen
    stderrHandle = host.hostStderrHandle
    loadsHostWorkspaceContext = host.hostLoadsWorkspaceContext
    renderStateRef = controls.controlRenderStateRef
    steeringInputs = controls.controlSteeringInputs
    agentViewportRuntime = controls.controlAgentViewportRuntime
    installSkills context queueContext skills = do
        before <- readIORef context
        installSkillToolRoots toolEnv skills
        omitted <- installSkillCatalogWithOmissions
            reservedSlashNames queueContext context
            skillsRef skillInvocationsRef skills
        after <- readIORef context
        pure
            ( omitted
            , max 0 (contextLength after - contextLength before)
            )
    loadLearnedSkills =
        loadApplicableLearnedSkillsForStore
            startup.startupDatabaseStore
            databaseScopes
    installLearnedSkills context maximum queueContext =
        loadLearnedSkills
            >>= installLearnedSkillResult context maximum queueContext
    installLearnedSkillResult context maximum queueContext = \case
        Left err -> do
            reportLearnedSkillWarning
                ("learned skills unavailable: " <> err)
            pure []
        Right learnedSkills -> do
            omitted <-
                if queueContext
                    then queueLearnedSkillContextWithOmissions
                        maximum
                        context
                        learnedSkills
                    else pure 0
            when (omitted > 0) $
                reportLearnedSkillWarning
                    ("learned skills: "
                        <> Text.pack (show omitted)
                        <> " omitted from model context due to the context budget")
            pure learnedSkills
    reloadGeneratedContext = do
        freshAgents <-
            if loadsHostWorkspaceContext
                then
                    loadAgentsContext
                        stderrHandle
                        fullscreen
                        SuppressAgentsContextLoaded
                        options
                        dialect
                        workspace.home
                        workspace.cwd
                        []
                        Nothing
                        ((.catalogEnvironmentContext)
                            <$> codexCatalogSession)
                else
                    newIORef
                        ((.catalogEnvironmentContext)
                            <$> codexCatalogSession)
        freshSkills <- loadAvailableSkills
        (omitted, _) <-
            installSkills freshAgents True freshSkills
        reportSkillCatalog True freshSkills omitted
        void $ installLearnedSkills
            freshAgents
            defaultLearnedSkillContextMaxChars
            True
        fresh <- readIORef freshAgents
        writeIORef startupContext fresh
    sessionReset = do
        resetLiveConversationWith
            resetBackendState
            conversationRef
            planMode
        clearImageGenerationHistory
        writeIORef usageRef emptyTokenUsage
        writeIORef contextOccupancyRef Nothing
        modifyIORef' renderStateRef clearRenderTokenRate
        RuntimeState.clearLastAssistant host.hostSessionState
        writeIORef subagentSessions Map.empty
        RuntimeState.clearGrokContext host.hostSessionState
        resetAgentViewport agentViewportRuntime
        case multiCtx of
            Just ctx -> resetSubagentRegistry ctx.multiRegistry
            Nothing -> pure ()
        clearPendingInputs pendingNotices
        clearSteeringInputs steeringInputs
        installBackgroundTaskSteering toolEnv steeringInputs
        readIORef toolEnv.toolSessionTmp >>= mapM_ resetToolSessionTemp
        clearMemoryOutputArtifacts toolEnv.toolOutputMemoryStore
        reloadGeneratedContext
    refreshSkills queueContext = do
        refreshed <- loadAvailableSkills
        current <- readIORef skillsRef
        when (refreshed /= current) do
            (omitted, _) <-
                installSkills startupContext queueContext refreshed
            when queueContext $
                reportSkillCatalog True refreshed omitted
    loadAvailableSkills = do
        local <-
            if loadsHostWorkspaceContext
                then loadSkillsCatalogQuiet
                    options workspace.home workspace.projectRoot workspace.cwd
                else pure (SkillCatalog [] [])
        remote <-
            if options.optSkills
                then maybe
                    (pure (SkillCatalog [] []))
                    loadMcpSkillsCatalog
                    mcpFleet
                else pure (SkillCatalog [] [])
        pure (mergeSkillCatalogs local remote)
    contextLength = maybe 0 Text.length
    formatSkillWarning warning =
        "skill ignored: "
            <> toText warning.skillWarningPath
            <> ": "
            <> warning.skillWarningMessage
    formatSkillOmission omitted =
        "skills: "
            <> Text.pack (show omitted)
            <> " omitted from model context due to the catalog budget"
    reportLearnedSkillWarning message =
        case fullscreen of
            Nothing -> do
                color <- resolveColor stderrHandle
                putTextLn stderrHandle $
                    roleWarn color (glyphWarn <> message)
            Just runtime ->
                emitUiEvent runtime (UiSystemMessage message)
    reportSkillCatalog includeSummary catalog omitted =
        case fullscreen of
            Nothing -> do
                color <- resolveColor stderrHandle
                when includeSummary do
                    let count = length catalog.catalogSkills
                    putTextLn stderrHandle $
                        roleMuted color
                            (glyphSession
                                <> "skills: loaded "
                                <> Text.pack (show count)
                                <> if count == 1
                                    then " skill"
                                    else " skills")
                mapM_
                    (putTextLn stderrHandle
                        . roleWarn color
                        . (glyphWarn <>)
                        . formatSkillWarning)
                    catalog.catalogWarnings
                when (omitted > 0) $
                    putTextLn stderrHandle $
                        roleWarn color
                            (glyphWarn <> formatSkillOmission omitted)
            Just runtime -> do
                when includeSummary do
                    let count = length catalog.catalogSkills
                    emitUiEvent runtime $
                        UiSystemMessage
                            ("skills: loaded "
                                <> Text.pack (show count)
                                <> if count == 1
                                    then " skill"
                                    else " skills")
                mapM_
                    (emitUiEvent runtime
                        . UiSystemMessage
                        . formatSkillWarning)
                    catalog.catalogWarnings
                when (omitted > 0) $
                    emitUiEvent runtime
                        (UiSystemMessage (formatSkillOmission omitted))
    initializeSkills = do
        markStartupStage startup "Loading skills…"
        skills <- readIORef skillsRef
        (omitted, _) <- installSkills startupContext
            queueInitialContext
            skills
        reportSkillCatalog (isNothing fullscreen) skills omitted
        learnedSkills <-
            if needsInitialContext
                then do
                    loaded <-
                        loadLearnedSkillsWithPreload
                            initialContextPreload.preloadedLearnedSkills
                            loadLearnedSkills
                    installLearnedSkillResult
                        startupContext
                        defaultLearnedSkillContextMaxChars
                        queueInitialContext
                        loaded
                else pure []
        callbacks.runnerFinishStartup startup
        pure learnedSkills

data SessionLoopEventRuntime = SessionLoopEventRuntime
    { loopEventRender :: !RenderConfig
    , loopEventEmit :: !(LoopEvent -> IO ())
    }

buildSessionLoopEventRuntime
    :: SessionHostRuntime
    -> SessionControlRuntime
    -> SessionRequest
    -> IORef (Maybe ProposedPlanStream)
    -> (LoopEvent -> IO ())
    -> SessionLoopEventRuntime
buildSessionLoopEventRuntime
        host controls SessionRequest{..}
        proposedPlanStreamRef managedLoopPublisher =
    SessionLoopEventRuntime
        { loopEventRender = render
        , loopEventEmit = emitLoop
        }
  where
    fullscreen = host.hostFullscreen
    terminal = host.hostTerminal
    renderStateRef = controls.controlRenderStateRef
    agentViewportRuntime = controls.controlAgentViewportRuntime
    render = RenderConfig
        { renderShowThinking = host.hostStderrTty
        , renderThinkingSpinner = controls.controlSpinnerRef
        , renderState = renderStateRef
        , renderColor = host.hostUseColor
        , renderLock = host.hostIoLock
        , renderStdout = host.hostStdoutHandle
        , renderStderr = host.hostStderrHandle
        , renderModel = readSessionRequestModel paramsRef
        , renderNativeProgress =
            host.hostStderrTty
                && terminal.terminalNativeProgress
                && nativeProgressAnimationEnabled options.optMotionMode
        , renderMotionMode = options.optMotionMode
        , renderWorkspace = toText workspace.cwd
        }
    emitLoop event =
        projectPlanProtocol event >>= mapM_ emitPresentedLoop
    emitPresentedLoop event = do
        readIORef host.hostObservationPublisher >>= mapM_
            (\publisher -> Observation.publishObservedLoopEvent publisher event)
        recordAgentViewportEvent agentViewportRuntime event
        forM_ startup.startupNativeHooks \hooks ->
            hooks.nativeOnLoopEvent event
        managedLoopPublisher event
        case event of
            TurnFinished turn ->
                -- Keep the provider checkpoint recorded by middleware:
                -- a host-renumbered commit may restart the live process.
                withLiveBackendState conversationRef \snapshot ->
                    modifyIORef' contextOccupancyRef $
                        occupancyOnTurnFinished snapshot turn
            _ -> pure ()
        case fullscreen of
            Nothing -> renderEvent render event
            Just runtime -> do
                now <- getCurrentTime
                modifyIORef' renderStateRef \state ->
                    case event of
                        TurnStarted -> beginRenderTurn now state
                        TextDelta delta ->
                            countGenerationChars delta
                                state{statePrintedText = True}
                        PlanDelta delta ->
                            countGenerationChars delta
                                state{statePrintedText = True}
                        ReasoningDelta delta ->
                            countGenerationChars delta state
                        ResponseRestarted _ ->
                            resetRenderGeneration now state
                        ToolStarted _ ->
                            state{stateActivity = "Running tool…"}
                        TurnFinished turn ->
                            recordRenderTurnRate now turn state
                        _ -> state
                emitUiEvent runtime (UiLoop event)
                case event of
                    TurnFinished _ -> do
                        occupancy <- readIORef contextOccupancyRef
                        params <- readSessionRequestParams paramsRef
                        history <- readLiveTranscript conversationRef
                        contextWindow <- currentContextWindow
                        emitUiEvent runtime $
                            UiSetContextUsage
                                (Just
                                    (contextUsageTokens
                                        occupancy
                                        params
                                        history))
                                contextWindow
                    _ -> pure ()
    projectPlanProtocol = \case
        TurnStarted -> do
            planActive <-
                if dialectId dialect == CodexDialect
                    then isPlanModeActive planMode
                    else pure False
            writeIORef proposedPlanStreamRef $
                if planActive
                    then Just initialProposedPlanStream
                    else Nothing
            pure [TurnStarted]
        TextDelta delta ->
            atomicModifyIORef' proposedPlanStreamRef \case
                Nothing -> (Nothing, [TextDelta delta])
                Just stream ->
                    let (next, segments) =
                            feedProposedPlanStream stream delta
                    in ( Just next
                       , concatMap proposedPlanSegmentEvents segments
                       )
        ResponseRestarted message -> do
            modifyIORef' proposedPlanStreamRef $
                fmap (const initialProposedPlanStream)
            pure [ResponseRestarted message]
        ResponseAttemptDiscarded -> do
            modifyIORef' proposedPlanStreamRef $
                fmap (const initialProposedPlanStream)
            pure [ResponseAttemptDiscarded]
        TurnFinished output ->
            atomicModifyIORef' proposedPlanStreamRef \case
                Nothing -> (Nothing, [TurnFinished output])
                Just stream ->
                    let projectedOutput =
                            output
                                { assistantText =
                                    stripProposedPlan <$> output.assistantText
                                }
                        tailEvents =
                            concatMap
                                proposedPlanSegmentEvents
                                (finishProposedPlanStream stream)
                    in (Nothing, tailEvents <> [TurnFinished projectedOutput])
        event -> pure [event]

    proposedPlanSegmentEvents = \case
        AssistantText text -> [TextDelta text | not (Text.null text)]
        ProposedPlanDelta text -> [PlanDelta text | not (Text.null text)]
        ProposedPlanStart -> []
        ProposedPlanEnd -> []

data SessionApprovalRuntime = SessionApprovalRuntime
    { approvalApproveClassified
        :: !(Maybe Bool -> ToolCall -> IO (Either Text.Text Bool))
    , approvalApproveRegistered
        :: !(ToolCall -> IO (Either Text.Text Bool))
    }

buildSessionApprovalRuntime
    :: SessionHostRuntime
    -> SessionControlRuntime
    -> SessionRequest
    -> SessionApprovalRuntime
buildSessionApprovalRuntime host controls SessionRequest{..} =
    SessionApprovalRuntime
        { approvalApproveClassified = approveToolWithClassification
        , approvalApproveRegistered = approveRegisteredTool
        }
  where
    approveToolWithClassification classifiedReadOnly call =
        withMVar host.hostApprovalLock \_ ->
            chooseApproval
      where
        chooseApproval =
            case startup.startupNativeHooks of
                Just hooks ->
                    approve
                        hooks.nativeRequestApproval
                        (const (pure ()))
                        (pure ())
                Nothing -> case promptRequest of
                    Just request
                        | isJust request.managedTurnBridgeDirectory ->
                            approve
                                (requestManagedApproval request)
                                (const (pure ()))
                                (pure ())
                    _ -> case host.hostFullscreen of
                        Nothing ->
                            approve
                                (\requested ->
                                    withStdinPaused stdinControl do
                                        color <-
                                            resolveColor host.hostStderrHandle
                                        promptPermission
                                            color
                                            (toText workspace.cwd)
                                            requested)
                                reportLineApproval
                                (saveProjectAutoApprove workspace.projectRoot True)
                        Just runtime ->
                            approve
                                (requestFullscreenPermission
                                    runtime
                                    (toText workspace.cwd))
                                (\case
                                    ApprovalWarning _ -> pure ()
                                    ApprovalSuccess message ->
                                        emitUiEvent runtime
                                            (UiSetNotice
                                                (Just
                                                    (successNotice
                                                        message))))
                                (saveProjectAutoApprove workspace.projectRoot True)
        classify = const (pure classifiedReadOnly)
        approve request report persist =
            approveToolDecisionWithReporterAndPersistenceClassifiedWithPrompt
                classify
                (\requiresExplicit requested ->
                    withToolHumanInputWait toolEnv $
                        withMVar host.hostIoLock \_ ->
                            if requiresExplicit
                                then requestFreshApproval requested
                                else request requested)
                (\notice ->
                    withMVar host.hostIoLock \_ ->
                        report notice)
                persist
                policyRef
                controls.controlAllowedToolsRef
                controls.controlToolRegistry
                planMode
                call
        -- Existing external/fullscreen protocols do not carry once-only
        -- approval semantics. Do not silently downgrade a fresh request to
        -- their generic permission dialog.
        requestFreshApproval requested =
            case startup.startupNativeHooks of
                Just hooks -> hooks.nativeRequestFreshApproval requested
                Nothing -> case promptRequest of
                    Just _ -> pure Nothing
                    Nothing -> case host.hostFullscreen of
                        Just _ -> pure Nothing
                        Nothing ->
                            withStdinPaused stdinControl do
                                color <- resolveColor host.hostStderrHandle
                                promptPermissionOnce color (toText workspace.cwd) requested
        reportLineApproval = \case
            ApprovalWarning message -> do
                color <- resolveColor host.hostStderrHandle
                putTextLn host.hostStderrHandle (roleWarn color message)
            ApprovalSuccess message -> do
                color <- resolveColor host.hostStderrHandle
                putTextLn host.hostStderrHandle (roleSuccess color message)
    approveRegisteredTool =
        approveToolWithClassification Nothing

data SessionShellRuntime = SessionShellRuntime
    { shellToolDisabledReason :: !(ToolCall -> IO (Maybe Text.Text))
    , shellActiveToolNames :: !(IO [Text.Text])
    , shellCurrentMode :: !(IO ShellMode)
    , shellSetMode :: !(ShellMode -> IO Text.Text)
    , shellComputerUseEnabled :: !(IO Bool)
    , shellSetComputerUseEnabled :: !(Bool -> IO Text.Text)
    , shellSetTempDir :: !(OsPath -> IO ())
    , shellRefreshRequestParams :: !(IO ())
    }

buildSessionShellRuntime
    :: SessionHostRuntime
    -> SessionControlRuntime
    -> SessionRequest
    -> SessionShellRuntime
buildSessionShellRuntime host controls SessionRequest{..} =
    SessionShellRuntime
        { shellToolDisabledReason = toolDisabledReason
        , shellActiveToolNames = currentActiveToolNames
        , shellCurrentMode = currentShellMode
        , shellSetMode = setShellMode
        , shellComputerUseEnabled = readIORef computerUseEnabledRef
        , shellSetComputerUseEnabled = setComputerUse
        , shellSetTempDir = setSessionTempDir
        , shellRefreshRequestParams = refreshCurrentSessionParams
        }
  where
    nativeCapabilities = host.hostNativeCapabilities
    computerUseEnabledRef = controls.controlComputerUseEnabledRef
    sessionTools = sessionDirectTools refreshTools codeModeRuntime
    computerUseAvailable =
        isJust (lookupAppTool computerToolName sessionTools)
    toolDisabledReason call = do
        ghciEnabled <- readIORef ghciEnabledRef
        bashEnabled <- readIORef bashEnabledRef
        computerUseEnabled <- readIORef computerUseEnabledRef
        let toolName = canonicalToolName call.name
        pure $
            if isComputerToolCallKind call.callKind
                && not computerUseEnabled
                then Just
                    "Computer use is disabled. Run /computer-use to enable it."
            else if (isGhciToolName toolName && not ghciEnabled)
                || (isBashToolName toolName && not bashEnabled)
                then Just
                    ("Tool " <> call.name
                        <> " is disabled by the current /shell setting.")
            else Nothing
    activeSessionTools ghciEnabled bashEnabled computerUseEnabled =
        filterComputerUseTools computerUseEnabled $
            filterGhciTools ghciEnabled
                (filterBashTools bashEnabled sessionTools)
    providerVisibleTools enabledTools =
        case codeModeRuntime of
            Nothing -> enabledTools
            Just runtime ->
                runtime.codeModeWireTools
                    <> (projectCodeModeToolsFor
                            runtime.codeModeProjectionStrategy
                            enabledTools
                        ).directCodeModeTools
    filterComputerUseTools True = id
    filterComputerUseTools False =
        filter \tool ->
            canonicalToolName tool.appToolName /= computerToolName
    currentShellMode = do
        ghciEnabled <- readIORef ghciEnabledRef
        bashEnabled <- readIORef bashEnabledRef
        pure $ case (ghciEnabled, bashEnabled) of
            (True, False) -> ShellGhci
            (False, True) -> ShellBash
            (True, True) -> ShellBoth
            (False, False) -> ShellNone
    currentActiveToolNames = do
        ghciEnabled <- readIORef ghciEnabledRef
        bashEnabled <- readIORef bashEnabledRef
        computerUseEnabled <- readIORef computerUseEnabledRef
        let active =
                activeSessionTools
                    ghciEnabled
                    bashEnabled
                    computerUseEnabled
            reportTools =
                active
                    <> maybe [] (.codeModeWireTools) codeModeRuntime
            internalNames = map (.appToolName) reportTools
            projectedNames =
                case dialectToolLayout dialect of
                    NoHostToolLayout -> []
                    FlatToolLayout
                        | dialectId dialect == GrokBuildDialect ->
                            map grokBuildPublicToolName internalNames
                        | otherwise -> internalNames
                    CollaborationNamespaceLayout ->
                        filter (`notElem` multiAgentToolNames) internalNames
                            <> if any
                                (`elem` multiAgentToolNames)
                                internalNames
                                then ["collaboration"]
                                else []
        pure $
            case dialectToolLayout dialect of
                NoHostToolLayout -> []
                _ ->
                    hostedSearchToolNamesWhen
                        nativeCapabilities.nativeProviderHostedTools
                        dialect
                        ++ projectedNames
    shellModeFlags = \case
        ShellGhci -> (True, False)
        ShellBash -> (False, True)
        ShellBoth -> (True, True)
        ShellNone -> (False, False)
    shellModeLabel = \case
        ShellGhci -> "ghci"
        ShellBash -> "bash"
        ShellBoth -> "ghci + bash"
        ShellNone -> "none"
    refreshSessionParams ghciEnabled bashEnabled computerUseEnabled = do
        sessionTmp <- readIORef toolEnv.toolSessionTmp
        effectiveModel <- readSessionRequestModel paramsRef
        today <- utctDay <$> getCurrentTime
        let enabledTools =
                activeSessionTools
                    ghciEnabled
                    bashEnabled
                    computerUseEnabled
            enabledNames = map (.appToolName) enabledTools
            instructionText =
                appendMcpInstructions mcpInstructions case codexCatalogSession of
                    Just catalog ->
                        catalog.catalogInstructionsFor
                            enabledNames sessionTmp
                    Nothing ->
                        systemPromptForToolsWithHostedSearch
                            nativeCapabilities.nativeProviderHostedTools
                            dialect
                            commitAttributionModel
                            commitAttributionEffort
                            enabledNames
                            workspace.cwd
                            sessionTmp
                            today
                            (isOneShot options)
            toolSchemas =
                let modelSupportsAsync =
                        catalogSupportsAsyncToolCallsForTransport
                            catalog
                            connectionId
                            effectiveModel
                in
                case codeModeRuntime of
                    Just _ ->
                        schemasFromAppToolsCodeModeWithHostedSearchAndAsyncCapability
                            nativeCapabilities.nativeProviderHostedTools
                            modelSupportsAsync
                            dialect
                            (providerVisibleTools enabledTools)
                    Nothing ->
                        schemasFromAppToolsWithHostedSearchAndAsyncCapability
                            nativeCapabilities.nativeProviderHostedTools
                            modelSupportsAsync
                            dialect
                            enabledTools
        modifySessionRequestOptions paramsRef
            (setRequestInstructionsAndTools
                instructionText
                (Just toolSchemas))
    refreshCurrentSessionParams = do
        ghciEnabled <- readIORef ghciEnabledRef
        bashEnabled <- readIORef bashEnabledRef
        computerUseEnabled <- readIORef computerUseEnabledRef
        refreshSessionParams
            ghciEnabled
            bashEnabled
            computerUseEnabled
    setShellMode mode = do
        let (ghciEnabled, bashEnabled) = shellModeFlags mode
        writeIORef ghciEnabledRef ghciEnabled
        writeIORef bashEnabledRef bashEnabled
        unless ghciEnabled suspendGhci
        computerUseEnabled <- readIORef computerUseEnabledRef
        refreshSessionParams
            ghciEnabled
            bashEnabled
            computerUseEnabled
        pure ("shell tools: " <> shellModeLabel mode)
    setComputerUse enabled
        | enabled && not computerUseAvailable =
            pure
                "computer use is unavailable for this provider or platform"
        | otherwise = do
            writeIORef computerUseEnabledRef enabled
            modifyIORef' controls.controlAllowedToolsRef
                (Set.delete computerToolName)
            ghciEnabled <- readIORef ghciEnabledRef
            bashEnabled <- readIORef bashEnabledRef
            refreshSessionParams ghciEnabled bashEnabled enabled
            pure $
                if enabled
                    then
                        "computer use: on \
                        \(approval required before control)"
                    else "computer use: off"
    setSessionTempDir tempDir = do
        -- Persistent tool runtimes capture temp-backed state at startup.
        -- Reset them before publishing the new root so no process or state
        -- file remains attached to the previous session.
        resetToolSessionTemp tempDir
        setToolSessionTmp toolEnv (Just tempDir)
        refreshCurrentSessionParams

data SessionSubagentRuntime = SessionSubagentRuntime
    { subagentBeginTurn :: !(IO (Maybe RootTurnId))
    , subagentFinishTurn :: !(Maybe RootTurnId -> IO ())
    , subagentAbortTurn :: !(Maybe RootTurnId -> IO ())
    , subagentConcurrentLimit :: !(IO Int)
    , subagentSetConcurrentLimit :: !(Int -> IO Text.Text)
    }

buildSessionSubagentRuntime :: SessionRequest -> SessionSubagentRuntime
buildSessionSubagentRuntime SessionRequest{..} =
    SessionSubagentRuntime
        { subagentBeginTurn = beginSubagentTurn
        , subagentFinishTurn = finishSubagentTurn
        , subagentAbortTurn = abortSubagentTurn
        , subagentConcurrentLimit = currentConcurrentLimit
        , subagentSetConcurrentLimit = setConcurrentLimit
        }
  where
    beginSubagentTurn =
        case multiCtx of
            Nothing -> pure Nothing
            Just ctx -> do
                rootTurnId <- beginRootTurn ctx.multiRegistry
                writeIORef rootTurnRef (Just rootTurnId)
                pure (Just rootTurnId)
    finishSubagentTurn rootTurnId =
        atomicModifyIORef' rootTurnRef \current ->
            (if current == rootTurnId then Nothing else current, ())
    abortSubagentTurn rootTurnId = do
        case rootTurnId of
            Just owned -> case multiCtx of
                Just ctx -> abortRootTurn ctx.multiRegistry owned
                Nothing -> pure ()
            Nothing -> pure ()
        finishSubagentTurn rootTurnId
    currentConcurrentLimit = case multiCtx of
        Nothing ->
            pure defaultSubagentConfig.maxConcurrent
        Just ctx ->
            (.maxConcurrent) <$> subagentConfig ctx.multiRegistry
    setConcurrentLimit limit = do
        let next = max 1 limit
        case multiCtx of
            Just ctx -> setMaxConcurrent ctx.multiRegistry next
            Nothing -> pure ()
        saveProjectMaxConcurrentAgents workspace.projectRoot next
        pure ("concurrent agent limit: " <> Text.pack (show next))

buildSessionLoopConfig
    :: SessionControlRuntime
    -> SessionRequest
    -> SessionBackend
    -> SessionLoopEventRuntime
    -> SessionShellRuntime
    -> SessionApprovalRuntime
    -> LoopConfig
buildSessionLoopConfig
        controls SessionRequest{..} SessionBackend{..}
        eventRuntime shellRuntime approvalRuntime =
    LoopConfig
        { loopBackend = backend
        , loopBackendState = BackendStateStore
            { readBackendState =
                withLiveBackendState conversationRef pure
            , commitBackendState =
                commitLiveBackendState conversationRef
            }
        , loopTools = controls.controlToolRegistry
        , loopDispatch =
            defaultLoopDispatch
                { toolDispatchFinalizeOutput = \call output ->
                    if isComputerToolCallKind call.callKind
                        then pure output
                        else finalizeToolOutput toolEnv call output
                }
        , loopMaxTurns = options.optMaxTurns
        , loopOnEvent = eventRuntime.loopEventEmit
        , loopApprove = \call ->
            shellRuntime.shellToolDisabledReason call >>= \case
                Just reason -> pure (Left reason)
                Nothing -> approvalRuntime.approvalApproveRegistered call
        , loopReadSteering =
            readSteeringInputs controls.controlSteeringInputs
        , loopCommitSteering = \count ->
            commitSteeringInputs controls.controlSteeringInputs count
        , loopInterrupt = interruptBackend
        , loopCancel = toolEnv.toolCancel
        }

installSessionToolRuntimes
    :: SessionHostRuntime
    -> SessionControlRuntime
    -> SessionRequest
    -> SessionLoopEventRuntime
    -> SessionShellRuntime
    -> SessionApprovalRuntime
    -> LoopConfig
    -> IO ()
installSessionToolRuntimes
        host controls SessionRequest{..}
        eventRuntime shellRuntime approvalRuntime config = do
    installClaudeSessionRuntime claudeRuntimeSlot ClaudeSessionRuntime
        { approveNativeTool = \call readOnly ->
            approvalRuntime.approvalApproveClassified readOnly call
        , approveRegisteredTool =
            approvalRuntime.approvalApproveRegistered
        , planMode
        , providerNativeToolsEnabled =
            host.hostNativeCapabilities.nativeProviderNativeTools
        }
    forM_ ((.codeModeNestedSlot) <$> codeModeRuntime) \slot ->
        setCodeModeNestedInvoke slot \call -> do
            shellRuntime.shellToolDisabledReason call >>= \case
                Just reason -> pure (Left reason)
                Nothing ->
                    approvalRuntime.approvalApproveRegistered call >>= \case
                        Left denial -> pure (Left denial)
                        Right False ->
                            pure (Left "Tool call rejected by user.")
                        Right True -> do
                            eventRuntime.loopEventEmit (ToolStarted call)
                            result <- dispatchApprovedRegisteredToolCall
                                config.loopDispatch
                                controls.controlToolRegistry
                                call
                            eventRuntime.loopEventEmit (ToolFinished result)
                            pure (Right result)

data SessionLoopRuntime = SessionLoopRuntime
    { loopRuntimeConfig :: !LoopConfig
    , loopRuntimeRender :: !RenderConfig
    , loopRuntimeShell :: !SessionShellRuntime
    , loopRuntimeSubagents :: !SessionSubagentRuntime
    }

newSessionLoopRuntime
    :: SessionHostRuntime
    -> SessionControlRuntime
    -> SessionRequest
    -> SessionBackend
    -> IO SessionLoopRuntime
newSessionLoopRuntime host controls request@SessionRequest{..} sessionBackend = do
    managedLoopPublisher <-
        maybe
            (pure (const (pure ())))
            newManagedLoopEventPublisher
            promptRequest
    proposedPlanStreamRef <- newIORef Nothing
    sessionDir <- readIORef planMode.planSessionDir
    forM_ sessionDir (writeIORef storeRoot . Just)
    let eventRuntime =
            buildSessionLoopEventRuntime
                host controls request proposedPlanStreamRef managedLoopPublisher
        shellRuntime = buildSessionShellRuntime host controls request
        approvalRuntime =
            buildSessionApprovalRuntime host controls request
        subagentRuntime = buildSessionSubagentRuntime request
        config =
            buildSessionLoopConfig
                controls
                request
                sessionBackend
                eventRuntime
                shellRuntime
                approvalRuntime
    installSessionToolRuntimes
        host
        controls
        request
        eventRuntime
        shellRuntime
        approvalRuntime
        config
    pure SessionLoopRuntime
        { loopRuntimeConfig = config
        , loopRuntimeRender = eventRuntime.loopEventRender
        , loopRuntimeShell = shellRuntime
        , loopRuntimeSubagents = subagentRuntime
        }

data SessionPersistenceRuntime = SessionPersistenceRuntime
    { persistenceBeginTurnActivity :: !(IO ())
    , persistenceEndTurnActivity :: !(IO ())
    , persistenceOnPersisted :: !(SessionHandle -> IO ())
    , persistenceCommitAutomaticCompaction
        :: !(CompactOutcome -> [TurnInput] -> IO CompactionInstall)
    , persistenceCompact
        :: !(Maybe Text.Text -> IO (Either Text.Text CompactOutcome))
    }

newSessionPersistenceRuntime
    :: SessionHostRuntime
    -> SkillContextRuntime
    -> SessionRequest
    -> IO SessionPersistenceRuntime
newSessionPersistenceRuntime
        host skillsRuntime SessionRequest{..} = do
    turnActivity <- Activity.newTurnActivity
    nativeSessionIdRef <- newIORef Nothing
    let acquireTurnActivity handle =
            Activity.acquireTurnActivity turnActivity $
                -- The marker is best effort; the lifetime session lock
                -- remains authoritative.
                either (const Nothing) Just <$>
                    acquireSessionActivityLock
                        handle.sessionDir handle.sessionMeta.metaId
        endTurnActivity = do
            Activity.endTurnActivity turnActivity releaseSessionLock
            writeIORef host.hostInboxRuntime.inboxTurnActive False
        beginTurnActivity = mask_ $
            (do
                writeIORef host.hostInboxRuntime.inboxTurnActive True
                Activity.beginTurnActivity turnActivity
                case persist of
                    PersistenceDisabled -> pure ()
                    PersistenceEnabled slotRef ->
                        readIORef slotRef >>= \case
                            PersistencePending{} -> pure ()
                            PersistenceActive handle ->
                                void (acquireTurnActivity handle)
                fromInbox <- atomicModifyIORef'
                    host.hostInboxRuntime.inboxTurn
                    (\active -> (False, active))
                when fromInbox $
                    releaseInboxPending host.hostInboxRuntime.inboxMessages)
                `onException` endTurnActivity
        notifyNativeSessionId sessionId = do
            shouldNotify <-
                atomicModifyIORef' nativeSessionIdRef \current ->
                    if current == Just sessionId
                        then (current, False)
                        else (Just sessionId, True)
            when shouldNotify $
                forM_ startup.startupNativeHooks
                    (\hooks -> hooks.nativeOnSessionId sessionId)
                `onException`
                    atomicModifyIORef' nativeSessionIdRef
                        (\current ->
                            if current == Just sessionId
                                then (Nothing, ())
                                else (current, ()))
        onPersistedWithActivity handle = do
            -- Preserve the established lock order: own the session before
            -- attempting its best-effort activity marker.
            onPersisted handle
            void (tryPutMVar host.hostInboxRuntime.inboxReady handle)
            acquired <- acquireTurnActivity handle
            notifyNativeSessionId handle.sessionMeta.metaId
                `onException`
                    when acquired endTurnActivity
        reloadGeneratedContextSafely =
            skillsRuntime.skillReloadGeneratedContext `catchAny` \err ->
                host.hostReportSessionError
                    ("failed to reload generated context: "
                        <> formatException err)
        -- The durable replace, pending input, and in-memory publication form
        -- one checkpoint transaction. Keeping pending input in the replacement
        -- means a process death before the continuation cannot lose the user's
        -- request. Do not allow cancellation after PostgreSQL commits but
        -- before the boundary becomes visible to turn cleanup.
        commitAutomaticCompaction outcome pendingInputs = do
            let durableHistory =
                    outcome.compactHistory <> turnInputsToItems pendingInputs
            uninterruptibleMask_ do
                case persist of
                    PersistenceDisabled -> pure ()
                    PersistenceEnabled slotRef -> do
                        now <- getCurrentTime
                        handle <- ensureSession slotRef
                        let checkpointTurn = SessionTurn
                                { turnAt = now
                                , turnUserText = ""
                                , turnAssistantText = Nothing
                                , turnError = Nothing
                                , turnResponseId = Nothing
                                , turnEffect = TranscriptReplace
                                , turnItems = durableHistory
                                , turnDisplayItems = []
                                , turnUsage = Nothing
                                , turnProviderTelemetry = []
                                }
                        (updated, _) <-
                            appendTurnWithMetaUpdateIndexed
                                handle
                                checkpointTurn
                                \meta -> meta { metaLastResponseId = Nothing }
                        writeIORef slotRef (PersistenceActive updated)
                let boundary = AutomaticCompactionBoundary
                        { automaticCompactionHistory = durableHistory
                        -- These inputs are already part of the checkpoint.
                        -- A failure/retry must not append or submit them again.
                        , automaticCompactionPendingInputs = []
                        }
                writeIORef automaticCompactionRef (Just boundary)
                _ <-
                    replaceLiveConversation
                        conversationRef
                        Nothing
                        durableHistory
                pure ()
            -- Reloading skills/project state may perform arbitrary I/O and is
            -- not part of the atomic persistence critical section.
            reloadGeneratedContextSafely
            pure CompactionInstalled
        compactRunnerWithContext focus = do
            result <- compactRunner focus
            case result of
                Left _ -> pure ()
                Right _ -> reloadGeneratedContextSafely
            pure result
    pure SessionPersistenceRuntime
        { persistenceBeginTurnActivity = beginTurnActivity
        , persistenceEndTurnActivity = endTurnActivity
        , persistenceOnPersisted = onPersistedWithActivity
        , persistenceCommitAutomaticCompaction =
            commitAutomaticCompaction
        , persistenceCompact = compactRunnerWithContext
        }

buildSessionEnv
    :: SessionHostRuntime
    -> SessionControlRuntime
    -> SkillContextRuntime
    -> SessionLoopRuntime
    -> SessionPersistenceRuntime
    -> SessionRequest
    -> SessionBackend
    -> SessionTitleManager
    -> Chan RecapRequest
    -> SessionEnv
buildSessionEnv
        host
        controls
        skillsRuntime
        loopRuntime
        persistenceRuntime
        SessionRequest{..}
        SessionBackend{..}
        titleManager
        recapRequests =
    SessionEnv
        { sessionLoop = loopRuntime.loopRuntimeConfig
        , sessionSteeringInputs = controls.controlSteeringInputs
        , sessionModelInfo = modelInfo
        , sessionBtwBackend = btwBackend
        , sessionQueueRecap = writeChan recapRequests
        , sessionCompact = persistenceRuntime.persistenceCompact
        , sessionRender = loopRuntime.loopRuntimeRender
        , sessionProvider = provider
        , sessionConnection = connectionId
        , sessionGatewayIdentity = gatewayIdentity
        , sessionModelCatalog = catalog
        , sessionGatewayModels = gatewayModelsRef
        , sessionDialect = dialect
        , sessionRecordImageGenerationInputs =
            recordImageGenerationInputs
        , sessionUnavailableProviders =
            controls.controlUnavailableProvidersRef
        , sessionStartupUnavailable =
            controls.controlStartupUnavailableRef
        , sessionState = host.hostSessionState
        , sessionParams = paramsRef
        , sessionContextOccupancy = contextOccupancyRef
        , sessionContextWindow = currentContextWindow
        , sessionPolicy = policyRef
        , sessionPersist = persist
        , sessionObservationPublisher = host.hostObservationPublisher
        , sessionObservationEnabled = isNothing startup.startupNativeHooks
        , sessionInboxRuntime = host.hostInboxRuntime
        , sessionDatabasePool =
            trustedPool startup.startupDatabaseStore
        , sessionTitleManager = titleManager
        , sessionTitleTurnCount = controls.controlTitleTurnCount
        , sessionPlanMode = planMode
        , sessionTaskPlan = taskPlan
        , sessionWorkspace = workspace
        , sessionProviderFallback =
            host.hostNativeCapabilities.nativeProviderFallback
        , sessionPreparedWorkspaceEnvironment =
            host.hostPreparedWorkspaceEnvironment
        , sessionMcpRegistrations = mcpRegistrations
        , sessionMcpWarnings = mcpWarnings
        , sessionMcpFleet = mcpFleet
        , sessionSetTempDir =
            loopRuntime.loopRuntimeShell.shellSetTempDir
        , sessionTokenProvider = tokenProvider
        , sessionOpenAiPool = openAiPool
        , sessionSkills = skillsRef
        , sessionSkillInvocations = skillInvocationsRef
        , sessionRefreshSkills = skillsRuntime.skillRefresh
        , sessionActiveToolNames =
            loopRuntime.loopRuntimeShell.shellActiveToolNames
        , sessionGrokRuntime = grokRuntime
        , sessionShellMode =
            loopRuntime.loopRuntimeShell.shellCurrentMode
        , sessionSetShellMode =
            loopRuntime.loopRuntimeShell.shellSetMode
        , sessionComputerUseEnabled =
            loopRuntime.loopRuntimeShell.shellComputerUseEnabled
        , sessionSetComputerUseEnabled =
            loopRuntime.loopRuntimeShell.shellSetComputerUseEnabled
        , sessionRefreshRequestParams =
            loopRuntime.loopRuntimeShell.shellRefreshRequestParams
        , sessionBackground = startup.startupBackground
        , sessionStdinControl = stdinControl
        , sessionDraft = startup.startupSessionState.sessionDraft
        , sessionPreviewId =
            startup.startupSessionState.sessionPreviewId
        , sessionInterrupt = interrupt
        , sessionRestartEffort = controls.controlRestartEffortRef
        , sessionLastFailedTurn = controls.controlLastFailedTurnRef
        , sessionStoreRoot = storeRoot
        , sessionAccount = accountRef
        , sessionAccountLabel = accountLabel
        , sessionSelectAccount = selectAccount
        , sessionTerminal = host.hostTerminal
        , sessionFullscreen = host.hostFullscreen
        , sessionSetWindowTitle =
            host.hostWindowTitle.windowTitleSet
        , sessionBeginWindowTitleBusy =
            host.hostWindowTitle.windowTitleBeginBusy
        , sessionEndWindowTitleBusy =
            host.hostWindowTitle.windowTitleEndBusy
        , sessionBeginTurnActivity =
            persistenceRuntime.persistenceBeginTurnActivity
        , sessionEndTurnActivity =
            persistenceRuntime.persistenceEndTurnActivity
        , sessionAgentViewport = Just controls.controlAgentViewport
        , sessionBeginSubagentTurn =
            loopRuntime.loopRuntimeSubagents.subagentBeginTurn
        , sessionFinishSubagentTurn =
            loopRuntime.loopRuntimeSubagents.subagentFinishTurn
        , sessionAbortSubagentTurn =
            loopRuntime.loopRuntimeSubagents.subagentAbortTurn
        , sessionConcurrentLimit =
            loopRuntime.loopRuntimeSubagents.subagentConcurrentLimit
        , sessionSetConcurrentLimit =
            loopRuntime.loopRuntimeSubagents.subagentSetConcurrentLimit
        , sessionOnPersisted =
            persistenceRuntime.persistenceOnPersisted
        , sessionReset = skillsRuntime.skillResetSession
        , sessionMessageClock = options.optMessageClock
        }

installSessionActions
    :: SessionRunnerContinuation
    -> SessionHostRuntime
    -> SessionControlRuntime
    -> SessionPersistenceRuntime
    -> SessionRequest
    -> SessionEnv
    -> Chan Text.Text
    -> Chan RecapRequest
    -> IO ()
installSessionActions
        callbacks
        host
        controls
        persistenceRuntime
        SessionRequest{..}
        env
        btwRequests
        recapRequests = do
    writeIORef
        automaticCompactionHookRef
        persistenceRuntime.persistenceCommitAutomaticCompaction
    writeIORef startup.startupRestartEffort \level -> do
        setSessionEffortText env level
        writeIORef controls.controlRestartEffortRef (Just level)
        requestCancel toolEnv.toolCancel
    gatewayAccess <- readIORef gatewayModelsRef
    forM_ host.hostFullscreen \runtime ->
        setFullscreenSessionActions
            runtime
            (Just (dictationTargetForSession provider gatewayAccess))
            (requestCancel toolEnv.toolCancel)
            (\pasted text -> do
                images <- loadImagesFromPastedText text
                let input = case images of
                        Just attached@(_:_) ->
                            userMessageWithAttachments
                                "Image attached."
                                (map ImageAttachmentItem attached)
                        _ -> UserMessage text
                callbacks.runnerPreparePromptSkillInputs
                    env pasted text [input] >>= \case
                        Left err ->
                            emitUiEvent runtime (UiErrorMessage err)
                                >> pure (Left err)
                        Right inputs ->
                            enqueueSteeringInputs
                                controls.controlSteeringInputs
                                inputs >>= \case
                                    Left err -> pure (Left err)
                                    Right () -> do
                                        emitUiEvent
                                            runtime
                                            (UiInputSteered text)
                                        pure (Right ()))
            (writeChan btwRequests)
            (\command -> do
                let copyImmediate label missing payload =
                        case payload of
                            Nothing ->
                                emitUiEvent runtime (UiErrorMessage missing)
                            Just value -> do
                                copied <- runtime.runtimeCopy value
                                emitUiEvent runtime $
                                    if copied
                                        then UiSystemMessage ("copied " <> label)
                                        else UiErrorMessage
                                            "terminal clipboard is unavailable"
                    showImmediate message =
                        emitUiEvent runtime (UiSystemMessage message)
                case command of
                    ReplCopy request
                        | request.copyResponseIndex == 1
                        , Nothing <- request.copyDestination ->
                        controls.controlReadLastAssistant
                            >>= copyImmediate
                                "last response"
                                "no assistant response to copy"
                    ReplCopyCode index -> do
                        answer <-
                            controls.controlReadLastAssistant
                        let label =
                                "code block " <> Text.pack (show index)
                        copyImmediate
                            label
                            (label <> " was not found")
                            (answer >>= fencedCodeBlock index)
                    ReplCopyDiff -> do
                        answer <-
                            controls.controlReadLastAssistant
                        copyImmediate
                            "diff block"
                            "no diff block was found"
                            (answer >>= lastDiffBlock)
                    ReplCopyPath ->
                        copyImmediate
                            "worktree path"
                            "worktree path is unavailable"
                            (Just (toText workspace.cwd))
                    ReplCopySession ->
                        currentSessionId persist >>= copyImmediate
                            "session id"
                            "this session has no persisted id yet"
                    ReplQueue -> do
                        prompts <-
                            toList
                                <$> queuedFullscreenInputDisplays
                                    runtime.runtimeInput
                        showImmediate (formatQueuedPrompts prompts)
                    ReplContext -> do
                        currentParams <- readSessionRequestParams env.sessionParams
                        history <- readLiveTranscript conversationRef
                        occupancy <- readIORef contextOccupancyRef
                        contextWindow <- currentContextWindow
                        activeTools <- env.sessionActiveToolNames
                        showImmediate $
                            formatContextReport
                                (maybe "<unknown>" id currentParams.model)
                                contextWindow
                                occupancy
                                currentParams
                                history
                                activeTools
                    _ -> pure ())
            (writeChan recapRequests (RecapSession RecapAuto))
            (\level ->
                readIORef startup.startupRestartEffort >>= ($ level))
            (noteFullscreenCtrlC interrupt)
            (readIORef startup.startupAgentSnapshot >>= id)
            (\target ->
                readIORef startup.startupAgentSelect >>= ($ target))

runSessionInteraction
    :: SessionRunnerContinuation
    -> SessionHostRuntime
    -> SkillContextRuntime
    -> SessionRequest
    -> SessionEnv
    -> IO RunResult
runSessionInteraction
        callbacks host skillsRuntime SessionRequest{..} env = do
    learnedSkills <- skillsRuntime.skillInitialize
    case pendingTurn of
        Just pending ->
            callbacks.runnerRunPendingTurn
                (if startup.startupFullscreenReused
                    then ContinuePendingTurn
                    else SubmitPendingTurn)
                env
                pending
        Nothing -> case promptRequest of
            Just request -> do
                inputs <-
                    case startup.startupNativeHooks >>= (.nativeInitialTurnInputs) of
                        Just nativeInputs -> pure nativeInputs
                        Nothing -> managedTurnInputs workspace.cwd request
                skillInputs <-
                    callbacks.runnerPreparePromptSkillInputs
                        env
                        False
                        request.managedTurnText
                        inputs
                        >>= either
                            (startupDie startup)
                            pure
                result <- runOneTurn env request.managedTurnText skillInputs
                callbacks.runnerFinishTurn env True result
            Nothing -> do
                initialPrompt <-
                    atomicModifyIORef'
                        startup.startupSessionState.sessionInitialPrompt
                        (\pending -> (Nothing, pending))
                case initialPrompt of
                    Just text -> runInteractiveInitialPrompt text
                    Nothing ->
                        case learnAboutUserOnboardingPrompt learnedSkills of
                            Just onboardingPrompt
                                | learnAboutUserRequested
                                , isNothing host.hostInitialPrevious ->
                                    runInteractiveInitialPrompt
                                        onboardingPrompt
                            _ | startup.startupBackground ->
                                -- Background turns are one-shot. Do not inherit
                                -- the parent's stdin by opening an idle REPL.
                                pure RunQuit
                            _ ->
                                readIORef
                                    startup.startupSessionState.sessionDraft
                                    >>= callbacks.runnerReplWithDraft env
  where
    runInteractiveInitialPrompt text = do
        skillInputs <-
            callbacks.runnerPreparePromptSkillInputs
                env
                False
                text
                [UserMessage text]
                >>= either
                    (startupDie startup)
                    pure
        forM_ host.hostFullscreen \runtime ->
            emitUiEvent runtime (UiUserSubmitted text)
        result <- runOneTurn env text skillInputs
        callbacks.runnerFinishTurn env False result

runSessionWorkers
    :: SessionRunnerContinuation
    -> SessionHostRuntime
    -> SessionTitleManager
    -> SessionEnv
    -> Chan Text.Text
    -> Chan RecapRequest
    -> IO RunResult
    -> IO RunResult
runSessionWorkers
        callbacks host titleManager env
        btwRequests recapRequests sessionAction = do
    let btwWorker = do
            question <- readChan btwRequests
            runBtwQuestion False env question
            btwWorker
        recapWorker = do
            request <- readChan recapRequests
            case request of
                RecapSession kind ->
                    callbacks.runnerRunSessionRecap False env kind
                RecapTurnSummary ->
                    callbacks.runnerRunSessionTurnSummary env
            recapWorker
        withMcpSkillWatcher action =
            case env.sessionMcpFleet of
                Nothing -> action
                Just fleet ->
                    withAsync (watchMcpSkills fleet) (const action)
        watchMcpSkills fleet = do
            previous <- MCP.mcpFleetSkillRegistrations fleet
            -- Close the gap between the startup snapshot and this watcher.
            env.sessionRefreshSkills True
            waitForChange fleet previous
        waitForChange fleet previous = do
            current <-
                MCP.mcpFleetWaitForSkillRegistrations fleet previous
            env.sessionRefreshSkills True
            waitForChange fleet current
    result <- withAsync (runSessionInboxOwner env) \_ ->
        withAsync host.hostWindowTitle.windowTitleWorker \_ ->
            withMcpSkillWatcher $
                case host.hostFullscreen of
                    Just _ ->
                        withAsync btwWorker \_ ->
                            withAsync recapWorker (const sessionAction)
                    Nothing ->
                        withAsync recapWorker (const sessionAction)
    _ <- waitForSessionTitleResults 5000000 titleManager
    applyPendingSessionTitles env
    pure result

runSessionInboxOwner :: SessionEnv -> IO ()
runSessionInboxOwner env =
    case env.sessionPersist of
        PersistenceDisabled -> forever (threadDelay 100000000)
        PersistenceEnabled slotRef -> do
            readIORef slotRef >>= \case
                PersistenceActive handle ->
                    void (tryPutMVar env.sessionInboxRuntime.inboxReady handle)
                PersistencePending{} -> pure ()
            handle <- takeMVar env.sessionInboxRuntime.inboxReady
            withOptionalSessionInboxServer
                env.sessionInboxRuntime.inboxMessages
                handle.sessionMeta.metaId
                handle.sessionDir
                (forwardInboxMessages env)

forwardInboxMessages :: SessionEnv -> IO ()
forwardInboxMessages env = forever do
    message <- atomically (takeInboxMessage env.sessionInboxRuntime.inboxMessages)
    injected <- injectInboxMessage env message
    unless injected $
        releaseInboxPending env.sessionInboxRuntime.inboxMessages

injectInboxMessage :: SessionEnv -> Text.Text -> IO Bool
injectInboxMessage env message = do
    active <- readIORef env.sessionInboxRuntime.inboxTurnActive
    case env.sessionFullscreen of
        Just runtime -> do
            result <- atomically $
                appendFullscreenInput runtime.runtimeInput FullscreenInput
                    { fullscreenInputLine = ReplText message
                    , fullscreenInputQueued = True
                    , fullscreenInputDisplay = Just message
                    , fullscreenInputFromInbox = True
                    }
            pure $ either (const False) (const True) result
        Nothing
            | active ->
                enqueueSteeringInputs
                    env.sessionSteeringInputs
                    [UserMessage message] >>= \case
                    Left _ -> pure False
                    Right () -> do
                        releaseInboxPending env.sessionInboxRuntime.inboxMessages
                        pure True
            | otherwise -> do
                atomically (putTMVar env.sessionInboxRuntime.inboxInline message)
                pure True

runSession
    :: SessionRunnerContinuation
    -> SessionRequest
    -> SessionBackend
    -> IO RunResult
runSession callbacks request sessionBackend = do
    host <- newSessionHostRuntime request
    withSessionTitleRuntime host request sessionBackend \titleManager -> do
        controls <- newSessionControlRuntime host request
        let skillsRuntime =
                buildSkillContextRuntime
                    callbacks host controls request sessionBackend
        loopRuntime <-
            newSessionLoopRuntime host controls request sessionBackend
        btwRequests <- newChan
        recapRequests <- newChan
        persistenceRuntime <-
            newSessionPersistenceRuntime host skillsRuntime request
        let env =
                buildSessionEnv
                    host
                    controls
                    skillsRuntime
                    loopRuntime
                    persistenceRuntime
                    request
                    sessionBackend
                    titleManager
                    recapRequests
        installSessionActions
            callbacks
            host
            controls
            persistenceRuntime
            request
            env
            btwRequests
            recapRequests
        runSessionWorkers
            callbacks
            host
            titleManager
            env
            btwRequests
            recapRequests
            (runSessionInteraction
                callbacks host skillsRuntime request env)
