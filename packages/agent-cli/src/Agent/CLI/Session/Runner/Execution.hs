-- | Construction and execution of a CLI agent session.
module Agent.CLI.Session.Runner.Execution
    ( AgentStepCache(..)
    , SessionRunnerContinuation(..)
    , runSession
    ) where
import Agent.CLI.CodeModeRuntime
import Agent.CLI.Claude
    ( ClaudeSessionRuntime(..)
    , installClaudeSessionRuntime
    )
import Agent.CLI.Compaction
    ( AutomaticCompactionBoundary(..)
    , CompactOutcome(..)
    , CompactionInstall(CompactionInstalled)
    , reportedOccupancy
    )
import Agent.CLI.Compaction.Projection (reportedContextTokens)
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
import Agent.CLI.Permission (promptPermission, promptRootAccess)
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
import Agent.CLI.SessionEnv
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
import Agent.CLI.Error
import Agent.CLI.Dialects
import Agent.CLI.Dictation (dictationTargetForSession)
import Agent.CLI.TUI.App
import Agent.CLI.TUI.Types (FullscreenRuntime(..))
import Agent.TUI.Model
import Agent.TUI.Motion
import Agent.CLI.WindowTitle
import Agent.CLI.Turn
import Agent.Cancel
import Agent.Loop
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
import Control.Concurrent.Async (Async, withAsync)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Concurrent.STM (STM)
import Control.Exception.Safe
    ( catchAny
    , mask_
    , onException
    , uninterruptibleMask_
    )
import Control.Monad (forM_, unless, void, when)
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
    , hostGrokFirstTurnContextRef :: !(IORef (Maybe Text.Text))
    , hostIoLock :: !(MVar ())
    , hostApprovalLock :: !(MVar ())
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
    grokFirstTurnContextRef <- newIORef initialGrokContext
    ioLock <- newMVar ()
    approvalLock <- newMVar ()
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
                                        withStdinPaused escPaused
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
    setPlanModeInputWaitHooks
        planMode
        windowTitle.windowTitleBeginInputWait
        windowTitle.windowTitleEndInputWait
    setToolHumanInputWaitHooks
        toolEnv
        windowTitle.windowTitleBeginInputWait
        windowTitle.windowTitleEndInputWait
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
        , hostGrokFirstTurnContextRef = grokFirstTurnContextRef
        , hostIoLock = ioLock
        , hostApprovalLock = approvalLock
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
        (readIORef paramsRef)
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
    , controlLastAssistantRef :: !(IORef (Maybe Text.Text))
    , controlModelRef :: !(IORef Text.Text)
    , controlUnavailableProvidersRef :: !(IORef (Set.Set Provider))
    , controlStartupUnavailableRef :: !(IORef (Maybe (STM ApiError)))
    , controlRestartEffortRef :: !(IORef (Maybe Text.Text))
    , controlLastFailedTurnRef :: !(IORef (Maybe PendingTurn))
    , controlTitleTurnCount :: !(IORef Int)
    , controlAgentViewportRuntime :: !AgentViewportRuntime
    , controlAgentViewport :: !AgentViewportEnv
    }

newSessionControlRuntime
    :: SessionHostRuntime
    -> SessionRequest
    -> IO SessionControlRuntime
newSessionControlRuntime host SessionRequest{..} = do
    toolRegistry <- requireToolRegistry allTools
    steeringInputs <- newSteeringInputs
    setBackgroundTaskHooks toolEnv BackgroundTaskHooks
        { backgroundTaskCompleted = \notice ->
            enqueueBackgroundCompletion
                steeringInputs
                notice.noticeKey
                (UserMessage notice.noticeBody) >>= \case
                    -- Completion callbacks run while their delivery gate is
                    -- held. Keep this hook non-blocking; UI reporting can
                    -- backpressure on a full mailbox.
                    Left _ -> pure False
                    Right inserted -> pure inserted
        , backgroundTaskDismissed =
            dismissBackgroundCompletion steeringInputs
        }
    spinnerRef <- newIORef Nothing
    renderStateRef <- newIORef emptyRenderState
    allowedToolsRef <- newIORef Set.empty
    computerUseEnabledRef <- newIORef $
        resolveComputerUseEnabled options startup.startupStdinTty
            && isJust
                (lookupAppTool
                    computerToolName
                    (sessionDirectTools refreshTools codeModeRuntime))
    lastAssistantRef <- newIORef Nothing
    modelRef <- newIORef =<< (currentModel <$> readIORef paramsRef)
    unavailableProvidersRef <- newIORef unavailableProviders
    startupUnavailableRef <- newIORef startupUnavailable
    restartEffortRef <- newIORef Nothing
    lastFailedTurnRef <- newIORef Nothing
    titleTurnCount <- newIORef =<< sessionTitleTurnCountFromSlot persist
    let loadSelectedAgent agentId = do
            effectiveModel <- readIORef modelRef
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
            , viewportConfigWorkspace = toText cwd
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
        , controlLastAssistantRef = lastAssistantRef
        , controlModelRef = modelRef
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
    lastAssistantRef = controls.controlLastAssistantRef
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
    installLearnedSkills context maximum queueContext =
        loadApplicableLearnedSkillsForStore
            startup.startupDatabaseStore
            databaseScopes >>= \case
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
                        home
                        cwd
                        []
                        Nothing
                        ((.catalogEnvironmentContext)
                            <$> codexCatalogSession)
                else
                    newIORef
                        ((.catalogEnvironmentContext)
                            <$> codexCatalogSession)
        freshSkills <-
            if loadsHostWorkspaceContext
                then loadSkillsCatalogQuiet options home projectRoot cwd
                else pure (SkillCatalog [] [])
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
        writeIORef lastAssistantRef Nothing
        writeIORef subagentSessions Map.empty
        writeIORef host.hostGrokFirstTurnContextRef Nothing
        resetAgentViewport agentViewportRuntime
        case multiCtx of
            Just ctx -> resetSubagentRegistry ctx.multiRegistry
            Nothing -> pure ()
        clearPendingInputs pendingNotices
        clearSteeringInputs steeringInputs
        readIORef toolEnv.toolSessionTmp >>= mapM_ resetToolSessionTemp
        reloadGeneratedContext
    refreshSkills queueContext = do
        refreshed <-
            if loadsHostWorkspaceContext
                then loadSkillsCatalogQuiet options home projectRoot cwd
                else pure (SkillCatalog [] [])
        (omitted, _) <-
            installSkills startupContext queueContext refreshed
        when queueContext $
            reportSkillCatalog True refreshed omitted
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
                then installLearnedSkills
                    startupContext
                    defaultLearnedSkillContextMaxChars
                    queueInitialContext
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
    -> (LoopEvent -> IO ())
    -> SessionLoopEventRuntime
buildSessionLoopEventRuntime
        host controls SessionRequest{..} managedLoopPublisher =
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
        , renderModelRef = controls.controlModelRef
        , renderNativeProgress =
            host.hostStderrTty
                && terminal.terminalNativeProgress
                && nativeProgressAnimationEnabled options.optMotionMode
        , renderMotionMode = options.optMotionMode
        , renderWorkspace = toText cwd
        }
    emitLoop event = do
        recordAgentViewportEvent agentViewportRuntime event
        forM_ startup.startupNativeHooks \hooks ->
            hooks.nativeOnLoopEvent event
        managedLoopPublisher event
        case event of
            TurnFinished turn -> do
                history <- readLiveTranscript conversationRef
                forM_ (reportedContextTokens turn.tokenUsage) \tokens ->
                    writeIORef contextOccupancyRef $
                        Just (reportedOccupancy tokens (length history))
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
                        params <- readIORef paramsRef
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
                                    withStdinPaused escPaused do
                                        color <-
                                            resolveColor host.hostStderrHandle
                                        promptPermission
                                            color
                                            (toText cwd)
                                            requested)
                                reportLineApproval
                                (saveProjectAutoApprove projectRoot True)
                        Just runtime ->
                            approve
                                (requestFullscreenPermission
                                    runtime
                                    (toText cwd))
                                (\case
                                    ApprovalWarning _ -> pure ()
                                    ApprovalSuccess message ->
                                        emitUiEvent runtime
                                            (UiSetNotice
                                                (Just
                                                    (successNotice
                                                        message))))
                                (saveProjectAutoApprove projectRoot True)
        classify = const (pure classifiedReadOnly)
        approve request report persist =
            approveToolDecisionWithReporterAndPersistenceClassified
                classify
                (\requested ->
                    withToolHumanInputWait toolEnv $
                        withMVar host.hostIoLock \_ ->
                            request requested)
                (\notice ->
                    withMVar host.hostIoLock \_ ->
                        report notice)
                persist
                policyRef
                controls.controlAllowedToolsRef
                controls.controlToolRegistry
                planMode
                call
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
                            cwd
                            sessionTmp
                            today
                            (isOneShot options)
            toolSchemas =
                case codeModeRuntime of
                    Just _ ->
                        schemasFromAppToolsCodeModeWithHostedSearch
                            nativeCapabilities.nativeProviderHostedTools
                            dialect
                            (providerVisibleTools enabledTools)
                    Nothing ->
                        schemasFromAppToolsWithHostedSearch
                            nativeCapabilities.nativeProviderHostedTools
                            dialect
                            enabledTools
        modifyIORef' paramsRef
            (setRequestInstructionsAndTools
                instructionText
                (Just toolSchemas))
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
        ghciEnabled <- readIORef ghciEnabledRef
        bashEnabled <- readIORef bashEnabledRef
        computerUseEnabled <- readIORef computerUseEnabledRef
        refreshSessionParams
            ghciEnabled
            bashEnabled
            computerUseEnabled

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
        saveProjectMaxConcurrentAgents projectRoot next
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
                            result <- dispatchRegisteredToolCall
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
    sessionDir <- readIORef planMode.planSessionDir
    forM_ sessionDir (writeIORef storeRoot . Just)
    let eventRuntime =
            buildSessionLoopEventRuntime
                host controls request managedLoopPublisher
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
    turnActivityRef <- newIORef Nothing
    turnIsActiveRef <- newIORef False
    nativeSessionIdRef <- newIORef Nothing
    let acquireTurnActivity handle = mask_ do
            current <- readIORef turnActivityRef
            if isNothing current
                then
                    acquireSessionActivityLock
                        handle.sessionDir
                        handle.sessionMeta.metaId >>= \case
                            -- The activity lock is an external status marker;
                            -- the lifetime session lock remains authoritative.
                            Left _ -> pure False
                            Right lock -> do
                                writeIORef turnActivityRef (Just lock)
                                pure True
                else pure False
        beginTurnActivity = mask_ do
            -- Mark first so a concurrently completed first persistence sees
            -- the active turn and attempts the activity marker itself.
            writeIORef turnIsActiveRef True
            case persist of
                PersistenceDisabled -> pure ()
                PersistenceEnabled slotRef ->
                    readIORef slotRef >>= \case
                        PersistencePending{} -> pure ()
                        PersistenceActive handle ->
                            void (acquireTurnActivity handle)
        endTurnActivity = do
            writeIORef turnIsActiveRef False
            atomicModifyIORef' turnActivityRef
                (\current -> (Nothing, current))
                >>= mapM_ releaseSessionLock
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
            active <- readIORef turnIsActiveRef
            acquired <-
                if active
                    then acquireTurnActivity handle
                    else pure False
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
        , sessionConversation = conversationRef
        , sessionAutomaticCompaction = automaticCompactionRef
        , sessionParams = paramsRef
        , sessionContextOccupancy = contextOccupancyRef
        , sessionContextWindow = currentContextWindow
        , sessionPolicy = policyRef
        , sessionPersist = persist
        , sessionDatabasePool =
            trustedPool startup.startupDatabaseStore
        , sessionTitleManager = titleManager
        , sessionTitleTurnCount = controls.controlTitleTurnCount
        , sessionPlanMode = planMode
        , sessionTaskPlan = taskPlan
        , sessionProjectRoot = projectRoot
        , sessionCwd = cwd
        , sessionProviderFallback =
            host.hostNativeCapabilities.nativeProviderFallback
        , sessionPreparedWorkspaceEnvironment =
            host.hostPreparedWorkspaceEnvironment
        , sessionHome = home
        , sessionMcpRegistrations = mcpRegistrations
        , sessionMcpWarnings = mcpWarnings
        , sessionMcpFleet = mcpFleet
        , sessionSetTempDir =
            loopRuntime.loopRuntimeShell.shellSetTempDir
        , sessionTokenProvider = tokenProvider
        , sessionOpenAiPool = openAiPool
        , sessionStartupContext = startupContext
        , sessionGrokFirstTurnContext =
            host.hostGrokFirstTurnContextRef
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
        , sessionBackground = startup.startupBackground
        , sessionEscPaused = escPaused
        , sessionDraft = startup.startupSessionState.sessionDraft
        , sessionPreviewId =
            startup.startupSessionState.sessionPreviewId
        , sessionInterrupt = interrupt
        , sessionRestartEffort = controls.controlRestartEffortRef
        , sessionLastFailedTurn = controls.controlLastFailedTurnRef
        , sessionStoreRoot = storeRoot
        , sessionUsage = usageRef
        , sessionAccount = accountRef
        , sessionAccountId = accountIdRef
        , sessionAccountSelectionId = selectionRef
        , sessionAccountLabel = accountLabel
        , sessionSelectAccount = selectAccount
        , sessionLastAssistant = controls.controlLastAssistantRef
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
                        readIORef controls.controlLastAssistantRef
                            >>= copyImmediate
                                "last response"
                                "no assistant response to copy"
                    ReplCopyCode index -> do
                        answer <-
                            readIORef controls.controlLastAssistantRef
                        let label =
                                "code block " <> Text.pack (show index)
                        copyImmediate
                            label
                            (label <> " was not found")
                            (answer >>= fencedCodeBlock index)
                    ReplCopyDiff -> do
                        answer <-
                            readIORef controls.controlLastAssistantRef
                        copyImmediate
                            "diff block"
                            "no diff block was found"
                            (answer >>= lastDiffBlock)
                    ReplCopyPath ->
                        copyImmediate
                            "worktree path"
                            "worktree path is unavailable"
                            (Just (toText cwd))
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
                        currentParams <- readIORef env.sessionParams
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
                inputs <- managedTurnInputs cwd request
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
    result <- withAsync host.hostWindowTitle.windowTitleWorker \_ ->
        case host.hostFullscreen of
            Just _ ->
                withAsync btwWorker \_ ->
                    withAsync recapWorker (const sessionAction)
            Nothing ->
                withAsync recapWorker (const sessionAction)
    _ <- waitForSessionTitleResults 5000000 titleManager
    applyPendingSessionTitles env
    pure result

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
