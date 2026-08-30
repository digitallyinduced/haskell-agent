-- | Construction and execution of a CLI agent session.
module Agent.CLI.Session.Runner.Execution
    ( AgentStepCache(..)
    , SessionRunnerContinuation(..)
    , runSession
    ) where
import Agent.CLI.CodeModeRuntime
import Agent.CLI.Compaction
    ( AutomaticCompactionBoundary(..)
    , CompactOutcome(..)
    , CompactionInstall(CompactionInstalled)
    )
import Agent.Responses.LoopBackend (turnInputsToItems)
import Agent.CLI.Session.Runner.Types
    ( AgentStepCache(..)
    , SessionRunnerContinuation(..)
    )
import Agent.CLI.AgentViewport
import Agent.CLI.NativeAgents
import Agent.Tools.OutputArtifact
import Agent.CLI.SessionTitle
import Agent.Concurrent
import Agent.CLI.ManagedTurn
import Agent.CLI.GatewayBridge
import Agent.CLI.Notification
    ( AttentionRequest(PermissionRequested)
    , notifyAttention
    )
import Agent.CLI.Approval
import Agent.CLI.Permission (promptRootAccess)
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
import Agent.CLI.Session.Runtime.Types
import Agent.CLI.Interrupt
import Agent.Store.Postgres
import Agent.CLI.Project
import Agent.CLI.Prompt
import Agent.CLI.ProviderTransition
import Agent.CLI.SessionState
import Agent.CLI.Render
import Agent.CLI.Session
import Agent.CLI.Session.History
import Agent.CLI.SessionEnv
import Agent.CLI.Session.Interaction
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
import Agent.CLI.TUI.App
import qualified Agent.CLI.TUI.Bridge as TuiBridge
import Agent.TUI.Model
import Agent.TUI.Motion
import Agent.CLI.WindowTitle
import Agent.CLI.Turn
import Agent.Cancel
import Agent.Loop
import Agent.Dialect
import Agent.Skills
import Agent.Responses.Types
import Agent.Subagents
import Agent.Subagents.TaskPath
import Agent.ToolDispatch
import Agent.Tools.MultiAgents
import Agent.Tools.PlanMode
import Agent.Tools.Types
import Agent.OsPath
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import Control.Concurrent.MVar (newMVar, withMVar)
import Control.Exception.Safe (catchAny, uninterruptibleMask_)
import Control.Monad (forM_, unless, void, when)
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Set as Set
import Data.Time.Clock (getCurrentTime, utctDay)
import System.Mem.StableName (makeStableName)
runSession
    :: SessionRunnerContinuation
    -> SessionRequest
    -> SessionBackend
    -> IO RunResult
runSession callbacks SessionRequest{..} SessionBackend{..} = do
  initialPrevious <- readLivePreviousResponseId conversationRef
  ioLock <- newMVar ()
  let fullscreen = startup.startupFullscreen
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
          withMVar ioLock \_ ->
              case promptRequest of
                  Just request
                      | isJust request.managedTurnBridgeDirectory ->
                          requestManagedRootAccess request root
                  _ -> case fullscreen of
                      Just runtime -> do
                          notifyAttention stderrHandle PermissionRequested
                          maybe False (== 0)
                              <$> requestFullscreenChoiceWithBody
                                  runtime
                                  "Filesystem access requested"
                                  ("Allow access to " <> toText root
                                      <> " for this session?")
                                  0
                                  [ ("Allow directory for this session", "")
                                  , ("Deny", "")
                                  ]
                      Nothing ->
                          withStdinPaused escPaused (promptRootAccess useColor root)
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
  setToolRootAccessRequest toolEnv (Just requestRootAccess)
  let setWindowTitle = windowTitle.windowTitleSet
      beginWindowTitleBusy = windowTitle.windowTitleBeginBusy
      endWindowTitleBusy = windowTitle.windowTitleEndBusy
      windowTitleWorker = windowTitle.windowTitleWorker
      showTitleEvent = \case
        SessionTitleGenerated SessionTitleResult{..} ->
          case persist of
              PersistenceDisabled -> pure ()
              PersistenceEnabled slotRef ->
                  readIORef slotRef >>= \case
                      PersistenceActive handle
                          | handle.sessionMeta.metaId == resultSessionId
                          , not handle.sessionMeta.metaTitleIsManual ->
                              setWindowTitle
                                  (cliWindowTitle handle.sessionMeta.metaCwd
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
                                          color <- resolveColor stderrHandle
                                          putTextLn stderrHandle
                                              (roleWarn color
                                                  (glyphWarn <> message))
                      _ -> pure ()
  withSessionTitleManager btwBackend (readIORef paramsRef) showTitleEvent \titleManager -> do
    toolRegistry <- requireToolRegistry allTools
    steeringInputs <- newSteeringInputs
    let previewIdRef = startup.startupSessionState.sessionPreviewId
    spinnerRef <- newIORef Nothing
    renderStateRef <- newIORef emptyRenderState
    allowedToolsRef <- newIORef Set.empty
    lastAssistantRef <- newIORef Nothing
    modelRef <- newIORef =<< (currentModel <$> readIORef paramsRef)
    unavailableProvidersRef <- newIORef unavailableProviders
    startupUnavailableRef <- newIORef startupUnavailable
    restartEffortRef <- newIORef Nothing
    lastFailedTurnRef <- newIORef Nothing
    titleTurnCount <- newIORef =<< sessionTitleTurnCountFromSlot persist
    selectedAgent <- newIORef AgentRoot
    nativeAgentsRef <- newIORef emptyNativeAgentStore
    agentStepCache <- newIORef (Map.empty :: Map.Map AgentTarget AgentStepCache)
    let cachedAgentSteps target variant items build = do
            transcriptName <- makeStableName items
            cache <- readIORef agentStepCache
            case Map.lookup target cache of
                Just cached
                    | cached.cachedTranscript == transcriptName
                    , cached.cachedVariant == variant ->
                        pure cached.cachedSteps
                _ -> do
                    let steps = build items
                    atomicModifyIORef' agentStepCache \current ->
                        ( Map.insert target (AgentStepCache
                            { cachedTranscript = transcriptName
                            , cachedVariant = variant
                            , cachedSteps = steps
                            })
                            current
                        , ()
                        )
                    pure steps
        loadAgentSnapshot includeSummaries = do
            rootItems <- readLiveTranscript conversationRef
            currentSelected <- readIORef selectedAgent
            nativeAgents <-
                atomicModifyIORef' nativeAgentsRef \current ->
                    let restored =
                            restoreNativeAgents currentSelected rootItems current
                    in (restored, restored)
            agents <- case multiCtx of
                Nothing -> pure []
                Just ctx -> listAgents ctx.multiRegistry Nothing
            let availableTargets =
                    AgentRoot
                        : [ AgentChild agentId
                          | (_, agentId, _) <- agents
                          ]
                        <> nativeAgentTargets nativeAgents
            selected <-
                atomicModifyIORef' selectedAgent \current ->
                    let reconciled =
                            TuiBridge.reconcileAgentSelection
                                availableTargets
                                current
                    in (reconciled, reconciled)
            modifyIORef' nativeAgentsRef
                (setNativeAgentSelection
                    (case selected of
                        AgentNative identifier -> Just identifier
                        _ -> Nothing))
            modifyIORef' agentStepCache
                (\cache ->
                    Map.restrictKeys cache
                        (Set.fromList availableTargets))
            sessions <- readIORef subagentSessions
            let transcriptLines target items
                    | null agents = []
                    | target == selected = case target of
                        AgentRoot ->
                            responseItemPreviewLines 12 items
                        AgentChild _
                            | includeSummaries ->
                                responseItemPreviewLines 12 items
                            | otherwise ->
                                []
                        AgentNative nativeId ->
                            maybe [] nativeAgentTranscript
                                (nativeAgentLookup nativeId nativeAgents)
                    | includeSummaries =
                        responseItemPreviewLines 0 items
                    | otherwise = []
                conversationFor target status items
                    | includeSummaries = initialUiState
                    | target /= selected = initialUiState
                    | target == AgentRoot = initialUiState
                    | AgentNative nativeId <- target =
                        maybe initialUiState nativeAgentConversation
                            (nativeAgentLookup nativeId nativeAgents)
                    | otherwise =
                        settleConversation items status $
                            responseItemsToUiStateRelative
                                options.optShowRawReasoning
                                (toText cwd)
                                items
                settleConversation items status conversation =
                    case status of
                        Pending -> conversation
                        Running -> conversation
                        Completed result ->
                            let settled =
                                    finalizeAll BlockComplete conversation
                            in case result of
                                Just text
                                    | null items
                                    , not (Text.null (Text.strip text)) ->
                                        reduceUi
                                            (UiAssistantHistory
                                                (Text.strip text))
                                            settled
                                _ -> settled
                        Errored message ->
                            reduceUi
                                (UiErrorMessage
                                    (if Text.null (Text.strip message)
                                        then "Agent failed."
                                        else Text.strip message))
                                (finalizeAll BlockFailed conversation)
                        Interrupted ->
                            finalizeAll BlockCancelled conversation
                        Closed ->
                            finalizeAll BlockComplete conversation
                        NotFound ->
                            reduceUi
                                (UiErrorMessage
                                    "Agent transcript is unavailable.")
                                (finalizeAll BlockFailed conversation)
                  where
                    finalizeAll terminal ui =
                        reduceUi
                            (UiTurnEnded terminal)
                            ui { uiTurnStartBlock = 0 }
            rootSteps <-
                if null agents
                    then pure []
                    else cachedAgentSteps
                        AgentRoot
                        Nothing
                        rootItems
                        (responseItemStepPreviewsRelative (toText cwd) 2)
            let rootEntry = AgentEntry
                    { agentTarget = AgentRoot
                    , agentPath = "/root"
                    , agentStatus = "active"
                    , agentModel = Nothing
                    , agentSteps = rootSteps
                    , agentTranscript =
                        transcriptLines AgentRoot rootItems
                    , agentConversation = initialUiState
                    }
            children <- mapConcurrentlyBounded 8
                (materializeChild
                    transcriptLines
                    conversationFor
                    sessions)
                agents
            let nativeEntries = nativeAgentEntries selected nativeAgents
            pure (selected, rootEntry : children <> nativeEntries)
          where
            materializeChild
                    transcriptLines
                    conversationFor
                    sessions
                    (path, agentId, status) = do
                let target = AgentChild agentId
                items <- case Map.lookup agentId sessions of
                    Nothing -> pure []
                    Just session -> readIORef session.subSessionTranscript
                steps <- cachedAgentSteps
                    target
                    (Just status)
                    items
                    (agentStepsForStatusRelative (toText cwd) 2 status)
                let transcript =
                        transcriptLines target items
                            <> case status of
                                Completed (Just result)
                                    | null items
                                    , not (Text.null (Text.strip result)) ->
                                        ["assistant: " <> Text.strip result]
                                Errored message ->
                                    ["error: " <> Text.strip message]
                                _ -> []
                pure AgentEntry
                    { agentTarget = target
                    , agentPath = taskPathText path
                    , agentStatus = formatAgentStatus status
                    , agentModel =
                        (.subSessionEffectiveModel)
                            <$> Map.lookup agentId sessions
                    , agentSteps = steps
                    , agentTranscript = transcript
                    , agentConversation =
                        conversationFor target status items
                    }
        hydrateSelectedAgent agentId = do
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
        selectAgent target = do
            previous <- readIORef selectedAgent
            when (previous /= target) $
                releaseSelectedAgent previous
            case target of
                AgentRoot -> pure ()
                AgentNative _ -> pure ()
                AgentChild agentId -> do
                    session <-
                        (Just <$>
                            hydrateSelectedAgent agentId)
                            `catchAny` \err -> do
                                reportSessionError
                                    ("failed to load selected agent: "
                                        <> formatException err)
                                pure Nothing
                    forM_ session \selectedSession -> do
                        withMVar selectedSession.subSessionHydrated \_ ->
                            writeIORef selectedSession.subSessionPinned True
                        void
                            (hydrateSelectedAgent agentId)
                            `catchAny` \err ->
                                reportSessionError
                                    ("failed to pin selected agent: "
                                        <> formatException err)
            writeIORef selectedAgent target
            modifyIORef' nativeAgentsRef
                (setNativeAgentSelection
                    (case target of
                        AgentNative identifier -> Just identifier
                        _ -> Nothing))
        releaseSelectedAgent = \case
            AgentRoot -> pure ()
            AgentNative _ -> pure ()
            AgentChild agentId -> do
                sessions <- readIORef subagentSessions
                forM_ (Map.lookup agentId sessions) \session -> do
                    withMVar session.subSessionHydrated \_ ->
                        writeIORef session.subSessionPinned False
                    case multiCtx of
                        Nothing -> pure ()
                        Just ctx -> do
                            status <- getStatus ctx.multiRegistry agentId
                            void $
                                persistAndEvictSubagentSessionWithStatus
                                    storeRoot ctx.multiRegistry agentTypes
                                    agentId status session
        agentViewport = AgentViewportEnv
            { viewportSelected = selectedAgent
            , viewportSelect = selectAgent
            , viewportEntries = snd <$> loadAgentSnapshot True
            }
    writeIORef startup.startupAgentSnapshot
        (loadAgentSnapshot False)
    writeIORef startup.startupAgentSelect selectAgent
    let installSkills context queueContext skills = do
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
        installLearnedSkills context maximum =
            loadApplicableLearnedSkillsForStore
                startup.startupDatabaseStore
                databaseScopes >>= \case
                    Left err -> do
                        reportLearnedSkillWarning
                            ("learned skills unavailable: " <> err)
                        pure []
                    Right learnedSkills -> do
                        omitted <-
                            queueLearnedSkillContextWithOmissions
                                maximum
                                context
                                learnedSkills
                        when (omitted > 0) $
                            reportLearnedSkillWarning
                                ("learned skills: "
                                    <> Text.pack (show omitted)
                                    <> " omitted from model context due to the context budget")
                        pure learnedSkills
        reloadGeneratedContext = do
            freshAgents <-
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
                    ((.catalogEnvironmentContext) <$> codexCatalogSession)
            freshSkills <- loadSkillsCatalogQuiet options home projectRoot cwd
            (omitted, _) <-
                installSkills freshAgents True freshSkills
            reportSkillCatalog True freshSkills omitted
            void $ installLearnedSkills
                freshAgents
                defaultLearnedSkillContextMaxChars
            fresh <- readIORef freshAgents
            writeIORef startupContext fresh
        sessionReset = do
            resetLiveConversationWith
                resetBackendState
                conversationRef
                planMode
            writeIORef usageRef emptyTokenUsage
            modifyIORef' renderStateRef clearRenderTokenRate
            writeIORef lastAssistantRef Nothing
            writeIORef subagentSessions Map.empty
            writeIORef selectedAgent AgentRoot
            writeIORef nativeAgentsRef emptyNativeAgentStore
            writeIORef agentStepCache Map.empty
            case multiCtx of
                Just ctx -> resetSubagentRegistry ctx.multiRegistry
                Nothing -> pure ()
            clearPendingInputs pendingNotices
            clearSteeringInputs steeringInputs
            reloadGeneratedContext
        refreshSkills queueContext = do
            refreshed <- loadSkillsCatalogQuiet
                options home projectRoot cwd
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
    policyRef <- newIORef policy
    managedLoopPublisher <-
        maybe
            (pure (const (pure ())))
            newManagedLoopEventPublisher
            promptRequest
    let syncStore = do
            sessionDir <- readIORef planMode.planSessionDir
            case sessionDir of
                Just dir -> writeIORef storeRoot (Just dir)
                Nothing -> pure ()
    syncStore
    let render = RenderConfig
            { renderShowThinking = stderrTty
            , renderThinkingSpinner = spinnerRef
            , renderState = renderStateRef
            , renderColor = useColor
            , renderLock = ioLock
            , renderStdout = stdoutHandle
            , renderStderr = stderrHandle
            , renderModelRef = modelRef
            , renderNativeProgress =
                stderrTty
                    && terminal.terminalNativeProgress
                    && nativeProgressAnimationEnabled
                        options.optMotionMode
            , renderMotionMode = options.optMotionMode
            , renderWorkspace = toText cwd
            }
        emitLoop event = do
            atomicModifyIORef' nativeAgentsRef \current ->
                (applyNativeAgentEvent event current, ())
            managedLoopPublisher event
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
        shellToolAllowed call = do
            ghciEnabled <- readIORef ghciEnabledRef
            bashEnabled <- readIORef bashEnabledRef
            let toolName = canonicalToolName call.name
            pure $
                (not (isGhciToolName toolName) || ghciEnabled)
                    && (not (isBashToolName toolName) || bashEnabled)
        approveRegisteredTool call =
            withMVar ioLock \_ ->
                case promptRequest of
                    Just request
                        | isJust request.managedTurnBridgeDirectory ->
                            approveToolDecisionWithReporterAndPersistence
                                (requestManagedApproval request)
                                (const (pure ()))
                                (pure ())
                                policyRef
                                allowedToolsRef
                                toolRegistry
                                planMode
                                call
                    _ -> case fullscreen of
                        Nothing ->
                            withStdinPaused escPaused $
                                approveToolDecision
                                    policyRef allowedToolsRef toolRegistry planMode
                                    projectRoot cwd call
                        Just runtime ->
                            approveToolDecisionWithReporterAndPersistence
                                (requestFullscreenPermission runtime (toText cwd))
                                (\case
                                    ApprovalWarning _ -> pure ()
                                    ApprovalSuccess message ->
                                        emitUiEvent runtime
                                            (UiSetNotice
                                                (Just
                                                    (successNotice message))))
                                (saveProjectAutoApprove projectRoot True)
                                policyRef
                                allowedToolsRef
                                toolRegistry
                                planMode
                                call
        config = LoopConfig
            { loopBackend = backend
            , loopBackendState = BackendStateStore
                { readBackendState = readLiveTranscript conversationRef
                , commitBackendState = writeLiveTranscript conversationRef
                }
            , loopTools = toolRegistry
            , loopDispatch =
                defaultLoopDispatch
                    { toolDispatchFinalizeOutput = \call output ->
                        if call.callKind == ComputerCallKind
                            then pure output
                            else finalizeToolOutput toolEnv call output
                    }
            , loopMaxTurns = options.optMaxTurns
            , loopOnEvent = emitLoop
            , loopApprove = \call ->
                shellToolAllowed call >>= \case
                    False ->
                        pure (Left
                            ("Tool " <> call.name
                                <> " is disabled by the current /shell setting."))
                    True -> approveRegisteredTool call
            , loopReadSteering =
                readSteeringInputs steeringInputs
            , loopCommitSteering = \count ->
                commitSteeringInputs steeringInputs count
            , loopCancel = toolEnv.toolCancel
            }
        beginSubagentTurn = do
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
        activeShellTools ghciEnabled bashEnabled =
            filterGhciTools ghciEnabled
                (filterBashTools bashEnabled allTools)
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
            let active =
                    activeShellTools ghciEnabled bashEnabled
                internalNames = map (.appToolName) active
                projectedNames =
                    case dialectToolLayout dialect of
                        NoHostToolLayout -> []
                        FlatToolLayout
                            | dialectId dialect
                                == GrokBuildDialect ->
                                    map
                                        grokBuildPublicToolName
                                        internalNames
                            | otherwise -> internalNames
                        CollaborationNamespaceLayout ->
                            filter
                                (`notElem` multiAgentToolNames)
                                internalNames
                                <> if any
                                    (`elem` multiAgentToolNames)
                                    internalNames
                                    then ["collaboration"]
                                    else []
            pure $
                case dialectToolLayout dialect of
                    NoHostToolLayout -> []
                    _ -> hostedSearchToolNames dialect ++ projectedNames
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
        refreshShellParams ghciEnabled bashEnabled
            | isJust codeModeNestedSlot = pure ()
            | otherwise = do
                sessionTmp <- readIORef toolEnv.toolSessionTmp
                today <- utctDay <$> getCurrentTime
                let enabledTools = activeShellTools ghciEnabled bashEnabled
                    enabledNames = map (.appToolName) enabledTools
                    instructionText =
                        appendMcpInstructions mcpInstructions case codexCatalogSession of
                            Just catalog ->
                                catalog.catalogInstructionsFor
                                    enabledNames sessionTmp
                            Nothing ->
                                systemPromptForTools
                                    dialect
                                    enabledNames
                                    cwd
                                    sessionTmp
                                    today
                                    (isOneShot options)
                    toolSchemas = schemasFromAppTools dialect enabledTools
                modifyIORef' paramsRef
                    (setRequestInstructionsAndTools
                        instructionText
                        (Just toolSchemas))
        setShellMode mode = do
            let (ghciEnabled, bashEnabled) = shellModeFlags mode
            writeIORef ghciEnabledRef ghciEnabled
            writeIORef bashEnabledRef bashEnabled
            unless ghciEnabled suspendGhci
            refreshShellParams ghciEnabled bashEnabled
            pure ("shell tools: " <> shellModeLabel mode)
        setSessionTempDir tempDir = do
            setToolSessionTmp toolEnv (Just tempDir)
            ghciEnabled <- readIORef ghciEnabledRef
            bashEnabled <- readIORef bashEnabledRef
            refreshShellParams ghciEnabled bashEnabled
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
    forM_ codeModeNestedSlot \slot ->
        setCodeModeNestedInvoke slot \call -> do
            allowed <- shellToolAllowed call
            if not allowed
                then pure $ Left $
                    "Tool " <> call.name
                        <> " is disabled by the current /shell setting."
                else approveRegisteredTool call >>= \case
                    Left denial -> pure (Left denial)
                    Right False ->
                        pure (Left "Tool call rejected by user.")
                    Right True -> do
                        emitLoop (ToolStarted call)
                        result <- dispatchRegisteredToolCall
                            config.loopDispatch
                            toolRegistry
                            call
                        emitLoop (ToolFinished result)
                        pure (Right result.output)
    btwRequests <- newChan
    recapRequests <- newChan
    let
        reloadGeneratedContextSafely =
            reloadGeneratedContext `catchAny` \err ->
                reportSessionError
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
                                , turnUsage = Nothing
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
        env = SessionEnv
            { sessionLoop = config
            , sessionModelInfo = modelInfo
            , sessionBtwBackend = btwBackend
            , sessionQueueRecap = writeChan recapRequests
            , sessionCompact = compactRunnerWithContext
            , sessionRender = render
            , sessionProvider = provider
            , sessionConnection = connectionId
            , sessionModelCatalog = catalog
            , sessionDialect = dialect
            , sessionUnavailableProviders = unavailableProvidersRef
            , sessionStartupUnavailable = startupUnavailableRef
            , sessionConversation = conversationRef
            , sessionAutomaticCompaction = automaticCompactionRef
            , sessionParams = paramsRef
            , sessionPolicy = policyRef
            , sessionPersist = persist
            , sessionDatabasePool =
                trustedPool startup.startupDatabaseStore
            , sessionTitleManager = titleManager
            , sessionTitleTurnCount = titleTurnCount
            , sessionPlanMode = planMode
            , sessionProjectRoot = projectRoot
            , sessionCwd = cwd
            , sessionHome = home
            , sessionMcpRegistrations = mcpRegistrations
            , sessionMcpWarnings = mcpWarnings
            , sessionMcpFleet = mcpFleet
            , sessionSetTempDir = setSessionTempDir
            , sessionTokenProvider = tokenProvider
            , sessionOpenAiPool = openAiPool
            , sessionStartupContext = startupContext
            , sessionSkills = skillsRef
            , sessionSkillInvocations = skillInvocationsRef
            , sessionRefreshSkills = refreshSkills
            , sessionActiveToolNames = currentActiveToolNames
            , sessionGrokRuntime = grokRuntime
            , sessionShellMode = currentShellMode
            , sessionSetShellMode = setShellMode
            , sessionBackground = startup.startupBackground
            , sessionEscPaused = escPaused
            , sessionDraft = startup.startupSessionState.sessionDraft
            , sessionPreviewId = previewIdRef
            , sessionInterrupt = interrupt
            , sessionRestartEffort = restartEffortRef
            , sessionLastFailedTurn = lastFailedTurnRef
            , sessionStoreRoot = storeRoot
            , sessionUsage = usageRef
            , sessionAccount = accountRef
            , sessionAccountId = accountIdRef
            , sessionAccountSelectionId = selectionRef
            , sessionAccountLabel = accountLabel
            , sessionSelectAccount = selectAccount
            , sessionLastAssistant = lastAssistantRef
            , sessionTerminal = terminal
            , sessionFullscreen = fullscreen
            , sessionSetWindowTitle = setWindowTitle
            , sessionBeginWindowTitleBusy = beginWindowTitleBusy
            , sessionEndWindowTitleBusy = endWindowTitleBusy
            , sessionAgentViewport = Just agentViewport
            , sessionBeginSubagentTurn = beginSubagentTurn
            , sessionFinishSubagentTurn = finishSubagentTurn
            , sessionAbortSubagentTurn = abortSubagentTurn
            , sessionConcurrentLimit = currentConcurrentLimit
            , sessionSetConcurrentLimit = setConcurrentLimit
            , sessionOnPersisted = onPersisted
            , sessionReset = sessionReset
            }
    writeIORef automaticCompactionHookRef commitAutomaticCompaction
    writeIORef startup.startupRestartEffort \level -> do
        setSessionEffortText env level
        writeIORef restartEffortRef (Just level)
        requestCancel toolEnv.toolCancel
    forM_ fullscreen \runtime ->
        setFullscreenSessionActions
            runtime
            (requestCancel toolEnv.toolCancel)
            (\text -> do
                images <- loadImagesFromPastedText text
                let input = case images of
                        Just attached@(_:_) ->
                            UserMultimodal
                                { userText = "Image attached."
                                , userImages = attached
                                }
                        _ -> UserMessage text
                callbacks.runnerPreparePromptSkillInputs
                    env text [input] >>= \case
                        Left err ->
                            emitUiEvent runtime (UiErrorMessage err)
                                >> pure (Left err)
                        Right inputs ->
                            enqueueSteeringInputs steeringInputs inputs >>= \case
                                Left err -> pure (Left err)
                                Right () -> do
                                    emitUiEvent runtime (UiInputSteered text)
                                    pure (Right ()))
            (writeChan btwRequests)
            (writeChan recapRequests (RecapSession RecapAuto))
            (\level ->
                readIORef startup.startupRestartEffort >>= ($ level))
            (noteFullscreenCtrlC interrupt)
            (readIORef startup.startupAgentSnapshot >>= id)
            (\target ->
                readIORef startup.startupAgentSelect >>= ($ target))
    let initializeSkills = do
            markStartupStage startup "Loading skills…"
            skills <- loadSkillsCatalogQuiet
                options home projectRoot cwd
            (omitted, _) <- installSkills startupContext
                needsInitialContext
                skills
            reportSkillCatalog (isNothing fullscreen) skills omitted
            learnedSkills <-
                if needsInitialContext
                    then installLearnedSkills
                        startupContext
                        defaultLearnedSkillContextMaxChars
                    else pure []
            callbacks.runnerFinishStartup startup
            pure learnedSkills
        sessionAction = do
            learnedSkills <- initializeSkills
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
                                request.managedTurnText
                                inputs
                                >>= either
                                    (startupDie startup . Text.unpack)
                                    pure
                        result <- runOneTurn env request.managedTurnText skillInputs
                        callbacks.runnerFinishTurn env True result
                    Nothing ->
                        case learnAboutUserOnboardingPrompt learnedSkills of
                            Just onboardingPrompt
                                | learnAboutUserRequested
                                , isNothing initialPrevious -> do
                                    skillInputs <-
                                        callbacks.runnerPreparePromptSkillInputs
                                            env
                                            onboardingPrompt
                                            [UserMessage onboardingPrompt]
                                            >>= either
                                                (startupDie startup . Text.unpack)
                                                pure
                                    forM_ fullscreen \runtime ->
                                        emitUiEvent runtime
                                            (UiUserSubmitted onboardingPrompt)
                                    result <-
                                        runOneTurn
                                            env
                                            onboardingPrompt
                                            skillInputs
                                    callbacks.runnerFinishTurn env False result
                            _ ->
                                readIORef
                                    startup.startupSessionState.sessionDraft
                                    >>= callbacks.runnerReplWithDraft env
        btwWorker = do
            question <- readChan btwRequests
            runBtwQuestion False env question
            btwWorker
        recapWorker = do
            request <- readChan recapRequests
            case request of
                RecapSession kind -> callbacks.runnerRunSessionRecap False env kind
                RecapTurnSummary -> callbacks.runnerRunSessionTurnSummary env
            recapWorker
    result <- withAsync windowTitleWorker \_ ->
        case fullscreen of
            Just _ ->
                withAsync btwWorker \_ ->
                    withAsync recapWorker (const sessionAction)
            Nothing ->
                withAsync recapWorker (const sessionAction)
    _ <- waitForSessionTitleResults 5000000 titleManager
    applyPendingSessionTitles env
    pure result
