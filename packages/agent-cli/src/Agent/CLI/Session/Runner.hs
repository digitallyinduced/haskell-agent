-- | Construction and execution of a CLI agent session.
-- Import surface intentionally mirrors only the session runner dependencies.
module Agent.CLI.Session.Runner
    ( AgentStepCache(..)
    , SessionRunnerContinuation(..)
    , runSession
    ) where

import Agent.CLI.AgentViewport
    ( AgentEntry(..)
    , AgentStep
    , AgentTarget(..)
    , AgentViewportEnv(..)
    , agentStepsForStatusRelative
    , formatAgentStatus
    , responseItemPreviewLines
    , responseItemStepPreviewsRelative
    , responseItemsToUiStateRelative
    )
import Agent.CLI.SessionTitle
    ( SessionTitleEvent(..)
    , SessionTitleFailure(..)
    , SessionTitleResult(..)
    , waitForSessionTitleResults
    , withSessionTitleManager
    )
import Agent.Concurrent (mapConcurrentlyBounded)
import Agent.CLI.ManagedTurn
    ( ManagedTurnRequest(..)
    , managedTurnInputs
    )
import Agent.CLI.GatewayBridge
    ( newManagedLoopEventPublisher
    , requestManagedApproval
    )
import Agent.CLI.Approval
    ( ApprovalNotice(..)
    , approveToolDecision
    , approveToolDecisionWithReporterAndPersistence
    )
import Agent.CLI.Recap
    ( RecapKind(..)
    , RecapRequest(..)
    )
import Agent.CLI.CancelWatch (withStdinPaused)
import Agent.CLI.Clipboard (loadImagesFromPastedText)
import Agent.CLI.Command
import Agent.CLI.LearnedSkills
    ( defaultLearnedSkillContextMaxChars
    , queueLearnedSkillContextWithOmissions
    )
import Agent.CLI.LearnedSkills.Store
    ( loadApplicableLearnedSkillsForStore
    )
import Agent.CLI.Options (CliOptions(..), isOneShot)
import Agent.CLI.PendingInputs (clearPendingInputs)
import Agent.CLI.Runtime.Types
    ( PendingTurnPresentation(..)
    , RunResult(..)
    )
import Agent.CLI.Session.Runtime.Types
    ( SessionBackend(..)
    , SessionRequest(..)
    , StartupRuntime(..)
    )
import Agent.CLI.Interrupt (noteFullscreenCtrlC)
import Agent.Store.Postgres
    ( trustedPool )
import Agent.CLI.Project
    ( saveProjectAutoApprove
    , saveProjectMaxConcurrentAgents
    )
import Agent.CLI.Prompt
    ( systemPromptForTools )
import Agent.CLI.Resume (resumeNeedsGeneratedContext)
import Agent.CLI.ProviderTransition
    ( PendingTurn(..)
    , TurnResult(..)
    )
import Agent.CLI.SessionState (SessionState(..))
import Agent.CLI.Render
    ( RenderConfig(..)
    , RenderState(..)
    , beginRenderTurn
    , clearRenderTokenRate
    , countGenerationChars
    , emptyRenderState
    , putTextLn
    , recordRenderTurnRate
    , renderEvent
    , resetRenderGeneration
    )
import Agent.CLI.Session
import Agent.CLI.Session.History
    ( readLivePreviousResponseId
    , readLiveTranscript
    , resetLiveConversationWith
    , writeLiveTranscript
    )
import Agent.CLI.SessionEnv (SessionEnv(..))
import Agent.CLI.Session.Interaction (runBtwQuestion, setSessionEffort)
import Agent.CLI.Skills
    ( installSkillCatalogWithOmissions, installSkillToolRoots
    , loadSkillsCatalogQuiet, reservedSlashNames
    )
import Agent.CLI.StartupContext (loadAgentsContext)
import Agent.CLI.Startup.Auth
    ( learnAboutUserOnboardingPrompt
    , markStartupStage
    , startupDie
    )
import Agent.CLI.Subagents.Runtime
    ( SubagentSession(..)
    , lookupOrCreateSubagentSession
    , persistAndEvictSubagentSessionWithStatus
    )
import Agent.CLI.Style
    ( cliWindowTitle
    , glyphSession
    , glyphWarn
    , roleMuted
    , roleWarn
    , setCliWindowTitle
    )
import Agent.CLI.Terminal
    ( TerminalCapabilities(..)
    , resolveColor
    )
import Agent.CLI.Request (setRequestInstructionsAndTools)
import Agent.CLI.Tools
    ( hostedSearchToolNames
    , requireToolRegistry
    , schemasFromAppTools
    )
import Agent.CLI.Error (formatException)
import Agent.CLI.Dialects
    ( filterBashTools
    , filterGhciTools
    , isBashToolName
    , isGhciToolName
    )
import Agent.CLI.TUI.App
    ( emitUiEvent
    , requestFullscreenPermission
    , setFullscreenSessionActions
    , setFullscreenWindowTitle
    )
import qualified Agent.CLI.TUI.Bridge as TuiBridge
import Agent.TUI.Model
    ( BlockState(..)
    , UiEvent(..)
    , UiState(..)
    , successNotice
    , initialUiState
    , reduceUi
    )
import Agent.TUI.Motion (nativeProgressAnimationEnabled)
import Agent.CLI.WindowTitle
    ( WindowTitleController(..)
    , newWindowTitleController
    )
import Agent.CLI.Turn (applyPendingSessionTitles, runOneTurn)
import Agent.Cancel (requestCancel)
import Agent.Loop
import Agent.Dialect
    ( DialectId(..)
    , ToolLayout(..)
    , dialectId
    , dialectToolLayout
    , grokBuildPublicToolName
    )
import Agent.Skills
    ( SkillCatalog(..)
    , SkillWarning(..)
    )
import Agent.Responses.Types
import Agent.Subagents
    ( SubagentConfig(..)
    , SubagentStatus(..)
    , abortRootTurn
    , beginRootTurn
    , resetSubagentRegistry
    , defaultSubagentConfig
    , getStatus
    , listAgents
    , setMaxConcurrent
    , subagentConfig
    )
import Agent.Subagents.TaskPath (taskPathText)
import Agent.ToolDispatch (ToolCall(..), canonicalToolName)
import Agent.Tools.MultiAgents
    ( MultiAgentContext(..)
    , multiAgentToolNames
    )
import Agent.Tools.PlanMode
    ( PlanModeEnv(..) )
import Agent.Tools.Types
    ( AppTool(..)
    , ToolEnv(..)
    , setToolSessionTmp
    )
import Agent.OsPath (toText)
import Control.Concurrent.Async
    ( withAsync )
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import Control.Concurrent.MVar
    ( newMVar, withMVar )
import Control.Exception.Safe
    ( catchAny )
import Control.Monad (forM_, unless, void, when)
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Set as Set
import Data.Time.Clock
    ( getCurrentTime
    , utctDay
    )
import System.Mem.StableName (StableName, makeStableName)

data AgentStepCache = AgentStepCache
    { cachedTranscript :: !(StableName [ResponseItem])
    , cachedVariant :: !(Maybe SubagentStatus)
    , cachedSteps :: ![AgentStep]
    }

data SessionRunnerContinuation = SessionRunnerContinuation
    { runnerFinishStartup :: StartupRuntime -> IO ()
    , runnerRepl :: SessionEnv -> IO RunResult
    , runnerReplWithDraft :: SessionEnv -> Text -> IO RunResult
    , runnerRunPendingTurn :: PendingTurnPresentation -> SessionEnv -> PendingTurn -> IO RunResult
    , runnerFinishTurn :: SessionEnv -> Bool -> TurnResult -> IO RunResult
    , runnerPreparePromptSkillInputs :: SessionEnv -> Text -> [TurnInput] -> IO (Either Text [TurnInput])
    , runnerRunSessionRecap :: Bool -> SessionEnv -> RecapKind -> IO ()
    , runnerRunSessionTurnSummary :: SessionEnv -> IO ()
    }

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
    steeringRef <- newIORef []
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
            agents <- case multiCtx of
                Nothing -> pure []
                Just ctx -> listAgents ctx.multiRegistry Nothing
            let availableTargets =
                    AgentRoot
                        : [ AgentChild agentId
                          | (_, agentId, _) <- agents
                          ]
            selected <-
                atomicModifyIORef' selectedAgent \current ->
                    let reconciled =
                            TuiBridge.reconcileAgentSelection
                                availableTargets
                                current
                    in (reconciled, reconciled)
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
                    | includeSummaries =
                        responseItemPreviewLines 0 items
                    | otherwise = []
                conversationFor target status items
                    | includeSummaries = initialUiState
                    | target /= selected = initialUiState
                    | target == AgentRoot = initialUiState
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
            pure (selected, rootEntry : children)
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
                        -- If settlement won the race before the pin was set,
                        -- refill the same stable object now that it is pinned.
                        void
                            (hydrateSelectedAgent agentId)
                            `catchAny` \err ->
                                reportSessionError
                                    ("failed to pin selected agent: "
                                        <> formatException err)
            writeIORef selectedAgent target
        releaseSelectedAgent = \case
            AgentRoot -> pure ()
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
                    options
                    dialect
                    home
                    cwd
                    []
                    Nothing
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
            writeIORef agentStepCache Map.empty
            case multiCtx of
                Just ctx -> resetSubagentRegistry ctx.multiRegistry
                Nothing -> pure ()
            clearPendingInputs pendingNotices
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
    -- Mirror plan session dir into the subagent store root for this session.
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
            -- OSC 9;4 is ignored by terminals that do not implement it.
            -- Gate on the same TTY check as the in-pane spinner so pipes
            -- and redirected stderr stay clean.
            , renderNativeProgress =
                stderrTty
                    && terminal.terminalNativeProgress
                    && nativeProgressAnimationEnabled
                        options.optMotionMode
            , renderMotionMode = options.optMotionMode
            , renderWorkspace = toText cwd
            }
        emitLoop event = do
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
            , loopDispatch = defaultLoopDispatch
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
                readIORef steeringRef
            , loopCommitSteering = \count ->
                atomicModifyIORef' steeringRef \pending ->
                    (drop count pending, ())
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
        refreshShellParams ghciEnabled bashEnabled = do
            sessionTmp <- readIORef toolEnv.toolSessionTmp
            today <- utctDay <$> getCurrentTime
            let enabledTools = activeShellTools ghciEnabled bashEnabled
                instructionText =
                    systemPromptForTools
                        dialect
                        (map (.appToolName) enabledTools)
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
    btwRequests <- newChan
    recapRequests <- newChan
    let
        reloadGeneratedContextSafely =
            reloadGeneratedContext `catchAny` \err ->
                reportSessionError
                    ("failed to reload generated context: "
                        <> formatException err)
        compactRunnerWithContext focus = do
            result <- compactRunner focus
            case result of
                Left _ -> pure ()
                Right _ -> reloadGeneratedContextSafely
            pure result
        env = SessionEnv
            { sessionLoop = config
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
    writeIORef generatedContextReloadRef reloadGeneratedContextSafely
    writeIORef startup.startupRestartEffort \level -> do
        setSessionEffort env level
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
                        Right inputs -> do
                            atomicModifyIORef' steeringRef \pending ->
                                (pending <> inputs, ())
                            emitUiEvent runtime (UiInputSteered text))
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
            let queueInitialContext =
                    resumeNeedsGeneratedContext initialTurns
                        || (null initialTurns && isNothing initialPrevious)
            (omitted, _) <- installSkills startupContext
                queueInitialContext
                skills
            reportSkillCatalog (isNothing fullscreen) skills omitted
            learnedSkills <-
                if queueInitialContext
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
