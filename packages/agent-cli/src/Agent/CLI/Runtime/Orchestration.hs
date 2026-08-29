-- | Provider/session startup, restart, and interactive run orchestration.
module Agent.CLI.Runtime.Orchestration
    ( runAgentWithRuntime
    , withRestoredCurrentDirectory
    ) where

import Agent.CLI.AccountPicker ()
import Agent.CLI.AccountSelection
    ( SelectedAccount(..),
      providerSupportsUsageAccountSelection,
      selectProviderAccountCached )
import Agent.CLI.Afk ()
import Agent.CLI.AgentSessions
    ( AgentSessionToolsEnv(..),
      agentSessionTools,
      launchSessionThread,
      signalManagedSessionReady,
      sessionThreadStatus )
import Agent.CLI.AgentViewport ( AgentTarget(AgentRoot) )
import Agent.CLI.Approval ()
import Agent.CLI.Artifact ()
import Agent.CLI.Auth
    ( LoadedAuth(..),
      loadAuthForAccount,
      preferredOpenAiTokenProvider,
      probeLoadedAuthCredential,
      staticCredentialProvider )
import Agent.CLI.Clipboard ()
import Agent.CLI.Command ()
import Agent.CLI.Compaction
    ( installLiveCompactOutcome,
      runProviderCompactWith,
      runResponsesCompactWithContextWindow )
import Agent.CLI.Config
    ( HarnessConfig(..),
      McpServerConfig(..),
      loadHarnessConfig,
      useProgressiveMcp )
import Agent.CLI.Connectivity ( withConnectionRecovery )
import Agent.CLI.Database ( databaseTools )
import Agent.CLI.Database.Store
    ( databaseToolsEnvForStore, deriveDatabaseScopes )
import Agent.CLI.Dialects
    ( CodingTools(..),
      codingToolsForWithTypes,
      filterBashTools,
      filterGhciTools )
import Agent.CLI.Error ( formatApiErrorAt, formatException )
import Agent.CLI.GatewayBridge ( managedGatewayTools )
import Agent.CLI.Input ()
import Agent.CLI.Interrupt
    ( CtrlCDecision(..),
      catchUserInterrupt,
      newInterruptState,
      noteFullscreenCtrlC,
      withCtrlCHandler )
import Agent.CLI.LearnedSkills ( learnedSkillTools )
import Agent.CLI.LearnedSkills.Store
    ( learnedSkillToolsEnvForStore )
import Agent.CLI.Login ( runLoginManager )
import Agent.CLI.Lsp
    ( LspStartup(..), closeLspRuntime, lspRuntimeTool, newLspRuntime )
import Agent.CLI.ManagedTurn ( ManagedTurnRequest(..) )
import Agent.CLI.McpManager ()
import Agent.CLI.McpStatus
    ( formatMcpModelNoticeFor,
      formatMcpProgress,
      summarizeMcpStatuses )
import Agent.CLI.ModelConfig
    ( ConnectionKind(..),
      ModelConnection(..),
      ResponsesConnection(..),
      builtinConnectionId,
      catalogConnection,
      catalogContextWindowForTransport,
      loadModelCatalogAt )
import Agent.CLI.Models
    ( defaultModelFor,
      rawModelOption,
      resolveConfiguredModel,
      resolvePersistedDialect,
      ModelOption(modelTarget),
      ModelTarget(targetProvider, ModelTarget, targetModelId,
                  targetDialect, targetWireModelId, targetConnectionId) )
import Agent.CLI.Options
    ( ApprovalPolicy(PromptMutating),
      defaultEffortFor,
      isOneShot,
      resolveApprovalPolicy,
      CliOptions(optMotionMode, optNoYolo,
                 optYolo, optMaxConcurrentAgents, optCompactThreshold,
                 optShowRawReasoning, optProvider, optModel, optWorktree, optEffort,
                 optPrompt, optPromptFile, optManagedTurnFile,
                 optScreenMode, optGhci, optBash, optResume, optCwd),
      ScreenMode(ScreenMinimal) )
import Agent.CLI.PendingInputs ( withPendingInputs )
import Agent.CLI.Permission (PermissionChoice(..))
import Agent.CLI.Plan ( cliPlanHooks )
import Agent.CLI.Project
    ( ProjectAccount(..),
      ProjectModel(..),
      ProjectSettings(..),
      loadProjectSettings,
      projectAccountFor,
      projectModelProvider,
      resolveProjectRoot,
      saveProjectModel )
import Agent.CLI.Prompt
    ( subscriptionSubagentModelGuidance, systemPromptForTools )
import Agent.CLI.PromptHooks
    ( fullscreenAwarePlanHooks, fullscreenAwareSecretHooks )
import Agent.CLI.Provider.OpenAI
    ( OpenAiPersistentConnection(..), lockedOpenAiSession )
import Agent.CLI.Provider.Switch
    ( chooseStartupProviderTransition,
      continueAutomaticFallback,
      loadSelectedAccountAuth,
      prepareTransitionBackend,
      reportProviderUnavailable )
import Agent.CLI.ProviderAvailability ( probeLoadedAvailability )
import Agent.CLI.ProviderFallback
    ( allowsAutomaticBillingFallback, isProviderUnavailable )
import Agent.CLI.ProviderTransition
    ( applyProviderTransition,
      ProviderTransition(transitionCause, transitionUnavailableProviders,
                         transitionPendingTurn, transitionTarget,
                         transitionAccountSelectionId, transitionAccountId,
                         transitionAutomaticBilling),
      TransitionCause(AutomaticFallback) )
import Agent.CLI.Recap ()
import Agent.CLI.Resume ( resumeNeedsGeneratedContext )
import Agent.CLI.Render ( putTextLn )
import Agent.CLI.ReplMode ()
import Agent.CLI.Request ( requestParams )
import Agent.CLI.Runtime.Persistence ( preparePersistence )
import Agent.CLI.Runtime.Recap
    ( runSessionRecap, runSessionTurnSummary )
import Agent.CLI.Runtime.Repl
    ( finishTurn,
      preparePromptSkillInputs,
      repl,
      replWithDraft,
      runPendingTurn )
import Agent.CLI.Runtime.Types
    ( DevResult(..), PreparedAgent(..), RunResult(..) )
import Agent.CLI.Secret ( promptSecretLine )
import Agent.CLI.Session
    ( addSessionUsage,
      allocateSessionTemp,
      cleanupPendingPersistence,
      ensureSession,
      loadActiveSession,
      loadRecentSessionTurns,
      persistenceTempDir,
      removeSessionTemp,
      resumeHint,
      sessionDirForId,
      sessionLegacySubagentTarget,
      sessionTitleFromPrompt,
      sessionUsageFromTurns,
      sessionsRoot,
      Persistence(..),
      PersistenceState(PersistenceActive, PersistencePending),
      SessionHandle(sessionMeta, sessionDir),
      SessionMeta(metaCwd, metaTransportModel, metaConnection, metaModel,
                  metaDialect, metaEffort, metaLastResponseId, metaTitle, metaId,
                  metaProvider),
      SessionTurn )
import Agent.CLI.Session.Attachments ()
import Agent.CLI.Session.Choices ()
import Agent.CLI.Runtime.HistorySource
    ( emptyFullscreenHistoryPage
    , loadFullscreenHistoryPage
    , sessionUiPageSize
    )
import Agent.CLI.Session.History
    ( detectGitBranch,
      foldSessionItems,
      readLiveTranscript,
      writeLiveTranscript,
      LiveConversation(liveTranscript, livePreviousResponseId) )
import Agent.CLI.Session.Lifecycle ()
import Agent.CLI.Session.Runtime.Types
    ( SessionBackend(..),
      SessionRequest(..),
      StartupCancelled(..),
      StartupFailure(..),
      StartupRuntime(..) )
import Agent.CLI.Session.Selection
    ( currentSessionId, loadPrompt, reservedSessionId )
import Agent.CLI.SessionAdmin
    ( managedPostgresConfigForHome )
import Agent.CLI.SessionEnv ()
import Agent.CLI.SessionLock
    ( SessionLock,
      acquireSessionLock,
      releaseSessionLock,
      sessionLockFilePath,
      sessionLockPath )
import Agent.CLI.SessionState ( SessionState(..), newSessionState )
import Agent.CLI.SessionTitle ()
import Agent.CLI.Skills ()
import Agent.CLI.Startup.Auth
    ( loadStartupAuth,
      markStartupStage,
      recordStartupTiming,
      setStartupNotice,
      startupDie )
import Agent.CLI.StartupContext ( loadAgentsContext )
import Agent.CLI.Style
    ( cliWindowTitle,
      glyphSession,
      glyphWarn,
      roleMuted,
      roleWarn,
      setCliWindowTitle )
import Agent.CLI.Subagents.Runtime
    ( SubagentRuntime(..),
      flushAllSubagentSnapshots,
      freshOpenAiBackend,
      persistAndEvictSubagentSessionWithStatus,
      prepareCollaborationSpawn,
      restoreAgentFromDisk,
      runCodexSubagent,
      runHttpSubagent )
import Agent.CLI.TUI.App
    ( FullscreenInputBuffer,
      FullscreenRuntime,
      clearFullscreenHistorySource,
      emitUiEvent,
      newFullscreenInputBuffer,
      newFullscreenRuntime,
      queuedFullscreenInputDisplays,
      runFullscreen,
      setFullscreenHistorySource,
      setFullscreenSessionActions,
      setFullscreenWindowTitle,
      withFullscreenSuspended )
import Agent.CLI.TUI.History
    ( HistoryDirection(HistoryNewer)
    , HistoryGeneration(..)
    )
import Agent.CLI.TUI.SessionHistory (sessionHistoryPage)
import Agent.CLI.Terminal
    ( copyTerminalClipboard,
      detectTerminalCapabilities,
      reportTerminalCwd,
      resolveColor,
      TerminalCapabilities(terminalNativeProgress) )
import Agent.CLI.Tools ( schemasFromAppTools )
import Agent.CLI.Turn ()
import Agent.CLI.Usage ()
import Agent.CLI.WebFetch
    ( closeWebFetchRuntime, newWebFetchRuntime, webFetchRuntimeTool )
import Agent.CLI.Worktree
    ( createWorktree,
      removeWorktree,
      worktreeRoot )
import Agent.Cancel ( requestCancel )
import Agent.Claude
    ( ClaudeCodeAuth(..),
      ClaudeCodeOptions(..),
      ClaudeCodePermission(..),
      ClaudeToolPermissionDecision(..),
      ClaudeToolPermissionRequest(..),
      claudeCodeOneShotBackend,
      defaultClaudeCodeOptions,
      loadClaudeCodeAuth,
      withClaudeCodeBackendPermissions )
import Agent.Dialect ( dialectForId, DialectId(GrokBuildDialect) )
import Agent.Error ( ApiError(..) )
import Agent.GrokBuild.Dialect.Goal ()
import Agent.GrokBuild.Dialect.Runtime ()
import Agent.GrokBuild.Dialect.Workflow ()
import Agent.Loop
    ( Backend(submitTurn, Backend),
      TurnInput(UserMessage, AgentMessage),
      addTokenUsage,
      emptyTokenUsage,
      LoopError(LoopNoResponseId) )
import Agent.ToolDispatch (functionToolCall)
import Agent.OpenAI.Compaction ()
import Agent.OpenAI.Usage ()
import Agent.OpenAI.WebSocketClient
    ( CodexAuthFailed(..),
      closeCodexConn,
      codexConnTurnState,
      resetCodexTurnState,
      withCodexWsCredential,
      withCodexWsWithProvider )
import Agent.OpenRouter.LoopBackend ( openRouterBackend )
import Agent.OsPath ( toText, unsafeToFilePath )
import Agent.Provider
    ( BillingMode(..),
      Credential(..),
      FailedCredential(..),
      Provider(..),
      TokenProvider,
      getNextToken,
      providerSlug,
      runWithTokenProvider,
      tokenProvider,
      tokenProviderBillingMode )
import Agent.Responses.GenericBackend
    ( genericResponsesBackendWith )
import Agent.Responses.GenericClient ( GenericClientOptions(..) )
import Agent.Responses.Types
    ( ResponseItem, ResponseCreateParams(model) )
import Agent.Skills ( SkillCatalog(SkillCatalog) )
import Agent.Store.Postgres
    ( Store, closeStore, openStore, trustedPool )
import Agent.Store.Types ( renderStoreError )
import Agent.Subagents
    ( RootTurnId,
      SubagentConfig(..),
      closeSubagentRegistry,
      defaultMaxConcurrent,
      defaultSubagentConfig,
      formatCompletionNotice,
      interruptActiveSubagents,
      newSubagentRegistry,
      setSubagentOnComplete,
      setSubagentOnSettled,
      setSubagentRunner )
import Agent.Subagents.TaskPath ( taskPathRoot )
import Agent.TUI.Model
    ( initialUiState,
      progressNotice,
      reduceUi,
      warningNotice,
      UiEvent(UiSystemMessage, UiSetRepository, UiSetNotice),
      UiState(uiQueuedInputs) )
import Agent.TUI.Motion ( nativeProgressAnimationEnabled )
import Agent.Tools.MultiAgents
    ( MultiAgentContext(..), SubagentWorktree(..) )
import Agent.Tools.PlanMode
    ( PlanModeEnv(planSessionDir),
      PlanModeHooks(planAskQuestion, PlanModeHooks, planConfirmEnter,
                    planDecideExit),
      PlanDecision(PlanCancel) )
import Agent.Tools.Secret
    ( SecretPrompt(..), SecretPromptHooks(..) )
import Agent.Tools.Types
    ( AppTool(..), ToolEnv(..), defaultToolEnv, setToolSessionTmp )
import Agent.XAI.LoopBackend ( xaiBackend )
import Control.Applicative ( (<|>) )
import Control.Concurrent.Async
    ( concurrently,
      concurrently_,
      link,
      waitSTM,
      withAsync )
import Control.Concurrent.Chan
    ( Chan, newChan, readChan, writeChan )
import Control.Concurrent.MVar
    ( modifyMVar_,
      newEmptyMVar,
      newMVar,
      putMVar,
      readMVar,
      takeMVar,
      tryPutMVar,
      withMVar )
import Control.Concurrent.STM ( retry )
import Control.Exception ()
import Control.Exception.Safe
    ( SomeException,
      catchAny,
      finally,
      mask_,
      onException,
      throwIO,
      try )
import Control.Monad ( forM_, unless, void, when )
import Data.IORef
    ( IORef,
      modifyIORef',
      atomicModifyIORef',
      newIORef,
      readIORef,
      writeIORef )
import Data.List ()
import Data.Maybe ( isNothing, fromMaybe, isJust )
import Data.Text ( Text )
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as TextEncoding
import Data.Time.Clock ( getCurrentTime, utctDay )
import System.Console.ANSI ()
import System.Console.ANSI.Codes ()
import System.Directory.OsPath
    ( doesDirectoryExist,
      getCurrentDirectory,
      getHomeDirectory,
      makeAbsolute,
      setCurrentDirectory )
import System.Environment ( getProgName, lookupEnv )
import System.Exit ( die )
import System.IO
    ( hIsTerminalDevice,
      stderr,
      stdin )
import System.OsPath
    ( OsPath,
      decodeFS,
      unsafeEncodeUtf,
      (</>),
      takeDirectory,
      takeFileName )
import qualified Data.ByteString as BS ()
import qualified Agent.Responses.GenericClient as GenericResponses
    ( GenericClientOptions(model),
      createResponseWith,
      createResponseWithEvents )
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
import qualified Agent.OpenAI.Auth as OpenAI
    ( discoverAccounts, getAccessTokenForAccount )
import qualified Agent.OpenRouter as OpenRouter
    ( clientOptionsFromEnv, createResponseWith, mapModel )
import qualified Agent.OpenRouter.Usage as OpenRouterUsage ()
import qualified Agent.Provider as Provider ( tokenProvider )
import qualified Agent.CLI.Session.Lifecycle as SessionLifecycle ()
import qualified Agent.CLI.Session.Runner as SessionRunner
    ( runSession, SessionRunnerContinuation(..) )
import qualified Data.Set as Set ()
import qualified Data.Text as Text
    ( intercalate, null, pack, unpack )
import qualified Data.Text.IO as Text ( hPutStr )
import qualified Agent.XAI.Client as XAIClient ( createResponseWith )
import qualified Agent.XAI.Options as XAI ( clientOptionsFromEnv )
import qualified Agent.XAI.Request as XAIRequest ( mapModel )
import qualified Agent.XAI.Usage as XAIUsage ()

import Agent.CLI.Runtime.Orchestration.Types
    ( ActiveHttpAuth(..), AccountSwitchRequest(..), AgentProcessRuntime(..),
      AgentRunMode(..), NativeRunHooks(..) )
import Agent.CLI.Runtime.Orchestration.Restart
    ( RestartCallbacks(..), runFullscreenRestartLoop )
import Agent.CLI.Runtime.Orchestration.Background
    ( runInProcessSessionTurn )
import Agent.CLI.Runtime.Orchestration.Concurrent ( concurrentlyAcquire )
import Agent.CLI.Runtime.Orchestration.Startup
    ( clearNativeProgress
    , finishStartup
    , mcpToolCollision
    , reportStartupWarning
    , setNativeProgress
    , setStartupRepository
    )
runAgentWithRuntime
    :: AgentProcessRuntime
    -> AgentRunMode
    -> CliOptions
    -> IO DevResult
runAgentWithRuntime processRuntime runMode options = do
    fullscreenInputs <- newFullscreenInputBuffer
    sessionState <- newSessionState
    go fullscreenInputs sessionState options Nothing
  where
    go fullscreenInputs sessionState current transition =
        runAgent
            processRuntime
            runMode
            fullscreenInputs
            sessionState
            current
            transition >>= \case
            RunResumeSession sessionId ->
                newSessionState >>= \nextState ->
                    go fullscreenInputs nextState
                        current
                            { optProvider = Nothing
                            , optModel = Nothing
                            , optCwd = Nothing
                            , optWorktree = False
                            , optEffort = Nothing
                            , optPrompt = Nothing
                            , optPromptFile = Nothing
                            , optResume = Just sessionId
                            }
                        Nothing
            RunSwitchWorktree path provider model effort ->
                newSessionState >>= \nextState ->
                    go fullscreenInputs nextState
                        current
                            { optProvider = Just provider
                            , optModel = Just model
                            , optCwd = Just path
                            , optWorktree = False
                            , optEffort = Just effort
                            , optPrompt = Nothing
                            , optPromptFile = Nothing
                            , optResume = Nothing
                            }
                        Nothing
            RunSwitchProvider next ->
                go fullscreenInputs sessionState
                    (applyProviderTransition current next)
                    (Just next)
            RunRestart sessionId ->
                go fullscreenInputs sessionState
                    (restartSessionOptions current sessionId)
                    Nothing
            RunProviderStartFailed apiError ->
                case transition of
                    Just failed
                        | failed.transitionCause == AutomaticFallback ->
                            continueAutomaticFallback
                                runMode.runCwdHint
                                runMode.runStderr
                                Nothing
                                failed
                                apiError >>= \case
                                Just next ->
                                    go fullscreenInputs sessionState
                                        (applyProviderTransition current next)
                                        (Just next)
                                Nothing
                                    | runMode.runInBackground -> do
                                        now <- getCurrentTime
                                        throwIO $
                                            StartupFailure
                                                (Text.unpack
                                                    (formatApiErrorAt
                                                        now
                                                        apiError))
                                    | otherwise -> do
                                        reportProviderUnavailable Nothing apiError
                                        pure DevQuit
                    _
                        | runMode.runInBackground -> do
                            now <- getCurrentTime
                            throwIO $
                                StartupFailure
                                    (Text.unpack
                                        (formatApiErrorAt now apiError))
                        | otherwise -> pure DevQuit
            RunQuit -> pure DevQuit
            RunReload sessionId -> pure (DevReload sessionId)

-- | Restore the process cwd after an action succeeds or throws. Cabal gives
-- GHCi relative source paths, so returning from an agent session in its cwd
-- would make the following @:reload@ lose local modules.
withRestoredCurrentDirectory :: IO a -> IO a
withRestoredCurrentDirectory action = do
    originalCwd <- getCurrentDirectory
    action `finally` setCurrentDirectory originalCwd

runAgent
    :: AgentProcessRuntime
    -> AgentRunMode
    -> FullscreenInputBuffer
    -> SessionState
    -> CliOptions
    -> Maybe ProviderTransition
    -> IO RunResult
runAgent
        processRuntime runMode fullscreenInputs sessionState options transition = do
    prepared <-
        prepareAgentIteration
            processRuntime
            runMode
            fullscreenInputs
            sessionState
            Nothing
            options
            transition
    let runPrepared = case prepared.preparedFullscreen of
            Nothing -> prepared.preparedRun
            Just runtime ->
                let callbacks = RestartCallbacks
                        { restartPrepare =
                            \nextOptions nextTransition ->
                                prepareAgentIteration
                                    processRuntime
                                    runMode
                                    fullscreenInputs
                                    sessionState
                                    (Just runtime)
                                    nextOptions
                                    nextTransition
                        , restartFallback =
                            \failed apiError ->
                                continueAutomaticFallback
                                    runMode.runCwdHint
                                    runMode.runStderr
                                    (Just runtime)
                                    failed
                                    apiError
                        , restartFormatFailure = \apiError -> do
                            now <- getCurrentTime
                            pure (formatApiErrorAt now apiError)
                        , restartOptions = restartSessionOptions
                        , restartApplyTransition = applyProviderTransition
                        , restartManageAccounts = do
                            color <- resolveColor stderr
                            runLoginManager color
                        }
                in
                runFullscreen runtime $
                    runFullscreenRestartLoop
                        callbacks
                        runtime
                        options
                        transition
                        prepared.preparedRun
    outcome <- try @_ @StartupCancelled (try @_ @StartupFailure runPrepared)
    result <- case outcome of
        Left StartupCancelled -> pure RunQuit
        Right startupOutcome ->
            either
                (\failure@(StartupFailure message) ->
                    if runMode.runInBackground
                        then throwIO failure
                        else die message)
                pure
                startupOutcome
    case (prepared.preparedFullscreen, result) of
        -- The retained screen has been restored before this persistent final
        -- diagnostic is printed.
        (Just _, RunProviderStartFailed apiError) -> do
            reportProviderUnavailable Nothing apiError
            pure RunQuit
        _ -> pure result

-- | Prepare one provider-specific backend. The outer Brick worker loops over
-- these prepared actions while reusing @activeFullscreen@, so Vty stays in the
-- alternate screen until the whole provider-restart chain finishes. Session
-- resumes still return to 'runAgentWithRestarts' and start a fresh UI.
prepareAgentIteration
    :: AgentProcessRuntime
    -> AgentRunMode
    -> FullscreenInputBuffer
    -> SessionState
    -> Maybe FullscreenRuntime
    -> CliOptions
    -> Maybe ProviderTransition
    -> IO PreparedAgent
prepareAgentIteration
        processRuntime runMode
        fullscreenInputs sessionState activeFullscreen options transition = do
    resumeLockRef <- newIORef (Nothing :: Maybe SessionLock)
    databaseStoreRef <- newIORef (Nothing :: Maybe Store)
    prepareAgentIterationTracked
        resumeLockRef
        databaseStoreRef
        processRuntime
        runMode
        fullscreenInputs
        sessionState
        activeFullscreen
        options
        transition
        `onException`
            releasePreparationResources resumeLockRef databaseStoreRef

prepareAgentIterationTracked
    :: IORef (Maybe SessionLock)
    -> IORef (Maybe Store)
    -> AgentProcessRuntime
    -> AgentRunMode
    -> FullscreenInputBuffer
    -> SessionState
    -> Maybe FullscreenRuntime
    -> CliOptions
    -> Maybe ProviderTransition
    -> IO PreparedAgent
prepareAgentIterationTracked
        resumeLockRef databaseStoreRef
        processRuntime runMode
        fullscreenInputs sessionState activeFullscreen options transition = do
    forM_ activeFullscreen resetFullscreenSessionActions
    let stdoutHandle = runMode.runStdout
        stderrHandle = runMode.runStderr
        background = runMode.runInBackground
        signalReady result =
            unless background (signalManagedSessionReady result)
        failPreparation message =
            releasePreparationResources resumeLockRef databaseStoreRef >>
                case activeFullscreen of
                    Nothing
                        | background -> throwIO (StartupFailure message)
                        | otherwise -> die message
                    Just _ -> throwIO (StartupFailure message)
    startedAt <- getCurrentTime
    startupTimingsRef <- newIORef []
    syntaxLoadDurationRef <- newIORef Nothing
    startupFinishedRef <- newIORef False
    home <- getHomeDirectory
    let root = sessionsRoot home
    databaseConfig <- managedPostgresConfigForHome home
    databaseStore <- openStore databaseConfig >>= \case
        Left err -> failPreparation (Text.unpack (renderStoreError err))
        Right store -> writeIORef databaseStoreRef (Just store) >> pure store
    let sessionPool = trustedPool databaseStore
    resumed <- case options.optResume of
        Nothing -> pure Nothing
        Just sessionId -> do
            dir <- either
                (\err -> do
                    signalReady (Left err)
                    failPreparation (Text.unpack err))
                pure
                (sessionDirForId root sessionId)
            exists <- doesDirectoryExist dir
            when (not exists) do
                let err = "session not found: " <> sessionId
                signalReady (Left err)
                failPreparation (Text.unpack err)
            acquireSessionLock dir sessionId >>= \case
                Left err -> do
                    signalReady (Left err)
                    failPreparation (Text.unpack err)
                Right lock -> do
                    writeIORef resumeLockRef (Just lock)
                    loadActiveSession sessionPool root sessionId >>= \case
                        Left err -> do
                            signalReady (Left err)
                            failPreparation (Text.unpack err)
                        Right loaded -> do
                            signalReady (Right ())
                            pure (Just loaded)

    source <- case options.optCwd of
        Just requestedCwd -> makeAbsolute requestedCwd
        Nothing -> case resumed of
            Just (meta, _) -> makeAbsolute meta.metaCwd
            Nothing ->
                maybe getCurrentDirectory makeAbsolute runMode.runCwdHint
    let initialCwd = source
    uiRuntimeRef <- newIORef Nothing
    cancelToolRef <- newIORef (pure ())
    interrupt <- newInterruptState \msg -> do
        readIORef uiRuntimeRef >>= \case
            Just runtime ->
                emitUiEvent runtime
                    (UiSetNotice (Just (warningNotice msg)))
            Nothing -> do
                -- Drop an in-place "Thinking…" status so the hint is its own line.
                Text.hPutStr stderrHandle "\r\ESC[K"
                clearNativeProgress stderrHandle
                color <- resolveColor stderrHandle
                putTextLn stderrHandle (roleMuted color msg)
    -- Shared with Esc cancel and plan prompts so arrow-key pickers own stdin.
    escPaused <- newIORef False
    stderrTty <-
        if background then pure False else hIsTerminalDevice stderrHandle
    stdinTty <- if background then pure False else hIsTerminalDevice stdin
    stdoutTty <-
        if background then pure False else hIsTerminalDevice stdoutHandle
    terminal <- detectTerminalCapabilities stdoutHandle
    useColor <- if background then pure False else resolveColor stdoutHandle
    agentSnapshotRef <- newIORef (pure (AgentRoot, []))
    agentSelectRef <- newIORef (\_ -> pure ())
    restartEffortActionRef <- newIORef (\_ -> pure ())
    queuedInputDisplays <- queuedFullscreenInputDisplays fullscreenInputs
    let fullscreenEnabled =
            stdinTty
                && stdoutTty
                && not (isOneShot options)
                && options.optScreenMode /= ScreenMinimal
        initialFullscreenState =
            (reduceUi
                (UiSetNotice
                    (Just (progressNotice
                        (if options.optWorktree
                            then "Creating worktree…"
                            else "Loading project…"))))
                (reduceUi
                    (UiSetRepository
                        ""
                        (toText (takeFileName (takeDirectory initialCwd))
                            <> "/"
                            <> toText (takeFileName initialCwd)))
                    initialUiState))
                        { uiQueuedInputs = queuedInputDisplays }
    firstFrameReady <-
        if isJust activeFullscreen || not fullscreenEnabled
            then newMVar ()
            else newEmptyMVar
    fullscreen <- case activeFullscreen of
        Just runtime -> pure (Just runtime)
        Nothing
            | fullscreenEnabled ->
                Just <$> newFullscreenRuntime
                    fullscreenInputs
                    (readIORef cancelToolRef >>= id)
                    (\level ->
                        readIORef restartEffortActionRef >>= ($ level))
                    (noteFullscreenCtrlC interrupt)
                    (copyTerminalClipboard terminal stdoutHandle)
                    (setCliWindowTitle stdoutTty stdoutHandle)
                    (\active ->
                        when
                            (terminal.terminalNativeProgress
                                && nativeProgressAnimationEnabled
                                    options.optMotionMode) $
                            setNativeProgress stderrHandle active)
                    (readIORef agentSnapshotRef >>= id)
                    (\target -> readIORef agentSelectRef >>= ($ target))
                    (do
                        recordStartupTiming
                            startedAt startupTimingsRef "first frame"
                        void (tryPutMVar firstFrameReady ()))
                    (writeIORef syntaxLoadDurationRef . Just)
                    options.optMotionMode
                    useColor
                    initialFullscreenState
            | otherwise -> pure Nothing
    forM_ fullscreen \runtime ->
        case resumed of
            Nothing ->
                clearFullscreenHistorySource runtime
            Just (meta, _) ->
                loadRecentSessionTurns
                    sessionPool
                    root
                    meta.metaId
                    sessionUiPageSize >>= \case
                        Left err ->
                            failPreparation (Text.unpack err)
                        Right page ->
                            setFullscreenHistorySource
                                runtime
                                meta.metaId
                                (loadFullscreenHistoryPage
                                    sessionPool root meta.metaId)
                                (sessionHistoryPage
                                    (HistoryGeneration 0)
                                    HistoryNewer
                                    page)
    writeIORef uiRuntimeRef fullscreen
    resumeLock <- readIORef resumeLockRef
    let action =
            do
                cwd <- case resumed of
                    Just _ -> pure initialCwd
                    Nothing
                        | options.optWorktree -> do
                            readMVar firstFrameReady
                            case fullscreen of
                                Just _ -> pure ()
                                Nothing ->
                                    putTextLn stderrHandle "Creating worktree…"
                            createWorktree source (worktreeRoot home)
                                >>= either
                                    (\err -> do
                                        mapM_ releaseSessionLock resumeLock
                                        case fullscreen of
                                            Nothing -> die (Text.unpack err)
                                            Just _ ->
                                                throwIO
                                                    (StartupFailure
                                                        (Text.unpack err)))
                                    (\path -> do
                                        color <- resolveColor stderrHandle
                                        case fullscreen of
                                            Nothing ->
                                                putTextLn stderrHandle
                                                    (roleMuted color
                                                        (glyphSession
                                                            <> "worktree: "
                                                            <> toText path))
                                            Just runtime ->
                                                emitUiEvent runtime
                                                    (UiSystemMessage
                                                        (glyphSession
                                                            <> "worktree: "
                                                            <> toText path))
                                        setStartupNotice fullscreen
                                            "Loading project…"
                                        pure path)
                        | otherwise -> pure initialCwd
                unless background (setCurrentDirectory cwd)
                terminalCwd <- decodeFS cwd
                reportTerminalCwd terminal stdoutHandle terminalCwd
                toolEnv <- defaultToolEnv cwd
                writeIORef cancelToolRef (requestCancel toolEnv.toolCancel)
                forM_ runMode.runNativeHooks \hooks ->
                    hooks.nativeRegisterCancel
                        (requestCancel toolEnv.toolCancel)
                forM_ fullscreen \runtime ->
                    setFullscreenSessionActions
                        runtime
                        (requestCancel toolEnv.toolCancel)
                        (const (pure ()))
                        (pure ())
                        (\level ->
                            readIORef restartEffortActionRef >>= ($ level))
                        (noteFullscreenCtrlC interrupt)
                        (readIORef agentSnapshotRef >>= id)
                        (\target -> readIORef agentSelectRef >>= ($ target))
                let startup = StartupRuntime
                        { startupToolEnv = toolEnv
                        , startupDatabaseStore = databaseStore
                        , startupInterrupt = interrupt
                        , startupEscPaused = escPaused
                        , startupUiRuntimeRef = uiRuntimeRef
                        , startupFullscreen = fullscreen
                        , startupTerminal = terminal
                        , startupStdout = stdoutHandle
                        , startupStderr = stderrHandle
                        , startupBackground = background
                        , startupUseColor = useColor
                        , startupStderrTty = stderrTty
                        , startupStdinTty = stdinTty
                        , startupStdoutTty = stdoutTty
                        , startupFullscreenReused = isJust activeFullscreen
                        , startupAgentSnapshot = agentSnapshotRef
                        , startupAgentSelect = agentSelectRef
                        , startupRestartEffort = restartEffortActionRef
                        , startupStartedAt = startedAt
                        , startupTimings = startupTimingsRef
                        , startupSyntaxLoadDuration = syntaxLoadDurationRef
                        , startupFinished = startupFinishedRef
                        , startupSessionState = sessionState
                        , startupNativeHooks = runMode.runNativeHooks
                        }
                runAgentInitialized
                    processRuntime
                    options
                    transition
                    home
                    root
                    resumed
                    resumeLock
                    cwd
                    startup
        cleanup = do
            forM_ runMode.runNativeHooks \hooks ->
                hooks.nativeRegisterCancel (pure ())
            writeIORef uiRuntimeRef Nothing
            writeIORef cancelToolRef (pure ())
            forM_ fullscreen resetFullscreenSessionActions
            closeStore databaseStore
    pure PreparedAgent
        { preparedFullscreen = fullscreen
        , preparedRun = action `finally` cleanup
        }

releasePreparationResources
    :: IORef (Maybe SessionLock)
    -> IORef (Maybe Store)
    -> IO ()
releasePreparationResources resumeLockRef databaseStoreRef = do
    atomicModifyIORef' resumeLockRef (\current -> (Nothing, current))
        >>= mapM_ releaseSessionLock
    atomicModifyIORef' databaseStoreRef (\current -> (Nothing, current))
        >>= mapM_ closeStore

resetFullscreenSessionActions :: FullscreenRuntime -> IO ()
resetFullscreenSessionActions runtime =
    setFullscreenSessionActions
        runtime
        (pure ())
        (const (pure ()))
        (pure ())
        (const (pure ()))
        -- No session-local interrupt state is alive between providers. A
        -- transition must remain escapable even if auth probing blocks.
        (pure ForceExit)
        (pure (AgentRoot, []))
        (const (pure ()))

nativeClaudePermissionHandler
    :: Maybe NativeRunHooks
    -> Maybe
        (ClaudeToolPermissionRequest
            -> IO ClaudeToolPermissionDecision)
nativeClaudePermissionHandler = fmap \hooks request -> do
    let arguments =
            TextEncoding.decodeUtf8
                (LBS.toStrict
                    (Aeson.encode request.claudePermissionInput))
        call = functionToolCall
            request.claudePermissionRequestId
            request.claudePermissionToolName
            arguments
    hooks.nativeRequestApproval call >>= \case
        Just PermissionAllowOnce ->
            pure ClaudeToolPermissionAllow
        Just PermissionAllowTool ->
            pure ClaudeToolPermissionAllow
        Just PermissionAllowAll ->
            pure ClaudeToolPermissionAllow
        Just PermissionDeny ->
            pure (ClaudeToolPermissionDeny "Denied by user.")
        Nothing ->
            pure (ClaudeToolPermissionDeny "Permission request dismissed.")

runAgentInitialized
    :: AgentProcessRuntime
    -> CliOptions
    -> Maybe ProviderTransition
    -> OsPath
    -> OsPath
    -> Maybe (SessionMeta, [SessionTurn])
    -> Maybe SessionLock
    -> OsPath
    -> StartupRuntime
    -> IO RunResult
runAgentInitialized
        processRuntime options transition home root resumed resumeLock cwd startup =
    runAgentInitializedWithLock
        processRuntime options transition home root resumed resumeLock cwd startup
        `onException` mapM_ releaseSessionLock resumeLock

runAgentInitializedWithLock
    :: AgentProcessRuntime
    -> CliOptions
    -> Maybe ProviderTransition
    -> OsPath
    -> OsPath
    -> Maybe (SessionMeta, [SessionTurn])
    -> Maybe SessionLock
    -> OsPath
    -> StartupRuntime
    -> IO RunResult
runAgentInitializedWithLock
        processRuntime
        options transition home root resumed resumeLock cwd startup = do
    let baseToolEnv = startup.startupToolEnv
        mcpSupervisor = processRuntime.processMcpSupervisor
        interrupt = startup.startupInterrupt
        escPaused = startup.startupEscPaused
        uiRuntimeRef = startup.startupUiRuntimeRef
        fullscreen = startup.startupFullscreen
        isTty = startup.startupStdinTty
        stdoutTty = startup.startupStdoutTty
        stdoutHandle = startup.startupStdout
        stderrHandle = startup.startupStderr
        setWindowTitle title =
            case fullscreen of
                Just runtime -> setFullscreenWindowTitle runtime title
                Nothing -> setCliWindowTitle stdoutTty stdoutHandle title
    projectRoot <- resolveProjectRoot cwd
    stateDirectory <- decodeFS (home </> unsafeEncodeUtf ".haskell-agent")
    projectRootPath <- decodeFS projectRoot
    databaseScopes <-
        deriveDatabaseScopes stateDirectory projectRootPath >>= \case
            Left err -> startupDie startup (Text.unpack err)
            Right scopes -> pure scopes
    (projectSettings, (catalogResult, branch)) <-
        concurrently
            (loadProjectSettings projectRoot)
            (concurrently
                (loadModelCatalogAt home cwd)
                (detectGitBranch cwd))
    catalog <- either
        (startupDie startup . Text.unpack)
        pure
        catalogResult
    setStartupRepository fullscreen home branch cwd
    markStartupStage startup "Loading credentials…"
    let transitionTarget = (.transitionTarget) <$> transition
        pendingTurn = transition >>= (.transitionPendingTurn)
        unavailableProviders =
            maybe [] (.transitionUnavailableProviders) transition
        configuredOptionTarget =
            (.modelTarget)
                <$> (options.optModel >>= resolveConfiguredModel catalog)
        savedTarget provider connection model transport dialect =
            case resolveConfiguredModel catalog model of
                Just option
                    | option.modelTarget.targetConnectionId == connection ->
                        Right option.modelTarget
                _
                    | connection == builtinConnectionId provider ->
                        Right ModelTarget
                            { targetProvider = provider
                            , targetConnectionId = connection
                            , targetModelId = model
                            , targetWireModelId = fromMaybe model transport
                            , targetDialect = dialect
                            }
                    | otherwise ->
                        Left $
                            "saved model "
                                <> connection <> "/" <> model
                                <> " is not present in ~/.haskell-agent/models.json"
        resumedTargetResult
            | isJust transitionTarget || isJust options.optModel =
                Right Nothing
            | otherwise = case fst <$> resumed of
            Nothing -> Right Nothing
            Just meta ->
                Just <$> savedTarget
                    meta.metaProvider
                    meta.metaConnection
                    meta.metaModel
                    meta.metaTransportModel
                    meta.metaDialect
        projectTargetResult
            | isJust transitionTarget
                || isJust options.optModel
                || isJust resumed =
                    Right Nothing
            | otherwise = case projectSettings.settingsLastModel of
            Nothing -> Right Nothing
            Just remembered ->
                let target = remembered.projectModelTarget
                in
                Just <$> savedTarget
                    target.targetProvider
                    target.targetConnectionId
                    target.targetModelId
                    (Just target.targetWireModelId)
                    target.targetDialect
    resumedTarget <-
        either (startupDie startup . Text.unpack) pure resumedTargetResult
    projectTarget <-
        either (startupDie startup . Text.unpack) pure projectTargetResult
    let targetHint =
            transitionTarget
                <|> configuredOptionTarget
                <|> resumedTarget
                <|> if isNothing options.optModel
                    then projectTarget
                    else Nothing
        requestedProvider =
            (.targetProvider) <$> targetHint
                <|> options.optProvider
                <|> if isNothing options.optModel
                    then projectModelProvider projectSettings
                    else Nothing
        targetConnection =
            targetHint >>= catalogConnection catalog . (.targetConnectionId)
        customResponses = targetConnection >>= \connection ->
            case connection.connectionKind of
                CustomResponsesConnection responses -> Just
                    (connection.connectionId, responses)
                BuiltinConnection _ -> Nothing
        checkStartupUsageInBackground =
            isJust fullscreen
                && isNothing transition
                && isNothing resumed
                && isNothing options.optProvider
                && isNothing options.optModel
    ((initialLoaded, learnAboutUserRequested), customBearerToken) <-
        case customResponses of
            Nothing -> do
                startupAuth <-
                    loadStartupAuth startup transition requestedProvider
                pure (startupAuth, Nothing)
            Just (connectionId, responses) -> do
                token <- case responses.responsesApiKeyEnv of
                    Nothing
                        | responses.responsesApiKeyOptional -> pure ""
                        | otherwise ->
                            startupDie startup $
                                "custom connection "
                                    <> Text.unpack connectionId
                                    <> " requires api_key_env or api_key_optional=true"
                    Just envName ->
                        lookupEnv (Text.unpack envName) >>= \case
                            Just value
                                | not (null value) -> pure (Text.pack value)
                            _
                                | responses.responsesApiKeyOptional -> pure ""
                                | otherwise ->
                                    startupDie startup $
                                        "custom connection "
                                            <> Text.unpack connectionId
                                            <> " requires environment variable "
                                            <> Text.unpack envName
                let credential = Credential
                        { accessToken = token
                        , accountId = connectionId
                        , leaseId = Nothing
                        , provider = OpenRouterProvider
                        }
                pure
                    ( ( LoadedAuth
                            { loadedProvider = OpenRouterProvider
                            , loadedTokenProvider =
                                staticCredentialProvider ApiBilled credential
                            , loadedAccountLabel = const (pure connectionId)
                            , loadedSelectionId = Nothing
                            , loadedOpenAiPool = Nothing
                            }
                      , False
                      )
                    , if Text.null token then Nothing else Just token
                    )
    (loaded, startupAccountIds) <- case customResponses of
        Just _ -> pure (initialLoaded, Nothing)
        Nothing
            | Just active <- transition
            , Just selectionId <- active.transitionAccountSelectionId ->
                pure
                    ( initialLoaded
                    , Just
                        ( selectionId
                        , fromMaybe selectionId active.transitionAccountId
                        )
                    )
            | not
                (providerSupportsUsageAccountSelection
                    initialLoaded.loadedProvider) ->
                        pure (initialLoaded, Nothing)
            | checkStartupUsageInBackground -> do
                -- Make the remembered model/account usable immediately. The
                -- scoped availability worker below checks the account pool
                -- after the prompt is ready and triggers startup fallback if
                -- every credential is exhausted.
                let provider = initialLoaded.loadedProvider
                case projectAccountFor provider projectSettings of
                    Nothing -> pure (initialLoaded, Nothing)
                    Just remembered ->
                        loadSelectedAccountAuth
                            provider
                            remembered.projectAccountSelectionId
                            remembered.projectAccountId >>= \case
                                Left _ -> pure (initialLoaded, Nothing)
                                Right selectedLoaded ->
                                    pure
                                        ( selectedLoaded
                                        , Just
                                            ( remembered.projectAccountSelectionId
                                            , remembered.projectAccountId
                                            )
                                        )
            | otherwise -> do
                let provider = initialLoaded.loadedProvider
                    rememberedIds = fmap
                        (\account ->
                            ( account.projectAccountSelectionId
                            , account.projectAccountId
                            ))
                        (projectAccountFor provider projectSettings)
                selectProviderAccountCached
                    (trustedPool startup.startupDatabaseStore)
                    provider
                    Nothing
                    rememberedIds >>= \case
                        Left err ->
                            startupDie startup (Text.unpack err)
                        Right selected ->
                            loadSelectedAccountAuth
                                provider
                                selected.selectedSelectionId
                                selected.selectedAccountId
                                >>= either
                                    (startupDie startup . Text.unpack)
                                    (\selectedLoaded ->
                                        pure
                                            ( selectedLoaded
                                            , Just
                                                ( selected.selectedSelectionId
                                                , selected.selectedAccountId
                                                )
                                            ))
    case (transitionTarget, resumed) of
        (Just target, _)
            | loaded.loadedProvider /= target.targetProvider ->
                startupDie startup $ "provider transition requested "
                    <> Text.unpack (providerSlug target.targetProvider)
                    <> " but auth resolved "
                    <> Text.unpack (providerSlug loaded.loadedProvider)
        (Nothing, Just (meta, _))
            | loaded.loadedProvider /= meta.metaProvider ->
                startupDie startup $ "session provider is "
                    <> Text.unpack (providerSlug meta.metaProvider)
                    <> " but auth resolved "
                    <> Text.unpack (providerSlug loaded.loadedProvider)
        _ -> pure ()
    case transition >>= (.transitionAutomaticBilling) of
        Just sourceBilling
            | not
                (allowsAutomaticBillingFallback
                    sourceBilling
                    (tokenProviderBillingMode loaded.loadedTokenProvider)) ->
                startupDie startup
                    "automatic provider fallback would cross from subscription \
                    \billing to API-credit billing"
        _ -> pure ()
    activeAccountRef <- newIORef ""
    activeAccountIdRef <-
        newIORef (maybe "" snd startupAccountIds)
    activeSelectionRef <-
        newIORef $
            maybe
                (fromMaybe "" loaded.loadedSelectionId)
                fst
                startupAccountIds
    preferredOpenAiAccountRef <-
        newIORef $
            case (loaded.loadedProvider, startupAccountIds) of
                (OpenAIProvider, Just (_, accountId))
                    | not (Text.null accountId) -> Just accountId
                _ -> Nothing
    let selectableTokenProvider =
            case loaded.loadedOpenAiPool of
                Just pool ->
                    preferredOpenAiTokenProvider
                        preferredOpenAiAccountRef
                        pool
                        loaded.loadedTokenProvider
                Nothing ->
                    loaded.loadedTokenProvider
    initialHttp <- case customResponses of
        Just (connectionId, _) -> do
            writeIORef activeAccountRef connectionId
            pure
                ( selectableTokenProvider
                , const (pure connectionId)
                , connectionId
                )
        Nothing -> case loaded.loadedProvider of
            OpenAIProvider ->
                pure
                    ( selectableTokenProvider
                    , loaded.loadedAccountLabel
                    , ""
                    )
            _ ->
                probeLoadedAuthCredential loaded >>= \case
                    Right (credential, usable) -> do
                        label <- usable.loadedAccountLabel credential
                        writeIORef activeAccountRef label
                        writeIORef activeAccountIdRef credential.accountId
                        let selectionId =
                                fromMaybe
                                    credential.accountId
                                    usable.loadedSelectionId
                        writeIORef activeSelectionRef selectionId
                        pure
                            ( usable.loadedTokenProvider
                            , usable.loadedAccountLabel
                            , credential.accountId
                            )
                    Left _ -> do
                        let fallback = case loaded.loadedProvider of
                                XAIProvider -> "Grok"
                                OpenRouterProvider -> "OpenRouter"
                                ClaudeCodeProvider -> "Claude Code"
                            selectionId = fromMaybe "" loaded.loadedSelectionId
                        writeIORef activeAccountRef fallback
                        writeIORef activeSelectionRef selectionId
                        pure
                            ( selectableTokenProvider
                            , loaded.loadedAccountLabel
                            , ""
                            )
    let
        ( initialHttpProvider
            , initialHttpResolver
            , initialHttpAccountId
            ) = initialHttp
    activeHttpAuth <- newMVar ActiveHttpAuth
        { activeHttpGeneration = 0
        , activeHttpProvider = initialHttpProvider
        , activeHttpResolveLabel = initialHttpResolver
        , activeHttpAccountId = initialHttpAccountId
        }
    let switchableTokenProvider =
            Provider.tokenProvider
                (tokenProviderBillingMode selectableTokenProvider)
                \failed -> do
                    snapshot <- readMVar activeHttpAuth
                    let routedFailure = case failed of
                            Just reported
                                | reported.credential.accountId
                                    == snapshot.activeHttpAccountId ->
                                    Just reported
                            _ -> Nothing
                    getNextToken
                        snapshot.activeHttpProvider
                        routedFailure
                        >>= \case
                            Left err -> pure (Left err)
                            Right credential -> do
                                label <-
                                    snapshot.activeHttpResolveLabel credential
                                modifyMVar_ activeHttpAuth \current ->
                                    if current.activeHttpGeneration
                                        == snapshot.activeHttpGeneration
                                        then do
                                            writeIORef
                                                activeAccountIdRef
                                                credential.accountId
                                            writeIORef activeAccountRef label
                                            pure current
                                                { activeHttpAccountId =
                                                    credential.accountId
                                                }
                                        else pure current
                                pure (Right credential)
        resolveActiveAccountLabel credential =
            case loaded.loadedProvider of
                OpenAIProvider ->
                    loaded.loadedAccountLabel credential
                _ -> do
                    active <- readMVar activeHttpAuth
                    active.activeHttpResolveLabel credential
        tokenProvider =
            case loaded.loadedProvider of
                OpenAIProvider ->
                    trackCredentialAccount
                        activeAccountRef
                        activeAccountIdRef
                        activeSelectionRef
                        resolveActiveAccountLabel
                        selectableTokenProvider
                _ -> switchableTokenProvider
        selectHttpAccount selectedSelectionId =
            loadAuthForAccount loaded.loadedProvider selectedSelectionId
                >>= \case
                    Left err ->
                        pure (Left (CredentialError err))
                    Right selected
                        | tokenProviderBillingMode
                            selected.loadedTokenProvider
                            /= tokenProviderBillingMode
                                selectableTokenProvider ->
                            pure $ Left $ CredentialError
                                "selected account uses a different billing mode"
                        | otherwise ->
                            probeLoadedAuthCredential selected >>= \case
                                Left err -> pure (Left err)
                                Right (credential, usable) -> do
                                    label <-
                                        usable.loadedAccountLabel credential
                                    let selectionId =
                                            fromMaybe
                                                selectedSelectionId
                                                usable.loadedSelectionId
                                    modifyMVar_ activeHttpAuth \current -> do
                                        writeIORef
                                            activeAccountIdRef
                                            credential.accountId
                                        writeIORef
                                            activeSelectionRef
                                            selectionId
                                        writeIORef activeAccountRef label
                                        pure ActiveHttpAuth
                                            { activeHttpGeneration =
                                                current.activeHttpGeneration + 1
                                            , activeHttpProvider =
                                                usable.loadedTokenProvider
                                            , activeHttpResolveLabel =
                                                usable.loadedAccountLabel
                                            , activeHttpAccountId =
                                                credential.accountId
                                            }
                                    pure (Right label)

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
        effort = fromMaybe
            (maybe (defaultEffortFor provider) (.metaEffort) (fst <$> resumed))
            options.optEffort
        policy = case startup.startupNativeHooks of
            Just _ -> PromptMutating
            Nothing ->
                resolveApprovalPolicy options isTty
                    projectSettings.settingsAutoApprove
        claudeBypassEnabled =
            not options.optNoYolo
                && (options.optYolo || projectSettings.settingsAutoApprove)
    -- Provider transitions commit their selection separately: manual switches
    -- immediately, automatic fallbacks only after the replacement succeeds.
    when (isNothing transition) $
        saveProjectModel projectRoot
            inferredTarget { targetDialect = dialectId }
    activeSessionLock <- newIORef resumeLock
    persistSlotRef <- newIORef PersistenceDisabled
    -- Per-subagent transcripts / previous ids, shared across send_input / task.
    subagentSessions <- newIORef Map.empty
    subagentStoreRoot <- newIORef Nothing
    subagentForkSource <- newIORef (Nothing :: Maybe (IO [ResponseItem]))
    pendingNotices <- newIORef ([] :: [TurnInput])
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
    let sendToRoot message = do
            atomicModifyIORef' pendingNotices \xs ->
                (xs <> [AgentMessage message], ())
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
            }
    promptRequest <- loadPrompt options
    let promptText = fmap (\request -> request.managedTurnText) promptRequest
    persist <-
        preparePersistence
            (trustedPool startup.startupDatabaseStore)
            startup options root
                inferredTarget { targetDialect = dialectId }
                (isNothing transition) cwd effort promptText resumed
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
                atomicModifyIORef' pendingNotices \notices ->
                    ( notices
                        <> [ UserMessage
                                (formatMcpModelNoticeFor dialectId statuses)
                           ]
                    , ()
                    )
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
                atomicModifyIORef' pendingNotices \xs ->
                    (xs <> [UserMessage (formatCompletionNotice agentId status)], ())
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
            , toolsEffort = effort
            , toolsCurrentSessionId =
                readIORef persistSlotRef >>= currentSessionId
            , toolsLaunchTurn = \handle message -> do
                ghciEnabled <- readIORef ghciEnabledRef
                bashEnabled <- readIORef bashEnabledRef
                let action =
                        runInProcessSessionTurn
                            (runAgentWithRuntime processRuntime)
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
        nativeAppTools =
            maybe [] (.nativeTools) startup.startupNativeHooks
        allTools =
            coding.codingAppTools
                ++ extraTools
                ++ mcpTools
                ++ sessionTools
                ++ gatewayTools
                ++ databaseAppTools
                ++ learnedSkillAppTools
                ++ nativeAppTools
        tools =
            filterGhciTools options.optGhci
                (filterBashTools options.optBash coding.codingAppTools)
                ++ extraTools
                ++ mcpTools
                ++ sessionTools
                ++ gatewayTools
                ++ databaseAppTools
                ++ learnedSkillAppTools
                ++ nativeAppTools
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
                                                    cleanupScratch))))
    flip finally closeAll do
        case
                mcpToolCollision
                    ( coding.codingAppTools
                        ++ extraTools
                        ++ sessionTools
                        ++ gatewayTools
                        ++ databaseAppTools
                        ++ learnedSkillAppTools
                        ++ nativeAppTools
                    )
                    mcpFleet.mcpFleetRegistrations
            of
                Just err ->
                    startupDie startup
                        ("Failed to initialize MCP tools: " <> Text.unpack err)
                Nothing -> pure ()
        today <- utctDay <$> getCurrentTime
        let instructions =
                systemPromptForTools
                    dialect
                    (map (.appToolName) tools)
                    cwd
                    (Just sessionTmp)
                    today
                    (isOneShot options)
            params = requestParams provider model instructions
                (schemasFromAppTools dialect tools) effort
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
        generatedContextReloadRef <- newIORef (pure ())
        let currentModelContextWindow mapTransportModel = do
                currentParams <- readIORef paramsRef
                pure $ do
                    currentModel <- currentParams.model
                    catalogContextWindowForTransport
                        catalog
                        inferredTarget.targetConnectionId
                        currentModel
                        (mapTransportModel currentModel)
            subagentRuntime = SubagentRuntime
                { subagentOptions = options
                , subagentGhciEnabled = ghciEnabledRef
                , subagentBashEnabled = bashEnabledRef
                , subagentPolicy = policy
                , subagentPlanHooks = planHooks
                , subagentSkillRoots = toolEnv.toolSkillRoots
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
                }
        let conversationRef = startup.startupSessionState.sessionConversation
        atomicModifyIORef' conversationRef \state ->
            ( state
                { livePreviousResponseId = initialPrevious
                , liveTranscript = initialItems
                }
            , ()
            )
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
        startupContext <-
            loadAgentsContext
                stderrHandle
                fullscreen
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
        -- Fullscreen sessions load skills after Brick has taken over the
        -- terminal, so filesystem discovery cannot delay the first frame.
        -- Minimal and one-shot sessions still initialize them synchronously
        -- before their first prompt/turn below.
        usageRef <- newIORef $ case resumed of
            Just (meta, turns) -> sessionUsageFromTurns meta turns
            Nothing -> emptyTokenUsage
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
                                , connectionId =
                                    inferredTarget.targetConnectionId
                                , options
                                , provider
                                , dialect
                                , policy
                                , allTools
                                , suspendGhci = coding.codingSuspendGhci
                                , grokRuntime = coding.codingGrokRuntime
                                , mcpRegistrations =
                                    mcpFleet.mcpFleetRegistrations
                                , mcpWarnings = mcpFleet.mcpFleetWarnings
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
                                , initialTurns
                                , persist
                                , startupWindowTitle
                                , projectRoot
                                , home
                                , cwd
                                , tokenProvider = sessionTokenProvider
                                , openAiPool = sessionOpenAiPool
                                , startupContext
                                , generatedContextReloadRef
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
                    case provider of
                    OpenAIProvider ->
                        try @_ @CodexAuthFailed
                            (withCodexWsWithProvider tokenProvider \conn credential -> do
                                wsLock <- newMVar ()
                                initialWsHealthy <- newIORef True
                                activeConnectionRef <- newIORef $
                                    OpenAiPersistentConnection
                                        credential
                                        initialWsHealthy
                                        conn
                                httpFallbackActive <- newIORef False
                                switchRequests <-
                                    newChan :: IO (Chan AccountSwitchRequest)
                                let selectAccount = case loaded.loadedOpenAiPool of
                                        Nothing -> Nothing
                                        Just pool ->
                                            Just \selectedAccountId -> do
                                                    _ <- OpenAI.discoverAccounts pool
                                                    OpenAI.getAccessTokenForAccount
                                                        pool
                                                        selectedAccountId
                                                        >>= \case
                                                            Left err ->
                                                                pure (Left err)
                                                            Right
                                                                ( accessToken
                                                                , accountId
                                                                ) -> do
                                                                reply <- newEmptyMVar
                                                                writeChan
                                                                    switchRequests
                                                                    (AccountSwitchRequest
                                                                        Credential
                                                                            { accessToken
                                                                            , accountId
                                                                            , leaseId = Nothing
                                                                            , provider =
                                                                                OpenAIProvider
                                                                            }
                                                                        reply)
                                                                takeMVar reply
                                    switchLoop = case loaded.loadedOpenAiPool of
                                        Nothing -> pure ()
                                        Just pool ->
                                            readChan switchRequests
                                                >>= switchTo pool
                                    switchTo pool request =
                                        runSwitch pool request >>= \case
                                            Nothing -> switchLoop
                                            Just next -> switchTo pool next
                                    runSwitch
                                        pool
                                        (AccountSwitchRequest
                                            selectedCredential
                                            reply) = do
                                                takeMVar wsLock
                                                lockHeld <- newIORef True
                                                let releaseLock = do
                                                        held <-
                                                            atomicModifyIORef'
                                                                lockHeld
                                                                (\held ->
                                                                    (False, held))
                                                        when held $
                                                            putMVar wsLock ()
                                                    failSwitch err = do
                                                        releaseLock
                                                        _ <- tryPutMVar
                                                            reply
                                                            (Left err)
                                                        pure Nothing
                                                    installConnection
                                                        newCredential
                                                        newConn = do
                                                            newHealthy <-
                                                                newIORef True
                                                            label <-
                                                                resolveActiveAccountLabel
                                                                    newCredential
                                                            writeIORef
                                                                activeConnectionRef $
                                                                OpenAiPersistentConnection
                                                                    newCredential
                                                                    newHealthy
                                                                    newConn
                                                            writeIORef
                                                                activeAccountIdRef
                                                                newCredential.accountId
                                                            writeIORef
                                                                activeSelectionRef
                                                                newCredential.accountId
                                                            writeIORef
                                                                activeAccountRef
                                                                label
                                                            pure (newHealthy, label)
                                                    awaitNext newHealthy =
                                                        readChan switchRequests
                                                            `finally`
                                                                writeIORef
                                                                    newHealthy
                                                                    False
                                                oldConnection <-
                                                    readIORef activeConnectionRef
                                                previousAccountId <-
                                                    readIORef activeAccountIdRef
                                                let OpenAiPersistentConnection
                                                        _
                                                        oldHealthy
                                                        oldConn =
                                                            oldConnection
                                                writeIORef oldHealthy False
                                                closeCodexConn oldConn
                                                writeIORef
                                                    preferredOpenAiAccountRef
                                                    (Just
                                                        selectedCredential.accountId)
                                                let connectSelected =
                                                        withCodexWsCredential
                                                            selectedCredential
                                                            \newConn
                                                                newCredential -> do
                                                                    (newHealthy, label) <-
                                                                        installConnection
                                                                            newCredential
                                                                            newConn
                                                                    releaseLock
                                                                    _ <- tryPutMVar
                                                                        reply
                                                                        (Right label)
                                                                    awaitNext
                                                                        newHealthy
                                                    restorePrevious
                                                        selectedError
                                                        | Text.null
                                                            previousAccountId =
                                                            failSwitch
                                                                selectedError
                                                        | otherwise = do
                                                            writeIORef
                                                                preferredOpenAiAccountRef
                                                                (Just
                                                                    previousAccountId)
                                                            OpenAI.getAccessTokenForAccount
                                                                pool
                                                                previousAccountId
                                                                >>= \case
                                                                    Left _ ->
                                                                        failSwitch
                                                                            selectedError
                                                                    Right
                                                                        ( previousToken
                                                                        , restoredId
                                                                        ) -> do
                                                                            let restoredCredential =
                                                                                    Credential
                                                                                        { accessToken =
                                                                                            previousToken
                                                                                        , accountId =
                                                                                            restoredId
                                                                                        , leaseId =
                                                                                            Nothing
                                                                                        , provider =
                                                                                            OpenAIProvider
                                                                                        }
                                                                            (withCodexWsCredential
                                                                                restoredCredential
                                                                                \newConn
                                                                                    newCredential -> do
                                                                                        (newHealthy, _) <-
                                                                                            installConnection
                                                                                                newCredential
                                                                                                newConn
                                                                                        releaseLock
                                                                                        _ <- tryPutMVar
                                                                                            reply
                                                                                            (Left
                                                                                                selectedError)
                                                                                        awaitNext
                                                                                            newHealthy)
                                                                                >>= \case
                                                                                    Left _ ->
                                                                                        failSwitch
                                                                                            selectedError
                                                                                    Right next ->
                                                                                        pure
                                                                                            (Just
                                                                                                next)
                                                (connectSelected >>= \case
                                                    Left selectedError ->
                                                        restorePrevious
                                                            selectedError
                                                    Right next ->
                                                        pure (Just next))
                                                    `catchAny` \_ ->
                                                        failSwitch $
                                                            ConnectionError
                                                                "account switch failed"
                                case multiCtx of
                                    Just ctx ->
                                        setSubagentRunner ctx.multiRegistry $
                                            runCodexSubagent
                                                subagentRuntime
                                                selectableTokenProvider
                                                ctx.multiSendToRoot
                                    Nothing -> pure ()
                                let (compactSender, lockedBackend) =
                                        lockedOpenAiSession
                                            options.optCompactThreshold
                                            options.optShowRawReasoning
                                            wsLock
                                            httpFallbackActive
                                            tokenProvider
                                            activeConnectionRef
                                            (readIORef paramsRef)
                                            contextTokensRef
                                            recordCompactionUsage
                                            (readIORef generatedContextReloadRef >>= id)
                                    noticingBackend =
                                        withPendingInputs pendingNotices
                                            lockedBackend
                                    btwBackend privateParams =
                                        freshOpenAiBackend
                                            options.optShowRawReasoning
                                            tokenProvider
                                            (pure privateParams)
                                    compactRunner focus =
                                        withMVar wsLock \_ -> do
                                            OpenAiPersistentConnection
                                                _credential
                                                _connectionHealthy
                                                activeConn <-
                                                    readIORef activeConnectionRef
                                            historyRef <-
                                                newIORef =<< readLiveTranscript
                                                    conversationRef
                                            let turnState =
                                                    codexConnTurnState activeConn
                                                runCompact =
                                                    installLiveCompactOutcome
                                                        conversationRef
                                                        (Just contextTokensRef)
                                                        (runProviderCompactWith
                                                            (Just compactSender)
                                                            recordCompactionUsage
                                                            provider
                                                            (Just tokenProvider)
                                                            paramsRef
                                                            historyRef)
                                                        focus
                                            resetCodexTurnState turnState
                                            runCompact `finally`
                                                resetCodexTurnState turnState
                                activeBackend <-
                                    prepareTransitionBackend
                                        projectRoot transition persist noticingBackend
                                withAsync switchLoop \switchWorker -> do
                                    link switchWorker
                                    runSession
                                        (sessionRequest
                                            startupUnavailable
                                            (Just tokenProvider)
                                            loaded.loadedOpenAiPool
                                            selectAccount
                                            compactRunner)
                                        SessionBackend
                                            { backend = activeBackend
                                            , btwBackend
                                            , resetBackendState = do
                                                OpenAiPersistentConnection
                                                    _credential
                                                    _connectionHealthy
                                                    activeConn <-
                                                        readIORef activeConnectionRef
                                                resetCodexTurnState
                                                    (codexConnTurnState activeConn)
                                            })
                            >>= \case
                                Left (CodexAuthFailed err) ->
                                    case transition of
                                        Just active
                                            | active.transitionCause == AutomaticFallback ->
                                                pure (RunProviderStartFailed err)
                                        _
                                            | shouldProbeAtStartup
                                            , isProviderUnavailable err ->
                                                chooseStartupProviderTransition
                                                    catalog
                                                    cwd
                                                    fullscreen
                                                    (tokenProviderBillingMode
                                                        tokenProvider)
                                                    provider
                                                    unavailableProviders
                                                    Nothing
                                                    err >>= \case
                                                        Just next ->
                                                            pure
                                                                (RunSwitchProvider
                                                                    next)
                                                        Nothing ->
                                                            startupFailure err
                                        _ -> do
                                            startupFailure err
                                Right result -> pure result
                    XAIProvider -> do
                        xaiOptions <- XAI.clientOptionsFromEnv
                        case multiCtx of
                            Just ctx ->
                                setSubagentRunner ctx.multiRegistry $
                                    runHttpSubagent
                                        subagentRuntime
                                        dialect
                                        XAIProvider
                                        ctx.multiSendToRoot
                                        (\childParams ->
                                            xaiBackend xaiOptions tokenProvider
                                                (pure childParams))
                            Nothing -> pure ()
                        let backend =
                                withPendingInputs pendingNotices $
                                    withConnectionRecovery $
                                        xaiBackend xaiOptions tokenProvider
                                            (readIORef paramsRef)
                            btwBackend privateParams =
                                xaiBackend xaiOptions tokenProvider
                                    (pure privateParams)
                            compactRunner focus = do
                                contextWindow <-
                                    currentModelContextWindow
                                        (XAIRequest.mapModel xaiOptions)
                                historyRef <-
                                    newIORef =<< readLiveTranscript conversationRef
                                installLiveCompactOutcome conversationRef Nothing
                                    (runResponsesCompactWithContextWindow
                                        contextWindow
                                        (\request ->
                                            runWithTokenProvider tokenProvider
                                                \credential ->
                                                    XAIClient.createResponseWith
                                                        xaiOptions
                                                        credential
                                                        request)
                                        recordCompactionUsage
                                        paramsRef
                                        historyRef)
                                    focus
                        activeBackend <-
                            prepareTransitionBackend
                                projectRoot transition persist backend
                        runSession
                            (sessionRequest
                                startupUnavailable
                                (Just tokenProvider)
                                loaded.loadedOpenAiPool
                                (if isJust customGenericOptions
                                    then Nothing
                                    else Just selectHttpAccount)
                                compactRunner)
                            SessionBackend
                                { backend = activeBackend
                                , btwBackend
                                , resetBackendState = pure ()
                                }
                    ClaudeCodeProvider -> do
                        claudeAuth <-
                            loadClaudeCodeAuth
                                >>= either (startupDie startup . Text.unpack) pure
                        let permission =
                                if claudeBypassEnabled
                                    then ClaudeCodeBypass
                                    else ClaudeCodeDontAsk
                            claudeOptions =
                                (defaultClaudeCodeOptions
                                    claudeAuth.executable
                                    (unsafeToFilePath cwd))
                                    { permission
                                    , safeMode = True
                                    }
                            compactRunner _ =
                                pure $ Left
                                    "Claude Code manages its own context; /compact is unavailable."
                            btwBackend privateParams =
                                Backend \state previous inputs onEvent -> do
                                    privateTranscript <- newIORef state
                                    let privateBackend =
                                            claudeCodeOneShotBackend
                                                claudeOptions
                                                    { permission =
                                                        ClaudeCodeDontAsk
                                                    }
                                                (pure privateParams)
                                                privateTranscript
                                    privateBackend.submitTurn
                                        state
                                        previous
                                        inputs
                                        onEvent
                        if claudeBypassEnabled
                            then pure ()
                            else
                                case fullscreen of
                                    Just runtime ->
                                        emitUiEvent runtime
                                            (UiSystemMessage
                                                "Claude Code is in non-blocking restricted mode; restart with --yolo to bypass Claude Code permission checks.")
                                    Nothing -> do
                                        color <- resolveColor stderrHandle
                                        putTextLn stderrHandle $
                                            roleWarn color $
                                                glyphWarn
                                                    <> "Claude Code is restricted; restart with --yolo to bypass Claude Code permission checks."
                        writeIORef activeAccountRef claudeAuth.accountLabel
                        claudeTranscriptRef <-
                            newIORef =<< readLiveTranscript conversationRef
                        withClaudeCodeBackendPermissions
                            claudeOptions
                            (nativeClaudePermissionHandler
                                startup.startupNativeHooks)
                            initialPrevious
                            (readIORef paramsRef)
                            claudeTranscriptRef
                            \backend -> do
                                activeBackend <-
                                    prepareTransitionBackend
                                        projectRoot transition persist backend
                                result <- runSession
                                    (sessionRequest
                                        startupUnavailable
                                        Nothing
                                        Nothing
                                        Nothing
                                        compactRunner)
                                    SessionBackend
                                        { backend = activeBackend
                                        , btwBackend
                                        , resetBackendState =
                                            writeIORef claudeTranscriptRef []
                                        }
                                writeLiveTranscript conversationRef
                                    =<< readIORef claudeTranscriptRef
                                pure result
                    OpenRouterProvider -> do
                        let makeBackend params =
                                case customGenericOptions of
                                    Just genericOptions ->
                                        genericResponsesBackendWith
                                            (\request onEvent ->
                                                GenericResponses.createResponseWithEvents
                                                    genericOptions
                                                        { GenericResponses.model =
                                                            transportModel
                                                                (fromMaybe
                                                                    model
                                                                    request.model)
                                                        }
                                                    request
                                                    onEvent)
                                            params
                                    Nothing ->
                                        openRouterBackend openRouterOptions
                                            tokenProvider params
                        case multiCtx of
                            Just ctx ->
                                setSubagentRunner ctx.multiRegistry $
                                    runHttpSubagent
                                        subagentRuntime
                                        dialect
                                        OpenRouterProvider
                                        ctx.multiSendToRoot
                                        (\childParams ->
                                            makeBackend
                                                (pure childParams))
                            Nothing -> pure ()
                        let backend =
                                withPendingInputs pendingNotices $
                                    withConnectionRecovery $
                                        makeBackend
                                            (readIORef paramsRef)
                            btwBackend privateParams =
                                makeBackend
                                    (pure privateParams)
                            compactRunner focus = do
                                contextWindow <-
                                    currentModelContextWindow transportModel
                                historyRef <-
                                    newIORef =<< readLiveTranscript conversationRef
                                installLiveCompactOutcome conversationRef Nothing
                                    (case customGenericOptions of
                                        Just genericOptions ->
                                            runResponsesCompactWithContextWindow
                                                contextWindow
                                                (\request ->
                                                    GenericResponses.createResponseWith
                                                        genericOptions
                                                            { GenericResponses.model =
                                                                transportModel
                                                                    (fromMaybe
                                                                        model
                                                                        request.model)
                                                            }
                                                        request)
                                                recordCompactionUsage
                                                paramsRef
                                                historyRef
                                        Nothing ->
                                            runResponsesCompactWithContextWindow
                                                contextWindow
                                                (\request ->
                                                    runWithTokenProvider
                                                        tokenProvider
                                                        \credential ->
                                                            OpenRouter.createResponseWith
                                                                openRouterOptions
                                                                credential
                                                                request)
                                                recordCompactionUsage
                                                paramsRef
                                                historyRef)
                                    focus
                        activeBackend <-
                            prepareTransitionBackend
                                projectRoot transition persist backend
                        runSession
                            (sessionRequest
                                startupUnavailable
                                (Just tokenProvider)
                                loaded.loadedOpenAiPool
                                (Just selectHttpAccount)
                                compactRunner)
                            SessionBackend
                                { backend = activeBackend
                                , btwBackend
                                , resetBackendState = pure ()
                                }
          where
            startupFailure err = do
                now <- getCurrentTime
                startupDie startup
                    (Text.unpack (formatApiErrorAt now err))

trackCredentialAccount
    :: IORef Text
    -> IORef Text
    -> IORef Text
    -> (Credential -> IO Text)
    -> TokenProvider
    -> TokenProvider
trackCredentialAccount accountRef accountIdRef selectionRef resolveLabel provider =
    tokenProvider (tokenProviderBillingMode provider) \failed ->
        getNextToken provider failed >>= \case
            Left err -> pure (Left err)
            Right credential -> do
                previousAccountId <- readIORef accountIdRef
                writeIORef accountIdRef credential.accountId
                when (previousAccountId /= credential.accountId) $
                    writeIORef selectionRef credential.accountId
                resolveLabel credential >>= writeIORef accountRef
                pure (Right credential)

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

sessionRunnerContinuation :: SessionRunner.SessionRunnerContinuation
sessionRunnerContinuation =
    SessionRunner.SessionRunnerContinuation
        { runnerRepl = repl
        , runnerReplWithDraft = replWithDraft
        , runnerRunPendingTurn = runPendingTurn
        , runnerFinishTurn = finishTurn
        , runnerFinishStartup = finishStartup
        , runnerPreparePromptSkillInputs = preparePromptSkillInputs
        , runnerRunSessionRecap = runSessionRecap
        , runnerRunSessionTurnSummary = runSessionTurnSummary
        }
runSession
    :: SessionRequest
    -> SessionBackend
    -> IO RunResult
runSession = SessionRunner.runSession sessionRunnerContinuation

restartSessionOptions :: CliOptions -> Text -> CliOptions
restartSessionOptions options sessionId =
    options
        { optProvider = Nothing
        , optModel = Nothing
        , optCwd = Nothing
        , optWorktree = False
        , optEffort = Nothing
        , optPrompt = Nothing
        , optPromptFile = Nothing
        , optManagedTurnFile = Nothing
        , optResume = Just sessionId
        }
