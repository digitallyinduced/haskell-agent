module Agent.CLI.Runtime.Orchestration.Flow
    ( runAgentWithRuntime
    , withRestoredCurrentDirectory
    ) where

import Agent.CLI.AccountPicker ()
import Agent.CLI.AccountSelection ()
import Agent.CLI.Afk ()
import Agent.CLI.AgentSessions ( signalManagedSessionReady )
import Agent.CLI.AgentViewport ( AgentTarget(AgentRoot) )
import Agent.CLI.Approval ()
import Agent.CLI.Artifact ()
import Agent.CLI.Auth ()
import Agent.CLI.Clipboard ()
import Agent.CLI.CodeModeRuntime ()
import Agent.CLI.Command ()
import Agent.CLI.Compaction ()
import Agent.CLI.Config
    ( HarnessConfig(configTheme)
    , loadHarnessConfig
    )
import Agent.Connectivity ()
import Agent.CLI.Database ()
import Agent.CLI.Database.Store ()
import Agent.CLI.Dialects ()
import Agent.CLI.Error ( formatApiErrorAt )
import Agent.CLI.GatewayClient
    ( GatewayModelAccess
    , gatewayCredentialIdentity
    , loadGatewayCredential
    , newGatewayModelAccess
    )
import Agent.CLI.GatewayBridge ()
import Agent.CLI.Input ()
import Agent.CLI.Interrupt
    ( newInterruptState,
      noteFullscreenCtrlC,
      CtrlCDecision(ForceExit) )
import Agent.CLI.LearnedSkills ()
import Agent.CLI.LearnedSkills.Store ()
import Agent.CLI.Login ( runLoginManager )
import Agent.CLI.Lsp ()
import Agent.CLI.ManagedTurn ()
import Agent.CLI.McpManager ()
import Agent.CLI.McpStatus ()
import Agent.CLI.ModelConfig ( loadModelCatalogAt )
import Agent.CLI.Models
    ( defaultModelOptionFor,
      resolveConfiguredModel,
      ModelOption(modelTarget),
      ModelTarget(targetDialect, targetConnectionId, targetProvider,
                  targetModelId) )
import Agent.CLI.Options
    ( defaultEffortFor,
      freshSessionOptions,
      gatewayRoutingChanged,
      isOneShot,
      CliOptions(optMotionMode, optManagedTurnFile, optScreenMode,
                 optProvider, optModel, optWorktree, optEffort, optPrompt,
                 optPromptFile, optResume, optCwd, optCodeMode, optYolo),
      ScreenMode(ScreenMinimal) )
import Agent.CLI.PendingInputs ()
import Agent.CLI.Plan ()
import Agent.CLI.Project ()
import Agent.CLI.Prompt ()
import Agent.CLI.PromptHooks ()
import Agent.CLI.Provider.OpenAI ()
import Agent.CLI.Provider.Switch
    ( continueAutomaticFallback, reportProviderUnavailable )
import Agent.CLI.ProviderAvailability ()
import Agent.CLI.ProviderFallback ()
import Agent.CLI.ProviderTransition
    ( applyProviderTransition,
      ProviderTransition(ProviderTransition, transitionCause,
                         transitionUnavailableProviders, transitionPendingTurn,
                         transitionTarget, transitionAccountSelectionId,
                         transitionAccountId, transitionAutomaticBilling,
                         transitionSessionId, transitionEffort),
      TransitionCause(AutomaticFallback, ManualTransition) )
import Agent.CLI.Recap ()
import Agent.CLI.Render ( putTextLn )
import Agent.CLI.ReplMode ()
import Agent.CLI.Request ()
import Agent.CLI.Resume ( validateResumeMetaForBoundary )
import Agent.CLI.Runtime.HistorySource ()
import Agent.CLI.Runtime.Orchestration.Background ()
import Agent.CLI.Runtime.Orchestration.Concurrent ()
import Agent.CLI.Runtime.Orchestration.Initialized
    ( PreparedStartupAuthWorker
    , runAgentInitialized
    , withPreparedStartupAuth
    )
import Agent.CLI.Runtime.Orchestration.Restart
    ( RestartCallbacks(..)
    , runFullscreenRestartLoop
    )
import Agent.CLI.Runtime.Orchestration.Startup
    ( clearNativeProgress, setNativeProgress )
import Agent.CLI.Runtime.Orchestration.Types
    ( AgentProcessRuntime(processNetworkRecovery)
    , AgentRunMode
        ( runInBackground
        , runStdout
        , runStderr
        , runCwdHint
        , runNativeHooks
        )
    , NativeRunHooks(nativeRegisterCancel)
    )
import Agent.CLI.Runtime.Persistence ()
import Agent.CLI.Runtime.Recap ()
import Agent.CLI.Runtime.Repl ()
import Agent.CLI.Runtime.Types
    ( DevResult(..), PreparedAgent(..), RunResult(..) )
import Agent.CLI.Secret ()
import Agent.CLI.Session
    ( deleteSession,
      loadActiveSession,
      loadSessionMeta,
      sessionDirForId,
      sessionsRoot,
      SessionMeta(metaCwd) )
import Agent.CLI.Session.Attachments ()
import Agent.CLI.ModelPicker
    ( ModelPickerSelection(modelPickerEffort, modelPickerOption) )
import Agent.CLI.Session.Choices ( modelChoiceWithEffort )
import Agent.CLI.Session.History ()
import Agent.CLI.Session.Lifecycle ()
import Agent.CLI.Session.Runtime.Types
    ( StartupCancelled(..),
      StartupFailure(..),
      StartupRuntime(startupSessionState, StartupRuntime, startupToolEnv,
                     startupNetworkRecovery, startupDatabaseStore,
                     startupInterrupt, startupEscPaused,
                     startupUiRuntimeRef, startupFullscreen, startupTerminal,
                     startupStdout, startupStderr, startupBackground, startupUseColor,
                     startupStderrTty, startupStdinTty, startupStdoutTty,
                     startupFullscreenReused, startupAgentSnapshot, startupAgentSelect,
                     startupRestartEffort, startupStartedAt, startupTimings,
                     startupSyntaxLoadDuration, startupFinished,
                     startupNativeHooks) )
import Agent.CLI.Session.Selection ()
import Agent.CLI.SessionAdmin ( managedPostgresConfigForHome )
import Agent.CLI.SessionEnv ()
import Agent.CLI.SessionLock
    ( acquireSessionLock, releaseSessionLock, SessionLock )
import Agent.CLI.SessionState ( SessionState(..), newSessionState )
import Agent.CLI.SessionTitle ()
import Agent.CLI.Skills ()
import Agent.CLI.Startup.Auth
    ( recordStartupTiming, setStartupNotice )
import Agent.CLI.StartupContext ()
import Agent.CLI.Style
    ( glyphOk, glyphSession, roleError, roleMuted, setCliWindowTitle )
import Agent.CLI.Subagents.Runtime ()
import Agent.CLI.TUI.App
    ( FullscreenInputBuffer,
      FullscreenRuntime,
      clearFullscreenHistorySource,
      emitUiEvent,
      newFullscreenInputBuffer,
      newFullscreenRuntimeWithTheme,
      queuedFullscreenInputDisplays,
      runFullscreen,
      setFullscreenSessionActions )
import Agent.CLI.TUI.History ()
import Agent.CLI.TUI.SessionHistory ()
import Agent.CLI.Terminal
    ( copyTerminalClipboard,
      detectTerminalCapabilities,
      reportTerminalCwd,
      resolveColor,
      TerminalCapabilities(terminalNativeProgress) )
import Agent.CLI.Tools ()
import Agent.CLI.Turn ()
import Agent.CLI.Usage ()
import Agent.CLI.WebFetch ()
import Agent.CLI.Worktree
    ( createManagedWorktreeWithProgress, worktreeProgressMessage )
import Agent.Cancel ( requestCancel )
import Agent.Claude ()
import Agent.Dialect ()
import Agent.Error ()
import Agent.GrokBuild.Dialect.Goal ()
import Agent.GrokBuild.Dialect.Runtime ()
import Agent.GrokBuild.Dialect.Task ()
import Agent.GrokBuild.Dialect.Workflow ()
import Agent.Loop ()
import Agent.OpenAI.Compaction ()
import Agent.OpenAI.Usage ()
import Agent.OpenAI.WebSocketClient ()
import Agent.OpenRouter.LoopBackend ()
import Agent.OsPath ( toText )
import Agent.Provider ( Provider(OpenAIProvider) )
import Agent.ReasoningEffort ()
import Agent.Responses.GenericBackend ()
import Agent.Responses.GenericClient ()
import Agent.Responses.Types ()
import Agent.Skills ()
import Agent.Store.Postgres
    ( Store, closeStore, openStore, trustedPool )
import Agent.Store.Types ( renderStoreError )
import Agent.Subagents ()
import Agent.Subagents.TaskPath ()
import Agent.TUI.Model
    ( initialUiState,
      progressNotice,
      reduceUi,
      warningNotice,
      UiEvent(UiSystemMessage, UiSetRepository, UiSetNotice),
      UiState(uiQueuedInputs) )
import Agent.TUI.Motion ( nativeProgressAnimationEnabled )
import Agent.Tools.MultiAgents ()
import Agent.Tools.PlanMode ()
import Agent.Tools.Secret ()
import Agent.Tools.Types ( defaultToolEnv, ToolEnv(toolCancel) )
import Agent.XAI.LoopBackend ()
import Control.Applicative ( (<|>) )
import Control.Concurrent.Chan ()
import Control.Concurrent.MVar
    ( newEmptyMVar, newMVar, readMVar, tryPutMVar )
import Control.Concurrent.STM ()
import Control.Exception ()
import Control.Exception.Safe
    ( displayException, finally, mask, onException, throwIO, try, tryAny )
import Control.Monad ( when, forM_, void, unless )
import Data.Functor ()
import Data.IORef
    ( IORef, atomicModifyIORef', newIORef, readIORef, writeIORef )
import Data.List ()
import Data.Maybe ( fromMaybe, isJust, isNothing )
import Data.Text ( Text )
import Data.Time.Clock ( getCurrentTime )
import System.Console.ANSI ()
import System.Console.ANSI.Codes ()
import System.Directory.OsPath
    ( doesDirectoryExist,
      getCurrentDirectory,
      getHomeDirectory,
      makeAbsolute,
      setCurrentDirectory )
import System.Environment ()
import System.Exit ( die )
import System.IO ( hIsTerminalDevice, stderr, stdin )
import System.OsPath ( decodeFS, takeDirectory, takeFileName )
import System.Posix.Process ( executeFile )
import System.Process ( callProcess )
import qualified Data.ByteString as BS ()
import qualified Agent.Responses.GenericClient as GenericResponses
    ()
import qualified Agent.MCP as MCP ()
import qualified Data.Map.Strict as Map ()
import qualified Agent.OpenAI.Auth as OpenAI ()
import qualified Agent.OpenRouter as OpenRouter ()
import qualified Agent.OpenRouter.Usage as OpenRouterUsage ()
import qualified Agent.Provider as Provider ()
import qualified Agent.CLI.Session.Lifecycle as SessionLifecycle ()
import qualified Agent.CLI.Session.Runner as SessionRunner ()
import qualified Data.Set as Set ( empty )
import qualified Data.Text as Text ( pack, unpack )
import qualified Data.Text.IO as Text ( hPutStr )
import qualified Agent.XAI.Options as XAI ()
import qualified Agent.XAI.Client as XAIClient ()
import qualified Agent.XAI.Request as XAIRequest ()
import qualified Agent.XAI.Usage as XAIUsage ()

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
            RunForkSession sessionId directive ->
                newSessionState >>= \nextState -> do
                    writeIORef nextState.sessionInitialPrompt directive
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
            RunFreshSession cwd -> do
                -- A gateway login change is a routing boundary. Drop prompts
                -- queued for the prior endpoint along with its session state.
                nextInputs <- newFullscreenInputBuffer
                nextState <- newSessionState
                go nextInputs nextState
                    (freshSessionOptions current cwd)
                    Nothing
            RunDeleteSession sessionId cwd -> do
                home <- getHomeDirectory
                config <- managedPostgresConfigForHome home
                deletion <-
                    openStore config >>= \case
                        Left err ->
                            pure (Left (renderStoreError err))
                        Right store ->
                            deleteSession
                                (trustedPool store)
                                (sessionsRoot home)
                                sessionId
                                `finally` closeStore store
                color <- resolveColor runMode.runStderr
                case deletion of
                    Left err -> do
                        putTextLn runMode.runStderr
                            (roleError color
                                ("could not delete session "
                                    <> sessionId
                                    <> ": "
                                    <> err))
                        pure DevQuit
                    Right () -> do
                        putTextLn runMode.runStderr
                            (roleMuted color
                                (glyphOk
                                    <> "deleted session "
                                    <> sessionId))
                        newSessionState >>= \nextState ->
                            go fullscreenInputs nextState
                                current
                                    { optCwd = Just cwd
                                    , optWorktree = False
                                    , optPrompt = Nothing
                                    , optPromptFile = Nothing
                                    , optManagedTurnFile = Nothing
                                    , optResume = Nothing
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
            RunUpdateAndRestart sessionId -> do
                color <- resolveColor runMode.runStderr
                putTextLn runMode.runStderr
                    (roleMuted color
                        (glyphSession <> "installing the latest Haskell Agent…"))
                tryAny (updateAndResume sessionId) >>= \case
                    Left err -> do
                        putTextLn runMode.runStderr
                            (roleError color
                                ("update failed: "
                                    <> Text.pack (displayException err)))
                        go fullscreenInputs sessionState
                            (restartSessionOptions current sessionId)
                            Nothing
                    Right result -> pure result
            RunEnableCodeMode sessionId ->
                let nextOptions =
                        (restartSessionOptions current sessionId)
                            { optCodeMode = True }
                in go fullscreenInputs sessionState nextOptions Nothing
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

    updateAndResume sessionId = do
        callProcess "nix"
            [ "profile"
            , "remove"
            , "haskell-agent"
            ]
        callProcess "nix"
            [ "profile"
            , "add"
            , "--accept-flake-config"
            , "github:digitallyinduced/haskell-agent"
            ]
        executeFile
            "agent-cli"
            True
            ["--resume", Text.unpack sessionId]
            Nothing

-- | Restore the process cwd after an action succeeds or throws. Cabal gives
-- GHCi relative source paths, so returning from an agent session in its cwd
-- would make the following @:reload@ lose local modules.
withRestoredCurrentDirectory :: IO a -> IO a
withRestoredCurrentDirectory action = do
    originalCwd <- getCurrentDirectory
    action `finally` setCurrentDirectory originalCwd

recoveryGatewayAccess
    :: Provider
    -> Maybe ProviderTransition
    -> IO (Either Text (Maybe GatewayModelAccess))
recoveryGatewayAccess _ _ =
    loadGatewayCredential >>= \case
        Left err -> pure (Left ("Could not load gateway credentials: " <> err))
        Right Nothing -> pure (Right Nothing)
        Right (Just credential) ->
            Right . Just <$> newGatewayModelAccess credential

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
                let chooseRecoveryModel nextOptions nextTransition = do
                        home <- getHomeDirectory
                        cwd <- case nextOptions.optCwd <|> runMode.runCwdHint of
                            Nothing -> getCurrentDirectory
                            Just path -> makeAbsolute path
                        loadModelCatalogAt home cwd >>= \case
                            Left err -> pure (Left err)
                            Right catalog -> do
                                color <- resolveColor runMode.runStderr
                                let currentTarget =
                                        ((.transitionTarget) <$> nextTransition)
                                            <|> ( (.modelTarget)
                                                    <$> (nextOptions.optModel
                                                        >>= resolveConfiguredModel
                                                            catalog)
                                                )
                                            <|> ( (.modelTarget)
                                                    <$> (nextOptions.optProvider
                                                        >>= defaultModelOptionFor
                                                            catalog)
                                                )
                                            <|> ( (.modelTarget)
                                                    <$> defaultModelOptionFor
                                                        catalog
                                                        OpenAIProvider
                                                )
                                case currentTarget of
                                    Nothing ->
                                        pure
                                            (Left
                                                "No configured models are available.")
                                    Just current ->
                                        recoveryGatewayAccess
                                            current.targetProvider
                                            nextTransition >>= \case
                                                Left err -> pure (Left err)
                                                Right gatewayAccess ->
                                                    modelChoiceWithEffort
                                                        catalog
                                                        gatewayAccess
                                                        (Just runtime)
                                                        color
                                                        current.targetConnectionId
                                                        current.targetProvider
                                                        current.targetModelId
                                                        current.targetDialect
                                                        (fromMaybe
                                                            (defaultEffortFor
                                                                current.targetProvider)
                                                            ( (nextTransition
                                                                    >>= (.transitionEffort))
                                                                <|> nextOptions.optEffort
                                                            ))
                                                        >>= \case
                                                            Left err ->
                                                                pure (Left err)
                                                            Right Nothing ->
                                                                pure (Right Nothing)
                                                            Right (Just selection) ->
                                                                pure $ Right $ Just $
                                                                    recoveryModelTransition
                                                                        nextOptions
                                                                        nextTransition
                                                                        selection.modelPickerOption.modelTarget
                                                                        selection.modelPickerEffort
                    recoveryModelTransition
                            nextOptions nextTransition target selectedEffort =
                        case nextTransition of
                            Just active ->
                                active
                                    { transitionTarget = target
                                    , transitionEffort = Just selectedEffort
                                    , transitionAccountSelectionId = Nothing
                                    , transitionAccountId = Nothing
                                    , transitionUnavailableProviders = Set.empty
                                    , transitionCause = ManualTransition
                                    , transitionAutomaticBilling = Nothing
                                    }
                            Nothing ->
                                ProviderTransition
                                    { transitionTarget = target
                                    , transitionEffort = Just selectedEffort
                                    , transitionAccountSelectionId = Nothing
                                    , transitionAccountId = Nothing
                                    , transitionSessionId = nextOptions.optResume
                                    , transitionPendingTurn = Nothing
                                    , transitionUnavailableProviders = Set.empty
                                    , transitionCause = ManualTransition
                                    , transitionAutomaticBilling = Nothing
                                    }
                    callbacks = RestartCallbacks
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
                            gatewayBefore <- loadGatewayCredential
                            recoveryCwd <- getCurrentDirectory
                            color <- resolveColor stderr
                            runLoginManager color
                            gatewayAfter <- loadGatewayCredential
                            pure
                                if gatewayRoutingChanged
                                    gatewayBefore
                                    gatewayAfter
                                then Just (RunFreshSession recoveryCwd)
                                else Nothing
                        , restartChooseModel = chooseRecoveryModel
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
    configuredTheme <-
        loadHarnessConfig home >>= \case
            Left err -> failPreparation (Text.unpack err)
            Right config -> pure config.configTheme
    let root = sessionsRoot home
    databaseConfig <- managedPostgresConfigForHome home
    databaseStore <- openStore databaseConfig >>= \case
        Left err -> failPreparation (Text.unpack (renderStoreError err))
        Right store -> writeIORef databaseStoreRef (Just store) >> pure store
    let sessionPool = trustedPool databaseStore
    connectedGateway <-
        loadGatewayCredential >>= \case
            Left err ->
                failPreparation
                    ("Could not load gateway credentials: " <> Text.unpack err)
            Right credential -> pure credential
    let connectedGatewayIdentity =
            gatewayCredentialIdentity <$> connectedGateway
    resumed <- case options.optResume of
        Nothing -> pure Nothing
        Just sessionId -> mask \restore -> do
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
                    restore $ loadSessionMeta sessionPool root sessionId >>= \case
                        Left err -> do
                            signalReady (Left err)
                            failPreparation (Text.unpack err)
                        Right meta ->
                            case
                                validateResumeMetaForBoundary
                                    connectedGatewayIdentity
                                    meta
                            of
                                Left err -> do
                                    signalReady (Left err)
                                    failPreparation (Text.unpack err)
                                Right () ->
                                    loadActiveSession
                                        sessionPool
                                        root
                                        sessionId >>= \case
                                            Left err -> do
                                                signalReady (Left err)
                                                failPreparation
                                                    (Text.unpack err)
                                            Right loaded@(loadedMeta, _) ->
                                                case
                                                    validateResumeMetaForBoundary
                                                        connectedGatewayIdentity
                                                        loadedMeta
                                                of
                                                    Left err -> do
                                                        signalReady (Left err)
                                                        failPreparation
                                                            (Text.unpack err)
                                                    Right () -> do
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
                            <> toText (takeFileName initialCwd))
                        (toText initialCwd))
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
                Just <$> newFullscreenRuntimeWithTheme
                    configuredTheme
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
    -- A reused fullscreen must not retain its prior transcript while resumed
    -- history is revalidated and installed for this preparation snapshot.
    forM_ fullscreen clearFullscreenHistorySource
    writeIORef uiRuntimeRef fullscreen
    resumeLock <- readIORef resumeLockRef
    let runAction
            :: Maybe PreparedStartupAuthWorker
            -> IO RunResult
        runAction preparedAuth =
            do
                cwd <- case resumed of
                    Just _ -> pure initialCwd
                    Nothing
                        | options.optWorktree -> do
                            readMVar firstFrameReady
                            let reportWorktreeProgress progress = do
                                    let message =
                                            worktreeProgressMessage progress
                                    case fullscreen of
                                        Nothing ->
                                            putTextLn stderrHandle message
                                        Just runtime ->
                                            emitUiEvent runtime
                                                (UiSetNotice
                                                    (Just
                                                        (progressNotice
                                                            message)))
                            createManagedWorktreeWithProgress
                                reportWorktreeProgress
                                home
                                source
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
                        Nothing
                        (requestCancel toolEnv.toolCancel)
                        (\_ _ -> pure (Right ()))
                        (const (pure ()))
                        (pure ())
                        (\level ->
                            readIORef restartEffortActionRef >>= ($ level))
                        (noteFullscreenCtrlC interrupt)
                        (readIORef agentSnapshotRef >>= id)
                        (\target -> readIORef agentSelectRef >>= ($ target))
                let startup = StartupRuntime
                        { startupToolEnv = toolEnv
                        , startupNetworkRecovery =
                            processRuntime.processNetworkRecovery
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
                    (runAgentWithRuntime processRuntime)
                    processRuntime
                    options
                    transition
                    home
                    root
                    resumed
                    resumeLock
                    cwd
                    startup
                    connectedGateway
                    preparedAuth
        action
            | options.optWorktree
            , isNothing resumed
            , isNothing transition = do
                let prepareAccountUsage =
                        options.optYolo
                            || isNothing fullscreen
                            || isJust options.optProvider
                            || isJust options.optModel
                withPreparedStartupAuth
                    prepareAccountUsage
                    options.optProvider
                    (runAction . Just)
            | otherwise = runAction Nothing
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
        Nothing
        (pure ())
        (\_ _ -> pure (Right ()))
        (const (pure ()))
        (pure ())
        (const (pure ()))
        -- No session-local interrupt state is alive between providers. A
        -- transition must remain escapable even if auth probing blocks.
        (pure ForceExit)
        (pure (AgentRoot, []))
        (const (pure ()))


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
