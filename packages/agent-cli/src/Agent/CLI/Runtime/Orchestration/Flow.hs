module Agent.CLI.Runtime.Orchestration.Flow
    ( runAgentWithRuntime
    , withRestoredCurrentDirectory
    ) where

import Agent.CLI.CancelWatch (newStdinControl)
import Agent.CLI.AgentSessions ( signalManagedSessionReady )
import Agent.CLI.AgentViewport ( AgentTarget(AgentRoot) )
import Agent.Runtime.Config
    ( HarnessConfig(configTheme)
    , loadHarnessConfig
    )
import Agent.Runtime.Error ( formatApiErrorAt )
import Agent.Runtime.GatewayClient
    ( GatewayCredential
    , GatewayModelAccess
    , gatewayCredentialIdentity
    , loadGatewayCredential
    , loadGatewayCredentialAt
    , newGatewayModelAccess
    )
import Agent.CLI.Interrupt
    ( newInterruptState,
      noteFullscreenCtrlC,
      CtrlCDecision(ForceExit) )
import Agent.CLI.Login ( runLoginManager )
import Agent.Runtime.ModelConfig ( loadModelCatalogAt )
import Agent.Runtime.Models
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
                 optPromptFile, optResume, optCwd, optCodeMode, optYolo), Override(..),
      CodeModeOption(CodeModeEnabled),
      ScreenMode(ScreenMinimal) )
import Agent.CLI.Provider.Switch
    ( continueAutomaticFallback, reportProviderUnavailable )
import Agent.CLI.ProviderTransition
    ( applyProviderTransition,
      ProviderTransition(ProviderTransition, transitionCause,
                         transitionUnavailableProviders, transitionPendingTurn,
                         transitionTarget, transitionAccountSelectionId,
                         transitionAccountId,
                         transitionSessionId, transitionEffort),
      TransitionCause(AutomaticFallback, ManualTransition) )
import Agent.CLI.Render ( putTextLn )
import Agent.CLI.Resume ( validateResumeMetaForBoundary )
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
    ( AgentProcessRuntime(processNetworkRecovery, processToolResourceArbiter)
    , AgentRunMode
        ( runInBackground
        , runStdout
        , runStderr
        , runCwdHint
        , runNativeHooks
        )
    , NativeRunHooks
        ( nativeDatabaseStore
        , nativeCapabilities
        , nativeHome
        , nativeRegisterCancel
        , nativeWorkspaceDiscovery
        )
    , NativeDiscoveryContext
        ( nativeDiscoveryCatalogRoot
        , nativeDiscoveryHome
        )
    , NativeRunCapabilities(nativeProviderFallback)
    , fullNativeRunCapabilities
    , nativePreparedDiscovery
    )
import Agent.CLI.Runtime.Types
    ( DevResult(..), PreparedAgent(..), RunResult(..) )
import Agent.Runtime.Session
    ( deleteSession,
      loadActiveSession,
      loadSessionMeta,
      sessionDirForId,
      sessionsRoot,
      SessionMeta(metaCwd),
      SessionTurn )
import Agent.CLI.ModelPicker
    ( ModelPickerSelection(modelPickerEffort, modelPickerOption) )
import Agent.CLI.Session.Choices ( modelChoiceWithEffort )
import Agent.CLI.Session.Runtime.Types
    ( StartupCancelled(..),
      StartupFailure(..),
      StartupRuntime(startupSessionState, StartupRuntime, startupToolEnv,
                     startupHarnessConfig,
                     startupNetworkRecovery, startupDatabaseStore,
                     startupInterrupt, startupStdinControl,
                     startupUiRuntimeRef, startupFullscreen, startupTerminal,
                     startupStdout, startupStderr, startupBackground, startupUseColor,
                     startupStderrTty, startupStdinTty, startupStdoutTty,
                     startupFullscreenReused, startupAgentSnapshot, startupAgentSelect,
                     startupRestartEffort, startupStartedAt, startupTimings,
                     startupSyntaxLoadDuration, startupFinished,
                     startupNativeHooks) )
import Agent.CLI.SessionAdmin ( managedPostgresConfigForHome )
import Agent.Runtime.SessionLock
    ( acquireSessionLock, releaseSessionLock, SessionLock )
import Agent.CLI.SessionState ( SessionState(..), newSessionState )
import Agent.CLI.Startup.Auth
    ( recordStartupTiming, setStartupNotice )
import Agent.CLI.Style
    ( glyphOk, glyphSession, roleError, roleMuted, setCliWindowTitle )
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
import Agent.CLI.Terminal
    ( copyTerminalClipboard,
      detectTerminalCapabilities,
      reportTerminalCwd,
      resolveColor,
      TerminalCapabilities(terminalNativeProgress) )
import Agent.CLI.Worktree
    ( createManagedWorktreeFromConfigWithProgress, worktreeProgressMessage
    , restoreManagedWorktree, worktreeRoot )
import Agent.Cancel ( requestCancel )
import Agent.OsPath ( toText )
import Agent.Provider ( Provider(OpenAIProvider) )
import Agent.Store.Postgres
    ( Store, closeStore, openStore, trustedPool )
import Agent.Store.Types ( renderStoreError )
import Agent.TUI.Model
    ( initialUiState,
      progressNotice,
      reduceUi,
      warningNotice,
      UiEvent(UiSystemMessage, UiSetRepository, UiSetNotice),
      UiState(uiQueuedInputs) )
import Agent.TUI.Motion ( nativeProgressAnimationEnabled )
import Agent.TUI.Theme ( ThemeKind )
import Agent.Tools.Types ( defaultToolEnv, ToolEnv(toolCancel, toolResourceArbiter) )
import Control.Applicative ( (<|>) )
import Control.Concurrent.MVar
    ( MVar, newEmptyMVar, newMVar, readMVar, tryPutMVar )
import Control.Exception.Safe
    ( bracketOnError, displayException, finally, onException, throwIO, try, tryAny )
import Control.Monad ( when, forM_, void, unless )
import Data.IORef
    ( IORef, atomicModifyIORef', newIORef, readIORef, writeIORef )
import Data.Maybe ( fromMaybe, isJust, isNothing )
import Data.Text ( Text )
import Data.Time.Clock ( NominalDiffTime, UTCTime, getCurrentTime )
import System.Directory.OsPath
    ( doesDirectoryExist,
      getCurrentDirectory,
      getHomeDirectory,
      makeAbsolute,
      setCurrentDirectory )
import System.Exit ( die )
import System.IO ( hIsTerminalDevice, stderr, stdin )
import System.OsPath ( OsPath, decodeFS, takeDirectory, takeFileName )
import System.Posix.Process ( executeFile )
import System.Process ( callProcess )
import qualified Data.Set as Set ( empty )
import qualified Data.Text as Text ( pack, unpack )
import qualified Data.Text.IO as Text ( hPutStr )

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
                            { optCodeMode = CodeModeEnabled }
                in go fullscreenInputs sessionState nextOptions Nothing
            RunProviderStartFailed apiError ->
                case transition of
                    Just failed
                        | AutomaticFallback _ <- failed.transitionCause ->
                            continueAutomaticFallback
                                (nativeProviderFallbackEnabled runMode)
                                (nativeRunHomeHint runMode)
                                (nativeDiscoveryCwdHint runMode)
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
                                                (formatApiErrorAt now apiError)
                                    | otherwise -> do
                                        reportProviderUnavailable Nothing apiError
                                        pure DevQuit
                    _
                        | runMode.runInBackground -> do
                            now <- getCurrentTime
                            throwIO $
                                StartupFailure
                                    (formatApiErrorAt now apiError)
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

nativeDiscoveryCwdHint :: AgentRunMode -> Maybe OsPath
nativeDiscoveryCwdHint runMode =
    (runMode.runNativeHooks
        >>= nativePreparedDiscovery . (.nativeWorkspaceDiscovery)
        >>= Just . (.nativeDiscoveryCatalogRoot))
        <|> runMode.runCwdHint

nativeRunHomeHint :: AgentRunMode -> Maybe OsPath
nativeRunHomeHint runMode =
    runMode.runNativeHooks >>= \hooks ->
        ( (.nativeDiscoveryHome)
            <$> nativePreparedDiscovery hooks.nativeWorkspaceDiscovery
        )
            <|> hooks.nativeHome

nativeProviderFallbackEnabled :: AgentRunMode -> Bool
nativeProviderFallbackEnabled runMode =
    maybe
        fullNativeRunCapabilities.nativeProviderFallback
        ((.nativeProviderFallback) . (.nativeCapabilities))
        runMode.runNativeHooks

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
                        home <-
                            maybe
                                getHomeDirectory
                                pure
                                (nativeRunHomeHint runMode)
                        cwd <- case nextOptions.optCwd <|> runMode.runCwdHint of
                            Nothing -> getCurrentDirectory
                            Just path -> makeAbsolute path
                        loadModelCatalogAt
                            home
                            (fromMaybe cwd
                                (runMode.runNativeHooks
                                    >>= nativePreparedDiscovery
                                        . (.nativeWorkspaceDiscovery)
                                    >>= Just . (.nativeDiscoveryCatalogRoot)))
                            >>= \case
                            Left err -> pure (Left err)
                            Right catalog -> do
                                color <- resolveColor runMode.runStderr
                                let preferredTarget =
                                        ((.transitionTarget) <$> nextTransition)
                                            <|> ( (.modelTarget)
                                                    <$> (nextOptions.optModel
                                                        >>= resolveConfiguredModel
                                                            catalog)
                                                )
                                    defaultOption = defaultModelOptionFor catalog
                                        (fromMaybe OpenAIProvider nextOptions.optProvider)
                                    current = fromMaybe defaultOption.modelTarget
                                        preferredTarget
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
                                    (nativeProviderFallbackEnabled runMode)
                                    (nativeRunHomeHint runMode)
                                    (nativeDiscoveryCwdHint runMode)
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
                        else die (Text.unpack message))
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

data AgentIterationRequest = AgentIterationRequest
    { iterationResumeLockRef :: IORef (Maybe SessionLock)
    , iterationDatabaseStoreRef :: IORef (Maybe Store)
    , iterationProcessRuntime :: AgentProcessRuntime
    , iterationRunMode :: AgentRunMode
    , iterationFullscreenInputs :: FullscreenInputBuffer
    , iterationSessionState :: SessionState
    , iterationActiveFullscreen :: Maybe FullscreenRuntime
    , iterationOptions :: CliOptions
    , iterationTransition :: Maybe ProviderTransition
    }

-- | Only the local information needed to draw the first editable frame.
-- Database connections, gateway credentials and resumed history belong to the
-- backend worker, not the terminal's initialization path.
data AgentIterationBootstrap = AgentIterationBootstrap
    { iterationStartedAt :: UTCTime
    , iterationStartupTimings :: IORef [(Text, NominalDiffTime)]
    , iterationSyntaxLoadDuration :: IORef (Maybe NominalDiffTime)
    , iterationStartupFinished :: IORef Bool
    , iterationHarnessConfig :: HarnessConfig
    , iterationConfiguredTheme :: ThemeKind
    , iterationHome :: OsPath
    , iterationRoot :: OsPath
    , iterationInitialSource :: OsPath
    }

data AgentIterationResources = AgentIterationResources
    { iterationBootstrap :: AgentIterationBootstrap
    , iterationDatabaseStore :: Store
    , iterationConnectedGateway :: Maybe GatewayCredential
    , iterationResumed :: Maybe (SessionMeta, [SessionTurn])
    , iterationSource :: OsPath
    }

data AgentIterationInterface = AgentIterationInterface
    { iterationFullscreen :: Maybe FullscreenRuntime
    , iterationFirstFrameReady :: MVar ()
    , iterationTerminal :: TerminalCapabilities
    , iterationUiRuntimeRef :: IORef (Maybe FullscreenRuntime)
    , iterationCancelToolRef :: IORef (IO ())
    , iterationInstallToolRuntime :: ToolEnv -> IO ()
    , iterationBuildStartupRuntime :: Store -> ToolEnv -> StartupRuntime
    }

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
        fullscreenInputs sessionState activeFullscreen options transition =
    prepareTrackedAgentIteration AgentIterationRequest
        { iterationResumeLockRef = resumeLockRef
        , iterationDatabaseStoreRef = databaseStoreRef
        , iterationProcessRuntime = processRuntime
        , iterationRunMode = runMode
        , iterationFullscreenInputs = fullscreenInputs
        , iterationSessionState = sessionState
        , iterationActiveFullscreen = activeFullscreen
        , iterationOptions = options
        , iterationTransition = transition
        }

prepareTrackedAgentIteration
    :: AgentIterationRequest
    -> IO PreparedAgent
prepareTrackedAgentIteration request = do
    forM_
        request.iterationActiveFullscreen
        resetFullscreenSessionActions
    bootstrap <- prepareAgentIterationBootstrap request
    interface <- prepareAgentIterationInterface request bootstrap
    -- Run resource preparation inside the existing fullscreen worker. Input
    -- can be edited/queued immediately, but no session action or model request
    -- is installed until initialization and resume validation have succeeded.
    let activeRequest =
            request
                { iterationActiveFullscreen = interface.iterationFullscreen }
        action = do
            resources <- prepareAgentIterationResources activeRequest bootstrap
            resumeLock <- readIORef request.iterationResumeLockRef
            prepareAgentIterationAction
                request
                resources
                interface
                resumeLock
        cleanup =
            cleanupAgentIteration request interface
    pure PreparedAgent
        { preparedFullscreen = interface.iterationFullscreen
        , preparedRun = action `finally` cleanup
        }

prepareAgentIterationBootstrap
    :: AgentIterationRequest
    -> IO AgentIterationBootstrap
prepareAgentIterationBootstrap request = do
    startedAt <- getCurrentTime
    startupTimingsRef <- newIORef []
    syntaxLoadDurationRef <- newIORef Nothing
    startupFinishedRef <- newIORef False
    home <- case nativeRunHomeHint request.iterationRunMode of
        Nothing -> getHomeDirectory
        Just path -> pure path
    harnessConfig <-
        loadHarnessConfig home >>= \case
            Left err ->
                failAgentIterationPreparation request err
            Right config -> pure config
    let configuredTheme = harnessConfig.configTheme
    let root = sessionsRoot home
    initialSource <-
        maybe
            getCurrentDirectory
            makeAbsolute
            (request.iterationOptions.optCwd
                <|> request.iterationRunMode.runCwdHint)
    recordStartupTiming startedAt startupTimingsRef "configuration"
    pure AgentIterationBootstrap
        { iterationStartedAt = startedAt
        , iterationStartupTimings = startupTimingsRef
        , iterationSyntaxLoadDuration = syntaxLoadDurationRef
        , iterationStartupFinished = startupFinishedRef
        , iterationHarnessConfig = harnessConfig
        , iterationConfiguredTheme = configuredTheme
        , iterationHome = home
        , iterationRoot = root
        , iterationInitialSource = initialSource
        }

prepareAgentIterationResources
    :: AgentIterationRequest
    -> AgentIterationBootstrap
    -> IO AgentIterationResources
prepareAgentIterationResources request bootstrap = do
    let home = bootstrap.iterationHome
        root = bootstrap.iterationRoot
        recordStage =
            recordStartupTiming
                bootstrap.iterationStartedAt
                bootstrap.iterationStartupTimings
    setStartupNotice request.iterationActiveFullscreen "Opening session storage…"
    databaseStore <-
        case
            request.iterationRunMode.runNativeHooks
                >>= (.nativeDatabaseStore)
        of
        Just borrowed -> pure borrowed
        Nothing -> do
            databaseConfig <- managedPostgresConfigForHome home
            trackPreparationResource
                request.iterationDatabaseStoreRef
                closeStore
                (openStore databaseConfig) >>= \case
                Left err ->
                    failAgentIterationPreparation request
                        (renderStoreError err)
                Right store -> pure store
    recordStage "database"
    connectedGateway <-
        loadGatewayCredentialAt home >>= \case
            Left err ->
                failAgentIterationPreparation request
                    ("Could not load gateway credentials: " <> err)
            Right credential -> pure credential
    recordStage "gateway credentials"
    let connectedGatewayIdentity =
            gatewayCredentialIdentity <$> connectedGateway
    resumed <-
        loadAgentIterationResume
            request
            root
            databaseStore
            connectedGatewayIdentity
    recordStage "resume"
    source <- case request.iterationOptions.optCwd of
        Just requestedCwd -> makeAbsolute requestedCwd
        Nothing -> case resumed of
            Just (meta, _) -> makeAbsolute meta.metaCwd
            Nothing -> pure bootstrap.iterationInitialSource
    -- Native resumes supply optCwd too. Restore the selected effective cwd,
    -- not an unrelated historical path when the caller overrides it.
    forM_ resumed $ \_ ->
        restoreManagedWorktree (worktreeRoot home) source >>= \case
            Left err -> failAgentIterationPreparation request
                ("Could not restore session worktree: " <> err)
            Right () -> pure ()
    setStartupNotice request.iterationActiveFullscreen
        (if request.iterationOptions.optWorktree && isNothing resumed
            then "Creating worktree…"
            else "Loading project…")
    pure AgentIterationResources
        { iterationBootstrap = bootstrap
        , iterationDatabaseStore = databaseStore
        , iterationConnectedGateway = connectedGateway
        , iterationResumed = resumed
        , iterationSource = source
        }

loadAgentIterationResume
    :: AgentIterationRequest
    -> OsPath
    -> Store
    -> Maybe Text
    -> IO (Maybe (SessionMeta, [SessionTurn]))
loadAgentIterationResume request root databaseStore connectedGatewayIdentity =
    case request.iterationOptions.optResume of
        Nothing -> pure Nothing
        Just sessionId -> do
            let sessionPool = trustedPool databaseStore
            dir <- either
                (\err -> do
                    signalAgentIterationReady request (Left err)
                    failAgentIterationPreparation request err)
                pure
                (sessionDirForId root sessionId)
            exists <- doesDirectoryExist dir
            when (not exists) do
                let err = "session not found: " <> sessionId
                signalAgentIterationReady request (Left err)
                failAgentIterationPreparation request err
            trackPreparationResource
                request.iterationResumeLockRef
                releaseSessionLock
                (acquireSessionLock dir sessionId) >>= \case
                Left err -> do
                    signalAgentIterationReady request (Left err)
                    failAgentIterationPreparation request err
                Right _ -> do
                    loadSessionMeta sessionPool root sessionId >>= \case
                        Left err -> do
                            signalAgentIterationReady request (Left err)
                            failAgentIterationPreparation request err
                        Right meta ->
                            case
                                validateResumeMetaForBoundary
                                    connectedGatewayIdentity
                                    meta
                            of
                                Left err -> do
                                    signalAgentIterationReady request (Left err)
                                    failAgentIterationPreparation request err
                                Right () ->
                                    loadActiveSession
                                        sessionPool
                                        root
                                        sessionId >>= \case
                                            Left err -> do
                                                signalAgentIterationReady
                                                    request
                                                    (Left err)
                                                failAgentIterationPreparation
                                                    request
                                                    err
                                            Right loaded@(loadedMeta, _) ->
                                                case
                                                    validateResumeMetaForBoundary
                                                        connectedGatewayIdentity
                                                        loadedMeta
                                                of
                                                    Left err -> do
                                                        signalAgentIterationReady
                                                            request
                                                            (Left err)
                                                        failAgentIterationPreparation
                                                            request
                                                            err
                                                    Right () -> do
                                                        signalAgentIterationReady
                                                            request
                                                            (Right ())
                                                        pure (Just loaded)

signalAgentIterationReady
    :: AgentIterationRequest
    -> Either Text ()
    -> IO ()
signalAgentIterationReady request result =
    unless
        request.iterationRunMode.runInBackground
        (signalManagedSessionReady result)

failAgentIterationPreparation
    :: AgentIterationRequest
    -> Text
    -> IO a
failAgentIterationPreparation request message =
    releasePreparationResources
        request.iterationResumeLockRef
        request.iterationDatabaseStoreRef >>
        case request.iterationActiveFullscreen of
            Nothing
                | request.iterationRunMode.runInBackground ->
                    throwIO (StartupFailure message)
                | otherwise -> die (Text.unpack message)
            Just _ -> throwIO (StartupFailure message)

prepareAgentIterationInterface
    :: AgentIterationRequest
    -> AgentIterationBootstrap
    -> IO AgentIterationInterface
prepareAgentIterationInterface request bootstrap = do
    let runMode = request.iterationRunMode
        options = request.iterationOptions
        stdoutHandle = runMode.runStdout
        stderrHandle = runMode.runStderr
        background = runMode.runInBackground
        initialCwd = bootstrap.iterationInitialSource
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
    stdinControl <- newStdinControl
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
    queuedInputDisplays <-
        queuedFullscreenInputDisplays request.iterationFullscreenInputs
    let fullscreenEnabled =
            stdinTty
                && stdoutTty
                && not (isOneShot options)
                && options.optScreenMode /= ScreenMinimal
        initialFullscreenState =
            (reduceUi
                (UiSetNotice
                    (Just (progressNotice "Opening session storage…")))
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
        if isJust request.iterationActiveFullscreen || not fullscreenEnabled
            then newMVar ()
            else newEmptyMVar
    fullscreen <- case request.iterationActiveFullscreen of
        Just runtime -> pure (Just runtime)
        Nothing
            | fullscreenEnabled ->
                Just <$> newFullscreenRuntimeWithTheme
                    bootstrap.iterationConfiguredTheme
                    request.iterationFullscreenInputs
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
                            bootstrap.iterationStartedAt
                            bootstrap.iterationStartupTimings
                            "first frame"
                        void (tryPutMVar firstFrameReady ()))
                    (writeIORef
                        bootstrap.iterationSyntaxLoadDuration . Just)
                    options.optMotionMode
                    useColor
                    initialFullscreenState
            | otherwise -> pure Nothing
    -- A reused fullscreen must not retain its prior transcript while resumed
    -- history is revalidated and installed for this preparation snapshot.
    forM_ fullscreen clearFullscreenHistorySource
    writeIORef uiRuntimeRef fullscreen
    let installToolRuntime toolEnv = do
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
                    (const (pure ()))
                    (pure ())
                    (\level ->
                        readIORef restartEffortActionRef >>= ($ level))
                    (noteFullscreenCtrlC interrupt)
                    (readIORef agentSnapshotRef >>= id)
                    (\target -> readIORef agentSelectRef >>= ($ target))
        buildStartupRuntime databaseStore toolEnv = StartupRuntime
            { startupToolEnv = toolEnv
            , startupHarnessConfig = bootstrap.iterationHarnessConfig
            , startupNetworkRecovery =
                request.iterationProcessRuntime.processNetworkRecovery
            , startupDatabaseStore = databaseStore
            , startupInterrupt = interrupt
            , startupStdinControl = stdinControl
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
            , startupFullscreenReused =
                isJust request.iterationActiveFullscreen
            , startupAgentSnapshot = agentSnapshotRef
            , startupAgentSelect = agentSelectRef
            , startupRestartEffort = restartEffortActionRef
            , startupStartedAt = bootstrap.iterationStartedAt
            , startupTimings = bootstrap.iterationStartupTimings
            , startupSyntaxLoadDuration =
                bootstrap.iterationSyntaxLoadDuration
            , startupFinished = bootstrap.iterationStartupFinished
            , startupSessionState = request.iterationSessionState
            , startupNativeHooks = runMode.runNativeHooks
            }
    pure AgentIterationInterface
        { iterationFullscreen = fullscreen
        , iterationFirstFrameReady = firstFrameReady
        , iterationTerminal = terminal
        , iterationUiRuntimeRef = uiRuntimeRef
        , iterationCancelToolRef = cancelToolRef
        , iterationInstallToolRuntime = installToolRuntime
        , iterationBuildStartupRuntime = buildStartupRuntime
        }

prepareAgentIterationAction
    :: AgentIterationRequest
    -> AgentIterationResources
    -> AgentIterationInterface
    -> Maybe SessionLock
    -> IO RunResult
prepareAgentIterationAction request resources interface resumeLock
    | request.iterationOptions.optWorktree
    , isNothing resources.iterationResumed
    , isNothing request.iterationTransition = do
        let options = request.iterationOptions
            prepareAccountUsage =
                options.optYolo == Explicit True
                    || isNothing interface.iterationFullscreen
                    || isJust options.optProvider
                    || isJust options.optModel
        withPreparedStartupAuth
            prepareAccountUsage
            options.optProvider
            (runAction . Just)
    | otherwise = runAction Nothing
  where
    runAction =
        runPreparedAgentIteration
            request
            resources
            interface
            resumeLock

runPreparedAgentIteration
    :: AgentIterationRequest
    -> AgentIterationResources
    -> AgentIterationInterface
    -> Maybe SessionLock
    -> Maybe PreparedStartupAuthWorker
    -> IO RunResult
runPreparedAgentIteration
        request resources interface resumeLock preparedAuth = do
    let runMode = request.iterationRunMode
    cwd <-
        resolveAgentIterationCwd
            request
            resources
            interface
            resumeLock
    unless runMode.runInBackground (setCurrentDirectory cwd)
    terminalCwd <- decodeFS cwd
    reportTerminalCwd
        interface.iterationTerminal
        runMode.runStdout
        terminalCwd
    freshToolEnv <- defaultToolEnv cwd
    let toolEnv = freshToolEnv
            { toolResourceArbiter =
                request.iterationProcessRuntime.processToolResourceArbiter
            }
    interface.iterationInstallToolRuntime toolEnv
    let startup =
            interface.iterationBuildStartupRuntime
                resources.iterationDatabaseStore
                toolEnv
    runAgentInitialized
        (runAgentWithRuntime request.iterationProcessRuntime)
        request.iterationProcessRuntime
        request.iterationOptions
        request.iterationTransition
        resources.iterationBootstrap.iterationHome
        resources.iterationBootstrap.iterationRoot
        resources.iterationResumed
        resumeLock
        cwd
        startup
        resources.iterationConnectedGateway
        preparedAuth

resolveAgentIterationCwd
    :: AgentIterationRequest
    -> AgentIterationResources
    -> AgentIterationInterface
    -> Maybe SessionLock
    -> IO OsPath
resolveAgentIterationCwd request resources interface resumeLock =
    case resources.iterationResumed of
        Just _ -> pure resources.iterationSource
        Nothing
            | request.iterationOptions.optWorktree -> do
                readMVar interface.iterationFirstFrameReady
                createManagedWorktreeFromConfigWithProgress
                    reportWorktreeProgress
                    resources.iterationBootstrap.iterationHarnessConfig
                    resources.iterationBootstrap.iterationHome
                    resources.iterationSource
                    >>= either worktreeFailed worktreeCreated
            | otherwise -> pure resources.iterationSource
  where
    stderrHandle = request.iterationRunMode.runStderr
    fullscreen = interface.iterationFullscreen
    reportWorktreeProgress progress = do
        let message = worktreeProgressMessage progress
        case fullscreen of
            Nothing -> putTextLn stderrHandle message
            Just runtime ->
                emitUiEvent runtime
                    (UiSetNotice (Just (progressNotice message)))
    worktreeFailed err = do
        mapM_ releaseSessionLock resumeLock
        case fullscreen of
            Nothing -> die (Text.unpack err)
            Just _ -> throwIO (StartupFailure err)
    worktreeCreated path = do
        color <- resolveColor stderrHandle
        case fullscreen of
            Nothing ->
                putTextLn stderrHandle
                    (roleMuted color
                        (glyphSession <> "worktree: " <> toText path))
            Just runtime ->
                emitUiEvent runtime
                    (UiSystemMessage
                        (glyphSession <> "worktree: " <> toText path))
        setStartupNotice fullscreen "Loading project…"
        pure path

cleanupAgentIteration
    :: AgentIterationRequest
    -> AgentIterationInterface
    -> IO ()
cleanupAgentIteration request interface =
    resetInterface `finally`
        releasePreparationResources
            request.iterationResumeLockRef
            request.iterationDatabaseStoreRef
  where
    resetInterface = do
        let runMode = request.iterationRunMode
        forM_ runMode.runNativeHooks \hooks ->
            hooks.nativeRegisterCancel (pure ())
        writeIORef interface.iterationUiRuntimeRef Nothing
        writeIORef interface.iterationCancelToolRef (pure ())
        forM_
            interface.iterationFullscreen
            resetFullscreenSessionActions

releasePreparationResources
    :: IORef (Maybe SessionLock)
    -> IORef (Maybe Store)
    -> IO ()
releasePreparationResources resumeLockRef databaseStoreRef = do
    -- Session storage may already have released a successfully resumed lock.
    -- filelock guards unlockFile with an atomic alive flag, so this also covers
    -- cancellation before that ownership handoff without unlocking twice.
    atomicModifyIORef' resumeLockRef (\current -> (Nothing, current))
        >>= mapM_ releaseSessionLock
    atomicModifyIORef' databaseStoreRef (\current -> (Nothing, current))
        >>= mapM_ closeStore

-- | Transfer a freshly acquired resource into its initially empty tracking
-- slot without leaking it if fullscreen cancellation lands during handoff.
-- Once the callback returns, the enclosing iteration's finalizer owns it.
trackPreparationResource
    :: IORef (Maybe a)
    -> (a -> IO ())
    -> IO (Either e a)
    -> IO (Either e a)
trackPreparationResource ref release acquire =
    bracketOnError
        acquire
        (mapM_ \resource -> do
            writeIORef ref Nothing
            release resource)
        \result -> do
            forM_ result (writeIORef ref . Just)
            pure result

resetFullscreenSessionActions :: FullscreenRuntime -> IO ()
resetFullscreenSessionActions runtime =
    setFullscreenSessionActions
        runtime
        Nothing
        (pure ())
        (\_ _ -> pure (Right ()))
        (const (pure ()))
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
