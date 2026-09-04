module Agent.CLI.Runtime.Orchestration.Initialized
    ( PreparedStartupAuthWorker
    , runAgentInitialized
    , withPreparedStartupAuth
    ) where

import Agent.CLI.AccountPicker ()
import Agent.CLI.AccountSelection
    ( PreparedProviderAccounts,
      SelectedAccount(..),
      loadedAuthSupportsUsageAccountSelection,
      prepareProviderAccounts,
      selectPreparedProviderAccount,
      selectProviderAccount )
import Agent.CLI.Afk ()
import Agent.CLI.AgentSessions ()
import Agent.CLI.AgentViewport ()
import Agent.CLI.Approval ()
import Agent.CLI.Artifact ()
import Agent.CLI.Auth
    ( LoadedAuth(loadedAccountLabel, LoadedAuth, loadedOpenAiPool,
                 loadedProvider, loadedTokenProvider, loadedSelectionId),
      gatewayAuthSelectionId,
      gatewayLoadedAuthForProvider,
      gatewayRouterTokenProvider,
      isGatewayLoadedAuth,
      preferredOpenAiTokenProvider,
      loadAuth,
      loadAuthForAccount,
      probeLoadedAuthCredential,
      staticCredentialProvider )
import Agent.CLI.Clipboard ()
import Agent.CLI.CodeModeRuntime ()
import Agent.CLI.Command ()
import Agent.CLI.Compaction ()
import Agent.CLI.Config ()
import Agent.Connectivity ()
import Agent.CLI.Database ()
import Agent.CLI.Database.Store
    ( DatabaseScopes
    , deriveDatabaseScopesWithNamespace
    )
import Agent.CLI.Dialects ()
import Agent.CLI.Error ()
import Agent.CLI.GatewayClient
    ( GatewayCredential
    , GatewayModelAccess
    , gatewayCredentialIdentity
    , newGatewayModelAccess
    , refreshGatewayModels
    )
import Agent.CLI.GatewayModels (gatewayProviderForStartup)
import Agent.CLI.GatewayBridge ()
import Agent.CLI.Input ()
import Agent.CLI.Interrupt ()
import Agent.CLI.LearnedSkills ()
import Agent.CLI.LearnedSkills.Store ()
import Agent.CLI.Login ()
import Agent.CLI.Lsp ()
import Agent.CLI.ManagedTurn ()
import Agent.CLI.McpManager ()
import Agent.CLI.McpStatus ()
import Agent.CLI.ModelConfig
    ( ModelCatalog
    , catalogConnection,
      loadModelCatalogAt,
      organizationGatewayConnectionId,
      ConnectionKind(BuiltinConnection, CustomResponsesConnection,
                     OrganizationGatewayConnection),
      ModelConnection(connectionId, connectionKind),
      ResponsesConnection(responsesApiKeyEnv, responsesApiKeyOptional) )
import Agent.CLI.Models
    ( resolveConfiguredModel,
      resolveSavedModelTarget,
      validateResumedGatewayBoundary,
      ModelOption(modelTarget),
      ModelTarget(targetWireModelId, targetConnectionId, targetProvider,
                  targetModelId, targetDialect) )
import Agent.CLI.Options ( CliOptions(optModel, optProvider, optYolo) )
import Agent.CLI.PendingInputs ()
import Agent.CLI.Plan ()
import Agent.CLI.Project
    ( loadProjectSettings,
      loadUserSettings,
      projectAccountFor,
      projectModelProvider,
      resolveProjectRoot,
      withInheritedLastModel,
      ProjectAccount(projectAccountId, projectAccountSelectionId),
      ProjectModel(projectModelTarget),
      ProjectSettings(settingsLastModel) )
import Agent.CLI.Prompt ()
import Agent.CLI.PromptHooks ()
import Agent.CLI.Provider.OpenAI ()
import Agent.CLI.Provider.Switch ( loadSelectedAccountAuth )
import Agent.CLI.ProviderAvailability ()
import Agent.CLI.ProviderFallback
    ( allowsAutomaticBillingFallback )
import Agent.CLI.ProviderTransition
    ( ProviderTransition(transitionAutomaticBilling,
                         transitionUnavailableProviders, transitionPendingTurn,
                         transitionTarget, transitionAccountSelectionId,
                         transitionAccountId) )
import Agent.CLI.Recap ()
import Agent.CLI.Render ()
import Agent.CLI.ReplMode ()
import Agent.CLI.Request ()
import Agent.CLI.Resume ( publishResumeHistoryAfterBoundary )
import Agent.CLI.Runtime.HistorySource
    ( loadFullscreenHistoryPage, sessionUiPageSize )
import Agent.CLI.Runtime.Orchestration.Background ()
import Agent.CLI.Runtime.Orchestration.Concurrent ()
import Agent.CLI.Runtime.Orchestration.Restart ()
import Agent.CLI.Runtime.Orchestration.Startup
    ( setStartupRepository )
import Agent.CLI.Runtime.Orchestration.Tools ( runAgentTools )
import Agent.CLI.Runtime.Orchestration.Types
    ( ActiveHttpAuth(activeHttpGeneration, ActiveHttpAuth,
                     activeHttpAccountId, activeHttpProvider, activeHttpResolveLabel),
      AgentProcessRuntime(processMcpSupervisor),
      AgentRunMode,
      NativeDiscoveryContext(..),
      NativeRunHooks(nativeDatabaseScopeNamespace, nativeWorkspaceDiscovery),
      nativePreparedDiscovery )
import Agent.CLI.Runtime.Persistence ()
import Agent.CLI.Runtime.Recap ()
import Agent.CLI.Runtime.Repl ()
import Agent.CLI.Runtime.Types ( DevResult, RunResult )
import Agent.CLI.Secret ()
import Agent.CLI.Session
    ( loadRecentSessionTurns,
      SessionMeta(metaId, metaProvider, metaConnection, metaModel,
                  metaTransportModel, metaDialect, metaGatewayIdentity),
      SessionTurn )
import Agent.CLI.Session.Attachments ()
import Agent.CLI.Session.Choices ()
import Agent.CLI.Session.History ( detectGitBranch )
import Agent.CLI.Session.Lifecycle ()
import Agent.CLI.Session.Runtime.Types
    ( StartupRuntime(startupToolEnv, startupStderr, startupStdout,
                     startupStdoutTty, startupStdinTty, startupFullscreen,
                     startupUiRuntimeRef, startupEscPaused, startupInterrupt,
                     startupDatabaseStore, startupNativeHooks) )
import Agent.CLI.Session.Selection ()
import Agent.CLI.SessionAdmin ()
import Agent.CLI.SessionEnv ()
import Agent.CLI.SessionLock ( releaseSessionLock, SessionLock )
import Agent.CLI.SessionState ()
import Agent.CLI.SessionTitle ()
import Agent.CLI.Skills ()
import Agent.CLI.Startup.Auth
    ( loadStartupAuth, loadStartupAuthFromResult, markStartupStage, startupDie )
import Agent.CLI.StartupContext ()
import Agent.CLI.Style ( setCliWindowTitle )
import Agent.CLI.Subagents.Runtime ()
import Agent.CLI.TUI.App
    ( setFullscreenHistorySource, setFullscreenWindowTitle )
import Agent.CLI.TUI.History
    ( HistoryDirection(HistoryNewer), HistoryGeneration(..) )
import Agent.CLI.TUI.SessionHistory ( sessionHistoryPage )
import Agent.CLI.Terminal ()
import Agent.CLI.Tools ()
import Agent.CLI.Turn ()
import Agent.CLI.Usage ()
import Agent.CLI.WebFetch ()
import Agent.CLI.Worktree ()
import Agent.Cancel ()
import Agent.Claude ()
import Agent.Dialect ()
import Agent.Error ( ApiError(..) )
import Agent.GrokBuild.Dialect.Goal ()
import Agent.GrokBuild.Dialect.Runtime ()
import Agent.GrokBuild.Dialect.Task ()
import Agent.GrokBuild.Dialect.Workflow ()
import Agent.Loop ()
import Agent.OpenAI.Compaction ()
import Agent.OpenAI.Usage ()
import Agent.OpenAI.WebSocketClient ()
import Agent.OpenRouter.LoopBackend ()
import Agent.OsPath ()
import Agent.Provider
    ( Provider(OpenAIProvider, XAIProvider, OpenRouterProvider,
               GeminiProvider, ClaudeCodeProvider),
      TokenProvider,
      Credential(..),
      getNextToken,
      providerSlug,
      tokenProviderBillingMode,
      tokenProviderWithNextToken,
      BillingMode(ApiBilled),
      FailedCredential(credential) )
import Agent.ReasoningEffort ()
import Agent.Responses.GenericBackend ()
import Agent.Responses.GenericClient ()
import Agent.Responses.Types ()
import Agent.Skills ()
import Agent.Store.Postgres ( trustedPool )
import Agent.Store.Types ()
import Agent.Subagents ()
import Agent.Subagents.TaskPath ()
import Agent.TUI.Model ()
import Agent.TUI.Motion ()
import Agent.Tools.MultiAgents ()
import Agent.Tools.PlanMode ()
import Agent.Tools.Secret ()
import Agent.Tools.Types ()
import Agent.XAI.LoopBackend ()
import Control.Applicative ( (<|>) )
import Control.Concurrent.Async
    ( Async, cancel, concurrently, poll, wait, withAsync )
import Control.Concurrent.Chan ()
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar_
    , newEmptyMVar
    , newMVar
    , readMVar
    , takeMVar
    , tryPutMVar
    )
import Control.Concurrent.STM ()
import Control.Exception ()
import Control.Exception.Safe ( onException )
import Control.Monad ( forM_, void, when )
import Data.Functor ()
import Data.IORef ( IORef, newIORef, readIORef, writeIORef )
import Data.List ()
import Data.Maybe ( isNothing, fromMaybe, isJust )
import Data.Text ( Text )
import Data.Time.Clock ()
import System.Console.ANSI ()
import System.Console.ANSI.Codes ()
import System.Directory.OsPath ()
import System.Environment ( lookupEnv )
import System.Exit ()
import System.IO ()
import System.OsPath ( OsPath, (</>), decodeFS, unsafeEncodeUtf )
import qualified Data.ByteString as BS ()
import qualified Agent.Responses.GenericClient as GenericResponses
    ()
import qualified Agent.MCP as MCP ()
import qualified Data.Map.Strict as Map ()
import qualified Agent.OpenAI.Auth as OpenAI ()
import qualified Agent.OpenRouter as OpenRouter ()
import qualified Agent.OpenRouter.Usage as OpenRouterUsage ()
import qualified Agent.Provider as Provider ( tokenProvider )
import qualified Agent.CLI.Session.Lifecycle as SessionLifecycle ()
import qualified Agent.CLI.Session.Runner as SessionRunner ()
import qualified Data.Set as Set ( empty )
import qualified Data.Text as Text ( null, pack, unpack )
import qualified Data.Text.IO as Text ()
import qualified Agent.XAI.Options as XAI ()
import qualified Agent.XAI.Client as XAIClient ()
import qualified Agent.XAI.Request as XAIRequest ()
import qualified Agent.XAI.Usage as XAIUsage ()

data PreparedStartupAuth = PreparedStartupAuth
    { preparedAuthResult :: !(Either Text LoadedAuth)
    , preparedAccountUsage :: !(Maybe PreparedProviderAccounts)
    }

data PreparedStartupAuthWorker = PreparedStartupAuthWorker
    { preparedStartupAsync :: !(Async PreparedStartupAuth)
    , retirePreparedStartup :: !(IO ())
    }

data InitializedRequest = InitializedRequest
    { initializedRunAgentChild :: AgentRunMode -> CliOptions -> IO DevResult
    , initializedProcessRuntime :: AgentProcessRuntime
    , initializedOptions :: CliOptions
    , initializedTransition :: Maybe ProviderTransition
    , initializedHome :: OsPath
    , initializedRoot :: OsPath
    , initializedResumed :: Maybe (SessionMeta, [SessionTurn])
    , initializedResumeLock :: Maybe SessionLock
    , initializedCwd :: OsPath
    , initializedStartup :: StartupRuntime
    , initializedConnectedGateway :: Maybe GatewayCredential
    , initializedPreparedAuth :: Maybe PreparedStartupAuthWorker
    }

data InitializedWorkspace = InitializedWorkspace
    { initializedProjectRoot :: OsPath
    , initializedStateDirectory :: FilePath
    , initializedDatabaseScopes :: DatabaseScopes
    , initializedProjectSettings :: ProjectSettings
    , initializedCatalog :: ModelCatalog
    }

data InitializedTargets = InitializedTargets
    { initializedGatewayIdentity :: Maybe Text
    , initializedTransitionTarget :: Maybe ModelTarget
    , initializedConfiguredTarget :: Maybe ModelTarget
    , initializedResumedTarget :: Maybe ModelTarget
    , initializedProjectTarget :: Maybe ModelTarget
    , initializedTargetHint :: Maybe ModelTarget
    , initializedRequestedProvider :: Maybe Provider
    , initializedCustomResponses :: Maybe (Text, ResponsesConnection)
    , initializedCheckStartupUsageInBackground :: Bool
    }

data RoutedStartupAuth = RoutedStartupAuth
    { routedInitialLoaded :: LoadedAuth
    , routedLearnAboutUserRequested :: Bool
    , routedPreparedAccountUsage :: Maybe PreparedProviderAccounts
    , routedCustomBearerToken :: Maybe Text
    }

data InitializedAuth = InitializedAuth
    { initializedLoaded :: LoadedAuth
    , initializedLearnAboutUserRequested :: Bool
    , initializedPreparedAccountUsage :: Maybe PreparedProviderAccounts
    , initializedCustomBearerToken :: Maybe Text
    , initializedStartupAccountIds :: Maybe (Text, Text)
    }

data InitializedAccountRefs = InitializedAccountRefs
    { initializedGatewayModelsRef :: IORef (Maybe GatewayModelAccess)
    , initializedActiveAccountIdRef :: IORef Text
    , initializedActiveAccountRef :: IORef Text
    , initializedActiveSelectionRef :: IORef Text
    , initializedPreferredOpenAiAccountRef :: IORef (Maybe Text)
    , initializedSelectableTokenProvider :: TokenProvider
    }

data InitializedHttpRuntime = InitializedHttpRuntime
    { initializedResolveActiveAccountLabel :: Credential -> IO Text
    , initializedTokenProvider :: TokenProvider
    , initializedSelectHttpAccount ::
        Text -> IO (Either ApiError Text)
    }

prepareStartupAuth :: Bool -> Maybe Provider -> IO PreparedStartupAuth
prepareStartupAuth prepareAccountUsage requestedProvider = do
    authResult <- loadAuth requestedProvider
    accountUsage <- case authResult of
        Right loaded
            | prepareAccountUsage
            , loadedAuthSupportsUsageAccountSelection loaded ->
                Just <$> prepareProviderAccounts loaded.loadedProvider Nothing
        _ -> pure Nothing
    pure PreparedStartupAuth
        { preparedAuthResult = authResult
        , preparedAccountUsage = accountUsage
        }

withPreparedStartupAuth
    :: Bool
    -> Maybe Provider
    -> (PreparedStartupAuthWorker -> IO a)
    -> IO a
withPreparedStartupAuth prepareAccountUsage requestedProvider action =
    withAsync
        (prepareStartupAuth prepareAccountUsage requestedProvider)
        \worker -> do
            retire <- newEmptyMVar
            withAsync
                (takeMVar retire >> cancel worker)
                \_ ->
                    action PreparedStartupAuthWorker
                        { preparedStartupAsync = worker
                        , retirePreparedStartup =
                            void (tryPutMVar retire ())
                        }

runAgentInitialized
    :: (AgentRunMode -> CliOptions -> IO DevResult)
    -> AgentProcessRuntime
    -> CliOptions
    -> Maybe ProviderTransition
    -> OsPath
    -> OsPath
    -> Maybe (SessionMeta, [SessionTurn])
    -> Maybe SessionLock
    -> OsPath
    -> StartupRuntime
    -> Maybe GatewayCredential
    -> Maybe PreparedStartupAuthWorker
    -> IO RunResult
runAgentInitialized
        runAgentChild
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
        preparedAuth =
    runAgentInitializedWithLock
        runAgentChild
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
        `onException` mapM_ releaseSessionLock resumeLock

runAgentInitializedWithLock
    :: (AgentRunMode -> CliOptions -> IO DevResult)
    -> AgentProcessRuntime
    -> CliOptions
    -> Maybe ProviderTransition
    -> OsPath
    -> OsPath
    -> Maybe (SessionMeta, [SessionTurn])
    -> Maybe SessionLock
    -> OsPath
    -> StartupRuntime
    -> Maybe GatewayCredential
    -> Maybe PreparedStartupAuthWorker
    -> IO RunResult
runAgentInitializedWithLock
        runAgentChild processRuntime
        options transition home root resumed resumeLock cwd startup
        connectedGateway preparedAuth =
    runInitialized InitializedRequest
        { initializedRunAgentChild = runAgentChild
        , initializedProcessRuntime = processRuntime
        , initializedOptions = options
        , initializedTransition = transition
        , initializedHome = home
        , initializedRoot = root
        , initializedResumed = resumed
        , initializedResumeLock = resumeLock
        , initializedCwd = cwd
        , initializedStartup = startup
        , initializedConnectedGateway = connectedGateway
        , initializedPreparedAuth = preparedAuth
        }

prepareInitializedWorkspace
    :: InitializedRequest
    -> IO InitializedWorkspace
prepareInitializedWorkspace request = do
    let startup = request.initializedStartup
        home = request.initializedHome
        cwd = request.initializedCwd
        fullscreen = startup.startupFullscreen
        preparedDiscovery =
            nativePreparedDiscovery . (.nativeWorkspaceDiscovery)
                =<< startup.startupNativeHooks
    projectRoot <-
        maybe
            (resolveProjectRoot cwd)
            (pure . (.nativeDiscoveryProjectRoot))
            preparedDiscovery
    stateDirectory <- decodeFS (home </> unsafeEncodeUtf ".haskell-agent")
    projectRootPath <- decodeFS projectRoot
    let scopeNamespace =
            startup.startupNativeHooks >>= (.nativeDatabaseScopeNamespace)
    databaseScopes <-
        deriveDatabaseScopesWithNamespace
            scopeNamespace
            stateDirectory
            projectRootPath >>= \case
            Left err -> startupDie startup (Text.unpack err)
            Right scopes -> pure scopes
    ((projectSettings0, userSettings), (catalogResult, branch)) <-
        concurrently
            (concurrently
                (maybe
                    (loadProjectSettings projectRoot)
                    (pure . (.nativeDiscoveryProjectSettings))
                    preparedDiscovery)
                (loadUserSettings home))
            (concurrently
                (loadModelCatalogAt home
                    (maybe
                        cwd
                        (.nativeDiscoveryCatalogRoot)
                        preparedDiscovery))
                (maybe
                    (detectGitBranch cwd)
                    (pure . (.nativeDiscoveryGitBranch))
                    preparedDiscovery))
    let projectSettings =
            withInheritedLastModel projectSettings0 userSettings
    catalog <- either
        (startupDie startup . Text.unpack)
        pure
        catalogResult
    setStartupRepository fullscreen home branch cwd
    markStartupStage startup "Loading credentials…"
    pure InitializedWorkspace
        { initializedProjectRoot = projectRoot
        , initializedStateDirectory = stateDirectory
        , initializedDatabaseScopes = databaseScopes
        , initializedProjectSettings = projectSettings
        , initializedCatalog = catalog
        }

resolveInitializedTargets
    :: InitializedRequest
    -> InitializedWorkspace
    -> IO InitializedTargets
resolveInitializedTargets request workspace = do
    let startup = request.initializedStartup
        options = request.initializedOptions
        transition = request.initializedTransition
        resumed = request.initializedResumed
        connectedGateway = request.initializedConnectedGateway
        fullscreen = startup.startupFullscreen
        catalog = workspace.initializedCatalog
        projectSettings = workspace.initializedProjectSettings
        connectedGatewayIdentity =
            gatewayCredentialIdentity <$> connectedGateway
        transitionTarget = (.transitionTarget) <$> transition
        configuredOptionTarget =
            (.modelTarget)
                <$> (options.optModel >>= resolveConfiguredModel catalog)
        resumedBoundaryResult =
            case fst <$> resumed of
                Nothing -> Right ()
                Just meta ->
                    validateResumedGatewayBoundary
                        connectedGatewayIdentity
                        meta.metaConnection
                        meta.metaGatewayIdentity
        savedTarget =
            resolveSavedModelTarget catalog False
        resumedTargetResult
            | isJust transitionTarget || isJust options.optModel =
                Right Nothing
            | otherwise = case fst <$> resumed of
                Nothing -> Right Nothing
                Just meta
                    | isNothing connectedGateway
                    , meta.metaConnection
                        == organizationGatewayConnectionId ->
                        Right $
                            case resolveConfiguredModel catalog meta.metaModel of
                                Just option
                                    | option.modelTarget.targetConnectionId
                                        /= organizationGatewayConnectionId ->
                                        Just option.modelTarget
                                _ -> Nothing
                Just meta ->
                    Just <$> resolveSavedModelTarget
                        catalog
                        (isJust connectedGateway)
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
                    in if isJust connectedGateway
                    then Right (Just target)
                    else if target.targetConnectionId
                        == organizationGatewayConnectionId
                    then Right Nothing
                    else
                        Just <$> savedTarget
                            target.targetProvider
                            target.targetConnectionId
                            target.targetModelId
                            (Just target.targetWireModelId)
                            target.targetDialect
    resumedHistoryResult <-
        publishResumeHistoryAfterBoundary resumedBoundaryResult $
            forM_ fullscreen \runtime ->
                forM_ resumed \(meta, _) ->
                    loadRecentSessionTurns
                        (trustedPool startup.startupDatabaseStore)
                        request.initializedRoot
                        meta.metaId
                        sessionUiPageSize >>= \case
                            Left err ->
                                startupDie startup (Text.unpack err)
                            Right page ->
                                setFullscreenHistorySource
                                    runtime
                                    meta.metaId
                                    (loadFullscreenHistoryPage
                                        (trustedPool
                                            startup.startupDatabaseStore)
                                        request.initializedRoot
                                        meta.metaId)
                                    (sessionHistoryPage
                                        (HistoryGeneration 0)
                                        HistoryNewer
                                        page)
    either (startupDie startup . Text.unpack) pure resumedHistoryResult
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
        requestedProvider
            | isJust connectedGateway =
                Just $
                    gatewayProviderForStartup
                        targetHint
                        options.optProvider
                        ((.metaProvider) . fst <$> resumed)
            | otherwise =
                (.targetProvider) <$> targetHint
                    <|> options.optProvider
                    <|> ((.metaProvider) . fst <$> resumed)
                    <|> if isNothing options.optModel
                        then projectModelProvider projectSettings
                        else Nothing
        targetConnection =
            targetHint >>= catalogConnection catalog . (.targetConnectionId)
        customResponses
            | isJust connectedGateway = Nothing
            | otherwise =
                targetConnection >>= \connection ->
                    case connection.connectionKind of
                        CustomResponsesConnection responses -> Just
                            (connection.connectionId, responses)
                        BuiltinConnection _ -> Nothing
                        OrganizationGatewayConnection -> Nothing
        checkStartupUsageInBackground =
            isNothing connectedGateway
                && isJust fullscreen
                && isNothing transition
                && isNothing resumed
                && isNothing options.optProvider
                && isNothing options.optModel
    pure InitializedTargets
        { initializedGatewayIdentity = connectedGatewayIdentity
        , initializedTransitionTarget = transitionTarget
        , initializedConfiguredTarget = configuredOptionTarget
        , initializedResumedTarget = resumedTarget
        , initializedProjectTarget = projectTarget
        , initializedTargetHint = targetHint
        , initializedRequestedProvider = requestedProvider
        , initializedCustomResponses = customResponses
        , initializedCheckStartupUsageInBackground =
            checkStartupUsageInBackground
        }

loadInitializedAuth
    :: InitializedRequest
    -> InitializedTargets
    -> IO RoutedStartupAuth
loadInitializedAuth request targets =
    case
        ( request.initializedConnectedGateway
        , targets.initializedCustomResponses
        )
    of
        (Just gateway, Nothing) -> do
            mapM_
                (.retirePreparedStartup)
                request.initializedPreparedAuth
            exactLoaded <-
                either
                    (startupDie startup . Text.unpack)
                    pure
                    (gatewayLoadedAuthForProvider requestedProvider gateway)
            pure RoutedStartupAuth
                { routedInitialLoaded = exactLoaded
                , routedLearnAboutUserRequested = False
                , routedPreparedAccountUsage = Nothing
                , routedCustomBearerToken = Nothing
                }
        (Nothing, Nothing) -> do
            (startupAuth, accountUsage) <- loadPreparedOrStartupAuth
                request.initializedPreparedAuth
                (options.optYolo
                    && targets.initializedCheckStartupUsageInBackground)
                startup
                request.initializedTransition
                requestedProvider
            pure RoutedStartupAuth
                { routedInitialLoaded = fst startupAuth
                , routedLearnAboutUserRequested = snd startupAuth
                , routedPreparedAccountUsage = accountUsage
                , routedCustomBearerToken = Nothing
                }
        (Nothing, Just (connectionId, responses)) -> do
            (loaded, bearerToken) <-
                loadCustomResponsesAuth startup connectionId responses
            pure RoutedStartupAuth
                { routedInitialLoaded = loaded
                , routedLearnAboutUserRequested = False
                , routedPreparedAccountUsage = Nothing
                , routedCustomBearerToken = bearerToken
                }
        (Just _, Just _) ->
            startupDie startup
                "gateway and custom connection routing cannot both be active"
  where
    startup = request.initializedStartup
    options = request.initializedOptions
    requestedProvider = targets.initializedRequestedProvider

loadCustomResponsesAuth
    :: StartupRuntime
    -> Text
    -> ResponsesConnection
    -> IO (LoadedAuth, Maybe Text)
loadCustomResponsesAuth startup connectionId responses = do
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
        loaded = LoadedAuth
            { loadedProvider = OpenRouterProvider
            , loadedTokenProvider =
                staticCredentialProvider ApiBilled credential
            , loadedAccountLabel = const (pure connectionId)
            , loadedSelectionId = Nothing
            , loadedOpenAiPool = Nothing
            }
    pure
        ( loaded
        , if Text.null token then Nothing else Just token
        )

selectInitializedStartupAccount
    :: InitializedRequest
    -> InitializedWorkspace
    -> InitializedTargets
    -> RoutedStartupAuth
    -> IO InitializedAuth
selectInitializedStartupAccount request workspace targets routed = do
    (loaded, startupAccountIds) <-
        case targets.initializedCustomResponses of
            Just _ -> pure (initialLoaded, Nothing)
            Nothing
                | not (isGatewayLoadedAuth initialLoaded)
                , Just active <- request.initializedTransition
                , Just selectionId <-
                    active.transitionAccountSelectionId ->
                    pure
                        ( initialLoaded
                        , Just
                            ( selectionId
                            , fromMaybe
                                selectionId
                                active.transitionAccountId
                            )
                        )
                | not
                    (loadedAuthSupportsUsageAccountSelection initialLoaded) ->
                        pure (initialLoaded, Nothing)
                | targets.initializedCheckStartupUsageInBackground
                , isNothing preparedAccountUsage ->
                    selectRememberedAccount initialLoaded
                | otherwise ->
                    selectUsableAccount initialLoaded
    pure InitializedAuth
        { initializedLoaded = loaded
        , initializedLearnAboutUserRequested =
            routed.routedLearnAboutUserRequested
        , initializedPreparedAccountUsage = preparedAccountUsage
        , initializedCustomBearerToken =
            routed.routedCustomBearerToken
        , initializedStartupAccountIds = startupAccountIds
        }
  where
    initialLoaded = routed.routedInitialLoaded
    preparedAccountUsage = routed.routedPreparedAccountUsage
    projectSettings = workspace.initializedProjectSettings
    startup = request.initializedStartup

    selectRememberedAccount loaded = do
        -- Make the remembered model/account usable immediately. The scoped
        -- availability worker later checks the pool and triggers startup
        -- fallback if every credential is exhausted.
        let provider = loaded.loadedProvider
        case projectAccountFor provider projectSettings of
            Nothing -> pure (loaded, Nothing)
            Just remembered ->
                loadSelectedAccountAuth
                    provider
                    remembered.projectAccountSelectionId
                    remembered.projectAccountId >>= \case
                        Left _ -> pure (loaded, Nothing)
                        Right selectedLoaded ->
                            pure
                                ( selectedLoaded
                                , Just
                                    ( remembered.projectAccountSelectionId
                                    , remembered.projectAccountId
                                    )
                                )

    selectUsableAccount loaded = do
        let provider = loaded.loadedProvider
            rememberedIds = fmap
                (\account ->
                    ( account.projectAccountSelectionId
                    , account.projectAccountId
                    ))
                (projectAccountFor provider projectSettings)
            selectStartupAccount = case preparedAccountUsage of
                Just accountUsage ->
                    pure $ selectPreparedProviderAccount
                        rememberedIds
                        accountUsage
                Nothing ->
                    selectProviderAccount
                        provider
                        Nothing
                        rememberedIds
        selectStartupAccount >>= \case
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

validateInitializedAuth
    :: InitializedRequest
    -> InitializedTargets
    -> LoadedAuth
    -> IO ()
validateInitializedAuth request targets loaded = do
    when
        ( isJust request.initializedConnectedGateway
            /= isGatewayLoadedAuth loaded
        ) $
        startupDie startup
            "gateway model routing and loaded credentials disagree; refusing \
            \to start with an unsafe transport configuration"
    case ( targets.initializedTransitionTarget
        , request.initializedResumed
        ) of
        (Just target, _)
            | not (isGatewayLoadedAuth loaded)
            , loaded.loadedProvider /= target.targetProvider ->
                startupDie startup $ "provider transition requested "
                    <> Text.unpack (providerSlug target.targetProvider)
                    <> " but auth resolved "
                    <> Text.unpack (providerSlug loaded.loadedProvider)
        _ -> pure ()
    case request.initializedTransition
        >>= (.transitionAutomaticBilling) of
        Just sourceBilling
            | not
                (allowsAutomaticBillingFallback
                    sourceBilling
                    (tokenProviderBillingMode loaded.loadedTokenProvider)) ->
                startupDie startup
                    "automatic provider fallback would cross from subscription \
                    \billing to API-credit billing"
        _ -> pure ()
  where
    startup = request.initializedStartup

newInitializedAccountRefs
    :: InitializedRequest
    -> InitializedAuth
    -> IO InitializedAccountRefs
newInitializedAccountRefs request auth = do
    let loaded = auth.initializedLoaded
        startupAccountIds = auth.initializedStartupAccountIds
    initialGatewayModels <-
        loadGatewayModelAccess
            request.initializedConnectedGateway
            loaded >>= either
                (startupDie request.initializedStartup . Text.unpack)
                pure
    gatewayModelsRef <- newIORef initialGatewayModels
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
    let unguardedSelectableTokenProvider =
            case loaded.loadedOpenAiPool of
                Just pool ->
                    preferredOpenAiTokenProvider
                        preferredOpenAiAccountRef
                        pool
                        loaded.loadedTokenProvider
                Nothing ->
                    loaded.loadedTokenProvider
        selectableTokenProvider
            | isGatewayLoadedAuth loaded =
                gatewayRouterTokenProvider
                    unguardedSelectableTokenProvider
            | otherwise =
                unguardedSelectableTokenProvider
    pure InitializedAccountRefs
        { initializedGatewayModelsRef = gatewayModelsRef
        , initializedActiveAccountIdRef = activeAccountIdRef
        , initializedActiveAccountRef = activeAccountRef
        , initializedActiveSelectionRef = activeSelectionRef
        , initializedPreferredOpenAiAccountRef =
            preferredOpenAiAccountRef
        , initializedSelectableTokenProvider =
            selectableTokenProvider
        }

initializeActiveHttpAuth
    :: InitializedTargets
    -> InitializedAuth
    -> InitializedAccountRefs
    -> IO (MVar ActiveHttpAuth)
initializeActiveHttpAuth targets auth refs = do
    initialHttp <- case targets.initializedCustomResponses of
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
                                GeminiProvider -> "Google Gemini"
                                ClaudeCodeProvider -> "Claude Code"
                            selectionId =
                                fromMaybe "" loaded.loadedSelectionId
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
    newMVar ActiveHttpAuth
        { activeHttpGeneration = 0
        , activeHttpProvider = initialHttpProvider
        , activeHttpResolveLabel = initialHttpResolver
        , activeHttpAccountId = initialHttpAccountId
        }
  where
    loaded = auth.initializedLoaded
    activeAccountRef = refs.initializedActiveAccountRef
    activeAccountIdRef = refs.initializedActiveAccountIdRef
    activeSelectionRef = refs.initializedActiveSelectionRef
    selectableTokenProvider =
        refs.initializedSelectableTokenProvider

makeSwitchableTokenProvider
    :: InitializedAccountRefs
    -> MVar ActiveHttpAuth
    -> TokenProvider
makeSwitchableTokenProvider refs activeHttpAuth =
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
                                        refs.initializedActiveAccountIdRef
                                        credential.accountId
                                    writeIORef
                                        refs.initializedActiveAccountRef
                                        label
                                    pure current
                                        { activeHttpAccountId =
                                            credential.accountId
                                        }
                                else pure current
                        pure (Right credential)
  where
    selectableTokenProvider =
        refs.initializedSelectableTokenProvider

resolveInitializedActiveAccountLabel
    :: LoadedAuth
    -> MVar ActiveHttpAuth
    -> Credential
    -> IO Text
resolveInitializedActiveAccountLabel loaded activeHttpAuth credential =
    case loaded.loadedProvider of
        OpenAIProvider ->
            loaded.loadedAccountLabel credential
        _ -> do
            active <- readMVar activeHttpAuth
            active.activeHttpResolveLabel credential

selectInitializedHttpAccount
    :: LoadedAuth
    -> InitializedAccountRefs
    -> MVar ActiveHttpAuth
    -> Text
    -> IO (Either ApiError Text)
selectInitializedHttpAccount
        loaded refs activeHttpAuth selectedSelectionId
    | isGatewayLoadedAuth loaded =
        pure $ Left $ CredentialError
            "Account switching is unavailable while connected to the organization gateway. Disconnect the gateway first."
    | otherwise =
        loadAuthForAccount loaded.loadedProvider selectedSelectionId
            >>= \case
                Left err ->
                    pure (Left (CredentialError err))
                Right selected
                    | selected.loadedProvider /= loaded.loadedProvider ->
                        pure $ Left $ CredentialError
                            "selected account belongs to a different provider"
                    | tokenProviderBillingMode
                        selected.loadedTokenProvider
                        /= tokenProviderBillingMode selectableTokenProvider ->
                        pure $ Left $ CredentialError
                            "selected account uses a different billing mode"
                    | otherwise ->
                        loadGatewayModelAccess Nothing selected >>= \case
                            Left err ->
                                pure (Left (CredentialError err))
                            Right selectedGatewayModels ->
                                probeLoadedAuthCredential selected >>= \case
                                    Left err -> pure (Left err)
                                    Right (credential, usable) -> do
                                        label <-
                                            usable.loadedAccountLabel credential
                                        when
                                            (loaded.loadedProvider
                                                == OpenAIProvider) $
                                            writeIORef
                                                preferredOpenAiAccountRef
                                                (if selectedSelectionId
                                                        == gatewayAuthSelectionId
                                                    then Nothing
                                                    else
                                                        Just
                                                            credential.accountId)
                                        let selectionId =
                                                fromMaybe
                                                    selectedSelectionId
                                                    usable.loadedSelectionId
                                        modifyMVar_ activeHttpAuth \current -> do
                                            writeIORef
                                                refs.initializedGatewayModelsRef
                                                selectedGatewayModels
                                            writeIORef
                                                refs.initializedActiveAccountIdRef
                                                credential.accountId
                                            writeIORef
                                                refs.initializedActiveSelectionRef
                                                selectionId
                                            writeIORef
                                                refs.initializedActiveAccountRef
                                                label
                                            pure ActiveHttpAuth
                                                { activeHttpGeneration =
                                                    current.activeHttpGeneration
                                                        + 1
                                                , activeHttpProvider =
                                                    usable.loadedTokenProvider
                                                , activeHttpResolveLabel =
                                                    usable.loadedAccountLabel
                                                , activeHttpAccountId =
                                                    credential.accountId
                                                }
                                        pure (Right label)
  where
    selectableTokenProvider =
        refs.initializedSelectableTokenProvider
    preferredOpenAiAccountRef =
        refs.initializedPreferredOpenAiAccountRef

newInitializedHttpRuntime
    :: InitializedAuth
    -> InitializedAccountRefs
    -> MVar ActiveHttpAuth
    -> InitializedHttpRuntime
newInitializedHttpRuntime auth refs activeHttpAuth =
    InitializedHttpRuntime
        { initializedResolveActiveAccountLabel =
            resolveActiveAccountLabel
        , initializedTokenProvider = tokenProvider
        , initializedSelectHttpAccount =
            selectInitializedHttpAccount loaded refs activeHttpAuth
        }
  where
    loaded = auth.initializedLoaded
    selectableTokenProvider =
        refs.initializedSelectableTokenProvider
    resolveActiveAccountLabel =
        resolveInitializedActiveAccountLabel loaded activeHttpAuth
    switchableTokenProvider =
        makeSwitchableTokenProvider refs activeHttpAuth
    tokenProvider =
        case loaded.loadedProvider of
            OpenAIProvider ->
                trackCredentialAccount
                    refs.initializedActiveAccountRef
                    refs.initializedActiveAccountIdRef
                    refs.initializedActiveSelectionRef
                    resolveActiveAccountLabel
                    selectableTokenProvider
            _ -> switchableTokenProvider

runInitialized :: InitializedRequest -> IO RunResult
runInitialized request = do
    workspace <- prepareInitializedWorkspace request
    targets <- resolveInitializedTargets request workspace
    routedAuth <- loadInitializedAuth request targets
    initializedAuth <-
        selectInitializedStartupAccount
            request
            workspace
            targets
            routedAuth
    validateInitializedAuth
        request
        targets
        initializedAuth.initializedLoaded
    accountRefs <- newInitializedAccountRefs request initializedAuth
    activeHttpAuth <-
        initializeActiveHttpAuth
            targets
            initializedAuth
            accountRefs
    let httpRuntime =
            newInitializedHttpRuntime
                initializedAuth
                accountRefs
                activeHttpAuth
    launchInitializedTools
        request
        workspace
        targets
        initializedAuth
        accountRefs
        httpRuntime

launchInitializedTools
    :: InitializedRequest
    -> InitializedWorkspace
    -> InitializedTargets
    -> InitializedAuth
    -> InitializedAccountRefs
    -> InitializedHttpRuntime
    -> IO RunResult
launchInitializedTools request workspace targets auth refs httpRuntime =
    runAgentTools
        request.initializedRunAgentChild
        auth.initializedLoaded
        request.initializedConnectedGateway
        auth.initializedLearnAboutUserRequested
        auth.initializedCustomBearerToken
        refs.initializedActiveAccountIdRef
        refs.initializedActiveAccountRef
        refs.initializedActiveSelectionRef
        startup.startupToolEnv
        workspace.initializedCatalog
        refs.initializedGatewayModelsRef
        targets.initializedGatewayIdentity
        ( targets.initializedCheckStartupUsageInBackground
            && isNothing auth.initializedPreparedAccountUsage
        )
        targets.initializedConfiguredTarget
        targets.initializedCustomResponses
        request.initializedCwd
        workspace.initializedDatabaseScopes
        startup.startupEscPaused
        fullscreen
        request.initializedHome
        startup.startupInterrupt
        startup.startupStdinTty
        request.initializedProcessRuntime.processMcpSupervisor
        request.initializedOptions
        pendingTurn
        refs.initializedPreferredOpenAiAccountRef
        request.initializedProcessRuntime
        workspace.initializedProjectRoot
        workspace.initializedProjectSettings
        targets.initializedProjectTarget
        httpRuntime.initializedResolveActiveAccountLabel
        request.initializedResumeLock
        request.initializedResumed
        targets.initializedResumedTarget
        request.initializedRoot
        httpRuntime.initializedSelectHttpAccount
        refs.initializedSelectableTokenProvider
        setWindowTitle
        startup
        workspace.initializedStateDirectory
        startup.startupStderr
        targets.initializedTargetHint
        httpRuntime.initializedTokenProvider
        transition
        targets.initializedTransitionTarget
        startup.startupUiRuntimeRef
        unavailableProviders
  where
    startup = request.initializedStartup
    fullscreen = startup.startupFullscreen
    transition = request.initializedTransition
    pendingTurn = transition >>= (.transitionPendingTurn)
    unavailableProviders =
        maybe Set.empty (.transitionUnavailableProviders) transition
    setWindowTitle title =
        case fullscreen of
            Just runtime -> setFullscreenWindowTitle runtime title
            Nothing ->
                setCliWindowTitle
                    startup.startupStdoutTty
                    startup.startupStdout
                    title

loadPreparedOrStartupAuth
    :: Maybe PreparedStartupAuthWorker
    -> Bool
    -> StartupRuntime
    -> Maybe ProviderTransition
    -> Maybe Provider
    -> IO ((LoadedAuth, Bool), Maybe PreparedProviderAccounts)
loadPreparedOrStartupAuth
    prepared
    usePreparedOnlyIfReady
    startup
    transition
    requestedProvider =
    case prepared of
        Nothing -> loadFallback
        Just preparedWorker -> do
            let worker = preparedWorker.preparedStartupAsync
            preparedResult <-
                if usePreparedOnlyIfReady
                    then
                        poll worker >>= \case
                            Nothing -> do
                                -- Some HTTP clients take time to acknowledge
                                -- async cancellation. Ask the scoped retirement
                                -- worker to handle that wait so startup can
                                -- continue to the post-prompt availability
                                -- probe.
                                preparedWorker.retirePreparedStartup
                                pure Nothing
                            Just _ -> Just <$> wait worker
                    else Just <$> wait worker
            case preparedResult of
                Just result
                    | Right loaded <- result.preparedAuthResult
                    , maybe True (== loaded.loadedProvider) requestedProvider ->
                        (, result.preparedAccountUsage)
                            <$> loadStartupAuthFromResult
                                startup
                                transition
                                requestedProvider
                                result.preparedAuthResult
                _ -> loadFallback
  where
    loadFallback =
        (, Nothing) <$> loadStartupAuth startup transition requestedProvider

trackCredentialAccount
    :: IORef Text
    -> IORef Text
    -> IORef Text
    -> (Credential -> IO Text)
    -> TokenProvider
    -> TokenProvider
trackCredentialAccount accountRef accountIdRef selectionRef resolveLabel provider =
    tokenProviderWithNextToken provider \failed ->
        getNextToken provider failed >>= \case
            Left err -> pure (Left err)
            Right credential -> do
                previousAccountId <- readIORef accountIdRef
                writeIORef accountIdRef credential.accountId
                when (previousAccountId /= credential.accountId) $
                    writeIORef selectionRef credential.accountId
                resolveLabel credential >>= writeIORef accountRef
                pure (Right credential)

loadGatewayModelAccess
    :: Maybe GatewayCredential
    -> LoadedAuth
    -> IO (Either Text (Maybe GatewayModelAccess))
loadGatewayModelAccess connectedGateway loaded
    | not (isGatewayLoadedAuth loaded) =
        pure case connectedGateway of
            Nothing -> Right Nothing
            Just _ ->
                Left
                    "Gateway credential and loaded authentication disagree."
    | otherwise =
        case connectedGateway of
            Nothing ->
                pure (Left "No organization gateway credential is connected.")
            Just credential -> do
                access <- newGatewayModelAccess credential
                refreshGatewayModels access >>= \case
                    Left err -> pure (Left err)
                    Right [] ->
                        pure
                            (Left
                                "The organization gateway does not offer any models.")
                    Right _ -> pure (Right (Just access))
