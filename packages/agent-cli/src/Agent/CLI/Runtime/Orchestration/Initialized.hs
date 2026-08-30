module Agent.CLI.Runtime.Orchestration.Initialized (runAgentInitialized) where

import Agent.CLI.AccountPicker ()
import Agent.CLI.AccountSelection
    ( SelectedAccount(..),
      loadedAuthSupportsUsageAccountSelection,
      selectProviderAccount )
import Agent.CLI.Afk ()
import Agent.CLI.AgentSessions ()
import Agent.CLI.AgentViewport ()
import Agent.CLI.Approval ()
import Agent.CLI.Artifact ()
import Agent.CLI.Auth
    ( LoadedAuth(loadedAccountLabel, LoadedAuth, loadedOpenAiPool,
                 loadedProvider, loadedTokenProvider, loadedSelectionId),
      preferredOpenAiTokenProvider,
      loadAuthForAccount,
      probeLoadedAuthCredential,
      staticCredentialProvider )
import Agent.CLI.Clipboard ()
import Agent.CLI.CodeModeRuntime ()
import Agent.CLI.Command ()
import Agent.CLI.Compaction ()
import Agent.CLI.Config ()
import Agent.CLI.Connectivity ()
import Agent.CLI.Database ()
import Agent.CLI.Database.Store ( deriveDatabaseScopes )
import Agent.CLI.Dialects ()
import Agent.CLI.Error ()
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
    ( builtinConnectionId,
      catalogConnection,
      loadModelCatalogAt,
      ConnectionKind(BuiltinConnection, CustomResponsesConnection),
      ModelConnection(connectionId, connectionKind),
      ResponsesConnection(responsesRequestTimeoutSeconds,
                          responsesApiKeyEnv, responsesApiKeyOptional, responsesBaseUrl) )
import Agent.CLI.Models
    ( resolveConfiguredModel,
      ModelOption(modelTarget),
      ModelTarget(targetWireModelId, ModelTarget, targetConnectionId,
                  targetProvider, targetModelId, targetDialect) )
import Agent.CLI.Options ( CliOptions(optModel, optProvider) )
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
      ProjectSettings(settingsMaxConcurrentAgents, settingsAutoApprove,
                      settingsLastModel) )
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
import Agent.CLI.Resume ()
import Agent.CLI.Runtime.HistorySource ()
import Agent.CLI.Runtime.Orchestration.Background ()
import Agent.CLI.Runtime.Orchestration.Concurrent ()
import Agent.CLI.Runtime.Orchestration.Restart ()
import Agent.CLI.Runtime.Orchestration.Startup
    ( setStartupRepository )
import Agent.CLI.Runtime.Orchestration.Tools ( runAgentTools )
import Agent.CLI.Runtime.Orchestration.Types
    ( ActiveHttpAuth(activeHttpGeneration, ActiveHttpAuth,
                     activeHttpAccountId, activeHttpProvider, activeHttpResolveLabel),
      AgentProcessRuntime(processSessionThreads, processMcpSupervisor, processMcpElicitation),
      AgentRunMode )
import Agent.CLI.Runtime.Persistence ()
import Agent.CLI.Runtime.Recap ()
import Agent.CLI.Runtime.Repl ()
import Agent.CLI.Runtime.Types ( DevResult, RunResult )
import Agent.CLI.Secret ()
import Agent.CLI.Session
    ( SessionMeta(metaProvider, metaConnection, metaModel,
                  metaTransportModel, metaDialect),
      SessionTurn )
import Agent.CLI.Session.Attachments ()
import Agent.CLI.Session.Choices ()
import Agent.CLI.Session.History ( detectGitBranch )
import Agent.CLI.Session.Lifecycle ()
import Agent.CLI.Session.Runtime.Types
    ( StartupRuntime(startupToolEnv, startupStderr, startupStdout,
                     startupStdoutTty, startupStdinTty, startupFullscreen,
                     startupUiRuntimeRef, startupEscPaused, startupInterrupt) )
import Agent.CLI.Session.Selection ()
import Agent.CLI.SessionAdmin ()
import Agent.CLI.SessionEnv ()
import Agent.CLI.SessionLock ( releaseSessionLock, SessionLock )
import Agent.CLI.SessionState ()
import Agent.CLI.SessionTitle ()
import Agent.CLI.Skills ()
import Agent.CLI.Startup.Auth
    ( loadStartupAuth, markStartupStage, startupDie )
import Agent.CLI.StartupContext ()
import Agent.CLI.Style ( setCliWindowTitle )
import Agent.CLI.Subagents.Runtime ()
import Agent.CLI.TUI.App ( setFullscreenWindowTitle )
import Agent.CLI.TUI.History ()
import Agent.CLI.TUI.SessionHistory ()
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
               ClaudeCodeProvider),
      TokenProvider,
      Credential(..),
      getNextToken,
      providerSlug,
      tokenProvider,
      tokenProviderBillingMode,
      BillingMode(ApiBilled),
      FailedCredential(credential) )
import Agent.ReasoningEffort ()
import Agent.Responses.GenericBackend ()
import Agent.Responses.GenericClient ()
import Agent.Responses.Types ()
import Agent.Skills ()
import Agent.Store.Postgres ()
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
import Control.Concurrent.Async ( concurrently )
import Control.Concurrent.Chan ()
import Control.Concurrent.MVar ( modifyMVar_, newMVar, readMVar )
import Control.Concurrent.STM ()
import Control.Exception ()
import Control.Exception.Safe ( onException )
import Control.Monad ( when )
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
    -> IO RunResult
runAgentInitialized
        runAgentChild processRuntime options transition home root resumed resumeLock cwd startup =
    runAgentInitializedWithLock
        runAgentChild processRuntime options transition home root resumed resumeLock cwd startup
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
    -> IO RunResult
runAgentInitializedWithLock
        runAgentChild processRuntime
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
    ((projectSettings0, userSettings), (catalogResult, branch)) <-
        concurrently
            (concurrently
                (loadProjectSettings projectRoot)
                (loadUserSettings home))
            (concurrently
                (loadModelCatalogAt home cwd)
                (detectGitBranch cwd))
    let projectSettings =
            withInheritedLastModel projectSettings0 userSettings
    catalog <- either
        (startupDie startup . Text.unpack)
        pure
        catalogResult
    setStartupRepository fullscreen home branch cwd
    markStartupStage startup "Loading credentials…"
    let transitionTarget = (.transitionTarget) <$> transition
        pendingTurn = transition >>= (.transitionPendingTurn)
        unavailableProviders =
            maybe Set.empty (.transitionUnavailableProviders) transition
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
                (loadedAuthSupportsUsageAccountSelection initialLoaded) ->
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
                selectProviderAccount
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

    runAgentTools
        runAgentChild
        loaded
        learnAboutUserRequested
        customBearerToken
        activeAccountIdRef
        activeAccountRef
        activeSelectionRef
        baseToolEnv
        catalog
        checkStartupUsageInBackground
        configuredOptionTarget
        customResponses
        cwd
        databaseScopes
        escPaused
        fullscreen
        home
        interrupt
        isTty
        mcpSupervisor
        options
        pendingTurn
        preferredOpenAiAccountRef
        processRuntime
        projectRoot
        projectSettings
        projectTarget
        resolveActiveAccountLabel
        resumeLock
        resumed
        resumedTarget
        root
        selectHttpAccount
        selectableTokenProvider
        setWindowTitle
        startup
        stateDirectory
        stderrHandle
        targetHint
        tokenProvider
        transition
        transitionTarget
        uiRuntimeRef
        unavailableProviders

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
