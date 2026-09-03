module Agent.CLI.Runtime.Orchestration.Providers (runAgentProviders) where

import Agent.CLI.AccountPicker ()
import Agent.CLI.Claude
    ( approveClaudeRegisteredTool
    , handleClaudePermissionRequest
    )
import Agent.CLI.AccountSelection ()
import Agent.CLI.Afk ()
import Agent.CLI.AgentSessions ()
import Agent.CLI.AgentViewport ()
import Agent.CLI.Approval ()
import Agent.CLI.Artifact ()
import Agent.CLI.Auth.Types (LoadedAuth(..), isGatewayLoadedAuth)
import Agent.CLI.ClaudeGatewayProxy (withClaudeGatewayProxy)
import Agent.CLI.Clipboard ()
import Agent.CLI.CodeModeRuntime ()
import Agent.CLI.Command ()
import Agent.CLI.Compaction
    ( CompactionInstall,
      CompactOutcome,
      OccupancySnapshot,
      autoCompactBackendWith,
      boundCompletedToolContinuations,
      decorateCompactOutcomeWithTaskPlan,
      decorateCompactOutcomeWithTaskPlanWithin,
      installLiveCompactOutcome,
      runProviderCompactWith,
      runBackendCompactHistoryWithContextWindow,
      runBackendCompactWithContextWindow,
      runResponsesCompactWithContextWindow )
import Agent.CLI.Config ()
import Agent.Connectivity ( withConnectionRecoveryOn )
import Agent.CLI.Database ()
import Agent.CLI.Database.Store ()
import Agent.CLI.Dialects ()
import Agent.CLI.Error ( formatApiErrorAt )
import Agent.CLI.GatewayClient (GatewayCredential)
import Agent.CLI.GatewayBridge ()
import Agent.CLI.Input ()
import Agent.CLI.LearnedSkills ()
import Agent.CLI.LearnedSkills.Store ()
import Agent.CLI.Login ()
import Agent.CLI.Lsp ()
import Agent.CLI.ManagedTurn ()
import Agent.CLI.McpManager ()
import Agent.CLI.McpStatus ()
import Agent.CLI.ModelConfig (ModelCatalog)
import Agent.CLI.Models ()
import Agent.CLI.Options (CliOptions(..))
import Agent.CLI.PendingInputs (PendingInputs, withPendingInputs)
import Agent.CLI.Plan ()
import Agent.CLI.Project (ModelSwitchScope)
import Agent.CLI.Prompt ()
import Agent.CLI.PromptHooks ()
import Agent.CLI.Provider.OpenAI
    ( OpenAiPersistentConnection(..), lockedOpenAiSession )
import Agent.CLI.Provider.Switch
    ( chooseStartupProviderTransition, prepareTransitionBackend )
import Agent.CLI.ProviderAvailability ()
import Agent.CLI.ProviderFallback ( isProviderUnavailable )
import Agent.CLI.ProviderTransition
    ( ProviderTransition(transitionCause),
      TransitionCause(AutomaticFallback) )
import Agent.Tools.TaskPlan (TaskPlanEnv)
import Agent.CLI.Recap ()
import Agent.CLI.Render ( putTextLn )
import Agent.CLI.ReplMode ()
import Agent.CLI.Request ()
import Agent.CLI.Resume ()
import Agent.CLI.Runtime.HistorySource ()
import Agent.CLI.Runtime.Orchestration.Background ()
import Agent.CLI.Runtime.Orchestration.Concurrent ()
import Agent.CLI.Runtime.Orchestration.Restart ()
import Agent.CLI.Runtime.Orchestration.Startup
    ( finishStartup )
import Agent.CLI.Runtime.Orchestration.Types
    ( AccountSwitchRequest(..)
    , NativeRunCapabilities(..)
    , NativeRunHooks(..)
    , fullNativeRunCapabilities
    )
import Agent.CLI.Runtime.Persistence ()
import Agent.CLI.Runtime.Recap
    ( runSessionRecap, runSessionTurnSummary )
import Agent.CLI.Runtime.Repl
    ( finishTurn,
      preparePromptSkillInputsWithPaste,
      repl,
      replWithDraft,
      runPendingTurn )
import Agent.CLI.Runtime.Types
    ( RunResult(RunSwitchProvider, RunProviderStartFailed) )
import Agent.CLI.Secret ()
import Agent.CLI.Session.Attachments ()
import Agent.CLI.Session.Choices ()
import Agent.CLI.Session.History
    ( LiveConversation, readLiveTranscript )
import Agent.CLI.Session.Lifecycle ()
import Agent.CLI.Session.Runtime.Types
    ( StartupRuntime(..),
      SessionBackend(..)
    , SessionRequest(..)
    )
import Agent.CLI.Session.Types (Persistence)
import Agent.CLI.Session.Selection ()
import Agent.CLI.SessionAdmin ()
import Agent.CLI.SessionEnv ()
import Agent.CLI.SessionLock ()
import Agent.CLI.SessionState ()
import Agent.CLI.SessionTitle ()
import Agent.CLI.Skills ()
import Agent.CLI.Startup.Auth ( startupDie )
import Agent.CLI.StartupContext ()
import Agent.CLI.Style ( glyphWarn, roleWarn )
import Agent.CLI.Subagents.Runtime
    ( freshOpenAiBackend,
      runCodexSubagent,
      runHttpSubagent,
      runXaiParentSubagent )
import Agent.CLI.Subagents.Runtime.Types (SubagentRuntime)
import Agent.CLI.TUI.App
    ( FullscreenRuntime, emitUiEvent )
import Agent.CLI.TUI.History ()
import Agent.CLI.TUI.SessionHistory ()
import Agent.CLI.Terminal ( resolveColor )
import Agent.CLI.Tools ()
import Agent.CLI.Turn ()
import Agent.CLI.Usage ()
import Agent.CLI.WebFetch ()
import Agent.CLI.Worktree ()
import Agent.Cancel ()
import Agent.Claude
    ( ClaudeCodeAuth(..),
      ClaudeCodeBackendHandle(..),
      ClaudeCodeOptions(..),
      ClaudeCodePermission(..),
      claudeCodeOneShotBackend,
      defaultClaudeCodeOptions,
      loadClaudeCodeAuth,
      loadClaudeCodeGatewayAuth,
      withClaudeCodeBackendWithHost )
import Agent.Claude.Control
    ( ClaudeCodeHostHandlers(..)
    , ClaudeCodeMcpRequest(..)
    , defaultClaudeCodeHostHandlers
    )
import Agent.Dialect (Dialect)
import Agent.Error ( ApiError(..), ErrorType(InvalidRequestError) )
import Agent.GrokBuild.Dialect.Goal ()
import Agent.GrokBuild.Dialect.Runtime ()
import Agent.GrokBuild.Dialect.Task ()
import Agent.GrokBuild.Dialect.Workflow ()
import Agent.Gemini.LoopBackend
    ( tokenProviderStatelessGeminiBackend )
import Agent.Loop
    ( Backend(submitTurn, Backend)
    , BackendSnapshot(..)
    , TokenUsage
    , TurnInput
    , defaultLoopDispatch
    )
import Agent.OpenAI.Auth (Pool)
import Agent.OpenAI.Compaction ()
import Agent.OpenAI.ModelMetadata (codexEffectiveContextWindowFor)
import Agent.OpenAI.Usage ()
import Agent.OpenAI.WebSocketClient
    ( CodexAuthFailed(..),
      closeCodexConn,
      codexConnTurnState,
      codexConnUsesHttpFallback,
      isGatewayWebSocketCredential,
      resetCodexTurnState,
      withCodexWsCredentialOrHttpFallback,
      withCodexWsWithProviderOrHttpFallback )
import Agent.OpenRouter.LoopBackend ( openRouterBackend )
import Agent.OpenRouter.Options (ClientOptions)
import Agent.OsPath (unsafeToFilePath)
import Agent.Provider
    ( Provider(OpenRouterProvider, OpenAIProvider, XAIProvider,
               GeminiProvider, ClaudeCodeProvider),
      Credential(accountId, Credential, accessToken, leaseId, provider),
      TokenProvider,
      runWithTokenProvider,
      tokenProviderBillingMode )
import Agent.ReasoningEffort ()
import Agent.Responses.GenericBackend
    ( genericResponsesBackendWith )
import Agent.Responses.GenericClient ()
import Agent.Responses.Types ( ResponseCreateParams(model) )
import Agent.Skills ()
import Agent.Store.Postgres ()
import Agent.Store.Types ()
import Agent.Subagents ( setSubagentRunner )
import Agent.Subagents.TaskPath ()
import Agent.TUI.Model ( UiEvent(UiSystemMessage) )
import Agent.TUI.Motion ()
import Agent.Tools.MultiAgents ()
import Agent.Tools.MultiAgents (MultiAgentContext(..))
import Agent.Tools.PlanMode ()
import Agent.Tools.Secret ()
import Agent.Tools.Types ()
import Agent.Tools.OutputArtifact (finalizeToolOutput)
import Agent.ToolDispatch (ToolDispatchConfig(..))
import Agent.XAI.LoopBackend ( xaiBackend )
import Control.Applicative ()
import Control.Concurrent.Async ( link, withAsync )
import Control.Concurrent.Chan
    ( Chan, newChan, readChan, writeChan )
import Control.Concurrent.MVar
    ( withMVar, newEmptyMVar, newMVar, putMVar, takeMVar, tryPutMVar )
import Control.Concurrent.STM (STM)
import Control.Exception ()
import Control.Exception.Safe ( catchAny, finally, try )
import Control.Monad ( when )
import Data.Functor ()
import Data.IORef
    ( IORef, atomicModifyIORef', newIORef, readIORef, writeIORef )
import Data.List ()
import Data.Maybe ( fromMaybe, isJust )
import Data.Set (Set)
import Data.Text ()
import Data.Text (Text)
import Data.Time.Clock ( getCurrentTime )
import System.Console.ANSI ()
import System.Console.ANSI.Codes ()
import System.Directory.OsPath ()
import System.Environment ()
import System.Exit ()
import System.IO (Handle)
import System.OsPath (OsPath)
import System.OsPath ()
import qualified Data.ByteString as BS ()
import qualified Data.Aeson as Aeson
import qualified Agent.Responses.GenericClient as GenericResponses
    ( GenericClientOptions(model),
      createResponseWith,
      createResponseWithEvents )
import qualified Agent.MCP as MCP
import qualified Data.Map.Strict as Map ()
import qualified Agent.OpenAI.Auth as OpenAI
    ( discoverAccounts, getAccessTokenForAccount )
import qualified Agent.OpenRouter as OpenRouter
    ( createResponseWith )
import qualified Agent.OpenRouter.Usage as OpenRouterUsage ()
import qualified Agent.Provider as Provider ()
import qualified Agent.CLI.Session.Lifecycle as SessionLifecycle ()
import qualified Agent.CLI.Session.Runner as SessionRunner
    ( runSession, SessionRunnerContinuation(..) )
import qualified Data.Set as Set ()
import qualified Data.Text as Text ( null, unpack )
import qualified Agent.XAI.Options as XAI
    ( ClientOptions(hostedXSearchEnabled)
    , clientOptionsFromEnv
    )
import qualified Agent.XAI.Client as XAIClient
    ( createResponseWith )
import qualified Agent.XAI.Request as XAIRequest ( mapModel )
import qualified Agent.XAI.Usage as XAIUsage ()
import qualified Agent.Gemini.Client as GeminiClient
    ( createResponseWith, createResponseWithEvents )
import qualified Agent.Gemini.Options as Gemini
    ( clientOptionsFromEnv )

runAgentProviders
    :: ModelSwitchScope
    -> LoadedAuth
    -> Maybe GatewayCredential
    -> (Maybe (STM ApiError)
        -> Maybe TokenProvider
        -> Maybe Pool
        -> Maybe (Text -> IO (Either ApiError Text))
        -> IO (Maybe Int)
        -> (Maybe Text -> IO (Either Text CompactOutcome))
        -> SessionRequest)
    -> IORef Text
    -> IORef Text
    -> IORef Text
    -> ModelCatalog
    -> Bool
    -> IORef (Maybe OccupancySnapshot)
    -> ((Text -> Text) -> Int -> ResponseCreateParams -> Int)
    -> IORef LiveConversation
    -> ((Text -> Text) -> IO (Maybe Int))
    -> Maybe GenericResponses.GenericClientOptions
    -> OsPath
    -> Dialect
    -> Maybe FullscreenRuntime
    -> IORef (CompactOutcome -> [TurnInput] -> IO CompactionInstall)
    -> Maybe TaskPlanEnv
    -> OsPath
    -> Maybe Text
    -> Text
    -> Maybe MultiAgentContext
    -> ClientOptions
    -> CliOptions
    -> ResponseCreateParams
    -> IORef ResponseCreateParams
    -> PendingInputs
    -> Persistence
    -> IORef (Maybe Text)
    -> OsPath
    -> Provider
    -> (TokenUsage -> IO ())
    -> (Credential -> IO Text)
    -> (Text -> IO (Either ApiError Text))
    -> TokenProvider
    -> Bool
    -> StartupRuntime
    -> Maybe (STM ApiError)
    -> Handle
    -> SubagentRuntime
    -> TokenProvider
    -> Maybe ProviderTransition
    -> (Text -> Text)
    -> Set Provider
    -> IO RunResult
runAgentProviders
    modelSwitchScope
    loaded
    connectedGateway
    sessionRequest
    activeAccountIdRef
    activeAccountRef
    activeSelectionRef
    catalog
    claudeBypassEnabled
    contextTokensRef
    contextWindowForParams
    conversationRef
    currentModelContextWindow
    customGenericOptions
    cwd
    dialect
    fullscreen
    automaticCompactionHookRef
    taskPlan
    home
    initialPrevious
    model
    multiCtx
    openRouterOptions
    options
    _params
    paramsRef
    pendingNotices
    persist
    preferredOpenAiAccountRef
    projectRoot
    provider
    recordCompactionUsage
    resolveActiveAccountLabel
    selectHttpAccount
    selectableTokenProvider
    shouldProbeAtStartup
    startup
    startupUnavailable
    stderrHandle
    subagentRuntime
    tokenProvider
    transition
    transportModel
    unavailableProviders
    =
        let nativeCapabilities =
                maybe
                    fullNativeRunCapabilities
                    (.nativeCapabilities)
                    startup.startupNativeHooks
        in case provider of
                    OpenAIProvider ->
                        try @_ @CodexAuthFailed
                            (withCodexWsWithProviderOrHttpFallback tokenProvider \conn credential -> do
                                wsLock <- newMVar ()
                                let startsOnHttp =
                                        codexConnUsesHttpFallback conn
                                initialWsHealthy <- newIORef (not startsOnHttp)
                                activeConnectionRef <- newIORef $
                                    OpenAiPersistentConnection
                                        credential
                                        initialWsHealthy
                                        conn
                                httpFallbackActive <- newIORef startsOnHttp
                                switchRequests <-
                                    newChan :: IO (Chan AccountSwitchRequest)
                                let selectableOpenAiPool
                                        | isGatewayLoadedAuth loaded = Nothing
                                        | otherwise = loaded.loadedOpenAiPool
                                    selectAccount = case selectableOpenAiPool of
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
                                    switchLoop = case selectableOpenAiPool of
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
                                                            let usesHttp =
                                                                    codexConnUsesHttpFallback
                                                                        newConn
                                                            newHealthy <-
                                                                newIORef
                                                                    (not usesHttp)
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
                                                            writeIORef
                                                                httpFallbackActive
                                                                usesHttp
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
                                                        withCodexWsCredentialOrHttpFallback
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
                                                                            (withCodexWsCredentialOrHttpFallback
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
                                                (isGatewayWebSocketCredential
                                                    credential)
                                                subagentRuntime
                                                selectableTokenProvider
                                                ctx.multiSendToRoot
                                    Nothing -> pure ()
                                let (compactSender, lockedBackend) =
                                        lockedOpenAiSession
                                            startup.startupNetworkRecovery
                                            (isGatewayWebSocketCredential
                                                credential)
                                            options.optCompactThreshold
                                            options.optShowRawReasoning
                                            wsLock
                                            httpFallbackActive
                                            tokenProvider
                                            activeConnectionRef
                                            (readIORef paramsRef)
                                            contextTokensRef
                                            recordCompactionUsage
                                            (decorateCompactOutcomeWithTaskPlan
                                                taskPlan)
                                            (\outcome inputs ->
                                                readIORef
                                                    automaticCompactionHookRef
                                                    >>= \hook ->
                                                        hook outcome inputs)
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
                                                        (\requestedFocus ->
                                                            runProviderCompactWith
                                                                (Just compactSender)
                                                                recordCompactionUsage
                                                                provider
                                                                (Just tokenProvider)
                                                                paramsRef
                                                                historyRef
                                                                requestedFocus
                                                                >>= decorateManualCompact
                                                                    (codexEffectiveContextWindowFor
                                                                        . (.model)))
                                                        focus
                                            resetCodexTurnState turnState
                                            runCompact `finally`
                                                resetCodexTurnState turnState
                                activeBackend <-
                                    prepareTransitionBackend
                                        modelSwitchScope home projectRoot
                                        transition persist noticingBackend
                                withAsync switchLoop \switchWorker -> do
                                    link switchWorker
                                    runSession
                                        (sessionRequest
                                            startupUnavailable
                                            (Just tokenProvider)
                                            selectableOpenAiPool
                                            selectAccount
                                            (currentModelContextWindow transportModel)
                                            compactRunner)
                                        SessionBackend
                                            { backend = activeBackend
                                            , btwBackend
                                            , interruptBackend = pure ()
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
                                            , not (isGatewayLoadedAuth loaded)
                                            , isProviderUnavailable err ->
                                                chooseStartupProviderTransition
                                                    nativeCapabilities.nativeProviderFallback
                                                    catalog
                                                    projectRoot
                                                    fullscreen
                                                    (tokenProviderBillingMode
                                                        tokenProvider)
                                                    provider
                                                    model
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
                        xaiOptions0 <- XAI.clientOptionsFromEnv
                        let xaiOptions =
                                xaiOptions0
                                    { XAI.hostedXSearchEnabled =
                                        nativeCapabilities.nativeProviderHostedTools
                                    }
                        let xaiContextWindow =
                                contextWindowForParams
                                    (XAIRequest.mapModel xaiOptions)
                                    500_000
                            protectXaiOverflow occupancy getParams backend =
                                boundCompletedToolContinuations
                                    xaiContextWindow
                                    getParams
                                    occupancy
                                    backend
                        case multiCtx of
                            Just ctx ->
                                setSubagentRunner ctx.multiRegistry $
                                    runXaiParentSubagent
                                        subagentRuntime
                                        dialect
                                        ctx.multiSendToRoot
                                        (\childParams ->
                                            protectXaiOverflow
                                                contextTokensRef
                                                (pure childParams)
                                                (xaiBackend xaiOptions tokenProvider
                                                    (pure childParams)))
                            Nothing -> pure ()
                        let backend =
                                withPendingInputs pendingNotices $
                                    withConnectionRecoveryOn
                                        startup.startupNetworkRecovery $
                                        protectXaiOverflow
                                            contextTokensRef
                                            (readIORef paramsRef)
                                            (xaiBackend xaiOptions tokenProvider
                                                (readIORef paramsRef))
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
                                    (\requestedFocus ->
                                        runResponsesCompactWithContextWindow
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
                                            historyRef
                                            requestedFocus
                                            >>= decorateManualCompact
                                                xaiContextWindow)
                                    focus
                        activeBackend <-
                            prepareTransitionBackend
                                modelSwitchScope home projectRoot
                                transition persist backend
                        runSession
                            (sessionRequest
                                startupUnavailable
                                (Just tokenProvider)
                                loaded.loadedOpenAiPool
                                (if isGatewayLoadedAuth loaded
                                    || isJust customGenericOptions
                                    then Nothing
                                    else Just selectHttpAccount)
                                (Just . xaiContextWindow
                                    <$> readIORef paramsRef)
                                compactRunner)
                            SessionBackend
                                { backend = activeBackend
                                , btwBackend
                                , interruptBackend = pure ()
                                , resetBackendState = pure ()
                                }
                    GeminiProvider -> do
                        geminiOptions <- Gemini.clientOptionsFromEnv
                        geminiOccupancy <- newIORef Nothing
                        let geminiContextWindow =
                                contextWindowForParams id 1_048_576
                            makeBackend getParams =
                                tokenProviderStatelessGeminiBackend
                                    tokenProvider
                                    (GeminiClient.createResponseWithEvents
                                        geminiOptions)
                                    getParams
                            protectGeminiOverflow occupancy getParams backend =
                                boundCompletedToolContinuations
                                    geminiContextWindow
                                    getParams
                                    occupancy
                                    backend
                        case multiCtx of
                            Just ctx ->
                                setSubagentRunner ctx.multiRegistry $
                                    runHttpSubagent
                                        subagentRuntime
                                        dialect
                                        GeminiProvider
                                        ctx.multiSendToRoot
                                        (\childParams ->
                                            protectGeminiOverflow
                                                geminiOccupancy
                                                (pure childParams)
                                                (makeBackend
                                                    (pure childParams)))
                            Nothing -> pure ()
                        let backend =
                                withPendingInputs pendingNotices $
                                    withConnectionRecoveryOn
                                        startup.startupNetworkRecovery $
                                        protectGeminiOverflow
                                            geminiOccupancy
                                            (readIORef paramsRef)
                                            (makeBackend
                                                (readIORef paramsRef))
                            btwBackend privateParams =
                                makeBackend (pure privateParams)
                            compactRunner focus = do
                                contextWindow <-
                                    currentModelContextWindow id
                                historyRef <-
                                    newIORef =<< readLiveTranscript conversationRef
                                installLiveCompactOutcome conversationRef Nothing
                                    (\requestedFocus ->
                                        runResponsesCompactWithContextWindow
                                            contextWindow
                                            (\request ->
                                                runWithTokenProvider tokenProvider
                                                    \credential ->
                                                        GeminiClient.createResponseWith
                                                            geminiOptions
                                                            credential
                                                            request)
                                            recordCompactionUsage
                                            paramsRef
                                            historyRef
                                            requestedFocus
                                            >>= decorateManualCompact
                                                geminiContextWindow)
                                    focus
                        activeBackend <-
                            prepareTransitionBackend
                                modelSwitchScope home projectRoot
                                transition persist backend
                        runSession
                            (sessionRequest
                                startupUnavailable
                                (Just tokenProvider)
                                loaded.loadedOpenAiPool
                                (if isGatewayLoadedAuth loaded
                                    then Nothing
                                    else Just selectHttpAccount)
                                (Just . geminiContextWindow
                                    <$> readIORef paramsRef)
                                compactRunner)
                            SessionBackend
                                { backend = activeBackend
                                , btwBackend
                                , interruptBackend = pure ()
                                , resetBackendState = pure ()
                                }
                    ClaudeCodeProvider
                        | not
                            nativeCapabilities.nativeProviderNativeTools ->
                            startupDie startup
                                "Claude Code is unavailable in this runtime"
                        | otherwise -> withSelectedClaudeAuth
                            connectedGateway
                            loaded
                            (startupDie startup . Text.unpack)
                            \claudeAuth -> do
                        let permission =
                                ClaudeCodeManual
                            claudeOptions =
                                (defaultClaudeCodeOptions
                                    claudeAuth.executable
                                    (unsafeToFilePath cwd))
                                    { permission
                                    , safeMode = True
                                    , transport = claudeAuth.transport
                                    }
                            claudeContextWindow = do
                                currentParams <- readIORef paramsRef
                                pure $
                                    contextWindowForParams
                                        transportModel
                                        200_000
                                        currentParams
                            claudeCompactThreshold = do
                                contextWindow <- claudeContextWindow
                                pure $
                                    max 1 $
                                        min contextWindow $
                                            fromMaybe
                                                (contextWindow * 4 `div` 5)
                                                options.optCompactThreshold
                            btwBackend privateParams =
                                Backend \state previous inputs onEvent -> do
                                    privateTranscript <-
                                        newIORef state.backendItems
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
                            compactRunner focus = do
                                contextWindow <- claudeContextWindow
                                historyRef <-
                                    newIORef =<< readLiveTranscript
                                        conversationRef
                                installLiveCompactOutcome
                                    conversationRef
                                    (Just contextTokensRef)
                                    (\requestedFocus ->
                                        runBackendCompactWithContextWindow
                                            contextWindow
                                            btwBackend
                                            recordCompactionUsage
                                            paramsRef
                                            historyRef
                                            requestedFocus
                                            >>= decorateManualCompact
                                                (const contextWindow))
                                    focus
                            claudeRequest =
                                sessionRequest
                                    startupUnavailable
                                    Nothing
                                    Nothing
                                    Nothing
                                    (Just <$> claudeContextWindow)
                                    compactRunner
                        claudeMcpServer <-
                            case MCP.createInProcessMcpServer
                                "haskell-agent"
                                "0.1.0"
                                (defaultLoopDispatch
                                    { toolDispatchFinalizeOutput =
                                        \call output ->
                                            finalizeToolOutput
                                                claudeRequest.toolEnv
                                                call
                                                output
                                    })
                                (approveClaudeRegisteredTool
                                    claudeRequest.claudeRuntimeSlot)
                                claudeRequest.claudeBridgeTools of
                                Left err -> startupDie startup (Text.unpack err)
                                Right server -> pure server
                        let hostHandlers =
                                defaultClaudeCodeHostHandlers
                                    { canUseTool =
                                        Just
                                            (handleClaudePermissionRequest
                                                claudeRequest.claudeRuntimeSlot)
                                    , handleMcpMessage =
                                        Just \request ->
                                            if request.serverName
                                                    /= "haskell-agent"
                                                then
                                                    pure Aeson.Null
                                                else
                                                    MCP.handleInProcessMcpMessage
                                                        claudeMcpServer
                                                        request.message
                                                        >>= pure . fromMaybe
                                                            (Aeson.object [])
                                    , mcpToolNames =
                                        MCP.inProcessMcpToolNames
                                            claudeMcpServer
                                    , nativeToolsEnabled =
                                        nativeCapabilities.nativeProviderNativeTools
                                    }
                        when claudeBypassEnabled $
                            case fullscreen of
                                Just runtime ->
                                    emitUiEvent runtime
                                        (UiSystemMessage
                                            "Claude Code --yolo is active; host catastrophic-command and Plan Mode denies remain enforced.")
                                Nothing -> do
                                    color <- resolveColor stderrHandle
                                    putTextLn stderrHandle $
                                        roleWarn color $
                                            glyphWarn
                                                <> "Claude Code --yolo is active; host catastrophic-command and Plan Mode denies remain enforced."
                        writeIORef activeAccountRef claudeAuth.accountLabel
                        claudeTranscriptRef <-
                            newIORef =<< readLiveTranscript conversationRef
                        withClaudeCodeBackendWithHost
                            claudeOptions
                            hostHandlers
                            initialPrevious
                            (readIORef paramsRef)
                            claudeTranscriptRef
                            \handle -> do
                                let compactHistory history _inputs = do
                                        contextWindow <- claudeContextWindow
                                        currentParams <- readIORef paramsRef
                                        runBackendCompactHistoryWithContextWindow
                                            contextWindow
                                            btwBackend
                                            recordCompactionUsage
                                            currentParams
                                            history
                                            Nothing
                                            >>= decorateAutomaticCompact
                                                (const contextWindow)
                                    compactingBackend =
                                        autoCompactBackendWith
                                            claudeCompactThreshold
                                            compactHistory
                                            (\outcome inputs ->
                                                readIORef
                                                    automaticCompactionHookRef
                                                    >>= \hook ->
                                                        hook outcome inputs)
                                            (readIORef paramsRef)
                                            contextTokensRef
                                            handle.loopBackend
                                activeBackend <-
                                    prepareTransitionBackend
                                        modelSwitchScope home projectRoot
                                        transition persist compactingBackend
                                result <- runSession
                                    claudeRequest
                                    SessionBackend
                                        { backend = activeBackend
                                        , btwBackend
                                        , interruptBackend =
                                            handle.interruptActiveTurn
                                        , resetBackendState =
                                            writeIORef claudeTranscriptRef []
                                        }
                                pure result
                    OpenRouterProvider -> do
                        let openRouterContextWindow =
                                contextWindowForParams transportModel 1_048_576
                            makeBackend params =
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
                            protectOverflow occupancy getParams backend =
                                boundCompletedToolContinuations
                                    openRouterContextWindow
                                    getParams
                                    occupancy
                                    backend
                        case multiCtx of
                            Just ctx ->
                                setSubagentRunner ctx.multiRegistry $
                                    runHttpSubagent
                                        subagentRuntime
                                        dialect
                                        OpenRouterProvider
                                        ctx.multiSendToRoot
                                        (\childParams ->
                                            protectOverflow
                                                contextTokensRef
                                                (pure childParams)
                                                (makeBackend
                                                    (pure childParams)))
                            Nothing -> pure ()
                        let backend =
                                withPendingInputs pendingNotices $
                                    withConnectionRecoveryOn
                                        startup.startupNetworkRecovery $
                                        protectOverflow
                                            contextTokensRef
                                            (readIORef paramsRef)
                                            (makeBackend
                                                (readIORef paramsRef))
                            btwBackend privateParams =
                                makeBackend
                                    (pure privateParams)
                            compactRunner focus = do
                                contextWindow <-
                                    currentModelContextWindow transportModel
                                historyRef <-
                                    newIORef =<< readLiveTranscript conversationRef
                                installLiveCompactOutcome conversationRef Nothing
                                    (\requestedFocus ->
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
                                            requestedFocus
                                            >>= decorateManualCompact
                                                openRouterContextWindow)
                                    focus
                        activeBackend <-
                            prepareTransitionBackend
                                modelSwitchScope home projectRoot
                                transition persist backend
                        runSession
                            (sessionRequest
                                startupUnavailable
                                (Just tokenProvider)
                                loaded.loadedOpenAiPool
                                (if isGatewayLoadedAuth loaded
                                    then Nothing
                                    else Just selectHttpAccount)
                                (Just . openRouterContextWindow
                                    <$> readIORef paramsRef)
                                compactRunner)
                            SessionBackend
                                { backend = activeBackend
                                , btwBackend
                                , interruptBackend = pure ()
                                , resetBackendState = pure ()
                                }
          where
            decorateManualCompact contextWindowForResult = \case
                Left err -> pure (Left err)
                Right outcome -> do
                    currentParams <- readIORef paramsRef
                    decorateCompactOutcomeWithTaskPlanWithin
                        (contextWindowForResult currentParams)
                        currentParams
                        taskPlan
                        outcome
            decorateAutomaticCompact contextWindowForResult = \case
                Left err -> pure (Left err)
                Right outcome ->
                    decorateManualCompact
                        contextWindowForResult
                        (Right outcome) >>= pure . \case
                            Left message ->
                                Left
                                    (ProviderError
                                        InvalidRequestError
                                        message
                                        Nothing)
                            Right decorated -> Right decorated
            startupFailure err = do
                now <- getCurrentTime
                startupDie startup
                    (Text.unpack (formatApiErrorAt now err))


withSelectedClaudeAuth
    :: Maybe GatewayCredential
    -> LoadedAuth
    -> (Text -> IO value)
    -> (ClaudeCodeAuth -> IO value)
    -> IO value
withSelectedClaudeAuth connectedGateway loaded onError action
    -- Use the same immutable credential snapshot that selected the catalog,
    -- auth, and session boundary. Reloading here could cross organizations.
    | not (isGatewayLoadedAuth loaded) =
        loadClaudeCodeAuth >>= either onError action
    | otherwise = case connectedGateway of
        Nothing ->
            onError "No organization gateway credential is connected."
        Just credential -> do
            result <- withClaudeGatewayProxy credential \transport ->
                loadClaudeCodeGatewayAuth transport
                    >>= either onError action
            either onError pure result

sessionRunnerContinuation :: SessionRunner.SessionRunnerContinuation
sessionRunnerContinuation =
    SessionRunner.SessionRunnerContinuation
        { runnerRepl = repl
        , runnerReplWithDraft = replWithDraft
        , runnerRunPendingTurn = runPendingTurn
        , runnerFinishTurn = finishTurn
        , runnerFinishStartup = finishStartup
        , runnerPreparePromptSkillInputs = preparePromptSkillInputsWithPaste
        , runnerRunSessionRecap = runSessionRecap
        , runnerRunSessionTurnSummary = runSessionTurnSummary
        }
runSession
    :: SessionRequest
    -> SessionBackend
    -> IO RunResult
runSession = SessionRunner.runSession sessionRunnerContinuation
