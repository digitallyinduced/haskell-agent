module Agent.CLI.Runtime.Orchestration.Providers (runAgentProviders) where

import Agent.CLI.AccountPicker ()
import Agent.CLI.AccountSelection ()
import Agent.CLI.Afk ()
import Agent.CLI.AgentSessions ()
import Agent.CLI.AgentViewport ()
import Agent.CLI.Approval ()
import Agent.CLI.Artifact ()
import Agent.CLI.Auth ()
import Agent.CLI.Clipboard ()
import Agent.CLI.CodeModeRuntime ()
import Agent.CLI.Command ()
import Agent.CLI.Compaction
    ( boundCompletedToolContinuations,
      installLiveCompactOutcome,
      runProviderCompactWith,
      runResponsesCompactWithContextWindow )
import Agent.CLI.Config ()
import Agent.CLI.Connectivity ( withConnectionRecovery )
import Agent.CLI.Database ()
import Agent.CLI.Database.Store ()
import Agent.CLI.Dialects ()
import Agent.CLI.Error ( formatApiErrorAt )
import Agent.CLI.GatewayBridge ()
import Agent.CLI.Input ()
import Agent.CLI.Interrupt ( catchUserInterrupt )
import Agent.CLI.LearnedSkills ()
import Agent.CLI.LearnedSkills.Store ()
import Agent.CLI.Login ()
import Agent.CLI.Lsp ()
import Agent.CLI.ManagedTurn ()
import Agent.CLI.McpManager ()
import Agent.CLI.McpStatus ()
import Agent.CLI.ModelConfig ()
import Agent.CLI.Models ()
import Agent.CLI.Options ()
import Agent.CLI.PendingInputs ( withPendingInputs )
import Agent.CLI.Plan ()
import Agent.CLI.Project ()
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
    ( clearNativeProgress, finishStartup )
import Agent.CLI.Runtime.Orchestration.Types
    ( AccountSwitchRequest(..) )
import Agent.CLI.Runtime.Persistence ()
import Agent.CLI.Runtime.Recap
    ( runSessionRecap, runSessionTurnSummary )
import Agent.CLI.Runtime.Repl
    ( finishTurn,
      preparePromptSkillInputs,
      repl,
      replWithDraft,
      runPendingTurn )
import Agent.CLI.Runtime.Types
    ( RunResult(RunSwitchProvider, RunProviderStartFailed) )
import Agent.CLI.Secret ()
import Agent.CLI.Session
    ( resumeHint,
      Persistence(..),
      PersistenceState(PersistenceActive, PersistencePending),
      SessionHandle(sessionMeta),
      SessionMeta(metaId) )
import Agent.CLI.Session.Attachments ()
import Agent.CLI.Session.Choices ()
import Agent.CLI.Session.History
    ( readLiveTranscript, writeLiveTranscript )
import Agent.CLI.Session.Lifecycle ()
import Agent.CLI.Session.Runtime.Types
    ( SessionBackend(..), SessionRequest )
import Agent.CLI.Session.Selection ()
import Agent.CLI.SessionAdmin ()
import Agent.CLI.SessionEnv ()
import Agent.CLI.SessionLock ()
import Agent.CLI.SessionState ()
import Agent.CLI.SessionTitle ()
import Agent.CLI.Skills ()
import Agent.CLI.Startup.Auth ( startupDie )
import Agent.CLI.StartupContext ()
import Agent.CLI.Style ( glyphWarn, roleMuted, roleWarn )
import Agent.CLI.Subagents.Runtime
    ( freshOpenAiBackend,
      runCodexSubagent,
      runHttpSubagent,
      runXaiParentSubagent )
import Agent.CLI.TUI.App
    ( FullscreenRuntime, emitUiEvent, withFullscreenSuspended )
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
      ClaudeCodeOptions(..),
      ClaudeCodePermission(..),
      claudeCodeOneShotBackend,
      defaultClaudeCodeOptions,
      loadClaudeCodeAuth,
      withClaudeCodeBackend )
import Agent.Dialect ()
import Agent.Error ( ApiError(..) )
import Agent.GrokBuild.Dialect.Goal ()
import Agent.GrokBuild.Dialect.Runtime ()
import Agent.GrokBuild.Dialect.Task ()
import Agent.GrokBuild.Dialect.Workflow ()
import Agent.Loop ( Backend(submitTurn, Backend) )
import Agent.OpenAI.Compaction ()
import Agent.OpenAI.Usage ()
import Agent.OpenAI.WebSocketClient
    ( CodexAuthFailed(..),
      closeCodexConn,
      codexConnTurnState,
      isGatewayWebSocketCredential,
      resetCodexTurnState,
      withCodexWsCredential,
      withCodexWsWithProvider )
import Agent.OpenRouter.LoopBackend ( openRouterBackend )
import Agent.OsPath ( unsafeToFilePath )
import Agent.Provider
    ( Provider(OpenRouterProvider, OpenAIProvider, XAIProvider,
               ClaudeCodeProvider),
      Credential(accountId, Credential, accessToken, leaseId, provider),
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
import Agent.Tools.PlanMode ()
import Agent.Tools.Secret ()
import Agent.Tools.Types ()
import Agent.XAI.LoopBackend ( xaiBackend )
import Control.Applicative ()
import Control.Concurrent.Async ( link, withAsync )
import Control.Concurrent.Chan
    ( Chan, newChan, readChan, writeChan )
import Control.Concurrent.MVar
    ( withMVar, newEmptyMVar, newMVar, putMVar, takeMVar, tryPutMVar )
import Control.Concurrent.STM ()
import Control.Exception ()
import Control.Exception.Safe ( catchAny, finally, try )
import Control.Monad ( when )
import Data.Functor ()
import Data.IORef
    ( atomicModifyIORef', newIORef, readIORef, writeIORef )
import Data.List ()
import Data.Maybe ( fromMaybe, isJust )
import Data.Text ()
import Data.Time.Clock ( getCurrentTime )
import System.Console.ANSI ()
import System.Console.ANSI.Codes ()
import System.Directory.OsPath ()
import System.Environment ()
import System.Exit ()
import System.IO ( stderr )
import System.OsPath ()
import qualified Data.ByteString as BS ()
import qualified Agent.Responses.GenericClient as GenericResponses
    ( GenericClientOptions(model),
      createResponseWith,
      createResponseWithEvents )
import qualified Agent.MCP as MCP ()
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
import qualified Data.Text.IO as Text ( hPutStr )
import qualified Agent.XAI.Options as XAI ( clientOptionsFromEnv )
import qualified Agent.XAI.Client as XAIClient
    ( createResponseWith )
import qualified Agent.XAI.Request as XAIRequest ( mapModel )
import qualified Agent.XAI.Usage as XAIUsage ()

runAgentProviders
    modelSwitchScope
    loaded
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
    home
    initialPrevious
    model
    multiCtx
    openRouterOptions
    options
    params
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
    = case provider of
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
                                                (isGatewayWebSocketCredential
                                                    credential)
                                                subagentRuntime
                                                selectableTokenProvider
                                                ctx.multiSendToRoot
                                    Nothing -> pure ()
                                let (compactSender, lockedBackend) =
                                        lockedOpenAiSession
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
                                        modelSwitchScope home projectRoot
                                        transition persist noticingBackend
                                withAsync switchLoop \switchWorker -> do
                                    link switchWorker
                                    runSession
                                        (sessionRequest
                                            startupUnavailable
                                            (Just tokenProvider)
                                            loaded.loadedOpenAiPool
                                            selectAccount
                                            (currentModelContextWindow transportModel)
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
                                    withConnectionRecovery $
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
                                modelSwitchScope home projectRoot
                                transition persist backend
                        runSession
                            (sessionRequest
                                startupUnavailable
                                (Just tokenProvider)
                                loaded.loadedOpenAiPool
                                (if isJust customGenericOptions
                                    then Nothing
                                    else Just selectHttpAccount)
                                (Just . xaiContextWindow
                                    <$> readIORef paramsRef)
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
                                    , transport = claudeAuth.transport
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
                        withClaudeCodeBackend
                            claudeOptions
                            initialPrevious
                            (readIORef paramsRef)
                            claudeTranscriptRef
                            \backend -> do
                                activeBackend <-
                                    prepareTransitionBackend
                                        modelSwitchScope home projectRoot
                                        transition persist backend
                                result <- runSession
                                    (sessionRequest
                                        startupUnavailable
                                        Nothing
                                        Nothing
                                        Nothing
                                        (pure Nothing)
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
                                    withConnectionRecovery $
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
                                modelSwitchScope home projectRoot
                                transition persist backend
                        runSession
                            (sessionRequest
                                startupUnavailable
                                (Just tokenProvider)
                                loaded.loadedOpenAiPool
                                (Just selectHttpAccount)
                                (Just . openRouterContextWindow
                                    <$> readIORef paramsRef)
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
