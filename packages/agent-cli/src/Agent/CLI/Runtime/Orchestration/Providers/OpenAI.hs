module Agent.CLI.Runtime.Orchestration.Providers.OpenAI
    ( runOpenAiProvider
    ) where

import Agent.CLI.Auth.Types
    ( LoadedAuth(..)
    , isGatewayLoadedAuth
    )
import Agent.CLI.Compaction
    ( decorateCompactOutcomeWithTaskPlan
    , installLiveCompactOutcome
    , runProviderCompactWith
    )
import Agent.CLI.PendingInputs (withPendingInputs)
import Agent.CLI.Options (CliOptions(..))
import Agent.CLI.Provider.OpenAI
    ( OpenAiPersistentConnection(..)
    , lockedOpenAiSession
    )
import Agent.CLI.Provider.Switch
    ( chooseStartupProviderTransition
    , prepareTransitionBackend
    )
import Agent.CLI.ProviderFallback (isProviderUnavailable)
import Agent.CLI.ProviderTransition
    ( ProviderTransition(transitionCause)
    , TransitionCause(AutomaticFallback)
    )
import Agent.CLI.Runtime.Orchestration.Providers.Common
    ( decorateManualCompact
    , runSession
    , startupFailure
    )
import Agent.CLI.Runtime.Orchestration.Providers.Types
    ( AgentProviderRequest(..)
    )
import Agent.CLI.Runtime.Orchestration.Types
    ( AccountSwitchRequest(..)
    , NativeRunCapabilities(..)
    )
import Agent.CLI.Runtime.Types
    ( RunResult(RunProviderStartFailed, RunSwitchProvider)
    )
import Agent.CLI.Session.History (readLiveTranscript)
import Agent.CLI.Session.Runtime.Types
    ( SessionBackend(..)
    , StartupRuntime(..)
    )
import Agent.CLI.Subagents.Runtime
    ( freshOpenAiBackend
    , runCodexSubagent
    )
import Agent.Error (ApiError(..))
import Agent.OpenAI.Auth (Pool)
import Agent.OpenAI.ModelMetadata (codexEffectiveContextWindowFor)
import Agent.OpenAI.WebSocketClient
    ( CodexAuthFailed(..)
    , CodexConn
    , closeCodexConn
    , codexConnTurnState
    , codexConnUsesHttpFallback
    , isGatewayWebSocketCredential
    , resetCodexTurnState
    , withCodexWsCredentialOrHttpFallback
    , withCodexWsWithProviderOrHttpFallback
    )
import Agent.Provider
    ( Credential(..)
    , Provider(OpenAIProvider)
    , tokenProviderBillingMode
    )
import Agent.Responses.Types (ResponseCreateParams(model))
import Agent.Subagents (setSubagentRunner)
import Agent.Tools.MultiAgents (MultiAgentContext(..))
import Control.Concurrent.Async (link, withAsync)
import Control.Concurrent.Chan
    ( Chan
    , newChan
    , readChan
    , writeChan
    )
import Control.Concurrent.MVar
    ( MVar
    , newEmptyMVar
    , newMVar
    , putMVar
    , takeMVar
    , tryPutMVar
    , withMVar
    )
import Control.Exception.Safe (catchAny, finally, try)
import Control.Monad (when)
import Data.IORef
    ( IORef
    , atomicModifyIORef'
    , newIORef
    , readIORef
    , writeIORef
    )
import Data.Text (Text)
import qualified Agent.OpenAI.Auth as OpenAI
import qualified Data.Text as Text

runOpenAiProvider
    :: AgentProviderRequest
    -> NativeRunCapabilities
    -> IO RunResult
runOpenAiProvider request@AgentProviderRequest{tokenProvider} nativeCapabilities =
    try @_ @CodexAuthFailed
        (withCodexWsWithProviderOrHttpFallback tokenProvider \conn credential ->
            runConnectedOpenAiProvider request conn credential)
        >>= handleOpenAiProviderResult request nativeCapabilities

data OpenAiProviderRuntime = OpenAiProviderRuntime
    { openAiWsLock :: MVar ()
    , openAiActiveConnectionRef :: IORef OpenAiPersistentConnection
    , openAiHttpFallbackActive :: IORef Bool
    , openAiSelectablePool :: Maybe Pool
    , openAiSelectAccount :: Maybe (Text -> IO (Either ApiError Text))
    , openAiSwitchLoop :: IO ()
    }

data OpenAiConnectionState = OpenAiConnectionState
    { openAiConnectionLock :: MVar ()
    , openAiConnectionRef :: IORef OpenAiPersistentConnection
    , openAiFallbackRef :: IORef Bool
    }

newOpenAiConnectionState
    :: CodexConn
    -> Credential
    -> IO OpenAiConnectionState
newOpenAiConnectionState conn credential = do
    openAiConnectionLock <- newMVar ()
    let startsOnHttp = codexConnUsesHttpFallback conn
    initialWsHealthy <- newIORef (not startsOnHttp)
    openAiConnectionRef <-
        newIORef $
            OpenAiPersistentConnection credential initialWsHealthy conn
    openAiFallbackRef <- newIORef startsOnHttp
    pure OpenAiConnectionState{..}

newOpenAiProviderRuntime
    :: AgentProviderRequest
    -> CodexConn
    -> Credential
    -> IO OpenAiProviderRuntime
newOpenAiProviderRuntime AgentProviderRequest{..} conn credential = do
    OpenAiConnectionState
        { openAiConnectionLock = wsLock
        , openAiConnectionRef =
            activeConnectionRef
        , openAiFallbackRef =
            httpFallbackActive
        } <- newOpenAiConnectionState conn credential
    switchRequests <-
        newChan :: IO (Chan AccountSwitchRequest)
    let selectableOpenAiPool
            | isGatewayLoadedAuth loaded = Nothing
            | otherwise = loaded.loadedOpenAiPool
        selectAccount =
            selectOpenAiAccount switchRequests
                <$> selectableOpenAiPool
        switchLoop = case selectableOpenAiPool of
            Nothing -> pure ()
            Just pool ->
                readChan switchRequests
                    >>= switchTo pool
        switchTo pool switchRequest =
            runSwitch pool switchRequest >>= \case
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
                        (Just selectedCredential.accountId)
                    let connectSelected =
                            withCodexWsCredentialOrHttpFallback
                                selectedCredential
                                \newConn newCredential -> do
                                    (newHealthy, label) <-
                                        installConnection
                                            newCredential
                                            newConn
                                    releaseLock
                                    _ <- tryPutMVar
                                        reply
                                        (Right label)
                                    awaitNext newHealthy
                        restorePrevious
                            selectedError
                            | Text.null previousAccountId =
                                failSwitch selectedError
                            | otherwise = do
                                writeIORef
                                    preferredOpenAiAccountRef
                                    (Just previousAccountId)
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
                                                    \newConn newCredential -> do
                                                        (newHealthy, _) <-
                                                            installConnection
                                                                newCredential
                                                                newConn
                                                        releaseLock
                                                        _ <- tryPutMVar
                                                            reply
                                                            (Left selectedError)
                                                        awaitNext
                                                            newHealthy)
                                                    >>= \case
                                                        Left _ ->
                                                            failSwitch
                                                                selectedError
                                                        Right next ->
                                                            pure
                                                                (Just next)
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
    pure
        OpenAiProviderRuntime
            { openAiWsLock = wsLock
            , openAiActiveConnectionRef =
                activeConnectionRef
            , openAiHttpFallbackActive =
                httpFallbackActive
            , openAiSelectablePool =
                selectableOpenAiPool
            , openAiSelectAccount = selectAccount
            , openAiSwitchLoop = switchLoop
            }

selectOpenAiAccount
    :: Chan AccountSwitchRequest
    -> Pool
    -> Text
    -> IO (Either ApiError Text)
selectOpenAiAccount switchRequests pool selectedAccountId = do
    _ <- OpenAI.discoverAccounts pool
    OpenAI.getAccessTokenForAccount pool selectedAccountId >>= \case
        Left err ->
            pure (Left err)
        Right (accessToken, accountId) -> do
            reply <- newEmptyMVar
            writeChan switchRequests $
                AccountSwitchRequest
                    Credential
                        { accessToken
                        , accountId
                        , leaseId = Nothing
                        , provider = OpenAIProvider
                        }
                    reply
            takeMVar reply

runConnectedOpenAiProvider
    :: AgentProviderRequest
    -> CodexConn
    -> Credential
    -> IO RunResult
runConnectedOpenAiProvider request@AgentProviderRequest{..} conn credential = do
    OpenAiProviderRuntime
        { openAiWsLock = wsLock
        , openAiActiveConnectionRef =
            activeConnectionRef
        , openAiHttpFallbackActive =
            httpFallbackActive
        , openAiSelectablePool =
            selectableOpenAiPool
        , openAiSelectAccount = selectAccount
        , openAiSwitchLoop = switchLoop
        } <- newOpenAiProviderRuntime
            request
            conn
            credential
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
                                    >>= decorateManualCompact request
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
                }

handleOpenAiProviderResult
    :: AgentProviderRequest
    -> NativeRunCapabilities
    -> Either CodexAuthFailed RunResult
    -> IO RunResult
handleOpenAiProviderResult request@AgentProviderRequest{..} nativeCapabilities = \case
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
                                startupFailure request err
            _ ->
                startupFailure request err
    Right result -> pure result
