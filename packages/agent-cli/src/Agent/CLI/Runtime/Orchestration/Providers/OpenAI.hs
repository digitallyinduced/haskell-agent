module Agent.CLI.Runtime.Orchestration.Providers.OpenAI
    ( withOpenAiProvider
    ) where

import Agent.CLI.Session.Request
    ( readSessionRequestParams
    )
import Agent.CLI.Compaction
    ( decorateCompactOutcomeWithTaskPlan
    , installLiveCompactOutcome
    , runProviderCompactWith
    )
import Agent.CLI.Provider.OpenAI
    ( OpenAiPersistentConnection(..)
    , lockedOpenAiSession
    )
import Agent.CLI.Runtime.Orchestration.Providers.Common
    ( decorateManualCompact
    )
import Agent.CLI.Runtime.Orchestration.Providers.Types
    ( OpenAiConfig(..), OpenAiAccounts(..), ProviderHost(..)
    , ProviderCompaction(..), ProviderRuntime(..)
    , ProviderAccountSelection(..), ProviderSubagents(..)
    )
import Agent.CLI.Session.History (readLiveTranscript)
import Agent.CLI.Session.Runtime.Types
    ( SessionBackend(..)
    )
import Agent.CLI.Subagents.Runtime.OpenAI
    ( freshOpenAiBackend
    )
import Agent.Error (ApiError(..))
import Agent.OpenAI.Auth (Pool)
import Agent.OpenAI.ModelMetadata (codexEffectiveContextWindowFor)
import Agent.OpenAI.WebSocketClient
    ( CodexConn
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
    )
import Agent.Responses.Types (ResponseCreateParams(model))
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
import Control.Exception.Safe (catchAny, finally)
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

withOpenAiProvider
    :: OpenAiConfig
    -> ProviderHost
    -> (ProviderRuntime -> IO a)
    -> IO a
withOpenAiProvider config@OpenAiConfig{tokenProvider} host use =
    withCodexWsWithProviderOrHttpFallback tokenProvider \conn credential ->
        withConnectedOpenAiProvider config host conn credential use

data AccountSwitchRequest
    = AccountSwitchRequest !Credential !(MVar (Either ApiError Text))

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
    :: OpenAiAccounts
    -> CodexConn
    -> Credential
    -> IO OpenAiProviderRuntime
newOpenAiProviderRuntime OpenAiAccounts{..} conn credential = do
    OpenAiConnectionState
        { openAiConnectionLock = wsLock
        , openAiConnectionRef =
            activeConnectionRef
        , openAiFallbackRef =
            httpFallbackActive
        } <- newOpenAiConnectionState conn credential
    switchRequests <-
        newChan :: IO (Chan AccountSwitchRequest)
    let selectableOpenAiPool = selectablePool
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
                                    resolveAccountLabel
                                        newCredential
                                writeIORef
                                    activeConnectionRef $
                                    OpenAiPersistentConnection
                                        newCredential
                                        newHealthy
                                        newConn
                                installAccount newCredential label
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
                        readActiveAccountId
                    let OpenAiPersistentConnection
                            _
                            oldHealthy
                            oldConn =
                                oldConnection
                    writeIORef oldHealthy False
                    closeCodexConn oldConn
                    preferAccount selectedCredential.accountId
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
                                preferAccount previousAccountId
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

withConnectedOpenAiProvider
    :: OpenAiConfig
    -> ProviderHost
    -> CodexConn
    -> Credential
    -> (ProviderRuntime -> IO a)
    -> IO a
withConnectedOpenAiProvider OpenAiConfig{..}
        ProviderHost{compaction = ProviderCompaction{..}, networkRecovery}
        conn credential use = do
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
            accounts
            conn
            credential
    let (compactSender, lockedBackend) =
            lockedOpenAiSession
                networkRecovery
                (isGatewayWebSocketCredential
                    credential)
                compactThreshold
                showRawReasoning
                wsLock
                httpFallbackActive
                tokenProvider
                activeConnectionRef
                (readSessionRequestParams paramsRef)
                contextTokensRef
                recordCompactionUsage
                (decorateCompactOutcomeWithTaskPlan
                    taskPlan)
                installAutomaticCompact
        btwBackend privateParams =
            freshOpenAiBackend
                showRawReasoning
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
                                    OpenAIProvider
                                    (Just tokenProvider)
                                    paramsRef
                                    historyRef
                                    requestedFocus
                                    >>= decorateManualCompact (readSessionRequestParams paramsRef) taskPlan
                                        (codexEffectiveContextWindowFor
                                            . (.model)))
                            focus
                resetCodexTurnState turnState
                runCompact `finally`
                    resetCodexTurnState turnState
    withAsync switchLoop \switchWorker -> do
        link switchWorker
        use ProviderRuntime
            { sessionBackend = SessionBackend
                { backend = lockedBackend
                , btwBackend
                , interruptBackend = pure ()
                , resetBackendState = do
                    OpenAiPersistentConnection
                        _credential
                        _connectionHealthy
                        activeConn <-
                            readIORef activeConnectionRef
                    resetCodexTurnState (codexConnTurnState activeConn)
                }
            , currentContextWindow = currentModelContextWindow transportModel
            , compactRunner
            , accountSelection = OpenAiAccountSelection selectableOpenAiPool selectAccount
            , subagents = CodexSubagents (isGatewayWebSocketCredential credential)
            }
