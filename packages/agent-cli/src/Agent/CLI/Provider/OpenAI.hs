-- | OpenAI persistent connection and serialized compaction backend.
module Agent.CLI.Provider.OpenAI
    ( OpenAiPersistentConnection(..)
    , lockedOpenAiSession
    ) where

import Agent.CLI.Compaction
    ( OpenAiCompactionSender
    , autoCompactOpenAiBackendWithSenderAndHook
    )
import Agent.CLI.Connectivity (withConnectionRecovery)
import Agent.Loop
    ( Backend(..)
    , TokenUsage
    )
import qualified Agent.OpenAI.Client as OpenAIClient
import Agent.OpenAI.LoopBackend
    ( isOpenAiReplayUnsafeWebSocketTransportFailure
    , isOpenAiWebSocketTransportFailure
    , openAiAuxiliaryResponseSenderReconnecting
    , openAiBackendWithReasoningVisibility
    , openAiBackendWithTransportFallback
    , openAiResponseSenderReconnecting
    , withCodexTurnStateScope
    )
import Agent.OpenAI.WebSocketClient
    ( CodexConn
    , codexConnTurnState
    )
import Agent.Provider
    ( Credential
    , TokenProvider
    )
import Agent.Responses.LoopBackend
    ( statelessResponsesBackendWithRawReasoning
    )
import Agent.Responses.Types (ResponseCreateParams)
import Control.Concurrent.MVar
    ( MVar
    , withMVar
    )
import Data.IORef
    ( IORef
    , readIORef
    , writeIORef
    )

data OpenAiPersistentConnection
    = OpenAiPersistentConnection !Credential !(IORef Bool) !CodexConn

-- | Build a root OpenAI backend plus an unlocked sender for manual compaction.
-- Callers hold the same lock around the entire manual compact/install
-- transition. A normal logical turn holds it across automatic compaction and
-- its continuation because the active WebSocket is not multiplexed and
-- transcript mutation must not interleave between those two requests.
lockedOpenAiSession
    :: Bool
    -- ^ When true, every model and compaction request must remain on the
    -- gateway WebSocket; direct provider HTTP fallback is forbidden.
    -> Maybe Int
    -> Bool
    -> MVar ()
    -> IORef Bool
    -> TokenProvider
    -> IORef OpenAiPersistentConnection
    -> IO ResponseCreateParams
    -> IORef (Maybe (Int, Int))
    -> (TokenUsage -> IO ())
    -> IO ()
    -> (OpenAiCompactionSender, Backend)
lockedOpenAiSession gatewayOnly compactThreshold showRawReasoning wsLock fallbackActive
        provider activeConnection getParams contextTokens
        recordCompactionUsage onCompacted =
    let sendResponse request previousResponseId onEvent = do
            OpenAiPersistentConnection
                credential
                connectionHealthy
                conn <-
                    readIORef activeConnection
            openAiResponseSenderReconnecting
                provider
                credential
                connectionHealthy
                conn
                request
                previousResponseId
                onEvent
        sendAuxiliary request previousResponseId onEvent = do
            OpenAiPersistentConnection
                credential
                connectionHealthy
                conn <-
                    readIORef activeConnection
            openAiAuxiliaryResponseSenderReconnecting
                provider
                credential
                connectionHealthy
                conn
                request
                previousResponseId
                onEvent
        getTurnState = do
            OpenAiPersistentConnection _credential _connectionHealthy conn <-
                readIORef activeConnection
            pure (codexConnTurnState conn)
        sendHttp request = do
            turnState <- getTurnState
            OpenAIClient.createCodexMessageWithProviderWithTurnState
                turnState provider request
        sendHttpCompaction request = do
            turnState <- getTurnState
            OpenAIClient.createCodexMessageWithProviderWithOptionsAndTurnState
                OpenAIClient.remoteCompactionV2RequestOptions
                turnState
                provider
                request
        websocketBackend =
            openAiBackendWithReasoningVisibility
                showRawReasoning
                sendResponse
                getParams
        httpFallbackBackend =
            statelessResponsesBackendWithRawReasoning
                showRawReasoning
                (\request _onEvent -> sendHttp request)
                getParams
        baseBackend =
            withConnectionRecovery $
                if gatewayOnly
                    then websocketBackend
                    else
                        openAiBackendWithTransportFallback
                            fallbackActive
                            websocketBackend
                            httpFallbackBackend
        compactSender request = do
            active <- readIORef fallbackActive
            if active && not gatewayOnly
                then sendHttpCompaction request
                else do
                    result <-
                        sendAuxiliary request Nothing (const (pure ()))
                    case result of
                        Left err
                            | not gatewayOnly
                            , isOpenAiReplayUnsafeWebSocketTransportFailure err -> do
                                -- The socket is dead, so route later work over
                                -- HTTP. Do not replay this compaction: an
                                -- opaque checkpoint already arrived and the
                                -- provider may have committed/billed it.
                                writeIORef fallbackActive True
                                pure result
                            | not gatewayOnly
                            , isOpenAiWebSocketTransportFailure err -> do
                                writeIORef fallbackActive True
                                sendHttpCompaction request
                        _ -> pure result
        compactingBackend =
            autoCompactOpenAiBackendWithSenderAndHook
                compactThreshold
                compactSender
                recordCompactionUsage
                getParams
                onCompacted
                contextTokens
                baseBackend
        turnScopedBackend =
            withCodexTurnStateScope getTurnState compactingBackend
        serializedBackend = Backend \state previous inputs onEvent ->
            withMVar wsLock \_ ->
                turnScopedBackend.submitTurn state previous inputs onEvent
    in (compactSender, serializedBackend)
