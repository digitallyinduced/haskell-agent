-- | OpenAI persistent connection and serialized compaction backend.
module Agent.CLI.Provider.OpenAI
    ( OpenAiPersistentConnection(..)
    , lockedOpenAiSession
    ) where

import Agent.CLI.Compaction
    ( OpenAiCompactionSender
    , autoCompactOpenAiBackendWithSender
    )
import Agent.CLI.Connectivity (withConnectionRecovery)
import Agent.Loop
    ( Backend(..)
    , TokenUsage
    )
import Agent.OpenAI.LoopBackend
    ( openAiAuxiliaryResponseSenderReconnecting
    , openAiBackendWithReasoningVisibility
    , openAiBackendWithReasoningVisibilityAndToolSpeculation
    , openAiResponseSenderReconnecting
    )
import Agent.OpenAI.WebSocketClient (CodexConn)
import Agent.Provider
    ( Credential
    , TokenProvider
    )
import Agent.Responses.Types (ResponseCreateParams)
import Agent.Tools.Speculation (ToolSpeculationRuntime)
import Control.Concurrent.MVar
    ( MVar
    , withMVar
    )
import Data.IORef
    ( IORef
    , readIORef
    )

data OpenAiPersistentConnection
    = OpenAiPersistentConnection !Credential !(IORef Bool) !CodexConn

-- | Build a root OpenAI backend plus an unlocked sender for manual compaction.
-- Callers hold the same lock around the entire manual compact/install
-- transition. A normal logical turn holds it across automatic compaction and
-- its continuation because the active WebSocket is not multiplexed and
-- transcript mutation must not interleave between those two requests.
lockedOpenAiSession
    :: Maybe Int
    -> Bool
    -> Maybe ToolSpeculationRuntime
    -> MVar ()
    -> TokenProvider
    -> IORef OpenAiPersistentConnection
    -> IO ResponseCreateParams
    -> IORef (Maybe (Int, Int))
    -> (TokenUsage -> IO ())
    -> (OpenAiCompactionSender, Backend)
lockedOpenAiSession compactThreshold showRawReasoning toolSpeculation wsLock provider
        activeConnection getParams contextTokens
        recordCompactionUsage =
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
        baseBackend =
            withConnectionRecovery $
                case toolSpeculation of
                    Nothing ->
                        openAiBackendWithReasoningVisibility
                            showRawReasoning
                            sendResponse
                            getParams
                    Just speculation ->
                        openAiBackendWithReasoningVisibilityAndToolSpeculation
                            showRawReasoning
                            speculation
                            sendResponse
                            getParams
        compactSender request =
            sendAuxiliary request Nothing (const (pure ()))
        compactingBackend =
            autoCompactOpenAiBackendWithSender
                compactThreshold
                compactSender
                recordCompactionUsage
                getParams
                contextTokens
                baseBackend
        serializedBackend = Backend \state previous inputs onEvent ->
            withMVar wsLock \_ ->
                compactingBackend.submitTurn state previous inputs onEvent
    in (compactSender, serializedBackend)
