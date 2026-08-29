-- | Disposable OpenAI backends used by child agents.
module Agent.CLI.Subagents.Runtime.OpenAI
    ( freshOpenAiBackend
    , freshOpenAiBackendWithTurnState
    ) where

import Agent.Loop (Backend(..))
import Agent.OpenAI.LoopBackend (openAiBackendWithReasoningVisibility)
import Agent.OpenAI.WebSocketClient
    ( CodexTurnState
    , newCodexTurnState
    , sendWsRequestWithEvents
    , withCodexWsRetryingUsingTurnState
    )
import Agent.Provider (TokenProvider)
import Agent.Responses.Types (ResponseCreateParams)

-- | Each submission is its own logical turn with disposable connections.
freshOpenAiBackend
    :: Bool -> TokenProvider -> IO ResponseCreateParams -> Backend
freshOpenAiBackend showRawReasoning provider getParams =
    Backend \state previous inputs onEvent -> do
        turnState <- newCodexTurnState
        let Backend submit =
                freshOpenAiBackendWithTurnState
                    showRawReasoning turnState provider getParams
        submit state previous inputs onEvent

-- | Every request dials its own connection attached to the shared logical
-- turn. That includes the resubmission after a socket died mid-response: the
-- backend's reconnect policy simply calls the sender again, and a request that
-- reused the dead connection would fail immediately instead of recovering.
freshOpenAiBackendWithTurnState
    :: Bool -> CodexTurnState -> TokenProvider
    -> IO ResponseCreateParams -> Backend
freshOpenAiBackendWithTurnState showRawReasoning turnState provider getParams =
    openAiBackendWithReasoningVisibility
        showRawReasoning
        (\request previousResponseId onStreamEvent ->
            withCodexWsRetryingUsingTurnState provider turnState
                \conn _credential ->
                    sendWsRequestWithEvents conn request
                        previousResponseId onStreamEvent)
        getParams
