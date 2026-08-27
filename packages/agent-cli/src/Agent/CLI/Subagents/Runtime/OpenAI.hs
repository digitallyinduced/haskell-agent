-- | Disposable OpenAI backends used by child agents.
module Agent.CLI.Subagents.Runtime.OpenAI
    ( freshOpenAiBackend
    , freshOpenAiBackendWithTurnState
    ) where

import Agent.Loop (Backend(..))
import Agent.OpenAI.LoopBackend
    ( openAiBackendWithRawReasoning
    , openAiBackendWithReasoningVisibility
    )
import Agent.OpenAI.WebSocketClient
    ( CodexTurnState
    , sendWsRequestWithEvents
    , withCodexWsRetrying
    , withCodexWsRetryingUsingTurnState
    )
import Agent.Provider (TokenProvider)
import Agent.Responses.Types (ResponseCreateParams)

freshOpenAiBackend
    :: Bool -> TokenProvider -> IO ResponseCreateParams -> Backend
freshOpenAiBackend showRawReasoning provider getParams =
    Backend \state previous inputs onEvent ->
        withCodexWsRetrying provider \conn _credential ->
            let Backend submit =
                    openAiBackendWithRawReasoning
                        showRawReasoning conn getParams
            in submit state previous inputs onEvent

freshOpenAiBackendWithTurnState
    :: Bool -> CodexTurnState -> TokenProvider
    -> IO ResponseCreateParams -> Backend
freshOpenAiBackendWithTurnState showRawReasoning turnState provider getParams =
    Backend \state previous inputs onEvent ->
        withCodexWsRetryingUsingTurnState provider turnState
            \conn _credential ->
                let Backend submit =
                        openAiBackendWithReasoningVisibility
                            showRawReasoning
                            (\request previousResponseId onStreamEvent ->
                                sendWsRequestWithEvents conn request
                                    previousResponseId onStreamEvent)
                            getParams
                in submit state previous inputs onEvent
