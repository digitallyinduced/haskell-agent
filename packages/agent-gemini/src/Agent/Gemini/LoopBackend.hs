-- | Adapter from native Gemini streaming events to the provider-neutral loop.
module Agent.Gemini.LoopBackend
    ( statelessGeminiBackend
    , tokenProviderStatelessGeminiBackend
    , geminiStreamEventToLoopEvent
    ) where

import Agent.Error (ApiError)
import Agent.Gemini.Response (GeminiStreamEvent(..))
import Agent.Loop
    ( Backend(..)
    , BackendResult(..)
    , BackendSnapshot(..)
    , LoopEvent(..)
    , advanceBackendSnapshot
    )
import Agent.Provider
    ( Credential
    , TokenProvider
    , runWithTokenProvider
    )
import Agent.Responses.LoopBackend
    ( responseItemToToolCall
    , responseToTurnOutput
    , turnInputsToItems
    , withRequestInput
    )
import Agent.Responses.Types
    ( FunctionCall
    , Response(..)
    , ResponseCreateParams
    , ResponseItem(..)
    )
import Agent.ToolDispatch (ToolCall)

statelessGeminiBackend
    :: (ResponseCreateParams
        -> (GeminiStreamEvent -> IO ())
        -> IO (Either ApiError Response))
    -> IO ResponseCreateParams
    -> Backend
statelessGeminiBackend send getParams =
    Backend \snapshot _previousResponseId inputs onEvent -> do
        baseParams <- getParams
        let newItems = turnInputsToItems inputs
            requestItems = snapshot.backendItems <> newItems
            request = withRequestInput baseParams requestItems
        result <- send request \event ->
            maybe (pure ()) onEvent (geminiStreamEventToLoopEvent event)
        case result of
            Left err -> pure (Left err)
            Right response ->
                pure $ Right BackendResult
                    { backendOutput = responseToTurnOutput response
                    , backendState =
                        advanceBackendSnapshot snapshot
                            (requestItems <> response.output)
                            Nothing
                    }

tokenProviderStatelessGeminiBackend
    :: TokenProvider
    -> (Credential
        -> ResponseCreateParams
        -> (GeminiStreamEvent -> IO ())
        -> IO (Either ApiError Response))
    -> IO ResponseCreateParams
    -> Backend
tokenProviderStatelessGeminiBackend provider send =
    statelessGeminiBackend \params onEvent ->
        runWithTokenProvider provider \credential ->
            send credential params onEvent

geminiStreamEventToLoopEvent
    :: GeminiStreamEvent
    -> Maybe LoopEvent
geminiStreamEventToLoopEvent = \case
    GeminiTextDelta text -> Just (TextDelta text)
    GeminiReasoningDelta text -> Just (ReasoningDelta text)
    GeminiFunctionCallReady call ->
        ToolStarted <$> functionCallToToolCall call

functionCallToToolCall :: FunctionCall -> Maybe ToolCall
functionCallToToolCall =
    responseItemToToolCall . FunctionCallItem
