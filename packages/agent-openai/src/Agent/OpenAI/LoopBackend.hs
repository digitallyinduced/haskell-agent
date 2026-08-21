-- | Map the provider-neutral loop onto the OpenAI Responses WebSocket transport.
module Agent.OpenAI.LoopBackend
    ( openAiBackend
    , openAiBackendReconnecting
    , openAiBackendWith
    , openAiBackendWithRetryPolicy
    , openAiBackendWithConnectionRecovery
    , statelessResponsesBackend
    , turnInputsToItems
    , responseToTurnOutput
    , streamEventToLoopEvent
    , assistantTextFromResponse
    , toolResultToItem
    , withRequestInput
    ) where

import Agent.Error
    ( ApiError(..)
    , isInlineRetryableProviderResponseError
    )
import Agent.Loop (Backend(..))
import Agent.OpenAI.Error (isPreviousResponseIdError)
import Agent.OpenAI.WebSocketClient
    ( CodexConn
    , sendWsRequestWithEvents
    , withCodexWsRetrying
    )
import Agent.Provider (TokenProvider)
import Agent.Responses.LoopBackend
    ( assistantTextFromResponse
    , responseToTurnOutput
    , statelessResponsesBackend
    , streamEventToLoopEvent
    , toolResultToItem
    , turnInputsToItems
    , withRequestInput
    )
import Agent.Responses.Types
import Control.Retry
    ( RetryPolicyM
    , exponentialBackoff
    , limitRetries
    , retrying
    )
import Data.IORef
import Data.Maybe (isJust)
import Data.Text (Text)

-- | Close over a live Codex WebSocket and the request fields the loop does
-- not own (model, instructions, tools, reasoning). The params action is
-- re-run each turn so the REPL can change reasoning effort in place.
--
-- The wire protocol still sends only new items plus @previous_response_id@.
-- The shared 'IORef' mirrors the full transcript locally so sessions can be
-- persisted and resumed when the server-side chain is gone.
openAiBackend
    :: CodexConn
    -> IO ResponseCreateParams
    -> IORef [ResponseItem]
    -> Backend
openAiBackend conn =
    openAiBackendWith \request previousResponseId onEvent ->
        sendWsRequestWithEvents conn request previousResponseId onEvent

-- | Reuse the session WebSocket while it is healthy, reconnecting after it dies.
openAiBackendReconnecting
    :: TokenProvider
    -> IORef Bool
    -> CodexConn
    -> IO ResponseCreateParams
    -> IORef [ResponseItem]
    -> Backend
openAiBackendReconnecting provider connectionHealthy conn =
    openAiBackendWithConnectionRecovery connectionHealthy sendCurrent sendFresh
  where
    sendCurrent request previousResponseId onEvent =
        sendWsRequestWithEvents conn request previousResponseId onEvent
    sendFresh request previousResponseId onEvent =
        withCodexWsRetrying provider
            (sendOnFresh request previousResponseId onEvent)
    sendOnFresh request previousResponseId onEvent freshConn _credential =
        sendWsRequestWithEvents freshConn request previousResponseId onEvent

-- | Injectable connection recovery used by the reconnecting backend.
openAiBackendWithConnectionRecovery
    :: IORef Bool
    -> (ResponseCreateParams
        -> Maybe Text
        -> (ResponseStreamEvent -> IO ())
        -> IO (Either ApiError Response))
    -> (ResponseCreateParams
        -> Maybe Text
        -> (ResponseStreamEvent -> IO ())
        -> IO (Either ApiError Response))
    -> IO ResponseCreateParams
    -> IORef [ResponseItem]
    -> Backend
openAiBackendWithConnectionRecovery connectionHealthy sendCurrent sendFresh =
    openAiBackendWith sendWithRecovery
  where
    sendWithRecovery request previousResponseId onEvent = do
        healthy <- readIORef connectionHealthy
        if healthy
            then tryCurrent request previousResponseId onEvent
            else sendFresh request previousResponseId onEvent

    tryCurrent request previousResponseId onEvent = do
        emittedLoopEvent <- newIORef False
        result <- sendCurrent request previousResponseId
            (trackOutput emittedLoopEvent onEvent)
        case result of
            Left ConnectionError {} -> do
                writeIORef connectionHealthy False
                emitted <- readIORef emittedLoopEvent
                if emitted
                    then pure result
                    else sendFresh request previousResponseId onEvent
            _ -> pure result

    trackOutput emittedLoopEvent onEvent event = do
        if isJust (streamEventToLoopEvent event)
            then writeIORef emittedLoopEvent True
            else pure ()
        onEvent event

-- | Same mapping as 'openAiBackend', with an injectable transport for tests.
openAiBackendWith
    :: (ResponseCreateParams
        -> Maybe Text
        -> (ResponseStreamEvent -> IO ())
        -> IO (Either ApiError Response))
    -> IO ResponseCreateParams
    -> IORef [ResponseItem]
    -> Backend
openAiBackendWith =
    openAiBackendWithRetryPolicy transientStreamingResultPolicy

-- | Streaming retries are replay-safe only until the loop has observed output.
-- Server error events themselves are not loop-visible, so transient Codex
-- failures can wait and retry without printing an error or duplicating output.
openAiBackendWithRetryPolicy
    :: RetryPolicyM IO
    -> (ResponseCreateParams
        -> Maybe Text
        -> (ResponseStreamEvent -> IO ())
        -> IO (Either ApiError Response))
    -> IO ResponseCreateParams
    -> IORef [ResponseItem]
    -> Backend
openAiBackendWithRetryPolicy retryPolicy send getParams transcript =
    Backend \previousResponseId inputs onEvent -> do
        baseParams <- getParams
        history <- readIORef transcript
        let newItems = turnInputsToItems inputs
            deltaRequest = withRequestInput baseParams newItems
            fullRequest = withRequestInput baseParams (history <> newItems)
            emit event = mapM_ onEvent (streamEventToLoopEvent event)
            (initialRequest, initialPrevious) =
                case previousResponseId of
                    Nothing | not (null history) -> (fullRequest, Nothing)
                    _ -> (deltaRequest, previousResponseId)
        result <- sendRetrying initialRequest initialPrevious emit
        recovered <- case result of
            Left err
                | isPreviousResponseIdError err && not (null history) ->
                    sendRetrying fullRequest Nothing emit
                | otherwise -> pure (Left err)
            Right response -> pure (Right response)
        case recovered of
            Left err -> pure (Left err)
            Right response -> do
                writeIORef transcript (history <> newItems <> response.output)
                pure (Right (responseToTurnOutput response))
  where
    sendRetrying request previousResponseId onEvent = do
        emittedLoopEvent <- newIORef False
        retrying retryPolicy (shouldRetry emittedLoopEvent) \_status ->
            send request previousResponseId \event -> do
                if isJust (streamEventToLoopEvent event)
                    then writeIORef emittedLoopEvent True
                    else pure ()
                onEvent event

    shouldRetry emittedLoopEvent _retryStatus = \case
        Left apiError
            | isInlineRetryableProviderResponseError apiError ->
                not <$> readIORef emittedLoopEvent
        _ -> pure False

transientStreamingResultPolicy :: RetryPolicyM IO
transientStreamingResultPolicy =
    exponentialBackoff 5_000_000 <> limitRetries 3
