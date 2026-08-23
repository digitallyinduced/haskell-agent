-- | Map the provider-neutral loop onto the OpenAI Responses WebSocket transport.
--
-- Provider-neutral conversion and stateless transport helpers live in
-- "Agent.Responses.LoopBackend" and are re-exported here for compatibility.
module Agent.OpenAI.LoopBackend
    ( openAiBackend
    , openAiBackendReconnecting
    , openAiAuxiliaryResponseSenderReconnecting
    , openAiAuxiliaryResponseSenderWithConnectionRecovery
    , openAiResponseSenderReconnecting
    , openAiResponseSenderWithConnectionRecoveryWhen
    , openAiResponseSenderWithConnectionRecovery
    , openAiResponseSenderWithRetryPolicy
    , openAiBackendWith
    , openAiBackendWithRetryPolicy
    , openAiBackendWithConnectionRecovery
    , openAiBackendWithTransportFallback
    , statelessResponsesBackend
    , turnInputsToItems
    , responseToTurnOutput
    , responseTokenUsage
    , streamEventToLoopEvent
    , assistantTextFromResponse
    , toolResultToItem
    , withRequestInput
    ) where

import Agent.Error
    ( ApiError(..)
    , ErrorType(..)
    , isInlineRetryableProviderError
    , isInlineRetryableProviderResponseError
    )
import Agent.Loop (Backend(..), LoopEvent(..))
import Agent.OpenAI.Error (isPreviousResponseIdError)
import Agent.OpenAI.WebSocketClient
    ( CodexConn
    , sendWsRequestWithEvents
    , withCodexWsRetrying
    , withCodexWsRetryingAfter
    )
import Agent.Provider
    ( Credential
    , FailedCredential(..)
    , TokenProvider
    , accountFailureFromApiError
    )
import Agent.Responses.LoopBackend
    ( assistantTextFromResponse
    , responseToTurnOutput
    , responseTokenUsage
    , statelessResponsesBackend
    , streamEventToLoopEvent
    , toolResultToItem
    , turnInputsToItems
    , withRequestInput
    )
import Agent.Responses.Types
import Control.Concurrent (threadDelay)
import Control.Exception.Safe (onException)
import Control.Retry
    ( RetryPolicyM
    , applyPolicy
    , defaultRetryStatus
    , exponentialBackoff
    , limitRetries
    , rsIterNumber
    , rsPreviousDelay
    )
import Data.IORef
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as Text

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
    -> Credential
    -> IORef Bool
    -> CodexConn
    -> IO ResponseCreateParams
    -> IORef [ResponseItem]
    -> Backend
openAiBackendReconnecting provider currentCredential connectionHealthy conn =
    openAiBackendWith
        (openAiResponseSenderReconnecting
            provider
            currentCredential
            connectionHealthy
            conn)

-- | Send a Responses request through the active Codex WebSocket while it is
-- healthy, with the same replay-safe fresh-connection recovery used by the
-- normal model backend. Callers can reuse this for auxiliary model requests
-- such as remote compaction without opening a separate REST session/account.
openAiResponseSenderReconnecting
    :: TokenProvider
    -> Credential
    -> IORef Bool
    -> CodexConn
    -> ResponseCreateParams
    -> Maybe Text
    -> (ResponseStreamEvent -> IO ())
    -> IO (Either ApiError Response)
openAiResponseSenderReconnecting =
    openAiResponseSenderReconnectingWhen
        (isJust . streamEventToLoopEvent)
        markLoopReplayUnsafe

-- | Auxiliary Responses requests are not rendered to the loop, so their
-- replay boundary also includes completed output items such as an opaque
-- compaction checkpoint. Replaying after one of those items arrived could
-- bill the request twice even though no text delta was visible.
openAiAuxiliaryResponseSenderReconnecting
    :: TokenProvider
    -> Credential
    -> IORef Bool
    -> CodexConn
    -> ResponseCreateParams
    -> Maybe Text
    -> (ResponseStreamEvent -> IO ())
    -> IO (Either ApiError Response)
openAiAuxiliaryResponseSenderReconnecting provider currentCredential
        connectionHealthy conn =
    openAiResponseSenderWithRetryPolicy
        transientStreamingResultPolicy
        auxiliaryOutputObserved
        (openAiResponseSenderReconnectingWhen
            auxiliaryOutputObserved
            markAuxiliaryReplayUnsafe
            provider
            currentCredential
            connectionHealthy
            conn)

openAiResponseSenderReconnectingWhen
    :: (ResponseStreamEvent -> Bool)
    -> (ApiError -> ApiError)
    -> TokenProvider
    -> Credential
    -> IORef Bool
    -> CodexConn
    -> ResponseCreateParams
    -> Maybe Text
    -> (ResponseStreamEvent -> IO ())
    -> IO (Either ApiError Response)
openAiResponseSenderReconnectingWhen observed markObservedFailure provider
        currentCredential connectionHealthy conn =
    openAiResponseSenderWithConnectionRecoveryUsing
        observed
        markObservedFailure
        connectionHealthy
        sendCurrent
        sendFresh
  where
    sendCurrent request previousResponseId onEvent =
        sendWsRequestWithEvents conn request previousResponseId onEvent
    sendFresh previousFailure request previousResponseId onEvent =
        case previousFailure >>= accountFailureFromApiError of
            Just failure ->
                withCodexWsRetryingAfter provider
                    FailedCredential
                        { credential = currentCredential
                        , failure
                        }
                    (sendOnFresh request previousResponseId onEvent)
            Nothing ->
                withCodexWsRetrying provider
                    (sendOnFresh request previousResponseId onEvent)
    sendOnFresh request previousResponseId onEvent freshConn _credential =
        do
            emittedOutput <- newIORef False
            result <-
                sendWsRequestWithEvents freshConn request previousResponseId
                    \event -> do
                        if observed event
                            then writeIORef emittedOutput True
                            else pure ()
                        onEvent event
            emitted <- readIORef emittedOutput
            pure $ case result of
                Left err | emitted -> Left (markObservedFailure err)
                _ -> result

-- | Injectable connection recovery used by the reconnecting backend.
openAiBackendWithConnectionRecovery
    :: IORef Bool
    -> (ResponseCreateParams
        -> Maybe Text
        -> (ResponseStreamEvent -> IO ())
        -> IO (Either ApiError Response))
    -> (Maybe ApiError
        -> ResponseCreateParams
        -> Maybe Text
        -> (ResponseStreamEvent -> IO ())
        -> IO (Either ApiError Response))
    -> IO ResponseCreateParams
    -> IORef [ResponseItem]
    -> Backend
openAiBackendWithConnectionRecovery connectionHealthy sendCurrent sendFresh =
    openAiBackendWith $
        openAiResponseSenderWithConnectionRecovery
            connectionHealthy
            sendCurrent
            sendFresh

openAiResponseSenderWithConnectionRecovery
    :: IORef Bool
    -> (ResponseCreateParams
        -> Maybe Text
        -> (ResponseStreamEvent -> IO ())
        -> IO (Either ApiError Response))
    -> (Maybe ApiError
        -> ResponseCreateParams
        -> Maybe Text
        -> (ResponseStreamEvent -> IO ())
        -> IO (Either ApiError Response))
    -> ResponseCreateParams
    -> Maybe Text
    -> (ResponseStreamEvent -> IO ())
    -> IO (Either ApiError Response)
openAiResponseSenderWithConnectionRecovery =
    openAiResponseSenderWithConnectionRecoveryUsing
        (isJust . streamEventToLoopEvent)
        markLoopReplayUnsafe

openAiAuxiliaryResponseSenderWithConnectionRecovery
    :: IORef Bool
    -> (ResponseCreateParams
        -> Maybe Text
        -> (ResponseStreamEvent -> IO ())
        -> IO (Either ApiError Response))
    -> (Maybe ApiError
        -> ResponseCreateParams
        -> Maybe Text
        -> (ResponseStreamEvent -> IO ())
        -> IO (Either ApiError Response))
    -> ResponseCreateParams
    -> Maybe Text
    -> (ResponseStreamEvent -> IO ())
    -> IO (Either ApiError Response)
openAiAuxiliaryResponseSenderWithConnectionRecovery =
    openAiResponseSenderWithConnectionRecoveryUsing
        auxiliaryOutputObserved
        markAuxiliaryReplayUnsafe

-- | Connection recovery with an explicit definition of when a request has
-- produced enough output that replaying it is no longer safe.
openAiResponseSenderWithConnectionRecoveryWhen
    :: (ResponseStreamEvent -> Bool)
    -> IORef Bool
    -> (ResponseCreateParams
        -> Maybe Text
        -> (ResponseStreamEvent -> IO ())
        -> IO (Either ApiError Response))
    -> (Maybe ApiError
        -> ResponseCreateParams
        -> Maybe Text
        -> (ResponseStreamEvent -> IO ())
        -> IO (Either ApiError Response))
    -> ResponseCreateParams
    -> Maybe Text
    -> (ResponseStreamEvent -> IO ())
    -> IO (Either ApiError Response)
openAiResponseSenderWithConnectionRecoveryWhen observed =
    openAiResponseSenderWithConnectionRecoveryUsing
        observed
        markLoopReplayUnsafe

openAiResponseSenderWithConnectionRecoveryUsing
    :: (ResponseStreamEvent -> Bool)
    -> (ApiError -> ApiError)
    -> IORef Bool
    -> (ResponseCreateParams
        -> Maybe Text
        -> (ResponseStreamEvent -> IO ())
        -> IO (Either ApiError Response))
    -> (Maybe ApiError
        -> ResponseCreateParams
        -> Maybe Text
        -> (ResponseStreamEvent -> IO ())
        -> IO (Either ApiError Response))
    -> ResponseCreateParams
    -> Maybe Text
    -> (ResponseStreamEvent -> IO ())
    -> IO (Either ApiError Response)
openAiResponseSenderWithConnectionRecoveryUsing observed markObservedFailure
        connectionHealthy sendCurrent sendFresh =
    sendWithRecovery
  where
    sendWithRecovery request previousResponseId onEvent = do
        healthy <- readIORef connectionHealthy
        if healthy
            then tryCurrent request previousResponseId onEvent
            else sendFreshTracked Nothing request previousResponseId onEvent

    tryCurrent request previousResponseId onEvent = do
        emittedLoopEvent <- newIORef False
        result <- sendCurrent request previousResponseId
            (trackOutput emittedLoopEvent onEvent)
            `onException` writeIORef connectionHealthy False
        case result of
            Left err -> do
                let deadConnectionOrAccount =
                        isDeadConnectionOrAccount err
                if deadConnectionOrAccount
                    then writeIORef connectionHealthy False
                    else pure ()
                emitted <- readIORef emittedLoopEvent
                if emitted
                    then pure (Left (markObservedFailure err))
                    else if deadConnectionOrAccount
                        then sendFreshTracked
                            (Just err)
                            request
                            previousResponseId
                            onEvent
                        else pure result
            _ -> pure result

    sendFreshTracked previousFailure request previousResponseId onEvent = do
        emittedOutput <- newIORef False
        result <-
            sendFresh previousFailure request previousResponseId
                (trackOutput emittedOutput onEvent)
        emitted <- readIORef emittedOutput
        pure $ case result of
            Left err | emitted -> Left (markObservedFailure err)
            _ -> result

    trackOutput emittedLoopEvent onEvent event = do
        if observed event
            then writeIORef emittedLoopEvent True
            else pure ()
        onEvent event

    isDeadConnectionOrAccount = \case
        ConnectionError {} -> True
        ProviderError WebSocketConnectionLimitReached _ _ -> True
        err -> isJust (accountFailureFromApiError err)

-- | Retry transient provider responses only while a request has produced no
-- output that would make replay unsafe.
openAiResponseSenderWithRetryPolicy
    :: RetryPolicyM IO
    -> (ResponseStreamEvent -> Bool)
    -> (ResponseCreateParams
        -> Maybe Text
        -> (ResponseStreamEvent -> IO ())
        -> IO (Either ApiError Response))
    -> ResponseCreateParams
    -> Maybe Text
    -> (ResponseStreamEvent -> IO ())
    -> IO (Either ApiError Response)
openAiResponseSenderWithRetryPolicy retryPolicy observed send
        request previousResponseId onEvent = do
    emittedOutput <- newIORef False
    go emittedOutput defaultRetryStatus
  where
    go emittedOutput retryStatus = do
        result <- send request previousResponseId \event -> do
            if observed event
                then writeIORef emittedOutput True
                else pure ()
            onEvent event
        emitted <- readIORef emittedOutput
        case result of
            Left apiError
                | not emitted
                , isInlineRetryableProviderError apiError ->
                    applyPolicy retryPolicy retryStatus >>= \case
                        Nothing -> pure result
                        Just nextStatus -> do
                            threadDelay
                                (fromMaybe 0 nextStatus.rsPreviousDelay)
                            go emittedOutput nextStatus
            _ -> pure result

auxiliaryOutputObserved :: ResponseStreamEvent -> Bool
auxiliaryOutputObserved = \case
    ResponseCompletedEvent{} -> True
    ResponseDoneEvent{} -> True
    ResponseFailedEvent{response} -> not (null response.output)
    ResponseIncompleteEvent{} -> True
    ResponseOutputItemAddedEvent{} -> True
    ResponseOutputItemDoneEvent{} -> True
    event -> isJust (streamEventToLoopEvent event)

markLoopReplayUnsafe :: ApiError -> ApiError
markLoopReplayUnsafe err
    | isJust (accountFailureFromApiError err) =
        replayUnsafeError "model output" err
    | otherwise = err

markAuxiliaryReplayUnsafe :: ApiError -> ApiError
markAuxiliaryReplayUnsafe =
    replayUnsafeError "auxiliary response output"

replayUnsafeError :: Text -> ApiError -> ApiError
replayUnsafeError outputLabel err =
    if isReplayUnsafeError err
        then err
        else ProviderError (UnknownErrorType "replay_unsafe")
            ( "provider failed after "
                <> outputLabel
                <> "; refusing to replay: "
                <> Text.pack (show err)
            )
            Nothing

isReplayUnsafeError :: ApiError -> Bool
isReplayUnsafeError = \case
    ProviderError (UnknownErrorType errorType) _ _ ->
        errorType == "replay_unsafe"
    _ -> False

-- | Prefer a WebSocket backend, then switch this agent session permanently to
-- a fallback transport when the socket dies before exposing model output.
--
-- Independent agent sessions need their own WebSocket connections. The Codex
-- endpoint can accept the upgrade and then close a fresh connection before its
-- first response frame. Replaying the same logical turn over the stateless HTTP
-- backend is safe while no text or reasoning delta has reached the caller.
openAiBackendWithTransportFallback
    :: IORef Bool
    -> Backend
    -> Backend
    -> Backend
openAiBackendWithTransportFallback fallbackActive primary fallback =
    Backend \previousResponseId inputs onEvent -> do
        active <- readIORef fallbackActive
        if active
            then fallback.submitTurn previousResponseId inputs onEvent
            else tryPrimary previousResponseId inputs onEvent
  where
    tryPrimary previousResponseId inputs onEvent = do
        emittedModelOutput <- newIORef False
        result <- primary.submitTurn previousResponseId inputs \event -> do
            if isModelOutput event
                then writeIORef emittedModelOutput True
                else pure ()
            onEvent event
        case result of
            Left err
                | isWebSocketTransportFailure err -> do
                    writeIORef fallbackActive True
                    emitted <- readIORef emittedModelOutput
                    if emitted
                        then pure result
                        else fallback.submitTurn previousResponseId inputs onEvent
            _ -> pure result

    isModelOutput = \case
        TextDelta {} -> True
        ReasoningDelta {} -> True
        _ -> False

    isWebSocketTransportFailure = \case
        ConnectionError {} -> True
        ProviderError WebSocketConnectionLimitReached _ _ -> True
        _ -> False

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
    Backend \previousResponseId inputs onLoopEvent -> do
        baseParams <- getParams
        history <- readIORef transcript
        let newItems = turnInputsToItems inputs
            deltaRequest = withRequestInput baseParams newItems
            -- Live and resumed transcripts already apply compaction snapshots
            -- as full replacements. Remote v2 intentionally keeps retained
            -- messages before its opaque checkpoint, so replay the complete
            -- replacement instead of trimming that retained prefix.
            fullRequest = withRequestInput baseParams (history <> newItems)
            emit event = mapM_ onLoopEvent (streamEventToLoopEvent event)
            (initialRequest, initialPrevious) =
                case previousResponseId of
                    Nothing | not (null history) -> (fullRequest, Nothing)
                    _ -> (deltaRequest, previousResponseId)
        result <- sendRetrying onLoopEvent initialRequest initialPrevious emit
        recovered <- case result of
            Left err
                | isPreviousResponseIdError err && not (null history) ->
                    sendRetrying onLoopEvent fullRequest Nothing emit
                | otherwise -> pure (Left err)
            Right response -> pure (Right response)
        case recovered of
            Left err -> pure (Left err)
            Right response -> do
                writeIORef transcript (history <> newItems <> response.output)
                pure (Right (responseToTurnOutput response))
  where
    sendRetrying onLoopEvent request previousResponseId onStreamEvent = do
        emittedLoopEvent <- newIORef False
        go emittedLoopEvent defaultRetryStatus
      where
        go emittedLoopEvent retryStatus = do
            result <- send request previousResponseId \event -> do
                if isJust (streamEventToLoopEvent event)
                    then writeIORef emittedLoopEvent True
                    else pure ()
                onStreamEvent event
            emitted <- readIORef emittedLoopEvent
            case result of
                Left apiError
                    | not emitted
                    , isInlineRetryableProviderResponseError apiError ->
                        applyPolicy retryPolicy retryStatus >>= \case
                            Nothing -> pure result
                            Just nextStatus -> do
                                let delayMicros =
                                        fromMaybe 0 nextStatus.rsPreviousDelay
                                    attempt = nextStatus.rsIterNumber
                                onLoopEvent $ ActivityUpdated $
                                    formatRetryScheduled apiError attempt delayMicros
                                threadDelay delayMicros
                                onLoopEvent $ ActivityUpdated $
                                    "Retrying Codex request (attempt "
                                        <> Text.pack (show attempt) <> ")…"
                                go emittedLoopEvent nextStatus
                _ -> pure result

transientStreamingResultPolicy :: RetryPolicyM IO
transientStreamingResultPolicy =
    exponentialBackoff 5_000_000 <> limitRetries 3

formatRetryScheduled :: ApiError -> Int -> Int -> Text
formatRetryScheduled apiError attempt delayMicros =
    retryReason apiError
        <> "; retrying in "
        <> Text.pack (show delaySeconds)
        <> "s (attempt "
        <> Text.pack (show attempt)
        <> ")…"
  where
    delaySeconds
        | delayMicros <= 0 = 0
        | otherwise = (delayMicros + 999_999) `div` 1_000_000

    retryReason = \case
        ProviderError OverloadedError _ _ -> "Codex is overloaded"
        ProviderError ServiceUnavailableError _ _ -> "Codex is unavailable"
        ProviderError WebSocketConnectionLimitReached _ _ ->
            "Codex connection limit reached"
        _ -> "Codex server error"
