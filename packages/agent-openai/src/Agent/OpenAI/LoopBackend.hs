-- | Map the provider-neutral loop onto the OpenAI Responses WebSocket transport.
--
-- Provider-neutral conversion and stateless transport helpers live in
-- "Agent.Responses.LoopBackend" and are re-exported here for compatibility.
module Agent.OpenAI.LoopBackend
    ( openAiBackend
    , openAiBackendWithRawReasoning
    , openAiBackendReconnecting
    , openAiAuxiliaryResponseSenderReconnecting
    , openAiAuxiliaryResponseSenderWithConnectionRecovery
    , openAiResponseSenderReconnecting
    , openAiResponseSenderWithConnectionRecoveryWhen
    , openAiResponseSenderWithConnectionRecovery
    , openAiResponseSenderWithRetryPolicy
    , openAiBackendWith
    , openAiBackendWithReasoningVisibility
    , openAiBackendWithRetryPolicy
    , openAiBackendWithRetryPolicies
    , connectionReplayPolicy
    , openAiBackendWithConnectionRecovery
    , openAiBackendWithTransportFallback
    , withCodexTurnStateScope
    , isOpenAiWebSocketTransportFailure
    , isOpenAiReplayUnsafeWebSocketTransportFailure
    , statelessResponsesBackend
    , turnInputsToItems
    , responseToTurnOutput
    , responseTokenUsage
    , streamEventToLoopEvent
    , assistantTextFromResponse
    , toolResultToItem
    , withRequestInput
    , normalizeResponseInputItems
    ) where

import Agent.Error
    ( ApiError(..)
    , ErrorType(..)
    , isInlineRetryableProviderError
    , isInlineRetryableProviderResponseError
    )
import qualified Agent.Responses.LoopBackend as Responses
import Agent.Loop
    ( Backend(..)
    , BackendContinuation(..)
    , BackendResult(..)
    , BackendSnapshot(..)
    , LoopEvent(..)
    , TurnInput(..)
    , advanceBackendSnapshot
    , backendContinuationToken
    )
import Agent.OpenAI.Error (isResponseChainCompatibilityError)
import Agent.OpenAI.Request (sanitizeCodexRequest)
import Agent.OpenAI.WebSocketClient
    ( CodexConn
    , CodexTurnState
    , codexConnTurnState
    , copyCodexTurnState
    , sendWsRequestWithEvents
    , sendWsRequestWithEventsPreservingTurnState
    , resetCodexTurnState
    , withCodexWsRetrying
    , withCodexWsRetryingAfter
    )
import Agent.Provider
    ( Credential
    , FailedCredential(..)
    , TokenProvider
    , accountFailureFromApiError
    , accountFailureReason
    )
import Agent.Responses.LoopBackend
    ( assistantTextFromResponse
    , responseToTurnOutput
    , responseTokenUsage
    , streamOutputObserved
    , toolResultToItem
    , turnInputsToItems
    , withRequestInput
    , normalizeResponseInputItems
    )
import Agent.Responses.Types
import Agent.ToolDispatch (ToolCallKind(..), ToolCallResult(..))
import qualified Agent.Transport.WebSocket as WebSocket
import Control.Concurrent (threadDelay)
import Control.Applicative ((<|>))
import Control.Exception.Safe (onException)
import Control.Monad (when)
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
-- The loop threads the full transcript so sessions can be persisted and
-- resumed when the server-side chain is gone.
openAiBackend
    :: CodexConn
    -> IO ResponseCreateParams
    -> Backend
openAiBackend conn getParams =
    withCodexTurnStateScope (pure (codexConnTurnState conn)) $
        openAiBackendWith
            (\request previousResponseId onEvent ->
                sendWsRequestWithEvents conn request previousResponseId onEvent)
            getParams

-- | OpenAI backend with explicit raw-reasoning visibility. Normal Codex
-- sessions should use 'openAiBackend', which displays summaries only.
openAiBackendWithRawReasoning
    :: Bool
    -> CodexConn
    -> IO ResponseCreateParams
    -> Backend
openAiBackendWithRawReasoning showRawReasoning conn getParams =
    withCodexTurnStateScope (pure (codexConnTurnState conn)) $
        openAiBackendWithReasoningVisibility showRawReasoning
            (\request previousResponseId onEvent ->
                sendWsRequestWithEvents conn request previousResponseId onEvent)
            getParams

-- | Reuse the session WebSocket while it is healthy, reconnecting after it dies.
openAiBackendReconnecting
    :: TokenProvider
    -> Credential
    -> IORef Bool
    -> CodexConn
    -> IO ResponseCreateParams
    -> Backend
openAiBackendReconnecting provider currentCredential connectionHealthy conn
        getParams =
    withCodexTurnStateScope (pure (codexConnTurnState conn)) $
        openAiBackendWith
            (openAiResponseSenderReconnecting
                provider
                currentCredential
                connectionHealthy
                conn)
            getParams

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
        sendWsRequestWithEvents
        streamOutputObserved
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
            sendWsRequestWithEventsPreservingTurnState
            auxiliaryOutputObserved
            markAuxiliaryReplayUnsafe
            provider
            currentCredential
            connectionHealthy
            conn)

openAiResponseSenderReconnectingWhen
    :: (CodexConn
        -> ResponseCreateParams
        -> Maybe Text
        -> (ResponseStreamEvent -> IO ())
        -> IO (Either ApiError Response))
    -> (ResponseStreamEvent -> Bool)
    -> (ApiError -> ApiError)
    -> TokenProvider
    -> Credential
    -> IORef Bool
    -> CodexConn
    -> ResponseCreateParams
    -> Maybe Text
    -> (ResponseStreamEvent -> IO ())
    -> IO (Either ApiError Response)
openAiResponseSenderReconnectingWhen sendOnConnection observed
        markObservedFailure provider currentCredential connectionHealthy conn =
    openAiResponseSenderWithConnectionRecoveryUsing
        observed
        markObservedFailure
        connectionHealthy
        sendCurrent
        sendFresh
  where
    sendCurrent request previousResponseId onEvent =
        sendOnConnection conn request previousResponseId onEvent
    sendFresh previousFailure request previousResponseId onEvent =
        case previousFailure >>= \err ->
                (\failure ->
                    (failure, accountFailureReason err failure))
                    <$> accountFailureFromApiError err of
            Just (failure, failureReason) ->
                withCodexWsRetryingAfter provider
                    FailedCredential
                        { credential = currentCredential
                        , failure
                        , failureReason
                        }
                    (sendOnFresh request previousResponseId onEvent)
            Nothing ->
                withCodexWsRetrying provider
                    (sendOnFresh request previousResponseId onEvent)
    sendOnFresh request previousResponseId onEvent freshConn _credential =
        do
            copyCodexTurnState conn freshConn
            emittedOutput <- newIORef False
            result <-
                sendOnConnection freshConn request previousResponseId
                    \event -> do
                        if observed event
                            then writeIORef emittedOutput True
                            else pure ()
                        onEvent event
            copyCodexTurnState freshConn conn
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
        streamOutputObserved
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
auxiliaryOutputObserved = streamOutputObserved

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
        else ProviderError replayUnsafeType
            ( "provider failed after "
                <> outputLabel
                <> "; refusing to replay: "
                <> Text.pack (show err)
            )
            Nothing
  where
    -- Retain transport provenance in the structured error type. Auxiliary
    -- callers must not replay a request after an opaque checkpoint arrived,
    -- but they still need to retire the broken WebSocket for later requests.
    replayUnsafeType
        | isOpenAiWebSocketTransportFailure err =
            UnknownErrorType replayUnsafeWebSocketTransportType
        | otherwise = UnknownErrorType replayUnsafeTypeName

isReplayUnsafeError :: ApiError -> Bool
isReplayUnsafeError = \case
    ProviderError (UnknownErrorType errorType) _ _ ->
        errorType == replayUnsafeTypeName
            || errorType == replayUnsafeWebSocketTransportType
    _ -> False

replayUnsafeTypeName :: Text
replayUnsafeTypeName = "replay_unsafe"

replayUnsafeWebSocketTransportType :: Text
replayUnsafeWebSocketTransportType =
    "replay_unsafe_websocket_transport"

-- | Prefer a WebSocket backend, then switch this agent session permanently to
-- a fallback transport once the socket has failed.
--
-- Independent agent sessions need their own WebSocket connections. The Codex
-- endpoint can accept the upgrade and then close a fresh connection before its
-- first response frame, and a live socket can die mid-response after the
-- WebSocket backend exhausted its own reconnect attempts. A dead socket commits
-- nothing on the server, so the same logical turn is replayed over the
-- stateless HTTP backend. Text or reasoning that already reached the caller is
-- closed with a visible restart boundary first; an announced tool block that
-- never ran is discarded. This mirrors Codex, which retries the sampling
-- request over WebSocket and then falls back to HTTPS for the session.
openAiBackendWithTransportFallback
    :: IORef Bool
    -> Backend
    -> Backend
    -> Backend
openAiBackendWithTransportFallback fallbackActive primary fallback =
    Backend \state legacyPreviousResponseId inputs onEvent -> do
        active <- readIORef fallbackActive
        if active
            then fallback.submitTurn state legacyPreviousResponseId inputs onEvent
            else tryPrimary state legacyPreviousResponseId inputs onEvent
  where
    tryPrimary state legacyPreviousResponseId inputs onEvent = do
        emittedModelOutput <- newIORef False
        announcedToolCall <- newIORef False
        let resetAttempt = do
                writeIORef emittedModelOutput False
                writeIORef announcedToolCall False
        result <-
            primary.submitTurn state legacyPreviousResponseId inputs \event -> do
                case event of
                    -- The primary already closed that attempt; only activity
                    -- from its newest attempt still needs a boundary before a
                    -- replay.
                    ResponseRestarted _ -> resetAttempt
                    ResponseAttemptDiscarded -> resetAttempt
                    _ -> do
                        when (isModelOutput event) $
                            writeIORef emittedModelOutput True
                        when (isToolAnnouncement event) $
                            writeIORef announcedToolCall True
                onEvent event
        case result of
            Left err
                | isOpenAiWebSocketTransportFailure err -> do
                    writeIORef fallbackActive True
                    emitted <- readIORef emittedModelOutput
                    announced <- readIORef announcedToolCall
                    if emitted
                        then onEvent (ResponseRestarted fallbackRestartMessage)
                        else
                            -- A tool block the dead socket announced must not
                            -- linger as running next to the replayed attempt.
                            when announced (onEvent ResponseAttemptDiscarded)
                    fallback.submitTurn
                        state legacyPreviousResponseId inputs onEvent
            _ -> pure result

    isModelOutput = \case
        TextDelta {} -> True
        ReasoningDelta {} -> True
        _ -> False

    isToolAnnouncement = \case
        ToolStarted {} -> True
        ToolUpdated {} -> True
        _ -> False

hasLegacyComputerContinuation :: [ResponseItem] -> [TurnInput] -> Bool
hasLegacyComputerContinuation history inputs =
    any isNativeComputerItem history
        || any isNativeComputerResult inputs
  where
    isNativeComputerItem = \case
        ComputerCallItem{} -> True
        ComputerCallOutputItem{} -> True
        FunctionCallItem call ->
            call.name == legacyComputerFunctionName
                && call.namespace == Just computerFunctionNamespace
        _ -> False
    isNativeComputerResult = \case
        CompletedTool result -> result.callKind == ComputerCallKind
        _ -> False

-- | Errors that indicate the Codex Responses WebSocket transport is
-- unavailable rather than that the logical request itself was rejected.
isOpenAiWebSocketTransportFailure :: ApiError -> Bool
isOpenAiWebSocketTransportFailure err = case err of
    ConnectionError {} -> True
    -- Rejected WebSocket upgrades reach this exact transport-specific shape
    -- only after bounded connection retries have been exhausted. A same-status
    -- logical HTTP error does not match and remains a permission failure.
    err
        | Just status <- WebSocket.webSocketHandshakeFailureStatus err ->
            status /= 401
    ProviderError WebSocketConnectionLimitReached _ _ -> True
    -- A server that does not support the Responses WebSocket protocol
    -- advertises that explicitly with HTTP 426.
    HttpError 426 _ -> True
    -- Some direct ChatGPT accounts reject only the WebSocket upgrade while
    -- accepting the same Responses request over HTTPS. Match the transport's
    -- exact handshake fingerprint so an application-level 403 remains a
    -- permission error.
    _ -> WebSocket.webSocketHandshakeFailureStatus err == Just 403

-- | A WebSocket failure whose request has already produced provider output.
--
-- The connection should not be reused, but the failed logical request must
-- not be replayed: an auxiliary response may contain a billable opaque
-- compaction checkpoint even when no text was rendered.
isOpenAiReplayUnsafeWebSocketTransportFailure :: ApiError -> Bool
isOpenAiReplayUnsafeWebSocketTransportFailure = \case
    ProviderError (UnknownErrorType errorType) _ _ ->
        errorType == replayUnsafeWebSocketTransportType
    _ -> False

-- | Reset Codex sticky-routing state when a backend submission starts a new
-- logical turn, while preserving it for tool continuations in the same turn.
--
-- Keep this wrapper outside automatic compaction and connection recovery:
-- compaction may mint the token needed by the immediately following request,
-- and recovery retries must replay the same turn state rather than clearing it.
withCodexTurnStateScope :: IO CodexTurnState -> Backend -> Backend
withCodexTurnStateScope getTurnState (Backend submit) =
    Backend \state legacyPreviousResponseId inputs onEvent -> do
        when (startsNewLogicalTurn inputs) $
            getTurnState >>= resetCodexTurnState
        submit state legacyPreviousResponseId inputs onEvent
  where
    startsNewLogicalTurn turnInputs =
        not (null turnInputs) && not (any isCompletedTool turnInputs)

    isCompletedTool = \case
        CompletedTool{} -> True
        _ -> False

-- | Same mapping as 'openAiBackend', with an injectable transport for tests.
openAiBackendWith
    :: (ResponseCreateParams
        -> Maybe Text
        -> (ResponseStreamEvent -> IO ())
        -> IO (Either ApiError Response))
    -> IO ResponseCreateParams
    -> Backend
openAiBackendWith =
    openAiBackendWithReasoningVisibility False

-- | Injectable OpenAI backend with Codex-style reasoning visibility:
-- summaries are always shown and raw reasoning is opt-in.
openAiBackendWithReasoningVisibility
    :: Bool
    -> (ResponseCreateParams
        -> Maybe Text
        -> (ResponseStreamEvent -> IO ())
        -> IO (Either ApiError Response))
    -> IO ResponseCreateParams
    -> Backend
openAiBackendWithReasoningVisibility showRawReasoning =
    openAiBackendWithRetryPolicyAndReasoningVisibility
        showRawReasoning
        transientStreamingResultPolicy

-- | Transient server errors are retried only until the loop has observed
-- output. Server error events themselves are not loop-visible, so transient
-- Codex failures can wait and retry without printing an error or duplicating
-- output.
--
-- A connection that dies mid-response is different: the dead socket committed
-- nothing on either side, so the same request is resubmitted even after
-- output streamed. Partial text or reasoning stays visible behind a restart
-- boundary and hidden activity is discarded, then the transport (a
-- reconnecting sender or a per-request dial) opens a fresh connection. Codex
-- retries its sampling request the same way before falling back to HTTPS.
openAiBackendWithRetryPolicy
    :: RetryPolicyM IO
    -> (ResponseCreateParams
        -> Maybe Text
        -> (ResponseStreamEvent -> IO ())
        -> IO (Either ApiError Response))
    -> IO ResponseCreateParams
    -> Backend
openAiBackendWithRetryPolicy transientPolicy =
    openAiBackendWithRetryPolicies transientPolicy connectionReplayPolicy

-- | 'openAiBackendWithRetryPolicy' with an explicit reconnect policy for
-- mid-response connection failures.
openAiBackendWithRetryPolicies
    :: RetryPolicyM IO
    -> RetryPolicyM IO
    -> (ResponseCreateParams
        -> Maybe Text
        -> (ResponseStreamEvent -> IO ())
        -> IO (Either ApiError Response))
    -> IO ResponseCreateParams
    -> Backend
openAiBackendWithRetryPolicies =
    openAiBackendWithRetryPoliciesAndReasoningVisibility False

openAiBackendWithRetryPolicyAndReasoningVisibility
    :: Bool
    -> RetryPolicyM IO
    -> (ResponseCreateParams
        -> Maybe Text
        -> (ResponseStreamEvent -> IO ())
        -> IO (Either ApiError Response))
    -> IO ResponseCreateParams
    -> Backend
openAiBackendWithRetryPolicyAndReasoningVisibility
        showRawReasoning transientPolicy =
    openAiBackendWithRetryPoliciesAndReasoningVisibility
        showRawReasoning
        transientPolicy
        connectionReplayPolicy

openAiBackendWithRetryPoliciesAndReasoningVisibility
    :: Bool
    -> RetryPolicyM IO
    -> RetryPolicyM IO
    -> (ResponseCreateParams
        -> Maybe Text
        -> (ResponseStreamEvent -> IO ())
        -> IO (Either ApiError Response))
    -> IO ResponseCreateParams
    -> Backend
openAiBackendWithRetryPoliciesAndReasoningVisibility
        showRawReasoning transientPolicy reconnectPolicy send getParams =
    Backend \snapshot legacyPreviousResponseId inputs onLoopEvent -> do
        baseParams <- sanitizeCodexRequest <$> getParams
        let history = snapshot.backendItems
            previousResponseId =
                backendContinuationToken "openai.responses" snapshot
                    <|> legacyPreviousResponseId
            newItems = turnInputsToItems inputs
            deltaRequest = withRequestInput baseParams newItems
            -- Live and resumed transcripts already apply compaction snapshots
            -- as full replacements. Remote v2 intentionally keeps retained
            -- messages before its opaque checkpoint, so replay the complete
            -- replacement instead of trimming that retained prefix.
            fullRequest = withRequestInput baseParams (history <> newItems)
            (initialRequest, initialPrevious) =
                case previousResponseId of
                    _ | hasLegacyComputerContinuation history inputs ->
                        (fullRequest, Nothing)
                    Nothing | not (null history) -> (fullRequest, Nothing)
                    _ -> (deltaRequest, previousResponseId)
        result <- sendRetrying onLoopEvent initialRequest initialPrevious
        recovered <- case result of
            Left err
                | isJust initialPrevious
                , isResponseChainCompatibilityError err
                , not (null history) ->
                    sendRetrying onLoopEvent fullRequest Nothing
                | otherwise -> pure (Left err)
            Right response -> pure (Right response)
        case recovered of
            Left err -> pure (Left err)
            Right response ->
                pure $ Right BackendResult
                    { backendOutput = responseToTurnOutput response
                    , backendState =
                        advanceBackendSnapshot snapshot
                            ( normalizeResponseInputItems
                                (history <> newItems)
                                <> response.output
                            )
                            (Just BackendContinuation
                                { continuationProvider = "openai.responses"
                                , continuationToken = response.responseId
                                })
                    }
  where
    sendRetrying onLoopEvent request previousResponseId = do
        emittedRawOutput <- newIORef False
        emittedVisibleOutput <- newIORef False
        go emittedRawOutput emittedVisibleOutput
            defaultRetryStatus defaultRetryStatus
      where
        go emittedRawOutput emittedVisibleOutput transientStatus
                reconnectStatus = do
            -- One projector per attempt: argument-progress counters must
            -- describe a single provider sample, not the whole retry chain.
            projectEvent <-
                Responses.newStreamEventToLoopEvents showRawReasoning
            result <- send request previousResponseId \event -> do
                if streamOutputObserved event
                    then writeIORef emittedRawOutput True
                    else pure ()
                projectEvent event
                    >>= mapM_ \loopEvent -> do
                        when (isVisibleModelOutput loopEvent) $
                            writeIORef emittedVisibleOutput True
                        onLoopEvent loopEvent
            emitted <- readIORef emittedRawOutput
            case result of
                Left apiError
                    -- A pre-output connection failure is handled by the
                    -- connection-recovery sender and the transport fallback;
                    -- only a socket that died mid-response is retried here.
                    | emitted
                    , isReconnectableTransportFailure apiError ->
                        applyPolicy reconnectPolicy reconnectStatus >>= \case
                            Nothing -> settle apiError result
                            Just nextStatus -> do
                                visible <- readIORef emittedVisibleOutput
                                let delayMicros =
                                        fromMaybe 0 nextStatus.rsPreviousDelay
                                    attempt = nextStatus.rsIterNumber
                                onLoopEvent $ ActivityUpdated $
                                    formatReconnectScheduled
                                        apiError attempt delayMicros
                                threadDelay delayMicros
                                -- Close the interrupted attempt in every
                                -- renderer before the replay streams. Visible
                                -- partial output stays on screen marked as
                                -- failed; hidden activity such as an announced
                                -- tool call is removed.
                                if visible
                                    then onLoopEvent
                                        (ResponseRestarted
                                            connectionRestartMessage)
                                    else onLoopEvent ResponseAttemptDiscarded
                                onLoopEvent $ ActivityUpdated $
                                    "Reconnecting to Codex (attempt "
                                        <> Text.pack (show attempt) <> ")…"
                                writeIORef emittedRawOutput False
                                writeIORef emittedVisibleOutput False
                                go emittedRawOutput emittedVisibleOutput
                                    transientStatus nextStatus
                    | not emitted
                    , isInlineRetryableProviderResponseError apiError ->
                        applyPolicy transientPolicy transientStatus >>= \case
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
                                go emittedRawOutput emittedVisibleOutput
                                    nextStatus reconnectStatus
                    | emitted -> settle apiError result
                _ -> pure result
          where
            -- The transport fallback may still replay a dropped connection
            -- after this backend gives up; every other failure after output
            -- is terminal because the provider may have committed the sample.
            settle apiError result = do
                visible <- readIORef emittedVisibleOutput
                pure $ if visible || isReconnectableTransportFailure apiError
                    then result
                    else Left (replayUnsafeError "model output" apiError)

    isVisibleModelOutput = \case
        TextDelta{} -> True
        ReasoningDelta{} -> True
        _ -> False

transientStreamingResultPolicy :: RetryPolicyM IO
transientStreamingResultPolicy =
    exponentialBackoff 5_000_000 <> limitRetries 3

-- | Reconnect attempts after a socket died mid-response: 200ms, 400ms, 800ms,
-- 1.6s and 3.2s, matching Codex's @stream_max_retries@ default of five with
-- its 200ms exponential backoff. The transport fallback takes over afterwards.
connectionReplayPolicy :: RetryPolicyM IO
connectionReplayPolicy =
    exponentialBackoff 200_000 <> limitRetries 5

-- | Failures that mean the current WebSocket is gone, not that the request was
-- rejected. A dropped socket commits nothing on either side, so the request
-- may be resubmitted on a fresh connection even after output streamed.
isReconnectableTransportFailure :: ApiError -> Bool
isReconnectableTransportFailure = \case
    ConnectionError{} -> True
    ProviderError WebSocketConnectionLimitReached _ _ -> True
    _ -> False

connectionRestartMessage :: Text
connectionRestartMessage =
    "Connection interrupted the response; restarting automatically. "
        <> "The new attempt may repeat partial output shown above."

fallbackRestartMessage :: Text
fallbackRestartMessage =
    "Connection interrupted the response; retrying over the HTTP transport. "
        <> "The new attempt may repeat partial output shown above."

formatReconnectScheduled :: ApiError -> Int -> Int -> Text
formatReconnectScheduled apiError attempt delayMicros =
    "Connection lost mid-response ("
        <> transportFailureReason apiError
        <> "); reconnecting in "
        <> Text.pack (show (ceilingSeconds delayMicros))
        <> "s (attempt "
        <> Text.pack (show attempt)
        <> ")…"

transportFailureReason :: ApiError -> Text
transportFailureReason = \case
    ConnectionError reason -> reason
    ProviderError WebSocketConnectionLimitReached _ _ ->
        "Codex connection limit reached"
    other -> Text.pack (show other)

-- | OpenAI's default event projection: reasoning summaries are visible, while
-- raw chain-of-thought deltas are suppressed.
streamEventToLoopEvent :: ResponseStreamEvent -> Maybe LoopEvent
streamEventToLoopEvent =
    Responses.streamEventToLoopEventWithRawReasoning False

-- | Stateless OpenAI transport with the same summary-only default as the
-- WebSocket backend.
statelessResponsesBackend
    :: (ResponseCreateParams
        -> (ResponseStreamEvent -> IO ())
        -> IO (Either ApiError Response))
    -> IO ResponseCreateParams
    -> Backend
statelessResponsesBackend =
    Responses.statelessResponsesBackendWithRawReasoning False

formatRetryScheduled :: ApiError -> Int -> Int -> Text
formatRetryScheduled apiError attempt delayMicros =
    retryReason apiError
        <> "; retrying in "
        <> Text.pack (show (ceilingSeconds delayMicros))
        <> "s (attempt "
        <> Text.pack (show attempt)
        <> ")…"
  where
    retryReason = \case
        ProviderError OverloadedError _ _ -> "Codex is overloaded"
        ProviderError ServiceUnavailableError _ _ -> "Codex is unavailable"
        ProviderError WebSocketConnectionLimitReached _ _ ->
            "Codex connection limit reached"
        _ -> "Codex server error"

ceilingSeconds :: Int -> Int
ceilingSeconds delayMicros
    | delayMicros <= 0 = 0
    | otherwise = (delayMicros + 999_999) `div` 1_000_000
