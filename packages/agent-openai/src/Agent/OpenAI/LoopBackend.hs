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
    , openAiBackendWithConnectionRecovery
    , openAiBackendWithTransportFallback
    , withCodexTurnStateScope
    , isOpenAiWebSocketTransportFailure
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
import qualified Agent.Responses.LoopBackend as Responses
import Agent.Loop
    ( Backend(..)
    , BackendResult(..)
    , LoopEvent(..)
    , TurnInput(..)
    )
import Agent.ToolDispatch
    ( ToolArgumentStreamEvent(..)
    , ToolCall
    , functionToolCall
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
    )
import Agent.Responses.Types
import Agent.ToolDispatch
    ( ToolArgumentStreamEvent(..)
    , ToolCall
    , functionToolCall
    , ToolCallStreamRef(..)
    )
import Control.Concurrent (threadDelay)
import Control.Exception.Safe (onException)
import Control.Monad (forM_, when)
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
import Data.Maybe (fromMaybe, isJust, mapMaybe)
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
    Backend \state previousResponseId inputs onEvent -> do
        active <- readIORef fallbackActive
        if active
            then fallback.submitTurn state previousResponseId inputs onEvent
            else tryPrimary state previousResponseId inputs onEvent
  where
    tryPrimary state previousResponseId inputs onEvent = do
        emittedModelOutput <- newIORef False
        result <- primary.submitTurn state previousResponseId inputs \event -> do
            if isModelOutput event
                then writeIORef emittedModelOutput True
                else pure ()
            onEvent event
        case result of
            Left err
                | isOpenAiWebSocketTransportFailure err -> do
                    writeIORef fallbackActive True
                    emitted <- readIORef emittedModelOutput
                    if emitted
                        then pure result
                        else fallback.submitTurn
                            state previousResponseId inputs onEvent
            _ -> pure result

    isModelOutput = \case
        TextDelta {} -> True
        ReasoningDelta {} -> True
        _ -> False

-- | Errors that indicate the Codex Responses WebSocket transport is
-- unavailable rather than that the logical request itself was rejected.
isOpenAiWebSocketTransportFailure :: ApiError -> Bool
isOpenAiWebSocketTransportFailure = \case
    ConnectionError {} -> True
    ProviderError WebSocketConnectionLimitReached _ _ -> True
    -- A server that does not support the Responses WebSocket protocol
    -- advertises that explicitly with HTTP 426.
    HttpError 426 _ -> True
    _ -> False

-- | Reset Codex sticky-routing state when a backend submission starts a new
-- logical turn, while preserving it for tool continuations in the same turn.
--
-- Keep this wrapper outside automatic compaction and connection recovery:
-- compaction may mint the token needed by the immediately following request,
-- and recovery retries must replay the same turn state rather than clearing it.
withCodexTurnStateScope :: IO CodexTurnState -> Backend -> Backend
withCodexTurnStateScope getTurnState (Backend submit) =
    Backend \state previousResponseId inputs onEvent -> do
        when (startsNewLogicalTurn inputs) $
            getTurnState >>= resetCodexTurnState
        submit state previousResponseId inputs onEvent
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
    openAiBackendWithRetryPolicyAndFeatures
        showRawReasoning
        transientStreamingResultPolicy

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
    -> Backend
openAiBackendWithRetryPolicy =
    openAiBackendWithRetryPolicyAndFeatures False

openAiBackendWithRetryPolicyAndFeatures
    :: Bool
    -> RetryPolicyM IO
    -> (ResponseCreateParams
        -> Maybe Text
        -> (ResponseStreamEvent -> IO ())
        -> IO (Either ApiError Response))
    -> IO ResponseCreateParams
    -> Backend
openAiBackendWithRetryPolicyAndFeatures
        showRawReasoning retryPolicy send getParams =
    Backend \history previousResponseId inputs onLoopEvent -> do
        let submit = do
                baseParams <- sanitizeCodexRequest <$> getParams
                let newItems = turnInputsToItems inputs
                    deltaRequest = withRequestInput baseParams newItems
                    -- Live and resumed transcripts already apply compaction
                    -- snapshots as full replacements. Remote v2 intentionally
                    -- keeps retained messages before its opaque checkpoint, so
                    -- replay the complete replacement instead of trimming that
                    -- retained prefix.
                    fullRequest =
                        withRequestInput baseParams (history <> newItems)
                    (initialRequest, initialPrevious) =
                        case previousResponseId of
                            Nothing | not (null history) ->
                                (fullRequest, Nothing)
                            _ -> (deltaRequest, previousResponseId)
                result <-
                    sendRetrying
                        onLoopEvent initialRequest initialPrevious
                recovered <- case result of
                    Left err
                        | isJust initialPrevious
                        , isResponseChainCompatibilityError err
                        , not (null history) ->
                            sendRetrying onLoopEvent fullRequest Nothing
                        | otherwise -> pure (Left err)
                    Right response -> pure (Right response)
                case recovered of
                    Left err ->
                        pure (Left err)
                    Right response -> do
                        mapM_
                            (onLoopEvent . ToolArgumentEvent)
                            (responseToolCallCompletions response.output)
                        pure $ Right BackendResult
                            { backendOutput = responseToTurnOutput response
                            , backendState =
                                history <> newItems <> response.output
                            }
        submit
  where
    sendRetrying onLoopEvent request previousResponseId = do
        emittedRawOutput <- newIORef False
        emittedVisibleOutput <- newIORef False
        go emittedRawOutput emittedVisibleOutput defaultRetryStatus
      where
        go emittedRawOutput emittedVisibleOutput retryStatus = do
            -- One projector per attempt: argument-progress counters must
            -- describe a single provider sample, not the whole retry chain.
            projectEvent <-
                Responses.newStreamEventToLoopEvents showRawReasoning
            result <- send request previousResponseId \event -> do
                if streamOutputObserved event
                    then writeIORef emittedRawOutput True
                    else pure ()
                forM_ (responseEventToToolArgumentEvent event) $
                    onLoopEvent . ToolArgumentEvent
                projectEvent event
                    >>= mapM_ \loopEvent -> do
                        when (isVisibleModelOutput loopEvent) $
                            writeIORef emittedVisibleOutput True
                        onLoopEvent loopEvent
            emitted <- readIORef emittedRawOutput
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
                                go emittedRawOutput emittedVisibleOutput nextStatus
                    | emitted -> do
                        visible <- readIORef emittedVisibleOutput
                        pure $ if visible
                            then result
                            else Left (replayUnsafeError "model output" apiError)
                _ -> pure result

    isVisibleModelOutput = \case
        TextDelta{} -> True
        ReasoningDelta{} -> True
        _ -> False

transientStreamingResultPolicy :: RetryPolicyM IO
transientStreamingResultPolicy =
    exponentialBackoff 5_000_000 <> limitRetries 3

responseEventToToolArgumentEvent
    :: ResponseStreamEvent
    -> Maybe ToolArgumentStreamEvent
responseEventToToolArgumentEvent = \case
    ResponseOutputItemAddedEvent
        { item = FunctionCallItem call
        , outputIndex
        } ->
            Just $
                ToolArgumentsStarted
                    { argumentStreamRefs =
                        responseCallRefs call.itemId outputIndex
                    , argumentStreamCallId = call.callId
                    , argumentStreamName = Just call.name
                    , argumentStreamArguments = call.arguments
                    }
    ResponseFunctionCallArgumentsDeltaEvent
        { streamItemId
        , streamOutputIndex
        , delta = Just value
        } ->
            Just $
                ToolArgumentsDelta
                    { argumentStreamRefs =
                        responseCallRefs streamItemId streamOutputIndex
                    , argumentStreamDelta = value
                    }
    ResponseFunctionCallArgumentsDoneEvent
        { streamItemId
        , streamOutputIndex
        , functionName
        , arguments = Just value
        } ->
            Just $
                ToolArgumentsDone
                    { argumentStreamRefs =
                        responseCallRefs streamItemId streamOutputIndex
                    , argumentStreamName = functionName
                    , argumentStreamArguments = value
                    }
    ResponseOutputItemDoneEvent
        { item = FunctionCallItem call
        , outputIndex
        } ->
            Just $
                ToolCallStreamCompleted
                    { argumentStreamRefs =
                        responseCallRefs call.itemId outputIndex
                    , argumentStreamCall =
                        functionToolCall
                            call.callId
                            call.name
                            call.arguments
                    }
    _ -> Nothing

responseToolCalls :: [ResponseItem] -> [ToolCall]
responseToolCalls =
    mapMaybe \case
        FunctionCallItem call ->
            Just $
                functionToolCall
                    call.callId
                    call.name
                    call.arguments
        _ -> Nothing

responseToolCallCompletions
    :: [ResponseItem]
    -> [ToolArgumentStreamEvent]
responseToolCallCompletions =
    mapMaybe \case
        FunctionCallItem call ->
            Just $
                ToolCallStreamCompleted
                    { argumentStreamRefs =
                        responseCallRefs call.itemId Nothing
                    , argumentStreamCall =
                        functionToolCall
                            call.callId
                            call.name
                            call.arguments
                    }
        _ -> Nothing

responseCallRefs
    :: Maybe Text
    -> Maybe Int
    -> [ToolCallStreamRef]
responseCallRefs itemId outputIndex =
    maybe [] (pure . ToolCallStreamItem) itemId
        <> maybe [] (pure . ToolCallStreamOutput) outputIndex

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
