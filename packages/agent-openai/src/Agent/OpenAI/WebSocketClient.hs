module Agent.OpenAI.WebSocketClient
    ( withCodexWs
    , withCodexWsWithProvider
    , withCodexWsCredential
    , withCodexWsRetrying
    , withCodexWsRetryingAfter
    , sendWsRequest
    , sendWsRequestWithOptions
    , sendWsRequestWithEvents
    , sendWsRequestWithRawEvents
    , retryTransientWsResultWithPolicy
    , CodexWsOptions(..)
    , defaultCodexWsOptions
    , buildWsPayloadWithOptions
    , buildCodexWsHeaders
    , StreamEventCallback
    , RawStreamEventCallback
    , CodexConn
    , closeCodexConn
    , CodexAuthFailed(..)
    ) where

import Agent.OpenAI.Auth (Pool)
import Agent.OpenAI.Credential (poolTokenProvider)
import Agent.Error
import Agent.OpenAI.Error (isPreviousResponseIdError, mkOpenAIError)
import Agent.OpenAI.Features (remoteCompactionV2Feature)
import Agent.Responses.ResponseMerge (mergeCompletedResponseOutput)
import Agent.Responses.StreamAssembly (assembleDoneResponse)
import qualified Agent.Responses.Codec as ResponsesCodec
import qualified Agent.Transport.WebSocket as WebSocket
import Agent.Provider
    ( Credential(..)
    , FailedCredential
    , Provider(..)
    , TokenProvider
    , runWithTokenProvider
    , runWithTokenProviderAfter
    )
import Agent.Responses.Types
import Control.Retry
    ( RetryPolicyM
    , exponentialBackoff
    , limitRetries
    , retrying
    )
import qualified Control.Exception as Exception
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text (lenientDecode)
import qualified Network.WebSockets as WS
import qualified Wuss

--------------------------------------------------------------------------------
-- Connection handle
--------------------------------------------------------------------------------

-- | An OpenAI Codex connection backed by the provider-neutral WebSocket
-- transport session from @agent-core@.
newtype CodexConn = CodexWsConn WebSocket.WebSocketSession

-- | Close a reusable Codex connection before its owning callback returns.
closeCodexConn :: CodexConn -> IO ()
closeCodexConn (CodexWsConn session) =
    WebSocket.closeWebSocketSession session "switching account"

--------------------------------------------------------------------------------
-- Connect
--------------------------------------------------------------------------------

wsHost :: String
wsHost = "chatgpt.com"

wsPath :: String
wsPath = "/backend-api/codex/responses"

-- | Run an action with a WebSocket connection to the Codex Responses API.
-- The connection is open for the duration of the callback and closed
-- afterwards.
--
-- The callback receives both the live 'CodexConn' and the @accountId@ of the
-- ChatGPT account that was selected for this connection. Passing the id back
-- out is what lets the caller mark the account as cooling down (via
-- 'Agent.OpenAI.Auth.reportRateLimit') when a @usage_limit_reached@ error is observed
-- mid-session.
--
-- The provider-neutral WebSocket transport keeps the reusable connection
-- alive and drains control frames for the lifetime of @action@.
--
-- A handshake HTTP 401/403 is recovered centrally: the rejected account is
-- force-refreshed even when its JWT has not expired, then the handshake is
-- retried once with the rotated token. If refresh or the second handshake
-- fails, the account is cooled down and the next configured account is tried.
-- This happens before the callback starts, so retrying cannot duplicate caller
-- side effects.
--
-- Unrecoverable 'ApiError' values are propagated as 'CodexAuthFailed', in
-- particular a 'CredentialsExhausted' when every account is cooling down.
withCodexWs
    :: Pool
    -> (CodexConn -> Text -> IO a)
    -> IO a
withCodexWs pool action = do
    provider <- poolTokenProvider pool
    withCodexWsWithProvider provider \conn credential ->
        action conn credential.accountId

-- | Provider-based WebSocket acquisition. Handshake authentication failures
-- are fed back into the provider before the callback starts, so retrying with
-- a refreshed or different credential cannot duplicate caller side effects.
--
-- The callback receives the full credential rather than only the account id.
-- Long-running callers can therefore preserve an opaque broker @leaseId@ when
-- they report a later in-band usage-limit failure to their replay layer.
withCodexWsWithProvider
    :: TokenProvider
    -> (CodexConn -> Credential -> IO a)
    -> IO a
withCodexWsWithProvider provider action =
    withCodexWsRetrying provider (\conn credential -> Right <$> action conn credential) >>= \case
        Left err -> Exception.throwIO (CodexAuthFailed err)
        Right value -> pure value

-- | Open one exact credential for an interactive account switch. Connection
-- retries are deliberately bounded so the selector cannot hang indefinitely.
withCodexWsCredential
    :: Credential
    -> (CodexConn -> Credential -> IO a)
    -> IO (Either ApiError a)
withCodexWsCredential credential action =
    runConnectionAttemptWithPolicy
        (limitRetries 2 <> exponentialBackoff 500000)
        credential
        (\conn activeCredential -> Right <$> action conn activeCredential)

-- | Run a replay-safe WebSocket action and automatically reacquire a
-- credential after handshake or in-band account failures. The callback is
-- executed again from the beginning after a 401, 403, or usage-limit error;
-- callers must therefore keep externally visible side effects idempotent.
withCodexWsRetrying
    :: TokenProvider
    -> (CodexConn -> Credential -> IO (Either ApiError a))
    -> IO (Either ApiError a)
withCodexWsRetrying provider action =
    runWithTokenProvider provider \credential ->
        runConnectionAttempt credential action

-- | Like 'withCodexWsRetrying', but first reports a credential that failed on
-- an already-open connection. This ensures an exhausted resumed-session
-- account is cooled down before a replacement WebSocket credential is chosen.
withCodexWsRetryingAfter
    :: TokenProvider
    -> FailedCredential
    -> (CodexConn -> Credential -> IO (Either ApiError a))
    -> IO (Either ApiError a)
withCodexWsRetryingAfter provider failed action =
    runWithTokenProviderAfter provider (Just failed) \credential ->
        runConnectionAttempt credential action

runConnectionAttempt
    :: Credential
    -> (CodexConn -> Credential -> IO (Either ApiError a))
    -> IO (Either ApiError a)
runConnectionAttempt =
    runConnectionAttemptWithPolicy WebSocket.transientWsConnectRetryPolicy

runConnectionAttemptWithPolicy
    :: RetryPolicyM IO
    -> Credential
    -> (CodexConn -> Credential -> IO (Either ApiError a))
    -> IO (Either ApiError a)
runConnectionAttemptWithPolicy _ credential _action
    | credential.provider == XAIProvider = pure $ Left $ ProviderError ApiErrorType
        "XAI credentials must be used through agent-xai"
        Nothing
runConnectionAttemptWithPolicy _ credential _action
    | credential.provider == OpenRouterProvider = pure $ Left $ ProviderError ApiErrorType
        "OpenRouter credentials must be used through agent-openrouter"
        Nothing
runConnectionAttemptWithPolicy _ credential _action
    | credential.provider == ClaudeCodeProvider = pure $ Left $ ProviderError ApiErrorType
        "Claude Code subscription sessions must use agent-claude"
        Nothing
runConnectionAttemptWithPolicy retryPolicy credential action = do
    let headers = buildCodexWsHeaders credential
    WebSocket.retryTransientWsConnectWithPolicy
        retryPolicy
        \connected ->
            Wuss.runSecureClientWith
                wsHost
                443
                wsPath
                WS.defaultConnectionOptions
                headers
                \conn -> do
                    connected
                    WebSocket.withWebSocketSession
                        WebSocket.defaultWebSocketSessionOptions
                        conn
                        (\session -> action (CodexWsConn session) credential)

-- | Pure handshake-header builder exported for transport contract tests.
buildCodexWsHeaders :: Credential -> WS.Headers
buildCodexWsHeaders credential =
    [ ("Authorization", "Bearer " <> Text.encodeUtf8 credential.accessToken)
    , ("chatgpt-account-id", Text.encodeUtf8 credential.accountId)
    , ("OpenAI-Beta", "responses_websockets=2026-02-06")
    , ("x-codex-beta-features", Text.encodeUtf8 remoteCompactionV2Feature)
    ]

-- | Wraps an 'ApiError' from 'getAccessToken' so it can propagate out of
-- 'withCodexWs' as an exception. The agent-loop's failover layer unwraps it
-- to decide whether to retry with another account or reschedule the job.
newtype CodexAuthFailed = CodexAuthFailed ApiError
    deriving (Show)

instance Exception.Exception CodexAuthFailed

--------------------------------------------------------------------------------
-- Send request / receive response
--------------------------------------------------------------------------------

-- | Callback invoked for each typed WebSocket stream event before the client
-- uses it for response accumulation.
type StreamEventCallback = ResponseStreamEvent -> IO ()

-- | Escape hatch for callers that need the raw event @type@ and JSON object,
-- e.g. for compatibility with another streaming event model.
type RawStreamEventCallback = Text -> Aeson.Value -> IO ()

-- | Optional controls for a WebSocket Responses request.
data CodexWsOptions = CodexWsOptions
    { compactThreshold :: !(Maybe Int)
    } deriving (Eq, Show)

defaultCodexWsOptions :: CodexWsOptions
defaultCodexWsOptions = CodexWsOptions
    { compactThreshold = Nothing
    }

-- | Send a Codex request over the WebSocket and receive the complete response.
--
-- When @previousResponseId@ is @Just rid@, the server uses its internal
-- state to avoid reprocessing the conversation prefix — only the new
-- @input@ items are evaluated. This is the main mechanism for reducing
-- token usage across turns.
sendWsRequest
    :: CodexConn
    -> ResponseCreateParams
    -> Maybe Text     -- ^ previous_response_id (Nothing on first turn)
    -> IO (Either ApiError Response)
sendWsRequest cc request previousResponseId =
    sendWsRequestWithOptions defaultCodexWsOptions cc request previousResponseId

-- | Like 'sendWsRequest', with optional server-side context management.
-- Setting 'compactThreshold' does not compact every request: the server emits
-- a compaction checkpoint only after rendered input crosses the threshold.
sendWsRequestWithOptions
    :: CodexWsOptions
    -> CodexConn
    -> ResponseCreateParams
    -> Maybe Text
    -> IO (Either ApiError Response)
sendWsRequestWithOptions options cc request previousResponseId =
    retryTransientWsResultWithPolicy transientWsResultPolicy $
        sendWsRequestWithEventsAndOptions options cc request previousResponseId (\_ -> pure ())

-- | Retry short-lived provider responses on the current WebSocket. Connection
-- failures and connection-limit responses are deliberately excluded because
-- the caller's reconnect/failover layer must replace or bypass that socket.
-- The policy is injectable for deterministic tests; normal requests use
-- 5s/10s/20s backoff.
retryTransientWsResultWithPolicy
    :: RetryPolicyM IO
    -> IO (Either ApiError value)
    -> IO (Either ApiError value)
retryTransientWsResultWithPolicy policy request =
    retrying policy shouldRetry (const request)
  where
    shouldRetry _retryStatus = \case
        Left apiError
            | isInlineRetryableProviderResponseError apiError -> pure True
        _ -> pure False

transientWsResultPolicy :: RetryPolicyM IO
transientWsResultPolicy = exponentialBackoff 5_000_000 <> limitRetries 3

-- | Like 'sendWsRequest', but also streams typed event objects to a callback
-- as they arrive. This lets applications preserve token-level UI updates while
-- still reusing the library's WebSocket pump, ping handling, error parsing, and
-- final-response reconstruction.
sendWsRequestWithEvents
    :: CodexConn
    -> ResponseCreateParams
    -> Maybe Text
    -> StreamEventCallback
    -> IO (Either ApiError Response)
sendWsRequestWithEvents cc request previousResponseId onEvent =
    sendWsRequestWithEventsAndOptions defaultCodexWsOptions cc request previousResponseId onEvent

-- | Like 'sendWsRequestWithEvents', but exposes the raw event @type@ and JSON
-- event object.
sendWsRequestWithRawEvents
    :: CodexConn
    -> ResponseCreateParams
    -> Maybe Text
    -> RawStreamEventCallback
    -> IO (Either ApiError Response)
sendWsRequestWithRawEvents =
    sendWsRequestWithRawEventsAndOptions defaultCodexWsOptions

sendWsRequestWithRawEventsAndOptions
    :: CodexWsOptions
    -> CodexConn
    -> ResponseCreateParams
    -> Maybe Text
    -> RawStreamEventCallback
    -> IO (Either ApiError Response)
sendWsRequestWithRawEventsAndOptions options cc request previousResponseId onEvent =
    sendWsRequestWithEventsAndOptions options cc request previousResponseId \event ->
        onEvent
            (streamEventTypeText (responseStreamEventType event))
            (Aeson.toJSON event)

sendWsRequestWithEventsAndOptions
    :: CodexWsOptions
    -> CodexConn
    -> ResponseCreateParams
    -> Maybe Text
    -> StreamEventCallback
    -> IO (Either ApiError Response)
sendWsRequestWithEventsAndOptions options cc request previousResponseId onEvent = case cc of
    CodexWsConn session -> sendOverWs session
  where
    sendOverWs session = do
        let wsPayload = buildWsPayloadWithOptions options request previousResponseId
            encoded = Aeson.encode wsPayload
        WebSocket.withWebSocketRequest session \wsRequest -> do
            sendRes <- WebSocket.sendWebSocketText wsRequest encoded
            case sendRes of
                Left apiError -> pure (Left apiError)
                Right () -> receiveWsResponse wsRequest onEvent

-- | Pure WebSocket envelope builder, exported for payload contract tests.
-- All fields are flattened at the top level (not nested inside "response").
buildWsPayloadWithOptions :: CodexWsOptions -> ResponseCreateParams -> Maybe Text -> Aeson.Value
buildWsPayloadWithOptions options request previousResponseId =
    case Aeson.toJSON request of
        Aeson.Object object -> Aeson.Object
            $ addContextManagement
            $ addPreviousResponseId
            $ KeyMap.insert "store" (Aeson.Bool False)
            $ KeyMap.insert "type" (Aeson.String "response.create") object
        other -> other
  where
    addPreviousResponseId = case previousResponseId of
        Just responseId -> KeyMap.insert "previous_response_id" (Aeson.String responseId)
        Nothing -> id

    addContextManagement = case options.compactThreshold of
        Just threshold | threshold > 0 -> KeyMap.insert "context_management" (Aeson.toJSON
            [ Aeson.object
                [ "type" Aeson..= ("compaction" :: Text)
                , "compact_threshold" Aeson..= threshold
                ]
            ])
        _ -> id

-- | Receive typed WebSocket events until the response is complete.
-- Accumulates output items from 'ResponseOutputItemDoneEvent' values and
-- returns the response carried by a terminal response event.
receiveWsResponse :: WebSocket.WebSocketRequest -> StreamEventCallback -> IO (Either ApiError Response)
receiveWsResponse cc onEvent = do
    itemsRef <- newIORef ([] :: [Aeson.Value])
    lifecycleResponseRef <- newIORef (Nothing :: Maybe Response)
    framesRef <- newIORef (0 :: Int)
    bytesRef <- newIORef (0 :: Int64)
    loop itemsRef lifecycleResponseRef framesRef bytesRef
  where
    loop itemsRef lifecycleResponseRef framesRef bytesRef = do
        msgResult <- WebSocket.receiveWebSocketData cc
        case msgResult of
            Left e -> do
                logStreamStats "connection_error" itemsRef framesRef bytesRef
                pure $ Left e
            Right (msgBytes :: LBS.ByteString) -> do
                recordFrame framesRef bytesRef msgBytes
                case ResponsesCodec.decodeResponseStreamEvent msgBytes of
                    Left err -> do
                        let msgPreview = Text.decodeUtf8With Text.lenientDecode (LBS.toStrict msgBytes)
                        logStreamStats "json_decode_error" itemsRef framesRef bytesRef
                        WebSocket.invalidateWebSocketRequest cc
                            "received a malformed response frame"
                        pure $ Left (JsonDecodeError (Text.pack err) (Text.take 500 msgPreview))
                    Right event -> do
                        onEvent event
                        case event of
                            ResponseErrorEvent { streamError } -> do
                                logStreamStats "error_event" itemsRef framesRef bytesRef
                                WebSocket.completeWebSocketRequest cc
                                pure $ Left (parseWsErrorEvent streamError)

                            ResponseNestedErrorEvent { streamError } -> do
                                logStreamStats "error_event" itemsRef framesRef bytesRef
                                WebSocket.completeWebSocketRequest cc
                                pure $ Left (parseWsErrorEvent streamError)

                            ResponseOutputItemDoneEvent { item } -> do
                                modifyIORef' itemsRef (Aeson.toJSON item :)
                                loop itemsRef lifecycleResponseRef framesRef bytesRef

                            ResponseCreatedEvent { response } -> do
                                writeIORef lifecycleResponseRef (Just response)
                                loop itemsRef lifecycleResponseRef framesRef bytesRef

                            ResponseInProgressEvent { response } -> do
                                writeIORef lifecycleResponseRef (Just response)
                                loop itemsRef lifecycleResponseRef framesRef bytesRef

                            ResponseQueuedEvent { response } -> do
                                writeIORef lifecycleResponseRef (Just response)
                                loop itemsRef lifecycleResponseRef framesRef bytesRef

                            ResponseCompletedEvent { response } -> do
                                items <- reverse <$> readIORef itemsRef
                                logStreamStats "completed" itemsRef framesRef bytesRef
                                WebSocket.completeWebSocketRequest cc
                                parseCompletedResponse items (Aeson.toJSON response)

                            ResponseDoneEvent { responseValue } -> do
                                items <- reverse <$> readIORef itemsRef
                                lifecycleResponse <- readIORef lifecycleResponseRef
                                logStreamStats "done" itemsRef framesRef bytesRef
                                WebSocket.completeWebSocketRequest cc
                                pure $
                                    assembleDoneResponse
                                        lifecycleResponse
                                        items
                                        responseValue

                            ResponseIncompleteEvent { response } -> do
                                items <- reverse <$> readIORef itemsRef
                                logStreamStats "incomplete" itemsRef framesRef bytesRef
                                WebSocket.completeWebSocketRequest cc
                                parseCompletedResponse items (Aeson.toJSON response)

                            ResponseFailedEvent { response } -> do
                                logStreamStats "response_failed" itemsRef framesRef bytesRef
                                WebSocket.completeWebSocketRequest cc
                                pure $ Left (failedResponseError response)

                            -- Ignore other event variants (added, content
                            -- deltas, and future event types).
                            _ -> loop itemsRef lifecycleResponseRef framesRef bytesRef

    recordFrame :: IORef Int -> IORef Int64 -> LBS.ByteString -> IO ()
    recordFrame framesRef bytesRef msgBytes = do
        modifyIORef' framesRef (+ 1)
        modifyIORef' bytesRef (+ LBS.length msgBytes)

    logStreamStats :: Text -> IORef [Aeson.Value] -> IORef Int -> IORef Int64 -> IO ()
    logStreamStats _label _itemsRef _framesRef _bytesRef = pure ()

    parseCompletedResponse :: [Aeson.Value] -> Aeson.Value -> IO (Either ApiError Response)
    parseCompletedResponse doneItems responseVal = do
        -- Merge accumulated output items into the response object. The
        -- completed event may contain only messages/reasoning while streamed
        -- tool calls are present solely in response.output_item.done events.
        let patched = mergeCompletedResponseOutput doneItems responseVal
        case Aeson.fromJSON patched of
            Aeson.Success resp -> pure (Right resp)
            Aeson.Error err -> pure $ Left (JsonDecodeError (Text.pack err) (Text.decodeUtf8 (LBS.toStrict (LBS.take 2000 (Aeson.encode patched)))))

    failedResponseMessage :: Response -> Text
    failedResponseMessage response =
        case response.error of
            Just responseError -> responseError.message
            Nothing -> case response.incompleteDetails of
                Just details -> "response.failed: " <> details.reason
                Nothing -> "response.failed (no details)"

    failedResponseError response = case response.error of
        Just responseError ->
            mkOpenAIError
                (errorTypeFromText responseError.code)
                responseError.message
                (Just responseError.code)
                Nothing
        Nothing -> ConnectionError (failedResponseMessage response)

-- | Parse a server @type: error@ event into a structured 'ApiError'.
--
-- The Codex WebSocket sends errors as nested objects under an @error@ key:
--
-- > {"type":"error","error":{
-- >   "type":"usage_limit_reached",
-- >   "message":"The usage limit has been reached",
-- >   "resets_in_seconds":11907,...}}
--
-- We lift typed server errors into 'ProviderError', carrying the exact
-- @resets_in_seconds@ (if present), so the caller can pass rate-limit windows
-- straight through to 'Agent.OpenAI.Auth.reportRateLimit'. A code-only event
-- is also typed; only events with neither a type nor a code fall back to
-- 'ConnectionError' (apart from previous-response wording).
parseWsErrorEvent :: ResponseStreamError -> ApiError
parseWsErrorEvent streamError =
    let parsedError = mkOpenAIError
            (maybe
                (maybe (UnknownErrorType "") errorTypeFromText streamError.code)
                errorTypeFromText
                streamError.errorType)
            streamError.message
            streamError.code
            streamError.retryAfter
    in case (streamError.errorType, streamError.code) of
        (Just _, _) -> parsedError
        (Nothing, Just _) -> parsedError
        (Nothing, Nothing)
            | isPreviousResponseIdError parsedError -> parsedError
            | otherwise -> ConnectionError
                ("WebSocket error (no type): "
                    <> Text.decodeUtf8 (LBS.toStrict (Aeson.encode streamError)))
