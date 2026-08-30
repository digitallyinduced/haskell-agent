module Agent.OpenAI.WebSocketClient
    ( withCodexWs
    , withCodexWsWithProvider
    , withCodexWsWithProviderOrHttpFallback
    , withCodexWsCredential
    , withCodexWsCredentialOrHttpFallback
    , withCodexWsRetrying
    , withCodexWsRetryingAfter
    , sendWsRequest
    , sendWsRequestWithOptions
    , sendWsRequestWithEvents
    , sendWsRequestWithEventsPreservingTurnState
    , sendWsRequestWithRawEvents
    , retryTransientWsResultWithPolicy
    , CodexWsOptions(..)
    , defaultCodexWsOptions
    , buildWsPayloadWithOptions
    , addTurnStateToPayload
    , buildCodexWsHeaders
    , WebSocketEndpoint(..)
    , gatewayWebSocketEndpoint
    , isGatewayWebSocketCredential
    , validateGatewayWebSocketUrl
    , CodexTurnState
    , newCodexTurnState
    , codexConnTurnState
    , readCodexTurnState
    , recordCodexTurnState
    , resetCodexTurnState
    , finishCodexTurnStateResponse
    , copyCodexTurnState
    , withCodexWsRetryingUsingTurnState
    , StreamEventCallback
    , RawStreamEventCallback
    , WebSocketReceiveActions(..)
    , receiveWsResponseWithActions
    , CodexConn
    , codexConnUsesHttpFallback
    , shouldFallbackDirectCodexHandshakeToHttp
    , closeCodexConn
    , CodexAuthFailed(..)
    ) where

import Agent.OpenAI.Auth (Pool)
import Agent.OpenAI.Credential (poolTokenProvider)
import Agent.Error
import Agent.Http.Header (parseRetryAfterSeconds)
import Agent.OpenAI.Error (isPreviousResponseIdError, mkOpenAIError)
import Agent.OpenAI.Features (remoteCompactionV2Feature)
import Agent.OpenAI.ModelMetadata (isCodexResponsesLiteModel)
import Agent.OpenAI.Request (sanitizeCodexRequest)
import Agent.Responses.StreamAssembly
    ( ResponseFailure(..)
    , applyStreamEvent
    , emptyStreamAssemblyState
    , failedStreamResponseMessage
    , finishStreamResponse
    , responseFailureFromState
    )
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
import Control.Applicative ((<|>))
import Control.Retry
    ( RetryPolicyM
    , exponentialBackoff
    , limitRetries
    , retrying
    )
import qualified Control.Exception as Exception
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int64)
import Data.IORef
    ( IORef
    , atomicModifyIORef'
    , newIORef
    , readIORef
    )
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Read as TextRead
import qualified Network.WebSockets as WS
import qualified Network.URI as URI
import qualified Wuss
import Text.Read (readMaybe)

--------------------------------------------------------------------------------
-- Connection handle
--------------------------------------------------------------------------------

-- | Turn-scoped sticky-routing state shared by every physical transport used
-- during one logical Codex turn. Codex treats this as a first-write-wins token:
-- once received from response headers/metadata, it must be replayed unchanged
-- on tool continuations, reconnects, HTTP fallback, and inline compaction.
newtype CodexTurnState = CodexTurnState (IORef (Maybe Text))

data CodexConn = CodexWsConn
    !WebSocket.WebSocketSession
    !CodexTurnState
    | CodexHttpFallback
    !CodexTurnState

newCodexTurnState :: IO CodexTurnState
newCodexTurnState = CodexTurnState <$> newIORef Nothing

codexConnTurnState :: CodexConn -> CodexTurnState
codexConnTurnState (CodexWsConn _ turnState) = turnState
codexConnTurnState (CodexHttpFallback turnState) = turnState

-- | Whether persistent session acquisition had to bypass WebSockets. The
-- marker still owns turn-scoped routing state so the HTTP backend can preserve
-- the same continuation and compaction semantics as a live socket.
codexConnUsesHttpFallback :: CodexConn -> Bool
codexConnUsesHttpFallback CodexWsConn{} = False
codexConnUsesHttpFallback CodexHttpFallback{} = True

readCodexTurnState :: CodexTurnState -> IO (Maybe Text)
readCodexTurnState (CodexTurnState turnState) = readIORef turnState

recordCodexTurnState :: CodexTurnState -> Text -> IO ()
recordCodexTurnState (CodexTurnState turnState) value
    | Text.null (Text.strip value) = pure ()
    | otherwise =
        atomicModifyIORef' turnState \current ->
            (current <|> Just value, ())

resetCodexTurnState :: CodexTurnState -> IO ()
resetCodexTurnState (CodexTurnState turnState) =
    atomicModifyIORef' turnState (const (Nothing, ()))

-- | End a normal model request. Tool calls keep the turn open for their output
-- continuation; every other successful response closes the routing scope.
finishCodexTurnStateResponse :: CodexTurnState -> Response -> IO ()
finishCodexTurnStateResponse turnState response
    | responseHasToolContinuation response = pure ()
    | otherwise = resetCodexTurnState turnState

-- | Close a reusable Codex connection before its owning callback returns.
closeCodexConn :: CodexConn -> IO ()
closeCodexConn (CodexWsConn session _) =
    WebSocket.closeWebSocketSession session "switching account"
closeCodexConn CodexHttpFallback{} = pure ()

-- | Carry the current logical turn's sticky-routing token across a physical
-- reconnect. Codex scopes this state to the turn rather than to the socket.
copyCodexTurnState :: CodexConn -> CodexConn -> IO ()
copyCodexTurnState source destination = do
    snapshot <- readCodexTurnState (codexConnTurnState source)
    let CodexTurnState destinationState = codexConnTurnState destination
    atomicModifyIORef' destinationState (const (snapshot, ()))

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
-- A handshake HTTP 401 is recovered centrally: the rejected account is
-- force-refreshed even when its JWT has not expired, then the handshake is
-- retried once with the rotated token. If refresh or the second handshake
-- fails, the account is cooled down and the next configured account is tried.
-- HTTP 403 remains a permission/policy failure and does not rotate credentials.
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

-- | Persistent-session acquisition with a direct HTTPS escape hatch.
--
-- Some ChatGPT accounts can use the Codex Responses API over HTTPS while the
-- WebSocket upgrade is rejected with HTTP 403 (or 426). Convert only that exact
-- pre-callback handshake error for a direct credential into an HTTP-fallback
-- marker. Gateway credentials remain WebSocket-only, so their rejection is
-- still returned to the token provider and may rotate to a direct account.
withCodexWsWithProviderOrHttpFallback
    :: TokenProvider
    -> (CodexConn -> Credential -> IO a)
    -> IO a
withCodexWsWithProviderOrHttpFallback provider action =
    runWithTokenProvider provider
        (\credential ->
            runPersistentConnectionAttempt
                WebSocket.transientWsConnectRetryPolicy
                credential
                action)
        >>= \case
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

-- | Exact-credential variant of
-- 'withCodexWsWithProviderOrHttpFallback', used by interactive account
-- switches.
withCodexWsCredentialOrHttpFallback
    :: Credential
    -> (CodexConn -> Credential -> IO a)
    -> IO (Either ApiError a)
withCodexWsCredentialOrHttpFallback credential =
    runPersistentConnectionAttempt
        (limitRetries 2 <> exponentialBackoff 500000)
        credential

runPersistentConnectionAttempt
    :: RetryPolicyM IO
    -> Credential
    -> (CodexConn -> Credential -> IO a)
    -> IO (Either ApiError a)
runPersistentConnectionAttempt retryPolicy credential action = do
    result <-
        runConnectionAttemptWithPolicy
            retryPolicy
            credential
            (\conn activeCredential ->
                Right <$> action conn activeCredential)
    case result of
        Left err
            | shouldFallbackDirectCodexHandshakeToHttp credential err -> do
                turnState <- newCodexTurnState
                Right <$> action (CodexHttpFallback turnState) credential
        _ -> pure result

-- | Recognize only the transport's exact pre-upgrade failure. A generic
-- application-level 403 must remain a permission error, and gateways cannot
-- use the direct ChatGPT HTTPS endpoint.
shouldFallbackDirectCodexHandshakeToHttp
    :: Credential
    -> ApiError
    -> Bool
shouldFallbackDirectCodexHandshakeToHttp credential err =
    credential.provider == OpenAIProvider
        && not (isGatewayWebSocketCredential credential)
        && WebSocket.webSocketHandshakeFailureStatus err `elem`
            [Just 403, Just 426]
-- | Run a replay-safe WebSocket action and automatically reacquire a
-- credential after handshake or in-band account failures. The callback is
-- executed again from the beginning after a 401, an explicitly typed
-- authentication error, or a usage-limit error;
-- callers must therefore keep externally visible side effects idempotent.
withCodexWsRetrying
    :: TokenProvider
    -> (CodexConn -> Credential -> IO (Either ApiError a))
    -> IO (Either ApiError a)
withCodexWsRetrying provider action =
    runWithTokenProvider provider \credential ->
        runConnectionAttempt credential action

-- | Like 'withCodexWsRetrying', but attach every disposable physical
-- connection to an existing logical turn. This is used by subagents whose
-- cancellation-safe backend opens a fresh socket for each tool continuation.
withCodexWsRetryingUsingTurnState
    :: TokenProvider
    -> CodexTurnState
    -> (CodexConn -> Credential -> IO (Either ApiError a))
    -> IO (Either ApiError a)
withCodexWsRetryingUsingTurnState provider turnState action =
    runWithTokenProvider provider \credential ->
        runConnectionAttemptUsingTurnState credential turnState action

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
    runConnectionAttemptWithPolicyAndTurnState
        WebSocket.transientWsConnectRetryPolicy
        Nothing

runConnectionAttemptUsingTurnState
    :: Credential
    -> CodexTurnState
    -> (CodexConn -> Credential -> IO (Either ApiError a))
    -> IO (Either ApiError a)
runConnectionAttemptUsingTurnState credential turnState =
    runConnectionAttemptWithPolicyAndTurnState
        WebSocket.transientWsConnectRetryPolicy
        (Just turnState)
        credential

runConnectionAttemptWithPolicy
    :: RetryPolicyM IO
    -> Credential
    -> (CodexConn -> Credential -> IO (Either ApiError a))
    -> IO (Either ApiError a)
runConnectionAttemptWithPolicy retryPolicy =
    runConnectionAttemptWithPolicyAndTurnState retryPolicy Nothing

runConnectionAttemptWithPolicyAndTurnState
    :: RetryPolicyM IO
    -> Maybe CodexTurnState
    -> Credential
    -> (CodexConn -> Credential -> IO (Either ApiError a))
    -> IO (Either ApiError a)
runConnectionAttemptWithPolicyAndTurnState _ _ credential _action
    | credential.provider == XAIProvider = pure $ Left $ ProviderError ApiErrorType
        "XAI credentials must be used through agent-xai"
        Nothing
runConnectionAttemptWithPolicyAndTurnState _ _ credential _action
    | credential.provider == OpenRouterProvider = pure $ Left $ ProviderError ApiErrorType
        "OpenRouter credentials must be used through agent-openrouter"
        Nothing
runConnectionAttemptWithPolicyAndTurnState _ _ credential _action
    | credential.provider == ClaudeCodeProvider = pure $ Left $ ProviderError ApiErrorType
        "Claude Code subscription sessions must use agent-claude"
        Nothing
runConnectionAttemptWithPolicyAndTurnState retryPolicy sharedTurnState
        credential action = do
    let headers = buildCodexWsHeaders credential
    case gatewayWebSocketEndpoint credential of
        Left err -> pure (Left (ConnectionError err))
        Right endpoint ->
            WebSocket.retryTransientWsConnectWithPolicy
                retryPolicy
                \connected ->
                    runCredentialWebSocket endpoint headers \conn -> do
                        connected
                        WebSocket.withWebSocketSession
                            WebSocket.defaultWebSocketSessionOptions
                            conn
                            (\session -> do
                                turnState <- maybe newCodexTurnState pure sharedTurnState
                                action (CodexWsConn session turnState) credential)

data WebSocketEndpoint = WebSocketEndpoint
    { endpointSecure :: !Bool
    , endpointHost :: !String
    , endpointPort :: !Int
    , endpointPath :: !String
    }
    deriving stock (Eq, Show)

-- | Parse the endpoint carried by a gateway credential. A non-WebSocket
-- account id is an ordinary ChatGPT credential. Once the account id declares
-- @ws://@ or @wss://@, parse failures stay errors so the gateway bearer can
-- never fall through to the direct ChatGPT endpoint.
gatewayWebSocketEndpoint
    :: Credential
    -> Either Text (Maybe WebSocketEndpoint)
gatewayWebSocketEndpoint credential
    | not (isGatewayWebSocketCredential credential) = Right Nothing
    | otherwise = Just <$> parseEndpoint credential.accountId

parseEndpoint :: Text -> Either Text WebSocketEndpoint
parseEndpoint raw = do
    uri <- maybe (Left "invalid gateway WebSocket URL") Right $
        URI.parseURI (Text.unpack raw)
    authority <- maybe (Left "gateway WebSocket URL has no host") Right $
        URI.uriAuthority uri
    secure <- case URI.uriScheme uri of
        "wss:" -> Right True
        "ws:"
            | localHost (URI.uriRegName authority) -> Right False
            | otherwise ->
                Left "insecure gateway WebSocket URLs are allowed only for localhost"
        _ -> Left "gateway WebSocket URL must use wss"
    let rawHost = URI.uriRegName authority
        host = case rawHost of
            '[' : rest | not (null rest) && last rest == ']' -> init rest
            _ -> rawHost
        defaultPort = if secure then 443 else 80
        port = case URI.uriPort authority of
            "" -> Right defaultPort
            ':' : portText -> case readMaybe portText of
                Just value | value > 0 && value <= 65535 -> Right value
                _ -> Left "gateway WebSocket URL has an invalid port"
            _ -> Left "gateway WebSocket URL has an invalid port"
        path = case URI.uriPath uri of
            "" -> "/v1/responses"
            value -> value
    endpointPort <- port
    if null host || not (null (URI.uriUserInfo authority))
        || not (null (URI.uriQuery uri))
        || not (null (URI.uriFragment uri))
        then Left "gateway WebSocket URL contains unsupported components"
        else Right WebSocketEndpoint
            { endpointSecure = secure
            , endpointHost = host
            , endpointPort
            , endpointPath = path
            }
  where
    localHost rawHost =
        Text.toLower (Text.pack rawHost)
            `elem` ["localhost", "127.0.0.1", "::1", "[::1]"]

isGatewayWebSocketCredential :: Credential -> Bool
isGatewayWebSocketCredential credential =
    let accountId = Text.toLower (Text.strip credential.accountId)
     in "wss://" `Text.isPrefixOf` accountId
            || "ws://" `Text.isPrefixOf` accountId

validateGatewayWebSocketUrl :: Text -> Either Text ()
validateGatewayWebSocketUrl raw =
    parseEndpoint raw >> pure ()

runCredentialWebSocket
    :: Maybe WebSocketEndpoint
    -> WS.Headers
    -> (WS.Connection -> IO value)
    -> IO value
runCredentialWebSocket endpoint headers action =
    case endpoint of
        Nothing ->
            Wuss.runSecureClientWith
                wsHost
                443
                wsPath
                WS.defaultConnectionOptions
                headers
                action
        Just endpoint
            | endpoint.endpointSecure ->
                Wuss.runSecureClientWith
                    endpoint.endpointHost
                    (fromIntegral endpoint.endpointPort)
                    endpoint.endpointPath
                    WS.defaultConnectionOptions
                    headers
                    action
            | otherwise ->
                WS.runClientWith
                    endpoint.endpointHost
                    endpoint.endpointPort
                    endpoint.endpointPath
                    WS.defaultConnectionOptions
                    headers
                    action

-- | Pure handshake-header builder exported for transport contract tests.
buildCodexWsHeaders :: Credential -> WS.Headers
buildCodexWsHeaders credential =
    [ ("Authorization", "Bearer " <> Text.encodeUtf8 credential.accessToken)
    ]
    <> [ ("chatgpt-account-id", Text.encodeUtf8 credential.accountId)
       | not (Text.null credential.accountId)
       , not (isGatewayWebSocketCredential credential)
       ]
    <> [ ("OpenAI-Beta", "responses_websockets=2026-02-06")
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
    , sendIdleTimeoutMicros :: !(Maybe Int)
    , receiveIdleTimeoutMicros :: !(Maybe Int)
    } deriving (Eq, Show)

defaultCodexWsOptions :: CodexWsOptions
defaultCodexWsOptions = CodexWsOptions
    { compactThreshold = Nothing
    , sendIdleTimeoutMicros = Just 30_000_000
    -- Reasoning-heavy turns can legitimately spend several minutes between
    -- frames. Keep an idle guard, but match Codex's five-minute default
    -- instead of failing healthy long-running requests after one minute.
    , receiveIdleTimeoutMicros = Just 300_000_000
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
        sendWsRequestWithEventsAndOptions
            FinishNormalTurnState
            options
            cc
            request
            previousResponseId
            (\_ -> pure ())

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
    sendWsRequestWithEventsAndOptions
        FinishNormalTurnState
        defaultCodexWsOptions
        cc
        request
        previousResponseId
        onEvent

-- | Send an auxiliary request that belongs to the current logical turn, such
-- as inline remote compaction. Its response may mint the sticky-routing token
-- needed by the immediately following model continuation, so do not clear the
-- token merely because the auxiliary response has no tool call.
sendWsRequestWithEventsPreservingTurnState
    :: CodexConn
    -> ResponseCreateParams
    -> Maybe Text
    -> StreamEventCallback
    -> IO (Either ApiError Response)
sendWsRequestWithEventsPreservingTurnState cc request previousResponseId onEvent =
    sendWsRequestWithEventsAndOptions
        PreserveTurnState
        defaultCodexWsOptions
        cc
        request
        previousResponseId
        onEvent

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
    sendWsRequestWithEventsAndOptions
        FinishNormalTurnState
        options
        cc
        request
        previousResponseId
        \event ->
        onEvent
            (streamEventTypeText (responseStreamEventType event))
            (Aeson.toJSON event)

data TurnStateCompletion
    = FinishNormalTurnState
    | PreserveTurnState
    deriving (Eq)

sendWsRequestWithEventsAndOptions
    :: TurnStateCompletion
    -> CodexWsOptions
    -> CodexConn
    -> ResponseCreateParams
    -> Maybe Text
    -> StreamEventCallback
    -> IO (Either ApiError Response)
sendWsRequestWithEventsAndOptions completion options cc request previousResponseId
        onEvent = case cc of
    CodexWsConn session turnState -> sendOverWs session turnState
    CodexHttpFallback{} ->
        pure $ Left $ ConnectionError
            "OpenAI WebSocket unavailable; HTTPS fallback required"
  where
    sendOverWs session turnState = do
        turnStateValue <- readCodexTurnState turnState
        let wsPayload = addTurnStateToPayload turnStateValue
                (buildWsPayloadWithOptions options request previousResponseId)
            encoded = Aeson.encode wsPayload
        result <- WebSocket.withWebSocketRequestWithTimeout
            options.sendIdleTimeoutMicros
            options.receiveIdleTimeoutMicros
            session
            \wsRequest -> do
            sendRes <- WebSocket.sendWebSocketText wsRequest encoded
            case sendRes of
                Left apiError -> pure (Left apiError)
                Right () ->
                    receiveWsResponse options request.model wsRequest
                        (captureTurnState turnState onEvent)
        case (completion, result) of
            (FinishNormalTurnState, Right response) ->
                finishCodexTurnStateResponse turnState response
            _ -> pure ()
        pure result

captureTurnState
    :: CodexTurnState
    -> StreamEventCallback
    -> StreamEventCallback
captureTurnState turnState callback event = do
    case responseEventTurnState event of
        Just value -> recordCodexTurnState turnState value
        Nothing -> pure ()
    callback event

responseHasToolContinuation :: Response -> Bool
responseHasToolContinuation response =
    any isToolCall response.output
  where
    isToolCall = \case
        FunctionCallItem{} -> True
        CustomToolCallItem{} -> True
        _ -> False

responseEventTurnState :: ResponseStreamEvent -> Maybe Text
responseEventTurnState OtherResponseStreamEvent { eventExtraFields } =
    extractTurnState eventExtraFields
responseEventTurnState _ = Nothing

extractTurnState :: Aeson.Object -> Maybe Text
extractTurnState fields =
    textFieldCaseInsensitive "x-codex-turn-state" fields
        <|> textFieldCaseInsensitive "turn_state" fields
        <|> (lookupFieldCaseInsensitive "headers" fields >>= \case
            Aeson.Object headers ->
                textFieldCaseInsensitive "x-codex-turn-state" headers
                    <|> textFieldCaseInsensitive "turn_state" headers
            _ -> Nothing)

addTurnStateToPayload :: Maybe Text -> Aeson.Value -> Aeson.Value
addTurnStateToPayload Nothing payload = payload
addTurnStateToPayload (Just turnState) (Aeson.Object object) =
    -- WebSocket request headers are fixed at handshake time. The Responses
    -- WebSocket protocol carries this per-request sticky-routing value through
    -- client_metadata instead.
    Aeson.Object
        (KeyMap.insert "client_metadata" metadataValue object)
  where
    metadataValue = case KeyMap.lookup "client_metadata" object of
        Just (Aeson.Object metadata) ->
            Aeson.Object
                (KeyMap.insert "x-codex-turn-state"
                    (Aeson.String turnState)
                    metadata)
        _ -> Aeson.object
            [ "x-codex-turn-state" Aeson..= turnState ]
addTurnStateToPayload _ payload = payload

-- | Pure WebSocket envelope builder, exported for payload contract tests.
-- All fields are flattened at the top level (not nested inside "response").
buildWsPayloadWithOptions :: CodexWsOptions -> ResponseCreateParams -> Maybe Text -> Aeson.Value
buildWsPayloadWithOptions options request previousResponseId =
    case Aeson.toJSON (sanitizeCodexRequest request) of
        Aeson.Object object -> Aeson.Object
            $ addContextManagement
            $ addPreviousResponseId
            $ addResponsesLiteMetadata
            $ KeyMap.insert "stream" (Aeson.Bool True)
            $ KeyMap.insert "store" (Aeson.Bool False)
            $ KeyMap.insert "type" (Aeson.String "response.create") object
        other -> other
  where
    addPreviousResponseId = case previousResponseId of
        Just responseId -> KeyMap.insert "previous_response_id" (Aeson.String responseId)
        Nothing -> KeyMap.delete "previous_response_id"

    addResponsesLiteMetadata object
        | Just modelName <- request.model
        , isCodexResponsesLiteModel modelName =
            let metadata = case KeyMap.lookup "client_metadata" object of
                    Just (Aeson.Object fields) -> fields
                    _ -> KeyMap.empty
            in KeyMap.insert "client_metadata"
                (Aeson.Object (KeyMap.insert
                    "ws_request_header_x_openai_internal_codex_responses_lite"
                    (Aeson.String "true")
                    metadata))
                object
        | otherwise = object

    addContextManagement = case options.compactThreshold of
        Just threshold | threshold > 0 -> KeyMap.insert "context_management" (Aeson.toJSON
            [ Aeson.object
                [ "type" Aeson..= ("compaction" :: Text)
                , "compact_threshold" Aeson..= threshold
                ]
            ])
        _ -> id

data WebSocketReceiveActions = WebSocketReceiveActions
    { receiveFrame      :: !(IO (Either ApiError LBS.ByteString))
    , completeRequest   :: !(IO ())
    , invalidateRequest :: !(Text -> IO ())
    }

receiveWsResponse
    :: CodexWsOptions
    -> Maybe Text
    -> WebSocket.WebSocketRequest
    -> StreamEventCallback
    -> IO (Either ApiError Response)
receiveWsResponse _options modelHint cc =
    receiveWsResponseWithActions modelHint WebSocketReceiveActions
        { receiveFrame = WebSocket.receiveWebSocketData cc
        , completeRequest = WebSocket.completeWebSocketRequest cc
        , invalidateRequest = WebSocket.invalidateWebSocketRequest cc
        }

-- | Injectable receive driver used by the production WebSocket path and by
-- deterministic terminal-boundary regression tests.
receiveWsResponseWithActions
    :: Maybe Text
    -> WebSocketReceiveActions
    -> StreamEventCallback
    -> IO (Either ApiError Response)
receiveWsResponseWithActions modelHint actions onEvent =
    loop emptyStreamAssemblyState 0 0
  where
    loop assembly frames bytes = do
        msgResult <- actions.receiveFrame
        case msgResult of
            Left e -> do
                logStreamStats "connection_error" frames bytes
                pure $ Left e
            Right (msgBytes :: LBS.ByteString) -> do
                let frames' = frames + 1
                    bytes' = bytes + LBS.length msgBytes
                case ResponsesCodec.decodeResponseStreamEvent msgBytes of
                    Left _err -> do
                        -- Codex treats an unrecognised or malformed event as
                        -- forward-compatible noise. Keep receiving so one
                        -- bad frame cannot strand an otherwise valid turn.
                        logStreamStats "json_decode_error" frames' bytes'
                        loop assembly frames' bytes'
                    Right event -> do
                        onEvent event
                        let assembly' = applyStreamEvent assembly event
                        case event of
                            ResponseErrorEvent { streamError, eventExtraFields } -> do
                                logStreamStats "error_event" frames' bytes'
                                actions.invalidateRequest "WebSocket response error"
                                pure $ Left
                                    (parseWsErrorEvent eventExtraFields streamError)

                            ResponseNestedErrorEvent { streamError, eventExtraFields } -> do
                                logStreamStats "error_event" frames' bytes'
                                actions.invalidateRequest "WebSocket response error"
                                pure $ Left
                                    (parseWsErrorEvent eventExtraFields streamError)

                            ResponseCompletedEvent{} ->
                                finishTerminal "completed" assembly' frames' bytes' event

                            ResponseDoneEvent{} ->
                                finishTerminal "done" assembly' frames' bytes' event

                            ResponseIncompleteEvent{} ->
                                do
                                    logStreamStats "incomplete" frames' bytes'
                                    actions.invalidateRequest
                                        "WebSocket response incomplete"
                                    pure $ Left
                                        (failedResponseError
                                            ((responseFailureFromState assembly')
                                                { failureStatus =
                                                    Just "incomplete" }))

                            ResponseFailedEvent{} -> do
                                logStreamStats "response_failed" frames' bytes'
                                actions.invalidateRequest "WebSocket response failed"
                                pure $ Left
                                    (failedResponseError
                                        ((responseFailureFromState assembly')
                                            { failureStatus = Just "failed" }))

                            -- Ignore other event variants (added, content
                            -- deltas, and future event types).
                            _ -> loop assembly' frames' bytes'

    finishTerminal label assembly frames bytes event = do
        logStreamStats label frames bytes
        actions.completeRequest
        pure (finishStreamResponse modelHint assembly event)

    logStreamStats :: Text -> Int -> Int64 -> IO ()
    logStreamStats _label _frames _bytes = pure ()

    failedResponseError failure =
        case failure.failureErrorType
                <|> failure.failureErrorCode
                <|> failure.failureErrorMessage of
            Nothing -> ConnectionError (failedStreamResponseMessage failure)
            Just _ ->
                mkOpenAIError
                    (maybe ApiErrorType errorTypeFromText
                        (failure.failureErrorType <|> failure.failureErrorCode))
                    (failedStreamResponseMessage failure)
                    failure.failureErrorCode
                    Nothing

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
-- is also typed; untyped events use an outer HTTP status when one is
-- available, otherwise falling back to 'ConnectionError'.
parseWsErrorEvent :: Aeson.Object -> ResponseStreamError -> ApiError
parseWsErrorEvent outerFields streamError =
    let typedError =
            nonBlank =<< (streamError.errorType <|> streamError.code)
        retryAfter =
            streamError.retryAfter <|> outerRetryAfter outerFields
        parsedError = mkOpenAIError
            (maybe
                (UnknownErrorType "")
                errorTypeFromText
                typedError)
            streamError.message
            streamError.code
            retryAfter
        outerStatus = lookupFieldCaseInsensitive "status" outerFields
            >>= jsonInt
        outerStatusCode = lookupFieldCaseInsensitive "status_code" outerFields
            >>= jsonInt
        status = outerStatus <|> outerStatusCode
    in case (typedError, status) of
        (Just _, _) -> parsedError
        (Nothing, Just 429) ->
            ProviderError RateLimitError streamError.message retryAfter
        (Nothing, Just status) ->
            HttpError status streamError.message
        (Nothing, Nothing)
            | isPreviousResponseIdError parsedError -> parsedError
            | otherwise -> ConnectionError
                ("WebSocket error (no type): "
                    <> Text.decodeUtf8 (LBS.toStrict (Aeson.encode streamError)))

outerRetryAfter :: Aeson.Object -> Maybe Int
outerRetryAfter outerFields = do
    Aeson.Object headers <-
        lookupFieldCaseInsensitive "headers" outerFields
    value <- lookupFieldCaseInsensitive "retry-after" headers
    case value of
        Aeson.String text ->
            parseRetryAfterSeconds [Text.encodeUtf8 text]
        Aeson.Number{} ->
            max 1 <$> jsonInt value
        _ -> Nothing

jsonInt :: Aeson.Value -> Maybe Int
jsonInt = \case
    Aeson.Number value -> case Aeson.fromJSON (Aeson.Number value) of
        Aeson.Success parsed -> Just parsed
        Aeson.Error _ -> Nothing
    Aeson.String value -> case TextRead.decimal value of
        Right (parsed, remainder) | Text.null remainder -> Just parsed
        _ -> Nothing
    _ -> Nothing

lookupFieldCaseInsensitive :: Text -> Aeson.Object -> Maybe Aeson.Value
lookupFieldCaseInsensitive wanted object =
    snd <$> find
        (\(key, _) ->
            Text.toCaseFold (Key.toText key) == Text.toCaseFold wanted)
        (KeyMap.toList object)

textFieldCaseInsensitive :: Text -> Aeson.Object -> Maybe Text
textFieldCaseInsensitive name object =
    lookupFieldCaseInsensitive name object >>= \case
        Aeson.String value -> Just value
        _ -> Nothing

nonBlank :: Text -> Maybe Text
nonBlank value
    | Text.null (Text.strip value) = Nothing
    | otherwise = Just value
