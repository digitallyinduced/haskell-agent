{-# LANGUAGE ScopedTypeVariables #-}

module Agent.Transport.WebSocket
    ( WebSocketSession
    , WebSocketRequest
    , WebSocketSessionOptions(..)
    , defaultWebSocketSessionOptions
    , withWebSocketSession
    , withWebSocketRequest
    , completeWebSocketRequest
    , invalidateWebSocketRequest
    , sendWebSocketText
    , receiveWebSocketData
    , transientWsConnectRetryPolicy
    , retryTransientWsConnectWithPolicy
    , transientWsConnectFailureLabel
    , transientWsMidRunFailureLabel
    , wsHandshakeAuthFailure
    , wsHandshakeAuthFailureStatus
    ) where

import Agent.Error (ApiError(..))
import Agent.Retry
    ( ExceptionRetry(..)
    , retryingBeforeCommit
    )
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM
import qualified Control.Exception as Exception
import qualified Control.Exception.Safe as Safe
import Control.Retry
    ( RetryPolicyM
    , capDelay
    , exponentialBackoff
    )
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text (lenientDecode)
import Data.Word (Word64)
import GHC.IO.Exception (IOErrorType(ResourceVanished))
import qualified Network.Connection as Connection
import qualified Network.TLS as TLS
import qualified Network.WebSockets as WS
import System.IO.Error (ioeGetErrorType)

-- | A provider-neutral, reusable WebSocket session. The underlying socket is
-- owned by a background pump for the duration of 'withWebSocketSession'.
data WebSocketSession = WebSocketSession
    { sessionOut :: !(TQueue OutCommand)
    , sessionIn :: !(TBQueue LBS.ByteString)
    , sessionState :: !(TVar SessionState)
    }

-- | Opaque ownership token for one request/response exchange.
data WebSocketRequest = WebSocketRequest
    { requestSession :: !WebSocketSession
    , requestGeneration :: !Word64
    }

data SessionState
    = SessionIdle !Word64
    | SessionActive !Word64
    | SessionFinished !Word64
    | SessionClosed !(Maybe Word64) !ApiError
    | SessionPoisoned !ApiError

-- | Transport-level settings for a reusable WebSocket session.
data WebSocketSessionOptions = WebSocketSessionOptions
    { inboundFrameCapacity :: !Int
    -- ^ Maximum number of unread data frames buffered in memory.
    , clientPingIntervalSeconds :: !(Maybe Int)
    -- ^ Optional interval for client-initiated keepalive pings.
    } deriving (Eq, Show)

defaultWebSocketSessionOptions :: WebSocketSessionOptions
defaultWebSocketSessionOptions = WebSocketSessionOptions
    { inboundFrameCapacity = 128
    , clientPingIntervalSeconds = Just 20
    }

data OutCommand
    = SendText !Word64 !LBS.ByteString !(TMVar (Either ApiError ()))
    | SendControl !WS.Message
    | CloseSession !LBS.ByteString

-- | Run an action with a reusable WebSocket session.
--
-- A single pump continuously reads the socket, responds to server pings, and
-- serializes all writes. This keeps the connection alive while callers are
-- between requests, such as during long-running tool execution.
withWebSocketSession
    :: WebSocketSessionOptions
    -> WS.Connection
    -> (WebSocketSession -> IO value)
    -> IO value
withWebSocketSession options connection action =
    withClientPings do
        outbound <- newTQueueIO
        inbound <- newTBQueueIO (fromIntegral (max 1 options.inboundFrameCapacity))
        state <- newTVarIO (SessionIdle 0)
        let session = WebSocketSession
                { sessionOut = outbound
                , sessionIn = inbound
                , sessionState = state
                }
        withAsync (readerLoop connection session) \_ ->
            withAsync (writerLoop connection session) \_ ->
                action session
  where
    withClientPings inner = case options.clientPingIntervalSeconds of
        Just seconds | seconds > 0 -> WS.withPingThread connection seconds (pure ()) inner
        _ -> inner

-- | Serialize one request/response exchange on a reusable session.
--
-- Only 'completeWebSocketRequest' permits reuse. If the action throws,
-- including through asynchronous cancellation, or returns without marking a
-- terminal frame, the session is atomically poisoned. A later request
-- therefore reconnects instead of observing abandoned frames.
withWebSocketRequest
    :: WebSocketSession
    -> (WebSocketRequest -> IO (Either ApiError value))
    -> IO (Either ApiError value)
withWebSocketRequest session action = Safe.mask \restore -> do
    acquired <- atomically (acquireRequest session)
    case acquired of
        Left apiError -> pure (Left apiError)
        Right request -> do
            result <- restore (action request)
                `Safe.onException`
                    invalidateWebSocketRequest request "response consumer interrupted"
            atomically (finishRequest request)
            pure result

-- | Send one text frame through the request's serialized writer.
sendWebSocketText :: WebSocketRequest -> LBS.ByteString -> IO (Either ApiError ())
sendWebSocketText request bytes = do
    reply <- newEmptyTMVarIO
    queued <- atomically (queueRequestText request bytes reply)
    case queued of
        Left apiError -> pure (Left apiError)
        Right () -> atomically $
            takeTMVar reply
            `orElse`
            requestFailure request

-- | Receive the next application data frame. Control frames are handled by
-- the session pump and are never exposed to callers.
receiveWebSocketData :: WebSocketRequest -> IO (Either ApiError LBS.ByteString)
receiveWebSocketData request = atomically do
    let session = request.requestSession
    state <- readTVar session.sessionState
    case state of
        SessionActive generation
            | generation == request.requestGeneration ->
                Right <$> readTBQueue session.sessionIn
        SessionClosed owner apiError
            | owner == Just request.requestGeneration -> do
                frame <- tryReadTBQueue session.sessionIn
                pure (maybe (Left apiError) Right frame)
        SessionClosed _ apiError -> pure (Left apiError)
        SessionPoisoned apiError -> pure (Left apiError)
        _ -> pure (Left inactiveRequestError)

queueInboundFrame :: WebSocketSession -> LBS.ByteString -> STM ()
queueInboundFrame session bytes = do
    state <- readTVar session.sessionState
    case state of
        SessionActive _ -> writeTBQueue session.sessionIn bytes
        SessionIdle _ -> do
            let apiError = invalidatedSessionError
                    "received a response frame without an active request"
            writeTVar session.sessionState (SessionPoisoned apiError)
            writeTQueue session.sessionOut
                (CloseSession "unexpected response frame")
        SessionFinished _ -> do
            let apiError = invalidatedSessionError
                    "received a response frame after request completion"
            writeTVar session.sessionState (SessionPoisoned apiError)
            writeTQueue session.sessionOut
                (CloseSession "response frame after completion")
        SessionClosed _ _ -> pure ()
        SessionPoisoned _ -> pure ()

closeSession :: WebSocketSession -> ApiError -> STM ()
closeSession session apiError = do
    state <- readTVar session.sessionState
    case state of
        SessionIdle _ ->
            writeTVar session.sessionState
                (SessionClosed Nothing apiError)
        SessionActive generation ->
            writeTVar session.sessionState
                (SessionClosed (Just generation) apiError)
        SessionFinished generation ->
            writeTVar session.sessionState
                (SessionClosed (Just generation) apiError)
        SessionClosed _ _ -> pure ()
        SessionPoisoned _ -> pure ()

requestGenerationActive :: WebSocketSession -> Word64 -> STM Bool
requestGenerationActive session generation = do
    state <- readTVar session.sessionState
    pure $ case state of
        SessionActive activeGeneration ->
            activeGeneration == generation
        _ -> False

queueControl :: WebSocketSession -> WS.Message -> STM ()
queueControl session message = do
    state <- readTVar session.sessionState
    case state of
        SessionIdle _ -> writeTQueue session.sessionOut (SendControl message)
        SessionActive _ -> writeTQueue session.sessionOut (SendControl message)
        SessionFinished _ -> writeTQueue session.sessionOut (SendControl message)
        SessionClosed _ _ -> pure ()
        SessionPoisoned _ -> pure ()

finishRequest :: WebSocketRequest -> STM ()
finishRequest request = do
    let session = request.requestSession
    state <- readTVar session.sessionState
    case state of
        SessionActive generation
            | generation == request.requestGeneration -> do
                let apiError = invalidatedSessionError
                        "request returned before response completion"
                writeTVar session.sessionState
                    (SessionPoisoned apiError)
                writeTQueue session.sessionOut
                    (CloseSession "response not completed")
        SessionFinished generation
            | generation == request.requestGeneration -> do
                empty <- isEmptyTBQueue session.sessionIn
                if empty
                    then writeTVar session.sessionState
                        (SessionIdle (generation + 1))
                    else do
                        let apiError = invalidatedSessionError
                                "response frames remained after request completion"
                        writeTVar session.sessionState
                            (SessionPoisoned apiError)
                        writeTQueue session.sessionOut
                            (CloseSession "response boundary lost")
        _ -> pure ()

-- | Mark the protocol terminal frame as consumed. A request scope that
-- returns normally without this transition is poisoned rather than reused.
completeWebSocketRequest :: WebSocketRequest -> IO ()
completeWebSocketRequest request = atomically do
    let session = request.requestSession
    state <- readTVar session.sessionState
    case state of
        SessionActive generation
            | generation == request.requestGeneration ->
                writeTVar session.sessionState
                    (SessionFinished generation)
        _ -> pure ()

-- | Poison an exchange whose response boundary cannot be recovered, such as
-- after a malformed frame. This is idempotent and never affects a newer
-- generation if a stale request token escapes its scope.
invalidateWebSocketRequest :: WebSocketRequest -> Text -> IO ()
invalidateWebSocketRequest request reason = atomically do
    let session = request.requestSession
    state <- readTVar session.sessionState
    case state of
        SessionActive generation
            | generation == request.requestGeneration -> do
                let apiError = invalidatedSessionError reason
                writeTVar session.sessionState (SessionPoisoned apiError)
                writeTQueue session.sessionOut
                    (CloseSession (LBS.fromStrict (Text.encodeUtf8 reason)))
        SessionFinished generation
            | generation == request.requestGeneration -> do
                let apiError = invalidatedSessionError reason
                writeTVar session.sessionState (SessionPoisoned apiError)
                writeTQueue session.sessionOut
                    (CloseSession (LBS.fromStrict (Text.encodeUtf8 reason)))
        _ -> pure ()

queueRequestText
    :: WebSocketRequest
    -> LBS.ByteString
    -> TMVar (Either ApiError ())
    -> STM (Either ApiError ())
queueRequestText request bytes reply = do
    let session = request.requestSession
    state <- readTVar session.sessionState
    case state of
        SessionActive generation
            | generation == request.requestGeneration -> do
                writeTQueue session.sessionOut
                    (SendText generation bytes reply)
                pure (Right ())
        SessionClosed _ apiError -> pure (Left apiError)
        SessionPoisoned apiError -> pure (Left apiError)
        _ -> pure (Left inactiveRequestError)

requestFailure :: WebSocketRequest -> STM (Either ApiError ())
requestFailure request = do
    state <- readTVar request.requestSession.sessionState
    case state of
        SessionActive generation
            | generation == request.requestGeneration -> retry
        SessionClosed _ apiError -> pure (Left apiError)
        SessionPoisoned apiError -> pure (Left apiError)
        _ -> pure (Left inactiveRequestError)

inactiveRequestError :: ApiError
inactiveRequestError = ConnectionError "WebSocket request is no longer active"

invalidatedSessionError :: Text -> ApiError
invalidatedSessionError reason =
    ConnectionError ("WebSocket session invalidated: " <> reason)

acquireRequest :: WebSocketSession -> STM (Either ApiError WebSocketRequest)
acquireRequest session = do
    state <- readTVar session.sessionState
    case state of
        SessionIdle generation -> do
            empty <- isEmptyTBQueue session.sessionIn
            if empty
                then do
                    writeTVar session.sessionState (SessionActive generation)
                    pure (Right (WebSocketRequest session generation))
                else do
                    let apiError = invalidatedSessionError
                            "unclaimed response frames were queued"
                    writeTVar session.sessionState (SessionPoisoned apiError)
                    writeTQueue session.sessionOut
                        (CloseSession "unclaimed response frames")
                    pure (Left apiError)
        SessionActive _ -> retry
        SessionFinished _ -> retry
        SessionClosed _ apiError -> pure (Left apiError)
        SessionPoisoned apiError -> pure (Left apiError)

readerLoop
    :: WS.Connection
    -> WebSocketSession
    -> IO ()
readerLoop connection session = loop
  where
    loop = do
        result <- Exception.try @Exception.SomeException (WS.receive connection)
        case result of
            Left exception
                | isAsyncException exception -> Exception.throwIO exception
                | otherwise -> terminate
                    ("WebSocket receive error: " <> showText exception)
            Right (WS.DataMessage _ _ _ message) -> do
                let bytes = case message of
                        WS.Text value _ -> value
                        WS.Binary value -> value
                atomically (queueInboundFrame session bytes)
                loop
            Right (WS.ControlMessage (WS.Ping payload)) -> do
                atomically (queueControl session
                    (WS.ControlMessage (WS.Pong payload)))
                loop
            Right (WS.ControlMessage (WS.Pong _)) -> loop
            Right (WS.ControlMessage (WS.Close code reason)) ->
                terminate
                    ("WebSocket closed by server (" <> showText code <> "): "
                        <> Text.decodeUtf8With Text.lenientDecode (LBS.toStrict reason))

    terminate reason =
        atomically (closeSession session (ConnectionError reason))

writerLoop
    :: WS.Connection
    -> WebSocketSession
    -> IO ()
writerLoop connection session = loop
  where
    loop = do
        command <- atomically (readTQueue session.sessionOut)
        case command of
            CloseSession reason -> do
                result <- Exception.try @Exception.SomeException
                    (WS.sendClose connection reason)
                case result of
                    Left exception | isAsyncException exception ->
                        Exception.throwIO exception
                    _ -> pure ()
            SendControl message -> do
                result <- Exception.try @Exception.SomeException (WS.send connection message)
                case result of
                    Left exception | isAsyncException exception -> Exception.throwIO exception
                    Left exception -> atomically (closeSession session
                        (ConnectionError
                            ("WebSocket send error: " <> showText exception)))
                    Right () -> loop
            SendText generation bytes reply -> do
                allowed <- atomically (requestGenerationActive session generation)
                if not allowed
                    then do
                        atomically (putTMVar reply (Left inactiveRequestError))
                        loop
                    else do
                        result <- Exception.try @Exception.SomeException
                            (WS.sendTextData connection bytes)
                        case result of
                            Left exception | isAsyncException exception ->
                                Exception.throwIO exception
                            Left exception -> do
                                let apiError = ConnectionError
                                        ("WebSocket send error: " <> showText exception)
                                atomically do
                                    putTMVar reply (Left apiError)
                                    closeSession session apiError
                            Right () -> do
                                atomically (putTMVar reply (Right ()))
                                loop

isAsyncException :: Exception.SomeException -> Bool
isAsyncException exception =
    isJust (Exception.fromException exception :: Maybe Exception.SomeAsyncException)

-- | Capped exponential backoff for transient WebSocket connection / handshake
-- failures: 1s, 2s, 4s, 8s, then 15s until the connection succeeds or the
-- caller cancels the operation.
transientWsConnectRetryPolicy :: RetryPolicyM IO
transientWsConnectRetryPolicy =
    capDelay 15000000 (exponentialBackoff 1000000)

-- | Normalize and retry WebSocket connection / handshake failures until the
-- connection callback begins. Once the callback has started, retrying the
-- enclosing action could replay visible effects from an active session, so
-- later exceptions are propagated unchanged.
--
-- The policy is injectable so tests can retry without sleeping.
retryTransientWsConnectWithPolicy
    :: RetryPolicyM IO
    -> (IO () -> IO (Either ApiError value))
    -> IO (Either ApiError value)
retryTransientWsConnectWithPolicy policy =
    retryingBeforeCommit policy wsConnectExceptionRetry

-- | Classify transient failures that happen before a WebSocket session is
-- established.
transientWsConnectFailureLabel :: Exception.SomeException -> Maybe Text
transientWsConnectFailureLabel exception =
    case wsConnectExceptionRetry exception of
        RetryException (ConnectionError message) -> Just message
        RetryException (HttpError _ message) -> Just message
        _ -> Nothing

wsConnectExceptionRetry
    :: Exception.SomeException
    -> ExceptionRetry ApiError
wsConnectExceptionRetry exception
    | Just authError <- wsHandshakeAuthFailure exception =
        StopException authError
    | Just (WS.MalformedResponse responseHead _reason) <-
        Exception.fromException exception =
            let status = WS.responseCode responseHead
                apiError = HttpError status
                    ("WebSocket handshake returned HTTP " <> showText status)
            in if status `elem` retryableHandshakeStatusCodes
                then RetryException apiError
                else StopException apiError
    | Just WS.ConnectionTimeout <- Exception.fromException exception =
        retryConnection "WebSocket handshake timed out"
    | Just (Connection.HostNotResolved host) <-
        Exception.fromException exception =
            retryConnection
                ("WebSocket host could not be resolved: " <> Text.pack host)
    | Just (Connection.HostCannotConnect host failures) <-
        Exception.fromException exception =
            retryConnection
                ("WebSocket host could not be reached: "
                    <> Text.pack host
                    <> formatNestedFailures failures)
    | Just tlsException <- Exception.fromException exception =
        classifyTlsException tlsException
    | Just (ioException :: IOError) <- Exception.fromException exception =
        retryConnection ("WebSocket connect IO error: " <> showText ioException)
    | otherwise =
        StopException $ ConnectionError
            ("WebSocket connection failed: " <> showText exception)
  where
    retryableHandshakeStatusCodes = [408, 425, 429, 500, 502, 503, 504]
    retryConnection = RetryException . ConnectionError
    formatNestedFailures [] = ""
    formatNestedFailures failures = " (" <> showText failures <> ")"

classifyTlsException :: TLS.TLSException -> ExceptionRetry ApiError
classifyTlsException tlsException =
    let message = tlsExceptionMessage tlsException
        transient = RetryException (ConnectionError message)
        permanent = StopException (ConnectionError message)
    in case tlsException of
        TLS.HandshakeFailed TLS.Error_EOF -> transient
        TLS.HandshakeFailed TLS.Error_TCP_Terminate -> transient
        TLS.HandshakeFailed (TLS.Error_Misc detail)
            | isTransientNetworkErrorText (Text.pack detail) -> transient
        TLS.Terminated _ _ TLS.Error_EOF -> transient
        TLS.Terminated _ _ TLS.Error_TCP_Terminate -> transient
        TLS.Terminated _ _ (TLS.Error_Misc detail)
            | isTransientNetworkErrorText (Text.pack detail) -> transient
        _ -> permanent

tlsExceptionMessage :: TLS.TLSException -> Text
tlsExceptionMessage = \case
    TLS.HandshakeFailed (TLS.Error_Misc detail) ->
        "TLS handshake failed: " <> Text.pack detail
    TLS.HandshakeFailed TLS.Error_EOF ->
        "TLS handshake failed: unexpected EOF"
    TLS.HandshakeFailed TLS.Error_TCP_Terminate ->
        "TLS handshake failed: TCP connection terminated"
    tlsException ->
        "TLS handshake failed: " <> showText tlsException

-- | Extract an authentication status from a rejected WebSocket handshake.
wsHandshakeAuthFailureStatus :: Exception.SomeException -> Maybe Int
wsHandshakeAuthFailureStatus exception = case Exception.fromException exception of
    Just (WS.MalformedResponse responseHead _reason)
        | WS.responseCode responseHead `elem` [401, 403] ->
            Just (WS.responseCode responseHead)
    _ -> Nothing

-- | Convert a raw WebSocket handshake 401/403 into the shared HTTP error
-- shape. Headers are intentionally omitted because they may contain cookies.
wsHandshakeAuthFailure :: Exception.SomeException -> Maybe ApiError
wsHandshakeAuthFailure exception = do
    status <- wsHandshakeAuthFailureStatus exception
    pure (HttpError status ("WebSocket handshake returned HTTP " <> showText status))

-- | Classify transient failures that happen during a WebSocket session.
transientWsMidRunFailureLabel :: Exception.SomeException -> Maybe Text
transientWsMidRunFailureLabel exception
    | Just (WS.CloseRequest code _reason) <- Exception.fromException exception =
        Just ("WebSocket closed by server (" <> showText code <> ")")
    | Just WS.ConnectionClosed <- Exception.fromException exception =
        Just "WebSocket connection closed"
    | Just (WS.ParseException message) <- Exception.fromException exception =
        Just ("WebSocket parse error: " <> Text.pack message)
    | Just (ioException :: IOError) <- Exception.fromException exception
    , isTransientIOError ioException =
        Just ("WebSocket IO error: " <> showText ioException)
    | otherwise = Nothing

isTransientIOError :: IOError -> Bool
isTransientIOError ioException =
    ioeGetErrorType ioException == ResourceVanished
        || isTransientNetworkErrorText (showText ioException)

isTransientNetworkErrorText :: Text -> Bool
isTransientNetworkErrorText message =
    any (`Text.isInfixOf` Text.toLower message)
        [ "connection reset"
        , "broken pipe"
        , "connection refused"
        , "network is unreachable"
        , "timed out"
        ]

showText :: Show value => value -> Text
showText = Text.pack . show
