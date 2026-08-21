{-# LANGUAGE ScopedTypeVariables #-}

module Agent.Transport.WebSocket
    ( WebSocketSession
    , WebSocketSessionOptions(..)
    , defaultWebSocketSessionOptions
    , withWebSocketSession
    , withWebSocketRequest
    , invalidateWebSocketSession
    , sendWebSocketText
    , receiveWebSocketData
    , transientWsRetryDelayMicros
    , transientWsConnectFailureLabel
    , transientWsMidRunFailureLabel
    , wsHandshakeAuthFailure
    , wsHandshakeAuthFailureStatus
    ) where

import Agent.Error (ApiError(..))
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM
import qualified Control.Exception as Exception
import Control.Exception.Safe (onException)
import Control.Monad (unless)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text (lenientDecode)
import GHC.IO.Exception (IOErrorType(ResourceVanished))
import qualified Network.WebSockets as WS
import System.IO.Error (ioeGetErrorType)

-- | A provider-neutral, reusable WebSocket session. The underlying socket is
-- owned by a background pump for the duration of 'withWebSocketSession'.
data WebSocketSession = WebSocketSession
    { sessionOut :: !(TQueue OutCommand)
    , sessionIn :: !(TBQueue (Either ApiError LBS.ByteString))
    , sessionClosed :: !(TVar Bool)
    , sessionPoisoned :: !(TVar (Maybe ApiError))
    }

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
    = SendText !LBS.ByteString !(TMVar (Either ApiError ()))
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
        closed <- newTVarIO False
        poisoned <- newTVarIO Nothing
        let session = WebSocketSession
                { sessionOut = outbound
                , sessionIn = inbound
                , sessionClosed = closed
                , sessionPoisoned = poisoned
                }
        withAsync (readerLoop connection inbound outbound closed poisoned) \_ ->
            withAsync (writerLoop connection outbound closed) \_ ->
                action session
  where
    withClientPings inner = case options.clientPingIntervalSeconds of
        Just seconds | seconds > 0 -> WS.withPingThread connection seconds (pure ()) inner
        _ -> inner

-- | Poison the reusable connection if a response consumer is interrupted.
-- Once interrupted, its remaining frames cannot be assigned safely to a
-- later request.
withWebSocketRequest :: WebSocketSession -> IO value -> IO value
withWebSocketRequest session action =
    action `onException`
        invalidateWebSocketSession session "response consumer interrupted"

-- | Mark a response boundary as lost and asynchronously close the socket.
invalidateWebSocketSession :: WebSocketSession -> Text -> IO ()
invalidateWebSocketSession session reason = atomically do
    current <- readTVar session.sessionPoisoned
    case current of
        Just _ -> pure ()
        Nothing -> do
            let apiError = ConnectionError
                    ("WebSocket session invalidated: " <> reason)
            writeTVar session.sessionPoisoned (Just apiError)
            writeTVar session.sessionClosed True
            writeTQueue session.sessionOut
                (CloseSession (LBS.fromStrict (Text.encodeUtf8 reason)))

-- | Send one text frame through the session's serialized writer.
sendWebSocketText :: WebSocketSession -> LBS.ByteString -> IO (Either ApiError ())
sendWebSocketText session bytes = do
    reply <- newEmptyTMVarIO
    queued <- atomically do
        poisoned <- readTVar session.sessionPoisoned
        closed <- readTVar session.sessionClosed
        case poisoned of
            Just apiError -> pure (Left apiError)
            Nothing
                | closed -> pure (Left connectionClosedError)
                | otherwise -> do
                    writeTQueue session.sessionOut (SendText bytes reply)
                    pure (Right ())
    case queued of
        Left apiError -> pure (Left apiError)
        Right () -> atomically $
            takeTMVar reply
            `orElse`
            poisonedSession session
            `orElse`
            (do
                closed <- readTVar session.sessionClosed
                check closed
                pure (Left connectionClosedError))

-- | Receive the next application data frame. Control frames are handled by
-- the session pump and are never exposed to callers.
receiveWebSocketData :: WebSocketSession -> IO (Either ApiError LBS.ByteString)
receiveWebSocketData session = atomically do
    poisoned <- readTVar session.sessionPoisoned
    case poisoned of
        Just apiError -> pure (Left apiError)
        Nothing ->
            readTBQueue session.sessionIn
            `orElse`
            (do
                closed <- readTVar session.sessionClosed
                check closed
                pure (Left connectionClosedError))

poisonedSession :: WebSocketSession -> STM (Either ApiError value)
poisonedSession session = do
    poisoned <- readTVar session.sessionPoisoned
    case poisoned of
        Just apiError -> pure (Left apiError)
        Nothing -> retry

connectionClosedError :: ApiError
connectionClosedError = ConnectionError "WebSocket connection closed"

readerLoop
    :: WS.Connection
    -> TBQueue (Either ApiError LBS.ByteString)
    -> TQueue OutCommand
    -> TVar Bool
    -> TVar (Maybe ApiError)
    -> IO ()
readerLoop connection inbound outbound closed poisoned = loop
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
                stopped <- atomically do
                    readTVar poisoned >>= \case
                        Just _ -> pure True
                        Nothing -> writeTBQueue inbound (Right bytes) >> pure False
                unless stopped loop
            Right (WS.ControlMessage (WS.Ping payload)) -> do
                atomically (writeTQueue outbound
                    (SendControl (WS.ControlMessage (WS.Pong payload))))
                loop
            Right (WS.ControlMessage (WS.Pong _)) -> loop
            Right (WS.ControlMessage (WS.Close code reason)) ->
                terminate
                    ("WebSocket closed by server (" <> showText code <> "): "
                        <> Text.decodeUtf8With Text.lenientDecode (LBS.toStrict reason))

    terminate reason = atomically do
        writeTVar closed True
        readTVar poisoned >>= \case
            Just _ -> pure ()
            Nothing -> do
                full <- isFullTBQueue inbound
                unless full $
                    writeTBQueue inbound (Left (ConnectionError reason))

writerLoop
    :: WS.Connection
    -> TQueue OutCommand
    -> TVar Bool
    -> IO ()
writerLoop connection outbound closed = loop
  where
    loop = do
        command <- atomically (readTQueue outbound)
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
                    Left _ -> atomically (writeTVar closed True)
                    Right () -> loop
            SendText bytes reply -> do
                result <- Exception.try @Exception.SomeException
                    (WS.sendTextData connection bytes)
                case result of
                    Left exception | isAsyncException exception -> Exception.throwIO exception
                    Left exception -> do
                        let apiError = ConnectionError
                                ("WebSocket send error: " <> showText exception)
                        atomically do
                            putTMVar reply (Left apiError)
                            writeTVar closed True
                    Right () -> do
                        atomically (putTMVar reply (Right ()))
                        loop

isAsyncException :: Exception.SomeException -> Bool
isAsyncException exception =
    isJust (Exception.fromException exception :: Maybe Exception.SomeAsyncException)

-- | Small fixed backoff before retrying a transient WebSocket connect /
-- handshake failure.
transientWsRetryDelayMicros :: Int
transientWsRetryDelayMicros = 3000000

-- | Classify transient failures that happen before a WebSocket session is
-- established.
transientWsConnectFailureLabel :: Exception.SomeException -> Maybe Text
transientWsConnectFailureLabel exception = case Exception.fromException exception of
    Just WS.ConnectionTimeout ->
        Just "WebSocket handshake timed out"
    Just (WS.MalformedResponse responseHead _reason)
        | WS.responseCode responseHead `elem` retryableHandshakeStatusCodes ->
            Just ("WebSocket handshake returned HTTP " <> showText (WS.responseCode responseHead))
    _ -> Nothing
  where
    retryableHandshakeStatusCodes = [408, 425, 429, 500, 502, 503, 504]

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
  where
    isTransientIOError :: IOError -> Bool
    isTransientIOError ioException =
        ioeGetErrorType ioException == ResourceVanished || matchesTransientMessage
      where
        matchesTransientMessage =
            any (`Text.isInfixOf` message)
                [ "connection reset"
                , "broken pipe"
                , "connection refused"
                , "network is unreachable"
                , "timed out"
                ]
        message = Text.toLower (showText ioException)

showText :: Show value => value -> Text
showText = Text.pack . show
