{-# LANGUAGE ScopedTypeVariables #-}

module Agent.Transport.WebSocket
    ( WebSocketSession
    , WebSocketSessionOptions(..)
    , defaultWebSocketSessionOptions
    , withWebSocketSession
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
        let session = WebSocketSession
                { sessionOut = outbound
                , sessionIn = inbound
                , sessionClosed = closed
                }
        withAsync (readerLoop connection inbound outbound closed) \_ ->
            withAsync (writerLoop connection outbound closed) \_ ->
                action session
  where
    withClientPings inner = case options.clientPingIntervalSeconds of
        Just seconds | seconds > 0 -> WS.withPingThread connection seconds (pure ()) inner
        _ -> inner

-- | Send one text frame through the session's serialized writer.
sendWebSocketText :: WebSocketSession -> LBS.ByteString -> IO (Either ApiError ())
sendWebSocketText session bytes = do
    reply <- newEmptyTMVarIO
    queued <- atomically do
        closed <- readTVar session.sessionClosed
        if closed
            then pure False
            else writeTQueue session.sessionOut (SendText bytes reply) >> pure True
    if not queued
        then pure (Left connectionClosedError)
        else atomically $
            takeTMVar reply
            `orElse`
            (do
                closed <- readTVar session.sessionClosed
                check closed
                pure (Left connectionClosedError))

-- | Receive the next application data frame. Control frames are handled by
-- the session pump and are never exposed to callers.
receiveWebSocketData :: WebSocketSession -> IO (Either ApiError LBS.ByteString)
receiveWebSocketData session = atomically $
    readTBQueue session.sessionIn
    `orElse`
    (do
        closed <- readTVar session.sessionClosed
        check closed
        pure (Left connectionClosedError))

connectionClosedError :: ApiError
connectionClosedError = ConnectionError "WebSocket connection closed"

readerLoop
    :: WS.Connection
    -> TBQueue (Either ApiError LBS.ByteString)
    -> TQueue OutCommand
    -> TVar Bool
    -> IO ()
readerLoop connection inbound outbound closed = loop
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
                atomically (writeTBQueue inbound (Right bytes))
                loop
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
