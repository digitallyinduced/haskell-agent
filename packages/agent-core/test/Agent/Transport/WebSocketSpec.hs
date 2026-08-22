module Agent.Transport.WebSocketSpec (spec) where

import Agent.Error (ApiError(..))
import Agent.Transport.WebSocket
import Control.Concurrent
    ( Chan
    , MVar
    , forkFinally
    , killThread
    , newChan
    , newEmptyMVar
    , putMVar
    , readChan
    , takeMVar
    , writeChan
    )
import Control.Exception (SomeException, finally, throwIO, toException)
import qualified Control.Exception.Safe as Safe
import Control.Retry
    ( RetryPolicyM
    , applyPolicy
    , constantDelay
    , defaultRetryStatus
    , limitRetries
    , rsPreviousDelay
    )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import qualified Network.TLS as TLS
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Stream as WSStream
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "Agent.Transport.WebSocket" do
    it "classifies websocket connection timeouts" do
        transientWsConnectFailureLabel (toException WS.ConnectionTimeout :: SomeException)
            `shouldBe` Just "WebSocket handshake timed out"

    it "classifies connection resets wrapped by the TLS handshake" do
        let exception = TLS.HandshakeFailed $ TLS.Error_Misc
                "Network.Socket.recvBuf: resource vanished (Connection reset by peer)"
        transientWsConnectFailureLabel (toException exception :: SomeException)
            `shouldBe` Just
                "TLS handshake failed: Network.Socket.recvBuf: resource vanished (Connection reset by peer)"

    it "does not classify permanent TLS handshake failures as transient" do
        let exception = TLS.HandshakeFailed $ TLS.Error_Certificate
                "certificate validation failed"
        transientWsConnectFailureLabel (toException exception :: SomeException)
            `shouldBe` Nothing

    it "backs off transient handshake retries exponentially with a cap" do
        retryPolicyDelays 7 transientWsConnectRetryPolicy
            `shouldReturn`
                [ 1000000
                , 2000000
                , 4000000
                , 8000000
                , 15000000
                , 15000000
                , 15000000
                ]

    it "classifies websocket parse errors during a run" do
        transientWsMidRunFailureLabel (toException (WS.ParseException "not enough bytes") :: SomeException)
            `shouldBe` Just "WebSocket parse error: not enough bytes"

    it "classifies handshake 401 without leaking headers" do
        let responseHead = WS.ResponseHead
                { WS.responseCode = 401
                , WS.responseMessage = BS8.pack "Unauthorized"
                , WS.responseHeaders = [("Set-Cookie", "secret-cookie")]
                }
            exception = toException (WS.MalformedResponse responseHead "Wrong response status") :: SomeException
        wsHandshakeAuthFailureStatus exception `shouldBe` Just 401
        wsHandshakeAuthFailure exception
            `shouldBe` Just (HttpError 401 "WebSocket handshake returned HTTP 401")

    it "retries a transient handshake failure before the connection callback" do
        attempts <- newIORef (0 :: Int)
        let exception = TLS.HandshakeFailed $ TLS.Error_Misc
                "Network.Socket.recvBuf: resource vanished (Connection reset by peer)"
        result <- retryTransientWsConnectWithPolicy
            (constantDelay 0 <> limitRetries 3)
            \connected -> do
                attempt <- atomicModifyIORef' attempts \n -> (n + 1, n + 1)
                if attempt == 1
                    then throwIO exception
                    else connected >> pure ("connected" :: String)

        result `shouldBe` "connected"
        readIORef attempts `shouldReturn` 2

    it "does not retry after the connection callback has started" do
        attempts <- newIORef (0 :: Int)
        result <- Safe.tryAny $
            retryTransientWsConnectWithPolicy
                (constantDelay 0 <> limitRetries 3)
                \connected -> do
                    modifyIORef' attempts (+ 1)
                    connected
                    throwIO (transientHandshakeException 503)

        case result of
            Left exception ->
                transientWsConnectFailureLabel exception
                    `shouldBe` Just "WebSocket handshake returned HTTP 503"
            Right () -> expectationFailure "expected the callback exception to escape"
        readIORef attempts `shouldReturn` 1

    it "does not retry a non-transient handshake rejection" do
        attempts <- newIORef (0 :: Int)
        result <- Safe.tryAny $
            retryTransientWsConnectWithPolicy
                (constantDelay 0 <> limitRetries 3)
                \_connected -> do
                    modifyIORef' attempts (+ 1)
                    throwIO (transientHandshakeException 401)

        case result of
            Left exception ->
                wsHandshakeAuthFailureStatus exception `shouldBe` Just 401
            Right () -> expectationFailure "expected the handshake rejection to escape"
        readIORef attempts `shouldReturn` 1

    it "handles server pings while delivering application data" do
        withConnectionPair \client server -> do
            pongResult <- newEmptyMVar
            _ <- forkFinally
                (do
                    request <- WS.receiveData server :: IO LBS.ByteString
                    request `shouldBe` "request-data"
                    WS.send server (WS.ControlMessage (WS.Ping "probe"))
                    WS.sendTextData server ("response-data" :: LBS.ByteString)
                    WS.receive server)
                (putMVar pongResult)

            withWebSocketSession testSessionOptions client \session -> do
                withWebSocketRequest session sendAndReceive
                    `shouldReturn` Right "response-data"
                pongResult' <- requireWithin "timed out waiting for client pong" (takeMVar pongResult)
                case pongResult' of
                    Left exception -> expectationFailure (show exception)
                    Right pong -> pong `shouldBe` WS.ControlMessage (WS.Pong "probe")

    it "serializes outgoing text through the shared session" do
        withConnectionPair \client server -> do
            receivedResult <- newEmptyMVar
            _ <- forkFinally
                (WS.receiveData server :: IO LBS.ByteString)
                (putMVar receivedResult)

            withWebSocketSession testSessionOptions client \session -> do
                withWebSocketRequest session sendOnly
                    `shouldReturn` Right ()
                received <- requireWithin
                    "timed out waiting for text frame"
                    (takeMVar receivedResult)
                case received of
                    Left exception -> expectationFailure (show exception)
                    Right bytes -> bytes `shouldBe` "request-data"

    it "reuses the session after a completed exchange" do
        withConnectionPair testSessionReuse

    it "poisons a request that returns without completing" do
        withConnectionPair testIncompleteRequest

    it "poisons a cancelled request instead of exposing its queued frames" do
        withConnectionPair \client server -> do
            firstFrame <- newEmptyMVar
            never <- newEmptyMVar
            finished <- newEmptyMVar
            withWebSocketSession testSessionOptions client \session -> do
                worker <- forkFinally
                    (cancelledRequest session firstFrame never)
                    (putMVar finished)
                assertFirstFrame server firstFrame
                assertQueuedFrame server
                killThread worker
                assertCancelledRequest finished
                withWebSocketRequest session receiveWebSocketData
                    `shouldReturn` Left
                        (ConnectionError
                            "WebSocket session invalidated: response consumer interrupted")

transientHandshakeException :: Int -> SomeException
transientHandshakeException status =
    toException $ WS.MalformedResponse responseHead "Wrong response status"
  where
    responseHead = WS.ResponseHead
        { WS.responseCode = status
        , WS.responseMessage = BS8.pack "Service Unavailable"
        , WS.responseHeaders = []
        }

retryPolicyDelays :: Int -> RetryPolicyM IO -> IO [Int]
retryPolicyDelays count policy = go count defaultRetryStatus
  where
    go remaining retryStatus
        | remaining <= 0 = pure []
        | otherwise =
            applyPolicy policy retryStatus >>= \case
                Nothing -> pure []
                Just nextStatus -> do
                    rest <- go (remaining - 1) nextStatus
                    pure (maybe 0 id nextStatus.rsPreviousDelay : rest)

-- WebSocket session fixtures.
testSessionReuse :: WS.Connection -> WS.Connection -> IO ()
testSessionReuse client server = do
    serverResult <- newEmptyMVar
    _ <- forkFinally
        (serveRoundTrips server
            [ ("request-1", "response-1")
            , ("request-2", "response-2")
            ])
        (putMVar serverResult)
    withWebSocketSession testSessionOptions client \session -> do
        withWebSocketRequest session (roundTrip "request-1")
            `shouldReturn` Right "response-1"
        withWebSocketRequest session (roundTrip "request-2")
            `shouldReturn` Right "response-2"
        result <- requireWithin
            "timed out waiting for round trips"
            (takeMVar serverResult)
        either throwIO pure result

testIncompleteRequest :: WS.Connection -> WS.Connection -> IO ()
testIncompleteRequest client _server =
    withWebSocketSession testSessionOptions client \session -> do
        withWebSocketRequest session unfinishedRequest
            `shouldReturn` Right ()
        withWebSocketRequest session unfinishedRequest
            `shouldReturn` Left incompleteRequestError

unfinishedRequest :: WebSocketRequest -> IO (Either ApiError ())
unfinishedRequest _ = pure (Right ())

incompleteRequestError :: ApiError
incompleteRequestError =
    ConnectionError
        "WebSocket session invalidated: request returned before response completion"

serveRoundTrips
    :: WS.Connection
    -> [(LBS.ByteString, LBS.ByteString)]
    -> IO ()
serveRoundTrips server exchanges =
    mapM_ serve exchanges
  where
    serve (expected, response) = do
        request <- WS.receiveData server
        request `shouldBe` expected
        WS.sendTextData server response

roundTrip
    :: LBS.ByteString
    -> WebSocketRequest
    -> IO (Either ApiError LBS.ByteString)
roundTrip bytes request = do
    sent <- sendWebSocketText request bytes
    case sent of
        Left apiError -> pure (Left apiError)
        Right () -> receiveCompleted request

sendAndReceive
    :: WebSocketRequest
    -> IO (Either ApiError LBS.ByteString)
sendAndReceive request = do
    sent <- sendWebSocketText request "request-data"
    case sent of
        Left apiError -> pure (Left apiError)
        Right () -> receiveCompleted request

receiveCompleted
    :: WebSocketRequest
    -> IO (Either ApiError LBS.ByteString)
receiveCompleted request = do
    result <- receiveWebSocketData request
    case result of
        Left apiError -> pure (Left apiError)
        Right response -> do
            completeWebSocketRequest request
            pure (Right response)

sendOnly :: WebSocketRequest -> IO (Either ApiError ())
sendOnly request = do
    result <- sendWebSocketText request "request-data"
    case result of
        Left apiError -> pure (Left apiError)
        Right () -> do
            completeWebSocketRequest request
            pure (Right ())

cancelledRequest
    :: WebSocketSession
    -> MVar (Either ApiError LBS.ByteString)
    -> MVar ()
    -> IO (Either ApiError ())
cancelledRequest session firstFrame never =
    withWebSocketRequest session \request -> do
        sent <- sendWebSocketText request "request-1"
        case sent of
            Left apiError -> pure (Left apiError)
            Right () -> do
                frame <- receiveWebSocketData request
                putMVar firstFrame frame
                takeMVar never
                pure (Right ())

assertFirstFrame
    :: WS.Connection
    -> MVar (Either ApiError LBS.ByteString)
    -> IO ()
assertFirstFrame server firstFrame = do
    request <- requireWithin
        "timed out waiting for first request"
        (WS.receiveData server :: IO LBS.ByteString)
    request `shouldBe` "request-1"
    WS.sendTextData server ("partial" :: LBS.ByteString)
    frame <- requireWithin
        "timed out waiting for first response frame"
        (takeMVar firstFrame)
    frame `shouldBe` Right "partial"

assertQueuedFrame :: WS.Connection -> IO ()
assertQueuedFrame server = do
    WS.sendTextData server ("stale-response" :: LBS.ByteString)
    WS.send server (WS.ControlMessage (WS.Ping "queued"))
    pong <- requireWithin
        "timed out waiting for stale frame to be queued"
        (WS.receive server)
    pong `shouldBe` WS.ControlMessage (WS.Pong "queued")

assertCancelledRequest
    :: MVar (Either SomeException (Either ApiError ()))
    -> IO ()
assertCancelledRequest finished = do
    result <- requireWithin
        "timed out joining cancelled request"
        (takeMVar finished)
    result `shouldSatisfy` \case
        Left _ -> True
        Right _ -> False

testSessionOptions :: WebSocketSessionOptions
testSessionOptions = defaultWebSocketSessionOptions
    { clientPingIntervalSeconds = Nothing
    }

withConnectionPair :: (WS.Connection -> WS.Connection -> IO value) -> IO value
withConnectionPair action = do
    clientToServer <- newChan
    serverToClient <- newChan
    clientStream <- makeChannelStream serverToClient clientToServer
    serverStream <- makeChannelStream clientToServer serverToClient
    serverResult <- newEmptyMVar
    _ <- forkFinally
        (WS.makePendingConnectionFromStream serverStream WS.defaultConnectionOptions
            >>= WS.acceptRequest)
        (putMVar serverResult)
    client <- WS.newClientConnection
        clientStream
        "localhost"
        "/"
        WS.defaultConnectionOptions
        []
    server <- requireWithin "timed out creating server connection" (takeMVar serverResult)
        >>= either throwIO pure
    action client server `finally` do
        WSStream.close clientStream
        WSStream.close serverStream

makeChannelStream
    :: Chan (Maybe BS.ByteString)
    -> Chan (Maybe BS.ByteString)
    -> IO WSStream.Stream
makeChannelStream incoming outgoing =
    WSStream.makeStream
        (readChan incoming)
        (writeChan outgoing . fmap LBS.toStrict)

requireWithin :: String -> IO value -> IO value
requireWithin message action = do
    result <- timeout 2000000 action
    case result of
        Just value -> pure value
        Nothing -> expectationFailure message >> fail message
