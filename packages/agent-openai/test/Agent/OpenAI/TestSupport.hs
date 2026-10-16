module Agent.OpenAI.TestSupport
    ( requireLoopbackListener
    , withLoopbackApplication
    , withWebSocketServer
    ) where

import Control.Concurrent.Async (wait, withAsync)
import Control.Exception (IOException, try)
import Control.Exception.Safe (bracket, finally)
import Network.HTTP.Types (status200)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)
import qualified Network.Socket as Socket
import qualified Network.WebSockets as WS
import System.IO.Error (isPermissionError)
import Test.Hspec (pendingWith)

-- | Run a loopback-server test when the enclosing sandbox permits an
-- ephemeral listener. Other listener startup errors remain test failures.
withLoopbackApplication application action = do
    requireLoopbackListener
    testWithApplication application action

requireLoopbackListener :: IO ()
requireLoopbackListener = do
    result <- (try $ testWithApplication (pure probeApplication)
        (const $ pure ())) :: IO (Either IOException ())
    case result of
        Right () -> pure ()
        Left err
            | isPermissionError err ->
                pendingWith
                    "the test environment cannot bind a loopback listener"
            | otherwise -> ioError err

probeApplication :: Application
probeApplication _ respond =
    respond $ responseLBS status200 [] ""

-- | Serve one WebSocket connection on an ephemeral loopback port. The action
-- receives the port; the server must finish before the result is returned.
withWebSocketServer
    :: WS.ServerApp
    -> (Int -> IO value)
    -> IO value
withWebSocketServer server action =
    requireLoopbackListener >>
    bracket
        (WS.makeListenSocket "127.0.0.1" 0)
        Socket.close
        \listenSocket -> do
            port <- socketAddressPort <$> Socket.getSocketName listenSocket
            withAsync
                (do
                    (socket, _) <- Socket.accept listenSocket
                    (WS.makePendingConnection
                        socket
                        WS.defaultConnectionOptions >>= server)
                        `finally` Socket.close socket)
                \serverThread -> do
                    result <- action port
                    wait serverThread
                    pure result

socketAddressPort :: Socket.SockAddr -> Int
socketAddressPort = \case
    Socket.SockAddrInet port _ ->
        fromIntegral port
    Socket.SockAddrInet6 port _ _ _ ->
        fromIntegral port
    address ->
        error ("unexpected WebSocket test address: " <> show address)
