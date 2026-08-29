module Agent.Runtime.Daemon.Server
    ( ServerConfig (..)
    , defaultServerConfig
    , runServer
    , runServerOnListener
    , serveConnection
    ) where

import Control.Concurrent.Async (concurrently_, mapConcurrently_, race_)
import Control.Concurrent.STM
import Control.Exception.Safe (bracket, catchAny, finally)
import Control.Monad (forever)
import Data.Aeson (toJSON)
import Data.Foldable (for_)
import Network.Socket (Socket, close)

import Agent.Runtime.Daemon.Framing
import Agent.Runtime.Daemon.Journal
import Agent.Runtime.Daemon.Protocol
import Agent.Runtime.Daemon.Socket
import Agent.Runtime.Daemon.Supervisor

data ServerConfig = ServerConfig
    { maximumFrameBytes :: Int
    , heartbeatSeconds :: Int
    , workerCount :: Int
    , outboundQueueSize :: Int
    }
    deriving stock (Eq, Show)

defaultServerConfig :: ServerConfig
defaultServerConfig =
    ServerConfig
        { maximumFrameBytes = defaultMaximumFrameBytes
        , heartbeatSeconds = 15
        , workerCount = 8
        , outboundQueueSize = 256
        }

runServer :: SocketConfig -> ServerConfig -> Journal -> Supervisor -> IO ()
runServer socketConfig serverConfig journal supervisor =
    withUnixListener socketConfig $ \listener ->
        runServerOnListener listener serverConfig journal supervisor

runServerOnListener :: Socket -> ServerConfig -> Journal -> Supervisor -> IO ()
runServerOnListener listener serverConfig journal supervisor = do
    accepted <- newTBQueueIO (fromIntegral (max 1 serverConfig.workerCount))
    let acceptLoop = forever (acceptOwnedPeer listener >>= atomically . writeTBQueue accepted)
        worker =
            forever $
                bracket
                    (atomically (readTBQueue accepted))
                    close
                    ( \peer ->
                        serveConnection serverConfig journal supervisor peer
                            `catchAny` const (pure ())
                    )
        closeQueued = atomically (flushTBQueue accepted) >>= mapM_ close
    ( concurrently_ acceptLoop $
        mapConcurrently_ (const worker) [1 .. max 1 serverConfig.workerCount]
      ) `finally` closeQueued

serveConnection :: ServerConfig -> Journal -> Supervisor -> Socket -> IO ()
serveConnection config journal supervisor peer = do
    receiveJSONFrame config.maximumFrameBytes peer >>= \case
        ClientHello hello ->
            case negotiateVersion hello.versions of
                Nothing ->
                    sendJSONFrame config.maximumFrameBytes peer $
                        ServerVersionRejected supportedProtocolVersions
                Just version -> serveNegotiated config journal supervisor peer hello version
        _ -> sendJSONFrame config.maximumFrameBytes peer $
            ServerVersionRejected supportedProtocolVersions

serveNegotiated ::
    ServerConfig ->
    Journal ->
    Supervisor ->
    Socket ->
    Hello ->
    ProtocolVersion ->
    IO ()
serveNegotiated config journal supervisor peer hello version = do
    let cursor = maybe 0 id hello.resumeAfter
    (currentSnapshot, resumableReplay, eventChannel) <- subscribeReplay journal cursor
    let replay =
            case hello.resumeAfter of
                Nothing -> ReplaySnapshot currentSnapshot
                Just _ -> resumableReplay
    sendJSONFrame config.maximumFrameBytes peer $
        ServerWelcome
            Welcome
                { version
                , currentSequence = currentSnapshot.lastSequence
                , heartbeatSeconds = config.heartbeatSeconds
                }
    case replay of
        ReplaySnapshot saved ->
            sendJSONFrame config.maximumFrameBytes peer $
                ServerSnapshot saved.lastSequence (toJSON saved)
        ReplayEvents events ->
            for_ events (sendJSONFrame config.maximumFrameBytes peer . ServerEvent)
    outbound <- newTBQueueIO (fromIntegral (max 1 config.outboundQueueSize))
    acknowledged <- newTVarIO cursor
    latestSequence <- newTVarIO currentSnapshot.lastSequence
    let enqueue message = atomically (writeTBQueue outbound message)
        receiver =
            forever $
                receiveJSONFrame config.maximumFrameBytes peer >>= \case
                    ClientAck sequenceNumber -> acknowledge acknowledged latestSequence sequenceNumber
                    ClientPong sequenceNumber -> acknowledge acknowledged latestSequence sequenceNumber
                    ClientCommand commandId command -> do
                        result <- supervisor.handleCommand commandId command
                        enqueue (ServerCommandResult commandId result)
                    ClientHello _ -> pure ()
        sender = forever $ do
            delay <- registerDelay (max 1 config.heartbeatSeconds * 1_000_000)
            message <-
                atomically $
                    (readTBQueue outbound)
                        `orElse` (do
                            event <- readTChan eventChannel
                            writeTVar latestSequence event.sequenceNumber
                            pure (ServerEvent event)
                        )
                        `orElse` do
                            expired <- readTVar delay
                            check expired
                            sequenceNumber <- readTVar latestSequence
                            pure (ServerHeartbeat sequenceNumber)
            sendJSONFrame config.maximumFrameBytes peer message
    race_ receiver sender
  where
    acknowledge variable latest sequenceNumber =
        atomically $ do
            newest <- readTVar latest
            modifyTVar' variable (max (min sequenceNumber newest))
