module Agent.Runtime.Daemon.Server
    ( ServerConfig (..)
    , defaultServerConfig
    , runServer
    , runServerOnListener
    , serveConnection
    ) where

import Control.Concurrent.Async (concurrently_, mapConcurrently_, race_)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Exception.Safe
    ( Exception
    , SomeException
    , bracket
    , bracketOnError
    , catchAny
    , finally
    , isAsyncException
    , throwIO
    )
import Control.Monad (forever)
import Data.Aeson (FromJSON, ToJSON, encode)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (for_)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Network.Socket (Socket, close)
import System.Timeout (timeout)

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
    , ioTimeoutSeconds :: Int
    , supervisorTimeoutSeconds :: Int
    }
    deriving stock (Eq, Show)

defaultServerConfig :: ServerConfig
defaultServerConfig =
    ServerConfig
        { maximumFrameBytes = defaultMaximumFrameBytes
        , heartbeatSeconds = 15
        , workerCount = 8
        , outboundQueueSize = 256
        , ioTimeoutSeconds = 30
        , supervisorTimeoutSeconds = 300
        }

data ServerError
    = ServerTimedOut Text
    | ServerSubscriberOverflow
    deriving stock (Eq, Show)

instance Exception ServerError

runServer :: SocketConfig -> ServerConfig -> Journal -> Supervisor -> IO ()
runServer socketConfig serverConfig journal supervisor =
    withUnixListener socketConfig $ \listener ->
        runServerOnListener listener serverConfig journal supervisor

runServerOnListener :: Socket -> ServerConfig -> Journal -> Supervisor -> IO ()
runServerOnListener listener serverConfig journal supervisor = do
    accepted <- newTBQueueIO (fromIntegral (max 1 serverConfig.workerCount))
    let acceptLoop =
            forever $
                bracketOnError
                    (acceptOwnedPeer listener)
                    close
                    (atomically . writeTBQueue accepted)
                    `catchSync` const (threadDelay 100_000)
        worker =
            forever $
                bracket
                    (atomically (readTBQueue accepted))
                    close
                    ( \peer ->
                        serveConnection serverConfig journal supervisor peer
                            `catchSync` const (pure ())
                    )
        closeQueued = atomically (flushTBQueue accepted) >>= mapM_ close
    ( concurrently_ acceptLoop $
        mapConcurrently_ (const worker) [1 .. max 1 serverConfig.workerCount]
      ) `finally` closeQueued

serveConnection :: ServerConfig -> Journal -> Supervisor -> Socket -> IO ()
serveConnection config journal supervisor peer = do
    receive config peer >>= \case
        ClientHello hello ->
            case negotiateVersion hello.versions of
                Nothing ->
                    send config peer $
                        ServerVersionRejected supportedProtocolVersions
                Just version -> serveNegotiated config journal supervisor peer hello version
        _ -> send config peer $
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
    bracket
        (subscribeReplay journal cursor)
        (\(_, _, _, _, unsubscribe) -> unsubscribe)
        $ \(currentSnapshot, resumableReplay, eventQueue, overflowed, _) -> do
            let replay =
                    case hello.resumeAfter of
                        Nothing -> ReplaySnapshot currentSnapshot
                        Just _ -> resumableReplay
            send config peer $
                ServerWelcome
                    Welcome
                        { version
                        , currentSequence = currentSnapshot.lastSequence
                        , heartbeatSeconds = config.heartbeatSeconds
                        }
            case replay of
                ReplaySnapshot saved ->
                    sendSnapshot config peer saved
                ReplayEvents events ->
                    for_ events (sendEvent config peer)
            outbound <- newTBQueueIO (fromIntegral (max 1 config.outboundQueueSize))
            acknowledged <- newTVarIO cursor
            latestSequence <- newTVarIO currentSnapshot.lastSequence
            let enqueue message =
                    within config.ioTimeoutSeconds "outbound queue" $
                        atomically (writeTBQueue outbound message)
                receiver =
                    forever $
                        receive config peer >>= \case
                            ClientAck sequenceNumber -> acknowledge acknowledged latestSequence sequenceNumber
                            ClientPong sequenceNumber -> acknowledge acknowledged latestSequence sequenceNumber
                            ClientCommand commandId command -> do
                                result <-
                                    within config.supervisorTimeoutSeconds "supervisor command" $
                                        supervisor.handleCommand commandId command
                                enqueue (ServerCommandResult commandId result)
                            ClientHello _ -> pure ()
                sender = forever $ do
                    delay <- registerDelay (max 1 config.heartbeatSeconds * 1_000_000)
                    next <-
                        atomically $
                            (do
                                didOverflow <- readTVar overflowed
                                check didOverflow
                                pure Nothing
                            )
                                `orElse` (Just <$> readTBQueue outbound)
                                `orElse` (do
                                    event <- readTBQueue eventQueue
                                    writeTVar latestSequence event.sequenceNumber
                                    pure (Just (ServerEvent event))
                                )
                                `orElse` do
                                    expired <- readTVar delay
                                    check expired
                                    sequenceNumber <- readTVar latestSequence
                                    pure (Just (ServerHeartbeat sequenceNumber))
                    case next of
                        Nothing -> throwIO ServerSubscriberOverflow
                        Just message -> sendMessage config peer message
            race_ receiver sender
  where
    acknowledge variable latest sequenceNumber =
        atomically $ do
            newest <- readTVar latest
            modifyTVar' variable (max (min sequenceNumber newest))

sendSnapshot :: ServerConfig -> Socket -> JournalSnapshot -> IO ()
sendSnapshot config peer saved = do
    let rawChunkBytes = max 1 ((config.maximumFrameBytes - 256) * 3 `div` 4)
        chunks = chunkBytes rawChunkBytes (LBS.toStrict (encode saved))
        count = length chunks
    for_ (zip [0 ..] chunks) $ \(index, bytes) ->
        send config peer $
            ServerSnapshotChunk
                saved.lastSequence
                index
                count
                (Text.decodeUtf8 (Base64.encode bytes))

sendEvent :: ServerConfig -> Socket -> EventEnvelope -> IO ()
sendEvent config peer event
    | LBS.length (encode (ServerEvent event)) <= fromIntegral config.maximumFrameBytes =
        send config peer (ServerEvent event)
    | otherwise = do
        let rawChunkBytes = max 1 ((config.maximumFrameBytes - 256) * 3 `div` 4)
            chunks = chunkBytes rawChunkBytes (LBS.toStrict (encode event))
            count = length chunks
        for_ (zip [0 ..] chunks) $ \(index, bytes) ->
            send config peer $
                ServerEventChunk
                    event.sequenceNumber
                    index
                    count
                    (Text.decodeUtf8 (Base64.encode bytes))

sendMessage :: ServerConfig -> Socket -> ServerMessage -> IO ()
sendMessage config peer = \case
    ServerEvent event -> sendEvent config peer event
    message -> send config peer message

chunkBytes :: Int -> ByteString -> [ByteString]
chunkBytes size bytes
    | BS.null bytes = [BS.empty]
    | otherwise =
        let (chunk, rest) = BS.splitAt size bytes
         in if BS.null rest
                then [chunk]
                else chunk : chunkBytes size rest

send :: ToJSON value => ServerConfig -> Socket -> value -> IO ()
send config peer value =
    within config.ioTimeoutSeconds "socket write" $
        sendJSONFrame config.maximumFrameBytes peer value

receive :: FromJSON value => ServerConfig -> Socket -> IO value
receive config peer =
    within config.ioTimeoutSeconds "socket read" $
        receiveJSONFrame config.maximumFrameBytes peer

within :: Int -> Text -> IO value -> IO value
within seconds label action =
    timeout (max 1 seconds * 1_000_000) action >>= \case
        Nothing -> throwIO (ServerTimedOut label)
        Just value -> pure value

catchSync :: IO value -> (SomeException -> IO value) -> IO value
catchSync action handler =
    action `catchAny` \exception ->
        if isAsyncException exception
            then throwIO exception
            else handler exception
