{-# LANGUAGE DeriveAnyClass #-}
module Agent.CLI.MacOS.MobileGateway
    ( MobileSession, MobileRelay, MobileFailure(..)
    , openSession, closeSession, sessionBaseURL
    , registerRunner, listPairings, revokePairing, wakePairing
    , openRelay, closeRelay, sendRelay, receiveRelay, validPairingID
    ) where

import Agent.CLI.GatewayClient
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (race_)
import Control.Concurrent.STM
import Control.Exception.Safe (Exception, throwIO, bracketOnError, finally, tryAny)
import Control.Monad (forever, unless, when, void)
import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char (isHexDigit)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import qualified Network.WebSockets as WS
import qualified Wuss
import System.Timeout (timeout)

-- Never expose HTTP exceptions: they may contain Authorization headers.
data MobileFailure = AccountUnavailable | InvalidMobileInput | MobileUnavailable
    deriving (Show, Exception)

data MobileSession = MobileSession
    { credential :: GatewayCredential
    , closed :: TVar Bool
    , unregister :: IO ()
    }

sessionBaseURL :: MobileSession -> Text
sessionBaseURL session = session.credential.gatewayBaseUrl

openSession :: IO MobileSession
openSession = do
    closed <- newTVarIO False
    bracketOnError
        (registerGatewayCredentialInvalidator (atomically $ writeTVar closed True))
        id
        \unregister -> withGatewayCredentialLease do
            credential <- loadGatewayCredential >>= \case
                Right (Just value) -> pure value
                _ -> throwIO AccountUnavailable
            _ <- either (const $ throwIO AccountUnavailable) pure
                (validateBaseUrl credential.gatewayBaseUrl)
            let session = MobileSession{credential, closed, unregister}
            ensureOpen session
            pure session

closeSession :: MobileSession -> IO ()
closeSession session = do
    atomically $ writeTVar session.closed True
    session.unregister

ensureOpen :: MobileSession -> IO ()
ensureOpen session = do
    closed <- readTVarIO session.closed
    when closed $ throwIO AccountUnavailable

-- Called under the credential lease. Identity is never exported.
checkIdentity :: MobileSession -> IO ()
checkIdentity session = do
    ensureOpen session
    current <- loadGatewayCredential
    unless (case current of
        Right (Just value) ->
            gatewayCredentialIdentity value == gatewayCredentialIdentity session.credential
        _ -> False) do
        atomically $ writeTVar session.closed True
        throwIO AccountUnavailable

validPairingID :: Text -> Bool
validPairingID value =
    Text.length value == 36 && and
        [if elem index [8,13,18,23] then char == '-' else isHexDigit char
        | (index, char) <- zip [0 :: Int ..] (Text.unpack value)]

pairingPath :: Text -> IO Text
pairingPath value
    | validPairingID value = pure $ "/api/v1/mobile/runner/pairings/" <> Text.toLower value
    | otherwise = throwIO InvalidMobileInput

maximumBytes :: Int
maximumBytes = 1024 * 1024

bounded :: IO a -> IO a
bounded action = timeout (20 * 1000000) action >>= maybe (throwIO MobileUnavailable) pure

request :: MobileSession -> BS.ByteString -> Text -> Value -> IO BS.ByteString
request session method suffix body = bounded $ withGatewayCredentialLease do
    checkIdentity session
    initial <- HTTP.parseRequest $ Text.unpack (sessionBaseURL session <> suffix)
    let request = initial
            { HTTP.method = method
            , HTTP.redirectCount = 0
            , HTTP.checkResponse = \_ _ -> pure ()
            , HTTP.responseTimeout = HTTP.responseTimeoutMicro (15 * 1000000)
            , HTTP.requestHeaders =
                [("Authorization", "Bearer " <> Text.encodeUtf8 session.credential.gatewayAccessToken)
                ,("Content-Type", "application/json")]
            , HTTP.requestBody = HTTP.RequestBodyLBS (encode body)
            }
    manager <- HTTP.newManager tlsManagerSettings
    finally (HTTP.withResponse request manager \response -> do
        let status = statusCode response.responseStatus
        unless (status >= 200 && status < 300 || method == "DELETE" && status == 404) $
            throwIO MobileUnavailable
        let collect size chunks = do
                chunk <- HTTP.brRead response.responseBody
                let total = size + BS.length chunk
                when (total > maximumBytes) $ throwIO MobileUnavailable
                if BS.null chunk then pure (BS.concat $ reverse chunks)
                else collect total (chunk:chunks)
        collect 0 []) (HTTP.closeManager manager)

decodeResponse :: (Value -> Parser a) -> BS.ByteString -> IO a
decodeResponse parser bytes = either (const $ throwIO MobileUnavailable) pure
    (eitherDecodeStrict' bytes >>= parseEither parser)

registerRunner :: MobileSession -> Text -> Text -> IO Text
registerRunner session deviceID name = do
    unless (validPairingID deviceID && not (Text.null name) && Text.length name <= 160) $
        throwIO InvalidMobileInput
    bytes <- request session "POST" "/api/v1/mobile/runner"
        (object ["device_id" .= deviceID, "name" .= name,
                 "capabilities" .= object ["protocol" .= (1 :: Int)]])
    decodeResponse (withObject "runner" (.: "id")) bytes

listPairings :: MobileSession -> IO [(Text, Text)]
listPairings session = do
    bytes <- request session "GET" "/api/v1/mobile/runner/pairings" Null
    result <- decodeResponse (withObject "pairings" \o -> do
        rows <- o .: "data"
        mapM (withObject "pairing" \p -> (,) <$> p .: "id" <*> (p .:? "mobile_name" .!= "")) rows) bytes
    unless (length result <= 1024 && all (validPairingID . fst) result) $
        throwIO MobileUnavailable
    pure result

revokePairing :: MobileSession -> Text -> IO ()
revokePairing session pairingID = do
    path <- pairingPath pairingID
    void $ request session "DELETE" path Null

wakePairing :: MobileSession -> Text -> IO ()
wakePairing session pairingID = do
    path <- pairingPath pairingID
    void $ request session "POST" (path <> "/wake") Null

data MobileRelay = MobileRelay
    { session :: MobileSession
    , closed :: TVar Bool
    , incoming :: TBQueue BS.ByteString
    , outgoing :: TBQueue BS.ByteString
    }

relayAlive :: MobileRelay -> STM ()
relayAlive relay = do
    stopped <- (||) <$> readTVar relay.closed <*> readTVar relay.session.closed
    when stopped $ throwSTM AccountUnavailable

closeRelay :: MobileRelay -> IO ()
closeRelay relay = atomically $ writeTVar relay.closed True

sendRelay :: MobileRelay -> BS.ByteString -> IO ()
sendRelay relay bytes = bounded do
    when (BS.length bytes > maximumBytes) $ throwIO InvalidMobileInput
    atomically $ relayAlive relay >> writeTBQueue relay.outgoing bytes

receiveRelay :: MobileRelay -> IO BS.ByteString
receiveRelay relay = atomically $ relayAlive relay >> readTBQueue relay.incoming

openRelay :: MobileSession -> Text -> IO MobileRelay
openRelay session pairingID = do
    unless (validPairingID pairingID) $ throwIO InvalidMobileInput
    withGatewayCredentialLease $ checkIdentity session
    relay <- MobileRelay session <$> newTVarIO False <*> newTBQueueIO 8 <*> newTBQueueIO 8
    ready <- newEmptyTMVarIO
    let stop = atomically do
            stopped <- (||) <$> readTVar relay.closed <*> readTVar session.closed
            check stopped
        -- Cross-process changes also close sockets. No lifetime credential lease.
        monitor = forever do
            threadDelay 1000000
            withGatewayCredentialLease $ checkIdentity session
        connect = do
            endpoint <- HTTP.parseRequest $ Text.unpack $
                sessionBaseURL session <> "/api/v1/mobile/relay/" <> Text.toLower pairingID <> "/runner"
            let options = WS.defaultConnectionOptions
                    { WS.connectionFramePayloadSizeLimit = WS.SizeLimit (fromIntegral maximumBytes)
                    , WS.connectionMessageDataSizeLimit = WS.SizeLimit (fromIntegral maximumBytes)
                    }
                headers = [("Authorization", "Bearer " <> Text.encodeUtf8 session.credential.gatewayAccessToken)]
                client connection = do
                    atomically $ relayAlive relay >> putTMVar ready ()
                    race_
                        (forever do
                            bytes <- WS.receiveData connection
                            when (BS.length bytes > maximumBytes) $ throwIO MobileUnavailable
                            atomically $ relayAlive relay >> writeTBQueue relay.incoming bytes)
                        (forever do
                            bytes <- atomically $ relayAlive relay >> readTBQueue relay.outgoing
                            WS.sendBinaryData connection bytes)
            if endpoint.secure
                then Wuss.runSecureClientWith (BSC.unpack endpoint.host) (fromIntegral endpoint.port)
                    (BSC.unpack endpoint.path) options headers client
                else WS.runClientWith (BSC.unpack endpoint.host) endpoint.port
                    (BSC.unpack endpoint.path) options headers client
    _ <- forkIO $ finally (void $ tryAny $ race_ stop (race_ monitor connect)) (closeRelay relay)
    outcome <- tryAny $ bounded $ atomically $ relayAlive relay >> readTMVar ready
    case outcome of
        Left _ -> closeRelay relay >> throwIO MobileUnavailable
        Right () -> pure relay
