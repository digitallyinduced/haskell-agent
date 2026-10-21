-- | Discovery and lifecycle of the authenticated per-user local service.
module Agent.Server.Client.Local
    ( LocalServiceDescriptor(..)
    , LocalServiceError(..)
    , LocalClientLease(..)
    , localServiceDirectory
    , readLocalServiceDescriptor
    , connectLocalService
    , ensureLocalService
    , withLocalClientLease
    , attachLocalClientSession
    , detachLocalClientSession
    , localServiceStatus
    , stopLocalService
    , stopLocalServiceForced
    ) where

import Agent.Server.Client
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race, withAsync, waitCatch)
import Control.Exception.Safe (bracket, bracketOnError, throwString, tryIO)
import Control.Monad (unless)
import Data.Aeson
import Data.Bits ((.&.))
import Data.ByteString qualified as ByteString
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Network.HTTP.Types.URI (urlEncode)
import System.Directory (createDirectoryIfMissing, getHomeDirectory, makeAbsolute)
import System.Environment (getEnvironment, lookupEnv)
import System.FileLock qualified as FileLock
import System.FilePath ((</>), normalise, takeDirectory)
import System.IO (Handle, hClose)
import System.IO.Error (isDoesNotExistError, isEOFError)
import System.Posix.Files
import System.Posix.IO
import System.Posix.IO.ByteString qualified as Posix
import System.Posix.User (getEffectiveUserID)
import System.Process
import System.Timeout (timeout)
import Text.Read (readMaybe)

data LocalServiceDescriptor = LocalServiceDescriptor
    { localProtocolVersion :: !Int
    , localServerInstanceId :: !Text
    , localServerBuildCommit :: !Text
    , localServerBaseUrl :: !Text
    , localServerCredentialFile :: !FilePath
    } deriving (Eq, Show)

instance FromJSON LocalServiceDescriptor where
    parseJSON = withObject "LocalServiceDescriptor" \value ->
        LocalServiceDescriptor <$> value .: "protocolVersion"
            <*> value .: "serverInstanceId" <*> value .: "buildCommit"
            <*> value .: "baseUrl" <*> value .: "credentialFile"

data LocalServiceError
    = LocalServiceAbsent
    | LocalServiceInvalid !Text
    | LocalServiceIncompatible !Int
    | LocalServiceConnectionError !AgentServerClientError
    | LocalServiceStartupFailed !Text
    | LocalServiceBuildMismatch !Text !Text
    deriving (Eq, Show)

data LocalClientLease = LocalClientLease
    { localClientId :: !Text
    , localHeartbeatIntervalMs :: !Int
    , localLeaseTimeoutMs :: !Int
    } deriving (Eq, Show)

instance FromJSON LocalClientLease where
    parseJSON = withObject "LocalClientLease" \value -> do
        lease <- LocalClientLease <$> value .: "id"
            <*> value .: "heartbeatIntervalMs" <*> value .: "leaseTimeoutMs"
        if Text.null lease.localClientId || lease.localHeartbeatIntervalMs < 100
            || lease.localHeartbeatIntervalMs > 1000 || lease.localLeaseTimeoutMs > 5000
            || lease.localLeaseTimeoutMs <= lease.localHeartbeatIntervalMs
            then fail "invalid local client lease"
            else pure lease

localServiceDirectory :: IO FilePath
localServiceDirectory = do
    home <- getHomeDirectory
    configured <- lookupEnv "HASKELL_AGENT_RUNTIME_DIR"
    makeAbsolute (maybe (home </> ".haskell-agent" </> "runtime") id configured)

prepareDirectory :: FilePath -> IO ()
prepareDirectory path = do
    createDirectoryIfMissing True path
    bracket (openFd path ReadOnly defaultFileFlags
        { nofollow = True, cloexec = True, directory = True }) closeFd \descriptor -> do
        status <- getFdStatus descriptor
        user <- getEffectiveUserID
        unless (isDirectory status && fileOwner status == user) $
            throwString "the local runtime directory must be owned by the current user"
        setFdMode descriptor 0o700

readLocalServiceDescriptor :: IO (Either LocalServiceError LocalServiceDescriptor)
readLocalServiceDescriptor = do
    directory <- localServiceDirectory
    result <- tryIO $ bracket
        (openFd (directory </> "local-server.json") ReadOnly defaultFileFlags
            { nofollow = True, cloexec = True }) closeFd \descriptor -> do
        status <- getFdStatus descriptor
        user <- getEffectiveUserID
        if not (isRegularFile status) || fileOwner status /= user || fileMode status .&. 0o077 /= 0
            then pure (Left (LocalServiceInvalid "the local service descriptor must be a private user-owned regular file"))
            else do
                bytes <- readDescriptorBytes descriptor (65537 :: Int) []
                pure $ if ByteString.length bytes > 65536
                    then Left (LocalServiceInvalid "the local service descriptor is too large")
                    else case eitherDecodeStrict' bytes of
                        Left _ -> Left (LocalServiceInvalid "the local service descriptor is malformed")
                        Right descriptorValue
                            | descriptorValue.localProtocolVersion /= 1 ->
                                Left (LocalServiceIncompatible descriptorValue.localProtocolVersion)
                            | normalise (takeDirectory descriptorValue.localServerCredentialFile) /= normalise directory ->
                                Left (LocalServiceInvalid "the credential file must belong to the local runtime directory")
                            | not ("http://127.0.0.1:" `Text.isPrefixOf` descriptorValue.localServerBaseUrl) ->
                                Left (LocalServiceInvalid "the local service must use the loopback address")
                            | otherwise -> Right descriptorValue
    pure $ case result of
        Left err | isDoesNotExistError err -> Left LocalServiceAbsent
        Left _ -> Left (LocalServiceInvalid "the local service descriptor cannot be read")
        Right value -> value
  where
    readDescriptorBytes descriptor remaining chunks
        | remaining <= 0 = pure (ByteString.concat (reverse chunks))
        | otherwise = do
            result <- tryIO (Posix.fdRead descriptor (fromIntegral remaining))
            case result of
                Left err | isEOFError err -> pure (ByteString.concat (reverse chunks))
                Left err -> ioError err
                Right bytes | ByteString.null bytes -> pure (ByteString.concat (reverse chunks))
                Right bytes -> readDescriptorBytes descriptor (remaining - ByteString.length bytes) (bytes : chunks)

connectLocalService :: IO (Either LocalServiceError AgentServerClient)
connectLocalService = readLocalServiceDescriptor >>= \case
    Left err -> pure (Left err)
    Right descriptor -> newAgentServerClient AgentServerClientConfig
        { agentServerBaseUrl = descriptor.localServerBaseUrl
        , agentServerCredentialFile = descriptor.localServerCredentialFile
        } >>= \case
            Left err -> pure (Left (LocalServiceConnectionError err))
            Right client -> localServiceStatus client >>= \case
                Left err -> pure (Left (LocalServiceConnectionError err))
                Right status -> case fromJSON status of
                    Error _ -> pure (Left (LocalServiceInvalid "the local server returned an invalid service identity"))
                    Success (ServiceIdentity version identifier)
                        | version /= 1 -> pure (Left (LocalServiceIncompatible version))
                        | identifier /= descriptor.localServerInstanceId ->
                            pure (Left (LocalServiceInvalid "the service identity does not match its descriptor"))
                        | otherwise -> pure (Right client)

data ServiceIdentity = ServiceIdentity !Int !Text

instance FromJSON ServiceIdentity where
    parseJSON = withObject "service identity" \value ->
        ServiceIdentity <$> value .: "protocolVersion" <*> value .: "serverInstanceId"

localServiceStatus :: AgentServerClient -> IO (Either AgentServerClientError Value)
localServiceStatus client = performAgentServerJsonRequest client "GET" "/v1/service" [200] Nothing

stopLocalService :: AgentServerClient -> IO (Either AgentServerClientError Value)
stopLocalService client = performAgentServerJsonRequest client "POST" "/v1/service/stop" [202] (Just (object []))

stopLocalServiceForced :: AgentServerClient -> IO (Either AgentServerClientError Value)
stopLocalServiceForced client = performAgentServerJsonRequest client "POST" "/v1/service/stop?force=true" [202] (Just (object []))

-- | The service is an independent process with its own lifetime lock. The
-- startup client observes it until readiness; it does not own daemon shutdown.
ensureLocalService :: IO (Either LocalServiceError AgentServerClient)
ensureLocalService = connectMatchingService >>= \case
    Right client -> pure (Right client)
    Left err@(LocalServiceIncompatible _) -> pure (Left err)
    Left err@(LocalServiceInvalid _) -> pure (Left err)
    Left _ -> do
        directory <- localServiceDirectory
        seconds <- maybe 20 (max 1 . min 300) . (>>= readMaybe)
            <$> lookupEnv "HASKELL_AGENT_SERVER_STARTUP_TIMEOUT_SECONDS"
        prepared <- tryIO (prepareDirectory directory >> prepareLock (directory </> "local-server-start.lock"))
        case prepared of
            Left _ -> pure (Left (LocalServiceStartupFailed "cannot prepare the local runtime directory"))
            Right () -> do
                result <- timeout (seconds * 1_000_000) $
                    FileLock.withFileLock (directory </> "local-server-start.lock") FileLock.Exclusive \_ ->
                        connectMatchingService >>= \case
                            Right client -> pure (Right client)
                            Left err@(LocalServiceIncompatible _) -> pure (Left err)
                            Left err@(LocalServiceInvalid _) -> pure (Left err)
                            Left (LocalServiceBuildMismatch _ _) -> connectLocalService >>= \case
                                Left err -> pure (Left err)
                                Right old -> stopLocalService old >>= \case
                                    Left _ -> pure (Left (LocalServiceStartupFailed
                                        "a different server build has active clients; close them, then run agent-cli server restart"))
                                    Right _ -> awaitStopped >> start directory
                            Left _ -> start directory
                pure (maybe (Left (LocalServiceStartupFailed "the local server did not become ready before the startup timeout")) id result)
  where
    start directory = do
        executable <- maybe "agent-server" id <$> lookupEnv "HASKELL_AGENT_SERVER"
        environment <- filter ((/= "AGENT_SERVER_TOKEN") . fst) <$> getEnvironment
        launched <- tryIO $ bracket
            (openLog (directory </> "local-server.log"))
            hClose \output -> do
                (_, _, _, processHandle) <- createProcess (proc executable ["--local"])
                    { std_in = NoStream, std_out = UseHandle output, std_err = UseHandle output
                    , new_session = True, close_fds = True, env = Just environment }
                withAsync (waitForProcess processHandle) \worker -> awaitReady worker
        pure $ either (const (Left (LocalServiceStartupFailed "could not start the local server executable"))) id launched
    awaitReady worker = connectMatchingService >>= \case
        Right client -> pure (Right client)
        Left (LocalServiceIncompatible version) -> pure (Left (LocalServiceIncompatible version))
        Left _ -> do
            -- Waiting for a short observation interval also detects startup
            -- failure without retaining an unowned background waiter.
            ended <- race (threadDelay 50000) (waitCatch worker)
            case ended of
                Right _ -> pure (Left (LocalServiceStartupFailed "the local server exited during startup; see local-server.log"))
                Left () -> awaitReady worker
    awaitStopped = connectLocalService >>= \case
        Left _ -> pure ()
        Right _ -> threadDelay 50000 >> awaitStopped

-- Development profiles are isolated by the frontend. Release clients require
-- matching build identities; an idle old service is replaced under the start
-- lock, while an active service is left for its current clients.
connectMatchingService :: IO (Either LocalServiceError AgentServerClient)
connectMatchingService = connectLocalService >>= \case
    Left err -> pure (Left err)
    Right client -> do
        expected <- fmap Text.pack <$> lookupEnv "AGENT_BUILD_COMMIT"
        descriptor <- readLocalServiceDescriptor
        pure $ case (expected, descriptor) of
            (Just build, Right actual)
                | build /= "unknown", not (Text.null build)
                , actual.localServerBuildCommit /= "unknown"
                , build /= actual.localServerBuildCommit ->
                    Left (LocalServiceBuildMismatch build actual.localServerBuildCommit)
            _ -> Right client

openLog :: FilePath -> IO Handle
openLog path = bracketOnError
    (openFd path WriteOnly defaultFileFlags
        { creat = Just 0o600, append = True, nofollow = True, cloexec = True })
    closeFd \descriptor -> do
        status <- getFdStatus descriptor
        user <- getEffectiveUserID
        unless (isRegularFile status && fileOwner status == user && fileMode status .&. 0o077 == 0) $
            throwString "the local service log must be a private user-owned regular file"
        fdToHandle descriptor

prepareLock :: FilePath -> IO ()
prepareLock path = bracket
    (openFd path ReadWrite defaultFileFlags { creat = Just 0o600, nofollow = True, cloexec = True })
    closeFd \descriptor -> do
        status <- getFdStatus descriptor
        user <- getEffectiveUserID
        unless (isRegularFile status && fileOwner status == user) $
            throwString "the local startup lock must be a user-owned regular file"
        setFdMode descriptor 0o600

withLocalClientLease
    :: AgentServerClient
    -> (AgentServerClient -> LocalClientLease -> IO value)
    -> IO (Either AgentServerClientError value)
withLocalClientLease client action = bracket
    (performAgentServerJsonRequest client "POST" "/v1/clients" [201] (Just (object [])))
    release \case
        Left err -> pure (Left err)
        Right lease -> do
            let owned = withAgentServerClientIdentity lease.localClientId client
            outcome <- race (heartbeat owned lease) (action owned lease)
            pure (either Left Right outcome)
  where
    release (Left _) = pure ()
    release (Right lease) = do
        _ <- timeout 1_000_000 $
            (performAgentServerJsonRequest client "DELETE" (clientPath lease) [202] Nothing
                :: IO (Either AgentServerClientError Value))
        pure ()
    heartbeat owned lease = do
        threadDelay (lease.localHeartbeatIntervalMs * 1000)
        result <- timeout 2_000_000 $
            (performAgentServerJsonRequest owned "POST" (clientPath lease <> "/heartbeat") [200] (Just (object []))
                :: IO (Either AgentServerClientError Value))
        case result of
            Just (Right _) -> heartbeat owned lease
            Just (Left err) -> pure err
            Nothing -> pure (AgentServerTransportError "the local server heartbeat timed out")

attachLocalClientSession
    :: AgentServerClient -> LocalClientLease -> Text -> IO (Either AgentServerClientError Value)
attachLocalClientSession client lease sessionId = performAgentServerJsonRequest client "POST"
    (clientPath lease <> "/sessions/" <> segment sessionId) [200] (Just (object []))

detachLocalClientSession :: AgentServerClient -> LocalClientLease -> Text -> IO (Either AgentServerClientError Value)
detachLocalClientSession client lease sessionId = performAgentServerJsonRequest client "DELETE"
    (clientPath lease <> "/sessions/" <> segment sessionId) [200] Nothing

clientPath :: LocalClientLease -> Text
clientPath lease = "/v1/clients/" <> segment lease.localClientId

segment :: Text -> Text
segment = TextEncoding.decodeUtf8 . urlEncode True . TextEncoding.encodeUtf8
