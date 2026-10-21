-- | One authenticated local server per user and runtime directory.
module Agent.Server.LocalService
    ( LocalService
    , withLocalService
    , localServiceSocket
    , localServiceReady
    , localServiceInstallShutdown
    , withLocalApplication
    ) where

import Agent.Server.Auth
import Agent.Server.Backend (Backend(..))
import Agent.Server.ClientRegistry
import Agent.Server.Config (ServerConfig(..), ResolvedServerConfig(..))
import Agent.Server.Identifier (newUUIDv7Text)
import Agent.Server.LocalEvents (localEventFile, localTurnResultFile, cleanupLocalEvents)
import Agent.Server.Supervisor
import Agent.Server.Types
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.MVar (MVar, newEmptyMVar, readMVar, tryPutMVar)
import Control.Exception.Safe (bracket, finally, throwString, tryIO)
import Control.Monad (forever, forM_, unless, void, when)
import Data.Aeson (Value(..), eitherDecodeStrict', encode, object, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as ByteString
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Lazy qualified as LazyByteString
import Data.List (find)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import GHC.Clock (getMonotonicTimeNSec)
import Network.HTTP.Types
import Network.Socket (Socket, close)
import Network.Wai
import Network.Wai.Handler.Warp (openFreePort)
import System.Directory (createDirectoryIfMissing, getHomeDirectory, makeAbsolute, removeFile, renameFile, listDirectory)
import System.Entropy (getEntropy)
import System.Environment (lookupEnv)
import System.FileLock qualified as FileLock
import System.FilePath ((</>))
import System.IO (hClose)
import System.Posix.Files
import System.Posix.IO (OpenMode(..), OpenFileFlags(..), closeFd, defaultFileFlags, dup, fdToHandle, openFd)
import System.Posix.Process (getProcessID)
import System.Posix.User (getEffectiveUserID)

data LocalService = LocalService
    { serviceSocket :: !Socket
    , servicePort :: !Int
    , serviceIdentity :: !Text
    , serviceDirectory :: !FilePath
    , serviceTokenFile :: !FilePath
    , serviceShutdown :: !(MVar (IO ()))
    }

localServiceSocket :: LocalService -> Socket
localServiceSocket service = service.serviceSocket

withLocalService
    :: ServerConfig -> (ServerConfig -> LocalService -> IO value) -> IO value
withLocalService config action = do
    when (config.serverTenantRegistry /= Nothing || config.serverAllowRemote
        || not (null config.serverCorsOrigins) || config.serverTokenFile /= Nothing) $
        throwString "--local cannot be combined with tenant, remote, CORS or token-file options"
    home <- getHomeDirectory
    directory <- makeAbsolute =<< (maybe (home </> ".haskell-agent" </> "runtime") id
        <$> lookupEnv "HASKELL_AGENT_RUNTIME_DIR")
    createDirectoryIfMissing True directory
    bracket (openFd directory ReadOnly defaultFileFlags
        { nofollow = True, cloexec = True, directory = True }) closeFd \descriptor -> do
        status <- getFdStatus descriptor
        user <- getEffectiveUserID
        unless (isDirectory status && fileOwner status == user) $
            throwString "the local runtime directory must be owned by the current user"
        setFdMode descriptor 0o700
    let lockPath = directory </> "local-server.lock"
    prepareLock lockPath
    ownership <- FileLock.withTryFileLock lockPath FileLock.Exclusive \_ ->
        bracket openFreePort (close . snd) \(port, socket) -> do
            -- The lifetime lock proves previous instance credentials are stale.
            names <- listDirectory directory
            forM_ names \name ->
                when ("local-server-" `Text.isPrefixOf` Text.pack name
                    && (".token" `Text.isSuffixOf` Text.pack name || ".json" `Text.isSuffixOf` Text.pack name))
                    (removeOwnedFile (directory </> name))
            identifier <- newUUIDv7Text
            shutdown <- newEmptyMVar
            let tokenPath = directory </> ("local-server-" <> Text.unpack identifier <> ".token")
                service = LocalService socket port identifier directory tokenPath shutdown
                ownedConfig = config
                    { serverLocal = True
                    , serverHost = "127.0.0.1"
                    , serverPort = port
                    , serverTokenFile = Just tokenPath
                    , serverWorkspaceRoots = ["/"]
                    }
            token <- Base64.encode <$> getEntropy 32
            bracket (pure service)
                (\_ -> removeServiceDescriptor service `finally` removeOwnedFile tokenPath)
                (\_ -> writePrivateFile tokenPath token >> action ownedConfig service)
    maybe (throwString "the local server is already running") pure ownership

prepareLock :: FilePath -> IO ()
prepareLock path = bracket
    (openFd path ReadWrite defaultFileFlags { creat = Just 0o600, nofollow = True, cloexec = True })
    closeFd \descriptor -> do
        status <- getFdStatus descriptor
        user <- getEffectiveUserID
        unless (isRegularFile status && fileOwner status == user) $
            throwString "the local server lock must be a user-owned regular file"
        setFdMode descriptor 0o600

writePrivateFile :: FilePath -> ByteString.ByteString -> IO ()
writePrivateFile path bytes = bracket
    (openFd path WriteOnly defaultFileFlags
        { creat = Just 0o600, exclusive = True, nofollow = True, cloexec = True }
        >>= fdToHandle)
    hClose (\handle -> ByteString.hPut handle bytes)

removeOwnedFile :: FilePath -> IO ()
removeOwnedFile path = do
    result <- tryIO (getSymbolicLinkStatus path)
    user <- getEffectiveUserID
    case result of
        Right status | isRegularFile status && fileOwner status == user ->
            void (tryIO (removeFile path))
        _ -> pure ()

localServiceReady :: LocalService -> IO ()
localServiceReady service = do
    processId <- getProcessID
    build <- maybe "unknown" Text.pack <$> lookupEnv "AGENT_BUILD_COMMIT"
    let temporary = service.serviceDirectory </>
            ("local-server-" <> Text.unpack service.serviceIdentity <> ".json")
        descriptor = object
            [ "protocolVersion" .= (1 :: Int)
            , "serverInstanceId" .= service.serviceIdentity
            , "buildCommit" .= build
            , "baseUrl" .= ("http://127.0.0.1:" <> Text.pack (show service.servicePort))
            , "credentialFile" .= service.serviceTokenFile
            , "processId" .= (fromIntegral processId :: Integer)
            ]
    writePrivateFile temporary (LazyByteString.toStrict (encode descriptor))
    renameFile temporary (service.serviceDirectory </> "local-server.json")

localServiceInstallShutdown :: LocalService -> IO () -> IO ()
localServiceInstallShutdown service shutdown =
    void (tryPutMVar service.serviceShutdown shutdown)

-- The lifetime lock is still held here. Only remove metadata for this exact
-- instance, including when startup failed before publishing readiness.
removeServiceDescriptor :: LocalService -> IO ()
removeServiceDescriptor service = do
    let path = service.serviceDirectory </> "local-server.json"
    result <- tryIO $ bracket
        (openFd path ReadOnly defaultFileFlags { nofollow = True, cloexec = True })
        closeFd \descriptor -> do
            status <- getFdStatus descriptor
            user <- getEffectiveUserID
            if isRegularFile status && fileOwner status == user && fileSize status <= 65536
                then do
                    handle <- fdToHandle =<< dup descriptor
                    bracket (pure handle) hClose (\input -> ByteString.hGet input 65537)
                else pure ByteString.empty
    case result of
        Right bytes | Right (Object fields) <- eitherDecodeStrict' bytes
            , KeyMap.lookup "serverInstanceId" fields == Just (String service.serviceIdentity) ->
                removeOwnedFile path
        _ -> pure ()

withLocalApplication
    :: LocalService
    -> ResolvedServerConfig
    -> Backend
    -> Supervisor
    -> Application
    -> (Application -> IO value)
    -> IO value
withLocalApplication service config backend supervisor application action = do
    registry <- newClientRegistry getMonotonicTimeNSec (5 * 1000 * 1000 * 1000)
    stopRequested <- newEmptyMVar
    let requestCancellation :: ClientSession -> IO ()
        requestCancellation session = do
            turns <- listTurns supervisor session.clientSessionBoundary (Just session.clientSessionId)
            forM_ turns \turn -> when (activeStatus turn.turnRecordStatus) $
                void (requestTurnCancellation supervisor session.clientSessionBoundary turn.turnRecordId)
        sessionBusy :: ClientSession -> IO Bool
        sessionBusy session = sessionHasActiveTurn supervisor
            session.clientSessionBoundary session.clientSessionId
        maintenance = forever do
            maintainClientRegistry registry requestCancellation sessionBusy
            threadDelay 100000
        cleanup = closeClientRegistry registry requestCancellation
        artifactMaintenance = forever do
            void (tryIO cleanupLocalEvents)
            threadDelay 60000000
        wrapped :: Application
        wrapped request respond =
            case authorizeRequest config.resolvedAuth request of
                Left _ -> application request respond
                Right authenticated -> dispatch registry authenticated.authenticatedPrincipal request respond
        dispatch :: ClientRegistry -> Principal -> Application
        dispatch registry principal request respond =
            case (request.requestMethod, request.pathInfo) of
                ("GET", ["v1", "service"]) -> do
                    (clients, sessions) <- clientRegistryCounts registry
                    respond (json status200 (object
                        [ "protocolVersion" .= (1 :: Int)
                        , "serverInstanceId" .= service.serviceIdentity
                        , "clients" .= clients, "sessions" .= sessions ]))
                ("GET", ["v1", "local", "event-payloads", turnId, identifier]) ->
                    localEventFile turnId identifier >>= localFileResponse >>= respond
                ("GET", ["v1", "local", "turns", turnId, "result"]) ->
                    localTurnResultFile turnId >>= localFileResponse >>= respond
                ("GET", ["v1", "local", "sessions", sessionId, "ownership"]) -> do
                    owner <- sessionClientOwner registry sessionId
                    respond (json status200 (object ["clientId" .= owner]))
                ("POST", ["v1", "clients"]) -> do
                    identifier <- newUUIDv7Text
                    registerClient registry identifier >>= either
                        (respond . leaseError)
                        (const (respond (json status201 (object
                            [ "id" .= identifier, "heartbeatIntervalMs" .= (1000 :: Int)
                            , "leaseTimeoutMs" .= (5000 :: Int) ]))))
                ("POST", ["v1", "clients", identifier, "heartbeat"]) ->
                    renewClient registry identifier >>= either (respond . leaseError)
                        (const (respond (json status200 (object ["ok" .= True]))))
                ("DELETE", ["v1", "clients", identifier]) -> do
                    releaseClient registry identifier
                    respond (json status202 (object ["closing" .= True]))
                ("POST", ["v1", "clients", identifier, "sessions", sessionId]) -> do
                    admitted <- admitLocalBoundary backend principal \boundary ->
                        backend.backendGetSession boundary sessionId >>= \case
                            Left err -> pure (json (mkStatus err.apiErrorStatus "")
                                (object ["error" .= object ["code" .= err.apiErrorCode, "message" .= err.apiErrorMessage]]))
                            Right _ -> bindClientSession registry identifier (ClientSession boundary sessionId)
                                >>= pure . either leaseError (const (json status200 (object ["attached" .= True])))
                    respond (either boundaryError id admitted)
                ("POST", ["v1", "service", "stop"]) -> do
                    let force = lookup "force" request.queryString == Just (Just "true")
                    active <- supervisorHasActiveTurns supervisor
                    admitted <- if active && not force then pure False else beginServiceShutdown registry force
                    if not admitted
                        then respond (failure status409 "service_busy" "the local server has connected clients or active work")
                        else do
                            received <- respond $ responseLBS status202
                                [(hContentType, "application/json"), ("Connection", "close")]
                                "{\"stopping\":true}"
                            -- Warp waits for HTTP handlers to finish during
                            -- shutdown. Signal a separate scoped worker so this
                            -- response never waits for its own connection.
                            void (tryPutMVar stopRequested ())
                            pure received
                ("DELETE", ["v1", "clients", identifier, "sessions", sessionId]) -> do
                    admitted <- admitLocalBoundary backend principal \boundary -> do
                        busy <- sessionHasActiveTurn supervisor boundary sessionId
                        if busy
                            then pure (failure status409 "session_busy" "finish or cancel the current turn before detaching")
                            else unbindClientSession registry identifier sessionId
                                >>= pure . either leaseError (const (json status200 (object ["detached" .= True])))
                    respond (either boundaryError id admitted)
                _ | request.requestMethod `elem` ["POST", "PATCH", "DELETE"] -> do
                    target <- mutationSession backend supervisor principal request
                    case target of
                        Left err -> respond (boundaryError err)
                        Right sessionId -> do
                            let clientId = lookup "X-Agent-Client-Id" request.requestHeaders
                                    >>= either (const Nothing) Just . TextEncoding.decodeUtf8'
                            owner <- maybe (pure Nothing) (sessionClientOwner registry) sessionId
                            case clientId of
                                Nothing | isJust owner -> respond (leaseError ClientSessionOwned)
                                Nothing -> withServiceMutation registry (application request respond)
                                    >>= either (respond . leaseError) pure
                                Just identifier -> withClientMutation registry identifier sessionId
                                    (withServiceMutation registry (application request respond)
                                        >>= either (respond . leaseError) pure)
                                    >>= either (respond . leaseError) pure
                  | otherwise -> application request respond
    withAsync (readMVar stopRequested >> joinShutdown service) \_ ->
        withAsync artifactMaintenance (\_ -> withAsync maintenance (\_ -> action wrapped)) `finally` cleanup

joinShutdown :: LocalService -> IO ()
joinShutdown service = readMVar service.serviceShutdown >>= id

localFileResponse :: Maybe FilePath -> IO Response
localFileResponse Nothing = pure (failure status404 "payload_not_found" "local payload not found")
localFileResponse (Just path) = do
    status <- tryIO (getSymbolicLinkStatus path)
    user <- getEffectiveUserID
    pure $ case status of
        Right file | isRegularFile file && fileOwner file == user && fileSize file <= 128 * 1024 * 1024 ->
            responseFile status200 [(hContentType, "application/json"), (hCacheControl, "no-store")] path Nothing
        _ -> failure status404 "payload_not_found" "local payload not found"

mutationSession :: Backend -> Supervisor -> Principal -> Request -> IO (Either ApiError (Maybe Text))
mutationSession backend supervisor principal request = case request.pathInfo of
    "v1" : "sessions" : sessionId : _ -> pure (Right (Just sessionId))
    "v1" : "turns" : rawTurnId : _ -> do
        result <- admitLocalBoundary backend principal \boundary -> do
            let turnId = TurnId (Text.toLower rawTurnId)
            current <- lookupTurn supervisor boundary turnId
            case current of
                Just turn -> pure (Right (Just turn.turnRecordSessionId))
                Nothing -> backend.backendLookupTurn boundary turnId >>= pure . (>>= \case
                    Just turn -> Right (Just turn.turnRecordSessionId)
                    Nothing -> Left (ApiError 404 "turn_not_found" "turn not found" Nothing))
        pure (result >>= id)
    "v1" : "requests" : rawRequestId : _ -> do
        admitted <- admitLocalBoundary backend principal \boundary ->
            listHumanRequests supervisor boundary Nothing
        pure $ case admitted of
            Left err -> Left err
            Right (Left message) -> Left (ApiError 503 "requests_unavailable" message Nothing)
            Right (Right requests) -> case find
                ((== RequestId (Text.toLower rawRequestId)) . (.humanRequestId)) requests of
                    Nothing -> Left (ApiError 404 "request_not_found" "the human request is not available" Nothing)
                    Just human -> Right (Just human.humanRequestSessionId)
    _ -> pure (Right Nothing)

admitLocalBoundary
    :: Backend -> Principal -> (AccessBoundary -> IO value) -> IO (Either ApiError value)
admitLocalBoundary Backend { backendAdmitBoundary = admit } principal action = admit principal action

activeStatus :: TurnStatus -> Bool
activeStatus status = status `elem` [TurnQueued, TurnRunning, TurnWaitingForInput]

json :: Status -> Value -> Response
json status value = responseLBS status [(hContentType, "application/json"), (hCacheControl, "no-store")] (encode value)

failure :: Status -> Text -> Text -> Response
failure status code message = json status (object ["error" .= object ["code" .= code, "message" .= message]])

leaseError :: ClientLeaseError -> Response
leaseError err = failure status409 "client_lease_rejected" (case err of
    ClientNotRegistered -> "the client is not registered"
    ClientLeaseExpired -> "the client lease has expired"
    ClientLimitReached -> "the local client limit was reached"
    ClientSessionLimitReached -> "the client session limit was reached"
    ClientSessionOwned -> "the session already has a writing client"
    ClientSessionNotOwned -> "the client does not own this session"
    ClientRegistryClosed -> "the local server is closing")

boundaryError :: ApiError -> Response
boundaryError err = failure (mkStatus err.apiErrorStatus "") err.apiErrorCode err.apiErrorMessage
