-- | Scoped supervision for the optional Apple on-device model helper.
--
-- The private macOS application embeds @apfel@ under @Contents/Helpers@. The
-- public runtime owns the child process so its bearer token never enters the
-- app or any tool subprocess environment.
module Agent.CLI.AppleFoundationModels
    ( appleFoundationModelId
    , appleFoundationModelRuntimeAvailable
    , withAppleFoundationModelRuntime
    ) where

import Agent.CLI.RuntimeModel
    ( RuntimeResponsesModel
    , appleFoundationModelsModelId
    , mkAppleFoundationModelsRuntime
    )
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar
    , modifyMVar_
    , newMVar
    )
import Control.Exception.Safe
    ( bracket
    , mask
    , onException
    , throwIO
    , tryAny
    )
import Control.Monad (forM_, unless, void)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.:), withObject)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as Base64Url
import Data.Char (isDigit)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Time.Clock
    ( UTCTime
    , addUTCTime
    , diffUTCTime
    , getCurrentTime
    )
import qualified Network.HTTP.Client as Http
import Network.HTTP.Types.Status (statusCode)
import qualified Network.Socket as Socket
import System.Directory
    ( canonicalizePath
    , doesFileExist
    , executable
    , getPermissions
    )
import System.Environment
    ( getEnvironment
    , getExecutablePath
    , lookupEnv
    )
import System.Exit (ExitCode(ExitSuccess))
import qualified System.FilePath as FilePath
import System.Info (arch, os)
import System.IO
    ( IOMode(ReadMode, WriteMode)
    , withBinaryFile
    , withFile
    )
import System.Posix.Signals (sigKILL, signalProcess)
import System.Process
    ( CreateProcess(..)
    , ProcessHandle
    , StdStream(NoStream, UseHandle)
    , createProcess
    , getPid
    , getProcessExitCode
    , proc
    , readProcess
    , readProcessWithExitCode
    , terminateProcess
    , waitForProcess
    )
import System.Timeout (timeout)
import System.IO.Unsafe (unsafePerformIO)

appleFoundationModelId :: Text
appleFoundationModelId = appleFoundationModelsModelId

data ManagedAppleModel = ManagedAppleModel
    { managedRuntime :: !RuntimeResponsesModel
    , managedProcess :: !ProcessHandle
    , managedPort :: !Int
    , managedContextWindow :: !Int
    , managedHttpManager :: !Http.Manager
    , managedHealthPendingSince :: !(Maybe UTCTime)
    }

data SharedAppleModel
    = AppleModelNotStarted
    | AppleModelUnavailable !UTCTime
    | AppleModelRunning !ManagedAppleModel !Int

data AppleModelLease
    = UnavailableLease
    | RunningLease !RuntimeResponsesModel

data AppleModelStart
    = AppleModelStarted !ManagedAppleModel
    | AppleModelStartUnavailable
    | AppleModelStartRetryable

data ManagedAppleModelHealth
    = ManagedAppleModelHealthy
    | ManagedAppleModelPending
    | ManagedAppleModelUnhealthy

{-# NOINLINE sharedAppleModel #-}
sharedAppleModel :: MVar SharedAppleModel
sharedAppleModel = unsafePerformIO (newMVar AppleModelNotStarted)

data HealthResponse = HealthResponse
    { healthStatus :: !Text
    , healthModelAvailable :: !Bool
    , healthModel :: !Text
    , healthContextWindow :: !Int
    }

instance Aeson.FromJSON HealthResponse where
    parseJSON = withObject "apfel health response" \object ->
        HealthResponse
            <$> object .: "status"
            <*> object .: "model_available"
            <*> object .: "model"
            <*> object .: "context_window"

withAppleFoundationModelRuntime
    :: (IO (Maybe RuntimeResponsesModel) -> IO value)
    -> IO value
withAppleFoundationModelRuntime action =
    bracket
        acquireAppleModelClient
        releaseAppleModelClient
        (action . refreshAppleModelClient)

acquireAppleModelClient :: IO (MVar AppleModelLease)
acquireAppleModelClient =
    mask \_ -> acquireAppleModel >>= newMVar

releaseAppleModelClient :: MVar AppleModelLease -> IO ()
releaseAppleModelClient leaseVariable =
    modifyMVar_ leaseVariable \lease -> do
        releaseAppleModel lease
        pure UnavailableLease

refreshAppleModelClient
    :: MVar AppleModelLease
    -> IO (Maybe RuntimeResponsesModel)
refreshAppleModelClient leaseVariable =
    modifyMVar leaseVariable \lease ->
        case lease of
            UnavailableLease -> replaceLease lease
            RunningLease runtime ->
                appleFoundationModelRuntimeAvailable runtime >>= \case
                    True -> pure (lease, Just runtime)
                    False -> replaceLease lease
  where
    replaceLease lease =
        mask \_ -> do
            releaseAppleModel lease
            replacement <- acquireAppleModel
            pure (replacement, leaseRuntime replacement)
    leaseRuntime = \case
        UnavailableLease -> Nothing
        RunningLease runtime -> Just runtime

acquireAppleModel :: IO AppleModelLease
acquireAppleModel =
    modifyMVar sharedAppleModel \case
        AppleModelRunning managed references ->
            refreshManagedAppleModel managed >>= \case
                Just refreshed ->
                    pure
                        ( AppleModelRunning refreshed (references + 1)
                        , RunningLease refreshed.managedRuntime
                        )
                Nothing -> do
                    stopAppleModel managed
                    startLease
        unavailable@(AppleModelUnavailable retryAfter) -> do
            now <- getCurrentTime
            if now < retryAfter
                then pure (unavailable, UnavailableLease)
                else startLease
        AppleModelNotStarted -> startLease
  where
    startLease =
        tryAny discoverAndStartAppleModel >>= \case
            Left _ -> cacheUnavailable
            Right (Just managed) ->
                pure
                    ( AppleModelRunning managed 1
                    , RunningLease managed.managedRuntime
                    )
            Right Nothing -> cacheUnavailable
    cacheUnavailable = do
        retryAfter <- addUTCTime 30 <$> getCurrentTime
        pure (AppleModelUnavailable retryAfter, UnavailableLease)

releaseAppleModel :: AppleModelLease -> IO ()
releaseAppleModel = \case
    UnavailableLease -> pure ()
    RunningLease runtime ->
        modifyMVar_ sharedAppleModel \case
            AppleModelRunning managed references
                | managed.managedRuntime == runtime
                    && references <= 1 -> do
                        stopAppleModel managed
                        pure AppleModelNotStarted
                | managed.managedRuntime == runtime ->
                    pure (AppleModelRunning managed (references - 1))
            state -> pure state

-- | Revalidate that the shared helper behind a runtime descriptor is still
-- the live owner of its loopback listener.
appleFoundationModelRuntimeAvailable
    :: RuntimeResponsesModel
    -> IO Bool
appleFoundationModelRuntimeAvailable runtime =
    modifyMVar sharedAppleModel \case
        AppleModelRunning managed references
            | managed.managedRuntime == runtime ->
                refreshManagedAppleModel managed >>= \case
                    Just refreshed ->
                        pure
                            ( AppleModelRunning refreshed references
                            , True
                            )
                    Nothing -> do
                        stopAppleModel managed
                        pure (AppleModelNotStarted, False)
        state -> pure (state, False)

discoverAndStartAppleModel :: IO (Maybe ManagedAppleModel)
discoverAndStartAppleModel =
    findApfelExecutable >>= \case
        Nothing -> pure Nothing
        Just executablePath -> startWithRetries 3 executablePath
  where
    startWithRetries :: Int -> FilePath -> IO (Maybe ManagedAppleModel)
    startWithRetries remaining executablePath =
        startAppleModel executablePath >>= \case
            AppleModelStarted managed -> pure (Just managed)
            AppleModelStartUnavailable -> pure Nothing
            AppleModelStartRetryable
                | remaining > 1 ->
                    startWithRetries (remaining - 1) executablePath
                | otherwise -> pure Nothing

findApfelExecutable :: IO (Maybe FilePath)
findApfelExecutable
    | os /= "darwin" || arch /= "aarch64" = pure Nothing
    | otherwise =
        macOSSupportsFoundationModels >>= \case
            False -> pure Nothing
            True -> do
                executablePath <- getExecutablePath
                let directory = FilePath.takeDirectory executablePath
                    bundledPath =
                        directory FilePath.</> ".."
                            FilePath.</> "Helpers"
                            FilePath.</> "apfel"
                    isApplicationBundle =
                        FilePath.takeFileName directory == "MacOS"
                            && FilePath.takeFileName
                                (FilePath.takeDirectory directory)
                                == "Contents"
                if isApplicationBundle
                    then firstExecutable [bundledPath]
                    else do
                        configured <-
                            lookupEnv "HASKELL_AGENT_APFEL_EXECUTABLE"
                        firstExecutable $
                            [bundledPath, directory FilePath.</> "apfel"]
                                <> maybe
                                    []
                                    (\path ->
                                        [path | not (null path)])
                                    configured

macOSSupportsFoundationModels :: IO Bool
macOSSupportsFoundationModels =
    tryAny (readProcess "/usr/bin/sw_vers" ["-productVersion"] "") >>= \case
        Left _ -> pure False
        Right version ->
            pure $ case reads (takeWhile isDigit version) of
                [(major, "")] -> (major :: Int) >= 26
                _ -> False

firstExecutable :: [FilePath] -> IO (Maybe FilePath)
firstExecutable = \case
    [] -> pure Nothing
    candidate : rest ->
        doesFileExist candidate >>= \case
            False -> firstExecutable rest
            True -> do
                permissions <- getPermissions candidate
                if executable permissions
                    then Just <$> canonicalizePath candidate
                    else firstExecutable rest

startAppleModel :: FilePath -> IO AppleModelStart
startAppleModel executablePath = mask \restore -> do
    port <- reserveLoopbackPort
    token <- randomBearerToken
    inherited <- getEnvironment
    let childEnvironment =
            setChildEnvironment "NO_COLOR" "1"
                (setChildEnvironment "APFEL_TOKEN" (Text.unpack token) inherited)
        process =
            (proc executablePath
                [ "--serve"
                , "--host", "127.0.0.1"
                , "--port", show port
                , "--max-concurrent", "1"
                ])
                { env = Just childEnvironment
                , std_in = NoStream
                , std_out = NoStream
                , std_err = NoStream
                }
    (_, _, _, processHandle) <-
        withFile "/dev/null" WriteMode \nullHandle ->
            createProcess process
                { std_out = UseHandle nullHandle
                , std_err = UseHandle nullHandle
                }
    let cleanup = stopProcess processHandle
    restore
        (do
            manager <- Http.newManager Http.defaultManagerSettings
            awaitAppleModel manager processHandle port >>= \case
                HealthReady contextWindow -> do
                    listenerOwnedByProcess processHandle port >>= \case
                        False -> do
                            cleanup
                            pure AppleModelStartRetryable
                        True -> do
                            runtime <-
                                either
                                    (throwIO . userError . Text.unpack)
                                    pure
                                    (mkAppleFoundationModelsRuntime
                                        ("http://127.0.0.1:"
                                            <> Text.pack (show port)
                                            <> "/v1")
                                        token
                                        contextWindow)
                            pure $ AppleModelStarted ManagedAppleModel
                                { managedRuntime = runtime
                                , managedProcess = processHandle
                                , managedPort = port
                                , managedContextWindow = contextWindow
                                , managedHttpManager = manager
                                , managedHealthPendingSince = Nothing
                                }
                HealthUnavailable -> do
                    cleanup
                    pure AppleModelStartUnavailable
                HealthFailed -> do
                    cleanup
                    pure AppleModelStartRetryable
                HealthPending -> do
                    cleanup
                    pure AppleModelStartRetryable)
        `onException` cleanup

setChildEnvironment
    :: String
    -> String
    -> [(String, String)]
    -> [(String, String)]
setChildEnvironment name value inherited =
    (name, value) : filter ((/= name) . fst) inherited

reserveLoopbackPort :: IO Int
reserveLoopbackPort =
    Socket.withSocketsDo $
        bracket
            (Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol)
            Socket.close
            \socket -> do
                Socket.bind socket
                    (Socket.SockAddrInet
                        0
                        (Socket.tupleToHostAddress (127, 0, 0, 1)))
                Socket.getSocketName socket >>= \case
                    Socket.SockAddrInet port _ ->
                        pure (fromIntegral port)
                    _ -> throwIO (userError "could not reserve an IPv4 port")

randomBearerToken :: IO Text
randomBearerToken = do
    bytes <- withBinaryFile "/dev/urandom" ReadMode (`BS.hGet` 32)
    unless (BS.length bytes == 32) $
        throwIO (userError "could not read bearer-token entropy")
    pure $ TextEncoding.decodeUtf8 (Base64Url.encodeUnpadded bytes)

awaitAppleModel :: Http.Manager -> ProcessHandle -> Int -> IO HealthStatus
awaitAppleModel manager processHandle port =
    timeout 3_000_000 (go (30 :: Int)) >>= \case
        Just status -> pure status
        Nothing -> pure HealthFailed
  where
    go 0 = pure HealthFailed
    go remaining =
        getProcessExitCode processHandle >>= \case
            Just _ -> pure HealthFailed
            Nothing ->
                checkHealth manager port >>= \case
                    HealthReady contextWindow ->
                        pure (HealthReady contextWindow)
                    HealthUnavailable -> pure HealthUnavailable
                    HealthFailed -> pure HealthFailed
                    HealthPending -> do
                        threadDelay 100_000
                        go (remaining - 1)

data HealthStatus
    = HealthReady !Int
    | HealthUnavailable
    | HealthPending
    | HealthFailed

checkHealth :: Http.Manager -> Int -> IO HealthStatus
checkHealth manager port =
    timeout 750_000 (tryAny request) >>= \case
        Just (Right (responseStatus, Just responseBody))
            | statusCode responseStatus /= 200 ->
                pure HealthPending
            | otherwise ->
                case
                    (Aeson.eitherDecodeStrict' responseBody
                        :: Either String HealthResponse) of
                    Left _ -> pure HealthPending
                    Right health
                        | health.healthStatus == "ok"
                            && health.healthModelAvailable
                            && health.healthModel == appleFoundationModelId
                            && health.healthContextWindow
                                `elem` [4096, 8192] ->
                                pure
                                    (HealthReady
                                        health.healthContextWindow)
                        | otherwise -> pure HealthUnavailable
        _ -> pure HealthPending
  where
    request = do
        requestWithoutToken <- Http.parseRequest
            ("http://127.0.0.1:" <> show port <> "/health")
        let bounded = requestWithoutToken
                { Http.responseTimeout =
                    Http.responseTimeoutMicro 500_000
                }
        Http.withResponse bounded manager \response -> do
            body <- readBoundedResponseBody 16_384
                (Http.responseBody response)
            pure (Http.responseStatus response, body)

readBoundedResponseBody
    :: Int
    -> Http.BodyReader
    -> IO (Maybe BS.ByteString)
readBoundedResponseBody limit = go [] 0
  where
    go chunks size reader = do
        chunk <- Http.brRead reader
        if BS.null chunk
            then pure (Just (BS.concat (reverse chunks)))
            else
                let nextSize = size + BS.length chunk
                in if nextSize > limit
                    then pure Nothing
                    else go (chunk : chunks) nextSize reader

managedAppleModelHealth
    :: ManagedAppleModel
    -> IO ManagedAppleModelHealth
managedAppleModelHealth managed =
    getProcessExitCode managed.managedProcess >>= \case
        Just _ -> do
            void (waitForProcess managed.managedProcess)
            pure ManagedAppleModelUnhealthy
        Nothing ->
            listenerOwnedByProcess
                managed.managedProcess
                managed.managedPort >>= \case
                    False -> pure ManagedAppleModelUnhealthy
                    True ->
                        checkHealth
                            managed.managedHttpManager
                            managed.managedPort >>= \case
                                HealthReady contextWindow
                                    | contextWindow
                                        == managed.managedContextWindow ->
                                            pure ManagedAppleModelHealthy
                                    | otherwise ->
                                        pure ManagedAppleModelUnhealthy
                                HealthPending ->
                                    pure ManagedAppleModelPending
                                HealthUnavailable ->
                                    pure ManagedAppleModelUnhealthy
                                HealthFailed ->
                                    pure ManagedAppleModelUnhealthy

refreshManagedAppleModel
    :: ManagedAppleModel
    -> IO (Maybe ManagedAppleModel)
refreshManagedAppleModel managed =
    tryAny (managedAppleModelHealth managed) >>= \case
        Left _ -> pure Nothing
        Right ManagedAppleModelUnhealthy -> pure Nothing
        Right ManagedAppleModelHealthy ->
            pure $ Just managed
                { managedHealthPendingSince = Nothing
                }
        Right ManagedAppleModelPending -> do
            now <- getCurrentTime
            case managed.managedHealthPendingSince of
                Nothing ->
                    pure $ Just managed
                        { managedHealthPendingSince = Just now
                        }
                Just pendingSince
                    | diffUTCTime now pendingSince < 5 ->
                        pure (Just managed)
                    | otherwise -> pure Nothing

listenerOwnedByProcess :: ProcessHandle -> Int -> IO Bool
listenerOwnedByProcess processHandle port =
    getPid processHandle >>= \case
        Nothing -> pure False
        Just processId ->
            timeout 1_000_000
                (tryAny
                    (readProcessWithExitCode
                        "/usr/sbin/lsof"
                        [ "-nP"
                        , "-a"
                        , "-p", show processId
                        , "-iTCP@127.0.0.1:" <> show port
                        , "-sTCP:LISTEN"
                        , "-Fp"
                        ]
                        "")) >>= \case
                            Just (Right (ExitSuccess, output, _)) ->
                                pure $
                                    ("p" <> show processId)
                                        `elem` lines output
                            _ -> pure False

stopAppleModel :: ManagedAppleModel -> IO ()
stopAppleModel = stopProcess . (.managedProcess)

stopProcess :: ProcessHandle -> IO ()
stopProcess processHandle =
    tryAny (getProcessExitCode processHandle) >>= \case
        Right (Just _) -> reap
        _ -> do
            processId <-
                tryAny (getPid processHandle) >>= \case
                    Right result -> pure result
                    Left _ -> pure Nothing
            void $ tryAny (terminateProcess processHandle)
            timeout 2_000_000 (tryAny (waitForProcess processHandle))
                >>= \case
                    Just (Right _) -> pure ()
                    _ -> do
                        forM_ processId \pid ->
                            void $ tryAny (signalProcess sigKILL pid)
                        void $
                            timeout 2_000_000
                                (tryAny (waitForProcess processHandle))
  where
    reap = void $ tryAny (waitForProcess processHandle)
