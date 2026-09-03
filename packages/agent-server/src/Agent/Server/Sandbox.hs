-- | Fail-closed, versioned transport to one tenant's sandbox runner.
--
-- The runner owns the microVM lifecycle. Its stdout is reserved for bounded
-- newline-delimited JSON; stderr is drained and discarded so an untrusted guest
-- cannot block the server or inject into its logs.
module Agent.Server.Sandbox
    ( TenantSandbox
    , openTenantSandbox
    , closeTenantSandbox
    , composeSandboxTools
    ) where

import Agent.Dialect (DialectId, dialectSlug)
import Agent.Server.Identifier (isUUIDText, newUUIDv7Text)
import Agent.Server.Tenant
    ( ResolvedTenant(..)
    , renderTenantId
    )
import Agent.ToolDispatch
    ( ToolCall(..)
    , ToolCallKind(..)
    , ToolHandlerResult(..)
    , ToolResultImage(..)
    , passthroughTool
    )
import Agent.Tools.Types
    ( AppTool(..)
    , AppToolGroup(..)
    )
import Control.Concurrent
    ( threadDelay )
import Control.Concurrent.Async
    ( Async
    , async
    , cancel
    , race
    , waitCatch
    )
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar_
    , newMVar
    , readMVar
    , swapMVar
    , withMVar
    )
import Control.Exception.Safe
    ( displayException
    , mask
    , onException
    , tryAny
    )
import Control.Monad (unless, void, when)
import Data.Aeson
    ( FromJSON(..)
    , Object
    , Value(..)
    , eitherDecodeStrict'
    , encode
    , object
    , withObject
    , (.:)
    , (.:?)
    , (.=)
    )
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser)
import Data.Aeson.Types qualified as AesonTypes
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.IORef
    ( IORef
    , newIORef
    , readIORef
    , writeIORef
    )
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import System.Directory
    ( createDirectoryIfMissing
    )
import System.Environment (getEnvironment)
import System.FilePath
    ( addTrailingPathSeparator
    , makeRelative
    , normalise
    , splitDirectories
    , (</>)
    )
import System.IO
    ( BufferMode(NoBuffering)
    , Handle
    , hClose
    , hFlush
    , hSetBinaryMode
    , hSetBuffering
    )
import System.Posix.Files (setFileMode)
import System.Posix.Signals
    ( sigKILL
    , signalProcessGroup
    )
import System.Process
    ( CreateProcess(..)
    , ProcessHandle
    , StdStream(CreatePipe)
    , createProcess
    , getPid
    , interruptProcessGroupOf
    , proc
    , terminateProcess
    , waitForProcess
    )
import System.Timeout (timeout)

protocolVersion :: Int
protocolVersion = 1

maximumRequestBytes :: Int
maximumRequestBytes = 4 * 1024 * 1024

maximumResponseBytes :: Int
maximumResponseBytes = 16 * 1024 * 1024

maximumStreamBytes :: Int
maximumStreamBytes = 16 * 1024 * 1024

maximumToolDurationMicroseconds :: Int
maximumToolDurationMicroseconds = 15 * 60 * 1000 * 1000

maximumReadinessDurationMicroseconds :: Int
maximumReadinessDurationMicroseconds = 120 * 1000 * 1000

data TenantSandbox = TenantSandbox
    { sandboxRunner :: !FilePath
    , sandboxTenant :: !ResolvedTenant
    , sandboxLock :: !(MVar ())
    , sandboxProcess :: !(MVar (Maybe RunningSandbox))
    , sandboxClosed :: !(MVar Bool)
    }

data RunningSandbox = RunningSandbox
    { runningInput :: !Handle
    , runningOutput :: !Handle
    , runningProcess :: !ProcessHandle
    , runningStderrDrain :: !(Async ())
    , runningGeneration :: !Text
    , runningReadBuffer :: !(IORef ByteString)
    }

data BrokerReady = BrokerReady
    { readyTenantId :: !Text
    , readyGeneration :: !Text
    , readyWorkspace :: !Text
    , readyState :: !Text
    }

data BrokerMessage
    = BrokerOutput !Text !Text !Text !Text
    | BrokerResult !Text !Text !Text !Bool !Text ![ToolResultImage]

openTenantSandbox
    :: FilePath
    -> ResolvedTenant
    -> IO (Either Text TenantSandbox)
openTenantSandbox runner tenant = do
    prepareTenantDirectories tenant >>= \case
        Left err -> pure (Left err)
        Right () -> do
            lock <- newMVar ()
            process <- newMVar Nothing
            closed <- newMVar False
            pure $
                Right TenantSandbox
                    { sandboxRunner = runner
                    , sandboxTenant = tenant
                    , sandboxLock = lock
                    , sandboxProcess = process
                    , sandboxClosed = closed
                    }

closeTenantSandbox :: TenantSandbox -> IO ()
closeTenantSandbox sandbox =
    withMVar sandbox.sandboxLock \_ -> do
        modifyMVar_ sandbox.sandboxClosed (const (pure True))
        swapMVar sandbox.sandboxProcess Nothing >>= mapM_ stopSandbox

-- | Keep explicit host-service groups in process and replace every execution
-- group with protocol-backed handlers before the generic runtime receives a
-- flat tool list.
composeSandboxTools
    :: TenantSandbox
    -> Text
    -- ^ Durable session id.
    -> FilePath
    -- ^ Canonical host cwd.
    -> DialectId
    -> [AppToolGroup]
    -> [AppTool]
composeSandboxTools sandbox sessionId cwd dialect =
    concatMap composeGroup
  where
    composeGroup = \case
        HostToolGroup tools -> tools
        ExecutionToolGroup tools -> map proxy tools
    proxy tool =
        tool
            { appToolHandler =
                passthroughTool tool.appToolName
                    (invokeTenantTool sandbox sessionId cwd dialect)
            -- Host resource resolvers must not inspect paths for a guest
            -- operation. The tenant broker serializes calls itself.
            , appToolResourceClaims = Nothing
            }

invokeTenantTool
    :: TenantSandbox
    -> Text
    -> FilePath
    -> DialectId
    -> (Text -> IO ())
    -> ToolCall
    -> IO (Either Text ToolHandlerResult)
invokeTenantTool sandbox sessionId cwd dialect emit call =
    withMVar sandbox.sandboxLock \_ -> mask \restore -> do
        closed <- readMVar sandbox.sandboxClosed
        if closed
            then pure (Left "the tenant sandbox is closed")
            else do
                ensureRunning sandbox >>= \case
                    Left err -> pure (Left err)
                    Right running -> do
                        requestId <- newUUIDv7Text
                        case guestCwd sandbox.sandboxTenant cwd of
                            Left err -> pure (Left err)
                            Right _
                                | call.argumentsEncrypted ->
                                    pure
                                        (Left
                                            "encrypted tool arguments cannot cross the sandbox protocol")
                            Right mappedCwd -> do
                                let request =
                                        encodeToolRequest
                                            sandbox
                                            running
                                            requestId
                                            sessionId
                                            mappedCwd
                                            dialect
                                            call
                                if LazyByteString.length request
                                    > fromIntegral maximumRequestBytes
                                    then
                                        pure
                                            (Left
                                                "sandbox tool request exceeds the protocol limit")
                                    else do
                                        outcome <-
                                            tryAny
                                                (restore
                                                    (timeout
                                                        maximumToolDurationMicroseconds
                                                        (exchange
                                                            sandbox
                                                            running
                                                            requestId
                                                            emit
                                                            request)))
                                                `onException`
                                                    invalidateSandbox
                                                        sandbox
                                                        running
                                        case outcome of
                                            Left _ -> do
                                                invalidateSandbox sandbox running
                                                pure
                                                    (Left
                                                        "sandbox transport failed")
                                            Right Nothing -> do
                                                invalidateSandbox sandbox running
                                                pure
                                                    (Left
                                                        "sandbox tool request timed out")
                                            Right (Just (Left err)) -> do
                                                invalidateSandbox sandbox running
                                                pure
                                                    (Left
                                                        ("sandbox protocol failed: "
                                                            <> err))
                                            Right (Just (Right result)) ->
                                                pure result

ensureRunning
    :: TenantSandbox
    -> IO (Either Text RunningSandbox)
ensureRunning sandbox =
    readMVar sandbox.sandboxProcess >>= \case
        Just running -> pure (Right running)
        Nothing ->
            launchSandbox sandbox >>= \case
                Left err -> pure (Left err)
                Right running ->
                    (do
                        modifyMVar_
                            sandbox.sandboxProcess
                            (const (pure (Just running)))
                        pure (Right running))
                        `onException` stopSandbox running

launchSandbox
    :: TenantSandbox
    -> IO (Either Text RunningSandbox)
launchSandbox sandbox = mask \restore -> do
    environment <- sanitizedRunnerEnvironment sandbox.sandboxTenant
    let tenant = sandbox.sandboxTenant
        arguments =
            [ "serve"
            , "--protocol-version", show protocolVersion
            , "--tenant-id", Text.unpack (renderTenantId tenant.resolvedTenantId)
            , "--workspace-root", tenant.resolvedTenantWorkspaceRoot
            , "--workspace-device"
            , show tenant.resolvedTenantWorkspaceDevice
            , "--workspace-inode"
            , show tenant.resolvedTenantWorkspaceInode
            , "--state-root", tenant.resolvedTenantStateDirectory
            ]
        command =
            (proc sandbox.sandboxRunner arguments)
                { std_in = CreatePipe
                , std_out = CreatePipe
                , std_err = CreatePipe
                , cwd = Just tenant.resolvedTenantStateDirectory
                , env = Just environment
                , close_fds = True
                , create_group = True
                }
    started <- tryAny (restore (createProcess command))
    case started of
        Left exception ->
            pure
                (Left
                    ("could not start the tenant sandbox runner: "
                        <> Text.pack (displayException exception)))
        Right (Just input, Just output, Just stderrHandle, process) -> do
            readBuffer <- newIORef ByteString.empty
            let stopUnmanaged =
                    stopUnmanagedSandbox
                        input
                        output
                        stderrHandle
                        process
            (do
                configurePipe input
                configurePipe output
                configurePipe stderrHandle)
                `onException` stopUnmanaged
            stderrDrain <-
                async (drainHandle stderrHandle)
                    `onException` stopUnmanaged
            let provisional = RunningSandbox
                    { runningInput = input
                    , runningOutput = output
                    , runningProcess = process
                    , runningStderrDrain = stderrDrain
                    , runningGeneration = ""
                    , runningReadBuffer = readBuffer
                    }
            handshake <-
                restore
                    (race
                        (threadDelay maximumReadinessDurationMicroseconds)
                        (readBoundedJson
                            output
                            readBuffer
                            maximumResponseBytes))
                    `onException` stopSandbox provisional
            case handshake of
                Left () -> do
                    stopSandbox provisional
                    pure (Left "the tenant sandbox readiness handshake timed out")
                Right (Left err) -> do
                    stopSandbox provisional
                    pure (Left ("invalid tenant sandbox handshake: " <> err))
                Right (Right value) ->
                    case parseReady value of
                        Left err -> do
                            stopSandbox provisional
                            pure (Left ("invalid tenant sandbox handshake: " <> err))
                        Right ready ->
                            case validateReady tenant ready of
                                Left err -> do
                                    stopSandbox provisional
                                    pure (Left err)
                                Right () ->
                                    pure $
                                        Right provisional
                                            { runningGeneration =
                                                ready.readyGeneration
                                            }
        Right _ ->
            pure
                (Left
                    "the tenant sandbox runner did not provide dedicated pipes")

exchange
    :: TenantSandbox
    -> RunningSandbox
    -> Text
    -> (Text -> IO ())
    -> LazyByteString.ByteString
    -> IO (Either Text (Either Text ToolHandlerResult))
exchange sandbox running requestId emit request = do
    LazyByteString.hPut running.runningInput request
    LazyByteString.hPut running.runningInput "\n"
    hFlush running.runningInput
    readResponses 0
  where
    expectedTenant =
        renderTenantId sandbox.sandboxTenant.resolvedTenantId
    expectedGeneration = running.runningGeneration

    readResponses streamed =
        readBoundedJson
            running.runningOutput
            running.runningReadBuffer
            maximumResponseBytes >>= \case
            Left err -> pure (Left err)
            Right value ->
                case parseBrokerMessage value of
                    Left err -> pure (Left ("invalid sandbox response: " <> err))
                    Right (BrokerOutput tenant generation responseId output)
                        | tenant /= expectedTenant ->
                            pure (Left "sandbox response tenant mismatch")
                        | generation /= expectedGeneration ->
                            pure (Left "sandbox response generation mismatch")
                        | responseId /= requestId ->
                            pure (Left "sandbox response request mismatch")
                        | streamed + encodedTextLength output
                            > maximumStreamBytes ->
                            pure (Left "sandbox streamed output exceeds the protocol limit")
                        | otherwise -> do
                            emit output
                            readResponses
                                (streamed + encodedTextLength output)
                    Right (BrokerResult tenant generation responseId ok output images)
                        | tenant /= expectedTenant ->
                            pure (Left "sandbox response tenant mismatch")
                        | generation /= expectedGeneration ->
                            pure (Left "sandbox response generation mismatch")
                        | responseId /= requestId ->
                            pure (Left "sandbox response request mismatch")
                        | otherwise ->
                            pure $
                                Right $
                                    if ok
                                        then
                                            Right ToolHandlerResult
                                                { resultText = output
                                                , resultImages = images
                                                }
                                        else Left output

invalidateSandbox :: TenantSandbox -> RunningSandbox -> IO ()
invalidateSandbox sandbox expected =
    swapMVar sandbox.sandboxProcess Nothing >>= \case
        Just current
            | current.runningGeneration == expected.runningGeneration ->
                stopSandbox current
            | otherwise -> do
                -- A newer generation won a restart race. Preserve it.
                modifyMVar_
                    sandbox.sandboxProcess
                    (const (pure (Just current)))
        Nothing -> pure ()

stopSandbox :: RunningSandbox -> IO ()
stopSandbox running = do
    void (tryAny (hClose running.runningInput))
    void (tryAny (hClose running.runningOutput))
    stopSandboxProcess running.runningProcess
    cancel running.runningStderrDrain
    void (waitCatch running.runningStderrDrain)

stopUnmanagedSandbox
    :: Handle
    -> Handle
    -> Handle
    -> ProcessHandle
    -> IO ()
stopUnmanagedSandbox input output stderrHandle process = do
    mapM_ (void . tryAny . hClose) [input, output, stderrHandle]
    stopSandboxProcess process

stopSandboxProcess :: ProcessHandle -> IO ()
stopSandboxProcess process = do
    void (tryAny (interruptProcessGroupOf process))
    void (tryAny (terminateProcess process))
    timeout (5 * 1000 * 1000) (waitForProcess process)
        >>= \case
            Just _ -> pure ()
            Nothing -> do
                getPid process >>= mapM_
                    (signalProcessGroup sigKILL)
                void $
                    timeout
                        (5 * 1000 * 1000)
                        (waitForProcess process)

configurePipe :: Handle -> IO ()
configurePipe handle = do
    hSetBinaryMode handle True
    hSetBuffering handle NoBuffering

drainHandle :: Handle -> IO ()
drainHandle handle = do
    let loop =
            ByteString.hGetSome handle 4096 >>= \chunk ->
                unless (ByteString.null chunk) loop
    void (tryAny loop)
    void (tryAny (hClose handle))

readBoundedJson
    :: Handle
    -> IORef ByteString
    -> Int
    -> IO (Either Text Value)
readBoundedJson handle buffer limit =
    readBoundedLine handle buffer limit >>= \case
        Left err -> pure (Left err)
        Right bytes ->
            pure $
                case eitherDecodeStrict' bytes of
                    Left _ -> Left "message is not valid JSON"
                    Right value -> Right value

readBoundedLine
    :: Handle
    -> IORef ByteString
    -> Int
    -> IO (Either Text ByteString)
readBoundedLine handle buffer limit = readIORef buffer >>= go [] 0
  where
    go chunks used bytes =
        case ByteString.elemIndex 10 bytes of
            Just newline -> do
                let line = ByteString.take newline bytes
                    remainder = ByteString.drop (newline + 1) bytes
                writeIORef buffer remainder
                if used + ByteString.length line > limit
                    then pure (Left "message exceeds the protocol limit")
                    else
                        pure
                            (Right
                                (ByteString.concat
                                    (reverse (line : chunks))))
            Nothing
                | used + ByteString.length bytes > limit ->
                    pure (Left "message exceeds the protocol limit")
                | otherwise -> do
                    chunk <- ByteString.hGetSome handle 32768
                    if ByteString.null chunk
                        then
                            pure
                                (Left
                                    "sandbox transport closed unexpectedly")
                        else
                            go
                                (bytes : chunks)
                                (used + ByteString.length bytes)
                                chunk

encodeToolRequest
    :: TenantSandbox
    -> RunningSandbox
    -> Text
    -> Text
    -> FilePath
    -> DialectId
    -> ToolCall
    -> LazyByteString.ByteString
encodeToolRequest sandbox running requestId sessionId cwd dialect call =
    encode $
        object
            [ "type" .= ("tool" :: Text)
            , "version" .= protocolVersion
            , "tenantId" .=
                renderTenantId sandbox.sandboxTenant.resolvedTenantId
            , "generation" .= running.runningGeneration
            , "requestId" .= requestId
            , "sessionId" .= sessionId
            , "cwd" .= cwd
            , "dialect" .= dialectSlug dialect
            , "call" .= object
                [ "id" .= call.callId
                , "name" .= call.name
                , "arguments" .=
                    translateArgumentsToGuest
                        sandbox.sandboxTenant
                        call
                , "kind" .= callKindText call.callKind
                , "argumentsEncrypted" .= call.argumentsEncrypted
                ]
            ]

parseReady :: Value -> Either Text BrokerReady
parseReady value =
    case AesonTypes.parseEither parseJSON value of
        Left _ -> Left "readiness object does not match the protocol"
        Right ready -> Right ready

parseBrokerMessage :: Value -> Either Text BrokerMessage
parseBrokerMessage value =
    case AesonTypes.parseEither parser value of
        Left _ -> Left "response object does not match the protocol"
        Right message -> Right message
  where
    parser = withObject "BrokerMessage" \payload -> do
        messageType <- payload .: "type"
        case (messageType :: Text) of
            "output" -> do
                rejectUnknownFields
                    "BrokerOutput"
                    [ "type", "version", "tenantId", "generation"
                    , "requestId", "output"
                    ]
                    payload
                requireProtocol payload
                BrokerOutput
                    <$> payload .: "tenantId"
                    <*> payload .: "generation"
                    <*> payload .: "requestId"
                    <*> payload .: "output"
            "result" -> do
                rejectUnknownFields
                    "BrokerResult"
                    [ "type", "version", "tenantId", "generation"
                    , "requestId", "ok", "output", "images"
                    ]
                    payload
                requireProtocol payload
                BrokerResult
                    <$> payload .: "tenantId"
                    <*> payload .: "generation"
                    <*> payload .: "requestId"
                    <*> payload .: "ok"
                    <*> payload .: "output"
                    <*> (payload .:? "images" >>= \images -> do
                        let values = maybe [] id images
                        when (length values > 8)
                            (fail "too many sandbox result images")
                        traverse parseResultImage values)
            _ -> fail "unsupported broker message type"

instance FromJSON BrokerReady where
    parseJSON = withObject "BrokerReady" \payload -> do
        rejectUnknownFields
            "BrokerReady"
            [ "type", "version", "tenantId", "generation"
            , "workspace", "state"
            ]
            payload
        messageType <- payload .: "type"
        unless ((messageType :: Text) == "ready")
            (fail "expected a readiness message")
        requireProtocol payload
        BrokerReady
            <$> payload .: "tenantId"
            <*> payload .: "generation"
            <*> payload .: "workspace"
            <*> payload .: "state"

parseResultImage :: Value -> Parser ToolResultImage
parseResultImage = withObject "ToolResultImage" \payload -> do
        rejectUnknownFields "ToolResultImage" ["url", "detail"] payload
        url <- payload .: "url"
        detail <- payload .:? "detail"
        unless
            (isSafeImageDataUrl url
                && encodedTextLength url <= maximumResponseBytes)
            (fail "sandbox result image must be a bounded image data URL")
        pure ToolResultImage
            { imageUrl = url
            , imageDetail = detail
            }

validateReady :: ResolvedTenant -> BrokerReady -> Either Text ()
validateReady tenant ready
    | ready.readyTenantId /= renderTenantId tenant.resolvedTenantId =
        Left "tenant sandbox attested the wrong tenant"
    | not (isUUIDText ready.readyGeneration) =
        Left "tenant sandbox supplied an invalid generation"
    | ready.readyWorkspace /= "/workspace" =
        Left "tenant sandbox attested an unexpected workspace mount"
    | ready.readyState /= "/state" =
        Left "tenant sandbox attested an unexpected state mount"
    | otherwise = Right ()

requireProtocol :: Object -> Parser ()
requireProtocol payload = do
    version <- payload .: "version"
    unless ((version :: Int) == protocolVersion)
        (fail "unsupported protocol version")

rejectUnknownFields :: String -> [Key] -> Object -> Parser ()
rejectUnknownFields typeName allowed payload =
    case filter (`notElem` allowed) (KeyMap.keys payload) of
        [] -> pure ()
        unknown ->
            fail
                (typeName <> " contains unknown fields: "
                    <> show unknown)

guestCwd :: ResolvedTenant -> FilePath -> Either Text FilePath
guestCwd tenant cwd =
    let root = normalise tenant.resolvedTenantWorkspaceRoot
        relative = normalise (makeRelative root (normalise cwd))
    in if case splitDirectories relative of
        ".." : _ -> True
        _ -> False
        then Left "sandbox cwd is outside the tenant workspace"
        else
            Right $
                if relative == "."
                    then "/workspace"
                    else "/workspace" </> relative

translateArgumentsToGuest :: ResolvedTenant -> ToolCall -> Text
translateArgumentsToGuest tenant call =
    case call.callKind of
        FunctionCallKind -> translateJson
        ComputerFunctionCallKind -> translateJson
        _ -> call.arguments
  where
    translateJson =
        case
            eitherDecodeStrict'
                (TextEncoding.encodeUtf8 call.arguments)
        of
            Left _ -> call.arguments
            Right value ->
                TextEncoding.decodeUtf8
                    (LazyByteString.toStrict
                        (encode (translateValue value)))

    translateValue = \case
        String value -> String (translatePath value)
        Array values -> Array (fmap translateValue values)
        Object fields -> Object (fmap translateValue fields)
        value -> value

    translatePath value
        | value == hostRoot = "/workspace"
        | Just relative <- Text.stripPrefix hostPrefix value =
            "/workspace/" <> relative
        | otherwise = value

    hostRoot =
        Text.pack
            (normalise tenant.resolvedTenantWorkspaceRoot)
    hostPrefix =
        Text.pack
            (addTrailingPathSeparator
                (normalise tenant.resolvedTenantWorkspaceRoot))

encodedTextLength :: Text -> Int
encodedTextLength = ByteString.length . TextEncoding.encodeUtf8

isSafeImageDataUrl :: Text -> Bool
isSafeImageDataUrl url =
    any (`Text.isPrefixOf` Text.toLower url)
        [ "data:image/png;base64,"
        , "data:image/jpeg;base64,"
        , "data:image/gif;base64,"
        , "data:image/webp;base64,"
        , "data:image/bmp;base64,"
        , "data:image/tiff;base64,"
        ]

prepareTenantDirectories :: ResolvedTenant -> IO (Either Text ())
prepareTenantDirectories tenant =
    tryAny
        (mapM_
            prepare
            [ tenant.resolvedTenantHome
            , tenant.resolvedTenantStateDirectory
            , tenant.resolvedTenantStateDirectory </> "tmp"
            ]) >>= \case
                Left exception ->
                    pure
                        (Left
                            ("could not prepare tenant sandbox state: "
                                <> Text.pack (displayException exception)))
                Right () -> pure (Right ())
  where
    prepare path = do
        createDirectoryIfMissing True path
        setFileMode path 0o700

sanitizedRunnerEnvironment
    :: ResolvedTenant
    -> IO [(String, String)]
sanitizedRunnerEnvironment tenant = do
    inherited <- getEnvironment
    let allowed =
            [ (name, value)
            | (name, value) <- inherited
            , name `elem`
                [ "LANG"
                , "LC_ALL"
                , "SSL_CERT_FILE"
                , "NIX_SSL_CERT_FILE"
                , "TZ"
                ]
            ]
    pure $
        [ ("HOME", tenant.resolvedTenantStateDirectory)
        , ("TMPDIR", tenant.resolvedTenantStateDirectory </> "tmp")
        ] <> allowed

callKindText :: ToolCallKind -> Text
callKindText = \case
    FunctionCallKind -> "function"
    CustomCallKind -> "custom"
    ComputerCallKind -> "computer"
    ComputerFunctionCallKind -> "computer_function"
