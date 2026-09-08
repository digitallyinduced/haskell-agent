module Agent.MCP.Client.Internal.Core where

import Agent.Json ( rawJsonBytes, rawJsonDecoder, RawJson )
import Agent.MCP.Client.Internal.Operations
    ( discoverMcpTools, discoverMcpSkills )
import Agent.MCP.Client.Internal.Runtime
    ( McpRequest(requestTimeoutMicros, requestMeta, requestEra),
      modernProtocolVersion,
      supportedLegacyVersions,
      preferredLegacyVersion,
      discoverProbeTimeoutSeconds,
      clientInfoValue,
      legacyClientCapabilities,
      clientRequest,
      requestMcpFull,
      emptyRequestRegistry,
      startSubscriptions,
      sendNotification,
      readerLoop,
      stderrLoop,
      capturedStderrText,
      closeMcpClient,
      handleNotification,
      closeOptionalHandles,
      mergedEnvironment,
      secondsToMicros,
      exceptionSummary,
      redactConfiguredValues )
import Agent.MCP.Types
    ( McpTool,
      McpIcon,
      McpClientLifecycle(ClientReady, ClientClosed, ClientPending,
                         ClientInitializing, ClientFailed),
      McpClient(..),
      McpClientTransport(..),
      McpToolServer(..),
      McpHttpTransport(McpHttpTransport),
      McpStdioTransport(stdioReader, McpStdioTransport, stdioInput,
                        stdioProcess, stdioGroupId, stdioWriteLock, stdioStderr,
                        stdioStderrReader),
      McpServerEvent,
      McpError(..),
      McpServerInfo(serverInfoCapabilities, McpServerInfo, serverInfoEra,
                    serverInfoProtocolVersion, serverInfoName, serverInfoVersion,
                    serverInfoTitle, serverInfoIcons, serverInfoInstructions),
      McpServerCapabilities(capabilityLogging),
      McpProtocolEra(..),
      McpServerStatus(..),
      McpInitState(McpFailed, McpClosed, McpPending, McpInitializing,
                   McpReady),
      McpHostHooks(mcpHostElicit, mcpHostRoots, mcpHostSample),
      McpServerConfig(mcpServerEnv, mcpServerCwd, mcpServerCommand,
                      mcpServerArgs, mcpServerStartupTimeoutSeconds, mcpServerProtocol,
                      mcpServerName, mcpServerUrl, mcpServerRootsEnabled,
                      mcpServerSamplingEnabled, mcpServerLogLevel),
      McpProtocolPreference(McpProtocolModern, McpProtocolLegacy,
                            McpProtocolAuto),
      defaultMcpHostHooks,
      emptyServerCapabilities,
      serverCapabilitiesDecoder,
      renderMcpError,
      mcpLogLevelText,
      mcpIconDecoder,
      errorCodeHeaderMismatch,
      errorCodeMissingClientCapability,
      errorCodeUnsupportedProtocolVersion,
      emptyCapturedStderr,
      projectRawOr )
import Agent.ToolDispatch ()
import Agent.Tools.IO ( terminateProcessGroup )
import Agent.Tools.Types ()
import Control.Concurrent ()
import Control.Concurrent.Async ( asyncWithUnmask, cancel )
import Control.Concurrent.MVar ( newMVar )
import Control.Concurrent.STM
    ( atomically,
      STM,
      newTVarIO,
      readTVar,
      readTVarIO,
      writeTVar,
      newEmptyTMVar,
      readTMVar,
      tryPutTMVar,
      TMVar )
import Control.Exception.Safe
    ( bracketOnError, finally, onException, tryAny, MonadMask(mask) )
import Control.Monad ( void )
import Control.Monad.Trans.Class ()
import Control.Monad.Trans.Except ()
import Data.Aeson ( KeyValue((.=)) )
import Data.Char ()
import Data.IORef ( newIORef, readIORef, writeIORef )
import Data.List ( find )
import Data.Maybe ( fromMaybe, isJust )
import Data.Scientific ()
import Data.String ()
import Data.Text ( Text )
import Data.Text.Encoding.Error ()
import Data.Time.Clock.POSIX ()
import Data.Word ()
import GHC.Clock ()
import Network.HTTP.Client ()
import Network.HTTP.Client.TLS ()
import Network.HTTP.Types ()
import System.Environment ()
import System.IO
    ( hClose,
      hSetBinaryMode,
      hSetBuffering,
      BufferMode(LineBuffering) )
import System.IO.Unsafe ()
import System.Process
    ( createProcess,
      getPid,
      proc,
      CreateProcess(create_group, cwd, env, std_in, std_out, std_err),
      StdStream(CreatePipe) )
import System.Timeout ()
import qualified Data.Aeson as Aeson ()
import qualified Data.Aeson.Encoding as AesonEncoding ()
import qualified Data.Aeson.Encoding.Internal as AesonEncodingInternal
    ()
import qualified Data.ByteString as BS ()
import qualified Data.ByteString.Char8 as BS8 ()
import qualified Data.ByteString.Base64 as Base64 ()
import qualified Network.HTTP.Client as HC ()
import qualified Data.IntMap.Strict as IntMap ()
import qualified Agent.Json.Decode as Json
    ( Decoder,
      decodeEither,
      defaultKey,
      optionalKey,
      list,
      object,
      text,
      JsonError(jsonErrorMessage) )
import qualified Data.Aeson.Key as Key ()
import qualified Data.Aeson.KeyMap as KeyMap ()
import qualified Data.ByteString.Lazy as LBS ()
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Agent.MCP.OAuth as OAuth ()
import qualified Data.Text as Text
    ( pack, unpack, intercalate, null )
import qualified Data.Text.Encoding as TextEncoding ()

startMcpClient :: McpServerConfig -> IO McpClient
startMcpClient = startMcpClientWith defaultMcpHostHooks Nothing

-- | Connect directly to a typed server. There are no sockets or worker threads.
startInMemoryMcpClient :: McpHostHooks -> McpServerConfig -> McpToolServer -> IO McpClient
startInMemoryMcpClient hooks config server = mask \_ -> do
    unsubscribe <- newIORef (pure ())
    client <- newClientRecord hooks Nothing config (McpClientInMemory server unsubscribe)
    release <- server.toolServerSubscribe
        (handleNotification client "notifications/tools/list_changed" Nothing)
    writeIORef unsubscribe release
    pure client

startMcpClientWith
    :: McpHostHooks
    -> Maybe McpProtocolEra
    -> McpServerConfig
    -> IO McpClient
startMcpClientWith hooks eraHint config = case config.mcpServerUrl of
    Just _ -> startMcpHttpClient hooks eraHint config
    Nothing -> mask \_ -> do
        processEnvironment <- mergedEnvironment config.mcpServerEnv
        let processSpec =
                (proc config.mcpServerCommand config.mcpServerArgs)
                    { cwd = config.mcpServerCwd
                    , env = Just processEnvironment
                    , std_in = CreatePipe
                    , std_out = CreatePipe
                    , std_err = CreatePipe
                    , create_group = True
                    }
        -- Keep startup masked through the handoff to the long-lived client.
        -- Every intermediate owner has rollback before the next setup step.
        bracketOnError (createProcess processSpec) rollbackProcess \case
            (Just input, Just output, Just errOutput, processHandle) -> do
                groupId <- getPid processHandle
                hSetBinaryMode input True
                hSetBinaryMode output True
                hSetBinaryMode errOutput True
                hSetBuffering input LineBuffering
                writeLock <- newMVar ()
                stderrRef <- newIORef emptyCapturedStderr
                readerRef <- newIORef Nothing
                stderrReaderRef <- newIORef Nothing
                let transport = McpStdioTransport
                        { stdioInput = input
                        , stdioProcess = processHandle
                        , stdioGroupId = groupId
                        , stdioWriteLock = writeLock
                        , stdioStderr = stderrRef
                        , stdioReader = readerRef
                        , stdioStderrReader = stderrReaderRef
                        }
                client <-
                    newClientRecord hooks eraHint config
                        (McpClientStdio transport)
                bracketOnError
                    (asyncWithUnmask \unmask ->
                        unmask (stderrLoop errOutput transport.stdioStderr)
                            `finally` void (tryAny (hClose errOutput)))
                    cancel
                    \stderrReader -> do
                        writeIORef transport.stdioStderrReader (Just stderrReader)
                        bracketOnError
                            (asyncWithUnmask \unmask ->
                                unmask (readerLoop client output)
                                    `finally` void (tryAny (hClose output)))
                            cancel
                            \reader -> do
                                writeIORef transport.stdioReader (Just reader)
                                pure client
            _ ->
                ioError (userError "MCP server did not provide all stdio pipes")
  where
    rollbackProcess created@(_, _, _, processHandle) =
        (getPid processHandle >>= \groupId ->
            terminateProcessGroup groupId processHandle)
            `finally` closeOptionalHandles created

startMcpHttpClient
    :: McpHostHooks
    -> Maybe McpProtocolEra
    -> McpServerConfig
    -> IO McpClient
startMcpHttpClient hooks eraHint config =
    case config.mcpServerUrl of
        Nothing ->
            ioError (userError "MCP HTTP client requires a server URL")
        Just url -> do
            -- HTTP has no subprocess or background reader. Its lifecycle is
            -- driven by requestMcp and closeMcpClient below.
            session <- newIORef Nothing
            newClientRecord hooks eraHint config
                (McpClientHttp (McpHttpTransport url session))

newClientRecord
    :: McpHostHooks
    -> Maybe McpProtocolEra
    -> McpServerConfig
    -> McpClientTransport
    -> IO McpClient
newClientRecord hooks eraHint config transport = do
    requestRegistry <- newTVarIO emptyRequestRegistry
    failure <- newTVarIO Nothing
    closed <- newMVar False
    lifecycle <- newTVarIO ClientPending
    serverInfo <- newTVarIO Nothing
    discoveredSkills <- newTVarIO []
    toolsRevision <- newTVarIO 0
    readyToolsRevision <- newTVarIO Nothing
    resourceSubscriptionsRequested <- newTVarIO Set.empty
    resourceSubscriptionsAccepted <- newTVarIO Set.empty
    taskStatuses <- newTVarIO Map.empty
    subscriptionWorker <- newMVar Nothing
    workers <- newTVarIO []
    eventHandler <- newIORef (const (pure ()))
    pure McpClient
        { clientConfig = config
        , clientHooks = hooks
        , clientTransport = transport
        , clientRequestRegistry = requestRegistry
        , clientFailure = failure
        , clientWorkers = workers
        , clientClosed = closed
        , clientLifecycle = lifecycle
        , clientServerInfo = serverInfo
        , clientDiscoveredSkills = discoveredSkills
        , clientToolsRevision = toolsRevision
        , clientReadyToolsRevision = readyToolsRevision
        , clientResourceSubscriptionsRequested = resourceSubscriptionsRequested
        , clientResourceSubscriptionsAccepted = resourceSubscriptionsAccepted
        , clientTaskStatuses = taskStatuses
        , clientSubscriptionWorker = subscriptionWorker
        , clientEventHandler = eventHandler
        , clientEraHint = eraHint
        }

-- | Replace the handler invoked for server-initiated notifications. The
-- handler runs on the transport reader thread and must not block.
setMcpClientEventHandler :: McpClient -> (McpServerEvent -> IO ()) -> IO ()
setMcpClientEventHandler client = writeIORef client.clientEventHandler

-- | Era negotiated with the server, if initialization has completed.

-- * Initialization

data InitializeRole
    = InitializeLeader
        !(TMVar (Either Text ([McpTool], [Text])))
    | InitializeWaiter
        !(TMVar (Either Text ([McpTool], [Text])))
    | InitializeComplete
        !(Either Text ([McpTool], [Text]))

-- | Initialize and discover one client exactly once. Concurrent callers wait
-- on the same result. If the leader is cancelled, waiters are released and
-- the partially initialized stdio client becomes terminally failed.
ensureMcpClientReady
    :: McpClient
    -> IO (Either Text ([McpTool], [Text]))
ensureMcpClientReady = ensureMcpClientReadyWith (const (pure ()))

ensureMcpClientReadyWith
    :: ([McpTool] -> STM ())
    -> McpClient
    -> IO (Either Text ([McpTool], [Text]))
ensureMcpClientReadyWith publishReady client = mask \restore -> do
    role <- atomically do
        readTVar client.clientLifecycle >>= \case
            ClientPending -> do
                completion <- newEmptyTMVar
                writeTVar client.clientLifecycle
                    (ClientInitializing completion)
                pure (InitializeLeader completion)
            ClientInitializing completion ->
                pure (InitializeWaiter completion)
            ClientReady tools warnings ->
                pure (InitializeComplete (Right (tools, warnings)))
            ClientFailed err ->
                pure (InitializeComplete (Left err))
            ClientClosed ->
                pure (InitializeComplete (Left "MCP server closed"))
    case role of
        InitializeComplete result -> pure result
        InitializeWaiter completion ->
            restore (atomically (readTMVar completion))
        InitializeLeader completion -> do
            let cancelled = do
                    atomically do
                        state <- readTVar client.clientLifecycle
                        case state of
                            ClientInitializing current
                                | current == completion ->
                                    writeTVar client.clientLifecycle
                                        (ClientFailed
                                            "MCP initialization cancelled")
                            _ -> pure ()
                        void $
                            tryPutTMVar completion
                                (Left "MCP initialization cancelled")
                    closeMcpClient client
                initialize = do
                    negotiateProtocol client
                    loggingWarnings <- configureLegacyLogging client
                    skillWarnings <- discoverMcpSkills client
                    startSubscriptions client
                    discoverStableTools
                        (skillWarnings <> loggingWarnings)
                        0
                discoverStableTools skillWarnings attempt = do
                    revision <- readTVarIO client.clientToolsRevision
                    (tools, warnings) <- discoverMcpTools client
                    published <- atomically do
                        state <- readTVar client.clientLifecycle
                        currentRevision <- readTVar client.clientToolsRevision
                        case state of
                            ClientInitializing current
                                | current == completion
                                , currentRevision == revision -> do
                                    let ready =
                                            (tools, warnings <> skillWarnings)
                                    publishReady tools
                                    writeTVar
                                        client.clientReadyToolsRevision
                                        (Just revision)
                                    writeTVar client.clientLifecycle
                                        (uncurry ClientReady ready)
                                    void (tryPutTMVar completion (Right ready))
                                    pure (Just (Right ready))
                                | current == completion ->
                                    pure Nothing
                            ClientClosed -> do
                                let closed = Left "MCP server closed"
                                void (tryPutTMVar completion closed)
                                pure (Just closed)
                            ClientFailed err -> do
                                let failed = Left err
                                void (tryPutTMVar completion failed)
                                pure (Just failed)
                            _ -> do
                                let changed =
                                        Left "MCP client lifecycle changed during initialization"
                                void (tryPutTMVar completion changed)
                                pure (Just changed)
                    case published of
                        Just result -> pure result
                        Nothing
                            | attempt < maximumInitializationRelists ->
                                discoverStableTools skillWarnings (attempt + 1)
                            | otherwise -> do
                                let message =
                                        "MCP tools changed repeatedly during initialization"
                                    changed = Left message
                                atomically do
                                    state <- readTVar client.clientLifecycle
                                    case state of
                                        ClientInitializing current
                                            | current == completion ->
                                                writeTVar
                                                    client.clientLifecycle
                                                    (ClientFailed message)
                                        _ -> pure ()
                                    void (tryPutTMVar completion changed)
                                pure changed
            outcome <-
                restore (tryAny initialize)
                    `onException` cancelled
            case outcome of
                Right result -> pure result
                Left exception -> do
                    let err =
                            redactConfiguredValues client.clientConfig
                                (exceptionSummary exception)
                        result = Left err
                    atomically do
                        state <- readTVar client.clientLifecycle
                        case state of
                            ClientInitializing current
                                | current == completion ->
                                    writeTVar client.clientLifecycle
                                        (ClientFailed err)
                            _ -> pure ()
                        void (tryPutTMVar completion result)
                    pure result

maximumInitializationRelists :: Int
maximumInitializationRelists = 8

mcpClientStatus :: McpClient -> IO McpServerStatus
mcpClientStatus client = do
    state <- readTVarIO client.clientLifecycle
    transportFailure <- readTVarIO client.clientFailure
    pure McpServerStatus
        { mcpStatusName = client.clientConfig.mcpServerName
        , mcpStatusState = case (state, transportFailure) of
            (ClientClosed, _) -> McpClosed
            (_, Just err) -> McpFailed err
            (ClientPending, _) -> McpPending
            (ClientInitializing _, _) -> McpInitializing
            (ClientReady _ _, _) -> McpReady
            (ClientFailed err, _) -> McpFailed err
        , mcpStatusToolCount = case state of
            ClientReady tools _ -> length tools
            _ -> 0
        }

-- | Decide which protocol era the server speaks and complete the handshake
-- that era requires.
negotiateProtocol :: McpClient -> IO ()
negotiateProtocol client | McpClientInMemory server _ <- client.clientTransport = do
    info <- server.toolServerInitialize
    atomically $ writeTVar client.clientServerInfo (Just info)
negotiateProtocol client =
    case (client.clientConfig.mcpServerProtocol, client.clientEraHint) of
        (McpProtocolLegacy, _) -> legacyInitialize client preferredLegacyVersion
        (McpProtocolAuto, Just McpEraLegacy) ->
            legacyInitialize client preferredLegacyVersion
        (preference, _) -> do
            outcome <- probeDiscover client
            case classifyProbe outcome of
                ProbeModern raw -> applyDiscoverResult client raw
                ProbeVersions supported -> selectFromVersions client supported
                ProbeLegacy reason
                    | preference == McpProtocolModern ->
                        startupFailure client
                            ("server did not answer server/discover as a modern MCP server ("
                                <> reason
                                <> "); set \"protocol\": \"legacy\" to use the initialize handshake")
                    | otherwise -> legacyInitialize client preferredLegacyVersion
                ProbeFailure err -> startupFailure client err

probeDiscover :: McpClient -> IO (Either McpError RawJson)
probeDiscover client =
    requestMcpFull client
        (clientRequest client "server/discover" mempty)
            { requestEra = Just McpEraModern
            , requestTimeoutMicros =
                secondsToMicros
                    (min discoverProbeTimeoutSeconds
                        client.clientConfig.mcpServerStartupTimeoutSeconds)
            }

data ProbeOutcome
    = ProbeModern !RawJson
    | ProbeVersions ![Text]
    | ProbeLegacy !Text
    | ProbeFailure !Text
    deriving (Eq, Show)

-- | Interpret the outcome of a @server/discover@ probe. A recognized modern
-- error identifies a modern server; anything else identifies a legacy one
-- (legacy servers answer unknown pre-@initialize@ requests with
-- implementation-defined errors, or not at all).
classifyProbe :: Either McpError RawJson -> ProbeOutcome
classifyProbe = \case
    Right raw -> ProbeModern raw
    Left (McpRpcError code message payload)
        | code == errorCodeUnsupportedProtocolVersion ->
            ProbeVersions (maybe [] supportedVersionsOf payload)
        | code == errorCodeHeaderMismatch
            || code == errorCodeMissingClientCapability ->
            ProbeFailure (renderMcpError (McpRpcError code message payload))
        | otherwise ->
            ProbeLegacy ("error " <> Text.pack (show code))
    Left (McpTimeout _) -> ProbeLegacy "no response"
    Left (McpHttpStatus status _) -> ProbeLegacy ("HTTP " <> Text.pack (show status))
    Left (McpTransportError message) -> ProbeFailure message
  where
    supportedVersionsOf payload =
        projectRawOr []
            (Json.object (Json.defaultKey [] "supported" (Json.list Json.text)))
            payload

applyDiscoverResult :: McpClient -> RawJson -> IO ()
applyDiscoverResult client raw =
    case Json.decodeEither discoverDecoder (rawJsonBytes raw) of
        Left err ->
            startupFailure client
                ("invalid server/discover response: " <> err.jsonErrorMessage)
        Right (supported, capabilities, instructions, identity)
            | null supported || modernProtocolVersion `elem` supported -> do
                let (name, version, title, icons) = identity
                atomically $ writeTVar client.clientServerInfo $ Just McpServerInfo
                    { serverInfoEra = McpEraModern
                    , serverInfoProtocolVersion = modernProtocolVersion
                    , serverInfoName = name
                    , serverInfoVersion = version
                    , serverInfoTitle = title
                    , serverInfoIcons = icons
                    , serverInfoInstructions = instructions
                    , serverInfoCapabilities = capabilities
                    }
            | otherwise -> selectFromVersions client supported
  where
    discoverDecoder = Json.object do
        supported <- Json.defaultKey [] "supportedVersions" (Json.list Json.text)
        capabilities <-
            fromMaybe emptyServerCapabilities
                <$> Json.optionalKey "capabilities" serverCapabilitiesDecoder
        instructions <- Json.optionalKey "instructions" Json.text
        meta <- Json.optionalKey "_meta" rawJsonDecoder
        let identity =
                maybe (Nothing, Nothing, Nothing, [])
                    (projectRawOr (Nothing, Nothing, Nothing, []) metaServerInfoDecoder)
                    meta
        pure (supported, capabilities, instructions, identity)
    metaServerInfoDecoder = Json.object do
        info <-
            Json.optionalKey "io.modelcontextprotocol/serverInfo"
                implementationDecoder
        pure (fromMaybe (Nothing, Nothing, Nothing, []) info)

implementationDecoder :: Json.Decoder (Maybe Text, Maybe Text, Maybe Text, [McpIcon])
implementationDecoder = Json.object do
    name <- Json.optionalKey "name" Json.text
    version <- Json.optionalKey "version" Json.text
    title <- Json.optionalKey "title" Json.text
    icons <- Json.defaultKey [] "icons" (Json.list mcpIconDecoder)
    pure (name, version, title, icons)

-- | Pick a mutually supported version from a server's advertised list.
selectFromVersions :: McpClient -> [Text] -> IO ()
selectFromVersions client supported
    | modernProtocolVersion `elem` supported =
        -- The server advertises our modern version but rejected the probe;
        -- retry the probe once so a transient rejection does not strand us.
        probeDiscover client >>= \case
            Right raw -> applyDiscoverResult client raw
            Left err -> startupFailure client (renderMcpError err)
    | Just legacy <- find (`elem` supported) supportedLegacyVersions =
        legacyInitialize client legacy
    | otherwise =
        startupFailure client
            ("server supports protocol versions "
                <> Text.intercalate ", " supported
                <> "; this client supports "
                <> Text.intercalate ", " (modernProtocolVersion : supportedLegacyVersions))

-- | The @initialize@ handshake of protocol revisions up to @2025-11-25@.
legacyInitialize :: McpClient -> Text -> IO ()
legacyInitialize client requestedVersion = do
    elicitEnabled <- isJust <$> client.clientHooks.mcpHostElicit
    rootsHandler <- isJust <$> client.clientHooks.mcpHostRoots
    samplingHandler <- isJust <$> client.clientHooks.mcpHostSample
    let rootsEnabled =
            client.clientConfig.mcpServerRootsEnabled && rootsHandler
        samplingEnabled =
            client.clientConfig.mcpServerSamplingEnabled && samplingHandler
    let parameters =
            "protocolVersion" .= requestedVersion
                <> "capabilities" .= legacyClientCapabilities
                    elicitEnabled rootsEnabled samplingEnabled
                <> "clientInfo" .= clientInfoValue client.clientHooks
    result <-
        requestMcpFull client
            (clientRequest client "initialize" parameters)
                { requestEra = Nothing
                , requestMeta = False
                , requestTimeoutMicros =
                    secondsToMicros client.clientConfig.mcpServerStartupTimeoutSeconds
                }
    case result of
        Left err -> startupFailure client (renderMcpError err)
        Right response ->
            case Json.decodeEither initializeDecoder (rawJsonBytes response) of
                Left err ->
                    startupFailure client
                        ("invalid initialize response: " <> err.jsonErrorMessage)
                Right (version, capabilities, instructions, (name, serverVersion, title, icons))
                    | version `notElem` supportedLegacyVersions ->
                        startupFailure client
                            ("server negotiated unsupported protocol version " <> version)
                    | otherwise -> do
                        atomically $ writeTVar client.clientServerInfo $ Just McpServerInfo
                            { serverInfoEra = McpEraLegacy
                            , serverInfoProtocolVersion = version
                            , serverInfoName = name
                            , serverInfoVersion = serverVersion
                            , serverInfoTitle = title
                            , serverInfoIcons = icons
                            , serverInfoInstructions = instructions
                            , serverInfoCapabilities = capabilities
                            }
                        sendNotification client "notifications/initialized" mempty
                            >>= either (startupFailure client . renderMcpError) pure
  where
    initializeDecoder = Json.object do
        version <- Json.defaultKey "" "protocolVersion" Json.text
        capabilities <-
            fromMaybe emptyServerCapabilities
                <$> Json.optionalKey "capabilities" serverCapabilitiesDecoder
        instructions <- Json.optionalKey "instructions" Json.text
        identity <-
            fromMaybe (Nothing, Nothing, Nothing, [])
                <$> Json.optionalKey "serverInfo" implementationDecoder
        pure (version, capabilities, instructions, identity)

configureLegacyLogging :: McpClient -> IO [Text]
configureLegacyLogging client = do
    serverInfo <- readTVarIO client.clientServerInfo
    case (serverInfo, client.clientConfig.mcpServerLogLevel) of
        ( Just info
          , Just level
          )
            | info.serverInfoEra == McpEraLegacy
            , info.serverInfoCapabilities.capabilityLogging -> do
                requestMcpFull client
                    (clientRequest client "logging/setLevel"
                        ("level" .= mcpLogLevelText level))
                    >>= \case
                        Right _ -> pure []
                        Left err ->
                            pure
                                [ "MCP logging level request was rejected: "
                                    <> renderMcpError err
                                ]
        _ -> pure []

startupFailure :: McpClient -> Text -> IO a
startupFailure client err = do
    stderrText <- case client.clientTransport of
        McpClientStdio transport ->
            capturedStderrText <$> readIORef transport.stdioStderr
        McpClientHttp _ ->
            pure ""
        McpClientInMemory _ _ ->
            pure ""
    ioError . userError . Text.unpack $
        redactConfiguredValues client.clientConfig
            (err <> if Text.null stderrText then "" else "\nstderr:\n" <> stderrText)

-- * Discovery
