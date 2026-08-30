-- | One MCP client connection: transport, protocol negotiation, request
-- routing, and the request/response patterns of the specification (multi
-- round-trip input, tasks, subscriptions).
module Agent.MCP.Client where

import Agent.Json
    ( RawJson
    , rawJsonBytes
    , rawJsonDecoder
    , rawJsonEncoding
    , rawJsonFromEncoding
    )
import qualified Agent.Json.Decode as Json
import Agent.MCP.Types
import qualified Agent.MCP.OAuth as OAuth
import Agent.Tools.IO (terminateProcessGroup)
import Agent.Tools.Types
    ( AppTool(..)
    , ApprovalRule(..)
    , ToolExecutionPolicy(..)
    , ToolSchema(..)
    )
import Agent.ToolDispatch (typedStreamingTool)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
    ( Async
    , asyncWithUnmask
    , cancel
    , poll
    , waitCatch
    )
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar_
    , newMVar
    , readMVar
    , withMVar
    )
import Control.Concurrent.STM
    ( STM
    , TMVar
    , TVar
    , atomically
    , isEmptyTMVar
    , modifyTVar'
    , newEmptyTMVar
    , newEmptyTMVarIO
    , newTVarIO
    , readTMVar
    , readTVar
    , readTVarIO
    , takeTMVar
    , tryPutTMVar
    , tryReadTMVar
    , writeTVar
    )
import Control.Exception.Safe
    ( SomeException
    , displayException
    , finally
    , mask
    , mask_
    , onException
    , tryAny
    )
import Control.Monad (forM, forM_, unless, void, when)
import Data.Aeson
    ( Series
    , ToJSON(toJSON)
    , Value(..)
    , object
    , (.=)
    )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as AesonEncoding
import qualified Data.Aeson.Encoding.Internal as AesonEncodingInternal
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isAlphaNum, isAscii, ord)
import Data.IORef
    ( IORef
    , atomicModifyIORef'
    , newIORef
    , readIORef
    , writeIORef
    )
import qualified Data.IntMap.Strict as IntMap
import Data.List (find, foldl')
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, mapMaybe)
import Data.Scientific (floatingOrInteger)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Text.Encoding.Error (lenientDecode)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Network.HTTP.Client
    ( Manager
    , RequestBody(..)
    , brRead
    , parseRequest
    , responseBody
    , responseHeaders
    , responseStatus
    , withResponse
    )
import qualified Network.HTTP.Client as HC
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (Header, statusCode)
import System.Environment (getEnvironment)
import System.IO
    ( BufferMode(..)
    , Handle
    , hClose
    , hFlush
    , hSetBinaryMode
    , hSetBuffering
    )
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Types (ProcessGroupID)
import System.Process
    ( CreateProcess(..)
    , ProcessHandle
    , StdStream(..)
    , createProcess
    , getPid
    , proc
    )
import System.Timeout (timeout)

-- * Protocol constants

-- | The modern protocol revision this client implements.
modernProtocolVersion :: Text
modernProtocolVersion = "2026-07-28"

-- | Legacy revisions that this client can drive through the @initialize@
-- handshake, most preferred first.
supportedLegacyVersions :: [Text]
supportedLegacyVersions = ["2025-11-25", "2025-06-18", "2025-03-26", "2024-11-05"]

preferredLegacyVersion :: Text
preferredLegacyVersion = "2025-11-25"

-- | How long the @server/discover@ era probe waits before treating a silent
-- server as legacy.
discoverProbeTimeoutSeconds :: Int
discoverProbeTimeoutSeconds = 5

-- | Upper bound on input rounds for one multi round-trip request.
maxInputRounds :: Int
maxInputRounds = 8

-- | A request whose server keeps reporting progress may run this many times
-- longer than the configured request timeout.
hardTimeoutMultiplier :: Int
hardTimeoutMultiplier = 10

-- * Client construction

startMcpClient :: McpServerConfig -> IO McpClient
startMcpClient = startMcpClientWith defaultMcpHostHooks Nothing

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
        created <- createProcess processSpec
        case created of
            (Just input, Just output, Just errOutput, processHandle) -> do
                groupId <- getPid processHandle
                hSetBinaryMode input True
                hSetBinaryMode output True
                hSetBinaryMode errOutput True
                hSetBuffering input LineBuffering
                client <-
                    newClientRecord hooks eraHint config
                        (Just input) (Just processHandle) groupId
                stderrReader <- asyncWithUnmask \unmask ->
                    unmask (stderrLoop errOutput client.clientStderr)
                        `finally` void (tryAny (hClose errOutput))
                let withStderr = client { clientStderrReader = Just stderrReader }
                reader <- asyncWithUnmask \unmask ->
                    unmask (readerLoop withStderr output)
                        `finally` void (tryAny (hClose output))
                writeIORef withStderr.clientReader (Just reader)
                pure withStderr
            _ -> do
                let (_, _, _, processHandle) = created
                groupId <- getPid processHandle
                terminateProcessGroup groupId processHandle
                closeOptionalHandles created
                ioError (userError "MCP server did not provide all stdio pipes")

mcpHttpManager :: Manager
mcpHttpManager = unsafePerformIO newTlsManager
{-# NOINLINE mcpHttpManager #-}

startMcpHttpClient
    :: McpHostHooks
    -> Maybe McpProtocolEra
    -> McpServerConfig
    -> IO McpClient
startMcpHttpClient hooks eraHint config =
    -- HTTP has no subprocess or background reader.  Its lifecycle is driven
    -- by requestMcp and closeMcpClient below.
    newClientRecord hooks eraHint config Nothing Nothing Nothing

newClientRecord
    :: McpHostHooks
    -> Maybe McpProtocolEra
    -> McpServerConfig
    -> Maybe Handle
    -> Maybe ProcessHandle
    -> Maybe ProcessGroupID
    -> IO McpClient
newClientRecord hooks eraHint config input processHandle groupId = do
    nextId <- newIORef 1
    pending <- newTVarIO IntMap.empty
    failure <- newTVarIO Nothing
    writeLock <- newMVar ()
    stderrRef <- newIORef emptyCapturedStderr
    closed <- newMVar False
    lifecycle <- newTVarIO ClientPending
    serverInfo <- newTVarIO Nothing
    discoveredSkills <- newTVarIO []
    session <- newIORef Nothing
    reader <- newIORef Nothing
    workers <- newTVarIO []
    eventHandler <- newIORef (const (pure ()))
    pure McpClient
        { clientConfig = config
        , clientHooks = hooks
        , clientHttpSession = session
        , clientInput = input
        , clientProcess = processHandle
        , clientGroupId = groupId
        , clientNextId = nextId
        , clientPending = pending
        , clientFailure = failure
        , clientWriteLock = writeLock
        , clientStderr = stderrRef
        , clientReader = reader
        , clientStderrReader = Nothing
        , clientWorkers = workers
        , clientClosed = closed
        , clientLifecycle = lifecycle
        , clientServerInfo = serverInfo
        , clientDiscoveredSkills = discoveredSkills
        , clientEventHandler = eventHandler
        , clientEraHint = eraHint
        }

-- | Replace the handler invoked for server-initiated notifications. The
-- handler runs on the transport reader thread and must not block.
setMcpClientEventHandler :: McpClient -> (McpServerEvent -> IO ()) -> IO ()
setMcpClientEventHandler client = writeIORef client.clientEventHandler

-- | Era negotiated with the server, if initialization has completed.
mcpClientEra :: McpClient -> IO (Maybe McpProtocolEra)
mcpClientEra client = fmap (.serverInfoEra) <$> readTVarIO client.clientServerInfo

mcpClientServerInfo :: McpClient -> IO (Maybe McpServerInfo)
mcpClientServerInfo client = readTVarIO client.clientServerInfo

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
                    (tools, warnings) <- discoverMcpTools client
                    skillWarnings <- discoverMcpSkills client
                    startSubscriptions client
                    pure (tools, warnings <> skillWarnings)
            outcome <-
                restore (tryAny initialize)
                    `onException` cancelled
            let result = case outcome of
                    Left exception ->
                        Left
                            (redactConfiguredValues client.clientConfig
                                (exceptionSummary exception))
                    Right ready -> Right ready
            atomically do
                state <- readTVar client.clientLifecycle
                case state of
                    ClientClosed ->
                        void $
                            tryPutTMVar completion
                                (Left "MCP server closed")
                    ClientInitializing current
                        | current == completion -> do
                            case result of
                                Left err ->
                                    writeTVar client.clientLifecycle
                                        (ClientFailed err)
                                Right (tools, warnings) -> do
                                    publishReady tools
                                    writeTVar client.clientLifecycle
                                        (ClientReady tools warnings)
                            void (tryPutTMVar completion result)
                    _ -> void (tryPutTMVar completion result)
            pure result

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
                let (name, version, title) = identity
                atomically $ writeTVar client.clientServerInfo $ Just McpServerInfo
                    { serverInfoEra = McpEraModern
                    , serverInfoProtocolVersion = modernProtocolVersion
                    , serverInfoName = name
                    , serverInfoVersion = version
                    , serverInfoTitle = title
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
                maybe (Nothing, Nothing, Nothing)
                    (projectRawOr (Nothing, Nothing, Nothing) metaServerInfoDecoder)
                    meta
        pure (supported, capabilities, instructions, identity)
    metaServerInfoDecoder = Json.object do
        info <-
            Json.optionalKey "io.modelcontextprotocol/serverInfo"
                implementationDecoder
        pure (fromMaybe (Nothing, Nothing, Nothing) info)

implementationDecoder :: Json.Decoder (Maybe Text, Maybe Text, Maybe Text)
implementationDecoder = Json.object do
    name <- Json.optionalKey "name" Json.text
    version <- Json.optionalKey "version" Json.text
    title <- Json.optionalKey "title" Json.text
    pure (name, version, title)

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
    let parameters =
            "protocolVersion" .= requestedVersion
                <> "capabilities" .= legacyClientCapabilities elicitEnabled
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
                Right (version, capabilities, instructions, (name, serverVersion, title))
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
            fromMaybe (Nothing, Nothing, Nothing)
                <$> Json.optionalKey "serverInfo" implementationDecoder
        pure (version, capabilities, instructions, identity)

clientInfoValue :: McpHostHooks -> Value
clientInfoValue hooks = object
    [ "name" .= hooks.mcpHostClientName
    , "version" .= hooks.mcpHostClientVersion
    ]

-- | Capabilities declared to modern servers on every request.
clientCapabilitiesValue :: Bool -> Value
clientCapabilitiesValue elicitEnabled = object $
    [ "elicitation" .= object ["form" .= object [], "url" .= object []]
    | elicitEnabled
    ]
    <> [ "extensions" .= object
            [ "io.modelcontextprotocol/tasks" .= object []
            , "io.modelcontextprotocol/skills" .= object []
            ]
       ]

-- | Capabilities declared to legacy servers during @initialize@.
legacyClientCapabilities :: Bool -> Value
legacyClientCapabilities elicitEnabled = object $
    [ "elicitation" .= object ["form" .= object [], "url" .= object []]
    | elicitEnabled
    ]

startupFailure :: McpClient -> Text -> IO a
startupFailure client err = do
    stderrText <- capturedStderrText <$> readIORef client.clientStderr
    ioError . userError . Text.unpack $
        redactConfiguredValues client.clientConfig
            (err <> if Text.null stderrText then "" else "\nstderr:\n" <> stderrText)

-- * Discovery

discoverMcpTools :: McpClient -> IO ([McpTool], [Text])
discoverMcpTools client = do
    tools <- paginate client "tools/list" "tools" mcpToolDecoder
        >>= either (ioError . userError . Text.unpack . renderMcpError) pure
    let isHttp = isJust client.clientConfig.mcpServerUrl
        annotated =
            [ (tool.discoveredName, annotateHeaderParams isHttp tool)
            | tool <- tools
            ]
        accepted = [tool | (_, Right tool) <- annotated]
        warnings =
            [ "MCP server " <> client.clientConfig.mcpServerName
                <> " tool " <> name <> " was rejected: " <> reason
            | (name, Left reason) <- annotated
            ]
    pure (accepted, warnings)

-- | Fetch every page of a list request.
paginate
    :: McpClient
    -> Text
    -> Text
    -> Json.Decoder a
    -> IO (Either McpError [a])
paginate client method key itemDecoder = go Nothing []
  where
    go cursor collected = do
        let parameters = maybe mempty
                (\value -> AesonEncoding.pair "cursor" (rawJsonEncoding value))
                cursor
        requestMcpFull client (clientRequest client method parameters) >>= \case
            Left err -> pure (Left err)
            Right result ->
                case Json.decodeEither pageDecoder (rawJsonBytes result) of
                    Left err ->
                        pure . Left . McpTransportError $
                            "invalid " <> method <> " response: " <> err.jsonErrorMessage
                    Right (items, nextCursor) ->
                        case nextCursor of
                            Just next -> go (Just next) (collected <> items)
                            Nothing -> pure (Right (collected <> items))

    pageDecoder = Json.object $
        (,)
            <$> Json.defaultKey [] key (Json.list itemDecoder)
            <*> Json.optionalKey "nextCursor" rawJsonDecoder

-- | Enumerate skill metadata only.  Skill resources are intentionally not
-- fetched here; hosts retrieve and verify SKILL.md when the user activates a
-- skill.
discoverMcpSkills :: McpClient -> IO [Text]
discoverMcpSkills client = do
    capability <- clientSkillsCapability client
    case capability of
        Nothing -> pure []
        Just _ -> go Nothing []
  where
    go cursor warnings = do
        let parameters = maybe mempty
                (\value -> AesonEncoding.pair "cursor" (rawJsonEncoding value))
                cursor
        requestMcpFull client (clientRequest client "skills/list" parameters) >>= \case
            Left err -> pure ["MCP server " <> client.clientConfig.mcpServerName
                <> " skills/list failed: " <> renderMcpError err]
            Right result ->
                case Json.decodeEither pageDecoder (rawJsonBytes result) of
                    Left err -> pure ["MCP server "
                        <> client.clientConfig.mcpServerName
                        <> " returned invalid skills/list response: " <> err.jsonErrorMessage]
                    Right (rawSkills, nextCursor) -> do
                        let skills = mapMaybe decodeSkill rawSkills
                            invalid = length skills /= length rawSkills
                        atomically $ modifyTVar' client.clientDiscoveredSkills
                            (<> skills)
                        let pageWarnings =
                                [ "MCP server " <> client.clientConfig.mcpServerName
                                    <> " returned invalid skills/list entry"
                                | invalid
                                ]
                        case nextCursor of
                            Nothing -> pure (warnings <> pageWarnings)
                            Just next -> go (Just next) (warnings <> pageWarnings)

    pageDecoder = Json.object $
        (,) <$> Json.defaultKey [] "skills" (Json.list rawJsonDecoder)
            <*> Json.optionalKey "nextCursor" rawJsonDecoder
    decodeSkill value =
        case Json.decodeEither mcpSkillEntryDecoder (rawJsonBytes value) of
            Left _ -> Nothing
            Right skill -> Just skill

clientSkillsCapability :: McpClient -> IO (Maybe McpSkillsCapability)
clientSkillsCapability client =
    (>>= (.serverInfoCapabilities.capabilitySkills))
        <$> readTVarIO client.clientServerInfo

-- | Retrieve one skill manifest by URI.  Unlike 'skills/list', this also
-- supports servers whose catalog is not enumerable.
getMcpSkill :: McpClient -> Text -> IO (Either Text McpSkillEntry)
getMcpSkill client uri = do
    capability <- clientSkillsCapability client
    case capability of
        Nothing -> pure (Left ("MCP server "
            <> client.clientConfig.mcpServerName
            <> " does not support io.modelcontextprotocol/skills"))
        Just _ ->
            requestMcpFull client (clientRequest client "skills/get" ("uri" .= uri)) >>= \case
                Left err -> pure (Left (renderMcpError err))
                Right result ->
                    case Json.decodeEither
                            (Json.object (Json.atKey "skill" mcpSkillEntryDecoder))
                            (rawJsonBytes result) of
                        Left err -> pure (Left ("invalid skills/get response: "
                            <> err.jsonErrorMessage))
                        Right skill -> pure (Right skill)

-- | Read one or more resource contents using the standard MCP resources/read
-- method.  This does not activate a skill; callers must perform their own
-- approval, frontmatter, and manifest verification.
readMcpResource :: McpClient -> Text -> IO (Either Text [McpResourceContent])
readMcpResource client uri =
    invokeWithInputRounds client
        (clientRequest client "resources/read" ("uri" .= uri))
            { requestName = Just uri }
        >>= \case
        Left err -> pure (Left (renderMcpError err))
        Right result ->
            case Json.decodeEither
                    (Json.object
                        (Json.defaultKey [] "contents"
                            (Json.list mcpResourceContentDecoder)))
                    (rawJsonBytes result) of
                Left err -> pure (Left ("invalid resources/read response: "
                    <> err.jsonErrorMessage))
                Right contents -> pure (Right contents)

listMcpResources :: McpClient -> IO (Either McpError [McpResource])
listMcpResources client =
    paginate client "resources/list" "resources" mcpResourceDecoder

listMcpResourceTemplates :: McpClient -> IO (Either McpError [McpResourceTemplate])
listMcpResourceTemplates client =
    paginate client "resources/templates/list" "resourceTemplates"
        mcpResourceTemplateDecoder

listMcpPrompts :: McpClient -> IO (Either McpError [McpPrompt])
listMcpPrompts client =
    paginate client "prompts/list" "prompts" mcpPromptDecoder

-- | Resolve a prompt template. Servers may ask for additional input first.
getMcpPrompt
    :: McpClient
    -> Text
    -> [(Text, Text)]
    -> IO (Either McpError McpPromptResult)
getMcpPrompt client name arguments =
    invokeWithInputRounds client
        (clientRequest client "prompts/get"
            ("name" .= name
                <> "arguments" .= object [Key.fromText key .= value | (key, value) <- arguments]))
            { requestName = Just name }
        >>= \case
        Left err -> pure (Left err)
        Right result ->
            pure . either
                (\err -> Left (McpTransportError ("invalid prompts/get response: " <> err.jsonErrorMessage)))
                Right $
                Json.decodeEither mcpPromptResultDecoder (rawJsonBytes result)

data McpCompletionRef
    = McpCompletePrompt !Text
    | McpCompleteResource !Text
    deriving (Eq, Show)

-- | Request argument completions for a prompt or resource template.
completeMcpArgument
    :: McpClient
    -> McpCompletionRef
    -> Text
    -> Text
    -> [(Text, Text)]
    -> IO (Either McpError McpCompletion)
completeMcpArgument client ref argumentName partial context = do
    let refValue = case ref of
            McpCompletePrompt name ->
                object ["type" .= ("ref/prompt" :: Text), "name" .= name]
            McpCompleteResource uri ->
                object ["type" .= ("ref/resource" :: Text), "uri" .= uri]
        parameters =
            "ref" .= refValue
                <> "argument" .= object ["name" .= argumentName, "value" .= partial]
                <> (if null context
                        then mempty
                        else "context" .= object
                            [ "arguments" .= object
                                [Key.fromText key .= value | (key, value) <- context]
                            ])
    requestMcpFull client (clientRequest client "completion/complete" parameters) >>= \case
        Left err -> pure (Left err)
        Right result ->
            pure . either
                (\err -> Left (McpTransportError ("invalid completion/complete response: " <> err.jsonErrorMessage)))
                Right $
                Json.decodeEither mcpCompletionDecoder (rawJsonBytes result)

-- * Tools

appToolFor :: McpClient -> McpTool -> AppTool
appToolFor client tool = AppTool
    { appToolName = qualifiedName
    , appToolDescription = describeTool tool
    -- Tool schemas enter the legacy Aeson-valued tool API here. Their wire
    -- decode and storage remain RawJson.
    , appToolSchema =
        RawJsonFunctionSchema (toJSON tool.discoveredInputSchema)
    , appToolHandler =
        typedStreamingTool qualifiedName rawObjectDecoder \publish arguments -> do
            -- Snapshots accumulate: each progress line is appended to the
            -- text already shown for this call.
            shown <- newIORef Text.empty
            callDiscoveredToolWith client tool arguments $ Just \progress -> do
                snapshot <- atomicModifyIORef' shown \current ->
                    let next = current <> formatProgress progress
                    in (next, next)
                publish snapshot
    , appToolApproval =
        if tool.discoveredReadOnly then AlwaysReadOnly else AlwaysPrompt
    , appToolExecution =
        if tool.discoveredReadOnly then ParallelSafe else TurnSequential
    , appToolResourceClaims = Nothing
    }
  where
    qualifiedName = qualifiedMcpToolName
        client.clientConfig.mcpServerName
        tool.discoveredName

-- | Description shown to the model: the server's description, with the
-- human-readable title as a prefix when the server provides one.
describeTool :: McpTool -> Text
describeTool tool =
    case tool.discoveredTitle of
        Just title
            | not (Text.null (Text.strip title))
            , Text.strip title /= tool.discoveredName ->
                Text.strip title
                    <> (if Text.null tool.discoveredDescription
                        then ""
                        else ": " <> tool.discoveredDescription)
        _ -> tool.discoveredDescription

formatProgress :: McpProgress -> Text
formatProgress progress =
    Text.intercalate " "
        (catMaybes
            [ Just ("progress " <> showNumber progress.progressValue
                <> maybe "" (\total -> "/" <> showNumber total) progress.progressTotal)
            , progress.progressMessage
            ])
        <> "\n"
  where
    showNumber value
        | value == fromIntegral (round value :: Integer) =
            Text.pack (show (round value :: Integer))
        | otherwise = Text.pack (show value)

rawObjectDecoder :: Json.Decoder RawJson
rawObjectDecoder =
    Json.getType >>= \case
        Json.VObject -> rawJsonDecoder
        _ -> fail "expected object"

qualifiedMcpToolName :: Text -> Text -> Text
qualifiedMcpToolName serverName toolName =
    escapeComponent serverName <> "__" <> escapeComponent toolName
  where
    escapeComponent =
        Text.replace "__" "%5F%5F"
            . Text.replace "%" "%25"

callDiscoveredTool :: McpClient -> McpTool -> RawJson -> IO (Either Text Text)
callDiscoveredTool client tool arguments =
    callDiscoveredToolWith client tool arguments Nothing

callDiscoveredToolWith
    :: McpClient
    -> McpTool
    -> RawJson
    -> Maybe (McpProgress -> IO ())
    -> IO (Either Text Text)
callDiscoveredToolWith client tool arguments onProgress = do
    let parameters =
            "name" .= tool.discoveredName
                <> AesonEncoding.pair "arguments" (rawJsonEncoding arguments)
    invokeWithInputRounds client
        (clientRequest client "tools/call" parameters)
            { requestName = Just tool.discoveredName
            , requestHeaderParams = headerParamValues tool arguments
            , requestOnProgress = onProgress
            }
        >>= \case
        Left err -> pure (Left (renderMcpError err))
        Right result -> pure (normalizeMcpToolResult result)

-- | Render a @CallToolResult@ for the model.
normalizeMcpToolResult :: RawJson -> Either Text Text
normalizeMcpToolResult result =
    case Json.decodeEither mcpToolResultDecoder (rawJsonBytes result) of
        Left _ -> Right (compactRawJson result)
        Right (isError, structured, blocks) ->
            let rendered = mapMaybe renderContentBlock blocks
                output
                    | isJust structured && not (null rendered) =
                        compactRawJson result
                    | Just value <- structured = compactRawJson value
                    | not (null rendered) = Text.intercalate "\n" rendered
                    | otherwise = compactRawJson result
            in if isError then Left output else Right output

-- | Flatten a resolved prompt into one user turn. Assistant-authored
-- messages are labelled so the model can tell the two roles apart.
renderMcpPromptResult :: McpPromptResult -> Text
renderMcpPromptResult result =
    Text.intercalate "\n\n" $
        filter (not . Text.null) $
            maybe [] (\description -> ["# " <> description]) result.promptResultDescription
                <> map renderMessage result.promptResultMessages
  where
    renderMessage :: McpPromptMessage -> Text
    renderMessage message =
        let blocks = projectRawOr [] blocksDecoder message.promptMessageContent
            body = Text.intercalate "\n" (mapMaybe renderContentBlock blocks)
        in if message.promptMessageRole == "assistant"
            then "[assistant]\n" <> body
            else body
    blocksDecoder =
        Json.getType >>= \case
            Json.VArray -> Json.list contentBlockDecoder
            _ -> pure <$> contentBlockDecoder

data McpContentBlock
    = McpTextBlock !Text
    | McpImageBlock !(Maybe Text) !Int
    | McpAudioBlock !(Maybe Text) !Int
    | McpResourceLinkBlock !Text !(Maybe Text) !(Maybe Text) !(Maybe Text)
    | McpEmbeddedResourceBlock !McpResourceContent
    | McpUnknownBlock !Text
    deriving (Eq, Show)

renderContentBlock :: McpContentBlock -> Maybe Text
renderContentBlock = \case
    McpTextBlock text -> Just text
    McpImageBlock mimeType size ->
        Just ("[image " <> fromMaybe "image" mimeType <> ", "
            <> Text.pack (show size) <> " base64 bytes; binary content is not shown]")
    McpAudioBlock mimeType size ->
        Just ("[audio " <> fromMaybe "audio" mimeType <> ", "
            <> Text.pack (show size) <> " base64 bytes; binary content is not shown]")
    McpResourceLinkBlock uri name description mimeType ->
        Just ("[resource_link] " <> uri
            <> maybe "" (\value -> " (" <> value <> ")") name
            <> maybe "" (\value -> " [" <> value <> "]") mimeType
            <> maybe "" (": " <>) description)
    McpEmbeddedResourceBlock content ->
        Just $ case (content.mcpResourceText, content.mcpResourceBlob) of
            (Just text, _) ->
                "[resource " <> content.mcpResourceUri
                    <> maybe "" (\value -> " (" <> value <> ")") content.mcpResourceMimeType
                    <> "]\n" <> text
            (Nothing, Just blob) ->
                "[resource " <> content.mcpResourceUri
                    <> maybe "" (\value -> " (" <> value <> ")") content.mcpResourceMimeType
                    <> ": " <> Text.pack (show (Text.length blob))
                    <> " base64 bytes; binary content is not shown]"
            (Nothing, Nothing) -> "[resource " <> content.mcpResourceUri <> "]"
    McpUnknownBlock _ -> Nothing

mcpToolResultDecoder
    :: Json.Decoder (Bool, Maybe RawJson, [McpContentBlock])
mcpToolResultDecoder = Json.object do
    rawError <- Json.optionalKey "isError" rawJsonDecoder
    structured <- Json.optionalKey "structuredContent" rawJsonDecoder
    rawContent <- Json.optionalKey "content" rawJsonDecoder
    let isError = maybe False (projectRawOr False Json.bool) rawError
        blocks = maybe [] (projectRawOr [] (Json.list contentBlockDecoder)) rawContent
    pure (isError, structured, blocks)

contentBlockDecoder :: Json.Decoder McpContentBlock
contentBlockDecoder = Json.object do
    contentType <- Json.defaultKey "" "type" Json.text
    case contentType of
        "text" -> McpTextBlock <$> Json.defaultKey "" "text" Json.text
        "image" ->
            McpImageBlock
                <$> Json.optionalKey "mimeType" Json.text
                <*> (maybe 0 Text.length <$> Json.optionalKey "data" Json.text)
        "audio" ->
            McpAudioBlock
                <$> Json.optionalKey "mimeType" Json.text
                <*> (maybe 0 Text.length <$> Json.optionalKey "data" Json.text)
        "resource_link" ->
            McpResourceLinkBlock
                <$> Json.defaultKey "" "uri" Json.text
                <*> Json.optionalKey "name" Json.text
                <*> Json.optionalKey "description" Json.text
                <*> Json.optionalKey "mimeType" Json.text
        "resource" ->
            McpEmbeddedResourceBlock <$> Json.atKey "resource" mcpResourceContentDecoder
        other -> pure (McpUnknownBlock other)

compactJson :: Value -> Text
compactJson = TextEncoding.decodeUtf8 . LBS.toStrict . Aeson.encode

compactRawJson :: RawJson -> Text
compactRawJson = TextEncoding.decodeUtf8 . rawJsonBytes

-- * Header mirroring (@x-mcp-header@)

-- | Resolve the @x-mcp-header@ annotations of a tool's input schema. Stdio
-- transports ignore the annotations entirely; HTTP transports reject tools
-- whose annotations violate the specification's constraints.
annotateHeaderParams :: Bool -> McpTool -> Either Text McpTool
annotateHeaderParams isHttp tool
    | not isHttp = Right tool { discoveredHeaderParams = [] }
    | otherwise =
        case Aeson.decodeStrict (rawJsonBytes tool.discoveredInputSchema) of
            Nothing -> Right tool { discoveredHeaderParams = [] }
            Just schema -> do
                params <- collectHeaderParams schema
                validateHeaderNames params
                pure tool { discoveredHeaderParams = params }

collectHeaderParams :: Value -> Either Text [McpHeaderParam]
collectHeaderParams root = case root of
    Object fields
        | KeyMap.member "x-mcp-header" fields ->
            Left "x-mcp-header must annotate a property, not the schema root"
        | otherwise -> walkObject [] fields
    _ -> Right []
  where
    walkObject path fields =
        concat <$> forM (KeyMap.toList fields) (walkField path)

    walkField path (key, value)
        | Key.toText key == "properties" = walkProperties path value
        | containsHeaderAnnotation value =
            Left ("x-mcp-header under \"" <> Key.toText key
                <> "\" is not statically reachable from the schema root")
        | otherwise = Right []

    walkProperties path = \case
        Object properties ->
            concat <$> forM (KeyMap.toList properties) \(name, property) ->
                walkProperty (path <> [Key.toText name]) property
        _ -> Right []

    walkProperty path property = case property of
        Object fields -> do
            here <- case KeyMap.lookup "x-mcp-header" fields of
                Nothing -> Right []
                Just (String name) -> do
                    case KeyMap.lookup "type" fields of
                        Just (String kind)
                            | kind `elem` ["string", "integer", "boolean"] -> Right ()
                        _ -> Left ("x-mcp-header on \"" <> Text.intercalate "." path
                            <> "\" requires a string, integer, or boolean type")
                    Right [McpHeaderParam path name]
                Just _ -> Left ("x-mcp-header on \"" <> Text.intercalate "." path
                    <> "\" must be a string")
            nested <- walkObject path (KeyMap.delete "x-mcp-header" fields)
            Right (here <> nested)
        _ -> Right []

containsHeaderAnnotation :: Value -> Bool
containsHeaderAnnotation = \case
    Object fields ->
        KeyMap.member "x-mcp-header" fields
            || any containsHeaderAnnotation (KeyMap.elems fields)
    Array values -> any containsHeaderAnnotation values
    _ -> False

validateHeaderNames :: [McpHeaderParam] -> Either Text ()
validateHeaderNames params = go [] params
  where
    go :: [Text] -> [McpHeaderParam] -> Either Text ()
    go _ [] = Right ()
    go seen (param : rest)
        | Text.null param.headerParamName = Left "x-mcp-header must not be empty"
        | not (Text.all isToken param.headerParamName) =
            Left ("x-mcp-header \"" <> param.headerParamName
                <> "\" is not a valid HTTP field name")
        | Text.toLower param.headerParamName `elem` seen =
            Left ("x-mcp-header \"" <> param.headerParamName
                <> "\" is not unique within the tool")
        | otherwise = go (Text.toLower param.headerParamName : seen) rest
    isToken character =
        isAscii character
            && (isAlphaNum character || character `elem` ("!#$%&'*+-.^_`|~" :: String))

-- | Compute the @Mcp-Param-{name}@ headers for one call.
headerParamValues :: McpTool -> RawJson -> [(Text, Text)]
headerParamValues tool arguments
    | null tool.discoveredHeaderParams = []
    | otherwise =
        case Aeson.decodeStrict (rawJsonBytes arguments) of
            Nothing -> []
            Just value -> mapMaybe (paramHeader value) tool.discoveredHeaderParams
  where
    paramHeader :: Value -> McpHeaderParam -> Maybe (Text, Text)
    paramHeader value param = do
        leaf <- lookupPath param.headerParamPath value
        rendered <- renderHeaderValue leaf
        pure ("Mcp-Param-" <> param.headerParamName, encodeHeaderValue rendered)
    lookupPath [] value = Just value
    lookupPath (key : rest) value = case value of
        Object fields -> KeyMap.lookup (Key.fromText key) fields >>= lookupPath rest
        _ -> Nothing
    renderHeaderValue = \case
        String text -> Just text
        Bool flag -> Just (if flag then "true" else "false")
        Number number -> case floatingOrInteger number of
            Right integer -> Just (Text.pack (show (integer :: Integer)))
            Left _ -> Nothing
        _ -> Nothing

-- | Encode a header value per the Streamable HTTP value-encoding rules: plain
-- ASCII passes through, anything else is carried as a Base64 sentinel.
encodeHeaderValue :: Text -> Text
encodeHeaderValue value
    | safe = value
    | otherwise =
        "=?base64?"
            <> TextEncoding.decodeUtf8 (Base64.encode (TextEncoding.encodeUtf8 value))
            <> "?="
  where
    safe =
        not (Text.null value)
            && Text.all visibleAscii value
            && not (isSpaceLike (Text.head value))
            && not (isSpaceLike (Text.last value))
            && not (looksLikeSentinel value)
    visibleAscii character =
        let code = ord character
        in (code >= 0x21 && code <= 0x7E) || code == 0x20 || code == 0x09
    isSpaceLike character = character == ' ' || character == '\t'
    looksLikeSentinel text =
        "=?base64?" `Text.isPrefixOf` text && "?=" `Text.isSuffixOf` text

-- * Requests

-- | One outbound JSON-RPC request.
data McpRequest = McpRequest
    { requestMethod :: !Text
    , requestParams :: !Series
    , requestName :: !(Maybe Text)
    -- ^ Value mirrored into the @Mcp-Name@ header on Streamable HTTP.
    , requestHeaderParams :: ![(Text, Text)]
    -- ^ Already encoded @Mcp-Param-*@ headers.
    , requestTimeoutMicros :: !Int
    -- ^ Idle timeout. Non-positive waits indefinitely (subscriptions).
    , requestEra :: !(Maybe McpProtocolEra)
    -- ^ Era override. 'Nothing' uses the negotiated era.
    , requestMeta :: !Bool
    -- ^ Whether to attach @_meta@ at all (@initialize@ carries none).
    , requestOnProgress :: !(Maybe (McpProgress -> IO ()))
    }

clientRequest :: McpClient -> Text -> Series -> McpRequest
clientRequest client method parameters = McpRequest
    { requestMethod = method
    , requestParams = parameters
    , requestName = Nothing
    , requestHeaderParams = []
    , requestTimeoutMicros =
        secondsToMicros client.clientConfig.mcpServerRequestTimeoutSeconds
    , requestEra = Nothing
    , requestMeta = True
    , requestOnProgress = Nothing
    }

-- | Compatibility entry point: one request, rendered error.
requestMcp
    :: McpClient
    -> Int
    -> Text
    -> Series
    -> IO (Either Text RawJson)
requestMcp client timeoutMicros method parameters =
    either (Left . renderMcpError) Right
        <$> requestMcpFull client
            (clientRequest client method parameters)
                { requestTimeoutMicros = timeoutMicros }

requestMcpFull :: McpClient -> McpRequest -> IO (Either McpError RawJson)
requestMcpFull client request = do
    failed <- readTVarIO client.clientFailure
    case failed of
        Just err -> pure (Left (McpTransportError err))
        Nothing -> do
            era <- case request.requestEra of
                Just era -> pure (Just era)
                Nothing -> mcpClientEra client
            requestId <- atomicModifyIORef' client.clientNextId \current ->
                (current + 1, current)
            pending <- newPendingRequest request
            atomically $
                modifyTVar' client.clientPending (IntMap.insert requestId pending)
            elicitEnabled <-
                if request.requestMeta && era == Just McpEraModern
                    then isJust <$> client.clientHooks.mcpHostElicit
                    else pure False
            let meta = metaSeries client era request requestId elicitEnabled
                message = requestEnvelope (Just requestId) request.requestMethod
                    (request.requestParams <> meta)
            case client.clientConfig.mcpServerUrl of
                Just url ->
                    httpExchange client url era request (Just (requestId, pending)) message
                        `finally` unregister requestId
                Nothing ->
                    sendMessage client message >>= \case
                        Left err -> do
                            unregister requestId
                            pure (Left (McpTransportError err))
                        Right () ->
                            awaitResponse client requestId pending request
                                `finally` unregister requestId
  where
    unregister requestId =
        atomically $ modifyTVar' client.clientPending (IntMap.delete requestId)

newPendingRequest :: McpRequest -> IO PendingRequest
newPendingRequest request = do
    response <- newEmptyTMVarIO
    activity <- newTVarIO 0
    pure PendingRequest
        { pendingResponse = response
        , pendingActivity = activity
        , pendingOnProgress = fromMaybe (const (pure ())) request.requestOnProgress
        }

requestEnvelope :: Maybe Int -> Text -> Series -> Aeson.Encoding
requestEnvelope requestId method parameters =
    Aeson.pairs $
        "jsonrpc" .= ("2.0" :: Text)
            <> maybe mempty ("id" .=) requestId
            <> "method" .= method
            <> AesonEncoding.pair "params" (Aeson.pairs parameters)

-- | Per-request metadata. Modern servers require the protocol version and
-- client capabilities on every request; every era accepts a progress token.
metaSeries :: McpClient -> Maybe McpProtocolEra -> McpRequest -> Int -> Bool -> Series
metaSeries client era request requestId elicitEnabled
    | not request.requestMeta = mempty
    | otherwise = "_meta" .= object (progress <> modern)
  where
    progress = ["progressToken" .= requestId]
    modern = case era of
        Just McpEraModern ->
            [ "io.modelcontextprotocol/protocolVersion" .= modernProtocolVersion
            , "io.modelcontextprotocol/clientInfo" .= clientInfoValue client.clientHooks
            , "io.modelcontextprotocol/clientCapabilities"
                .= clientCapabilitiesValue elicitEnabled
            ]
        _ -> []

-- | Wait for a stdio response. Progress notifications extend the wait up to
-- a hard limit; a timeout cancels the request.
awaitResponse
    :: McpClient
    -> Int
    -> PendingRequest
    -> McpRequest
    -> IO (Either McpError RawJson)
awaitResponse client requestId pending request
    | request.requestTimeoutMicros <= 0 =
        atomically (takeTMVar pending.pendingResponse)
    | otherwise = do
        start <- getMonotonicTimeNSec
        go start 0
  where
    slice = max 1 request.requestTimeoutMicros
    go start seen = do
        timed <- timeout slice (atomically (takeTMVar pending.pendingResponse))
        case timed of
            Just value -> pure value
            Nothing -> do
                now <- getMonotonicTimeNSec
                activity <- readTVarIO pending.pendingActivity
                if activity /= seen && not (pastHardDeadline start now slice)
                    then go start activity
                    else do
                        _ <- sendNotification client "notifications/cancelled"
                            ("requestId" .= requestId
                                <> "reason" .= ("timeout" :: Text))
                        pure (Left (timeoutError request.requestMethod slice))

pastHardDeadline :: Word64 -> Word64 -> Int -> Bool
pastHardDeadline start now sliceMicros =
    now - start
        >= fromIntegral sliceMicros * fromIntegral hardTimeoutMultiplier * 1000

timeoutError :: Text -> Int -> McpError
timeoutError method sliceMicros =
    McpTimeout $
        "MCP request "
            <> method
            <> " timed out after "
            <> Text.pack (show ((sliceMicros + 999999) `div` 1000000))
            <> " seconds"

-- * Multi round-trip requests and tasks

data ResultKind
    = ResultComplete
    | ResultInputRequired
    | ResultTask
    | ResultUnknown !Text
    deriving (Eq, Show)

resultKind :: RawJson -> ResultKind
resultKind raw =
    case projectRawOr Nothing (Json.object (Json.optionalKey "resultType" Json.text)) raw of
        Nothing -> ResultComplete
        Just "complete" -> ResultComplete
        Just "input_required" -> ResultInputRequired
        Just "task" -> ResultTask
        Just other -> ResultUnknown other

-- | Issue a request that may return @input_required@ or @task@ results and
-- drive it to completion.
invokeWithInputRounds :: McpClient -> McpRequest -> IO (Either McpError RawJson)
invokeWithInputRounds client request = go (0 :: Int) mempty
  where
    go rounds extra =
        requestMcpFull client request { requestParams = request.requestParams <> extra }
            >>= \case
            Left err -> pure (Left err)
            Right raw -> case resultKind raw of
                ResultComplete -> pure (Right raw)
                ResultTask -> awaitTask client request raw
                ResultUnknown kind ->
                    pure (Left (McpTransportError ("unrecognized resultType \"" <> kind <> "\"")))
                ResultInputRequired
                    | rounds >= maxInputRounds ->
                        pure (Left (McpTransportError
                            ("MCP server kept requesting input after "
                                <> Text.pack (show maxInputRounds) <> " rounds")))
                    | otherwise ->
                        case Json.decodeEither inputRequiredDecoder (rawJsonBytes raw) of
                            Left err ->
                                pure (Left (McpTransportError
                                    ("invalid input_required result: " <> err.jsonErrorMessage)))
                            Right (requests, requestState) ->
                                fulfilInputRequests client requests >>= \case
                                    Left err -> pure (Left err)
                                    Right responses ->
                                        go (rounds + 1)
                                            (inputResponsesSeries responses
                                                <> maybe mempty
                                                    (\state -> AesonEncoding.pair "requestState" (rawJsonEncoding state))
                                                    requestState)

inputResponsesSeries :: [(Text, RawJson)] -> Series
inputResponsesSeries responses =
    AesonEncoding.pair "inputResponses" $ Aeson.pairs $ mconcat
        [ AesonEncoding.pair (Key.fromText key) (rawJsonEncoding value)
        | (key, value) <- responses
        ]

-- | Decode @inputRequests@ (key → method, params) and the opaque
-- @requestState@.
inputRequiredDecoder :: Json.Decoder ([(Text, Text, Maybe RawJson)], Maybe RawJson)
inputRequiredDecoder = Json.object do
    requests <-
        Json.optionalKey "inputRequests"
            (Json.objectAsMap pure inputRequestDecoder)
    requestState <- Json.optionalKey "requestState" rawJsonDecoder
    pure
        ( [ (key, method, params)
          | (key, (method, params)) <- maybe [] Map.toList requests
          ]
        , requestState
        )
  where
    inputRequestDecoder = Json.object do
        method <- Json.defaultKey "" "method" Json.text
        params <- Json.optionalKey "params" rawJsonDecoder
        pure (method, params)

fulfilInputRequests
    :: McpClient
    -> [(Text, Text, Maybe RawJson)]
    -> IO (Either McpError [(Text, RawJson)])
fulfilInputRequests client = go []
  where
    go collected [] = pure (Right (reverse collected))
    go collected ((key, method, params) : rest) =
        case method of
            "elicitation/create" -> do
                result <- runElicitation client params
                go ((key, encodeElicitResult result) : collected) rest
            other ->
                pure (Left (McpTransportError
                    ("MCP server requested " <> other
                        <> ", which this client does not support")))

-- | Ask the host for the requested input. Hosts without an elicitation UI
-- cancel; a crashing host hook also cancels rather than failing the call.
runElicitation :: McpClient -> Maybe RawJson -> IO McpElicitResult
runElicitation client params =
    client.clientHooks.mcpHostElicit >>= \case
        Nothing -> pure McpElicitCancel
        Just hook ->
            case params >>= decodeElicitRequest client.clientConfig.mcpServerName of
                Nothing -> pure McpElicitCancel
                Just request ->
                    tryAny (hook request) >>= \case
                        Left _ -> pure McpElicitCancel
                        Right result -> pure result

decodeElicitRequest :: Text -> RawJson -> Maybe McpElicitRequest
decodeElicitRequest serverName raw =
    either (const Nothing) Just (Json.decodeEither decoder (rawJsonBytes raw))
  where
    decoder = Json.object do
        mode <- Json.defaultKey "form" "mode" Json.text
        message <- Json.defaultKey "" "message" Json.text
        schema <- Json.optionalKey "requestedSchema" rawJsonDecoder
        url <- Json.optionalKey "url" Json.text
        elicitMode <- case mode of
            "url" -> maybe (fail "url mode requires a url") (pure . McpElicitUrl) url
            _ -> pure (McpElicitForm (fromMaybe emptyInputSchema schema))
        pure McpElicitRequest
            { elicitServerName = serverName
            , elicitMessage = message
            , elicitMode
            }

-- | Poll a task returned by a task-augmented request until it settles.
awaitTask :: McpClient -> McpRequest -> RawJson -> IO (Either McpError RawJson)
awaitTask client request raw =
    case Json.decodeEither taskDecoder (rawJsonBytes raw) of
        Left err ->
            pure (Left (McpTransportError ("invalid task result: " <> err.jsonErrorMessage)))
        Right task -> do
            start <- getMonotonicTimeNSec
            report 0 task
            poll' start (1 :: Int) task.taskPollIntervalMs
  where
    slice = max 1 request.requestTimeoutMicros
    report :: Int -> TaskState -> IO ()
    report count task =
        forM_ request.requestOnProgress \onProgress ->
            onProgress McpProgress
                { progressValue = fromIntegral (count :: Int)
                , progressTotal = Nothing
                , progressMessage =
                    Just ("task " <> task.taskStatus
                        <> maybe "" (": " <>) task.taskStatusMessage)
                }
    poll' start count intervalMs = do
        threadDelay (max 100 intervalMs * 1000)
        now <- getMonotonicTimeNSec
        if request.requestTimeoutMicros > 0 && pastHardDeadline start now slice
            then do
                _ <- requestMcpFull client
                    (clientRequest client "tasks/cancel" ("taskId" .= taskIdOf))
                pure (Left (timeoutError (request.requestMethod <> " task") slice))
            else
                requestMcpFull client
                    (clientRequest client "tasks/get" ("taskId" .= taskIdOf))
                    >>= \case
                    Left err -> pure (Left err)
                    Right state ->
                        case Json.decodeEither taskDecoder (rawJsonBytes state) of
                            Left err ->
                                pure (Left (McpTransportError
                                    ("invalid tasks/get result: " <> err.jsonErrorMessage)))
                            Right task -> do
                                report count task
                                case task.taskStatus of
                                    "completed" -> case task.taskResult of
                                        Just result -> pure (Right result)
                                        Nothing -> legacyTaskResult
                                    "failed" ->
                                        pure . Left $ fromMaybe
                                            (McpTransportError "MCP task failed")
                                            (task.taskError >>= decodeRpcError)
                                    "cancelled" ->
                                        pure (Left (McpTransportError "MCP task was cancelled"))
                                    "input_required" ->
                                        fulfilInputRequests client task.taskInputRequests
                                            >>= \case
                                            Left err -> pure (Left err)
                                            Right responses -> do
                                                _ <- requestMcpFull client
                                                    (clientRequest client "tasks/update"
                                                        ("taskId" .= taskIdOf
                                                            <> inputResponsesSeries responses))
                                                poll' start (count + 1) task.taskPollIntervalMs
                                    _ -> poll' start (count + 1) task.taskPollIntervalMs
      where
        taskIdOf = initialTaskId
    initialTaskId =
        projectRawOr "" (Json.object (Json.defaultKey "" "taskId" Json.text)) raw
    legacyTaskResult =
        requestMcpFull client
            (clientRequest client "tasks/result" ("taskId" .= initialTaskId))

data TaskState = TaskState
    { taskStatus :: !Text
    , taskStatusMessage :: !(Maybe Text)
    , taskPollIntervalMs :: !Int
    , taskResult :: !(Maybe RawJson)
    , taskError :: !(Maybe RawJson)
    , taskInputRequests :: ![(Text, Text, Maybe RawJson)]
    }

taskDecoder :: Json.Decoder TaskState
taskDecoder = Json.object do
    taskStatus <- Json.defaultKey "working" "status" Json.text
    taskStatusMessage <- Json.optionalKey "statusMessage" Json.text
    pollMs <- Json.optionalKey "pollIntervalMs" Json.int
    pollLegacy <- Json.optionalKey "pollInterval" Json.int
    taskResult <- Json.optionalKey "result" rawJsonDecoder
    taskError <- Json.optionalKey "error" rawJsonDecoder
    requests <-
        Json.optionalKey "inputRequests"
            (Json.objectAsMap pure inputRequestDecoder)
    pure TaskState
        { taskStatus
        , taskStatusMessage
        , taskPollIntervalMs = fromMaybe 1000 (maybe pollLegacy Just pollMs)
        , taskResult
        , taskError
        , taskInputRequests =
            [ (key, method, params)
            | (key, (method, params)) <- maybe [] Map.toList requests
            ]
        }
  where
    inputRequestDecoder = Json.object do
        method <- Json.defaultKey "" "method" Json.text
        params <- Json.optionalKey "params" rawJsonDecoder
        pure (method, params)

decodeRpcError :: RawJson -> Maybe McpError
decodeRpcError raw =
    either (const Nothing) Just $
        Json.decodeEither
            (Json.object do
                code <- Json.defaultKey errorCodeInternal "code" Json.int
                message <- Json.defaultKey "" "message" Json.text
                payload <- Json.optionalKey "data" rawJsonDecoder
                pure (McpRpcError code message payload))
            (rawJsonBytes raw)

-- * Subscriptions

-- | Open a @subscriptions/listen@ stream for the list-change notifications
-- the server can emit. Legacy servers deliver list changes unsolicited.
startSubscriptions :: McpClient -> IO ()
startSubscriptions client = do
    info <- readTVarIO client.clientServerInfo
    case info of
        Just McpServerInfo{serverInfoEra = McpEraModern, serverInfoCapabilities = capabilities} -> do
            let wanted =
                    [ "toolsListChanged" .= True
                    | maybe False (.listChanged) capabilities.capabilityTools
                    ]
                    <> [ "promptsListChanged" .= True
                       | maybe False (.listChanged) capabilities.capabilityPrompts
                       ]
                    <> [ "resourcesListChanged" .= True
                       | maybe False (.resourcesListChanged) capabilities.capabilityResources
                       ]
            unless (null wanted) $
                spawnClientWorker client (subscriptionLoop client (mconcat wanted))
        _ -> pure ()

subscriptionLoop :: McpClient -> Series -> IO ()
subscriptionLoop client filterSeries = go (1 :: Int)
  where
    go delaySeconds = do
        outcome <-
            requestMcpFull client
                (clientRequest client "subscriptions/listen"
                    (AesonEncoding.pair "notifications" (Aeson.pairs filterSeries)))
                    { requestTimeoutMicros = 0 }
        closed <- readMVar client.clientClosed
        failed <- readTVarIO client.clientFailure
        case outcome of
            Left (McpRpcError _ _ _) -> pure ()
            _ | closed || isJust failed -> pure ()
              | otherwise -> do
                    -- The server closed the stream gracefully or the
                    -- connection dropped; re-establish with backoff.
                    threadDelay (delaySeconds * 1000000)
                    go (min 30 (delaySeconds * 2))

-- * Transport: stdio

sendNotification
    :: McpClient
    -> Text
    -> Series
    -> IO (Either McpError ())
sendNotification client method parameters =
    case client.clientConfig.mcpServerUrl of
        Just url -> do
            era <- mcpClientEra client
            void <$> httpExchange client url era
                (clientRequest client method parameters)
                Nothing
                (requestEnvelope Nothing method parameters)
        Nothing ->
            either (Left . McpTransportError) Right
                <$> sendMessage client (requestEnvelope Nothing method parameters)

sendMessage :: McpClient -> Aeson.Encoding -> IO (Either Text ())
sendMessage client message =
    tryAny
        (case client.clientInput of
            Nothing -> ioError (userError "MCP client has no stdio transport")
            Just input -> withMVar client.clientWriteLock \_ -> do
                LBS.hPutStr input
                    (AesonEncodingInternal.encodingToLazyByteString message <> "\n")
                hFlush input)
        >>= \case
            Left exception -> do
                let err = "MCP write failed: " <> exceptionSummary exception
                failPending client.clientPending client.clientFailure err
                pure (Left err)
            Right () -> pure (Right ())

readerLoop :: McpClient -> Handle -> IO ()
readerLoop client output =
    loop `finally`
        failPending client.clientPending client.clientFailure
            "MCP server stdout closed"
  where
    loop = do
        line <- BS8.hGetLine output
        unless (BS.null (BS8.strip line)) $
            case Json.decodeEither inboundDecoder line of
                Left err ->
                    failPending client.clientPending client.clientFailure
                        ("Invalid MCP JSON message: " <> err.jsonErrorMessage)
                Right inbound ->
                    handleInbound client inbound
        loop

-- | Any JSON-RPC message received from the server.
data McpInbound = McpInbound
    { inboundRawId :: !(Maybe RawJson)
    , inboundMethod :: !(Maybe Text)
    , inboundParams :: !(Maybe RawJson)
    , inboundResult :: !(Maybe RawJson)
    , inboundError :: !(Maybe RawJson)
    }

inboundDecoder :: Json.Decoder McpInbound
inboundDecoder = Json.object do
    inboundRawId <- Json.optionalKey "id" rawJsonDecoder
    inboundMethod <- Json.optionalKey "method" Json.text
    inboundParams <- Json.optionalKey "params" rawJsonDecoder
    inboundResult <- Json.optionalKey "result" rawJsonDecoder
    inboundError <- Json.optionalKey "error" rawJsonDecoder
    pure McpInbound{..}

-- | Route one inbound message: response, server request, or notification.
handleInbound :: McpClient -> McpInbound -> IO ()
handleInbound client inbound =
    case inbound.inboundMethod of
        Just method -> case inbound.inboundRawId of
            Just requestId -> handleServerRequest client requestId method inbound.inboundParams
            Nothing -> handleNotification client method inbound.inboundParams
        Nothing -> routeResponse client inbound

routeResponse :: McpClient -> McpInbound -> IO ()
routeResponse client inbound =
    case inbound.inboundRawId >>= decodeIntId of
        Nothing -> pure ()
        Just ident -> do
            destination <- atomically do
                current <- readTVar client.clientPending
                writeTVar client.clientPending (IntMap.delete ident current)
                pure (IntMap.lookup ident current)
            forM_ destination \pending ->
                atomically . void . tryPutTMVar pending.pendingResponse $
                    case inbound.inboundError of
                        Just err ->
                            Left (fromMaybe
                                (McpTransportError ("MCP error: " <> compactRawJson err))
                                (decodeRpcError err))
                        Nothing -> case inbound.inboundResult of
                            Just result -> Right result
                            Nothing -> Left (McpTransportError "MCP response omitted result")

decodeIntId :: RawJson -> Maybe Int
decodeIntId value =
    either (const Nothing) Just (Json.decodeEither Json.int (rawJsonBytes value))

-- | Answer a server-initiated request. Only @ping@ and legacy
-- @elicitation/create@ are supported; everything else is unknown.
handleServerRequest :: McpClient -> RawJson -> Text -> Maybe RawJson -> IO ()
handleServerRequest client requestId method params =
    case method of
        "ping" -> respondResult (Aeson.toEncoding (object []))
        "elicitation/create" ->
            spawnClientWorker client do
                result <- runElicitation client params
                respondResult (rawJsonEncoding (encodeElicitResult result))
        _ ->
            respondError errorCodeMethodNotFound ("Method not found: " <> method)
  where
    respondResult result =
        void $ sendResponse client $
            Aeson.pairs $
                "jsonrpc" .= ("2.0" :: Text)
                    <> AesonEncoding.pair "id" (rawJsonEncoding requestId)
                    <> AesonEncoding.pair "result" result
    respondError code message =
        void $ sendResponse client $
            Aeson.pairs $
                "jsonrpc" .= ("2.0" :: Text)
                    <> AesonEncoding.pair "id" (rawJsonEncoding requestId)
                    <> "error" .= object ["code" .= code, "message" .= message]

sendResponse :: McpClient -> Aeson.Encoding -> IO (Either McpError ())
sendResponse client message =
    case client.clientConfig.mcpServerUrl of
        Just url -> do
            era <- mcpClientEra client
            void <$> httpExchange client url era
                (clientRequest client "" mempty) Nothing message
        Nothing ->
            either (Left . McpTransportError) Right <$> sendMessage client message

handleNotification :: McpClient -> Text -> Maybe RawJson -> IO ()
handleNotification client method params =
    case method of
        "notifications/progress" ->
            forM_ (params >>= decodeProgress) \(token, progress) -> do
                pending <- IntMap.lookup token <$> readTVarIO client.clientPending
                forM_ pending \entry -> do
                    atomically $ modifyTVar' entry.pendingActivity (+ 1)
                    void (tryAny (entry.pendingOnProgress progress))
        "notifications/tools/list_changed" -> emit McpToolsListChanged
        "notifications/prompts/list_changed" -> emit McpPromptsListChanged
        "notifications/resources/list_changed" -> emit McpResourcesListChanged
        "notifications/resources/updated" ->
            forM_ (params >>= uriOf) (emit . McpResourceUpdated)
        "notifications/message" ->
            forM_ (params >>= decodeLogMessage) \(level, logger, payload) ->
                emit (McpLogMessage level logger payload)
        _ -> pure ()
  where
    emit event = do
        handler <- readIORef client.clientEventHandler
        void (tryAny (handler event))
    uriOf raw =
        projectRawOr Nothing (Json.object (Json.optionalKey "uri" Json.text)) raw
    decodeLogMessage raw =
        either (const Nothing) Just $
            Json.decodeEither
                (Json.object do
                    level <- Json.defaultKey "info" "level" Json.text
                    logger <- Json.optionalKey "logger" Json.text
                    payload <- Json.defaultKey emptyObjectJson "data" rawJsonDecoder
                    pure (level, logger, payload))
                (rawJsonBytes raw)

emptyObjectJson :: RawJson
emptyObjectJson = rawJsonFromEncoding (Aeson.toEncoding (object []))

decodeProgress :: RawJson -> Maybe (Int, McpProgress)
decodeProgress raw =
    either (const Nothing) Just $
        Json.decodeEither
            (Json.object do
                token <- Json.atKey "progressToken" tokenDecoder
                value <- Json.defaultKey 0 "progress" Json.double
                total <- Json.optionalKey "total" Json.double
                message <- Json.optionalKey "message" Json.text
                pure (token, McpProgress value total message))
            (rawJsonBytes raw)
  where
    tokenDecoder =
        Json.getType >>= \case
            Json.VNumber -> Json.int
            Json.VString -> do
                text <- Json.text
                case reads (Text.unpack text) of
                    [(number, "")] -> pure number
                    _ -> fail "progress token is not an integer"
            _ -> fail "unsupported progress token"

-- | Run background work owned by the client. Finished workers are pruned on
-- the next spawn; remaining ones are cancelled by 'closeMcpClient'.
spawnClientWorker :: McpClient -> IO () -> IO ()
spawnClientWorker client action =
    -- Share the close lock through registration: shutdown either observes the
    -- worker and joins it, or completes first and prevents it from starting.
    withMVar client.clientClosed \closed ->
        unless closed $ mask_ do
            worker <- asyncWithUnmask \unmask -> unmask (void (tryAny action))
            current <- readTVarIO client.clientWorkers
            live <- fmap catMaybes $ forM current \existing ->
                poll existing >>= \case
                    Nothing -> pure (Just existing)
                    Just _ -> pure Nothing
            atomically $ writeTVar client.clientWorkers (worker : live)

-- * Transport: Streamable HTTP

data HttpOutcome
    = HttpDelivered
    -- ^ The message was accepted (2xx). Any response was routed to the
    -- pending map.
    | HttpUnauthorized !Int ![Header]
    | HttpFailed !McpError

-- | Perform one POST to the MCP endpoint. Responses (single JSON objects or
-- SSE streams) are routed through 'handleInbound'; the pending entry, when
-- present, then carries the result.
httpExchange
    :: McpClient
    -> Text
    -> Maybe McpProtocolEra
    -> McpRequest
    -> Maybe (Int, PendingRequest)
    -> Aeson.Encoding
    -> IO (Either McpError RawJson)
httpExchange client url era request pending message = do
    baseRequest <- parseRequest (Text.unpack url)
    session <- readIORef client.clientHttpSession
    negotiated <- readTVarIO client.clientServerInfo
    tokenResult <- configuredAccessToken client
    let body = AesonEncodingInternal.encodingToLazyByteString message
        protocolHeader = case era of
            Just McpEraModern -> [("MCP-Protocol-Version", TextEncoding.encodeUtf8 modernProtocolVersion)]
            Just McpEraLegacy ->
                [ ("MCP-Protocol-Version", TextEncoding.encodeUtf8 info.serverInfoProtocolVersion)
                | Just info <- [negotiated]
                ]
            Nothing -> []
        modernHeaders
            | era == Just McpEraModern && not (Text.null request.requestMethod) =
                [("Mcp-Method", TextEncoding.encodeUtf8 request.requestMethod)]
                    <> [ ("Mcp-Name", TextEncoding.encodeUtf8 (encodeHeaderValue name))
                       | Just name <- [request.requestName]
                       ]
                    <> [ (fromString (Text.unpack name), TextEncoding.encodeUtf8 value)
                       | (name, value) <- request.requestHeaderParams
                       ]
            | otherwise = []
        sessionHeader
            | era == Just McpEraModern = []
            | otherwise =
                [ ("Mcp-Session-Id", TextEncoding.encodeUtf8 value)
                | Just value <- [session]
                ]
        headersFor token =
            [ ("Content-Type", "application/json")
            , ("Accept", "application/json, text/event-stream")
            ]
                <> [ ("Authorization", "Bearer " <> TextEncoding.encodeUtf8 value)
                   | Just value <- [token]
                   ]
                <> protocolHeader
                <> modernHeaders
                <> sessionHeader
        slice = request.requestTimeoutMicros
        perform token = do
            let httpRequest = baseRequest
                    { HC.method = "POST"
                    , HC.requestBody = RequestBodyLBS body
                    , HC.requestHeaders = headersFor token
                    , HC.responseTimeout =
                        if slice <= 0
                            then HC.responseTimeoutNone
                            else HC.responseTimeoutMicro slice
                    }
            tryAny (withResponse httpRequest mcpHttpManager (consume era))
        consume responseEra response = do
            let status = statusCode (responseStatus response)
                headers = responseHeaders response
            when (responseEra /= Just McpEraModern) $
                forM_ (lookup "Mcp-Session-Id" headers)
                    (writeIORef client.clientHttpSession . Just . TextEncoding.decodeUtf8)
            if status == 401 || status == 403
                then pure (HttpUnauthorized status headers)
                else if status < 200 || status >= 300
                    then do
                        readBounded (responseBody response) >>= \case
                            Left _ ->
                                pure (HttpFailed (McpTransportError
                                    ("MCP HTTP error response exceeded "
                                        <> Text.pack (show mcpBodyLimit) <> " bytes")))
                            Right bytes ->
                                pure . HttpFailed $ fromMaybe
                                    (McpHttpStatus status
                                        (TextEncoding.decodeUtf8With lenientDecode bytes))
                                    (decodeErrorBody bytes)
                    else if status == 202 || status == 204
                        then pure HttpDelivered
                        else if isEventStream headers
                            then readSseStream client (responseBody response) pending request
                            else do
                                readBounded (responseBody response) >>= \case
                                    Left _ ->
                                        pure (HttpFailed (McpTransportError
                                            ("MCP HTTP response exceeded "
                                                <> Text.pack (show mcpBodyLimit) <> " bytes")))
                                    Right bytes ->
                                        if BS.null (BS8.strip bytes)
                                            then pure HttpDelivered
                                            else case Json.decodeEither inboundDecoder bytes of
                                                Left err ->
                                                    pure (HttpFailed (McpTransportError
                                                        ("Invalid MCP HTTP response: " <> err.jsonErrorMessage)))
                                                Right inbound -> do
                                                    handleInbound client inbound
                                                    pure HttpDelivered
        settle outcome = case outcome of
            Left exception ->
                pure (Left (McpTransportError
                    ("MCP HTTP request failed: " <> exceptionSummary exception)))
            Right (HttpFailed err) -> pure (Left err)
            Right (HttpUnauthorized status headers) ->
                pure (Left (authorizationError status headers))
            Right HttpDelivered -> case pending of
                Nothing -> pure (Right emptyObjectJson)
                Just (_, entry) ->
                    atomically (tryReadTMVar entry.pendingResponse) >>= \case
                        Just value -> pure value
                        Nothing ->
                            pure (Left (McpTransportError
                                "MCP HTTP response did not include a result for the request"))
    case tokenResult of
        Left err -> pure (Left (McpTransportError err))
        Right configuredToken ->
            perform configuredToken >>= \case
                Right (HttpUnauthorized 401 _)
                    | Just path <- lookup "MCP_OAUTH_TOKEN_FILE" client.clientConfig.mcpServerEnv ->
                        OAuth.refreshOAuthTokenFile mcpHttpManager path >>= \case
                            Left err -> pure (Left (McpTransportError ("MCP OAuth refresh failed: " <> err)))
                            Right (OAuth.OAuthTokenFile _ _ token _ _) ->
                                perform (Just token) >>= settle
                outcome -> settle outcome

-- | Read a whole response body with a size cap.  The over-limit case is
-- reported before retaining any further bytes, so an unexpectedly large
-- diagnostic/JSON body cannot become a process-sized allocation.
readBounded :: HC.BodyReader -> IO (Either Int BS.ByteString)
readBounded reader = go [] 0
  where
    go chunks total = do
        chunk <- brRead reader
        if BS.null chunk
            then pure (Right (BS.concat (reverse chunks)))
            else
                let next = total + BS.length chunk
                in if next > mcpBodyLimit
                    then pure (Left next)
                    else go (chunk : chunks) next

mcpBodyLimit :: Int
mcpBodyLimit = 16 * 1024 * 1024

isEventStream :: [Header] -> Bool
isEventStream headers =
    case lookup "Content-Type" headers of
        Just value -> "text/event-stream" `BS.isPrefixOf` BS8.map toLowerAscii value
        Nothing -> False
  where
    toLowerAscii character
        | character >= 'A' && character <= 'Z' = toEnum (fromEnum character + 32)
        | otherwise = character

decodeErrorBody :: BS.ByteString -> Maybe McpError
decodeErrorBody bytes =
    case Json.decodeEither inboundDecoder bytes of
        Right inbound -> inbound.inboundError >>= decodeRpcError
        Left _ -> Nothing

authorizationError :: Int -> [Header] -> McpError
authorizationError status headers =
    McpHttpStatus status $
        case lookup "WWW-Authenticate" headers >>= OAuth.parseWwwAuthenticate of
            Just challenge
                | status == 403 || challenge.challengeError == Just "insufficient_scope" ->
                    "MCP server requires additional authorization"
                        <> scopeHint challenge
                        <> "; run `agent mcp login <url> --scope <scope>` to re-authorize"
                | otherwise ->
                    "MCP server requires OAuth authorization"
                        <> scopeHint challenge
                        <> "; run `agent mcp login <url>`"
            Nothing
                | status == 401 -> "MCP server requires OAuth authorization; run `agent mcp login <url>` or configure MCP OAuth credentials"
                | otherwise -> "MCP server denied the request"
  where
    scopeHint challenge =
        (case OAuth.challengeScopes challenge of
            [] -> ""
            scopes -> " (scope: " <> Text.unwords scopes <> ")")
            <> maybe "" (\description -> ": " <> description) challenge.challengeErrorDescription

-- | Consume a Server-Sent Events response stream, routing every event
-- through 'handleInbound' until the awaited response arrives or the stream
-- ends. Idle periods are bounded by the request timeout, extended while the
-- server reports progress.
readSseStream
    :: McpClient
    -> HC.BodyReader
    -> Maybe (Int, PendingRequest)
    -> McpRequest
    -> IO HttpOutcome
readSseStream client reader pending request = do
    start <- getMonotonicTimeNSec
    go start 0 BS.empty [] 0
  where
    slice = request.requestTimeoutMicros
    settled = case pending of
        Nothing -> pure False
        Just (_, entry) -> not <$> atomically (isEmptyTMVar entry.pendingResponse)
    go start seen buffer dataLines dataBytes = do
        done <- settled
        if done
            then pure HttpDelivered
            else do
                chunk <-
                    if slice <= 0
                        then Just <$> brRead reader
                        else timeout (max 1 slice) (brRead reader)
                case chunk of
                    Nothing -> do
                        now <- getMonotonicTimeNSec
                        activity <- maybe (pure 0) (readTVarIO . (.pendingActivity) . snd) pending
                        if activity /= seen && not (pastHardDeadline start now slice)
                            then go start activity buffer dataLines dataBytes
                            else pure (HttpFailed (timeoutError request.requestMethod slice))
                    Just bytes
                        | BS.null bytes -> do
                            -- End of stream: flush a trailing event without a blank line.
                            trailing <- if BS.null buffer
                                then pure (Right (dataLines, dataBytes))
                                else foldLines dataLines dataBytes [buffer]
                            case trailing of
                                Left err -> pure (HttpFailed err)
                                Right (remaining, _) -> do
                                    dispatchEvent remaining
                                    finished <- settled
                                    pure $ if finished || isNothing pending
                                        then HttpDelivered
                                        else HttpFailed (McpTransportError
                                            "MCP SSE stream closed before the response arrived")
                        | otherwise -> do
                            -- Check the combined partial line before
                            -- concatenating.  A peer can otherwise force an
                            -- arbitrarily large retained buffer by omitting
                            -- newlines.
                            splitSseChunk buffer bytes >>= \case
                                Left err -> pure (HttpFailed err)
                                Right (complete, rest) ->
                                    foldLines dataLines dataBytes complete >>= \case
                                        Left err -> pure (HttpFailed err)
                                        Right (remaining, remainingBytes) ->
                                            go start seen rest remaining remainingBytes
    foldLines dataLines dataBytes [] = pure (Right (dataLines, dataBytes))
    foldLines dataLines dataBytes (line : rest)
        | BS.null line = do
            dispatchEvent dataLines
            foldLines [] 0 rest
        | BS8.isPrefixOf ":" line = foldLines dataLines dataBytes rest
        | Just payload <- BS.stripPrefix "data:" line =
            let value = fromMaybe payload (BS.stripPrefix " " payload)
                nextBytes = dataBytes + BS.length value + 1
            in if nextBytes > mcpSseEventLimit
                then pure (Left (McpTransportError
                    "MCP SSE event exceeded the size limit"))
                else foldLines (value : dataLines) nextBytes rest
        | otherwise = foldLines dataLines dataBytes rest
    dispatchEvent [] = pure ()
    dispatchEvent dataLines =
        case Json.decodeEither inboundDecoder (BS8.intercalate "\n" (reverse dataLines)) of
            Left _ -> pure ()
            Right inbound -> handleInbound client inbound

mcpSseLineLimit :: Int
mcpSseLineLimit = 1024 * 1024

mcpSseEventLimit :: Int
mcpSseEventLimit = 16 * 1024 * 1024

-- Split a reader chunk without concatenating it wholesale with the partial
-- line.  This permits large transport chunks containing many ordinary lines,
-- while still bounding an individual unterminated line.
splitSseChunk
    :: BS.ByteString
    -> BS.ByteString
    -> Either McpError ([BS.ByteString], BS.ByteString)
splitSseChunk initial chunk = go initial chunk []
  where
    go partial rest reversedLines =
        case BS.elemIndex 10 rest of
            Nothing
                | BS.length partial + BS.length rest > mcpSseLineLimit ->
                    Left (McpTransportError "MCP SSE line exceeded the size limit")
                | otherwise ->
                    Right (reverse reversedLines, partial <> rest)
            Just index ->
                let prefix = BS.take index rest
                    remainder = BS.drop (index + 1) rest
                    lineLength = BS.length partial + BS.length prefix
                in if lineLength > mcpSseLineLimit
                    then Left (McpTransportError "MCP SSE line exceeded the size limit")
                    else
                        let line = fromMaybe (partial <> prefix)
                                (BS.stripSuffix "\r" (partial <> prefix))
                        in go BS.empty remainder (line : reversedLines)

-- | Split a buffer into complete lines (without terminators) and the
-- trailing partial line.
splitLines :: BS.ByteString -> ([BS.ByteString], BS.ByteString)
splitLines buffer =
    let pieces = BS8.split '\n' buffer
    in case reverse pieces of
        [] -> ([], BS.empty)
        partial : completeReversed ->
            (map stripCarriage (reverse completeReversed), partial)
  where
    stripCarriage line = fromMaybe line (BS.stripSuffix "\r" line)

configuredAccessToken :: McpClient -> IO (Either Text (Maybe Text))
configuredAccessToken client =
    case lookup "MCP_OAUTH_TOKEN_FILE" client.clientConfig.mcpServerEnv of
        Nothing -> pure (Right envToken)
        Just path -> OAuth.loadOAuthTokenFile path >>= \case
            Left err
                | isJust envToken -> pure (Right envToken)
                | otherwise -> pure (Left err)
            Right (OAuth.OAuthTokenFile _ _ token _ expiresAt) -> do
                now <- round <$> getPOSIXTime
                if maybe False (<= now + 60) expiresAt
                    then OAuth.refreshOAuthTokenFile mcpHttpManager path >>= \case
                        Left err -> pure (Left err)
                        Right (OAuth.OAuthTokenFile _ _ refreshed _ _) -> pure (Right (Just refreshed))
                    else pure (Right (Just token))
  where
    envToken = fmap Text.pack (lookup "MCP_ACCESS_TOKEN" client.clientConfig.mcpServerEnv)

-- | Compatibility helper for tests: decode a single JSON-RPC response body.
decodeHttpMcpResponse :: BS.ByteString -> Either Text RawJson
decodeHttpMcpResponse bytes = do
    inbound <- either (Left . (.jsonErrorMessage)) Right $
        Json.decodeEither inboundDecoder bytes
    case inbound.inboundError of
        Just err ->
            Left (maybe ("MCP error: " <> compactRawJson err) renderMcpError (decodeRpcError err))
        Nothing -> case inbound.inboundResult of
            Just result -> Right result
            Nothing -> Left "MCP response omitted result"

-- * Stderr capture

stderrLoop :: Handle -> IORef CapturedStderr -> IO ()
stderrLoop handle captured =
    let loop = do
            chunk <- BS.hGetSome handle 4096
            if BS.null chunk
                then pure ()
                else appendStderr captured chunk >> loop
    in loop

appendStderr :: IORef CapturedStderr -> BS.ByteString -> IO ()
appendStderr ref chunk =
    atomicModifyIORef' ref \current ->
        let combined = current.stderrBytes <> chunk
            overflow = max 0 (BS.length combined - stderrLimit)
            kept = BS.drop overflow combined
        in ( CapturedStderr
                { stderrBytes = kept
                , stderrDropped = current.stderrDropped + overflow
                }
           , ()
           )

capturedStderrText :: CapturedStderr -> Text
capturedStderrText captured =
    let body =
            TextEncoding.decodeUtf8With lenientDecode captured.stderrBytes
    in if captured.stderrDropped <= 0
        then body
        else
            "[... "
                <> Text.pack (show captured.stderrDropped)
                <> " stderr bytes omitted ...]\n"
                <> body

-- * Failure and shutdown

failClient
    :: TVar (IntMap.IntMap PendingRequest)
    -> TVar (Maybe Text)
    -> Text
    -> IO ()
failClient = failPending

failPending
    :: TVar (IntMap.IntMap PendingRequest)
    -> TVar (Maybe Text)
    -> Text
    -> IO ()
failPending pending failure err =
    atomically do
        existing <- readTVar failure
        when (existing == Nothing) (writeTVar failure (Just err))
        requests <- readTVar pending
        writeTVar pending IntMap.empty
        mapM_ (\entry -> void (tryPutTMVar entry.pendingResponse (Left (McpTransportError err))))
            (IntMap.elems requests)

closeMcpClient :: McpClient -> IO ()
closeMcpClient client =
    modifyMVar_ client.clientClosed \closed ->
        if closed
            then pure True
            else do
                atomically do
                    readTVar client.clientLifecycle >>= \case
                        ClientInitializing completion -> do
                            writeTVar client.clientLifecycle ClientClosed
                            void $
                                tryPutTMVar completion
                                    (Left "MCP server closed")
                        _ ->
                            writeTVar client.clientLifecycle ClientClosed
                workers <- atomically do
                    current <- readTVar client.clientWorkers
                    writeTVar client.clientWorkers []
                    pure current
                mapM_ stopWorker workers
                forM_ client.clientInput \input ->
                    void $ tryAny (hClose input)
                forM_ client.clientProcess $ \processHandle ->
                    terminateProcessGroup client.clientGroupId processHandle
                when (isJust client.clientConfig.mcpServerUrl) $
                    closeHttpSession client
                readIORef client.clientReader >>= mapM_ stopWorker
                forM_ client.clientStderrReader stopWorker
                failClient client.clientPending client.clientFailure
                    "MCP server closed"
                pure True

-- Legacy Streamable HTTP sessions are explicitly terminated with DELETE when
-- the server assigned a session id.  Failure is intentionally ignored during
-- shutdown: the local client is already being closed and the server may have
-- expired the session independently.
closeHttpSession :: McpClient -> IO ()
closeHttpSession client = do
    session <- readIORef client.clientHttpSession
    era <- mcpClientEra client
    case (client.clientConfig.mcpServerUrl, session, era) of
        (Just url, Just sessionId, era')
            | era' /= Just McpEraModern -> void $ tryAny do
                request <- parseRequest (Text.unpack url)
                bearer <- either (const Nothing) id <$> configuredAccessToken client
                let request' = request
                        { HC.method = "DELETE"
                        , HC.requestHeaders =
                            [ ("Mcp-Session-Id", TextEncoding.encodeUtf8 sessionId)
                            ]
                            <> maybe [] (\token ->
                                [ ("Authorization", "Bearer " <> TextEncoding.encodeUtf8 token) ])
                                bearer
                        }
                void $ timeout (secondsToMicros client.clientConfig.mcpServerRequestTimeoutSeconds)
                    (HC.httpNoBody request' mcpHttpManager)
        _ -> pure ()

stopWorker :: Async () -> IO ()
stopWorker worker = do
    cancel worker
    void (waitCatch worker)

closeOptionalHandles
    :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
    -> IO ()
closeOptionalHandles (input, output, errOutput, _) =
    mapM_ (\handle -> void (tryAny (hClose handle)))
        (catMaybes [input, output, errOutput])

mergedEnvironment :: [(String, String)] -> IO [(String, String)]
mergedEnvironment overrides = do
    inherited <- getEnvironment
    pure . Map.toList $
        foldl'
            (\environment (name, value) -> Map.insert name value environment)
            (Map.fromList inherited)
            overrides

secondsToMicros :: Int -> Int
secondsToMicros seconds = max 1 seconds * 1000000

exceptionSummary :: SomeException -> Text
exceptionSummary =
    Text.take 1000
        . fst
        . Text.breakOn "\nHasCallStack backtrace:"
        . Text.pack
        . displayException

redactConfiguredValues :: McpServerConfig -> Text -> Text
redactConfiguredValues config input =
    foldl' redact input (map (Text.pack . snd) config.mcpServerEnv)
  where
    redact current secret
        | Text.null secret = current
        | otherwise = Text.replace secret "<redacted>" current
