module Agent.MCP.Client.Internal.Runtime where

import Agent.Json
    ( RawJson,
      rawJsonBytes,
      rawJsonDecoder,
      rawJsonEncoding,
      rawJsonFromEncoding )
import Agent.MCP.Types
    ( CapturedStderr(..),
      McpClientLifecycle(ClientClosed, ClientInitializing),
      McpClient(clientHooks, clientEventHandler, clientClosed,
                clientRequestRegistry, clientFailure, clientLifecycle,
                clientWorkers, clientTransport, clientServerInfo, clientConfig),
      McpClientTransport(McpClientHttp, McpClientStdio),
      McpHttpTransport(httpUrl, httpSession),
      McpStdioTransport(stdioStderrReader, stdioWriteLock, stdioInput,
                        stdioGroupId, stdioProcess, stdioReader),
      RequestRegistry(..),
      PendingRequest(..),
      McpServerEvent(McpLogMessage, McpToolsListChanged,
                     McpPromptsListChanged, McpResourcesListChanged,
                     McpResourceUpdated),
      McpProgress(..),
      McpError(..),
      McpResourcesCapability(resourcesListChanged),
      McpListCapability(listChanged),
      McpServerCapabilities(capabilityResources, capabilityTools,
                            capabilityPrompts),
      McpServerInfo(serverInfoProtocolVersion, McpServerInfo,
                    serverInfoCapabilities, serverInfoEra),
      McpProtocolEra(..),
      McpElicitResult(McpElicitCancel),
      McpElicitMode(McpElicitForm, McpElicitUrl),
      McpElicitRequest(..),
      McpHostHooks(mcpHostElicit, mcpHostClientName,
                   mcpHostClientVersion),
      McpServerConfig(mcpServerEnv, mcpServerName,
                      mcpServerRequestTimeoutSeconds),
      encodeElicitResult,
      renderMcpError,
      errorCodeMethodNotFound,
      errorCodeInternal,
      stderrLimit,
      projectRawOr,
      emptyInputSchema )
import Agent.ToolDispatch ()
import Agent.Tools.IO ( terminateProcessGroup )
import Agent.Tools.Types ()
import Control.Concurrent ( threadDelay )
import Control.Concurrent.Async
    ( Async, asyncWithUnmask, cancel, poll, waitCatch )
import Control.Concurrent.MVar ( modifyMVar_, withMVar, readMVar )
import Control.Concurrent.STM
    ( atomically,
      TVar,
      newTVarIO,
      readTVar,
      readTVarIO,
      writeTVar,
      isEmptyTMVar,
      newEmptyTMVarIO,
      takeTMVar,
      tryPutTMVar,
      tryReadTMVar,
      modifyTVar' )
import Control.Exception.Safe
    ( SomeException,
      Exception(displayException),
      mask_,
      finally,
      tryAny )
import Control.Monad ( forM, forM_, unless, void, when )
import Control.Monad.Trans.Class ( lift )
import Control.Monad.Trans.Except
    ( ExceptT(..), runExceptT, throwE )
import Data.Aeson ( Value, object, Series, KeyValue((.=)) )
import Data.Char ( ord )
import Data.IORef
    ( IORef, atomicModifyIORef', readIORef, writeIORef )
import Data.List ()
import Data.Maybe ( catMaybes, fromMaybe, isJust, isNothing )
import Data.Scientific ()
import Data.String ( fromString )
import Data.Text ( Text )
import Data.Text.Encoding.Error ( lenientDecode )
import Data.Time.Clock.POSIX ( getPOSIXTime )
import Data.Word ( Word64 )
import GHC.Clock ( getMonotonicTimeNSec )
import Network.HTTP.Client
    ( Manager,
      RequestBody(..),
      brRead,
      parseRequest,
      responseBody,
      responseHeaders,
      responseStatus,
      withResponse )
import Network.HTTP.Client.TLS ( newTlsManager )
import Network.HTTP.Types ( Header, statusCode )
import System.Environment ( getEnvironment )
import System.IO ( Handle, hClose, hFlush )
import System.IO.Unsafe ( unsafePerformIO )
import System.Process ( ProcessHandle )
import System.Timeout ( timeout )
import qualified Data.Aeson as Aeson
    ( Encoding, encode, pairs, ToJSON(toEncoding) )
import qualified Data.Aeson.Encoding as AesonEncoding ( pair )
import qualified Data.Aeson.Encoding.Internal as AesonEncodingInternal
    ( encodingToLazyByteString )
import qualified Data.ByteString as BS
    ( ByteString,
      empty,
      concat,
      drop,
      elemIndex,
      hGetSome,
      isPrefixOf,
      length,
      null,
      stripPrefix,
      stripSuffix,
      take )
import qualified Data.ByteString.Char8 as BS8
    ( intercalate, isPrefixOf, hGetLine, map, split, strip )
import qualified Data.ByteString.Base64 as Base64 ( encode )
import qualified Network.HTTP.Client as HC
    ( responseTimeoutMicro,
      responseTimeoutNone,
      httpNoBody,
      BodyReader,
      Request(responseTimeout, method, requestBody, requestHeaders) )
import qualified Data.IntMap.Strict as IntMap
    ( delete, elems, empty, lookup, insert )
import qualified Agent.Json.Decode as Json
    ( Decoder,
      decodeEither,
      defaultKey,
      optionalKey,
      atKey,
      double,
      getType,
      int,
      object,
      objectAsMap,
      text,
      JsonError(jsonErrorMessage),
      ValueType(VString, VNumber) )
import qualified Data.Aeson.Key as Key ( fromText )
import qualified Data.Aeson.KeyMap as KeyMap ()
import qualified Data.ByteString.Lazy as LBS ( hPutStr, toStrict )
import qualified Data.Map.Strict as Map
    ( fromList, insert, toList )
import qualified Agent.MCP.OAuth as OAuth
    ( WwwAuthenticateChallenge(challengeError,
                               challengeErrorDescription),
      OAuthTokenFile(OAuthTokenFile),
      loadOAuthTokenFile,
      refreshOAuthTokenFile,
      parseWwwAuthenticate,
      challengeScopes )
import qualified Data.Text as Text
    ( pack,
      unpack,
      all,
      breakOn,
      head,
      isPrefixOf,
      isSuffixOf,
      last,
      null,
      replace,
      take,
      unwords )
import qualified Data.Text.Encoding as TextEncoding
    ( encodeUtf8, decodeUtf8, decodeUtf8With )

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
mcpHttpManager :: Manager
mcpHttpManager = unsafePerformIO newTlsManager
{-# NOINLINE mcpHttpManager #-}
mcpClientEra :: McpClient -> IO (Maybe McpProtocolEra)
mcpClientEra client = fmap (.serverInfoEra) <$> readTVarIO client.clientServerInfo

mcpClientServerInfo :: McpClient -> IO (Maybe McpServerInfo)
mcpClientServerInfo client = readTVarIO client.clientServerInfo
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

-- | Private error effect used to keep request/decode pipelines linear. Public
-- entry points continue to return their existing @Either@ types.
type McpCall = ExceptT McpError IO

requestMcpT :: McpClient -> McpRequest -> McpCall RawJson
requestMcpT client = ExceptT . requestMcpFull client

decodeMcpPayload
    :: Text
    -> Json.Decoder a
    -> RawJson
    -> McpCall a
decodeMcpPayload context decoder raw =
    case Json.decodeEither decoder (rawJsonBytes raw) of
        Left err ->
            throwE . McpTransportError $
                "invalid " <> context <> ": " <> err.jsonErrorMessage
        Right value -> pure value

requestAndDecode
    :: McpClient
    -> McpRequest
    -> Text
    -> Json.Decoder a
    -> McpCall a
requestAndDecode client request context decoder =
    requestMcpT client request >>= decodeMcpPayload context decoder

-- | Preserve the historical error text of APIs which predate 'McpError':
-- decode errors were already labelled but RPC/transport errors were rendered.
renderTextMcpResult :: Text -> Either McpError a -> Either Text a
renderTextMcpResult decodeContext = \case
    Left (McpTransportError err)
        | ("invalid " <> decodeContext <> ": ") `Text.isPrefixOf` err ->
            Left err
    Left err -> Left (renderMcpError err)
    Right value -> Right value

compactJson :: Value -> Text
compactJson = TextEncoding.decodeUtf8 . LBS.toStrict . Aeson.encode

compactRawJson :: RawJson -> Text
compactRawJson = TextEncoding.decodeUtf8 . rawJsonBytes

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
    era <- case request.requestEra of
        Just era -> pure (Just era)
        Nothing -> mcpClientEra client
    pending <- newPendingRequest request
    registration <- atomically do
        failed <- readTVar client.clientFailure
        case failed of
            Just err -> pure (Left err)
            Nothing -> do
                registry <- readTVar client.clientRequestRegistry
                let (nextRegistry, requestId) =
                        registerPending pending registry
                writeTVar client.clientRequestRegistry nextRegistry
                pure (Right requestId)
    case registration of
        Left err -> pure (Left (McpTransportError err))
        Right requestId -> do
            elicitEnabled <-
                if request.requestMeta && era == Just McpEraModern
                    then isJust <$> client.clientHooks.mcpHostElicit
                    else pure False
            let meta = metaSeries client era request requestId elicitEnabled
                message = requestEnvelope (Just requestId) request.requestMethod
                    (request.requestParams <> meta)
            case client.clientTransport of
                McpClientHttp transport ->
                    httpExchange client transport era request
                        (Just (requestId, pending)) message
                        `finally` unregister requestId
                McpClientStdio transport ->
                    sendMessage client transport message >>= \case
                        Left err -> do
                            unregister requestId
                            pure (Left (McpTransportError err))
                        Right () ->
                            awaitResponse client requestId pending request
                                `finally` unregister requestId
  where
    unregister requestId =
        atomically do
            registry <- readTVar client.clientRequestRegistry
            writeTVar client.clientRequestRegistry
                (registry
                    { requestRegistryPending =
                        IntMap.delete requestId registry.requestRegistryPending
                    })

emptyRequestRegistry :: RequestRegistry
emptyRequestRegistry = RequestRegistry
    { requestRegistryNextId = 1
    , requestRegistryPending = IntMap.empty
    }

-- | Allocate an id and install its response destination as one pure state
-- transition. The caller applies it to 'clientRequestRegistry' atomically.
registerPending
    :: PendingRequest
    -> RequestRegistry
    -> (RequestRegistry, Int)
registerPending pending registry =
    ( registry
        { requestRegistryNextId = requestId + 1
        , requestRegistryPending =
            IntMap.insert requestId pending registry.requestRegistryPending
        }
    , requestId
    )
  where
    requestId = registry.requestRegistryNextId

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
invokeWithInputRounds client request =
    runExceptT (invokeWithInputRoundsT client request)

invokeWithInputRoundsT :: McpClient -> McpRequest -> McpCall RawJson
invokeWithInputRoundsT client request = go (0 :: Int) mempty
  where
    go rounds extra = do
        raw <- requestMcpT client
            request { requestParams = request.requestParams <> extra }
        case resultKind raw of
            ResultComplete -> pure raw
            ResultTask -> awaitTaskT client request raw
            ResultUnknown kind ->
                throwE (McpTransportError
                    ("unrecognized resultType \"" <> kind <> "\""))
            ResultInputRequired
                | rounds >= maxInputRounds ->
                    throwE (McpTransportError
                        ("MCP server kept requesting input after "
                            <> Text.pack (show maxInputRounds) <> " rounds"))
                | otherwise -> do
                    (requests, requestState) <-
                        decodeMcpPayload "input_required result"
                            inputRequiredDecoder raw
                    responses <- ExceptT (fulfilInputRequests client requests)
                    go (rounds + 1)
                        (inputResponsesSeries responses
                            <> maybe mempty
                                (\state -> AesonEncoding.pair
                                    "requestState" (rawJsonEncoding state))
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
awaitTask client request raw = runExceptT (awaitTaskT client request raw)

awaitTaskT :: McpClient -> McpRequest -> RawJson -> McpCall RawJson
awaitTaskT client request raw = do
    task <- decodeMcpPayload "task result" taskDecoder raw
    start <- lift getMonotonicTimeNSec
    lift (report 0 task)
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
        lift (threadDelay (max 100 intervalMs * 1000))
        now <- lift getMonotonicTimeNSec
        if request.requestTimeoutMicros > 0 && pastHardDeadline start now slice
            then do
                -- Cancellation is best-effort; preserve the timeout as the
                -- primary error if the server rejects the cancel request.
                _ <- lift $ requestMcpFull client
                    (clientRequest client "tasks/cancel" ("taskId" .= taskIdOf))
                throwE (timeoutError (request.requestMethod <> " task") slice)
            else do
                task <- requestAndDecode client
                    (clientRequest client "tasks/get" ("taskId" .= taskIdOf))
                    "tasks/get result"
                    taskDecoder
                lift (report count task)
                case task.taskStatus of
                    "completed" -> case task.taskResult of
                        Just result -> pure result
                        Nothing -> legacyTaskResult
                    "failed" ->
                        throwE $ fromMaybe
                            (McpTransportError "MCP task failed")
                            (task.taskError >>= decodeRpcError)
                    "cancelled" ->
                        throwE (McpTransportError "MCP task was cancelled")
                    "input_required" -> do
                        responses <-
                            ExceptT (fulfilInputRequests client task.taskInputRequests)
                        -- tasks/update is best-effort: its successful response
                        -- body is only an acknowledgement, and RPC errors are
                        -- deliberately ignored. The next tasks/get is canonical.
                        _ <- lift $ requestMcpFull client
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
        requestMcpT client
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
    case client.clientTransport of
        McpClientHttp transport -> do
            era <- mcpClientEra client
            void <$> httpExchange client transport era
                (clientRequest client method parameters)
                Nothing
                (requestEnvelope Nothing method parameters)
        McpClientStdio transport ->
            either (Left . McpTransportError) Right
                <$> sendMessage client transport
                    (requestEnvelope Nothing method parameters)

sendMessage
    :: McpClient
    -> McpStdioTransport
    -> Aeson.Encoding
    -> IO (Either Text ())
sendMessage client transport message =
    tryAny
        (withMVar transport.stdioWriteLock \_ -> do
                LBS.hPutStr transport.stdioInput
                    (AesonEncodingInternal.encodingToLazyByteString message <> "\n")
                hFlush transport.stdioInput)
        >>= \case
            Left exception -> do
                let err = "MCP write failed: " <> exceptionSummary exception
                failPending client.clientRequestRegistry client.clientFailure err
                pure (Left err)
            Right () -> pure (Right ())

readerLoop :: McpClient -> Handle -> IO ()
readerLoop client output =
    loop `finally`
        failPending client.clientRequestRegistry client.clientFailure
            "MCP server stdout closed"
  where
    loop = do
        line <- BS8.hGetLine output
        unless (BS.null (BS8.strip line)) $
            case Json.decodeEither inboundDecoder line of
                Left err ->
                    failPending client.clientRequestRegistry client.clientFailure
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
                registry <- readTVar client.clientRequestRegistry
                let pending = registry.requestRegistryPending
                writeTVar client.clientRequestRegistry
                    (registry
                        { requestRegistryPending =
                            IntMap.delete ident pending
                        })
                pure (IntMap.lookup ident pending)
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
    case client.clientTransport of
        McpClientHttp transport -> do
            era <- mcpClientEra client
            void <$> httpExchange client transport era
                (clientRequest client "" mempty) Nothing message
        McpClientStdio transport ->
            either (Left . McpTransportError) Right
                <$> sendMessage client transport message

handleNotification :: McpClient -> Text -> Maybe RawJson -> IO ()
handleNotification client method params =
    case method of
        "notifications/progress" ->
            forM_ (params >>= decodeProgress) \(token, progress) -> do
                registry <- readTVarIO client.clientRequestRegistry
                let pending =
                        IntMap.lookup token registry.requestRegistryPending
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
    -> McpHttpTransport
    -> Maybe McpProtocolEra
    -> McpRequest
    -> Maybe (Int, PendingRequest)
    -> Aeson.Encoding
    -> IO (Either McpError RawJson)
httpExchange client transport era request pending message = do
    baseRequest <- parseRequest (Text.unpack transport.httpUrl)
    session <- readIORef transport.httpSession
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
                    (writeIORef transport.httpSession . Just . TextEncoding.decodeUtf8)
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
            else if BS.length chunk > mcpBodyLimit - total
                then pure (Left mcpBodyLimit)
                else go (chunk : chunks) (total + BS.length chunk)

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
                                else
                                    foldLines dataLines dataBytes
                                        [stripCarriage buffer]
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
                            case splitSseChunk buffer bytes of
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
                valueBytes = BS.length value + 1
            in if valueBytes > mcpSseEventLimit - dataBytes
                then pure (Left (McpTransportError
                    "MCP SSE event exceeded the size limit"))
                else
                    foldLines
                        (value : dataLines)
                        (dataBytes + valueBytes)
                        rest
        | otherwise = foldLines dataLines dataBytes rest
    dispatchEvent [] = pure ()
    dispatchEvent dataLines =
        case Json.decodeEither inboundDecoder (BS8.intercalate "\n" (reverse dataLines)) of
            Left _ -> pure ()
            Right inbound -> handleInbound client inbound

mcpSseLineLimit :: Int
mcpSseLineLimit = 16 * 1024 * 1024

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
                | exceedsLineLimit partial rest ->
                    Left (McpTransportError "MCP SSE line exceeded the size limit")
                | otherwise ->
                    Right (reverse reversedLines, partial <> rest)
            Just index ->
                let prefix = BS.take index rest
                    remainder = BS.drop (index + 1) rest
                in if exceedsLineLimit partial prefix
                    then Left (McpTransportError "MCP SSE line exceeded the size limit")
                    else
                        let combined
                                | BS.null partial = prefix
                                | otherwise = partial <> prefix
                            line = stripCarriage combined
                        in go BS.empty remainder (line : reversedLines)

    exceedsLineLimit left right =
        BS.length right > mcpSseLineLimit - BS.length left

stripCarriage :: BS.ByteString -> BS.ByteString
stripCarriage line = fromMaybe line (BS.stripSuffix "\r" line)

-- | Split a buffer into complete lines (without terminators) and the
-- trailing partial line.
splitLines :: BS.ByteString -> ([BS.ByteString], BS.ByteString)
splitLines buffer =
    let pieces = BS8.split '\n' buffer
    in case reverse pieces of
        [] -> ([], BS.empty)
        partial : completeReversed ->
            (map stripCarriage (reverse completeReversed), partial)

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
    :: TVar RequestRegistry
    -> TVar (Maybe Text)
    -> Text
    -> IO ()
failClient = failPending

failPending
    :: TVar RequestRegistry
    -> TVar (Maybe Text)
    -> Text
    -> IO ()
failPending pending failure err =
    atomically do
        existing <- readTVar failure
        when (existing == Nothing) (writeTVar failure (Just err))
        registry <- readTVar pending
        let requests = registry.requestRegistryPending
        writeTVar pending
            (registry { requestRegistryPending = IntMap.empty })
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
                case client.clientTransport of
                    McpClientStdio transport -> do
                        void $ tryAny (hClose transport.stdioInput)
                        terminateProcessGroup
                            transport.stdioGroupId
                            transport.stdioProcess
                        readIORef transport.stdioReader >>= mapM_ stopWorker
                        readIORef transport.stdioStderrReader >>= mapM_ stopWorker
                    McpClientHttp transport ->
                        closeHttpSession client transport
                failClient client.clientRequestRegistry client.clientFailure
                    "MCP server closed"
                pure True

-- Legacy Streamable HTTP sessions are explicitly terminated with DELETE when
-- the server assigned a session id.  Failure is intentionally ignored during
-- shutdown: the local client is already being closed and the server may have
-- expired the session independently.
closeHttpSession :: McpClient -> McpHttpTransport -> IO ()
closeHttpSession client transport = do
    session <- readIORef transport.httpSession
    era <- mcpClientEra client
    case (session, era) of
        (Just sessionId, era')
            | era' /= Just McpEraModern -> void $ tryAny do
                request <- parseRequest (Text.unpack transport.httpUrl)
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
