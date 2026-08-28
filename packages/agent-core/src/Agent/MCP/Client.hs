module Agent.MCP.Client where


import Agent.Json
    ( RawJson
    , rawJsonBytes
    , rawJsonDecoder
    , rawJsonEncoding
    , rawJsonFromEncoding
    )
import qualified Agent.Json.Decode as Json
import Agent.Tools.IO (terminateProcessGroup)
import Agent.Tools.Types
    ( AppTool(..)
    , ApprovalRule(..)
    , ToolExecutionPolicy(..)
    , ToolSchema(..)
    )
import Agent.ToolDispatch (typedTool)
import Agent.Concurrent (forConcurrentlyBounded_)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
    ( Async
    , asyncWithUnmask
    , cancel
    , mapConcurrently
    , waitCatch
    )
import Control.Concurrent.QSem
    ( newQSem
    , signalQSem
    , waitQSem
    )
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar
    , modifyMVar_
    , newMVar
    , withMVar
    )
import Control.Concurrent.STM
    ( STM
    , TMVar
    , TVar
    , atomically
    , modifyTVar'
    , newEmptyTMVar
    , newEmptyTMVarIO
    , newTVarIO
    , readTMVar
    , readTVar
    , readTVarIO
    , takeTMVar
    , tryPutTMVar
    , writeTVar
    )
import Control.Exception.Safe
    ( SomeException
    , displayException
    , bracket_
    , finally
    , mask
    , onException
    , throwIO
    , tryAny
    )
import Control.Monad (forM, forM_, unless, void, when)
import Data.Aeson
    ( ToJSON(toJSON)
    , Value
    , object
    , withObject
    , (.:)
    , (.:?)
    , (.!=)
    , (.=)
    )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as AesonEncoding
import qualified Data.Aeson.Encoding.Internal as AesonEncodingInternal
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isAlphaNum)
import Data.IORef
    ( IORef
    , atomicModifyIORef'
    , newIORef
    , readIORef
    , writeIORef
    )
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Data.List (find, sortOn)
import Data.Maybe (catMaybes, isJust)
import Data.Ord (Down(..))
import Data.Scientific (floatingOrInteger)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Vector as Vector
import System.Environment (getEnvironment)
import System.Directory (getCurrentDirectory)
import System.IO
    ( BufferMode(..)
    , Handle
    , hClose
    , hFlush
    , hSetBinaryMode
    , hSetBuffering
    )
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
import System.IO.Unsafe (unsafePerformIO)
import Agent.MCP.Types
import qualified Agent.MCP.OAuth as OAuth
import Network.HTTP.Client (Manager, RequestBody(..), httpLbs, parseRequest, responseBody, responseHeaders, responseStatus)
import qualified Network.HTTP.Client as HC
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (statusCode)
startMcpClient :: McpServerConfig -> IO McpClient
startMcpClient config = case config.mcpServerUrl of
    Just url -> startMcpHttpClient config url
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
                nextId <- newIORef 1
                pending <- newTVarIO IntMap.empty
                failure <- newTVarIO Nothing
                writeLock <- newMVar ()
                stderrRef <- newIORef emptyCapturedStderr
                closed <- newMVar False
                lifecycle <- newTVarIO ClientPending
                reader <- asyncWithUnmask \unmask ->
                    unmask (readerLoop output pending failure)
                        `finally` void (tryAny (hClose output))
                stderrReader <- asyncWithUnmask \unmask ->
                    unmask (stderrLoop errOutput stderrRef)
                        `finally` void (tryAny (hClose errOutput))
                session <- newIORef Nothing
                let client = McpClient
                        { clientConfig = config
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
                        , clientStderrReader = stderrReader
                        , clientClosed = closed
                        , clientLifecycle = lifecycle
                        }
                pure client
            _ -> do
                let (_, _, _, processHandle) = created
                groupId <- getPid processHandle
                terminateProcessGroup groupId processHandle
                closeOptionalHandles created
                ioError (userError "MCP server did not provide all stdio pipes")

mcpHttpManager :: Manager
mcpHttpManager = unsafePerformIO newTlsManager
{-# NOINLINE mcpHttpManager #-}

startMcpHttpClient :: McpServerConfig -> Text -> IO McpClient
startMcpHttpClient config url = mask $ \_ -> do
    -- Keep the existing client record/lifecycle while using synchronous HTTP.
    -- An exited helper process supplies the legacy handles needed by shutdown.
    created <- createProcess ((proc "true" [])
        { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe })
    case created of
        (Just input, Just output, Just errOutput, processHandle) -> do
            nextId <- newIORef 1
            pending <- newTVarIO IntMap.empty
            failure <- newTVarIO Nothing
            writeLock <- newMVar ()
            stderrRef <- newIORef emptyCapturedStderr
            closed <- newMVar False
            lifecycle <- newTVarIO ClientPending
            session <- newIORef Nothing
            reader <- asyncWithUnmask $ \unmask -> unmask (threadDelay maxBound)
            stderrReader <- asyncWithUnmask $ \unmask -> unmask (threadDelay maxBound)
            pure McpClient
                { clientConfig = config, clientHttpSession = session
                , clientInput = input, clientProcess = processHandle
                , clientGroupId = Nothing, clientNextId = nextId
                , clientPending = pending, clientFailure = failure
                , clientWriteLock = writeLock, clientStderr = stderrRef
                , clientReader = reader, clientStderrReader = stderrReader
                , clientClosed = closed, clientLifecycle = lifecycle }
        _ -> ioError (userError "MCP HTTP client setup failed")
  where
    _ = url

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
                    initializeClient client
                    discoverMcpTools client
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

initializeClient :: McpClient -> IO ()
initializeClient client = do
    let timeoutMicros =
            secondsToMicros client.clientConfig.mcpServerStartupTimeoutSeconds
        parameters = object
            [ "protocolVersion" .= ("2025-11-25" :: Text)
            , "capabilities" .= object []
            , "clientInfo" .= object
                [ "name" .= ("haskell-agent" :: Text)
                , "version" .= ("0.1.0" :: Text)
                ]
            ]
    result <- requestMcp client timeoutMicros "initialize"
        (Aeson.toEncoding parameters)
    case result of
        Left err -> startupFailure client err
        Right _ ->
            sendNotification client "notifications/initialized"
                (Aeson.toEncoding (object []))
                >>= either (startupFailure client) pure

startupFailure :: McpClient -> Text -> IO a
startupFailure client err = do
    stderrText <- capturedStderrText <$> readIORef client.clientStderr
    ioError . userError . Text.unpack $
        redactConfiguredValues client.clientConfig
            (err <> if Text.null stderrText then "" else "\nstderr:\n" <> stderrText)

discoverMcpTools :: McpClient -> IO ([McpTool], [Text])
discoverMcpTools client = go Nothing [] []
  where
    go cursor tools warnings = do
        let parameters = maybe
                (Aeson.toEncoding (object []))
                (\value ->
                    Aeson.pairs
                        (AesonEncoding.pair "cursor" (rawJsonEncoding value)))
                cursor
            timeoutMicros =
                secondsToMicros client.clientConfig.mcpServerRequestTimeoutSeconds
        requestMcp client timeoutMicros "tools/list" parameters >>= \case
            Left err -> ioError (userError (Text.unpack err))
            Right result ->
                case parsePage result of
                    Left err -> ioError (userError ("invalid tools/list response: " <> Text.unpack err))
                    Right (pageTools, nextCursor) -> do
                        let (readOnlyTools, skipped) =
                                foldr classify ([], []) pageTools
                            skippedWarnings =
                                [ "MCP server "
                                    <> client.clientConfig.mcpServerName
                                    <> " skipped non-read-only tool "
                                    <> tool.discoveredName
                                | tool <- skipped
                                ]
                        if isJust nextCursor
                            then go nextCursor
                                (tools <> readOnlyTools)
                                (warnings <> skippedWarnings)
                            else pure
                                (tools <> readOnlyTools, warnings <> skippedWarnings)

    classify
        :: McpTool
        -> ([McpTool], [McpTool])
        -> ([McpTool], [McpTool])
    classify tool (allowed, skipped)
        | tool.discoveredReadOnly = (tool : allowed, skipped)
        | otherwise = (allowed, tool : skipped)

    parsePage :: RawJson -> Either Text ([McpTool], Maybe RawJson)
    parsePage raw =
        case Json.decodeEither pageDecoder (rawJsonBytes raw) of
            Left err -> Left err.jsonErrorMessage
            Right page -> Right page

    pageDecoder = Json.object $
        (,)
            <$> Json.defaultKey [] "tools" (Json.list mcpToolDecoder)
            <*> Json.optionalKey "nextCursor" rawJsonDecoder

appToolFor :: McpClient -> McpTool -> AppTool
appToolFor client tool = AppTool
    { appToolName = qualifiedName
    , appToolDescription = tool.discoveredDescription
    -- Tool schemas enter the legacy Aeson-valued tool API here. Their wire
    -- decode and storage remain RawJson.
    , appToolSchema =
        RawJsonFunctionSchema (toJSON tool.discoveredInputSchema)
    , appToolHandler =
        typedTool qualifiedName rawObjectDecoder \arguments -> do
            callDiscoveredTool client tool arguments
    , appToolApproval = AlwaysReadOnly
    , appToolExecution = ParallelSafe
    , appToolResourceClaims = Nothing
    }
  where
    qualifiedName = qualifiedMcpToolName
        client.clientConfig.mcpServerName
        tool.discoveredName

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
callDiscoveredTool client tool arguments = do
    let encodedParameters = Aeson.pairs $
            "name" .= tool.discoveredName
                <> AesonEncoding.pair
                    "arguments"
                    (rawJsonEncoding arguments)
        timeoutMicros =
            secondsToMicros
                client.clientConfig.mcpServerRequestTimeoutSeconds
    requestMcp client timeoutMicros "tools/call" encodedParameters >>= \case
        Left err -> pure (Left err)
        Right result -> pure (normalizeMcpToolResult result)

normalizeMcpToolResult :: RawJson -> Either Text Text
normalizeMcpToolResult result =
    case Json.decodeEither mcpToolResultDecoder (rawJsonBytes result) of
        Left _ -> Right (compactRawJson result)
        Right (isError, structured, textParts) ->
            let output
                    | isJust structured && not (null textParts) =
                        compactRawJson result
                    | Just value <- structured = compactRawJson value
                    | not (null textParts) = Text.intercalate "\n" textParts
                    | otherwise = compactRawJson result
            in if isError then Left output else Right output

mcpToolResultDecoder
    :: Json.Decoder (Bool, Maybe RawJson, [Text])
mcpToolResultDecoder = Json.object do
    rawError <- Json.optionalKey "isError" rawJsonDecoder
    structured <- Json.optionalKey "structuredContent" rawJsonDecoder
    rawContent <- Json.optionalKey "content" rawJsonDecoder
    let isError = maybe False (projectOr False Json.bool) rawError
        textParts =
            maybe [] (projectOr [] contentTextPartsDecoder) rawContent
    pure (isError, structured, textParts)
  where
    projectOr fallback decoder value =
        either (const fallback) id $
            Json.decodeEither decoder (rawJsonBytes value)

    contentTextPartsDecoder =
        catMaybes <$> Json.list contentTextDecoder

    contentTextDecoder = Json.object do
        contentType <- Json.optionalKey "type" Json.text
        text <- Json.optionalKey "text" Json.text
        pure $ case (contentType, text) of
            (Just "text", Just value) -> Just value
            _ -> Nothing

compactJson :: Value -> Text
compactJson = TextEncoding.decodeUtf8 . LBS.toStrict . Aeson.encode

compactRawJson :: RawJson -> Text
compactRawJson = TextEncoding.decodeUtf8 . rawJsonBytes

requestMcp
    :: McpClient
    -> Int
    -> Text
    -> Aeson.Encoding
    -> IO (Either Text RawJson)
requestMcp client timeoutMicros method parameters = do
    failed <- readTVarIO client.clientFailure
    case (failed, client.clientConfig.mcpServerUrl) of
        (Just err, _) -> pure (Left err)
        (Nothing, Just url) -> httpRequestMcp client timeoutMicros url method parameters
        (Nothing, Nothing) -> do
            requestId <- atomicModifyIORef' client.clientNextId \current ->
                (current + 1, current)
            response <- newEmptyTMVarIO
            atomically $
                modifyTVar' client.clientPending (IntMap.insert requestId response)
            let message = Aeson.pairs $
                    "jsonrpc" .= ("2.0" :: Text)
                        <> "id" .= requestId
                        <> "method" .= method
                        <> AesonEncoding.pair "params" parameters
            sendMessage client message >>= \case
                Left err -> do
                    atomically $
                        modifyTVar' client.clientPending (IntMap.delete requestId)
                    pure (Left err)
                Right () -> do
                    timed <- timeout (max 1 timeoutMicros)
                        (atomically (takeTMVar response))
                    case timed of
                        Just value -> pure value
                        Nothing -> do
                            atomically $
                                modifyTVar' client.clientPending
                                    (IntMap.delete requestId)
                            pure . Left $
                                "MCP request "
                                    <> method
                                    <> " timed out after "
                                    <> Text.pack
                                        (show
                                            ((timeoutMicros + 999999) `div` 1000000))
                                    <> " seconds"

httpRequestMcp :: McpClient -> Int -> Text -> Text -> Aeson.Encoding -> IO (Either Text RawJson)
httpRequestMcp client timeoutMicros url method parameters = do
    requestId <- atomicModifyIORef' client.clientNextId (\current -> (current + 1, current))
    request <- parseRequest (Text.unpack url)
    session <- readIORef client.clientHttpSession
    file <- case lookup "MCP_OAUTH_TOKEN_FILE" client.clientConfig.mcpServerEnv of
        Nothing -> pure Nothing
        Just path -> OAuth.loadOAuthTokenFile path >>= \case
            Right (OAuth.OAuthTokenFile _ _ token _ _) -> pure (Just token)
            Left _ -> pure Nothing
    let configuredToken = case file of
            Just token -> Just token
            Nothing -> fmap Text.pack (lookup "MCP_ACCESS_TOKEN" client.clientConfig.mcpServerEnv)
        body = AesonEncodingInternal.encodingToLazyByteString $ Aeson.pairs
            ("jsonrpc" .= ("2.0" :: Text) <> "id" .= requestId <> "method" .= method <> AesonEncoding.pair "params" parameters)
        perform token = do
            let headers = [("Content-Type", "application/json"), ("Accept", "application/json, text/event-stream")]
                    <> maybe [] (\value -> [("Authorization", "Bearer " <> TextEncoding.encodeUtf8 value)]) token
                    <> maybe [] (\value -> [("Mcp-Session-Id", TextEncoding.encodeUtf8 value)]) session
                request' = request { HC.method = "POST", HC.requestBody = RequestBodyLBS body, HC.requestHeaders = headers }
            tryAny (timeout (max 1 timeoutMicros) (httpLbs request' mcpHttpManager))
        decodeResponse response
            | statusCode (responseStatus response) < 200 || statusCode (responseStatus response) >= 300 =
                pure (Left ("MCP HTTP request failed with status " <> Text.pack (show (statusCode (responseStatus response)))))
            | otherwise = do
                forM_ (lookup "Mcp-Session-Id" (responseHeaders response)) (writeIORef client.clientHttpSession . Just . TextEncoding.decodeUtf8)
                let bytes = LBS.toStrict (responseBody response)
                if BS.null bytes then pure (Right (rawJsonFromEncoding (Aeson.toEncoding (object []))))
                else case Json.decodeEither rawJsonDecoder bytes of
                    Right value -> pure (Right value)
                    Left jsonError -> case find (BS8.isPrefixOf "data: ") (BS8.lines bytes) of
                        Just eventData -> pure (either (Left . ("Invalid MCP SSE response: " <>) . Text.pack . show) Right (Json.decodeEither rawJsonDecoder (BS8.drop 6 eventData)))
                        Nothing -> pure (Left ("Invalid MCP HTTP response: " <> Text.pack (show jsonError)))
    perform configuredToken >>= \case
        Left exception -> pure (Left ("MCP HTTP request failed: " <> exceptionSummary exception))
        Right Nothing -> pure (Left ("MCP request " <> method <> " timed out"))
        Right (Just response)
            | statusCode (responseStatus response) == 401 -> case lookup "MCP_OAUTH_TOKEN_FILE" client.clientConfig.mcpServerEnv of
                Nothing -> pure (Left "MCP server requires OAuth authorization; configure MCP OAuth credentials")
                Just path -> OAuth.refreshOAuthTokenFile mcpHttpManager path >>= \case
                    Left err -> pure (Left ("MCP OAuth refresh failed: " <> err))
                    Right (OAuth.OAuthTokenFile _ _ token _ _) -> perform (Just token) >>= \case
                        Right (Just retryResponse) -> decodeResponse retryResponse
                        Right Nothing -> pure (Left ("MCP request " <> method <> " timed out"))
                        Left retryException -> pure (Left ("MCP HTTP request failed: " <> exceptionSummary retryException))
            | otherwise -> decodeResponse response

sendNotification
    :: McpClient
    -> Text
    -> Aeson.Encoding
    -> IO (Either Text ())
sendNotification client method parameters =
    case client.clientConfig.mcpServerUrl of
        Just url -> fmap (fmap (const ())) $
            httpRequestMcp client
                (secondsToMicros client.clientConfig.mcpServerRequestTimeoutSeconds)
                url method parameters
        Nothing -> sendMessage client . Aeson.pairs $
            "jsonrpc" .= ("2.0" :: Text)
                <> "method" .= method
                <> AesonEncoding.pair "params" parameters

sendMessage :: McpClient -> Aeson.Encoding -> IO (Either Text ())
sendMessage client message =
    tryAny
        (withMVar client.clientWriteLock \_ -> do
            LBS.hPutStr client.clientInput
                (AesonEncodingInternal.encodingToLazyByteString message <> "\n")
            hFlush client.clientInput)
        >>= \case
            Left exception -> do
                let err = "MCP write failed: " <> exceptionSummary exception
                failClient client.clientPending client.clientFailure err
                pure (Left err)
            Right () -> pure (Right ())

readerLoop
    :: Handle
    -> TVar (IntMap.IntMap (TMVar (Either Text RawJson)))
    -> TVar (Maybe Text)
    -> IO ()
readerLoop output pending failure =
    loop `finally` failPending pending failure "MCP server stdout closed"
  where
    loop = do
        line <- BS8.hGetLine output
        unless (BS.null line) $
            case Json.decodeEither responseEnvelopeDecoder line of
                Left err ->
                    failPending pending failure
                        ("Invalid MCP JSON response: " <> err.jsonErrorMessage)
                Right response ->
                    routeResponse pending response
        loop

data ResponseEnvelope = ResponseEnvelope
    { envelopeId :: !(Maybe Int)
    , envelopeError :: !(Maybe RawJson)
    , envelopeResult :: !(Maybe RawJson)
    }

responseEnvelopeDecoder :: Json.Decoder ResponseEnvelope
responseEnvelopeDecoder = Json.object do
    rawId <- Json.optionalKey "id" rawJsonDecoder
    envelopeError <- Json.optionalKey "error" rawJsonDecoder
    envelopeResult <- Json.optionalKey "result" rawJsonDecoder
    let envelopeId = rawId >>= \value ->
            either (const Nothing) Just $
                Json.decodeEither Json.int (rawJsonBytes value)
    pure ResponseEnvelope{..}

routeResponse
    :: TVar (IntMap.IntMap (TMVar (Either Text RawJson)))
    -> ResponseEnvelope
    -> IO ()
routeResponse pending envelope =
    case envelope.envelopeId of
        Nothing -> pure ()
        Just ident -> do
            destination <- atomically do
                current <- readTVar pending
                writeTVar pending (IntMap.delete ident current)
                pure (IntMap.lookup ident current)
            case destination of
                Nothing -> pure ()
                Just response ->
                    atomically . void . tryPutTMVar response $
                        case envelope.envelopeError of
                            Just err -> Left ("MCP error: " <> compactRawJson err)
                            Nothing -> case envelope.envelopeResult of
                                Just result -> Right result
                                Nothing -> Left "MCP response omitted result"

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

failClient
    :: TVar (IntMap.IntMap (TMVar (Either Text RawJson)))
    -> TVar (Maybe Text)
    -> Text
    -> IO ()
failClient = failPending

failPending
    :: TVar (IntMap.IntMap (TMVar (Either Text RawJson)))
    -> TVar (Maybe Text)
    -> Text
    -> IO ()
failPending pending failure err =
    atomically do
        existing <- readTVar failure
        when (existing == Nothing) (writeTVar failure (Just err))
        requests <- readTVar pending
        writeTVar pending IntMap.empty
        mapM_ (\response -> void (tryPutTMVar response (Left err)))
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
                void $ tryAny (hClose client.clientInput)
                terminateProcessGroup client.clientGroupId client.clientProcess
                stopWorker client.clientReader
                stopWorker client.clientStderrReader
                failClient client.clientPending client.clientFailure
                    "MCP server closed"
                pure True

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
        foldl
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
    foldl redact input (map (Text.pack . snd) config.mcpServerEnv)
  where
    redact current secret
        | Text.null secret = current
        | otherwise = Text.replace secret "<redacted>" current
