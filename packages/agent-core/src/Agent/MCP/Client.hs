module Agent.MCP.Client where


import Agent.Tools.IO (terminateProcessGroup)
import Agent.Tools.Types
    ( AppTool(..)
    , ApprovalRule(..)
    , ToolExecutionPolicy(..)
    , ToolSchema(..)
    )
import Agent.ToolDispatch (typedTool)
import Agent.Concurrent (forConcurrentlyBounded_)
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
import Control.Monad (forM, unless, void, when)
import Data.Aeson
    ( FromJSON(..)
    , Value(..)
    , object
    , withObject
    , (.:)
    , (.:?)
    , (.!=)
    , (.=)
    )
import qualified Data.Aeson as Aeson
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
import Agent.MCP.Types
startMcpClient :: McpServerConfig -> IO McpClient
startMcpClient config = mask \_ -> do
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
            let client = McpClient
                    { clientConfig = config
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
    result <- requestMcp client timeoutMicros "initialize" parameters
    case result of
        Left err -> startupFailure client err
        Right _ ->
            sendNotification client "notifications/initialized" (object [])
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
        let parameters = maybe (object []) (\value -> object ["cursor" .= value]) cursor
            timeoutMicros =
                secondsToMicros client.clientConfig.mcpServerRequestTimeoutSeconds
        requestMcp client timeoutMicros "tools/list" parameters >>= \case
            Left err -> ioError (userError (Text.unpack err))
            Right result ->
                case AesonTypes.parseEither parsePage result of
                    Left err -> ioError (userError ("invalid tools/list response: " <> err))
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

    parsePage :: Value -> AesonTypes.Parser ([McpTool], Maybe Value)
    parsePage = withObject "tools/list result" \fields ->
        (,)
            <$> fields .:? "tools" .!= []
            <*> fields .:? "nextCursor"

appToolFor :: McpClient -> McpTool -> AppTool
appToolFor client tool = AppTool
    { appToolName = qualifiedName
    , appToolDescription = tool.discoveredDescription
    , appToolSchema = RawJsonFunctionSchema tool.discoveredInputSchema
    , appToolHandler =
        typedTool qualifiedName \arguments -> do
            callDiscoveredTool client tool arguments
    , appToolApproval = AlwaysReadOnly
    , appToolExecution = ParallelSafe
    , appToolResourceClaims = Nothing
    }
  where
    qualifiedName = qualifiedMcpToolName
        client.clientConfig.mcpServerName
        tool.discoveredName

qualifiedMcpToolName :: Text -> Text -> Text
qualifiedMcpToolName serverName toolName =
    escapeComponent serverName <> "__" <> escapeComponent toolName
  where
    escapeComponent =
        Text.replace "__" "%5F%5F"
            . Text.replace "%" "%25"

callDiscoveredTool :: McpClient -> McpTool -> Value -> IO (Either Text Text)
callDiscoveredTool client tool arguments = do
    let parameters = object
            [ "name" .= tool.discoveredName
            , "arguments" .= arguments
            ]
        timeoutMicros =
            secondsToMicros
                client.clientConfig.mcpServerRequestTimeoutSeconds
    requestMcp client timeoutMicros "tools/call" parameters >>= \case
        Left err -> pure (Left err)
        Right result -> pure (normalizeMcpToolResult result)

normalizeMcpToolResult :: Value -> Either Text Text
normalizeMcpToolResult result@(Object fields) =
    let isError = case KeyMap.lookup "isError" fields of
            Just (Bool value) -> value
            _ -> False
        structured = KeyMap.lookup "structuredContent" fields
        textParts = maybe [] extractTextParts (KeyMap.lookup "content" fields)
        output
            | isJust structured && not (null textParts) = compactJson result
            | Just value <- structured = compactJson value
            | not (null textParts) = Text.intercalate "\n" textParts
            | otherwise = compactJson result
    in if isError then Left output else Right output
normalizeMcpToolResult result = Right (compactJson result)

extractTextParts :: Value -> [Text]
extractTextParts (Array items) =
    [ text
    | Object item <- Vector.toList items
    , Just (String "text") <- [KeyMap.lookup "type" item]
    , Just (String text) <- [KeyMap.lookup "text" item]
    ]
extractTextParts _ = []

compactJson :: Value -> Text
compactJson = TextEncoding.decodeUtf8 . LBS.toStrict . Aeson.encode

requestMcp
    :: McpClient
    -> Int
    -> Text
    -> Value
    -> IO (Either Text Value)
requestMcp client timeoutMicros method parameters = do
    failed <- readTVarIO client.clientFailure
    case failed of
        Just err -> pure (Left err)
        Nothing -> do
            requestId <- atomicModifyIORef' client.clientNextId \current ->
                (current + 1, current)
            response <- newEmptyTMVarIO
            atomically $
                modifyTVar' client.clientPending (IntMap.insert requestId response)
            let message = object
                    [ "jsonrpc" .= ("2.0" :: Text)
                    , "id" .= requestId
                    , "method" .= method
                    , "params" .= parameters
                    ]
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

sendNotification :: McpClient -> Text -> Value -> IO (Either Text ())
sendNotification client method parameters =
    sendMessage client $ object
        [ "jsonrpc" .= ("2.0" :: Text)
        , "method" .= method
        , "params" .= parameters
        ]

sendMessage :: McpClient -> Value -> IO (Either Text ())
sendMessage client message =
    tryAny
        (withMVar client.clientWriteLock \_ -> do
            LBS.hPutStr client.clientInput (Aeson.encode message <> "\n")
            hFlush client.clientInput)
        >>= \case
            Left exception -> do
                let err = "MCP write failed: " <> exceptionSummary exception
                failClient client.clientPending client.clientFailure err
                pure (Left err)
            Right () -> pure (Right ())

readerLoop
    :: Handle
    -> TVar (IntMap.IntMap (TMVar (Either Text Value)))
    -> TVar (Maybe Text)
    -> IO ()
readerLoop output pending failure =
    loop `finally` failPending pending failure "MCP server stdout closed"
  where
    loop = do
        line <- BS8.hGetLine output
        unless (BS.null line) $
            case Aeson.eitherDecodeStrict' line of
                Left err ->
                    failPending pending failure
                        ("Invalid MCP JSON response: " <> Text.pack err)
                Right value ->
                    routeResponse pending value
        loop

routeResponse
    :: TVar (IntMap.IntMap (TMVar (Either Text Value)))
    -> Value
    -> IO ()
routeResponse pending (Object fields) =
    case KeyMap.lookup "id" fields >>= responseId of
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
                        case KeyMap.lookup "error" fields of
                            Just err -> Left ("MCP error: " <> compactJson err)
                            Nothing -> case KeyMap.lookup "result" fields of
                                Just result -> Right result
                                Nothing -> Left "MCP response omitted result"
routeResponse _ _ = pure ()

responseId :: Value -> Maybe Int
responseId (Number value) =
    case floatingOrInteger value of
        Right integer -> Just integer
        Left (_ :: Double) -> Nothing
responseId _ = Nothing

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
    :: TVar (IntMap.IntMap (TMVar (Either Text Value)))
    -> TVar (Maybe Text)
    -> Text
    -> IO ()
failClient = failPending

failPending
    :: TVar (IntMap.IntMap (TMVar (Either Text Value)))
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
