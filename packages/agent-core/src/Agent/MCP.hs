-- | Local Model Context Protocol clients over the stdio transport.
--
-- Each configured server is started once, initialized, and queried for its
-- read-only tools. The returned 'AppTool' handlers share the retained client;
-- 'closeMcpFleet' must run after all loops and subagents using those handlers
-- have stopped.
module Agent.MCP
    ( McpServerConfig(..)
    , McpToolRegistration(..)
    , McpFleet(..)
    , startMcpFleet
    , startMcpFleetWithProgress
    , closeMcpFleet
    , mcpFleetTools
    , normalizeMcpToolResult
    ) where

import Agent.Tools.IO (terminateProcessGroup)
import Agent.Tools.Types
    ( AppTool(..)
    , ApprovalRule(..)
    , ToolExecutionPolicy(..)
    , ToolSchema(..)
    )
import Agent.ToolDispatch (typedTool)
import Control.Concurrent.Async
    ( Async
    , asyncWithUnmask
    , cancel
    , waitCatch
    )
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar_
    , newMVar
    , withMVar
    )
import Control.Concurrent.STM
    ( TMVar
    , TVar
    , atomically
    , modifyTVar'
    , newEmptyTMVarIO
    , newTVarIO
    , readTVar
    , readTVarIO
    , takeTMVar
    , tryPutTMVar
    , writeTVar
    )
import Control.Exception.Safe
    ( SomeException
    , displayException
    , finally
    , mask
    , onException
    , tryAny
    )
import Control.Monad (unless, void, when)
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
import Data.IORef
    ( IORef
    , atomicModifyIORef'
    , newIORef
    , readIORef
    )
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, isJust)
import Data.Scientific (floatingOrInteger)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Vector as Vector
import System.Environment (getEnvironment)
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

data McpServerConfig = McpServerConfig
    { mcpServerName :: !Text
    , mcpServerCommand :: !FilePath
    , mcpServerArgs :: ![String]
    , mcpServerCwd :: !(Maybe FilePath)
    , mcpServerEnv :: ![(String, String)]
    , mcpServerStartupTimeoutSeconds :: !Int
    , mcpServerRequestTimeoutSeconds :: !Int
    } deriving (Eq)

instance Show McpServerConfig where
    show config =
        "McpServerConfig"
            <> " { mcpServerName = " <> show config.mcpServerName
            <> ", mcpServerCommand = " <> show config.mcpServerCommand
            <> ", mcpServerArgs = " <> show config.mcpServerArgs
            <> ", mcpServerCwd = " <> show config.mcpServerCwd
            <> ", mcpServerEnv = "
            <> show [(name, "<redacted>" :: String) | (name, _) <- config.mcpServerEnv]
            <> ", mcpServerStartupTimeoutSeconds = "
            <> show config.mcpServerStartupTimeoutSeconds
            <> ", mcpServerRequestTimeoutSeconds = "
            <> show config.mcpServerRequestTimeoutSeconds
            <> " }"

data McpToolRegistration = McpToolRegistration
    { mcpRegistrationServer :: !Text
    , mcpRegistrationTool :: !AppTool
    }

data McpFleet = McpFleet
    { mcpFleetRegistrations :: ![McpToolRegistration]
    , mcpFleetWarnings :: ![Text]
    , mcpFleetClients :: ![McpClient]
    , mcpFleetClosed :: !(MVar Bool)
    }

mcpFleetTools :: McpFleet -> [AppTool]
mcpFleetTools = map (.mcpRegistrationTool) . (.mcpFleetRegistrations)

data McpClient = McpClient
    { clientConfig :: !McpServerConfig
    , clientInput :: !Handle
    , clientProcess :: !ProcessHandle
    , clientGroupId :: !(Maybe ProcessGroupID)
    , clientNextId :: !(IORef Int)
    , clientPending :: !(TVar (Map.Map Int (TMVar (Either Text Value))))
    , clientFailure :: !(TVar (Maybe Text))
    , clientWriteLock :: !(MVar ())
    , clientStderr :: !(IORef CapturedStderr)
    , clientReader :: !(Async ())
    , clientStderrReader :: !(Async ())
    , clientClosed :: !(MVar Bool)
    }

data CapturedStderr = CapturedStderr
    { stderrBytes :: !BS.ByteString
    , stderrDropped :: !Int
    }

emptyCapturedStderr :: CapturedStderr
emptyCapturedStderr = CapturedStderr BS.empty 0

stderrLimit :: Int
stderrLimit = 16 * 1024

data McpTool = McpTool
    { discoveredName :: !Text
    , discoveredDescription :: !Text
    , discoveredInputSchema :: !Value
    , discoveredReadOnly :: !Bool
    }

instance FromJSON McpTool where
    parseJSON = withObject "MCP tool" \fields -> do
        annotations <- fields .:? "annotations" .!= object []
        readOnly <- withObject "MCP tool annotations"
            (\values -> values .:? "readOnlyHint" .!= False)
            annotations
        McpTool
            <$> fields .: "name"
            <*> fields .:? "description" .!= ""
            <*> fields .:? "inputSchema" .!= emptyInputSchema
            <*> pure readOnly

emptyInputSchema :: Value
emptyInputSchema = object
    [ "type" .= ("object" :: Text)
    , "properties" .= object []
    , "additionalProperties" .= False
    ]

-- | Start every server independently. Ordinary server failures become
-- warnings so one unavailable integration does not disable healthy servers.
-- Duplicate MCP tool names are fatal because dispatch would be ambiguous.
startMcpFleet :: [McpServerConfig] -> IO McpFleet
startMcpFleet = startMcpFleetWithProgress (const (pure ()))

-- | Start every server while reporting its configured name immediately before
-- launching it. The callback is intended for startup UI and deliberately
-- receives no command arguments or environment values.
startMcpFleetWithProgress :: (Text -> IO ()) -> [McpServerConfig] -> IO McpFleet
startMcpFleetWithProgress reportStarting configs = mask \restore -> do
    closed <- newMVar False
    (clients, registrations, warnings) <-
        startServers restore [] [] [] configs
    let
        fleet = McpFleet
            { mcpFleetRegistrations = registrations
            , mcpFleetWarnings = warnings
            , mcpFleetClients = clients
            , mcpFleetClosed = closed
            }
    case duplicateRegistration registrations of
        Nothing -> pure fleet
        Just err -> closeMcpFleet fleet >> ioError (userError (Text.unpack err))
  where
    startServers _ clients registrations warnings [] =
        pure (reverse clients, registrations, warnings)
    startServers restore clients registrations warnings (config : rest) = do
        reportStarting config.mcpServerName
            `onException` mapM_ closeMcpClient clients
        attempt <- tryAny (restore (startServer config))
            `onException` mapM_ closeMcpClient clients
        case attempt of
            Left exception ->
                startServers restore clients registrations
                    (warnings <> [startupWarning config exception])
                    rest
            Right (client, tools, serverWarnings) ->
                startServers restore
                    (client : clients)
                    (registrations <> map (registrationFor client) tools)
                    (warnings <> serverWarnings)
                    rest
                    `onException` mapM_ closeMcpClient (client : clients)

    startServer config = do
        client <- startMcpClient config
        (tools, warnings) <- discoverMcpTools client
            `onException` closeMcpClient client
        pure (client, tools, warnings)

    registrationFor :: McpClient -> McpTool -> McpToolRegistration
    registrationFor client tool = McpToolRegistration
        { mcpRegistrationServer = client.clientConfig.mcpServerName
        , mcpRegistrationTool = appToolFor client tool
        }

    startupWarning :: McpServerConfig -> SomeException -> Text
    startupWarning config exception =
        "MCP server "
            <> config.mcpServerName
            <> " failed to start: "
            <> redactConfiguredValues config (exceptionSummary exception)

duplicateRegistration :: [McpToolRegistration] -> Maybe Text
duplicateRegistration = go Map.empty
  where
    go
        :: Map.Map Text Text
        -> [McpToolRegistration]
        -> Maybe Text
    go _ [] = Nothing
    go seen (registration : rest) =
        let tool = registration.mcpRegistrationTool
            name = tool.appToolName
            server = registration.mcpRegistrationServer
        in case Map.lookup name seen of
            Nothing -> go (Map.insert name server seen) rest
            Just previous ->
                Just
                    ( "duplicate MCP tool name "
                        <> name
                        <> " from servers "
                        <> previous
                        <> " and "
                        <> server
                    )

closeMcpFleet :: McpFleet -> IO ()
closeMcpFleet fleet =
    modifyMVar_ fleet.mcpFleetClosed \closed ->
        if closed
            then pure True
            else do
                mapM_ closeMcpClient fleet.mcpFleetClients
                pure True

startMcpClient :: McpServerConfig -> IO McpClient
startMcpClient config = mask \restore -> do
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
            pending <- newTVarIO Map.empty
            failure <- newTVarIO Nothing
            writeLock <- newMVar ()
            stderrRef <- newIORef emptyCapturedStderr
            closed <- newMVar False
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
                    }
                cleanup = closeMcpClient client
            restore (initializeClient client) `onException` cleanup
            pure client
        _ -> do
            let (_, _, _, processHandle) = created
            groupId <- getPid processHandle
            terminateProcessGroup groupId processHandle
            closeOptionalHandles created
            ioError (userError "MCP server did not provide all stdio pipes")

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
    { appToolName = tool.discoveredName
    , appToolDescription = tool.discoveredDescription
    , appToolSchema = RawJsonFunctionSchema tool.discoveredInputSchema
    , appToolHandler =
        typedTool tool.discoveredName \arguments -> do
            let parameters = object
                    [ "name" .= tool.discoveredName
                    , "arguments" .= (arguments :: Value)
                    ]
                timeoutMicros =
                    secondsToMicros
                        client.clientConfig.mcpServerRequestTimeoutSeconds
            requestMcp client timeoutMicros "tools/call" parameters >>= \case
                Left err -> pure (Left err)
                Right result -> pure (normalizeMcpToolResult result)
    , appToolApproval = AlwaysReadOnly
    , appToolExecution = ParallelSafe
    }

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
                modifyTVar' client.clientPending (Map.insert requestId response)
            let message = object
                    [ "jsonrpc" .= ("2.0" :: Text)
                    , "id" .= requestId
                    , "method" .= method
                    , "params" .= parameters
                    ]
            sendMessage client message >>= \case
                Left err -> do
                    atomically $
                        modifyTVar' client.clientPending (Map.delete requestId)
                    pure (Left err)
                Right () -> do
                    timed <- timeout (max 1 timeoutMicros)
                        (atomically (takeTMVar response))
                    case timed of
                        Just value -> pure value
                        Nothing -> do
                            atomically $
                                modifyTVar' client.clientPending
                                    (Map.delete requestId)
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
    -> TVar (Map.Map Int (TMVar (Either Text Value)))
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
    :: TVar (Map.Map Int (TMVar (Either Text Value)))
    -> Value
    -> IO ()
routeResponse pending (Object fields) =
    case KeyMap.lookup "id" fields >>= responseId of
        Nothing -> pure ()
        Just ident -> do
            destination <- atomically do
                current <- readTVar pending
                writeTVar pending (Map.delete ident current)
                pure (Map.lookup ident current)
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
    :: TVar (Map.Map Int (TMVar (Either Text Value)))
    -> TVar (Maybe Text)
    -> Text
    -> IO ()
failClient = failPending

failPending
    :: TVar (Map.Map Int (TMVar (Either Text Value)))
    -> TVar (Maybe Text)
    -> Text
    -> IO ()
failPending pending failure err =
    atomically do
        existing <- readTVar failure
        when (existing == Nothing) (writeTVar failure (Just err))
        requests <- readTVar pending
        writeTVar pending Map.empty
        mapM_ (\response -> void (tryPutTMVar response (Left err)))
            (Map.elems requests)

closeMcpClient :: McpClient -> IO ()
closeMcpClient client =
    modifyMVar_ client.clientClosed \closed ->
        if closed
            then pure True
            else do
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
