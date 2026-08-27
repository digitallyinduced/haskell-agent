-- | Session-owned stdio Language Server Protocol clients for Grok's @lsp@ tool.
module Agent.CLI.Lsp
    ( LspRuntime
    , LspStartup(..)
    , newLspRuntime
    , closeLspRuntime
    , lspRuntimeTool
    , encodeLspFrame
    ) where
import Agent.CLI.Config
    ( LspConfig(..)
    , LspServerConfig(..)
    )
import Agent.CLI.Lsp.Capabilities (clientCapabilities)
import Agent.CLI.FileUri
    ( fileUri
    )
import Agent.CLI.Lsp.Formatting (formatLspResult)
import Agent.CLI.Lsp.Path
    ( exceptionText
    , pathWithin
    , quote
    , resolveWorkspace
    , sanitizeName
    )
import Agent.CLI.Lsp.Protocol
    ( encodeLspFrame
    , readMessage
    , sendMessage
    )
import Agent.GrokBuild.Dialect.Lsp
    ( LspOperation(..)
    , LspRequest(..)
    , lspOperationName
    , lspTool
    )
import Agent.OsPath (unsafeToFilePath)
import Agent.Tools.IO (terminateProcessGroup)
import Agent.Tools.Types (AppTool, ToolEnv(..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
    ( Async
    , asyncWithUnmask
    , cancel
    , race
    , waitCatch
    )
import Control.Concurrent.MVar
    ( MVar
    , newMVar
    , withMVar
    )
import Control.Exception.Safe
    ( finally
    , mask
    , onException
    , tryAny
    )
import Control.Monad
    ( forM
    , unless
    , void
    , when
    )
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
    ( IORef
    , atomicModifyIORef'
    , newIORef
    , readIORef
    , writeIORef
    )
import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Vector as Vector
import System.Directory
    ( canonicalizePath
    , createDirectoryIfMissing
    )
import System.Environment (getEnvironment)
import System.Exit (ExitCode)
import qualified System.FilePath as FilePath
import System.IO
    ( BufferMode(NoBuffering)
    , Handle
    , IOMode(AppendMode)
    , hClose
    , hFlush
    , hSetBinaryMode
    , hSetBuffering
    , openFile
    )
import System.Posix.Files (setFileMode)
import System.Process
    ( CreateProcess(..)
    , ProcessHandle
    , StdStream(CreatePipe)
    , createProcess
    , getProcessExitCode
    , getPid
    , proc
    , waitForProcess
    )
import System.Timeout (timeout)
import System.Posix.Types (ProcessID)
data LspStartup = LspStartup
    { lspStartupRuntime :: !(Maybe LspRuntime)
    , lspStartupWarnings :: ![Text]
    }
data LspRuntime = LspRuntime
    { runtimeClients :: !(Map Text LspClient)
    , runtimeWorkspace :: !FilePath
    }
data LspClient = LspClient
    { clientName :: !Text
    , clientConfig :: !LspServerConfig
    , clientWorkspace :: !FilePath
    , clientInput :: !Handle
    , clientOutput :: !Handle
    , clientLog :: !Handle
    , clientStderrWorker :: !(Async ())
    , clientProcess :: !ProcessHandle
    , clientGroupId :: !(Maybe ProcessID)
    , clientNextRequestId :: !(IORef Int)
    , clientDocumentVersions :: !(IORef (Map FilePath Int))
    , clientOperationLock :: !(MVar ())
    , clientRequestLock :: !(MVar ())
    , clientClosed :: !(IORef Bool)
    }
newLspRuntime :: LspConfig -> ToolEnv -> IO LspStartup
newLspRuntime config env
    | not config.lspEnabled =
        pure LspStartup
            { lspStartupRuntime = Nothing
            , lspStartupWarnings = []
            }
    | otherwise = do
        scratch <- readIORef env.toolSessionTmp
        case scratch of
            Nothing ->
                pure LspStartup
                    { lspStartupRuntime = Nothing
                    , lspStartupWarnings =
                        [ "LSP tools are enabled, but session scratch storage \
                          \is unavailable."
                        ]
                    }
            Just scratchRoot -> do
                let workspace = unsafeToFilePath env.toolCwd
                    logDirectory =
                        unsafeToFilePath scratchRoot
                            FilePath.</> "lsp"
                directoryResult <- tryAny do
                    createDirectoryIfMissing True logDirectory
                    setFileMode logDirectory 0o700
                case directoryResult of
                    Left exception ->
                        pure LspStartup
                            { lspStartupRuntime = Nothing
                            , lspStartupWarnings =
                                [ "Failed to prepare LSP scratch directory: "
                                    <> exceptionText exception
                                ]
                            }
                    Right () -> do
                        startedClients <- newIORef []
                        attempts <-
                            (forM
                                (Map.toAscList config.lspServers)
                                \(name, server) ->
                                    mask \restore -> do
                                        attempt <-
                                            restore
                                                (startLspClient
                                                    workspace
                                                    logDirectory
                                                    name
                                                    server)
                                        case snd attempt of
                                            Right client ->
                                                atomicModifyIORef'
                                                    startedClients
                                                    (\clients ->
                                                        (client : clients, ()))
                                            Left _ -> pure ()
                                        pure attempt)
                                `onException`
                                    (readIORef startedClients
                                        >>= mapM_ forceCloseLspClient)
                        let clients =
                                Map.fromList
                                    [ (name, client)
                                    | (name, Right client) <- attempts
                                    ]
                            warnings =
                                [ "LSP server "
                                    <> quote name
                                    <> " was not started: "
                                    <> err
                                | (name, Left err) <- attempts
                                ]
                            runtime
                                | Map.null clients = Nothing
                                | otherwise =
                                    Just LspRuntime
                                        { runtimeClients = clients
                                        , runtimeWorkspace = workspace
                                        }
                        pure LspStartup
                            { lspStartupRuntime = runtime
                            , lspStartupWarnings =
                                if Map.null config.lspServers
                                    then
                                        [ "LSP tools are enabled, but no \
                                          \language servers are configured."
                                        ]
                                    else warnings
                            }
closeLspRuntime :: LspRuntime -> IO ()
closeLspRuntime runtime =
    mapM_ closeLspClient (Map.elems runtime.runtimeClients)
lspRuntimeTool :: LspRuntime -> AppTool
lspRuntimeTool runtime =
    lspTool (runLsp runtime)
startLspClient
    :: FilePath
    -> FilePath
    -> Text
    -> LspServerConfig
    -> IO (Text, Either Text LspClient)
startLspClient workspace logDirectory name config = do
    rootResult <- resolveWorkspace workspace config.lspWorkspaceFolder
    case rootResult of
        Left err -> pure (name, Left err)
        Right serverWorkspace -> do
            let logPath =
                    logDirectory
                        FilePath.</> sanitizeName name
                        FilePath.<.> "stderr.log"
            logResult <- tryAny do
                logHandle <- openFile logPath AppendMode
                setFileMode logPath 0o600
                pure logHandle
            case logResult of
                Left exception ->
                    pure
                        ( name
                        , Left
                            ( "failed to open stderr log: "
                                <> exceptionText exception
                            )
                        )
                Right logHandle -> do
                    started <-
                        tryAny
                            (spawnClient
                                name config serverWorkspace logHandle)
                    case started of
                        Left exception -> do
                            ignoreException (hClose logHandle)
                            pure
                                ( name
                                , Left
                                    ( "failed to spawn command: "
                                        <> exceptionText exception
                                    )
                                )
                        Right client -> do
                            initialized <-
                                initializeClient client
                            case initialized of
                                Left err -> do
                                    forceCloseLspClient client
                                    pure (name, Left err)
                                Right () -> pure (name, Right client)
spawnClient
    :: Text
    -> LspServerConfig
    -> FilePath
    -> Handle
    -> IO LspClient
spawnClient name config workspace logHandle = mask \restore -> do
    parentEnvironment <- getEnvironment
    let configuredEnvironment =
            Map.fromList
                [ (Text.unpack key, Text.unpack value)
                | (key, value) <- Map.toList config.lspEnv
                ]
        environment =
            Map.toList
                (configuredEnvironment
                    <> Map.fromList parentEnvironment)
        command =
            proc
                (Text.unpack config.lspCommand)
                (map Text.unpack config.lspArgs)
    (maybeInput, maybeOutput, maybeError, process) <-
        restore $ createProcess command
            { cwd = Just workspace
            , env = Just environment
            , std_in = CreatePipe
            , std_out = CreatePipe
            , std_err = CreatePipe
            , create_group = True
            }
    groupId <- getPid process
    let closePartial = do
            terminateProcessGroup groupId process
            mapM_
                (\handle -> mapM_ (ignoreException . hClose) handle)
                [maybeInput, maybeOutput, maybeError]
    restore
        (do
            input <-
                maybe
                    (ioError
                        (userError "LSP stdin pipe was not created"))
                    pure
                    maybeInput
            output <-
                maybe
                    (ioError
                        (userError "LSP stdout pipe was not created"))
                    pure
                    maybeOutput
            errorOutput <-
                maybe
                    (ioError
                        (userError "LSP stderr pipe was not created"))
                    pure
                    maybeError
            hSetBinaryMode input True
            hSetBinaryMode output True
            hSetBinaryMode errorOutput True
            hSetBuffering input NoBuffering
            hSetBuffering output NoBuffering
            hSetBuffering errorOutput NoBuffering
            stderrWorker <-
                asyncWithUnmask \unmask ->
                    unmask
                        (drainStderrCapped logHandle errorOutput
                            `finally`
                                ignoreException (hClose errorOutput))
            let finishSetup = do
                    nextId <- newIORef 1
                    documentVersions <- newIORef Map.empty
                    operationLock <- newMVar ()
                    requestLock <- newMVar ()
                    closed <- newIORef False
                    pure LspClient
                        { clientName = name
                        , clientConfig = config
                        , clientWorkspace = workspace
                        , clientInput = input
                        , clientOutput = output
                        , clientLog = logHandle
                        , clientStderrWorker = stderrWorker
                        , clientProcess = process
                        , clientGroupId = groupId
                        , clientNextRequestId = nextId
                        , clientDocumentVersions =
                            documentVersions
                        , clientOperationLock = operationLock
                        , clientRequestLock = requestLock
                        , clientClosed = closed
                        }
            finishSetup
                `onException` stopAsync stderrWorker)
        `onException` closePartial
initializeClient :: LspClient -> IO (Either Text ())
initializeClient client = do
    let rootUri = fileUri client.clientWorkspace
        rootName =
            Text.pack
                (let value = FilePath.takeFileName client.clientWorkspace
                 in if null value then "workspace" else value)
        params =
            Aeson.object
                [ "processId" .= Aeson.Null
                , "clientInfo" .= Aeson.object
                    [ "name" .= ("haskell-agent" :: Text)
                    ]
                , "rootUri" .= rootUri
                , "workspaceFolders" .=
                    [ Aeson.object
                        [ "uri" .= rootUri
                        , "name" .= rootName
                        ]
                    ]
                , "capabilities" .= clientCapabilities
                , "initializationOptions"
                    .= fromMaybe Aeson.Null
                        client.clientConfig.lspInitializationOptions
                ]
    requestClient
        client
        client.clientConfig.lspStartupTimeoutMilliseconds
        "initialize"
        params >>= \case
            Left err -> pure (Left ("initialize failed: " <> err))
            Right _ -> do
                initialized <-
                    sendNotificationWithin
                        client
                        client.clientConfig.lspStartupTimeoutMilliseconds
                        "initialized"
                        (Aeson.object [])
                case initialized of
                    Left err ->
                        pure
                            (Left
                                ("post-initialize notification failed: "
                                    <> err))
                    Right () ->
                        case client.clientConfig.lspSettings of
                            Nothing -> pure (Right ())
                            Just settings ->
                                sendNotificationWithin
                                    client
                                    client.clientConfig.lspStartupTimeoutMilliseconds
                                    "workspace/didChangeConfiguration"
                                    (Aeson.object
                                        ["settings" .= settings]) >>= \case
                                            Left err ->
                                                pure
                                                    (Left
                                                        ("settings notification failed: "
                                                            <> err))
                                            Right () -> pure (Right ())
runLsp :: LspRuntime -> LspRequest -> IO (Either Text Text)
runLsp runtime request =
    case request.lspOperation of
        WorkspaceSymbol -> runWorkspaceSymbol runtime request
        operation -> do
            prepared <- prepareFileRequest runtime request
            case prepared of
                Left err -> pure (Left err)
                Right (client, path, uri) ->
                    dispatchFileOperation client path uri request operation
runWorkspaceSymbol
    :: LspRuntime
    -> LspRequest
    -> IO (Either Text Text)
runWorkspaceSymbol runtime request =
    case Text.strip <$> request.lspQuery of
        Nothing -> pure (Left "workspaceSymbol requires query")
        Just query | Text.null query ->
            pure (Left "workspaceSymbol requires a non-empty query")
        Just query -> do
            results <-
                forM
                    (Map.toAscList runtime.runtimeClients)
                    \(name, client) -> do
                        result <-
                            requestClient client requestTimeoutMilliseconds
                                "workspace/symbol"
                                (Aeson.object ["query" .= query])
                        pure (name, result)
            let successes =
                    [ (name, value)
                    | (name, Right value) <- results
                    ]
            if null successes
                then
                    pure . Left $
                        "workspaceSymbol failed for every configured server: "
                            <> Text.intercalate "; "
                                [ name <> ": " <> err
                                | (name, Left err) <- results
                                ]
                else
                    pure . Right . Text.intercalate "\n\n" $
                        [ "## " <> name <> "\n"
                            <> formatLspResult WorkspaceSymbol value
                        | (name, value) <- successes
                        ]
prepareFileRequest
    :: LspRuntime
    -> LspRequest
    -> IO (Either Text (LspClient, FilePath, Text))
prepareFileRequest runtime request =
    case request.lspFilePath of
        Nothing ->
            pure
                (Left
                    ( lspOperationName request.lspOperation
                        <> " requires file_path"
                    ))
        Just rawPath -> do
            let path = Text.unpack rawPath
            if not (FilePath.isAbsolute path)
                then pure (Left "lsp file_path must be absolute")
                else do
                    canonicalResult <-
                        tryAny $
                            (,)
                                <$> canonicalizePath runtime.runtimeWorkspace
                                <*> canonicalizePath path
                    case canonicalResult of
                        Left exception ->
                            pure . Left $
                                "lsp could not resolve file_path: "
                                    <> exceptionText exception
                        Right (workspace, canonical)
                            | not
                                (pathWithin
                                    workspace
                                    canonical) ->
                                pure
                                    (Left
                                        "lsp file_path must be inside the \
                                        \active workspace")
                            | otherwise ->
                                case clientForPath
                                    runtime.runtimeClients canonical
                                of
                                    Nothing ->
                                        pure . Left $
                                            "No initialized LSP server is \
                                            \configured for "
                                                <> Text.pack
                                                    (FilePath.takeExtension
                                                        canonical)
                                    Just client
                                        | not
                                            (pathWithin
                                                client.clientWorkspace
                                                canonical) ->
                                            pure . Left $
                                                "lsp file_path is outside the \
                                                \configured server workspace for "
                                                    <> client.clientName
                                    Just client ->
                                        pure
                                            (Right
                                                ( client
                                                , canonical
                                                , fileUri canonical
                                                ))
dispatchFileOperation
    :: LspClient
    -> FilePath
    -> Text
    -> LspRequest
    -> LspOperation
    -> IO (Either Text Text)
dispatchFileOperation client path uri request operation =
    withMVar client.clientOperationLock \() ->
        synchronizeDocument client path uri >>= \case
            Left err -> pure (Left err)
            Right () ->
                case operation of
                    DocumentSymbol ->
                        run "textDocument/documentSymbol"
                            (Aeson.object
                                [ "textDocument" .=
                                    Aeson.object ["uri" .= uri]
                                ])
                    GoToDefinition ->
                        position "textDocument/definition"
                    FindReferences ->
                        positionWith
                            "textDocument/references"
                            ["context" .=
                                Aeson.object
                                    ["includeDeclaration" .= True]]
                    Hover -> position "textDocument/hover"
                    GoToImplementation ->
                        position "textDocument/implementation"
                    WorkspaceSymbol ->
                        pure (Left "internal LSP dispatch error")
  where
    position method = positionWith method []
    positionWith method extras =
        case (request.lspLine, request.lspCharacter) of
            (Just line, Just character)
                | line >= 0 && character >= 0 ->
                    run method
                        (Aeson.object
                            ( [ "textDocument" .=
                                    Aeson.object ["uri" .= uri]
                              , "position" .= Aeson.object
                                    [ "line" .= line
                                    , "character" .= character
                                    ]
                              ]
                                <> extras
                            ))
            _ ->
                pure . Left $
                    lspOperationName operation
                        <> " requires non-negative line and character"
    run method params =
        requestClient client requestTimeoutMilliseconds method params
            >>= \case
                Left err -> pure (Left err)
                Right value ->
                    pure (Right (formatLspResult operation value))
synchronizeDocument
    :: LspClient
    -> FilePath
    -> Text
    -> IO (Either Text ())
synchronizeDocument client path uri = do
    contentResult <- tryAny (BS.readFile path)
    case contentResult of
        Left exception ->
            pure . Left $
                "lsp could not read file contents: "
                    <> exceptionText exception
        Right bytes
            | BS.length bytes > maxLspDocumentBytes ->
                pure . Left $
                    "lsp document exceeds "
                        <> Text.pack (show maxLspDocumentBytes)
                        <> " bytes"
            | otherwise -> do
                versions <-
                    readIORef client.clientDocumentVersions
                let previous = Map.lookup path versions
                    version = maybe 1 (+ 1) previous
                    content =
                        Text.decodeUtf8With lenientDecode bytes
                    extension =
                        Text.pack (FilePath.takeExtension path)
                    languageId =
                        Map.findWithDefault
                            ""
                            extension
                            client.clientConfig.lspExtensionToLanguage
                    notification =
                        case previous of
                            Nothing ->
                                ( "textDocument/didOpen"
                                , Aeson.object
                                    [ "textDocument" .= Aeson.object
                                        [ "uri" .= uri
                                        , "languageId" .= languageId
                                        , "version" .= version
                                        , "text" .= content
                                        ]
                                    ]
                                )
                            Just _ ->
                                ( "textDocument/didChange"
                                , Aeson.object
                                    [ "textDocument" .= Aeson.object
                                        [ "uri" .= uri
                                        , "version" .= version
                                        ]
                                    , "contentChanges" .=
                                        [ Aeson.object
                                            ["text" .= content]
                                        ]
                                    ]
                                )
                sent <-
                    uncurry
                        (sendNotificationWithin
                            client
                            requestTimeoutMilliseconds)
                        notification
                case sent of
                    Left err ->
                        pure . Left $
                            "failed to synchronize file with LSP server: "
                                <> err
                    Right () -> do
                        writeIORef
                            client.clientDocumentVersions
                            (Map.insert path version versions)
                        pure (Right ())

requestTimeoutMilliseconds :: Int
requestTimeoutMilliseconds = 30000

maxLspDocumentBytes :: Int
maxLspDocumentBytes = 5 * 1024 * 1024

clientForPath :: Map Text LspClient -> FilePath -> Maybe LspClient
clientForPath clients path =
    snd <$> find handlesExtension (Map.toAscList clients)
  where
    extension = Text.pack (FilePath.takeExtension path)
    handlesExtension (_, client) =
        pathWithin client.clientWorkspace path
            && Map.member extension
                client.clientConfig.lspExtensionToLanguage

requestClient
    :: LspClient
    -> Int
    -> Text
    -> Aeson.Value
    -> IO (Either Text Aeson.Value)
requestClient client timeoutMilliseconds method params =
    withMVar client.clientRequestLock \() -> do
        closed <- readIORef client.clientClosed
        if closed
            then pure (Left "LSP server is closed")
            else do
                processState <- getProcessExitCode client.clientProcess
                case processState of
                    Just exitCode ->
                        pure
                            (Left
                                ( "LSP server exited: "
                                    <> Text.pack (show exitCode)
                                ))
                    Nothing -> do
                        requestId <-
                            atomicModifyIORef'
                                client.clientNextRequestId
                                \current -> (current + 1, current)
                        sent <-
                            sendMessageWithin
                                client
                                timeoutMilliseconds
                                ("request " <> method)
                                (Aeson.object
                                    [ "jsonrpc" .= ("2.0" :: Text)
                                    , "id" .= requestId
                                    , "method" .= method
                                    , "params" .= params
                                    ])
                        case sent of
                            Left err -> pure (Left err)
                            Right () -> do
                                response <-
                                    timeout
                                        (timeoutMilliseconds * 1000)
                                        (awaitResponse client requestId)
                                pure case response of
                                    Nothing ->
                                        Left
                                            ( "LSP request "
                                                <> method
                                                <> " timed out after "
                                                <> Text.pack
                                                    (show timeoutMilliseconds)
                                                <> " ms"
                                            )
                                    Just result -> result

awaitResponse
    :: LspClient
    -> Int
    -> IO (Either Text Aeson.Value)
awaitResponse client requestId =
    readMessage client.clientOutput >>= \case
        Left err -> pure (Left err)
        Right value ->
            case value of
                Aeson.Object object
                    | Just incomingId <- KeyMap.lookup "id" object
                    , incomingId == Aeson.toJSON requestId ->
                        pure (decodeResponse object)
                    | Just (Aeson.String method) <-
                        KeyMap.lookup "method" object
                    , Just serverRequestId <- KeyMap.lookup "id" object -> do
                        answerServerRequest
                            client method serverRequestId
                            (fromMaybe Aeson.Null
                                (KeyMap.lookup "params" object))
                        awaitResponse client requestId
                    | otherwise ->
                        awaitResponse client requestId
                _ -> awaitResponse client requestId

decodeResponse
    :: KeyMap.KeyMap Aeson.Value
    -> Either Text Aeson.Value
decodeResponse object =
    case KeyMap.lookup "error" object of
        Just errorValue ->
            Left ("LSP server returned an error: " <> compactJson errorValue)
        Nothing ->
            Right
                (fromMaybe Aeson.Null (KeyMap.lookup "result" object))

compactJson :: Aeson.Value -> Text
compactJson =
    Text.decodeUtf8With lenientDecode
        . LBS.toStrict
        . Aeson.encode

answerServerRequest
    :: LspClient
    -> Text
    -> Aeson.Value
    -> Aeson.Value
    -> IO ()
answerServerRequest client method requestId params =
    sendMessage client.clientInput $
        case method of
            "workspace/configuration" ->
                success (Aeson.toJSON (replicate configurationCount Aeson.Null))
            "client/registerCapability" -> success Aeson.Null
            "client/unregisterCapability" -> success Aeson.Null
            "window/workDoneProgress/create" -> success Aeson.Null
            "window/showMessageRequest" -> success Aeson.Null
            "workspace/workspaceFolders" ->
                success . Aeson.toJSON $
                    [ Aeson.object
                        [ "uri" .= fileUri client.clientWorkspace
                        , "name" .=
                            Text.pack
                                (FilePath.takeFileName
                                    client.clientWorkspace)
                        ]
                    ]
            "workspace/applyEdit" ->
                success
                    (Aeson.object
                        [ "applied" .= False
                        , "failureReason"
                            .= ("The lsp tool is read-only." :: Text)
                        ])
            _ ->
                Aeson.object
                    [ "jsonrpc" .= ("2.0" :: Text)
                    , "id" .= requestId
                    , "error" .= Aeson.object
                        [ "code" .= (-32601 :: Int)
                        , "message"
                            .= ("Method not supported by haskell-agent" :: Text)
                        ]
                    ]
  where
    success result =
        Aeson.object
            [ "jsonrpc" .= ("2.0" :: Text)
            , "id" .= requestId
            , "result" .= result
            ]
    configurationCount =
        case params of
            Aeson.Object object ->
                case KeyMap.lookup "items" object of
                    Just (Aeson.Array items) ->
                        min maxLspConfigurationItems
                            (Vector.length items)
                    _ -> 0
            _ -> 0

    maxLspConfigurationItems = 256

sendNotificationWithin
    :: LspClient
    -> Int
    -> Text
    -> Aeson.Value
    -> IO (Either Text ())
sendNotificationWithin client timeoutMilliseconds method params =
    sendMessageWithin
        client
        timeoutMilliseconds
        ("notification " <> method)
        (Aeson.object
            [ "jsonrpc" .= ("2.0" :: Text)
            , "method" .= method
            , "params" .= params
            ])

sendMessageWithin
    :: LspClient
    -> Int
    -> Text
    -> Aeson.Value
    -> IO (Either Text ())
sendMessageWithin client timeoutMilliseconds label value =
    timeout
        (max 1 timeoutMilliseconds * 1000)
        (tryAny (sendMessage client.clientInput value)) >>= \case
            Nothing -> do
                forceCloseLspClient client
                pure . Left $
                    "LSP " <> label <> " write timed out after "
                        <> Text.pack (show timeoutMilliseconds)
                        <> " ms; the server was closed"
            Just (Left exception) -> do
                forceCloseLspClient client
                pure . Left $
                    "failed to write LSP "
                        <> label
                        <> ": "
                        <> exceptionText exception
            Just (Right ()) -> pure (Right ())

closeLspClient :: LspClient -> IO ()
closeLspClient client =
    withMVar client.clientRequestLock \() -> do
        alreadyClosed <- readIORef client.clientClosed
        unless alreadyClosed do
            writeIORef client.clientClosed True
            requestId <-
                atomicModifyIORef'
                    client.clientNextRequestId
                    \current -> (current + 1, current)
            sent <-
                sendMessageWithin
                    client
                    client.clientConfig.lspShutdownTimeoutMilliseconds
                    "shutdown request"
                    (Aeson.object
                        [ "jsonrpc" .= ("2.0" :: Text)
                        , "id" .= requestId
                        , "method" .= ("shutdown" :: Text)
                        , "params" .= Aeson.Null
                        ])
            case sent of
                Left _ -> pure ()
                Right () ->
                    void $
                        timeout
                            ( client.clientConfig.lspShutdownTimeoutMilliseconds
                                * 1000
                            )
                            (awaitResponse client requestId)
            _ <-
                sendNotificationWithin
                    client
                    client.clientConfig.lspShutdownTimeoutMilliseconds
                    "exit"
                    Aeson.Null
            finishProcess client

forceCloseLspClient :: LspClient -> IO ()
forceCloseLspClient client = do
    shouldClose <-
        atomicModifyIORef'
            client.clientClosed
            (\closed -> (True, not closed))
    when shouldClose (finishProcess client)

finishProcess :: LspClient -> IO ()
finishProcess client = do
    ignoreException (hClose client.clientInput)
    stopped <-
        timeout
            ( client.clientConfig.lspShutdownTimeoutMilliseconds
                * 1000
            )
            (waitForProcess client.clientProcess)
    case stopped of
        Just (_ :: ExitCode) -> pure ()
        Nothing -> do
            ignoreException
                (terminateProcessGroup
                    client.clientGroupId
                    client.clientProcess)
            void . tryAny $ waitForProcess client.clientProcess
    stopAsync client.clientStderrWorker
    ignoreException (hClose client.clientOutput)
    ignoreException (hClose client.clientLog)

maxLspStderrBytes :: Int
maxLspStderrBytes = 1024 * 1024

drainStderrCapped :: Handle -> Handle -> IO ()
drainStderrCapped logHandle source = go 0
  where
    go written = do
        chunk <- BS.hGetSome source 4096
        unless (BS.null chunk) do
            let remaining = max 0 (maxLspStderrBytes - written)
                retained = BS.take remaining chunk
            unless (BS.null retained) do
                BS.hPut logHandle retained
                hFlush logHandle
            go (written + BS.length retained)

stopAsync :: Async a -> IO ()
stopAsync worker =
    race
        (threadDelay 1000000)
        (waitCatch worker) >>= \case
            Right _ -> pure ()
            Left () -> do
                cancel worker
                void (waitCatch worker)

ignoreException :: IO a -> IO ()
ignoreException action =
    void (tryAny action)
