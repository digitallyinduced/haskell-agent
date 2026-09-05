-- | Guest-side implementation of the tenant sandbox tool protocol.
--
-- This process is intended to run inside the tenant VM. Its standard input
-- and output are a dedicated, bounded NDJSON channel; callers must keep all
-- diagnostics on standard error.
module Agent.Server.Sandbox.Worker
    ( SandboxWorkerConfig(..)
    , runSandboxWorker
    , sandboxWorkerMain
    ) where

import Agent.CLI.Dialects
    ( CodingTools(..)
    , codingToolsFor
    )
import Agent.CLI.Session (isValidSessionId)
import Agent.Dialect
    ( DialectId
    , dialectForId
    , parseDialect
    )
import Agent.Server.Identifier
    ( isUUIDText
    , newUUIDv7Text
    )
import Agent.Server.Tenant
    ( TenantId
    , parseTenantId
    , renderTenantId
    )
import Agent.ToolDispatch
    ( ToolCall(..)
    , ToolCallMode(..)
    , ToolCallKind(..)
    , ToolCallResult(..)
    , ToolDispatchConfig(..)
    , withToolCallMode
    , ToolDispatchOutcome(..)
    , ToolResultImage(..)
    , toolCallResultImages
    )
import Agent.Tools.Types
    ( defaultToolEnv
    , dispatchRegisteredToolCallDetailed
    , executionToolsFromGroups
    , mkToolRegistry
    , setToolSessionTmp
    , ToolRegistry
    )
import Control.Exception.Safe
    ( finally
    , onException
    , tryAny
    )
import Control.Monad
    ( forM_
    , unless
    , when
    )
import Data.Aeson
    ( FromJSON(..)
    , Object
    , Value
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
import Data.List (minimumBy)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text qualified as Text
import Options.Applicative
    ( Parser
    , auto
    , execParser
    , fullDesc
    , header
    , help
    , helper
    , info
    , long
    , metavar
    , option
    , progDesc
    , showDefault
    , strOption
    , value
    , (<**>)
    )
import System.Directory
    ( canonicalizePath
    , createDirectoryIfMissing
    , doesDirectoryExist
    )
import System.Exit (exitFailure)
import System.FilePath
    ( makeRelative
    , normalise
    , splitDirectories
    , (</>)
    )
import System.IO
    ( BufferMode(NoBuffering)
    , Handle
    , hFlush
    , hPutStrLn
    , hSetBinaryMode
    , hSetBuffering
    , stderr
    , stdin
    , stdout
    )
import System.OsPath (unsafeEncodeUtf)
import System.Posix.Files (setFileMode)

protocolVersion :: Int
protocolVersion = 1

maximumRequestBytes :: Int
maximumRequestBytes = 4 * 1024 * 1024

maximumResponseBytes :: Int
maximumResponseBytes = 16 * 1024 * 1024

maximumStreamBytes :: Int
maximumStreamBytes = 8 * 1024 * 1024

data SandboxWorkerConfig = SandboxWorkerConfig
    { workerProtocolVersion :: !Int
    , workerTenantId :: !TenantId
    , workerWorkspace :: !FilePath
    , workerStateRoot :: !FilePath
    , workerMaximumSessions :: !Int
    }
    deriving (Eq, Show)

data WorkerRequest = WorkerRequest
    { requestTenantId :: !Text
    , requestGeneration :: !Text
    , requestId :: !Text
    , requestSessionId :: !Text
    , requestCwd :: !FilePath
    , requestDialect :: !Text
    , requestCall :: !ToolCall
    }

data WorkerSession = WorkerSession
    { sessionDialect :: !DialectId
    , sessionCwd :: !FilePath
    , sessionCoding :: !CodingTools
    , sessionRegistry :: !ToolRegistry
    , sessionLastUsed :: !Integer
    }

data WorkerState = WorkerState
    { stateSessions :: !(Map Text WorkerSession)
    , stateClock :: !Integer
    }

-- | Run one guest worker on dedicated protocol handles.
runSandboxWorker
    :: SandboxWorkerConfig
    -> Handle
    -> Handle
    -> IO (Either Text ())
runSandboxWorker config input output =
    validateWorkerConfig config >>= \case
        Left err -> pure (Left err)
        Right (workspace, stateRoot) -> do
            configureProtocolHandle input
            configureProtocolHandle output
            generation <- newUUIDv7Text
            sessions <- newIORef WorkerState
                { stateSessions = Map.empty
                , stateClock = 0
                }
            inputBuffer <- newIORef ByteString.empty
            let tenantId = renderTenantId config.workerTenantId
                ready =
                    object
                        [ "type" .= ("ready" :: Text)
                        , "version" .= protocolVersion
                        , "tenantId" .= tenantId
                        , "generation" .= generation
                        , "workspace" .= Text.pack config.workerWorkspace
                        , "state" .= Text.pack config.workerStateRoot
                        ]
                run = do
                    writeProtocolValue output ready >>= \case
                        Left err -> pure (Left err)
                        Right () ->
                            requestLoop
                                config
                                workspace
                                stateRoot
                                generation
                                sessions
                                inputBuffer
                                input
                                output
            run `finally` closeWorkerSessions sessions

requestLoop
    :: SandboxWorkerConfig
    -> FilePath
    -> FilePath
    -> Text
    -> IORef WorkerState
    -> IORef ByteString
    -> Handle
    -> Handle
    -> IO (Either Text ())
requestLoop
        config workspace stateRoot generation sessions inputBuffer input output =
    readBoundedLine input inputBuffer maximumRequestBytes >>= \case
        Left err -> pure (Left err)
        Right Nothing -> pure (Right ())
        Right (Just bytes) ->
            case eitherDecodeStrict' bytes of
                Left _ -> pure (Left "sandbox request is not valid JSON")
                Right value ->
                    case parseWorkerRequest value of
                        Left err -> pure (Left err)
                        Right request ->
                            case validateRequest config generation request of
                                Left err -> pure (Left err)
                                Right dialect -> do
                                    processRequest
                                        config
                                        workspace
                                        stateRoot
                                        sessions
                                        output
                                        request
                                        dialect >>= \case
                                            Left err -> pure (Left err)
                                            Right () ->
                                                requestLoop
                                                    config
                                                    workspace
                                                    stateRoot
                                                    generation
                                                    sessions
                                                    inputBuffer
                                                    input
                                                    output

processRequest
    :: SandboxWorkerConfig
    -> FilePath
    -> FilePath
    -> IORef WorkerState
    -> Handle
    -> WorkerRequest
    -> DialectId
    -> IO (Either Text ())
processRequest config workspace stateRoot sessions output request dialect = do
    resolvedCwd <- tryAny (canonicalizePath request.requestCwd)
    case resolvedCwd of
        Left _ ->
            pure (Left "sandbox request cwd does not exist")
        Right cwd
            | not (isWithin workspace cwd) ->
                pure (Left "sandbox request cwd escapes the workspace")
            | otherwise -> do
                acquired <-
                    tryAny
                        (acquireWorkerSession
                            config
                            stateRoot
                            sessions
                            request.requestSessionId
                            cwd
                            dialect)
                case acquired of
                    Left _ ->
                        writeFailure output request
                            "sandbox session initialization failed"
                    Right (Left err) -> writeFailure output request err
                    Right (Right workerSession) -> do
                        streamed <- newIORef (0 :: Int)
                        let dispatchConfig = workerDispatchConfig
                                output
                                request
                                streamed
                        outcome <-
                            dispatchRegisteredToolCallDetailed
                                dispatchConfig
                                workerSession.sessionRegistry
                                request.requestCall
                        writeOutcome output request outcome

acquireWorkerSession
    :: SandboxWorkerConfig
    -> FilePath
    -> IORef WorkerState
    -> Text
    -> FilePath
    -> DialectId
    -> IO (Either Text WorkerSession)
acquireWorkerSession config stateRoot stateRef sessionId cwd dialect = do
    state <- readIORef stateRef
    let clock = state.stateClock + 1
    case Map.lookup sessionId state.stateSessions of
        Just existing
            | existing.sessionDialect /= dialect
                || existing.sessionCwd /= cwd ->
                pure
                    (Left
                        "sandbox session metadata changed within one VM generation")
            | otherwise -> do
                let refreshed = existing { sessionLastUsed = clock }
                writeIORef stateRef state
                    { stateSessions =
                        Map.insert sessionId refreshed state.stateSessions
                    , stateClock = clock
                    }
                pure (Right refreshed)
        Nothing -> do
            stateWithCapacity <-
                ensureSessionCapacity
                    config.workerMaximumSessions
                    state
            let sessionTmp =
                    stateRoot </> "sessions"
                        </> Text.unpack sessionId
                        </> "tmp"
            createDirectoryIfMissing True sessionTmp
            setFileMode (stateRoot </> "sessions" </> Text.unpack sessionId) 0o700
            setFileMode sessionTmp 0o700
            env <- defaultToolEnv (unsafeEncodeUtf cwd)
            setToolSessionTmp env (Just (unsafeEncodeUtf sessionTmp))
            coding <-
                codingToolsFor
                    (dialectForId dialect)
                    env
                    Nothing
                    Nothing
                    Nothing
                    Nothing
            let executionTools =
                    executionToolsFromGroups coding.codingAppToolGroups
            case mkToolRegistry executionTools of
                Left err -> do
                    coding.codingClose
                    pure (Left err)
                Right registry -> do
                    coding.codingResetSessionTemp (unsafeEncodeUtf sessionTmp)
                        `onException` coding.codingClose
                    let created = WorkerSession
                            { sessionDialect = dialect
                            , sessionCwd = cwd
                            , sessionCoding = coding
                            , sessionRegistry = registry
                            , sessionLastUsed = clock
                            }
                        updated = stateWithCapacity
                            { stateSessions =
                                Map.insert
                                    sessionId
                                    created
                                    stateWithCapacity.stateSessions
                            , stateClock = clock
                            }
                    writeIORef stateRef updated
                    pure (Right created)

ensureSessionCapacity :: Int -> WorkerState -> IO WorkerState
ensureSessionCapacity maximumSessions state
    | Map.size state.stateSessions < maximumSessions = pure state
    | otherwise = do
        let (evictedId, evicted) =
                minimumBy
                    (comparing
                        (\(_, workerSession) ->
                            workerSession.sessionLastUsed))
                    (Map.toList state.stateSessions)
        evicted.sessionCoding.codingClose
        pure state
            { stateSessions = Map.delete evictedId state.stateSessions
            }

workerDispatchConfig
    :: Handle
    -> WorkerRequest
    -> IORef Int
    -> ToolDispatchConfig
workerDispatchConfig output request streamed = ToolDispatchConfig
    { toolDispatchUnknownTool = \name ->
        "tool is unavailable in the tenant sandbox: " <> name
    , toolDispatchFormatResult = either id id
    , toolDispatchFormatException = \name _ ->
        "tool failed inside the tenant sandbox: " <> name
    , toolDispatchOnException = \_ _ -> pure ()
    , toolDispatchOnOutput = \_ snapshot -> do
        let value = responseBase "output" request
                <> [ "output" .= snapshot ]
            bytes = encode (object value)
            size = fromIntegral (LazyByteString.length bytes)
        used <- readIORef streamed
        when
            (size <= maximumResponseBytes
                && used + size <= maximumStreamBytes) do
                    LazyByteString.hPut output bytes
                    LazyByteString.hPut output "\n"
                    hFlush output
                    writeIORef streamed (used + size)
    , toolDispatchFinalizeOutput = \_ result -> pure result
    }

writeOutcome
    :: Handle
    -> WorkerRequest
    -> ToolDispatchOutcome
    -> IO (Either Text ())
writeOutcome output request outcome =
    let result = outcome.toolDispatchResult
        value =
            object $
                responseBase "result" request
                    <> [ "ok" .= outcome.toolDispatchSucceeded
                       , "output" .= result.output
                       , "images" .= map imageValue
                            (toolCallResultImages result)
                       ]
    in writeProtocolValue output value >>= \case
        Right () -> pure (Right ())
        Left _ ->
            writeFailure output request
                "sandbox tool result exceeds the protocol limit"

writeFailure :: Handle -> WorkerRequest -> Text -> IO (Either Text ())
writeFailure output request message =
    writeProtocolValue output $
        object $
            responseBase "result" request
                <> [ "ok" .= False
                   , "output" .= message
                   , "images" .= ([] :: [Value])
                   ]

responseBase
    :: Text
    -> WorkerRequest
    -> [(Key, Value)]
responseBase messageType request =
    [ "type" .= messageType
    , "version" .= protocolVersion
    , "tenantId" .= request.requestTenantId
    , "generation" .= request.requestGeneration
    , "requestId" .= request.requestId
    ]

imageValue :: ToolResultImage -> Value
imageValue image =
    object
        [ "url" .= image.imageUrl
        , "detail" .= image.imageDetail
        ]

writeProtocolValue :: Handle -> Value -> IO (Either Text ())
writeProtocolValue output value =
    let bytes = encode value
    in if LazyByteString.length bytes
        > fromIntegral maximumResponseBytes
        then pure (Left "sandbox response exceeds the protocol limit")
        else do
            LazyByteString.hPut output bytes
            LazyByteString.hPut output "\n"
            hFlush output
            pure (Right ())

parseWorkerRequest :: Value -> Either Text WorkerRequest
parseWorkerRequest value =
    case AesonTypes.parseEither parseJSON value of
        Left _ -> Left "sandbox request does not match the protocol"
        Right request -> Right request

instance FromJSON WorkerRequest where
    parseJSON = withObject "WorkerRequest" \payload -> do
        rejectUnknownFields
            "WorkerRequest"
            [ "type", "version", "tenantId", "generation"
            , "requestId", "sessionId", "cwd", "dialect", "call"
            ]
            payload
        messageType <- payload .: "type"
        unless ((messageType :: Text) == "tool")
            (fail "unsupported sandbox request type")
        version <- payload .: "version"
        unless ((version :: Int) == protocolVersion)
            (fail "unsupported sandbox protocol version")
        WorkerRequest
            <$> payload .: "tenantId"
            <*> payload .: "generation"
            <*> payload .: "requestId"
            <*> payload .: "sessionId"
            <*> payload .: "cwd"
            <*> payload .: "dialect"
            <*> (payload .: "call" >>= parseToolCall)

parseToolCall :: Value -> AesonTypes.Parser ToolCall
parseToolCall = withObject "ToolCall" \payload -> do
    rejectUnknownFields
        "ToolCall"
        [ "id", "name", "arguments", "kind", "argumentsEncrypted", "async" ]
        payload
    encrypted <- payload .: "argumentsEncrypted"
    when encrypted (fail "encrypted sandbox tool arguments are unsupported")
    mode <- parseToolCallMode <$> payload .:? "async"
    call <- ToolCall
        <$> payload .: "id"
        <*> payload .: "name"
        <*> payload .: "arguments"
        <*> (payload .: "kind" >>= parseToolCallKind)
        <*> pure False
    pure (withToolCallMode mode call)

parseToolCallMode :: Maybe Bool -> ToolCallMode
parseToolCallMode = \case
    Just True -> AsyncToolCall
    _ -> BlockingToolCall

parseToolCallKind :: Text -> AesonTypes.Parser ToolCallKind
parseToolCallKind = \case
    "function" -> pure FunctionCallKind
    "custom" -> pure CustomCallKind
    "computer" -> pure ComputerCallKind
    "computer_function" -> pure ComputerFunctionCallKind
    _ -> fail "unsupported sandbox tool call kind"

validateRequest
    :: SandboxWorkerConfig
    -> Text
    -> WorkerRequest
    -> Either Text DialectId
validateRequest config generation request
    | request.requestTenantId /= renderTenantId config.workerTenantId =
        Left "sandbox request tenant mismatch"
    | request.requestGeneration /= generation =
        Left "sandbox request generation mismatch"
    | not (isUUIDText request.requestId) =
        Left "sandbox request id is invalid"
    | not (isValidSessionId request.requestSessionId) =
        Left "sandbox session id is invalid"
    | Text.null (Text.strip request.requestCall.callId) =
        Left "sandbox tool call id is empty"
    | Text.null (Text.strip request.requestCall.name) =
        Left "sandbox tool name is empty"
    | otherwise =
        maybe
            (Left "sandbox request dialect is unsupported")
            Right
            (parseDialect request.requestDialect)

validateWorkerConfig
    :: SandboxWorkerConfig
    -> IO (Either Text (FilePath, FilePath))
validateWorkerConfig config
    | config.workerProtocolVersion /= protocolVersion =
        pure (Left "unsupported sandbox worker protocol version")
    | config.workerMaximumSessions < 1 =
        pure (Left "sandbox worker session limit must be positive")
    | otherwise = do
        workspaceExists <- doesDirectoryExist config.workerWorkspace
        stateExists <- doesDirectoryExist config.workerStateRoot
        if not workspaceExists || not stateExists
            then pure (Left "sandbox workspace or state mount is unavailable")
            else do
                workspace <- canonicalizePath config.workerWorkspace
                stateRoot <- canonicalizePath config.workerStateRoot
                pure (Right (workspace, stateRoot))

isWithin :: FilePath -> FilePath -> Bool
isWithin root candidate =
    case splitDirectories
        (normalise (makeRelative (normalise root) (normalise candidate))) of
        ".." : _ -> False
        _ -> True

readBoundedLine
    :: Handle
    -> IORef ByteString
    -> Int
    -> IO (Either Text (Maybe ByteString))
readBoundedLine handle buffer limit = readIORef buffer >>= go [] 0
  where
    go chunks used bytes =
        case ByteString.elemIndex 10 bytes of
            Just newline -> do
                let line = ByteString.take newline bytes
                    remainder = ByteString.drop (newline + 1) bytes
                writeIORef buffer remainder
                if used + ByteString.length line > limit
                    then
                        pure
                            (Left
                                "sandbox request exceeds the protocol limit")
                    else
                        pure
                            (Right
                                (Just
                                    (ByteString.concat
                                        (reverse (line : chunks)))))
            Nothing
                | used + ByteString.length bytes > limit ->
                    pure
                        (Left
                            "sandbox request exceeds the protocol limit")
                | otherwise -> do
                    chunk <- ByteString.hGetSome handle 32768
                    if ByteString.null chunk
                        then
                            pure $
                                if used == 0 && ByteString.null bytes
                                    then Right Nothing
                                    else
                                        Left
                                            "sandbox request ended mid-message"
                        else
                            go
                                (bytes : chunks)
                                (used + ByteString.length bytes)
                                chunk

configureProtocolHandle :: Handle -> IO ()
configureProtocolHandle handle = do
    hSetBinaryMode handle True
    hSetBuffering handle NoBuffering

closeWorkerSessions :: IORef WorkerState -> IO ()
closeWorkerSessions sessions = do
    state <- readIORef sessions
    forM_ state.stateSessions \workerSession -> do
        _ <- tryAny workerSession.sessionCoding.codingClose
        pure ()

rejectUnknownFields
    :: String
    -> [Key]
    -> Object
    -> AesonTypes.Parser ()
rejectUnknownFields typeName allowed payload =
    case filter (`notElem` allowed) (KeyMap.keys payload) of
        [] -> pure ()
        unknown ->
            fail
                (typeName <> " contains unknown fields: " <> show unknown)

data SandboxWorkerOptions = SandboxWorkerOptions
    { optionProtocolVersion :: !Int
    , optionTenantId :: !String
    , optionWorkspace :: !FilePath
    , optionStateRoot :: !FilePath
    , optionMaximumSessions :: !Int
    }

sandboxWorkerOptions :: Parser SandboxWorkerOptions
sandboxWorkerOptions =
    SandboxWorkerOptions
        <$> option auto
            ( long "protocol-version"
                <> metavar "VERSION"
                <> value protocolVersion
                <> showDefault
                <> help "Sandbox broker protocol version"
            )
        <*> strOption
            ( long "tenant-id"
                <> metavar "UUID"
                <> help "Canonical tenant UUID"
            )
        <*> strOption
            ( long "workspace"
                <> metavar "PATH"
                <> value "/workspace"
                <> showDefault
                <> help "Guest workspace mount"
            )
        <*> strOption
            ( long "state"
                <> metavar "PATH"
                <> value "/state"
                <> showDefault
                <> help "Guest private state mount"
            )
        <*> option auto
            ( long "max-sessions"
                <> metavar "COUNT"
                <> value 64
                <> showDefault
                <> help "Maximum resident session tool runtimes"
            )

sandboxWorkerMain :: IO ()
sandboxWorkerMain = do
    options <-
        execParser $
            info
                (sandboxWorkerOptions <**> helper)
                ( fullDesc
                    <> progDesc
                        "Serve the guest side of the tenant sandbox protocol"
                    <> header "agent-sandbox-worker"
                )
    case parseTenantId (Text.pack options.optionTenantId) of
        Left err -> do
            hPutStrLn stderr (Text.unpack err)
            exitFailure
        Right tenantId ->
            runSandboxWorker
                SandboxWorkerConfig
                    { workerProtocolVersion = options.optionProtocolVersion
                    , workerTenantId = tenantId
                    , workerWorkspace = options.optionWorkspace
                    , workerStateRoot = options.optionStateRoot
                    , workerMaximumSessions = options.optionMaximumSessions
                    }
                stdin
                stdout >>= \case
                    Left err -> do
                        hPutStrLn stderr (Text.unpack err)
                        exitFailure
                    Right () -> pure ()
