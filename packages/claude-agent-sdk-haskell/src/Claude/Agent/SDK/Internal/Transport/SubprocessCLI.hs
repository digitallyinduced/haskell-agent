-- | Official Claude Code CLI subprocess transport.
module Claude.Agent.SDK.Internal.Transport.SubprocessCLI
    ( newSubprocessCLITransport
    , subprocessArguments
    ) where

import Claude.Agent.SDK.Errors
    ( ClaudeSDKError(..)
    )
import Claude.Agent.SDK.Internal.Process
    ( terminateProcessGroup
    )
import Claude.Agent.SDK.Transport
    ( Transport(..)
    , TransportMode(..)
    )
import Claude.Agent.SDK.Types
    ( ClaudeAgentOptions(..)
    , PermissionMode(..)
    , SystemPrompt(..)
    , permissionModeName
    )
import Control.Concurrent.Async
    ( Async
    , asyncWithUnmask
    , cancel
    , waitCatch
    )
import Control.Concurrent
    ( ThreadId
    , myThreadId
    , throwTo
    )
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar
    , modifyMVar_
    , newMVar
    , withMVar
    )
import Control.Exception (Exception, IOException)
import Control.Exception.Safe
    ( SomeException
    , catchAny
    , finally
    , fromException
    , mask
    , onException
    , throwIO
    , try
    , tryAny
    )
import Control.Monad (void)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString8
import Data.IORef
    ( IORef
    , atomicModifyIORef'
    , newIORef
    , readIORef
    , writeIORef
    )
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Text.Encoding.Error (lenientDecode)
import System.Directory (canonicalizePath)
import qualified System.Directory as Directory
import System.Environment (getEnvironment)
import System.Exit (ExitCode)
import System.IO
    ( BufferMode(NoBuffering)
    , Handle
    , hClose
    , hFlush
    , hSetBinaryMode
    , hSetBuffering
    )
import System.IO.Error (isDoesNotExistError, isEOFError)
import System.Posix.Types (ProcessGroupID)
import System.Process
    ( CreateProcess(..)
    , ProcessHandle
    , StdStream(CreatePipe)
    , createProcess
    , getPid
    , getProcessExitCode
    , proc
    , waitForProcess
    )
import System.Timeout (timeout)

data RunningTransport = RunningTransport
    { inputHandle :: !Handle
    , outputHandle :: !Handle
    , errorHandle :: !Handle
    , processHandle :: !ProcessHandle
    , groupId :: !(Maybe ProcessGroupID)
    , diagnosticBytes :: !(IORef ByteString.ByteString)
    , outputBytes :: !(IORef ByteString.ByteString)
    , outputReaderState :: !(MVar OutputReaderState)
    , inputWriterState :: !(MVar InputWriterState)
    , inputOpen :: !(IORef Bool)
    , errorDrain :: !(Async ())
    }

data InputWriterState = InputWriterState
    { inputWriterClosing :: !Bool
    , inputWriterThread :: !(Maybe ThreadId)
    }

data OutputReaderState = OutputReaderState
    { outputReaderClosing :: !Bool
    , outputReaderThread :: !(Maybe ThreadId)
    }

data InputWriteInterrupted = InputWriteInterrupted
    deriving (Show)

instance Exception InputWriteInterrupted

data OutputReadInterrupted = OutputReadInterrupted
    deriving (Show)

instance Exception OutputReadInterrupted

-- | Construct a disconnected subprocess transport. 'transportConnect'
-- performs the actual spawn.
newSubprocessCLITransport
    :: ClaudeAgentOptions
    -> TransportMode
    -> Maybe Text
    -> Maybe Text
    -> IO Transport
newSubprocessCLITransport options mode model effort = do
    stateRef <- newIORef Nothing
    lifecycleLock <- newMVar ()
    pure Transport
        { transportConnect =
            withMVar lifecycleLock \_ ->
                connectTransport stateRef options mode model effort
        , transportWrite = writeTransport stateRef options
        , transportRead = readTransport stateRef options
        , transportClose =
            withMVar lifecycleLock \_ ->
                closeTransport stateRef
        , transportIsReady = transportReady stateRef
        , transportEndInput =
            withMVar lifecycleLock \_ ->
                endInput stateRef
        , transportProcessExit = processExit stateRef
        , transportDiagnostic = diagnostic stateRef
        }

connectTransport
    :: IORef (Maybe RunningTransport)
    -> ClaudeAgentOptions
    -> TransportMode
    -> Maybe Text
    -> Maybe Text
    -> IO (Either ClaudeSDKError ())
connectTransport stateRef options mode model effort = do
    existing <- readIORef stateRef
    case existing of
        Just _ -> pure (Right ())
        Nothing -> do
            created <- tryAny (startTransport options mode model effort)
            case created of
                Left exception ->
                    pure $
                        Left (connectionException options.executable exception)
                Right running -> do
                    writeIORef stateRef (Just running)
                    pure (Right ())

startTransport
    :: ClaudeAgentOptions
    -> TransportMode
    -> Maybe Text
    -> Maybe Text
    -> IO RunningTransport
startTransport options mode model effort =
    mask \restore -> do
        workingDirectoryExists <-
            Directory.doesDirectoryExist options.cwd
        if workingDirectoryExists
            then pure ()
            else
                throwIO $
                    CLIConnectionError
                        ( "Claude Code working directory does not exist: "
                            <> Text.pack options.cwd
                        )
        workingDirectory <- canonicalizePath options.cwd
        environment <- prepareEnvironment options workingDirectory
        let processSpec =
                (proc
                    options.executable
                    (subprocessArguments options mode model effort))
                    { cwd = Just workingDirectory
                    , env = Just environment
                    , std_in = CreatePipe
                    , std_out = CreatePipe
                    , std_err = CreatePipe
                    , close_fds = True
                    , create_group = True
                    , new_session = True
                    }
        created <- createProcess processSpec
        let (maybeInput, maybeOutput, maybeError, processHandle) = created
            closeCreatedHandles =
                mapM_ closeHandleQuietly
                    [ handle
                    | Just handle <-
                        [maybeInput, maybeOutput, maybeError]
                    ]
            stopCreated processGroupId = do
                terminateProcessGroup processGroupId processHandle
                closeCreatedHandles
                waitForProcessQuietly processHandle
        processGroupId <-
            getPid processHandle
                `onException` stopCreated Nothing
        (inputHandle, outputHandle, errorHandle) <-
            case (maybeInput, maybeOutput, maybeError) of
                (Just input, Just output, Just err) ->
                    pure (input, output, err)
                _ -> do
                    stopCreated processGroupId
                    fail
                        "Claude Code did not provide all requested stdio pipes"
        (do
            mapM_ (`hSetBinaryMode` True)
                [inputHandle, outputHandle, errorHandle]
            mapM_ (`hSetBuffering` NoBuffering)
                [inputHandle, outputHandle, errorHandle])
            `onException` stopCreated processGroupId
        diagnosticRef <- newIORef ByteString.empty
        outputRef <- newIORef ByteString.empty
        outputReaderState <-
            newMVar OutputReaderState
                { outputReaderClosing = False
                , outputReaderThread = Nothing
                }
        inputWriterState <-
            newMVar InputWriterState
                { inputWriterClosing = False
                , inputWriterThread = Nothing
                }
        inputOpenRef <- newIORef True
        errorDrain <-
            (asyncWithUnmask \unmask ->
                unmask (drainDiagnostic errorHandle diagnosticRef)
                    `catchAny` \_ -> pure ())
                `onException` stopCreated processGroupId
        let running = RunningTransport
                { inputHandle
                , outputHandle
                , errorHandle
                , processHandle
                , groupId = processGroupId
                , diagnosticBytes = diagnosticRef
                , outputBytes = outputRef
                , outputReaderState
                , inputWriterState
                , inputOpen = inputOpenRef
                , errorDrain
                }
        restore (pure running)
            `onException` stopRunningTransport running

writeTransport
    :: IORef (Maybe RunningTransport)
    -> ClaudeAgentOptions
    -> ByteString.ByteString
    -> IO (Either ClaudeSDKError ())
writeTransport stateRef options bytes = do
    state <- readIORef stateRef
    case state of
        Nothing ->
            pure (Left (CLIConnectionError "Claude SDK transport is not connected."))
        Just running ->
            withInputWriter running do
                inputOpen <- readIORef running.inputOpen
                if not inputOpen
                    then
                        pure $
                            Left $
                                CLIConnectionError
                                    "Claude SDK transport input is closed."
                    else do
                        result <-
                            timeout
                                (max 1 options.promptWriteTimeoutMicros)
                                do
                                    ByteString.hPut running.inputHandle bytes
                                    hFlush running.inputHandle
                        pure case result of
                            Nothing ->
                                Left $
                                    CLIConnectionError
                                        "Claude Code did not accept input within the prompt-write timeout."
                            Just () ->
                                Right ()

withInputWriter
    :: RunningTransport
    -> IO (Either ClaudeSDKError ())
    -> IO (Either ClaudeSDKError ())
withInputWriter running action =
    mask \restore -> do
        threadId <- myThreadId
        registered <-
            modifyMVar
                running.inputWriterState
                \state ->
                    if state.inputWriterClosing
                        then
                            pure
                                ( state
                                , Left $
                                    CLIConnectionError
                                        "Claude SDK transport is closing."
                                )
                        else if state.inputWriterThread /= Nothing
                            then
                                pure
                                    ( state
                                    , Left $
                                        CLIProtocolError
                                            "Claude SDK transport already has an active writer."
                                    )
                            else
                                pure
                                    ( state
                                        { inputWriterThread =
                                            Just threadId
                                        }
                                    , Right ()
                                    )
        case registered of
            Left err ->
                pure (Left err)
            Right () -> do
                outcome <-
                    tryAny (restore action)
                        `finally`
                            unregisterInputWriter
                                running
                                threadId
                pure case outcome of
                    Left exception
                        | Just InputWriteInterrupted <-
                            fromException exception ->
                            Left $
                                CLIConnectionError
                                    "Claude SDK transport was closed while writing."
                        | otherwise ->
                            Left $
                                CLIConnectionError
                                    ( "Failed to write to Claude Code: "
                                        <> Text.pack (show exception)
                                    )
                    Right result ->
                        result

unregisterInputWriter :: RunningTransport -> ThreadId -> IO ()
unregisterInputWriter running threadId =
    modifyMVar_ running.inputWriterState \state ->
        pure
            if state.inputWriterThread == Just threadId
                then state { inputWriterThread = Nothing }
                else state

interruptInputWriter :: RunningTransport -> IO ()
interruptInputWriter running = do
    writer <-
        modifyMVar running.inputWriterState \state ->
            pure
                ( state { inputWriterClosing = True }
                , state.inputWriterThread
                )
    currentThread <- myThreadId
    case writer of
        Just writerThread
            | writerThread /= currentThread ->
                throwTo writerThread InputWriteInterrupted
        _ ->
            pure ()

readTransport
    :: IORef (Maybe RunningTransport)
    -> ClaudeAgentOptions
    -> IO (Either ClaudeSDKError (Maybe ByteString.ByteString))
readTransport stateRef options = do
    state <- readIORef stateRef
    case state of
        Nothing ->
            pure (Left (CLIConnectionError "Claude SDK transport is not connected."))
        Just running ->
            withOutputReader running $
                readBoundedLine
                    running
                    (max 1 options.maxBufferSizeBytes)

withOutputReader
    :: RunningTransport
    -> IO (Either ClaudeSDKError (Maybe ByteString.ByteString))
    -> IO (Either ClaudeSDKError (Maybe ByteString.ByteString))
withOutputReader running action =
    mask \restore -> do
        threadId <- myThreadId
        registered <-
            modifyMVar
                running.outputReaderState
                \state ->
                    if state.outputReaderClosing
                        then
                            pure
                                ( state
                                , Left $
                                    CLIConnectionError
                                        "Claude SDK transport is closing."
                                )
                        else if state.outputReaderThread /= Nothing
                            then
                                pure
                                    ( state
                                    , Left $
                                        CLIProtocolError
                                            "Claude SDK transport already has an active reader."
                                    )
                            else
                                pure
                                    ( state
                                        { outputReaderThread =
                                            Just threadId
                                        }
                                    , Right ()
                                    )
        case registered of
            Left err ->
                pure (Left err)
            Right () -> do
                outcome <-
                    tryAny (restore action)
                        `finally`
                            unregisterOutputReader
                                running
                                threadId
                case outcome of
                    Left exception
                        | Just OutputReadInterrupted <-
                            fromException exception ->
                            pure $
                                Left $
                                    CLIConnectionError
                                        "Claude SDK transport was closed while reading."
                        | otherwise ->
                            throwIO exception
                    Right result ->
                        pure result

unregisterOutputReader :: RunningTransport -> ThreadId -> IO ()
unregisterOutputReader running threadId =
    modifyMVar_ running.outputReaderState \state ->
        pure
            if state.outputReaderThread == Just threadId
                then state { outputReaderThread = Nothing }
                else state

interruptOutputReader :: RunningTransport -> IO ()
interruptOutputReader running = do
    reader <-
        modifyMVar running.outputReaderState \state ->
            pure
                ( state { outputReaderClosing = True }
                , state.outputReaderThread
                )
    currentThread <- myThreadId
    case reader of
        Just readerThread
            | readerThread /= currentThread ->
                throwTo readerThread OutputReadInterrupted
        _ ->
            pure ()

readBoundedLine
    :: RunningTransport
    -> Int
    -> IO (Either ClaudeSDKError (Maybe ByteString.ByteString))
readBoundedLine running maximumBytes =
    go
  where
    go = do
        buffered <- readIORef running.outputBytes
        case ByteString8.elemIndex '\n' buffered of
            Just newlineIndex -> do
                let (line, withNewline) =
                        ByteString.splitAt newlineIndex buffered
                writeIORef
                    running.outputBytes
                    (ByteString.drop 1 withNewline)
                if ByteString.length line > maximumBytes
                    then pure (Left oversizedRecordError)
                    else pure (Right (Just line))
            Nothing
                | ByteString.length buffered > maximumBytes ->
                    pure (Left oversizedRecordError)
                | otherwise -> do
                    let remainingBytes =
                            maximumBytes - ByteString.length buffered
                        readSize =
                            min 8_192 (remainingBytes + 1)
                    result <-
                        try
                            (ByteString.hGetSome
                                running.outputHandle
                                readSize)
                            :: IO
                                (Either
                                    IOException
                                    ByteString.ByteString)
                    case result of
                        Right chunk
                            | ByteString.null chunk ->
                                if ByteString.null buffered
                                    then pure (Right Nothing)
                                    else do
                                        writeIORef
                                            running.outputBytes
                                            ByteString.empty
                                        if ByteString.length buffered
                                                > maximumBytes
                                            then
                                                pure
                                                    (Left
                                                        oversizedRecordError)
                                            else
                                                pure
                                                    (Right
                                                        (Just buffered))
                            | otherwise -> do
                                writeIORef
                                    running.outputBytes
                                    (buffered <> chunk)
                                go
                        Left exception
                            | isEOFError exception ->
                                if ByteString.null buffered
                                    then pure (Right Nothing)
                                    else do
                                        writeIORef
                                            running.outputBytes
                                            ByteString.empty
                                        pure (Right (Just buffered))
                            | otherwise ->
                                pure $
                                    Left $
                                        CLIConnectionError
                                            ( "Failed to read Claude Code output: "
                                                <> Text.pack
                                                    (show exception)
                                            )

    oversizedRecordError =
        CLIProtocolError
            ( "Claude Code emitted a structured output record larger than "
                <> Text.pack (show maximumBytes)
                <> " bytes."
            )

closeTransport :: IORef (Maybe RunningTransport) -> IO ()
closeTransport stateRef =
    mask \restore -> do
        current <- readIORef stateRef
        case current of
            Nothing ->
                pure ()
            Just running -> do
                -- Retain the state until cleanup succeeds. If this thread is
                -- cancelled during cleanup, a later close can retry it.
                restore (stopRunningTransport running)
                writeIORef stateRef Nothing

endInput :: IORef (Maybe RunningTransport) -> IO ()
endInput stateRef = do
    state <- readIORef stateRef
    mapM_
        (\running -> do
            writeIORef running.inputOpen False
            interruptInputWriter running
            closeHandleQuietly running.inputHandle)
        state

processExit
    :: IORef (Maybe RunningTransport)
    -> IO (Maybe ExitCode)
processExit stateRef = do
    state <- readIORef stateRef
    case state of
        Nothing -> pure Nothing
        Just running ->
            getProcessExitCode running.processHandle

diagnostic :: IORef (Maybe RunningTransport) -> IO Text
diagnostic stateRef = do
    state <- readIORef stateRef
    case state of
        Nothing -> pure ""
        Just running ->
            TextEncoding.decodeUtf8With lenientDecode
                <$> readIORef running.diagnosticBytes

transportReady :: IORef (Maybe RunningTransport) -> IO Bool
transportReady stateRef = do
    state <- readIORef stateRef
    case state of
        Nothing -> pure False
        Just running -> do
            inputOpen <- readIORef running.inputOpen
            processExit <- getProcessExitCode running.processHandle
            pure (inputOpen && processExit == Nothing)

stopRunningTransport :: RunningTransport -> IO ()
stopRunningTransport running = do
    writeIORef running.inputOpen False
    -- Interrupt a blocked stdin write before closing its handle. A detached
    -- descendant can retain the pipe's read end after the Claude process
    -- group exits, leaving hClose blocked behind the writer's handle lock.
    interruptInputWriter running
    -- Interrupt a blocked stdout read before closing its handle. Otherwise a
    -- detached descendant that inherited stdout can keep hClose waiting on
    -- the reader's handle lock after the Claude process group has exited.
    interruptOutputReader running
    -- Signal first: closing a pipe can block behind another thread currently
    -- writing to it, whereas process-group termination is bounded.
    terminateProcessGroup running.groupId running.processHandle
    closeHandleQuietly running.inputHandle
    closeHandleQuietly running.outputHandle
    -- A detached descendant may inherit stderr and keep the drain blocked
    -- after the Claude process group exits. Cancel the reader before closing
    -- its handle so shutdown cannot wait indefinitely for that descendant.
    cancel running.errorDrain
    void (waitCatch running.errorDrain)
    closeHandleQuietly running.errorHandle
    waitForProcessQuietly running.processHandle

waitForProcessQuietly :: ProcessHandle -> IO ()
waitForProcessQuietly processHandle =
    void $
        timeout processWaitTimeoutMicros
            (tryAny (waitForProcess processHandle))

processWaitTimeoutMicros :: Int
processWaitTimeoutMicros = 2 * 1_000_000

drainDiagnostic
    :: Handle
    -> IORef ByteString.ByteString
    -> IO ()
drainDiagnostic handle diagnosticRef =
    go
  where
    go = do
        chunk <- ByteString.hGetSome handle 8_192
        if ByteString.null chunk
            then pure ()
            else do
                atomicModifyIORef' diagnosticRef \bytes ->
                    (appendDiagnostic bytes chunk, ())
                go

appendDiagnostic
    :: ByteString.ByteString
    -> ByteString.ByteString
    -> ByteString.ByteString
appendDiagnostic previous chunk =
    let combined = previous <> chunk
        excess = ByteString.length combined - diagnosticByteLimit
    in if excess > 0
        then ByteString.drop excess combined
        else combined

diagnosticByteLimit :: Int
diagnosticByteLimit = 65_536

closeHandleQuietly :: Handle -> IO ()
closeHandleQuietly handle =
    void (tryAny (hClose handle))

connectionException :: FilePath -> SomeException -> ClaudeSDKError
connectionException executable exception =
    case fromException exception of
        Just sdkError ->
            sdkError
        Nothing ->
            case fromExceptionIOException exception of
                Just ioException
                    | isDoesNotExistError ioException ->
                        CLINotFoundError executable
                _ ->
                    CLIConnectionError
                        ( "Failed to start Claude Code: "
                            <> Text.pack (show exception)
                        )

fromExceptionIOException :: SomeException -> Maybe IOException
fromExceptionIOException = fromException

prepareEnvironment
    :: ClaudeAgentOptions
    -> FilePath
    -> IO [(String, String)]
prepareEnvironment options workingDirectory = do
    base <- maybe getEnvironment pure options.environment
    pure $
        setEnvironmentVariable
            "CLAUDE_AGENT_SDK_VERSION"
            "0.1.0.0"
            (setEnvironmentVariable
                "CLAUDE_CODE_ENTRYPOINT"
                "sdk-cli"
                (setEnvironmentVariable
                    "CLAUDE_AGENT_SDK_CLIENT_APP"
                    ( Text.unpack $
                        fromMaybe
                            "claude-agent-sdk-haskell"
                            options.clientApplication
                    )
                    (setEnvironmentVariable
                        "PWD"
                        workingDirectory
                        (filter ((/= "CLAUDECODE") . fst) base))))

setEnvironmentVariable
    :: String
    -> String
    -> [(String, String)]
    -> [(String, String)]
setEnvironmentVariable name value environment =
    Map.toList (Map.insert name value (Map.fromList environment))

subprocessArguments
    :: ClaudeAgentOptions
    -> TransportMode
    -> Maybe Text
    -> Maybe Text
    -> [String]
subprocessArguments options mode model effort =
    [ "-p"
    , "--input-format"
    , "stream-json"
    , "--output-format"
    , "stream-json"
    , "--verbose"
    ]
        <> systemPromptArguments options.systemPrompt
        <> startArguments mode
        <> toolsArguments options.tools
        <> commaSeparatedArgument "--allowedTools" options.allowedTools
        <> commaSeparatedArgument
            "--disallowedTools"
            options.disallowedTools
        <> permissionArguments options
        <> settingSourcesArguments options.settingSources
        <> mcpArguments options.mcpServers
        <> boolArgument
            "--strict-mcp-config"
            options.strictMcpConfig
        <> boolArgument
            "--include-partial-messages"
            options.includePartialMessages
        <> boolArgument "--safe-mode" options.safeMode
        <> boolArgument
            "--disable-slash-commands"
            options.disableSlashCommands
        <> boolArgument "--no-chrome" options.noChrome
        <> optionalArgument "--model" model
        <> optionalEffortArgument effort
        <> concatMap extraArgument (Map.toAscList options.extraArgs)

systemPromptArguments :: SystemPrompt -> [String]
systemPromptArguments = \case
    SystemPromptNone -> ["--system-prompt", ""]
    SystemPromptText prompt ->
        ["--system-prompt", Text.unpack prompt]
    SystemPromptClaudeCode -> []

startArguments :: TransportMode -> [String]
startArguments = \case
    TransportNew sessionId ->
        ["--session-id", Text.unpack sessionId]
    TransportResume target ->
        ["--resume=" <> Text.unpack target]
    TransportContinue ->
        ["--continue"]

toolsArguments :: Maybe [Text] -> [String]
toolsArguments = \case
    Nothing -> []
    Just tools ->
        ["--tools", Text.unpack (Text.intercalate "," tools)]

permissionArguments :: ClaudeAgentOptions -> [String]
permissionArguments options =
    boolArgument
        "--allow-dangerously-skip-permissions"
        ( options.allowDangerouslySkipPermissions
            || options.permissionMode == Just PermissionBypassPermissions
        )
        <> case options.permissionMode of
            Nothing -> []
            Just permission ->
                [ "--permission-mode"
                , Text.unpack (permissionModeName permission)
                ]

settingSourcesArguments :: Maybe [Text] -> [String]
settingSourcesArguments = \case
    Nothing -> []
    Just sources ->
        [ "--setting-sources"
        , Text.unpack (Text.intercalate "," sources)
        ]

mcpArguments :: Maybe Aeson.Value -> [String]
mcpArguments = \case
    Nothing -> []
    Just value ->
        [ "--mcp-config"
        , Text.unpack $
            TextEncoding.decodeUtf8
                (ByteString.toStrict (Aeson.encode value))
        ]

commaSeparatedArgument :: String -> [Text] -> [String]
commaSeparatedArgument _ [] = []
commaSeparatedArgument name values =
    [name, Text.unpack (Text.intercalate "," values)]

optionalArgument :: String -> Maybe Text -> [String]
optionalArgument name = \case
    Just value -> [name, Text.unpack value]
    Nothing -> []

optionalEffortArgument :: Maybe Text -> [String]
optionalEffortArgument = \case
    Just value
        | Text.toLower value /= "none" ->
            ["--effort", Text.unpack value]
    _ -> []

boolArgument :: String -> Bool -> [String]
boolArgument name enabled =
    if enabled then [name] else []

extraArgument :: (Text, Maybe Text) -> [String]
extraArgument (name, maybeValue) =
    let flag = "--" <> Text.unpack name
    in case maybeValue of
        Nothing -> [flag]
        Just value
            | "-" `Text.isPrefixOf` value ->
                [flag <> "=" <> Text.unpack value]
            | otherwise ->
                [flag, Text.unpack value]
