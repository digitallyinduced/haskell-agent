{-# LANGUAGE CPP #-}

-- | Shared filesystem and process helpers for coding tools.
module Agent.Tools.IO
    ( CommandResult(..)
    , RunningCommand(..)
    , combineCommandOutput
    , commandResultOutput
    , commandResultArtifacts
    , formatCommandResult
    , displayPathInWorkspace
    , resolveForRead
    , resolveUnderCwd
    , readTextFile
    , writeTextFile
    , deleteTextFile
    , renameTextFile
    , listDirectoryEntries
    , runShellCommand
    , runShellCommandStreaming
    , startShellCommand
    , startShellCommandWithCompletion
    , startShellCommandWithInput
    , startShellCommandWithInputAndCompletion
    , configuredProcess
    , configuredProcessEnv
    , sessionTempProcessEnv
    , writeShellCommandInput
    , interruptShellCommand
    , stopShellCommand
    , terminateProcessGroup
    , RunningOutputCursor
    , initialRunningOutputCursor
    , runningLiveOutput
    , runningOutputSince
    ) where

import Agent.Cancel (isCancelled, waitCancel)
import Agent.OsPath (unsafeToFilePath)
import Agent.Process (terminateProcessGroup)
import Agent.Tools.FileSystem
    ( deleteTextFile
    , displayPathInWorkspace
    , listDirectoryEntries
    , readTextFile
    , renameTextFile
    , resolveForRead
    , resolveUnderCwd
    , writeTextFile
    )
import Agent.Tools.OutputArtifact
    ( OutputArtifact
    , OutputArtifactWriter
    , abortOutputArtifact
    , appendOutputArtifact
    , finishOutputArtifact
    , openOutputArtifact
    , renderOutputArtifactNotice
    )
import System.OsPath (OsPath)
import Agent.Tools.Types (ToolEnv(..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
    ( Async
    , asyncWithUnmask
    , cancel
    , concurrently
    , race
    , withAsync
    , waitCatch
    )
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar
    , newEmptyMVar
    , newMVar
    , readMVar
    , tryReadMVar
    , tryPutMVar
    )
import Control.Exception.Safe
    ( SomeException
    , finally
    , mask
    , onException
    , try
    , tryAny
    )
import Control.Monad (unless, void, when)
import Data.IORef
    ( IORef
    , atomicModifyIORef'
    , newIORef
    , readIORef
    , writeIORef
    )
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.ByteString as BS
import Data.Text.Encoding (decodeUtf8With)
import qualified Data.Text.Encoding as TextEncoding
import Data.Text.Encoding.Error (lenientDecode)
import System.Environment (getEnvironment)
import System.Exit (ExitCode(..))
#if defined(darwin_HOST_OS)
import qualified System.Directory as Directory
import System.FilePath
    ( takeDirectory
    , takeFileName
    )
#endif
import System.IO (Handle, hClose, hFlush)
import System.Posix.Signals
    ( sigINT
    , signalProcessGroup
    )
import System.Posix.Types (ProcessGroupID)
import System.Process
    ( CreateProcess(..)
    , CmdSpec(..)
    , ProcessHandle
    , StdStream(..)
    , createProcess
    , getPid
    , getProcessExitCode
    , shell
    , terminateProcess
    , waitForProcess
    )

data CommandResult = CommandResult
    { commandExitCode :: !(Maybe Int)
    , commandStdout :: !Text
    , commandStderr :: !Text
    , commandStdoutArtifact :: !(Maybe OutputArtifact)
    , commandStderrArtifact :: !(Maybe OutputArtifact)
    , commandTimedOut :: !Bool
    , commandCancelled :: !Bool
    } deriving (Eq, Show)

-- | Combine captured stdout and stderr without introducing a blank line when
-- either stream is empty.
combineCommandOutput :: Text -> Text -> Text
combineCommandOutput out err
    | Text.null err = out
    | Text.null out = err
    | otherwise = out <> "\n" <> err

-- | Combined captured output from a completed command.
commandResultOutput :: CommandResult -> Text
commandResultOutput result =
    Text.intercalate "\n" . filter (not . Text.null) $
        [ result.commandStdout
        , result.commandStderr
        , commandResultArtifacts result
        ]

-- | Artifact notices for dialects that retain their own stdout/stderr format.
commandResultArtifacts :: CommandResult -> Text
commandResultArtifacts result =
    Text.intercalate "\n" . filter (not . Text.null) $
        [ maybe "" (renderOutputArtifactNotice "shell stdout")
            result.commandStdoutArtifact
        , maybe "" (renderOutputArtifactNotice "shell stderr")
            result.commandStderrArtifact
        ]

-- | Render the stable terminal-tool result format shared by foreground and
-- background commands.
formatCommandResult :: CommandResult -> Text
formatCommandResult result
    | result.commandCancelled = "exit: cancelled\n" <> body
    | result.commandTimedOut = "exit: killed (timeout)\n" <> body
    | otherwise =
        "exit: "
            <> Text.pack (show (fromMaybe 1 result.commandExitCode))
            <> "\n"
            <> body
  where
    body = commandResultOutput result

data RunningCommand = RunningCommand
    { runningHandle :: !ProcessHandle
    , runningGroupId :: !(Maybe ProcessGroupID)
    , runningResult :: !(MVar CommandResult)
    , runningStdin :: !(MVar (Maybe Handle))
    , runningStdout :: !(IORef CapturedBytes)
    , runningStderr :: !(IORef CapturedBytes)
    , runningStdoutRecent :: !(IORef RecentBytes)
    , runningStderrRecent :: !(IORef RecentBytes)
    , runningStdoutHandle :: !Handle
    , runningStderrHandle :: !Handle
    , runningSupervisor :: !(Async ())
    }

data CapturedBytes = CapturedBytes
    { capturedBytes :: !BS.ByteString
    , capturedDropped :: !Int
    }

data RecentBytes = RecentBytes
    { recentBytes :: !BS.ByteString
    , recentDropped :: !Int
    }

data RunningOutputCursor = RunningOutputCursor
    { cursorStdoutBytes :: !Int
    , cursorStderrBytes :: !Int
    }

initialRunningOutputCursor :: RunningOutputCursor
initialRunningOutputCursor = RunningOutputCursor 0 0

-- | Bytes already drained from a still-running command, decoded and capped.
runningLiveOutput :: RunningCommand -> IO (Text, Text)
runningLiveOutput running = do
    out <- readIORef running.runningStdout
    err <- readIORef running.runningStderr
    pure
        ( renderCapturedBytes out
        , renderCapturedBytes err
        )

-- | Return output produced after a prior cursor. The capture is a bounded tail,
-- so a slow reader may receive a truncation marker followed by the newest data.
runningOutputSince
    :: RunningCommand
    -> RunningOutputCursor
    -> IO ((Text, Text), RunningOutputCursor)
runningOutputSince running cursor = do
    out <- readIORef running.runningStdoutRecent
    err <- readIORef running.runningStderrRecent
    let (outText, outCursor) =
            renderRecentBytesSince cursor.cursorStdoutBytes out
        (errText, errCursor) =
            renderRecentBytesSince cursor.cursorStderrBytes err
    pure
        ( (outText, errText)
        , RunningOutputCursor outCursor errCursor
        )

-- | Run a shell command in @workdir@, killing the process group on timeout.
runShellCommand
    :: ToolEnv
    -> OsPath
    -> String
    -> Int
    -> IO CommandResult
runShellCommand env workdir command timeoutMs =
    runShellCommandStreaming env workdir command timeoutMs (\_ _ -> pure ())

-- | Run a foreground shell command while publishing accumulated stdout/stderr
-- snapshots. Snapshots are coalesced to at most roughly ten per second and the
-- final 'CommandResult' remains authoritative.
runShellCommandStreaming
    :: ToolEnv
    -> OsPath
    -> String
    -> Int
    -> (Text -> Text -> IO ())
    -> IO CommandResult
runShellCommandStreaming env workdir command timeoutMs onSnapshot =
    mask \restore -> do
    let baseSpec = (shell command)
            { cwd = Just (unsafeToFilePath workdir)
            , std_in = CreatePipe
            , std_out = CreatePipe
            , std_err = CreatePipe
            , create_group = True
            }
    try @_ @SomeException
        (configuredProcess env baseSpec >>= createProcess) >>= \case
        Left err -> pure CommandResult
            { commandExitCode = Just 127
            , commandStdout = ""
            , commandStderr = "Failed to start command: " <> Text.pack (show err)
            , commandStdoutArtifact = Nothing
            , commandStderrArtifact = Nothing
            , commandTimedOut = False
            , commandCancelled = False
            }
        Right (Just hin, Just hout, Just herr, processHandle) -> do
            groupId <- getPid processHandle
            let closePipes =
                    mapM_ (void . try @_ @SomeException . hClose) [hin, hout, herr]
                stopCommand = do
                    terminateProcessGroup groupId processHandle
                    closePipes
            hClose hin `onException` stopCommand
            stdoutRef <- newIORef emptyCapturedBytes
            stderrRef <- newIORef emptyCapturedBytes
            lastSnapshotRef <- newIORef Nothing
            let collect = do
                    -- Drain stdout and stderr concurrently so a child that
                    -- fills one pipe cannot deadlock the other.
                    withAsync sampleSnapshots \_sampler -> do
                        (out, err) <- concurrently
                            (drainHandle env "shell stdout" hout stdoutRef Nothing)
                            (drainHandle env "shell stderr" herr stderrRef Nothing)
                        code <- waitForProcess processHandle
                        emitSnapshot
                        pure (out, err, code)
                sampleSnapshots = do
                    threadDelay 100000
                    emitSnapshot
                    sampleSnapshots
                emitSnapshot = do
                    outBytes <- readIORef stdoutRef
                    errBytes <- readIORef stderrRef
                    let out = renderCapturedBytes outBytes
                        err = renderCapturedBytes errBytes
                        snapshot = (out, err)
                    changed <- atomicModifyIORef' lastSnapshotRef \previous ->
                        if previous == Just snapshot
                            then (previous, False)
                            else (Just snapshot, True)
                    when (changed && not (Text.null out && Text.null err)) $
                        onSnapshot out err
                -- Prefer cancel over timeout when both fire: race cancel
                -- against (timeout `race` collect).
                waitStop = do
                    timed <- race (threadDelay (max 1 timeoutMs * 1000)) collect
                    pure $ case timed of
                        Left () -> StopTimeout
                        Right triple -> StopDone triple
            restore (do
                already <- isCancelled env.toolCancel
                if already
                    then do
                        stopCommand
                        pure cancelledResult
                    else do
                        stopped <- race (waitCancel env.toolCancel) waitStop
                        case stopped of
                            Left () -> do
                                stopCommand
                                pure cancelledResult
                            Right StopTimeout -> do
                                stopCommand
                                pure CommandResult
                                    { commandExitCode = Nothing
                                    , commandStdout = ""
                                    , commandStderr = ""
                                    , commandStdoutArtifact = Nothing
                                    , commandStderrArtifact = Nothing
                                    , commandTimedOut = True
                                    , commandCancelled = False
                                    }
                            Right (StopDone (out, err, code)) -> do
                                closePipes
                                pure CommandResult
                                    { commandExitCode = Just (exitCodeInt code)
                                    , commandStdout =
                                        renderCapturedBytes out.drainedCaptured
                                    , commandStderr =
                                        renderCapturedBytes err.drainedCaptured
                                    , commandStdoutArtifact = out.drainedArtifact
                                    , commandStderrArtifact = err.drainedArtifact
                                    , commandTimedOut = False
                                    , commandCancelled = False
                                    })
                `onException` stopCommand
        Right (hin, hout, herr, processHandle) -> do
            groupId <- getPid processHandle
            terminateProcessGroup groupId processHandle
            closeOptionalPipes [hin, hout, herr]
            pure CommandResult
                { commandExitCode = Just 127
                , commandStdout = ""
                , commandStderr = "Failed to capture command output"
                , commandStdoutArtifact = Nothing
                , commandStderrArtifact = Nothing
                , commandTimedOut = False
                , commandCancelled = False
                }

-- | Outcomes of racing timeout against completion (cancel is the other race arm).
data ShellStop
    = StopTimeout
    | StopDone (DrainedOutput, DrainedOutput, ExitCode)

cancelledResult :: CommandResult
cancelledResult = CommandResult
    { commandExitCode = Nothing
    , commandStdout = ""
    , commandStderr = ""
    , commandStdoutArtifact = Nothing
    , commandStderrArtifact = Nothing
    , commandTimedOut = False
    , commandCancelled = True
    }

-- | Start a command without waiting. Call 'stopShellCommand' when its owner
-- is closed or the task is explicitly killed.
startShellCommand
    :: ToolEnv
    -> OsPath
    -> String
    -> IO (Either Text RunningCommand)
startShellCommand env workdir command =
    startShellCommandWithStdin False env workdir command (\_ -> pure ())

-- | Start a command and invoke a non-blocking owner callback exactly once
-- after its final result has been published.
startShellCommandWithCompletion
    :: ToolEnv
    -> OsPath
    -> String
    -> (CommandResult -> IO ())
    -> IO (Either Text RunningCommand)
startShellCommandWithCompletion =
    startShellCommandWithStdin False

-- | Start a command without waiting and retain stdin for later writes.
-- Call 'stopShellCommand' when its owner is closed or the task is explicitly
-- killed.
startShellCommandWithInput
    :: ToolEnv
    -> OsPath
    -> String
    -> IO (Either Text RunningCommand)
startShellCommandWithInput env workdir command =
    startShellCommandWithStdin True env workdir command (\_ -> pure ())

startShellCommandWithInputAndCompletion
    :: ToolEnv
    -> OsPath
    -> String
    -> (CommandResult -> IO ())
    -> IO (Either Text RunningCommand)
startShellCommandWithInputAndCompletion =
    startShellCommandWithStdin True

startShellCommandWithStdin
    :: Bool
    -> ToolEnv
    -> OsPath
    -> String
    -> (CommandResult -> IO ())
    -> IO (Either Text RunningCommand)
startShellCommandWithStdin keepStdin env workdir command onComplete = do
    let baseSpec = (shell command)
            { cwd = Just (unsafeToFilePath workdir)
            , std_in = CreatePipe
            , std_out = CreatePipe
            , std_err = CreatePipe
            , create_group = True
            }
    try @_ @SomeException
        (configuredProcess env baseSpec >>= \spec ->
            acquireRunningCommand env spec keepStdin onComplete) >>= \case
        Left err -> pure $ Left $ "Failed to start command: " <> Text.pack (show err)
        Right running -> pure (Right running)

-- | Apply the session-private environment and, on macOS, a Seatbelt profile
-- that denies the shared temp namespace. Managed session layouts also hide
-- sibling scratch directories while allowing the current one.
configuredProcess :: ToolEnv -> CreateProcess -> IO CreateProcess
configuredProcess env spec = do
    sessionTmp <- readIORef env.toolSessionTmp
    processEnv <- configuredProcessEnvFor sessionTmp spec.env
    command <- configuredCommandSpec sessionTmp spec.cmdspec
    pure spec
        { cmdspec = command
        , env = case processEnv of
            Nothing -> spec.env
            Just values -> Just values
        }

configuredCommandSpec :: Maybe OsPath -> CmdSpec -> IO CmdSpec
#if defined(darwin_HOST_OS)
configuredCommandSpec sessionTmp command =
    case sessionTmp of
        Nothing -> pure command
        Just temp -> do
            let sandboxExecutable = "/usr/bin/sandbox-exec"
            canonicalTemp <-
                Directory.canonicalizePath (unsafeToFilePath temp)
            let profile = sessionSandboxProfile canonicalTemp
                wrapped = case command of
                    RawCommand executable arguments ->
                        executable : arguments
                    ShellCommand script ->
                        ["/bin/sh", "-c", script]
            pure $
                RawCommand sandboxExecutable
                    (["-p", profile] <> wrapped)
#else
configuredCommandSpec _ command = pure command
#endif

#if defined(darwin_HOST_OS)
sessionSandboxProfile :: FilePath -> String
sessionSandboxProfile temp =
    unwords $
        [ "(version 1)"
        , "(allow default)"
        , "(deny file-read* file-write*"
        , "  (subpath \"/tmp\")"
        , "  (subpath \"/private/tmp\")"
        ]
        <> managedParentRule
        <> [")"]
        <> [ "(allow file-read* file-write*"
           , "  (subpath " <> sandboxString temp <> "))"
           ]
  where
    parent = takeDirectory temp
    grandparent = takeDirectory parent
    managedParentRule
        | takeFileName parent == "sessions"
        , takeFileName grandparent == "tmp" =
            ["  (subpath " <> sandboxString parent <> ")"]
        | otherwise = []

sandboxString :: String -> String
sandboxString value =
    "\"" <> concatMap escape value <> "\""
  where
    escape '\\' = "\\\\"
    escape '"' = "\\\""
    escape '\n' = "\\n"
    escape '\r' = "\\r"
    escape '\t' = "\\t"
    escape char = [char]
#endif

configuredProcessEnv :: ToolEnv -> IO (Maybe [(String, String)])
configuredProcessEnv env =
    readIORef env.toolSessionTmp >>= (`configuredProcessEnvFor` Nothing)

configuredProcessEnvFor
    :: Maybe OsPath
    -> Maybe [(String, String)]
    -> IO (Maybe [(String, String)])
configuredProcessEnvFor sessionTmp inherited =
    case sessionTmp of
        Nothing -> pure inherited
        Just temp ->
            Just . sessionTempProcessEnv temp
                <$> maybe getEnvironment pure inherited

-- | Point a child process at one session's private scratch directory without
-- mutating the parent process environment (several sessions may coexist).
sessionTempProcessEnv :: OsPath -> [(String, String)] -> [(String, String)]
sessionTempProcessEnv temp inherited =
    ( ("TMPDIR", tempPath)
    : ("HASKELL_AGENT_TMPDIR", tempPath)
    : withoutTemp
    )
  where
    tempPath = unsafeToFilePath temp
    withoutTemp =
        filter
            (\(name, _) ->
                name /= "TMPDIR"
                    && name /= "HASKELL_AGENT_TMPDIR"
                    -- Never propagate an ambient host-temp escape.
                    && name /= "HASKELL_AGENT_HOST_TMPDIR")
            inherited

acquireRunningCommand
    :: ToolEnv
    -> CreateProcess
    -> Bool
    -> (CommandResult -> IO ())
    -> IO RunningCommand
acquireRunningCommand env spec keepStdin onComplete = mask \restore -> do
    created@(_, _, _, processHandle) <- createProcess spec
    groupId <- getPid processHandle
    case created of
        (Just hin, Just hout, Just herr, _) -> do
            stdinVar <- newMVar (if keepStdin then Just hin else Nothing)
            let closePipes =
                    mapM_ (void . try @_ @SomeException . hClose) [hin, hout, herr]
                stopCreated = do
                    terminateProcessGroup groupId processHandle
                    closePipes
            unless keepStdin $
                hClose hin `onException` stopCreated
            resultVar <- newEmptyMVar
            stdoutRef <- newIORef emptyCapturedBytes
            stderrRef <- newIORef emptyCapturedBytes
            stdoutRecentRef <- newIORef emptyRecentBytes
            stderrRecentRef <- newIORef emptyRecentBytes
            supervisor <- asyncWithUnmask \unmask ->
                (do
                    result <- try @_ @SomeException $ unmask do
                        ((out, err), code) <- concurrently
                            (concurrently
                                (drainHandle
                                    env "shell stdout" hout stdoutRef
                                    (Just stdoutRecentRef))
                                (drainHandle
                                    env "shell stderr" herr stderrRef
                                    (Just stderrRecentRef)))
                            (waitForProcessPolling processHandle)
                        pure (out, err, code)
                    commandResult <- case result of
                        Left exception -> do
                            terminateProcessGroup groupId processHandle
                            let diagnostic =
                                    TextEncoding.encodeUtf8 (Text.pack (show exception))
                            atomicModifyIORef' stderrRef \soFar ->
                                (appendCapturedBytes env.toolStdoutCap diagnostic soFar, ())
                            atomicModifyIORef' stderrRecentRef \soFar ->
                                (appendRecentBytes env.toolStdoutCap diagnostic soFar, ())
                            pure (failedCommandResult exception)
                        Right (out, err, code) ->
                            pure CommandResult
                                { commandExitCode = Just (exitCodeInt code)
                                , commandStdout =
                                    renderCapturedBytes out.drainedCaptured
                                , commandStderr =
                                    renderCapturedBytes err.drainedCaptured
                                , commandStdoutArtifact = out.drainedArtifact
                                , commandStderrArtifact = err.drainedArtifact
                                , commandTimedOut = False
                                , commandCancelled = False
                                }
                    published <- tryPutMVar resultVar commandResult
                    when published $
                        void $ tryAny (onComplete commandResult))
                `finally` do
                    void $ tryPutMVar resultVar cancelledResult
                    closeRunningStdinVar stdinVar
                    mapM_ (void . try @_ @SomeException . hClose) [hout, herr]
            let running = RunningCommand
                    { runningHandle = processHandle
                    , runningGroupId = groupId
                    , runningResult = resultVar
                    , runningStdin = stdinVar
                    , runningStdout = stdoutRef
                    , runningStderr = stderrRef
                    , runningStdoutRecent = stdoutRecentRef
                    , runningStderrRecent = stderrRecentRef
                    , runningStdoutHandle = hout
                    , runningStderrHandle = herr
                    , runningSupervisor = supervisor
                    }
            restore (pure running) `onException` stopShellCommand running
        (hin, hout, herr, _) -> do
            terminateProcessGroup groupId processHandle
            closeOptionalPipes [hin, hout, herr]
            fail "Failed to capture command output"

-- | Write input to a running command. The command must have been started with
-- 'startShellCommandWithInput'.
writeShellCommandInput :: RunningCommand -> Text -> IO (Either Text ())
writeShellCommandInput running input =
    modifyMVar running.runningStdin \case
        Nothing ->
            pure (Nothing, Left "stdin is closed")
        Just handle ->
            try @_ @SomeException
                (BS.hPut handle (TextEncoding.encodeUtf8 input) >> hFlush handle) >>= \case
                    Left exception -> do
                        void $ try @_ @SomeException (hClose handle)
                        pure
                            ( Nothing
                            , Left ("Failed to write stdin: " <> Text.pack (show exception))
                            )
                    Right () ->
                        pure (Just handle, Right ())

-- | Send an interrupt to the command's process group.
interruptShellCommand :: RunningCommand -> IO ()
interruptShellCommand running =
    case running.runningGroupId of
        Nothing ->
            void $ try @_ @SomeException (terminateProcess running.runningHandle)
        Just groupId ->
            void $ try @_ @SomeException (signalProcessGroup sigINT groupId)

stopShellCommand :: RunningCommand -> IO ()
stopShellCommand running = do
    closeRunningStdin running
    finished <- tryReadMVar running.runningResult
    case finished of
        Just _ ->
            joinRunningSupervisor running
        Nothing -> do
            terminateProcessGroup running.runningGroupId running.runningHandle
            joined <- race
                (threadDelay 1000000)
                (readMVar running.runningResult)
            case joined of
                Right _ ->
                    joinRunningSupervisor running
                Left () -> do
                    closeRunningPipes running
                    cancel running.runningSupervisor
                    void (waitCatch running.runningSupervisor)
                    void $ tryPutMVar running.runningResult cancelledResult
    closeRunningPipes running

joinRunningSupervisor :: RunningCommand -> IO ()
joinRunningSupervisor running = do
    joined <- race
        (threadDelay 1000000)
        (waitCatch running.runningSupervisor)
    case joined of
        Right _ -> pure ()
        Left () -> do
            cancel running.runningSupervisor
            void (waitCatch running.runningSupervisor)

closeRunningPipes :: RunningCommand -> IO ()
closeRunningPipes running =
    mapM_ (void . try @_ @SomeException . hClose)
        [running.runningStdoutHandle, running.runningStderrHandle]

closeRunningStdin :: RunningCommand -> IO ()
closeRunningStdin = closeRunningStdinVar . (.runningStdin)

closeRunningStdinVar :: MVar (Maybe Handle) -> IO ()
closeRunningStdinVar stdinVar =
    modifyMVar stdinVar \current -> do
        mapM_ (void . try @_ @SomeException . hClose) current
        pure (Nothing, ())

closeOptionalPipes :: [Maybe Handle] -> IO ()
closeOptionalPipes =
    mapM_ (mapM_ (void . try @_ @SomeException . hClose))

failedCommandResult :: SomeException -> CommandResult
failedCommandResult exception = CommandResult
    { commandExitCode = Just 127
    , commandStdout = ""
    , commandStderr = Text.pack (show exception)
    , commandStdoutArtifact = Nothing
    , commandStderrArtifact = Nothing
    , commandTimedOut = False
    , commandCancelled = False
    }

data DrainedOutput = DrainedOutput
    { drainedCaptured :: !CapturedBytes
    , drainedArtifact :: !(Maybe OutputArtifact)
    }

-- | Read a process stream in chunks. The live snapshot remains bounded; once
-- it overflows, the complete stream is spilled to a session artifact.
drainHandle
    :: ToolEnv
    -> Text
    -> Handle
    -> IORef CapturedBytes
    -> Maybe (IORef RecentBytes)
    -> IO DrainedOutput
drainHandle env _source handle ref recentRef = do
    writerRef <- newIORef Nothing
    let cleanup =
            readIORef writerRef >>= mapM_ (\writer -> do
                _ <- finishOutputArtifact writer
                pure ())
    go writerRef Nothing False `onException` cleanup
  where
    cap = env.toolStdoutCap

    go writerRef writer disabled = do
        chunk <- BS.hGetSome handle 8192
        if BS.null chunk
            then do
                captured <- readIORef ref
                artifact <- traverse finishOutputArtifact writer
                writeIORef writerRef Nothing
                pure DrainedOutput
                    { drainedCaptured = captured
                    , drainedArtifact = artifact
                    }
            else do
                before <- readIORef ref
                (nextWriter, nextDisabled) <-
                    spillChunk writer disabled before chunk
                writeIORef writerRef nextWriter
                atomicModifyIORef' ref \soFar ->
                    (appendCapturedBytes cap chunk soFar, ())
                mapM_
                    (\recent ->
                        atomicModifyIORef' recent \soFar ->
                            (appendRecentBytes cap chunk soFar, ()))
                    recentRef
                go writerRef nextWriter nextDisabled

    spillChunk
        :: Maybe OutputArtifactWriter
        -> Bool
        -> CapturedBytes
        -> BS.ByteString
        -> IO (Maybe OutputArtifactWriter, Bool)
    spillChunk (Just writer) disabled _ chunk =
        appendOutputArtifact writer chunk >>= \case
            Right () -> pure (Just writer, disabled)
            Left _ -> do
                abortOutputArtifact writer
                pure (Nothing, True)
    spillChunk Nothing True _ _ = pure (Nothing, True)
    spillChunk Nothing False before chunk
        | cap <= 0
            || BS.length before.capturedBytes + BS.length chunk <= cap =
                pure (Nothing, False)
        | otherwise =
            openOutputArtifact env >>= \case
                Left _ -> pure (Nothing, True)
                Right writer ->
                    appendOutputArtifact writer
                        (before.capturedBytes <> chunk) >>= \case
                            Right () -> pure (Just writer, False)
                            Left _ -> do
                                abortOutputArtifact writer
                                pure (Nothing, True)

emptyCapturedBytes :: CapturedBytes
emptyCapturedBytes = CapturedBytes
    { capturedBytes = BS.empty
    , capturedDropped = 0
    }

emptyRecentBytes :: RecentBytes
emptyRecentBytes = RecentBytes
    { recentBytes = BS.empty
    , recentDropped = 0
    }

appendCapturedBytes :: Int -> BS.ByteString -> CapturedBytes -> CapturedBytes
appendCapturedBytes cap chunk captured
    | BS.null chunk = captured
    | cap <= 0 =
        captured { capturedBytes = captured.capturedBytes <> chunk }
    | otherwise =
        let remaining = max 0 (cap - BS.length captured.capturedBytes)
            kept = BS.take remaining chunk
        in CapturedBytes
            { capturedBytes = captured.capturedBytes <> kept
            , capturedDropped =
                captured.capturedDropped + BS.length chunk - BS.length kept
            }

renderCapturedBytes :: CapturedBytes -> Text
renderCapturedBytes captured =
    decodeUtf8With lenientDecode captured.capturedBytes
        <> if captured.capturedDropped <= 0
            then ""
            else
                "\n...[truncated "
                    <> Text.pack (show captured.capturedDropped)
                    <> " bytes]"

appendRecentBytes :: Int -> BS.ByteString -> RecentBytes -> RecentBytes
appendRecentBytes cap chunk recent
    | BS.null chunk = recent
    | cap <= 0 =
        recent { recentBytes = recent.recentBytes <> chunk }
    | otherwise =
        let combined = recent.recentBytes <> chunk
            overflow = max 0 (BS.length combined - cap)
        in RecentBytes
            { recentBytes = BS.drop overflow combined
            , recentDropped = recent.recentDropped + overflow
            }

renderRecentBytesSince :: Int -> RecentBytes -> (Text, Int)
renderRecentBytesSince cursor recent =
    let retainedFrom = recent.recentDropped
        total = retainedFrom + BS.length recent.recentBytes
        missed = max 0 (retainedFrom - cursor)
        offset = max 0 (min (BS.length recent.recentBytes) (cursor - retainedFrom))
        bytes = BS.drop offset recent.recentBytes
    in
        ( truncationMarker missed <> decodeUtf8With lenientDecode bytes
        , total
        )

truncationMarker :: Int -> Text
truncationMarker dropped
    | dropped <= 0 = ""
    | otherwise =
        "...[truncated "
            <> Text.pack (show dropped)
            <> " bytes]\n"

waitForProcessPolling :: ProcessHandle -> IO ExitCode
waitForProcessPolling processHandle =
    getProcessExitCode processHandle >>= \case
        Just code -> pure code
        Nothing -> do
            threadDelay 10000
            waitForProcessPolling processHandle

exitCodeInt :: ExitCode -> Int
exitCodeInt = \case
    ExitSuccess -> 0
    ExitFailure n -> n
