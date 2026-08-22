-- | Shared filesystem and process helpers for coding tools.
module Agent.Tools.IO
    ( CommandResult(..)
    , RunningCommand(..)
    , resolveUnderCwd
    , readTextFile
    , writeTextFile
    , deleteTextFile
    , renameTextFile
    , listDirectoryEntries
    , runShellCommand
    , runShellCommandStreaming
    , startShellCommand
    , stopShellCommand
    , terminateProcessGroup
    , runningLiveOutput
    , truncateText
    ) where

import Agent.Cancel (isCancelled, waitCancel)
import Agent.Tools.FileSystem
    ( deleteTextFile
    , listDirectoryEntries
    , readTextFile
    , renameTextFile
    , resolveUnderCwd
    , writeTextFile
    )
import System.OsPath (OsPath, decodeUtf)
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
    , newEmptyMVar
    , readMVar
    , tryReadMVar
    , tryPutMVar
    )
import Control.Exception.Safe
    ( SomeException
    , finally
    , impureThrow
    , mask
    , onException
    , try
    )
import Control.Monad (unless, void, when)
import Data.Either (isRight)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.ByteString as BS
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import System.Exit (ExitCode(..))
import System.IO (Handle, hClose)
import System.Posix.Signals
    ( nullSignal
    , sigINT
    , sigKILL
    , sigTERM
    , signalProcessGroup
    )
import System.Posix.Types (ProcessGroupID)
import System.Process
    ( CreateProcess(..)
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
    , commandTimedOut :: !Bool
    , commandCancelled :: !Bool
    } deriving (Eq, Show)

data RunningCommand = RunningCommand
    { runningHandle :: !ProcessHandle
    , runningGroupId :: !(Maybe ProcessGroupID)
    , runningResult :: !(MVar CommandResult)
    , runningStdout :: !(IORef CapturedBytes)
    , runningStderr :: !(IORef CapturedBytes)
    , runningStdoutHandle :: !Handle
    , runningStderrHandle :: !Handle
    , runningSupervisor :: !(Async ())
    }

data CapturedBytes = CapturedBytes
    { capturedBytes :: !BS.ByteString
    , capturedDropped :: !Int
    }

-- | Bytes already drained from a still-running command, decoded and capped.
runningLiveOutput :: RunningCommand -> IO (Text, Text)
runningLiveOutput running = do
    out <- readIORef running.runningStdout
    err <- readIORef running.runningStderr
    pure
        ( renderCapturedBytes out
        , renderCapturedBytes err
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
    let spec = (shell command)
            { cwd = Just (decodeUtfPath workdir)
            , std_in = CreatePipe
            , std_out = CreatePipe
            , std_err = CreatePipe
            , create_group = True
            }
    try @_ @SomeException (createProcess spec) >>= \case
        Left err -> pure CommandResult
            { commandExitCode = Just 127
            , commandStdout = ""
            , commandStderr = "Failed to start command: " <> Text.pack (show err)
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
                            (drainHandle env.toolStdoutCap hout stdoutRef)
                            (drainHandle env.toolStdoutCap herr stderrRef)
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
                                    , commandTimedOut = True
                                    , commandCancelled = False
                                    }
                            Right (StopDone (out, err, code)) -> do
                                closePipes
                                pure CommandResult
                                    { commandExitCode = Just (exitCodeInt code)
                                    , commandStdout = renderCapturedBytes out
                                    , commandStderr = renderCapturedBytes err
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
                , commandTimedOut = False
                , commandCancelled = False
                }

-- | Outcomes of racing timeout against completion (cancel is the other race arm).
data ShellStop
    = StopTimeout
    | StopDone (CapturedBytes, CapturedBytes, ExitCode)

cancelledResult :: CommandResult
cancelledResult = CommandResult
    { commandExitCode = Nothing
    , commandStdout = ""
    , commandStderr = ""
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
startShellCommand env workdir command = do
    let spec = (shell command)
            { cwd = Just (decodeUtfPath workdir)
            , std_in = CreatePipe
            , std_out = CreatePipe
            , std_err = CreatePipe
            , create_group = True
            }
    try @_ @SomeException
        (acquireRunningCommand spec env.toolStdoutCap) >>= \case
        Left err -> pure $ Left $ "Failed to start command: " <> Text.pack (show err)
        Right running -> pure (Right running)

acquireRunningCommand :: CreateProcess -> Int -> IO RunningCommand
acquireRunningCommand spec outputCap = mask \restore -> do
    created@(_, _, _, processHandle) <- createProcess spec
    groupId <- getPid processHandle
    case created of
        (Just hin, Just hout, Just herr, _) -> do
            let closePipes =
                    mapM_ (void . try @_ @SomeException . hClose) [hin, hout, herr]
                stopCreated = do
                    terminateProcessGroup groupId processHandle
                    closePipes
            hClose hin `onException` stopCreated
            resultVar <- newEmptyMVar
            stdoutRef <- newIORef emptyCapturedBytes
            stderrRef <- newIORef emptyCapturedBytes
            supervisor <- asyncWithUnmask \unmask ->
                (do
                    result <- try @_ @SomeException $ unmask do
                        ((out, err), code) <- concurrently
                            (concurrently
                                (drainHandle outputCap hout stdoutRef)
                                (drainHandle outputCap herr stderrRef))
                            (waitForProcessPolling processHandle)
                        pure (out, err, code)
                    commandResult <- case result of
                        Left exception -> do
                            terminateProcessGroup groupId processHandle
                            pure (failedCommandResult exception)
                        Right (out, err, code) ->
                            pure CommandResult
                                { commandExitCode = Just (exitCodeInt code)
                                , commandStdout = renderCapturedBytes out
                                , commandStderr = renderCapturedBytes err
                                , commandTimedOut = False
                                , commandCancelled = False
                                }
                    void $ tryPutMVar resultVar commandResult)
                `finally` do
                    void $ tryPutMVar resultVar cancelledResult
                    mapM_ (void . try @_ @SomeException . hClose) [hout, herr]
            let running = RunningCommand
                    { runningHandle = processHandle
                    , runningGroupId = groupId
                    , runningResult = resultVar
                    , runningStdout = stdoutRef
                    , runningStderr = stderrRef
                    , runningStdoutHandle = hout
                    , runningStderrHandle = herr
                    , runningSupervisor = supervisor
                    }
            restore (pure running) `onException` stopShellCommand running
        (hin, hout, herr, _) -> do
            terminateProcessGroup groupId processHandle
            closeOptionalPipes [hin, hout, herr]
            fail "Failed to capture command output"

stopShellCommand :: RunningCommand -> IO ()
stopShellCommand running = do
    finished <- tryReadMVar running.runningResult
    case finished of
        Just _ ->
            void (waitCatch running.runningSupervisor)
        Nothing -> do
            terminateProcessGroup running.runningGroupId running.runningHandle
            joined <- race
                (threadDelay 1000000)
                (readMVar running.runningResult)
            case joined of
                Right _ ->
                    void (waitCatch running.runningSupervisor)
                Left () -> do
                    closeRunningPipes running
                    cancel running.runningSupervisor
                    void (waitCatch running.runningSupervisor)
                    void $ tryPutMVar running.runningResult cancelledResult
    closeRunningPipes running

closeRunningPipes :: RunningCommand -> IO ()
closeRunningPipes running =
    mapM_ (void . try @_ @SomeException . hClose)
        [running.runningStdoutHandle, running.runningStderrHandle]

closeOptionalPipes :: [Maybe Handle] -> IO ()
closeOptionalPipes =
    mapM_ (mapM_ (void . try @_ @SomeException . hClose))

failedCommandResult :: SomeException -> CommandResult
failedCommandResult exception = CommandResult
    { commandExitCode = Just 127
    , commandStdout = ""
    , commandStderr = Text.pack (show exception)
    , commandTimedOut = False
    , commandCancelled = False
    }

-- | Read the handle in chunks so a live snapshot can see output before EOF.
drainHandle :: Int -> Handle -> IORef CapturedBytes -> IO CapturedBytes
drainHandle cap handle ref = go
  where
    go = do
        chunk <- BS.hGetSome handle 8192
        if BS.null chunk
            then readIORef ref
            else do
                atomicModifyIORef' ref \soFar ->
                    (appendCapturedBytes cap chunk soFar, ())
                go

emptyCapturedBytes :: CapturedBytes
emptyCapturedBytes = CapturedBytes
    { capturedBytes = BS.empty
    , capturedDropped = 0
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

truncateText :: Int -> Text -> Text
truncateText cap text
    | cap <= 0 = text
    | Text.length text <= cap = text
    | otherwise =
        Text.take cap text
            <> "\n...[truncated "
            <> Text.pack (show (Text.length text - cap))
            <> " chars]"

terminateProcessGroup
    :: Maybe ProcessGroupID
    -> ProcessHandle
    -> IO ()
terminateProcessGroup groupId processHandle = do
    alive <- processGroupAlive groupId processHandle
    whenAlive alive do
        signalGroup sigINT
        interrupted <- waitForProcessGroupExit groupId processHandle 250
        unless interrupted do
            signalGroup sigTERM
            void $ try @_ @SomeException (terminateProcess processHandle)
            terminated <- waitForProcessGroupExit groupId processHandle 750
            unless terminated do
                signalGroup sigKILL
                void $ try @_ @SomeException (terminateProcess processHandle)
                void $ waitForProcessGroupExit groupId processHandle 1000
  where
    whenAlive True action = action
    whenAlive False _ = pure ()

    signalGroup signal =
        case groupId of
            Just pid ->
                void $ try @_ @SomeException (signalProcessGroup signal pid)
            Nothing ->
                void $ try @_ @SomeException (terminateProcess processHandle)

waitForProcessGroupExit
    :: Maybe ProcessGroupID
    -> ProcessHandle
    -> Int
    -> IO Bool
waitForProcessGroupExit groupId processHandle timeoutMs =
    go (max 0 timeoutMs)
  where
    go remaining = do
        alive <- processGroupAlive groupId processHandle
        if not alive
            then pure True
            else if remaining <= 0
                then pure False
                else do
                    let delayMs = min 10 remaining
                    threadDelay (delayMs * 1000)
                    go (remaining - delayMs)

processGroupAlive
    :: Maybe ProcessGroupID
    -> ProcessHandle
    -> IO Bool
processGroupAlive groupId processHandle = do
    processExit <- getProcessExitCode processHandle
    case groupId of
        Nothing -> pure (case processExit of Nothing -> True; Just _ -> False)
        Just pid ->
            isRight <$> try @_ @SomeException
                (signalProcessGroup nullSignal pid)

decodeUtfPath :: OsPath -> FilePath
decodeUtfPath = either impureThrow id . decodeUtf
