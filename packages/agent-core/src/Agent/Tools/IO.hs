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
    , runningLiveOutput
    , truncateText
    ) where

import Agent.Cancel (isCancelled, waitCancel)
import Agent.OsPath (OsPath, toFilePath)
import Agent.Tools.FileSystem
    ( deleteTextFile
    , listDirectoryEntries
    , readTextFile
    , renameTextFile
    , resolveUnderCwd
    , writeTextFile
    )
import Agent.Tools.Types (ToolEnv(..))
import Control.Concurrent (threadDelay)
import Control.Monad (void, when)
import Control.Concurrent.Async
    ( Async
    , async
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
    , tryPutMVar
    )
import Control.Exception.Safe
    ( SomeException
    , finally
    , mask
    , onException
    , try
    )
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.ByteString as BS
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import System.Exit (ExitCode(..))
import System.IO (Handle, hClose)
import System.Process
    ( CreateProcess(..)
    , ProcessHandle
    , StdStream(..)
    , createProcess
    , interruptProcessGroupOf
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
    , runningResult :: !(MVar CommandResult)
    , runningStdout :: !(IORef BS.ByteString)
    , runningStderr :: !(IORef BS.ByteString)
    , runningCap :: !Int
    , runningSupervisor :: !(Async ())
    }

-- | Bytes already drained from a still-running command, decoded and capped.
runningLiveOutput :: RunningCommand -> IO (Text, Text)
runningLiveOutput running = do
    outBytes <- readIORef running.runningStdout
    errBytes <- readIORef running.runningStderr
    pure
        ( truncateText running.runningCap (decodeUtf8With lenientDecode outBytes)
        , truncateText running.runningCap (decodeUtf8With lenientDecode errBytes)
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
runShellCommandStreaming env workdir command timeoutMs onSnapshot = do
    let spec = (shell command)
            { cwd = Just (toFilePath workdir)
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
            hClose hin
            stdoutRef <- newIORef BS.empty
            stderrRef <- newIORef BS.empty
            lastSnapshotRef <- newIORef Nothing
            let collect = do
                    -- Drain stdout and stderr concurrently so a child that
                    -- fills one pipe cannot deadlock the other.
                    withAsync sampleSnapshots \_sampler -> do
                        (outBytes, errBytes) <- concurrently
                            (drainHandle hout stdoutRef)
                            (drainHandle herr stderrRef)
                        code <- waitForProcess processHandle
                        emitSnapshot
                        pure (outBytes, errBytes, code)
                sampleSnapshots = do
                    threadDelay 100000
                    emitSnapshot
                    sampleSnapshots
                emitSnapshot = do
                    outBytes <- readIORef stdoutRef
                    errBytes <- readIORef stderrRef
                    let out = truncateText env.toolStdoutCap
                            (decodeUtf8With lenientDecode outBytes)
                        err = truncateText env.toolStdoutCap
                            (decodeUtf8With lenientDecode errBytes)
                        snapshot = (out, err)
                    changed <- atomicModifyIORef' lastSnapshotRef \previous ->
                        if previous == Just snapshot
                            then (previous, False)
                            else (Just snapshot, True)
                    when (changed && not (Text.null out && Text.null err)) $
                        onSnapshot out err
                killGroup = void $ try @_ @SomeException
                    (interruptProcessGroupOf processHandle)
                -- Prefer cancel over timeout when both fire: race cancel
                -- against (timeout `race` collect).
                waitStop = do
                    timed <- race (threadDelay (max 1 timeoutMs * 1000)) collect
                    pure $ case timed of
                        Left () -> StopTimeout
                        Right triple -> StopDone triple
            already <- isCancelled env.toolCancel
            if already
                then do
                    killGroup
                    pure cancelledResult
                else do
                    stopped <- race (waitCancel env.toolCancel) waitStop
                    case stopped of
                        Left () -> do
                            killGroup
                            pure cancelledResult
                        Right StopTimeout -> do
                            killGroup
                            pure CommandResult
                                { commandExitCode = Nothing
                                , commandStdout = ""
                                , commandStderr = ""
                                , commandTimedOut = True
                                , commandCancelled = False
                                }
                        Right (StopDone (outBytes, errBytes, code)) ->
                            pure CommandResult
                                { commandExitCode = Just (exitCodeInt code)
                                , commandStdout = truncateText env.toolStdoutCap
                                    (decodeUtf8With lenientDecode outBytes)
                                , commandStderr = truncateText env.toolStdoutCap
                                    (decodeUtf8With lenientDecode errBytes)
                                , commandTimedOut = False
                                , commandCancelled = False
                                }
        Right _ ->
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
    | StopDone (BS.ByteString, BS.ByteString, ExitCode)

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
            { cwd = Just (toFilePath workdir)
            , std_in = CreatePipe
            , std_out = CreatePipe
            , std_err = CreatePipe
            , create_group = True
            }
    try @_ @SomeException
        (acquireRunningCommand spec env.toolStdoutCap) >>= \case
        Left err -> pure $ Left $ "Failed to start command: " <> Text.pack (show err)
        Right running ->
            pure (Right running)

acquireRunningCommand :: CreateProcess -> Int -> IO RunningCommand
acquireRunningCommand spec outputCap =
    mask \restore ->
    restore (createProcess spec) >>= \case
        (Just hin, Just hout, Just herr, processHandle) -> do
            let rollback = cleanupProcess processHandle [hin, hout, herr]
            flip onException rollback do
                hClose hin
                resultVar <- newEmptyMVar
                stdoutRef <- newIORef BS.empty
                stderrRef <- newIORef BS.empty
                supervisor <- async $
                    (do
                        result <- try @_ @SomeException do
                            ((outBytes, errBytes), code) <- concurrently
                                (concurrently
                                    (drainHandle hout stdoutRef)
                                    (drainHandle herr stderrRef))
                                (waitForProcess processHandle)
                            pure (outBytes, errBytes, code)
                        void $ tryPutMVar resultVar $ case result of
                            Left exception -> commandFailure exception
                            Right (outBytes, errBytes, code) -> CommandResult
                                { commandExitCode = Just (exitCodeInt code)
                                , commandStdout = truncateText outputCap
                                    (decodeUtf8With lenientDecode outBytes)
                                , commandStderr = truncateText outputCap
                                    (decodeUtf8With lenientDecode errBytes)
                                , commandTimedOut = False
                                , commandCancelled = False
                                })
                    `finally` do
                        void $ tryPutMVar resultVar cancelledResult
                        mapM_
                            (\handle ->
                                void $ try @_ @SomeException (hClose handle))
                            [hout, herr]
                pure RunningCommand
                    { runningHandle = processHandle
                    , runningResult = resultVar
                    , runningStdout = stdoutRef
                    , runningStderr = stderrRef
                    , runningCap = outputCap
                    , runningSupervisor = supervisor
                    }
        _ -> fail "Failed to capture command output"

cleanupProcess :: ProcessHandle -> [Handle] -> IO ()
cleanupProcess processHandle handles = do
    void $ try @_ @SomeException (interruptProcessGroupOf processHandle)
    void $ try @_ @SomeException (terminateProcess processHandle)
    mapM_ (\handle -> void $ try @_ @SomeException (hClose handle)) handles
    void $ try @_ @SomeException (waitForProcess processHandle)

stopShellCommand :: RunningCommand -> IO ()
stopShellCommand running = do
    void $ try @_ @SomeException
        (interruptProcessGroupOf running.runningHandle)
    stopped <- race
        (threadDelay 5000000)
        (readMVar running.runningResult)
    case stopped of
        Right _ ->
            void (waitCatch running.runningSupervisor)
        Left () -> do
            void $ try @_ @SomeException
                (terminateProcess running.runningHandle)
            stoppedAfterTerminate <- race
                (threadDelay 1000000)
                (readMVar running.runningResult)
            case stoppedAfterTerminate of
                Right _ ->
                    void (waitCatch running.runningSupervisor)
                Left () -> do
                    cancel running.runningSupervisor
                    void (waitCatch running.runningSupervisor)
                    void $ tryPutMVar running.runningResult cancelledResult

commandFailure :: SomeException -> CommandResult
commandFailure exception = CommandResult
    { commandExitCode = Just 127
    , commandStdout = ""
    , commandStderr = Text.pack (show exception)
    , commandTimedOut = False
    , commandCancelled = False
    }

-- | Read the handle in chunks so a live snapshot can see output before EOF.
drainHandle :: Handle -> IORef BS.ByteString -> IO BS.ByteString
drainHandle handle ref = go
  where
    go = do
        chunk <- BS.hGetSome handle 8192
        if BS.null chunk
            then readIORef ref
            else do
                atomicModifyIORef' ref \soFar -> (soFar <> chunk, ())
                go

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
