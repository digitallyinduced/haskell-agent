-- Worker process lifecycle is isolated from cell and tool orchestration.
module Agent.Tools.CodeMode.IsolatedHost.Process
    ( spawnWorker
    , stopWorker
    , sendLine
    , killWorker
    ) where

import Prelude
import Agent.Tools.CodeMode.IsolatedHost.Types
import Agent.Tools.CodeMode.Host.Availability (resolveBunExecutable, resolveWorkerScript)
import Agent.Tools.CodeMode.IsolatedHost.Limits
import Agent.Tools.CodeMode.Protocol.Value
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (asyncWithUnmask, cancel, race, waitCatch)
import Control.Concurrent.MVar (newMVar, withMVar)
import Control.Exception.Safe (bracketOnError, displayException, onException, tryAny)
import Control.Monad (void)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as Text
import System.IO (BufferMode(NoBuffering), Handle, hClose, hFlush, hSetBinaryMode, hSetBuffering)
import System.Posix.Signals (sigKILL, signalProcessGroup)
import System.Posix.Types (ProcessGroupID)
import System.Process

spawnWorker :: CodeModeConfig -> IO (Either CodeModeError Worker)
spawnWorker config = do
    executable <- resolveBunExecutable config.bunExecutable
    script <- resolveWorkerScript config.workerScript
    case (executable, script) of
        (Nothing, _) -> pure (Left (CodeModeStartupError "Bun executable not found"))
        (_, Nothing) -> pure (Left (CodeModeStartupError "Code-mode worker not found"))
        (Just bun, Just worker) -> do
            result <- tryAny $
                bracketOnError
                    (config.prepareWorkerProcess bun worker >>=
                        either (ioError . userError . Text.unpack) createWorkerProcess)
                    closeIncomplete
                    \(input, output, stderr, process) -> case (input, output, stderr) of
                        (Just childIn, Just childOut, Just childErr) -> do
                            mapM_ configure [childIn, childOut, childErr]
                            group <- getPid process
                            writer <- newMVar ()
                            -- Owned by Worker; stopWorker cancels and joins it.
                            errorReader <- asyncWithUnmask (\unmask -> unmask (drainError childErr))
                            let child = Worker childIn childOut childErr process group writer errorReader
                            (do
                                readFrame <- boundedProtocolReader childOut
                                ready <- race
                                    (threadDelay (max 1 config.startupTimeoutMs * 1000))
                                    readFrame
                                case ready of
                                    Right line | Right WorkerReady <- decodeProtocolMessage line ->
                                        pure (Right child)
                                    _ -> stopWorker child >>
                                        pure (Left (CodeModeStartupError "Worker did not become ready")))
                                `onException` stopWorker child
                        _ -> ioError (userError "Worker pipe creation failed")
            pure case result of
                Left err -> Left (CodeModeStartupError (Text.pack (displayException err)))
                Right childResult -> childResult
  where
    -- Pipe ownership and process-group termination are host invariants, not
    -- optional launcher settings. Containment/environment remain caller-owned.
    createWorkerProcess process = createProcess process
        { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe
        , create_group = True, close_fds = True
        }
    configure handle = hSetBinaryMode handle True >> hSetBuffering handle NoBuffering
    -- Drain without retaining arbitrary worker stderr in the application heap.
    drainError handle = BS.hGetSome handle 4096 >>= \bytes ->
        if BS.null bytes then pure () else drainError handle
    closeIncomplete (input, output, stderr, process) = do
        group <- getPid process
        killProcess process group
        mapM_ (mapM_ closeQuietly) [input, output, stderr]
        void (tryAny (waitForProcess process))

sendLine :: Worker -> BS.ByteString -> IO ()
sendLine worker bytes = withMVar worker.workerWriter \() -> do
    BS.hPut worker.workerInput bytes
    BS8.hPutStrLn worker.workerInput ""
    hFlush worker.workerInput

stopWorker :: Worker -> IO ()
stopWorker worker = do
    -- Cells are disposable: kill the group before joining pipe readers so a
    -- CPU-bound cell cannot prevent cancellation. Do not wait on JS timers.
    killProcess worker.workerProcess worker.workerGroup
    cancel worker.workerErrorReader
    void (waitCatch worker.workerErrorReader)
    mapM_ closeQuietly [worker.workerInput, worker.workerOutput, worker.workerError]
    void (tryAny (waitForProcess worker.workerProcess))

-- Kill without reaping: the owner later joins readers and reaps, so the PID
-- cannot be recycled while a timed-out cell remains in the host registry.
killWorker :: Worker -> IO ()
killWorker worker = killProcess worker.workerProcess worker.workerGroup

killProcess :: ProcessHandle -> Maybe ProcessGroupID -> IO ()
killProcess process group = do
    case group of
        Just identifier -> void (tryAny (signalProcessGroup sigKILL identifier))
        Nothing -> void (tryAny (terminateProcess process))

closeQuietly :: Handle -> IO ()
closeQuietly handle = void (tryAny (hClose handle))
