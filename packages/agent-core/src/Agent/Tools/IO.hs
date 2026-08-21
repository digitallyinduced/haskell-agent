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
    , startShellCommand
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
import Control.Concurrent (forkFinally, threadDelay)
import Control.Monad (void)
import Control.Concurrent.Async (concurrently, race)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar)
import Control.Exception (evaluate)
import Control.Exception.Safe (SomeException, try)
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
runShellCommand env workdir command timeoutMs = do
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
            let collect = do
                    -- Drain stdout and stderr concurrently so a child that
                    -- fills one pipe cannot deadlock the other.
                    (outBytes, errBytes) <- concurrently
                        (strictGetContents hout)
                        (strictGetContents herr)
                    code <- waitForProcess processHandle
                    pure (outBytes, errBytes, code)
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

-- | Start a command without waiting. The result is written to 'runningResult'
-- when the process exits. Use 'interruptProcessGroupOf' on 'runningHandle' to kill it.
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
    try @_ @SomeException (createProcess spec) >>= \case
        Left err -> pure $ Left $ "Failed to start command: " <> Text.pack (show err)
        Right (Just hin, Just hout, Just herr, processHandle) -> do
            hClose hin
            resultVar <- newEmptyMVar
            stdoutRef <- newIORef BS.empty
            stderrRef <- newIORef BS.empty
            _ <- forkFinally
                (do
                    ((outBytes, errBytes), code) <- concurrently
                        (concurrently
                            (drainHandle hout stdoutRef)
                            (drainHandle herr stderrRef))
                        (waitForProcess processHandle)
                    pure CommandResult
                        { commandExitCode = Just (exitCodeInt code)
                        , commandStdout = truncateText env.toolStdoutCap
                            (decodeUtf8With lenientDecode outBytes)
                        , commandStderr = truncateText env.toolStdoutCap
                            (decodeUtf8With lenientDecode errBytes)
                        , commandTimedOut = False
                        , commandCancelled = False
                        })
                (publishCommandResult resultVar)
            pure $ Right RunningCommand
                { runningHandle = processHandle
                , runningResult = resultVar
                , runningStdout = stdoutRef
                , runningStderr = stderrRef
                , runningCap = env.toolStdoutCap
                }
        Right _ ->
            pure $ Left "Failed to capture command output"

publishCommandResult :: MVar CommandResult -> Either SomeException CommandResult -> IO ()
publishCommandResult resultVar result =
    putMVar resultVar (either failedCommandResult id result)

failedCommandResult :: SomeException -> CommandResult
failedCommandResult exception = CommandResult
    { commandExitCode = Just 127
    , commandStdout = ""
    , commandStderr = Text.pack (show exception)
    , commandTimedOut = False
    , commandCancelled = False
    }

strictGetContents :: Handle -> IO BS.ByteString
strictGetContents handle = do
    bytes <- BS.hGetContents handle
    _ <- evaluate (BS.length bytes)
    pure bytes

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
