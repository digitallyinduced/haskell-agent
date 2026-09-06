-- | Check-process validation and ownership. Repository locking and snapshot
-- validation remain with the caller, which must hold the lock during startup.
module Agent.CLI.RepositoryReview.Checks
    ( RepositoryCheck
    , RepositoryCheckStream(..)
    , startRepositoryCheckAtRoot
    , cancelRepositoryCheck
    , waitRepositoryCheck
    ) where

import Agent.CLI.RepositoryReview.Error
import Agent.CLI.RepositoryReview.ProcessSupport
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
    ( Async, asyncWithUnmask, cancel, poll, waitCatch, withAsync )
import Control.Exception.Safe (finally, mask, onException, tryAny)
import qualified Data.ByteString as BS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Text as Text
import System.Exit (ExitCode)
import System.IO (Handle, hClose)
import System.Posix.Signals (sigKILL, sigTERM)
import System.Posix.Types (ProcessID)
import System.Process
    ( CreateProcess(..), ProcessHandle, StdStream(CreatePipe)
    , createProcess, getPid, proc, terminateProcess, waitForProcess
    )
import System.Timeout (timeout)

data RepositoryCheckStream
    = RepositoryCheckStdout
    | RepositoryCheckStderr
    deriving (Eq, Show)

data RepositoryCheck = RepositoryCheck
    { repositoryCheckProcess :: !ProcessHandle
    , repositoryCheckProcessGroup :: !(Maybe ProcessID)
    , repositoryCheckWorker :: !(Async ())
    , repositoryCheckCancelled :: !(IORef Bool)
    }

startRepositoryCheckAtRoot
    :: FilePath
    -> FilePath
    -> [String]
    -> (RepositoryCheckStream -> BS.ByteString -> IO ())
    -> (Bool -> ExitCode -> IO ())
    -> IO (Either RepositoryError RepositoryCheck)
startRepositoryCheckAtRoot root executable arguments onOutput onExit
    | null executable || '\NUL' `elem` executable =
        pure
            (Left
                (InvalidRepositoryRequest
                    "check executable is invalid"))
    | length executable > maxRepositoryArgumentCharacters =
        pure
            (Left
                (InvalidRepositoryRequest
                    "check executable exceeds the 1 MiB character limit"))
    | length arguments > maxRepositoryCheckArguments =
        pure
            (Left
                (InvalidRepositoryRequest
                    "check has too many arguments"))
    | any (elem '\NUL') arguments =
        pure
            (Left
                (InvalidRepositoryRequest
                    "check argument contains a NUL byte"))
    | any null arguments =
        pure
            (Left
                (InvalidRepositoryRequest
                    "check argument is empty"))
    | any
        ((> maxRepositoryArgumentCharacters) . length)
        arguments =
            pure
                (Left
                    (InvalidRepositoryRequest
                        "check argument exceeds the 1 MiB character limit"))
    | sum (map (toInteger . length) arguments)
        > toInteger maxRepositoryCheckTotalCharacters =
            pure
                (Left
                    (InvalidRepositoryRequest
                        "check arguments exceed the 8 MiB total limit"))
    | otherwise = do
        -- Keep asynchronous cancellation masked across process
        -- acquisition and construction of its owning worker.
        -- Once this returns, every resource is reachable through
        -- RepositoryCheck and cancellation is owned by that
        -- worker's finalizer.
        created <- trySynchronous (mask \_ -> do
            (maybeOutput, maybeError, process, processGroup) <-
                createCheckProcess root executable arguments
            cancelled <- newIORef False
            let cleanup = do
                    closeQuietly maybeOutput
                    closeQuietly maybeError
                    signalCheckProcessGroup
                        sigKILL processGroup process
                    _ <- tryAny (waitForProcess process)
                    pure ()
            worker <- asyncWithUnmask
                (\unmask ->
                    unmask
                        (withAsync
                            (drainCheckOutput
                                RepositoryCheckStdout
                                maybeOutput)
                            \outputReader ->
                                withAsync
                                    (drainCheckOutput
                                        RepositoryCheckStderr
                                        maybeError)
                                    \errorReader -> do
                                        exitCode <-
                                            waitForProcess process
                                        -- The requested leader is
                                        -- complete. Any process
                                        -- still in its dedicated
                                        -- group is a residual
                                        -- descendant and must not
                                        -- outlive the check.
                                        signalCheckProcessGroup
                                            sigKILL
                                            processGroup
                                            process
                                        -- Reader failures
                                        -- (including callback
                                        -- failures) must not
                                        -- suppress the one terminal
                                        -- callback. An escaped
                                        -- process outside the
                                        -- group still cannot hold
                                        -- this worker indefinitely.
                                        drained <- timeout
                                            processPipeTeardownMicros
                                            ((,)
                                                <$> waitCatch outputReader
                                                <*> waitCatch errorReader)
                                        case drained of
                                            Just _ -> pure ()
                                            Nothing -> do
                                                closeQuietly
                                                    maybeOutput
                                                closeQuietly
                                                    maybeError
                                                cancel outputReader
                                                cancel errorReader
                                        wasCancelled <-
                                            readIORef cancelled
                                        onExit
                                            wasCancelled
                                            exitCode)
                        `finally` cleanup)
                    `onException` cleanup
            pure RepositoryCheck
                { repositoryCheckProcess = process
                , repositoryCheckProcessGroup = processGroup
                , repositoryCheckWorker = worker
                , repositoryCheckCancelled = cancelled
                })
        pure case created of
            Left exception ->
                Left
                    (RepositoryCommandFailed
                        (renderCommand executable arguments)
                        (-1)
                        (Text.pack (show exception)))
            Right check -> Right check
  where
    drainCheckOutput stream handle =
        let drain callbackEnabled = do
                bytes <- BS.hGetSome handle (64 * 1024)
                if BS.null bytes
                    then pure ()
                    else do
                        nextEnabled <-
                            if callbackEnabled
                                then
                                    trySynchronous (onOutput stream bytes) >>= \case
                                        Left _ -> pure False
                                        Right () -> pure True
                                else pure False
                        -- A failed foreign callback is disabled for this
                        -- stream, but the pipe remains actively drained.
                        drain nextEnabled
        in drain True `finally` closeQuietly handle

cancelRepositoryCheck :: RepositoryCheck -> IO ()
cancelRepositoryCheck check = do
    writeIORef check.repositoryCheckCancelled True
    signalCheckProcessGroup
        sigTERM
        check.repositoryCheckProcessGroup
        check.repositoryCheckProcess
    threadDelay 250_000
    poll check.repositoryCheckWorker >>= \case
        Nothing ->
            signalCheckProcessGroup
                sigKILL
                check.repositoryCheckProcessGroup
                check.repositoryCheckProcess
        Just _ -> pure ()
    -- Cancellation owns process teardown: do not return while descendants,
    -- pipe readers, or the terminal callback are still active.
    waitRepositoryCheck check

waitRepositoryCheck :: RepositoryCheck -> IO ()
waitRepositoryCheck check = do
    _ <- waitCatch check.repositoryCheckWorker
    pure ()

createCheckProcess
    :: FilePath
    -> FilePath
    -> [String]
    -> IO (Handle, Handle, ProcessHandle, Maybe ProcessID)
createCheckProcess root executable arguments = mask \_ -> do
    (maybeInput, maybeOutput, maybeError, process) <-
        createProcess
            (proc executable arguments)
                { cwd = Just root
                , std_in = CreatePipe
                , std_out = CreatePipe
                , std_err = CreatePipe
                , close_fds = True
                , create_group = True
                }
    case (maybeInput, maybeOutput, maybeError) of
        (Just input, Just output, Just errors) -> do
            let cleanupWithoutGroup = do
                    closeQuietly input
                    closeQuietly output
                    closeQuietly errors
                    _ <- tryAny (terminateProcess process)
                    _ <- tryAny (waitForProcess process)
                    pure ()
            processGroup <- getPid process
                `onException` cleanupWithoutGroup
            let cleanup = do
                    closeQuietly input
                    closeQuietly output
                    closeQuietly errors
                    signalCheckProcessGroup sigKILL processGroup process
                    _ <- tryAny (waitForProcess process)
                    pure ()
            -- Checks have no stdin API. Closing immediately prevents an
            -- inherited terminal or pipe from leaving a check blocked.
            (hClose input >> pure (output, errors, process, processGroup))
                `onException` cleanup
        _ -> do
            _ <- tryAny (terminateProcess process)
            _ <- tryAny (waitForProcess process)
            fail "could not create check output pipes"

maxRepositoryCheckArguments :: Int
maxRepositoryCheckArguments = 4096

maxRepositoryArgumentCharacters :: Int
maxRepositoryArgumentCharacters = 1024 * 1024

maxRepositoryCheckTotalCharacters :: Int
maxRepositoryCheckTotalCharacters = 8 * 1024 * 1024
