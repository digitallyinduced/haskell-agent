-- | Keep native process diagnostics off the interactive terminal.
--
-- File descriptor 2 is process-global, so this scope belongs outside all
-- foreground session workers and provider restarts. Intentional UI output
-- must obtain 'getTerminalStderr', or retain that handle in its renderer.
module Agent.CLI.TerminalDiagnostics
    ( getTerminalStderr
    , dieToTerminal
    , withInteractiveDiagnostics
    , withTerminalDiagnostics
    , withTerminalDiagnosticsSuspended
    ) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception.Safe (bracket, bracket_, bracketOnError, finally, onException)
import Control.Monad (void)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text.IO as Text
import System.Directory (createDirectoryIfMissing, getHomeDirectory)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO
    ( Handle, hClose, hFlush, hGetBuffering, hGetEncoding, hIsTerminalDevice
    , hSetBinaryMode, hSetBuffering, hSetEncoding, stderr, stdin
    )
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Files (setFileMode)
import System.Posix.IO
    ( FdOption(AppendOnWrite, CloseOnExec), closeFd, dup, dupTo, fdToHandle, handleToFd
    , queryFdOption, setFdOption, stdError
    )
import System.Posix.Temp (mkstemp)
import System.Posix.Types (Fd)

-- The process has one foreground terminal. Keep this registry effectful so
-- tests and non-interactive callers still observe the current stderr handle.
{-# NOINLINE terminalStderrReference #-}
terminalStderrReference :: IORef (Maybe (Handle, Fd))
terminalStderrReference = unsafePerformIO (newIORef Nothing)

{-# NOINLINE terminalDiagnosticsLock #-}
terminalDiagnosticsLock :: MVar ()
terminalDiagnosticsLock = unsafePerformIO (newMVar ())

getTerminalStderr :: IO Handle
getTerminalStderr = maybe stderr fst <$> readIORef terminalStderrReference

-- | Like 'System.Exit.die', but preserve visible foreground errors while
-- native diagnostics are redirected.
dieToTerminal :: Text -> IO a
dieToTerminal message = do
    terminal <- getTerminalStderr
    Text.hPutStrLn terminal message
    hFlush terminal
    exitFailure

-- | Non-interactive invocations, redirected stderr, and one-shot commands
-- retain the ordinary operating-system stderr behavior.
withInteractiveDiagnostics :: Bool -> IO a -> IO a
withInteractiveDiagnostics interactive action = do
    terminal <- (&&) <$> hIsTerminalDevice stdin <*> hIsTerminalDevice stderr
    if not interactive || not terminal
        then action
        else do
            home <- getHomeDirectory
            let directory = home </> ".haskell-agent" </> "logs"
            createDirectoryIfMissing True directory
            setFileMode directory 0o700
            withTerminalDiagnostics directory (\_ -> action)

-- | Retain a private, uniquely named log and restore descriptor 2 even when
-- the action throws. The action receives its log path for diagnostics/tests.
-- Subprocesses inheriting stderr also write to this log; children that should
-- interact with the user must receive the terminal handle explicitly.
-- Do not nest this process-level scope.
withTerminalDiagnostics :: FilePath -> (FilePath -> IO a) -> IO a
withTerminalDiagnostics directory action =
    withMVar terminalDiagnosticsLock \_ ->
    bracket duplicateTerminalStderr hClose \terminalHandle ->
    bracket (dup stdError) closeFd \originalDescriptor -> do
        setFdOption originalDescriptor CloseOnExec True
        originalCloseOnExec <- queryFdOption stdError CloseOnExec
        bracket
            (do
                (path, handle) <- mkstemp (directory </> "native-diagnostics-")
                descriptor <- handleToFd handle `onException` hClose handle
                pure (path, descriptor))
            (closeFd . snd)
            \(path, logDescriptor) -> do
                setFdOption logDescriptor CloseOnExec True
                setFdOption logDescriptor AppendOnWrite True
                hFlush stderr
                bracket_
                    (void (dupTo logDescriptor stdError))
                    (hFlush stderr `finally` do
                        void (dupTo originalDescriptor stdError)
                        setFdOption stdError CloseOnExec originalCloseOnExec)
                    (bracket_
                        (writeIORef terminalStderrReference (Just (terminalHandle, originalDescriptor)))
                        (writeIORef terminalStderrReference Nothing)
                        (action path))

-- | Return inherited stderr to the terminal for a process handoff, after all
-- session UI workers have stopped. A successful exec keeps the terminal;
-- returning or throwing resumes diagnostic isolation. This is process-global
-- and must not run concurrently with another handoff.
withTerminalDiagnosticsSuspended :: IO a -> IO a
withTerminalDiagnosticsSuspended action = do
    current <- readIORef terminalStderrReference
    case current of
        Nothing -> action
        Just (_, terminalDescriptor) ->
            bracket (dup stdError) closeFd \diagnosticDescriptor -> do
                setFdOption diagnosticDescriptor CloseOnExec True
                originalCloseOnExec <- queryFdOption stdError CloseOnExec
                hFlush stderr
                bracket_
                    (void (dupTo terminalDescriptor stdError))
                    (hFlush stderr `finally` do
                        void (dupTo diagnosticDescriptor stdError)
                        setFdOption stdError CloseOnExec originalCloseOnExec)
                    action

-- Do not let subprocesses inherit an additional route to the terminal.
duplicateTerminalStderr :: IO Handle
duplicateTerminalStderr =
    bracketOnError
        (bracketOnError (dup stdError) closeFd \descriptor -> do
            setFdOption descriptor CloseOnExec True
            fdToHandle descriptor)
        hClose
        \handle -> do
            encoding <- hGetEncoding stderr
            case encoding of
                Nothing -> hSetBinaryMode handle True
                Just value -> hSetEncoding handle value
            hGetBuffering stderr >>= hSetBuffering handle
            pure handle
