module Agent.CLI.TerminalDiagnosticsSpec (spec) where

import Agent.CLI.ExternalProgram (ExternalProgram(..), runExternalProgramOnFile)
import Agent.CLI.TerminalDiagnostics
import Control.Concurrent.Async (cancel, withAsync)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception.Safe (bracket, bracket_, finally, throwIO)
import Control.Monad (replicateM_, void)
import Data.Bits ((.&.))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import System.Exit (ExitCode(ExitFailure))
import System.IO (hFlush, hPutStrLn, stderr)
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (fileMode, getFileStatus)
import System.Posix.IO
    ( FdOption(CloseOnExec), closeFd, createPipe, dup, dupTo, fdWrite
    , queryFdOption, setFdOption, stdError )
import System.Posix.Types (Fd)
import System.Process (callProcess)
import Test.Hspec
import qualified Data.Text.IO as Text
import qualified Data.Text.Encoding as Text
import qualified System.Posix.IO.ByteString as Posix

-- Descriptor 2 is process-global; these examples must remain sequential.
spec :: Spec
spec = describe "native terminal diagnostics" do
    it "isolates descriptor writes while preserving intentional UI output" $
        withSystemTempDirectory "terminal-diagnostics" \directory ->
        captureStderr \output -> do
            path <- withTerminalDiagnostics directory \path -> do
                terminal <- getTerminalStderr
                hPutStrLn terminal "Visible UI"
                hFlush terminal
                void (fdWrite stdError "Native diagnostic\n")
                pure path
            void (fdWrite stdError "Restored diagnostic\n")
            readCapture output `shouldReturn` "Visible UI\nRestored diagnostic\n"
            Text.readFile path `shouldReturn` "Native diagnostic\n"
            mode <- fileMode <$> getFileStatus path
            mode .&. 0o777 `shouldBe` 0o600
            getTerminalStderr `shouldReturn` stderr

    it "restores descriptor 2 and the UI handle when the action throws" $
        withSystemTempDirectory "terminal-diagnostics" \directory ->
        captureStderr \output -> do
            pathReference <- newIORef Nothing
            withTerminalDiagnostics directory (\path -> do
                writeIORef pathReference (Just path)
                void (fdWrite stdError "Before exception\n")
                throwIO (userError "diagnostic test"))
                `shouldThrow` anyIOException
            void (fdWrite stdError "After exception\n")
            readCapture output `shouldReturn` "After exception\n"
            path <- readIORef pathReference
            case path of
                Nothing -> expectationFailure "the diagnostic scope did not start"
                Just value -> Text.readFile value `shouldReturn` "Before exception\n"
            getTerminalStderr `shouldReturn` stderr

    it "does not redirect one-shot output" $
        captureStderr \output -> do
            withInteractiveDiagnostics False $
                void (fdWrite stdError "One-shot diagnostic\n")
            readCapture output `shouldReturn` "One-shot diagnostic\n"

    it "keeps fatal foreground errors visible and restores stderr" $
        withSystemTempDirectory "terminal-diagnostics" \directory ->
        captureStderr \output -> do
            withTerminalDiagnostics directory (\_ -> dieToTerminal "Visible fatal error")
                `shouldThrow` (== ExitFailure 1)
            readCapture output `shouldReturn` "Visible fatal error\n"
            getTerminalStderr `shouldReturn` stderr

    it "does not redirect non-terminal output" $
        captureStderr \output -> do
            withInteractiveDiagnostics True $
                void (fdWrite stdError "Redirected diagnostic\n")
            readCapture output `shouldReturn` "Redirected diagnostic\n"

    it "restores descriptor flags after asynchronous cancellation" $
        withSystemTempDirectory "terminal-diagnostics" \directory ->
        captureStderr \output -> do
            entered <- newEmptyMVar
            blocked <- newEmptyMVar
            originalFlags <- queryFdOption stdError CloseOnExec
            bracket_
                (setFdOption stdError CloseOnExec True)
                (setFdOption stdError CloseOnExec originalFlags)
                (do
                    withAsync
                        (withTerminalDiagnostics directory \_ ->
                            putMVar entered () >> takeMVar blocked)
                        \worker -> takeMVar entered >> cancel worker
                    queryFdOption stdError CloseOnExec `shouldReturn` True
                    getTerminalStderr `shouldReturn` stderr
                    void (fdWrite stdError "After cancellation\n")
                    readCapture output `shouldReturn` "After cancellation\n")

    it "sends inherited subprocess diagnostics to the log" $
        withSystemTempDirectory "terminal-diagnostics" \directory ->
        captureStderr \output -> do
            path <- withTerminalDiagnostics directory \path -> do
                callProcess "/bin/sh" ["-c", "printf 'Child diagnostic\\n' >&2"]
                pure path
            Text.readFile path `shouldReturn` "Child diagnostic\n"
            void (fdWrite stdError "Parent restored\n")
            readCapture output `shouldReturn` "Parent restored\n"

    it "keeps interactive child stderr visible without exposing parent diagnostics" $
        withSystemTempDirectory "terminal-diagnostics" \directory ->
        captureStderr \output -> do
            path <- withTerminalDiagnostics directory \path -> do
                replicateM_ 2 $
                    runExternalProgramOnFile
                        (ExternalProgram "/bin/sh" ["-c", "printf 'Editor prompt\\n' >&2"])
                        path
                        `shouldReturn` Right ()
                terminal <- getTerminalStderr
                hPutStrLn terminal "Parent UI resumed"
                hFlush terminal
                void (fdWrite stdError "Parent native diagnostic\n")
                pure path
            readCapture output `shouldReturn` "Editor prompt\nEditor prompt\nParent UI resumed\n"
            Text.readFile path `shouldReturn` "Parent native diagnostic\n"

    it "preserves interactive child stderr outside diagnostic isolation" $
        captureStderr \output -> do
            runExternalProgramOnFile
                (ExternalProgram "/bin/sh" ["-c", "printf 'Pager error\\n' >&2; exit 7"])
                "unused"
                `shouldReturn` Left "/bin/sh exited with status 7"
            readCapture output `shouldReturn` "Pager error\n"

    it "suspends isolation for subprocess handoffs and resumes after errors" $
        withSystemTempDirectory "terminal-diagnostics" \directory ->
        captureStderr \output -> do
            path <- withTerminalDiagnostics directory \path -> do
                withTerminalDiagnosticsSuspended do
                    callProcess "/bin/sh" ["-c", "printf 'Installer output\\n' >&2"]
                withTerminalDiagnosticsSuspended (throwIO (userError "handoff failed"))
                    `shouldThrow` anyIOException
                void (fdWrite stdError "Isolation resumed\n")
                pure path
            readCapture output `shouldReturn` "Installer output\n"
            Text.readFile path `shouldReturn` "Isolation resumed\n"

captureStderr :: (Fd -> IO a) -> IO a
captureStderr action =
    bracket createPipe
        (\(input, output) -> closeFd input `finally` closeFd output)
        \(input, output) ->
    bracket (dup stdError)
        (\original -> void (dupTo original stdError) `finally` closeFd original)
        (\_ -> void (dupTo output stdError) >> action input)

readCapture :: Fd -> IO Text
readCapture input = Text.decodeUtf8 <$> Posix.fdRead input 4096
