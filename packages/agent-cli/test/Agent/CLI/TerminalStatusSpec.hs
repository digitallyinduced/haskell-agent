module Agent.CLI.TerminalStatusSpec (spec) where

import Agent.CLI.TerminalStatus
import Control.Concurrent (yield)
import Control.Concurrent.Async (cancel, concurrently_, wait, withAsync)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception.Safe (bracket, bracket_, throwIO)
import Control.Monad (replicateM_, void)
import qualified Data.ByteString as ByteString
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import System.IO (BufferMode(..), hFlush, hGetBuffering, hPutStr, hSetBuffering, stdout)
import System.Posix.IO (closeFd, createPipe, dup, dupTo, stdOutput)
import qualified System.Posix.IO.ByteString as Posix
import System.Posix.Terminal (openPseudoTerminal)
import System.Posix.Types (Fd)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "terminal lifecycle status" do
    it "uses fixed OSC 7501 tokens with ST termination" do
        terminalStatusSequence TerminalWorking
            `shouldBe` "\ESC]7501;state=working:app=haskell-agent\ESC\\"
        terminalStatusSequence TerminalClear
            `shouldBe` "\ESC]7501;state=clear\ESC\\"
    it "reports idle, work, idle and clears on exit" do
        actual <- captureTerminal $ withTerminalStatus True $
            withTerminalWorking (pure ())
        actual `shouldBe` sequences [TerminalIdle, TerminalWorking, TerminalIdle, TerminalClear]
    it "retains blocked status until every input wait finishes" do
        actual <- captureTerminal $ withTerminalStatus True $ withTerminalWorking do
            beginTerminalInputWait
            beginTerminalInputWait
            endTerminalInputWait
            withTerminalWorking (pure ())
            endTerminalInputWait
        actual `shouldBe` sequences
            [TerminalIdle, TerminalWorking, TerminalBlocked, TerminalWorking, TerminalIdle, TerminalClear]
    it "keeps OSC intact alongside direct terminal writes with unbuffered stdout" do
        replicateM_ 20 do
            actual <- captureTerminal $
                bracket (hGetBuffering stdout) (hSetBuffering stdout) \_ -> do
                    hSetBuffering stdout NoBuffering
                    withTerminalStatus True $
                        concurrently_
                            (replicateM_ 5 (withTerminalWorking yield))
                            (replicateM_ 100 (void (Posix.fdWrite stdOutput "z") >> yield))
            let withoutStatus = foldr
                    (\status -> Text.replace (terminalStatusSequence status) "")
                    actual [TerminalIdle, TerminalWorking, TerminalClear]
            withoutStatus `shouldBe` Text.replicate 100 "z"
    it "clears during nested handoff and restores the current status" do
        actual <- captureTerminal $ withTerminalStatus True $ withTerminalWorking $
            withTerminalStatusSuspended $ withTerminalStatusSuspended (pure ())
        actual `shouldBe` sequences
            [TerminalIdle, TerminalWorking, TerminalClear, TerminalWorking, TerminalIdle, TerminalClear]
    it "restores state after exceptions" do
        actual <- captureTerminal $
            (withTerminalStatus True $ withTerminalWorking $
                throwIO (userError "test")) `shouldThrow` anyIOException
        actual `shouldBe` sequences [TerminalIdle, TerminalWorking, TerminalIdle, TerminalClear]
    it "does not emit for noninteractive callers" do
        actual <- captureTerminal $ withTerminalStatus False $
            withTerminalWorking (pure ())
        actual `shouldBe` ""
    it "does not emit when stdout is redirected" do
        actual <- captureOutput createPipe $ withTerminalStatus True $
            withTerminalWorking (pure ())
        actual `shouldBe` ""
    it "restores working status when an input waiter is cancelled" do
        ready <- newEmptyMVar
        blocked <- newEmptyMVar
        actual <- captureTerminal $ withTerminalStatus True $ withTerminalWorking $
            withAsync
                (bracket_ beginTerminalInputWait endTerminalInputWait $
                    putMVar ready () >> takeMVar blocked)
                \worker -> takeMVar ready >> cancel worker
        actual `shouldBe` sequences
            [TerminalIdle, TerminalWorking, TerminalBlocked, TerminalWorking, TerminalIdle, TerminalClear]

sequences :: [TerminalStatus] -> Text
sequences = foldMap terminalStatusSequence

-- A real PTY exercises the production TTY gate. The marker also allows empty
-- output cases to finish without relying on EOF semantics (different on Darwin).
captureTerminal :: IO () -> IO Text
captureTerminal = captureOutput openPseudoTerminal

captureOutput :: IO (Fd, Fd) -> IO () -> IO Text
captureOutput allocate action =
    bracket allocate (\(master, slave) -> closeFd master >> closeFd slave) \(master, slave) ->
    bracket (dup stdOutput) closeFd \original -> do
        let receive accumulated = do
                chunk <- Posix.fdRead master 4096
                let output = accumulated <> chunk
                if "~" `ByteString.isSuffixOf` output
                    then pure output
                    else receive output
        result <- timeout 5000000 $
            withAsync (receive mempty) \reader -> do
                bracket_
                    (hFlush stdout >> void (dupTo slave stdOutput))
                    (hFlush stdout >> void (dupTo original stdOutput))
                    (action >> hPutStr stdout "~" >> hFlush stdout)
                wait reader
        case result of
            Nothing -> expectationFailure "terminal output was not received" >> pure ""
            Just output -> pure (Text.dropEnd 1 (Text.decodeUtf8 output))
