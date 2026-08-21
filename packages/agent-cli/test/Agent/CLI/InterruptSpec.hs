module Agent.CLI.InterruptSpec (spec) where

import Agent.CLI.Interrupt
import Control.Concurrent (threadDelay)
import Control.Exception (AsyncException(ThreadKilled, UserInterrupt))
import qualified Control.Exception as Exception
import Control.Exception.Safe (finally, toSyncException)
import Data.IORef (newIORef, readIORef, writeIORef)
import System.Exit (ExitCode(..))
import System.Posix.Signals (Signal, raiseSignal, sigHUP, sigTERM)
import Test.Hspec

spec :: Spec
spec = do
    describe "decideCtrlC" do
        it "warns on first idle Ctrl-C" do
            decideCtrlC Idle False `shouldBe` WarnExit

        it "exits on second idle Ctrl-C within the window" do
            decideCtrlC Idle True `shouldBe` ForceExit

        it "soft-cancels the first Ctrl-C during a turn" do
            decideCtrlC (TurnActive False) False `shouldBe` SoftCancel
            decideCtrlC (TurnActive False) True `shouldBe` SoftCancel

        it "force-exits when Ctrl-C arrives after the turn is already cancelled" do
            decideCtrlC (TurnActive True) False `shouldBe` ForceExit

    describe "isWrappedUserInterrupt" do
        it "recognizes a synchronously wrapped UserInterrupt" do
            isWrappedUserInterrupt (toSyncException UserInterrupt) `shouldBe` True

        it "rejects other wrapped async exceptions" do
            isWrappedUserInterrupt (toSyncException ThreadKilled) `shouldBe` False

    describe "withTerminationHandlers" do
        it "turns SIGHUP into exit 129 and runs enclosing cleanup" do
            checkTermination sigHUP 129

        it "turns SIGTERM into exit 143 and runs enclosing cleanup" do
            checkTermination sigTERM 143

checkTermination :: Signal -> Int -> Expectation
checkTermination signal expectedCode = do
    cleaned <- newIORef False
    result <- Exception.try @ExitCode $
        withTerminationHandlers $
            (raiseSignal signal >> threadDelay 1000000)
                `finally` writeIORef cleaned True
    result `shouldBe` Left (ExitFailure expectedCode)
    readIORef cleaned `shouldReturn` True
