module Agent.CLI.InterruptSpec (spec) where

import Agent.CLI.Interrupt
import qualified Control.Exception as Base
import Control.Exception (AsyncException(ThreadKilled, UserInterrupt))
import Control.Exception.Safe (finally, throwIO, toSyncException)
import Data.IORef
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

    describe "catchUserInterrupt" do
        it "handles an asynchronous UserInterrupt" do
            catchUserInterrupt
                (Base.throwIO UserInterrupt)
                (pure ("stopped" :: String))
                `shouldReturn` ("stopped" :: String)

        it "handles a synchronously wrapped UserInterrupt" do
            catchUserInterrupt
                (throwIO UserInterrupt)
                (pure ("stopped" :: String))
                `shouldReturn` ("stopped" :: String)

        it "handles a UserInterrupt propagated through a finalizer" do
            catchUserInterrupt
                (pure ("completed" :: String)
                    `finally` Base.throwIO UserInterrupt)
                (pure "stopped")
                `shouldReturn` "stopped"

        it "does not swallow other asynchronous exceptions" do
            catchUserInterrupt
                (Base.throwIO ThreadKilled)
                (pure ())
                `shouldThrow` anyException

    describe "retryUserInterruptOnce" do
        it "recovers from one UserInterrupt" do
            attempts <- newIORef (0 :: Int)
            retryUserInterruptOnce
                (do
                    attempt <- atomicModifyIORef' attempts \count ->
                        let next = count + 1
                        in (next, next)
                    if attempt == 1
                        then Base.throwIO UserInterrupt
                        else pure ("recovered" :: String))
                `shouldReturn` "recovered"
            readIORef attempts `shouldReturn` 2

        it "propagates a UserInterrupt from the recovery attempt" do
            attempts <- newIORef (0 :: Int)
            retryUserInterruptOnce
                (atomicModifyIORef' attempts
                    (\count -> (count + 1, ()))
                    >> Base.throwIO UserInterrupt)
                `shouldThrow` (== UserInterrupt)
            readIORef attempts `shouldReturn` 2
