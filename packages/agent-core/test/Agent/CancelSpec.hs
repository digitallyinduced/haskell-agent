module Agent.CancelSpec (spec) where

import Agent.Cancel
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Test.Hspec

spec :: Spec
spec = describe "CancelFlag" do
    it "starts unset" do
        flag <- newCancelFlag
        isCancelled flag `shouldReturn` False

    it "latches requestCancel idempotently" do
        flag <- newCancelFlag
        requestCancel flag
        requestCancel flag
        isCancelled flag `shouldReturn` True

    it "unblocks waitCancel" do
        flag <- newCancelFlag
        done <- newEmptyMVar
        _ <- forkIO do
            waitCancel flag
            putMVar done ()
        threadDelay 20000
        requestCancel flag
        takeMVar done
        pure ()

    it "resetCancel clears the latch" do
        flag <- newCancelFlag
        requestCancel flag
        resetCancel flag
        isCancelled flag `shouldReturn` False
