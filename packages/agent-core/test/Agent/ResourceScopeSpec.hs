module Agent.ResourceScopeSpec (spec) where

import Agent.ResourceScope
import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.MVar
import Data.IORef
import GHC.Conc (BlockReason(..), ThreadStatus(..), threadStatus)
import Test.Hspec

spec :: Spec
spec = describe "Agent.ResourceScope" do
    it "releases resources in reverse acquisition order" do
        released <- newIORef ([] :: [Int])
        scope <- newResourceScope
        _ <- registerResource scope (record released 1)
        _ <- registerResource scope (record released 2)
        closeResourceScope scope
        readIORef released `shouldReturn` [2, 1]

    it "supports explicit idempotent release" do
        releases <- newIORef (0 :: Int)
        scope <- newResourceScope
        key <- registerResource scope $
            atomicModifyIORef' releases \n -> (n + 1, ())
        releaseResource key
        releaseResource key
        closeResourceScope scope
        readIORef releases `shouldReturn` 1

    it "can be closed more than once" do
        scope <- newResourceScope
        closeResourceScope scope
        closeResourceScope scope

    it "makes concurrent close callers wait for cleanup completion" do
        cleanupStarted <- newEmptyMVar
        finishCleanup <- newEmptyMVar
        secondStarted <- newEmptyMVar
        scope <- newResourceScope
        _ <- registerResource scope $
            putMVar cleanupStarted () >> takeMVar finishCleanup

        Async.withAsync (closeResourceScope scope) \firstClose -> do
            takeMVar cleanupStarted
            Async.withAsync
                (putMVar secondStarted () >> closeResourceScope scope)
                \secondClose -> do
                    takeMVar secondStarted
                    waitForSTMBlock secondClose
                    putMVar finishCleanup ()
                    Async.wait firstClose
                    Async.wait secondClose

    it "waits for an in-flight explicit release before close returns" do
        cleanupStarted <- newEmptyMVar
        finishCleanup <- newEmptyMVar
        closeStarted <- newEmptyMVar
        scope <- newResourceScope
        key <- registerResource scope $
            putMVar cleanupStarted () >> takeMVar finishCleanup

        Async.withAsync (releaseResource key) \releasing -> do
            takeMVar cleanupStarted
            Async.withAsync
                (putMVar closeStarted () >> closeResourceScope scope)
                \closing -> do
                    takeMVar closeStarted
                    waitForSTMBlock closing
                    putMVar finishCleanup ()
                    Async.wait releasing
                    Async.wait closing

    it "allows close to be retried when its draining wait is cancelled" do
        acquireStarted <- newEmptyMVar
        finishAcquire <- newEmptyMVar
        releases <- newIORef (0 :: Int)
        scope <- newResourceScope

        Async.withAsync
            (allocateResource scope
                (putMVar acquireStarted () >> takeMVar finishAcquire)
                (const (increment releases)))
            \allocating -> do
                takeMVar acquireStarted
                Async.withAsync (closeResourceScope scope) \closing -> do
                    waitForSTMBlock closing
                    Async.cancel closing
                    _ <- Async.waitCatch closing
                    _ <- registerResource scope (increment releases)
                    putMVar finishAcquire ()
                    _ <- Async.wait allocating
                    closeResourceScope scope
                    readIORef releases `shouldReturn` 2

    it "continues cleanup when the initiating close caller is cancelled" do
        cleanupStarted <- newEmptyMVar
        finishCleanup <- newEmptyMVar
        releases <- newIORef (0 :: Int)
        scope <- newResourceScope
        _ <- registerResource scope do
            putMVar cleanupStarted ()
            takeMVar finishCleanup
            increment releases

        Async.withAsync (closeResourceScope scope) \firstClose -> do
            takeMVar cleanupStarted
            Async.withAsync (closeResourceScope scope) \joiningCloseA ->
                Async.withAsync (closeResourceScope scope) \joiningCloseB -> do
                    waitForSTMBlock joiningCloseA
                    waitForSTMBlock joiningCloseB
                    Async.cancel firstClose
                    Async.waitCatch firstClose >>= \case
                        Left _ -> pure ()
                        Right () ->
                            expectationFailure "cancelled close returned successfully"
                    waitForSTMBlock joiningCloseA
                    waitForSTMBlock joiningCloseB
                    putMVar finishCleanup ()
                    Async.wait joiningCloseA
                    Async.wait joiningCloseB
        readIORef releases `shouldReturn` 1

record :: IORef [Int] -> Int -> IO ()
record ref value =
    atomicModifyIORef' ref \values -> (values <> [value], ())

increment :: IORef Int -> IO ()
increment ref =
    atomicModifyIORef' ref \value -> (value + 1, ())

waitForSTMBlock :: Async.Async a -> IO ()
waitForSTMBlock running = go (1000 :: Int)
  where
    go 0 = expectationFailure "close thread did not block while draining"
    go attempts =
        threadStatus (Async.asyncThreadId running) >>= \case
            ThreadBlocked BlockedOnSTM -> pure ()
            ThreadFinished -> expectationFailure "close thread finished while an allocation was active"
            ThreadDied -> expectationFailure "close thread died before cancellation"
            _ -> threadDelay 1000 >> go (attempts - 1)
