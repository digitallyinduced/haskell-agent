module Agent.ResourceScopeSpec (spec) where

import Agent.ResourceScope
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.MVar
import Data.IORef
import System.Timeout (timeout)
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

    it "makes concurrent close callers wait for cleanup" do
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
                premature <- timeout 1000000 (Async.wait secondClose)
                putMVar finishCleanup ()
                Async.wait firstClose
                case premature of
                    Nothing -> Async.wait secondClose
                    Just () -> pure ()
                premature `shouldBe` Nothing

    it "continues cleanup when the initiating close caller is cancelled" do
        cleanupStarted <- newEmptyMVar
        finishCleanup <- newEmptyMVar
        joiningStarted <- newEmptyMVar
        releases <- newIORef (0 :: Int)
        scope <- newResourceScope
        _ <- registerResource scope do
            putMVar cleanupStarted ()
            takeMVar finishCleanup
            increment releases

        Async.withAsync (closeResourceScope scope) \firstClose -> do
            takeMVar cleanupStarted
            Async.cancel firstClose
            Async.waitCatch firstClose >>= \case
                Left _ -> pure ()
                Right () ->
                    expectationFailure "cancelled close returned successfully"
            Async.withAsync
                (putMVar joiningStarted () >> closeResourceScope scope)
                \joiningClose -> do
                takeMVar joiningStarted
                premature <- timeout 1000000 (Async.wait joiningClose)
                putMVar finishCleanup ()
                case premature of
                    Nothing -> Async.wait joiningClose
                    Just () -> pure ()
                premature `shouldBe` Nothing
        readIORef releases `shouldReturn` 1

    it "allows a cleanup action to close its own scope" do
        scope <- newResourceScope
        _ <- registerResource scope (closeResourceScope scope)
        timeout 1000000 (closeResourceScope scope)
            `shouldReturn` Just ()

record :: IORef [Int] -> Int -> IO ()
record ref value =
    atomicModifyIORef' ref \values -> (values <> [value], ())

increment :: IORef Int -> IO ()
increment ref =
    atomicModifyIORef' ref \value -> (value + 1, ())
