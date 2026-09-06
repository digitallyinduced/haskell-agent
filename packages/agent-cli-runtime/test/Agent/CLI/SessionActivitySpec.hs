module Agent.CLI.SessionActivitySpec (spec) where

import Agent.CLI.Session.Activity
import Control.Concurrent.Async (concurrently, wait, withAsync)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception.Safe (finally)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "turn activity ownership" do
    it "acquires once when persistence callbacks overlap" do
        activity <- newTurnActivity
        acquisitions <- newIORef (0 :: Int)
        releases <- newIORef (0 :: Int)
        let acquire = do
                atomicModifyIORef' acquisitions (\n -> (n + 1, ()))
                pure (Just ())
            release () = atomicModifyIORef' releases (\n -> (n + 1, ()))
        beginTurnActivity activity
        (first, second) <- concurrently
            (acquireTurnActivity activity acquire)
            (acquireTurnActivity activity acquire)
        (first /= second) `shouldBe` True
        readIORef acquisitions `shouldReturn` 1
        endTurnActivity activity release
        endTurnActivity activity release
        readIORef releases `shouldReturn` 1

    it "joins an in-flight acquisition before ending and rejects late persistence" do
        activity <- newTurnActivity
        started <- newEmptyMVar
        allowAcquire <- newEmptyMVar
        ending <- newEmptyMVar
        releases <- newIORef (0 :: Int)
        let acquire = do
                putMVar started ()
                takeMVar allowAcquire
                pure (Just ())
            release () = atomicModifyIORef' releases (\n -> (n + 1, ()))
        beginTurnActivity activity
        withAsync (acquireTurnActivity activity acquire) \worker -> do
            takeMVar started
            withAsync (putMVar ending () >> endTurnActivity activity release) \cleanup -> do
                takeMVar ending
                blocked <- timeout 20_000 (wait cleanup)
                    `finally` putMVar allowAcquire ()
                blocked `shouldBe` Nothing
                wait worker `shouldReturn` True
                wait cleanup
        readIORef releases `shouldReturn` 1
        acquireTurnActivity activity
            (expectationFailure "late persistence acquired a marker" >> pure (Just ()))
            `shouldReturn` False

    it "allows retry after an acquisition throws without losing the owner" do
        activity <- newTurnActivity
        beginTurnActivity activity
        acquireTurnActivity activity (ioError (userError "failed acquisition"))
            `shouldThrow` anyIOException
        acquireTurnActivity activity (pure (Just ())) `shouldReturn` True
        endTurnActivity activity (const (pure ()))
        acquireTurnActivity activity (pure (Just ())) `shouldReturn` False
