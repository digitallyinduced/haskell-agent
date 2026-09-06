module Agent.CLI.NativeProcessSpec (spec) where

import Agent.CLI.Session.Threads (launchSessionThread)
import Agent.CLI.NativeProcess
import Control.Concurrent.MVar
    ( newEmptyMVar, putMVar, takeMVar, tryTakeMVar )
import Control.Exception.Safe (bracket, finally)
import Data.IORef (modifyIORef', newIORef, readIORef)
import System.OsPath (unsafeEncodeUtf)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "shared native process resources" do
    it "starts stale-resource cleanup once and joins it on close" do
        started <- newEmptyMVar
        blocked <- newEmptyMVar
        stopped <- newEmptyMVar
        attempts <- newIORef (0 :: Int)
        withinDeadline $ withRuntime \runtime -> do
            runtime.nativeStartCleanup $
                (modifyIORef' attempts (+ 1)
                    >> putMVar started ()
                    >> takeMVar blocked)
                    `finally` putMVar stopped ()
            takeMVar started
            runtime.nativeStartCleanup (modifyIORef' attempts (+ 1))
        tryTakeMVar stopped `shouldReturn` Just ()
        readIORef attempts `shouldReturn` 1

    it "joins active session turns and rejects launches after close" do
        started <- newEmptyMVar
        blocked <- newEmptyMVar
        stopped <- newEmptyMVar
        manager <- withinDeadline $ withRuntime \runtime -> do
            let manager = runtime.nativeSessionThreads
            launchSessionThread manager "shutdown" (
                (putMVar started () >> takeMVar blocked >> pure (Right ()))
                    `finally` putMVar stopped ())
                `shouldReturn` Right "started session shutdown"
            takeMVar started
            launchSessionThread manager "shutdown" (pure (Right ()))
                `shouldReturn` Left "session shutdown is already running"
            pure manager
        tryTakeMVar stopped `shouldReturn` Just ()
        launchSessionThread manager "later" (pure (Right ()))
            `shouldReturn` Left "agent session manager is closed"

    it "closes an unused cleanup worker without requiring a cleanup request" do
        withinDeadline (withRuntime (const (pure ())))

-- Bounds interruptible test handshakes; this is not a hard deadline for
-- masked resource cleanup.
withinDeadline :: IO a -> IO a
withinDeadline action =
    timeout 5000000 action >>= maybe
        (fail "native process resource test exceeded its deadline")
        pure

withRuntime :: (NativeProcessRuntime -> IO a) -> IO a
withRuntime =
    bracket (newNativeProcessRuntime (unsafeEncodeUtf "."))
        closeNativeProcessRuntime
