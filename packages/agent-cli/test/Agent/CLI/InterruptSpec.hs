module Agent.CLI.InterruptSpec (spec) where

import Agent.CLI.Interrupt
import Agent.Cancel (newCancelFlag, isCancelled, requestCancel, waitCancel)
import Control.Concurrent.Async (concurrently)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import qualified Control.Exception as Base
import Control.Exception (AsyncException(ThreadKilled, UserInterrupt))
import Control.Exception.Safe (bracket, finally, throwIO, toSyncException)
import Control.Monad (forM_, void)
import Data.IORef
import System.Posix.Signals (Handler(..), Signal, installHandler, sigHUP, sigINT, sigTERM)
import Test.Hspec
import System.Timeout (timeout)

spec :: Spec
spec = do
    describe "nested cancellation scopes" do
        it "does not erase hangup when preparing a delegated turn" do
            state <- newInterruptState (const (pure ()))
            flag <- newCancelFlag
            withTurnCancel state flag do
                requestCancel flag
                resetIdleTurnCancel state flag
                isCancelled flag `shouldReturn` True
            resetIdleTurnCancel state flag
            isCancelled flag `shouldReturn` False
        it "restores the call's cancellation after a delegated turn" do
            state <- newInterruptState (const (pure ()))
            outer <- newCancelFlag
            inner <- newCancelFlag
            withTurnCancel state outer do
                withTurnCancel state inner (pure ())
                noteFullscreenCtrlC state `shouldReturn` SoftCancel
                isCancelled outer `shouldReturn` True
                isCancelled inner `shouldReturn` False
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

        it "stays on force-exit once a confirmed quit is in flight" do
            decideCtrlC Exiting False `shouldBe` ForceExit
            decideCtrlC Exiting True `shouldBe` ForceExit

    describe "noteIdleCtrlC" do
        it "keeps quitting after the first confirmed idle Ctrl-C" do
            state <- newInterruptState (const (pure ()))
            noteIdleCtrlC state `shouldReturn` ContinuePrompt
            noteIdleCtrlC state `shouldReturn` QuitProcess
            noteIdleCtrlC state `shouldReturn` QuitProcess

    describe "noteFullscreenCtrlC" do
        it "keeps force-exiting after a confirmed fullscreen quit" do
            state <- newInterruptState (const (pure ()))
            noteFullscreenCtrlC state `shouldReturn` WarnExit
            noteFullscreenCtrlC state `shouldReturn` ForceExit
            noteFullscreenCtrlC state `shouldReturn` ForceExit

        it "serializes simultaneous Ctrl-C requests against the active turn" do
            state <- newInterruptState (const (pure ()))
            flag <- newCancelFlag
            decisions <- withTurnCancel state flag $
                concurrently (noteFullscreenCtrlC state) (noteFullscreenCtrlC state)
            decisions `shouldSatisfy`
                (`elem` [(SoftCancel, ForceExit), (ForceExit, SoftCancel)])
            isCancelled flag `shouldReturn` True

    describe "withSessionInterrupts" do
        it "preserves a normally completed action without requesting UI stop" do
            state <- newInterruptState (const (pure ()))
            withSessionInterrupts state
                (expectationFailure "Unexpected UI stop")
                (pure (42 :: Int))
                `shouldReturn` Just 42

        it "joins a keyboard-cancelled action after notifying its UI owner" do
            state <- newInterruptState (const (pure ()))
            blocked <- newEmptyMVar
            events <- newIORef ([] :: [String])
            let record event = atomicModifyIORef' events \old -> (old <> [event], ())
            timeout 1_000_000
                (withSessionInterrupts state (record "stop") $
                    (do
                        noteIdleCtrlC state `shouldReturn` ContinuePrompt
                        void (noteIdleCtrlC state)
                        takeMVar blocked)
                    `finally` record "joined")
                `shouldReturn` Just (Nothing :: Maybe ())
            readIORef events `shouldReturn` ["stop", "joined"]

        it "treats hangup and termination as confirmed session quit" do
            forM_ [sigHUP, sigTERM] \signal -> do
                state <- newInterruptState (const (pure ()))
                blocked <- newEmptyMVar
                joined <- newIORef False
                timeout 1_000_000
                    (withSessionInterrupts state (pure ()) $
                        (invokeInstalledHandler signal >> takeMVar blocked)
                            `finally` writeIORef joined True)
                    `shouldReturn` Just (Nothing :: Maybe ())
                readIORef joined `shouldReturn` True
                noteIdleCtrlC state `shouldReturn` QuitProcess

        it "soft-cancels the first SIGINT and quits on the next" do
            messages <- newEmptyMVar
            state <- newInterruptState (putMVar messages)
            flag <- newCancelFlag
            blocked <- newEmptyMVar
            joined <- newIORef False
            timeout 1_000_000
                (withSessionInterrupts state (pure ()) $
                    withTurnCancel state flag $
                        (do
                            invokeInstalledHandler sigINT
                            waitCancel flag
                            takeMVar messages
                                `shouldReturn` "Interrupted; press Ctrl-C again to exit"
                            invokeInstalledHandler sigINT
                            takeMVar blocked)
                        `finally` writeIORef joined True)
                `shouldReturn` Just (Nothing :: Maybe ())
            readIORef joined `shouldReturn` True

        it "keeps repeated exit requests from interrupting a worker finalizer" do
            state <- newInterruptState (const (pure ()))
            blocked <- newEmptyMVar
            joined <- newIORef False
            timeout 1_000_000
                (withSessionInterrupts state (pure ()) $
                    (requestSessionExit state >> takeMVar blocked)
                        `finally` do
                            forM_ [sigINT, sigHUP, sigTERM] invokeInstalledHandler
                            requestSessionExit state
                            writeIORef joined True)
                `shouldReturn` Just (Nothing :: Maybe ())
            readIORef joined `shouldReturn` True

        it "quits on the second SIGINT while the first notice is blocked" do
            noticeStarted <- newEmptyMVar
            noticeBlocked <- newEmptyMVar
            noticeJoined <- newIORef False
            state <- newInterruptState \_ ->
                (putMVar noticeStarted () >> takeMVar noticeBlocked)
                    `finally` writeIORef noticeJoined True
            workerBlocked <- newEmptyMVar
            workerJoined <- newIORef False
            timeout 1_000_000
                (withSessionInterrupts state (pure ()) $
                    (do
                        invokeInstalledHandler sigINT
                        takeMVar noticeStarted
                        invokeInstalledHandler sigINT
                        takeMVar workerBlocked)
                    `finally` writeIORef workerJoined True)
                `shouldReturn` Just (Nothing :: Maybe ())
            readIORef workerJoined `shouldReturn` True
            readIORef noticeJoined `shouldReturn` True

        it "prioritizes queued Ctrl-C requests over a completed provider transition" do
            state <- newInterruptState (const (pure ()))
            timeout 1_000_000
                (withSessionInterrupts state (pure ()) do
                    invokeInstalledHandler sigINT
                    invokeInstalledHandler sigINT
                    pure ("provider transition" :: String))
                `shouldReturn` Just Nothing

        it "propagates worker failures instead of reporting a requested quit" do
            state <- newInterruptState (const (pure ()))
            withSessionInterrupts state
                (expectationFailure "Unexpected UI stop")
                (ioError (userError "provider failed") :: IO ())
                `shouldThrow` anyIOException

        it "propagates failed worker cleanup during a requested quit" do
            state <- newInterruptState (const (pure ()))
            blocked <- newEmptyMVar
            timeout 1_000_000
                (withSessionInterrupts state (pure ()) $
                    (requestSessionExit state >> takeMVar blocked :: IO ())
                        -- Model a dependency finalizer that propagates its
                        -- cleanup failure. Safe.finally instead preserves the
                        -- original async cancellation over synchronous errors.
                        `Base.finally` ioError (userError "cleanup failed"))
                `shouldThrow` anyIOException

        it "restores previous handlers after normal and exceptional exits" do
            forM_ [(signal, fails) | signal <- [sigINT, sigHUP, sigTERM], fails <- [False, True]] \(signal, fails) -> do
                restored <- newIORef False
                bracket
                    (installHandler signal
                        (Catch (writeIORef restored True)) Nothing)
                    (\previous -> void (installHandler signal previous Nothing))
                    \_ -> do
                        state <- newInterruptState (const (pure ()))
                        let session = withSessionInterrupts state (pure ()) $
                                if fails then ioError (userError "session failed") else pure ()
                        if fails
                            then session `shouldThrow` anyIOException
                            else session `shouldReturn` Just ()
                        invokeInstalledHandler signal
                        readIORef restored `shouldReturn` True

    describe "withSessionInterruptScope" do
        it "retains latched signal handlers through post-join reporting" do
            forM_ [sigINT, sigHUP, sigTERM] \signal -> do
                previousCalled <- newIORef False
                joined <- newIORef False
                blocked <- newEmptyMVar
                state <- newInterruptState (const (pure ()))
                bracket
                    (installHandler signal
                        (Catch (writeIORef previousCalled True)) Nothing)
                    (\previous -> void (installHandler signal previous Nothing))
                    \_ -> do
                        let report supervise = do
                                supervise `shouldReturn` (Nothing :: Maybe ())
                                readIORef joined `shouldReturn` True
                                forM_ [sigINT, sigHUP, sigTERM] invokeInstalledHandler
                                noteFullscreenCtrlC state `shouldReturn` ForceExit
                                readIORef previousCalled `shouldReturn` False
                        timeout 1_000_000
                            (withSessionInterruptScope state (pure ()) report $
                                (requestSessionExit state >> takeMVar blocked)
                                    `finally` writeIORef joined True)
                            `shouldReturn` Just ()
                        invokeInstalledHandler signal
                        readIORef previousCalled `shouldReturn` True

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

-- Invoke the installed callback directly rather than deliver a process-wide
-- signal to the test runner or GHCi. Restore the callback before invoking it,
-- since publishing exit can let the supervisor restore its outer handler.
invokeInstalledHandler :: Signal -> IO ()
invokeInstalledHandler signal = do
    installed <- bracket
        (installHandler signal Ignore Nothing)
        (\previous -> void (installHandler signal previous Nothing))
        pure
    case installed of
        Catch handler -> handler
        _ -> expectationFailure "Expected a session signal handler"
