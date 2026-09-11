module Agent.CLI.InterruptSpec (spec) where

import Agent.CLI.Interrupt
import Agent.Cancel (newCancelFlag, isCancelled, requestCancel)
import qualified Control.Exception as Base
import Control.Exception (AsyncException(ThreadKilled, UserInterrupt))
import Control.Exception.Safe (bracket, finally, throwIO, toSyncException)
import Control.Monad (forM_, void)
import Data.IORef
import System.Posix.Signals (Handler(..), Signal, installHandler, sigHUP, sigINT, sigTERM)
import Test.Hspec

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

    describe "withCtrlCHandler" do
        it "keeps repeated confirmed interrupts within the session boundary" do
            state <- newInterruptState (const (pure ()))
            withCtrlCHandler state do
                noteIdleCtrlC state `shouldReturn` ContinuePrompt
                noteIdleCtrlC state `shouldReturn` QuitProcess
                forM_ [1 :: Int, 2] \_ ->
                    catchUserInterrupt
                        (invokeInstalledHandler sigINT >> pure False)
                        (pure True)
                        `shouldReturn` True

        it "treats hangup and termination as confirmed session quit" do
            forM_ [sigHUP, sigTERM] \signal -> do
                state <- newInterruptState (const (pure ()))
                catchUserInterrupt
                    (withCtrlCHandler state
                        (invokeInstalledHandler signal >> pure False))
                    (pure True)
                    `shouldReturn` True
                noteIdleCtrlC state `shouldReturn` QuitProcess

        it "restores previous signal handlers after an exceptional session exit" do
            forM_ [sigINT, sigHUP, sigTERM] \signal -> do
                restored <- newIORef False
                bracket
                    (installHandler signal
                        (Catch (writeIORef restored True)) Nothing)
                    (\previous -> void (installHandler signal previous Nothing))
                    \_ -> do
                        state <- newInterruptState (const (pure ()))
                        catchUserInterrupt
                            (withCtrlCHandler state (Base.throwIO UserInterrupt))
                            (pure ())
                        invokeInstalledHandler signal
                        readIORef restored `shouldReturn` True

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
-- signal to the test runner or GHCi. The callback still targets the session's
-- owning thread, exercising the same exception path as the signal dispatcher.
invokeInstalledHandler :: Signal -> IO ()
invokeInstalledHandler signal =
    bracket
        (installHandler signal Ignore Nothing)
        (\previous -> void (installHandler signal previous Nothing))
        \case
            Catch handler -> handler
            _ -> expectationFailure "Expected a session signal handler"
