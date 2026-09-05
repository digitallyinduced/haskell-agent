module Agent.CLI.MacOS.RepositoryWorkersSpec (spec) where

import Agent.CLI.MacOS.RepositoryWorkers
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (waitCatch, withAsync)
import Control.Concurrent.MVar
    ( newEmptyMVar, putMVar, readMVar, tryReadMVar )
import Control.Exception.Safe (finally, throwString, uninterruptibleMask_)
import Data.Maybe (isJust, isNothing)
import Test.Hspec (Spec, describe, it, shouldReturn)

spec :: Spec
spec = describe "repository worker lifecycle" do
    it "blocks new repository workers until cancel-all returns" do
        repositoryCancelAllAdmissionSmoke `shouldReturn` True
    it "does not self-deadlock on callback lifecycle calls" do
        repositoryCancelAllReentrancySmoke `shouldReturn` True
    it "classifies async cancellation as cancelled, not failure" do
        repositoryCancelClassificationSmoke `shouldReturn` True
    it "does not retry a terminal callback that throws" do
        repositoryTerminalThrowSmoke `shouldReturn` True

-- Deterministic regression hook for the admission-barrier lifecycle. This is
-- a Haskell test hook, not part of the C ABI.
repositoryCancelAllAdmissionSmoke :: IO Bool
repositoryCancelAllAdmissionSmoke = do
    entered <- newEmptyMVar
    cancelled <- newEmptyMVar
    firstAccepted <- startRepositoryWorker (putMVar cancelled ()) do
        putMVar entered ()
        uninterruptibleMask_ (threadDelay 250_000)
        pure (pure ())
    if not firstAccepted
        then pure False
        else do
            readMVar entered
            withAsync cancelRepositoryWorkers \canceller -> do
                rejectedDuringBarrier <- awaitRejection 1000
                _ <- waitCatch canceller
                cancellationDelivered <- readMVar cancelled >> pure True
                completed <- newEmptyMVar
                acceptedAfter <- startRepositoryWorker
                    (pure ())
                    (pure (putMVar completed ()))
                finishedAfter <- if acceptedAfter
                    then readMVar completed >> pure True
                    else pure False
                pure
                    ( rejectedDuringBarrier
                        && cancellationDelivered
                        && finishedAfter
                    )
  where
    awaitRejection attempts
        | attempts <= (0 :: Int) = pure False
        | otherwise =
            startRepositoryWorker (pure ()) (pure (pure ())) >>= \case
                False -> pure True
                True -> threadDelay 1000 >> awaitRejection (attempts - 1)

repositoryCancelAllReentrancySmoke :: IO Bool
repositoryCancelAllReentrancySmoke = do
    completed <- newEmptyMVar
    accepted <- startRepositoryWorker (pure ()) do
        pure do
            cancelRepositoryWorkers
            putMVar completed ()
    if accepted
        then readMVar completed >> pure True
        else pure False

repositoryCancelClassificationSmoke :: IO Bool
repositoryCancelClassificationSmoke = do
    entered <- newEmptyMVar
    cancelled <- newEmptyMVar
    synthesizedFailure <- newEmptyMVar
    accepted <- startRepositoryWorker (putMVar cancelled ()) do
        putMVar entered ()
        tryRepositorySynchronous (threadDelay 30_000_000) >>= \case
            Left _ -> putMVar synthesizedFailure ()
            Right () -> pure ()
        pure (pure ())
    if not accepted
        then pure False
        else do
            readMVar entered
            cancelRepositoryWorkers
            cancellation <- tryReadMVar cancelled
            failure <- tryReadMVar synthesizedFailure
            pure (isJust cancellation && isNothing failure)

repositoryTerminalThrowSmoke :: IO Bool
repositoryTerminalThrowSmoke = do
    cancelled <- newEmptyMVar
    terminal <- newEmptyMVar
    finished <- newEmptyMVar
    accepted <- startRepositoryWorker (putMVar cancelled ()) do
        pure
            ((putMVar terminal () >> throwString "terminal callback failed")
                `finally` putMVar finished ())
    if not accepted
        then pure False
        else do
            readMVar terminal
            readMVar finished
            cancelRepositoryWorkers
            cancellation <- tryReadMVar cancelled
            pure (isNothing cancellation)

