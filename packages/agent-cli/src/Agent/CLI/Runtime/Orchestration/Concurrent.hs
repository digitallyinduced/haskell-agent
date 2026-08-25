module Agent.CLI.Runtime.Orchestration.Concurrent
    ( concurrentlyAcquire
    ) where

import Control.Concurrent.Async
    ( cancel, waitCatch, waitEitherCatch, withAsync )
import Control.Exception.Safe ( mask, onException, throwIO )

-- | Acquire two resources concurrently and release either successful
-- acquisition if its peer fails or the caller is interrupted.
concurrentlyAcquire
    :: IO a
    -> (a -> IO ())
    -> IO b
    -> (b -> IO ())
    -> IO (a, b)
concurrentlyAcquire acquireLeft releaseLeft acquireRight releaseRight =
    mask \restore ->
        withAsync (restore acquireLeft) \leftWorker ->
            withAsync (restore acquireRight) \rightWorker -> do
                let cleanupResult release = \case
                        Left _ -> pure ()
                        Right value -> release value
                    cancelAndCleanup = do
                        cancel leftWorker
                        cancel rightWorker
                        leftResult <- waitCatch leftWorker
                        rightResult <- waitCatch rightWorker
                        cleanupResult releaseLeft leftResult
                        cleanupResult releaseRight rightResult
                first <-
                    restore (waitEitherCatch leftWorker rightWorker)
                        `onException` cancelAndCleanup
                case first of
                    Left (Left exception) -> do
                        cancel rightWorker
                        waitCatch rightWorker >>= cleanupResult releaseRight
                        throwIO exception
                    Right (Left exception) -> do
                        cancel leftWorker
                        waitCatch leftWorker >>= cleanupResult releaseLeft
                        throwIO exception
                    Left (Right leftValue) -> do
                        rightResult <-
                            restore (waitCatch rightWorker)
                                `onException` releaseLeft leftValue
                        case rightResult of
                            Left exception -> do
                                releaseLeft leftValue
                                throwIO exception
                            Right rightValue ->
                                pure (leftValue, rightValue)
                    Right (Right rightValue) -> do
                        leftResult <-
                            restore (waitCatch leftWorker)
                                `onException` releaseRight rightValue
                        case leftResult of
                            Left exception -> do
                                releaseRight rightValue
                                throwIO exception
                            Right leftValue ->
                                pure (leftValue, rightValue)
