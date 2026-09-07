module Agent.CLI.MacOS.RepositoryWorkers
    ( startRepositoryWorker
    , cancelRepositoryWorkers
    , withRepositoryCallbackThread
    , isRepositoryCallbackThread
    , tryRepositorySynchronous
    ) where

import Control.Concurrent (ThreadId, myThreadId)
import Control.Concurrent.Async (Async, asyncWithUnmask, cancel, waitCatch)
import Control.Concurrent.MVar
    ( MVar, modifyMVar, newEmptyMVar, newMVar, putMVar, readMVar )
import Control.Exception.Safe
    ( SomeAsyncException, SomeException, catchAsync, finally, isAsyncException
    , mask, throwIO, tryAny
    )
import Control.Monad (when)
import Data.Map.Strict (Map)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- The C API owns one process-wide repository worker group. Workers are
-- registered before their admission gate opens; cancel-all closes admission,
-- cancels and joins the group, then permits new work again.
startRepositoryWorker :: IO () -> IO (IO ()) -> IO Bool
startRepositoryWorker onCancelled prepare =
    tryRepositorySynchronous
        (mask \_ -> do
            gate <- newEmptyMVar
            admitted <- modifyMVar repositoryWorkers \state ->
                case state.repositoryWorkerBarrier of
                    Just _ -> pure (state, False)
                    Nothing -> do
                        let workerId = state.repositoryWorkerNextId
                        worker <- asyncWithUnmask \unmask -> do
                            withRepositoryCallbackThread
                                (((Just <$> (readMVar gate >> unmask prepare))
                                    `catchAsync`
                                        \(_ :: SomeAsyncException) ->
                                            onCancelled >> pure Nothing)
                                    >>= mapM_ id)
                                `finally`
                                unregisterRepositoryWorker workerId
                        pure
                            ( state
                                { repositoryWorkerNextId = workerId + 1
                                , repositoryWorkerActive =
                                    Map.insert
                                        workerId
                                        worker
                                        state.repositoryWorkerActive
                                }
                            , True
                            )
            when admitted (putMVar gate ())
            pure admitted)
        >>= \case
            Left _ -> pure False
            Right admitted -> pure admitted

unregisterRepositoryWorker :: Int -> IO ()
unregisterRepositoryWorker workerId =
    modifyMVar repositoryWorkers \state ->
        pure
            ( state
                { repositoryWorkerActive =
                    Map.delete workerId state.repositoryWorkerActive
                }
            , ()
            )

cancelRepositoryWorkers :: IO ()
cancelRepositoryWorkers =
    isRepositoryCallbackThread >>= \case
        True -> pure ()
        False -> mask \restore -> do
            admission <- modifyMVar repositoryWorkers \state ->
                case state.repositoryWorkerBarrier of
                    Just barrier ->
                        pure (state, Left barrier)
                    Nothing -> do
                        barrier <- newEmptyMVar
                        pure
                            ( state
                                { repositoryWorkerActive = Map.empty
                                , repositoryWorkerBarrier = Just barrier
                                }
                            , Right
                                ( barrier
                                , Map.elems state.repositoryWorkerActive
                                )
                            )
            case admission of
                Left activeBarrier -> restore (readMVar activeBarrier)
                Right (barrier, workers) ->
                    restore
                        (mapM_ cancel workers >> mapM_ waitCatch workers)
                        `finally` do
                            modifyMVar repositoryWorkers \state ->
                                pure
                                    ( state
                                        { repositoryWorkerBarrier = Nothing }
                                    , ()
                                    )
                            putMVar barrier ()

{-# NOINLINE repositoryWorkers #-}
repositoryWorkers :: MVar RepositoryWorkerState
repositoryWorkers = unsafePerformIO
    (newMVar RepositoryWorkerState
        { repositoryWorkerNextId = 0
        , repositoryWorkerActive = Map.empty
        , repositoryWorkerBarrier = Nothing
        })

data RepositoryWorkerState = RepositoryWorkerState
    { repositoryWorkerNextId :: !Int
    , repositoryWorkerActive :: !(Map Int (Async ()))
    , repositoryWorkerBarrier :: !(Maybe (MVar ()))
    }

{-# NOINLINE repositoryCallbackThreads #-}
repositoryCallbackThreads :: MVar (Set.Set ThreadId)
repositoryCallbackThreads = unsafePerformIO (newMVar Set.empty)

withRepositoryCallbackThread :: IO value -> IO value
withRepositoryCallbackThread action = do
    thread <- myThreadId
    modifyMVar repositoryCallbackThreads \threads ->
        pure (Set.insert thread threads, ())
    action `finally`
        modifyMVar repositoryCallbackThreads \threads ->
            pure (Set.delete thread threads, ())

isRepositoryCallbackThread :: IO Bool
isRepositoryCallbackThread = do
    thread <- myThreadId
    Set.member thread <$> readMVar repositoryCallbackThreads

tryRepositorySynchronous
    :: IO value
    -> IO (Either SomeException value)
tryRepositorySynchronous action =
    tryAny action >>= \case
        Left exception
            | isAsyncException exception -> throwIO exception
            | otherwise -> pure (Left exception)
        Right value -> pure (Right value)

