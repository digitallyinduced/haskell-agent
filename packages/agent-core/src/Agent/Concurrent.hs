-- | Small structured-concurrency helpers shared by agent subsystems.
module Agent.Concurrent
    ( mapConcurrentlyBounded
    , forConcurrentlyBounded_
    ) where

import Control.Concurrent.Async (replicateConcurrently_)
import Control.Concurrent.STM
    ( atomically
    , newTQueueIO
    , newEmptyTMVarIO
    , readTQueue
    , readTMVar
    , putTMVar
    , writeTQueue
    )
import Control.Exception (evaluate)
import Control.Monad (forM_, replicateM_, void)

-- | Apply an action with at most the requested number of active workers.
--
-- Results retain input order. Worker lifetimes are scoped by
-- 'replicateConcurrently_', so an exception cancels and joins the remaining
-- workers before it escapes.
mapConcurrentlyBounded :: Int -> (a -> IO b) -> [a] -> IO [b]
mapConcurrentlyBounded _ _ [] = pure []
mapConcurrentlyBounded requested action values = do
    let workerCount = min (max 1 requested) (length values)
    queue <- newTQueueIO
    slots <- mapM (const newEmptyTMVarIO) values
    atomically do
        forM_ (zip slots values) $
            writeTQueue queue . Just
        replicateM_ workerCount (writeTQueue queue Nothing)
    let worker =
            atomically (readTQueue queue) >>= \case
                Nothing -> pure ()
                Just (slot, value) -> do
                    result <- action value
                    -- 'Map.insert' in the previous implementation used a
                    -- strict map, forcing each result to WHNF before making
                    -- it observable. Keep that behavior when publishing to
                    -- the result slot.
                    result' <- evaluate result
                    atomically (putTMVar slot result')
                    worker
    replicateConcurrently_ workerCount worker
    mapM (atomically . readTMVar) slots

-- | Bounded, structured traversal when results are not needed.
forConcurrentlyBounded_ :: Int -> (a -> IO b) -> [a] -> IO ()
forConcurrentlyBounded_ requested action values =
    void (mapConcurrentlyBounded requested action values)
