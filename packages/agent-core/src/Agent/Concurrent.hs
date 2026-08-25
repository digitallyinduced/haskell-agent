-- | Small structured-concurrency helpers shared by agent subsystems.
module Agent.Concurrent
    ( mapConcurrentlyBounded
    , forConcurrentlyBounded_
    ) where

import Control.Concurrent.Async (replicateConcurrently_)
import Control.Concurrent.STM
    ( atomically
    , modifyTVar'
    , newTQueueIO
    , newTVarIO
    , readTQueue
    , readTVarIO
    , writeTQueue
    )
import Control.Monad (forM_, replicateM_, void)
import qualified Data.Map.Strict as Map

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
    results <- newTVarIO Map.empty
    atomically do
        forM_ (zip [0 :: Int ..] values) $
            writeTQueue queue . Just
        replicateM_ workerCount (writeTQueue queue Nothing)
    let worker =
            atomically (readTQueue queue) >>= \case
                Nothing -> pure ()
                Just (index, value) -> do
                    result <- action value
                    atomically $
                        modifyTVar' results (Map.insert index result)
                    worker
    replicateConcurrently_ workerCount worker
    completed <- readTVarIO results
    pure
        [ completed Map.! index
        | index <- [0 .. length values - 1]
        ]

-- | Bounded, structured traversal when results are not needed.
forConcurrentlyBounded_ :: Int -> (a -> IO b) -> [a] -> IO ()
forConcurrentlyBounded_ requested action values =
    void (mapConcurrentlyBounded requested action values)
