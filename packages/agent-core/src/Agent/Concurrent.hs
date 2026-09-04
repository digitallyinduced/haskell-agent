-- | Small structured-concurrency helpers shared by agent subsystems.
module Agent.Concurrent
    ( mapConcurrentlyBounded
    , forConcurrentlyBounded_
    ) where

import qualified Control.Concurrent.Stream as ConcurrentStream
import Control.Monad (void)

-- | Apply an action with at most the requested number of active workers.
--
-- Results retain input order. Worker lifetimes are scoped by
-- 'replicateConcurrently_', so an exception cancels and joins the remaining
-- workers before it escapes.
mapConcurrentlyBounded :: Int -> (a -> IO b) -> [a] -> IO [b]
mapConcurrentlyBounded requested =
    ConcurrentStream.mapConcurrentlyBounded (max 1 requested)

-- | Bounded, structured traversal when results are not needed.
forConcurrentlyBounded_ :: Int -> (a -> IO b) -> [a] -> IO ()
forConcurrentlyBounded_ requested action values =
    void (mapConcurrentlyBounded requested action values)
