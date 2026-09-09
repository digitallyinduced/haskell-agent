{-# LANGUAGE NumericUnderscores #-}

-- | Dynamically owned resources with explicit early release.
--
-- The public API stays in 'IO' so resource ownership does not force
-- 'ResourceT' through the agent loop and provider interfaces.
module Agent.ResourceScope
    ( ResourceScope
    , ResourceKey
    , withResourceScope
    , newResourceScope
    , closeResourceScope
    , allocateResource
    , allocateResourcesConcurrently
    , allocateFourResourcesConcurrently
    , registerResource
    , releaseResource
    , logSlowCleanup
    , logSlowCleanupWith
    ) where

import Control.Concurrent.Async (concurrently)
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar
    , newMVar
    , withMVar
    )
import qualified Control.Exception as Exception
import Control.Exception.Safe (bracket, catchAny, throwIO)
import Control.Monad (when)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import System.IO (hPutStrLn, stderr)
import Control.Monad.Trans.Resource
    ( InternalState
    , ReleaseKey
    , ResourceT
    , allocate
    , closeInternalState
    , createInternalState
    , register
    , release
    , runInternalState
    )

newtype ResourceScope = ResourceScope (MVar (Maybe InternalState))

newtype ResourceKey = ResourceKey ReleaseKey

-- | Report cleanup taking at least 100 ms to stderr, without requiring debug
-- mode. Labels should describe the resource, not include commands or secrets.
-- This observes completion (including exceptions); it does not impose a
-- timeout or change the cleanup's thread, masking state, or ownership.
logSlowCleanup :: String -> IO a -> IO a
logSlowCleanup = logSlowCleanupWith getMonotonicTimeNSec (hPutStrLn stderr)

-- | Injectable monotonic nanosecond clock and diagnostic sink for testing.
logSlowCleanupWith :: IO Word64 -> (String -> IO ()) -> String -> IO a -> IO a
logSlowCleanupWith clock report label action = do
    started <- clock
    -- Base finally avoids making a diagnostic write uninterruptible when
    -- the caller was interruptible. The action retains its incoming state.
    action `Exception.finally`
        -- Only the diagnostic is best-effort; never swallow an action failure.
        ((do
            finished <- clock
            let elapsed = (finished - started) `div` 1_000_000
            when (elapsed >= 100) $
                report ("[slow cleanup] " <> label <> ": " <> show elapsed <> " ms")
        ) `catchAny` \_ -> pure ())

-- | Run an action inside a lexical resource scope.
--
-- Resources which have not already been released are closed when the action
-- returns or throws.
withResourceScope :: (ResourceScope -> IO a) -> IO a
withResourceScope = bracket newResourceScope closeResourceScope

newResourceScope :: IO ResourceScope
newResourceScope = do
    state <- createInternalState
    ResourceScope <$> newMVar (Just state)

closeResourceScope :: ResourceScope -> IO ()
closeResourceScope (ResourceScope stateVar) = do
    state <- modifyMVar stateVar \current ->
        pure (Nothing, current)
    mapM_ closeInternalState state

allocateResource
    :: ResourceScope
    -> IO a
    -> (a -> IO ())
    -> IO (ResourceKey, a)
allocateResource scope acquire cleanup =
    withOpenScope scope \state ->
        allocateInScope state acquire cleanup

-- | Acquire two independently-owned resources concurrently.
--
-- Both resources are registered with the scope before this function returns.
-- If either acquisition fails, or the caller is interrupted, the lexical
-- scope remains responsible for every acquisition which completed.  This
-- avoids transferring partially-acquired resources out of worker threads.
allocateResourcesConcurrently
    :: ResourceScope
    -> IO a
    -> (a -> IO ())
    -> IO b
    -> (b -> IO ())
    -> IO ((ResourceKey, a), (ResourceKey, b))
allocateResourcesConcurrently scope acquireLeft releaseLeft acquireRight releaseRight =
    withOpenScope scope \state ->
        concurrently
            (allocateInScope state acquireLeft releaseLeft)
            (allocateInScope state acquireRight releaseRight)

-- | Acquire four independently-owned, potentially heterogeneous resources
-- concurrently.
--
-- All four resources are registered in the same scope before this function
-- returns.  If any acquisition fails, or the caller is interrupted, the
-- lexical scope remains responsible for every acquisition which completed.
allocateFourResourcesConcurrently
    :: ResourceScope
    -> IO a
    -> (a -> IO ())
    -> IO b
    -> (b -> IO ())
    -> IO c
    -> (c -> IO ())
    -> IO d
    -> (d -> IO ())
    -> IO
        ( (ResourceKey, a)
        , (ResourceKey, b)
        , (ResourceKey, c)
        , (ResourceKey, d)
        )
allocateFourResourcesConcurrently
    scope
    acquireFirst
    releaseFirst
    acquireSecond
    releaseSecond
    acquireThird
    releaseThird
    acquireFourth
    releaseFourth =
        withOpenScope scope \state -> do
            ((first, second), (third, fourth)) <-
                concurrently
                    ( concurrently
                        (allocateInScope state acquireFirst releaseFirst)
                        (allocateInScope state acquireSecond releaseSecond)
                    )
                    ( concurrently
                        (allocateInScope state acquireThird releaseThird)
                        (allocateInScope state acquireFourth releaseFourth)
                    )
            pure (first, second, third, fourth)

registerResource :: ResourceScope -> IO () -> IO ResourceKey
registerResource scope cleanup =
    withOpenScope scope \state ->
        ResourceKey <$> runInScope state (register cleanup)

releaseResource :: ResourceKey -> IO ()
releaseResource (ResourceKey key) = release key

runInScope :: InternalState -> ResourceT IO a -> IO a
runInScope = flip runInternalState

allocateInScope
    :: InternalState
    -> IO a
    -> (a -> IO ())
    -> IO (ResourceKey, a)
allocateInScope state acquireResource cleanupResource = do
    (key, value) <- runInScope state (allocate acquireResource cleanupResource)
    pure (ResourceKey key, value)

withOpenScope :: ResourceScope -> (InternalState -> IO a) -> IO a
withOpenScope (ResourceScope stateVar) action =
    withMVar stateVar \case
        Nothing -> throwIO (userError "resource scope is closed")
        Just state -> action state
