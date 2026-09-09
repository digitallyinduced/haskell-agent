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
    , closeResourceScopeChecked
    , allocateAcquire
    , allocateResource
    , allocateResourcesConcurrently
    , allocateFourResourcesConcurrently
    , registerResource
    , releaseResource
    ) where

import Control.Concurrent.Async (concurrently)
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar
    , newMVar
    , withMVar
    )
import Control.Exception.Safe (bracket, throwIO)
-- safe-exceptions makes bracket release uninterruptible.  State cleanup must
-- retain interruptible masking so blocking finalizers can be cancelled.
import qualified Control.Exception as Exception (bracket)
import Data.Acquire (Acquire)
import qualified Data.Acquire as Acquire
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
-- resourcet exposes checked cleanup of an existing state only in this module.
-- Keep that dependency at the resource-ownership boundary.
import Control.Monad.Trans.Resource.Internal (stateCleanupChecked)

newtype ResourceScope = ResourceScope (MVar (Maybe InternalState))

newtype ResourceKey = ResourceKey ReleaseKey

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

-- | Close all remaining resources using resourcet's best-effort cleanup:
-- finalizer exceptions are suppressed after all finalizers are attempted.
-- Use 'closeResourceScopeChecked' or 'releaseResource' when the caller needs
-- cleanup failure reporting.
closeResourceScope :: ResourceScope -> IO ()
closeResourceScope = closeResourceScopeWith closeInternalState

-- | Close all remaining resources and report cleanup failures after every
-- finalizer has been attempted.  Subsequent closure calls are no-ops, including
-- when this call reports a resourcet @ResourceCleanupException@.
closeResourceScopeChecked :: ResourceScope -> IO ()
closeResourceScopeChecked = closeResourceScopeWith (stateCleanupChecked Nothing)

closeResourceScopeWith :: (InternalState -> IO ()) -> ResourceScope -> IO ()
closeResourceScopeWith cleanup (ResourceScope stateVar) =
    -- Detachment transfers ownership from the scope to this bracket.  Its
    -- release action cannot be skipped by cancellation between those steps.
    Exception.bracket
        (modifyMVar stateVar \current -> pure (Nothing, current))
        (mapM_ cleanup)
        (const (pure ()))

-- | Acquire a resource together with its composed release action.
--
-- Intermediate acquisitions are released if composition fails before the
-- complete value can be registered.  The returned key releases the complete
-- composition once, in reverse dependency order.
allocateAcquire :: ResourceScope -> Acquire a -> IO (ResourceKey, a)
allocateAcquire scope acquisition =
    withOpenScope scope \state -> do
        (key, value) <- runInScope state (Acquire.allocateAcquire acquisition)
        pure (ResourceKey key, value)

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
