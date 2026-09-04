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
    withOpenScope scope \state -> do
        (key, value) <- runInScope state (allocate acquire cleanup)
        pure (ResourceKey key, value)

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
            (wrap <$> runInScope state (allocate acquireLeft releaseLeft))
            (wrap <$> runInScope state (allocate acquireRight releaseRight))
  where
    wrap (key, value) = (ResourceKey key, value)

registerResource :: ResourceScope -> IO () -> IO ResourceKey
registerResource scope cleanup =
    withOpenScope scope \state ->
        ResourceKey <$> runInScope state (register cleanup)

releaseResource :: ResourceKey -> IO ()
releaseResource (ResourceKey key) = release key

runInScope :: InternalState -> ResourceT IO a -> IO a
runInScope = flip runInternalState

withOpenScope :: ResourceScope -> (InternalState -> IO a) -> IO a
withOpenScope (ResourceScope stateVar) action =
    withMVar stateVar \case
        Nothing -> throwIO (userError "resource scope is closed")
        Just state -> action state
