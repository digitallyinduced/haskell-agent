-- | Dynamically owned resources with explicit early release.
--
-- The public API stays in 'IO' so resource ownership does not force
-- 'ResourceT' through the agent loop and provider interfaces.
module Agent.ResourceScope
    ( ResourceScope
    , ResourceKey
    , newResourceScope
    , closeResourceScope
    , allocateResource
    , allocateAcquireResource
    , registerResource
    , releaseResource
    ) where

import Control.Concurrent.MVar
    ( MVar
    , modifyMVar
    , newMVar
    , withMVar
    )
import Control.Exception.Safe (throwIO)
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
import Data.Acquire (Acquire, allocateAcquire)

newtype ResourceScope = ResourceScope (MVar (Maybe InternalState))

newtype ResourceKey = ResourceKey ReleaseKey

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

allocateAcquireResource
    :: ResourceScope
    -> Acquire a
    -> IO (ResourceKey, a)
allocateAcquireResource scope acquire =
    withOpenScope scope \state -> do
        (key, value) <- runInScope state (allocateAcquire acquire)
        pure (ResourceKey key, value)

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
