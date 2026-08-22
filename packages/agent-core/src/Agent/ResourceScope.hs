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
    , registerResource
    , releaseResource
    ) where

import Control.Concurrent (ThreadId, forkIOWithUnmask, myThreadId)
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar
    , newEmptyMVar
    , newMVar
    , putMVar
    , readMVar
    , withMVar
    )
import qualified Control.Exception as Exception
import Control.Exception.Safe
    ( SomeException
    , mask
    , throwIO
    , uninterruptibleMask_
    )
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

type ScopeOutcome = Either SomeException ()

data ResourceScopeState
    = ScopeOpen !InternalState
    -- A full completion cell is also the stable closed state.
    | ScopeClosing !ThreadId !(MVar ScopeOutcome)

newtype ResourceScope = ResourceScope (MVar ResourceScopeState)

data CloseDecision
    = CloseComplete !ScopeOutcome
    | CloseWait !(MVar ScopeOutcome)

newtype ResourceKey = ResourceKey ReleaseKey

newResourceScope :: IO ResourceScope
newResourceScope = do
    state <- createInternalState
    ResourceScope <$> newMVar (ScopeOpen state)

closeResourceScope :: ResourceScope -> IO ()
closeResourceScope (ResourceScope stateVar) =
    mask \restore -> do
        caller <- myThreadId
        decision <- modifyMVar stateVar \case
            ScopeOpen state -> do
                done <- newEmptyMVar
                -- Cleanup belongs to the scope, not whichever close caller
                -- happened to start it.
                owner <- forkIOWithUnmask \unmask -> do
                    result <- Exception.try @SomeException $
                        unmask (closeInternalState state)
                    uninterruptibleMask_ (putMVar done result)
                pure
                    ( ScopeClosing owner done
                    , CloseWait done
                    )
            closing@(ScopeClosing owner done)
                | caller == owner ->
                    pure (closing, CloseComplete (Right ()))
                | otherwise ->
                    pure (closing, CloseWait done)
        result <- case decision of
            CloseComplete completed -> pure completed
            CloseWait done -> restore (readMVar done)
        either throwIO pure result

allocateResource
    :: ResourceScope
    -> IO a
    -> (a -> IO ())
    -> IO (ResourceKey, a)
allocateResource scope acquire cleanup =
    withOpenScope scope \state -> do
        (key, value) <- runInScope state (allocate acquire cleanup)
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
        ScopeOpen state -> action state
        ScopeClosing{} -> throwIO (userError "resource scope is closed")
