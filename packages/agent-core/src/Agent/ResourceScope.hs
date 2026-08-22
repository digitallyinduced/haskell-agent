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

import Control.Concurrent (ThreadId, forkIO, myThreadId)
import Control.Concurrent.STM
    ( STM
    , TMVar
    , TVar
    , atomically
    , modifyTVar'
    , newEmptyTMVar
    , newTVarIO
    , readTMVar
    , readTVar
    , retry
    , tryPutTMVar
    , writeTVar
    )
import Control.Exception.Safe
    ( SomeException
    , finally
    , mask
    , onException
    , throwIO
    , tryAny
    )
import Control.Monad (void)
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

-- Active operations cover allocation, registration, and explicit release.
-- Closing rejects new allocations and drains those operations before ResourceT
-- claims the remaining cleanup actions. The completion cell makes close a
-- barrier for concurrent close/release callers.
data ResourceScopeState
    = ScopeOpen !InternalState !Int
    | ScopeClosing !InternalState !Int !(TMVar CloseOutcome)
    | ScopeCleaning !ThreadId !(TMVar CloseOutcome)
    | ScopeClosed

data CloseOutcome
    = CloseComplete !(Either SomeException ())
    | CloseRetry

data CloseDecision
    = CloseLeader !InternalState !(TMVar CloseOutcome)
    | CloseFollower !(TMVar CloseOutcome)
    | CloseFinished

data ReleaseDecision
    = ReleaseNow
    | ReleaseFollower !(TMVar CloseOutcome)
    | ReleaseFinished

newtype ResourceScope = ResourceScope (TVar ResourceScopeState)

data ResourceKey = ResourceKey !ResourceScope !ReleaseKey

newResourceScope :: IO ResourceScope
newResourceScope = do
    state <- createInternalState
    ResourceScope <$> newTVarIO (ScopeOpen state 0)

closeResourceScope :: ResourceScope -> IO ()
closeResourceScope scope =
    mask \restore -> do
        owner <- myThreadId
        let close = atomically (beginClose scope owner) >>= \case
                CloseFinished -> pure ()
                CloseFollower done ->
                    restore (atomically (readTMVar done)) >>= \case
                        CloseComplete result -> either throwIO pure result
                        CloseRetry -> close
                CloseLeader state done -> do
                    restore (atomically (waitForOperations scope))
                        `onException` atomically (rollbackClose scope)
                    -- Cleanup must not be owned by the caller that happened
                    -- to initiate close: cancelling that waiter must not
                    -- interrupt a blocking finalizer and make the detached
                    -- ResourceT state unrecoverable. The worker is tracked by
                    -- the scope's Cleaning state and completion TMVar; later
                    -- close/release callers join it through that barrier.
                    void (forkIO (runCleanup scope state done))
                        `onException` atomically (rollbackClose scope)
                    restore (atomically (readTMVar done)) >>= \case
                        CloseComplete result -> either throwIO pure result
                        CloseRetry -> close
        close

runCleanup
    :: ResourceScope
    -> InternalState
    -> TMVar CloseOutcome
    -> IO ()
runCleanup scope state done = do
    owner <- myThreadId
    atomically (markCleaning scope owner)
    result <- tryAny (closeInternalState state)
    atomically (finishClose scope done result)

allocateResource
    :: ResourceScope
    -> IO a
    -> (a -> IO ())
    -> IO (ResourceKey, a)
allocateResource scope acquire cleanup =
    withOpenScope scope \state -> do
        (key, value) <- runInScope state (allocate acquire cleanup)
        pure (ResourceKey scope key, value)

registerResource :: ResourceScope -> IO () -> IO ResourceKey
registerResource scope cleanup =
    withOpenScope scope \state ->
        ResourceKey scope <$> runInScope state (register cleanup)

releaseResource :: ResourceKey -> IO ()
releaseResource (ResourceKey scope key) =
    mask \restore -> do
        owner <- myThreadId
        let releaseOrWait = atomically (beginRelease scope owner) >>= \case
                ReleaseFinished -> pure ()
                ReleaseFollower done ->
                    restore (atomically (readTMVar done)) >>= \case
                        CloseComplete result -> either throwIO pure result
                        CloseRetry -> releaseOrWait
                ReleaseNow ->
                    restore (release key)
                        `finally` atomically (finishOperation scope)
        releaseOrWait

runInScope :: InternalState -> ResourceT IO a -> IO a
runInScope = flip runInternalState

withOpenScope :: ResourceScope -> (InternalState -> IO a) -> IO a
withOpenScope scope action =
    mask \restore ->
        atomically (beginOperation scope) >>= \case
            Nothing -> throwIO (userError "resource scope is closed")
            Just state ->
                restore (action state)
                    `finally` atomically (finishOperation scope)

beginOperation :: ResourceScope -> STM (Maybe InternalState)
beginOperation (ResourceScope stateVar) =
    readTVar stateVar >>= \case
        ScopeOpen state active -> do
            writeTVar stateVar (ScopeOpen state (active + 1))
            pure (Just state)
        ScopeClosing{} -> pure Nothing
        ScopeCleaning{} -> pure Nothing
        ScopeClosed -> pure Nothing

finishOperation :: ResourceScope -> STM ()
finishOperation (ResourceScope stateVar) =
    modifyTVar' stateVar \case
        ScopeOpen state active ->
            ScopeOpen state (max 0 (active - 1))
        ScopeClosing state active done ->
            ScopeClosing state (max 0 (active - 1)) done
        state -> state

beginRelease :: ResourceScope -> ThreadId -> STM ReleaseDecision
beginRelease (ResourceScope stateVar) owner =
    readTVar stateVar >>= \case
        ScopeOpen state active -> do
            writeTVar stateVar (ScopeOpen state (active + 1))
            pure ReleaseNow
        ScopeClosing _ _ done ->
            pure (ReleaseFollower done)
        ScopeCleaning cleaningOwner done
            | cleaningOwner == owner -> pure ReleaseFinished
            | otherwise -> pure (ReleaseFollower done)
        ScopeClosed -> pure ReleaseFinished

beginClose :: ResourceScope -> ThreadId -> STM CloseDecision
beginClose (ResourceScope stateVar) owner =
    readTVar stateVar >>= \case
        ScopeOpen state active -> do
            done <- newEmptyTMVar
            writeTVar stateVar (ScopeClosing state active done)
            pure (CloseLeader state done)
        ScopeClosing _ _ done ->
            pure (CloseFollower done)
        ScopeCleaning cleaningOwner done
            | cleaningOwner == owner -> pure CloseFinished
            | otherwise -> pure (CloseFollower done)
        ScopeClosed -> pure CloseFinished

waitForOperations :: ResourceScope -> STM ()
waitForOperations (ResourceScope stateVar) =
    readTVar stateVar >>= \case
        ScopeClosing _ 0 _ -> pure ()
        ScopeClosing{} -> retry
        _ -> pure ()

rollbackClose :: ResourceScope -> STM ()
rollbackClose (ResourceScope stateVar) =
    readTVar stateVar >>= \case
        ScopeClosing state active done -> do
            writeTVar stateVar (ScopeOpen state active)
            void (tryPutTMVar done CloseRetry)
        _ -> pure ()

markCleaning :: ResourceScope -> ThreadId -> STM ()
markCleaning (ResourceScope stateVar) owner =
    readTVar stateVar >>= \case
        ScopeClosing _ 0 done ->
            writeTVar stateVar (ScopeCleaning owner done)
        _ -> retry

finishClose
    :: ResourceScope
    -> TMVar CloseOutcome
    -> Either SomeException ()
    -> STM ()
finishClose (ResourceScope stateVar) done result = do
    writeTVar stateVar ScopeClosed
    void (tryPutTMVar done (CloseComplete result))
