{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

-- | A lifecycle-safe, per-key single-flight cache for owned resources.
module Agent.Store.PoolCache
    ( PoolCache
    , newPoolCache
    , acquirePoolCache
    , closePoolCache
    ) where

import qualified Control.Concurrent.Stream as ConcurrentStream
import Control.Concurrent.MVar
import Control.Concurrent.STM
import qualified Control.Exception as Exception
import Control.Monad (void)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data PoolCache key err resource = PoolCache
    { cacheState :: !(MVar (PoolCacheState key err resource))
    , cacheOpen :: !(key -> IO (Either err resource))
    , cacheClose :: !(resource -> IO ())
    , cacheClosedError :: !err
    , cacheExceptionError :: !(Exception.SomeException -> err)
    , cacheCloseConcurrency :: !Int
    }

data PoolCacheState key err resource
    = CacheOpen !(Map key (PoolCacheEntry err resource))
    | CacheClosing !(TMVar (Either Exception.SomeException ()))
    | CacheClosed !(Either Exception.SomeException ())

data PoolCacheEntry err resource
    = PoolReady !resource
    | PoolOpening !(TMVar (Either err resource))

data AcquireDecision err resource
    = AcquireReady !(Either err resource)
    | AcquireWait !(TMVar (Either err resource))
    | AcquireLead !(TMVar (Either err resource))

data CloseDecision err resource
    = CloseComplete !(Either Exception.SomeException ())
    | CloseWait !(TMVar (Either Exception.SomeException ()))
    | CloseLead
        !(TMVar (Either Exception.SomeException ()))
        ![resource]
        ![TMVar (Either err resource)]

newPoolCache
    :: Int
    -> err
    -> (Exception.SomeException -> err)
    -> (key -> IO (Either err resource))
    -> (resource -> IO ())
    -> IO (PoolCache key err resource)
newPoolCache closeConcurrency closedError exceptionError open close = do
    state <- newMVar (CacheOpen Map.empty)
    pure PoolCache
        { cacheState = state
        , cacheOpen = open
        , cacheClose = close
        , cacheClosedError = closedError
        , cacheExceptionError = exceptionError
        , cacheCloseConcurrency = max 1 closeConcurrency
        }

acquirePoolCache
    :: Ord key
    => PoolCache key err resource
    -> key
    -> IO (Either err resource)
acquirePoolCache cache key =
    Exception.mask \restore -> do
        decision <- modifyMVar cache.cacheState \case
            state@CacheClosing{} ->
                pure (state, AcquireReady (Left cache.cacheClosedError))
            state@CacheClosed{} ->
                pure (state, AcquireReady (Left cache.cacheClosedError))
            CacheOpen entries ->
                case Map.lookup key entries of
                    Just (PoolReady resource) ->
                        pure
                            ( CacheOpen entries
                            , AcquireReady (Right resource)
                            )
                    Just (PoolOpening completion) ->
                        pure (CacheOpen entries, AcquireWait completion)
                    Nothing -> do
                        completion <- newEmptyTMVarIO
                        pure
                            ( CacheOpen
                                (Map.insert key
                                    (PoolOpening completion)
                                    entries)
                            , AcquireLead completion
                            )
        case decision of
            AcquireReady result -> pure result
            AcquireWait completion ->
                restore (atomically (readTMVar completion))
            AcquireLead completion -> do
                opened <-
                    tryAnyException
                        (restore (cache.cacheOpen key))
                case opened of
                    Left exception -> do
                        abandonOpening cache key completion
                            (Left (cache.cacheExceptionError exception))
                        Exception.throwIO exception
                    Right (Left err) -> do
                        abandonOpening cache key completion (Left err)
                        pure (Left err)
                    Right (Right resource) ->
                        publishResource cache key completion resource restore

abandonOpening
    :: Ord key
    => PoolCache key err resource
    -> key
    -> TMVar (Either err resource)
    -> Either err resource
    -> IO ()
abandonOpening cache key completion result = do
    modifyMVar_ cache.cacheState \case
        CacheOpen entries ->
            pure (CacheOpen (Map.delete key entries))
        state -> pure state
    atomically $ void (tryPutTMVar completion result)

publishResource
    :: Ord key
    => PoolCache key err resource
    -> key
    -> TMVar (Either err resource)
    -> resource
    -> (forall a. IO a -> IO a)
    -> IO (Either err resource)
publishResource cache key completion resource restore = do
    accepted <- modifyMVar cache.cacheState \case
        CacheOpen entries ->
            case Map.lookup key entries of
                Just PoolOpening{} -> do
                    -- Publish the result before releasing the cache-state
                    -- lock. Shutdown can therefore linearize only before
                    -- this acquisition (and reject it) or after it.
                    atomically $
                        void (tryPutTMVar completion (Right resource))
                    pure
                        ( CacheOpen
                            (Map.insert key (PoolReady resource) entries)
                        , True
                        )
                _ -> pure (CacheOpen entries, False)
        state -> pure (state, False)
    if accepted
        then pure (Right resource)
        else do
            closed <-
                tryAnyException
                    (restore (cache.cacheClose resource))
            let result = Left cache.cacheClosedError
            atomically $ void (tryPutTMVar completion result)
            case closed of
                Left exception -> Exception.throwIO exception
                Right () -> pure result

closePoolCache :: PoolCache key err resource -> IO ()
closePoolCache cache =
    Exception.mask \restore -> do
        decision <- modifyMVar cache.cacheState \case
            state@(CacheClosed outcome) ->
                pure (state, CloseComplete outcome)
            state@(CacheClosing completion) ->
                pure (state, CloseWait completion)
            CacheOpen entries -> do
                completion <- newEmptyTMVarIO
                let ready =
                        [ resource
                        | PoolReady resource <- Map.elems entries
                        ]
                    opening =
                        [ pending
                        | PoolOpening pending <- Map.elems entries
                        ]
                pure
                    ( CacheClosing completion
                    , CloseLead completion ready opening
                    )
        case decision of
            CloseComplete outcome -> replayClose outcome
            CloseWait completion ->
                restore (atomically (readTMVar completion))
                    >>= replayClose
            CloseLead completion ready opening -> do
                outcome <-
                    tryAnyException (restore (closeOwned cache ready opening))
                modifyMVar_ cache.cacheState \_ ->
                    pure (CacheClosed outcome)
                atomically $ void (tryPutTMVar completion outcome)
                replayClose outcome

closeOwned
    :: PoolCache key err resource
    -> [resource]
    -> [TMVar (Either err resource)]
    -> IO ()
closeOwned cache ready opening = do
    outcomes <- ConcurrentStream.mapConcurrentlyBounded
        cache.cacheCloseConcurrency
        run
        (map CloseResource ready <> map WaitOpening opening)
    case [exception | Left exception <- outcomes] of
        exception : _ -> Exception.throwIO exception
        [] -> pure ()
  where
    run = \case
        CloseResource resource ->
            tryAnyException (cache.cacheClose resource)
        WaitOpening completion -> do
            _ <- atomically (readTMVar completion)
            pure (Right ())

data CloseAction err resource
    = CloseResource !resource
    | WaitOpening !(TMVar (Either err resource))

replayClose :: Either Exception.SomeException () -> IO ()
replayClose = either Exception.throwIO pure

tryAnyException
    :: IO a
    -> IO (Either Exception.SomeException a)
tryAnyException = Exception.try
