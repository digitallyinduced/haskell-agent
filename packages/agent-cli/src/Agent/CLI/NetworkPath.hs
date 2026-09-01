{-# LANGUAGE CPP #-}

-- | Scoped operating-system network recovery notifications.
--
-- On macOS this uses @NWPathMonitor@ to notice an unavailable-to-satisfied
-- path transition. Other platforms retain the polling-only recovery path.
module Agent.CLI.NetworkPath
    ( NetworkRecovery
    , NetworkRecoveryMonitor
    , armNetworkRecovery
    , closeNetworkRecoveryMonitor
    , networkRecovery
    , newNetworkRecoveryMonitor
    , withNetworkRecovery
    ) where

import Control.Concurrent.STM
    ( TVar
    , atomically
    , check
    , modifyTVar'
    , newTVarIO
    , readTVar
    )
import Control.Exception.Safe (bracket)

#if defined(darwin_HOST_OS)
import Control.Concurrent.MVar
    ( modifyMVar_
    , newMVar
    )
import Control.Exception.Safe
    ( onException
    , uninterruptibleMask_
    )
import Foreign.C.Types (CInt(..))
import Foreign.Ptr
    ( FunPtr
    , Ptr
    , freeHaskellFunPtr
    , nullPtr
    )
#endif

data NetworkPathState = NetworkPathState
    { pathSatisfied :: !(Maybe Bool)
    , recoveryGeneration :: !Int
    }

newtype NetworkRecovery = NetworkRecovery (TVar NetworkPathState)

data NetworkRecoveryMonitor = NetworkRecoveryMonitor
    { monitorRecovery :: !(Maybe NetworkRecovery)
    , monitorClose :: !(IO ())
    }

-- | Capture the current recovery generation and return an action that blocks
-- until a later unavailable-to-satisfied transition. The returned action is
-- restartable after asynchronous cancellation, which lets callers use the
-- same edge while racing both an in-flight request and its retry delay.
armNetworkRecovery :: NetworkRecovery -> IO (IO ())
armNetworkRecovery (NetworkRecovery stateVar) = do
    baseline <- atomically ((.recoveryGeneration) <$> readTVar stateVar)
    pure $ atomically do
        current <- (.recoveryGeneration) <$> readTVar stateVar
        check (current /= baseline)

-- | Acquire a process-scoped network path monitor. Unsupported platforms and
-- native setup failures expose 'Nothing', preserving polling-only recovery.
newNetworkRecoveryMonitor :: IO NetworkRecoveryMonitor
#if defined(darwin_HOST_OS)
newNetworkRecoveryMonitor =
    uninterruptibleMask_ do
        stateVar <- newTVarIO initialNetworkPathState
        let recovery = NetworkRecovery stateVar
        callback <- mkPathUpdateCallback (recordPathStatus recovery)
        monitor <-
            c_networkPathMonitorCreate callback
                `onException` freeHaskellFunPtr callback
        if monitor == nullPtr
            then do
                freeHaskellFunPtr callback
                pure inertNetworkRecoveryMonitor
            else do
                activeVar <- newMVar $
                    Just ActiveNetworkMonitor
                        { activeMonitor = monitor
                        , activeCallback = callback
                        }
                pure NetworkRecoveryMonitor
                    { monitorRecovery = Just recovery
                    , monitorClose =
                        uninterruptibleMask_ $
                            modifyMVar_ activeVar \case
                                Nothing -> pure Nothing
                                Just active -> do
                                    c_networkPathMonitorDestroy
                                        active.activeMonitor
                                    freeHaskellFunPtr active.activeCallback
                                    pure Nothing
                    }
#else
newNetworkRecoveryMonitor = pure inertNetworkRecoveryMonitor
#endif

inertNetworkRecoveryMonitor :: NetworkRecoveryMonitor
inertNetworkRecoveryMonitor = NetworkRecoveryMonitor
    { monitorRecovery = Nothing
    , monitorClose = pure ()
    }

networkRecovery :: NetworkRecoveryMonitor -> Maybe NetworkRecovery
networkRecovery = (.monitorRecovery)

-- | Stop callbacks, drain the monitor queue, and release its native callback.
-- Closing a monitor more than once is safe.
closeNetworkRecoveryMonitor :: NetworkRecoveryMonitor -> IO ()
closeNetworkRecoveryMonitor = (.monitorClose)

-- | Run an action with a scoped process-level network monitor.
withNetworkRecovery :: (Maybe NetworkRecovery -> IO a) -> IO a
withNetworkRecovery action =
    bracket
        newNetworkRecoveryMonitor
        closeNetworkRecoveryMonitor
        (action . networkRecovery)

initialNetworkPathState :: NetworkPathState
initialNetworkPathState = NetworkPathState
    { pathSatisfied = Nothing
    , recoveryGeneration = 0
    }

#if defined(darwin_HOST_OS)
data CNetworkPathMonitor

data ActiveNetworkMonitor = ActiveNetworkMonitor
    { activeMonitor :: !(Ptr CNetworkPathMonitor)
    , activeCallback :: !(FunPtr PathUpdateCallback)
    }

type PathUpdateCallback = CInt -> IO ()

recordPathStatus :: NetworkRecovery -> CInt -> IO ()
recordPathStatus (NetworkRecovery stateVar) rawSatisfied =
    atomically $ modifyTVar' stateVar \current ->
        let satisfied = rawSatisfied /= 0
            recovered =
                satisfied && current.pathSatisfied == Just False
        in NetworkPathState
            { pathSatisfied = Just satisfied
            , recoveryGeneration =
                if recovered
                    then current.recoveryGeneration + 1
                    else current.recoveryGeneration
            }

foreign import ccall "wrapper"
    mkPathUpdateCallback
        :: PathUpdateCallback
        -> IO (FunPtr PathUpdateCallback)

foreign import ccall safe "haskell_agent_network_path_monitor_create"
    c_networkPathMonitorCreate
        :: FunPtr PathUpdateCallback
        -> IO (Ptr CNetworkPathMonitor)

foreign import ccall safe "haskell_agent_network_path_monitor_destroy"
    c_networkPathMonitorDestroy
        :: Ptr CNetworkPathMonitor
        -> IO ()
#endif
