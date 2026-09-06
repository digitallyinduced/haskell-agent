-- | Process-scoped resources shared by native and server session hosts.
-- This is resource ownership, not session startup or frontend orchestration.
module Agent.CLI.NativeProcess
    ( NativeProcessRuntime
        ( nativeMcpSupervisor
        , nativeSessionThreads
        , nativeNetworkRecovery
        , nativeStartCleanup
        , nativeMcpElicitation
        )
    , newNativeProcessRuntime
    , closeNativeProcessRuntime
    , restartNativeMcpRuntime
    ) where

import Agent.CLI.Session.Threads
    ( SessionThreadManager, closeSessionThreadManager, newSessionThreadManager )
import Agent.Connectivity.NetworkPath
    ( NetworkRecoveryMonitor
    , closeNetworkRecoveryMonitor
    , newNetworkRecoveryMonitor
    )
import qualified Agent.MCP as MCP
import Control.Concurrent.Async (Async, asyncWithUnmask, cancel)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception.Safe (finally, mask, mask_, onException)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import System.OsPath (OsPath)

-- | Allocated by 'newNativeProcessRuntime'; the owning host must close this
-- handle once no further turns can start. Workers outlive individual turns,
-- remain tracked here, and are cancelled and joined during close.
data NativeProcessRuntime = NativeProcessRuntime
    { nativeMcpSupervisor :: !MCP.McpSupervisor
    , nativeSessionThreads :: !SessionThreadManager
    , nativeNetworkRecovery :: !NetworkRecoveryMonitor
    , nativeStartCleanup :: !(IO () -> IO ())
    , nativeMcpElicitation
        :: !(IORef (Maybe
            (MCP.McpElicitRequest -> IO MCP.McpElicitResult)))
    , nativeCleanupWorker :: !(Async ())
    }

newNativeProcessRuntime :: OsPath -> IO NativeProcessRuntime
newNativeProcessRuntime root = mask \restore -> do
    elicitationRef <- newIORef Nothing
    cleanupStarted <- newIORef False
    cleanupRequest <- newEmptyMVar
    cleanupWorker <- asyncWithUnmask \unmask ->
        unmask (takeMVar cleanupRequest >>= id)
    let closeCleanupWorker = cancel cleanupWorker
        startCleanup action = mask_ do
            shouldStart <- atomicModifyIORef'
                cleanupStarted
                (\started -> (True, not started))
            if shouldStart
                then putMVar cleanupRequest action
                else pure ()
    networkMonitor <-
        restore newNetworkRecoveryMonitor
            `onException` closeCleanupWorker
    mcpSupervisor <-
        restore
            (MCP.newMcpSupervisorWith
                MCP.defaultMcpHostHooks
                    { MCP.mcpHostElicit = readIORef elicitationRef })
            `onException`
                (closeNetworkRecoveryMonitor networkMonitor
                    `finally` closeCleanupWorker)
    sessionThreads <-
        restore (newSessionThreadManager root)
            `onException`
                (MCP.closeMcpSupervisor mcpSupervisor
                    `finally`
                        (closeNetworkRecoveryMonitor networkMonitor
                            `finally` closeCleanupWorker))
    pure NativeProcessRuntime
        { nativeMcpSupervisor = mcpSupervisor
        , nativeSessionThreads = sessionThreads
        , nativeNetworkRecovery = networkMonitor
        , nativeStartCleanup = startCleanup
        , nativeMcpElicitation = elicitationRef
        , nativeCleanupWorker = cleanupWorker
        }

closeNativeProcessRuntime :: NativeProcessRuntime -> IO ()
closeNativeProcessRuntime runtime =
    closeSessionThreadManager runtime.nativeSessionThreads
        `finally`
            (MCP.closeMcpSupervisor runtime.nativeMcpSupervisor
                `finally`
                    (closeNetworkRecoveryMonitor
                        runtime.nativeNetworkRecovery
                        `finally` cancel runtime.nativeCleanupWorker))

restartNativeMcpRuntime :: NativeProcessRuntime -> IO ()
restartNativeMcpRuntime runtime =
    MCP.restartMcpSupervisor runtime.nativeMcpSupervisor
