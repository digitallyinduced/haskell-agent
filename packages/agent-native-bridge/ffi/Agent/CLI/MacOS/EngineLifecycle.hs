-- | The worker owns its process runtime, store, children and pending callbacks.
module Agent.CLI.MacOS.EngineLifecycle (workerLifecycle) where

import Agent.CLI.MacOS.BrowserBridge (BrowserHost)
import Agent.CLI.MacOS.ComputerBridge (ComputerHost)
import Agent.CLI.MacOS.EngineEvents (EventCallback)
import Agent.CLI.MacOS.EngineCallbacks (invokeIntegrationResultCallback)
import Agent.CLI.MacOS.EngineMailbox
import Agent.CLI.MacOS.EngineState
import Agent.CLI.MacOS.EngineStore (closeEngineStore)
import Agent.CLI.MacOS.InteractionState
import Agent.CLI.MacOS.McpAdminBridge (invokeMcpResultCallback)
import Agent.CLI.MacOS.NativeSupervisor
    ( newIntegrationWorkerRegistry
    , shutdownIntegrationWorkers
    , shutdownRunningTurns
    , supervisorLoop
    )
import Agent.CLI.MacOS.TurnState
import Agent.CLI.MacOS.Marshalling (withText)
import Agent.CLI.NativeRuntime
import Agent.Loop (ImageAttachment)
import Agent.Store.Postgres (ManagedPostgresConfig)
import Control.Concurrent.MVar (newMVar)
import Control.Concurrent.STM
import Control.Exception.Safe (finally, tryAny)
import Control.Monad (forM_, void)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text (Text)
import Foreign.Ptr (FunPtr, Ptr, nullPtr)
import System.OsPath (OsPath)

workerLifecycle
    :: FunPtr EventCallback
    -> Ptr ()
    -> ManagedPostgresConfig
    -> OsPath
    -> EngineMailbox EngineCommand
    -> TVar (Map Text [ImageAttachment])
    -> BrowserHost
    -> ComputerHost
    -> TVar (Map Text NativeTurnOptions)
    -> InteractionRuntime
    -> IO ()
workerLifecycle
        callback context config root commands stagedImages browser computer
        stagedTurnOptions interactions =
    (do
        store <- newMVar Nothing
        processRuntime <- newNativeProcessRuntime root
        workerRegistry <- newTVarIO Map.empty
        integrationWorkers <- newIntegrationWorkerRegistry
        let cleanup =
                shutdownRunningTurns workerRegistry
                    `finally` shutdownIntegrationWorkers integrationWorkers
                    `finally` closeNativeProcessRuntime processRuntime
                    `finally` closeEngineStore store
        supervisorLoop
            callback
            context
            config
            store
            root
            processRuntime
            commands
            integrationWorkers
            stagedImages
            browser
            computer
            stagedTurnOptions
            interactions
            workerRegistry
            TaskSupervisor
                { supervisorLimit = defaultTaskLimit
                , supervisorPending = Seq.empty
                , supervisorRunning = Map.empty
                , supervisorKnownTaskIds = Set.empty
                }
            `finally` cleanup)
        `finally`
            (atomically $
                cancelPendingInteractions interactions.interactionPending)
        `finally` cancelPendingCallbacks commands

cancelPendingCallbacks :: EngineMailbox EngineCommand -> IO ()
cancelPendingCallbacks commands = do
    pending <- atomically do
        _ <- closeEngineMailbox commands EngineStop
        drainEngineCommands commands
    forM_ pending \case
        EngineMcpRestart expected _ callback context ->
            void $ tryAny $
                withText "engine stopped before MCP restart completed" $
                    invokeMcpResultCallback callback context (-1) expected
        EngineIntegrationAdminList callback context ->
            sendIntegrationStopped callback context
        EngineIntegrationAdminCall _ _ callback context ->
            sendIntegrationStopped callback context
        _ -> pure ()
  where
    sendIntegrationStopped callback context =
        void $ tryAny $
            withText "engine stopped before integration operation completed"
                \errorPointer errorLength ->
                    invokeIntegrationResultCallback
                        callback context (-1)
                        nullPtr 0
                        errorPointer errorLength
