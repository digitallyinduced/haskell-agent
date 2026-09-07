-- | Engine mailbox protocol and resources whose lifetime is owned by the engine.
module Agent.CLI.MacOS.EngineState
    ( EngineCommand(..)
    , SessionMutation(..)
    , Engine(..)
    ) where

import Agent.CLI.MacOS.BrowserBridge (BrowserHost)
import Agent.CLI.MacOS.ComputerBridge (ComputerHost)
import Agent.CLI.MacOS.EngineCallbacks
    ( IntegrationResultCallback
    , SearchCallback
    , SessionResultCallback
    , TaskSnapshotCallback
    )
import Agent.CLI.MacOS.EngineMailbox (EngineMailbox)
import Agent.CLI.MacOS.InteractionState (InteractionRuntime)
import Agent.CLI.MacOS.McpAdminBridge (McpResultCallback)
import Agent.CLI.MacOS.NativeRequest (BridgeRequest)
import Agent.CLI.MacOS.TurnState (NativeTurnOptions, TaskResult)
import Agent.Loop (ImageAttachment)
import Agent.Json (RawJson)
import Control.Concurrent.Async (Async)
import Control.Concurrent.STM (TVar)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Word (Word64)
import Foreign.Ptr (FunPtr, Ptr)

data EngineCommand
    = EngineRequest !BridgeRequest
    | EngineSearch !Text !Int !(FunPtr SearchCallback) !(Ptr ())
    | EngineSessionMutation
        !SessionMutation !(FunPtr SessionResultCallback) !(Ptr ())
    | EngineMcpRestart !Word64 !Text !(FunPtr McpResultCallback) !(Ptr ())
    | EngineIntegrationAdminList
        !(FunPtr IntegrationResultCallback) !(Ptr ())
    | EngineIntegrationAdminCall
        !Text !RawJson !(FunPtr IntegrationResultCallback) !(Ptr ())
    | EngineCancelTask !Text
    | EngineTaskSnapshot !(FunPtr TaskSnapshotCallback) !(Ptr ())
    | EngineSetTaskLimit !Int
    | EngineTaskSession !Text !Text
    | EngineTaskFinished !Text !TaskResult
    | EngineStop

data SessionMutation
    = SessionRename !Text !Text
    | SessionDelete !Text
    | SessionArchive !Text !Bool

data Engine = Engine
    { engineCommands :: !(EngineMailbox EngineCommand)
    , engineWorker :: !(Async ())
    , engineStagedImages :: !(TVar (Map Text [ImageAttachment]))
    , engineBrowser :: !BrowserHost
    , engineComputer :: !ComputerHost
    , engineStagedTurnOptions :: !(TVar (Map Text NativeTurnOptions))
    , engineInteractions :: !InteractionRuntime
    }
