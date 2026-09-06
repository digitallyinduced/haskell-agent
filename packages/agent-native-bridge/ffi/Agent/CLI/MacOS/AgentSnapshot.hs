-- | Snapshot the currently running agent tree without changing turn ownership.
module Agent.CLI.MacOS.AgentSnapshot (activeAgentSnapshot) where

import qualified Agent.CLI.AgentViewport as Viewport
import Agent.CLI.MacOS.EngineEvents (failureEvent, successEvent)
import Agent.CLI.MacOS.NativeRequest (BridgeRequest(..))
import Agent.CLI.MacOS.TurnState (TurnControl(..))
import Control.Concurrent.STM (readTVarIO)
import Control.Exception.Safe (tryAny)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as Text

activeAgentSnapshot :: TurnControl -> BridgeRequest -> IO Aeson.Value
activeAgentSnapshot control request = do
    loadSnapshot <- readTVarIO control.turnControlAgentSnapshot
    tryAny loadSnapshot >>= \case
        Left exception -> pure $
            failureEvent request.requestId (Text.pack (show exception))
        Right agents -> pure $
            successEvent request.requestId (map agentEntryJSON agents)

agentEntryJSON :: Viewport.AgentEntry -> Aeson.Value
agentEntryJSON entry =
    Aeson.object
        [ "path" Aeson..= entry.agentPath
        , "status" Aeson..= entry.agentStatus
        , "model" Aeson..= entry.agentModel
        , "steps" Aeson..= map agentStepJSON entry.agentSteps
        ]

agentStepJSON :: Viewport.AgentStep -> Aeson.Value
agentStepJSON step =
    Aeson.object
        [ "state" Aeson..= agentStepStateText step.agentStepState
        , "title" Aeson..= step.agentStepTitle
        , "detail" Aeson..= step.agentStepDetail
        ]

agentStepStateText :: Viewport.AgentStepState -> Text
agentStepStateText = \case
    Viewport.AgentStepRunning -> "running"
    Viewport.AgentStepCompleted -> "completed"
    Viewport.AgentStepFailed -> "failed"
    Viewport.AgentStepInfo -> "info"
