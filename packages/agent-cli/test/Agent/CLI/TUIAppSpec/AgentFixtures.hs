module Agent.CLI.TUIAppSpec.AgentFixtures (rootEntry, childEntry) where

import Agent.CLI.AgentViewport (AgentEntry(..), AgentTarget(..))
import Agent.Subagents (SubagentId(..))
import Agent.TUI.Model (initialUiState)
import Data.Text (Text)
import qualified Data.Text as Text

rootEntry :: AgentEntry
rootEntry = AgentEntry
    { agentTarget = AgentRoot
    , agentPath = "/root"
    , agentStatus = "active"
    , agentModel = Nothing
    , agentSteps = []
    , agentTranscript = []
    , agentConversation = initialUiState
    }

childEntry :: Int -> AgentEntry
childEntry index = AgentEntry
    { agentTarget = AgentChild (SubagentId name)
    , agentPath = "/root/" <> name
    , agentStatus = "running"
    , agentModel = Just "gpt-5.6-luna"
    , agentSteps = []
    , agentTranscript = []
    , agentConversation = initialUiState
    }
  where
    name :: Text
    name = "agent-" <> Text.pack (show index)
