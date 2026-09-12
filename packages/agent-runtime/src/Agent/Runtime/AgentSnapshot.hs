-- | Presentation-independent metadata for observing the running agent tree.
-- Terminal transcripts and UI reducer state deliberately do not cross this
-- boundary.
module Agent.Runtime.AgentSnapshot
    ( AgentSnapshot(..)
    , AgentStep(..)
    , AgentStepState(..)
    ) where

import Data.Text (Text)

data AgentStepState
    = AgentStepRunning
    | AgentStepCompleted
    | AgentStepFailed
    | AgentStepInfo
    deriving (Eq, Show)

data AgentStep = AgentStep
    { agentStepState :: !AgentStepState
    , agentStepTitle :: !Text
    , agentStepDetail :: !(Maybe Text)
    }
    deriving (Eq, Show)

data AgentSnapshot = AgentSnapshot
    { agentPath :: !Text
    , agentStatus :: !Text
    , agentModel :: !(Maybe Text)
    , agentSteps :: ![AgentStep]
    }
    deriving (Eq, Show)
