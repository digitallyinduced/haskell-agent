-- | Compaction checkpoints shared by turn execution and its hosts.
module Agent.Runtime.Compaction
    ( AutomaticCompactionBoundary(..)
    ) where

import Agent.Loop (TurnInput)
import Agent.Responses.Types (ResponseItem)

-- | A provider compaction checkpoint that has already been installed in the
-- live conversation and durable transcript. The enclosing user turn uses this
-- as its new prefix so it appends only post-checkpoint items.
data AutomaticCompactionBoundary = AutomaticCompactionBoundary
    { automaticCompactionHistory :: ![ResponseItem]
    , automaticCompactionPendingInputs :: ![TurnInput]
    } deriving (Eq, Show)
