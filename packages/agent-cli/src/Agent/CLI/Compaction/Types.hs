module Agent.CLI.Compaction.Types
    ( AutomaticCompactionBoundary(..)
    , CompactOutcome(..)
    , CompactionInstall(..)
    , OpenAiCompactionSender
    , OccupancyKind(..)
    , OccupancySnapshot(..)
    , estimatedOccupancy
    , reportedOccupancy
    ) where

import Agent.Error (ApiError)
import Agent.Loop (TurnInput)
import Agent.Responses.Types (Response, ResponseCreateParams, ResponseItem)
import Data.Text (Text)

-- | A provider compaction checkpoint that has already been installed in the
-- live conversation and durable transcript. The enclosing user turn uses this
-- as its new prefix so it appends only post-checkpoint items.
data AutomaticCompactionBoundary = AutomaticCompactionBoundary
    { automaticCompactionHistory :: ![ResponseItem]
    , automaticCompactionPendingInputs :: ![TurnInput]
    } deriving (Eq, Show)

-- | Whether an automatic-compaction hook installed the checkpoint outside the
-- provider wrapper. Root sessions return 'CompactionInstalled' after their
-- durable replace; lightweight callers can defer installation until a
-- successful continuation and retain the old rollback behaviour.
data CompactionInstall
    = CompactionInstalled
    | CompactionNotInstalled
    deriving (Eq, Show)

data CompactOutcome = CompactOutcome
    { compactBeforeTokens :: !Int
    , compactAfterTokens :: !Int
    , compactHistory :: ![ResponseItem]
    , compactSummary :: !Text
    } deriving (Eq, Show)

-- | Whether cached occupancy is provider-reported full-request usage or an
-- items-only estimate. Estimated snapshots must not be treated as complete
-- occupancy because they omit instructions, skills, and tool schemas.
data OccupancyKind
    = ReportedOccupancy
    | EstimatedOccupancy
    deriving (Eq, Show)

data OccupancySnapshot = OccupancySnapshot
    { occupancyTokens :: !Int
    , occupancyLength :: !Int
    , occupancyKind :: !OccupancyKind
    } deriving (Eq, Show)

reportedOccupancy :: Int -> Int -> OccupancySnapshot
reportedOccupancy tokens historyLength =
    OccupancySnapshot
        { occupancyTokens = tokens
        , occupancyLength = historyLength
        , occupancyKind = ReportedOccupancy
        }

estimatedOccupancy :: Int -> Int -> OccupancySnapshot
estimatedOccupancy tokens historyLength =
    OccupancySnapshot
        { occupancyTokens = tokens
        , occupancyLength = historyLength
        , occupancyKind = EstimatedOccupancy
        }

type OpenAiCompactionSender =
    ResponseCreateParams -> IO (Either ApiError Response)
