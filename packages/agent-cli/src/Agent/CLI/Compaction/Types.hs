module Agent.CLI.Compaction.Types
    ( AutomaticCompactionBoundary(..)
    , CompactOutcome(..)
    , CompactionInstall(..)
    , OpenAiCompactionSender
    , OccupancyKind(..)
    , OccupancySnapshot(..)
    , estimatedOccupancy
    , occupancyMatchesHistory
    , reportedOccupancy
    ) where

import Agent.Error (ApiError)
import Agent.Loop (BackendSnapshot(..))
import Agent.Responses.Types (Response, ResponseCreateParams, ResponseItem)
import Agent.Runtime.Compaction (AutomaticCompactionBoundary(..))
import Data.Text (Text)

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
    -- | Bind live provider-managed context to the exact host checkpoint.
    -- Claude can compact its private context without shrinking host history;
    -- such counts are not valid when that history is imported afresh.
    , occupancyCheckpoint :: !(Maybe BackendSnapshot)
    } deriving (Eq, Show)

reportedOccupancy :: Int -> Int -> OccupancySnapshot
reportedOccupancy tokens historyLength =
    OccupancySnapshot
        { occupancyTokens = tokens
        , occupancyLength = historyLength
        , occupancyKind = ReportedOccupancy
        , occupancyCheckpoint = Nothing
        }

estimatedOccupancy :: Int -> Int -> OccupancySnapshot
estimatedOccupancy tokens historyLength =
    OccupancySnapshot
        { occupancyTokens = tokens
        , occupancyLength = historyLength
        , occupancyKind = EstimatedOccupancy
        , occupancyCheckpoint = Nothing
        }

occupancyMatchesHistory :: [ResponseItem] -> OccupancySnapshot -> Bool
occupancyMatchesHistory history snapshot =
    snapshot.occupancyLength == length history
        && maybe True ((== history) . (.backendItems))
            snapshot.occupancyCheckpoint

type OpenAiCompactionSender =
    ResponseCreateParams -> IO (Either ApiError Response)
