module Agent.CLI.Compaction.Types
    ( CompactOutcome(..)
    , OpenAiCompactionSender
    , OccupancyKind(..)
    , OccupancySnapshot(..)
    , estimatedOccupancy
    , reportedOccupancy
    ) where

import Agent.Error (ApiError)
import Agent.Responses.Types (Response, ResponseCreateParams, ResponseItem)
import Data.Text (Text)

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
