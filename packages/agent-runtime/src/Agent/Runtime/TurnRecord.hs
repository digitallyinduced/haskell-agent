-- | Typed input to turn persistence. Keep canonical model history separate
-- from display-only activity until a storage adapter encodes the record.
module Agent.Runtime.TurnRecord
    ( TurnRecord(..)
    ) where

import Agent.Loop (TokenUsage)
import Agent.Runtime.TurnEngine (ModelItems, DisplayItems)
import Agent.Store.Postgres.Session (TranscriptEffect)
import Agent.Telemetry (TurnTelemetry)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)

data TurnRecord = TurnRecord
    { turnAt :: !UTCTime
    , turnUserText :: !Text
    , turnAssistantText :: !(Maybe Text)
    , turnError :: !(Maybe Text)
    , turnResponseId :: !(Maybe Text)
    , turnEffect :: !TranscriptEffect
    , turnItems :: !ModelItems
    , turnDisplayItems :: !DisplayItems
    , turnUsage :: !(Maybe TokenUsage)
    , turnProviderTelemetry :: ![TurnTelemetry]
    }
    deriving (Eq, Show)
