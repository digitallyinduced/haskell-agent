{-# LANGUAGE NoFieldSelectors #-}

module Agent.Store.Postgres.Session.Types
    ( SessionMetadata(..)
    , SessionLegacyTarget(..)
    , TranscriptEffect(..)
    , SessionTurn(..)
    , SessionUsage(..)
    , StoredSession(..)
    , StoredEvent(..)
    , StoredTurn(..)
    , LegacySession(..)
    , ConversationSearchResult(..)
    , SessionTurnPage(..)
    , SessionResumeStats(..)
    ) where

import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Vector (Vector)

import Agent.Store.SessionItem (StoredResponseItem)

data SessionLegacyTarget = SessionLegacyTarget
    { sessionLegacyProvider :: !Text
    , sessionLegacyConnection :: !Text
    , sessionLegacyEffectiveModel :: !Text
    , sessionLegacyDialect :: !Text
    }
    deriving (Eq, Show)

data SessionTurnPage = SessionTurnPage
    { sessionPageTurns :: !(Vector StoredTurn)
    , sessionPageGenerationStart :: !Int64
    , sessionPageTotal :: !Int64
    , sessionPageHasOlder :: !Bool
    , sessionPageHasNewer :: !Bool
    }
    deriving (Eq, Show)

-- | Full-session aggregates for resume UI. The transcript preview stays
-- paged; these fields describe the whole stored conversation.
data SessionResumeStats = SessionResumeStats
    { sessionResumeTurnCount :: !Int64
    , sessionResumeMessageCount :: !Int64
    , sessionResumeToolCount :: !Int64
    , sessionResumeFirstPrompt :: !(Maybe Text)
    }
    deriving (Eq, Show)

data SessionMetadata = SessionMetadata
    { sessionMetadataKey :: !Text
    , sessionMetadataVersion :: !Int32
    , sessionMetadataCreatedAt :: !UTCTime
    , sessionMetadataUpdatedAt :: !UTCTime
    , sessionMetadataProvider :: !Text
    , sessionMetadataConnection :: !Text
    , sessionMetadataModel :: !Text
    , sessionMetadataTransportModel :: !(Maybe Text)
    , sessionMetadataDialect :: !Text
    , sessionMetadataLegacyTarget :: !(Maybe SessionLegacyTarget)
    , sessionMetadataCwd :: !Text
    , sessionMetadataEffort :: !Text
    , sessionMetadataTitle :: !Text
    , sessionMetadataTitleIsManual :: !Bool
    , sessionMetadataTitleRefreshIndex :: !Int64
    , sessionMetadataTitleUserTurns :: !Int64
    , sessionMetadataLastResponseId :: !(Maybe Text)
    , sessionMetadataInputTokens :: !Int64
    , sessionMetadataOutputTokens :: !Int64
    , sessionMetadataCachedTokens :: !Int64
    , sessionMetadataLastRecap :: !(Maybe Text)
    , sessionMetadataLastTurnSummary :: !(Maybe Text)
    , sessionMetadataLastRecapMainTurns :: !Int64
    }
    deriving (Eq, Show)

data SessionUsage = SessionUsage
    { sessionUsageInputTokens :: !Int64
    , sessionUsageOutputTokens :: !Int64
    , sessionUsageCachedTokens :: !Int64
    }
    deriving (Eq, Show)

data TranscriptEffect
    = TranscriptAppend
    | TranscriptReplace
    | TranscriptReset
    deriving (Eq, Show)

data SessionTurn = SessionTurn
    { sessionTurnOccurredAt :: !UTCTime
    , sessionTurnUserText :: !Text
    , sessionTurnAssistantText :: !(Maybe Text)
    , sessionTurnError :: !(Maybe Text)
    , sessionTurnResponseId :: !(Maybe Text)
    , sessionTurnEffect :: !TranscriptEffect
    , sessionTurnItems :: ![StoredResponseItem]
    , sessionTurnUsage :: !(Maybe SessionUsage)
    -- | Provider-neutral telemetry encoded as versioned JSON by the CLI.
    -- The store deliberately keeps this opaque to avoid depending on
    -- provider or loop packages.
    , sessionTurnProviderTelemetry :: !(Maybe Text)
    }
    deriving (Eq, Show)

data StoredSession = StoredSession
    { storedMetadata :: !SessionMetadata
    , storedTurns :: !(Vector StoredTurn)
    }
    deriving (Eq, Show)

data StoredEvent = StoredEvent
    { storedEventSequence :: !Int64
    , storedEventKind :: !Text
    , storedEventOccurredAt :: !UTCTime
    }
    deriving (Eq, Show)

data StoredTurn = StoredTurn
    { storedTurnIndex :: !Int64
    , storedEventSequence :: !Int64
    , storedTurn :: !SessionTurn
    }
    deriving (Eq, Show)

data ConversationSearchResult = ConversationSearchResult
    { searchSessionId :: !Text
    , searchTurnIndex :: !Int64
    , searchOccurredAt :: !UTCTime
    , searchUserText :: !Text
    , searchAssistantText :: !(Maybe Text)
    , searchRank :: !Double
    }
    deriving (Eq, Show)

-- | One fully decoded legacy JSONL session ready for an atomic import.
data LegacySession = LegacySession
    { legacySourcePath :: !Text
    , legacyContentHash :: !Text
    , legacyMetadata :: !SessionMetadata
    , legacyTurns :: ![SessionTurn]
    }
    deriving (Eq, Show)
