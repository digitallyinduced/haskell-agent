{-# LANGUAGE NoFieldSelectors #-}

module Agent.Store.Postgres.Session.Types
    ( SessionMetadata(..)
    , SessionLegacyTarget(..)
    , SessionPromptSnapshot(..)
    , SessionPromptEpoch(..)
    , TranscriptEffect(..)
    , SessionTurn(..)
    , SessionUsage(..)
    , StoredSession(..)
    , StoredEvent(..)
    , StoredTurn(..)
    , LegacySession(..)
    , ConversationSearchResult(..)
    , NativeConversationSearchResult(..)
    , SessionTurnPage(..)
    , SessionHistorySnapshot(..)
    , SessionResumeStats(..)
    , SessionTaskPlanStatus(..)
    , SessionTaskPlanItem(..)
    , SessionTaskPlan(..)
    , SessionTaskPlanSnapshot(..)
    ) where

import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Vector (Vector)

import Agent.Store.SessionItem (StoredResponseItem)

data SessionTaskPlanStatus
    = SessionTaskPlanPending
    | SessionTaskPlanInProgress
    | SessionTaskPlanCompleted
    deriving (Eq, Show)

data SessionTaskPlanItem = SessionTaskPlanItem
    { sessionTaskPlanItemStep :: !Text
    , sessionTaskPlanItemStatus :: !SessionTaskPlanStatus
    }
    deriving (Eq, Show)

-- | The current durable task plan for a session.
data SessionTaskPlan = SessionTaskPlan
    { sessionTaskPlanRevision :: !Int64
    , sessionTaskPlanExplanation :: !(Maybe Text)
    , sessionTaskPlanItems :: ![SessionTaskPlanItem]
    }
    deriving (Eq, Show)

-- | An unversioned task plan imported together with a session snapshot.
data SessionTaskPlanSnapshot = SessionTaskPlanSnapshot
    { sessionTaskPlanSnapshotExplanation :: !(Maybe Text)
    , sessionTaskPlanSnapshotItems :: ![SessionTaskPlanItem]
    }
    deriving (Eq, Show)

data SessionLegacyTarget = SessionLegacyTarget
    { sessionLegacyProvider :: !Text
    , sessionLegacyConnection :: !Text
    , sessionLegacyEffectiveModel :: !Text
    , sessionLegacyDialect :: !Text
    }
    deriving (Eq, Show)

data NativeConversationSearchResult = NativeConversationSearchResult
    { nativeSearchSessionId :: !Text
    , nativeSearchTitle :: !Text
    , nativeSearchCwd :: !Text
    , nativeSearchProvider :: !Text
    , nativeSearchModel :: !Text
    , nativeSearchUpdatedAt :: !UTCTime
    , nativeSearchArchived :: !Bool
    , nativeSearchTurnIndex :: !(Maybe Int64)
    , nativeSearchOccurredAt :: !(Maybe UTCTime)
    , nativeSearchRole :: !(Maybe Text)
    , nativeSearchUserText :: !(Maybe Text)
    , nativeSearchAssistantText :: !(Maybe Text)
    , nativeSearchRank :: !Double
    }
    deriving (Eq, Show)

-- | Metadata and immutable raw-history bounds observed in one repeatable-read
-- transaction. Later appends have indexes at or above start + total.
data SessionHistorySnapshot = SessionHistorySnapshot
    { sessionSnapshotMetadata :: !SessionMetadata
    , sessionSnapshotStart :: !Int64
    , sessionSnapshotTotal :: !Int64
    }
    deriving (Eq, Show)

-- | Provider-visible request prefix captured before a turn can be sent.
--
-- Tool schemas are stored as canonical JSON text by the caller. Keeping the
-- store representation opaque avoids coupling persistence to a provider's
-- evolving tool sum type while preserving schema order exactly.
data SessionPromptSnapshot = SessionPromptSnapshot
    { sessionPromptVersion :: !Int32
    , sessionPromptCreatedAt :: !UTCTime
    , sessionPromptProvider :: !Text
    , sessionPromptConnection :: !Text
    , sessionPromptModel :: !Text
    , sessionPromptDialect :: !Text
    , sessionPromptCwd :: !Text
    , sessionPromptInstructions :: !Text
    , sessionPromptTools :: !Text
    , sessionPromptGeneratedContext :: !(Maybe Text)
    , sessionPromptGrokContext :: !(Maybe Text)
    , sessionPromptCacheKey :: !Text
    }
    deriving (Eq, Show)

-- | One immutable prompt epoch. A session gets a new epoch only when its
-- provider-visible prefix or pending generated context intentionally changes.
data SessionPromptEpoch = SessionPromptEpoch
    { sessionPromptEpochIndex :: !Int64
    , sessionPromptEpochSnapshot :: !SessionPromptSnapshot
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
    , sessionMetadataGatewayIdentity :: !(Maybe Text)
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
    -- | Canonical model-context items.
    , sessionTurnItems :: ![StoredResponseItem]
    -- | Failed, uncommitted response items retained for display only.
    , sessionTurnDisplayItems :: ![StoredResponseItem]
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
    , legacyPromptSnapshot :: !(Maybe SessionPromptSnapshot)
    , legacyTaskPlan :: !(Maybe SessionTaskPlanSnapshot)
    }
    deriving (Eq, Show)
