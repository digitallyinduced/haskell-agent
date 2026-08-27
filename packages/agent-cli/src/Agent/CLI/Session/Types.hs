-- | Persistent session data types and JSON codecs.
module Agent.CLI.Session.Types
    ( SessionMeta(..)
    , SessionTransfer(..)
    , LegacySubagentTarget(..)
    , SessionTurn(..)
    , SessionTurnPage(..)
    , SessionResumeStats(..)
    , SessionActivity(..)
    , SessionHandle(..)
    , SessionCreate(..)
    , Persistence(..)
    , PersistenceState(..)
    , TranscriptEffect(..)
    , transcriptEffectText
    , parseTranscriptEffect
    , inferTranscriptEffect
    ) where

import Agent.CLI.Models (ModelTarget)
import Agent.Dialect
    ( DialectId
    , dialectSlug
    , legacyDialectIdForProvider
    , parseDialect
    , providerSupportsDialect
    )
import Agent.Loop (TokenUsage)
import Agent.OpenAI.Compaction
    ( hasCompactionCheckpoint
    , isClearSessionTurn
    , isCompactSessionTurn
    , isNewSessionTurn
    )
import Agent.OsPath (unsafeToFilePath)
import Agent.Provider (Provider, parseProvider, providerSlug)
import Agent.Responses.Types (ResponseItem)
import Agent.Store.Postgres.Connection (StorePool)
import Agent.Store.Postgres.Session (TranscriptEffect(..))
import Control.Monad (unless, when)
import Data.Aeson (FromJSON(..), ToJSON(..), object, withObject, (.:), (.:?), (.!=), (.=))
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Int (Int64)
import Data.IORef (IORef)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (UTCTime)
import System.OsPath (OsPath, unsafeEncodeUtf)

data SessionMeta = SessionMeta
    { metaVersion :: !Int
    , metaId :: !Text
    , metaCreatedAt :: !UTCTime
    , metaUpdatedAt :: !UTCTime
    , metaProvider :: !Provider
    , metaConnection :: !Text
    , metaModel :: !Text
    , metaTransportModel :: !(Maybe Text)
    , metaDialect :: !DialectId
    , metaLegacySubagentTarget :: !(Maybe LegacySubagentTarget)
    , metaCwd :: !OsPath
    , metaEffort :: !Text
    , metaTitle :: !Text
    , metaTitleIsManual :: !Bool
    , metaTitleRefreshIndex :: !Int
    , metaTitleUserTurns :: !Int
    , metaLastResponseId :: !(Maybe Text)
    , metaInputTokens :: !Int
    , metaOutputTokens :: !Int
    , metaCachedTokens :: !Int
    , metaLastRecap :: !(Maybe Text)
    , metaLastTurnSummary :: !(Maybe Text)
    , metaLastRecapMainTurns :: !Int
    } deriving (Eq, Show)

data SessionTransfer = SessionTransfer
    { transferMeta :: !SessionMeta
    , transferTurns :: ![SessionTurn]
    } deriving (Eq, Show)

instance ToJSON SessionTransfer where
    toJSON transfer = object
        [ "meta" .= transfer.transferMeta
        , "turns" .= transfer.transferTurns
        ]

instance FromJSON SessionTransfer where
    parseJSON = withObject "SessionTransfer" \o ->
        SessionTransfer <$> o .: "meta" <*> o .: "turns"

-- | Durable provenance for subagent transcripts written before child target
-- metadata was persisted. Keeping this target separate from the mutable root
-- target prevents a later reopen from treating stale legacy children as
-- compatible merely because the root metadata has already been retargeted.
data LegacySubagentTarget = LegacySubagentTarget
    { legacyTargetProvider :: !Provider
    , legacyTargetConnection :: !Text
    , legacyTargetEffectiveModel :: !Text
    , legacyTargetDialect :: !DialectId
    } deriving (Eq, Show)

instance ToJSON LegacySubagentTarget where
    toJSON target = object
        [ "provider" .= providerSlug target.legacyTargetProvider
        , "connection" .= target.legacyTargetConnection
        , "effectiveModel" .= target.legacyTargetEffectiveModel
        , "dialect" .= dialectSlug target.legacyTargetDialect
        ]

instance FromJSON LegacySubagentTarget where
    parseJSON = withObject "LegacySubagentTarget" \o -> do
        providerText <- o .: "provider"
        provider <- case parseProvider providerText of
            Just parsed -> pure parsed
            Nothing ->
                fail
                    ("unknown legacy subagent provider: "
                        <> Text.unpack providerText)
        dialectText <- o .: "dialect"
        dialect <- case parseDialect dialectText of
            Just parsed -> pure parsed
            Nothing ->
                fail
                    ("unknown legacy subagent dialect: "
                        <> Text.unpack dialectText)
        unless (providerSupportsDialect provider dialect) $
            fail
                ( "legacy subagent dialect "
                    <> Text.unpack (dialectSlug dialect)
                    <> " is incompatible with provider "
                    <> Text.unpack (providerSlug provider)
                )
        connection <- fromMaybe (providerSlug provider) <$> o .:? "connection"
        when (Text.null (Text.strip connection)) $
            fail "legacy subagent connection must not be empty"
        LegacySubagentTarget provider connection
            <$> o .: "effectiveModel"
            <*> pure dialect

instance ToJSON SessionMeta where
    toJSON meta = object
        [ "version" .= meta.metaVersion
        , "id" .= meta.metaId
        , "createdAt" .= meta.metaCreatedAt
        , "updatedAt" .= meta.metaUpdatedAt
        , "provider" .= providerSlug meta.metaProvider
        , "connection" .= meta.metaConnection
        , "model" .= meta.metaModel
        , "transportModel" .= meta.metaTransportModel
        , "dialect" .= dialectSlug meta.metaDialect
        , "legacySubagentTarget" .= meta.metaLegacySubagentTarget
        , "cwd" .= unsafeToFilePath meta.metaCwd
        , "effort" .= meta.metaEffort
        , "title" .= meta.metaTitle
        , "titleIsManual" .= meta.metaTitleIsManual
        , "titleRefreshIndex" .= meta.metaTitleRefreshIndex
        , "titleUserTurns" .= meta.metaTitleUserTurns
        , "lastResponseId" .= meta.metaLastResponseId
        , "inputTokens" .= meta.metaInputTokens
        , "outputTokens" .= meta.metaOutputTokens
        , "cachedTokens" .= meta.metaCachedTokens
        , "lastRecap" .= meta.metaLastRecap
        , "lastTurnSummary" .= meta.metaLastTurnSummary
        , "lastRecapMainTurns" .= meta.metaLastRecapMainTurns
        ]

instance FromJSON SessionMeta where
    parseJSON = withObject "SessionMeta" \o -> do
        version <- o .: "version"
        providerText <- o .: "provider"
        provider <- case parseProvider providerText of
            Just p -> pure p
            Nothing -> fail ("unknown provider: " <> Text.unpack providerText)
        model <- o .: "model"
        connection <- fromMaybe (providerSlug provider) <$> o .:? "connection"
        when (Text.null (Text.strip connection)) $
            fail "session connection must not be empty"
        dialectText <- o .:? "dialect"
        dialect <- case dialectText of
            Nothing -> pure (legacyDialectIdForProvider provider)
            Just text -> case parseDialect text of
                Just parsed -> pure parsed
                Nothing -> fail ("unknown dialect: " <> Text.unpack text)
        unless (providerSupportsDialect provider dialect) $
            fail
                ( "dialect "
                    <> Text.unpack (dialectSlug dialect)
                    <> " is incompatible with provider "
                    <> Text.unpack (providerSlug provider)
                )
        SessionMeta version
            <$> o .: "id"
            <*> o .: "createdAt"
            <*> o .: "updatedAt"
            <*> pure provider
            <*> pure connection
            <*> pure model
            <*> o .:? "transportModel"
            <*> pure dialect
            <*> o .:? "legacySubagentTarget"
            <*> (unsafeEncodeUtf <$> o .: "cwd")
            <*> o .: "effort"
            <*> o .: "title"
            <*> (o .:? "titleIsManual" .!= False)
            <*> (o .:? "titleRefreshIndex" .!= 2)
            <*> (o .:? "titleUserTurns" .!= 6)
            <*> o .:? "lastResponseId"
            <*> (o .:? "inputTokens" .!= 0)
            <*> (o .:? "outputTokens" .!= 0)
            <*> (o .:? "cachedTokens" .!= 0)
            <*> o .:? "lastRecap"
            <*> o .:? "lastTurnSummary"
            <*> (o .:? "lastRecapMainTurns" .!= 0)

data SessionTurn = SessionTurn
    { turnAt :: !UTCTime
    , turnUserText :: !Text
    , turnAssistantText :: !(Maybe Text)
    , turnError :: !(Maybe Text)
    , turnResponseId :: !(Maybe Text)
    , turnEffect :: !TranscriptEffect
    , turnItems :: ![ResponseItem]
    , turnUsage :: !(Maybe TokenUsage)
    } deriving (Eq, Show)

data SessionTurnPage = SessionTurnPage
    { pageTurns :: ![(Int64, SessionTurn)]
    , pageGenerationStart :: !Int64
    , pageTotalTurns :: !Int64
    , pageHasOlder :: !Bool
    , pageHasNewer :: !Bool
    } deriving (Eq, Show)

data SessionResumeStats = SessionResumeStats
    { resumeStatsTurnCount :: !Int
    , resumeStatsMessageCount :: !Int
    , resumeStatsToolCount :: !Int
    , resumeStatsFirstPrompt :: !(Maybe Text)
    } deriving (Eq, Show)

instance ToJSON SessionTurn where
    toJSON turn = object
        [ "at" .= turn.turnAt
        , "userText" .= turn.turnUserText
        , "assistantText" .= turn.turnAssistantText
        , "error" .= turn.turnError
        , "responseId" .= turn.turnResponseId
        , "effect" .= transcriptEffectText turn.turnEffect
        , "items" .= turn.turnItems
        , "usage" .= turn.turnUsage
        ]

instance FromJSON SessionTurn where
    parseJSON = withObject "SessionTurn" \o -> do
        at <- o .: "at"
        userText <- o .: "userText"
        assistantText <- o .:? "assistantText"
        turnErrorValue <- o .:? "error"
        responseId <- o .:? "responseId"
        items <- o .: "items"
        usage <- o .:? "usage"
        effect <- o .:? "effect" >>= \case
            Nothing -> pure (inferTranscriptEffect userText items)
            Just value ->
                either (fail . Text.unpack) pure
                    (parseTranscriptEffect value)
        pure SessionTurn
            { turnAt = at
            , turnUserText = userText
            , turnAssistantText = assistantText
            , turnError = turnErrorValue
            , turnResponseId = responseId
            , turnEffect = effect
            , turnItems = items
            , turnUsage = usage
            }

transcriptEffectText :: TranscriptEffect -> Text
transcriptEffectText = \case
    TranscriptAppend -> "append"
    TranscriptReplace -> "replace"
    TranscriptReset -> "reset"

parseTranscriptEffect :: Text -> Either Text TranscriptEffect
parseTranscriptEffect = \case
    "append" -> Right TranscriptAppend
    "replace" -> Right TranscriptReplace
    "reset" -> Right TranscriptReset
    value -> Left ("unknown transcript effect: " <> value)

inferTranscriptEffect :: Text -> [ResponseItem] -> TranscriptEffect
inferTranscriptEffect userText items
    | isClearSessionTurn userText || isNewSessionTurn userText =
        TranscriptReset
    | isCompactSessionTurn userText || hasCompactionCheckpoint items =
        TranscriptReplace
    | otherwise = TranscriptAppend

-- | Ephemeral progress for a running persisted session. This lives in the
-- session temp directory rather than the transcript so polling clients can
-- explain long waits without adding synthetic conversation turns.
data SessionActivity = SessionActivity
    { activityKind :: !Text
    , activityMessage :: !Text
    , activityRetryAt :: !(Maybe UTCTime)
    , activityUpdatedAt :: !UTCTime
    } deriving (Eq, Show)

instance ToJSON SessionActivity where
    toJSON activity = object
        [ "kind" .= activity.activityKind
        , "message" .= activity.activityMessage
        , "retry_at" .= activity.activityRetryAt
        , "updated_at" .= activity.activityUpdatedAt
        ]

instance FromJSON SessionActivity where
    parseJSON = withObject "SessionActivity" \o ->
        SessionActivity
            <$> o .: "kind"
            <*> o .: "message"
            <*> o .:? "retry_at"
            <*> o .: "updated_at"


data SessionHandle = SessionHandle
    { sessionPool :: !StorePool
    , sessionDir :: !OsPath
    , sessionTempDir :: !OsPath
    , sessionMetaPath :: !OsPath
    , sessionTranscriptPath :: !OsPath
    , sessionMeta :: !SessionMeta
    }

-- | Parameters for creating a session on the first persisted turn.
data SessionCreate = SessionCreate
    { createPool :: !StorePool
    , createRoot :: !OsPath
    , createTarget :: !ModelTarget
    , createCwd :: !OsPath
    , createEffort :: !Text
    , createTitleHint :: !(Maybe Text)
    , createTitleIsManual :: !Bool
    }

-- | Whether conversation state is persisted.
data Persistence
    = PersistenceDisabled
    | PersistenceEnabled (IORef PersistenceState)

-- | An enabled persistence slot, before or after its first use.
data PersistenceState
    = PersistencePending SessionCreate Text OsPath
    | PersistenceActive SessionHandle
