-- | Persistent session data types and JSON codecs.
module Agent.CLI.Session.Types
    ( SessionMeta(..)
    , sessionMetaDecoder
    , SessionTransfer(..)
    , sessionTransferDecoder
    , LegacySubagentTarget(..)
    , SessionTurn(..)
    , sessionTurnDecoder
    , SessionTurnPage(..)
    , SessionResumeStats(..)
    , SessionActivity(..)
    , sessionActivityDecoder
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
import Agent.Json.Decode (optionalKey)
import qualified Agent.Json.Decode as Hermes
import Agent.Loop (TokenUsage, tokenUsageDecoder)
import Agent.OpenAI.Compaction
    ( hasCompactionCheckpoint
    , isClearSessionTurn
    , isCompactSessionTurn
    , isNewSessionTurn
    )
import Agent.OsPath (unsafeToFilePath)
import Agent.Provider (Provider, parseProvider, providerSlug)
import Agent.Responses.Types (ResponseItem)
import Agent.Responses.Types.Items (responseItemDecoder)
import Agent.Store.Postgres.Connection (StorePool)
import Agent.Store.Postgres.Session (TranscriptEffect(..))
import Control.Monad (unless, when)
import Data.Aeson (ToJSON(..), object, (.=))
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

sessionTransferDecoder :: Hermes.Decoder SessionTransfer
sessionTransferDecoder = Hermes.object $
    SessionTransfer
        <$> Hermes.atKey "meta" sessionMetaDecoder
        <*> Hermes.atKey "turns" (Hermes.list sessionTurnDecoder)

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

legacySubagentTargetDecoder :: Hermes.Decoder LegacySubagentTarget
legacySubagentTargetDecoder = Hermes.object do
        providerText <- Hermes.atKey "provider" Hermes.text
        provider <- case parseProvider providerText of
            Just parsed -> pure parsed
            Nothing ->
                fail
                    ("unknown legacy subagent provider: "
                        <> Text.unpack providerText)
        dialectText <- Hermes.atKey "dialect" Hermes.text
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
        connection <- fromMaybe (providerSlug provider)
            <$> optionalKey "connection" Hermes.text
        when (Text.null (Text.strip connection)) $
            fail "legacy subagent connection must not be empty"
        LegacySubagentTarget provider connection
            <$> Hermes.atKey "effectiveModel" Hermes.text
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

sessionMetaDecoder :: Hermes.Decoder SessionMeta
sessionMetaDecoder = Hermes.object do
        version <- Hermes.atKey "version" Hermes.int
        providerText <- Hermes.atKey "provider" Hermes.text
        provider <- case parseProvider providerText of
            Just p -> pure p
            Nothing -> fail ("unknown provider: " <> Text.unpack providerText)
        model <- Hermes.atKey "model" Hermes.text
        connection <- fromMaybe (providerSlug provider)
            <$> optionalKey "connection" Hermes.text
        when (Text.null (Text.strip connection)) $
            fail "session connection must not be empty"
        dialectText <- optionalKey "dialect" Hermes.text
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
            <$> Hermes.atKey "id" Hermes.text
            <*> Hermes.atKey "createdAt" Hermes.utcTime
            <*> Hermes.atKey "updatedAt" Hermes.utcTime
            <*> pure provider
            <*> pure connection
            <*> pure model
            <*> optionalKey "transportModel" Hermes.text
            <*> pure dialect
            <*> optionalKey "legacySubagentTarget" legacySubagentTargetDecoder
            <*> (unsafeEncodeUtf <$> Hermes.atKey "cwd" Hermes.string)
            <*> Hermes.atKey "effort" Hermes.text
            <*> Hermes.atKey "title" Hermes.text
            <*> Hermes.defaultKey False "titleIsManual" Hermes.bool
            <*> Hermes.defaultKey 2 "titleRefreshIndex" Hermes.int
            <*> Hermes.defaultKey 6 "titleUserTurns" Hermes.int
            <*> optionalKey "lastResponseId" Hermes.text
            <*> Hermes.defaultKey 0 "inputTokens" Hermes.int
            <*> Hermes.defaultKey 0 "outputTokens" Hermes.int
            <*> Hermes.defaultKey 0 "cachedTokens" Hermes.int
            <*> optionalKey "lastRecap" Hermes.text
            <*> optionalKey "lastTurnSummary" Hermes.text
            <*> Hermes.defaultKey 0 "lastRecapMainTurns" Hermes.int

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

sessionTurnDecoder :: Hermes.Decoder SessionTurn
sessionTurnDecoder = Hermes.object do
        at <- Hermes.atKey "at" Hermes.utcTime
        userText <- Hermes.atKey "userText" Hermes.text
        assistantText <- optionalKey "assistantText" Hermes.text
        turnErrorValue <- optionalKey "error" Hermes.text
        responseId <- optionalKey "responseId" Hermes.text
        items <- Hermes.atKey "items" (Hermes.list responseItemDecoder)
        usage <- optionalKey "usage" tokenUsageDecoder
        effect <- optionalKey "effect" Hermes.text >>= \case
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

sessionActivityDecoder :: Hermes.Decoder SessionActivity
sessionActivityDecoder = Hermes.object $
    SessionActivity
        <$> Hermes.atKey "kind" Hermes.text
        <*> Hermes.atKey "message" Hermes.text
        <*> optionalKey "retry_at" Hermes.utcTime
        <*> Hermes.atKey "updated_at" Hermes.utcTime


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
