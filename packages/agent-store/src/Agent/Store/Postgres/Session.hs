{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Typed PostgreSQL persistence for harness sessions.
--
-- Harness-owned records and provider response items are stored in explicit
-- relational columns. Intentionally open provider leaves cross the database
-- boundary as opaque text and are interpreted only by the caller.
module Agent.Store.Postgres.Session
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
    , NativeConversationSearchResult(..)
    , sessionSchemaStatements
    , createSession
    , replaceSessionMetadata
    , setSessionTitle
    , setGeneratedSessionTitle
    , appendSessionTurn
    , appendSessionTurnIndexed
    , loadSession
    , loadSessions
    , loadSessionMetadata
    , loadActiveSession
    , SessionTurnPage(..)
    , SessionResumeStats(..)
    , loadRecentSessionTurns
    , loadRecentSessionHistoryTurns
    , loadSessionTurnsBefore
    , loadSessionHistoryTurnsBefore
    , loadSessionTurnsAfter
    , loadSessionResumeStats
    , loadSessionEvents
    , listSessionMetadata
    , listSessionArchiveKeys
    , setSessionArchived
    , searchConversationTurns
    , searchNativeConversations
    , deleteSession
    , importLegacySession
    , withSessionAdvisoryLock
    ) where

import Control.Monad (forM, forM_, unless)
import Data.ByteString (ByteString)
import Data.Functor.Contravariant ((>$<))
import Data.Int (Int32, Int64)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Session as HasqlSession
import qualified Hasql.Transaction as Transaction
import qualified Hasql.Transaction.Sessions as Transactions
import Hasql.Statement (Statement)

import Agent.Store.SessionItem (StoredResponseItem)
import Agent.Store.Postgres.Connection
    ( StorePool
    , withSession
    )
import Agent.Store.Postgres.Hasql (mkStatement)
import Agent.Store.Postgres.SessionItem
    ( insertResponseItems
    , loadResponseItems
    , sessionItemSchemaStatements
    )
import Agent.Store.Types (StoreError(..))

data SessionLegacyTarget = SessionLegacyTarget
    { sessionLegacyProvider :: !Text
    , sessionLegacyConnection :: !Text
    , sessionLegacyEffectiveModel :: !Text
    , sessionLegacyDialect :: !Text
    }
    deriving (Eq, Show)

data TitleUpdate = TitleUpdate
    { titleUpdateKey :: !Text
    , titleUpdateTitle :: !Text
    , titleUpdateManual :: !Bool
    , titleUpdateRefreshIndex :: !Int32
    , titleUpdateOccurredAt :: !UTCTime
    , titleUpdateOnlyWhenAutomatic :: !Bool
    }

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

data SessionTurnPage = SessionTurnPage
    { sessionPageTurns :: ![StoredTurn]
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
    }
    deriving (Eq, Show)

data StoredSession = StoredSession
    { storedMetadata :: !SessionMetadata
    , storedTurns :: ![StoredTurn]
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

sessionSchemaStatements :: [ByteString]
sessionSchemaStatements =
    [ "CREATE TABLE IF NOT EXISTS harness.sessions (\
      \ session_id uuid PRIMARY KEY DEFAULT pg_catalog.uuidv7(),\
      \ session_key text NOT NULL UNIQUE,\
      \ session_schema_version integer NOT NULL CHECK (session_schema_version > 0),\
      \ created_at timestamptz NOT NULL,\
      \ updated_at timestamptz NOT NULL,\
      \ provider text NOT NULL CHECK (length(btrim(provider)) > 0),\
      \ connection_id text NOT NULL CHECK (length(btrim(connection_id)) > 0),\
      \ model_id text NOT NULL CHECK (length(btrim(model_id)) > 0),\
      \ transport_model_id text,\
      \ dialect text NOT NULL CHECK (length(btrim(dialect)) > 0),\
      \ legacy_target_provider text,\
      \ legacy_target_connection text,\
      \ legacy_target_effective_model text,\
      \ legacy_target_dialect text,\
      \ cwd text NOT NULL,\
      \ effort text NOT NULL,\
      \ title text NOT NULL,\
      \ title_is_manual boolean NOT NULL,\
      \ title_refresh_index bigint NOT NULL CHECK (title_refresh_index >= 0),\
      \ title_user_turns bigint NOT NULL CHECK (title_user_turns >= 0),\
      \ last_response_id text,\
      \ input_tokens bigint NOT NULL CHECK (input_tokens >= 0),\
      \ output_tokens bigint NOT NULL CHECK (output_tokens >= 0),\
      \ cached_tokens bigint NOT NULL CHECK (cached_tokens >= 0),\
      \ last_recap text,\
      \ last_turn_summary text,\
      \ last_recap_main_turns bigint NOT NULL DEFAULT 0 CHECK (last_recap_main_turns >= 0),\
      \ next_event_sequence bigint NOT NULL DEFAULT 1,\
      \ next_turn_index bigint NOT NULL DEFAULT 0,\
      \ archived_at timestamptz,\
      \ deleted_at timestamptz,\
      \ CHECK (updated_at >= created_at),\
      \ CHECK (next_event_sequence >= 1),\
      \ CHECK (next_turn_index >= 0),\
      \ CHECK (\
      \   (legacy_target_provider IS NULL\
      \     AND legacy_target_connection IS NULL\
      \     AND legacy_target_effective_model IS NULL\
      \     AND legacy_target_dialect IS NULL)\
      \   OR\
      \   (legacy_target_provider IS NOT NULL\
      \     AND legacy_target_connection IS NOT NULL\
      \     AND legacy_target_effective_model IS NOT NULL\
      \     AND legacy_target_dialect IS NOT NULL\
      \     AND length(btrim(legacy_target_provider)) > 0\
      \     AND length(btrim(legacy_target_connection)) > 0\
      \     AND length(btrim(legacy_target_effective_model)) > 0\
      \     AND length(btrim(legacy_target_dialect)) > 0)\
      \ )\
      \ )"
    , "CREATE INDEX IF NOT EXISTS sessions_updated_at_idx\
      \ ON harness.sessions (updated_at DESC)\
      \ WHERE deleted_at IS NULL"
    , "CREATE INDEX IF NOT EXISTS sessions_archived_at_idx\
      \ ON harness.sessions (archived_at DESC)\
      \ WHERE deleted_at IS NULL AND archived_at IS NOT NULL"
    , "CREATE TABLE IF NOT EXISTS harness.session_events (\
      \ event_id uuid PRIMARY KEY DEFAULT pg_catalog.uuidv7(),\
      \ session_id uuid NOT NULL\
      \   REFERENCES harness.sessions(session_id),\
      \ sequence bigint NOT NULL,\
      \ event_kind text NOT NULL,\
      \ occurred_at timestamptz NOT NULL,\
      \ UNIQUE (session_id, sequence)\
      \ )"
    , "CREATE INDEX IF NOT EXISTS session_events_session_time_idx\
      \ ON harness.session_events (session_id, occurred_at DESC)"
    , "CREATE TABLE IF NOT EXISTS harness.session_turns (\
      \ turn_id uuid PRIMARY KEY DEFAULT pg_catalog.uuidv7(),\
      \ session_id uuid NOT NULL\
      \   REFERENCES harness.sessions(session_id),\
      \ event_id uuid NOT NULL UNIQUE\
      \   REFERENCES harness.session_events(event_id),\
      \ turn_index bigint NOT NULL,\
      \ event_sequence bigint NOT NULL,\
      \ occurred_at timestamptz NOT NULL,\
      \ user_text text NOT NULL,\
      \ assistant_text text,\
      \ error_text text,\
      \ response_id text,\
      \ transcript_effect text NOT NULL DEFAULT 'append'\
      \   CHECK (transcript_effect IN ('append', 'replace', 'reset')),\
      \ usage_input_tokens bigint,\
      \ usage_output_tokens bigint,\
      \ usage_cached_tokens bigint,\
      \ search_vector tsvector GENERATED ALWAYS AS (\
      \   setweight(to_tsvector('english', coalesce(user_text, '')), 'A') ||\
      \   setweight(to_tsvector('english', coalesce(assistant_text, '')), 'B')\
      \ ) STORED,\
      \ UNIQUE (session_id, turn_index),\
      \ UNIQUE (session_id, event_sequence),\
      \ FOREIGN KEY (session_id, event_sequence)\
      \   REFERENCES harness.session_events(session_id, sequence),\
      \ CHECK (\
      \   (usage_input_tokens IS NULL\
      \     AND usage_output_tokens IS NULL\
      \     AND usage_cached_tokens IS NULL)\
      \   OR\
      \   (usage_input_tokens >= 0\
      \     AND usage_output_tokens >= 0\
      \     AND usage_cached_tokens >= 0)\
      \ )\
      \ )"
    , "CREATE INDEX IF NOT EXISTS session_turns_search_idx\
      \ ON harness.session_turns USING gin (search_vector)"
    , "CREATE INDEX IF NOT EXISTS session_turns_session_time_idx\
      \ ON harness.session_turns (session_id, occurred_at DESC)"
    , "CREATE INDEX IF NOT EXISTS session_turns_session_index_idx\
      \ ON harness.session_turns (session_id, turn_index)"
    , "CREATE INDEX IF NOT EXISTS session_turns_checkpoint_idx\
      \ ON harness.session_turns (session_id, turn_index DESC)\
      \ WHERE transcript_effect <> 'append'"
    ]
    <> sessionItemSchemaStatements
    <> [ "CREATE TABLE IF NOT EXISTS harness.legacy_session_imports (\
       \ import_id uuid PRIMARY KEY DEFAULT pg_catalog.uuidv7(),\
       \ source_path text NOT NULL,\
       \ content_hash text NOT NULL,\
       \ session_id uuid NOT NULL REFERENCES harness.sessions(session_id),\
       \ imported_at timestamptz NOT NULL DEFAULT now(),\
       \ UNIQUE (source_path, content_hash),\
       \ UNIQUE (session_id, content_hash)\
       \ )"
       , "CREATE OR REPLACE FUNCTION harness.reject_session_fact_mutation()\
       \ RETURNS trigger\
       \ LANGUAGE plpgsql\
       \ AS $$ BEGIN\
       \ RAISE EXCEPTION 'session events and turns are immutable';\
       \ END $$"
       , "DROP TRIGGER IF EXISTS session_events_immutable\
       \ ON harness.session_events"
       , "CREATE TRIGGER session_events_immutable\
       \ BEFORE UPDATE OR DELETE ON harness.session_events\
       \ FOR EACH ROW EXECUTE FUNCTION harness.reject_session_fact_mutation()"
       , "DROP TRIGGER IF EXISTS session_turns_immutable\
       \ ON harness.session_turns"
       , "CREATE TRIGGER session_turns_immutable\
       \ BEFORE UPDATE OR DELETE ON harness.session_turns\
       \ FOR EACH ROW EXECUTE FUNCTION harness.reject_session_fact_mutation()"
       ]

createSession
    :: StorePool
    -> SessionMetadata
    -> IO (Either StoreError Bool)
createSession pool metadata =
    withSession pool $
        Transactions.transaction Transactions.Serializable Transactions.Write do
            _ <- Transaction.statement
                metadata.sessionMetadataKey
                blockingAdvisoryLockStatement
            exists <- Transaction.statement
                metadata.sessionMetadataKey
                sessionExistsStatement
            if exists
                then pure False
                else do
                    sessionId <- Transaction.statement metadata insertSessionStatement
                    _ <- Transaction.statement
                        EventInsert
                            { eventInsertSessionId = sessionId
                            , eventInsertSequence = 0
                            , eventInsertKind = "session.created"
                            , eventInsertOccurredAt =
                                metadata.sessionMetadataCreatedAt
                            }
                        insertEventStatement
                    pure True

setSessionTitle
    :: StorePool
    -> Text
    -> Text
    -> Bool
    -> Int32
    -> UTCTime
    -> IO (Either StoreError Bool)
setSessionTitle pool sessionKey title manual refreshIndex occurredAt =
    updateSessionTitle pool TitleUpdate
        { titleUpdateKey = sessionKey
        , titleUpdateTitle = title
        , titleUpdateManual = manual
        , titleUpdateRefreshIndex = refreshIndex
        , titleUpdateOccurredAt = occurredAt
        , titleUpdateOnlyWhenAutomatic = False
        }

setGeneratedSessionTitle
    :: StorePool
    -> Text
    -> Text
    -> Int32
    -> UTCTime
    -> IO (Either StoreError Bool)
setGeneratedSessionTitle pool sessionKey title refreshIndex occurredAt =
    updateSessionTitle pool TitleUpdate
        { titleUpdateKey = sessionKey
        , titleUpdateTitle = title
        , titleUpdateManual = False
        , titleUpdateRefreshIndex = refreshIndex
        , titleUpdateOccurredAt = occurredAt
        , titleUpdateOnlyWhenAutomatic = True
        }

updateSessionTitle
    :: StorePool
    -> TitleUpdate
    -> IO (Either StoreError Bool)
updateSessionTitle pool update =
    withSession pool $
        Transactions.transaction Transactions.Serializable Transactions.Write do
            _ <- Transaction.statement
                update.titleUpdateKey
                blockingAdvisoryLockStatement
            changed <- Transaction.statement update setTitleProjectionStatement
            case changed of
                Nothing -> pure False
                Just (sessionId, sequence) -> do
                    _ <- Transaction.statement
                        EventInsert
                            { eventInsertSessionId = sessionId
                            , eventInsertSequence = sequence
                            , eventInsertKind = "session.title_updated"
                            , eventInsertOccurredAt =
                                update.titleUpdateOccurredAt
                            }
                        insertEventStatement
                    pure True

replaceSessionMetadata
    :: StorePool
    -> Text
    -> SessionMetadata
    -> IO (Either StoreError Bool)
replaceSessionMetadata pool eventKind metadata =
    withSession pool $
        Transactions.transaction Transactions.Serializable Transactions.Write do
            _ <- Transaction.statement
                metadata.sessionMetadataKey
                blockingAdvisoryLockStatement
            changed <- Transaction.statement metadata replaceProjectionStatement
            case changed of
                Nothing -> pure False
                Just (sessionId, sequence) -> do
                    _ <- Transaction.statement
                        EventInsert
                            { eventInsertSessionId = sessionId
                            , eventInsertSequence = sequence
                            , eventInsertKind = eventKind
                            , eventInsertOccurredAt =
                                metadata.sessionMetadataUpdatedAt
                            }
                        insertEventStatement
                    pure True

appendSessionTurn
    :: StorePool
    -> SessionTurn
    -> SessionMetadata
    -> IO (Either StoreError Bool)
appendSessionTurn pool turn metadata =
    fmap (fmap (maybe False (const True))) $
        appendSessionTurnIndexed pool turn metadata

appendSessionTurnIndexed
    :: StorePool
    -> SessionTurn
    -> SessionMetadata
    -> IO (Either StoreError (Maybe Int64))
appendSessionTurnIndexed pool turn metadata =
    withSession pool $
        Transactions.transaction Transactions.Serializable Transactions.Write do
            _ <- Transaction.statement
                metadata.sessionMetadataKey
                blockingAdvisoryLockStatement
            changed <- Transaction.statement metadata appendProjectionStatement
            case changed of
                Nothing -> pure Nothing
                Just (sessionId, sequence, turnIndex) -> do
                    eventId <- Transaction.statement
                        EventInsert
                            { eventInsertSessionId = sessionId
                            , eventInsertSequence = sequence
                            , eventInsertKind = "turn.appended"
                            , eventInsertOccurredAt = turn.sessionTurnOccurredAt
                            }
                        insertEventStatement
                    turnId <- Transaction.statement
                        TurnInsert
                            { turnInsertSessionId = sessionId
                            , turnInsertEventId = eventId
                            , turnInsertIndex = turnIndex
                            , turnInsertEventSequence = sequence
                            , turnInsertTurn = turn
                            }
                        insertTurnStatement
                    insertResponseItems turnId turn.sessionTurnItems
                    pure (Just turnIndex)

loadSession
    :: StorePool
    -> Text
    -> IO (Either StoreError (Maybe StoredSession))
loadSession pool sessionKey =
    loadSessions pool [sessionKey] >>= \case
        [result] -> pure result
        _ -> pure (Left (StoreDataError "batched session load returned no result"))

-- | Load sessions in the same order as the requested keys.
--
-- Metadata and turn projections are fetched in two set-based statements under
-- one repeatable-read snapshot. Missing keys remain as 'Nothing', and duplicate
-- keys produce duplicate results without repeating the database reads.
loadSessions
    :: StorePool
    -> [Text]
    -> IO [Either StoreError (Maybe StoredSession)]
loadSessions _ [] = pure []
loadSessions pool sessionKeys =
    withSession pool
        (Transactions.transaction Transactions.RepeatableRead Transactions.Read do
            metadata <- Transaction.statement
                sessionKeys
                loadMetadataManyStatement
            rows <- Transaction.statement sessionKeys loadTurnsManyStatement
            turns <- forM rows \(sessionKey, row) ->
                fmap ((,) sessionKey) (loadStoredTurn row)
            pure (assembleSessions sessionKeys metadata turns))
        >>= \case
            Left err -> pure (replicate (length sessionKeys) (Left err))
            Right results ->
                pure (map (either (Left . StoreDataError) Right) results)

assembleSessions
    :: [Text]
    -> [SessionMetadata]
    -> [(Text, Either Text StoredTurn)]
    -> [Either Text (Maybe StoredSession)]
assembleSessions sessionKeys metadata turns =
    map assemble sessionKeys
  where
    metadataByKey =
        Map.fromList
            [(value.sessionMetadataKey, value) | value <- metadata]
    turnsByKey =
        Map.fromListWith (flip (++))
            [(sessionKey, [turn]) | (sessionKey, turn) <- turns]

    assemble sessionKey =
        case Map.lookup sessionKey metadataByKey of
            Nothing -> Right Nothing
            Just value -> do
                decodedTurns <-
                    sequence (Map.findWithDefault [] sessionKey turnsByKey)
                pure $ Just StoredSession
                    { storedMetadata = value
                    , storedTurns = decodedTurns
                    }

loadSessionMetadata
    :: StorePool
    -> Text
    -> IO (Either StoreError (Maybe SessionMetadata))
loadSessionMetadata pool sessionKey =
    withSession pool $
        Transactions.transaction Transactions.RepeatableRead Transactions.Read $
            Transaction.statement sessionKey loadMetadataStatement

loadActiveSession
    :: StorePool
    -> Text
    -> IO (Either StoreError (Maybe StoredSession))
loadActiveSession pool sessionKey =
    withSession pool
        (Transactions.transaction Transactions.RepeatableRead Transactions.Read do
            metadata <- Transaction.statement sessionKey loadMetadataStatement
            case metadata of
                Nothing -> pure (Right Nothing)
                Just value -> do
                    rows <- Transaction.statement sessionKey loadActiveTurnsStatement
                    turns <- forM rows loadStoredTurn
                    pure do
                        decodedTurns <- sequence turns
                        pure $ Just StoredSession
                            { storedMetadata = value
                            , storedTurns = decodedTurns
                            })
        >>= flattenDataResult

loadRecentSessionTurns
    :: StorePool
    -> Text
    -> Int
    -> IO (Either StoreError (Maybe SessionTurnPage))
loadRecentSessionTurns pool sessionKey limit =
    loadTurnPage pool sessionKey
        (Transaction.statement (sessionKey, fromIntegral (max 1 limit + 1))
            loadRecentTurnsStatement)
        (Transaction.statement sessionKey currentGenerationStatement)
        (max 1 limit)
        PageRecent

-- | Page the complete durable conversation, including turns that precede the
-- latest transcript replacement checkpoint. Native transcript views use this
-- while resume/model-context loading continues to use the active generation.
loadRecentSessionHistoryTurns
    :: StorePool
    -> Text
    -> Int
    -> IO (Either StoreError (Maybe SessionTurnPage))
loadRecentSessionHistoryTurns pool sessionKey limit =
    loadTurnPage pool sessionKey
        (Transaction.statement (sessionKey, fromIntegral (max 1 limit + 1))
            loadRecentHistoryTurnsStatement)
        (Transaction.statement sessionKey fullHistoryStatement)
        (max 1 limit)
        PageRecent

loadSessionTurnsBefore
    :: StorePool
    -> Text
    -> Int64
    -> Int
    -> IO (Either StoreError (Maybe SessionTurnPage))
loadSessionTurnsBefore pool sessionKey cursor limit =
    loadTurnPage pool sessionKey
        (Transaction.statement
            (sessionKey, cursor, fromIntegral (max 1 limit + 1))
            loadTurnsBeforeStatement)
        (Transaction.statement sessionKey currentGenerationStatement)
        (max 1 limit)
        PageBefore

loadSessionHistoryTurnsBefore
    :: StorePool
    -> Text
    -> Int64
    -> Int
    -> IO (Either StoreError (Maybe SessionTurnPage))
loadSessionHistoryTurnsBefore pool sessionKey cursor limit =
    loadTurnPage pool sessionKey
        (Transaction.statement
            (sessionKey, cursor, fromIntegral (max 1 limit + 1))
            loadHistoryTurnsBeforeStatement)
        (Transaction.statement sessionKey fullHistoryStatement)
        (max 1 limit)
        PageBefore

loadSessionTurnsAfter
    :: StorePool
    -> Text
    -> Int64
    -> Int
    -> IO (Either StoreError (Maybe SessionTurnPage))
loadSessionTurnsAfter pool sessionKey cursor limit =
    loadTurnPage pool sessionKey
        (Transaction.statement
            (sessionKey, cursor, fromIntegral (max 1 limit + 1))
            loadTurnsAfterStatement)
        (Transaction.statement sessionKey currentGenerationStatement)
        (max 1 limit)
        PageAfter

loadSessionResumeStats
    :: StorePool
    -> Text
    -> IO (Either StoreError (Maybe SessionResumeStats))
loadSessionResumeStats pool sessionKey =
    withSession pool $
        Transactions.transaction Transactions.RepeatableRead Transactions.Read $
            Transaction.statement sessionKey loadResumeStatsStatement

data PageMode = PageRecent | PageBefore | PageAfter
    deriving (Eq, Show)

loadTurnPage
    :: StorePool
    -> Text
    -> Transaction.Transaction [TurnRow]
    -> Transaction.Transaction (Int64, Int64)
    -> Int
    -> PageMode
    -> IO (Either StoreError (Maybe SessionTurnPage))
loadTurnPage pool sessionKey loadRows loadBounds limit mode =
    withSession pool
        (Transactions.transaction Transactions.RepeatableRead Transactions.Read do
            metadata <- Transaction.statement sessionKey loadMetadataStatement
            case metadata of
                Nothing -> pure (Right Nothing)
                Just _ -> do
                    rows0 <- loadRows
                    (generationStart, total) <- loadBounds
                    let visibleRows = case mode of
                            PageRecent -> reverse (take limit rows0)
                            PageBefore -> reverse (take limit rows0)
                            PageAfter -> take limit rows0
                    turns <- forM visibleRows loadStoredTurn
                    pure do
                        decoded <- sequence turns
                        let generationEnd =
                                generationStart + max 0 total - 1
                            (hasOlder, hasNewer) =
                                case decoded of
                                    firstTurn : rest ->
                                        let lastTurn =
                                                foldl
                                                    (\_ current -> current)
                                                    firstTurn
                                                    rest
                                        in
                                            ( firstTurn.storedTurnIndex
                                                > generationStart
                                            , lastTurn.storedTurnIndex
                                                < generationEnd
                                            )
                                    [] -> case mode of
                                        PageRecent -> (False, False)
                                        PageBefore -> (False, total > 0)
                                        PageAfter -> (total > 0, False)
                        pure $ Just SessionTurnPage
                            { sessionPageTurns = decoded
                            , sessionPageGenerationStart = generationStart
                            , sessionPageTotal = total
                            , sessionPageHasOlder = hasOlder
                            , sessionPageHasNewer = hasNewer
                            })
        >>= flattenDataResult

loadSessionEvents
    :: StorePool
    -> Text
    -> IO (Either StoreError [StoredEvent])
loadSessionEvents pool sessionKey =
    withSession pool $
        Transactions.transaction Transactions.RepeatableRead Transactions.Read $
            Transaction.statement sessionKey loadEventsStatement

listSessionMetadata
    :: StorePool
    -> IO (Either StoreError [SessionMetadata])
listSessionMetadata pool =
    withSession pool $
        Transactions.transaction Transactions.RepeatableRead Transactions.Read $
            Transaction.statement () listMetadataStatement

listSessionArchiveKeys
    :: StorePool
    -> IO (Either StoreError [Text])
listSessionArchiveKeys pool =
    withSession pool $
        Transactions.transaction Transactions.RepeatableRead Transactions.Read $
            Transaction.statement () listArchiveKeysStatement

setSessionArchived
    :: StorePool
    -> Text
    -> Bool
    -> UTCTime
    -> IO (Either StoreError Bool)
setSessionArchived pool sessionKey archived occurredAt =
    withSession pool $
        Transactions.transaction Transactions.Serializable Transactions.Write do
            _ <- Transaction.statement sessionKey blockingAdvisoryLockStatement
            changed <- Transaction.statement
                (sessionKey, archived, occurredAt)
                setArchivedProjectionStatement
            case changed of
                Nothing -> pure False
                Just (sessionId, sequence) -> do
                    _ <- Transaction.statement
                        EventInsert
                            { eventInsertSessionId = sessionId
                            , eventInsertSequence = sequence
                            , eventInsertKind =
                                if archived
                                    then "session.archived"
                                    else "session.restored"
                            , eventInsertOccurredAt = occurredAt
                            }
                        insertEventStatement
                    pure True

searchConversationTurns
    :: StorePool
    -> Text
    -> Int
    -> IO (Either StoreError [ConversationSearchResult])
searchConversationTurns pool query limit =
    withSession pool $
        HasqlSession.statement
            (query, fromIntegral (max 1 (min 100 limit)))
            searchTurnsStatement

searchNativeConversations
    :: StorePool -> Text -> Int
    -> IO (Either StoreError [NativeConversationSearchResult])
searchNativeConversations pool query limit =
    withSession pool $
        HasqlSession.statement
            (query, fromIntegral (max 1 (min 100 limit)))
            searchNativeConversationsStatement

deleteSession
    :: StorePool
    -> Text
    -> UTCTime
    -> IO (Either StoreError Bool)
deleteSession pool sessionKey occurredAt =
    withSession pool $
        Transactions.transaction Transactions.Serializable Transactions.Write do
            _ <- Transaction.statement sessionKey blockingAdvisoryLockStatement
            changed <- Transaction.statement
                (sessionKey, occurredAt)
                deleteProjectionStatement
            case changed of
                Nothing -> pure False
                Just (sessionId, sequence) -> do
                    _ <- Transaction.statement
                        EventInsert
                            { eventInsertSessionId = sessionId
                            , eventInsertSequence = sequence
                            , eventInsertKind = "session.deleted"
                            , eventInsertOccurredAt = occurredAt
                            }
                        insertEventStatement
                    pure True

importLegacySession
    :: StorePool
    -> LegacySession
    -> IO (Either StoreError Bool)
importLegacySession pool legacy =
    withSession pool $
        Transactions.transaction Transactions.Serializable Transactions.Write do
            let metadata = legacy.legacyMetadata
                sessionKey = metadata.sessionMetadataKey
            _ <- Transaction.statement sessionKey blockingAdvisoryLockStatement
            exists <- Transaction.statement sessionKey sessionExistsStatement
            if exists
                then pure False
                else do
                    sessionId <- Transaction.statement metadata insertSessionStatement
                    _ <- Transaction.statement
                        EventInsert
                            { eventInsertSessionId = sessionId
                            , eventInsertSequence = 0
                            , eventInsertKind = "session.created"
                            , eventInsertOccurredAt =
                                metadata.sessionMetadataCreatedAt
                            }
                        insertEventStatement
                    forM_ legacy.legacyTurns \turn -> do
                        appended <- appendTurnTransaction turn metadata
                        unless appended Transaction.condemn
                    changed <- Transaction.statement metadata replaceProjectionStatement
                    case changed of
                        Nothing -> Transaction.condemn
                        Just (internalId, sequence) -> do
                            _ <- Transaction.statement
                                EventInsert
                                    { eventInsertSessionId = internalId
                                    , eventInsertSequence = sequence
                                    , eventInsertKind = "legacy.import_completed"
                                    , eventInsertOccurredAt =
                                        metadata.sessionMetadataUpdatedAt
                                    }
                                insertEventStatement
                            pure ()
                    Transaction.statement
                        ( legacy.legacySourcePath
                        , legacy.legacyContentHash
                        , sessionId
                        )
                        recordLegacyImportStatement

withSessionAdvisoryLock
    :: StorePool
    -> Text
    -> Transaction.Transaction a
    -> IO (Either StoreError (Maybe a))
withSessionAdvisoryLock pool sessionId action =
    withSession pool $
        Transactions.transaction Transactions.Serializable Transactions.Write do
            acquired <- Transaction.statement sessionId tryAdvisoryLockStatement
            if acquired then Just <$> action else pure Nothing

appendTurnTransaction
    :: SessionTurn
    -> SessionMetadata
    -> Transaction.Transaction Bool
appendTurnTransaction turn metadata = do
    changed <- Transaction.statement metadata appendProjectionStatement
    case changed of
        Nothing -> pure False
        Just (sessionId, sequence, turnIndex) -> do
            eventId <- Transaction.statement
                EventInsert
                    { eventInsertSessionId = sessionId
                    , eventInsertSequence = sequence
                    , eventInsertKind = "turn.appended"
                    , eventInsertOccurredAt = turn.sessionTurnOccurredAt
                    }
                insertEventStatement
            turnId <- Transaction.statement
                TurnInsert
                    { turnInsertSessionId = sessionId
                    , turnInsertEventId = eventId
                    , turnInsertIndex = turnIndex
                    , turnInsertEventSequence = sequence
                    , turnInsertTurn = turn
                    }
                insertTurnStatement
            insertResponseItems turnId turn.sessionTurnItems
            pure True

data EventInsert = EventInsert
    { eventInsertSessionId :: !Text
    , eventInsertSequence :: !Int64
    , eventInsertKind :: !Text
    , eventInsertOccurredAt :: !UTCTime
    }

data TurnInsert = TurnInsert
    { turnInsertSessionId :: !Text
    , turnInsertEventId :: !Text
    , turnInsertIndex :: !Int64
    , turnInsertEventSequence :: !Int64
    , turnInsertTurn :: !SessionTurn
    }

data TurnRow = TurnRow
    { turnRowId :: !Text
    , turnRowIndex :: !Int64
    , turnRowEventSequence :: !Int64
    , turnRowOccurredAt :: !UTCTime
    , turnRowUserText :: !Text
    , turnRowAssistantText :: !(Maybe Text)
    , turnRowError :: !(Maybe Text)
    , turnRowResponseId :: !(Maybe Text)
    , turnRowEffect :: !Text
    , turnRowUsageInput :: !(Maybe Int64)
    , turnRowUsageOutput :: !(Maybe Int64)
    , turnRowUsageCached :: !(Maybe Int64)
    }

loadStoredTurn :: TurnRow -> Transaction.Transaction (Either Text StoredTurn)
loadStoredTurn row = do
    items <- loadResponseItems row.turnRowId
    pure do
        decodedItems <- items
        effect <- decodeTranscriptEffect row.turnRowEffect
        usage <- decodeUsage
            row.turnRowUsageInput
            row.turnRowUsageOutput
            row.turnRowUsageCached
        pure StoredTurn
            { storedTurnIndex = row.turnRowIndex
            , storedEventSequence = row.turnRowEventSequence
            , storedTurn = SessionTurn
                { sessionTurnOccurredAt = row.turnRowOccurredAt
                , sessionTurnUserText = row.turnRowUserText
                , sessionTurnAssistantText = row.turnRowAssistantText
                , sessionTurnError = row.turnRowError
                , sessionTurnResponseId = row.turnRowResponseId
                , sessionTurnEffect = effect
                , sessionTurnItems = decodedItems
                , sessionTurnUsage = usage
                }
            }

decodeUsage
    :: Maybe Int64
    -> Maybe Int64
    -> Maybe Int64
    -> Either Text (Maybe SessionUsage)
decodeUsage input output cached =
    case (input, output, cached) of
        (Nothing, Nothing, Nothing) -> Right Nothing
        (Just input', Just output', Just cached') ->
            Right $ Just SessionUsage
                { sessionUsageInputTokens = input'
                , sessionUsageOutputTokens = output'
                , sessionUsageCachedTokens = cached'
                }
        _ -> Left "stored session turn has partial usage counters"

decodeTranscriptEffect :: Text -> Either Text TranscriptEffect
decodeTranscriptEffect value = case value of
    "append" -> Right TranscriptAppend
    "replace" -> Right TranscriptReplace
    "reset" -> Right TranscriptReset
    _ -> Left ("unknown transcript effect: " <> value)

transcriptEffectText :: TranscriptEffect -> Text
transcriptEffectText effect = case effect of
    TranscriptAppend -> "append"
    TranscriptReplace -> "replace"
    TranscriptReset -> "reset"

flattenDataResult
    :: Either StoreError (Either Text a)
    -> IO (Either StoreError a)
flattenDataResult = pure . \case
    Left err -> Left err
    Right (Left err) -> Left (StoreDataError err)
    Right (Right value) -> Right value

sessionExistsStatement :: Statement Text Bool
sessionExistsStatement = mkStatement
    "SELECT EXISTS (SELECT 1 FROM harness.sessions WHERE session_key = $1)"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
    True

listArchiveKeysStatement :: Statement () [Text]
listArchiveKeysStatement = mkStatement
    "SELECT session_key FROM harness.sessions\
    \ WHERE deleted_at IS NULL AND archived_at IS NOT NULL\
    \ ORDER BY archived_at DESC, session_key ASC"
    Encoders.noParams
    (Decoders.rowList $
        Decoders.column (Decoders.nonNullable Decoders.text))
    True

setArchivedProjectionStatement
    :: Statement (Text, Bool, UTCTime) (Maybe (Text, Int64))
setArchivedProjectionStatement = mkStatement
    "UPDATE harness.sessions SET\
    \ archived_at = CASE WHEN $2 THEN $3 ELSE NULL END,\
    \ next_event_sequence = next_event_sequence + 1\
    \ WHERE session_key = $1 AND deleted_at IS NULL\
    \ RETURNING session_id::text, next_event_sequence - 1"
    ( ((\(key, _, _) -> key)
        >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((\(_, archived, _) -> archived)
            >$< Encoders.param (Encoders.nonNullable Encoders.bool))
        <> ((\(_, _, occurredAt) -> occurredAt)
            >$< Encoders.param (Encoders.nonNullable Encoders.timestamptz))
    )
    (Decoders.rowMaybe $
        (,)
            <$> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.int8))
    True

insertSessionStatement :: Statement SessionMetadata Text
insertSessionStatement = mkStatement
    "INSERT INTO harness.sessions (\
    \ session_key, session_schema_version, created_at, updated_at,\
    \ provider, connection_id, model_id, transport_model_id, dialect,\
    \ legacy_target_provider, legacy_target_connection,\
    \ legacy_target_effective_model, legacy_target_dialect,\
    \ cwd, effort, title, title_is_manual, title_refresh_index,\
    \ title_user_turns, last_response_id, input_tokens, output_tokens,\
    \ cached_tokens, last_recap, last_turn_summary, last_recap_main_turns\
    \ ) VALUES (\
    \ $1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13,\
    \ $14, $15, $16, $17, $18, $19, $20, $21, $22, $23, $24, $25, $26\
    \ ) RETURNING session_id::text"
    metadataParams
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.text)))
    True

replaceProjectionStatement
    :: Statement SessionMetadata (Maybe (Text, Int64))
replaceProjectionStatement = mkStatement
    (metadataUpdateSql
        <> ", next_event_sequence = next_event_sequence + 1\
           \ WHERE session_key = $1 AND deleted_at IS NULL\
           \ RETURNING session_id::text, next_event_sequence - 1")
    metadataParams
    (Decoders.rowMaybe $
        (,)
            <$> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.int8))
    True

appendProjectionStatement
    :: Statement SessionMetadata (Maybe (Text, Int64, Int64))
appendProjectionStatement = mkStatement
    (metadataUpdateSql
        <> ", next_event_sequence = next_event_sequence + 1\
           \, next_turn_index = next_turn_index + 1\
           \ WHERE session_key = $1 AND deleted_at IS NULL\
           \ RETURNING session_id::text, next_event_sequence - 1,\
           \ next_turn_index - 1")
    metadataParams
    (Decoders.rowMaybe $
        (,,)
            <$> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.int8)
            <*> Decoders.column (Decoders.nonNullable Decoders.int8))
    True

metadataUpdateSql :: Text
metadataUpdateSql =
    "UPDATE harness.sessions SET\
    \ session_schema_version = $2, created_at = $3, updated_at = $4,\
    \ provider = $5, connection_id = $6, model_id = $7,\
    \ transport_model_id = $8, dialect = $9,\
    \ legacy_target_provider = $10, legacy_target_connection = $11,\
    \ legacy_target_effective_model = $12, legacy_target_dialect = $13,\
    \ cwd = $14, effort = $15,\
    \ title = CASE WHEN title_is_manual THEN title ELSE $16 END,\
    \ title_is_manual = CASE\
    \   WHEN title_is_manual THEN title_is_manual ELSE $17 END,\
    \ title_refresh_index = CASE\
    \   WHEN title_is_manual THEN title_refresh_index ELSE $18 END,\
    \ title_user_turns = $19,\
    \ last_response_id = $20, input_tokens = $21, output_tokens = $22,\
    \ cached_tokens = $23, last_recap = $24, last_turn_summary = $25,\
    \ last_recap_main_turns = $26"

setTitleProjectionStatement
    :: Statement TitleUpdate (Maybe (Text, Int64))
setTitleProjectionStatement = mkStatement
    "UPDATE harness.sessions SET\
    \ title = $2, title_is_manual = $3, title_refresh_index = $4,\
    \ updated_at = $5, next_event_sequence = next_event_sequence + 1\
    \ WHERE session_key = $1 AND deleted_at IS NULL\
    \ AND (NOT $6 OR NOT title_is_manual)\
    \ RETURNING session_id::text, next_event_sequence - 1"
    ( ((.titleUpdateKey)
        >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.titleUpdateTitle)
            >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.titleUpdateManual)
            >$< Encoders.param (Encoders.nonNullable Encoders.bool))
        <> ((.titleUpdateRefreshIndex)
            >$< Encoders.param (Encoders.nonNullable Encoders.int4))
        <> ((.titleUpdateOccurredAt)
            >$< Encoders.param (Encoders.nonNullable Encoders.timestamptz))
        <> ((.titleUpdateOnlyWhenAutomatic)
            >$< Encoders.param (Encoders.nonNullable Encoders.bool))
    )
    (Decoders.rowMaybe $
        (,)
            <$> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.int8))
    True

insertEventStatement :: Statement EventInsert Text
insertEventStatement = mkStatement
    "INSERT INTO harness.session_events\
    \ (session_id, sequence, event_kind, occurred_at)\
    \ VALUES ($1::uuid, $2, $3, $4)\
    \ RETURNING event_id::text"
    ( ((.eventInsertSessionId) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.eventInsertSequence) >$< Encoders.param (Encoders.nonNullable Encoders.int8))
        <> ((.eventInsertKind) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.eventInsertOccurredAt) >$< Encoders.param (Encoders.nonNullable Encoders.timestamptz))
    )
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.text)))
    True

insertTurnStatement :: Statement TurnInsert Text
insertTurnStatement = mkStatement
    "INSERT INTO harness.session_turns (\
    \ session_id, event_id, turn_index, event_sequence, occurred_at,\
    \ user_text, assistant_text, error_text, response_id,\
    \ transcript_effect,\
    \ usage_input_tokens, usage_output_tokens, usage_cached_tokens\
    \ ) VALUES (\
    \ $1::uuid, $2::uuid, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13\
    \ ) RETURNING turn_id::text"
    ( ((.turnInsertSessionId) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.turnInsertEventId) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((.turnInsertIndex) >$< Encoders.param (Encoders.nonNullable Encoders.int8))
        <> ((.turnInsertEventSequence) >$< Encoders.param (Encoders.nonNullable Encoders.int8))
        <> (turnOccurredAt >$< Encoders.param (Encoders.nonNullable Encoders.timestamptz))
        <> (turnUserText >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> (turnAssistantText >$< Encoders.param (Encoders.nullable Encoders.text))
        <> (turnError >$< Encoders.param (Encoders.nullable Encoders.text))
        <> (turnResponseId >$< Encoders.param (Encoders.nullable Encoders.text))
        <> (turnEffect >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((usageField (.sessionUsageInputTokens) . (.turnInsertTurn))
            >$< Encoders.param (Encoders.nullable Encoders.int8))
        <> ((usageField (.sessionUsageOutputTokens) . (.turnInsertTurn))
            >$< Encoders.param (Encoders.nullable Encoders.int8))
        <> ((usageField (.sessionUsageCachedTokens) . (.turnInsertTurn))
            >$< Encoders.param (Encoders.nullable Encoders.int8))
    )
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.text)))
    True
  where
    usageField
        :: (SessionUsage -> Int64)
        -> SessionTurn
        -> Maybe Int64
    usageField field turn = field <$> turn.sessionTurnUsage
    turnOccurredAt :: TurnInsert -> UTCTime
    turnOccurredAt value = value.turnInsertTurn.sessionTurnOccurredAt
    turnUserText :: TurnInsert -> Text
    turnUserText value = value.turnInsertTurn.sessionTurnUserText
    turnAssistantText :: TurnInsert -> Maybe Text
    turnAssistantText value = value.turnInsertTurn.sessionTurnAssistantText
    turnError :: TurnInsert -> Maybe Text
    turnError value = value.turnInsertTurn.sessionTurnError
    turnResponseId :: TurnInsert -> Maybe Text
    turnResponseId value = value.turnInsertTurn.sessionTurnResponseId
    turnEffect :: TurnInsert -> Text
    turnEffect value = transcriptEffectText value.turnInsertTurn.sessionTurnEffect

loadMetadataManyStatement :: Statement [Text] [SessionMetadata]
loadMetadataManyStatement = mkStatement
    (metadataSelectSql
        <> " WHERE session_key = ANY($1::text[]) AND deleted_at IS NULL\
           \ ORDER BY array_position($1::text[], session_key)")
    textArrayParams
    (Decoders.rowList metadataRow)
    True

loadMetadataStatement :: Statement Text (Maybe SessionMetadata)
loadMetadataStatement = mkStatement
    (metadataSelectSql
        <> " WHERE session_key = $1 AND deleted_at IS NULL")
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.rowMaybe metadataRow)
    True

listMetadataStatement :: Statement () [SessionMetadata]
listMetadataStatement = mkStatement
    (metadataSelectSql
        <> " WHERE deleted_at IS NULL\
           \ ORDER BY updated_at DESC, session_key ASC")
    Encoders.noParams
    (Decoders.rowList metadataRow)
    True

metadataSelectSql :: Text
metadataSelectSql =
    "SELECT session_key, session_schema_version, created_at, updated_at,\
    \ provider, connection_id, model_id, transport_model_id, dialect,\
    \ legacy_target_provider, legacy_target_connection,\
    \ legacy_target_effective_model, legacy_target_dialect,\
    \ cwd, effort, title, title_is_manual, title_refresh_index,\
    \ title_user_turns, last_response_id, input_tokens, output_tokens,\
    \ cached_tokens, last_recap, last_turn_summary, last_recap_main_turns\
    \ FROM harness.sessions"

loadTurnsManyStatement :: Statement [Text] [(Text, TurnRow)]
loadTurnsManyStatement = mkStatement
    "SELECT s.session_key, t.turn_id::text, t.turn_index, t.event_sequence,\
    \ t.occurred_at, t.user_text, t.assistant_text, t.error_text,\
    \ t.response_id, t.transcript_effect, t.usage_input_tokens,\
    \ t.usage_output_tokens, t.usage_cached_tokens\
    \ FROM harness.session_turns t\
    \ JOIN harness.sessions s ON s.session_id = t.session_id\
    \ WHERE s.session_key = ANY($1::text[]) AND s.deleted_at IS NULL\
    \ ORDER BY array_position($1::text[], s.session_key), t.turn_index ASC"
    textArrayParams
    (Decoders.rowList $
        (,)
            <$> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> turnRowDecoder)
    True

loadActiveTurnsStatement :: Statement Text [TurnRow]
loadActiveTurnsStatement = mkStatement
    (turnSelectSql
        <> " WHERE s.session_key = $1\
           \ AND t.turn_index >= COALESCE((\
           \   SELECT checkpoint.turn_index\
           \   FROM harness.session_turns checkpoint\
           \   WHERE checkpoint.session_id = s.session_id\
           \     AND checkpoint.transcript_effect <> 'append'\
           \   ORDER BY checkpoint.turn_index DESC\
           \   LIMIT 1\
           \ ), 0)\
           \ ORDER BY t.turn_index ASC")
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.rowList turnRowDecoder)
    True

loadRecentTurnsStatement :: Statement (Text, Int64) [TurnRow]
loadRecentTurnsStatement = mkStatement
    (turnSelectSql
        <> " WHERE s.session_key = $1\
           \ AND t.turn_index >= COALESCE((\
           \   SELECT checkpoint.turn_index\
           \   FROM harness.session_turns checkpoint\
           \   WHERE checkpoint.session_id = s.session_id\
           \     AND checkpoint.transcript_effect <> 'append'\
           \   ORDER BY checkpoint.turn_index DESC\
           \   LIMIT 1\
           \ ), 0)\
           \ ORDER BY t.turn_index DESC\
           \ LIMIT $2")
    ( (fst >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> (snd >$< Encoders.param (Encoders.nonNullable Encoders.int8))
    )
    (Decoders.rowList turnRowDecoder)
    True

loadRecentHistoryTurnsStatement :: Statement (Text, Int64) [TurnRow]
loadRecentHistoryTurnsStatement = mkStatement
    ( turnSelectSql
        <> " WHERE s.session_key = $1"
        <> " ORDER BY t.turn_index DESC"
        <> " LIMIT $2"
    )
    ( (fst >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> (snd >$< Encoders.param (Encoders.nonNullable Encoders.int8))
    )
    (Decoders.rowList turnRowDecoder)
    True

loadTurnsBeforeStatement :: Statement (Text, Int64, Int64) [TurnRow]
loadTurnsBeforeStatement = mkStatement
    (turnSelectSql
        <> " WHERE s.session_key = $1\
           \ AND t.turn_index < $2\
           \ AND t.turn_index >= COALESCE((\
           \   SELECT checkpoint.turn_index\
           \   FROM harness.session_turns checkpoint\
           \   WHERE checkpoint.session_id = s.session_id\
           \     AND checkpoint.transcript_effect <> 'append'\
           \   ORDER BY checkpoint.turn_index DESC\
           \   LIMIT 1\
           \ ), 0)\
           \ ORDER BY t.turn_index DESC\
           \ LIMIT $3")
    ( ((\(value, _, _) -> value)
        >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((\(_, value, _) -> value)
            >$< Encoders.param (Encoders.nonNullable Encoders.int8))
        <> ((\(_, _, value) -> value)
            >$< Encoders.param (Encoders.nonNullable Encoders.int8))
    )
    (Decoders.rowList turnRowDecoder)
    True

loadHistoryTurnsBeforeStatement :: Statement (Text, Int64, Int64) [TurnRow]
loadHistoryTurnsBeforeStatement = mkStatement
    ( turnSelectSql
        <> " WHERE s.session_key = $1"
        <> " AND t.turn_index < $2"
        <> " ORDER BY t.turn_index DESC"
        <> " LIMIT $3"
    )
    ( ((\(value, _, _) -> value)
        >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((\(_, value, _) -> value)
            >$< Encoders.param (Encoders.nonNullable Encoders.int8))
        <> ((\(_, _, value) -> value)
            >$< Encoders.param (Encoders.nonNullable Encoders.int8))
    )
    (Decoders.rowList turnRowDecoder)
    True

loadTurnsAfterStatement :: Statement (Text, Int64, Int64) [TurnRow]
loadTurnsAfterStatement = mkStatement
    (turnSelectSql
        <> " WHERE s.session_key = $1\
           \ AND t.turn_index > $2\
           \ AND t.turn_index >= COALESCE((\
           \   SELECT checkpoint.turn_index\
           \   FROM harness.session_turns checkpoint\
           \   WHERE checkpoint.session_id = s.session_id\
           \     AND checkpoint.transcript_effect <> 'append'\
           \   ORDER BY checkpoint.turn_index DESC\
           \   LIMIT 1\
           \ ), 0)\
           \ ORDER BY t.turn_index ASC\
           \ LIMIT $3")
    ( ((\(value, _, _) -> value)
        >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((\(_, value, _) -> value)
            >$< Encoders.param (Encoders.nonNullable Encoders.int8))
        <> ((\(_, _, value) -> value)
            >$< Encoders.param (Encoders.nonNullable Encoders.int8))
    )
    (Decoders.rowList turnRowDecoder)
    True

fullHistoryStatement :: Statement Text (Int64, Int64)
fullHistoryStatement = mkStatement
    ( "WITH target AS ("
        <> " SELECT session_id"
        <> " FROM harness.sessions"
        <> " WHERE session_key = $1 AND deleted_at IS NULL"
        <> " )"
        <> " SELECT COALESCE(min(t.turn_index), 0)::bigint,"
        <> " count(t.turn_id)::bigint"
        <> " FROM target"
        <> " LEFT JOIN harness.session_turns t"
        <> " ON t.session_id = target.session_id"
    )
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.singleRow $
        (,)
            <$> Decoders.column (Decoders.nonNullable Decoders.int8)
            <*> Decoders.column (Decoders.nonNullable Decoders.int8))
    True

currentGenerationStatement :: Statement Text (Int64, Int64)
currentGenerationStatement = mkStatement
    "WITH target AS (\
    \ SELECT session_id\
    \ FROM harness.sessions\
    \ WHERE session_key = $1 AND deleted_at IS NULL\
    \ ), generation AS (\
    \ SELECT COALESCE((\
    \   SELECT t.turn_index\
    \   FROM harness.session_turns t\
    \   JOIN target ON target.session_id = t.session_id\
    \   WHERE t.transcript_effect <> 'append'\
    \   ORDER BY t.turn_index DESC\
    \   LIMIT 1\
    \ ), 0) AS start_index\
    \ )\
    \ SELECT generation.start_index, count(t.turn_id)::bigint\
    \ FROM generation\
    \ LEFT JOIN target ON true\
    \ LEFT JOIN harness.session_turns t\
    \   ON t.session_id = target.session_id\
    \   AND t.turn_index >= generation.start_index\
    \ GROUP BY generation.start_index"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.singleRow $
        (,)
            <$> Decoders.column (Decoders.nonNullable Decoders.int8)
            <*> Decoders.column (Decoders.nonNullable Decoders.int8))
    True

loadResumeStatsStatement :: Statement Text (Maybe SessionResumeStats)
loadResumeStatsStatement = mkStatement
    "WITH target AS (\
    \ SELECT session_id\
    \ FROM harness.sessions\
    \ WHERE session_key = $1 AND deleted_at IS NULL\
    \ )\
    \ SELECT\
    \   (SELECT count(*)::bigint\
    \    FROM harness.session_turns t\
    \    JOIN target ON target.session_id = t.session_id),\
    \   (SELECT COALESCE(sum(\
    \      (CASE WHEN btrim(t.user_text) <> '' THEN 1 ELSE 0 END)\
    \      + (CASE WHEN t.assistant_text IS NOT NULL\
    \               AND btrim(t.assistant_text) <> '' THEN 1 ELSE 0 END)\
    \    ), 0)::bigint\
    \    FROM harness.session_turns t\
    \    JOIN target ON target.session_id = t.session_id),\
    \   (SELECT count(*)::bigint\
    \    FROM harness.session_response_items i\
    \    JOIN harness.session_turns t ON t.turn_id = i.turn_id\
    \    JOIN target ON target.session_id = t.session_id\
    \    WHERE i.storage_kind IN ('function_call', 'custom_tool_call')),\
    \   (SELECT t.user_text\
    \    FROM harness.session_turns t\
    \    JOIN target ON target.session_id = t.session_id\
    \    WHERE btrim(t.user_text) <> ''\
    \    ORDER BY t.turn_index ASC\
    \    LIMIT 1)\
    \ FROM target"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.rowMaybe $
        SessionResumeStats
            <$> Decoders.column (Decoders.nonNullable Decoders.int8)
            <*> Decoders.column (Decoders.nonNullable Decoders.int8)
            <*> Decoders.column (Decoders.nonNullable Decoders.int8)
            <*> Decoders.column (Decoders.nullable Decoders.text))
    True

turnSelectSql :: Text
turnSelectSql =
    "SELECT t.turn_id::text, t.turn_index, t.event_sequence, t.occurred_at,\
    \ t.user_text, t.assistant_text, t.error_text, t.response_id,\
    \ t.transcript_effect,\
    \ t.usage_input_tokens, t.usage_output_tokens, t.usage_cached_tokens\
    \ FROM harness.session_turns t\
    \ JOIN harness.sessions s ON s.session_id = t.session_id"

turnRowDecoder :: Decoders.Row TurnRow
turnRowDecoder =
    TurnRow
        <$> Decoders.column (Decoders.nonNullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.int8)
        <*> Decoders.column (Decoders.nonNullable Decoders.int8)
        <*> Decoders.column (Decoders.nonNullable Decoders.timestamptz)
        <*> Decoders.column (Decoders.nonNullable Decoders.text)
        <*> Decoders.column (Decoders.nullable Decoders.text)
        <*> Decoders.column (Decoders.nullable Decoders.text)
        <*> Decoders.column (Decoders.nullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.text)
        <*> Decoders.column (Decoders.nullable Decoders.int8)
        <*> Decoders.column (Decoders.nullable Decoders.int8)
        <*> Decoders.column (Decoders.nullable Decoders.int8)

textArrayParams :: Encoders.Params [Text]
textArrayParams =
    Encoders.param $
        Encoders.nonNullable $
            Encoders.foldableArray (Encoders.nonNullable Encoders.text)

loadEventsStatement :: Statement Text [StoredEvent]
loadEventsStatement = mkStatement
    "SELECT e.sequence, e.event_kind, e.occurred_at\
    \ FROM harness.session_events e\
    \ JOIN harness.sessions s ON s.session_id = e.session_id\
    \ WHERE s.session_key = $1\
    \ ORDER BY e.sequence ASC"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.rowList $
        StoredEvent
            <$> Decoders.column (Decoders.nonNullable Decoders.int8)
            <*> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.timestamptz))
    True

deleteProjectionStatement
    :: Statement (Text, UTCTime) (Maybe (Text, Int64))
deleteProjectionStatement = mkStatement
    "UPDATE harness.sessions\
    \ SET updated_at = $2, deleted_at = $2,\
    \     next_event_sequence = next_event_sequence + 1\
    \ WHERE session_key = $1 AND deleted_at IS NULL\
    \ RETURNING session_id::text, next_event_sequence - 1"
    ( (fst >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> (snd >$< Encoders.param (Encoders.nonNullable Encoders.timestamptz))
    )
    (Decoders.rowMaybe $
        (,)
            <$> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.int8))
    True

recordLegacyImportStatement :: Statement (Text, Text, Text) Bool
recordLegacyImportStatement = mkStatement
    "INSERT INTO harness.legacy_session_imports\
    \ (source_path, content_hash, session_id)\
    \ VALUES ($1, $2, $3::uuid)\
    \ ON CONFLICT DO NOTHING"
    ( ((\(value, _, _) -> value) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((\(_, value, _) -> value) >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((\(_, _, value) -> value) >$< Encoders.param (Encoders.nonNullable Encoders.text))
    )
    (fmap (> 0) Decoders.rowsAffected)
    True

searchTurnsStatement
    :: Statement (Text, Int64) [ConversationSearchResult]
searchTurnsStatement = mkStatement
    "WITH query AS (SELECT websearch_to_tsquery('english', $1) AS value)\
    \ SELECT s.session_key, t.turn_index, t.occurred_at,\
    \   t.user_text, t.assistant_text,\
    \   ts_rank_cd(t.search_vector, query.value)::float8\
    \ FROM harness.session_turns t\
    \ JOIN harness.sessions s ON s.session_id = t.session_id\
    \ CROSS JOIN query\
    \ WHERE s.deleted_at IS NULL AND (\
    \   t.search_vector @@ query.value\
    \   OR t.user_text ILIKE '%' || $1 || '%'\
    \   OR coalesce(t.assistant_text, '') ILIKE '%' || $1 || '%'\
    \ )\
    \ ORDER BY ts_rank_cd(t.search_vector, query.value) DESC,\
    \   t.occurred_at DESC\
    \ LIMIT $2"
    ( (fst >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> (snd >$< Encoders.param (Encoders.nonNullable Encoders.int8))
    )
    (Decoders.rowList $
        ConversationSearchResult
            <$> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.int8)
            <*> Decoders.column (Decoders.nonNullable Decoders.timestamptz)
            <*> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.float8))
    True

searchNativeConversationsStatement
    :: Statement (Text, Int64) [NativeConversationSearchResult]
searchNativeConversationsStatement = mkStatement
    "WITH query AS (SELECT websearch_to_tsquery('english', $1) AS ts,\
    \ lower(btrim($1)) AS needle), candidates AS (\
    \ SELECT s.session_key,s.title,s.cwd,s.provider,s.model_id,s.updated_at,\
    \ s.archived_at IS NOT NULL,NULL::bigint,NULL::timestamptz,NULL::text,\
    \ NULL::text,NULL::text,CASE WHEN lower(s.title)=query.needle THEN 1000::float8\
    \ WHEN lower(s.title) LIKE query.needle || '%' THEN 800::float8\
    \ WHEN s.title ILIKE '%' || $1 || '%' THEN 600::float8\
    \ WHEN s.cwd ILIKE '%' || $1 || '%' THEN 400::float8 ELSE 300::float8 END\
    \ FROM harness.sessions s CROSS JOIN query WHERE s.deleted_at IS NULL AND (\
    \ s.title ILIKE '%' || $1 || '%' OR s.cwd ILIKE '%' || $1 || '%' OR\
    \ s.provider ILIKE '%' || $1 || '%' OR s.model_id ILIKE '%' || $1 || '%')\
    \ UNION ALL\
    \ SELECT s.session_key,s.title,s.cwd,s.provider,s.model_id,s.updated_at,\
    \ s.archived_at IS NOT NULL,t.turn_index,t.occurred_at,\
    \ CASE WHEN t.user_text ILIKE '%' || $1 || '%' OR\
    \ to_tsvector('english',coalesce(t.user_text,'')) @@ query.ts\
    \ THEN 'user' ELSE 'assistant' END,\
    \ t.user_text,t.assistant_text,\
    \ (100 + ts_rank_cd(t.search_vector,query.ts)*100)::float8\
    \ FROM harness.session_turns t JOIN harness.sessions s ON s.session_id=t.session_id\
    \ CROSS JOIN query WHERE s.deleted_at IS NULL AND (\
    \ t.search_vector @@ query.ts OR t.user_text ILIKE '%' || $1 || '%' OR\
    \ coalesce(t.assistant_text,'') ILIKE '%' || $1 || '%'))\
    \ SELECT * FROM candidates ORDER BY 13 DESC,6 DESC,9 DESC NULLS LAST LIMIT $2"
    ( (fst >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> (snd >$< Encoders.param (Encoders.nonNullable Encoders.int8)) )
    (Decoders.rowList $
        NativeConversationSearchResult
            <$> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.timestamptz)
            <*> Decoders.column (Decoders.nonNullable Decoders.bool)
            <*> Decoders.column (Decoders.nullable Decoders.int8)
            <*> Decoders.column (Decoders.nullable Decoders.timestamptz)
            <*> Decoders.column (Decoders.nullable Decoders.text)
            <*> Decoders.column (Decoders.nullable Decoders.text)
            <*> Decoders.column (Decoders.nullable Decoders.text)
            <*> Decoders.column (Decoders.nonNullable Decoders.float8))
    True

blockingAdvisoryLockStatement :: Statement Text Bool
blockingAdvisoryLockStatement = mkStatement
    "SELECT true FROM (\
    \ SELECT pg_advisory_xact_lock(hashtextextended($1, 684022778))\
    \ ) AS acquired"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
    True

tryAdvisoryLockStatement :: Statement Text Bool
tryAdvisoryLockStatement = mkStatement
    "SELECT pg_try_advisory_xact_lock(hashtextextended($1, 684022778))"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
    True

metadataParams :: Encoders.Params SessionMetadata
metadataParams =
    ((.sessionMetadataKey) >$< Encoders.param (Encoders.nonNullable Encoders.text))
    <> ((.sessionMetadataVersion) >$< Encoders.param (Encoders.nonNullable Encoders.int4))
    <> ((.sessionMetadataCreatedAt) >$< Encoders.param (Encoders.nonNullable Encoders.timestamptz))
    <> ((.sessionMetadataUpdatedAt) >$< Encoders.param (Encoders.nonNullable Encoders.timestamptz))
    <> ((.sessionMetadataProvider) >$< Encoders.param (Encoders.nonNullable Encoders.text))
    <> ((.sessionMetadataConnection) >$< Encoders.param (Encoders.nonNullable Encoders.text))
    <> ((.sessionMetadataModel) >$< Encoders.param (Encoders.nonNullable Encoders.text))
    <> ((.sessionMetadataTransportModel) >$< Encoders.param (Encoders.nullable Encoders.text))
    <> ((.sessionMetadataDialect) >$< Encoders.param (Encoders.nonNullable Encoders.text))
    <> ((legacyField (.sessionLegacyProvider)) >$< Encoders.param (Encoders.nullable Encoders.text))
    <> ((legacyField (.sessionLegacyConnection)) >$< Encoders.param (Encoders.nullable Encoders.text))
    <> ((legacyField (.sessionLegacyEffectiveModel)) >$< Encoders.param (Encoders.nullable Encoders.text))
    <> ((legacyField (.sessionLegacyDialect)) >$< Encoders.param (Encoders.nullable Encoders.text))
    <> ((.sessionMetadataCwd) >$< Encoders.param (Encoders.nonNullable Encoders.text))
    <> ((.sessionMetadataEffort) >$< Encoders.param (Encoders.nonNullable Encoders.text))
    <> ((.sessionMetadataTitle) >$< Encoders.param (Encoders.nonNullable Encoders.text))
    <> ((.sessionMetadataTitleIsManual) >$< Encoders.param (Encoders.nonNullable Encoders.bool))
    <> ((.sessionMetadataTitleRefreshIndex) >$< Encoders.param (Encoders.nonNullable Encoders.int8))
    <> ((.sessionMetadataTitleUserTurns) >$< Encoders.param (Encoders.nonNullable Encoders.int8))
    <> ((.sessionMetadataLastResponseId) >$< Encoders.param (Encoders.nullable Encoders.text))
    <> ((.sessionMetadataInputTokens) >$< Encoders.param (Encoders.nonNullable Encoders.int8))
    <> ((.sessionMetadataOutputTokens) >$< Encoders.param (Encoders.nonNullable Encoders.int8))
    <> ((.sessionMetadataCachedTokens) >$< Encoders.param (Encoders.nonNullable Encoders.int8))
    <> ((.sessionMetadataLastRecap) >$< Encoders.param (Encoders.nullable Encoders.text))
    <> ((.sessionMetadataLastTurnSummary) >$< Encoders.param (Encoders.nullable Encoders.text))
    <> ((.sessionMetadataLastRecapMainTurns) >$< Encoders.param (Encoders.nonNullable Encoders.int8))
  where
    legacyField
        :: (SessionLegacyTarget -> Text)
        -> SessionMetadata
        -> Maybe Text
    legacyField field metadata =
        field <$> metadata.sessionMetadataLegacyTarget

metadataRow :: Decoders.Row SessionMetadata
metadataRow =
    SessionMetadata
        <$> Decoders.column (Decoders.nonNullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.int4)
        <*> Decoders.column (Decoders.nonNullable Decoders.timestamptz)
        <*> Decoders.column (Decoders.nonNullable Decoders.timestamptz)
        <*> Decoders.column (Decoders.nonNullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.text)
        <*> Decoders.column (Decoders.nullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.text)
        <*> (decodeLegacyTarget
            <$> Decoders.column (Decoders.nullable Decoders.text)
            <*> Decoders.column (Decoders.nullable Decoders.text)
            <*> Decoders.column (Decoders.nullable Decoders.text)
            <*> Decoders.column (Decoders.nullable Decoders.text))
        <*> Decoders.column (Decoders.nonNullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.bool)
        <*> Decoders.column (Decoders.nonNullable Decoders.int8)
        <*> Decoders.column (Decoders.nonNullable Decoders.int8)
        <*> Decoders.column (Decoders.nullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.int8)
        <*> Decoders.column (Decoders.nonNullable Decoders.int8)
        <*> Decoders.column (Decoders.nonNullable Decoders.int8)
        <*> Decoders.column (Decoders.nullable Decoders.text)
        <*> Decoders.column (Decoders.nullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.int8)

decodeLegacyTarget
    :: Maybe Text
    -> Maybe Text
    -> Maybe Text
    -> Maybe Text
    -> Maybe SessionLegacyTarget
decodeLegacyTarget provider connection model dialect =
    case (provider, connection, model, dialect) of
        (Just provider', Just connection', Just model', Just dialect') ->
            Just SessionLegacyTarget
                { sessionLegacyProvider = provider'
                , sessionLegacyConnection = connection'
                , sessionLegacyEffectiveModel = model'
                , sessionLegacyDialect = dialect'
                }
        _ -> Nothing
