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
    , SessionTurn(..)
    , SessionUsage(..)
    , StoredSession(..)
    , StoredEvent(..)
    , StoredTurn(..)
    , LegacySession(..)
    , ConversationSearchResult(..)
    , sessionSchemaStatements
    , createSession
    , replaceSessionMetadata
    , appendSessionTurn
    , loadSession
    , loadSessionEvents
    , listSessionMetadata
    , searchConversationTurns
    , deleteSession
    , importLegacySession
    , withSessionAdvisoryLock
    ) where

import Control.Monad (forM, forM_, unless)
import Data.ByteString (ByteString)
import Data.Functor.Contravariant ((>$<))
import Data.Int (Int32, Int64)
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
    }
    deriving (Eq, Show)

data SessionUsage = SessionUsage
    { sessionUsageInputTokens :: !Int64
    , sessionUsageOutputTokens :: !Int64
    , sessionUsageCachedTokens :: !Int64
    }
    deriving (Eq, Show)

data SessionTurn = SessionTurn
    { sessionTurnOccurredAt :: !UTCTime
    , sessionTurnUserText :: !Text
    , sessionTurnAssistantText :: !(Maybe Text)
    , sessionTurnError :: !(Maybe Text)
    , sessionTurnResponseId :: !(Maybe Text)
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
      \ next_event_sequence bigint NOT NULL DEFAULT 1,\
      \ next_turn_index bigint NOT NULL DEFAULT 0,\
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
    withSession pool $
        Transactions.transaction Transactions.Serializable Transactions.Write do
            _ <- Transaction.statement
                metadata.sessionMetadataKey
                blockingAdvisoryLockStatement
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

loadSession
    :: StorePool
    -> Text
    -> IO (Either StoreError (Maybe StoredSession))
loadSession pool sessionKey =
    withSession pool
        (Transactions.transaction Transactions.RepeatableRead Transactions.Read do
            metadata <- Transaction.statement sessionKey loadMetadataStatement
            case metadata of
                Nothing -> pure (Right Nothing)
                Just value -> do
                    rows <- Transaction.statement sessionKey loadTurnsStatement
                    turns <- forM rows loadStoredTurn
                    pure do
                        decodedTurns <- sequence turns
                        pure $ Just StoredSession
                            { storedMetadata = value
                            , storedTurns = decodedTurns
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
    , turnRowUsageInput :: !(Maybe Int64)
    , turnRowUsageOutput :: !(Maybe Int64)
    , turnRowUsageCached :: !(Maybe Int64)
    }

loadStoredTurn :: TurnRow -> Transaction.Transaction (Either Text StoredTurn)
loadStoredTurn row = do
    items <- loadResponseItems row.turnRowId
    pure do
        decodedItems <- items
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
    textParam
    boolResult
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
    \ cached_tokens\
    \ ) VALUES (\
    \ $1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13,\
    \ $14, $15, $16, $17, $18, $19, $20, $21, $22, $23\
    \ ) RETURNING session_id::text"
    metadataParams
    textSingleResult
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
            <$> textColumn
            <*> int64Column)
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
            <$> textColumn
            <*> int64Column
            <*> int64Column)
    True

metadataUpdateSql :: Text
metadataUpdateSql =
    "UPDATE harness.sessions SET\
    \ session_schema_version = $2, created_at = $3, updated_at = $4,\
    \ provider = $5, connection_id = $6, model_id = $7,\
    \ transport_model_id = $8, dialect = $9,\
    \ legacy_target_provider = $10, legacy_target_connection = $11,\
    \ legacy_target_effective_model = $12, legacy_target_dialect = $13,\
    \ cwd = $14, effort = $15, title = $16, title_is_manual = $17,\
    \ title_refresh_index = $18, title_user_turns = $19,\
    \ last_response_id = $20, input_tokens = $21, output_tokens = $22,\
    \ cached_tokens = $23"

insertEventStatement :: Statement EventInsert Text
insertEventStatement = mkStatement
    "INSERT INTO harness.session_events\
    \ (session_id, sequence, event_kind, occurred_at)\
    \ VALUES ($1::uuid, $2, $3, $4)\
    \ RETURNING event_id::text"
    ( ((.eventInsertSessionId) >$< textParam)
        <> ((.eventInsertSequence) >$< int64Param)
        <> ((.eventInsertKind) >$< textParam)
        <> ((.eventInsertOccurredAt) >$< timeParam)
    )
    textSingleResult
    True

insertTurnStatement :: Statement TurnInsert Text
insertTurnStatement = mkStatement
    "INSERT INTO harness.session_turns (\
    \ session_id, event_id, turn_index, event_sequence, occurred_at,\
    \ user_text, assistant_text, error_text, response_id,\
    \ usage_input_tokens, usage_output_tokens, usage_cached_tokens\
    \ ) VALUES (\
    \ $1::uuid, $2::uuid, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12\
    \ ) RETURNING turn_id::text"
    ( ((.turnInsertSessionId) >$< textParam)
        <> ((.turnInsertEventId) >$< textParam)
        <> ((.turnInsertIndex) >$< int64Param)
        <> ((.turnInsertEventSequence) >$< int64Param)
        <> (turnOccurredAt >$< timeParam)
        <> (turnUserText >$< textParam)
        <> (turnAssistantText >$< nullableTextParam)
        <> (turnError >$< nullableTextParam)
        <> (turnResponseId >$< nullableTextParam)
        <> ((usageField (.sessionUsageInputTokens) . (.turnInsertTurn))
            >$< nullableInt64Param)
        <> ((usageField (.sessionUsageOutputTokens) . (.turnInsertTurn))
            >$< nullableInt64Param)
        <> ((usageField (.sessionUsageCachedTokens) . (.turnInsertTurn))
            >$< nullableInt64Param)
    )
    textSingleResult
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

loadMetadataStatement :: Statement Text (Maybe SessionMetadata)
loadMetadataStatement = mkStatement
    (metadataSelectSql
        <> " WHERE session_key = $1 AND deleted_at IS NULL")
    textParam
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
    \ cached_tokens FROM harness.sessions"

loadTurnsStatement :: Statement Text [TurnRow]
loadTurnsStatement = mkStatement
    "SELECT t.turn_id::text, t.turn_index, t.event_sequence, t.occurred_at,\
    \ t.user_text, t.assistant_text, t.error_text, t.response_id,\
    \ t.usage_input_tokens, t.usage_output_tokens, t.usage_cached_tokens\
    \ FROM harness.session_turns t\
    \ JOIN harness.sessions s ON s.session_id = t.session_id\
    \ WHERE s.session_key = $1\
    \ ORDER BY t.turn_index ASC"
    textParam
    (Decoders.rowList $
        TurnRow
            <$> textColumn
            <*> int64Column
            <*> int64Column
            <*> timeColumn
            <*> textColumn
            <*> nullableTextColumn
            <*> nullableTextColumn
            <*> nullableTextColumn
            <*> nullableInt64Column
            <*> nullableInt64Column
            <*> nullableInt64Column)
    True

loadEventsStatement :: Statement Text [StoredEvent]
loadEventsStatement = mkStatement
    "SELECT e.sequence, e.event_kind, e.occurred_at\
    \ FROM harness.session_events e\
    \ JOIN harness.sessions s ON s.session_id = e.session_id\
    \ WHERE s.session_key = $1\
    \ ORDER BY e.sequence ASC"
    textParam
    (Decoders.rowList $
        StoredEvent
            <$> int64Column
            <*> textColumn
            <*> timeColumn)
    True

deleteProjectionStatement
    :: Statement (Text, UTCTime) (Maybe (Text, Int64))
deleteProjectionStatement = mkStatement
    "UPDATE harness.sessions\
    \ SET updated_at = $2, deleted_at = $2,\
    \     next_event_sequence = next_event_sequence + 1\
    \ WHERE session_key = $1 AND deleted_at IS NULL\
    \ RETURNING session_id::text, next_event_sequence - 1"
    ( (fst >$< textParam)
        <> (snd >$< timeParam)
    )
    (Decoders.rowMaybe $
        (,)
            <$> textColumn
            <*> int64Column)
    True

recordLegacyImportStatement :: Statement (Text, Text, Text) Bool
recordLegacyImportStatement = mkStatement
    "INSERT INTO harness.legacy_session_imports\
    \ (source_path, content_hash, session_id)\
    \ VALUES ($1, $2, $3::uuid)\
    \ ON CONFLICT DO NOTHING"
    ( ((\(value, _, _) -> value) >$< textParam)
        <> ((\(_, value, _) -> value) >$< textParam)
        <> ((\(_, _, value) -> value) >$< textParam)
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
    ( (fst >$< textParam)
        <> (snd >$< int64Param)
    )
    (Decoders.rowList $
        ConversationSearchResult
            <$> textColumn
            <*> int64Column
            <*> timeColumn
            <*> textColumn
            <*> nullableTextColumn
            <*> Decoders.column (Decoders.nonNullable Decoders.float8))
    True

blockingAdvisoryLockStatement :: Statement Text Bool
blockingAdvisoryLockStatement = mkStatement
    "SELECT true FROM (\
    \ SELECT pg_advisory_xact_lock(hashtextextended($1, 684022778))\
    \ ) AS acquired"
    textParam
    boolResult
    True

tryAdvisoryLockStatement :: Statement Text Bool
tryAdvisoryLockStatement = mkStatement
    "SELECT pg_try_advisory_xact_lock(hashtextextended($1, 684022778))"
    textParam
    boolResult
    True

metadataParams :: Encoders.Params SessionMetadata
metadataParams =
    ((.sessionMetadataKey) >$< textParam)
    <> ((.sessionMetadataVersion) >$< int32Param)
    <> ((.sessionMetadataCreatedAt) >$< timeParam)
    <> ((.sessionMetadataUpdatedAt) >$< timeParam)
    <> ((.sessionMetadataProvider) >$< textParam)
    <> ((.sessionMetadataConnection) >$< textParam)
    <> ((.sessionMetadataModel) >$< textParam)
    <> ((.sessionMetadataTransportModel) >$< nullableTextParam)
    <> ((.sessionMetadataDialect) >$< textParam)
    <> ((legacyField (.sessionLegacyProvider)) >$< nullableTextParam)
    <> ((legacyField (.sessionLegacyConnection)) >$< nullableTextParam)
    <> ((legacyField (.sessionLegacyEffectiveModel)) >$< nullableTextParam)
    <> ((legacyField (.sessionLegacyDialect)) >$< nullableTextParam)
    <> ((.sessionMetadataCwd) >$< textParam)
    <> ((.sessionMetadataEffort) >$< textParam)
    <> ((.sessionMetadataTitle) >$< textParam)
    <> ((.sessionMetadataTitleIsManual) >$< boolParam)
    <> ((.sessionMetadataTitleRefreshIndex) >$< int64Param)
    <> ((.sessionMetadataTitleUserTurns) >$< int64Param)
    <> ((.sessionMetadataLastResponseId) >$< nullableTextParam)
    <> ((.sessionMetadataInputTokens) >$< int64Param)
    <> ((.sessionMetadataOutputTokens) >$< int64Param)
    <> ((.sessionMetadataCachedTokens) >$< int64Param)
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
        <$> textColumn
        <*> int32Column
        <*> timeColumn
        <*> timeColumn
        <*> textColumn
        <*> textColumn
        <*> textColumn
        <*> nullableTextColumn
        <*> textColumn
        <*> (decodeLegacyTarget
            <$> nullableTextColumn
            <*> nullableTextColumn
            <*> nullableTextColumn
            <*> nullableTextColumn)
        <*> textColumn
        <*> textColumn
        <*> textColumn
        <*> boolColumn
        <*> int64Column
        <*> int64Column
        <*> nullableTextColumn
        <*> int64Column
        <*> int64Column
        <*> int64Column

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

textParam :: Encoders.Params Text
textParam = Encoders.param (Encoders.nonNullable Encoders.text)

nullableTextParam :: Encoders.Params (Maybe Text)
nullableTextParam = Encoders.param (Encoders.nullable Encoders.text)

int32Param :: Encoders.Params Int32
int32Param = Encoders.param (Encoders.nonNullable Encoders.int4)

int64Param :: Encoders.Params Int64
int64Param = Encoders.param (Encoders.nonNullable Encoders.int8)

nullableInt64Param :: Encoders.Params (Maybe Int64)
nullableInt64Param = Encoders.param (Encoders.nullable Encoders.int8)

boolParam :: Encoders.Params Bool
boolParam = Encoders.param (Encoders.nonNullable Encoders.bool)

timeParam :: Encoders.Params UTCTime
timeParam = Encoders.param (Encoders.nonNullable Encoders.timestamptz)

textColumn :: Decoders.Row Text
textColumn = Decoders.column (Decoders.nonNullable Decoders.text)

nullableTextColumn :: Decoders.Row (Maybe Text)
nullableTextColumn = Decoders.column (Decoders.nullable Decoders.text)

int32Column :: Decoders.Row Int32
int32Column = Decoders.column (Decoders.nonNullable Decoders.int4)

int64Column :: Decoders.Row Int64
int64Column = Decoders.column (Decoders.nonNullable Decoders.int8)

nullableInt64Column :: Decoders.Row (Maybe Int64)
nullableInt64Column = Decoders.column (Decoders.nullable Decoders.int8)

boolColumn :: Decoders.Row Bool
boolColumn = Decoders.column (Decoders.nonNullable Decoders.bool)

timeColumn :: Decoders.Row UTCTime
timeColumn = Decoders.column (Decoders.nonNullable Decoders.timestamptz)

textSingleResult :: Decoders.Result Text
textSingleResult = Decoders.singleRow textColumn

boolResult :: Decoders.Result Bool
boolResult = Decoders.singleRow boolColumn
