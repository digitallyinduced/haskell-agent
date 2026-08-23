{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | PostgreSQL persistence for durable harness sessions.
--
-- PostgreSQL owns the canonical session projection and immutable event stream.
-- Arbitrary provider response values are stored in a normalized relational
-- value tree rather than JSON/JSONB columns. User and assistant text are also
-- projected into typed columns with a generated full-text-search vector.
module Agent.Store.Postgres.Session
    ( StoredSession(..)
    , StoredEvent(..)
    , StoredTurn(..)
    , LegacySession(..)
    , ConversationSearchResult(..)
    , sessionSchemaStatements
    , migrateSessionSchema
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
import Data.Aeson (Value(..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as ByteString
import Data.Functor.Contravariant ((>$<))
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Data.Time.Clock (UTCTime)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Session as Session
import Hasql.Statement (Statement)
import qualified Hasql.Transaction as Transaction
import qualified Hasql.Transaction.Sessions as Transactions

import Agent.Store.Postgres.Connection
    ( StorePool
    , withSession
    )
import Agent.Store.Postgres.Hasql (mkStatement)
import Agent.Store.Postgres.NormalizedValue
    ( insertNormalizedValue
    , loadNormalizedValueRequired
    , normalizedValueSchemaStatements
    )
import Agent.Store.Types (StoreError)

data StoredSession = StoredSession
    { storedMetadata :: !Value
    , storedTurns :: ![StoredTurn]
    } deriving (Eq, Show)

data StoredEvent = StoredEvent
    { storedEventSequence :: !Int64
    , storedEventKind :: !Text
    , storedEventOccurredAt :: !UTCTime
    , storedEventPayload :: !Value
    } deriving (Eq, Show)

data StoredTurn = StoredTurn
    { storedTurnIndex :: !Int64
    , storedEventSequence :: !Int64
    , storedOccurredAt :: !UTCTime
    , storedTurnPayload :: !Value
    } deriving (Eq, Show)

data ConversationSearchResult = ConversationSearchResult
    { searchSessionId :: !Text
    , searchTurnIndex :: !Int64
    , searchOccurredAt :: !UTCTime
    , searchUserText :: !Text
    , searchAssistantText :: !(Maybe Text)
    , searchRank :: !Double
    } deriving (Eq, Show)

-- | One fully decoded legacy JSONL session ready for an atomic import.
data LegacySession = LegacySession
    { legacySourcePath :: !Text
    , legacyContentHash :: !Text
    , legacySessionId :: !Text
    , legacyCreatedAt :: !UTCTime
    , legacyUpdatedAt :: !UTCTime
    , legacyMetadata :: !Value
    , legacyTurns :: ![(UTCTime, Value)]
    } deriving (Eq, Show)

sessionSchemaStatements :: [ByteString.ByteString]
sessionSchemaStatements =
    normalizedValueSchemaStatements
    <> [ "CREATE TABLE IF NOT EXISTS harness.sessions (\
       \ session_id uuid PRIMARY KEY DEFAULT pg_catalog.uuidv7(),\
       \ session_key text NOT NULL UNIQUE,\
       \ created_at timestamptz NOT NULL,\
       \ updated_at timestamptz NOT NULL,\
       \ metadata_value_id uuid NOT NULL\
       \   REFERENCES harness.structured_values(value_id),\
       \ next_event_sequence bigint NOT NULL DEFAULT 1,\
       \ next_turn_index bigint NOT NULL DEFAULT 0,\
       \ deleted_at timestamptz,\
       \ CHECK (next_event_sequence >= 1),\
       \ CHECK (next_turn_index >= 0)\
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
       \ payload_value_id uuid NOT NULL\
       \   REFERENCES harness.structured_values(value_id),\
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
       \ turn_value_id uuid NOT NULL\
       \   REFERENCES harness.structured_values(value_id),\
       \ search_vector tsvector GENERATED ALWAYS AS (\
       \   setweight(to_tsvector('english', coalesce(user_text, '')), 'A') ||\
       \   setweight(to_tsvector('english', coalesce(assistant_text, '')), 'B')\
       \ ) STORED,\
       \ UNIQUE (session_id, turn_index),\
       \ UNIQUE (session_id, event_sequence),\
       \ FOREIGN KEY (session_id, event_sequence)\
       \   REFERENCES harness.session_events(session_id, sequence)\
       \ )"
       , "CREATE INDEX IF NOT EXISTS session_turns_search_idx\
       \ ON harness.session_turns USING gin (search_vector)"
       , "CREATE INDEX IF NOT EXISTS session_turns_session_time_idx\
       \ ON harness.session_turns (session_id, occurred_at DESC)"
       , "CREATE TABLE IF NOT EXISTS harness.legacy_session_imports (\
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

migrateSessionSchema :: StorePool -> IO (Either StoreError ())
migrateSessionSchema pool =
    withSession pool
        (forM_ sessionSchemaStatements (Session.script . Text.decodeUtf8))

createSession
    :: StorePool
    -> Text
    -> UTCTime
    -> Value
    -> IO (Either StoreError Bool)
createSession pool sessionKey createdAt metadata =
    withSession pool $
        Transactions.transaction Transactions.Serializable Transactions.Write do
            _ <- Transaction.statement sessionKey blockingAdvisoryLockStatement
            exists <- Transaction.statement sessionKey sessionExistsStatement
            if exists
                then pure False
                else do
                    metadataId <- insertNormalizedValue metadata
                    sessionId <- Transaction.statement
                        (sessionKey, createdAt, metadataId)
                        insertSessionStatement
                    Transaction.statement
                        (sessionId, 0, "session.created", createdAt, metadataId)
                        insertEventStatement
                    pure True

replaceSessionMetadata
    :: StorePool
    -> Text
    -> UTCTime
    -> Text
    -> Value
    -> IO (Either StoreError Bool)
replaceSessionMetadata pool sessionKey occurredAt eventKind metadata =
    withSession pool $
        Transactions.transaction Transactions.Serializable Transactions.Write do
            _ <- Transaction.statement sessionKey blockingAdvisoryLockStatement
            target <- Transaction.statement sessionKey activeSessionStatement
            case target of
                Nothing -> pure False
                Just _ -> do
                    metadataId <- insertNormalizedValue metadata
                    changed <- Transaction.statement
                        (sessionKey, occurredAt, metadataId)
                        replaceProjectionStatement
                    case changed of
                        Nothing -> pure False
                        Just (sessionId, sequence) -> do
                            Transaction.statement
                                ( sessionId
                                , sequence
                                , eventKind
                                , occurredAt
                                , metadataId
                                )
                                insertEventStatement
                            pure ()
                            pure True

appendSessionTurn
    :: StorePool
    -> Text
    -> UTCTime
    -> Value
    -> Value
    -> IO (Either StoreError Bool)
appendSessionTurn pool sessionKey occurredAt turn metadata =
    withSession pool $
        Transactions.transaction Transactions.Serializable Transactions.Write do
            _ <- Transaction.statement sessionKey blockingAdvisoryLockStatement
            target <- Transaction.statement sessionKey activeSessionStatement
            case target of
                Nothing -> pure False
                Just _ -> do
                    turnId <- insertNormalizedValue turn
                    metadataId <- insertNormalizedValue metadata
                    changed <- Transaction.statement
                        (sessionKey, occurredAt, metadataId)
                        appendProjectionStatement
                    case changed of
                        Nothing -> pure False
                        Just (sessionId, sequence, turnIndex) -> do
                            eventId <- Transaction.statement
                                ( sessionId
                                , sequence
                                , "turn.appended"
                                , occurredAt
                                , turnId
                                )
                                insertEventStatement
                            let (userText, assistantText) = searchableTurnText turn
                            Transaction.statement
                                ( sessionId
                                , eventId
                                , turnIndex
                                , sequence
                                , occurredAt
                                , userText
                                , assistantText
                                , turnId
                                )
                                insertTurnStatement
                            pure True

loadSession
    :: StorePool
    -> Text
    -> IO (Either StoreError (Maybe StoredSession))
loadSession pool sessionKey =
    withSession pool $
        Transactions.transaction Transactions.RepeatableRead Transactions.Read do
            metadataRoot <- Transaction.statement sessionKey loadMetadataRootStatement
            case metadataRoot of
                Nothing -> pure Nothing
                Just root -> do
                    metadata <- requireValue "session metadata" root
                    turnRows <- Transaction.statement sessionKey loadTurnRootsStatement
                    turns <- forM turnRows \(turnIndex, sequence, at, valueRoot) -> do
                        payload <- requireValue "session turn" valueRoot
                        pure StoredTurn
                            { storedTurnIndex = turnIndex
                            , storedEventSequence = sequence
                            , storedOccurredAt = at
                            , storedTurnPayload = payload
                            }
                    pure $ Just StoredSession
                        { storedMetadata = metadata
                        , storedTurns = turns
                        }

loadSessionEvents
    :: StorePool
    -> Text
    -> IO (Either StoreError [StoredEvent])
loadSessionEvents pool sessionKey =
    withSession pool $
        Transactions.transaction Transactions.RepeatableRead Transactions.Read do
            rows <- Transaction.statement sessionKey loadEventRootsStatement
            forM rows \(sequence, kind, at, valueRoot) -> do
                payload <- requireValue "session event" valueRoot
                pure StoredEvent
                    { storedEventSequence = sequence
                    , storedEventKind = kind
                    , storedEventOccurredAt = at
                    , storedEventPayload = payload
                    }

listSessionMetadata :: StorePool -> IO (Either StoreError [Value])
listSessionMetadata pool =
    withSession pool $
        Transactions.transaction Transactions.RepeatableRead Transactions.Read do
            roots <- Transaction.statement () listMetadataRootsStatement
            traverse (requireValue "session metadata") roots

searchConversationTurns
    :: StorePool
    -> Text
    -> Int
    -> IO (Either StoreError [ConversationSearchResult])
searchConversationTurns pool query limit =
    withSession pool $
        Session.statement
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
            target <- Transaction.statement sessionKey activeSessionStatement
            case target of
                Nothing -> pure False
                Just _ -> do
                    emptyId <- insertNormalizedValue (Object KeyMap.empty)
                    changed <- Transaction.statement
                        (sessionKey, occurredAt)
                        deleteProjectionStatement
                    case changed of
                        Nothing -> pure False
                        Just (sessionId, sequence) -> do
                            Transaction.statement
                                ( sessionId
                                , sequence
                                , "session.deleted"
                                , occurredAt
                                , emptyId
                                )
                                insertEventStatement
                            pure True

importLegacySession
    :: StorePool
    -> LegacySession
    -> IO (Either StoreError Bool)
importLegacySession pool legacy =
    withSession pool $
        Transactions.transaction Transactions.Serializable Transactions.Write do
            _ <- Transaction.statement
                legacy.legacySessionId
                blockingAdvisoryLockStatement
            exists <- Transaction.statement
                legacy.legacySessionId
                sessionExistsStatement
            if exists
                then pure False
                else do
                    metadataId <- insertNormalizedValue legacy.legacyMetadata
                    sessionId <- Transaction.statement
                        ( legacy.legacySessionId
                        , legacy.legacyCreatedAt
                        , metadataId
                        )
                        insertSessionStatement
                    Transaction.statement
                        ( sessionId
                        , 0
                        , "session.created"
                        , legacy.legacyCreatedAt
                        , metadataId
                        )
                        insertEventStatement
                    forM_ legacy.legacyTurns \(at, turn) -> do
                        appended <- appendTurnTransaction
                            legacy.legacySessionId at turn legacy.legacyMetadata
                        unless appended Transaction.condemn
                    finalMetadataId <- insertNormalizedValue legacy.legacyMetadata
                    changed <- Transaction.statement
                        ( legacy.legacySessionId
                        , legacy.legacyUpdatedAt
                        , finalMetadataId
                        )
                        replaceProjectionStatement
                    case changed of
                        Nothing -> Transaction.condemn
                        Just (internalId, sequence) -> do
                            _ <- Transaction.statement
                                ( internalId
                                , sequence
                                , "legacy.import_completed"
                                , legacy.legacyUpdatedAt
                                , finalMetadataId
                                )
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

appendTurnTransaction :: Text -> UTCTime -> Value -> Value -> Transaction.Transaction Bool
appendTurnTransaction sessionKey occurredAt turn metadata = do
    turnId <- insertNormalizedValue turn
    metadataId <- insertNormalizedValue metadata
    changed <- Transaction.statement
        (sessionKey, occurredAt, metadataId)
        appendProjectionStatement
    case changed of
        Nothing -> pure False
        Just (sessionId, sequence, turnIndex) -> do
            eventId <- Transaction.statement
                (sessionId, sequence, "turn.appended", occurredAt, turnId)
                insertEventStatement
            let (userText, assistantText) = searchableTurnText turn
            Transaction.statement
                ( sessionId
                , eventId
                , turnIndex
                , sequence
                , occurredAt
                , userText
                , assistantText
                , turnId
                )
                insertTurnStatement
            pure True

requireValue :: Text -> Text -> Transaction.Transaction Value
requireValue _label = loadNormalizedValueRequired

searchableTurnText :: Value -> (Text, Maybe Text)
searchableTurnText = \case
    Object object ->
        ( textField "userText" object
        , optionalTextField "assistantText" object
        )
    _ -> ("", Nothing)
  where
    textField name object =
        case KeyMap.lookup (Key.fromText name) object of
            Just (String value) -> value
            _ -> ""
    optionalTextField name object =
        case KeyMap.lookup (Key.fromText name) object of
            Just (String value) -> Just value
            _ -> Nothing

sessionExistsStatement :: Statement Text Bool
sessionExistsStatement = mkStatement
    "SELECT EXISTS (SELECT 1 FROM harness.sessions WHERE session_key = $1)"
    textParam
    boolResult
    True

activeSessionStatement :: Statement Text (Maybe Text)
activeSessionStatement = mkStatement
    "SELECT session_id::text FROM harness.sessions\
    \ WHERE session_key = $1 AND deleted_at IS NULL"
    textParam
    (Decoders.rowMaybe textColumn)
    True

insertSessionStatement :: Statement (Text, UTCTime, Text) Text
insertSessionStatement = mkStatement
    "INSERT INTO harness.sessions\
    \ (session_key, created_at, updated_at, metadata_value_id)\
    \ VALUES ($1, $2, $2, $3::uuid)\
    \ RETURNING session_id::text"
    ( ((\(value, _, _) -> value) >$< textParam)
        <> ((\(_, value, _) -> value) >$< timeParam)
        <> ((\(_, _, value) -> value) >$< textParam)
    )
    textSingleResult
    True

replaceProjectionStatement
    :: Statement (Text, UTCTime, Text) (Maybe (Text, Int64))
replaceProjectionStatement = mkStatement
    "UPDATE harness.sessions\
    \ SET updated_at = $2, metadata_value_id = $3::uuid,\
    \     next_event_sequence = next_event_sequence + 1\
    \ WHERE session_key = $1 AND deleted_at IS NULL\
    \ RETURNING session_id::text, next_event_sequence - 1"
    ( ((\(value, _, _) -> value) >$< textParam)
        <> ((\(_, value, _) -> value) >$< timeParam)
        <> ((\(_, _, value) -> value) >$< textParam)
    )
    (Decoders.rowMaybe $
        (,)
            <$> textColumn
            <*> Decoders.column (Decoders.nonNullable Decoders.int8))
    True

appendProjectionStatement
    :: Statement (Text, UTCTime, Text) (Maybe (Text, Int64, Int64))
appendProjectionStatement = mkStatement
    "UPDATE harness.sessions\
    \ SET updated_at = $2, metadata_value_id = $3::uuid,\
    \     next_event_sequence = next_event_sequence + 1,\
    \     next_turn_index = next_turn_index + 1\
    \ WHERE session_key = $1 AND deleted_at IS NULL\
    \ RETURNING session_id::text, next_event_sequence - 1,\
    \   next_turn_index - 1"
    ( ((\(value, _, _) -> value) >$< textParam)
        <> ((\(_, value, _) -> value) >$< timeParam)
        <> ((\(_, _, value) -> value) >$< textParam)
    )
    (Decoders.rowMaybe $
        (,,)
            <$> textColumn
            <*> Decoders.column (Decoders.nonNullable Decoders.int8)
            <*> Decoders.column (Decoders.nonNullable Decoders.int8))
    True

insertEventStatement
    :: Statement (Text, Int64, Text, UTCTime, Text) Text
insertEventStatement = mkStatement
    "INSERT INTO harness.session_events\
    \ (session_id, sequence, event_kind, occurred_at, payload_value_id)\
    \ VALUES ($1::uuid, $2, $3, $4, $5::uuid)\
    \ RETURNING event_id::text"
    ( ((\(value, _, _, _, _) -> value) >$< textParam)
        <> ((\(_, value, _, _, _) -> value) >$< int64Param)
        <> ((\(_, _, value, _, _) -> value) >$< textParam)
        <> ((\(_, _, _, value, _) -> value) >$< timeParam)
        <> ((\(_, _, _, _, value) -> value) >$< textParam)
    )
    textSingleResult
    True

insertTurnStatement
    :: Statement
        (Text, Text, Int64, Int64, UTCTime, Text, Maybe Text, Text)
        ()
insertTurnStatement = mkStatement
    "INSERT INTO harness.session_turns\
    \ (session_id, event_id, turn_index, event_sequence, occurred_at,\
    \  user_text, assistant_text, turn_value_id)\
    \ VALUES ($1::uuid, $2::uuid, $3, $4, $5, $6, $7, $8::uuid)"
    ( ((\(value, _, _, _, _, _, _, _) -> value) >$< textParam)
        <> ((\(_, value, _, _, _, _, _, _) -> value) >$< textParam)
        <> ((\(_, _, value, _, _, _, _, _) -> value) >$< int64Param)
        <> ((\(_, _, _, value, _, _, _, _) -> value) >$< int64Param)
        <> ((\(_, _, _, _, value, _, _, _) -> value) >$< timeParam)
        <> ((\(_, _, _, _, _, value, _, _) -> value) >$< textParam)
        <> ((\(_, _, _, _, _, _, value, _) -> value) >$< nullableTextParam)
        <> ((\(_, _, _, _, _, _, _, value) -> value) >$< textParam)
    )
    Decoders.noResult
    True

loadMetadataRootStatement :: Statement Text (Maybe Text)
loadMetadataRootStatement = mkStatement
    "SELECT metadata_value_id::text FROM harness.sessions\
    \ WHERE session_key = $1 AND deleted_at IS NULL"
    textParam
    (Decoders.rowMaybe textColumn)
    True

loadTurnRootsStatement :: Statement Text [(Int64, Int64, UTCTime, Text)]
loadTurnRootsStatement = mkStatement
    "SELECT t.turn_index, t.event_sequence, t.occurred_at,\
    \ t.turn_value_id::text\
    \ FROM harness.session_turns t\
    \ JOIN harness.sessions s ON s.session_id = t.session_id\
    \ WHERE s.session_key = $1\
    \ ORDER BY t.turn_index ASC"
    textParam
    (Decoders.rowList $
        (,,,)
            <$> Decoders.column (Decoders.nonNullable Decoders.int8)
            <*> Decoders.column (Decoders.nonNullable Decoders.int8)
            <*> Decoders.column (Decoders.nonNullable Decoders.timestamptz)
            <*> textColumn)
    True

loadEventRootsStatement :: Statement Text [(Int64, Text, UTCTime, Text)]
loadEventRootsStatement = mkStatement
    "SELECT e.sequence, e.event_kind, e.occurred_at,\
    \ e.payload_value_id::text\
    \ FROM harness.session_events e\
    \ JOIN harness.sessions s ON s.session_id = e.session_id\
    \ WHERE s.session_key = $1\
    \ ORDER BY e.sequence ASC"
    textParam
    (Decoders.rowList $
        (,,,)
            <$> Decoders.column (Decoders.nonNullable Decoders.int8)
            <*> textColumn
            <*> Decoders.column (Decoders.nonNullable Decoders.timestamptz)
            <*> textColumn)
    True

listMetadataRootsStatement :: Statement () [Text]
listMetadataRootsStatement = mkStatement
    "SELECT metadata_value_id::text FROM harness.sessions\
    \ WHERE deleted_at IS NULL\
    \ ORDER BY updated_at DESC, session_key ASC"
    Encoders.noParams
    (Decoders.rowList textColumn)
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
            <*> Decoders.column (Decoders.nonNullable Decoders.int8))
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
            <*> Decoders.column (Decoders.nonNullable Decoders.int8)
            <*> Decoders.column (Decoders.nonNullable Decoders.timestamptz)
            <*> textColumn
            <*> Decoders.column (Decoders.nullable Decoders.text)
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

textParam :: Encoders.Params Text
textParam = Encoders.param (Encoders.nonNullable Encoders.text)

nullableTextParam :: Encoders.Params (Maybe Text)
nullableTextParam = Encoders.param (Encoders.nullable Encoders.text)

timeParam :: Encoders.Params UTCTime
timeParam = Encoders.param (Encoders.nonNullable Encoders.timestamptz)

int64Param :: Encoders.Params Int64
int64Param = Encoders.param (Encoders.nonNullable Encoders.int8)

textColumn :: Decoders.Row Text
textColumn = Decoders.column (Decoders.nonNullable Decoders.text)

textSingleResult :: Decoders.Result Text
textSingleResult = Decoders.singleRow textColumn

boolResult :: Decoders.Result Bool
boolResult =
    Decoders.singleRow
        (Decoders.column (Decoders.nonNullable Decoders.bool))
