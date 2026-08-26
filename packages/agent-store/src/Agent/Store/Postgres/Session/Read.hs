{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- These database statements are dominated by decoding and IO; optimizing the
-- generated expression trees adds substantially more build cost than value.
{-# OPTIONS_GHC -O0 #-}

-- | PostgreSQL session read operations.
module Agent.Store.Postgres.Session.Read
    ( loadSession
    , loadSessions
    , loadSessionMetadata
    , loadActiveSession
    , loadRecentSessionTurns
    , loadSessionTurnsBefore
    , loadSessionTurnsAfter
    , loadSessionResumeStats
    , loadSessionEvents
    , listSessionMetadata
    , searchConversationTurns
    ) where

import Data.Functor.Contravariant ((>$<))
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import qualified Data.Vector as Vector
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Session as HasqlSession
import qualified Hasql.Transaction as Transaction
import qualified Hasql.Transaction.Sessions as Transactions
import Hasql.Statement (Statement)

import Agent.Store.Postgres.Connection
    ( StorePool
    , withSession
    )
import Agent.Store.Postgres.Hasql (mkStatement)
import Agent.Store.Postgres.Session.Types
import Agent.Store.Postgres.SessionItem (loadResponseItems)
import Agent.Store.Types (StoreError(..))

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
            turns <- Vector.mapM
                (\(sessionKey, row) ->
                    fmap ((,) sessionKey) (loadStoredTurn row))
                rows
            pure (assembleSessions sessionKeys metadata turns))
        >>= \case
            Left err -> pure (replicate (length sessionKeys) (Left err))
            Right results ->
                pure (map (either (Left . StoreDataError) Right) results)

assembleSessions
    :: [Text]
    -> Vector.Vector SessionMetadata
    -> Vector.Vector (Text, Either Text StoredTurn)
    -> [Either Text (Maybe StoredSession)]
assembleSessions sessionKeys metadata turns =
    map assemble sessionKeys
  where
    metadataByKey =
        Vector.foldl'
            (\byKey value ->
                Map.insert value.sessionMetadataKey value byKey)
            Map.empty
            metadata
    turnsByKey =
        Map.fromList
            [ (sessionKey, Vector.map snd group)
            | group <- Vector.groupBy
                (\left right -> fst left == fst right)
                turns
            , Just ((sessionKey, _), _) <- [Vector.uncons group]
            ]

    assemble sessionKey =
        case Map.lookup sessionKey metadataByKey of
            Nothing -> Right Nothing
            Just value -> do
                decodedTurns <-
                    sequence
                        (Map.findWithDefault Vector.empty sessionKey turnsByKey)
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
                    turns <- Vector.mapM loadStoredTurn rows
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
    -> Transaction.Transaction (Vector.Vector TurnRow)
    -> Int
    -> PageMode
    -> IO (Either StoreError (Maybe SessionTurnPage))
loadTurnPage pool sessionKey loadRows limit mode =
    withSession pool
        (Transactions.transaction Transactions.RepeatableRead Transactions.Read do
            metadata <- Transaction.statement sessionKey loadMetadataStatement
            case metadata of
                Nothing -> pure (Right Nothing)
                Just _ -> do
                    rows0 <- loadRows
                    (generationStart, total) <-
                        Transaction.statement sessionKey currentGenerationStatement
                    let visibleRows = case mode of
                            PageRecent -> Vector.reverse (Vector.take limit rows0)
                            PageBefore -> Vector.reverse (Vector.take limit rows0)
                            PageAfter -> Vector.take limit rows0
                    turns <- Vector.mapM loadStoredTurn visibleRows
                    pure do
                        decoded <- sequence turns
                        let generationEnd =
                                generationStart + max 0 total - 1
                            (hasOlder, hasNewer) =
                                case Vector.uncons decoded of
                                    Just (firstTurn, _) ->
                                        let lastTurn = Vector.last decoded
                                        in
                                            ( firstTurn.storedTurnIndex
                                                > generationStart
                                            , lastTurn.storedTurnIndex
                                                < generationEnd
                                            )
                                    Nothing -> case mode of
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

flattenDataResult
    :: Either StoreError (Either Text a)
    -> IO (Either StoreError a)
flattenDataResult = pure . \case
    Left err -> Left err
    Right (Left err) -> Left (StoreDataError err)
    Right (Right value) -> Right value

loadMetadataManyStatement
    :: Statement [Text] (Vector.Vector SessionMetadata)
loadMetadataManyStatement = mkStatement
    (metadataSelectSql
        <> " WHERE session_key = ANY($1::text[]) AND deleted_at IS NULL\
           \ ORDER BY array_position($1::text[], session_key)")
    textArrayParams
    (Decoders.rowVector metadataRow)
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

loadTurnsManyStatement
    :: Statement [Text] (Vector.Vector (Text, TurnRow))
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
    (Decoders.rowVector $
        (,)
            <$> Decoders.column (Decoders.nonNullable Decoders.text)
            <*> turnRowDecoder)
    True

loadActiveTurnsStatement :: Statement Text (Vector.Vector TurnRow)
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
    (Decoders.rowVector turnRowDecoder)
    True

loadRecentTurnsStatement
    :: Statement (Text, Int64) (Vector.Vector TurnRow)
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
    (Decoders.rowVector turnRowDecoder)
    True

loadTurnsBeforeStatement
    :: Statement (Text, Int64, Int64) (Vector.Vector TurnRow)
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
    (Decoders.rowVector turnRowDecoder)
    True

loadTurnsAfterStatement
    :: Statement (Text, Int64, Int64) (Vector.Vector TurnRow)
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
    (Decoders.rowVector turnRowDecoder)
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
    \   OR t.assistant_text ILIKE '%' || $1 || '%'\
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
