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
    ( SessionReadImplementation(..)
    , loadSession
    , loadSessionWithImplementation
    , loadSessions
    , loadSessionMetadataMany
    , loadSessionMetadata
    , loadLatestSessionPromptEpoch
    , loadActiveSession
    , loadActiveSessionWithImplementation
    , loadRecentSessionTurns
    , loadRecentSessionHistoryTurns
    , loadSessionTurnsBefore
    , loadSessionHistoryTurnsBefore
    , loadSessionTurnsAfter
    , loadSessionHistoryTurnsRange
    , loadSessionHistoryTurnsRangeBounded
    , loadSessionHistorySnapshot
    , loadSessionResumeStats
    , loadSessionEvents
    , listSessionMetadata
    , listSessionMetadataForBoundary
    , listSessionArchiveKeys
    , searchConversationTurns
    , searchConversationTurnsForBoundary
    , searchNativeConversations
    , searchNativeConversationsForBoundary
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
import Agent.Store.Postgres.SessionItem
    ( loadResponseItems
    , loadResponseItemsPerItem
    )
import Agent.Store.Types (StoreError(..))

-- | Select the response-item read implementation. Normal callers should use
-- 'AdaptiveSessionRead'; 'PerItemSessionRead' remains available so allocation
-- benchmarks can execute the implementation being replaced.
data SessionReadImplementation
    = AdaptiveSessionRead
    | PerItemSessionRead
    deriving (Eq, Show)

loadSession
    :: StorePool
    -> Text
    -> IO (Either StoreError (Maybe StoredSession))
loadSession = loadSessionWithImplementation AdaptiveSessionRead

loadSessionWithImplementation
    :: SessionReadImplementation
    -> StorePool
    -> Text
    -> IO (Either StoreError (Maybe StoredSession))
loadSessionWithImplementation implementation pool sessionKey =
    loadSessionsWithImplementation implementation pool [sessionKey] >>= \case
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
loadSessions = loadSessionsWithImplementation AdaptiveSessionRead

loadSessionsWithImplementation
    :: SessionReadImplementation
    -> StorePool
    -> [Text]
    -> IO [Either StoreError (Maybe StoredSession)]
loadSessionsWithImplementation _ _ [] = pure []
loadSessionsWithImplementation implementation pool sessionKeys =
    withSession pool
        (Transactions.transaction Transactions.RepeatableRead Transactions.Read do
            metadata <- Transaction.statement
                sessionKeys
                loadMetadataManyStatement
            rows <- Transaction.statement sessionKeys loadTurnsManyStatement
            turns <- Vector.mapM
                (\(sessionKey, row) ->
                    fmap ((,) sessionKey)
                        (loadStoredTurnWith implementation row))
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

loadSessionMetadataMany
    :: StorePool
    -> [Text]
    -> IO (Either StoreError [SessionMetadata])
loadSessionMetadataMany pool sessionKeys =
    fmap (fmap Vector.toList) $
        withSession pool $
            Transactions.transaction
                Transactions.RepeatableRead
                Transactions.Read $
                    Transaction.statement
                        sessionKeys
                        loadMetadataManyStatement

loadLatestSessionPromptEpoch
    :: StorePool
    -> Text
    -> IO (Either StoreError (Maybe SessionPromptEpoch))
loadLatestSessionPromptEpoch pool sessionKey =
    withSession pool $
        Transactions.transaction Transactions.RepeatableRead Transactions.Read $
            Transaction.statement sessionKey loadLatestPromptEpochStatement

loadActiveSession
    :: StorePool
    -> Text
    -> IO (Either StoreError (Maybe StoredSession))
loadActiveSession =
    loadActiveSessionWithImplementation AdaptiveSessionRead

loadActiveSessionWithImplementation
    :: SessionReadImplementation
    -> StorePool
    -> Text
    -> IO (Either StoreError (Maybe StoredSession))
loadActiveSessionWithImplementation implementation pool sessionKey =
    withSession pool
        (Transactions.transaction Transactions.RepeatableRead Transactions.Read do
            metadata <- Transaction.statement sessionKey loadMetadataStatement
            case metadata of
                Nothing -> pure (Right Nothing)
                Just value -> do
                    rows <- Transaction.statement sessionKey loadActiveTurnsStatement
                    turns <- Vector.mapM
                        (loadStoredTurnWith implementation)
                        rows
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
        (Transaction.statement sessionKey displayHistoryBoundsStatement)
        (max 1 limit)
        PageRecent

-- | Page the complete durable conversation, including turns hidden by an
-- explicit transcript reset.
loadRecentSessionHistoryTurns
    :: StorePool
    -> Text
    -> Int
    -> IO (Either StoreError (Maybe SessionTurnPage))
loadRecentSessionHistoryTurns pool sessionKey limit =
    loadTurnPage pool sessionKey
        (Transaction.statement (sessionKey, fromIntegral (max 1 limit + 1))
            loadRecentHistoryTurnsStatement)
        (Transaction.statement sessionKey fullHistoryBoundsStatement)
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
        (Transaction.statement sessionKey displayHistoryBoundsStatement)
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
        (Transaction.statement sessionKey fullHistoryBoundsStatement)
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
        (Transaction.statement sessionKey displayHistoryBoundsStatement)
        (max 1 limit)
        PageAfter

loadSessionHistoryTurnsRange
    :: StorePool
    -> Text
    -> Int64
    -> Int
    -> IO (Either StoreError (Maybe SessionTurnPage))
loadSessionHistoryTurnsRange pool sessionKey start limit =
    loadTurnPage pool sessionKey
        (Transaction.statement
            (sessionKey, max 0 start, fromIntegral (max 1 limit + 1))
            loadHistoryTurnsRangeStatement)
        (Transaction.statement sessionKey fullHistoryBoundsStatement)
        (max 1 limit)
        PageAfter

loadSessionHistoryTurnsRangeBounded
    :: StorePool
    -> Text
    -> Int64
    -> Int64
    -> Int
    -> IO (Either StoreError (Maybe SessionTurnPage))
loadSessionHistoryTurnsRangeBounded pool sessionKey start endExclusive limit =
    loadTurnPage pool sessionKey
        (Transaction.statement
            ( sessionKey
            , max 0 start
            , max 0 endExclusive
            , fromIntegral (max 1 limit + 1)
            )
            loadHistoryTurnsRangeBoundedStatement)
        (Transaction.statement sessionKey fullHistoryBoundsStatement)
        (max 1 limit)
        PageAfter

loadSessionHistorySnapshot
    :: StorePool
    -> Text
    -> IO (Either StoreError (Maybe SessionHistorySnapshot))
loadSessionHistorySnapshot pool sessionKey =
    withSession pool $
        Transactions.transaction Transactions.RepeatableRead Transactions.Read do
            metadata <- Transaction.statement sessionKey loadMetadataStatement
            case metadata of
                Nothing -> pure Nothing
                Just value -> do
                    (start, total) <-
                        Transaction.statement sessionKey fullHistoryBoundsStatement
                    pure $ Just SessionHistorySnapshot
                        { sessionSnapshotMetadata = value
                        , sessionSnapshotStart = start
                        , sessionSnapshotTotal = total
                        }

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

-- | Load a visual history page. Compaction replacements stay in the
-- scrollback; only an explicit reset starts a new display generation.
loadTurnPage
    :: StorePool
    -> Text
    -> Transaction.Transaction (Vector.Vector TurnRow)
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

-- | List sessions inside the caller's exact organization-gateway boundary.
--
-- Gateway mode admits only rows for the reserved gateway connection whose
-- stored credential identity exactly matches the supplied identity. Direct
-- mode admits only identity-less rows outside the reserved gateway
-- connection. Both the boundary and cursor predicates are applied before
-- ordering and LIMIT so unauthorized rows cannot displace authorized results.
listSessionMetadataForBoundary
    :: StorePool
    -> Text
    -- ^ Reserved organization-gateway connection identifier.
    -> Maybe Text
    -- ^ Current gateway credential identity, or 'Nothing' for direct mode.
    -> Maybe SessionListCursor
    -> Int
    -> IO (Either StoreError SessionListPage)
listSessionMetadataForBoundary
        pool gatewayConnection gatewayIdentity cursor requestedLimit = do
    let
        limit = max 1 (min 100 requestedLimit)
        cursorUpdatedAt = (.sessionListCursorUpdatedAt) <$> cursor
        cursorKey = (.sessionListCursorKey) <$> cursor
    fmap (fmap (toSessionListPage limit)) $
        withSession pool $
            Transactions.transaction
                Transactions.RepeatableRead
                Transactions.Read $
                    Transaction.statement
                        ( gatewayConnection
                        , gatewayIdentity
                        , cursorUpdatedAt
                        , cursorKey
                        , fromIntegral (limit + 1)
                        )
                        listMetadataForBoundaryStatement

toSessionListPage :: Int -> [SessionMetadata] -> SessionListPage
toSessionListPage limit rows =
    let
        sessions = take limit rows
        nextCursor
            | length rows <= limit = Nothing
            | otherwise = case reverse sessions of
                [] -> Nothing
                metadata : _ ->
                    Just SessionListCursor
                        { sessionListCursorUpdatedAt =
                            metadata.sessionMetadataUpdatedAt
                        , sessionListCursorKey =
                            metadata.sessionMetadataKey
                        }
    in SessionListPage
        { sessionListPageSessions = sessions
        , sessionListPageNextCursor = nextCursor
        }

listSessionArchiveKeys
    :: StorePool
    -> IO (Either StoreError [Text])
listSessionArchiveKeys pool =
    withSession pool $
        Transactions.transaction Transactions.RepeatableRead Transactions.Read $
            Transaction.statement () listArchiveKeysStatement

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

-- | Search only sessions that belong to the caller's current gateway routing
-- boundary. The predicate is applied before ranking and LIMIT so matches from
-- other credentials cannot displace authorized results.
searchConversationTurnsForBoundary
    :: StorePool
    -> Text
    -- ^ Reserved organization-gateway connection identifier.
    -> Maybe Text
    -- ^ Current gateway credential identity, or 'Nothing' for direct mode.
    -> Text
    -> Int
    -> IO (Either StoreError [ConversationSearchResult])
searchConversationTurnsForBoundary
        pool gatewayConnection gatewayIdentity query limit =
    withSession pool $
        HasqlSession.statement
            ( query
            , gatewayConnection
            , gatewayIdentity
            , fromIntegral (max 1 (min 100 limit))
            )
            searchTurnsForBoundaryStatement

searchNativeConversations
    :: StorePool
    -> Text
    -> Int
    -> IO (Either StoreError [NativeConversationSearchResult])
searchNativeConversations pool query limit =
    withSession pool $
        HasqlSession.statement
            ( query
            , Nothing
            , Nothing
            , fromIntegral (max 1 (min 100 limit))
            )
            searchNativeConversationsStatement

-- | Native/sidebar search with the same exact credential boundary as CLI
-- search. The predicate is part of both candidate branches before ordering
-- and LIMIT, so another organization cannot displace or expose results.
searchNativeConversationsForBoundary
    :: StorePool
    -> Text
    -- ^ Reserved organization-gateway connection identifier.
    -> Maybe Text
    -- ^ Current gateway credential identity, or 'Nothing' for direct mode.
    -> Text
    -> Int
    -> IO (Either StoreError [NativeConversationSearchResult])
searchNativeConversationsForBoundary
        pool gatewayConnection gatewayIdentity query limit =
    withSession pool $
        HasqlSession.statement
            ( query
            , Just gatewayConnection
            , gatewayIdentity
            , fromIntegral (max 1 (min 100 limit))
            )
            searchNativeConversationsStatement

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
    , turnRowProviderTelemetry :: !(Maybe Text)
    , turnRowCanonicalItemCount :: !(Maybe Int64)
    }

loadStoredTurn :: TurnRow -> Transaction.Transaction (Either Text StoredTurn)
loadStoredTurn = loadStoredTurnWith AdaptiveSessionRead

loadStoredTurnWith
    :: SessionReadImplementation
    -> TurnRow
    -> Transaction.Transaction (Either Text StoredTurn)
loadStoredTurnWith implementation row = do
    items <- case implementation of
        AdaptiveSessionRead -> loadResponseItems row.turnRowId
        PerItemSessionRead -> loadResponseItemsPerItem row.turnRowId
    pure do
        decodedItems <- items
        (canonicalItems, displayItems) <-
            splitStoredTurnItems
                row.turnRowCanonicalItemCount
                decodedItems
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
                , sessionTurnItems = canonicalItems
                , sessionTurnDisplayItems = displayItems
                , sessionTurnUsage = usage
                , sessionTurnProviderTelemetry =
                    row.turnRowProviderTelemetry
                }
            }

-- Legacy rows predate the display-only channel, so every stored item is
-- canonical when the boundary is absent.
splitStoredTurnItems
    :: Maybe Int64
    -> [a]
    -> Either Text ([a], [a])
splitStoredTurnItems Nothing items = Right (items, [])
splitStoredTurnItems (Just canonicalCount) items
    | canonicalCount < 0 =
        Left "stored session turn has a negative canonical item count"
    | canonicalCount > fromIntegral (length items) =
        Left "stored session turn canonical item count exceeds stored items"
    | otherwise =
        Right (splitAt (fromIntegral canonicalCount) items)

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

searchNativeConversationsStatement
    :: Statement
        (Text, Maybe Text, Maybe Text, Int64)
        [NativeConversationSearchResult]
searchNativeConversationsStatement = mkStatement
    "WITH query AS (SELECT websearch_to_tsquery('english', $1) AS ts,\
    \ lower(btrim($1)) AS needle), candidates AS (\
    \ SELECT s.session_key,s.title,s.cwd,s.provider,s.model_id,s.updated_at,\
    \ s.archived_at IS NOT NULL,NULL::bigint,NULL::timestamptz,NULL::text,\
    \ NULL::text,NULL::text,CASE WHEN lower(s.title)=query.needle THEN 1000::float8\
    \ WHEN lower(s.title) LIKE query.needle || '%' THEN 800::float8\
    \ WHEN s.title ILIKE '%' || $1 || '%' THEN 600::float8\
    \ WHEN s.cwd ILIKE '%' || $1 || '%' THEN 400::float8 ELSE 300::float8 END\
    \ FROM harness.sessions s CROSS JOIN query WHERE s.deleted_at IS NULL\
    \ AND ($2 IS NULL OR (\
    \   ($3 IS NULL AND s.connection_id <> $2 AND s.gateway_identity IS NULL)\
    \   OR ($3 IS NOT NULL AND s.connection_id = $2 AND s.gateway_identity = $3)))\
    \ AND (\
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
    \ CROSS JOIN query WHERE s.deleted_at IS NULL\
    \ AND ($2 IS NULL OR (\
    \   ($3 IS NULL AND s.connection_id <> $2 AND s.gateway_identity IS NULL)\
    \   OR ($3 IS NOT NULL AND s.connection_id = $2 AND s.gateway_identity = $3)))\
    \ AND (\
    \ t.search_vector @@ query.ts OR t.user_text ILIKE '%' || $1 || '%' OR\
    \ t.assistant_text ILIKE '%' || $1 || '%'))\
    \ SELECT * FROM candidates ORDER BY 13 DESC,6 DESC,9 DESC NULLS LAST LIMIT $4"
    ( ((\(value, _, _, _) -> value)
        >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((\(_, value, _, _) -> value)
            >$< Encoders.param (Encoders.nullable Encoders.text))
        <> ((\(_, _, value, _) -> value)
            >$< Encoders.param (Encoders.nullable Encoders.text))
        <> ((\(_, _, _, value) -> value)
            >$< Encoders.param (Encoders.nonNullable Encoders.int8))
    )
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

fullHistoryBoundsStatement :: Statement Text (Int64, Int64)
fullHistoryBoundsStatement = mkStatement
    "WITH target AS (\
    \ SELECT session_id\
    \ FROM harness.sessions\
    \ WHERE session_key = $1 AND deleted_at IS NULL\
    \ )\
    \ SELECT COALESCE(min(t.turn_index), 0)::bigint,\
    \ count(t.turn_id)::bigint\
    \ FROM target\
    \ LEFT JOIN harness.session_turns t\
    \ ON t.session_id = target.session_id"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.singleRow $
        (,)
            <$> Decoders.column (Decoders.nonNullable Decoders.int8)
            <*> Decoders.column (Decoders.nonNullable Decoders.int8))
    True

loadLatestPromptEpochStatement
    :: Statement Text (Maybe SessionPromptEpoch)
loadLatestPromptEpochStatement = mkStatement
    "SELECT latest.epoch_index, latest.prompt_version, latest.created_at,\
    \ latest.provider, latest.connection_id, latest.model_id, latest.dialect,\
    \ latest.cwd_text, latest.instructions_text, latest.tools_text,\
    \ latest.generated_context_text, latest.grok_context_text,\
    \ latest.prompt_cache_key\
    \ FROM (\
    \   SELECT epoch.*\
    \   FROM harness.session_prompt_epochs epoch\
    \   JOIN harness.sessions session\
    \     ON session.session_id = epoch.session_id\
    \   WHERE session.session_key = $1 AND session.deleted_at IS NULL\
    \   ORDER BY epoch.epoch_index DESC\
    \   LIMIT 1\
    \ ) latest\
    \ WHERE latest.is_active"
    (Encoders.param (Encoders.nonNullable Encoders.text))
    (Decoders.rowMaybe $
        SessionPromptEpoch
            <$> Decoders.column (Decoders.nonNullable Decoders.int8)
            <*> (SessionPromptSnapshot
                <$> Decoders.column (Decoders.nonNullable Decoders.int4)
                <*> Decoders.column (Decoders.nonNullable Decoders.timestamptz)
                <*> Decoders.column (Decoders.nonNullable Decoders.text)
                <*> Decoders.column (Decoders.nonNullable Decoders.text)
                <*> Decoders.column (Decoders.nonNullable Decoders.text)
                <*> Decoders.column (Decoders.nonNullable Decoders.text)
                <*> Decoders.column (Decoders.nonNullable Decoders.text)
                <*> Decoders.column (Decoders.nonNullable Decoders.text)
                <*> Decoders.column (Decoders.nonNullable Decoders.text)
                <*> Decoders.column (Decoders.nullable Decoders.text)
                <*> Decoders.column (Decoders.nullable Decoders.text)
                <*> Decoders.column (Decoders.nonNullable Decoders.text)))
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

listMetadataForBoundaryStatement
    :: Statement
        (Text, Maybe Text, Maybe UTCTime, Maybe Text, Int64)
        [SessionMetadata]
listMetadataForBoundaryStatement = mkStatement
    (metadataSelectSql
        <> " WHERE deleted_at IS NULL\
           \ AND (\
           \   ($2 IS NULL\
           \     AND connection_id <> $1\
           \     AND gateway_identity IS NULL)\
           \   OR ($2 IS NOT NULL\
           \     AND connection_id = $1\
           \     AND gateway_identity = $2)\
           \ )\
           \ AND (\
           \   $3 IS NULL\
           \   OR updated_at < $3\
           \   OR (updated_at = $3 AND session_key > $4)\
           \ )\
           \ ORDER BY updated_at DESC, session_key ASC\
           \ LIMIT $5")
    ( ((\(value, _, _, _, _) -> value)
        >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((\(_, value, _, _, _) -> value)
            >$< Encoders.param (Encoders.nullable Encoders.text))
        <> ((\(_, _, value, _, _) -> value)
            >$< Encoders.param (Encoders.nullable Encoders.timestamptz))
        <> ((\(_, _, _, value, _) -> value)
            >$< Encoders.param (Encoders.nullable Encoders.text))
        <> ((\(_, _, _, _, value) -> value)
            >$< Encoders.param (Encoders.nonNullable Encoders.int8))
    )
    (Decoders.rowList metadataRow)
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

metadataSelectSql :: Text
metadataSelectSql =
    "SELECT session_key, session_schema_version, created_at, updated_at,\
    \ provider, connection_id, gateway_identity, model_id,\
    \ transport_model_id, dialect,\
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
    \ t.usage_output_tokens, t.usage_cached_tokens,\
    \ t.provider_telemetry_json, t.canonical_item_count\
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

loadHistoryTurnsRangeStatement
    :: Statement (Text, Int64, Int64) (Vector.Vector TurnRow)
loadHistoryTurnsRangeStatement = mkStatement
    (turnSelectSql
        <> " WHERE s.session_key = $1\
           \ AND t.turn_index >= $2\
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

loadHistoryTurnsRangeBoundedStatement
    :: Statement (Text, Int64, Int64, Int64) (Vector.Vector TurnRow)
loadHistoryTurnsRangeBoundedStatement = mkStatement
    (turnSelectSql
        <> " WHERE s.session_key = $1\
           \ AND t.turn_index >= $2\
           \ AND t.turn_index < $3\
           \ ORDER BY t.turn_index ASC\
           \ LIMIT $4")
    ( ((\(value, _, _, _) -> value)
        >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((\(_, value, _, _) -> value)
            >$< Encoders.param (Encoders.nonNullable Encoders.int8))
        <> ((\(_, _, value, _) -> value)
            >$< Encoders.param (Encoders.nonNullable Encoders.int8))
        <> ((\(_, _, _, value) -> value)
            >$< Encoders.param (Encoders.nonNullable Encoders.int8))
    )
    (Decoders.rowVector turnRowDecoder)
    True

loadHistoryTurnsBeforeStatement
    :: Statement (Text, Int64, Int64) (Vector.Vector TurnRow)
loadHistoryTurnsBeforeStatement = mkStatement
    (turnSelectSql
        <> " WHERE s.session_key = $1\
           \ AND t.turn_index < $2\
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

loadRecentHistoryTurnsStatement
    :: Statement (Text, Int64) (Vector.Vector TurnRow)
loadRecentHistoryTurnsStatement = mkStatement
    (turnSelectSql
        <> " WHERE s.session_key = $1\
           \ ORDER BY t.turn_index DESC\
           \ LIMIT $2")
    ( (fst >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> (snd >$< Encoders.param (Encoders.nonNullable Encoders.int8))
    )
    (Decoders.rowVector turnRowDecoder)
    True

loadRecentTurnsStatement
    :: Statement (Text, Int64) (Vector.Vector TurnRow)
loadRecentTurnsStatement = mkStatement
    (turnSelectSql
        <> " WHERE s.session_key = $1\
           \ AND t.turn_index >= "
        <> displayHistoryStartSql
        <> "\
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
           \ AND t.turn_index >= "
        <> displayHistoryStartSql
        <> "\
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
           \ AND t.turn_index >= "
        <> displayHistoryStartSql
        <> "\
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

-- | Visual history continues across compaction replacements. Only an explicit
-- reset (@/clear@, @/new@) hides earlier turns from scrollback. Model resume
-- context still clips at the latest replace-or-reset via
-- 'loadActiveTurnsStatement'.
displayHistoryStartSql :: Text
displayHistoryStartSql =
    "COALESCE((\
    \   SELECT checkpoint.turn_index\
    \   FROM harness.session_turns checkpoint\
    \   WHERE checkpoint.session_id = s.session_id\
    \     AND checkpoint.transcript_effect = 'reset'\
    \   ORDER BY checkpoint.turn_index DESC\
    \   LIMIT 1\
    \ ), 0)"

displayHistoryBoundsStatement :: Statement Text (Int64, Int64)
displayHistoryBoundsStatement = mkStatement
    "WITH target AS (\
    \ SELECT session_id\
    \ FROM harness.sessions\
    \ WHERE session_key = $1 AND deleted_at IS NULL\
    \ ), generation AS (\
    \ SELECT COALESCE((\
    \   SELECT t.turn_index\
    \   FROM harness.session_turns t\
    \   JOIN target ON target.session_id = t.session_id\
    \   WHERE t.transcript_effect = 'reset'\
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
    \ t.usage_input_tokens, t.usage_output_tokens, t.usage_cached_tokens,\
    \ t.provider_telemetry_json, t.canonical_item_count\
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
        <*> Decoders.column (Decoders.nullable Decoders.text)
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

searchTurnsForBoundaryStatement
    :: Statement
        (Text, Text, Maybe Text, Int64)
        [ConversationSearchResult]
searchTurnsForBoundaryStatement = mkStatement
    "WITH query AS (SELECT websearch_to_tsquery('english', $1) AS value)\
    \ SELECT s.session_key, t.turn_index, t.occurred_at,\
    \   t.user_text, t.assistant_text,\
    \   ts_rank_cd(t.search_vector, query.value)::float8\
    \ FROM harness.session_turns t\
    \ JOIN harness.sessions s ON s.session_id = t.session_id\
    \ CROSS JOIN query\
    \ WHERE s.deleted_at IS NULL\
    \ AND (\
    \   ($3 IS NULL\
    \     AND s.connection_id <> $2\
    \     AND s.gateway_identity IS NULL)\
    \   OR ($3 IS NOT NULL\
    \     AND s.connection_id = $2\
    \     AND s.gateway_identity = $3)\
    \ )\
    \ AND (\
    \   t.search_vector @@ query.value\
    \   OR t.user_text ILIKE '%' || $1 || '%'\
    \   OR t.assistant_text ILIKE '%' || $1 || '%'\
    \ )\
    \ ORDER BY ts_rank_cd(t.search_vector, query.value) DESC,\
    \   t.occurred_at DESC\
    \ LIMIT $4"
    ( ((\(value, _, _, _) -> value)
        >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((\(_, value, _, _) -> value)
            >$< Encoders.param (Encoders.nonNullable Encoders.text))
        <> ((\(_, _, value, _) -> value)
            >$< Encoders.param (Encoders.nullable Encoders.text))
        <> ((\(_, _, _, value) -> value)
            >$< Encoders.param (Encoders.nonNullable Encoders.int8))
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
        <*> Decoders.column (Decoders.nullable Decoders.text)
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
