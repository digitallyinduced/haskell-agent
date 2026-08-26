{-# LANGUAGE NumericUnderscores #-}

module Main (main) where

import Agent.Store.Postgres
    ( Store
    , closeStore
    , defaultManagedPostgresConfig
    , openStore
    , provisioningPool
    , trustedPool
    )
import Agent.Store.Postgres.Connection
    ( withSession
    )
import Agent.Store.Postgres.Managed (stopManagedPostgres)
import Agent.Store.Postgres.Session
import Agent.Store.Types (StoreError(..), renderStoreError)
import Control.Exception (evaluate)
import Control.Exception.Safe (bracket, finally)
import Control.Monad (forM, forM_, void)
import Data.Functor.Contravariant ((>$<))
import Data.Int (Int32, Int64)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (getCurrentTime)
import qualified Data.Vector as Vector
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats
    ( RTSStats(..)
    , getRTSStats
    , getRTSStatsEnabled
    )
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Session as Session
import Hasql.Statement (Statement)
import qualified Hasql.Statement as Statement
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import System.Exit (die)
import System.IO.Temp (withSystemTempDirectory)
import System.Mem (performGC)
import Text.Printf (printf)

data Workload
    = FullTranscript
    | ActiveTranscript
    | RecentPage
    | ListRows
    | VectorRows
    deriving (Eq, Show)

data Sample = Sample
    { elapsedMillis :: !Double
    , cpuMillis :: !Double
    , allocatedBytes :: !Integer
    , checksum :: !Int
    }

data SeedParams = SeedParams
    { seedSessionKey :: !Text
    , seedTurnCount :: !Int64
    , seedCheckpoint :: !Int64
    , seedPayloadBytes :: !Int32
    }

main :: IO ()
main = do
    enabled <- getRTSStatsEnabled
    if enabled
        then pure ()
        else die "RTS statistics are disabled; run with +RTS -T"
    (turnCounts, payloadBytes, sampleCount) <- parseArguments =<< getArgs
    withSystemTempDirectory "hah" \stateDirectory -> do
        let config = defaultManagedPostgresConfig stateDirectory ""
        (bracket
            (openStore config >>= either (die . Text.unpack . renderStoreError) pure)
            closeStore
            \store -> runMatrix store turnCounts payloadBytes sampleCount)
            `finally` void (stopManagedPostgres config)

parseArguments :: [String] -> IO ([Int], Int, Int)
parseArguments = \case
    [] -> pure ([1_000, 5_000, 10_000], 4 * 1024, 5)
    [turnCountsArg, payloadArg, samplesArg] -> do
        turnCounts <- traverse (parsePositive "turn count")
            (splitCommas turnCountsArg)
        payload <- parsePositive "payload bytes" payloadArg
        samples <- parsePositive "sample count" samplesArg
        pure (turnCounts, payload, samples)
    _ ->
        die $
            "usage: session-history-paging-bench "
                <> "[TURN_COUNTS_CSV PAYLOAD_BYTES SAMPLES]"

splitCommas :: String -> [String]
splitCommas input =
    case break (== ',') input of
        (value, []) -> [value]
        (value, _ : rest) -> value : splitCommas rest

parsePositive :: String -> String -> IO Int
parsePositive label raw =
    case reads raw of
        [(value, "")]
            | value > 0 -> pure value
        _ -> die ("invalid " <> label <> ": " <> raw)

runMatrix :: Store -> [Int] -> Int -> Int -> IO ()
runMatrix store turnCounts payloadBytes sampleCount = do
    putStrLn $
        "turns,payload_bytes,active_turns,workload,elapsed_ms,"
            <> "cpu_ms,allocated_bytes,checksum"
    forM_ turnCounts \turnCount -> do
        let activeTurns = min 80 turnCount
            sessionKey = "history-bench-" <> Text.pack (show turnCount)
        seedBenchmarkSession
            store
            sessionKey
            turnCount
            activeTurns
            payloadBytes
        forM_
            [ FullTranscript
            , ActiveTranscript
            , RecentPage
            , ListRows
            , VectorRows
            ]
            \workload -> do
            _ <- measure (workloadAction store sessionKey workload)
            samples <- forM [1 .. sampleCount] \_ ->
                measure (workloadAction store sessionKey workload)
            printSample
                turnCount
                payloadBytes
                activeTurns
                workload
                (median samples)

seedBenchmarkSession
    :: Store
    -> Text
    -> Int
    -> Int
    -> Int
    -> IO ()
seedBenchmarkSession store sessionKey turnCount activeTurns payloadBytes = do
    now <- getCurrentTime
    let metadata = SessionMetadata
            { sessionMetadataKey = sessionKey
            , sessionMetadataVersion = 1
            , sessionMetadataCreatedAt = now
            , sessionMetadataUpdatedAt = now
            , sessionMetadataProvider = "openai"
            , sessionMetadataConnection = "openai"
            , sessionMetadataModel = "benchmark"
            , sessionMetadataTransportModel = Nothing
            , sessionMetadataDialect = "codex"
            , sessionMetadataLegacyTarget = Nothing
            , sessionMetadataCwd = "/benchmark"
            , sessionMetadataEffort = "medium"
            , sessionMetadataTitle = "Session history benchmark"
            , sessionMetadataTitleIsManual = False
            , sessionMetadataTitleRefreshIndex = 0
            , sessionMetadataTitleUserTurns = fromIntegral turnCount
            , sessionMetadataLastResponseId = Nothing
            , sessionMetadataInputTokens = 0
            , sessionMetadataOutputTokens = 0
            , sessionMetadataCachedTokens = 0
            , sessionMetadataLastRecap = Nothing
            , sessionMetadataLastTurnSummary = Nothing
            , sessionMetadataLastRecapMainTurns = 0
            }
        checkpoint = fromIntegral (turnCount - activeTurns)
        params = SeedParams
            { seedSessionKey = sessionKey
            , seedTurnCount = fromIntegral turnCount
            , seedCheckpoint = checkpoint
            , seedPayloadBytes = fromIntegral payloadBytes
            }
    createSession (trustedPool store) metadata
        >>= requireStore "create benchmark session"
        >>= \created ->
            if created
                then pure ()
                else die ("benchmark session already exists: " <> Text.unpack sessionKey)
    withSession
        (provisioningPool store)
        (Session.statement params seedSessionStatement)
        >>= requireStore "seed benchmark session"
        >>= \inserted ->
            if inserted == fromIntegral turnCount
                then pure ()
                else
                    die $
                        "seeded "
                            <> show inserted
                            <> " turns, expected "
                            <> show turnCount

workloadAction
    :: Store
    -> Text
    -> Workload
    -> IO (Either StoreError Int)
workloadAction store sessionKey = \case
    FullTranscript ->
        fmap (fmap checksumStoredSession . joinMaybe "full transcript") $
            loadSession (trustedPool store) sessionKey
    ActiveTranscript ->
        fmap (fmap checksumStoredSession . joinMaybe "active transcript") $
            loadActiveSession (trustedPool store) sessionKey
    RecentPage ->
        fmap (fmap checksumPage . joinMaybe "recent page") $
            loadRecentSessionTurns (trustedPool store) sessionKey 80
    ListRows ->
        fmap (fmap checksumListRows) $
            withSession
                (trustedPool store)
                (Session.statement sessionKey listRowsStatement)
    VectorRows ->
        fmap (fmap checksumVectorRows) $
            withSession
                (trustedPool store)
                (Session.statement sessionKey vectorRowsStatement)

joinMaybe
    :: Text
    -> Either StoreError (Maybe a)
    -> Either StoreError a
joinMaybe label = \case
    Left err -> Left err
    Right Nothing ->
        Left (errorStore ("missing " <> label))
    Right (Just value) -> Right value

errorStore :: Text -> StoreError
errorStore = StoreDataError

measure :: IO (Either StoreError Int) -> IO Sample
measure action = do
    performGC
    beforeStats <- getRTSStats
    beforeCpu <- getCPUTime
    beforeElapsed <- getMonotonicTimeNSec
    result <- action >>= requireStore "run benchmark query"
    forced <- evaluate result
    afterElapsed <- getMonotonicTimeNSec
    afterCpu <- getCPUTime
    -- Allocation counters are published at collection boundaries. Keep the
    -- timed query separate, then collect before reading the allocation delta.
    performGC
    afterStats <- getRTSStats
    pure Sample
        { elapsedMillis =
            fromIntegral (afterElapsed - beforeElapsed) / 1.0e6
        , cpuMillis =
            fromIntegral (afterCpu - beforeCpu) / 1.0e9
        , allocatedBytes =
            fromIntegral
                (afterStats.allocated_bytes - beforeStats.allocated_bytes)
        , checksum = forced
        }

requireStore :: String -> Either StoreError a -> IO a
requireStore label =
    either
        (\err ->
            die $
                label
                    <> ": "
                    <> Text.unpack (renderStoreError err))
        pure

checksumStoredSession :: StoredSession -> Int
checksumStoredSession stored =
    foldl'
        checksumTurn
        (Text.length stored.storedMetadata.sessionMetadataTitle)
        stored.storedTurns

checksumPage :: SessionTurnPage -> Int
checksumPage page =
    foldl'
        checksumTurn
        (fromIntegral
            (page.sessionPageGenerationStart + page.sessionPageTotal))
        page.sessionPageTurns

checksumTurn :: Int -> StoredTurn -> Int
checksumTurn current stored =
    let turn = stored.storedTurn
    in current
        + fromIntegral stored.storedTurnIndex
        + Text.length turn.sessionTurnUserText
        + maybe 0 Text.length turn.sessionTurnAssistantText
        + maybe 0 Text.length turn.sessionTurnError
        + maybe 0 Text.length turn.sessionTurnResponseId
        + length turn.sessionTurnItems

checksumListRows :: [(Int64, Maybe Text)] -> Int
checksumListRows =
    foldl' checksumRow 0

checksumVectorRows :: Vector.Vector (Int64, Maybe Text) -> Int
checksumVectorRows =
    Vector.foldl' checksumRow 0

checksumRow :: Int -> (Int64, Maybe Text) -> Int
checksumRow current (turnIndex, assistantText) =
    current + fromIntegral turnIndex + maybe 0 Text.length assistantText

median :: [Sample] -> Sample
median samples =
    Sample
        { elapsedMillis = middle (sort (map (.elapsedMillis) samples))
        , cpuMillis = middle (sort (map (.cpuMillis) samples))
        , allocatedBytes = middle (sort (map (.allocatedBytes) samples))
        , checksum = middle (sort (map (.checksum) samples))
        }
  where
    middle values = values !! (length values `div` 2)

printSample :: Int -> Int -> Int -> Workload -> Sample -> IO ()
printSample turnCount payloadBytes activeTurns workload sample =
    printf
        "%d,%d,%d,%s,%.3f,%.3f,%d,%d\n"
        turnCount
        payloadBytes
        activeTurns
        (case workload of
            FullTranscript -> "full"
            ActiveTranscript -> "active"
            RecentPage -> "recent"
            ListRows -> "list-rows"
            VectorRows -> "vector-rows"
            :: String)
        sample.elapsedMillis
        sample.cpuMillis
        sample.allocatedBytes
        sample.checksum

seedSessionStatement :: Statement SeedParams Int64
seedSessionStatement =
    Statement.preparable
        "WITH target AS (\
        \ SELECT session_id\
        \ FROM harness.sessions\
        \ WHERE session_key = $1 AND deleted_at IS NULL\
        \ ), generated AS (\
        \ SELECT turn_index, pg_catalog.uuidv7() AS event_id\
        \ FROM generate_series(0::bigint, $2 - 1) AS turn_index\
        \ ), inserted_events AS (\
        \ INSERT INTO harness.session_events (\
        \   event_id, session_id, sequence, event_kind, occurred_at\
        \ )\
        \ SELECT generated.event_id, target.session_id,\
        \   generated.turn_index + 1, 'turn.appended', now()\
        \ FROM generated CROSS JOIN target\
        \ RETURNING event_id, session_id, sequence\
        \ ), inserted_turns AS (\
        \ INSERT INTO harness.session_turns (\
        \   session_id, event_id, turn_index, event_sequence, occurred_at,\
        \   user_text, assistant_text, transcript_effect\
        \ )\
        \ SELECT inserted_events.session_id, inserted_events.event_id,\
        \   inserted_events.sequence - 1, inserted_events.sequence, now(),\
        \   'prompt-' || (inserted_events.sequence - 1)::text,\
        \   repeat('x', $4),\
        \   CASE WHEN inserted_events.sequence - 1 = $3\
        \     THEN 'replace' ELSE 'append' END\
        \ FROM inserted_events\
        \ RETURNING turn_id\
        \ )\
        \ UPDATE harness.sessions\
        \ SET next_event_sequence = $2 + 1, next_turn_index = $2\
        \ WHERE session_key = $1\
        \ RETURNING (SELECT count(*)::bigint FROM inserted_turns)"
        ( ((.seedSessionKey)
            >$< Encoders.param (Encoders.nonNullable Encoders.text))
            <> ((.seedTurnCount)
                >$< Encoders.param (Encoders.nonNullable Encoders.int8))
            <> ((.seedCheckpoint)
                >$< Encoders.param (Encoders.nonNullable Encoders.int8))
            <> ((.seedPayloadBytes)
                >$< Encoders.param (Encoders.nonNullable Encoders.int4))
        )
        (Decoders.singleRow $
            Decoders.column (Decoders.nonNullable Decoders.int8))

listRowsStatement :: Statement Text [(Int64, Maybe Text)]
listRowsStatement =
    Statement.preparable
        benchmarkRowsSql
        (Encoders.param (Encoders.nonNullable Encoders.text))
        (Decoders.rowList benchmarkRowDecoder)

vectorRowsStatement :: Statement Text (Vector.Vector (Int64, Maybe Text))
vectorRowsStatement =
    Statement.preparable
        benchmarkRowsSql
        (Encoders.param (Encoders.nonNullable Encoders.text))
        (Decoders.rowVector benchmarkRowDecoder)

benchmarkRowsSql :: Text
benchmarkRowsSql =
    "SELECT t.turn_index, t.assistant_text\
    \ FROM harness.session_turns t\
    \ JOIN harness.sessions s ON s.session_id = t.session_id\
    \ WHERE s.session_key = $1 AND s.deleted_at IS NULL\
    \ ORDER BY t.turn_index ASC"

benchmarkRowDecoder :: Decoders.Row (Int64, Maybe Text)
benchmarkRowDecoder =
    (,)
        <$> Decoders.column (Decoders.nonNullable Decoders.int8)
        <*> Decoders.column (Decoders.nullable Decoders.text)
