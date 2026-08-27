{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- Compare fullscreen history lookups with and without the maintained indexes.
-- Lookup indexes are built before the timed interval; maintenance measures the
-- additional block->cursor index construction performed by the new window.
module Main (main) where

import Control.Exception (evaluate)
import Control.Monad (forM_)
import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats (RTSStats (..), getRTSStats, getRTSStatsEnabled)
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import System.Exit (die)
import System.Mem (performGC)
import Text.Printf (printf)

data Turn = Turn
    { turnCursor :: !Int
    , turnBlocks :: !(Seq Int)
    }

data LegacyIndexes = LegacyIndexes
    { legacyTurnsByCursor :: !(Map.Map Int Turn)
    , legacyBlocksById :: !(Map.Map Int Int)
    }

data IndexedIndexes = IndexedIndexes
    { indexedTurnsByCursor :: !(Map.Map Int Turn)
    , indexedBlocksById :: !(Map.Map Int Int)
    , indexedBlockCursors :: !(Map.Map Int Int)
    }

data State = State
    { stateTurns :: !(Seq Turn)
    , stateQueries :: ![Int]
    , stateLegacy :: !LegacyIndexes
    , stateIndexed :: !IndexedIndexes
    }

data Sample = Sample
    { sampleElapsed :: !Double
    , sampleCpu :: !Double
    , sampleAllocated :: !Integer
    , sampleChecksum :: !Int
    }

data Workload = Configured | Stress
data WorkloadKind
    = ConfiguredLookupLegacy
    | ConfiguredLookupIndexed
    | ConfiguredMaintenanceLegacy
    | ConfiguredMaintenanceIndexed
    | StressLookupLegacy
    | StressLookupIndexed
    | StressMaintenanceLegacy
    | StressMaintenanceIndexed

data Operation = Lookup | Maintenance

main :: IO ()
main = do
    enabled <- getRTSStatsEnabled
    if not enabled then die "run with +RTS -T" else pure ()
    workload <- getArgs >>= parse
    printf "workload,operation,implementation,turns,blocks,elapsed_ms,cpu_ms,allocated_bytes,checksum\n"
    forM_ (modes workload) \(kind, operation, implementation) -> do
        let (turnCount, blocksPerTurn) = bounds kind
            -- Keep eight prepared states outside the measured region, while
            -- retaining seven samples for the reported medians.
            states =
                [ buildState sample turnCount blocksPerTurn
                | sample <- [1 .. 8 :: Int]
                ]
        samples <- mapM
            (\(sample, state) ->
                measure operation implementation sample state)
            (zip [1 .. 7 :: Int] states)
        let middle values = sort values !! (length values `div` 2)
            elapsed = middle (map (.sampleElapsed) samples)
            cpu = middle (map (.sampleCpu) samples)
            allocated = middle (map (.sampleAllocated) samples)
            checksum = (last samples).sampleChecksum
        printf "%s,%s,%s,%d,%d,%.3f,%.3f,%d,%d\n"
            (showWorkload kind) (showOperation operation) implementation
            turnCount (turnCount * blocksPerTurn) elapsed cpu allocated checksum

parse :: [String] -> IO Workload
parse ["configured"] = pure Configured
parse ["stress"] = pure Stress
parse _ = die "usage: history-cursor-index-bench configured|stress +RTS -T"

showWorkload :: WorkloadKind -> String
showWorkload ConfiguredLookupLegacy = "configured"
showWorkload ConfiguredLookupIndexed = "configured"
showWorkload ConfiguredMaintenanceLegacy = "configured"
showWorkload ConfiguredMaintenanceIndexed = "configured"
showWorkload StressLookupLegacy = "stress"
showWorkload StressLookupIndexed = "stress"
showWorkload StressMaintenanceLegacy = "stress"
showWorkload StressMaintenanceIndexed = "stress"

showOperation :: Operation -> String
showOperation Lookup = "lookup"
showOperation Maintenance = "maintenance"

bounds :: WorkloadKind -> (Int, Int)
bounds ConfiguredLookupLegacy = (200, 6)
bounds ConfiguredLookupIndexed = (200, 6)
bounds ConfiguredMaintenanceLegacy = (200, 6)
bounds ConfiguredMaintenanceIndexed = (200, 6)
bounds StressLookupLegacy = (2000, 12)
bounds StressLookupIndexed = (2000, 12)
bounds StressMaintenanceLegacy = (2000, 12)
bounds StressMaintenanceIndexed = (2000, 12)

modes :: Workload -> [(WorkloadKind, Operation, String)]
modes Configured =
    [ (ConfiguredLookupLegacy, Lookup, "legacy")
    , (ConfiguredLookupIndexed, Lookup, "indexed")
    , (ConfiguredMaintenanceLegacy, Maintenance, "legacy")
    , (ConfiguredMaintenanceIndexed, Maintenance, "indexed")
    ]
modes Stress =
    [ (StressLookupLegacy, Lookup, "legacy")
    , (StressLookupIndexed, Lookup, "indexed")
    , (StressMaintenanceLegacy, Maintenance, "legacy")
    , (StressMaintenanceIndexed, Maintenance, "indexed")
    ]

measure :: Operation -> String -> Int -> State -> IO Sample
measure operation implementation sample state = do
    -- Correctness is checked before timing, so the timed path is only the
    -- operation under comparison.
    case operation of
        Lookup -> do
            let reference = legacyLookup state.stateTurns state.stateQueries
                candidate = indexedLookup state.stateIndexed state.stateQueries
            _ <- evaluate
                (if reference == candidate then () else error "legacy/indexed mismatch")
            pure ()
        Maintenance -> do
            let reference = maintenanceLegacy state.stateTurns
                candidate = maintenanceIndexed state.stateTurns
            _ <- evaluate
                (if reference == candidate then () else error "maintenance mismatch")
            pure ()
    -- Touch the prepared state and sample to prevent state hoisting/elision.
    _ <- evaluate
        (sample + Seq.length state.stateTurns + length state.stateQueries
            + forceLegacy state.stateLegacy
            + forceIndexed state.stateIndexed)
    performGC
    beforeStats <- getRTSStats
    beforeCpu <- getCPUTime
    beforeElapsed <- getMonotonicTimeNSec
    let result = case operation of
            Lookup -> case implementation of
                "legacy" -> legacyLookup state.stateTurns state.stateQueries
                _ -> indexedLookup state.stateIndexed state.stateQueries
            Maintenance -> case implementation of
                "legacy" -> maintenanceLegacy state.stateTurns
                _ -> maintenanceIndexed state.stateTurns
    !checksum <- evaluate result
    afterElapsed <- getMonotonicTimeNSec
    afterCpu <- getCPUTime
    performGC
    afterStats <- getRTSStats
    _ <- evaluate checksum
    pure Sample
        { sampleElapsed = fromIntegral (afterElapsed - beforeElapsed) / 1.0e6
        , sampleCpu = fromIntegral (afterCpu - beforeCpu) / 1.0e9
        , sampleAllocated =
            fromIntegral (afterStats.allocated_bytes - beforeStats.allocated_bytes)
        , sampleChecksum = checksum
        }

buildState :: Int -> Int -> Int -> State
buildState seed turnCount blocksPerTurn =
    let turns = buildTurns seed turnCount blocksPerTurn
    in State turns
        (buildQueries seed turnCount blocksPerTurn)
        (buildLegacyIndexes turns)
        (buildIndexedIndexes turns)

buildTurns :: Int -> Int -> Int -> Seq Turn
buildTurns seed turnCount blocksPerTurn =
    Seq.fromList
        [ Turn cursor
            (Seq.fromList
                [ seed * turnCount * blocksPerTurn
                    + cursor * blocksPerTurn + block
                | block <- [0 .. blocksPerTurn - 1]
                ])
        | cursor <- [0 .. turnCount - 1]
        ]

buildQueries :: Int -> Int -> Int -> [Int]
buildQueries seed turnCount blocksPerTurn =
    [ seed * turnCount * blocksPerTurn
        + ((index * 7919 + seed * 104729)
            `mod` (turnCount * blocksPerTurn))
    | index <- [0 .. turnCount * blocksPerTurn * 2 - 1]
    ]

buildLegacyIndexes :: Seq Turn -> LegacyIndexes
buildLegacyIndexes turns =
    LegacyIndexes
        (Map.fromList [(turn.turnCursor, turn) | turn <- Foldable.toList turns])
        (Map.fromList
            [ (blockId, turn.turnCursor)
            | turn <- Foldable.toList turns
            , blockId <- Foldable.toList turn.turnBlocks
            ])

buildIndexedIndexes :: Seq Turn -> IndexedIndexes
buildIndexedIndexes turns =
    let old = buildLegacyIndexes turns
    in IndexedIndexes old.legacyTurnsByCursor old.legacyBlocksById
        (Map.fromList
            [ (blockId, turn.turnCursor)
            | turn <- Foldable.toList turns
            , blockId <- Foldable.toList turn.turnBlocks
            ])

legacyLookup :: Seq Turn -> [Int] -> Int
legacyLookup turns queries =
    let base = snd (Foldable.foldl' step (0, 0) queries)
        anchor = maybe (-1) (.turnCursor) (lastMay (Foldable.toList turns))
        flashing = length
            [ block | block <- queries
            , Just _ <- [findCursor block (Foldable.toList turns)]
            , block `mod` 7 == 0
            ]
        merged = length
            [ block | block <- queries
            , Just _ <- [findCursor block (Foldable.toList turns)] ]
    in base * 31 + anchor * 17 + flashing * 7 + merged
  where
    step (!position, !checksum) blockId =
        let contribution = maybe (-1) id (findCursor blockId (Foldable.toList turns))
        in (position + 1, checksum * 33 + contribution + position)
    findCursor _ [] = Nothing
    findCursor blockId (turn : rest)
        | blockId `elem` Foldable.toList turn.turnBlocks = Just turn.turnCursor
        | otherwise = findCursor blockId rest
    lastMay [] = Nothing
    lastMay xs = Just (last xs)

indexedLookup :: IndexedIndexes -> [Int] -> Int
indexedLookup indexes queries =
    let byBlock = indexes.indexedBlocksById
        byCursor = indexes.indexedTurnsByCursor
        base = snd (Foldable.foldl'
            (\(!position, !checksum) blockId ->
                let contribution = Map.findWithDefault (-1) blockId byBlock
                in (position + 1, checksum * 33 + contribution + position))
            (0, 0) queries)
        anchor = maybe (-1) (.turnCursor) (snd <$> Map.lookupMax byCursor)
        flashing = length
            [ blockId | blockId <- queries
            , Map.member blockId byBlock, blockId `mod` 7 == 0 ]
        merged = length [blockId | blockId <- queries, Map.member blockId byBlock]
    in base * 31 + anchor * 17 + flashing * 7 + merged

-- Both paths construct exactly the two maps maintained by the old window.
-- The indexed path additionally forces blockCursors, making maintenance cost
-- visible without changing the canonical checksum.
maintenanceLegacy :: Seq Turn -> Int
maintenanceLegacy turns =
    let indexes = buildLegacyIndexes turns
        canonical = forceLegacy indexes
        extra = Map.foldlWithKey'
            (\acc block cursor -> acc + block + cursor)
            0 indexes.legacyBlocksById
    in canonical * 31 + extra

maintenanceIndexed :: Seq Turn -> Int
maintenanceIndexed turns =
    let indexes = buildIndexedIndexes turns
        canonical = forceLegacy
            (LegacyIndexes indexes.indexedTurnsByCursor indexes.indexedBlocksById)
        extra = Map.foldlWithKey' (\acc block cursor -> acc + block + cursor)
            0 indexes.indexedBlockCursors
    in canonical * 31 + extra

forceLegacy :: LegacyIndexes -> Int
forceLegacy indexes =
    let turns = Map.foldlWithKey'
            (\acc cursor turn -> acc + cursor + Foldable.length turn.turnBlocks)
            0 indexes.legacyTurnsByCursor
    in Map.foldlWithKey'
        (\acc block cursor -> acc + block + cursor)
        turns indexes.legacyBlocksById

forceIndexed :: IndexedIndexes -> Int
forceIndexed indexes =
    forceLegacy
        (LegacyIndexes indexes.indexedTurnsByCursor indexes.indexedBlocksById)
        + Map.foldlWithKey'
            (\acc block cursor -> acc + block + cursor)
            0 indexes.indexedBlockCursors

sort :: Ord a => [a] -> [a]
sort [] = []
sort (x : xs) = sort [y | y <- xs, y <= x] <> [x] <> sort [y | y <- xs, y > x]
