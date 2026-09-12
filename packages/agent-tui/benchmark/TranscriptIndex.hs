{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Control.DeepSeq (NFData(..), force)
import Control.Exception (evaluate) -- safe-exceptions does not export evaluate.
import Control.Exception.Safe (bracket)
import Control.Monad (foldM, forM, forM_, unless)
import Data.Coerce (coerce)
import Data.IORef (IORef, newIORef, readIORef)
import qualified Data.IntMap.Strict as IntMap
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Foreign.StablePtr (freeStablePtr, newStablePtr)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import System.Exit (die)
import System.IO (BufferMode(LineBuffering), hSetBuffering, stdout)
import System.Mem (performGC)
import Text.Printf (printf)
import Text.Read (readMaybe)

-- Same representation as production BlockId, without importing the UI stack.
newtype BlockId = BlockId Int deriving (Eq, Ord)
instance NFData BlockId where rnf (BlockId value) = value `seq` ()

data Measurement = Measurement !Double !Double !Integer !Int

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    enabled <- getRTSStatsEnabled
    unless enabled (die "RTS statistics required: +RTS -T")
    arguments <- getArgs
    let (dimensions, requestedShape) = splitAt 3 arguments
    (size, repetitions, samples) <- case dimensions of
        [a, b, c]
            | Just n <- readMaybe a, n > 0
            , Just r <- readMaybe b, r > 0
            , Just s <- readMaybe c, s > 0 -> pure (n, r, s :: Int)
        _ -> die "usage: transcript-index-bench SIZE REPETITIONS SAMPLES [SHAPE] +RTS -T"
    printf "# size=%d repetitions=%d samples=%d\n" size repetitions samples
    putStrLn "workload,implementation,median_cpu_ms,median_elapsed_ms,median_allocated_bytes,checksum"
    let positive = [(i + 4096, i) | i <- [0 .. size - 1]]
        negative = [(-i - 4096, i) | i <- [0 .. size - 1]]
        mixed = [(if even i then i + 4096 else -i - 4096, i) | i <- [0 .. size - 1]]
        paged = zip (concat (reverse (pages (map fst negative)))) [0 ..]
        sparse = [(if even i then (i + 4096) * 104729 else -(i + 4096) * 104729, i)
                 | i <- [0 .. size - 1]]
        shapes = [("positive", positive), ("negative", negative), ("mixed", mixed),
                  ("paged", paged), ("sparse", sparse)]
    selected <- case requestedShape of
        [] -> pure shapes
        [name] | name `elem` map fst shapes -> pure (filter ((== name) . fst) shapes)
        _ -> die "shape must be positive, negative, mixed, paged, or sparse"
    forM_ selected $ \(label, input) -> do
        _ <- evaluate (force input)
        source <- newIORef input
        let old = mapBuild input
            new = intBuild input
        _ <- evaluate (force (old, new))
        unless ([(key, value) | (BlockId key, value) <- Map.toAscList old]
                == IntMap.toAscList new) (die "index equivalence failure")
        unless (all (\(key, _) -> Map.notMember (BlockId key) old)
                (freshEntries size input)) (die "appended identifiers overlap the input")
        oldReference <- newIORef old
        newReference <- newIORef new
        let queries = map fst input <> [0, minBound, maxBound]
        _ <- evaluate (force queries)
        queryReference <- newIORef queries
        forM_
            [ ("rebuild", runRepeated repetitions source mapRebuild,
                           runRepeated repetitions source intRebuild)
            , ("append", runRepeated repetitions source mapAppend,
                          runRepeated repetitions source intAppend)
            , ("lookup", runRepeated repetitions queryReference
                    (\keys -> readIORef oldReference >>= \index -> evaluate (mapLookup index keys)),
                         runRepeated repetitions queryReference
                    (\keys -> readIORef newReference >>= \index -> evaluate (intLookup index keys)))
            , ("retract", runRepeated repetitions oldReference (mapRetract size),
                           runRepeated repetitions newReference (intRetract size))
            , ("lifecycle", runRepeated repetitions source (mapLifecycle size),
                             runRepeated repetitions source (intLifecycle size))
            ] $ \(workload, baseline, candidate) -> do
                results <- forM [1 .. samples] $ \sample ->
                    if odd sample
                        then do a <- measure baseline; b <- measure candidate; pure (a, b)
                        else do b <- measure candidate; a <- measure baseline; pure (a, b)
                let original = map fst results
                    replacement = map snd results
                unless (map resultChecksum original == map resultChecksum replacement)
                    (die ("checksum mismatch: " <> workload))
                report (label <> "-" <> workload) "map" original
                report (label <> "-" <> workload) "intmap" replacement
        oldSizes <- forM [1 .. samples] $ \_ -> retainedBytes source mapBuild
        newSizes <- forM [1 .. samples] $ \_ -> retainedBytes source intBuild
        printf "# retained,%s,map,%d\n" label (median oldSizes)
        printf "# retained,%s,intmap,%d\n" label (median newSizes)

-- Newly loaded older pages receive lower IDs and are prepended. IDs descend
-- inside each 64-block fixture page but not across the retained window.
pages :: [a] -> [[a]]
pages [] = []
pages input = let (page, remaining) = splitAt 64 input in page : pages remaining

{-# NOINLINE mapBuild #-}
mapBuild :: [(Int, Int)] -> Map.Map BlockId Int
mapBuild = Map.fromList . coerce

{-# NOINLINE intBuild #-}
intBuild :: [(Int, Int)] -> IntMap.IntMap Int
intBuild = IntMap.fromList

mapChecksum :: Map.Map BlockId Int -> Int
mapChecksum = Map.foldlWithKey' (\total (BlockId key) value -> total + key + value) 0

intChecksum :: IntMap.IntMap Int -> Int
intChecksum = IntMap.foldlWithKey' (\total key value -> total + key + value) 0

{-# NOINLINE mapRebuild #-}
mapRebuild :: [(Int, Int)] -> IO Int
mapRebuild input = evaluate (mapChecksum (mapBuild input))

{-# NOINLINE intRebuild #-}
intRebuild :: [(Int, Int)] -> IO Int
intRebuild input = evaluate (intChecksum (intBuild input))

{-# NOINLINE mapAppend #-}
mapAppend :: [(Int, Int)] -> IO Int
mapAppend input = evaluate $ mapChecksum $
    foldl' (\index (key, value) -> Map.insert (BlockId key) value index) Map.empty input

{-# NOINLINE intAppend #-}
intAppend :: [(Int, Int)] -> IO Int
intAppend input = evaluate $ intChecksum $
    foldl' (\index (key, value) -> IntMap.insert key value index) IntMap.empty input

{-# NOINLINE mapLookup #-}
mapLookup :: Map.Map BlockId Int -> [Int] -> Int
mapLookup index = foldl' (\total key -> total + fromMaybe (-1) (Map.lookup (BlockId key) index)) 0

{-# NOINLINE intLookup #-}
intLookup :: IntMap.IntMap Int -> [Int] -> Int
intLookup index = foldl' (\total key -> total + fromMaybe (-1) (IntMap.lookup key index)) 0

-- Mirrors abort/retraction filtering by transcript position.
{-# NOINLINE mapRetract #-}
mapRetract :: Int -> Map.Map BlockId Int -> IO Int
mapRetract size index = evaluate (mapChecksum (Map.filter (< size `div` 2) index))

{-# NOINLINE intRetract #-}
intRetract :: Int -> IntMap.IntMap Int -> IO Int
intRetract size index = evaluate (intChecksum (IntMap.filter (< size `div` 2) index))

freshEntries :: Int -> [(Int, Int)] -> [(Int, Int)]
freshEntries size input =
    let offset = 1 + foldl' (\largest (key, _) -> max largest (abs key)) 0 input
    in [(key + signum key * offset, value + size) | (key, value) <- take 20 input]

-- Build a window, append fresh entries, navigate, retract its second half, then
-- rebuild after removing a middle block (the existing deletion path).
{-# NOINLINE mapLifecycle #-}
mapLifecycle :: Int -> [(Int, Int)] -> IO Int
mapLifecycle size input = evaluate $
    let initial = mapBuild input
        added = foldl' (\index (key, value) -> Map.insert (BlockId key) value index)
            initial (freshEntries size input)
        navigation = mapLookup added (map fst input)
        retracted = Map.filter (< size `div` 2) added
        remaining = [(key, position) | (position, (key, _)) <-
            zip [0 ..] (filter ((/= size `div` 4) . snd) (take (size `div` 2) input))]
        rebuilt = mapBuild remaining
    in mapChecksum initial + mapChecksum added + navigation
        + mapChecksum retracted + mapChecksum rebuilt

{-# NOINLINE intLifecycle #-}
intLifecycle :: Int -> [(Int, Int)] -> IO Int
intLifecycle size input = evaluate $
    let initial = intBuild input
        added = foldl' (\index (key, value) -> IntMap.insert key value index)
            initial (freshEntries size input)
        navigation = intLookup added (map fst input)
        retracted = IntMap.filter (< size `div` 2) added
        remaining = [(key, position) | (position, (key, _)) <-
            zip [0 ..] (filter ((/= size `div` 4) . snd) (take (size `div` 2) input))]
        rebuilt = intBuild remaining
    in intChecksum initial + intChecksum added + navigation
        + intChecksum retracted + intChecksum rebuilt

{-# NOINLINE runRepeated #-}
runRepeated :: Int -> IORef input -> (input -> IO Int) -> IO Int
runRepeated repetitions reference action =
    foldM (\ !total _ -> do
        input <- readIORef reference
        result <- action input
        pure $! total + result) 0 [1 .. repetitions]

measure :: IO Int -> IO Measurement
measure action = do
    performGC
    before <- getRTSStats
    cpuStart <- getCPUTime
    wallStart <- getMonotonicTimeNSec
    result <- action >>= evaluate
    wallEnd <- getMonotonicTimeNSec
    cpuEnd <- getCPUTime
    performGC
    after <- getRTSStats
    pure (Measurement (fromIntegral (cpuEnd - cpuStart) / 1e9)
        (fromIntegral (wallEnd - wallStart) / 1e6)
        (toInteger (allocated_bytes after) - toInteger (allocated_bytes before)) result)

retainedBytes :: NFData result => IORef input -> (input -> result) -> IO Integer
retainedBytes reference build = do
    performGC
    before <- getRTSStats
    input <- readIORef reference
    result <- evaluate (force (build input))
    bracket (newStablePtr result) freeStablePtr $ \_ -> do
        performGC
        after <- getRTSStats
        pure (toInteger (gcdetails_live_bytes (gc after)) - toInteger (gcdetails_live_bytes (gc before)))

resultChecksum :: Measurement -> Int
resultChecksum (Measurement _ _ _ result) = result

report :: String -> String -> [Measurement] -> IO ()
report workload implementation measurements = do
    let checksums = map resultChecksum measurements
    expected <- case checksums of
        [] -> die "empty measurement list"
        first : _ -> pure first
    unless (all (== expected) checksums) (die "unstable checksum")
    printf "%s,%s,%.6f,%.6f,%d,%d\n" workload implementation
        (median [cpu | Measurement cpu _ _ _ <- measurements])
        (median [wall | Measurement _ wall _ _ <- measurements])
        (median [bytes | Measurement _ _ bytes _ <- measurements])
        expected

median :: Ord a => [a] -> a
median values = sort values !! (length values `div` 2)
