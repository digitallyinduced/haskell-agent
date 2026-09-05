module Main (main) where

import Agent.CLI.ComputerUse
    ( AccessibilityObservation(..)
    , AccessibilitySnapshot(..)
    , advanceAccessibilityObservation
    , initialAccessibilityDeltaState
    )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats (RTSStats(..), getRTSStats, getRTSStatsEnabled)
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import System.Exit (die)
import System.Mem (performGC)
import Text.Printf (printf)

data Workload = Static | SmallChange | HighChurn
    deriving (Eq, Show)

data Policy = AlwaysFull | RevisionedDelta
    deriving (Eq, Show)

data Sample = Sample
    { sampleElapsedMillis :: !Double
    , sampleCpuMillis :: !Double
    , sampleAllocatedBytes :: !Integer
    , sampleOutputBytes :: !Integer
    }

main :: IO ()
main = do
    statsEnabled <- getRTSStatsEnabled
    if statsEnabled
        then pure ()
        else die "RTS statistics are disabled; run with +RTS -T"
    getArgs >>= \case
        [] -> runMatrix
        [workloadArg, nodesArg, revisionsArg, samplesArg] -> do
            workload <- parseWorkload workloadArg
            nodes <- parsePositive "nodes" nodesArg
            revisions <- parsePositive "revisions" revisionsArg
            sampleCount <- parsePositive "samples" samplesArg
            putStrLn csvHeader
            runScenario sampleCount revisions nodes workload
        _ -> die $
            "usage: accessibility-delta-bench [WORKLOAD NODES REVISIONS SAMPLES]\n"
                <> "workloads: static, small-change, high-churn\n"
                <> "output: " <> csvHeader

runMatrix :: IO ()
runMatrix = do
    putStrLn csvHeader
    mapM_ (\nodes ->
        mapM_ (runScenario 5 24 nodes) [Static, SmallChange, HighChurn])
        [100, 500, 1000]

runScenario :: Int -> Int -> Int -> Workload -> IO ()
runScenario sampleCount revisions nodes workload = do
    -- Build the same native snapshot stream once so measurements cover only
    -- policy/diff work and JSON encoding, not fixture construction.
    let snapshots =
            [ makeSnapshot workload nodes revision
            | revision <- [1 .. revisions]
            ]
    -- Warm both code paths before reading allocation and timing counters.
    _ <- runPolicy AlwaysFull snapshots
    _ <- runPolicy RevisionedDelta snapshots
    mapM_ (\policy -> do
        samples <- sequence
            [ measure policy snapshots
            | _ <- [1 .. sampleCount]
            ]
        printSample workload policy nodes revisions (median samples))
        [AlwaysFull, RevisionedDelta]

measure :: Policy -> [AccessibilitySnapshot] -> IO Sample
measure policy snapshots = do
    performGC
    beforeStats <- getRTSStats
    beforeCpu <- getCPUTime
    beforeElapsed <- getMonotonicTimeNSec
    outputBytes <- runPolicy policy snapshots
    afterElapsed <- getMonotonicTimeNSec
    afterCpu <- getCPUTime
    afterStats <- getRTSStats
    pure Sample
        { sampleElapsedMillis =
            fromIntegral (afterElapsed - beforeElapsed) / 1.0e6
        , sampleCpuMillis =
            fromIntegral (afterCpu - beforeCpu) / 1.0e9
        , sampleAllocatedBytes =
            fromIntegral
                (afterStats.allocated_bytes - beforeStats.allocated_bytes)
        , sampleOutputBytes = outputBytes
        }

runPolicy :: Policy -> [AccessibilitySnapshot] -> IO Integer
runPolicy policy snapshots = do
    totalRef <- newIORef 0
    case policy of
        AlwaysFull ->
            mapM_ (\(revision, snapshot) ->
                addEncoded totalRef (AccessibilityFull revision snapshot))
                (zip [1 ..] snapshots)
        RevisionedDelta ->
            go initialAccessibilityDeltaState snapshots totalRef
    readIORef totalRef
  where
    go _ [] _ = pure ()
    go state (snapshot : rest) totalRef = do
        let (observation, successor) =
                advanceAccessibilityObservation state snapshot
        addEncoded totalRef observation
        go successor rest totalRef

addEncoded :: Aeson.ToJSON value => IORef Integer -> value -> IO ()
addEncoded totalRef value = do
    let bytes = fromIntegral (LBS.length (Aeson.encode value))
    atomicModifyIORef' totalRef \total -> (total + bytes, ())

makeSnapshot :: Workload -> Int -> Int -> AccessibilitySnapshot
makeSnapshot workload nodes revision = AccessibilitySnapshot
    { accessibilitySnapshotSchemaVersion = 1
    , accessibilitySnapshotScope = Aeson.object
        [ "application_bundle_identifier" Aeson..=
            ("com.example.benchmark" :: Text)
        , "window" Aeson..= Aeson.object
            [ "identifier" Aeson..= ("main" :: Text) ]
        ]
    , accessibilitySnapshotContents = Aeson.object
        [ "role" Aeson..= ("AXWindow" :: Text)
        , "children" Aeson..= Aeson.Object (KeyMap.fromList
            [ (Key.fromText (Text.pack (show index)), makeNode index)
            | index <- [0 .. nodes - 1]
            ])
        ]
    }
  where
    changedIndex = revision `mod` nodes
    makeNode index = Aeson.object
        [ "role" Aeson..= ("AXStaticText" :: Text)
        , "identifier" Aeson..= ("row-" <> Text.pack (show index))
        , "title" Aeson..= nodeTitle index
        , "enabled" Aeson..= True
        , "position" Aeson..= Aeson.object
            [ "x" Aeson..= (index `mod` 80)
            , "y" Aeson..= (index `div` 80)
            ]
        ]
    nodeTitle index =
        case workload of
            Static -> "unchanged"
            SmallChange
                | index == changedIndex ->
                    "changed-" <> Text.pack (show revision)
                | otherwise -> "unchanged"
            HighChurn ->
                "revision-" <> Text.pack (show revision)
                    <> "-node-" <> Text.pack (show index)

median :: [Sample] -> Sample
median samples =
    sortOn (.sampleElapsedMillis) samples !! (length samples `div` 2)

printSample :: Workload -> Policy -> Int -> Int -> Sample -> IO ()
printSample workload policy nodes revisions sample =
    printf
        "%s,%s,%d,%d,%.3f,%.3f,%d,%d\n"
        (workloadName workload)
        (policyName policy)
        nodes
        revisions
        sample.sampleElapsedMillis
        sample.sampleCpuMillis
        sample.sampleAllocatedBytes
        sample.sampleOutputBytes

csvHeader :: String
csvHeader =
    "workload,policy,nodes,revisions,elapsed_ms,cpu_ms,allocated_bytes,output_bytes"

workloadName :: Workload -> String
workloadName = \case
    Static -> "static"
    SmallChange -> "small-change"
    HighChurn -> "high-churn"

policyName :: Policy -> String
policyName = \case
    AlwaysFull -> "always-full"
    RevisionedDelta -> "revisioned-delta"

parseWorkload :: String -> IO Workload
parseWorkload = \case
    "static" -> pure Static
    "small-change" -> pure SmallChange
    "high-churn" -> pure HighChurn
    other -> die ("unknown workload: " <> other)

parsePositive :: String -> String -> IO Int
parsePositive label raw =
    case reads raw of
        [(value, "")] | value > 0 -> pure value
        _ -> die ("invalid " <> label <> ": " <> raw)
