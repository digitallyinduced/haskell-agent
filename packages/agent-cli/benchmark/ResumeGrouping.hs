module Main (main) where

import Agent.CLI.Resume (ResumeEntry(..), groupResumeEntries)
import Control.Exception (evaluate)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats
    ( RTSStats(..)
    , getRTSStats
    , getRTSStatsEnabled
    )
import System.Environment (getArgs)
import System.CPUTime (getCPUTime)
import System.Mem (performGC)
import Text.Printf (printf)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [entryCountArg, projectCountArg, sampleCountArg] -> do
            entryCount <- readPositive "entries" entryCountArg
            projectCount <- readPositive "projects" projectCountArg
            sampleCount <- readPositive "samples" sampleCountArg
            if sampleCount < 7
                then error "sample count must be at least 7"
                else pure ()
            let inputs =
                    [ [mkEntries variant distribution entryCount projectCount
                      | distribution <- [0 .. 7]]
                    | variant <- [0 .. sampleCount - 1]
                    ]
            equivalent <- evaluate (allEquivalent inputs)
            if equivalent
                then pure ()
                else error "legacy and indexed implementations disagree"
            benchmark "legacy" entryCount projectCount inputs legacyGroup
            benchmark "indexed" entryCount projectCount inputs groupResumeEntries
        _ ->
            error "usage: resume-group-bench ENTRIES PROJECTS SAMPLES"

readPositive :: String -> String -> IO Int
readPositive label value =
    case reads value of
        [(n, "")] | n > 0 -> pure n
        _ -> error ("invalid " <> label)

data Sample = Sample
    { elapsedMillis :: !Double
    , cpuMillis :: !Double
    , allocatedBytes :: !Integer
    , checksum :: !Int
    }

benchmark ::
    String ->
    Int ->
    Int ->
    [[[ResumeEntry]]] ->
    ([ResumeEntry] -> [(Text, [ResumeEntry])]) ->
    IO ()
benchmark name entryCount projectCount inputs grouping = do
    enabled <- getRTSStatsEnabled
    if not enabled
        then error "run with +RTS -T"
        else pure ()
    samples <- mapM (measure grouping) inputs
    let middle values = sort values !! (length values `div` 2)
        elapsed = middle (map (.elapsedMillis) samples)
        cpu = middle (map (.cpuMillis) samples)
        allocated = middle (map (.allocatedBytes) samples)
        checksums = map (.checksum) samples
    printf "%s,%d,%d,%d,%.3f,%.3f,%d,%d\n"
        name
        entryCount
        projectCount
        (length inputs)
        elapsed
        cpu
        allocated
        (foldl' (\acc value -> acc * 33 + value) 0 checksums)

measure ::
    ([ResumeEntry] -> [(Text, [ResumeEntry])]) ->
    [[ResumeEntry]] ->
    IO Sample
measure grouping inputSet = do
    performGC
    beforeStats <- getRTSStats
    beforeCpu <- getCPUTime
    start <- getMonotonicTimeNSec
    groups <- evaluate (map grouping inputSet)
    result <- evaluate (checksumGroupSets groups)
    end <- getMonotonicTimeNSec
    afterCpu <- getCPUTime
    -- Exclude collection from elapsed/CPU timings, but synchronize the RTS
    -- counter before reading allocation totals.
    performGC
    afterStats <- getRTSStats
    pure Sample
        { elapsedMillis =
            fromIntegral (end - start) / 1.0e6
        , cpuMillis =
            fromIntegral (afterCpu - beforeCpu) / 1.0e9
        , allocatedBytes =
            fromIntegral
                (afterStats.allocated_bytes - beforeStats.allocated_bytes)
        , checksum = result
        }

allEquivalent :: [[[ResumeEntry]]] -> Bool
allEquivalent inputs =
    all
        (\inputSet ->
            all
                (\entries -> legacyGroup entries == groupResumeEntries entries)
                inputSet)
        inputs

legacyGroup :: [ResumeEntry] -> [(Text, [ResumeEntry])]
legacyGroup = foldl' addGroup []
  where
    addGroup groups entry =
        case break ((== entry.resumeProject) . fst) groups of
            (_, []) -> groups <> [(entry.resumeProject, [entry])]
            (before, (project, entries) : after) ->
                before <> ((project, entries <> [entry]) : after)

mkEntries :: Int -> Int -> Int -> Int -> [ResumeEntry]
mkEntries variant distribution entryCount projectCount =
    [ ResumeEntry
        { resumeId = Text.pack (show variant <> ":" <> show index)
        , resumeTitle = ""
        , resumeModel = ""
        , resumeCwd = ""
        , resumeProject =
            "project-"
                <> Text.pack (show (projectIndex distribution index projectCount))
                <> "-distribution-"
                <> Text.pack (show distribution)
        , resumeWhen = ""
        , resumeProvider = ""
        , resumeCreatedAt = epoch
        , resumeUpdatedAt = epoch
        , resumeMessageCount = 0
        , resumeTurnCount = 0
        , resumeToolCount = 0
        , resumePrompt = ""
        , resumeRecap = Nothing
        , resumeLastTurnSummary = Nothing
        , resumeMatch = Nothing
        , resumeLoaded = False
        , resumeTranscript = []
        }
    | index <- [0 .. entryCount - 1]
    ]
  where
    epoch = read "1970-01-01 00:00:00 UTC" :: UTCTime

projectIndex :: Int -> Int -> Int -> Int
projectIndex distribution index projectCount =
    case distribution of
        0 -> index `mod` projectCount
        1 -> (index `div` max 1 (entryCountPerProject projectCount)) `mod` projectCount
        2 -> (projectCount - 1) - (index `mod` projectCount)
        3 -> (index * 17 + 3) `mod` projectCount
        4 -> (index * 31 + index `div` 7) `mod` projectCount
        5 -> ((index `div` 3) * 5 + index) `mod` projectCount
        6 -> (index * index + 11) `mod` projectCount
        _ -> (index + (index `div` 11) * 13) `mod` projectCount
  where
    -- Grouped runs are sized to cover every project before cycling.
    entryCountPerProject projects = max 1 projects

checksumGroups :: [(Text, [ResumeEntry])] -> Int
checksumGroups =
    foldl'
        (\acc (project, entries) ->
            foldl'
                (\inner entry ->
                    inner * 33
                        + hashText project
                        + hashText entry.resumeId)
                (acc * 33)
                entries)
        0

checksumGroupSets :: [[(Text, [ResumeEntry])]] -> Int
checksumGroupSets = foldl' (\acc groups -> acc * 33 + checksumGroups groups) 5381

hashText :: Text -> Int
hashText = Text.foldl' (\hash character -> hash * 33 + fromEnum character) 5381
