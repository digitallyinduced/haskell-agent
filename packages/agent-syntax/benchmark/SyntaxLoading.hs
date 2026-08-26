module Main (main) where

import Agent.Syntax
    ( SyntaxHighlighter
    , SyntaxSpan(..)
    , highlightCode
    , loadSyntaxHighlighterFrom
    , loadSyntaxLanguage
    , newSyntaxHighlighterFrom
    )

-- 'evaluate' is required to keep work inside the measured IO interval;
-- safe-exceptions does not re-export it.
import Control.Exception (evaluate)
import Control.Monad (foldM, forM)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats
    ( GCDetails(..)
    , RTSStats(..)
    , getRTSStats
    , getRTSStatsEnabled
    )
import System.CPUTime (getCPUTime)
import System.Environment (getArgs, lookupEnv)
import System.Exit (die)
import System.Mem (performGC)
import Text.Printf (printf)

data Workload
    = Eager
    | OnDemand

data Sample = Sample
    { elapsedMillis :: !Double
    , cpuMillis :: !Double
    , allocatedBytes :: !Integer
    , liveBytes :: !Integer
    }

main :: IO ()
main = do
    enabled <- getRTSStatsEnabled
    if enabled
        then pure ()
        else die "RTS statistics are disabled; run with +RTS -T"
    syntaxDirectory <-
        lookupEnv "AGENT_SYNTAX_DIR" >>= \case
            Just directory -> pure directory
            Nothing -> die "AGENT_SYNTAX_DIR is not configured"
    getArgs >>= \case
        [workloadArg, languagesArg, sourceLineCountArg, sampleCountArg] -> do
            workload <- parseWorkload workloadArg
            languages <- parseLanguages languagesArg
            sourceLineCount <-
                parsePositive "source line count" sourceLineCountArg
            sampleCount <- parsePositive "sample count" sampleCountArg
            let source = sampleSource sourceLineCount
            _ <- evaluate (Text.length source)
            samples <-
                forM [1 .. sampleCount] \_ ->
                    measure workload syntaxDirectory languages source
            let sample = median samples
            printf
                "%s,%d,%d,%d,%.3f,%.3f,%d,%d\n"
                workloadArg
                (length languages)
                sourceLineCount
                sampleCount
                sample.elapsedMillis
                sample.cpuMillis
                sample.allocatedBytes
                sample.liveBytes
        _ ->
            die $
                "usage: syntax-loading-bench WORKLOAD LANGUAGES "
                    <> "SOURCE_LINES SAMPLES\n"
                    <> "workloads: eager, on-demand\n"
                    <> "LANGUAGES is none or a comma-separated list\n"
                    <> "output: workload,language_count,source_lines,samples,"
                    <> "elapsed_ms,cpu_ms,allocated_bytes,live_bytes"

parseWorkload :: String -> IO Workload
parseWorkload = \case
    "eager" -> pure Eager
    "on-demand" -> pure OnDemand
    other -> die ("unknown workload: " <> other)

parseLanguages :: String -> IO [Text]
parseLanguages "none" = pure []
parseLanguages raw =
    case filter (not . Text.null) $
        map Text.strip $
            Text.splitOn "," (Text.pack raw) of
        [] -> die "LANGUAGES must be none or a non-empty comma-separated list"
        languages -> pure languages

parsePositive :: String -> String -> IO Int
parsePositive label raw =
    case reads raw of
        [(value, "")]
            | value > 0 -> pure value
        _ -> die ("invalid " <> label <> ": " <> raw)

measure :: Workload -> FilePath -> [Text] -> Text -> IO Sample
measure workload syntaxDirectory languages source = do
    performGC
    beforeStats <- getRTSStats
    beforeCpu <- getCPUTime
    beforeElapsed <- getMonotonicTimeNSec
    highlighter <- buildHighlighter workload syntaxDirectory languages
    checksum <- evaluate (highlightChecksum highlighter languages source)
    performGC
    afterStats <- getRTSStats
    afterElapsed <- getMonotonicTimeNSec
    afterCpu <- getCPUTime
    -- Keep the loaded grammar cache live through the measured collection.
    _ <- evaluate highlighter
    _ <- evaluate checksum
    pure
        Sample
            { elapsedMillis =
                fromIntegral (afterElapsed - beforeElapsed) / 1.0e6
            , cpuMillis =
                fromIntegral (afterCpu - beforeCpu) / 1.0e9
            , allocatedBytes =
                fromIntegral
                    (afterStats.allocated_bytes - beforeStats.allocated_bytes)
            , liveBytes =
                fromIntegral afterStats.gc.gcdetails_live_bytes
            }

buildHighlighter
    :: Workload
    -> FilePath
    -> [Text]
    -> IO SyntaxHighlighter
buildHighlighter workload syntaxDirectory languages =
    case workload of
        Eager ->
            loadSyntaxHighlighterFrom syntaxDirectory >>= requireHighlighter
        OnDemand -> do
            initial <-
                newSyntaxHighlighterFrom syntaxDirectory
                    >>= requireHighlighter
            foldM
                (\current language ->
                    loadSyntaxLanguage current language
                        >>= requireHighlighter)
                initial
                languages

requireHighlighter
    :: Either Text SyntaxHighlighter
    -> IO SyntaxHighlighter
requireHighlighter =
    either (die . Text.unpack) pure

highlightChecksum :: SyntaxHighlighter -> [Text] -> Text -> Int
highlightChecksum highlighter languages source =
    foldl' checksumLanguage 5381 languages
  where
    checksumLanguage checksum language =
        case highlightCode highlighter language source of
            Left message ->
                error (Text.unpack message)
            Right lines' ->
                foldl' (foldl' checksumSpan) checksum lines'
    checksumSpan checksum SyntaxSpan{syntaxClass, syntaxText} =
        Text.foldl'
            (\value character -> value * 33 + fromEnum character)
            (checksum * 33 + fromEnum syntaxClass)
            syntaxText

sampleSource :: Int -> Text
sampleSource lineCount =
    Text.intercalate
        "\n"
        (replicate lineCount "value = value + 42 -- benchmark")

median :: [Sample] -> Sample
median samples =
    Sample
        { elapsedMillis = middle (sort (map (.elapsedMillis) samples))
        , cpuMillis = middle (sort (map (.cpuMillis) samples))
        , allocatedBytes = middle (sort (map (.allocatedBytes) samples))
        , liveBytes = middle (sort (map (.liveBytes) samples))
        }
  where
    middle values = values !! (length values `div` 2)
