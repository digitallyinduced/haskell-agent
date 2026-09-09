{-# LANGUAGE BangPatterns #-}

-- End-to-end streaming: feed every delta, render completed blocks, flush the
-- final pending block, and consume every output character (including ANSI).
module Main (main) where

import qualified Agent.CLI.Render.MarkdownStream as New
import qualified MarkdownStreamBaseline as Old
-- safe-exceptions does not export evaluate.
import Control.Exception (evaluate)
import Control.Monad (forM, unless)
import Data.Char (ord)
import Data.IORef (newIORef, readIORef)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats (RTSStats(..), getRTSStats, getRTSStatsEnabled)
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import System.Exit (die)
import System.Mem (performGC)
import Text.Printf (printf)

data Renderer = forall state. Renderer
    !state
    (state -> Text -> (state, Text))
    (state -> Text)

oldRenderer, newRenderer :: Renderer
oldRenderer = Renderer Old.emptyMarkdownStreamState
    (Old.feedMarkdownStreamAtWidth 100) (Old.flushMarkdownStreamAtWidth 100)
newRenderer = Renderer New.emptyMarkdownStreamState
    (New.feedMarkdownStreamAtWidth 100) (New.flushMarkdownStreamAtWidth 100)

data Sample = Sample !Double !Double !Double

main :: IO ()
main = do
    enabled <- getRTSStatsEnabled
    unless enabled (die "run with +RTS -T")
    getArgs >>= \case
        [mode, scenario, countArg, chunkArg, sampleArg] -> do
            renderer <- case mode of
                "old" -> pure oldRenderer
                "new" -> pure newRenderer
                _ -> die "mode must be old or new"
            unless (scenario `elem`
                ["prose", "fence", "open-fence", "table", "open-table", "longline", "mixed"])
                (die "unknown scenario")
            count <- positive countArg
            chunkSize <- positive chunkArg
            samples <- positive sampleArg
            let checkInput = chunks chunkSize (workload scenario count 0)
            unless (outputs oldRenderer checkInput == outputs newRenderer checkInput)
                (die "old/new output differs (including per-delta emission timing)")
            results <- forM [1 .. samples] \sample ->
                measure renderer (chunks chunkSize (workload scenario count sample))
            printf "%s,%s,%d,%d,%d,%.6f,%.6f,%.0f\n"
                mode scenario count chunkSize samples
                (median [wall | Sample wall _ _ <- results])
                (median [cpu | Sample _ cpu _ <- results])
                (median [allocated | Sample _ _ allocated <- results])
        _ -> die "usage: markdown-streaming-bench old|new prose|fence|open-fence|table|open-table|longline|mixed COUNT CHUNK_CHARS SAMPLES"
  where
    positive raw = case reads raw of
        [(n, "")] | n > 0 -> pure n
        _ -> die ("expected positive integer: " <> raw)
    median values = sort values !! (length values `div` 2)

measure :: Renderer -> [Text] -> IO Sample
measure renderer input = do
    -- Force the chunk list and payload before measurement. Reading an IORef
    -- inside the timed interval prevents sharing a previously evaluated run.
    _ <- evaluate (foldl' (\n text -> n + checksum text) 0 input)
    ref <- newIORef input
    performGC
    beforeStats <- getRTSStats
    beforeCpu <- getCPUTime
    beforeWall <- getMonotonicTimeNSec
    freshInput <- readIORef ref
    !result <- evaluate (run renderer freshInput)
    afterWall <- getMonotonicTimeNSec
    afterCpu <- getCPUTime
    -- Account for the unfinished nursery, outside elapsed/CPU timing.
    performGC
    afterStats <- getRTSStats
    _ <- evaluate result
    pure (Sample
        (fromIntegral (afterWall - beforeWall) / 1e6)
        (fromIntegral (afterCpu - beforeCpu) / 1e9)
        (fromIntegral (afterStats.allocated_bytes - beforeStats.allocated_bytes)))

{-# NOINLINE run #-}
run :: Renderer -> [Text] -> Int
run (Renderer initial feed flush) = go initial 0
  where
    go !state !total [] = total + checksum (flush state)
    go !state !total (delta : rest) =
        let (next, output) = feed state delta
        in go next (total + checksum output) rest

outputs :: Renderer -> [Text] -> [Text]
outputs (Renderer initial feed flush) = go initial
  where
    go state [] = [flush state]
    go state (delta : rest) =
        let (next, output) = feed state delta
        in output : go next rest

checksum :: Text -> Int
checksum = Text.foldl' (\n character -> n * 33 + ord character) 5381

chunks :: Int -> Text -> [Text]
chunks size text
    | Text.null text = []
    | otherwise =
        let (prefix, suffix) = Text.splitAt size text
        in prefix : chunks size suffix

-- COUNT is the number of body rows, except longline (number of 16-character
-- units) and mixed (number of small prose/fence/table groups). ASCII fixtures
-- make CHUNK_CHARS equal bytes. Sample-specific text defeats cross-run sharing.
workload :: String -> Int -> Int -> Text
workload scenario count sample =
    let prose = "Here is **streamed Markdown**, with `inline code` and a [link](https://example.com).\n"
        code = "    result = transform(value) # retain the generated code\n"
        header = "| Name | Value |\n| --- | --- |\n"
        row = "| **item** | `some value` |\n"
        suffix = "\nSample " <> Text.pack (show sample) <> ".\n"
    in case scenario of
        "prose" -> Text.replicate count prose <> suffix
        "fence" -> "```python\n" <> Text.replicate count code <> "```\n" <> suffix
        "open-fence" -> "```python\n" <> Text.replicate count code
            <> "# " <> Text.pack (show sample)
        "table" -> header <> Text.replicate count row <> suffix
        "open-table" -> header <> Text.replicate count row
            <> "| last | " <> Text.pack (show sample) <> " |"
        "longline" -> "```\n" <> Text.replicate count "abcdefghijklmnop"
            <> Text.pack (show sample) <> "\n```\n"
        "mixed" -> Text.replicate count
            (prose <> "```\n" <> Text.replicate 5 code <> "```\n"
                <> header <> Text.replicate 5 row <> "\n") <> suffix
        _ -> error "validated scenario"
