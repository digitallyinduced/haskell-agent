{-# LANGUAGE BangPatterns #-}

-- Render every streaming frame through Brick, retaining its image cache between
-- frames and consuming Vty's actual display spans, not merely widget WHNF.
module Main (main) where

import qualified Agent.TUI.Markdown as Markdown
import Agent.TUI.FencedCode (FenceStreamState, emptyFenceStreamState, feedFenceStream)
import qualified Agent.TUI.Theme as Theme
import Brick
import Control.Exception (evaluate) -- safe-exceptions does not export evaluate.
import Control.Monad (forM, unless)
import Data.Char (ord)
import Data.Foldable (toList)
import Data.IORef (newIORef, readIORef)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Graphics.Vty as V
import Graphics.Vty.PictureToSpans (displayOpsForPic)
import Graphics.Vty.Span (SpanOp(..))
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats (RTSStats(..), getRTSStats, getRTSStatsEnabled)
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import System.Exit (die)
import System.Mem (performGC)
import Text.Printf (printf)

data Name = Transcript | Code !Int | Prose !Int !Int | Link !Text
    deriving (Eq, Ord, Read, Show)

data Sample = Sample !Double !Double !Double

-- The historical modes remain available so earlier benchmark reports can be
-- reproduced. "current" is the baseline for retained-parser measurements.
data Renderer = LegacyRenderer | SectionCacheRenderer | IncrementalRenderer
    deriving (Eq)

-- This is the pre-optimization production path: append the strict body, parse
-- the complete Markdown every frame, and cache only closed fenced-code bodies.
baseline :: Text -> Widget Name
baseline = Markdown.markdownWidgetWithSyntaxHighlightingAndLinks
    Nothing Link (\index -> cached (Code index)) (\_ _ -> emptyWidget)

optimized :: Text -> Widget Name
optimized = Markdown.markdownWidgetWithStreamingCache
    Nothing Link (\chunk section -> cached (Prose chunk section))
    (\index -> cached (Code index)) (\_ _ -> emptyWidget)

prepareFrame
    :: Renderer
    -> FenceStreamState
    -> Bool
    -> Text
    -> Text
    -> (FenceStreamState, Widget Name)
prepareFrame renderer parser streaming body delta
    | not streaming = (emptyFenceStreamState, baseline body)
    | renderer == LegacyRenderer = (parser, baseline body)
    | renderer == SectionCacheRenderer = (parser, optimized body)
    | otherwise =
        let !nextParser = feedFenceStream parser delta
        in ( nextParser
           , Markdown.markdownWidgetWithParsedStreamingCache
                Nothing Link (\chunk section -> cached (Prose chunk section))
                (\index -> cached (Code index)) (\_ _ -> emptyWidget)
                nextParser
           )

main :: IO ()
main = do
    enabled <- getRTSStatsEnabled
    unless enabled (die "run with +RTS -T")
    getArgs >>= \case
        [mode, scenario, countArg, chunkArg, samplesArg] -> do
            renderer <- case mode of
                "old" -> pure LegacyRenderer
                "new" -> pure SectionCacheRenderer
                "current" -> pure SectionCacheRenderer
                "incremental" -> pure IncrementalRenderer
                _ -> die "mode must be old, new, current, or incremental"
            unless (scenario `elem`
                ["prose", "prose-lines", "fence", "open-fence", "table", "open-table", "mixed", "resize",
                 "history-prose", "history-mixed"])
                (die "unknown scenario")
            count <- positive countArg
            chunkSize <- positive chunkArg
            samples <- positive samplesArg
            let input = frameInput scenario count chunkSize 0
            case [(index, old, new) |
                    (index, (old, new)) <- zip [0 :: Int ..]
                        (zip (frames SectionCacheRenderer (scenario == "resize") input)
                            (frames renderer (scenario == "resize") input)),
                    old /= new] of
                [] -> pure ()
                (index, old, new) : _ ->
                    die ("old/new per-frame display or click targets differ at "
                        <> show index <> "\nOLD: " <> show old <> "\nNEW: " <> show new)
            results <- forM [1 .. samples] \sample ->
                measure renderer (scenario == "resize")
                    (frameInput scenario count chunkSize sample)
            printf "%s,%s,%d,%d,%d,%.6f,%.6f,%.0f\n"
                mode scenario count chunkSize samples
                (median [wall | Sample wall _ _ <- results])
                (median [cpu | Sample _ cpu _ <- results])
                (median [bytes | Sample _ _ bytes <- results])
        _ -> die "usage: fullscreen-markdown-bench old|new|current|incremental SCENARIO COUNT CHUNK_CHARS SAMPLES"
  where
    positive raw = case reads raw of
        [(n, "")] | n > 0 -> pure n
        _ -> die ("expected positive integer: " <> raw)
    median values = sort values !! (length values `div` 2)

measure :: Renderer -> Bool -> [(Bool, Text)] -> IO Sample
measure renderer resize input = do
    _ <- evaluate (foldl' (\n (streaming, t) -> fromEnum streaming + n + textChecksum t) 0 input)
    ref <- newIORef input
    performGC
    beforeStats <- getRTSStats
    beforeCpu <- getCPUTime
    beforeWall <- getMonotonicTimeNSec
    fresh <- readIORef ref
    !result <- evaluate (run renderer resize fresh)
    afterWall <- getMonotonicTimeNSec
    afterCpu <- getCPUTime
    performGC
    afterStats <- getRTSStats
    _ <- evaluate result
    pure (Sample
        (fromIntegral (afterWall - beforeWall) / 1e6)
        (fromIntegral (afterCpu - beforeCpu) / 1e9)
        (fromIntegral (afterStats.allocated_bytes - beforeStats.allocated_bytes)))

{-# NOINLINE run #-}
run :: Renderer -> Bool -> [(Bool, Text)] -> Int
run renderer resize = go "" emptyFenceStreamState emptyRenderState 0 (0 :: Int)
  where
    go !_ !_ !_ !total !_ [] = total
    go !body !parser !state !total !frame ((streaming, delta) : rest) =
        let !nextBody = body <> delta
            (!nextParser, markdown) = prepareFrame renderer parser streaming nextBody delta
            width = if resize && (frame `div` 50) `mod` 2 == 1 then 40 else 80
            region = (width, 30)
            stateBefore =
                if resize && frame `mod` 50 == 0 then emptyRenderState else state
            widget = viewport Transcript Vertical $
                vBox [markdown, visible (txt " ")]
            (nextState, picture, _, extents) =
                renderFinal Theme.terminalDefault [widget] region
                    (const Nothing) stateBefore
            !checksum = foldl' (foldl' spanChecksum) 0
                (displayOpsForPic picture region)
                + foldl' (\n extent -> n + length (show extent)) 0 extents
        in go nextBody nextParser nextState (total + checksum) (frame + 1) rest

-- Equality checking deliberately stays outside the measurement. Compare the
-- complete display operations (including styles/URLs) and click extents.
frames :: Renderer -> Bool -> [(Bool, Text)] -> [([[(Char, V.Attr)]], [String])]
frames renderer resize = go "" emptyFenceStreamState emptyRenderState (0 :: Int)
  where
    go _ _ _ _ [] = []
    go body parser state frame ((streaming, delta) : rest) =
        let nextBody = body <> delta
            (nextParser, markdown) = prepareFrame renderer parser streaming nextBody delta
            width = if resize && (frame `div` 50) `mod` 2 == 1 then 40 else 80
            region = (width, 30)
            before = if resize && frame `mod` 50 == 0 then emptyRenderState else state
            widget = viewport Transcript Vertical $
                vBox [markdown, visible (txt " ")]
            (nextState, picture, _, extents) =
                renderFinal Theme.terminalDefault [widget] region (const Nothing) before
        in ( map (concatMap spanCells . toList) (toList (displayOpsForPic picture region))
            , sort (map show extents))
            : go nextBody nextParser nextState (frame + 1) rest

-- Span segmentation depends on image composition and is not observable.
-- SpanOp's Show instance also omits text, so compare explicit payloads.
spanCells :: SpanOp -> [(Char, V.Attr)]
spanCells = \case
    TextSpan{textSpanText, textSpanAttr} ->
        [(character, textSpanAttr) | character <- LazyText.unpack textSpanText]
    Skip count -> replicate count (' ', V.defAttr)
    RowEnd count -> replicate count (' ', V.defAttr)

spanChecksum :: Int -> SpanOp -> Int
spanChecksum n = \case
    TextSpan{textSpanText, textSpanAttr} ->
        LazyText.foldl' (\s c -> s * 33 + ord c) n textSpanText
            + length (show textSpanAttr)
    Skip count -> n + count
    RowEnd count -> n + count

textChecksum :: Text -> Int
textChecksum = Text.foldl' (\n c -> n * 33 + ord c) 5381

emptyRenderState :: RenderState Name
emptyRenderState = read
    "RS {viewportMap = fromList [], rsScrollRequests = [], \
    \observedNames = fromList [], renderCache = fromList [], \
    \clickableNames = [], requestedVisibleNames_ = fromList [], \
    \reportedExtents = fromList []}"

chunks :: Int -> Text -> [Text]
chunks size input
    | Text.null input = []
    | otherwise = let (prefix, suffix) = Text.splitAt size input
                  in prefix : chunks size suffix

frameInput :: String -> Int -> Int -> Int -> [(Bool, Text)]
frameInput scenario count chunkSize nonce =
    case Text.stripPrefix "history-" (Text.pack scenario) of
        Just bodyScenario -> [(False, workload (Text.unpack bodyScenario) count nonce)]
        Nothing ->
            [(True, delta) | delta <- chunks chunkSize (workload scenario count nonce)]
                <> [(False, "")]

workload :: String -> Int -> Int -> Text
workload scenario count nonce =
    "# Response " <> Text.pack (show nonce) <> "\n\n" <> case scenario of
        "prose" -> Text.replicate count (prose <> "\n")
        "prose-lines" -> Text.replicate count prose
        "fence" -> fence count <> prose
        "open-fence" -> "```haskell\n" <> Text.replicate count code
        "table" -> table count <> "\n" <> prose
        "open-table" -> table count
        _ -> Text.replicate count (prose <> "\n" <> fence 3 <> table 3 <> "\n")
  where
    prose = "A **bold** explanation with `inline code` and [docs](https://example.com).\n"
    code = "value = map (+ 1) [1, 2, 3]\n"
    row = "| item | **some value** |\n"
    fence n = "```haskell\n" <> Text.replicate n code <> "```\n"
    table n = "| Name | Value |\n| --- | --- |\n" <> Text.replicate n row
