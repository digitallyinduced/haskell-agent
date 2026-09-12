{-# LANGUAGE BangPatterns #-}

-- Render every streaming frame through Brick, retaining its image cache between
-- frames and consuming Vty's actual display spans, not merely widget WHNF.
module Main (main) where

import qualified Agent.TUI.Markdown as Markdown
import Agent.TUI.FencedCode (FenceStreamState, emptyFenceStreamState, feedFenceStream)
import Agent.TUI.Markdown.Stream (MarkdownStreamState, emptyMarkdownStreamState, feedMarkdownStream)
import qualified Agent.TUI.Markdown.Inline as Inline
import qualified Agent.TUI.Theme as Theme
import Brick
-- This standalone benchmark uses base's bracket solely for stable-pointer cleanup.
import Control.Exception (bracket, evaluate)
import Control.Monad (forM, unless)
import Data.Char (ord)
import Data.Foldable (toList)
import Data.IORef (newIORef, readIORef)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import Data.Word (Word64)
import qualified Graphics.Vty as V
import Graphics.Vty.PictureToSpans (displayOpsForPic)
import Graphics.Vty.Span (SpanOp(..))
import GHC.Clock (getMonotonicTimeNSec)
import Foreign.StablePtr (newStablePtr, freeStablePtr)
import GHC.Stats (RTSStats(..), GCDetails(..), getRTSStats, getRTSStatsEnabled)
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import System.Exit (die)
import System.Mem (performGC)
import Text.Printf (printf)

data Name = Transcript | Code !Int | Prose !Int !Int | Link !Text
    deriving (Eq, Ord, Read, Show)

data Sample = Sample !Double !Double !Double

-- The historical modes remain available so earlier benchmark reports can be
-- reproduced. "incremental" is the PR #1278 fence-only parser baseline.
data Renderer = LegacyRenderer | SectionCacheRenderer | IncrementalRenderer | StreamingRenderer
    | BaselineInlineParser | StreamingInlineParser | RetainedRenderer
    deriving (Eq)

data ParserState = ParserState !FenceStreamState !MarkdownStreamState

emptyParserState :: ParserState
emptyParserState = ParserState emptyFenceStreamState emptyMarkdownStreamState

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
    -> ParserState
    -> Bool
    -> Text
    -> Text
    -> (ParserState, Widget Name)
prepareFrame renderer parser@(ParserState fences markdown) streaming body delta
    | not streaming = (emptyParserState, baseline body)
    | renderer == LegacyRenderer = (parser, baseline body)
    | renderer == SectionCacheRenderer = (parser, optimized body)
    | renderer == IncrementalRenderer =
        let !nextParser = feedFenceStream fences delta
        in ( ParserState nextParser markdown
           , Markdown.markdownWidgetWithParsedStreamingCache
                Nothing Link (\chunk section -> cached (Prose chunk section))
                (\index -> cached (Code index)) (\_ _ -> emptyWidget)
                nextParser
           )
    | otherwise =
        let !nextParser = feedMarkdownStream markdown delta
        in ( ParserState fences nextParser
           , Markdown.markdownWidgetWithMarkdownStreamingCache
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
                "streaming" -> pure StreamingRenderer
                "retained" -> pure RetainedRenderer
                "baseline-inline" -> pure BaselineInlineParser
                "streaming-inline" -> pure StreamingInlineParser
                _ -> die "unknown renderer or parser mode"
            unless (scenario `elem`
                ["prose", "prose-lines", "fence", "open-fence", "table", "open-table", "mixed", "resize",
                 "history-prose", "history-mixed", "long-line", "incomplete",
                 "unmatched-code", "unmatched-link", "unicode-prose", "unicode-tail"])
                (die "unknown scenario")
            count <- positive countArg
            chunkSize <- positive chunkArg
            samples <- positive samplesArg
            let input = frameInput scenario count chunkSize 0
            if renderer `elem` [BaselineInlineParser, StreamingInlineParser]
                then verifyInlineFrames input
                else case [(index, old, new) |
                    (index, (old, new)) <- zip [0 :: Int ..]
                        (zip (frames SectionCacheRenderer (scenario == "resize") input)
                            (frames renderer (scenario == "resize") input)),
                    old /= new] of
                    [] -> pure ()
                    (index, old, new) : _ ->
                        die ("old/new per-frame display or click targets differ at "
                            <> show index <> "\nOLD: " <> show old <> "\nNEW: " <> show new)
            if renderer == RetainedRenderer
                then do
                    results <- forM [1 .. samples] \sample ->
                        measureRetained (scenario == "resize")
                            (frameInput scenario count chunkSize sample)
                    printf "%s,%s,%d,%d,%d,%d\n"
                        mode scenario count chunkSize samples (median results)
                else do
                    results <- forM [1 .. samples] \sample ->
                        measure renderer (scenario == "resize")
                            (frameInput scenario count chunkSize sample)
                    printf "%s,%s,%d,%d,%d,%.6f,%.6f,%.0f\n"
                        mode scenario count chunkSize samples
                        (median [wall | Sample wall _ _ <- results])
                        (median [cpu | Sample _ cpu _ <- results])
                        (median [bytes | Sample _ _ bytes <- results])
        _ -> die "usage: fullscreen-markdown-bench old|new|current|incremental|streaming|retained|baseline-inline|streaming-inline SCENARIO COUNT CHUNK_CHARS SAMPLES"
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
run BaselineInlineParser _ = runInline False
run StreamingInlineParser _ = runInline True
run renderer resize = \input ->
    case runRendered renderer resize input of
        (checksum, _, _, _) -> checksum

-- The normal timing runner discards its output. This diagnostic instead pins
-- the final body, Brick image cache, and picture across a major GC, without
-- retaining the input frame list. Its final CSV field is total live heap bytes,
-- not allocated bytes or peak RSS. Compare separately built helper variants.
measureRetained :: Bool -> [(Bool, Text)] -> IO Word64
measureRetained resize input = do
    result <- evaluate (runRendered StreamingRenderer resize input)
    bracket (newStablePtr result) freeStablePtr \_ -> do
        performGC
        stats <- getRTSStats
        pure stats.gc.gcdetails_live_bytes

{-# NOINLINE runRendered #-}
runRendered :: Renderer -> Bool -> [(Bool, Text)] -> (Int, Text, RenderState Name, V.Picture)
runRendered renderer resize =
    go "" emptyParserState emptyRenderState (V.picForImage V.emptyImage) 0 (0 :: Int)
  where
    go !body !_ !state !picture !total !_ [] = (total, body, state, picture)
    go !body !parser !state !_ !total !frame ((streaming, delta) : rest) =
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
        in go nextBody nextParser nextState picture (total + checksum) (frame + 1) rest

-- Diagnostics force the complete syntax tree, not just its rendered text.
-- They are not a substitute for the full-render acceptance measurements.
runInline :: Bool -> [(Bool, Text)] -> Int
runInline retained = go "" Inline.emptyInlineStreamState 0
  where
    go !_ !_ !total [] = total
    go !body !parser !total ((streaming, delta) : rest) =
        let !nextBody = body <> delta
            !nextParser = if retained && streaming
                then Inline.feedInlineStream parser delta else parser
            nodes = if retained && streaming
                then Inline.inlineStreamSnapshot nextParser
                else Inline.parseInline nextBody
            !checksum = inlineChecksum nodes
        in go nextBody nextParser (total + checksum) rest

inlineChecksum :: [Inline.Inline] -> Int
inlineChecksum = foldl' (\total node -> total * 33 + case node of
    Inline.InlineText value -> textChecksum value
    Inline.InlineCode value -> 1 + textChecksum value
    Inline.InlineStrong children -> 2 + inlineChecksum children
    Inline.InlineEmphasis children -> 3 + inlineChecksum children
    Inline.InlineLink url children -> 4 + textChecksum url + inlineChecksum children) 0

verifyInlineFrames :: [(Bool, Text)] -> IO ()
verifyInlineFrames = go "" Inline.emptyInlineStreamState (0 :: Int)
  where
    go _ _ _ [] = pure ()
    go body parser index ((_, delta) : rest) = do
        let nextBody = body <> delta
            nextParser = Inline.feedInlineStream parser delta
        unless (Inline.parseInline nextBody == Inline.inlineStreamSnapshot nextParser)
            (die ("inline per-frame syntax differs at " <> show index))
        go nextBody nextParser (index + 1) rest

-- Equality checking deliberately stays outside the measurement. Compare the
-- complete display operations (including styles/URLs) and click extents.
frames :: Renderer -> Bool -> [(Bool, Text)] -> [([[(Char, V.Attr)]], [String])]
frames renderer resize = go "" emptyParserState emptyRenderState (0 :: Int)
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
        "unicode-prose" -> Text.replicate count unicodeProse
        "unicode-tail" -> Text.replicate count unicodeTail
        "long-line" -> Text.replicate count (Text.stripEnd prose <> " ")
        "incomplete" -> "**unfinished [label (with nesting) " <> Text.replicate count "plain text and `code` "
        "unmatched-code" -> "`" <> Text.replicate count "unfinished code "
        "unmatched-link" -> "[label](https://example.com/" <> Text.replicate count "nested(segment)/"
        "fence" -> fence count <> prose
        "open-fence" -> "```haskell\n" <> Text.replicate count code
        "table" -> table count <> "\n" <> prose
        "open-table" -> table count
        _ -> Text.replicate count (prose <> "\n" <> fence 3 <> table 3 <> "\n")
  where
    prose = "A **bold** explanation with `inline code` and [docs](https://example.com).\n"
    -- No blank separators: completed lines remain in the active prose section.
    unicodeProse =
        "説明 **重要** cafe\x0301 with `変数` and [資料](https://example.com) "
            <> "\x1f469\x200d\x1f4bb \x1f1e9\x1f1ea.\n"
    -- The final non-ASCII character rejects a whole-span ASCII fast path only
    -- after scanning a long prefix. Keep the prefix free of inline delimiters.
    unicodeTail =
        Text.replicate 16 "A representative line of ordinary source text. " <> "界\n"
    code = "value = map (+ 1) [1, 2, 3]\n"
    row = "| item | **some value** |\n"
    fence n = "```haskell\n" <> Text.replicate n code <> "```\n"
    table n = "| Name | Value |\n| --- | --- |\n" <> Text.replicate n row
