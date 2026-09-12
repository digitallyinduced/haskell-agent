{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

-- Build twice, overriding only Markdown/Block.hs with the recorded baseline.
module Main (main) where

import qualified Agent.TUI.Markdown as Markdown
import qualified Agent.TUI.Markdown.Block as Block
import qualified Agent.TUI.Theme as Theme
import Brick
import Control.Exception (evaluate)
import Control.Monad (forM, unless)
import Data.Char (ord)
import Data.IORef (newIORef, readIORef)
import Data.List (foldl', sort)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats
import Graphics.Vty.PictureToSpans (displayOpsForPic)
import Graphics.Vty.Span (SpanOp(..))
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import System.Exit (die)
import System.Mem (performGC)
import Text.Printf (printf)

main :: IO ()
main = do
    enabled <- getRTSStatsEnabled
    unless enabled (die "use +RTS -T")
    getArgs >>= \case
        [mode, kind, sizeArg, repeatsArg, samplesArg] -> do
            unless (mode `elem` ["split", "render"]) (die "expected split or render")
            unless (kind `elem` ["plain", "mixed", "unicode", "escaped", "prose"]) (die "unknown kind")
            size <- positive sizeArg
            repeats <- positive repeatsArg
            samples <- positive samplesArg
            results <- forM [1..samples] $ \nonce -> do
                let input = [workload mode kind size (nonce * repeats + i) | i <- [1..repeats]]
                _ <- evaluate (foldl' (\s t -> s + checksum t) 0 input)
                ref <- newIORef input
                performGC
                before <- getRTSStats
                cpu <- getCPUTime
                wall <- getMonotonicTimeNSec
                fresh <- readIORef ref
                !result <- evaluate (consume mode fresh)
                wallEnd <- getMonotonicTimeNSec
                cpuEnd <- getCPUTime
                performGC
                after <- getRTSStats
                pure (fromIntegral (wallEnd-wall)/1e6 :: Double,
                      fromIntegral (cpuEnd-cpu)/1e9 :: Double,
                      fromIntegral (allocated_bytes after-allocated_bytes before) :: Double,
                      result)
            printf "%s,%s,%d,%d,%d,%.6f,%.6f,%.0f,%d\n"
                mode kind size repeats samples
                (median [w | (w,_,_,_) <- results])
                (median [c | (_,c,_,_) <- results])
                (median [a | (_,_,a,_) <- results])
                (sum [s | (_,_,_,s) <- results])
        _ -> die "usage: MarkdownTable split|render plain|mixed|unicode|escaped|prose SIZE REPEATS SAMPLES"
  where
    positive s = case reads s of
        [(n,"")] | n > 0 -> pure n
        _ -> die "expected positive integer"
    median xs = sort xs !! (length xs `div` 2)

{-# NOINLINE consume #-}
consume :: String -> [Text] -> Int
consume mode = foldl' (\s t -> s + one t) 0
  where
    one = if mode == "split"
        then maybe 0 (foldl' (\s t -> s + checksum t) 0) . Block.splitTableRow
        else renderChecksum

checksum :: Text -> Int
checksum = Text.foldl' (\s c -> s * 33 + ord c) 5381

renderChecksum :: Text -> Int
renderChecksum body =
    let widget :: Widget Text
        widget = viewport "body" Vertical $ vBox
            [Markdown.markdownWidgetWithSyntaxHighlightingAndLinks
                Nothing id (\_ w -> w) (\_ _ -> emptyWidget) body,
             visible (txt " ")]
        (_, picture, _, extents) =
            renderFinal Theme.terminalDefault [widget] (80,30) (const Nothing) emptyState
    in foldl' (foldl' spanChecksum) 0 (displayOpsForPic picture (80,30))
        + foldl' (\s e -> s + length (show e)) 0 extents
  where
    spanChecksum s = \case
        TextSpan{textSpanText, textSpanAttr} ->
            LazyText.foldl' (\n c -> n * 33 + ord c) s textSpanText + length (show textSpanAttr)
        Skip n -> s + n
        RowEnd n -> s + n
    emptyState = read
        "RS {viewportMap = fromList [], rsScrollRequests = [], \
        \observedNames = fromList [], renderCache = fromList [], \
        \clickableNames = [], requestedVisibleNames_ = fromList [], \
        \reportedExtents = fromList []}"

workload :: String -> String -> Int -> Int -> Text
workload mode kind size nonce
    | kind == "prose" = "# Answer " <> Text.pack (show nonce) <> "\n\n"
        <> Text.replicate size "An ordinary **Markdown** paragraph with `code` and 日本語.\n\n"
    | mode == "split" = "| " <> Text.pack (show nonce) <> " " <> Text.replicate size cell <> " | end |"
    | otherwise = "# Table " <> Text.pack (show nonce) <> "\n\n"
        <> "| Name | Value |\n| :--- | ---: |\n"
        <> Text.replicate size ("| entry | " <> cell <> " |\n")
        <> "\nA **complete** answer with [docs](https://example.com).\n"
  where
    cell = case kind of
        "plain" -> "ordinary words "
        "mixed" -> "**bold** `a|b` and ``c|d`` "
        "unicode" -> "日本語 café 🙂 é "
        _ -> "a\\|b \\\\| c `x\\|y` "
