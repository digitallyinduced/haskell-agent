module Main (main) where

import Agent.TUI.Markdown.Inline (Inline(..), inlinePlainText, parseInline)
import Control.Exception (evaluate)
import Data.List (sort)
import qualified Data.Text as Text
import Data.Text (Text)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats (RTSStats(..), getRTSStats, getRTSStatsEnabled)
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import System.Exit (die)
import System.Mem (performGC)
import Text.Printf (printf)

data Sample = Sample !Double !Double !Integer !Int

data Mode = LinkWorkload | CodeWorkload | EmphasisWorkload

main :: IO ()
main = do
    enabled <- getRTSStatsEnabled
    if enabled then pure () else die "run with +RTS -T"
    getArgs >>= \case
        [modeArg, sizeArg, samplesArg] -> do
            size <- positive sizeArg
            samples <- positive samplesArg
            mode <- parseMode modeArg
            let sources =
                    [ workload mode size variant
                    | variant <- [0 .. samples - 1]
                    ]
            _ <- evaluate (sum (map Text.length sources))
            if all (validSource mode . parseInline) sources
                then pure ()
                else die "benchmark workload did not parse as intended"
            results <- mapM (measure mode) sources
            let Sample elapsed cpu allocated checksum = median results
            printf "%s,%d,%d,%.3f,%.3f,%d,%d\n"
                modeArg size samples elapsed cpu allocated checksum
        _ -> die "usage: markdown-accumulators-bench inline-link|inline-code|inline-emphasis SIZE SAMPLES"
  where
    positive raw =
        case reads raw of
            [(value, "")] | value > 0 -> pure value
            _ -> die ("invalid positive integer: " <> raw)

parseMode :: String -> IO Mode
parseMode = \case
    "inline-link" -> pure LinkWorkload
    "inline-code" -> pure CodeWorkload
    "inline-emphasis" -> pure EmphasisWorkload
    other -> die ("unknown mode: " <> other)

measure :: Mode -> Text -> IO Sample
measure mode source = do
    performGC
    before <- getRTSStats
    beforeCpu <- getCPUTime
    beforeClock <- getMonotonicTimeNSec
    checksum <- evaluate (checksumSource mode source)
    afterClock <- getMonotonicTimeNSec
    afterCpu <- getCPUTime
    performGC
    after <- getRTSStats
    _ <- evaluate checksum
    pure
        ( Sample
            (fromIntegral (afterClock - beforeClock) / 1.0e6)
            (fromIntegral (afterCpu - beforeCpu) / 1.0e9)
            (fromIntegral (after.allocated_bytes - before.allocated_bytes))
            checksum
        )

checksumSource :: Mode -> Text -> Int
checksumSource mode source =
    case mode of
        LinkWorkload -> checksumInline (parseInline source)
        CodeWorkload -> checksumInline (parseInline source)
        EmphasisWorkload -> checksumInline (parseInline source)
  where
    checksumInline =
        foldl' step 5381
    step acc inline =
        foldl' (\value c -> value * 33 + fromEnum c) (acc * 33 + tag inline) (inlineText inline)
    tag = \case
        InlineText _ -> 1
        InlineCode _ -> 2
        InlineStrong _ -> 3
        InlineEmphasis _ -> 4
        InlineLink _ _ -> 5
    inlineText = \case
        InlineText text -> Text.unpack text
        InlineCode text -> Text.unpack text
        InlineStrong children -> Text.unpack (inlinePlainText children)
        InlineEmphasis children -> Text.unpack (inlinePlainText children)
        InlineLink url children ->
            Text.unpack (url <> inlinePlainText children)

workload :: Mode -> Int -> Int -> Text
workload LinkWorkload n variant =
    "[label\\] "
        <> Text.replicate n "x"
        <> variantText variant
        <> "](https://example.com/a_\\(b\\)/"
        <> variantText variant
        <> ") "
workload CodeWorkload n variant =
    let ticks = "```"
    in ticks <> Text.replicate n "code ` payload " <> variantText variant <> ticks
workload EmphasisWorkload n variant =
    "**bold "
        <> Text.replicate n "x"
        <> " *nested "
        <> Text.pack (show variant)
        <> "***"
variantText :: Int -> Text
variantText variant = Text.justifyRight 4 '0' (Text.pack (show variant))

validSource :: Mode -> [Inline] -> Bool
validSource EmphasisWorkload nodes =
    hasStrong nodes && hasEmphasis nodes
  where
    hasStrong = any $ \case
        InlineStrong _ -> True
        InlineEmphasis children -> hasStrong children
        _ -> False
    hasEmphasis = any $ \case
        InlineStrong children -> hasEmphasis children
        InlineEmphasis _ -> True
        _ -> False
validSource _ _ = True

median :: [Sample] -> Sample
median values =
    Sample
        (middle [elapsed | Sample elapsed _ _ _ <- values])
        (middle [cpu | Sample _ cpu _ _ <- values])
        (middle [allocated | Sample _ _ allocated _ <- values])
        (middle [checksum | Sample _ _ _ checksum <- values])
  where
    middle xs = sort xs !! (length xs `div` 2)
