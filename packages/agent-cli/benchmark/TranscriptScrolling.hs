{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Benchmark retained transcript redraws, the dominant work triggered by
-- scrolling a long Brick viewport.
module Main (main) where

import Brick
    ( Padding(..)
    , ViewportType(..)
    , Widget
    , attrMap
    , cached
    , hBox
    , padBottom
    , renderFinal
    , txt
    , txtWrap
    , vBox
    , viewport
    )
import Brick.Types (RenderState)
import Agent.CLI.TUI.Transcript (transcriptChunkSize)
import Control.DeepSeq (force)
import Control.Monad (replicateM)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats
    ( RTSStats(..)
    , getRTSStats
    )
import qualified Graphics.Vty as V
import Graphics.Vty.PictureToSpans (displayOpsForPic)
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import System.Mem (performGC)

data Name
    = TranscriptViewport
    | TranscriptBlock !Int
    | TranscriptChunk !Int !Int
    deriving (Eq, Ord, Read, Show)

data Workload
    = PerBlockCache
    | ChunkCache

data Sample = Sample
    { elapsedMillis :: !Double
    , cpuMillis :: !Double
    , allocatedBytes :: !Integer
    }

main :: IO ()
main = do
    arguments <- getArgs
    case arguments of
        [workloadText, blockCountText, bodyLinesText, sampleCountText] -> do
            workload <- parseWorkload workloadText
            let blockCount = read blockCountText
                bodyLines = read bodyLinesText
                sampleCount = read sampleCountText
                widget = transcriptWidget workload blockCount bodyLines
            initialState <- warmCache widget
            stateRef <- newIORef initialState
            samples <-
                replicateM sampleCount $
                    measure redrawsPerSample widget stateRef
            printSample
                workloadText
                blockCount
                bodyLines
                sampleCount
                (median samples)
        _ ->
            error
                "usage: transcript-scrolling-bench \
                \(per-block-cache|chunk-cache) BLOCKS BODY_LINES SAMPLES"

parseWorkload :: String -> IO Workload
parseWorkload = \case
    "per-block-cache" -> pure PerBlockCache
    "chunk-cache" -> pure ChunkCache
    other -> error ("unknown workload: " <> other)

transcriptWidget :: Workload -> Int -> Int -> Widget Name
transcriptWidget workload blockCount bodyLines =
    viewport TranscriptViewport Vertical $
        case workload of
            PerBlockCache -> vBox blocks
            ChunkCache ->
                vBox
                    [ if length widgets == transcriptChunkSize
                        then
                            cached
                                (TranscriptChunk
                                    first
                                    (first + length widgets - 1))
                                rendered
                        else rendered
                    | (first, widgets) <-
                        zip [1, 1 + transcriptChunkSize ..]
                            (chunksOf transcriptChunkSize blocks)
                    , let rendered = vBox widgets
                    ]
  where
    body =
        Text.intercalate "\n" $
            replicate bodyLines representativeLine
    blocks =
        [ cached (TranscriptBlock index) $
            padBottom (Pad 1) $
                hBox [txt "  ", txtWrap body]
        | index <- [1 .. blockCount]
        ]

representativeLine :: Text
representativeLine =
    "Representative retained transcript text that wraps across the viewport."

redrawsPerSample :: Int
redrawsPerSample = 25

benchmarkRegion :: V.DisplayRegion
benchmarkRegion = (100, 32)

warmCache :: Widget Name -> IO (RenderState Name)
warmCache widget = do
    let (renderState, picture, _, _) =
            renderFinal
                (attrMap V.defAttr [])
                [widget]
                benchmarkRegion
                (const Nothing)
                emptyRenderState
    let !_ = force (show (displayOpsForPic picture benchmarkRegion))
    pure renderState

measure
    :: Int
    -> Widget Name
    -> IORef (RenderState Name)
    -> IO Sample
measure iterations widget stateRef = do
    performGC
    beforeStats <- getRTSStats
    beforeCpu <- getCPUTime
    beforeTime <- getMonotonicTimeNSec
    checksum <- redraw iterations 0
    checksum `seq` pure ()
    afterTime <- getMonotonicTimeNSec
    afterCpu <- getCPUTime
    afterStats <- getRTSStats
    pure Sample
        { elapsedMillis =
            fromIntegral (afterTime - beforeTime) / 1.0e6
        , cpuMillis =
            fromIntegral (afterCpu - beforeCpu) / 1.0e9
        , allocatedBytes =
            fromIntegral
                (allocated_bytes afterStats - allocated_bytes beforeStats)
        }
  where
    redraw remaining checksum
        | remaining <= 0 = pure checksum
        | otherwise = do
            renderState <- readIORef stateRef
            let (nextState, picture, _, extents) =
                    renderFinal
                        (attrMap V.defAttr [])
                        [widget]
                        benchmarkRegion
                        (const Nothing)
                        renderState
            let !rendered =
                    force
                        ( show (displayOpsForPic picture benchmarkRegion)
                        , length extents
                        )
            writeIORef stateRef $! nextState
            redraw
                (remaining - 1)
                (checksum + length (fst rendered) + snd rendered)

median :: [Sample] -> Sample
median samples =
    sortOn (.elapsedMillis) samples !! (length samples `div` 2)

printSample :: String -> Int -> Int -> Int -> Sample -> IO ()
printSample workload blockCount bodyLines sampleCount sample =
    putStrLn $
        unwords
            [ workload
            , "blocks=" <> show blockCount
            , "body_lines=" <> show bodyLines
            , "redraws_per_sample=" <> show redrawsPerSample
            , "samples=" <> show sampleCount
            , "elapsed_ms=" <> show sample.elapsedMillis
            , "cpu_ms=" <> show sample.cpuMillis
            , "allocated_bytes=" <> show sample.allocatedBytes
            ]

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf size values =
    let (prefix, suffix) = splitAt size values
    in prefix : chunksOf size suffix

emptyRenderState :: RenderState Name
emptyRenderState =
    read
        "RS {viewportMap = fromList [], rsScrollRequests = [], \
        \observedNames = fromList [], renderCache = fromList [], \
        \clickableNames = [], requestedVisibleNames_ = fromList [], \
        \reportedExtents = fromList []}"
