module Main (main) where

import Agent.CLI.TUI.ImagePreview
    ( TuiImagePreview(..)
    , imageDimensions
    , prepareTuiImagePreview
    )
import Agent.Loop (ImageAttachment(..))
import Codec.Picture
    ( PixelRGB8(..)
    , PixelRGBA8(..)
    , convertRGBA8
    , decodeImage
    , encodePng
    , generateImage
    , imageHeight
    , imageWidth
    , pixelAt
    )
import Control.Exception (evaluate)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List (sort)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats
    ( RTSStats(..)
    , getRTSStats
    , getRTSStatsEnabled
    )
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import System.Mem (performMajorGC)

data Sample = Sample
    { sampleElapsedMillis :: !Double
    , sampleCpuMillis :: !Double
    , sampleAllocatedBytes :: !Word
    }

main :: IO ()
main = do
    (width, height, samples) <- parseArgs <$> getArgs
    statsEnabled <- getRTSStatsEnabled
    if not statsEnabled
        then fail "run with +RTS -T"
        else do
            let source =
                    generateImage
                        (\x y ->
                            PixelRGB8
                                (fromIntegral (x * 31 + y * 17))
                                (fromIntegral (x * 13 + y * 29))
                                (fromIntegral (x * 7 + y * 37)))
                        width
                        height
                encoded = LBS.toStrict (encodePng source)
            BS.length encoded `seq`
                putStrLn
                    ("png="
                        <> show width
                        <> "x"
                        <> show height
                        <> " bytes="
                        <> show (BS.length encoded)
                        <> " samples="
                        <> show samples)
            benchmark "old-full-decode" samples oldDimensions encoded
            benchmark "new-ansi-preview" samples newAnsiPreview encoded
            benchmark "new-header-only" samples newDimensions encoded

parseArgs :: [String] -> (Int, Int, Int)
parseArgs = \case
    [width, height, samples] ->
        (read width, read height, read samples)
    _ -> (3840, 2160, 7)

benchmark
    :: String
    -> Int
    -> (Int -> BS.ByteString -> Int)
    -> BS.ByteString
    -> IO ()
benchmark label count action source = do
    measured <- mapM (\nonce -> measure action nonce source) [1 .. max 1 count]
    let elapsed = median (map (.sampleElapsedMillis) measured)
        cpu = median (map (.sampleCpuMillis) measured)
        allocated = median (map (.sampleAllocatedBytes) measured)
    putStrLn
        (label
            <> " elapsed-ms="
            <> show elapsed
            <> " cpu-ms="
            <> show cpu
            <> " allocated-bytes="
            <> show allocated)

measure :: (Int -> BS.ByteString -> Int) -> Int -> BS.ByteString -> IO Sample
measure action nonce source = do
    let input = BS.copy source
    BS.length input `seq` pure ()
    performMajorGC
    beforeStats <- getRTSStats
    beforeCpu <- getCPUTime
    beforeElapsed <- getMonotonicTimeNSec
    checksum <- evaluate (action nonce input)
    afterElapsed <- getMonotonicTimeNSec
    afterCpu <- getCPUTime
    afterStats <- getRTSStats
    checksum `seq`
        pure Sample
            { sampleElapsedMillis =
                fromIntegral (afterElapsed - beforeElapsed) / 1_000_000
            , sampleCpuMillis =
                fromIntegral (afterCpu - beforeCpu) / 1_000_000_000
            , sampleAllocatedBytes =
                fromIntegral
                    (allocated_bytes afterStats - allocated_bytes beforeStats)
            }

{-# NOINLINE oldDimensions #-}
oldDimensions :: Int -> BS.ByteString -> Int
oldDimensions nonce bytes =
    case decodeImage bytes of
        Left err -> error err
        Right dynamic ->
            let image = convertRGBA8 dynamic
                width = imageWidth image
                height = imageHeight image
                PixelRGBA8 red green blue alpha =
                    pixelAt image (width - 1) (height - 1)
            in width
                + height
                + fromIntegral red
                + fromIntegral green
                + fromIntegral blue
                + fromIntegral alpha
                + nonce

{-# NOINLINE newDimensions #-}
newDimensions :: Int -> BS.ByteString -> Int
newDimensions nonce bytes =
    case imageDimensions "image/png" bytes of
        Left err -> error (show err)
        Right (width, height) -> width + height + nonce

{-# NOINLINE newAnsiPreview #-}
newAnsiPreview :: Int -> BS.ByteString -> Int
newAnsiPreview nonce bytes =
    case prepareTuiImagePreview
        ImageAttachment
            { imageMime = "image/png"
            , imageBytes = bytes
            } of
        Left err -> error (show err)
        Right preview ->
            let image = preview.previewSample
                width = imageWidth image
                height = imageHeight image
                PixelRGB8 red green blue =
                    pixelAt image (width - 1) (height - 1)
            in width
                + height
                + fromIntegral red
                + fromIntegral green
                + fromIntegral blue
                + nonce

median :: Ord a => [a] -> a
median values =
    sort values !! (length values `div` 2)
