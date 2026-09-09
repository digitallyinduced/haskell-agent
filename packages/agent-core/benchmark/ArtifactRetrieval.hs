{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- Compile with -O2 and run with +RTS -T. Single-line specialization of the
-- previous streaming search is retained as the baseline: scan the entire line,
-- but return only its first 16K characters, even when the match occurs later.
module Main (main) where

import Agent.Tools.OutputArtifact.Retrieval
import Control.Exception (evaluate)
import Control.Monad (forM_, replicateM)
import Data.Aeson (encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.List (sort)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as LazyEncoding
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats
import System.CPUTime (getCPUTime)
import System.Directory (getTemporaryDirectory, removeFile)
import System.Environment (getArgs)
import System.IO (IOMode(ReadMode), withBinaryFile, openBinaryTempFile, hClose)
import System.Mem (performGC)
import Text.Printf (printf)

main :: IO ()
main = do
    arguments <- getArgs
    case arguments of
        ["residency", path] -> do
            bytes <- occurrences True path
            print (BS.length bytes)
        [] -> runSamples
        _ -> error "usage: artifact-retrieval-benchmark [residency FILE]"

runSamples :: IO ()
runSamples = do
    root <- getTemporaryDirectory
    forM_ [65536, 262144, 1048576, 8388608, 1048576] $ \size ->
        forM_ [False, True] $ \late -> do
            let padding = BS.replicate (size - 6) 120
                payload = if late then padding <> "needle" else "needle" <> padding
            (path, handle) <- openBinaryTempFile root "artifact-retrieval-benchmark"
            BS.hPut handle payload
            hClose handle
            forM_
                [ ("baseline", baseline path)
                , ("occurrences", occurrences False path)
                , ("baseline-folded", baselineFolded path)
                , ("occurrences-folded", occurrences True path)
                ] $ \(label, action) -> do
                samples <- replicateM 7 (measure action)
                let median projection = sort (map projection samples) !! 3
                printf "%s,size=%d,late=%s,elapsed-ms=%.4f,cpu-ms=%.4f,allocated=%d\n"
                    (label :: String) size (show late)
                    (median (\(a,_,_) -> a)) (median (\(_,b,_) -> b))
                    (median (\(_,_,c) -> c))
            removeFile path

occurrences :: Bool -> FilePath -> IO BS.ByteString
occurrences insensitive path = withBinaryFile path ReadMode $ \handle -> do
    content <- LazyEncoding.decodeUtf8 <$> BL.hGetContents handle
    let result = either (error . Text.unpack) id
            (searchArtifactOccurrences content "needle" insensitive 0 1 128)
        bytes = BL.toStrict (encode result)
    _ <- evaluate (BS.length bytes)
    pure bytes

baseline :: FilePath -> IO BS.ByteString
baseline path = withBinaryFile path ReadMode $ \handle -> do
    let go !matched !carry !preview = do
            chunk <- BS.hGetSome handle 32768
            if BS.null chunk
                then pure (if matched then preview else "")
                else do
                    let candidate = carry <> chunk
                        matched' = matched || "needle" `BS.isInfixOf` candidate
                        carry' = if matched' then "" else BS.copy (BS.drop (max 0 (BS.length candidate - 5)) candidate)
                        preview' = if BS.length preview >= 16384 then preview
                            else preview <> BS.take (16384 - BS.length preview) chunk
                    go matched' carry' preview'
    go False "" ""

baselineFolded :: FilePath -> IO BS.ByteString
baselineFolded path = withBinaryFile path ReadMode $ \handle -> do
    content <- LazyEncoding.decodeUtf8 <$> BL.hGetContents handle
    let matches =
            [ LazyText.take 16384 line
            | line <- LazyText.lines content
            , "needle" `LazyText.isInfixOf` LazyText.toCaseFold line
            ]
        matchCount = length (take 2 matches)
        bytes = matchCount `seq` BL.toStrict (LazyEncoding.encodeUtf8 (LazyText.intercalate "\n" (take 1 matches)))
    _ <- evaluate (BS.length bytes)
    pure bytes

measure :: IO BS.ByteString -> IO (Double, Double, Integer)
measure action = do
    performGC
    before <- getRTSStats
    wallStart <- getMonotonicTimeNSec
    cpuStart <- getCPUTime
    bytes <- action
    _ <- evaluate (BS.foldl' (\n b -> n * 33 + fromIntegral b) (5381 :: Int) bytes)
    cpuEnd <- getCPUTime
    wallEnd <- getMonotonicTimeNSec
    performGC
    after <- getRTSStats
    pure ( fromIntegral (wallEnd - wallStart) / 1e6
         , fromIntegral (cpuEnd - cpuStart) / 1e9
         , fromIntegral (after.allocated_bytes - before.allocated_bytes)
         )
