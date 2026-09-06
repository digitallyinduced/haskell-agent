{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}

-- Synthetic startup benchmark: no microphone, credentials or network needed.
-- The baseline models opening the microphone after provider setup; buffered
-- uses the production capture helper with identical PCM and setup delays.
module Main (main) where

import Agent.CLI.Dictation.Capture (withBufferedCapture)
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Exception (evaluate)
import Control.Monad (forM_, replicateM, unless)
import qualified Data.ByteString as BS
import Data.IORef
import Data.List (sort)
import GHC.Clock (getMonotonicTimeNSec)
import Text.Printf (printf)

main :: IO ()
main = forM_ [200_000, 1_000_000, 200_000] \setupDelay ->
    forM_ [10, 100] \chunkCount ->
        forM_ [False, True] \buffered -> do
            samples <- replicateM 5 (sample buffered setupDelay chunkCount)
            let (ready, total) = unzip samples
            printf "%s setup=%dms chunks=%d ready=%.2fms total=%.2fms\n"
                (if buffered then "buffered" else "baseline" :: String)
                (setupDelay `div` 1000) chunkCount (median ready) (median total)

sample :: Bool -> Int -> Int -> IO (Double, Double)
sample buffered setupDelay chunkCount = do
    -- Force fixture outside the measured interval.
    let chunk = BS.replicate 4800 7
    expected <- evaluate (chunkCount * BS.length chunk)
    _ <- evaluate (BS.foldl' (\a b -> a + fromIntegral b) (0 :: Int) chunk)
    ready <- newEmptyMVar
    received <- newIORef (0 :: Int)
    let onRecording = getMonotonicTimeNSec >>= putMVar ready
        capture send = do
            threadDelay 20_000 -- same simulated hardware startup in both paths
            forM_ [1 .. chunkCount] \_ -> send chunk
        consume produce = do
            threadDelay setupDelay
            produce \bytes -> modifyIORef' received (+ BS.length bytes)
    start <- getMonotonicTimeNSec
    if buffered
        then withBufferedCapture (32 * 1024 * 1024) onRecording capture consume
        else consume \send -> do
            first <- newIORef True
            capture \bytes -> do
                isFirst <- atomicModifyIORef' first (\old -> (False, old))
                if isFirst then onRecording else pure ()
                send bytes
    end <- getMonotonicTimeNSec
    firstAudio <- takeMVar ready
    actual <- readIORef received
    unless (actual == expected) (fail "audio lost in benchmark")
    pure (fromIntegral (firstAudio - start) / 1e6, fromIntegral (end - start) / 1e6)

median :: [Double] -> Double
median xs = sort xs !! (length xs `div` 2)
