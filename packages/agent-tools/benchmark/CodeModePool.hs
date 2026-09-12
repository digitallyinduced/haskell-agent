module Main (main) where

import Agent.Tools.CodeMode.Host
import Control.Exception.Safe (bracket)
import Control.Monad (replicateM, replicateM_)
import Data.List (sort)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import Text.Printf (printf)
import Text.Read (readMaybe)

main :: IO ()
main = do
    (poolSize, cells, samples) <- parseArgs =<< getArgs
    worker <- codeModeWorkerPath
    results <- replicateM samples (sample worker poolSize cells)
    let elapsed = median (map fst results)
        cpu = median (map snd results)
    printf
        "pool-size=%d cells=%d samples=%d median elapsed=%.3fms cpu=%.3fms\n"
        poolSize
        cells
        samples
        (elapsed * 1000)
        (cpu * 1000)

sample :: FilePath -> Int -> Int -> IO (Double, Double)
sample worker poolSize cells =
    bracket
        (newCodeModeHost config)
        closeCodeModeHost
        \host -> do
            startedAt <- getCurrentTime
            startedCpu <- getCPUTime
            replicateM_ cells do
                execCodeCell host "text(\"ok\");" [] 3000 >>= finish host
            finishedCpu <- getCPUTime
            finishedAt <- getCurrentTime
            pure
                ( realToFrac (diffUTCTime finishedAt startedAt)
                , fromIntegral (finishedCpu - startedCpu) / 1e12
                )
  where
    finish host = \case
                    Right CodeModeFinished{} -> pure ()
                    Right CodeModeRunning{cellId} ->
                        waitCodeCell host cellId 3000 >>= finish host
                    other -> fail ("code-mode execution failed: " <> show other)
    config =
        (defaultCodeModeConfig worker noTools)
            { workerPoolSize = poolSize }
    noTools _ _ = pure (Left "no tools")

parseArgs :: [String] -> IO (Int, Int, Int)
parseArgs [poolArg, cellsArg, samplesArg]
    | Just poolSize <- readMaybe poolArg
    , Just cells <- readMaybe cellsArg
    , Just samples <- readMaybe samplesArg
    , poolSize >= 0
    , cells > 0
    , samples > 0 =
        pure (poolSize, cells, samples)
parseArgs _ =
    fail "usage: code-mode-pool-bench POOL_SIZE CELLS SAMPLES"

median :: [Double] -> Double
median values =
    let ordered = sort values
    in ordered !! (length ordered `div` 2)
