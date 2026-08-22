module Main (main) where

import Agent.CLI.Benchmark
    ( defaultBenchmarkOptions
    , parseBenchmarkArgs
    , runBenchmark
    )
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
    defaults <- defaultBenchmarkOptions
    args <- getArgs
    either die runBenchmark (parseBenchmarkArgs defaults args)
