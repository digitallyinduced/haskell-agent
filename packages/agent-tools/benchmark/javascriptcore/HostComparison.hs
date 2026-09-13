{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main (main) where

import Agent.Tools.CodeMode.Host
import Control.Concurrent.Async (mapConcurrently)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Exception.Safe (bracket)
import Control.Monad (forM, forM_, replicateM_, unless)
import Data.Aeson (Value(..), object, (.=))
import Data.List (sort)
import qualified Data.Text as Text
import GHC.Clock (getMonotonicTimeNSec)
import System.Environment (getArgs)
import Text.Printf (printf)

-- Actual Haskell host on BOTH sides: same protocol, pool, tool dispatch and
-- decoded outputs. No in-process-prototype timings appear in this comparison.
main :: IO ()
main = do
    arguments <- getArgs
    (worker, native, pools) <- case arguments of
        [script, executable] -> pure (script, executable, [0, 2])
        [script, executable, "--warm-only"] -> pure (script, executable, [2])
        _ -> fail "usage: host-comparison BUN_WORKER_SCRIPT NATIVE_EXECUTABLE [--warm-only]"
    let configure backend pool = (defaultCodeModeConfig worker (\_ value -> pure (Right value)))
            { codeModeBackend = backend, nativeWorkerExecutable = native, workerPoolSize = pool }
        metadata = ["echo"] <> map (Text.pack . ("unused_" <>) . show) [1..120 :: Int]
        expected textValue = object ["content" .= [object ["type" .= ("text" :: Text.Text), "text" .= textValue]]]
        execute host source output = do
            result <- execCodeCell host source metadata 3000 >>= finish host
            unless (result == expected output) (fail ("unexpected result: " <> show result))
            evaluate (force result) >> pure ()
    forM_ pools $ \pool ->
        bracket (newCodeModeHost (configure BunBackend pool)) closeCodeModeHost $ \bun ->
        bracket (newCodeModeHost (configure JavaScriptCoreBackend pool)) closeCodeModeHost $ \jsc ->
        forM_ [(0,16,1), (1,16,1), (10,16,1), (100,16,1), (1,4096,1), (10,4096,1), (1,65536,1), (10,65536,1), (1,1048576,1), (10,4096,8)] $ \(calls, bytes, concurrent) -> do
            let source = Text.pack (workload calls bytes)
                output = if calls == 0 then "ok" else Text.pack (show (calls * bytes))
                run host = mapConcurrently (\_ -> execute host source output) [1..concurrent :: Int] >> pure ()
                cells = if pool == 0 then 5 else 30
            evaluate (force source)
            replicateM_ 5 (run bun >> run jsc)
            samples <- forM [1..7 :: Int] $ \sample ->
                if odd sample then do
                    baseline <- measure cells (run bun)
                    candidate <- measure cells (run jsc)
                    pure (baseline, candidate)
                else do
                    candidate <- measure cells (run jsc)
                    baseline <- measure cells (run bun)
                    pure (baseline, candidate)
            printf "pool=%d calls=%d bytes=%d concurrency=%d batches=%d samples=7 bun_ms=%.4f javascriptcore_ms=%.4f\n"
                pool calls bytes concurrent cells (median (map fst samples)) (median (map snd samples))

finish :: CodeModeHost -> Either CodeModeError CodeModeResult -> IO Value
finish host result = case result of
    Right CodeModeRunning{cellId} -> waitCodeCell host cellId 3000 >>= finish host
    Right CodeModeFinished{cellValue} -> pure cellValue
    other -> fail ("execution failed: " <> show other)

workload :: Int -> Int -> String
workload 0 _ = "text('ok');"
workload calls bytes =
    "const payload = 'x'.repeat(" <> show bytes <> "); let total = 0; "
    <> "for (let index = 0; index < " <> show calls <> "; index++) { "
    <> "const result = await tools.echo({payload}); total += result.payload.length; } text(total);"

measure :: Int -> IO () -> IO Double
measure cells action = do
    start <- getMonotonicTimeNSec
    replicateM_ cells action
    end <- getMonotonicTimeNSec
    pure (fromIntegral (end - start) / 1e6 / fromIntegral cells)

median :: [Double] -> Double
median samples = sort samples !! (length samples `div` 2)
