{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Agent.Tools.CodeMode.Host as Bun
import Control.DeepSeq (force)
import Control.Exception.Safe (bracket)
import Control.Exception (evaluate)
import Control.Monad (forM, forM_, replicateM_, unless)
import Data.Aeson (Value(..), eitherDecodeStrict', encode)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as Lazy
import Data.List (sort)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as Vector
import GHC.Clock (getMonotonicTimeNSec)
import qualified JavaScriptCorePrototype as JavaScriptCore
import System.Environment (getArgs)
import Text.Printf (printf)
import Text.Read (readMaybe)

-- Compare a deliberately limited prototype with the actual production Bun host.
-- This is not a feature-equivalent backend or an end-to-end application benchmark.
main :: IO ()
main = do
    arguments <- getArgs
    (worker, cells, samples) <- case arguments of
        [path, count, repetitions]
            | Just count' <- readMaybe count, count' > 0
            , Just repetitions' <- (readMaybe repetitions :: Maybe Int), repetitions' > 0 ->
                pure (path, count', repetitions')
        _ -> fail "usage: javascriptcore-comparison WORKER_PATH CELLS SAMPLES"
    let config = Bun.defaultCodeModeConfig worker (\_ value -> pure (Right value))
    bracket (Bun.newCodeModeHost config) Bun.closeCodeModeHost $ \host ->
        JavaScriptCore.withRuntime $ \runtime -> do
            let runBun source = do
                    result <- Bun.execCodeCell host (Text.pack source) ["echo", "reject"] 3000
                    finishBun host result
                runNative source = do
                    result <- JavaScriptCore.executeCellWith runtime echoPayload source
                    either fail pure result
            forM_ [(0, 16), (1, 16), (10, 16), (100, 16), (1, 4096), (10, 4096)] $ \(calls, bytes) -> do
                let source = workload calls bytes
                    expected = [if calls == 0 then "ok" else show (calls * bytes)]
                    checked run = run source >>= \actual -> do
                        unless (actual == expected) $ fail ("unexpected output: " <> show actual)
                        evaluate (force actual) >> pure ()
                _ <- evaluate (force source)
                replicateM_ 5 (checked runBun >> checked runNative)
                pairs <- forM [1..samples] $ \index ->
                    if odd index then do
                        baseline <- measure cells (checked runBun)
                        native <- measure cells (checked runNative)
                        pure (baseline, native)
                    else do
                        native <- measure cells (checked runNative)
                        baseline <- measure cells (checked runBun)
                        pure (baseline, native)
                printf "calls=%d bytes=%d cells=%d samples=%d bun_ms=%.4f javascriptcore_ms=%.4f\n"
                    calls bytes cells samples (median (map fst pairs)) (median (map snd pairs))
echoPayload :: String -> IO (Either String String)
echoPayload payload = pure $ case eitherDecodeStrict' (Text.encodeUtf8 (Text.pack payload)) of
    Left message -> Left message
    Right value -> Right (Text.unpack (Text.decodeUtf8 (Lazy.toStrict (encode (value :: Value)))))

workload :: Int -> Int -> String
workload 0 _ = "text('ok');"
workload calls bytes =
    "const payload = 'x'.repeat(" <> show bytes <> "); let total = 0; "
    <> "for (let index = 0; index < " <> show calls <> "; index++) { "
    <> "const result = await tools.echo({payload}); total += result.payload.length; } text(total);"

finishBun :: Bun.CodeModeHost -> Either Bun.CodeModeError Bun.CodeModeResult -> IO [String]
finishBun host result = case result of
    Right Bun.CodeModeRunning{Bun.cellId = identifier} ->
        Bun.waitCodeCell host identifier 3000 >>= finishBun host
    Right Bun.CodeModeFinished{Bun.cellValue = value} -> case value of
        Object object | Just (Array content) <- KeyMap.lookup "content" object ->
            mapM extractText (Vector.toList content)
        _ -> fail ("unexpected Bun result: " <> show value)
    _ -> fail ("Bun execution failed: " <> show result)
  where
    extractText (Object item) | Just (String value) <- KeyMap.lookup "text" item = pure (Text.unpack value)
    extractText value = fail ("unexpected content: " <> show value)

measure :: Int -> IO () -> IO Double
measure cells action = do
    started <- getMonotonicTimeNSec
    replicateM_ cells action
    finished <- getMonotonicTimeNSec
    pure (fromIntegral (finished - started) / 1e6 / fromIntegral cells)

median :: [Double] -> Double
median values = sort values !! (length values `div` 2)
