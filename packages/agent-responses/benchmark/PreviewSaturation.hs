module Main (main) where

import qualified Agent.Responses.LoopBackend.ToolArgumentPreview as New
import qualified BaselinePreview as Old
import Agent.Responses.Types
import Agent.Responses.Codec (decodeResponseStreamEvent)
import Agent.Loop (LoopEvent(..))
import Agent.ToolDispatch (ToolCall(..))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as Text
import Control.Exception (evaluate)
import Control.Monad (forM_, replicateM, unless)
import Data.IORef
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import System.Mem (performGC)
import Text.Printf (printf)

-- Force every projected event, including preview bodies, warnings and final
-- completion. Comparison is outside timing; checksums remain inside each run.
fingerprint :: [LoopEvent] -> Int
fingerprint = sum . map \case
    ToolArgumentsUpdated call -> checksum call.arguments
    ToolUpdated call -> checksum call.arguments
    event -> sum (map fromEnum (show event))
  where
    checksum = Text.foldl' (\n c -> n + fromEnum c) 0

replay step initial events = do
    state <- newIORef initial
    total <- newIORef (0 :: Int)
    forM_ events \event -> do
        emitted <- atomicModifyIORef' state \s -> step event s
        value <- evaluate (fingerprint emitted)
        modifyIORef' total (+ value)
    readIORef total

decode :: String -> ResponseStreamEvent
decode = either (error . Text.unpack) id . decodeResponseStreamEvent . LBS.toStrict . LBS.pack

main :: IO ()
main = do
    [name, n, r, s] <- getArgs
    let count = read n :: Int
        reps = read r :: Int
        samples = read s :: Int
        event = decode "{\"type\":\"response.function_call_arguments.delta\",\"item_id\":\"fc\",\"output_index\":0,\"delta\":\"abcdefghijklmnop\"}"
        argumentDelta text = decode (LBS.unpack (Aeson.encode (Aeson.object
            [ "type" Aeson..= ("response.function_call_arguments.delta" :: Text.Text)
            , "item_id" Aeson..= ("fc" :: Text.Text)
            , "output_index" Aeson..= (0 :: Int)
            , "delta" Aeson..= text
            ])))
        events =
            [decode ("{\"type\":\"response.output_item.added\",\"output_index\":0,\"item\":{\"type\":\"function_call\",\"id\":\"fc\",\"call_id\":\"call\",\"name\":\"" <> name <> "\",\"arguments\":\"\"}}")]
            <> [argumentDelta ("{\"command\":\"" :: Text.Text)]
            <> replicate count event
            <> [argumentDelta ("\"}" :: Text.Text)]
            <> [decode ("{\"type\":\"response.output_item.done\",\"output_index\":0,\"item\":{\"type\":\"function_call\",\"id\":\"fc\",\"call_id\":\"call\",\"name\":\"" <> name <> "\",\"arguments\":\"\"}}")]
        variants =
            [ ("old" :: String, replay Old.toolArgumentStreamStep Old.emptyToolArgumentStreamState events)
            , ("new", replay New.toolArgumentStreamStep New.emptyToolArgumentStreamState events)
            ]
    unless (minimum [count,reps,samples] > 0) (fail "positive dimensions required")
    getRTSStatsEnabled >>= \enabled -> unless enabled (fail "requires +RTS -T")
    _ <- evaluate (sum (map (length . show) events))
    oldEvents <- projected Old.toolArgumentStreamStep Old.emptyToolArgumentStreamState events
    newEvents <- projected New.toolArgumentStreamStep New.emptyToolArgumentStreamState events
    unless (oldEvents == newEvents) (fail "projected events differ")
    expected <- snd (head variants)
    putStrLn "variant,tool,deltas,reps,sample,cpu_ms,wall_ms,allocated_bytes"
    forM_ [1..samples] \sample ->
        forM_ (if odd sample then variants else reverse variants) \(label,run) -> do
            performGC
            before <- getRTSStats
            cpu0 <- getCPUTime
            wall0 <- getMonotonicTimeNSec
            results <- replicateM reps run
            unless (all (== expected) results) (fail "checksum mismatch")
            wall1 <- getMonotonicTimeNSec
            cpu1 <- getCPUTime
            performGC
            after <- getRTSStats
            printf "%s,%s,%d,%d,%d,%.6f,%.6f,%.0f\n" label name count reps sample
                (fromIntegral (cpu1-cpu0) / 1e9 / fromIntegral reps :: Double)
                (fromIntegral (wall1-wall0) / 1e6 / fromIntegral reps :: Double)
                (fromIntegral (after.allocated_bytes-before.allocated_bytes) / fromIntegral reps :: Double)
  where
    projected step initial events = do
        ref <- newIORef initial
        results <- mapM (\event -> atomicModifyIORef' ref \state ->
            let (next, output) = step event state in (next, map show output)) events
        evaluate (Text.pack (concat (concat results)))
