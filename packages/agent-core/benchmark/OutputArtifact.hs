{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Agent.ToolDispatch
    ( ToolCall(..)
    , ToolCallResult(..)
    , ToolDispatchConfig(..)
    , dispatchToolHandler
    , functionToolCall
    )
import Agent.Tools.OutputArtifact
    ( OutputArtifact(..)
    , artifactTools
    , outputArtifactMetadata
    , readOutputArtifact
    , writeOutputArtifactDetailed
    )
import Agent.Tools.Types
    ( AppTool(..)
    , ToolEnv
    , defaultToolEnv
    , setToolSessionTmp
    )
import Control.Exception (evaluate)
import Control.Monad (when)
import qualified Data.ByteString as ByteString
import Data.List (find, sort)
import qualified Data.Text as Text
import GHC.Stats
    ( GCDetails(..)
    , RTSStats(..)
    , getRTSStats
    , getRTSStatsEnabled
    )
import System.CPUTime (getCPUTime)
import System.Directory
    ( createDirectory
    , doesDirectoryExist
    , getTemporaryDirectory
    , removeDirectoryRecursive
    )
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath ((</>))
import System.Mem (performGC)
import System.OsPath (unsafeEncodeUtf)
import GHC.Clock (getMonotonicTimeNSec)
import Text.Printf (printf)

data Sample = Sample
    { elapsedMillis :: !Double
    , cpuMillis :: !Double
    , allocatedBytes :: !Integer
    , liveDeltaBytes :: !Integer
    }

main :: IO ()
main = do
    enabled <- getRTSStatsEnabled
    if not enabled
        then die "RTS statistics are disabled; run with +RTS -T"
        else pure ()
    (sizeMb, samples) <- parseArgs =<< getArgs
    tmp <- getTemporaryDirectory
    let root = tmp </> "agent-output-artifact-bench"
        session = root </> "session"
    exists <- doesDirectoryExist root
    if exists then removeDirectoryRecursive root else pure ()
    createDirectory root
    createDirectory session
    env <- defaultToolEnv (unsafeEncodeUtf root)
    setToolSessionTmp env (Just (unsafeEncodeUtf session))
    let line = "needle: this is a representative tool output line\n"
        payload = ByteString.concat
            (replicate ((sizeMb * 1024 * 1024) `div` ByteString.length line) line)
    writeOutputArtifactDetailed env payload >>= \case
        Left err -> die (Text.unpack err)
        Right artifact -> do
            compareOutputs "read"
                (legacyRead env artifact.artifactHandle)
                (streamingRead env artifact.artifactHandle)
            compareOutputs "search"
                (legacySearch env artifact.artifactHandle)
                (streamingSearch env artifact.artifactHandle)
            benchmark "legacy-read" samples $
                legacyRead env artifact.artifactHandle
            benchmark "streaming-read" samples $
                streamingRead env artifact.artifactHandle
            benchmark "legacy-search" samples $
                legacySearch env artifact.artifactHandle
            benchmark "streaming-search" samples $
                streamingSearch env artifact.artifactHandle
            benchmark "streaming-metadata" samples $
                outputArtifactMetadata env artifact.artifactHandle
                    >>= either (die . Text.unpack) (pure . Text.pack . show)
    removeDirectoryRecursive root

compareOutputs :: String -> IO Text.Text -> IO Text.Text -> IO ()
compareOutputs label oldAction newAction = do
    oldOutput <- oldAction
    newOutput <- newAction
    let checksumText = Text.foldl' checksum 5381
    when (checksumText oldOutput /= checksumText newOutput) $
        die (label <> " baseline and streaming outputs differ")

parseArgs :: [String] -> IO (Int, Int)
parseArgs args =
    case map reads args of
        [] -> pure (32, 3)
        [[(size, "")], [(samples, "")]]
            | size > 0 && samples > 0 -> pure (size, samples)
        _ -> die "usage: output-artifact-bench [SIZE_MB SAMPLES]"

benchmark :: String -> Int -> IO Text.Text -> IO ()
benchmark label count action = do
    samples <- mapM (const (measure action)) [1 .. count]
    let median field = sort (map field samples) !! (length samples `div` 2)
    printf "%s,elapsed-ms=%.3f,cpu-ms=%.3f,allocated=%d,live-delta-after-gc=%d\n"
        label
        (median (.elapsedMillis))
        (median (.cpuMillis))
        (median (.allocatedBytes))
        (median (.liveDeltaBytes))

measure :: IO Text.Text -> IO Sample
measure action = do
    performGC
    before <- getRTSStats
    let beforeLive = before.gc.gcdetails_live_bytes
    wallStart <- getMonotonicTimeNSec
    cpuStart <- getCPUTime
    result <- action
    _ <- evaluate (Text.foldl' checksum 5381 result)
    cpuEnd <- getCPUTime
    wallEnd <- getMonotonicTimeNSec
    after <- getRTSStats
    performGC
    settled <- getRTSStats
    pure Sample
        { elapsedMillis = fromIntegral (wallEnd - wallStart) / 1.0e6
        , cpuMillis = fromIntegral (cpuEnd - cpuStart) / 1.0e9
        , allocatedBytes =
            fromIntegral (after.allocated_bytes - before.allocated_bytes)
        , liveDeltaBytes =
            fromIntegral settled.gc.gcdetails_live_bytes
                - fromIntegral beforeLive
        }

checksum :: Int -> Char -> Int
checksum value character = value * 33 + fromEnum character

legacyRead :: ToolEnv -> Text.Text -> IO Text.Text
legacyRead env handle = do
    content <- readOutputArtifact env handle >>= either (die . Text.unpack) pure
    let selected = take 200 (drop 0 (Text.lines content))
        end = length selected
    pure ("artifact " <> handle <> " lines 1-" <> Text.pack (show end)
        <> ":\n" <> Text.intercalate "\n" selected)

streamingRead :: ToolEnv -> Text.Text -> IO Text.Text
streamingRead env handle =
    runArtifactTool env $
        functionToolCall "read" "read_tool_output"
            ( "{\"handle\":\"" <> handle <> "\",\"offset\":1,\"limit\":200}" )

legacySearch :: ToolEnv -> Text.Text -> IO Text.Text
legacySearch env handle = do
    content <- readOutputArtifact env handle >>= either (die . Text.unpack) pure
    let matches =
            [ Text.pack (show n) <> ":" <> line
            | (n, line) <- zip [1 :: Int ..] (Text.lines content)
            , "needle" `Text.isInfixOf` line
            ]
        shown = take 5 matches
        suffix
            | length matches > 5 =
                "\n[search truncated after 5 matches]"
            | otherwise = ""
    pure (Text.intercalate "\n" shown <> suffix)

streamingSearch :: ToolEnv -> Text.Text -> IO Text.Text
streamingSearch env handle =
    runArtifactTool env $
        functionToolCall "search" "search_tool_output"
            ( "{\"handle\":\"" <> handle
                <> "\",\"pattern\":\"needle\",\"head_limit\":5}" )

runArtifactTool :: ToolEnv -> ToolCall -> IO Text.Text
runArtifactTool env call = do
    let tool = find ((== call.name) . (.appToolName)) (artifactTools env Nothing)
        config = ToolDispatchConfig
            { toolDispatchUnknownTool = ("unknown tool: " <>)
            , toolDispatchFormatResult = either id id
            , toolDispatchFormatException = \_ exception ->
                Text.pack (show exception)
            , toolDispatchOnException = \_ _ -> pure ()
            , toolDispatchOnOutput = \_ _ -> pure ()
            , toolDispatchFinalizeOutput = \_ output -> pure output
            }
    result <- dispatchToolHandler config ((.appToolHandler) <$> tool) call
    pure result.output
