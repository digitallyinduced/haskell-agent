{-# LANGUAGE BangPatterns #-}

-- | Production-shaped benchmark for Responses Lite tool construction.
--
-- The legacy implementation is kept locally so this benchmark can compare
-- the same workload without checking out a second tree.
-- Run with:
--   nix develop -c cabal run responses-lite-tools-bench -- 7 +RTS -T
-- For interactive inspection:
--   nix develop -c cabal repl agent-cli:bench:responses-lite-tools-bench
module Main (main) where

import Agent.CLI.Request (requestParams)
import Agent.CLI.Tools (webSearchTool)
import Agent.Provider (Provider(..))
import Agent.Responses.Types
import Control.Applicative ((<|>))
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (forM, unless)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats (RTSStats(..), getRTSStats, getRTSStatsEnabled)
import System.Environment (getArgs)
import System.Exit (die)
import System.CPUTime (getCPUTime)
import System.Mem (performGC)
import Text.Printf (printf)

data Sample = Sample
    { elapsedMillis :: !Double
    , cpuMillis :: !Double
    , allocatedBytes :: !Integer
    }

main :: IO ()
main = do
    enabled <- getRTSStatsEnabled
    unless enabled $
        die "RTS statistics are disabled; run with +RTS -T"
    args <- getArgs
    (sizes, sampleCount) <- case args of
        [] -> pure ([10, 100, 1000, 10000], 7)
        [raw] -> do
            samples <- parsePositive "sample count" raw
            pure ([10, 100, 1000, 10000], samples)
        [mode, count, raw] -> do
            unless (mode == "compare") $
                die "only MODE=compare is supported"
            size <- parsePositive "tool count" count
            samples <- parsePositive "sample count" raw
            pure ([size], samples)
        _ -> die "usage: responses-lite-tools-bench [MODE COUNT SAMPLES]"
    putStrLn "size,optimized_ms,legacy_ms,optimized_cpu_ms,legacy_cpu_ms,optimized_alloc_bytes,legacy_alloc_bytes,checksum"
    mapM_ (\size -> do
        samples <- forM [1 .. sampleCount] \sampleIndex -> do
            let tools = workload size sampleIndex
                optimized = productionValues tools
                legacy = legacyValues tools
            optimizedValues <- evaluate (force optimized)
            legacyValues' <- evaluate (force legacy)
            unless (optimizedValues == legacyValues') $
                die ("implementation mismatch at size " <> show size)
            let checksum = encodedChecksum optimizedValues
                legacyChecksum = encodedChecksum legacyValues'
            unless (checksum == legacyChecksum) $
                die ("checksum mismatch at size " <> show size)
            (optimizedSample, legacySample) <-
                if even sampleIndex
                    then do
                        optimizedSample <- measure
                            (evaluate (encodedChecksum (productionValues tools)))
                        legacySample <- measure
                            (evaluate (encodedChecksum (legacyValues tools)))
                        pure (optimizedSample, legacySample)
                    else do
                        legacySample <- measure
                            (evaluate (encodedChecksum (legacyValues tools)))
                        optimizedSample <- measure
                            (evaluate (encodedChecksum (productionValues tools)))
                        pure (optimizedSample, legacySample)
            pure (optimizedSample, legacySample, checksum)
        let optimizedSamples = map first samples
            legacySamples = map second samples
            checksum = last (map third samples)
            optimizedMedian = median optimizedSamples
            legacyMedian = median legacySamples
        printf
            "%d,%.3f,%.3f,%.3f,%.3f,%d,%d,%d\n"
            size
            optimizedMedian.elapsedMillis
            legacyMedian.elapsedMillis
            optimizedMedian.cpuMillis
            legacyMedian.cpuMillis
            optimizedMedian.allocatedBytes
            legacyMedian.allocatedBytes
            checksum) sizes
  where
    first (value, _, _) = value
    second (_, value, _) = value
    third (_, _, value) = value

parsePositive :: String -> String -> IO Int
parsePositive label raw =
    case reads raw of
        [(value, "")] | value > 0 -> pure value
        _ -> die ("invalid " <> label <> ": " <> raw)

-- Distinct tool names per sample prevent the benchmark from measuring reused
-- thunks. The mix also exercises grouped, nested, and top-level tools.
workload :: Int -> Int -> [ResponseTool]
workload size sampleIndex =
    [ case index `mod` 5 of
        0 -> functionTool name
        1 -> customTool name
        2 -> namespaceTool "functions" (Just description) [functionTool name]
        3 -> namespaceTool "editor" Nothing [functionTool name]
        _ -> webSearchTool
    | index <- [0 .. size - 1]
    , let name = "tool-" <> Text.pack (show sampleIndex) <> "-" <> Text.pack (show index)
    , let description = "namespace-" <> Text.pack (show sampleIndex)
    ]

productionValues :: [ResponseTool] -> [Aeson.Value]
productionValues tools =
    case (requestParams OpenAIProvider "gpt-5.6-sol" "instructions" tools "high").input of
        Just (ResponseInputItems (AdditionalToolsItemValue item : _)) -> item.tools
        _ -> []

legacyValues :: [ResponseTool] -> [Aeson.Value]
legacyValues tools =
    case groupedValues of
        [] -> ungroupedValues
        _ ->
            let namespaceValue = Aeson.object
                    [ "type" Aeson..= ("namespace" :: Text)
                    , "name" Aeson..= ("functions" :: Text)
                    , "description" Aeson..= namespaceDescription
                    , "tools" Aeson..= groupedValues
                    ]
                insertionIndex = fromMaybe 0 firstGroupedPosition
            in take insertionIndex ungroupedValues
                <> [namespaceValue]
                <> drop insertionIndex ungroupedValues
  where
    (firstGroupedPosition, groupedValues, namespaceDescription, ungroupedValues) =
        foldl collect (Nothing, [], "", []) tools

    collect (firstPosition, grouped, description, ungrouped) tool =
        case legacyGroupedToolValues tool of
            Just (values, nextDescription) ->
                ( firstPosition <|> Just (length ungrouped)
                , grouped <> values
                , fromMaybe description nextDescription
                , ungrouped
                )
            Nothing ->
                (firstPosition, grouped, description, ungrouped <> [Aeson.toJSON tool])

legacyGroupedToolValues :: ResponseTool -> Maybe ([Aeson.Value], Maybe Text)
legacyGroupedToolValues tool = case tool of
    FunctionToolValue{} -> Just ([Aeson.toJSON tool], Nothing)
    KnownResponseTool ToolCustom _ -> Just ([Aeson.toJSON tool], Nothing)
    UnknownResponseTool tagged
        | tagged.tag == "custom" -> Just ([Aeson.toJSON tool], Nothing)
    KnownResponseTool ToolNamespace tagged
        | textField "name" tagged.fields == Just "functions" ->
            Just
                ( arrayField "tools" tagged.fields
                , nonBlank =<< textField "description" tagged.fields
                )
    _ -> Nothing

functionTool :: Text -> ResponseTool
functionTool toolName = FunctionToolValue FunctionTool
    { name = toolName
    , description = Nothing
    , parameters = Nothing
    , strict = Just True
    , extraFields = KeyMap.empty
    }

customTool :: Text -> ResponseTool
customTool toolName = KnownResponseTool ToolCustom TaggedObject
    { tag = "custom"
    , fields = KeyMap.singleton (Key.fromText "name") (Aeson.String toolName)
    }

namespaceTool :: Text -> Maybe Text -> [ResponseTool] -> ResponseTool
namespaceTool namespaceName namespaceDescription nestedTools =
    KnownResponseTool ToolNamespace TaggedObject
        { tag = "namespace"
        , fields = KeyMap.fromList $
            [ (Key.fromText "name", Aeson.String namespaceName)
            , (Key.fromText "tools", Aeson.toJSON nestedTools)
            ]
            <> case namespaceDescription of
                Just description ->
                    [(Key.fromText "description", Aeson.String description)]
                Nothing -> []
        }

textField :: Text -> Aeson.Object -> Maybe Text
textField name object =
    case KeyMap.lookup (Key.fromText name) object of
        Just (Aeson.String value) -> Just value
        _ -> Nothing

arrayField :: Text -> Aeson.Object -> [Aeson.Value]
arrayField name object =
    case KeyMap.lookup (Key.fromText name) object of
        Just (Aeson.Array values) -> Vector.toList values
        _ -> []

nonBlank :: Text -> Maybe Text
nonBlank value
    | Text.null (Text.strip value) = Nothing
    | otherwise = Just value

measure :: IO Int -> IO Sample
measure action = do
    performGC
    beforeStats <- getRTSStats
    beforeCpu <- getCPUTime
    beforeWall <- getMonotonicTimeNSec
    checksum <- action >>= evaluate
    _ <- evaluate checksum
    afterWall <- getMonotonicTimeNSec
    afterCpu <- getCPUTime
    performGC
    afterStats <- getRTSStats
    pure Sample
        { elapsedMillis =
            fromIntegral (afterWall - beforeWall) / 1.0e6
        , cpuMillis =
            fromIntegral (afterCpu - beforeCpu) / 1.0e9
        , allocatedBytes =
            fromIntegral
                (afterStats.allocated_bytes - beforeStats.allocated_bytes)
        }

encodedChecksum :: [Aeson.Value] -> Int
encodedChecksum values =
    BS.foldl'
        (\checksum byte -> checksum * 33 + fromIntegral byte)
        5381
        (LBS.toStrict (Aeson.encode values))

median :: [Sample] -> Sample
median samples =
    Sample
        { elapsedMillis = middle (map (.elapsedMillis) samples)
        , cpuMillis = middle (map (.cpuMillis) samples)
        , allocatedBytes = middle (map (.allocatedBytes) samples)
        }
  where
    middle values = sort values !! (length values `div` 2)
