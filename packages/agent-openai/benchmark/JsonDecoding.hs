{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Agent.OpenAI.CompactClient (decodeCompactBodyBytes)
import Agent.OpenAI.Http
    ( decodeCodexHttpBody
    , decodeCodexHttpBodyBytes
    )
import Agent.Responses.Types
    ( MessageContent(..)
    , Response(..)
    , ResponseContentPart(..)
    , ResponseItem(..)
    , ResponseMessage(..)
    )
import Control.Exception (evaluate)
import Control.Monad (forM)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List (sort)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import System.Exit (die)
import System.Mem (performGC)
import Text.Printf (printf)

data Workload
    = HttpUtf8RoundTrip
    | HttpDirectBytes
    | HttpSseUtf8RoundTrip
    | HttpSseDirectBytes
    | CompactUtf8RoundTrip
    | CompactDirectBytes

data Sample = Sample
    { wallMillis :: !Double
    , cpuMillis :: !Double
    , allocatedBytes :: !Integer
    }

main :: IO ()
main = do
    enabled <- getRTSStatsEnabled
    if enabled then pure () else die "run with +RTS -T"
    getArgs >>= \case
        [workloadArg, responseCountArg, textBytesArg, samplesArg] -> do
            workload <- parseWorkload workloadArg
            responseCount <- positive "response count" responseCountArg
            textBytes <- positive "text bytes" textBytesArg
            sampleCount <- positive "sample count" samplesArg
            let payloadSets =
                    [ [ makePayload workload
                            (sampleIndex * responseCount + responseIndex)
                            textBytes
                      | responseIndex <- [1 .. responseCount]
                      ]
                    | sampleIndex <- [1 .. sampleCount]
                    ]
            _ <- evaluate (sum (map (sum . map BS.length) payloadSets))
            case payloadSets of
                firstPayloads : _ -> verifyEquivalent workload firstPayloads
                [] -> die "sample count must be positive"
            samples <- forM payloadSets \payloads ->
                measure (runWorkload workload payloads)
            let medianWall = median (map (.wallMillis) samples)
                medianCPU = median (map (.cpuMillis) samples)
                medianAllocation = median (map (.allocatedBytes) samples)
            printf
                "%s,%d,%d,%d,%.3f,%.3f,%d\n"
                workloadArg responseCount textBytes sampleCount
                medianWall medianCPU medianAllocation
        _ -> die $
            "usage: openai-json-decoding-bench WORKLOAD RESPONSES TEXT_BYTES SAMPLES\n"
                <> "workloads: http-utf8-round-trip, http-direct-bytes, "
                <> "http-sse-utf8-round-trip, http-sse-direct-bytes, "
                <> "compact-utf8-round-trip, compact-direct-bytes"

parseWorkload :: String -> IO Workload
parseWorkload = \case
    "http-utf8-round-trip" -> pure HttpUtf8RoundTrip
    "http-direct-bytes" -> pure HttpDirectBytes
    "http-sse-utf8-round-trip" -> pure HttpSseUtf8RoundTrip
    "http-sse-direct-bytes" -> pure HttpSseDirectBytes
    "compact-utf8-round-trip" -> pure CompactUtf8RoundTrip
    "compact-direct-bytes" -> pure CompactDirectBytes
    other -> die ("unknown workload: " <> other)

positive :: String -> String -> IO Int
positive label raw = case reads raw of
    [(value, "")] | value > 0 -> pure value
    _ -> die ("invalid " <> label <> ": " <> raw)

median :: Ord a => [a] -> a
median values = sort values !! (length values `div` 2)

verifyEquivalent :: Workload -> [BS.ByteString] -> IO ()
verifyEquivalent workload payloads = do
    let oldChecksum = runWorkload (oldVariant workload) payloads
        directChecksum = runWorkload (directVariant workload) payloads
    oldResult <- oldChecksum
    directResult <- directChecksum
    if oldResult == directResult
        then pure ()
        else die "old and direct decoders produced different checksums"

oldVariant :: Workload -> Workload
oldVariant = \case
    HttpUtf8RoundTrip -> HttpUtf8RoundTrip
    HttpDirectBytes -> HttpUtf8RoundTrip
    HttpSseUtf8RoundTrip -> HttpSseUtf8RoundTrip
    HttpSseDirectBytes -> HttpSseUtf8RoundTrip
    CompactUtf8RoundTrip -> CompactUtf8RoundTrip
    CompactDirectBytes -> CompactUtf8RoundTrip

directVariant :: Workload -> Workload
directVariant = \case
    HttpUtf8RoundTrip -> HttpDirectBytes
    HttpDirectBytes -> HttpDirectBytes
    HttpSseUtf8RoundTrip -> HttpSseDirectBytes
    HttpSseDirectBytes -> HttpSseDirectBytes
    CompactUtf8RoundTrip -> CompactDirectBytes
    CompactDirectBytes -> CompactDirectBytes

runWorkload :: Workload -> [BS.ByteString] -> IO Int
runWorkload workload = go checksumSeed
  where
    go !checksum [] = pure checksum
    go !checksum (payload : rest) = do
        decoded <- evaluate $ case workload of
            HttpUtf8RoundTrip ->
                responseChecksum <$>
                    decodeCodexHttpBody (Text.decodeUtf8 payload)
            HttpDirectBytes ->
                responseChecksum <$> decodeCodexHttpBodyBytes payload
            HttpSseUtf8RoundTrip ->
                responseChecksum <$>
                    decodeCodexHttpBody (Text.decodeUtf8 payload)
            HttpSseDirectBytes ->
                responseChecksum <$> decodeCodexHttpBodyBytes payload
            CompactUtf8RoundTrip ->
                itemsChecksum <$>
                    decodeCompactBodyBytes
                        (Text.encodeUtf8 (Text.decodeUtf8 payload))
            CompactDirectBytes ->
                itemsChecksum <$> decodeCompactBodyBytes payload
        value <- case decoded of
            Left err -> error (show err)
            Right result -> evaluate result
        let checksum' = checksum * 33 + value
        checksum' `seq` go checksum' rest

responseChecksum :: Response -> Int
responseChecksum response =
    Text.length response.responseId + itemsChecksum response.output

itemsChecksum :: [ResponseItem] -> Int
itemsChecksum = sum . map itemChecksum

itemChecksum :: ResponseItem -> Int
itemChecksum = \case
    MessageItem message ->
        maybe 0 Text.length message.messageId
            + contentChecksum message.content
    _ -> 0

contentChecksum :: MessageContent -> Int
contentChecksum = \case
    MessageContentText value -> Text.length value
    MessageContentParts parts -> sum (map partChecksum parts)

partChecksum :: ResponseContentPart -> Int
partChecksum = \case
    OutputTextPart { text } -> Text.length text
    _ -> 0

makePayload :: Workload -> Int -> Int -> BS.ByteString
makePayload workload responseIndex textBytes =
    case workload of
        HttpUtf8RoundTrip -> encode response
        HttpDirectBytes -> encode response
        HttpSseUtf8RoundTrip -> sseFrame
        HttpSseDirectBytes -> sseFrame
        CompactUtf8RoundTrip -> encode compact
        CompactDirectBytes -> encode compact
  where
    encode = LBS.toStrict . Aeson.encode
    identifier = Text.pack (show responseIndex)
    message = Aeson.object
        [ "type" Aeson..= ("message" :: Text.Text)
        , "id" Aeson..= ("msg-" <> identifier)
        , "role" Aeson..= ("assistant" :: Text.Text)
        , "status" Aeson..= ("completed" :: Text.Text)
        , "content" Aeson..=
            [ Aeson.object
                [ "type" Aeson..= ("output_text" :: Text.Text)
                , "text" Aeson..= Text.replicate textBytes "x"
                , "annotations" Aeson..= ([] :: [Aeson.Value])
                ]
            ]
        ]
    response = Aeson.object
        [ "id" Aeson..= ("resp-" <> identifier)
        , "created_at" Aeson..= (1_700_000_000 :: Int)
        , "error" Aeson..= Aeson.Null
        , "incomplete_details" Aeson..= Aeson.Null
        , "model" Aeson..= ("gpt-5.6" :: Text.Text)
        , "object" Aeson..= ("response" :: Text.Text)
        , "status" Aeson..= ("completed" :: Text.Text)
        , "output" Aeson..= [message]
        ]
    compact = Aeson.object ["output" Aeson..= [message]]
    sseResponse = Aeson.object
        [ "type" Aeson..= ("response.completed" :: Text.Text)
        , "response" Aeson..= response
        ]
    sseFrame = BS.concat
        [ "event: response.completed\n"
        , "data: "
        , encode sseResponse
        , "\n\n"
        ]

checksumSeed :: Int
checksumSeed = 5381

measure :: IO Int -> IO Sample
measure action = do
    performGC
    beforeStats <- getRTSStats
    beforeCPU <- getCPUTime
    beforeWall <- getMonotonicTimeNSec
    result <- action
    _ <- evaluate result
    afterWall <- getMonotonicTimeNSec
    afterCPU <- getCPUTime
    afterStats <- getRTSStats
    pure Sample
        { wallMillis = fromIntegral (afterWall - beforeWall) / 1.0e6
        , cpuMillis = fromIntegral (afterCPU - beforeCPU) / 1.0e9
        , allocatedBytes =
            fromIntegral
                (afterStats.allocated_bytes - beforeStats.allocated_bytes)
        }
