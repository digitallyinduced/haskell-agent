{-# LANGUAGE BangPatterns #-}

module Main (main) where

import qualified Agent.Json.Decoder as Decoder
import qualified Agent.Json.Decoder.Hermes as Hermes
import qualified Agent.Json.Encoder as Encoder
import Control.Exception (evaluate)
import Control.Monad (forM)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as Lazy
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Jsonifier
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import System.Exit (die)
import System.Mem (performGC)
import Text.Printf (printf)

data Workload
    = PortableDecode
    | HermesDecode
    | AesonDecode
    | DirectEncode
    | JsonifierEncode
    | AesonEncode

data BenchRecord = BenchRecord
    { recordId :: !Int
    , recordName :: !Text
    , recordActive :: !Bool
    , recordBody :: !Text
    }
    deriving stock (Eq, Show)

data RecordState = RecordState
    { stateId :: !(Maybe Int)
    , stateName :: !(Maybe Text)
    , stateActive :: !(Maybe Bool)
    , stateBody :: !(Maybe Text)
    }

data Sample = Sample
    { wallMillis :: !Double
    , cpuMillis :: !Double
    , allocatedBytes :: !Integer
    }

instance Aeson.FromJSON BenchRecord where
    parseJSON = Aeson.withObject "BenchRecord" \objectValue ->
        BenchRecord
            <$> objectValue Aeson..: "id"
            <*> objectValue Aeson..: "name"
            <*> objectValue Aeson..: "active"
            <*> objectValue Aeson..: "body"

instance Aeson.ToJSON BenchRecord where
    toJSON record =
        Aeson.object
            [ "id" Aeson..= record.recordId
            , "name" Aeson..= record.recordName
            , "active" Aeson..= record.recordActive
            , "body" Aeson..= record.recordBody
            ]
    toEncoding record =
        Aeson.pairs
            ( "id" Aeson..= record.recordId
                <> "name" Aeson..= record.recordName
                <> "active" Aeson..= record.recordActive
                <> "body" Aeson..= record.recordBody
            )

recordDecoder :: Decoder.Decoder BenchRecord
recordDecoder =
    Decoder.object
        (RecordState Nothing Nothing Nothing Nothing)
        [ Decoder.field "id" Decoder.int \value state ->
            Right state { stateId = Just value }
        , Decoder.field "name" Decoder.text \value state ->
            Right state { stateName = Just value }
        , Decoder.field "active" Decoder.bool \value state ->
            Right state { stateActive = Just value }
        , Decoder.field "body" Decoder.text \value state ->
            Right state { stateBody = Just value }
        ]
        (Decoder.unknownField Decoder.skip
            \_ () state -> Right state)
        \state ->
            BenchRecord
                <$> required "id" state.stateId
                <*> required "name" state.stateName
                <*> required "active" state.stateActive
                <*> required "body" state.stateBody
  where
    required label =
        maybe (Left ("missing " <> label)) Right

recordEncoder :: Encoder.Encoder BenchRecord
recordEncoder = Encoder.object
    [ Encoder.field "id" Encoder.int (.recordId)
    , Encoder.field "name" Encoder.text (.recordName)
    , Encoder.field "active" Encoder.bool (.recordActive)
    , Encoder.field "body" Encoder.text (.recordBody)
    ]

main :: IO ()
main = do
    statsEnabled <- getRTSStatsEnabled
    if statsEnabled then pure () else die "run with +RTS -T"
    getArgs >>= \case
        [workloadArg, recordCountArg, bodyBytesArg, sampleCountArg] -> do
            workload <- parseWorkload workloadArg
            recordCount <- positive "record count" recordCountArg
            bodyBytes <- positive "body bytes" bodyBytesArg
            sampleCount <- positive "sample count" sampleCountArg
            let records = map (makeRecord bodyBytes) [1 .. recordCount]
                payloads = map encodeFixture records
            _ <- evaluate $
                sum (map BS.length payloads)
                    + sum (map (.recordId) records)
            validateImplementations records payloads
            samples <- Hermes.withDecoderSession \session ->
                forM [1 .. sampleCount] \_ ->
                    measure (runWorkload workload session records payloads)
            let middle =
                    sortOn (.wallMillis) samples
                        !! (length samples `div` 2)
            printf
                "%s,%d,%d,%d,%.3f,%.3f,%d\n"
                workloadArg
                recordCount
                bodyBytes
                sampleCount
                middle.wallMillis
                middle.cpuMillis
                middle.allocatedBytes
        _ -> die $
            "usage: direct-json-bench WORKLOAD RECORDS BODY_BYTES SAMPLES\n"
                <> "workloads: portable-decode, hermes-decode, aeson-decode, "
                <> "direct-encode, jsonifier-encode, aeson-encode"

parseWorkload :: String -> IO Workload
parseWorkload = \case
    "portable-decode" -> pure PortableDecode
    "hermes-decode" -> pure HermesDecode
    "aeson-decode" -> pure AesonDecode
    "direct-encode" -> pure DirectEncode
    "jsonifier-encode" -> pure JsonifierEncode
    "aeson-encode" -> pure AesonEncode
    other -> die ("unknown workload: " <> other)

positive :: String -> String -> IO Int
positive label raw = case reads raw of
    [(value, "")] | value > 0 -> pure value
    _ -> die ("invalid " <> label <> ": " <> raw)

runWorkload
    :: Workload
    -> Hermes.DecoderSession
    -> [BenchRecord]
    -> [BS.ByteString]
    -> IO Int
runWorkload workload session records payloads =
    case workload of
        PortableDecode ->
            decodeLoop
                (\bytes -> pure (Decoder.decode recordDecoder bytes))
                payloads
        HermesDecode ->
            decodeLoop (Hermes.decodeIO session recordDecoder) payloads
        AesonDecode ->
            decodeLoop
                (\bytes -> pure (Aeson.eitherDecodeStrict' bytes))
                payloads
        DirectEncode ->
            encodeLoop (Encoder.encode recordEncoder) records
        JsonifierEncode ->
            encodeLoop encodeJsonifier records
        AesonEncode ->
            encodeLoop (Lazy.toStrict . Aeson.encode) records

decodeLoop
    :: (BS.ByteString -> IO (Either error BenchRecord))
    -> [BS.ByteString]
    -> IO Int
decodeLoop decodeOne =
    go checksumSeed
  where
    go !checksum [] = pure checksum
    go !checksum (payload : rest) = do
        decoded <- decodeOne payload
        record <- case decoded of
            Left _ -> error "benchmark decoder rejected a fixture"
            Right value -> evaluate value
        let checksum' = checksumRecord checksum record
        checksum' `seq` go checksum' rest

encodeLoop
    :: (BenchRecord -> BS.ByteString)
    -> [BenchRecord]
    -> IO Int
encodeLoop encodeOne =
    go checksumSeed
  where
    go !checksum [] = pure checksum
    go !checksum (record : rest) = do
        bytes <- evaluate (encodeOne record)
        let checksum' =
                checksum * 33
                    + BS.length bytes
                    + fromIntegral (BS.head bytes)
        checksum' `seq` go checksum' rest

validateImplementations
    :: [BenchRecord]
    -> [BS.ByteString]
    -> IO ()
validateImplementations records payloads =
    case (records, payloads) of
        (expected : _, payload : _) -> do
            let portable = Decoder.decode recordDecoder payload
                aeson = Aeson.eitherDecodeStrict' payload
            hermes <- Hermes.withDecoderSession \session ->
                Hermes.decodeIO session recordDecoder payload
            if portable == Right expected
                    && hermes == Right expected
                    && aeson == Right expected
                    && all (isValidEncoding expected)
                        [ Encoder.encode recordEncoder expected
                        , encodeJsonifier expected
                        , Lazy.toStrict (Aeson.encode expected)
                        ]
                then pure ()
                else die "benchmark implementations disagree"
        _ -> die "benchmark requires non-empty fixtures"
  where
    isValidEncoding expected bytes =
        Aeson.eitherDecodeStrict' bytes
            == Right expected

makeRecord :: Int -> Int -> BenchRecord
makeRecord bodyBytes identifier = BenchRecord
    { recordId = identifier
    , recordName = "record-" <> Text.pack (show identifier)
    , recordActive = even identifier
    , recordBody = Text.replicate bodyBytes "x"
    }

encodeFixture :: BenchRecord -> BS.ByteString
encodeFixture = Lazy.toStrict . Aeson.encode

encodeJsonifier :: BenchRecord -> BS.ByteString
encodeJsonifier record =
    Jsonifier.toByteString $
        Jsonifier.object
            [ ("id", Jsonifier.intNumber record.recordId)
            , ("name", Jsonifier.textString record.recordName)
            , ("active", Jsonifier.bool record.recordActive)
            , ("body", Jsonifier.textString record.recordBody)
            ]

checksumRecord :: Int -> BenchRecord -> Int
checksumRecord checksum record =
    Text.foldl'
        (\current character -> current * 33 + fromEnum character)
        (checksum + record.recordId + fromEnum record.recordActive)
        record.recordBody

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
