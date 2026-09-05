module Main (main) where

import Agent.Responses.Types
import Agent.Json (rawJsonBytes, rawJsonFromEncoding)
-- 'evaluate' is the primitive for forcing the benchmark result;
-- safe-exceptions does not re-export it.
import Control.Exception (evaluate)
import Control.Exception.Safe (bracket, catchAny)
import Control.Monad (forM)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.ByteString as ByteString
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats
    ( GCDetails(..)
    , RTSStats(..)
    , getRTSStats
    , getRTSStatsEnabled
    )
import System.CPUTime (getCPUTime)
import System.Directory (getTemporaryDirectory, removeFile)
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath ((</>))
import System.Mem (performGC)
import Text.Printf (printf)

data Workload
    = Resident
    | Cold
    deriving (Eq, Show)

data Sample = Sample
    { elapsedMillis :: !Double
    , cpuMillis :: !Double
    , allocatedBytes :: !Integer
    , liveBytes :: !Integer
    , memoryInUseBytes :: !Integer
    }

data RetainedState
    = ResidentTranscript ![ResponseItem]
    | ColdTranscript !Int

main :: IO ()
main = do
    enabled <- getRTSStatsEnabled
    if enabled
        then pure ()
        else die "RTS statistics are disabled; run with +RTS -T"
    getArgs >>= \case
        [workloadArg, turnCountArg, payloadBytesArg, sampleCountArg] -> do
            workload <- parseWorkload workloadArg
            turnCount <- parsePositive "turn count" turnCountArg
            payloadBytes <- parsePositive "payload bytes per turn" payloadBytesArg
            sampleCount <- parsePositive "sample count" sampleCountArg
            samples <- forM [1 .. sampleCount] \sampleIndex ->
                measure workload turnCount payloadBytes sampleIndex
            let sample = median samples
            printf
                "%s,%d,%d,%.3f,%.3f,%d,%d,%d\n"
                workloadArg
                turnCount
                payloadBytes
                sample.elapsedMillis
                sample.cpuMillis
                sample.allocatedBytes
                sample.liveBytes
                sample.memoryInUseBytes
        _ ->
            die $
                "usage: conversation-retention-bench "
                    <> "WORKLOAD TURNS PAYLOAD_BYTES SAMPLES\n"
                    <> "workloads: resident, cold\n"
                    <> "output: workload,turns,payload_bytes,elapsed_ms,cpu_ms,"
                    <> "allocated_bytes,live_bytes,memory_in_use_bytes"

parseWorkload :: String -> IO Workload
parseWorkload = \case
    "resident" -> pure Resident
    "cold" -> pure Cold
    other -> die ("unknown workload: " <> other)

parsePositive :: String -> String -> IO Int
parsePositive label raw =
    case reads raw of
        [(value, "")]
            | value > 0 -> pure value
        _ -> die ("invalid " <> label <> ": " <> raw)

measure :: Workload -> Int -> Int -> Int -> IO Sample
measure workload turnCount payloadBytes sampleIndex =
    withColdPath sampleIndex \coldPath -> do
        performGC
        beforeStats <- getRTSStats
        beforeCpu <- getCPUTime
        beforeElapsed <- getMonotonicTimeNSec
        retained <- case workload of
            Resident ->
                pure $
                    ResidentTranscript
                        (buildTranscript sampleIndex turnCount payloadBytes)
            Cold ->
                ColdTranscript
                    <$> persistColdTranscript
                        coldPath
                        sampleIndex
                        turnCount
                        payloadBytes
        initialChecksum <- evaluate (checksumRetained retained)
        performGC
        afterStats <- getRTSStats
        finalChecksum <- evaluate (checksumRetained retained)
        afterElapsed <- getMonotonicTimeNSec
        afterCpu <- getCPUTime
        _ <- evaluate (initialChecksum + finalChecksum)
        pure Sample
            { elapsedMillis =
                fromIntegral (afterElapsed - beforeElapsed) / 1.0e6
            , cpuMillis =
                fromIntegral (afterCpu - beforeCpu) / 1.0e9
            , allocatedBytes =
                fromIntegral
                    (afterStats.allocated_bytes - beforeStats.allocated_bytes)
            , liveBytes =
                fromIntegral afterStats.gc.gcdetails_live_bytes
            , memoryInUseBytes =
                fromIntegral afterStats.gc.gcdetails_mem_in_use_bytes
            }

withColdPath :: Int -> (FilePath -> IO a) -> IO a
withColdPath sampleIndex use = do
    temporaryDirectory <- getTemporaryDirectory
    let path =
            temporaryDirectory
                </> ("agent-conversation-retention-"
                    <> show sampleIndex
                    <> ".json")
    bracket
        (pure path)
        (\file -> removeFile file `catchAny` const (pure ()))
        use

persistColdTranscript :: FilePath -> Int -> Int -> Int -> IO Int
persistColdTranscript path sampleIndex turnCount payloadBytes = do
    let transcript = buildTranscript sampleIndex turnCount payloadBytes
    LazyByteString.writeFile path (Aeson.encode transcript)
    fromIntegral <$> LazyByteString.length <$> LazyByteString.readFile path

checksumRetained :: RetainedState -> Int
checksumRetained = \case
    ResidentTranscript transcript -> checksumTranscript transcript
    ColdTranscript bytes -> bytes

buildTranscript :: Int -> Int -> Int -> [ResponseItem]
buildTranscript sampleIndex turnCount payloadBytes =
    concat
        [ buildTurn sampleIndex turnIndex payloadBytes
        | turnIndex <- [1 .. turnCount]
        ]

buildTurn :: Int -> Int -> Int -> [ResponseItem]
buildTurn sampleIndex turnIndex payloadBytes =
    [ MessageItem ResponseMessage
        { messageId = Just (itemId "message")
        , content = MessageContentText (payload "assistant" assistantBytes)
        , role = RoleAssistant
        , status = Nothing
        , phase = Just "final_answer"
        , passthrough = Nothing
        }
    , ReasoningItemValue ReasoningItem
        { itemId = Just (itemId "reasoning")
        , summary =
            [ ReasoningSummaryPart
                { partType = "summary_text"
                , text = Just "representative retained reasoning"
                }
            ]
        , content = Nothing
        , encryptedContent = Just (payload "reasoning" reasoningBytes)
        , status = Nothing
        }
    , FunctionCallItem FunctionCall
        { itemId = Just (itemId "call")
        , callId = callId
        , name = "shell_command"
        , namespace = Nothing
        , provider = Nothing
        , arguments = "{\"command\":\"representative tool call\"}"
        , encryptedFunctionArgs = Nothing
        , status = Nothing
        , async = Nothing
        }
    , FunctionCallOutputItem FunctionCallOutput
        { itemId = Just (itemId "output")
        , callId = callId
        , name = Just "shell_command"
        , namespace = Nothing
        , provider = Nothing
        , output = rawJsonFromEncoding $
            Aeson.toEncoding (Aeson.String
                (payload "tool-output" toolOutputBytes))
        , status = Nothing
        , async = Nothing
        }
    ]
  where
    assistantBytes = payloadBytes `div` 8
    reasoningBytes = payloadBytes `div` 8
    toolOutputBytes = payloadBytes - assistantBytes - reasoningBytes
    suffix = Text.pack (show sampleIndex <> "-" <> show turnIndex)
    itemId prefix = prefix <> "-" <> suffix
    callId = "call-" <> suffix
    payload prefix size =
        prefix <> ":" <> suffix <> ":" <> Text.replicate size "x"

checksumTranscript :: [ResponseItem] -> Int
checksumTranscript = foldl' checksumItem 5381

checksumItem :: Int -> ResponseItem -> Int
checksumItem checksum = \case
    MessageItem message ->
        checksumText checksum (messageText message.content)
    ReasoningItemValue reasoning ->
        maybe checksum (checksumText checksum) reasoning.encryptedContent
    FunctionCallItem call ->
        checksumText checksum call.arguments
    FunctionCallOutputItem output ->
        ByteString.foldl'
            (\value byte -> value * 33 + fromIntegral byte)
            checksum
            (rawJsonBytes output.output)
    _ -> checksum + 1

messageText :: MessageContent -> Text
messageText = \case
    MessageContentText text -> text
    MessageContentParts parts ->
        Text.concat
            [ text
            | InputTextPart { text } <- parts
            ]

checksumText :: Int -> Text -> Int
checksumText =
    Text.foldl' \checksum character ->
        checksum * 33 + fromEnum character

median :: [Sample] -> Sample
median samples =
    Sample
        { elapsedMillis = middle (sort (map (.elapsedMillis) samples))
        , cpuMillis = middle (sort (map (.cpuMillis) samples))
        , allocatedBytes = middle (sort (map (.allocatedBytes) samples))
        , liveBytes = middle (sort (map (.liveBytes) samples))
        , memoryInUseBytes =
            middle (sort (map (.memoryInUseBytes) samples))
        }
  where
    middle values = values !! (length values `div` 2)
