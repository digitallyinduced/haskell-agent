module Main (main) where

import Agent.Store.Postgres
    ( Store
    , closeStore
    , managedPostgresConfigFromEnv
    , openStore
    , trustedPool
    )
import Agent.Store.Postgres.Session
    ( SessionMetadata(..)
    , SessionReadImplementation(..)
    , SessionTurn(..)
    , StoredSession(..)
    , StoredTurn(..)
    , loadActiveSessionWithImplementation
    , loadSessionWithImplementation
    )
import Agent.Store.SessionItem
import Agent.Store.Types (StoreError, renderStoreError)
import Control.Exception.Safe (bracket)
import Control.Monad (forM)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats (RTSStats(..), getRTSStats, getRTSStatsEnabled)
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import System.Exit (die)
import System.Mem (performGC)
import Text.Printf (printf)

data Workload = Active | Full

data Sample = Sample
    { elapsedMillis :: !Double
    , cpuMillis :: !Double
    , allocatedBytes :: !Integer
    , checksum :: !Int
    }

main :: IO ()
main = do
    enabled <- getRTSStatsEnabled
    if enabled
        then pure ()
        else die "RTS statistics are disabled; run with +RTS -T"
    getArgs >>= \case
        [implementationArg, stateDirectory, workloadArg, sessionKeyArg, sampleCountArg] -> do
            implementation <- case implementationArg of
                "per-item" -> pure PerItemSessionRead
                "adaptive" -> pure AdaptiveSessionRead
                value -> die ("unknown implementation: " <> value)
            workload <- case workloadArg of
                "active" -> pure Active
                "full" -> pure Full
                value -> die ("unknown workload: " <> value)
            sampleCount <- parsePositive sampleCountArg
            config <- managedPostgresConfigFromEnv stateDirectory
            bracket
                (openStore config >>= requireStore "open store")
                closeStore
                \store -> do
                    let action =
                            loadWorkload
                                implementation
                                workload
                                store
                                (Text.pack sessionKeyArg)
                    _ <- measure action
                    samples <- forM [1 .. sampleCount] \_ ->
                        measure action
                    let sample = median samples
                    printf
                        "%s,%s,%s,%.3f,%.3f,%d,%d\n"
                        implementationArg
                        workloadArg
                        sessionKeyArg
                        sample.elapsedMillis
                        sample.cpuMillis
                        sample.allocatedBytes
                        sample.checksum
        _ ->
            die $
                "usage: real-session-load-bench "
                    <> "(per-item|adaptive) STATE_DIRECTORY "
                    <> "(active|full) SESSION_KEY SAMPLES\n"
                    <> "output: implementation,workload,session_key,"
                    <> "elapsed_ms,cpu_ms,allocated_bytes,checksum"

parsePositive :: String -> IO Int
parsePositive value =
    case reads value of
        [(parsed, "")]
            | parsed > 0 -> pure parsed
        _ -> die ("invalid sample count: " <> value)

loadWorkload
    :: SessionReadImplementation
    -> Workload
    -> Store
    -> Text
    -> IO (Either StoreError StoredSession)
loadWorkload implementation workload store sessionKey =
    loader (trustedPool store) sessionKey >>= \case
        Left err -> pure (Left err)
        Right Nothing ->
            die ("session not found: " <> Text.unpack sessionKey)
        Right (Just session) -> pure (Right session)
  where
    loader = case workload of
        Active -> loadActiveSessionWithImplementation implementation
        Full -> loadSessionWithImplementation implementation

measure :: IO (Either StoreError StoredSession) -> IO Sample
measure action = do
    performGC
    beforeStats <- getRTSStats
    beforeCpu <- getCPUTime
    beforeElapsed <- getMonotonicTimeNSec
    stored <- action >>= requireStore "load session"
    forced <- pure $! checksumSession stored
    afterElapsed <- getMonotonicTimeNSec
    afterCpu <- getCPUTime
    performGC
    afterStats <- getRTSStats
    pure Sample
        { elapsedMillis =
            fromIntegral (afterElapsed - beforeElapsed) / 1.0e6
        , cpuMillis =
            fromIntegral (afterCpu - beforeCpu) / 1.0e9
        , allocatedBytes =
            fromIntegral
                (afterStats.allocated_bytes - beforeStats.allocated_bytes)
        , checksum = forced
        }

requireStore :: String -> Either StoreError a -> IO a
requireStore label =
    either
        (die . ((label <> ": ") <>) . Text.unpack . renderStoreError)
        pure

checksumSession :: StoredSession -> Int
checksumSession stored =
    Vector.foldl'
        checksumTurn
        (Text.length stored.storedMetadata.sessionMetadataTitle)
        stored.storedTurns

checksumTurn :: Int -> StoredTurn -> Int
checksumTurn total stored =
    let turn = stored.storedTurn
    in foldl'
        checksumItem
        ( total
            + fromIntegral stored.storedTurnIndex
            + Text.length turn.sessionTurnUserText
            + maybe 0 Text.length turn.sessionTurnAssistantText
            + maybe 0 Text.length turn.sessionTurnError
            + maybe 0 Text.length turn.sessionTurnResponseId
        )
        turn.sessionTurnItems

checksumItem :: Int -> StoredResponseItem -> Int
checksumItem total = \case
    StoredMessageItem message ->
        total
            + maybeText message.storedMessageProviderItemId
            + Text.length message.storedMessageRole
            + maybeText message.storedMessageStatus
            + maybeText message.storedMessagePhase
            + opaqueObject message.storedMessageExtraFields
            + case message.storedMessageContent of
                StoredMessageText text -> Text.length text
                StoredMessageParts parts -> sum (map checksumPart parts)
    StoredFunctionCallItem call ->
        total
            + maybeText call.storedFunctionCallProviderItemId
            + Text.length call.storedFunctionCallCallId
            + Text.length call.storedFunctionCallName
            + Text.length call.storedFunctionCallArguments
            + maybeText call.storedFunctionCallStatus
            + opaqueObject call.storedFunctionCallExtraFields
    StoredFunctionCallOutputItem output ->
        total
            + maybeText output.storedFunctionCallOutputProviderItemId
            + Text.length output.storedFunctionCallOutputCallId
            + Text.length
                output.storedFunctionCallOutputValue.storedToolOutputText
            + maybeText output.storedFunctionCallOutputStatus
            + opaqueObject output.storedFunctionCallOutputExtraFields
    StoredCustomToolCallItem call ->
        total
            + maybeText call.storedCustomToolCallProviderItemId
            + Text.length call.storedCustomToolCallCallId
            + Text.length call.storedCustomToolCallName
            + Text.length call.storedCustomToolCallInput
            + maybeText call.storedCustomToolCallStatus
            + opaqueObject call.storedCustomToolCallExtraFields
    StoredCustomToolCallOutputItem output ->
        total
            + maybeText output.storedCustomToolCallOutputProviderItemId
            + Text.length output.storedCustomToolCallOutputCallId
            + maybeText output.storedCustomToolCallOutputName
            + Text.length
                output.storedCustomToolCallOutputValue.storedToolOutputText
            + maybeText output.storedCustomToolCallOutputStatus
            + opaqueObject output.storedCustomToolCallOutputExtraFields
    StoredReasoningItem reasoning ->
        total
            + maybeText reasoning.storedReasoningProviderItemId
            + sum (map checksumSummary reasoning.storedReasoningSummary)
            + maybe 0 (sum . map checksumPart)
                reasoning.storedReasoningContent
            + maybeText reasoning.storedReasoningEncryptedContent
            + maybeText reasoning.storedReasoningStatus
            + opaqueObject reasoning.storedReasoningExtraFields
    StoredItemReferenceItem reference ->
        total
            + Text.length reference.storedItemReferenceProviderItemId
            + opaqueObject reference.storedItemReferenceExtraFields
    StoredTaggedResponseItem tagged ->
        total
            + Text.length tagged.storedTaggedItemWireTag
            + opaqueObject tagged.storedTaggedItemFields

checksumSummary :: StoredReasoningSummaryPart -> Int
checksumSummary summary =
    Text.length summary.storedReasoningSummaryPartType
        + maybeText summary.storedReasoningSummaryPartText
        + opaqueObject summary.storedReasoningSummaryPartExtraFields

checksumPart :: StoredContentPart -> Int
checksumPart part =
    Text.length part.storedContentPartType
        + sum
            [ maybeText part.storedContentPartText
            , maybeText part.storedContentPartRefusal
            , maybeText part.storedContentPartDetail
            , maybeText part.storedContentPartFileData
            , maybeText part.storedContentPartFileId
            , maybeText part.storedContentPartFileUrl
            , maybeText part.storedContentPartFilename
            , maybeText part.storedContentPartImageUrl
            , maybeOpaque part.storedContentPartInputAudio
            , maybeOpaque part.storedContentPartPromptCacheBreakpoint
            , maybeOpaque part.storedContentPartAnnotations
            , maybeOpaque part.storedContentPartLogprobs
            ]
        + opaqueObject part.storedContentPartExtraFields

maybeText :: Maybe Text -> Int
maybeText = maybe 0 Text.length

maybeOpaque :: Maybe StoredOpaqueValue -> Int
maybeOpaque = maybe 0 (Text.length . (.storedOpaqueValueText))

opaqueObject :: StoredOpaqueObject -> Int
opaqueObject = Text.length . (.storedOpaqueObjectText)

median :: [Sample] -> Sample
median samples =
    Sample
        { elapsedMillis = middle (sort (map (.elapsedMillis) samples))
        , cpuMillis = middle (sort (map (.cpuMillis) samples))
        , allocatedBytes = middle (sort (map (.allocatedBytes) samples))
        , checksum = middle (sort (map (.checksum) samples))
        }
  where
    middle values = values !! (length values `div` 2)
