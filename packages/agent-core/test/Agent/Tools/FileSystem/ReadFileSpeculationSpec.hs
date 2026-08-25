module Agent.Tools.FileSystem.ReadFileSpeculationSpec (spec) where

import Agent.Loop (defaultLoopDispatch)
import Agent.Responses.Types
import Agent.ToolDispatch
    ( ToolCallResult(..)
    , functionToolCall
    )
import Agent.Tools.FileSystem.ReadFile (readFileToolWithSpeculation)
import Agent.Tools.FileSystem.ReadFileSpeculation
import Agent.Tools.Types
    ( ToolEnv
    , defaultToolEnv
    , dispatchRegisteredToolCall
    , mkToolRegistry
    )
import Control.Exception.Safe (bracket)
import Control.Monad (forM_)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import System.Directory
    ( createDirectoryIfMissing
    , getTemporaryDirectory
    , removeDirectoryRecursive
    )
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.OsPath (unsafeEncodeUtf)
import System.Posix.Temp (mkdtemp)
import System.Process (readProcessWithExitCode)
import Test.Hspec

spec :: Spec
spec = describe "read_file speculation" do
    it "prefetches a complete target from argument deltas before done" do
        withSpeculation \dir env cache -> do
            Text.writeFile (dir </> "alpha.txt") "prefetched"
            let callId = "call-complete"
                arguments = readArguments "alpha.txt"
            observeReadFileStreamEvent cache $
                outputItemAdded (Just "item-complete") (Just 0) callId ""
            observeReadFileStreamEvent cache $
                argumentsDelta (Just "item-complete") (Just 0) arguments
            waitForReadFileSpeculation cache

            beforeDone <- readReadFileSpeculationMetrics cache
            beforeDone.speculativeCompletePredictions `shouldBe` 1

            retainFinalReadFileCalls cache
                [finalReadCall (Just "item-complete") callId arguments]
            dispatchRead env cache callId arguments
                `shouldReturn` "1→prefetched"
            metrics <- readReadFileSpeculationMetrics cache
            metrics.speculativeReadHits `shouldBe` 1
            metrics.speculativeReadMisses `shouldBe` 0

    it "predicts a unique workspace filename from a streamed prefix" do
        withPreparedSpeculation
            (\dir -> do
                createDirectoryIfMissing True (dir </> "src")
                Text.writeFile
                    (dir </> "src" </> "unique-module.hs")
                    "unique"
                Text.writeFile
                    (dir </> "src" </> "other-module.hs")
                    "other"
                initializeGitRepository dir)
            \_dir env cache -> do
                let callId = "call-prefix"
                    itemId = Just "item-prefix"
                    outputIndex = Just 1
                    arguments = readArguments "src/unique-module.hs"
                observeReadFileStreamEvent cache $
                    outputItemAdded itemId outputIndex callId ""
                -- Let the session-scoped workspace index finish before
                -- testing the incremental filename prediction itself.
                waitForReadFileSpeculation cache
                observeReadFileStreamEvent cache $
                    argumentsDelta
                        itemId
                        outputIndex
                        "{\"target_file\":\"src/uni"
                waitForReadFileSpeculation cache

                predicted <- readReadFileSpeculationMetrics cache
                predicted.speculativePrefixPredictions `shouldBe` 1

                retainFinalReadFileCalls cache
                    [finalReadCall itemId callId arguments]
                dispatchRead env cache callId arguments
                    `shouldReturn` "1→unique"
                metrics <- readReadFileSpeculationMetrics cache
                metrics.speculativeReadHits `shouldBe` 1

    it "ignores nested target_file fields in incomplete JSON" do
        withSpeculation \dir _ cache -> do
            Text.writeFile (dir </> "nested.txt") "must not be prefetched"
            let itemId = Just "item-nested"
                outputIndex = Just 2
            observeReadFileStreamEvent cache $
                outputItemAdded itemId outputIndex "call-nested" ""
            observeReadFileStreamEvent cache $
                argumentsDelta
                    itemId
                    outputIndex
                    "{\"metadata\":{\"target_file\":\"nested.txt\"},"
            waitForReadFileSpeculation cache

            metrics <- readReadFileSpeculationMetrics cache
            metrics.speculativeReadsStarted `shouldBe` 0

    it "correlates an arguments.done event by output_index without a name" do
        withSpeculation \dir env cache -> do
            Text.writeFile (dir </> "indexed.txt") "first\nindexed"
            let callId = "call-output-index"
                outputIndex = Just 3
                arguments =
                    "{\"target_file\":\"indexed.txt\",\"offset\":2,\"limit\":1}"
            observeReadFileStreamEvent cache $
                outputItemAdded Nothing outputIndex callId ""
            observeReadFileStreamEvent cache $
                argumentsDone Nothing outputIndex Nothing arguments
            waitForReadFileSpeculation cache
            retainFinalReadFileCalls cache
                [finalReadCall Nothing callId arguments]

            dispatchRead env cache callId arguments
                `shouldReturn` "2→indexed"
            metrics <- readReadFileSpeculationMetrics cache
            metrics.speculativeCompletePredictions `shouldBe` 1
            metrics.speculativeReadHits `shouldBe` 1

    it "correlates output-index-only deltas after an item-id-keyed add" do
        withSpeculation \dir env cache -> do
            Text.writeFile (dir </> "aliased.txt") "aliased"
            let callId = "call-alias"
                itemId = Just "item-alias"
                outputIndex = Just 4
                arguments = readArguments "aliased.txt"
            observeReadFileStreamEvent cache $
                outputItemAdded itemId outputIndex callId ""
            observeReadFileStreamEvent cache $
                argumentsDelta Nothing outputIndex arguments
            waitForReadFileSpeculation cache
            retainFinalReadFileCalls cache
                [finalReadCall itemId callId arguments]

            dispatchRead env cache callId arguments
                `shouldReturn` "1→aliased"
            metrics <- readReadFileSpeculationMetrics cache
            metrics.speculativeCompletePredictions `shouldBe` 1
            metrics.speculativeReadHits `shouldBe` 1

    it "reuses prefetched content when later arguments select a range" do
        withPreparedSpeculation
            (\dir -> do
                createDirectoryIfMissing True (dir </> "src")
                Text.writeFile
                    (dir </> "src" </> "range-target.txt")
                    "first\nsecond"
                initializeGitRepository dir)
            \_dir env cache -> do
                let callId = "call-range"
                    itemId = Just "item-range"
                    outputIndex = Just 0
                    arguments =
                        "{\"target_file\":\"src/range-target.txt\",\"offset\":2,\"limit\":1}"
                observeReadFileStreamEvent cache $
                    outputItemAdded itemId outputIndex callId ""
                waitForReadFileSpeculation cache
                observeReadFileStreamEvent cache $
                    argumentsDelta
                        itemId
                        outputIndex
                        "{\"target_file\":\"src/range"
                waitForReadFileSpeculation cache
                retainFinalReadFileCalls cache
                    [finalReadCall itemId callId arguments]

                dispatchRead env cache callId arguments
                    `shouldReturn` "2→second"
                metrics <- readReadFileSpeculationMetrics cache
                metrics.speculativeReadHits `shouldBe` 1
                metrics.speculativeReadMisses `shouldBe` 0

    it "does not restart a completed-path prefetch when range fields arrive" do
        withSpeculation \dir env cache -> do
            Text.writeFile (dir </> "range-after-path.txt") "first\nsecond"
            let callId = "call-range-after-path"
                itemId = Just "item-range-after-path"
                outputIndex = Just 0
                arguments =
                    "{\"target_file\":\"range-after-path.txt\",\"offset\":2,\"limit\":1}"
            observeReadFileStreamEvent cache $
                outputItemAdded itemId outputIndex callId ""
            observeReadFileStreamEvent cache $
                argumentsDelta
                    itemId
                    outputIndex
                    "{\"target_file\":\"range-after-path.txt\""
            waitForReadFileSpeculation cache
            observeReadFileStreamEvent cache $
                argumentsDelta
                    itemId
                    outputIndex
                    ",\"offset\":2,\"limit\":1}"
            waitForReadFileSpeculation cache

            beforeDispatch <- readReadFileSpeculationMetrics cache
            beforeDispatch.speculativeReadsStarted `shouldBe` 1
            beforeDispatch.speculativeReadsCancelled `shouldBe` 0

            retainFinalReadFileCalls cache
                [finalReadCall itemId callId arguments]
            dispatchRead env cache callId arguments
                `shouldReturn` "2→second"
            metrics <- readReadFileSpeculationMetrics cache
            metrics.speculativeReadHits `shouldBe` 1

    it "falls back when the finalized target differs from the prediction" do
        withSpeculation \dir env cache -> do
            Text.writeFile (dir </> "alpha.txt") "wrong"
            Text.writeFile (dir </> "beta.txt") "right"
            let callId = "call-mismatch"
                predictedArguments = readArguments "alpha.txt"
                finalArguments = readArguments "beta.txt"
            observeReadFileStreamEvent cache $
                outputItemAdded (Just "item-mismatch") (Just 0) callId ""
            observeReadFileStreamEvent cache $
                argumentsDelta
                    (Just "item-mismatch")
                    (Just 0)
                    predictedArguments
            waitForReadFileSpeculation cache
            retainFinalReadFileCalls cache
                [finalReadCall
                    (Just "item-mismatch")
                    callId
                    finalArguments]

            dispatchRead env cache callId finalArguments
                `shouldReturn` "1→right"
            metrics <- readReadFileSpeculationMetrics cache
            metrics.speculativeReadHits `shouldBe` 0
            metrics.speculativeReadMisses `shouldBe` 1

    it "discards a prefetched snapshot when the file changes" do
        withSpeculation \dir env cache -> do
            let path = dir </> "changing.txt"
                callId = "call-stale"
                arguments = readArguments "changing.txt"
            Text.writeFile path "old"
            observeReadFileStreamEvent cache $
                outputItemAdded (Just "item-stale") (Just 0) callId ""
            observeReadFileStreamEvent cache $
                argumentsDelta (Just "item-stale") (Just 0) arguments
            waitForReadFileSpeculation cache
            Text.writeFile path "new-content"
            retainFinalReadFileCalls cache
                [finalReadCall (Just "item-stale") callId arguments]

            dispatchRead env cache callId arguments
                `shouldReturn` "1→new-content"
            metrics <- readReadFileSpeculationMetrics cache
            metrics.speculativeReadHits `shouldBe` 0
            metrics.speculativeReadMisses `shouldBe` 1
            metrics.speculativeReadStale `shouldBe` 1

    it "cancels predictions abandoned by the finalized response" do
        withSpeculation \dir _ cache -> do
            Text.writeFile (dir </> "abandoned.txt") "unused"
            observeReadFileStreamEvent cache $
                outputItemAdded
                    (Just "item-abandoned")
                    (Just 0)
                    "call-abandoned"
                    ""
            observeReadFileStreamEvent cache $
                argumentsDelta
                    (Just "item-abandoned")
                    (Just 0)
                    (readArguments "abandoned.txt")
            waitForReadFileSpeculation cache
            retainFinalReadFileCalls cache []

            metrics <- readReadFileSpeculationMetrics cache
            metrics.speculativeReadsCancelled `shouldBe` 1

    it "caps concurrent speculative reads" do
        withSpeculation \dir _ cache -> do
            forM_ [1 :: Int .. 5] \index -> do
                let suffix = Text.pack (show index)
                    target = "candidate-" <> suffix <> ".txt"
                    callId = "call-cap-" <> suffix
                    itemId = Just ("item-cap-" <> suffix)
                Text.writeFile (dir </> Text.unpack target) suffix
                observeReadFileStreamEvent cache $
                    outputItemAdded itemId (Just index) callId ""
                observeReadFileStreamEvent cache $
                    argumentsDelta
                        itemId
                        (Just index)
                        (readArguments target)
            waitForReadFileSpeculation cache

            metrics <- readReadFileSpeculationMetrics cache
            metrics.speculativeReadsStarted `shouldBe` 4

withSpeculation
    :: (FilePath -> ToolEnv -> ReadFileSpeculation -> IO a)
    -> IO a
withSpeculation =
    withPreparedSpeculation (const (pure ()))

withPreparedSpeculation
    :: (FilePath -> IO ())
    -> (FilePath -> ToolEnv -> ReadFileSpeculation -> IO a)
    -> IO a
withPreparedSpeculation prepare action =
    withTempDir \dir -> do
        prepare dir
        env <- defaultToolEnv (unsafeEncodeUtf dir)
        bracket
            (newReadFileSpeculation env)
            closeReadFileSpeculation
            (action dir env)

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir action = do
    tmp <- getTemporaryDirectory
    bracket
        (mkdtemp (tmp </> "agent-read-speculation-XXXXXX"))
        removeDirectoryRecursive
        action

initializeGitRepository :: FilePath -> IO ()
initializeGitRepository dir = do
    (exitCode, _, stderrText) <-
        readProcessWithExitCode "git" ["-C", dir, "init", "-q"] ""
    case exitCode of
        ExitSuccess -> pure ()
        ExitFailure code ->
            expectationFailure $
                "git init failed with exit "
                    <> show code
                    <> ": "
                    <> stderrText

dispatchRead
    :: ToolEnv
    -> ReadFileSpeculation
    -> Text
    -> Text
    -> IO Text
dispatchRead env cache callId arguments = do
    registry <- case mkToolRegistry [readFileToolWithSpeculation env (Just cache)] of
        Left err -> expectationFailure (Text.unpack err) >> fail "invalid registry"
        Right value -> pure value
    result <-
        dispatchRegisteredToolCall
            defaultLoopDispatch
            registry
            (functionToolCall callId "read_file" arguments)
    pure result.output

readArguments :: Text -> Text
readArguments target =
    Text.decodeUtf8 $
        LazyByteString.toStrict $
            Aeson.encode $
                Aeson.object ["target_file" Aeson..= target]

finalReadCall :: Maybe Text -> Text -> Text -> ResponseItem
finalReadCall itemId callId arguments =
    FunctionCallItem FunctionCall
        { itemId
        , callId
        , name = "read_file"
        , arguments
        , status = Nothing
        , extraFields = KeyMap.empty
        }

outputItemAdded
    :: Maybe Text
    -> Maybe Int
    -> Text
    -> Text
    -> ResponseStreamEvent
outputItemAdded itemId outputIndex callId arguments =
    ResponseOutputItemAddedEvent
        { item = finalReadCall itemId callId arguments
        , outputIndex
        , sequenceNumber = Nothing
        , eventExtraFields = KeyMap.empty
        }

argumentsDelta
    :: Maybe Text
    -> Maybe Int
    -> Text
    -> ResponseStreamEvent
argumentsDelta itemId outputIndex delta =
    OtherResponseStreamEvent
        { otherEventType = EventFunctionCallArgumentsDelta
        , sequenceNumber = Nothing
        , eventExtraFields =
            KeyMap.fromList $
                [("delta", Aeson.String delta)]
                    <> maybe
                        []
                        (\value -> [("item_id", Aeson.String value)])
                        itemId
                    <> maybe
                        []
                        (\value ->
                            [("output_index", Aeson.Number (fromIntegral value))])
                        outputIndex
        }

argumentsDone
    :: Maybe Text
    -> Maybe Int
    -> Maybe Text
    -> Text
    -> ResponseStreamEvent
argumentsDone itemId outputIndex name arguments =
    OtherResponseStreamEvent
        { otherEventType = EventFunctionCallArgumentsDone
        , sequenceNumber = Nothing
        , eventExtraFields =
            KeyMap.fromList $
                [("arguments", Aeson.String arguments)]
                    <> maybe
                        []
                        (\value -> [("item_id", Aeson.String value)])
                        itemId
                    <> maybe
                        []
                        (\value ->
                            [("output_index", Aeson.Number (fromIntegral value))])
                        outputIndex
                    <> maybe
                        []
                        (\value -> [("name", Aeson.String value)])
                        name
        }
