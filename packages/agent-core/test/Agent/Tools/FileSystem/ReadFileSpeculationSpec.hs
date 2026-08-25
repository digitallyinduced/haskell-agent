module Agent.Tools.FileSystem.ReadFileSpeculationSpec (spec) where

import Agent.Loop (defaultLoopDispatch)
import Agent.ToolDispatch
    ( ToolArgumentStreamEvent(..)
    , ToolCallResult(..)
    , ToolCallStreamRef(..)
    , functionToolCall
    )
import Agent.Tools.FileSystem.ReadFile (readFileToolWithSpeculation)
import Agent.Tools.FileSystem.ReadFileSpeculation
import Agent.Tools.Speculation
    ( ToolSpeculationRuntime
    , closeToolSpeculationRuntime
    , newToolSpeculationRuntime
    , observeToolArgumentEvent
    , retainToolSpeculation
    , takeToolSpeculation
    , waitForToolSpeculation
    )
import Agent.Tools.Types
    ( ToolEnv
    , defaultToolEnv
    , dispatchRegisteredToolCall
    , mkToolRegistry
    )
import Control.Exception.Safe (bracket)
import Control.Monad (forM_)
import qualified Data.Aeson as Aeson
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
        withSpeculation \dir env cache runtime -> do
            Text.writeFile (dir </> "alpha.txt") "prefetched"
            let callId = "call-complete"
                arguments = readArguments "alpha.txt"
                itemId = Just "item-complete"
                outputIndex = Just 0
            observeToolArgumentEvent runtime $
                outputItemAdded itemId outputIndex callId ""
            observeToolArgumentEvent runtime $
                argumentsDelta itemId outputIndex arguments
            waitForToolSpeculation runtime

            beforeDone <- readReadFileSpeculationMetrics cache
            beforeDone.speculativeCompletePredictions `shouldBe` 1

            retainRead runtime callId arguments
            dispatchRead env cache runtime callId arguments
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
            \_dir env cache runtime -> do
                let callId = "call-prefix"
                    itemId = Just "item-prefix"
                    outputIndex = Just 1
                    arguments = readArguments "src/unique-module.hs"
                observeToolArgumentEvent runtime $
                    outputItemAdded itemId outputIndex callId ""
                -- Let the session-scoped workspace index finish before
                -- testing the incremental filename prediction itself.
                waitForReadFileSpeculation cache
                observeToolArgumentEvent runtime $
                    argumentsDelta
                        itemId
                        outputIndex
                        "{\"target_file\":\"src/uni"
                waitForToolSpeculation runtime

                predicted <- readReadFileSpeculationMetrics cache
                predicted.speculativePrefixPredictions `shouldBe` 1

                retainRead runtime callId arguments
                dispatchRead env cache runtime callId arguments
                    `shouldReturn` "1→unique"
                metrics <- readReadFileSpeculationMetrics cache
                metrics.speculativeReadHits `shouldBe` 1

    it "ignores nested target_file fields in incomplete JSON" do
        withSpeculation \dir _ cache runtime -> do
            Text.writeFile (dir </> "nested.txt") "must not be prefetched"
            let itemId = Just "item-nested"
                outputIndex = Just 2
            observeToolArgumentEvent runtime $
                outputItemAdded itemId outputIndex "call-nested" ""
            observeToolArgumentEvent runtime $
                argumentsDelta
                    itemId
                    outputIndex
                    "{\"metadata\":{\"target_file\":\"nested.txt\"},"
            waitForToolSpeculation runtime

            metrics <- readReadFileSpeculationMetrics cache
            metrics.speculativeReadsStarted `shouldBe` 0

    it "correlates an arguments.done event by output_index without a name" do
        withSpeculation \dir env cache runtime -> do
            Text.writeFile (dir </> "indexed.txt") "first\nindexed"
            let callId = "call-output-index"
                outputIndex = Just 3
                arguments =
                    "{\"target_file\":\"indexed.txt\",\"offset\":2,\"limit\":1}"
            observeToolArgumentEvent runtime $
                outputItemAdded Nothing outputIndex callId ""
            observeToolArgumentEvent runtime $
                argumentsDone Nothing outputIndex Nothing arguments
            waitForToolSpeculation runtime
            retainRead runtime callId arguments

            dispatchRead env cache runtime callId arguments
                `shouldReturn` "2→indexed"
            metrics <- readReadFileSpeculationMetrics cache
            metrics.speculativeCompletePredictions `shouldBe` 1
            metrics.speculativeReadHits `shouldBe` 1

    it "correlates output-index-only deltas after an item-id-keyed add" do
        withSpeculation \dir env cache runtime -> do
            Text.writeFile (dir </> "aliased.txt") "aliased"
            let callId = "call-alias"
                itemId = Just "item-alias"
                outputIndex = Just 4
                arguments = readArguments "aliased.txt"
            observeToolArgumentEvent runtime $
                outputItemAdded itemId outputIndex callId ""
            observeToolArgumentEvent runtime $
                argumentsDelta Nothing outputIndex arguments
            waitForToolSpeculation runtime
            retainRead runtime callId arguments

            dispatchRead env cache runtime callId arguments
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
            \_dir env cache runtime -> do
                let callId = "call-range"
                    itemId = Just "item-range"
                    outputIndex = Just 0
                    arguments =
                        "{\"target_file\":\"src/range-target.txt\",\"offset\":2,\"limit\":1}"
                observeToolArgumentEvent runtime $
                    outputItemAdded itemId outputIndex callId ""
                waitForReadFileSpeculation cache
                observeToolArgumentEvent runtime $
                    argumentsDelta
                        itemId
                        outputIndex
                        "{\"target_file\":\"src/range"
                waitForToolSpeculation runtime
                retainRead runtime callId arguments

                dispatchRead env cache runtime callId arguments
                    `shouldReturn` "2→second"
                metrics <- readReadFileSpeculationMetrics cache
                metrics.speculativeReadHits `shouldBe` 1
                metrics.speculativeReadMisses `shouldBe` 0

    it "does not restart a completed-path prefetch when range fields arrive" do
        withSpeculation \dir env cache runtime -> do
            Text.writeFile (dir </> "range-after-path.txt") "first\nsecond"
            let callId = "call-range-after-path"
                itemId = Just "item-range-after-path"
                outputIndex = Just 0
                arguments =
                    "{\"target_file\":\"range-after-path.txt\",\"offset\":2,\"limit\":1}"
            observeToolArgumentEvent runtime $
                outputItemAdded itemId outputIndex callId ""
            observeToolArgumentEvent runtime $
                argumentsDelta
                    itemId
                    outputIndex
                    "{\"target_file\":\"range-after-path.txt\""
            waitForToolSpeculation runtime
            observeToolArgumentEvent runtime $
                argumentsDelta
                    itemId
                    outputIndex
                    ",\"offset\":2,\"limit\":1}"
            waitForToolSpeculation runtime

            beforeDispatch <- readReadFileSpeculationMetrics cache
            beforeDispatch.speculativeReadsStarted `shouldBe` 1
            beforeDispatch.speculativeReadsCancelled `shouldBe` 0

            retainRead runtime callId arguments
            dispatchRead env cache runtime callId arguments
                `shouldReturn` "2→second"
            metrics <- readReadFileSpeculationMetrics cache
            metrics.speculativeReadHits `shouldBe` 1

    it "falls back when the consumed target differs from the prediction" do
        withSpeculation \dir env cache runtime -> do
            Text.writeFile (dir </> "alpha.txt") "wrong"
            Text.writeFile (dir </> "beta.txt") "right"
            let callId = "call-mismatch"
                predictedArguments = readArguments "alpha.txt"
                finalArguments = readArguments "beta.txt"
            observeToolArgumentEvent runtime $
                outputItemAdded (Just "item-mismatch") (Just 0) callId ""
            observeToolArgumentEvent runtime $
                argumentsDelta
                    (Just "item-mismatch")
                    (Just 0)
                    predictedArguments
            waitForToolSpeculation runtime

            dispatchRead env cache runtime callId finalArguments
                `shouldReturn` "1→right"
            metrics <- readReadFileSpeculationMetrics cache
            metrics.speculativeReadHits `shouldBe` 0
            metrics.speculativeReadMisses `shouldBe` 1

    it "discards a prefetched snapshot when the file changes" do
        withSpeculation \dir env cache runtime -> do
            let path = dir </> "changing.txt"
                callId = "call-stale"
                arguments = readArguments "changing.txt"
            Text.writeFile path "old"
            observeToolArgumentEvent runtime $
                outputItemAdded (Just "item-stale") (Just 0) callId ""
            observeToolArgumentEvent runtime $
                argumentsDelta (Just "item-stale") (Just 0) arguments
            waitForToolSpeculation runtime
            waitForReadFileSpeculation cache
            Text.writeFile path "new-content"
            retainRead runtime callId arguments

            dispatchRead env cache runtime callId arguments
                `shouldReturn` "1→new-content"
            metrics <- readReadFileSpeculationMetrics cache
            metrics.speculativeReadHits `shouldBe` 0
            metrics.speculativeReadMisses `shouldBe` 1
            metrics.speculativeReadStale `shouldBe` 1

    it "cancels predictions abandoned by the finalized response" do
        withSpeculation \dir _ cache runtime -> do
            Text.writeFile (dir </> "abandoned.txt") "unused"
            observeToolArgumentEvent runtime $
                outputItemAdded
                    (Just "item-abandoned")
                    (Just 0)
                    "call-abandoned"
                    ""
            observeToolArgumentEvent runtime $
                argumentsDelta
                    (Just "item-abandoned")
                    (Just 0)
                    (readArguments "abandoned.txt")
            waitForToolSpeculation runtime
            retainToolSpeculation runtime []

            metrics <- readReadFileSpeculationMetrics cache
            metrics.speculativeReadsCancelled `shouldBe` 1

    it "caps concurrent speculative reads" do
        withSpeculation \dir _ cache runtime -> do
            forM_ [1 :: Int .. 5] \index -> do
                let suffix = Text.pack (show index)
                    target = "candidate-" <> suffix <> ".txt"
                    callId = "call-cap-" <> suffix
                    itemId = Just ("item-cap-" <> suffix)
                Text.writeFile (dir </> Text.unpack target) suffix
                observeToolArgumentEvent runtime $
                    outputItemAdded itemId (Just index) callId ""
                observeToolArgumentEvent runtime $
                    argumentsDelta
                        itemId
                        (Just index)
                        (readArguments target)
            waitForToolSpeculation runtime

            metrics <- readReadFileSpeculationMetrics cache
            metrics.speculativeReadsStarted `shouldBe` 4

    it "releases speculative-read capacity after preparing a result" do
        withSpeculation \dir env cache runtime -> do
            forM_ [1 :: Int .. 4] \index -> do
                let suffix = Text.pack (show index)
                    target = "retained-" <> suffix <> ".txt"
                    callId = "call-retained-" <> suffix
                    itemId = Just ("item-retained-" <> suffix)
                Text.writeFile (dir </> Text.unpack target) suffix
                observeToolArgumentEvent runtime $
                    outputItemAdded itemId (Just index) callId ""
                observeToolArgumentEvent runtime $
                    argumentsDelta
                        itemId
                        (Just index)
                        (readArguments target)
            waitForToolSpeculation runtime

            dispatchRead
                env
                cache
                runtime
                "call-retained-1"
                (readArguments "retained-1.txt")
                `shouldReturn` "1→1"

            Text.writeFile (dir </> "retained-5.txt") "5"
            observeToolArgumentEvent runtime $
                outputItemAdded
                    (Just "item-retained-5")
                    (Just 5)
                    "call-retained-5"
                    ""
            observeToolArgumentEvent runtime $
                argumentsDelta
                    (Just "item-retained-5")
                    (Just 5)
                    (readArguments "retained-5.txt")
            waitForToolSpeculation runtime

            metrics <- readReadFileSpeculationMetrics cache
            metrics.speculativeReadsStarted `shouldBe` 5

withSpeculation
    :: (FilePath
        -> ToolEnv
        -> ReadFileSpeculation
        -> ToolSpeculationRuntime
        -> IO a)
    -> IO a
withSpeculation =
    withPreparedSpeculation (const (pure ()))

withPreparedSpeculation
    :: (FilePath -> IO ())
    -> (FilePath
        -> ToolEnv
        -> ReadFileSpeculation
        -> ToolSpeculationRuntime
        -> IO a)
    -> IO a
withPreparedSpeculation prepare action =
    withTempDir \dir -> do
        prepare dir
        env <- defaultToolEnv (unsafeEncodeUtf dir)
        cache <- newReadFileSpeculation env
        let tool = readFileToolWithSpeculation env (Just cache)
        bracket
            (newToolSpeculationRuntime [tool])
            closeToolSpeculationRuntime
            (action dir env cache)

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
    -> ToolSpeculationRuntime
    -> Text
    -> Text
    -> IO Text
dispatchRead env cache runtime callId arguments = do
    let call = functionToolCall callId "read_file" arguments
    takeToolSpeculation runtime call >>= \case
        Just result -> pure (formatToolResult result)
        Nothing -> do
            registry <-
                case
                    mkToolRegistry
                        [readFileToolWithSpeculation env (Just cache)]
                of
                    Left err ->
                        expectationFailure (Text.unpack err)
                            >> fail "invalid registry"
                    Right value -> pure value
            result <-
                dispatchRegisteredToolCall
                    defaultLoopDispatch
                    registry
                    call
            pure result.output

formatToolResult :: Either Text Text -> Text
formatToolResult = \case
    Left err -> "Error: " <> err
    Right output -> output

retainRead :: ToolSpeculationRuntime -> Text -> Text -> IO ()
retainRead runtime callId arguments =
    retainToolSpeculation runtime
        [functionToolCall callId "read_file" arguments]

readArguments :: Text -> Text
readArguments target =
    Text.decodeUtf8 $
        LazyByteString.toStrict $
            Aeson.encode $
                Aeson.object ["target_file" Aeson..= target]

outputItemAdded
    :: Maybe Text
    -> Maybe Int
    -> Text
    -> Text
    -> ToolArgumentStreamEvent
outputItemAdded itemId outputIndex callId arguments =
    ToolArgumentsStarted
        { argumentStreamRefs = streamRefs itemId outputIndex
        , argumentStreamCallId = callId
        , argumentStreamName = Just "read_file"
        , argumentStreamArguments = arguments
        }

argumentsDelta
    :: Maybe Text
    -> Maybe Int
    -> Text
    -> ToolArgumentStreamEvent
argumentsDelta itemId outputIndex delta =
    ToolArgumentsDelta
        { argumentStreamRefs = streamRefs itemId outputIndex
        , argumentStreamDelta = delta
        }

argumentsDone
    :: Maybe Text
    -> Maybe Int
    -> Maybe Text
    -> Text
    -> ToolArgumentStreamEvent
argumentsDone itemId outputIndex name arguments =
    ToolArgumentsDone
        { argumentStreamRefs = streamRefs itemId outputIndex
        , argumentStreamName = name
        , argumentStreamArguments = arguments
        }

streamRefs :: Maybe Text -> Maybe Int -> [ToolCallStreamRef]
streamRefs itemId outputIndex =
    maybe [] (pure . ToolCallStreamItem) itemId
        <> maybe [] (pure . ToolCallStreamOutput) outputIndex
