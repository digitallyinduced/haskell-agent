module Agent.Tools.OutputArtifactSpec (spec) where

import Agent.ToolDispatch
    ( ToolCall
    , ToolCallResult(..)
    , ToolDispatchConfig(..)
    , dispatchToolHandler
    , functionToolCall
    )
import Agent.Tools.OutputArtifact
    ( OutputArtifact(..)
    , artifactTools
    , boundedPreview
    , openOutputArtifact
    , appendOutputArtifact
    , finishOutputArtifact
    , renderOutputArtifactNotice
    , finalizeToolOutput
    , OutputArtifactMetadata(..)
    , outputArtifactMetadata
    , writeOutputArtifactDetailed
    , readOutputArtifact
    , writeOutputArtifact
    )
import Agent.ToolDSL (PropertySchema(..), PropertyType(..))
import Agent.Tools.Types
    ( AppTool(..)
    , ToolEnv(..)
    , defaultToolEnv
    , jsonToolParameters
    , setToolSessionTmp
    )
import Control.Concurrent.Async (mapConcurrently)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (find, nub)
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import System.Directory
    ( createDirectory
    , getTemporaryDirectory
    , removeDirectoryRecursive
    , removeFile
    )
import System.FilePath ((</>))
import System.OsPath (unsafeEncodeUtf)
import System.Posix.Temp (mkdtemp)
import Test.Hspec

spec :: Spec
spec = describe "Agent.Tools.OutputArtifact" do
    it "retains validated chart documents instead of replacing them with temporary artifacts" do
        withTempEnv \env -> do
            let output = "{\"type\":\"chart\",\"summary\":\"Measurements\",\
                    \\"chart\":{\"version\":1,\"kind\":\"bar\",\"title\":\"Measurements\",\
                    \\"x_axis\":{\"type\":\"category\"},\"y_axis\":{},\
                    \\"series\":[{\"name\":\"Count\",\"points\":[{\"x\":\"A\",\"y\":1}]}]}}"
                boundedEnv = env { toolOutputInlineCap = 1 }
            finalizeToolOutput boundedEnv (functionToolCall "chart" "render_chart" "{}") output
                `shouldReturn` output
            finalized <- finalizeToolOutput boundedEnv
                (functionToolCall "ordinary" "read_file" "{}") output
            finalized `shouldNotBe` output

    it "stores and reads an opaque handle" do
        withTempEnv \env -> do
            writeOutputArtifact env "hello" >>= \case
                Left err -> expectationFailure (Text.unpack err)
                Right handle -> do
                    handle `shouldSatisfy` (Text.isPrefixOf "output-")
                    readOutputArtifact env handle `shouldReturn` Right "hello"
                    outputArtifactMetadata env handle
                        `shouldReturn` Right
                            (OutputArtifactMetadata handle 5 5)
    it "keeps previews bounded with a middle omission marker" do
        let result = boundedPreview 40 (Text.replicate 20 "0123456789")
        Text.length result `shouldSatisfy` (<= 40)
        result `shouldSatisfy` Text.isInfixOf "omitted"
    it "returns a compact marker for oversized output" do
        withTempEnv \env -> do
            let call = functionToolCall "c" "shell" ""
            rendered <- finalizeToolOutput env call (Text.replicate 60000 "x")
            rendered `shouldSatisfy` Text.isInfixOf "stored as artifact"
            rendered `shouldSatisfy`
                (not . Text.isInfixOf (Text.replicate 20000 "x"))

    it "caps persisted bytes and reports the cap in the marker" do
        withTempEnv \base -> do
            let env = base
                    { toolOutputInlineCap = 8
                    , toolOutputPreviewCap = 8
                    , toolOutputArtifactCap = 16
                    }
                call = functionToolCall "c" "shell" ""
            rendered <- finalizeToolOutput env call (Text.replicate 100 "x")
            rendered `shouldSatisfy` Text.isInfixOf "storage cap reached"
            rendered `shouldSatisfy` Text.isInfixOf "stored output is incomplete"
            rendered `shouldSatisfy` (not . Text.isInfixOf "complete tool response stored")
            handles <- listArtifactHandles rendered
            case handles of
                [] -> expectationFailure "artifact handle missing from marker"
                handle : _ ->
                    readOutputArtifact env handle >>= \case
                        Left err -> expectationFailure (Text.unpack err)
                        Right stored -> Text.length stored `shouldBe` 16

    it "exports complete single-line JSON explicitly for programmatic aggregation" do
        withTempEnv \env -> do
            let values = [1 .. 20000] :: [Int]
                bytes = LazyByteString.toStrict (Aeson.encode values)
            rendered <- finalizeToolOutput env
                (functionToolCall "response" "mcp_call" "{}")
                (Encoding.decodeUtf8 bytes)
            handles <- listArtifactHandles rendered
            case handles of
                [] -> expectationFailure "artifact handle missing"
                handle : _ -> do
                    captured <- newIORef ""
                    let analysis _ _ instruction =
                            writeIORef captured instruction >> pure (Right "spawned")
                    _ <- runArtifactToolWithAnalysis env (Just analysis)
                        "analyze_tool_output" $
                        functionToolCall "analysis" "analyze_tool_output"
                            ("{\"handle\":\"" <> handle <> "\",\"instruction\":\"Sum the values.\"}")
                    instruction <- readIORef captured
                    exported <- runArtifactTool env "export_tool_output" $
                        functionToolCall "export" "export_tool_output"
                            ("{\"handle\":\"" <> handle <> "\"}")
                    case exported >>= decodeObject of
                        Left err -> expectationFailure (Text.unpack err)
                        Right result -> case KeyMap.lookup "path" result of
                            Just (Aeson.String path) -> do
                                stored <- ByteString.readFile (Text.unpack path)
                                stored `shouldBe` bytes
                                (sum <$> (Aeson.eitherDecodeStrict stored :: Either String [Int]))
                                    `shouldBe` Right 200010000
                            _ -> expectationFailure "export path missing"
                    instruction `shouldSatisfy` Text.isInfixOf "export_tool_output"
                    instruction `shouldSatisfy` Text.isInfixOf "untrusted data"
                    instruction `shouldSatisfy` Text.isInfixOf "Sum the values."

    it "verifies stored bytes and retains failure across repeated finishes" do
        withTempEnv \env ->
            openOutputArtifact env >>= \case
                Left err -> expectationFailure (Text.unpack err)
                Right writer -> do
                    appendOutputArtifact writer "hello" `shouldReturn` Right ()
                    complete <- finishOutputArtifact writer
                    complete.artifactTruncated `shouldBe` False
                    ByteString.writeFile (fromJust complete.artifactPath) "hi"
                    partial <- finishOutputArtifact writer
                    partial.artifactStoredBytes `shouldBe` 2
                    partial.artifactTruncated `shouldBe` True
                    let notice = renderOutputArtifactNotice "test" partial
                    notice `shouldSatisfy` Text.isInfixOf "stored output is incomplete"
                    notice `shouldSatisfy` (not . Text.isInfixOf "complete tool response stored")
                    ByteString.writeFile (fromJust complete.artifactPath) "hello"
                    retried <- finishOutputArtifact writer
                    retried.artifactTruncated `shouldBe` True

    it "does not claim completeness when the finalized file cannot be inspected" do
        withTempEnv \env ->
            openOutputArtifact env >>= \case
                Left err -> expectationFailure (Text.unpack err)
                Right writer -> do
                    appendOutputArtifact writer "hello" `shouldReturn` Right ()
                    complete <- finishOutputArtifact writer
                    removeFile (fromJust complete.artifactPath)
                    missing <- finishOutputArtifact writer
                    missing.artifactStoredBytes `shouldBe` 0
                    missing.artifactTruncated `shouldBe` True

    it "allocates unique handles concurrently" do
        withTempEnv \env -> do
            results <- mapConcurrently
                (\n -> writeOutputArtifact env (Text.pack (show n)))
                [1 :: Int .. 16]
            let handles = [handle | Right handle <- results]
            length handles `shouldBe` 16
            length (nub handles) `shouldBe` length handles

    it "rejects traversal handles" do
        withTempEnv \env ->
            readOutputArtifact env "../output-secret"
                `shouldReturn` Left "invalid tool-output artifact handle"

    it "does not delegate invalid or missing artifact paths" do
        withTempEnv \env -> do
            called <- newIORef False
            let analysis _ _ _ = writeIORef called True >> pure (Right "spawned")
            mapM_ (\handle -> do
                _ <- runArtifactToolWithAnalysis env (Just analysis)
                    "analyze_tool_output" $
                    functionToolCall "analysis" "analyze_tool_output"
                        ("{\"handle\":\"" <> handle <> "\",\"instruction\":\"Inspect.\"}")
                readIORef called `shouldReturn` False)
                ["../output-secret", "output-missing"]

    it "reports on-disk bytes for invalid UTF-8 artifacts" do
        withTempEnv \env -> do
            writeOutputArtifactDetailed env "\xc3" >>= \case
                Left err -> expectationFailure (Text.unpack err)
                Right artifact -> do
                    outputArtifactMetadata env artifact.artifactHandle
                        `shouldReturn`
                        Right (OutputArtifactMetadata artifact.artifactHandle 1 1)

    it "reads bounded ranges without retaining giant lines" do
        withTempEnv \env -> do
            let bytes =
                    ByteString.concat
                        [ "first\n"
                        , ByteString.replicate (2 * 1024 * 1024) 120
                        , "\nlast\n"
                        ]
            writeOutputArtifactDetailed env bytes >>= \case
                Left err -> expectationFailure (Text.unpack err)
                Right artifact -> do
                    result <- runArtifactTool env "read_tool_output" $
                        functionToolCall "read" "read_tool_output"
                            ( "{\"handle\":\"" <> artifact.artifactHandle
                                <> "\",\"offset\":2,\"limit\":1}" )
                    result `shouldSatisfy` \case
                        Left _ -> False
                        Right value ->
                            Text.length value < 50 * 1024
                                && Text.isInfixOf "line omitted" value

    it "reconstructs the full minified JSON through native character pages" do
        withTempEnv \env -> do
            let values = [1 .. 20000] :: [Int]
                original = Encoding.decodeUtf8 (LazyByteString.toStrict (Aeson.encode values))
            writeOutputArtifact env original >>= \case
                Left err -> expectationFailure (Text.unpack err)
                Right handle -> do
                    reconstructed <- collectPages env handle 0 []
                    reconstructed `shouldBe` original
                    (sum <$> (Aeson.eitherDecodeStrict (Encoding.encodeUtf8 reconstructed)
                        :: Either String [Int])) `shouldBe` Right 200010000

    it "returns a late matching value rather than the beginning of its JSON line" do
        withTempEnv \env -> do
            let original = "{\"padding\":\"" <> Text.replicate 100000 "x"
                    <> "\",\"amount\":19900,\"currency\":\"eur\"}"
            writeOutputArtifact env original >>= \case
                Left err -> expectationFailure (Text.unpack err)
                Right handle -> do
                    result <- runArtifactTool env "search_tool_output" $
                        functionToolCall "search" "search_tool_output"
                            ("{\"handle\":\"" <> handle
                                <> "\",\"pattern\":\"amount\",\"context_chars\":30}")
                    result `shouldSatisfy` \case
                        Right value -> Text.isInfixOf "19900" value && Text.length value < 1000
                        Left _ -> False

    it "advertises integer pagination parameters" do
        withTempEnv \env -> do
            let types name =
                    [ (property.propertyName, property.propertyType)
                    | tool <- artifactTools env Nothing
                    , tool.appToolName == name
                    , property <- fromMaybe [] (jsonToolParameters tool)
                    , property.propertyName `elem`
                        ["offset", "limit", "cursor", "max_chars", "head_limit", "context_chars"]
                    ]
            types "read_tool_output" `shouldBe`
                [ ("offset", PropertyInteger)
                , ("limit", PropertyInteger)
                , ("cursor", PropertyInteger)
                , ("max_chars", PropertyInteger)
                ]
            types "search_tool_output" `shouldBe`
                [ ("head_limit", PropertyInteger)
                , ("cursor", PropertyInteger)
                , ("context_chars", PropertyInteger)
                ]

    it "accepts whole-number JSON floats for line pagination" do
        withTempEnv \env -> do
            writeOutputArtifact env "alpha\nbeta\ngamma\n" >>= \case
                Left err -> expectationFailure (Text.unpack err)
                Right handle -> do
                    result <- runArtifactTool env "read_tool_output" $
                        functionToolCall "read" "read_tool_output"
                            ("{\"handle\":\"" <> handle
                                <> "\",\"offset\":2.0,\"limit\":1.0}")
                    result `shouldSatisfy` \case
                        Right value ->
                            Text.isInfixOf "beta" value
                                && not (Text.isInfixOf "SIMDException" value)
                        Left _ -> False
                    negative <- runArtifactTool env "read_tool_output" $
                        functionToolCall "read" "read_tool_output"
                            ("{\"handle\":\"" <> handle
                                <> "\",\"offset\":-50.0}")
                    negative `shouldSatisfy` \case
                        Right value -> not (Text.isInfixOf "SIMDException" value)
                        Left _ -> False

    it "rejects mixed line and character pagination arguments" do
        withTempEnv \env -> do
            result <- runArtifactTool env "read_tool_output" $
                functionToolCall "read" "read_tool_output"
                    "{\"handle\":\"output-missing\",\"cursor\":0,\"offset\":1}"
            result `shouldSatisfy` \case
                Right value -> Text.isInfixOf "cannot be combined" value
                Left _ -> False

    it "searches giant lines while returning a bounded preview" do
        withTempEnv \env -> do
            let bytes =
                    ByteString.concat
                        [ "needle"
                        , ByteString.replicate (2 * 1024 * 1024) 97
                        , "\n"
                        ]
            writeOutputArtifactDetailed env bytes >>= \case
                Left err -> expectationFailure (Text.unpack err)
                Right artifact -> do
                    result <- runArtifactTool env "search_tool_output" $
                        functionToolCall "search" "search_tool_output"
                            ( "{\"handle\":\"" <> artifact.artifactHandle
                                <> "\",\"pattern\":\"needle\",\"head_limit\":5}" )
                    result `shouldSatisfy` \case
                        Left _ -> False
                        Right value ->
                            Text.length value < 50 * 1024
                                && Text.isInfixOf "\"start\":0" value
                                && Text.isInfixOf "needle" value

    it "finds a literal split across streaming input chunks" do
        withTempEnv \env -> do
            let bytes =
                    ByteString.concat
                        [ ByteString.replicate (32768 - 3) 97
                        , "nee"
                        , "dle\n"
                        ]
            writeOutputArtifactDetailed env bytes >>= \case
                Left err -> expectationFailure (Text.unpack err)
                Right artifact -> do
                    result <- runArtifactTool env "search_tool_output" $
                        functionToolCall "search" "search_tool_output"
                            ( "{\"handle\":\"" <> artifact.artifactHandle
                                <> "\",\"pattern\":\"needle\"}" )
                    result `shouldSatisfy` \case
                        Left _ -> False
                        Right value -> Text.isInfixOf "needle" value

    it "stops after proving that the match cap was exceeded" do
        withTempEnv \env -> do
            let bytes = ByteString.concat (replicate 100 "needle\n")
            writeOutputArtifactDetailed env bytes >>= \case
                Left err -> expectationFailure (Text.unpack err)
                Right artifact -> do
                    result <- runArtifactTool env "search_tool_output" $
                        functionToolCall "search" "search_tool_output"
                            ( "{\"handle\":\"" <> artifact.artifactHandle
                                <> "\",\"pattern\":\"needle\","
                                <> "\"head_limit\":5}" )
                    result `shouldSatisfy` \case
                        Left _ -> False
                        Right value ->
                            Text.isInfixOf "\"next_cursor\":34" value

    it "preserves Unicode case-insensitive artifact search" do
        withTempEnv \env -> do
            writeOutputArtifact env "Straße\n" >>= \case
                Left err -> expectationFailure (Text.unpack err)
                Right handle -> do
                    result <- runArtifactTool env "search_tool_output" $
                        functionToolCall "search" "search_tool_output"
                            ( "{\"handle\":\"" <> handle
                                <> "\",\"pattern\":\"STRASSE\","
                                <> "\"case_insensitive\":true}" )
                    result `shouldSatisfy` \case
                        Left _ -> False
                        Right value -> Text.isInfixOf "Straße" value

    it "exposes delegated analysis only when a spawner is available" do
        withTempEnv \env -> do
            let names = map (.appToolName)
                childNames = names (artifactTools env Nothing)
                rootTools = artifactTools env
                    (Just (\_ _ _ -> pure (Right "spawned")))
                rootNames = names rootTools
            childNames `shouldBe`
                ["read_tool_output", "search_tool_output", "export_tool_output"]
            rootNames `shouldBe`
                [ "read_tool_output"
                , "search_tool_output"
                , "export_tool_output"
                , "analyze_tool_output"
                ]
            let analysisDescriptions =
                    [ tool.appToolDescription
                    | tool <- rootTools
                    , tool.appToolName == "analyze_tool_output"
                    ]
            analysisDescriptions `shouldBe`
                [ "Spawn a tracked child agent to analyze an oversized tool-output artifact. \
                \Use wait_agent for its report."
                ]

decodeObject :: Text.Text -> Either Text.Text Aeson.Object
decodeObject value =
    case Aeson.eitherDecodeStrict (Encoding.encodeUtf8 value) of
        Left err -> Left (Text.pack err)
        Right (Aeson.Object object) -> Right object
        Right _ -> Left "expected JSON object"

collectPages :: ToolEnv -> Text.Text -> Int -> [Text.Text] -> IO Text.Text
collectPages env handle cursor pages = do
    result <- runArtifactTool env "read_tool_output" $
        functionToolCall "read" "read_tool_output"
            ("{\"handle\":\"" <> handle <> "\",\"cursor\":"
                <> Text.pack (show cursor) <> "}")
    case result >>= decodeObject of
        Left err -> expectationFailure (Text.unpack err) >> pure ""
        Right page -> case (KeyMap.lookup "text" page, KeyMap.lookup "next_cursor" page) of
            (Just (Aeson.String text), Just Aeson.Null) ->
                pure (Text.concat (reverse (text : pages)))
            (Just (Aeson.String text), Just nextValue) -> case Aeson.fromJSON nextValue of
                Aeson.Success next -> do
                    next `shouldSatisfy` (> cursor)
                    collectPages env handle next (text : pages)
                Aeson.Error err -> expectationFailure err >> pure ""
            _ -> expectationFailure "invalid character page" >> pure ""

listArtifactHandles :: Text.Text -> IO [Text.Text]
listArtifactHandles rendered =
    pure
        [ Text.takeWhile validHandleCharacter token
        | token <- Text.words rendered
        , "output-" `Text.isPrefixOf` token
        ]
  where
    validHandleCharacter character =
        character /= ';' && character /= ']' && character /= ','

runArtifactTool
    :: ToolEnv
    -> Text.Text
    -> ToolCall
    -> IO (Either Text.Text Text.Text)
runArtifactTool env toolName call = do
    runArtifactToolWithAnalysis env Nothing toolName call

runArtifactToolWithAnalysis
    :: ToolEnv
    -> Maybe (ToolCall -> Text.Text -> Text.Text -> IO (Either Text.Text Text.Text))
    -> Text.Text
    -> ToolCall
    -> IO (Either Text.Text Text.Text)
runArtifactToolWithAnalysis env analysis toolName call = do
    let tool = find ((== toolName) . (.appToolName)) (artifactTools env analysis)
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
    pure (Right result.output)

withTempEnv :: (ToolEnv -> IO a) -> IO a
withTempEnv action = do
    root <- getTemporaryDirectory
    dir <- mkdtemp (root </> "agent-artifacts-")
    env <- defaultToolEnv (unsafeEncodeUtf dir)
    createDirectory (dir </> "session")
    setToolSessionTmp env (Just (unsafeEncodeUtf (dir </> "session")))
    result <- action env
    removeDirectoryRecursive dir
    pure result
