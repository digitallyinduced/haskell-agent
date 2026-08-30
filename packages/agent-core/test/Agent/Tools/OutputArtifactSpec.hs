module Agent.Tools.OutputArtifactSpec (spec) where

import Agent.ToolDispatch
    ( ToolCall
    , ToolDispatchConfig(..)
    , dispatchToolHandler
    , functionToolCall
    )
import Agent.Tools.OutputArtifact
    ( artifactTools
    , boundedPreview
    , finalizeToolOutput
    , OutputArtifactMetadata(..)
    , outputArtifactMetadata
    , writeOutputArtifactDetailed
    , readOutputArtifact
    , writeOutputArtifact
    )
import Agent.Tools.Types
    ( AppTool(..)
    , ToolEnv(..)
    , defaultToolEnv
    , setToolSessionTmp
    )
import Control.Concurrent.Async (mapConcurrently)
import qualified Data.ByteString as ByteString
import Data.List (find, nub)
import qualified Data.Text as Text
import System.Directory
    ( createDirectory
    , getTemporaryDirectory
    , removeDirectoryRecursive
    )
import System.FilePath ((</>))
import System.OsPath (unsafeEncodeUtf)
import System.Posix.Temp (mkdtemp)
import Test.Hspec

spec :: Spec
spec = describe "Agent.Tools.OutputArtifact" do
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
            handles <- listArtifactHandles rendered
            case handles of
                [] -> expectationFailure "artifact handle missing from marker"
                handle : _ ->
                    readOutputArtifact env handle >>= \case
                        Left err -> expectationFailure (Text.unpack err)
                        Right stored -> Text.length stored `shouldBe` 16

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
                                && Text.isInfixOf "1:" value
                                && Text.isInfixOf "needle" value

    it "exposes delegated analysis only when a spawner is available" do
        withTempEnv \env -> do
            let names = map (.appToolName)
                childNames = names (artifactTools env Nothing)
                rootNames = names
                    (artifactTools env
                        (Just (\_ _ _ -> pure (Right "spawned"))))
            childNames `shouldBe`
                ["read_tool_output", "search_tool_output"]
            rootNames `shouldBe`
                [ "read_tool_output"
                , "search_tool_output"
                , "analyze_tool_output"
                ]

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
    let tool = find ((== toolName) . (.appToolName)) (artifactTools env Nothing)
        config = ToolDispatchConfig
            { toolDispatchUnknownTool = ("unknown tool: " <>)
            , toolDispatchFormatResult = either id id
            , toolDispatchFormatException = \_ exception ->
                Text.pack (show exception)
            , toolDispatchOnException = \_ _ -> pure ()
            , toolDispatchOnOutput = \_ _ -> pure ()
            , toolDispatchFinalizeOutput = \_ output -> pure output
            }
    result <- dispatchToolHandler config (appToolHandler <$> tool) call
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
