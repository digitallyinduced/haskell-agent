module Agent.Tools.OutputArtifactSpec (spec) where

import Agent.ToolDispatch (functionToolCall)
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
import Data.List (nub)
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
