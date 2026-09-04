module Agent.CLI.BundleSpec (spec) where

import Agent.CLI.Bundle
import Agent.CLI.Options
import Agent.ReasoningEffort (ReasoningEffort(..))
import Control.Exception.Safe (bracket)
import qualified Data.ByteString.Char8 as ByteString
import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hClose, openBinaryTempFile)
import System.OsPath (unsafeEncodeUtf)
import Test.Hspec

spec :: Spec
spec = do
    describe "decodeAgentBundle" do
        it "accepts and fully decodes AgentBundle v1" do
            case decodeAgentBundle validBundleBytes of
                Left err -> expectationFailure err
                Right bundle -> do
                    bundle.bundleName `shouldBe` "review-bundle"
                    bundle.bundleDefaultAgent `shouldBe` "reviewer"
                    Map.keys bundle.bundleAgents `shouldBe` ["reviewer"]
                    let rendered = formatAgentBundle bundle
                    rendered `shouldSatisfy`
                        Text.isInfixOf "Model alias: review"
                    rendered `shouldSatisfy`
                        Text.isInfixOf "Instructions:\n      Review carefully."

        it "uses hermetic workspace defaults" do
            case decodeAgentBundle minimalBundleBytes of
                Left err -> expectationFailure err
                Right bundle ->
                    case Map.lookup "main" bundle.bundleAgents of
                        Nothing -> expectationFailure "missing main agent"
                        Just agent -> do
                            agent.bundleAgentWorkspace.bundleWorkspaceAgentsMd
                                `shouldBe` False
                            agent.bundleAgentWorkspace.bundleWorkspaceAmbientSkills
                                `shouldBe` False

        it "rejects unknown fields, versions, and dangling references" do
            replaceBytes
                "\"max_turns\":7"
                "\"max_turns\":7,\"provider\":\"openai\""
                validBundleBytes
                `shouldSatisfy` leftContains "unknown field"
            replaceBytes
                "\"version\":1"
                "\"version\":2"
                validBundleBytes
                `shouldSatisfy` leftContains "unsupported bundle version"
            replaceBytes
                "\"skills\":[\"review\"]"
                "\"skills\":[\"missing\"]"
                validBundleBytes
                `shouldSatisfy` leftContains "unknown skill"

        it "rejects relative PATH entries and duplicate skill references" do
            replaceBytes
                "/nix/store/tools-a/bin:/nix/store/tools-b/bin"
                "/nix/store/tools-a/bin:relative/bin"
                validBundleBytes
                `shouldSatisfy` leftContains "only absolute entries"
            replaceBytes
                "\"skills\":[\"review\"]"
                "\"skills\":[\"review\",\"review\"]"
                validBundleBytes
                `shouldSatisfy` leftContains "duplicate skill"

    describe "loadAgentBundle" do
        it "fails hard when a declared asset is missing" do
            withTempManifest validBundleBytes \manifestPath -> do
                loaded <- loadAgentBundle (unsafeEncodeUtf manifestPath)
                loaded `shouldSatisfy`
                    leftContains "is not an existing directory"

    describe "prepareBundleRun" do
        it "maps a selected agent into safe provider-neutral options" do
            case decodeAgentBundle validBundleBytes of
                Left err -> expectationFailure err
                Right bundle ->
                    case prepareBundleRun defaultRunOptions bundle of
                        Left err -> expectationFailure err
                        Right prepared -> do
                            prepared.preparedBundleAgentName
                                `shouldBe` "reviewer"
                            let options = prepared.preparedBundleOptions
                            options.optProvider `shouldBe` Nothing
                            options.optModel `shouldBe` Just "review"
                            options.optEffort `shouldBe` Just EffortHigh
                            options.optMaxTurns `shouldBe` 7
                            options.optYolo `shouldBe` False
                            options.optNoYolo `shouldBe` True
                            options.optAgentsMd `shouldBe` False
                            options.optAmbientSkills `shouldBe` False
                            options.optWorktree `shouldBe` True
                            options.optGhci `shouldBe` True
                            options.optBash `shouldBe` True
                            options.optComputerUse `shouldBe` False
                            options.optCodeMode `shouldBe` True
                            options.optBundlePathPrefix
                                `shouldBe`
                                    Just
                                        ( "/nix/store/tools-a/bin"
                                            <> ":/nix/store/tools-b/bin"
                                        )
                            options.optBundleSkillRoots
                                `shouldBe`
                                    [unsafeEncodeUtf
                                        "/nix/store/review-skill"]
                            options.optBundleContext `shouldSatisfy`
                                maybe False
                                    (Text.isInfixOf "Review carefully.")

defaultRunOptions :: BundleRunOptions
defaultRunOptions = BundleRunOptions
    { bundleRunPath = unsafeEncodeUtf "./result"
    , bundleRunAgent = Nothing
    , bundleRunPrompt = Nothing
    , bundleRunPromptFile = Nothing
    , bundleRunCwd = Nothing
    , bundleRunYolo = False
    , bundleRunNoYolo = False
    }

minimalBundleBytes :: ByteString.ByteString
minimalBundleBytes = ByteString.pack
    "{\
    \\"format\":\"haskell-agent-bundle\",\
    \\"version\":1,\
    \\"system\":\"aarch64-darwin\",\
    \\"name\":\"minimal\",\
    \\"default_agent\":\"main\",\
    \\"agents\":{\"main\":{\"instructions\":\"Do the task.\"}}\
    \}"

validBundleBytes :: ByteString.ByteString
validBundleBytes = ByteString.pack $ concat
    [ "{"
    , "\"format\":\"haskell-agent-bundle\","
    , "\"version\":1,"
    , "\"system\":\"aarch64-darwin\","
    , "\"name\":\"review-bundle\","
    , "\"default_agent\":\"reviewer\","
    , "\"environments\":{"
    , "\"dev\":{\"path\":"
    , "\"/nix/store/tools-a/bin:/nix/store/tools-b/bin\"}},"
    , "\"skills\":{"
    , "\"review\":{\"path\":\"/nix/store/review-skill\"}},"
    , "\"agents\":{"
    , "\"reviewer\":{"
    , "\"description\":\"Review changes\","
    , "\"instructions\":\"Review carefully.\","
    , "\"model\":\"review\","
    , "\"effort\":\"high\","
    , "\"environment\":\"dev\","
    , "\"skills\":[\"review\"],"
    , "\"tools\":{"
    , "\"bash\":true,"
    , "\"ghci\":true,"
    , "\"computer_use\":false,"
    , "\"code_mode\":true},"
    , "\"workspace\":{"
    , "\"worktree\":true,"
    , "\"agents_md\":false,"
    , "\"ambient_skills\":false},"
    , "\"max_turns\":7"
    , "}}}"
    ]

replaceBytes
    :: Text.Text
    -> Text.Text
    -> ByteString.ByteString
    -> Either String AgentBundle
replaceBytes needle replacement =
    decodeAgentBundle
        . TextEncoding.encodeUtf8
        . Text.replace needle replacement
        . TextEncoding.decodeUtf8

leftContains :: String -> Either String value -> Bool
leftContains needle = \case
    Left err -> needle `isInfixOf` err
    Right _ -> False

withTempManifest
    :: ByteString.ByteString
    -> (FilePath -> IO value)
    -> IO value
withTempManifest bytes action = do
    temp <- getTemporaryDirectory
    bracket
        (do
            (path, handle) <- openBinaryTempFile temp "agent-bundle.json"
            ByteString.hPut handle bytes
            hClose handle
            pure path)
        removeFile
        action
