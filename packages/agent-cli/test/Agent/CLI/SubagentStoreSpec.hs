module Agent.CLI.SubagentStoreSpec (spec) where

import Agent.CLI.SubagentStore
import Agent.CLI.Subagents.Runtime (validatePersistedSubagentTarget)
import Agent.CLI.Session (LegacySubagentTarget(..))
import Agent.Dialect (DialectId(..))
import Agent.Provider (Provider(..))
import Agent.Responses.Types
import System.OsPath (OsPath, decodeUtf, unsafeEncodeUtf)
import Agent.Subagents
    ( SubagentId(..)
    , SubagentIdentity(..)
    , SubagentStatus(..)
    )
import Agent.Subagents.TaskPath (parseTaskPath)
import Control.Exception.Safe (bracket)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified System.Directory as Directory
import System.Directory.OsPath (doesFileExist)
import qualified System.FilePath as FilePath
import System.OsPath ((</>))
import System.Posix.Temp (mkdtemp)
import Test.Hspec

fromFilePath = unsafeEncodeUtf
toFilePath path = either (error . show) id (decodeUtf path)

spec :: Spec
spec = describe "Agent.CLI.SubagentStore" do
    it "round-trips transcript items and meta" do
        withTempDir \dir -> do
            let agentId = SubagentId "agent-test-1"
                parentId = SubagentId "agent-parent-1"
                item = MessageItem ResponseMessage
                    { messageId = Nothing
                    , content = MessageContentText "hello"
                    , role = RoleUser
                    , status = Nothing
                    , phase = Nothing
                    , extraFields = KeyMap.empty
                    }
            Right taskPath <- pure (parseTaskPath "/root/research/worker")
            let identity = SubagentIdentity (Just parentId) 2 taskPath
            saveSubagentState dir agentId [item] (Just "resp-1")
                (Completed (Just "done")) XAIProvider "grok-4.5-mini"
                GrokBuildDialect (Just "explore")
                (Just "grok-4.5-mini") (Just "high")
                (Just (fromFilePath "/tmp/work"))
                (Just identity)
                `shouldReturn` Right ()
            loaded <- loadSubagentState dir agentId
            case loaded of
                Right (Just (items, meta)) -> do
                    length items `shouldBe` 1
                    meta.diskPreviousResponseId `shouldBe` Just "resp-1"
                    meta.diskStatus `shouldBe` Just (Completed (Just "done"))
                    meta.diskProvider `shouldBe` Just XAIProvider
                    meta.diskEffectiveModel `shouldBe` Just "grok-4.5-mini"
                    meta.diskDialect `shouldBe` Just GrokBuildDialect
                    meta.diskAgentType `shouldBe` Just "explore"
                    meta.diskAgentModel `shouldBe` Just "grok-4.5-mini"
                    meta.diskReasoningEffort `shouldBe` Just "high"
                    meta.diskCwd `shouldBe` Just (fromFilePath "/tmp/work")
                    meta.diskTaskPath `shouldBe` Just "/root/research/worker"
                    meta.diskParentId `shouldBe` Just parentId
                    meta.diskDepth `shouldBe` Just 2
                other -> expectationFailure ("unexpected load: " <> show other)
            case subagentStoreDir dir agentId of
                Right path ->
                    doesFileExist (path </> fromFilePath "transcript.json")
                        `shouldReturn` True
                Left err -> expectationFailure (show err)

    it "rejects path-traversal agent ids" do
        isValidSubagentStoreId (SubagentId "../..") `shouldBe` False
        isValidSubagentStoreId (SubagentId "agent/../x") `shouldBe` False
        case subagentStoreDir (fromFilePath "/tmp/session") (SubagentId "../..") of
            Left _ -> pure ()
            Right _ -> expectationFailure "expected invalid id"

    it "loads legacy metadata without task topology" do
        withTempDir \dir -> do
            let agentId = SubagentId "agent-legacy-1"
            saveSubagentState dir agentId [] (Just "legacy") Interrupted
                XAIProvider "grok-4.6" GrokBuildDialect
                Nothing Nothing Nothing Nothing Nothing
                `shouldReturn` Right ()
            Right path <- pure (subagentStoreDir dir agentId)
            LBS.writeFile
                (toFilePath (path </> fromFilePath "meta.json"))
                "{\"previousResponseId\":\"legacy\"}"
            loaded <- loadSubagentState dir agentId
            case loaded of
                Right (Just (_, meta)) -> do
                    meta.diskTaskPath `shouldBe` Nothing
                    meta.diskParentId `shouldBe` Nothing
                    meta.diskDepth `shouldBe` Nothing
                    meta.diskStatus `shouldBe` Nothing
                    meta.diskProvider `shouldBe` Nothing
                    meta.diskEffectiveModel `shouldBe` Nothing
                    meta.diskDialect `shouldBe` Nothing
                other -> expectationFailure ("unexpected load: " <> show other)

    it "fails closed on corrupt transcript JSON" do
        withTempDir \dir -> do
            let agentId = SubagentId "agent-corrupt-1"
            saveSubagentState dir agentId [] (Just "r") Interrupted
                OpenAIProvider "gpt-5.6-luna" CodexDialect
                Nothing Nothing Nothing Nothing Nothing
                `shouldReturn` Right ()
            Right path <- pure (subagentStoreDir dir agentId)
            LBS.writeFile
                (toFilePath (path </> fromFilePath "transcript.json"))
                "{not-json"
            loadSubagentState dir agentId >>= \case
                Left err -> err `shouldSatisfy` (not . null . show)
                Right _ -> expectationFailure "expected decode failure"

    it "forks all, none, or the requested number of recent turns" do
        let items =
                [ messageItem RoleUser "one"
                , messageItem RoleAssistant "answer-one"
                , messageItem RoleUser "two"
                , messageItem RoleAssistant "answer-two"
                ]
        forkSubagentTranscript Nothing items `shouldBe` items
        forkSubagentTranscript (Just "none") items `shouldBe` []
        forkSubagentTranscript (Just "1") items
            `shouldBe` drop 2 items
        forkSubagentTranscript (Just "18446744073709551617") items
            `shouldBe` items

    describe "validatePersistedSubagentTarget" do
        it "accepts a matching provider, effective model, and dialect" do
            validatePersistedSubagentTarget
                OpenRouterProvider
                "openai/gpt-5.1"
                CodexDialect
                Nothing
                persistedMeta
                `shouldBe` Right ("openai/gpt-5.1", CodexDialect)

        it "rejects an inherited child after the parent target changes" do
            validatePersistedSubagentTarget
                OpenRouterProvider
                "anthropic/claude-sonnet-4"
                GenericResponsesDialect
                Nothing
                persistedMeta
                `shouldSatisfy` isLeft

        it "rejects transport-specific child models after a provider change" do
            validatePersistedSubagentTarget
                OpenAIProvider
                "openai/gpt-5.1"
                CodexDialect
                Nothing
                persistedMeta
                `shouldSatisfy` isLeft

        it "accepts missing target metadata only in legacy-compatible sessions" do
            validatePersistedSubagentTarget
                OpenRouterProvider
                "openai/gpt-5.1"
                GrokBuildDialect
                (Just legacyTarget)
                legacyMeta
                `shouldBe` Right ("openai/gpt-5.1", GrokBuildDialect)
            validatePersistedSubagentTarget
                OpenRouterProvider
                "openai/gpt-5.1"
                GrokBuildDialect
                Nothing
                legacyMeta
                `shouldSatisfy` isLeft

        it "rejects legacy metadata after a durable root target change" do
            validatePersistedSubagentTarget
                OpenRouterProvider
                "anthropic/claude-sonnet-4"
                GenericResponsesDialect
                (Just legacyTarget)
                legacyMeta
                `shouldSatisfy` isLeft

legacyTarget :: LegacySubagentTarget
legacyTarget = LegacySubagentTarget
    { legacyTargetProvider = OpenRouterProvider
    , legacyTargetEffectiveModel = "openai/gpt-5.1"
    , legacyTargetDialect = GrokBuildDialect
    }

persistedMeta :: SubagentDiskMeta
persistedMeta = legacyMeta
    { diskProvider = Just OpenRouterProvider
    , diskEffectiveModel = Just "openai/gpt-5.1"
    , diskDialect = Just CodexDialect
    }

legacyMeta :: SubagentDiskMeta
legacyMeta = SubagentDiskMeta
    { diskPreviousResponseId = Nothing
    , diskStatus = Nothing
    , diskProvider = Nothing
    , diskEffectiveModel = Nothing
    , diskDialect = Nothing
    , diskAgentType = Nothing
    , diskAgentModel = Nothing
    , diskReasoningEffort = Nothing
    , diskCwd = Nothing
    , diskTaskPath = Nothing
    , diskParentId = Nothing
    , diskDepth = Nothing
    }

isLeft :: Either a b -> Bool
isLeft = \case
    Left _ -> True
    Right _ -> False

messageItem :: ResponseRole -> Text -> ResponseItem
messageItem role text = MessageItem ResponseMessage
    { messageId = Nothing
    , content = MessageContentText text
    , role
    , status = Nothing
    , phase = Nothing
    , extraFields = KeyMap.empty
    }

withTempDir :: (OsPath -> IO a) -> IO a
withTempDir action = do
    tmp <- Directory.getTemporaryDirectory
    bracket
        (mkdtemp (tmp FilePath.</> "subagent-store-"))
        Directory.removeDirectoryRecursive
        (action . fromFilePath)
