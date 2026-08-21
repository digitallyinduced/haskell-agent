module Agent.CLI.SubagentStoreSpec (spec) where

import Agent.CLI.SubagentStore
import Agent.Responses.Types
import Agent.OsPath (OsPath, fromFilePath, toFilePath)
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
                (Completed (Just "done")) (Just "explore")
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
                other -> expectationFailure ("unexpected load: " <> show other)

    it "fails closed on corrupt transcript JSON" do
        withTempDir \dir -> do
            let agentId = SubagentId "agent-corrupt-1"
            saveSubagentState dir agentId [] (Just "r") Interrupted
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
