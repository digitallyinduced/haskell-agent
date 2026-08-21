module Agent.CLI.SubagentStoreSpec (spec) where

import Agent.CLI.SubagentStore
import Agent.Responses.Types
import Agent.OsPath (OsPath, fromFilePath, toFilePath)
import Agent.Subagents (SubagentId(..))
import Control.Exception (bracket)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
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
                item = MessageItem ResponseMessage
                    { messageId = Nothing
                    , content = MessageContentText "hello"
                    , role = RoleUser
                    , status = Nothing
                    , phase = Nothing
                    , extraFields = KeyMap.empty
                    }
            saveSubagentState dir agentId [item] (Just "resp-1") (Just "explore")
                (Just "grok-4.5-mini") (Just (fromFilePath "/tmp/work"))
                `shouldReturn` Right ()
            loaded <- loadSubagentState dir agentId
            case loaded of
                Right (Just (items, meta)) -> do
                    length items `shouldBe` 1
                    meta.diskPreviousResponseId `shouldBe` Just "resp-1"
                    meta.diskAgentType `shouldBe` Just "explore"
                    meta.diskAgentModel `shouldBe` Just "grok-4.5-mini"
                    meta.diskCwd `shouldBe` Just (fromFilePath "/tmp/work")
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

    it "fails closed on corrupt transcript JSON" do
        withTempDir \dir -> do
            let agentId = SubagentId "agent-corrupt-1"
            saveSubagentState dir agentId [] (Just "r") Nothing Nothing Nothing
                `shouldReturn` Right ()
            Right path <- pure (subagentStoreDir dir agentId)
            LBS.writeFile
                (toFilePath (path </> fromFilePath "transcript.json"))
                "{not-json"
            loadSubagentState dir agentId >>= \case
                Left err -> err `shouldSatisfy` (not . null . show)
                Right _ -> expectationFailure "expected decode failure"

withTempDir :: (OsPath -> IO a) -> IO a
withTempDir action = do
    tmp <- Directory.getTemporaryDirectory
    bracket
        (mkdtemp (tmp FilePath.</> "subagent-store-"))
        Directory.removeDirectoryRecursive
        (action . fromFilePath)
