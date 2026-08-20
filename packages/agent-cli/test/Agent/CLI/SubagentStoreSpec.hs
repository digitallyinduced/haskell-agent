module Agent.CLI.SubagentStoreSpec (spec) where

import Agent.CLI.SubagentStore
import Agent.OpenAI.Responses.Types
import Agent.Subagents (SubagentId(..))
import Control.Exception (bracket)
import qualified Data.Aeson.KeyMap as KeyMap
import System.Directory (getTemporaryDirectory, removeDirectoryRecursive, doesFileExist)
import System.FilePath ((</>))
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
            loaded <- loadSubagentState dir agentId
            case loaded of
                Nothing -> expectationFailure "expected persisted state"
                Just (items, meta) -> do
                    length items `shouldBe` 1
                    meta.diskPreviousResponseId `shouldBe` Just "resp-1"
                    meta.diskAgentType `shouldBe` Just "explore"
            doesFileExist (subagentStoreDir dir agentId </> "transcript.json")
                `shouldReturn` True

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir action = do
    tmp <- getTemporaryDirectory
    bracket (mkdtemp (tmp </> "subagent-store-")) removeDirectoryRecursive action
