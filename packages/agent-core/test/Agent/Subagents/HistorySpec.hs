module Agent.Subagents.HistorySpec (spec) where

import Agent.Responses.Types (ResponseRole(..))
import Agent.Subagents.History
import Agent.Subagents.TestItems
import Test.Hspec

spec :: Spec
spec = describe "Agent.Subagents.History" do
    describe "trimDanglingToolSuffix" do
        it "handles empty histories and a reasoning-only prefix before a live call" do
            trimDanglingToolSuffix [] `shouldBe` []
            trimDanglingToolSuffix
                [reasoningItem, reasoningItem, functionCallItem "live"]
                `shouldBe` []

        it "drops only trailing reasoning while preserving earlier reasoning" do
            let prefix = [reasoningItem, userItem "before"]
            trimDanglingToolSuffix
                (prefix <> [reasoningItem, reasoningItem, functionCallItem "live"])
                `shouldBe` prefix

        it "keeps complete tool pairs" do
            let items =
                    [ userItem "before"
                    , functionCallItem "call-1"
                    , functionOutputItem "call-1"
                    ]
            trimDanglingToolSuffix items `shouldBe` items

        it "drops reasoning and an unmatched live tool call" do
            let prefix = [userItem "before"]
                items =
                    prefix
                        <> [ reasoningItem
                           , functionCallItem "call-live"
                           ]
            trimDanglingToolSuffix items `shouldBe` prefix

        it "removes an old unmatched call before a later message" do
            let items =
                    [ userItem "before"
                    , functionCallItem "old-call"
                    , userItem "continued later"
                    ]
            trimDanglingToolSuffix items
                `shouldBe` [userItem "before", userItem "continued later"]

        it "removes an orphan output without a preceding call" do
            let items =
                    [ functionOutputItem "orphan"
                    , userItem "continued later"
                    ]
            trimDanglingToolSuffix items `shouldBe` [userItem "continued later"]

        it "keeps complete native computer-call pairs" do
            let items =
                    [ userItem "before"
                    , computerCallItem "computer-1"
                    , computerOutputItem "computer-1"
                    ]
            trimDanglingToolSuffix items `shouldBe` items

        it "drops reasoning and an unmatched native computer call" do
            let prefix = [userItem "before"]
                items = prefix <> [reasoningItem, computerCallItem "computer-live"]
            trimDanglingToolSuffix items `shouldBe` prefix

        it "removes an orphan native computer output" do
            let items =
                    [ computerOutputItem "computer-orphan"
                    , userItem "continued later"
                    ]
            trimDanglingToolSuffix items `shouldBe` [userItem "continued later"]

        it "does not pair an output that precedes its call" do
            let items =
                    [ functionOutputItem "torn"
                    , functionCallItem "torn"
                    , userItem "continued later"
                    ]
            trimDanglingToolSuffix items `shouldBe` [userItem "continued later"]

    describe "forkSubagentTranscript" do
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

        it "does not inherit a tool call the parent is still running" do
            let complete = [messageItem RoleUser "one"]
            forkSubagentTranscript Nothing
                (complete <> [reasoningItem, functionCallItem "spawn"])
                `shouldBe` complete
