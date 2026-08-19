module Agent.CLI.CommandSpec (spec) where

import Agent.CLI.Command
import Agent.OpenAI.Responses.Types
import qualified Data.Aeson.KeyMap as KeyMap
import Test.Hspec

spec :: Spec
spec = do
    describe "parseReplLine" do
        it "keeps :q and :quit as quit" do
            parseReplLine ":q" `shouldBe` ReplQuit
            parseReplLine ":quit" `shouldBe` ReplQuit
            parseReplLine "  :quit  " `shouldBe` ReplQuit

        it "sends ordinary lines to the model" do
            parseReplLine "list the files" `shouldBe` ReplPrompt "list the files"
            parseReplLine ":status" `shouldBe` ReplPrompt ":status"

        it "shows the current effort with a bare /effort" do
            parseReplLine "/effort" `shouldBe` ReplShowEffort
            parseReplLine "  /Effort  " `shouldBe` ReplShowEffort

        it "sets a valid effort level" do
            parseReplLine "/effort high" `shouldBe` ReplSetEffort "high"
            parseReplLine "/effort XHIGH" `shouldBe` ReplSetEffort "xhigh"
            parseReplLine "/effort medium" `shouldBe` ReplSetEffort "medium"

        it "toggles always-approve from slash and colon aliases" do
            parseReplLine "/always-approve" `shouldBe` ReplToggleAlwaysApprove
            parseReplLine "/Always-Approve" `shouldBe` ReplToggleAlwaysApprove
            parseReplLine "/yolo" `shouldBe` ReplToggleAlwaysApprove
            parseReplLine ":yolo" `shouldBe` ReplToggleAlwaysApprove
            parseReplLine ":always-approve" `shouldBe` ReplToggleAlwaysApprove

        it "rejects extra args on /always-approve" do
            parseReplLine "/always-approve now"
                `shouldBe` ReplCommandError "usage: /always-approve"
            parseReplLine "/yolo on"
                `shouldBe` ReplCommandError "usage: /always-approve"

        it "rejects unknown levels, extra args, and unknown commands" do
            parseReplLine "/effort bogus"
                `shouldBe` ReplCommandError
                    "effort must be low, medium, high, or xhigh (got bogus)"
            parseReplLine "/effort high extra"
                `shouldBe` ReplCommandError "usage: /effort [low|medium|high|xhigh]"
            parseReplLine "/model grok-4.5"
                `shouldBe` ReplCommandError "unknown command: /model"
            parseReplLine "/"
                `shouldBe` ReplCommandError "unknown command: /"

    describe "setReasoningEffort" do
        it "writes effort onto an empty reasoning config" do
            let updated = setReasoningEffort "high" defaultResponseCreateParams
            currentEffort updated `shouldBe` "high"
            fmap (.effort) updated.reasoning `shouldBe` Just (Just "high")

        it "preserves other reasoning fields" do
            let original = case defaultResponseCreateParams of
                    ResponseCreateParams{..} -> ResponseCreateParams
                        { reasoning = Just ReasoningConfig
                            { context = Just "256k"
                            , effort = Just "low"
                            , generateSummary = Just "auto"
                            , reasoningMode = Nothing
                            , summary = Just "concise"
                            , extraFields = KeyMap.empty
                            }
                        , ..
                        }
                updated = setReasoningEffort "xhigh" original
            currentEffort updated `shouldBe` "xhigh"
            case updated.reasoning of
                Just config -> do
                    config.context `shouldBe` Just "256k"
                    config.generateSummary `shouldBe` Just "auto"
                    config.summary `shouldBe` Just "concise"
                Nothing -> expectationFailure "expected reasoning config"

    describe "currentEffort" do
        it "defaults to low when reasoning is missing" do
            currentEffort defaultResponseCreateParams `shouldBe` "low"
