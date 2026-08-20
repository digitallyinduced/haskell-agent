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

        it "treats :reload as a GHCi reload request" do
            parseReplLine ":reload" `shouldBe` ReplReload
            parseReplLine "  :reload  " `shouldBe` ReplReload

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

        it "prints the current session id" do
            parseReplLine "/session" `shouldBe` ReplShowSession
            parseReplLine "/session now"
                `shouldBe` ReplCommandError "usage: /session"

        it "reloads auth from disk/env" do
            parseReplLine "/reload-auth" `shouldBe` ReplReloadAuth
            parseReplLine "  /Reload-Auth  " `shouldBe` ReplReloadAuth
            parseReplLine "/reload-auth now"
                `shouldBe` ReplCommandError "usage: /reload-auth"

        it "pastes clipboard images with an optional caption" do
            parseReplLine "/paste" `shouldBe` ReplPaste ""
            parseReplLine "  /Paste  " `shouldBe` ReplPaste ""
            parseReplLine "/paste what is this?"
                `shouldBe` ReplPaste "what is this?"
            parseReplLine "/paste   keep  spaces"
                `shouldBe` ReplPaste "keep  spaces"

        it "shows the current model with a bare /model" do
            parseReplLine "/model" `shouldBe` ReplShowModel
            parseReplLine "  /Model  " `shouldBe` ReplShowModel

        it "sets a model name" do
            parseReplLine "/model grok-4.5" `shouldBe` ReplSetModel "grok-4.5"
            parseReplLine "/model openai/gpt-5.1"
                `shouldBe` ReplSetModel "openai/gpt-5.1"

        it "rejects extra args on /model" do
            parseReplLine "/model grok-4.5 extra"
                `shouldBe` ReplCommandError "usage: /model [NAME]"

        it "rejects unknown levels, extra args, and unknown commands" do
            parseReplLine "/effort bogus"
                `shouldBe` ReplCommandError
                    "effort must be low, medium, high, or xhigh (got bogus)"
            parseReplLine "/effort high extra"
                `shouldBe` ReplCommandError "usage: /effort [low|medium|high|xhigh]"
            parseReplLine "/bogus"
                `shouldBe` ReplCommandError "unknown command: /bogus"
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

    describe "setModel" do
        it "writes the model onto request params" do
            let updated = setModel "grok-4.5" defaultResponseCreateParams
            currentModel updated `shouldBe` "grok-4.5"
            updated.model `shouldBe` Just "grok-4.5"

        it "preserves other request fields" do
            let original = case defaultResponseCreateParams of
                    ResponseCreateParams{..} -> ResponseCreateParams
                        { model = Just "old-model"
                        , instructions = Just "keep me"
                        , store = Just True
                        , ..
                        }
                updated = setModel "new-model" original
            currentModel updated `shouldBe` "new-model"
            updated.instructions `shouldBe` Just "keep me"
            updated.store `shouldBe` Just True

    describe "currentModel" do
        it "defaults to (unset) when model is missing" do
            currentModel defaultResponseCreateParams `shouldBe` "(unset)"
