module Agent.CLI.ModelPickerSpec (spec) where

import Agent.CLI.ModelPicker
import Agent.CLI.Models
import Agent.CLI.Prompt (defaultModelFor)
import Agent.Provider (Provider(..))
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = do
    describe "decodePickerKey" do
        it "maps arrows and vim keys" do
            decodePickerKey "\ESC[A" `shouldBe` Just PickerUp
            decodePickerKey "\ESC[B" `shouldBe` Just PickerDown
            decodePickerKey "k" `shouldBe` Just PickerUp
            decodePickerKey "j" `shouldBe` Just PickerDown

        it "maps confirm and cancel" do
            decodePickerKey "\n" `shouldBe` Just PickerConfirm
            decodePickerKey "\r" `shouldBe` Just PickerConfirm
            decodePickerKey "\ESC" `shouldBe` Just PickerCancel
            decodePickerKey "q" `shouldBe` Just PickerCancel

        it "maps filter editing" do
            decodePickerKey "\DEL" `shouldBe` Just PickerBackspace
            decodePickerKey "g" `shouldBe` Just (PickerType 'g')

    describe "renderPickerFrame" do
        it "mentions all providers, current model, and controls" do
            let frame =
                    renderPickerFrame False $
                        initialPickerState XAIProvider (defaultModelFor XAIProvider)
            frame `shouldSatisfy` Text.isInfixOf "xai"
            frame `shouldSatisfy` Text.isInfixOf "openai"
            frame `shouldSatisfy` Text.isInfixOf "openrouter"
            frame `shouldSatisfy` Text.isInfixOf "claude-code"
            frame `shouldSatisfy` Text.isInfixOf (defaultModelFor XAIProvider)
            frame `shouldSatisfy` Text.isInfixOf "enter"
            frame `shouldSatisfy` Text.isInfixOf "filter"

    describe "formatCatalogListing" do
        it "lists the current model and entries from every provider" do
            let listing =
                    formatCatalogListing False OpenAIProvider "gpt-5.6-luna"
            listing `shouldSatisfy` Text.isInfixOf "gpt-5.6-luna"
            listing `shouldSatisfy` Text.isInfixOf "gpt-5.6-terra"
            listing `shouldSatisfy` Text.isInfixOf "gpt-5.6-sol"
            listing `shouldSatisfy` Text.isInfixOf "openai"
            listing `shouldSatisfy` Text.isInfixOf "grok-4.6"
            listing `shouldSatisfy` Text.isInfixOf "openrouter"
            listing `shouldSatisfy` Text.isInfixOf "claude-code"
