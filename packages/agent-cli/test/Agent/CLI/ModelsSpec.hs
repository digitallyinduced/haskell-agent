module Agent.CLI.ModelsSpec (spec) where

import Agent.CLI.Models
import Agent.CLI.Prompt (defaultModelFor)
import Agent.Provider (Provider(..))
import Data.Maybe (listToMaybe)
import qualified Data.Text as Text
import Test.Hspec

firstId :: [ModelOption] -> Maybe Text.Text
firstId = fmap (.modelId) . listToMaybe

spec :: Spec
spec = do
    describe "modelsForProvider" do
        it "puts the provider default first" do
            firstId (modelsForProvider XAIProvider)
                `shouldBe` Just (defaultModelFor XAIProvider)
            firstId (modelsForProvider OpenAIProvider)
                `shouldBe` Just (defaultModelFor OpenAIProvider)
            firstId (modelsForProvider OpenRouterProvider)
                `shouldBe` Just (defaultModelFor OpenRouterProvider)


        it "lists the gpt-5.6 series for OpenAI" do
            let ids = map (.modelId) (modelsForProvider OpenAIProvider)
            ids `shouldBe` ["gpt-5.6-luna", "gpt-5.6-terra", "gpt-5.6-sol"]
        it "lists several options per provider" do
            length (modelsForProvider XAIProvider) `shouldSatisfy` (>= 2)
            length (modelsForProvider OpenAIProvider) `shouldSatisfy` (>= 2)
            length (modelsForProvider OpenRouterProvider) `shouldSatisfy` (>= 2)

    describe "ensureCurrentInList" do
        it "prepends an unknown current model" do
            let base = modelsForProvider XAIProvider
                opts = ensureCurrentInList "custom-model" base
            firstId opts `shouldBe` Just "custom-model"
            fmap (.modelLabel) (listToMaybe opts) `shouldBe` Just (Just "current")

        it "does not duplicate a known current model" do
            let base = modelsForProvider XAIProvider
                def = defaultModelFor XAIProvider
                opts = ensureCurrentInList def base
            length (filter (\opt -> opt.modelId == def) opts) `shouldBe` 1

        it "ignores unset placeholders" do
            ensureCurrentInList "(unset)" [ModelOption "a" Nothing]
                `shouldBe` [ModelOption "a" Nothing]

    describe "picker navigation" do
        let state0 = initialPickerState XAIProvider (defaultModelFor XAIProvider)

        it "starts on the current model" do
            fmap (.modelId) (selectedOption state0)
                `shouldBe` Just (defaultModelFor XAIProvider)

        it "moves down and wraps" do
            let n = length (visibleOptions state0)
                down = case applyPickerEvent PickerDown state0 of
                    Right s -> s
                    Left _ -> state0
                walked = foldl
                    (\s _ -> case applyPickerEvent PickerDown s of
                        Right s' -> s'
                        Left _ -> s)
                    state0
                    [1 .. n]
            down.pickerIndex `shouldBe` 1 `mod` max 1 n
            walked.pickerIndex `shouldBe` state0.pickerIndex

        it "confirms the selected model id" do
            applyPickerEvent PickerConfirm state0
                `shouldBe` Left (Just (defaultModelFor XAIProvider))

        it "cancels without a selection" do
            applyPickerEvent PickerCancel state0 `shouldBe` Left Nothing

        it "filters by typed characters" do
            let typed =
                    foldl
                        (\s c -> case applyPickerEvent (PickerType c) s of
                            Right s' -> s'
                            Left _ -> s)
                        state0
                        ("mini" :: String)
            not (null (visibleOptions typed)) `shouldBe` True
            all
                (\opt -> "mini" `Text.isInfixOf` Text.toLower opt.modelId
                    || maybe
                        False
                        (("mini" `Text.isInfixOf`) . Text.toLower)
                        opt.modelLabel)
                (visibleOptions typed)
                `shouldBe` True
