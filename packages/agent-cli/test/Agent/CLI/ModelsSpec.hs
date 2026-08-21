module Agent.CLI.ModelsSpec (spec) where

import Agent.CLI.Models
import Agent.CLI.Prompt (defaultModelFor)
import Agent.Provider (Provider(..))
import Data.List (nub)
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

        it "lists Thinking Machines models for OpenRouter" do
            let ids = map (.modelId) (modelsForProvider OpenRouterProvider)
            ids `shouldContain`
                [ "thinkingmachines/inkling:free"
                , "thinkingmachines/inkling-small:free"
                ]

        it "lists several options per provider" do
            length (modelsForProvider XAIProvider) `shouldSatisfy` (>= 2)
            length (modelsForProvider OpenAIProvider) `shouldSatisfy` (>= 2)
            length (modelsForProvider OpenRouterProvider) `shouldSatisfy` (>= 2)

        it "tags every option with its provider" do
            all ((== OpenAIProvider) . (.modelProvider))
                (modelsForProvider OpenAIProvider)
                `shouldBe` True

    describe "modelCatalog" do
        it "includes every provider" do
            nub (map (.modelProvider) modelCatalog)
                `shouldMatchList`
                    [OpenAIProvider, XAIProvider, OpenRouterProvider]

    describe "ensureCurrentInList" do
        it "prepends an unknown current model" do
            let base = modelsForProvider XAIProvider
                opts = ensureCurrentInList XAIProvider "custom-model" base
            firstId opts `shouldBe` Just "custom-model"
            fmap (.modelLabel) (listToMaybe opts) `shouldBe` Just (Just "current")
            fmap (.modelProvider) (listToMaybe opts) `shouldBe` Just XAIProvider

        it "does not duplicate a known current model" do
            let base = modelsForProvider XAIProvider
                def = defaultModelFor XAIProvider
                opts = ensureCurrentInList XAIProvider def base
            length (filter (\opt -> opt.modelId == def) opts) `shouldBe` 1

        it "ignores unset placeholders" do
            let option = ModelOption XAIProvider "a" Nothing
            ensureCurrentInList XAIProvider "(unset)" [option]
                `shouldBe` [option]

    describe "picker navigation" do
        let state0 = initialPickerState XAIProvider (defaultModelFor XAIProvider)

        it "starts on the current model" do
            fmap (\option -> (option.modelProvider, option.modelId))
                (selectedOption state0)
                `shouldBe` Just (XAIProvider, defaultModelFor XAIProvider)

        it "shows models from every provider" do
            nub (map (.modelProvider) (visibleOptions state0))
                `shouldMatchList`
                    [OpenAIProvider, XAIProvider, OpenRouterProvider]

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
                `shouldBe` Left (Just ModelOption
                    { modelProvider = XAIProvider
                    , modelId = defaultModelFor XAIProvider
                    , modelLabel = Just "default"
                    })

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

        it "filters by provider" do
            let typed =
                    foldl
                        (\s c -> case applyPickerEvent (PickerType c) s of
                            Right s' -> s'
                            Left _ -> s)
                        state0
                        ("openrouter" :: String)
            visibleOptions typed `shouldSatisfy` (not . null)
            all ((== OpenRouterProvider) . (.modelProvider)) (visibleOptions typed)
                `shouldBe` True
