module Agent.CLI.ModelsSpec (spec) where

import Agent.CLI.Models
import Agent.CLI.ModelConfig
    ( ModelCatalog
    , decodeModelConfig
    , packagedModelCatalogPath
    )
import Agent.Dialect
    ( DialectId(..)
    , dialectIdForModel
    )
import Agent.Provider (Provider(..))
import Control.Exception.Safe (bracket)
import qualified Data.ByteString.Lazy as LBS
import Data.List (find, nub)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as Text
import System.Environment (lookupEnv, setEnv, unsetEnv)
import Test.Hspec

firstId :: [ModelOption] -> Maybe Text.Text
firstId = fmap (.modelId) . listToMaybe

spec :: Spec
spec = do
    catalog <- runIO readPackagedCatalog
    describe "modelsForProvider" do
        it "puts the provider default first" do
            firstId (modelsForProvider catalog XAIProvider)
                `shouldBe` defaultModelFor catalog XAIProvider
            firstId (modelsForProvider catalog OpenAIProvider)
                `shouldBe` defaultModelFor catalog OpenAIProvider
            firstId (modelsForProvider catalog OpenRouterProvider)
                `shouldBe` defaultModelFor catalog OpenRouterProvider


        it "lists the gpt-5.6 series for OpenAI" do
            let ids = map (.modelId) (modelsForProvider catalog OpenAIProvider)
            ids `shouldBe` ["gpt-5.6-luna", "gpt-5.6-terra", "gpt-5.6-sol"]

        it "lists Ox Alpha instead of Thinking Machines models for OpenRouter" do
            let ids = map (.modelId) (modelsForProvider catalog OpenRouterProvider)
            ids `shouldContain` ["stealth/ox-alpha"]
            ids `shouldSatisfy`
                all (not . Text.isPrefixOf "thinkingmachines/")

        it "lists several options per provider" do
            length (modelsForProvider catalog XAIProvider) `shouldSatisfy` (>= 2)
            length (modelsForProvider catalog OpenAIProvider) `shouldSatisfy` (>= 2)
            length (modelsForProvider catalog OpenRouterProvider) `shouldSatisfy` (>= 2)

        it "tags every option with its provider" do
            all ((== OpenAIProvider) . (.modelProvider))
                (modelsForProvider catalog OpenAIProvider)
                `shouldBe` True

        it "assigns OpenRouter dialects by model family" do
            let options = modelsForProvider catalog OpenRouterProvider
                dialectFor model =
                    (.modelDialect) <$> find ((== model) . (.modelId)) options
            dialectFor "openai/gpt-5.1" `shouldBe` Just CodexDialect
            dialectFor "x-ai/grok-4" `shouldBe` Just GrokBuildDialect
            dialectFor "anthropic/claude-sonnet-4"
                `shouldBe` Just GenericResponsesDialect
            dialectFor "google/gemini-2.5-pro"
                `shouldBe` Just GenericResponsesDialect

        it "keeps every catalog dialect consistent with model inference" do
            all
                (\option ->
                    option.modelDialect
                        == dialectIdForModel
                            option.modelProvider
                            option.modelId)
                (modelCatalog catalog)
                `shouldBe` True

    describe "modelCatalog" do
        it "includes every provider" do
            nub (map (.modelProvider) (modelCatalog catalog))
                `shouldMatchList`
                    [OpenAIProvider, XAIProvider, OpenRouterProvider]

    describe "modelTargetRequiresRebuild" do
        let sameDialect = ModelOption
                { modelConnectionId = "openrouter"
                , modelProvider = OpenRouterProvider
                , modelId = "openai/gpt-5.2"
                , modelTransportId = "openai/gpt-5.2"
                , modelDialect = CodexDialect
                , modelLabel = Nothing
                , modelFallbackPriority = Nothing
                }

        it "updates a model in place within the current provider and dialect" do
            modelTargetRequiresRebuild
                "openrouter" OpenRouterProvider CodexDialect sameDialect
                `shouldBe` False

        it "rebuilds when only the dialect changes" do
            modelTargetRequiresRebuild
                "openrouter"
                OpenRouterProvider
                GrokBuildDialect
                sameDialect
                `shouldBe` True

        it "rebuilds when the provider changes" do
            modelTargetRequiresRebuild
                "xai"
                XAIProvider
                CodexDialect
                sameDialect
                `shouldBe` True

    describe "resolveModelOptionDialect" do
        it "uses the final OpenRouter model after transport overrides" do
            withEnv
                "OPENROUTER_MODEL_MAP"
                (Just "friendly=anthropic/claude-sonnet-4") do
                    resolved <-
                        resolveModelOptionDialect ModelOption
                            { modelConnectionId = "openrouter"
                            , modelProvider = OpenRouterProvider
                            , modelId = "friendly"
                            , modelTransportId = "friendly"
                            , modelDialect = CodexDialect
                            , modelLabel = Nothing
                            , modelFallbackPriority = Nothing
                            }
                    resolved.modelDialect
                        `shouldBe` GenericResponsesDialect
                    resolved.modelTransportId
                        `shouldBe` "anthropic/claude-sonnet-4"

        it "keeps direct providers on their configured dialect" do
            resolved <-
                resolveModelOptionDialect ModelOption
                    { modelConnectionId = "xai"
                    , modelProvider = XAIProvider
                    , modelId = "openai/gpt-5.1"
                    , modelTransportId = "openai/gpt-5.1"
                    , modelDialect = CodexDialect
                    , modelLabel = Nothing
                    , modelFallbackPriority = Nothing
                    }
            resolved.modelDialect `shouldBe` CodexDialect

    describe "resolvePersistedDialect" do
        let inferred = ModelOption
                { modelConnectionId = "openrouter"
                , modelProvider = OpenRouterProvider
                , modelId = "friendly"
                , modelTransportId = "x-ai/grok-4"
                , modelDialect = GrokBuildDialect
                , modelLabel = Nothing
                , modelFallbackPriority = Nothing
                }

        it "retargets when a recorded OpenRouter mapping changes" do
            resolvePersistedDialect
                CodexDialect
                (Just "openai/gpt-5.1")
                inferred
                `shouldBe` (GrokBuildDialect, True)

        it "preserves an explicit dialect while the mapping is unchanged" do
            resolvePersistedDialect
                CodexDialect
                (Just "x-ai/grok-4")
                inferred
                `shouldBe` (CodexDialect, False)

        it "preserves legacy records without an effective model" do
            resolvePersistedDialect
                GrokBuildDialect
                Nothing
                inferred
                `shouldBe` (GrokBuildDialect, False)

    describe "ensureCurrentInList" do
        it "prepends an unknown current model" do
            let base = modelsForProvider catalog XAIProvider
                opts =
                    ensureCurrentInList
                        "xai"
                        XAIProvider
                        "custom-model"
                        GrokBuildDialect
                        base
            firstId opts `shouldBe` Just "custom-model"
            fmap (.modelLabel) (listToMaybe opts) `shouldBe` Just (Just "current")
            fmap (.modelProvider) (listToMaybe opts) `shouldBe` Just XAIProvider
            fmap (.modelDialect) (listToMaybe opts)
                `shouldBe` Just GrokBuildDialect

        it "does not duplicate a known current model" do
            let base = modelsForProvider catalog XAIProvider
                def = fromMaybe
                    (error "shipped xAI default is missing")
                    (defaultModelFor catalog XAIProvider)
                opts =
                    ensureCurrentInList
                        "xai"
                        XAIProvider
                        def
                        GrokBuildDialect
                        base
            length (filter (\opt -> opt.modelId == def) opts) `shouldBe` 1

        it "preserves a legacy OpenRouter dialect for a known model" do
            let model = "openai/gpt-5.1"
                opts =
                    ensureCurrentInList
                        "openrouter"
                        OpenRouterProvider
                        model
                        GrokBuildDialect
                        (modelsForProvider catalog OpenRouterProvider)
            fmap (.modelDialect) (listToMaybe opts)
                `shouldBe` Just GrokBuildDialect
            length (filter ((== model) . (.modelId)) opts) `shouldBe` 2

        it "ignores unset placeholders" do
            let option =
                    ModelOption
                        "xai"
                        XAIProvider
                        "a"
                        "a"
                        GrokBuildDialect
                        Nothing
                        Nothing
            ensureCurrentInList
                "xai"
                XAIProvider
                "(unset)"
                GrokBuildDialect
                [option]
                `shouldBe` [option]

    describe "picker navigation" do
        let state0 =
                initialPickerState
                    catalog
                    "xai"
                    XAIProvider
                    "grok-4.6"
                    GrokBuildDialect

        it "starts on the current model" do
            fmap
                (\option ->
                    ( option.modelProvider
                    , option.modelId
                    , option.modelDialect
                    ))
                (selectedOption state0)
                `shouldBe`
                    Just
                        ( XAIProvider
                        , "grok-4.6"
                        , GrokBuildDialect
                        )

        it "starts on a legacy dialect rather than the catalog replacement" do
            let state =
                    initialPickerState
                        catalog
                        "openrouter"
                        OpenRouterProvider
                        "openai/gpt-5.1"
                        GrokBuildDialect
            fmap (.modelDialect) (selectedOption state)
                `shouldBe` Just GrokBuildDialect

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
                    { modelConnectionId = "xai"
                    , modelProvider = XAIProvider
                    , modelId = "grok-4.6"
                    , modelTransportId = "grok-4.6"
                    , modelDialect = GrokBuildDialect
                    , modelLabel = Just "default"
                    , modelFallbackPriority = Just 10
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

withEnv :: String -> Maybe String -> IO a -> IO a
withEnv name value action =
    bracket
        (do
            previous <- lookupEnv name
            set value
            pure previous)
        set
        (const action)
  where
    set = \case
        Just current -> setEnv name current
        Nothing -> unsetEnv name

readPackagedCatalog :: IO ModelCatalog
readPackagedCatalog = do
    path <- packagedModelCatalogPath
    bytes <- LBS.readFile path
    case decodeModelConfig "models.default.json" bytes of
        Left err -> fail (Text.unpack err)
        Right catalog -> pure catalog
