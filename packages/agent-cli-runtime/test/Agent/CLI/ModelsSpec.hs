{-# LANGUAGE OverloadedStrings #-}

module Agent.CLI.ModelsSpec (spec) where

import Agent.CLI.Models
import Agent.CLI.ModelConfig
    ( CatalogModel(..)
    , ModelCatalog(..)
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
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as Text
import System.Environment (lookupEnv, setEnv, unsetEnv)
import Test.Hspec

firstId :: [ModelOption] -> Maybe Text.Text
firstId = fmap (.modelTarget.targetModelId) . listToMaybe

spec :: Spec
spec = do
    catalog <- runIO readPackagedCatalog
    let modelIdsFor provider =
            map (.modelTarget.targetModelId)
                (modelsForProvider catalog provider)
    describe "modelsForProvider" do
        it "puts the provider default first" do
            firstId (modelsForProvider catalog XAIProvider)
                `shouldBe` defaultModelFor catalog XAIProvider
            firstId (modelsForProvider catalog OpenAIProvider)
                `shouldBe` defaultModelFor catalog OpenAIProvider
            firstId (modelsForProvider catalog OpenRouterProvider)
                `shouldBe` defaultModelFor catalog OpenRouterProvider
            firstId (modelsForProvider catalog GeminiProvider)
                `shouldBe` defaultModelFor catalog GeminiProvider
            firstId (modelsForProvider catalog ClaudeCodeProvider)
                `shouldBe` defaultModelFor catalog ClaudeCodeProvider

        it "ships the configured frontier models for each provider" do
            modelIdsFor OpenAIProvider `shouldContain` ["gpt-5.6-sol"]
            modelIdsFor XAIProvider `shouldContain` ["grok-4.6"]
            modelIdsFor OpenRouterProvider `shouldContain` ["stealth/ox-alpha"]
            modelIdsFor GeminiProvider `shouldContain` ["gemini-3.7-flash"]

        it "ships the GPT-5.6 series and each other provider frontier model" do
            modelIdsFor OpenAIProvider
                `shouldBe`
                    [ "gpt-5.6-sol"
                    , "gpt-5.6-terra"
                    , "gpt-5.6-luna"
                    ]
            modelIdsFor XAIProvider `shouldBe` ["grok-4.6"]
            modelIdsFor OpenRouterProvider
                `shouldBe` ["stealth/ox-alpha", "meta/muse-spark-1.2"]
            modelIdsFor GeminiProvider
                `shouldBe`
                    [ "gemini-3.7-flash"
                    , "gemini-3.1-pro-preview"
                    , "gemini-3.5-flash-lite"
                    ]
            modelIdsFor ClaudeCodeProvider
                `shouldBe` ["sonnet", "opus", "fable"]

        it "tags every option with its provider" do
            all ((== OpenAIProvider) . (.modelTarget.targetProvider))
                (modelsForProvider catalog OpenAIProvider)
                `shouldBe` True

        it "assigns the OpenRouter frontier model its portable dialect" do
            let options = modelsForProvider catalog OpenRouterProvider
                dialectFor model =
                    (.modelTarget.targetDialect)
                        <$> find
                            ((== model) . (.modelTarget.targetModelId))
                            options
            dialectFor "stealth/ox-alpha"
                `shouldBe` Just GenericResponsesDialect

        it "assigns the owned-tool dialect to every Claude Code model" do
            map (.modelTarget.targetDialect)
                (modelsForProvider catalog ClaudeCodeProvider)
                `shouldSatisfy` all (== ClaudeCodeDialect)

        it "keeps structured context metadata on picker options" do
            let grok =
                    find
                        ((== "grok-4.6") . (.modelTarget.targetModelId))
                        (modelsForProvider catalog XAIProvider)
            fmap (.modelContextWindow) grok `shouldBe` Just (Just 500000)

        it "keeps every catalog dialect consistent with model inference" do
            all
                (\option ->
                    option.modelTarget.targetDialect
                        == dialectIdForModel
                            option.modelTarget.targetProvider
                            option.modelTarget.targetModelId)
                (modelCatalog catalog)
                `shouldBe` True

    describe "modelCatalog" do
        it "includes every provider" do
            nub (map (.modelTarget.targetProvider) (modelCatalog catalog))
                `shouldMatchList`
                    [ OpenAIProvider
                    , XAIProvider
                    , OpenRouterProvider
                    , GeminiProvider
                    , ClaudeCodeProvider
                    ]

        it "merges live models without replacing configured metadata" do
            let duplicate =
                    (rawModelOption OpenRouterProvider "stealth/ox-alpha")
                        { modelLabel = Just "live duplicate" }
                discovered =
                    (rawModelOption OpenRouterProvider "qwen/example-new")
                        { modelLabel = Just "OpenRouter live" }
            state <-
                initialPickerStateResolvedWith
                    catalog
                    [duplicate, discovered]
                    "openrouter"
                    OpenRouterProvider
                    "stealth/ox-alpha"
                    GenericResponsesDialect
            let matching model =
                    filter
                        ((== model) . (.modelTarget.targetModelId))
                        state.pickerAll
            length (matching "stealth/ox-alpha") `shouldBe` 1
            fmap (.modelLabel) (listToMaybe (matching "stealth/ox-alpha"))
                `shouldBe`
                    Just
                        (Just
                            "default · frontier · free · coding · 1M context")
            fmap (.modelLabel) (listToMaybe (matching "qwen/example-new"))
                `shouldBe` Just (Just "OpenRouter live")

    describe "gatewayModelOptions" do
        it "defers resumed custom connections to the gateway catalog" do
            let expected = ModelTarget
                    OpenRouterProvider
                    "removed-custom"
                    "company-private"
                    "upstream-private"
                    GenericResponsesDialect
                resolve =
                    resolveSavedModelTarget
                        catalog
                        True
                        OpenRouterProvider
                        "removed-custom"
                        "company-private"
                        (Just "upstream-private")
                        GenericResponsesDialect
            resolve `shouldBe` Right expected
            resolveSavedModelTarget
                catalog
                False
                OpenRouterProvider
                "removed-custom"
                "company-private"
                (Just "upstream-private")
                GenericResponsesDialect
                `shouldBe`
                    Left
                        "saved model removed-custom/company-private is not present in ~/.haskell-agent/models.json"

        it "uses only advertised aliases and pins them to the gateway" do
            let options =
                    gatewayModelOptions
                        catalog
                        "openai"
                        OpenAIProvider
                        [ " gpt-5.6-sol "
                        , "company-private"
                        , "gpt-5.6-sol"
                        , ""
                        ]
            map (.modelTarget.targetModelId) options
                `shouldBe` ["gpt-5.6-sol", "company-private"]
            all
                (\option ->
                    let target = option.modelTarget
                    in target.targetProvider == OpenAIProvider
                        && target.targetConnectionId == "openai"
                        && target.targetWireModelId == target.targetModelId)
                options
                `shouldBe` True

        it "pins mapped aliases to the gateway while retaining safe metadata" do
            let known =
                    CatalogModel
                        { catalogModelId = "company-known"
                        , catalogModelConnectionId = "openai"
                        , catalogModelWireId = "upstream-mapped-known"
                        , catalogModelDialect = GenericResponsesDialect
                        , catalogModelContextWindow = Nothing
                        , catalogModelLabel = Just "company label"
                        , catalogModelDefault = False
                        , catalogModelFallbackPriority = Just 9
                        }
                conflicting =
                    known
                        { catalogModelId = "company-foreign"
                        , catalogModelConnectionId = "xai"
                        , catalogModelWireId = "upstream-mapped-foreign"
                        , catalogModelDialect = GrokBuildDialect
                        , catalogModelLabel = Just "foreign label"
                        }
                mappedCatalog =
                    catalog
                        { catalogModels =
                            known : conflicting : catalog.catalogModels
                        , catalogModelsById =
                            Map.insert
                                known.catalogModelId
                                known
                                (Map.insert
                                    conflicting.catalogModelId
                                    conflicting
                                    catalog.catalogModelsById)
                        }
            case
                gatewayModelOptions
                    mappedCatalog
                    "openai"
                    OpenAIProvider
                    ["company-known", "company-foreign"]
                of
                [active, foreignOption] -> do
                    active.modelTarget
                        `shouldBe`
                            ModelTarget
                                OpenAIProvider
                                "openai"
                                "company-known"
                                "company-known"
                                GenericResponsesDialect
                    active.modelLabel `shouldBe` Just "company label"
                    active.modelFallbackPriority `shouldBe` Just 9
                    foreignOption.modelTarget
                        `shouldBe`
                            ModelTarget
                                OpenAIProvider
                                "openai"
                                "company-foreign"
                                "company-foreign"
                                (dialectIdForModel
                                    OpenAIProvider
                                    "company-foreign")
                    foreignOption.modelLabel `shouldBe` Nothing
                    foreignOption.modelFallbackPriority `shouldBe` Nothing
                other ->
                    expectationFailure $
                        "expected two gateway options, got " <> show other

        it "does not reinsert a stale current model into an authoritative picker" do
            let options =
                    gatewayModelOptions
                        catalog
                        "openai"
                        OpenAIProvider
                        ["gpt-5.6-terra"]
            state <-
                initialPickerStateForOptions
                    "organization gateway"
                    options
                    "openai"
                    OpenAIProvider
                    "revoked-model"
                    CodexDialect
            state.pickerScopeLabel `shouldBe` "organization gateway"
            map (.modelTarget.targetModelId) state.pickerAll
                `shouldBe` ["gpt-5.6-terra"]

    describe "modelTargetRequiresRebuild" do
        let sameDialect =
                testOption OpenRouterProvider "openrouter"
                    "openai/gpt-5.2" "openai/gpt-5.2" CodexDialect
                    Nothing Nothing

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
                        resolveModelOptionDialect $
                            testOption OpenRouterProvider "openrouter"
                                "friendly" "friendly" CodexDialect Nothing Nothing
                    resolved.modelTarget.targetDialect
                        `shouldBe` GenericResponsesDialect
                    resolved.modelTarget.targetWireModelId
                        `shouldBe` "anthropic/claude-sonnet-4"

        it "keeps direct providers on their configured dialect" do
            resolved <-
                resolveModelOptionDialect $
                    testOption XAIProvider "xai"
                        "openai/gpt-5.1" "openai/gpt-5.1"
                        CodexDialect Nothing Nothing
            resolved.modelTarget.targetDialect `shouldBe` CodexDialect

    describe "resolvePersistedDialect" do
        let option =
                testOption OpenRouterProvider "openrouter"
                    "friendly" "x-ai/grok-4" GrokBuildDialect Nothing Nothing
            inferred = option.modelTarget

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
            fmap (.modelTarget.targetProvider) (listToMaybe opts)
                `shouldBe` Just XAIProvider
            fmap (.modelTarget.targetDialect) (listToMaybe opts)
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
            length
                (filter (\opt -> opt.modelTarget.targetModelId == def) opts)
                `shouldBe` 1

        it "preserves a legacy OpenRouter dialect for a removed model" do
            let model = "openai/gpt-5.1"
                opts =
                    ensureCurrentInList
                        "openrouter"
                        OpenRouterProvider
                        model
                        GrokBuildDialect
                        (modelsForProvider catalog OpenRouterProvider)
            fmap (.modelTarget.targetDialect) (listToMaybe opts)
                `shouldBe` Just GrokBuildDialect
            length
                (filter ((== model) . (.modelTarget.targetModelId)) opts)
                `shouldBe` 1

        it "ignores unset placeholders" do
            let option =
                    testOption XAIProvider "xai" "a" "a"
                        GrokBuildDialect Nothing Nothing
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
                    ( option.modelTarget.targetProvider
                    , option.modelTarget.targetModelId
                    , option.modelTarget.targetDialect
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
            fmap (.modelTarget.targetDialect) (selectedOption state)
                `shouldBe` Just GrokBuildDialect

        it "shows models from every provider" do
            nub (map (.modelTarget.targetProvider) (visibleOptions state0))
                `shouldMatchList`
                    [ OpenAIProvider
                    , XAIProvider
                    , OpenRouterProvider
                    , GeminiProvider
                    , ClaudeCodeProvider
                    ]

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
                `shouldBe` Left (Just
                    ((testOption XAIProvider "xai"
                        "grok-4.6" "grok-4.6" GrokBuildDialect
                        (Just "default") (Just 10))
                        { modelContextWindow = Just 500000 }))

        it "cancels without a selection" do
            applyPickerEvent PickerCancel state0 `shouldBe` Left Nothing

        it "filters by typed characters" do
            let typed =
                    foldl
                        (\s c -> case applyPickerEvent (PickerType c) s of
                            Right s' -> s'
                            Left _ -> s)
                        state0
                        ("frontier" :: String)
            not (null (visibleOptions typed)) `shouldBe` True
            all
                (\opt ->
                    "frontier" `Text.isInfixOf`
                        Text.toLower opt.modelTarget.targetModelId
                    || maybe
                        False
                        (("frontier" `Text.isInfixOf`) . Text.toLower)
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
            all
                ((== OpenRouterProvider) . (.modelTarget.targetProvider))
                (visibleOptions typed)
                `shouldBe` True

testOption
    :: Provider
    -> Text.Text
    -> Text.Text
    -> Text.Text
    -> DialectId
    -> Maybe Text.Text
    -> Maybe Int
    -> ModelOption
testOption provider connection model wireModel dialect label priority =
    ModelOption
        { modelTarget = ModelTarget
            { targetProvider = provider
            , targetConnectionId = connection
            , targetModelId = model
            , targetWireModelId = wireModel
            , targetDialect = dialect
            }
        , modelContextWindow = Nothing
        , modelLabel = label
        , modelFallbackPriority = priority
        }

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
