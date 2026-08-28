module Agent.OpenAI.ModelsTypesSpec (spec) where

import qualified Agent.Json.Decode as Json
import Agent.OpenAI.Models
import qualified Agent.Tools.CodeMode.Tool as CoreTools
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Test.Hspec

spec :: Spec
spec = do
    describe "bundled Codex model catalog" do
        it "loads the current upstream catalog and selects Sol by priority" do
            catalog <- loadBundledModelsOrThrow
            length catalog.models `shouldBe` 10
            defaultModelForCatalog True catalog
                `shouldBe` Just "gpt-5.6-sol"
            catalog.catalogGeneration `shouldBe` Nothing
            decodeModelsOrFail (Aeson.toJSON catalog) `shouldBe` catalog

        it "serializes legacy base instructions for older Codex clients" do
            catalog <- loadBundledModelsOrThrow
            case Aeson.toJSON catalog of
                Aeson.Object response ->
                    case KeyMap.lookup "models" response of
                        Just (Aeson.Array models) ->
                            all hasLegacyBaseInstructions (Vector.toList models)
                                `shouldBe` True
                        _ -> expectationFailure "models must be an array"
                _ -> expectationFailure "catalog must be an object"

        it "keeps hidden Daybreak models for lookup but excludes them from the picker" do
            catalog <- loadBundledModelsOrThrow
            map (.slug) catalog.models `shouldContain`
                [ "gpt-daybreak-blue-latest"
                , "gpt-daybreak-red-latest"
                ]
            let pickerModels = map (.model) (pickerModelPresets True catalog)
            pickerModels `shouldBe`
                [ "gpt-5.6-sol"
                , "gpt-5.6-terra"
                , "gpt-5.6-luna"
                , "gpt-5.5"
                , "gpt-5.2"
                ]

        it "exposes current 5.6 dialect selectors and reasoning defaults" do
            catalog <- loadBundledModelsOrThrow
            let sol = modelInfoForSlug "gpt-5.6-sol" catalog
                terra = modelInfoForSlug "gpt-5.6-terra" catalog
                luna = modelInfoForSlug "gpt-5.6-luna" catalog
            sol.useResponsesLite `shouldBe` True
            toolModeForInfo CoreTools.ConventionalToolMode sol
                `shouldBe` CoreTools.CodeOnlyToolMode
            sol.multiAgentVersion `shouldBe` Just MultiAgentV2
            defaultReasoningEffortForInfo sol
                `shouldBe` Just ReasoningEffortLow
            defaultVerbosityForInfo sol `shouldBe` Just VerbosityLow
            defaultVerbosityForInfo (sol { supportVerbosity = False })
                `shouldBe` Nothing
            modelSupportsReasoningEffort sol ReasoningEffortUltra
                `shouldBe` True
            terra.defaultReasoningLevel `shouldBe` Just ReasoningEffortMedium
            terra.multiAgentVersion `shouldBe` Just MultiAgentV2
            modelPresetSupportsFastMode (modelPresetFromInfo terra)
                `shouldBe` True
            modelServiceTierForRequest terra (Just "priority")
                `shouldBe` Just "priority"
            modelServiceTierForRequest terra (Just "default")
                `shouldBe` Nothing
            luna.defaultReasoningLevel `shouldBe` Just ReasoningEffortMedium
            luna.multiAgentVersion `shouldBe` Just MultiAgentV1
            modelSupportsReasoningEffort luna ReasoningEffortUltra
                `shouldBe` False
            let legacy = modelInfoForSlug "gpt-5.5" catalog
            toolModeForInfo CoreTools.CodeToolMode legacy
                `shouldBe` CoreTools.CodeToolMode
            modelPresetForInfo sol `shouldBe` modelPresetFromInfo sol
            defaultReasoningSummaryForInfo sol
                `shouldBe` Just ReasoningSummaryNone
            defaultReasoningSummaryForInfo
                (sol { supportsReasoningSummaryParameter = False })
                `shouldBe` Nothing

        it "marks unknown models as fallback metadata with upstream instructions" do
            let unknown = fallbackModelInfo "future-unknown"
            unknown.usedFallbackModelMetadata `shouldBe` True
            renderModelInstructions ModelPersonalityDefault unknown
                `shouldBe` fallbackModelInstructions
            Text.length fallbackModelInstructions
                `shouldBe` 20_751
            fallbackModelInstructions `shouldSatisfy`
                Text.isPrefixOf
                    "You are a coding agent running in the Codex CLI"
            fallbackModelInstructions `shouldSatisfy`
                Text.isInfixOf "# How you work"
            fallbackModelInstructions `shouldSatisfy`
                Text.isInfixOf "# AGENTS.md spec"

        it "projects upgrade metadata into picker semantics" do
            catalog <- loadBundledModelsOrThrow
            let preset = modelPresetFromInfo
                    (modelInfoForSlug "gpt-5.4" catalog)
            preset.upgrade `shouldSatisfy` \case
                Just upgrade ->
                    upgrade.upgradeId == "gpt-5.6-terra"
                        && upgrade.migrationConfigKey == "gpt-5.4"
                Nothing -> False

    describe "known-field decoding" do
        it "ignores unknown model fields and preserves unknown enum values" do
            let encoded = Aeson.object
                    [ "models" Aeson..=
                        [ Aeson.object
                            [ "slug" Aeson..= ("future-model" :: String)
                            , "display_name" Aeson..= ("Future" :: String)
                            , "shell_type" Aeson..= ("quantum_shell" :: String)
                            , "tool_mode" Aeson..= ("future_mode" :: String)
                            , "visibility" Aeson..= ("preview" :: String)
                            , "minimal_client_version" Aeson..= Aeson.object
                                ["major" Aeson..= (99 :: Int)]
                            , "model_messages" Aeson..= Aeson.object
                                [ "instructions_template" Aeson..=
                                    ("future prompt" :: String)
                                , "guardian_v2" Aeson..= Aeson.object
                                    ["enabled" Aeson..= True]
                                ]
                            , "future_capability" Aeson..= Aeson.object
                                ["enabled" Aeson..= True]
                            ]
                        ]
                    , "catalog_generation" Aeson..= (42 :: Int)
                    ]
                decoded = decodeModelsOrFail encoded
                model = modelInfoForSlug "future-model" decoded
                roundtripped =
                    decodeModelsOrFail (Aeson.toJSON decoded)
                roundtrippedModel =
                    modelInfoForSlug "future-model" roundtripped
                encodedModel = Aeson.toJSON model
            model.shellType `shouldBe` ShellToolOther "quantum_shell"
            model.toolMode `shouldBe` Just (ToolModeOther "future_mode")
            model.visibility `shouldBe` ModelVisibilityOther "preview"
            jsonObjectField "future_capability" encodedModel
                `shouldBe` Nothing
            jsonObjectField "minimal_client_version" encodedModel
                `shouldBe` Nothing
            (jsonObjectField "model_messages" encodedModel
                >>= jsonObjectField "guardian_v2")
                `shouldBe` Nothing
            roundtrippedModel.shellType
                `shouldBe` ShellToolOther "quantum_shell"
            decoded.catalogGeneration `shouldBe` Just 42
            roundtripped.catalogGeneration `shouldBe` Just 42

        it "accepts legacy shell aliases as unified exec" do
            map
                (\shell -> (.shellType) $ decodeModelOrFail $ Aeson.object
                    [ "slug" Aeson..= ("alias" :: String)
                    , "shell_type" Aeson..= shell
                    , "base_instructions" Aeson..= ("prompt" :: String)
                    ])
                (["default", "local", "shell_command", "unified_exec"] :: [Text.Text])
                `shouldBe` replicate 4 ShellToolUnifiedExec

        it "promotes legacy base instructions into an incomplete messages object" do
            let model = decodeModelOrFail (Aeson.object
                    [ "slug" Aeson..= ("legacy-model" :: String)
                    , "base_instructions" Aeson..= ("legacy prompt" :: String)
                    , "model_messages" Aeson..= Aeson.object
                        [ "instructions_template" Aeson..= Aeson.Null
                        , "approvals" Aeson..= Aeson.object
                            ["never" Aeson..= ("approval prompt" :: String)]
                        ]
                    ])
            renderModelInstructions ModelPersonalityDefault model
                `shouldBe` "legacy prompt"
            (model.modelMessages >>= (.approvals) >>= (.never))
                `shouldBe` Just "approval prompt"

        it "rejects endpoint models without any instruction template" do
            Json.decodeEither modelInfoDecoder
                (LBS.toStrict (Aeson.encode (Aeson.object
                    [ "slug" Aeson..= ("missing-prompt" :: String)
                    ])))
                `shouldSatisfy` \case
                    Left err ->
                        "missing both base_instructions"
                            `Text.isInfixOf` Json.jsonErrorMessage err
                    Right _ -> False

        it "treats an instruction template as literal when variables are absent" do
            let model = decodeModelOrFail (Aeson.object
                    [ "slug" Aeson..= ("literal-model" :: String)
                    , "model_messages" Aeson..= Aeson.object
                        [ "instructions_template" Aeson..=
                            ("keep {{ personality }} literal" :: String)
                        ]
                    ])
            renderModelInstructions ModelPersonalityFriendly model
                `shouldBe` "keep {{ personality }} literal"

        it "uses longest-prefix and one-segment namespace matching" do
            let catalog = ModelsResponse
                    { models =
                        [ fallbackModelInfo "gpt-5"
                        , fallbackModelInfo "gpt-5.6"
                        ]
                    , catalogGeneration = Nothing
                    }
            (.slug) <$> findModelInfo "gpt-5.6-2026-08-23" catalog
                `shouldBe` Just "gpt-5.6"
            (.slug) <$> findModelInfo "openai/gpt-5.6-latest" catalog
                `shouldBe` Just "gpt-5.6"
            findModelInfo "invalid/name/with/slashes" catalog
                `shouldBe` Nothing

    describe "model-owned instructions" do
        it "renders personality variables only when the template supports them" do
            catalog <- loadBundledModelsOrThrow
            let model = modelInfoForSlug "gpt-5.5" catalog
            modelSupportsPersonality model `shouldBe` True
            renderModelInstructions ModelPersonalityPragmatic model
                `shouldSatisfy` Text.isInfixOf "You are a deeply pragmatic"

decodeModelsOrFail :: Aeson.Value -> ModelsResponse
decodeModelsOrFail =
    decodeOrFail modelsResponseDecoder

decodeModelOrFail :: Aeson.Value -> ModelInfo
decodeModelOrFail =
    decodeOrFail modelInfoDecoder

decodeOrFail :: Json.Decoder value -> Aeson.Value -> value
decodeOrFail decoder value =
    case Json.decodeEither decoder (LBS.toStrict (Aeson.encode value)) of
        Left err -> error (Text.unpack (Json.jsonErrorMessage err))
        Right decoded -> decoded

hasLegacyBaseInstructions :: Aeson.Value -> Bool
hasLegacyBaseInstructions (Aeson.Object object) =
    case KeyMap.lookup "base_instructions" object of
        Just (Aeson.String instructions) -> not (Text.null instructions)
        _ -> False
hasLegacyBaseInstructions _ = False

jsonObjectField :: Key.Key -> Aeson.Value -> Maybe Aeson.Value
jsonObjectField key (Aeson.Object object) = KeyMap.lookup key object
jsonObjectField _ _ = Nothing
