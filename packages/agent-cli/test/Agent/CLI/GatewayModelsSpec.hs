module Agent.CLI.GatewayModelsSpec (spec) where

import Agent.CLI.GatewayModels
import Agent.CLI.ModelConfig
import Agent.CLI.Models (ModelOption(..), ModelTarget(..), modelsForProvider)
import Agent.Dialect (DialectId (CodexDialect))
import Agent.Provider (Provider (OpenAIProvider))
import Data.Map.Strict qualified as Map
import Test.Hspec

spec :: Spec
spec = describe "Agent.CLI.GatewayModels" do
    it "exposes only gateway-backed GPT-5.6 models while connected" do
        let active = catalogForGatewayState True directCatalog
        map (.catalogModelId) active.catalogModels
            `shouldBe` gatewayModelIds
        map (.catalogModelConnectionId) active.catalogModels
            `shouldBe` replicate 3 gatewayConnectionId
        Map.keys active.catalogConnections
            `shouldBe` [gatewayConnectionId]
        catalogUsesGateway active `shouldBe` True
        (catalogDefaultForProvider active OpenAIProvider
            >>= Just . (.catalogModelId))
            `shouldBe` Just gatewayDefaultModelId
        map (.modelTarget.targetModelId)
            (modelsForProvider active OpenAIProvider)
            `shouldBe` gatewayModelIds

    it "removes legacy router aliases while retaining direct models" do
        let spoofed = directCatalog
                { catalogModels =
                    directCatalog.catalogModels
                        <> [directModel
                                { catalogModelId = "router-default"
                                , catalogModelWireId = "attacker-model"
                                }]
                }
            inactive = catalogForGatewayState False spoofed
        map (.catalogModelId) inactive.catalogModels
            `shouldBe` ["gpt-direct"]
        catalogUsesGateway inactive `shouldBe` False

    it "does not mistake direct GPT-5.6 models for gateway mode" do
        let direct = directCatalog
                { catalogModels =
                    [ directModel
                        { catalogModelId = gatewayDefaultModelId
                        , catalogModelWireId = gatewayDefaultModelId
                        }
                    ]
                }
        map (.catalogModelId) (catalogForGatewayState False direct).catalogModels
            `shouldBe` [gatewayDefaultModelId]
        catalogUsesGateway direct `shouldBe` False

    it "does not mistake a user connection named gateway for gateway mode" do
        let custom = ModelCatalog
                { catalogConnections =
                    Map.singleton gatewayConnectionId
                        ModelConnection
                            { connectionId = gatewayConnectionId
                            , connectionKind =
                                CustomResponsesConnection ResponsesConnection
                                    { responsesBaseUrl = "https://example.com/v1"
                                    , responsesApiKeyEnv = Nothing
                                    , responsesApiKeyOptional = True
                                    , responsesRequestTimeoutSeconds = 30
                                    }
                            }
                , catalogModels =
                    [ directModel
                        { catalogModelConnectionId = gatewayConnectionId
                        }
                    ]
                }
        catalogUsesGateway custom `shouldBe` False

directCatalog :: ModelCatalog
directCatalog =
    ModelCatalog
        { catalogConnections =
            Map.singleton "openai"
                ModelConnection
                    { connectionId = "openai"
                    , connectionKind = BuiltinConnection OpenAIProvider
                    }
        , catalogModels = [directModel]
        }

directModel :: CatalogModel
directModel =
    CatalogModel
        { catalogModelId = "gpt-direct"
        , catalogModelConnectionId = "openai"
        , catalogModelWireId = "gpt-direct"
        , catalogModelDialect = CodexDialect
        , catalogModelContextWindow = Nothing
        , catalogModelLabel = Nothing
        , catalogModelReasoningEfforts = Nothing
        , catalogModelDefaultReasoningEffort = Nothing
        , catalogModelDefault = True
        , catalogModelFallbackPriority = Nothing
        }
