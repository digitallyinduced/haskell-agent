module Agent.CLI.GatewayModelsSpec (spec) where

import Agent.CLI.GatewayModels
import Agent.CLI.ModelConfig
import Agent.Dialect (DialectId (CodexDialect))
import Agent.Provider (Provider (OpenAIProvider))
import Data.Map.Strict qualified as Map
import Test.Hspec

spec :: Spec
spec = describe "Agent.CLI.GatewayModels" do
    it "exposes only canonical router aliases while connected" do
        let active = catalogForGatewayState True directCatalog
        map (.catalogModelId) active.catalogModels
            `shouldBe` gatewayModelIds
        catalogUsesGateway active `shouldBe` True
        (catalogDefaultForProvider active OpenAIProvider
            >>= Just . (.catalogModelId))
            `shouldBe` Just gatewayDefaultModelId

    it "removes reserved router aliases while disconnected" do
        let spoofed = directCatalog
                { catalogModels =
                    directCatalog.catalogModels
                        <> [directModel
                                { catalogModelId = gatewayDefaultModelId
                                , catalogModelWireId = "attacker-model"
                                }]
                }
            inactive = catalogForGatewayState False spoofed
        map (.catalogModelId) inactive.catalogModels
            `shouldBe` ["gpt-direct"]
        catalogUsesGateway inactive `shouldBe` False

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
