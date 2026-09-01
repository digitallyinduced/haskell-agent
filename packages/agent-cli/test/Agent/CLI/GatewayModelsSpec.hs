module Agent.CLI.GatewayModelsSpec (spec) where

import Agent.CLI.GatewayModels
import Agent.CLI.ModelConfig
import Agent.CLI.Models (ModelOption(..), ModelTarget(..))
import Agent.Dialect (DialectId(..))
import Agent.Provider (Provider(OpenAIProvider))
import Data.Map.Strict qualified as Map
import Test.Hspec

spec :: Spec
spec = describe "Agent.CLI.GatewayModels" do
    it "uses only the aliases advertised by the connected gateway" do
        let options =
                modelOptionsForGatewayState
                    testCatalog
                    (Just ["company-b", "company-a", "company-b"])
        map (.modelTarget.targetModelId) options
            `shouldBe` ["company-b", "company-a"]
        map (.modelTarget.targetConnectionId) options
            `shouldBe` replicate 2 organizationGatewayConnectionId
        map (.modelTarget.targetWireModelId) options
            `shouldBe` ["company-b", "company-a"]
        map (.modelTarget.targetDialect) options
            `shouldBe` [CodexDialect, GenericResponsesDialect]
        map (.modelLabel) options
            `shouldBe` [Nothing, Just "Company A"]

    it "uses only direct catalog entries while disconnected" do
        let options = modelOptionsForGatewayState testCatalog Nothing
        map (.modelTarget.targetModelId) options
            `shouldBe` ["router-default"]
        map (.modelTarget.targetConnectionId) options
            `shouldBe` ["openai"]

testCatalog :: ModelCatalog
testCatalog =
    ModelCatalog
        { catalogConnections =
            Map.fromList
                [ ( "openai"
                  , ModelConnection
                        { connectionId = "openai"
                        , connectionKind = BuiltinConnection OpenAIProvider
                        }
                  )
                , ( organizationGatewayConnectionId
                  , ModelConnection
                        { connectionId = organizationGatewayConnectionId
                        , connectionKind = OrganizationGatewayConnection
                        }
                  )
                ]
        , catalogModels = [directModel, gatewayMetadata]
        , catalogModelsById =
            Map.singleton directModel.catalogModelId directModel
        , catalogGatewayModelsById =
            Map.singleton gatewayMetadata.catalogModelId gatewayMetadata
        }

directModel :: CatalogModel
directModel =
    CatalogModel
        { catalogModelId = "router-default"
        , catalogModelConnectionId = "openai"
        , catalogModelWireId = "router-default"
        , catalogModelDialect = CodexDialect
        , catalogModelContextWindow = Nothing
        , catalogModelLabel = Nothing
        , catalogModelReasoningEfforts = Nothing
        , catalogModelDefaultReasoningEffort = Nothing
        , catalogModelDefault = True
        , catalogModelFallbackPriority = Nothing
        }

gatewayMetadata :: CatalogModel
gatewayMetadata =
    CatalogModel
        { catalogModelId = "company-a"
        , catalogModelConnectionId = organizationGatewayConnectionId
        , catalogModelWireId = "company-a"
        , catalogModelDialect = GenericResponsesDialect
        , catalogModelContextWindow = Just 131_072
        , catalogModelLabel = Just "Company A"
        , catalogModelReasoningEfforts = Just ["high"]
        , catalogModelDefaultReasoningEffort = Just "high"
        , catalogModelDefault = False
        , catalogModelFallbackPriority = Nothing
        }
