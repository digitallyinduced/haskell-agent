module Agent.CLI.GatewayModelsSpec (spec) where

import Agent.CLI.GatewayModels
import Agent.CLI.ModelConfig
import Agent.CLI.RuntimeModel
    ( appleFoundationModelsModelId
    , applyRuntimeResponsesModel
    , mkAppleFoundationModelsRuntime
    )
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

    it "does not retain custom models that reuse reserved router aliases" do
        let customConnectionId = "custom"
            spoofed = directCatalog
                { catalogConnections =
                    Map.insert
                        customConnectionId
                        ModelConnection
                            { connectionId = customConnectionId
                            , connectionKind =
                                CustomResponsesConnection ResponsesConnection
                                    { responsesBaseUrl =
                                        "http://127.0.0.1:8000/v1"
                                    , responsesApiKeyEnv = Nothing
                                    , responsesApiKeyOptional = True
                                    , responsesRequestTimeoutSeconds = 60
                                    }
                            }
                        directCatalog.catalogConnections
                , catalogModels =
                    directCatalog.catalogModels
                        <> [directModel
                                { catalogModelId = gatewayDefaultModelId
                                , catalogModelConnectionId =
                                    customConnectionId
                                , catalogModelWireId = "attacker-model"
                                }]
                }
            active = catalogForGatewayState True spoofed
        map (.catalogModelId) active.catalogModels
            `shouldBe` gatewayModelIds

    it "keeps independently authenticated runtime models while connected" do
        runtime <- expectRight
            (mkAppleFoundationModelsRuntime
                "http://127.0.0.1:49152/v1"
                "ephemeral-token"
                4096)
        let active =
                catalogForGatewayState True
                    (applyRuntimeResponsesModel runtime directCatalog)
        map (.catalogModelId) active.catalogModels
            `shouldBe` gatewayModelIds <> [appleFoundationModelsModelId]

expectRight :: Show err => Either err value -> IO value
expectRight = \case
    Left err -> do
        expectationFailure (show err)
        fail "expected Right"
    Right value -> pure value

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
