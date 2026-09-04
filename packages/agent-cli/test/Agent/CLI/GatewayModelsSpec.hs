module Agent.CLI.GatewayModelsSpec (spec) where

import Agent.CLI.GatewayModels
import Agent.CLI.GatewayClient
    ( GatewayModel(..)
    , GatewayModelProtocol(..)
    )
import Agent.CLI.ModelConfig
import Agent.CLI.Models (ModelOption(..), ModelTarget(..))
import Agent.Dialect (DialectId(..))
import Agent.Provider (Provider(ClaudeCodeProvider, OpenAIProvider))
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

    it "maps the unified catalog to Responses and Claude transports" do
        let options =
                modelOptionsForGatewayModels
                    testCatalog
                    [ GatewayModel "company-a" GatewayResponsesProtocol
                    , GatewayModel "sonnet" GatewayAnthropicProtocol
                    , GatewayModel "router-default" GatewayResponsesProtocol
                    ]
        map (.modelTarget.targetProvider) options
            `shouldBe` [OpenAIProvider, ClaudeCodeProvider]
        map (.modelTarget.targetModelId) options
            `shouldBe` ["company-a", "sonnet"]
        map (.modelTarget.targetConnectionId) options
            `shouldBe` replicate 2 organizationGatewayConnectionId
        map (.modelTarget.targetDialect) options
            `shouldBe` [GenericResponsesDialect, ClaudeCodeDialect]

    it "resolves bundle aliases only from the live gateway catalog" do
        resolveGatewayModelTarget
            testCatalog
            [GatewayModel "company-a" GatewayResponsesProtocol]
            "company-a"
            `shouldBe`
                Right
                    (ModelTarget
                        OpenAIProvider
                        organizationGatewayConnectionId
                        "company-a"
                        "company-a"
                        GenericResponsesDialect)
        resolveGatewayModelTarget
            testCatalog
            [GatewayModel "company-a" GatewayResponsesProtocol]
            "router-default"
            `shouldBe`
                Left
                    "Model alias \"router-default\" is not offered by the active organization gateway."
        resolveGatewayModelTarget
            testCatalog
            [GatewayModel "sonnet" GatewayAnthropicProtocol]
            "sonnet"
            `shouldBe`
                Right
                    (ModelTarget
                        ClaudeCodeProvider
                        organizationGatewayConnectionId
                        "sonnet"
                        "sonnet"
                        ClaudeCodeDialect)

    it "aligns gateway auth with a resumed Claude model target" do
        let options =
                modelOptionsForGatewayModels
                    testCatalog
                    [GatewayModel "sonnet" GatewayAnthropicProtocol]
        case options of
            [option] ->
                gatewayProviderForStartup
                    (Just option.modelTarget)
                    Nothing
                    (Just OpenAIProvider)
                    `shouldBe` ClaudeCodeProvider
            _ -> expectationFailure "expected one Claude gateway model"

    it "defaults gateway auth to OpenAI without a target or provider hint" $
        gatewayProviderForStartup Nothing Nothing Nothing
            `shouldBe` OpenAIProvider

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
