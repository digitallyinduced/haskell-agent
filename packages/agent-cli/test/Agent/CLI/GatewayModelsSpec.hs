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
import Data.Aeson qualified as Aeson
import Data.Aeson ((.=))
import Data.Text (Text)
import Data.Text qualified as Text
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
            `shouldBe` ["router-default", "grok", "gemini", "router", "sonnet"]
        map (.modelTarget.targetConnectionId) options
            `shouldBe` ["openai", "xai", "gemini", "openrouter", "claude-code"]

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

-- Exercise the same validated construction boundary as production. A catalog
-- now always includes a default for each builtin provider.
testCatalog :: ModelCatalog
testCatalog = either (error . Text.unpack) id $
    decodeModelConfig "gateway-test.json" $ Aeson.encode $ Aeson.object
        [ "version" .= (1 :: Int)
        , "connections" .= Aeson.object
            [ "openai" .= builtin "openai"
            , "xai" .= builtin "xai"
            , "gemini" .= builtin "gemini"
            , "openrouter" .= builtin "openrouter"
            , "claude-code" .= builtin "claude-code"
            , "organization-gateway" .= Aeson.object ["api" .= ("gateway" :: Text)]
            ]
        , "models" .=
            ( [ Aeson.object
                    [ "id" .= model
                    , "connection" .= provider
                    , "dialect" .= dialect
                    , "default" .= True
                    ]
              | (provider, model, dialect) <- builtinModels
              ]
                <> [ Aeson.object
                        [ "id" .= ("company-a" :: Text)
                        , "connection" .= organizationGatewayConnectionId
                        , "dialect" .= ("generic-responses" :: Text)
                        , "context_window" .= (131_072 :: Int)
                        , "label" .= ("Company A" :: Text)
                        , "reasoning_efforts" .= ["high" :: Text]
                        , "default_reasoning_effort" .= ("high" :: Text)
                        ]
                   ]
            )
        ]
  where
    builtin :: Text -> Aeson.Value
    builtin provider = Aeson.object
        [ "api" .= ("builtin" :: Text), "provider" .= provider ]
    builtinModels :: [(Text, Text, Text)]
    builtinModels =
        [ ("openai", "router-default", "codex")
        , ("xai", "grok", "grok-build")
        , ("gemini", "gemini", "generic-responses")
        , ("openrouter", "router", "generic-responses")
        , ("claude-code", "sonnet", "claude-code")
        ]
