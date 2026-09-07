module Agent.CLI.GatewayModelsSpec (spec) where

import Agent.CLI.GatewayModels
import Agent.CLI.GatewayClient
    ( GatewayModel(..)
    , GatewayModelProtocol(..)
    , GatewayModelProvider(..)
    )
import Agent.CLI.ModelConfig
import Agent.CLI.Models (ModelOption(..), ModelTarget(..))
import Agent.Dialect (DialectId(..))
import Agent.Provider (Provider(ClaudeCodeProvider, GeminiProvider, OpenAIProvider, XAIProvider))
import Data.Aeson qualified as Aeson
import Data.Aeson ((.=))
import Data.Either (isLeft)
import Data.Text (Text)
import Data.Text qualified as Text
import Test.Hspec

spec :: Spec
spec = describe "Agent.CLI.GatewayModels" do
    it "uses only the aliases advertised by the connected gateway" do
        let options =
                modelOptionsForGatewayState
                    testCatalog
                    (Just
                        [ GatewayModel "company-b" GatewayResponsesProtocol GatewayOpenAIProvider
                        , GatewayModel "company-a" GatewayResponsesProtocol GatewayXAIProvider
                        , GatewayModel "company-b" GatewayResponsesProtocol GatewayOpenAIProvider
                        ])
        map (.modelTarget.targetModelId) options
            `shouldBe` ["company-b", "company-a"]
        map (.modelTarget.targetConnectionId) options
            `shouldBe` replicate 2 organizationGatewayConnectionId
        map (.modelTarget.targetWireModelId) options
            `shouldBe` ["company-b", "company-a"]
        map (.modelTarget.targetDialect) options
            `shouldBe` [CodexDialect, GrokBuildDialect]
        map (.modelLabel) options
            `shouldBe` [Nothing, Just "Company A"]

    it "uses only direct catalog entries while disconnected" do
        let options = modelOptionsForGatewayState testCatalog Nothing
        map (.modelTarget.targetModelId) options
            `shouldBe` ["router-default", "grok", "gemini", "router", "sonnet"]
        map (.modelTarget.targetConnectionId) options
            `shouldBe` ["openai", "xai", "gemini", "openrouter", "claude-code"]

    it "maps shared Responses models to their distinct native transports" do
        let options =
                modelOptionsForGatewayModels
                    testCatalog
                    [ GatewayModel "company-a" GatewayResponsesProtocol GatewayOpenAIProvider
                    , GatewayModel "company-grok" GatewayResponsesProtocol GatewayXAIProvider
                    , GatewayModel "sonnet" GatewayAnthropicProtocol GatewayAnthropicProvider
                    , GatewayModel "router-default" GatewayResponsesProtocol GatewayOpenAIProvider
                    ]
        map (.modelTarget.targetProvider) options
            `shouldBe` [OpenAIProvider, XAIProvider, ClaudeCodeProvider]
        map (.modelTarget.targetModelId) options
            `shouldBe` ["company-a", "company-grok", "sonnet"]
        map (.modelTarget.targetConnectionId) options
            `shouldBe` replicate 3 organizationGatewayConnectionId
        map (.modelTarget.targetDialect) options
            `shouldBe` [CodexDialect, GrokBuildDialect, ClaudeCodeDialect]

    it "uses provider metadata rather than model names or local dialect overrides" do
        let options = modelOptionsForGatewayModels testCatalog
                [ GatewayModel "gpt-company" GatewayResponsesProtocol GatewayXAIProvider
                , GatewayModel "grok-company" GatewayResponsesProtocol GatewayOpenAIProvider
                , GatewayModel "company-a" GatewayResponsesProtocol GatewayXAIProvider
                ]
        map (.modelTarget.targetProvider) options
            `shouldBe` [XAIProvider, OpenAIProvider, XAIProvider]
        map (.modelTarget.targetDialect) options
            `shouldBe` [GrokBuildDialect, CodexDialect, GrokBuildDialect]
        map (.modelTarget.targetWireModelId) options
            `shouldBe` ["gpt-company", "grok-company", "company-a"]
        map (.modelLabel) options
            `shouldBe` [Nothing, Nothing, Just "Company A"]

    describe "selectGatewayModelOption" do
        let options = modelOptionsForGatewayModels testCatalog
                [ GatewayModel "company-openai" GatewayResponsesProtocol GatewayOpenAIProvider
                , GatewayModel "company-xai" GatewayResponsesProtocol GatewayXAIProvider
                , GatewayModel "company-claude" GatewayAnthropicProtocol GatewayAnthropicProvider
                ]
            savedTarget provider model =
                ModelTarget provider organizationGatewayConnectionId model model CodexDialect
            selectedProvider model provider hints =
                (.modelTarget.targetProvider)
                    <$> selectGatewayModelOption options model provider hints

        it "selects an explicit alias using its advertised provider" $
            selectedProvider (Just "company-xai") Nothing []
                `shouldBe` Right XAIProvider

        it "rejects an explicit alias absent from the authorized catalog" $
            selectGatewayModelOption options (Just "unlisted") Nothing []
                `shouldSatisfy` isLeft

        it "rejects an explicit provider that conflicts with the explicit alias" $
            selectGatewayModelOption options
                (Just "company-xai") (Just OpenAIProvider) []
                `shouldSatisfy` isLeft

        it "accepts an explicit provider matching the explicit alias" $
            selectedProvider (Just "company-xai") (Just XAIProvider) []
                `shouldBe` Right XAIProvider

        it "resolves a saved Grok alias without trusting its old OpenAI provider" $
            selectedProvider Nothing Nothing
                [savedTarget OpenAIProvider "company-xai"]
                `shouldBe` Right XAIProvider

        it "uses the first available saved alias preference" $
            selectedProvider Nothing Nothing
                [ savedTarget OpenAIProvider "removed-alias"
                , savedTarget OpenAIProvider "company-xai"
                , savedTarget ClaudeCodeProvider "company-claude"
                ]
                `shouldBe` Right XAIProvider

        it "filters saved alias preferences and defaults by explicit provider" $
            selectedProvider Nothing (Just XAIProvider)
                [savedTarget OpenAIProvider "company-openai"]
                `shouldBe` Right XAIProvider

        it "defaults to the first authorized model when no preference resolves" $
            selectedProvider Nothing Nothing
                [savedTarget OpenAIProvider "removed-alias"]
                `shouldBe` Right OpenAIProvider

        it "rejects a provider with no authorized models" $
            selectGatewayModelOption options Nothing (Just GeminiProvider) []
                `shouldSatisfy` isLeft

        it "rejects an empty authorized catalog" $
            selectGatewayModelOption [] Nothing Nothing []
                `shouldSatisfy` isLeft

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
