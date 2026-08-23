module Agent.DialectSpec (spec) where

import Agent.Dialect
import Agent.Provider (Provider(..))
import Test.Hspec

spec :: Spec
spec = describe "Agent.Dialect" do
    describe "dialect slugs" do
        it "round-trips every persisted dialect identity" do
            map (parseDialect . dialectSlug)
                [CodexDialect, GrokBuildDialect, GenericResponsesDialect]
                `shouldBe`
                    map Just
                        [CodexDialect, GrokBuildDialect, GenericResponsesDialect]

        it "accepts compatibility aliases and normalized input" do
            parseDialect " CODEX " `shouldBe` Just CodexDialect
            parseDialect "grok" `shouldBe` Just GrokBuildDialect
            parseDialect "generic" `shouldBe` Just GenericResponsesDialect
            parseDialect "unknown" `shouldBe` Nothing

    describe "model resolution" do
        it "uses provider-native dialects for direct transports" do
            dialectIdForModel OpenAIProvider "arbitrary-model"
                `shouldBe` CodexDialect
            dialectIdForModel XAIProvider "arbitrary-model"
                `shouldBe` GrokBuildDialect

        it "selects OpenRouter dialects from the model family" do
            dialectIdForModel OpenRouterProvider "openai/gpt-5.1"
                `shouldBe` CodexDialect
            dialectIdForModel OpenRouterProvider " X-AI/GROK-4 "
                `shouldBe` GrokBuildDialect
            map (dialectIdForModel OpenRouterProvider)
                [ "anthropic/claude-sonnet-4"
                , "google/gemini-2.5-pro"
                , "stealth/ox-alpha"
                ]
                `shouldBe` replicate 3 GenericResponsesDialect

        it "resolves the executable profile from the same identity" do
            dialectForModel OpenRouterProvider "x-ai/grok-4"
                `shouldBe` dialectForId GrokBuildDialect
            dialectForModel OpenRouterProvider "anthropic/claude-sonnet-4"
                `shouldBe` dialectForId GenericResponsesDialect

    describe "legacy provider mapping" do
        it "preserves the dialect used before explicit persistence" do
            legacyDialectIdForProvider OpenAIProvider `shouldBe` CodexDialect
            legacyDialectIdForProvider XAIProvider `shouldBe` GrokBuildDialect
            legacyDialectIdForProvider OpenRouterProvider
                `shouldBe` GrokBuildDialect

    describe "provider compatibility" do
        it "keeps direct transports on their native dialect" do
            providerSupportsDialect OpenAIProvider CodexDialect
                `shouldBe` True
            providerSupportsDialect OpenAIProvider GrokBuildDialect
                `shouldBe` False
            providerSupportsDialect XAIProvider GrokBuildDialect
                `shouldBe` True
            providerSupportsDialect XAIProvider GenericResponsesDialect
                `shouldBe` False

        it "allows OpenRouter to carry every supported model dialect" do
            map (providerSupportsDialect OpenRouterProvider)
                [CodexDialect, GrokBuildDialect, GenericResponsesDialect]
                `shouldBe` replicate 3 True

    describe "static profiles" do
        it "defines the Codex model-facing contract" do
            dialectProfile codexDialect `shouldBe`
                ( CodexDialect
                , CodexToolSurface
                , StrictFunctionSchemas
                , CollaborationNamespaceLayout
                , CodexPromptStyle
                , CodexProjectInstructions
                , CodexInstructionHome
                , CodexCollaborationProtocol
                )

        it "defines the Grok Build model-facing contract" do
            dialectProfile grokBuildDialect `shouldBe`
                ( GrokBuildDialect
                , GrokBuildToolSurface
                , LooseFunctionSchemas
                , FlatToolLayout
                , GrokBuildPromptStyle
                , GrokProjectInstructions
                , GrokInstructionHome
                , GrokTaskProtocol
                )

        it "defines the portable generic Responses contract" do
            dialectProfile genericResponsesDialect `shouldBe`
                ( GenericResponsesDialect
                , GrokBuildToolSurface
                , LooseFunctionSchemas
                , FlatToolLayout
                , GenericResponsesPromptStyle
                , GrokProjectInstructions
                , HarnessInstructionHome
                , GenericTaskProtocol
                )

dialectProfile dialect =
    ( dialectId dialect
    , dialectToolSurface dialect
    , dialectFunctionSchemaStyle dialect
    , dialectToolLayout dialect
    , dialectPromptStyle dialect
    , dialectProjectInstructionStyle dialect
    , dialectInstructionHomeStyle dialect
    , dialectChildAgentProtocol dialect
    )
