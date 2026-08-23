module Agent.DialectSpec (spec) where

import Agent.Dialect
import Agent.Provider (Provider(..))
import Test.Hspec

spec :: Spec
spec = describe "Agent.Dialect" do
    describe "dialect slugs" do
        it "round-trips every persisted dialect identity" do
            map (parseDialect . dialectSlug)
                [ CodexDialect
                , GrokBuildDialect
                , GenericResponsesDialect
                , ClaudeCodeDialect
                ]
                `shouldBe`
                    map Just
                        [ CodexDialect
                        , GrokBuildDialect
                        , GenericResponsesDialect
                        , ClaudeCodeDialect
                        ]

        it "accepts compatibility aliases and normalized input" do
            parseDialect " CODEX " `shouldBe` Just CodexDialect
            parseDialect "grok" `shouldBe` Just GrokBuildDialect
            parseDialect "generic" `shouldBe` Just GenericResponsesDialect
            parseDialect "claude" `shouldBe` Just ClaudeCodeDialect
            parseDialect "unknown" `shouldBe` Nothing

    describe "model resolution" do
        it "uses provider-native dialects for direct transports" do
            dialectIdForModel OpenAIProvider "arbitrary-model"
                `shouldBe` CodexDialect
            dialectIdForModel XAIProvider "arbitrary-model"
                `shouldBe` GrokBuildDialect
            dialectIdForModel ClaudeCodeProvider "arbitrary-model"
                `shouldBe` ClaudeCodeDialect

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
            legacyDialectIdForProvider ClaudeCodeProvider
                `shouldBe` ClaudeCodeDialect

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
            providerSupportsDialect ClaudeCodeProvider ClaudeCodeDialect
                `shouldBe` True
            providerSupportsDialect ClaudeCodeProvider CodexDialect
                `shouldBe` False

        it "allows OpenRouter to carry every Responses model dialect" do
            map (providerSupportsDialect OpenRouterProvider)
                [CodexDialect, GrokBuildDialect, GenericResponsesDialect]
                `shouldBe` replicate 3 True
            providerSupportsDialect OpenRouterProvider ClaudeCodeDialect
                `shouldBe` False

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

        it "defines the Claude Code owned-tool contract" do
            dialectProfile claudeCodeDialect `shouldBe`
                ( ClaudeCodeDialect
                , ClaudeCodeToolSurface
                , NoFunctionSchemas
                , NoHostToolLayout
                , ClaudeCodePromptStyle
                , CodexProjectInstructions
                , ClaudeInstructionHome
                , NoHostChildAgentProtocol
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
