module Agent.Runtime.TitleModelSpec (spec) where

import Agent.Dialect (DialectId(..))
import Agent.Provider (Provider(..))
import Agent.Runtime.ModelConfig
    ( ModelCatalog
    , decodeModelConfig
    , packagedModelCatalogPath
    )
import Agent.Runtime.Models
    ( ModelOption(..)
    , ModelTarget(..)
    )
import Agent.Runtime.Session.TitleModel
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = do
    catalog <- runIO readPackagedCatalog
    describe "cheapTitleModel" do
        it "selects Claude Haiku instead of Fable or Opus" do
            (cheapTitleModel catalog ClaudeCodeProvider).modelTarget.targetModelId
                `shouldBe` "haiku"

        it "selects the labeled low-cost OpenAI and Gemini models" do
            (cheapTitleModel catalog OpenAIProvider).modelTarget.targetModelId
                `shouldBe` "gpt-5.6-luna"
            (cheapTitleModel catalog GeminiProvider).modelTarget.targetModelId
                `shouldBe` "gemini-3.5-flash-lite"

        it "selects the free OpenRouter model and the sole xAI model" do
            (cheapTitleModel catalog OpenRouterProvider).modelTarget.targetModelId
                `shouldBe` "stealth/ox-alpha"
            (cheapTitleModel catalog XAIProvider).modelTarget.targetModelId
                `shouldBe` "grok-4.6"

    describe "resolveTitleModel" do
        it "uses the cheap Claude model when no title model is pinned" do
            let resolved = resolveTitleModel catalog ClaudeCodeProvider Nothing
            resolved.titleModelId `shouldBe` "haiku"
            resolved.titleWireModelId `shouldBe` "haiku"
            resolved.titleReasoningEffort `shouldBe` "low"
            resolved.titlePinned `shouldBe` False

        it "gives OpenAI Luna titles high reasoning effort" do
            let resolved = resolveTitleModel catalog OpenAIProvider Nothing
            resolved.titleModelId `shouldBe` "gpt-5.6-luna"
            resolved.titleReasoningEffort `shouldBe` "high"

        it "keeps high effort when Luna is pinned for titles" do
            let pinned = ModelTarget
                    { targetProvider = OpenAIProvider
                    , targetConnectionId = "openai"
                    , targetModelId = "gpt-5.6-luna"
                    , targetWireModelId = "gpt-5.6-luna"
                    , targetDialect = CodexDialect
                    }
                resolved = resolveTitleModel catalog OpenAIProvider (Just pinned)
            resolved.titleReasoningEffort `shouldBe` "high"

        it "honors a pinned model on the same provider" do
            let pinned = ModelTarget
                    { targetProvider = ClaudeCodeProvider
                    , targetConnectionId = "claude-code"
                    , targetModelId = "sonnet"
                    , targetWireModelId = "sonnet"
                    , targetDialect = ClaudeCodeDialect
                    }
                resolved = resolveTitleModel catalog ClaudeCodeProvider (Just pinned)
            resolved.titleModelId `shouldBe` "sonnet"
            resolved.titlePinned `shouldBe` True

        it "ignores a pinned model from another provider" do
            let pinned = ModelTarget
                    { targetProvider = OpenAIProvider
                    , targetConnectionId = "openai"
                    , targetModelId = "gpt-5.6-luna"
                    , targetWireModelId = "gpt-5.6-luna"
                    , targetDialect = CodexDialect
                    }
                resolved = resolveTitleModel catalog ClaudeCodeProvider (Just pinned)
            resolved.titleModelId `shouldBe` "haiku"
            resolved.titlePinned `shouldBe` False

    describe "titleSourceCharBudget" do
        it "keeps 4K on-device windows inside a small prompt" do
            titleSourceCharBudget TitleModelResolution
                { titleModelId = "apple"
                , titleWireModelId = "apple"
                , titleContextWindow = Just 4096
                , titleReasoningEffort = "low"
                , titlePinned = True
                }
                `shouldBe` 2000

        it "allows a larger excerpt for ordinary catalog models" do
            titleSourceCharBudget TitleModelResolution
                { titleModelId = "haiku"
                , titleWireModelId = "haiku"
                , titleContextWindow = Nothing
                , titleReasoningEffort = "low"
                , titlePinned = False
                }
                `shouldBe` 6000

readPackagedCatalog :: IO ModelCatalog
readPackagedCatalog = do
    bytes <- packagedModelCatalogPath >>= LBS.readFile
    case decodeModelConfig "models.default.json" bytes of
        Left err -> fail (Text.unpack err)
        Right catalog -> pure catalog
