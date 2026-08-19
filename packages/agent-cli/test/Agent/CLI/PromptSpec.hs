module Agent.CLI.PromptSpec (spec) where

import Agent.CLI.Prompt
import Agent.Provider (Provider(..))
import Data.Time.Calendar (fromGregorian)
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = describe "systemPrompt" do
    it "names grok-build tools for xAI and Codex tools for OpenAI" do
        let day = fromGregorian 2026 8 19
            grok = systemPrompt XAIProvider "/tmp/repo" day
            openai = systemPrompt OpenAIProvider "/tmp/repo" day
        grok `shouldSatisfy` Text.isInfixOf "read_file"
        grok `shouldSatisfy` Text.isInfixOf "search_replace"
        grok `shouldSatisfy` Text.isInfixOf "run_terminal_cmd"
        grok `shouldNotSatisfy` Text.isInfixOf "apply_patch"
        openai `shouldSatisfy` Text.isInfixOf "shell_command"
        openai `shouldSatisfy` Text.isInfixOf "apply_patch"
        openai `shouldSatisfy` Text.isInfixOf "update_plan"
        openai `shouldNotSatisfy` Text.isInfixOf "read_file"
        grok `shouldSatisfy` Text.isInfixOf "2026-08-19"
        grok `shouldSatisfy` Text.isInfixOf "/tmp/repo"

    it "picks the documented default models" do
        defaultModelFor XAIProvider `shouldBe` "grok-4.5"
        defaultModelFor OpenAIProvider `shouldBe` "gpt-5.1-codex"
