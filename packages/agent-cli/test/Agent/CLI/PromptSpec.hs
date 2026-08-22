module Agent.CLI.PromptSpec (spec) where

import Agent.CLI.Prompt
import System.OsPath (unsafeEncodeUtf)
import Agent.Provider (Provider(..))
import Data.Time.Calendar (fromGregorian)
import qualified Data.Text as Text
import Test.Hspec

fromFilePath = unsafeEncodeUtf

spec :: Spec
spec = describe "systemPrompt" do
    it "names grok-build tools for xAI and Codex tools for OpenAI" do
        let day = fromGregorian 2026 8 19
            grok = systemPrompt XAIProvider (fromFilePath "/tmp/repo") day False
            openai = systemPrompt OpenAIProvider (fromFilePath "/tmp/repo") day False
        grok `shouldSatisfy` Text.isInfixOf "read_file"
        grok `shouldSatisfy` Text.isInfixOf "search_replace"
        grok `shouldSatisfy` Text.isInfixOf "run_terminal_cmd"
        grok `shouldSatisfy` Text.isInfixOf "<tool_calling>"
        grok `shouldNotSatisfy` Text.isInfixOf "apply_patch"
        grok `shouldSatisfy` Text.isInfixOf "<background_tasks>"
        grok `shouldSatisfy` Text.isInfixOf "get_task_output"
        grok `shouldSatisfy` Text.isInfixOf "kill_task"
        grok `shouldSatisfy` Text.isInfixOf "web_search"
        grok `shouldSatisfy` Text.isInfixOf "<plan_mode>"
        grok `shouldSatisfy` Text.isInfixOf "enter_plan_mode"
        grok `shouldSatisfy` Text.isInfixOf "exit_plan_mode"
        openai `shouldSatisfy` Text.isInfixOf "shell_command"
        openai `shouldSatisfy` Text.isInfixOf "apply_patch"
        openai `shouldSatisfy` Text.isInfixOf "update_plan"
        openai `shouldSatisfy` Text.isInfixOf "web_search"
        openai `shouldSatisfy` Text.isInfixOf "enter_plan_mode"
        openai `shouldSatisfy` Text.isInfixOf "ask_user_question"
        openai `shouldSatisfy` Text.isInfixOf "asks you to make, produce, or design a plan"
        openai `shouldSatisfy` Text.isInfixOf "<proposed_plan>"
        openai `shouldNotSatisfy` Text.isInfixOf "read_file"
        grok `shouldSatisfy` Text.isInfixOf "2026-08-19"
        grok `shouldSatisfy` Text.isInfixOf "/tmp/repo"
        let openrouter = systemPrompt OpenRouterProvider (fromFilePath "/tmp/repo") day False
        openrouter `shouldSatisfy` Text.isInfixOf "read_file"
        openrouter `shouldNotSatisfy` Text.isInfixOf "apply_patch"
        let claude = systemPrompt ClaudeCodeProvider (fromFilePath "/tmp/repo") day False
        claude `shouldSatisfy` Text.isInfixOf "Claude Code's built-in tools"
        claude `shouldSatisfy` Text.isInfixOf "validated structured output"
        claude `shouldNotSatisfy` Text.isInfixOf "local Claude Code transcript"
        claude `shouldSatisfy` Text.isInfixOf "/tmp/repo"
        claude `shouldNotSatisfy` Text.isInfixOf "run_ghci"

    it "uses the autonomous identity for one-shot Grok sessions" do
        let grok = systemPrompt XAIProvider (fromFilePath "/tmp/repo") (fromGregorian 2026 8 19) True
        grok `shouldSatisfy` Text.isInfixOf "no human operator"

    it "keeps OpenAI web-search references internal" do
        let openai =
                systemPrompt OpenAIProvider
                    (fromFilePath "/tmp/repo")
                    (fromGregorian 2026 8 19)
                    False
        openai `shouldSatisfy` Text.isInfixOf "turn2search5"
        openai `shouldSatisfy` Text.isInfixOf "never expose internal reference IDs"
        openai `shouldSatisfy` Text.isInfixOf "descriptive Markdown links"

    it "tells grok and openai to prefer ghci for general-purpose scripting" do
        let day = fromGregorian 2026 8 19
            grok = systemPrompt XAIProvider (fromFilePath "/tmp/repo") day False
            openai = systemPrompt OpenAIProvider (fromFilePath "/tmp/repo") day False
        grok `shouldSatisfy` Text.isInfixOf "Prefer ghci for scripting"
        grok `shouldSatisfy` Text.isInfixOf "Time context:"
        openai `shouldSatisfy` Text.isInfixOf "Time context:"
        openai `shouldSatisfy` Text.isInfixOf "Prefer ghci for scripting"
        grok `shouldSatisfy` Text.isInfixOf "Python, Node, bash"
        openai `shouldSatisfy` Text.isInfixOf "Python, Node, bash"
        grok `shouldSatisfy` Text.isInfixOf "run_ghci"
        openai `shouldSatisfy` Text.isInfixOf "run_ghci"
        grok `shouldSatisfy` Text.isInfixOf "OverloadedStrings"
        openai `shouldSatisfy` Text.isInfixOf "LambdaCase"
        grok `shouldSatisfy` Text.isInfixOf "Pure expressions do not need user approval"

    it "picks the documented default models" do
        defaultModelFor XAIProvider `shouldBe` "grok-4.6"
        defaultModelFor OpenAIProvider `shouldBe` "gpt-5.6-luna"
        defaultModelFor OpenRouterProvider `shouldBe` "openai/gpt-5.1"
        defaultModelFor ClaudeCodeProvider `shouldBe` "sonnet"
