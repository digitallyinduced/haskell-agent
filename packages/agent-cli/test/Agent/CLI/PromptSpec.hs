module Agent.CLI.PromptSpec (spec) where

import Agent.CLI.Prompt
import Agent.Dialect
    ( claudeCodeDialect
    , codexDialect
    , genericResponsesDialect
    , grokBuildDialect
    )
import System.OsPath (unsafeEncodeUtf)
import Agent.Provider (BillingMode(..), Provider(..))
import Data.Time.Calendar (fromGregorian)
import qualified Data.Text as Text
import Test.Hspec

fromFilePath = unsafeEncodeUtf

spec :: Spec
spec = describe "systemPrompt" do
    it "names grok-build tools for xAI and Codex tools for OpenAI" do
        let day = fromGregorian 2026 8 19
            grok =
                systemPrompt
                    grokBuildDialect
                    (fromFilePath "/tmp/repo")
                    Nothing
                    day
                    False
            openai =
                systemPrompt
                    codexDialect
                    (fromFilePath "/tmp/repo")
                    Nothing
                    day
                    False
        grok `shouldSatisfy` Text.isInfixOf "read_file"
        grok `shouldSatisfy` Text.isInfixOf "search_replace"
        grok `shouldSatisfy` Text.isInfixOf "run_terminal_command"
        grok `shouldSatisfy` Text.isInfixOf "<tool_calling>"
        grok `shouldNotSatisfy` Text.isInfixOf "apply_patch"
        grok `shouldSatisfy` Text.isInfixOf "<background_tasks>"
        grok `shouldSatisfy` Text.isInfixOf
            "get_command_or_subagent_output"
        grok `shouldSatisfy` Text.isInfixOf
            "kill_command_or_subagent"
        grok `shouldSatisfy` Text.isInfixOf "monitor"
        grok `shouldSatisfy` Text.isInfixOf "spawn_subagent"
        grok `shouldSatisfy` Text.isInfixOf "web_search"
        grok `shouldSatisfy` Text.isInfixOf "<plan_mode>"
        grok `shouldSatisfy` Text.isInfixOf "enter_plan_mode"
        grok `shouldSatisfy` Text.isInfixOf "exit_plan_mode"
        grok `shouldSatisfy` Text.isInfixOf "<user_guide>"
        openai `shouldSatisfy` Text.isInfixOf "shell_command"
        openai `shouldSatisfy` Text.isInfixOf "read_file"
        openai `shouldSatisfy` Text.isInfixOf "list_dir"
        openai `shouldSatisfy` Text.isInfixOf "apply_patch"
        openai `shouldSatisfy` Text.isInfixOf "update_plan"
        openai `shouldSatisfy` Text.isInfixOf "web_search"
        openai `shouldSatisfy` Text.isInfixOf "enter_plan_mode"
        openai `shouldSatisfy` Text.isInfixOf "write_plan"
        openai `shouldSatisfy` Text.isInfixOf "ask_user_question"
        openai `shouldSatisfy` Text.isInfixOf "software implementation or architectural planning"
        openai `shouldSatisfy` Text.isInfixOf "business, writing, or travel plans"
        openai `shouldNotSatisfy` Text.isInfixOf "asks you to make, produce, or design a plan"
        openai `shouldSatisfy` Text.isInfixOf "<proposed_plan>"
        grok `shouldSatisfy` Text.isInfixOf "2026-08-19"
        grok `shouldSatisfy` Text.isInfixOf "/tmp/repo"

    it "uses Claude Code's own tool runtime prompt" do
        let claude =
                systemPrompt
                    claudeCodeDialect
                    (fromFilePath "/tmp/repo")
                    Nothing
                    (fromGregorian 2026 8 19)
                    False
        claude `shouldSatisfy` Text.isInfixOf "Claude Code's built-in tools"
        claude `shouldSatisfy` Text.isInfixOf "validated structured output"
        claude `shouldNotSatisfy` Text.isInfixOf "local Claude Code transcript"
        claude `shouldSatisfy` Text.isInfixOf "/tmp/repo"
        claude `shouldNotSatisfy` Text.isInfixOf "run_ghci"

    it "uses a neutral identity for generic Responses models" do
        let generic =
                systemPrompt
                    genericResponsesDialect
                    (fromFilePath "/tmp/repo")
                    Nothing
                    (fromGregorian 2026 8 19)
                    False
        generic `shouldSatisfy` Text.isInfixOf "interactive coding agent"
        generic `shouldSatisfy` Text.isInfixOf "read_file"
        generic `shouldNotSatisfy` Text.isInfixOf "Grok released by xAI"

    it "uses the autonomous identity for one-shot Grok sessions" do
        let grok =
                systemPrompt
                    grokBuildDialect
                    (fromFilePath "/tmp/repo")
                    Nothing
                    (fromGregorian 2026 8 19)
                    True
        grok `shouldSatisfy` Text.isInfixOf "no human operator"

    it "renders restricted Grok child prompts from the registered tools" do
        let prompt =
                systemPromptForTools
                    grokBuildDialect
                    ["read_file", "list_dir", "grep"]
                    (fromFilePath "/tmp/repo")
                    Nothing
                    (fromGregorian 2026 8 19)
                    True
        prompt `shouldSatisfy` Text.isInfixOf "read_file"
        prompt `shouldSatisfy` Text.isInfixOf "web_search"
        prompt `shouldNotSatisfy` Text.isInfixOf "search_replace"
        prompt `shouldNotSatisfy` Text.isInfixOf "run_terminal_cmd"
        prompt `shouldNotSatisfy` Text.isInfixOf "run_ghci"
        prompt `shouldNotSatisfy` Text.isInfixOf "<background_tasks>"
        prompt `shouldNotSatisfy` Text.isInfixOf "<plan_mode>"

    it "renders restricted generic child prompts without unavailable tools" do
        let prompt =
                systemPromptForTools
                    genericResponsesDialect
                    ["read_file", "list_dir", "grep"]
                    (fromFilePath "/tmp/repo")
                    Nothing
                    (fromGregorian 2026 8 19)
                    True
        prompt `shouldSatisfy` Text.isInfixOf "read_file"
        prompt `shouldSatisfy` Text.isInfixOf "web_search"
        prompt `shouldNotSatisfy` Text.isInfixOf "search_replace"
        prompt `shouldNotSatisfy` Text.isInfixOf "run_terminal_cmd"
        prompt `shouldNotSatisfy` Text.isInfixOf "run_ghci"
        prompt `shouldNotSatisfy` Text.isInfixOf "Prefer ghci for scripting"
        prompt `shouldNotSatisfy` Text.isInfixOf "Use ask_secret"

    it "adds secret guidance only when ask_secret is registered" do
        let withSecret =
                systemPromptForTools
                    genericResponsesDialect
                    ["read_file", "ask_secret"]
                    (fromFilePath "/tmp/repo")
                    Nothing
                    (fromGregorian 2026 8 19)
                    False
            withoutSecret =
                systemPromptForTools
                    genericResponsesDialect
                    ["read_file"]
                    (fromFilePath "/tmp/repo")
                    Nothing
                    (fromGregorian 2026 8 19)
                    False
        withSecret `shouldSatisfy` Text.isInfixOf
            "Never ask the user to paste a token"
        withSecret `shouldSatisfy` Text.isInfixOf
            "It returns a private temporary file path"
        withSecret `shouldSatisfy` Text.isInfixOf
            "Never read, print, summarize"
        withoutSecret `shouldNotSatisfy` Text.isInfixOf "Use ask_secret"

    it "renders ghci-only and bash-only root prompts from registered tools" do
        let day = fromGregorian 2026 8 19
            ghciOnly =
                systemPromptForTools
                    codexDialect
                    ["read_file", "grep", "list_dir", "apply_patch", "run_ghci"]
                    (fromFilePath "/tmp/repo")
                    Nothing
                    day
                    False
            bashOnly =
                systemPromptForTools
                    codexDialect
                    ["read_file", "grep", "list_dir", "apply_patch", "shell_command"]
                    (fromFilePath "/tmp/repo")
                    Nothing
                    day
                    False
        ghciOnly `shouldSatisfy` Text.isInfixOf "Prefer ghci for scripting"
        ghciOnly `shouldNotSatisfy` Text.isInfixOf "shell_command"
        bashOnly `shouldSatisfy` Text.isInfixOf "shell_command"
        bashOnly `shouldNotSatisfy` Text.isInfixOf "run_ghci"
        bashOnly `shouldNotSatisfy` Text.isInfixOf "Prefer ghci for scripting"

    it "omits hidden Grok terminal names from ghci-only prompts" do
        let prompt =
                systemPromptForTools
                    grokBuildDialect
                    ["read_file", "grep", "list_dir", "search_replace", "run_ghci"]
                    (fromFilePath "/tmp/repo")
                    Nothing
                    (fromGregorian 2026 8 19)
                    False
        prompt `shouldSatisfy` Text.isInfixOf "run_ghci"
        prompt `shouldNotSatisfy` Text.isInfixOf "run_terminal_command"
        prompt `shouldNotSatisfy` Text.isInfixOf "run_terminal_cmd"

    it "keeps OpenAI web-search references internal" do
        let openai =
                systemPrompt codexDialect
                    (fromFilePath "/tmp/repo")
                    Nothing
                    (fromGregorian 2026 8 19)
                    False
        openai `shouldSatisfy` Text.isInfixOf "turn2search5"
        openai `shouldSatisfy` Text.isInfixOf "never expose internal reference IDs"
        openai `shouldSatisfy` Text.isInfixOf "descriptive Markdown links"

    it "tells grok and openai to prefer ghci for general-purpose scripting" do
        let day = fromGregorian 2026 8 19
            grok =
                systemPrompt
                    grokBuildDialect
                    (fromFilePath "/tmp/repo")
                    Nothing
                    day
                    False
            openai =
                systemPrompt
                    codexDialect
                    (fromFilePath "/tmp/repo")
                    Nothing
                    day
                    False
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

    it "identifies the private session scratch directory in root and child prompts" do
        let day = fromGregorian 2026 8 19
            scratch = fromFilePath
                "/Users/test/.haskell-agent/tmp/sessions/2026-08-19-abcd1234"
            rootPrompt =
                systemPrompt
                    codexDialect
                    (fromFilePath "/tmp/repo")
                    (Just scratch)
                    day
                    False
            childPrompt =
                systemPromptForTools
                    genericResponsesDialect
                    ["read_file", "shell_command"]
                    (fromFilePath "/tmp/repo")
                    (Just scratch)
                    day
                    True
        rootPrompt `shouldSatisfy` Text.isInfixOf
            "/Users/test/.haskell-agent/tmp/sessions/2026-08-19-abcd1234"
        childPrompt `shouldSatisfy` Text.isInfixOf
            "/Users/test/.haskell-agent/tmp/sessions/2026-08-19-abcd1234"
        rootPrompt `shouldSatisfy` Text.isInfixOf
            "clones, downloads, extracted files, generated assets"
        childPrompt `shouldSatisfy` Text.isInfixOf
            "relative paths still resolve against the workspace"
        rootPrompt `shouldSatisfy` Text.isInfixOf "HASKELL_AGENT_TMPDIR"
        childPrompt `shouldSatisfy` Text.isInfixOf "TMPDIR"
    it "recommends Luna only for subscription-backed OpenAI subagents" do
        let subscriptionGuidance =
                subscriptionSubagentModelGuidance
                    OpenAIProvider
                    SubscriptionBilled
        subscriptionGuidance `shouldSatisfy`
            maybe False (Text.isInfixOf "`gpt-5.6-luna`")
        subscriptionGuidance `shouldSatisfy`
            maybe False (Text.isInfixOf "small, bounded tasks")
        subscriptionSubagentModelGuidance OpenAIProvider ApiBilled
            `shouldBe` Nothing
        subscriptionSubagentModelGuidance XAIProvider SubscriptionBilled
            `shouldBe` Nothing
