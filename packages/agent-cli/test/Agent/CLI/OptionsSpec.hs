module Agent.CLI.OptionsSpec (spec) where

import Agent.CLI.Options
import System.OsPath (unsafeEncodeUtf)
import Agent.Provider (Provider(..))
import Agent.TUI.Motion (MotionMode(..))
import Test.Hspec

fromFilePath = unsafeEncodeUtf

spec :: Spec
spec = do
    describe "parseArgs" do
        it "prints help and version without running" do
            parseArgs ["--help"] `shouldBe` Right ShowHelp
            parseArgs ["-h"] `shouldBe` Right ShowHelp
            parseArgs ["--provider", "openai", "--help"] `shouldBe` Right ShowHelp
            parseArgs ["--version"] `shouldBe` Right ShowVersion

        it "parses one-shot flags" do
            parseArgs
                [ "--provider", "xai"
                , "--model", "grok-4.6"
                , "--cwd", "/tmp/work"
                , "--bash"
                , "--yolo"
                , "--max-turns", "3"
                , "--compact-threshold", "1200"
                , "--effort", "high"
                , "-p", "hello"
                ]
                `shouldBe` Right (RunAgent defaultCliOptions
                    { optProvider = Just XAIProvider
                    , optModel = Just "grok-4.6"
                    , optCwd = Just (fromFilePath "/tmp/work")
                    , optBash = True
                    , optYolo = True
                    , optMaxTurns = 3
                    , optCompactThreshold = Just 1200
                    , optEffort = Just "high"
                    , optPrompt = Just "hello"
                    })

        it "parses --subagents and --no-subagents" do
            parseArgs ["--no-subagents", "-p", "hello"]
                `shouldBe` Right (RunAgent defaultCliOptions
                    { optSubagents = False
                    , optPrompt = Just "hello"
                    })
            parseArgs ["--no-subagents", "--subagents", "-p", "hello"]
                `shouldBe` Right (RunAgent defaultCliOptions
                    { optSubagents = True
                    , optPrompt = Just "hello"
                    })

        it "parses --worktree" do
            parseArgs ["--worktree"]
                `shouldBe` Right (RunAgent defaultCliOptions { optWorktree = True })
            parseArgs ["--cwd", "/tmp/work", "--worktree", "-p", "hello"]
                `shouldBe` Right (RunAgent defaultCliOptions
                    { optCwd = Just (fromFilePath "/tmp/work")
                    , optWorktree = True
                    , optPrompt = Just "hello"
                    })

        it "accepts none, xhigh, and max effort" do
            parseArgs ["--effort", "none"]
                `shouldBe` Right (RunAgent defaultCliOptions { optEffort = Just "none" })
            parseArgs ["--effort", "xhigh"]
                `shouldBe` Right (RunAgent defaultCliOptions { optEffort = Just "xhigh" })
            parseArgs ["--effort", "max"]
                `shouldBe` Right (RunAgent defaultCliOptions { optEffort = Just "max" })
            parseArgs ["--effort", "HIGH"]
                `shouldBe` Right (RunAgent defaultCliOptions { optEffort = Just "high" })

        it "rejects unknown effort levels" do
            parseArgs ["--effort", "extreme"] `shouldSatisfy` isLeft

        it "accepts openrouter as a provider" do
            parseArgs ["--provider", "openrouter", "-p", "hi"]
                `shouldBe` Right (RunAgent defaultCliOptions
                    { optProvider = Just OpenRouterProvider
                    , optPrompt = Just "hi"
                    })

        it "accepts claude-code and claude as provider names" do
            parseArgs ["--provider", "claude-code", "-p", "hi"]
                `shouldBe` Right (RunAgent defaultCliOptions
                    { optProvider = Just ClaudeCodeProvider
                    , optPrompt = Just "hi"
                    })
            parseArgs ["--provider", "claude", "-p", "hi"]
                `shouldBe` Right (RunAgent defaultCliOptions
                    { optProvider = Just ClaudeCodeProvider
                    , optPrompt = Just "hi"
                    })

        it "rejects the removed openai-base-url command" do
            parseArgs ["openai-base-url"] `shouldSatisfy` isLeft

        it "rejects using both -p and --prompt-file" do
            parseArgs ["-p", "a", "--prompt-file", "b"] `shouldSatisfy` isLeft

        it "requires a positive compaction threshold" do
            parseArgs ["--compact-threshold", "0"] `shouldSatisfy` isLeft
            parseArgs ["--compact-threshold", "-1"] `shouldSatisfy` isLeft
            parseArgs ["--compact-threshold", "nope"] `shouldSatisfy` isLeft

        it "opens the credential manager without starting an agent" do
            parseArgs ["login"] `shouldBe` Right Login
            parseArgs ["login", "openai"] `shouldSatisfy` isLeft


        it "parses sessions list and show" do
            parseArgs ["sessions"] `shouldBe` Right ListSessions
            parseArgs ["sessions", "list"] `shouldBe` Right ListSessions
            parseArgs ["sessions", "show", "2026-08-19-abcd1234"]
                `shouldBe` Right (ShowSession "2026-08-19-abcd1234")

        it "parses --resume and --save-session" do
            parseArgs ["--resume", "2026-08-19-abcd1234"]
                `shouldBe` Right (RunAgent defaultCliOptions
                    { optResume = Just "2026-08-19-abcd1234" })
            parseArgs ["-p", "hi", "--save-session"]
                `shouldBe` Right (RunAgent defaultCliOptions
                    { optPrompt = Just "hi"
                    , optSaveSession = True
                    })

        it "rejects --resume with --worktree" do
            parseArgs ["--resume", "abc", "--worktree"] `shouldSatisfy` isLeft

        it "parses --agents-md and --no-agents-md" do
            parseArgs ["--no-agents-md", "-p", "hi"]
                `shouldBe` Right (RunAgent defaultCliOptions
                    { optPrompt = Just "hi"
                    , optAgentsMd = False
                    })
            parseArgs ["--no-agents-md", "--agents-md", "-p", "hi"]
                `shouldBe` Right (RunAgent defaultCliOptions
                    { optPrompt = Just "hi"
                    , optAgentsMd = True
                    })

        it "parses fullscreen and minimal rendering modes" do
            parseArgs ["--fullscreen"]
                `shouldBe` Right (RunAgent defaultCliOptions
                    { optScreenMode = ScreenFullscreen })
            parseArgs ["--fullscreen", "--minimal"]
                `shouldBe` Right (RunAgent defaultCliOptions
                    { optScreenMode = ScreenMinimal })

        it "parses full, reduced, and disabled motion policies" do
            parseArgs ["--motion", "full"]
                `shouldBe` Right (RunAgent defaultCliOptions
                    { optMotionMode = MotionFull })
            parseArgs ["--motion", "REDUCED"]
                `shouldBe` Right (RunAgent defaultCliOptions
                    { optMotionMode = MotionReduced })
            parseArgs ["--motion", "off"]
                `shouldBe` Right (RunAgent defaultCliOptions
                    { optMotionMode = MotionOff })
            parseArgs ["--motion", "fast"] `shouldSatisfy` isLeft

        it "parses --skills and --no-skills" do
            parseArgs ["--no-skills", "-p", "hi"]
                `shouldBe` Right (RunAgent defaultCliOptions
                    { optPrompt = Just "hi"
                    , optSkills = False
                    })
            parseArgs ["--no-skills", "--skills", "-p", "hi"]
                `shouldBe` Right (RunAgent defaultCliOptions
                    { optPrompt = Just "hi"
                    , optSkills = True
                    })

        it "keeps bash disabled by default and enables it explicitly" do
            parseArgs []
                `shouldBe` Right (RunAgent defaultCliOptions)
            parseArgs ["--bash"]
                `shouldBe` Right (RunAgent defaultCliOptions { optBash = True })
            parseArgs ["--bash", "--no-bash"]
                `shouldBe` Right (RunAgent defaultCliOptions { optBash = False })

        it "keeps ghci enabled by default and disables it explicitly" do
            parseArgs []
                `shouldBe` Right (RunAgent defaultCliOptions)
            parseArgs ["--no-ghci"]
                `shouldBe` Right (RunAgent defaultCliOptions { optGhci = False })
            parseArgs ["--no-ghci", "--ghci"]
                `shouldBe` Right (RunAgent defaultCliOptions { optGhci = True })

    describe "resolveApprovalPolicy" do
        it "auto-approves one-shot scripts without a TTY" do
            resolveApprovalPolicy defaultCliOptions { optPrompt = Just "hi" } False False
                `shouldBe` ApproveAll

        it "denies mutating tools without a TTY when --no-yolo is set" do
            resolveApprovalPolicy defaultCliOptions { optNoYolo = True } False False
                `shouldBe` DenyMutating

        it "does not auto-approve a piped interactive REPL" do
            resolveApprovalPolicy defaultCliOptions False False
                `shouldBe` DenyMutating

        it "prompts on a TTY unless --yolo is set" do
            resolveApprovalPolicy defaultCliOptions True False `shouldBe` PromptMutating
            resolveApprovalPolicy defaultCliOptions { optYolo = True } True False
                `shouldBe` ApproveAll

        it "honors project auto-approve on a TTY unless --no-yolo is set" do
            resolveApprovalPolicy defaultCliOptions True True `shouldBe` ApproveAll
            resolveApprovalPolicy defaultCliOptions { optNoYolo = True } True True
                `shouldBe` PromptMutating

    describe "parseApprovalAnswer" do
        it "allows once, always, or denies" do
            parseApprovalAnswer "y" `shouldBe` AllowOnce
            parseApprovalAnswer "Yes" `shouldBe` AllowOnce
            parseApprovalAnswer "  Y  " `shouldBe` AllowOnce
            parseApprovalAnswer "a" `shouldBe` AllowAlways
            parseApprovalAnswer "ALWAYS" `shouldBe` AllowAlways
            parseApprovalAnswer "yolo" `shouldBe` AllowAlways
            parseApprovalAnswer "" `shouldBe` Deny
            parseApprovalAnswer "n" `shouldBe` Deny
            parseApprovalAnswer "no" `shouldBe` Deny
            parseApprovalAnswer "maybe" `shouldBe` Deny

    describe "defaultEffortFor" do
        it "uses provider-specific effort defaults" do
            defaultEffortFor XAIProvider `shouldBe` "high"
            defaultEffortFor OpenAIProvider `shouldBe` "medium"
            defaultEffortFor OpenRouterProvider `shouldBe` "medium"
            defaultEffortFor ClaudeCodeProvider `shouldBe` "xhigh"

    describe "isOneShot" do
        it "is true for -p and --prompt-file" do
            isOneShot defaultCliOptions `shouldBe` False
            isOneShot defaultCliOptions { optPrompt = Just "x" } `shouldBe` True
            isOneShot defaultCliOptions { optPromptFile = Just (fromFilePath "x.md") } `shouldBe` True

isLeft :: Either a b -> Bool
isLeft = \case
    Left _ -> True
    Right _ -> False
