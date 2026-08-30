module Agent.CLI.OptionsSpec (spec) where

import Agent.CLI.Options
import Agent.Dialect (DialectId(..))
import Agent.Loop (defaultLoopMaxTurns)
import System.OsPath (unsafeEncodeUtf)
import Agent.Provider (Provider(..))
import Agent.ReasoningEffort (ReasoningEffort(..))
import Agent.TUI.Motion (MotionMode(..))
import Test.Hspec

fromFilePath = unsafeEncodeUtf

spec :: Spec
spec = do
    describe "parseArgs" do
        it "parses gateway account commands" do
            parseArgs ["gateway", "connect", "--url", "https://gateway.example"]
                `shouldBe` Right
                    (Gateway (GatewayConnect "https://gateway.example"))
            parseArgs ["gateway", "status"]
                `shouldBe` Right (Gateway GatewayStatus)
            parseArgs ["gateway", "disconnect"]
                `shouldBe` Right (Gateway GatewayDisconnect)

        it "prints help and version without running" do
            parseArgs ["--help"] `shouldBe` Right ShowHelp
            parseArgs ["-h"] `shouldBe` Right ShowHelp
            parseArgs ["--provider", "openai", "--help"] `shouldBe` Right ShowHelp
            parseArgs ["--version"] `shouldBe` Right ShowVersion
            parseArgs ["sessions", "list", "--version"]
                `shouldBe` Right ShowVersion

        it "parses one-shot flags" do
            parseArgs
                [ "--provider", "xai"
                , "--model", "grok-4.6"
                , "--cwd", "/tmp/work"
                , "--bash"
                , "--yolo"
                , "--max-turns", "3"
                , "--max-concurrent-agents", "64"
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
                    , optMaxConcurrentAgents = Just 64
                    , optCompactThreshold = Just 1200
                    , optEffort = Just EffortHigh
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
                `shouldBe` Right (RunAgent defaultCliOptions { optEffort = Just EffortNone })
            parseArgs ["--effort", "xhigh"]
                `shouldBe` Right (RunAgent defaultCliOptions { optEffort = Just EffortXHigh })
            parseArgs ["--effort", "max"]
                `shouldBe` Right (RunAgent defaultCliOptions { optEffort = Just EffortMax })
            parseArgs ["--effort", "HIGH"]
                `shouldBe` Right (RunAgent defaultCliOptions { optEffort = Just EffortHigh })

        it "keeps raw OpenAI reasoning hidden unless explicitly requested" do
            defaultCliOptions.optShowRawReasoning `shouldBe` False
            parseArgs ["--show-raw-reasoning"]
                `shouldBe` Right
                    (RunAgent defaultCliOptions { optShowRawReasoning = True })

        it "rejects unknown effort levels" do
            parseArgs ["--effort", "extreme"] `shouldSatisfy` isLeft

        it "accepts openrouter as a provider" do
            parseArgs ["--provider", "openrouter", "-p", "hi"]
                `shouldBe` Right (RunAgent defaultCliOptions
                    { optProvider = Just OpenRouterProvider
                    , optPrompt = Just "hi"
                    })

        it "accepts gemini and google as provider names" do
            parseArgs ["--provider", "gemini", "-p", "hi"]
                `shouldBe` Right (RunAgent defaultCliOptions
                    { optProvider = Just GeminiProvider
                    , optPrompt = Just "hi"
                    })
            parseArgs ["--provider", "google", "-p", "hi"]
                `shouldBe` Right (RunAgent defaultCliOptions
                    { optProvider = Just GeminiProvider
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
            parseArgs ["openai-base-url"]
                `shouldBe`
                    Left "openai-base-url was removed; run agent-cli --help"

        it "keeps the last repeated option value" do
            parseArgs
                [ "--model", "first"
                , "--effort", "low"
                , "--model", "second"
                , "--effort", "HIGH"
                ]
                `shouldBe` Right (RunAgent defaultCliOptions
                    { optModel = Just "second"
                    , optEffort = Just EffortHigh
                    })

        it "applies approval flags in command-line order" do
            parseArgs ["--yolo", "--no-yolo", "--yolo"]
                `shouldBe` Right (RunAgent defaultCliOptions
                    { optYolo = True
                    , optNoYolo = False
                    })
            parseArgs
                [ "--managed-deny-mutations"
                , "--yolo"
                ]
                `shouldBe` Right (RunAgent defaultCliOptions
                    { optYolo = True
                    , optNoYolo = False
                    , optManagedDenyMutations = True
                    })

        it "rejects using both -p and --prompt-file" do
            parseArgs ["-p", "a", "--prompt-file", "b"] `shouldSatisfy` isLeft
            parseArgs ["-p", "a", "--managed-turn-file", "b"]
                `shouldSatisfy` isLeft

        it "parses the internal managed-turn request file" do
            parseArgs ["--managed-turn-file", "turn.json"]
                `shouldBe` Right (RunAgent defaultCliOptions
                    { optManagedTurnFile = Just (fromFilePath "turn.json") })

        it "defaults max turns from agent-core and requires a positive override" do
            defaultCliOptions.optMaxTurns `shouldBe` defaultLoopMaxTurns
            defaultLoopMaxTurns `shouldBe` 2000
            parseArgs ["--max-turns", "0"] `shouldSatisfy` isLeft
            parseArgs ["--max-turns", "-1"] `shouldSatisfy` isLeft
            parseArgs ["--max-turns", "nope"] `shouldSatisfy` isLeft
            usage `shouldContain` ("default: " <> show defaultLoopMaxTurns)

        it "requires a positive concurrent agent limit" do
            parseArgs ["--max-concurrent-agents", "0"] `shouldSatisfy` isLeft
            parseArgs ["--max-concurrent-agents", "-1"] `shouldSatisfy` isLeft
            parseArgs ["--max-concurrent-agents", "nope"] `shouldSatisfy` isLeft
            parseArgs ["--max-concurrent-agents", "8"]
                `shouldBe` Right (RunAgent defaultCliOptions
                    { optMaxConcurrentAgents = Just 8 })

        it "requires a positive compaction threshold" do
            parseArgs ["--compact-threshold", "0"] `shouldSatisfy` isLeft
            parseArgs ["--compact-threshold", "-1"] `shouldSatisfy` isLeft
            parseArgs ["--compact-threshold", "nope"] `shouldSatisfy` isLeft

        it "opens the credential manager without starting an agent" do
            parseArgs ["login"] `shouldBe` Right Login
            parseArgs ["login", "openai"] `shouldSatisfy` isLeft


        it "parses session administration commands" do
            parseArgs ["sessions"] `shouldBe` Right ListSessions
            parseArgs ["sessions", "list"] `shouldBe` Right ListSessions
            parseArgs ["sessions", "show", "2026-08-19-abcd1234"]
                `shouldBe` Right (ShowSession "2026-08-19-abcd1234")
            parseArgs ["sessions", "wait", "2026-08-19-abcd1234"]
                `shouldBe` Right (WaitSession "2026-08-19-abcd1234")
            parseArgs ["sessions", "import"]
                `shouldBe` Right (ImportSession Nothing)
            parseArgs ["sessions", "import", "--cwd", "/srv/project"]
                `shouldBe` Right
                    (ImportSession (Just (unsafeEncodeUtf "/srv/project")))

        it "parses storage administration commands" do
            parseArgs ["storage", "status"]
                `shouldBe` Right (Storage StorageStatus)
            parseArgs ["storage", "start"]
                `shouldBe` Right (Storage StorageStart)
            parseArgs ["storage", "stop"]
                `shouldBe` Right (Storage StorageStop)
            parseArgs ["storage", "migrate"]
                `shouldBe` Right (Storage StorageMigrate)
            parseArgs ["storage", "doctor"]
                `shouldBe` Right (Storage StorageDoctor)
            parseArgs ["storage"] `shouldSatisfy` isLeft
            parseArgs ["storage", "vacuum"] `shouldSatisfy` isLeft

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

        it "keeps bash enabled by default and disables it explicitly" do
            parseArgs []
                `shouldBe` Right (RunAgent defaultCliOptions)
            defaultCliOptions.optBash `shouldBe` True
            parseArgs ["--no-bash"]
                `shouldBe` Right (RunAgent defaultCliOptions { optBash = False })
            parseArgs ["--no-bash", "--bash"]
                `shouldBe` Right (RunAgent defaultCliOptions { optBash = True })

        it "keeps computer use opt-in" do
            defaultCliOptions.optComputerUse `shouldBe` False
            parseArgs ["--computer-use"]
                `shouldBe` Right (RunAgent defaultCliOptions
                    { optComputerUse = True })
            parseArgs ["--computer-use", "--no-computer-use"]
                `shouldBe` Right (RunAgent defaultCliOptions
                    { optComputerUse = False })

        it "uses conventional tool calling by default" do
            defaultCliOptions.optCodeMode `shouldBe` False
            parseArgs ["--code-mode"]
                `shouldBe` Right (RunAgent defaultCliOptions
                    { optCodeMode = True })
            parseArgs ["--code-mode", "--no-code-mode"]
                `shouldBe` Right (RunAgent defaultCliOptions
                    { optCodeMode = False })

        it "keeps ghci disabled by default and enables it explicitly" do
            parseArgs []
                `shouldBe` Right (RunAgent defaultCliOptions)
            defaultCliOptions.optGhci `shouldBe` False
            parseArgs ["--ghci"]
                `shouldBe` Right (RunAgent defaultCliOptions { optGhci = True })
            parseArgs ["--ghci", "--no-ghci"]
                `shouldBe` Right (RunAgent defaultCliOptions { optGhci = False })

    describe "resolveApprovalPolicy" do
        it "auto-approves one-shot scripts without a TTY" do
            resolveApprovalPolicy defaultCliOptions { optPrompt = Just "hi" } False False
                `shouldBe` ApproveAll

        it "denies mutating tools without a TTY when --no-yolo is set" do
            resolveApprovalPolicy defaultCliOptions { optNoYolo = True } False False
                `shouldBe` DenyMutating

        it "keeps managed non-TTY turns in remote prompt mode" do
            resolveApprovalPolicy
                defaultCliOptions
                    { optManagedTurnFile = Just (fromFilePath "turn.json")
                    , optNoYolo = True
                    }
                False
                False
                `shouldBe` PromptMutating

        it "keeps explicitly denied managed turns non-mutating" do
            resolveApprovalPolicy
                defaultCliOptions
                    { optManagedTurnFile = Just (fromFilePath "turn.json")
                    , optManagedDenyMutations = True
                    }
                False
                False
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
        it "allows once, remembers a tool, enables yolo, or denies" do
            parseApprovalAnswer "y" `shouldBe` AllowOnce
            parseApprovalAnswer "Yes" `shouldBe` AllowOnce
            parseApprovalAnswer "  Y  " `shouldBe` AllowOnce
            parseApprovalAnswer "a" `shouldBe` AllowAlways
            parseApprovalAnswer "ALWAYS" `shouldBe` AllowAlways
            parseApprovalAnswer "A" `shouldBe` AllowAll
            parseApprovalAnswer "all" `shouldBe` AllowAll
            parseApprovalAnswer "yolo" `shouldBe` AllowAll
            parseApprovalAnswer "" `shouldBe` Deny
            parseApprovalAnswer "n" `shouldBe` Deny
            parseApprovalAnswer "no" `shouldBe` Deny
            parseApprovalAnswer "maybe" `shouldBe` Deny

    describe "defaultEffortFor" do
        it "uses provider-specific effort defaults" do
            defaultEffortFor XAIProvider `shouldBe` EffortHigh
            defaultEffortFor OpenAIProvider `shouldBe` EffortMedium
            defaultEffortFor OpenRouterProvider `shouldBe` EffortMedium
            defaultEffortFor GeminiProvider `shouldBe` EffortMedium
            defaultEffortFor ClaudeCodeProvider `shouldBe` EffortXHigh

    describe "reasoningEffortsForDialect" do
        it "does not offer OpenAI max effort to Grok models" do
            reasoningEffortsForDialect GrokBuildDialect
                `shouldBe`
                    [ EffortNone
                    , EffortLow
                    , EffortMedium
                    , EffortHigh
                    , EffortXHigh
                    ]
            reasoningEffortsForDialect CodexDialect
                `shouldBe` reasoningEfforts

        it "maps inherited max effort to high for Grok models" do
            normalizeReasoningEffortForDialect GrokBuildDialect EffortMax
                `shouldBe` EffortHigh
            normalizeReasoningEffortForDialect GrokBuildDialect EffortXHigh
                `shouldBe` EffortXHigh
            normalizeReasoningEffortForDialect CodexDialect EffortMax
                `shouldBe` EffortMax

    describe "isOneShot" do
        it "is true for text, prompt-file, and managed-turn-file input" do
            isOneShot defaultCliOptions `shouldBe` False
            isOneShot defaultCliOptions { optPrompt = Just "x" } `shouldBe` True
            isOneShot defaultCliOptions { optPromptFile = Just (fromFilePath "x.md") } `shouldBe` True
            isOneShot defaultCliOptions
                { optManagedTurnFile = Just (fromFilePath "turn.json") }
                `shouldBe` True

isLeft :: Either a b -> Bool
isLeft = \case
    Left _ -> True
    Right _ -> False
