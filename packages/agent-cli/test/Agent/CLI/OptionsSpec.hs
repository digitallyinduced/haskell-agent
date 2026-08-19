module Agent.CLI.OptionsSpec (spec) where

import Agent.CLI.Options
import Agent.Provider (Provider(..))
import Test.Hspec

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
                , "--model", "grok-4.5"
                , "--cwd", "/tmp/work"
                , "--yolo"
                , "--max-turns", "3"
                , "--effort", "high"
                , "--show-reasoning"
                , "-p", "hello"
                ]
                `shouldBe` Right (RunAgent defaultCliOptions
                    { optProvider = Just XAIProvider
                    , optModel = Just "grok-4.5"
                    , optCwd = Just "/tmp/work"
                    , optYolo = True
                    , optMaxTurns = 3
                    , optEffort = "high"
                    , optPrompt = Just "hello"
                    , optShowReasoning = True
                    })

        it "accepts xhigh effort" do
            parseArgs ["--effort", "xhigh"]
                `shouldBe` Right (RunAgent defaultCliOptions { optEffort = "xhigh" })
            parseArgs ["--effort", "HIGH"]
                `shouldBe` Right (RunAgent defaultCliOptions { optEffort = "high" })

        it "rejects unknown effort levels" do
            parseArgs ["--effort", "max"] `shouldSatisfy` isLeft

        it "accepts openrouter as a provider" do
            parseArgs ["--provider", "openrouter", "-p", "hi"]
                `shouldBe` Right (RunAgent defaultCliOptions
                    { optProvider = Just OpenRouterProvider
                    , optPrompt = Just "hi"
                    })

        it "rejects the removed openai-base-url command" do
            parseArgs ["openai-base-url"] `shouldSatisfy` isLeft

        it "rejects using both -p and --prompt-file" do
            parseArgs ["-p", "a", "--prompt-file", "b"] `shouldSatisfy` isLeft

        it "explains that login is not in this slice" do
            parseArgs ["login"] `shouldSatisfy` isLeft

    describe "resolveApprovalPolicy" do
        it "auto-approves one-shot scripts without a TTY" do
            resolveApprovalPolicy defaultCliOptions { optPrompt = Just "hi" } False
                `shouldBe` ApproveAll

        it "denies mutating tools without a TTY when --no-yolo is set" do
            resolveApprovalPolicy defaultCliOptions { optNoYolo = True } False
                `shouldBe` DenyMutating

        it "prompts on a TTY unless --yolo is set" do
            resolveApprovalPolicy defaultCliOptions True `shouldBe` PromptMutating
            resolveApprovalPolicy defaultCliOptions { optYolo = True } True
                `shouldBe` ApproveAll

    describe "isOneShot" do
        it "is true for -p and --prompt-file" do
            isOneShot defaultCliOptions `shouldBe` False
            isOneShot defaultCliOptions { optPrompt = Just "x" } `shouldBe` True
            isOneShot defaultCliOptions { optPromptFile = Just "x.md" } `shouldBe` True

isLeft :: Either a b -> Bool
isLeft = \case
    Left _ -> True
    Right _ -> False
