module Agent.CLI.ReplStatusSpec (spec) where

import Agent.CLI
    ( DevResult(..)
    , afterDev
    , applyReplMode
    , cycleReplInteraction
    , devArgs
    , formatReplStatusLine
    , formatRepositoryPath
    , formatStartupTimings
    , formatTokenUsage
    , withRestoredCurrentDirectory
    )
import Agent.CLI.Input (terminalTextWidth)
import Agent.CLI.Options (ApprovalPolicy(..))
import Agent.CLI.Project (ProjectSettings(..), loadProjectSettings)
import Agent.CLI.ReplMode
    ( ReplMode(..)
    , cycleReplMode
    , replModeFromState
    )
import Agent.Loop (TokenUsage(..), emptyTokenUsage)
import System.OsPath (unsafeEncodeUtf)
import Agent.Tools.PlanMode (PlanModeEnv(..), PlanModeState(..), newPlanModeEnv)
import Control.Exception.Safe (bracket, throwIO)
import Data.IORef (newIORef, readIORef)
import System.Directory
    ( getCurrentDirectory
    , getTemporaryDirectory
    , removeDirectoryRecursive
    , setCurrentDirectory
    )
import System.Posix.Temp (mkdtemp)
import Test.Hspec

fromFilePath = unsafeEncodeUtf

spec :: Spec
spec = do
    describe "devArgs" do
        it "starts fresh REPL sessions on gpt-5.6-sol in yolo mode" do
            devArgs Nothing False
                `shouldBe`
                    [ "--provider", "openai"
                    , "--model", "gpt-5.6-sol"
                    , "--yolo"
                    , "--worktree"
                    ]
            devArgs Nothing True
                `shouldBe`
                    [ "--provider", "openai"
                    , "--model", "gpt-5.6-sol"
                    , "--yolo"
                    ]

        it "keeps the session model and reapplies yolo when reloading" do
            devArgs (Just "2026-08-20-abcd1234") True
                `shouldBe`
                    [ "--yolo"
                    , "--resume", "2026-08-20-abcd1234"
                    ]

        it "carries reload state in the GHCi continuation" do
            afterDev (DevReload "2026-08-20-abcd1234")
                `shouldReturn`
                    unlines
                        [ ":reload"
                        , ":module +Agent.CLI"
                        , ":cmd afterDev =<< devMainResume (Just \"2026-08-20-abcd1234\")"
                        ]

    describe "withRestoredCurrentDirectory" do
        it "restores the GHCi cwd after normal completion" do
            original <- getCurrentDirectory
            withTempDir "agent-repl-cwd-" \temporary -> do
                withRestoredCurrentDirectory (setCurrentDirectory temporary)
                getCurrentDirectory `shouldReturn` original

        it "restores the GHCi cwd after an exception" do
            original <- getCurrentDirectory
            withTempDir "agent-repl-cwd-" \temporary -> do
                let failAfterChangingDirectory = do
                        setCurrentDirectory temporary
                        throwIO (userError "boom")
                withRestoredCurrentDirectory failAfterChangingDirectory
                    `shouldThrow` anyIOException
                getCurrentDirectory `shouldReturn` original

    describe "formatReplStatusLine" do
        it "shows model, effort, interaction mode, and active account" do
            formatReplStatusLine False Nothing "grok-4.6" "high"
                ReplModeNormal "person@example.com" emptyTokenUsage
                `shouldBe` "  grok-4.6 · high · ask · person@example.com"
            formatReplStatusLine False Nothing "gpt-5.1-codex" "medium"
                ReplModeAlwaysApprove "" emptyTokenUsage
                `shouldBe` "  gpt-5.1-codex · medium · yolo"
            formatReplStatusLine False Nothing "gpt-5.1" "low"
                ReplModePlan "" emptyTokenUsage
                `shouldBe` "  gpt-5.1 · low · plan"

        it "appends session usage when no width is known" do
            formatReplStatusLine False Nothing "grok-4.6" "high" ReplModeNormal ""
                TokenUsage { inputTokens = 1200, outputTokens = 340, cachedTokens = 0 }
                `shouldBe` "  grok-4.6 · high · ask  1.2k in · 340 out"

        it "right-aligns session usage when the TTY is wide enough" do
            formatReplStatusLine False (Just 48) "grok-4.6" "high" ReplModeNormal ""
                TokenUsage { inputTokens = 1200, outputTokens = 340, cachedTokens = 0 }
                `shouldBe` "  grok-4.6 · high · ask        1.2k in · 340 out"

        it "drops usage rather than wrapping when only the state fits" do
            formatReplStatusLine False (Just 24) "grok-4.6" "high" ReplModeNormal ""
                TokenUsage { inputTokens = 1200, outputTokens = 340, cachedTokens = 0 }
                `shouldBe` "  grok-4.6 · high · ask"

        it "truncates the state when a narrow pane cannot fit it" do
            formatReplStatusLine False (Just 20) "grok-4.6" "high" ReplModeNormal ""
                TokenUsage { inputTokens = 1200, outputTokens = 340, cachedTokens = 0 }
                `shouldBe` "  grok-4.6 · high ·…"

        it "measures and truncates wide model names in terminal columns" do
            let line =
                    formatReplStatusLine False (Just 16) "模型模型" "high"
                        ReplModeNormal "" emptyTokenUsage
            terminalTextWidth line `shouldBe` 16
            line `shouldBe` "  模型模型 · hi…"

    describe "formatStartupTimings" do
        it "sorts cumulative startup markers and keeps subsecond precision" do
            formatStartupTimings
                [ ("ready", 1.25)
                , ("first frame", 0.042)
                , ("Loading tools…", 0.4)
                ]
                `shouldBe`
                    "startup: first frame 42ms · Loading tools… 400ms · ready 1.25s"

    describe "formatRepositoryPath" do
        it "abbreviates paths below the home directory" do
            formatRepositoryPath
                (fromFilePath "/Users/marc")
                (fromFilePath "/Users/marc/src/haskell-agent")
                `shouldBe` "~/src/haskell-agent"

        it "keeps paths outside the home directory absolute" do
            formatRepositoryPath
                (fromFilePath "/Users/marc")
                (fromFilePath "/tmp/haskell-agent")
                `shouldBe` "/tmp/haskell-agent"

    describe "cycleReplMode" do
        it "walks ask → plan → always-approve → ask" do
            cycleReplMode ReplModeNormal `shouldBe` ReplModePlan
            cycleReplMode ReplModePlan `shouldBe` ReplModeAlwaysApprove
            cycleReplMode ReplModeAlwaysApprove `shouldBe` ReplModeNormal

        it "treats pending/active plan as plan even under yolo" do
            replModeFromState PlanPending ApproveAll `shouldBe` ReplModePlan
            replModeFromState PlanActive PromptMutating `shouldBe` ReplModePlan
            replModeFromState PlanInactive ApproveAll `shouldBe` ReplModeAlwaysApprove
            replModeFromState PlanInactive PromptMutating `shouldBe` ReplModeNormal
            replModeFromState PlanInactive DenyMutating `shouldBe` ReplModeNormal

        it "cycles from current plan/approval state" do
            cycleReplInteraction PlanInactive PromptMutating
                `shouldBe` ReplModePlan
            cycleReplInteraction PlanPending PromptMutating
                `shouldBe` ReplModeAlwaysApprove
            cycleReplInteraction PlanInactive ApproveAll
                `shouldBe` ReplModeNormal

        it "applies plan, yolo, then ask" $
            withTempDir "agent-mode-" \root -> do
                let rootPath = fromFilePath root
                plan <- newPlanModeEnv rootPath Nothing
                policyRef <- newIORef PromptMutating
                applyReplMode plan policyRef rootPath ReplModePlan
                readIORef plan.planStateRef `shouldReturn` PlanPending
                readIORef policyRef `shouldReturn` PromptMutating

                applyReplMode plan policyRef rootPath ReplModeAlwaysApprove
                readIORef plan.planStateRef `shouldReturn` PlanInactive
                readIORef policyRef `shouldReturn` ApproveAll
                settings <- loadProjectSettings rootPath
                settings.settingsAutoApprove `shouldBe` True

                applyReplMode plan policyRef rootPath ReplModeNormal
                readIORef plan.planStateRef `shouldReturn` PlanInactive
                readIORef policyRef `shouldReturn` PromptMutating
                settings' <- loadProjectSettings rootPath
                settings'.settingsAutoApprove `shouldBe` False

    describe "formatTokenUsage" do
        it "omits empty totals" do
            formatTokenUsage emptyTokenUsage `shouldBe` ""

        it "formats compact in/out counts" do
            formatTokenUsage TokenUsage
                { inputTokens = 42
                , outputTokens = 7
                , cachedTokens = 0
                } `shouldBe` "42 in · 7 out"

        it "includes cached tokens when present" do
            formatTokenUsage TokenUsage
                { inputTokens = 1500
                , outputTokens = 80
                , cachedTokens = 1200
                } `shouldBe` "1.5k in · 80 out · 1.2k cached"

        it "uses k/M suffixes" do
            formatTokenUsage TokenUsage
                { inputTokens = 12500
                , outputTokens = 1500000
                , cachedTokens = 0
                } `shouldBe` "13k in · 1.5M out"

withTempDir :: String -> (FilePath -> IO a) -> IO a
withTempDir prefix action = do
    tmp <- getTemporaryDirectory
    bracket (mkdtemp (tmp <> "/" <> prefix)) removeDirectoryRecursive action
