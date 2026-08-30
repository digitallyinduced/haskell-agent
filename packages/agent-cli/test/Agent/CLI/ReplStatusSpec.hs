module Agent.CLI.ReplStatusSpec (spec) where

import Agent.CLI
    ( DevResult(..)
    , accountSwitchTarget
    , afterDev
    , applyReplMode
    , buildPromptState
    , cycleReplInteraction
    , devArgs
    , formatMcpModelNotice
    , formatMcpModelNoticeFor
    , formatMcpProgress
    , formatReplStatusLine
    , formatRepositoryPath
    , formatStartupTimings
    , formatTokenUsage
    , formatEstimatedTokensPerSecond
    , formatTokensPerSecond
    , formatUsageWithRate
    , withRestoredCurrentDirectory
    )
import Agent.CLI.Command (setModel, setReasoningEffort)
import Agent.CLI.Input (terminalTextWidth)
import Agent.CLI.Models (ModelOption(..), ModelTarget(..))
import Agent.CLI.Status (formatTokenUsageOrZero)
import Agent.CLI.ModelConfig
    ( ModelCatalog
    , decodeModelConfig
    , packagedModelCatalogPath
    )
import Agent.CLI.Options (ApprovalPolicy(..))
import Agent.CLI.Project (ProjectSettings(..), loadProjectSettings)
import Agent.CLI.ReplMode
    ( ReplMode(..)
    , cycleReplMode
    , replModeFromState
    )
import Agent.Dialect (DialectId(..))
import Agent.ReasoningEffort (ReasoningEffort(..))
import Agent.Loop (LoopEvent(..), TokenUsage(..), emptyTokenUsage)
import Agent.MCP
    ( McpInitState(..)
    , McpServerStatus(..)
    )
import Agent.Provider (Provider(..))
import Agent.Responses.Types (defaultResponseCreateParams)
import System.OsPath (OsPath, unsafeEncodeUtf)
import Agent.TUI.Model
    ( PromptState(..)
    , UiEvent(..)
    , UiState(..)
    , initialUiState
    , reduceUi
    )
import Agent.Tools.PlanMode (PlanModeEnv(..), PlanModeState(..), newPlanModeEnv)
import Control.Exception.Safe (bracket, throwIO)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import Data.IORef (newIORef, readIORef, writeIORef)
import System.Directory
    ( getCurrentDirectory
    , getTemporaryDirectory
    , removeDirectoryRecursive
    , setCurrentDirectory
    )
import System.Posix.Temp (mkdtemp)
import Test.Hspec

fromFilePath :: FilePath -> OsPath
fromFilePath = unsafeEncodeUtf

mcpStatus :: Text.Text -> McpInitState -> Int -> McpServerStatus
mcpStatus name state toolCount = McpServerStatus
    { mcpStatusName = name
    , mcpStatusState = state
    , mcpStatusToolCount = toolCount
    }

spec :: Spec
spec = do
    catalog <- runIO readPackagedCatalog
    describe "progressive MCP status" do
        it "formats connecting and ready startup progress" do
            formatMcpProgress
                [mcpStatus "slow" McpInitializing 0]
                `shouldBe` "Loading built-in tools… MCP: 1 connecting, 0 ready"
            formatMcpProgress
                [mcpStatus "fast" McpReady 2, mcpStatus "bad" (McpFailed "boom") 0]
                `shouldBe` "Loading built-in tools… MCP: 1 ready, 1 unavailable"

        it "gives the model stable discovery and invocation guidance" do
            formatMcpModelNotice
                [mcpStatus "fast" McpReady 2, mcpStatus "bad" (McpFailed "boom") 0]
                `shouldBe`
                    "<system-reminder>MCP status changed. Ready: fast. Unavailable: bad. Use mcp_search to discover currently available MCP tools and mcp_call to invoke one by its server__tool name.</system-reminder>"
            formatMcpModelNoticeFor
                GrokBuildDialect
                [mcpStatus "fast" McpReady 2]
                `shouldBe`
                    "<system-reminder>MCP status changed. Ready: fast. Use search_tool to discover currently available MCP tools and use_tool to invoke one by its server__tool name.</system-reminder>"

    describe "accountSwitchTarget" do
        it "uses the destination provider default when changing provider" do
            let target =
                    accountSwitchTarget
                        catalog
                        OpenAIProvider
                        "openai"
                        "gpt-5.6-sol"
                        "gpt-5.6-sol"
                        CodexDialect
                        XAIProvider
            target.modelTarget.targetProvider `shouldBe` XAIProvider
            target.modelTarget.targetModelId `shouldBe` "grok-4.6"
            target.modelTarget.targetDialect `shouldBe` GrokBuildDialect

        it "keeps the current model when only the account backend restarts" do
            let target =
                    accountSwitchTarget
                        catalog
                        OpenAIProvider
                        "openai"
                        "gpt-5.6-sol"
                        "gpt-5.6-sol"
                        CodexDialect
                        OpenAIProvider
            target.modelTarget.targetProvider `shouldBe` OpenAIProvider
            target.modelTarget.targetModelId `shouldBe` "gpt-5.6-sol"
            target.modelTarget.targetDialect `shouldBe` CodexDialect

        it "preserves a legacy OpenRouter dialect on an account restart" do
            let target =
                    accountSwitchTarget
                        catalog
                        OpenRouterProvider
                        "openrouter"
                        "openai/gpt-5.1"
                        "openai/gpt-5.1"
                        GrokBuildDialect
                        OpenRouterProvider
            target.modelTarget.targetProvider `shouldBe` OpenRouterProvider
            target.modelTarget.targetModelId `shouldBe` "openai/gpt-5.1"
            target.modelTarget.targetWireModelId `shouldBe` "openai/gpt-5.1"
            target.modelTarget.targetDialect `shouldBe` GrokBuildDialect

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
                ReplModeNormal "person@example.com" emptyTokenUsage Nothing
                `shouldBe` "  grok-4.6 · high · ask · person@example.com"
            formatReplStatusLine False Nothing "gpt-5.1-codex" "medium"
                ReplModeAlwaysApprove "" emptyTokenUsage Nothing
                `shouldBe` "  gpt-5.1-codex · medium · yolo"
            formatReplStatusLine False Nothing "gpt-5.1" "low"
                ReplModePlan "" emptyTokenUsage Nothing
                `shouldBe` "  gpt-5.1 · low · plan"

        it "appends session usage when no width is known" do
            formatReplStatusLine False Nothing "grok-4.6" "high" ReplModeNormal ""
                TokenUsage { inputTokens = 1200, outputTokens = 340, cachedTokens = 0 }
                Nothing
                `shouldBe` "  grok-4.6 · high · ask  1.2k ↓ · 340 ↑"

        it "appends last generation speed next to usage" do
            formatReplStatusLine False Nothing "grok-4.6" "high" ReplModeNormal ""
                TokenUsage { inputTokens = 1200, outputTokens = 340, cachedTokens = 0 }
                (Just 42)
                `shouldBe` "  grok-4.6 · high · ask  1.2k ↓ · 340 ↑ · 42 ◈/s"

        it "right-aligns session usage when the TTY is wide enough" do
            formatReplStatusLine False (Just 48) "grok-4.6" "high" ReplModeNormal ""
                TokenUsage { inputTokens = 1200, outputTokens = 340, cachedTokens = 0 }
                Nothing
                `shouldBe` "  grok-4.6 · high · ask           1.2k ↓ · 340 ↑"

        it "drops usage rather than wrapping when only the state fits" do
            formatReplStatusLine False (Just 24) "grok-4.6" "high" ReplModeNormal ""
                TokenUsage { inputTokens = 1200, outputTokens = 340, cachedTokens = 0 }
                Nothing
                `shouldBe` "  grok-4.6 · high · ask"

        it "truncates the state when a narrow pane cannot fit it" do
            formatReplStatusLine False (Just 20) "grok-4.6" "high" ReplModeNormal ""
                TokenUsage { inputTokens = 1200, outputTokens = 340, cachedTokens = 0 }
                Nothing
                `shouldBe` "  grok-4.6 · high ·…"

        it "measures and truncates wide model names in terminal columns" do
            let line =
                    formatReplStatusLine False (Just 16) "模型模型" "high"
                        ReplModeNormal "" emptyTokenUsage Nothing
            terminalTextWidth line `shouldBe` 16
            line `shouldBe` "  模型模型 · hi…"

    describe "buildPromptState" do
        it "replaces stale Grok metadata before a fallback turn starts" do
            let stalePrompt =
                    initialUiState.uiPrompt
                        { promptModel = "grok-4.6"
                        , promptEffort = "high"
                        , promptAccount = "grok@example.com"
                        }
                openAiParams =
                    setReasoningEffort EffortMedium $
                        setModel "gpt-5.6-sol" defaultResponseCreateParams
                replacement =
                    buildPromptState
                        CodexDialect
                        openAiParams
                        PlanInactive
                        PromptMutating
                        "openai@example.com"
                        True
                        emptyTokenUsage
                        0
                running =
                    reduceUi (UiLoop TurnStarted) $
                        reduceUi (UiSetPrompt replacement) $
                            reduceUi UiTurnRestarted $
                                reduceUi (UiSetPrompt stalePrompt) initialUiState
            running.uiPrompt.promptModel `shouldBe` "gpt-5.6-sol"
            running.uiPrompt.promptEffort `shouldBe` "medium"
            running.uiPrompt.promptEffortOptions
                `shouldBe` ["none", "low", "medium", "high", "xhigh", "max"]
            running.uiPrompt.promptAccount `shouldBe` "openai@example.com"

        it "omits max for the Grok effort control" do
            let prompt =
                    buildPromptState
                        GrokBuildDialect
                        (setReasoningEffort
                            EffortMax
                            defaultResponseCreateParams)
                        PlanInactive
                        PromptMutating
                        "grok@example.com"
                        True
                        emptyTokenUsage
                        0
            prompt.promptEffortOptions
                `shouldBe` ["none", "low", "medium", "high", "xhigh"]
            prompt.promptEffort `shouldBe` "high"

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
            replModeFromState PlanExitPending ApproveAll
                `shouldBe` ReplModePlan
            replModeFromState PlanInactive ApproveAll `shouldBe` ReplModeAlwaysApprove
            replModeFromState PlanInactive PromptMutating `shouldBe` ReplModeNormal
            replModeFromState PlanInactive DenyMutating `shouldBe` ReplModeNormal

        it "cycles from current plan/approval state" do
            cycleReplInteraction PlanInactive PromptMutating
                `shouldBe` ReplModePlan
            cycleReplInteraction PlanPending PromptMutating
                `shouldBe` ReplModeAlwaysApprove
            cycleReplInteraction PlanExitPending PromptMutating
                `shouldBe` ReplModePlan
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

        it "does not cycle policy while a plan review is pending" $
            withTempDir "agent-mode-" \root -> do
                let rootPath = fromFilePath root
                plan <- newPlanModeEnv rootPath Nothing
                writeIORef plan.planStateRef PlanExitPending
                policyRef <- newIORef PromptMutating
                applyReplMode
                    plan
                    policyRef
                    rootPath
                    ReplModeAlwaysApprove
                readIORef plan.planStateRef
                    `shouldReturn` PlanExitPending
                readIORef policyRef `shouldReturn` PromptMutating

    describe "formatTokenUsage" do
        it "omits empty totals" do
            formatTokenUsage emptyTokenUsage `shouldBe` ""

        it "formats fresh-session zeroes with arrows when required" do
            formatTokenUsageOrZero emptyTokenUsage `shouldBe` "0 ↓ · 0 ↑"

        it "formats compact input/output counts with arrows" do
            formatTokenUsage TokenUsage
                { inputTokens = 42
                , outputTokens = 7
                , cachedTokens = 0
                } `shouldBe` "42 ↓ · 7 ↑"

        it "includes cached tokens when present" do
            formatTokenUsage TokenUsage
                { inputTokens = 1500
                , outputTokens = 80
                , cachedTokens = 1200
                } `shouldBe` "1.5k ↓ · 80 ↑ · 1.2k ↻"

        it "uses k/M suffixes" do
            formatTokenUsage TokenUsage
                { inputTokens = 12500
                , outputTokens = 1500000
                , cachedTokens = 0
                } `shouldBe` "13k ↓ · 1.5M ↑"

    describe "formatTokensPerSecond" do
        it "uses one decimal below 10 and compact counts above" do
            formatTokensPerSecond 0.04 `shouldBe` "<0.1 ◈/s"
            formatTokensPerSecond 4.2 `shouldBe` "4.2 ◈/s"
            formatTokensPerSecond 42 `shouldBe` "42 ◈/s"
            formatTokensPerSecond 12500 `shouldBe` "13k ◈/s"

        it "marks character-derived rates as estimates" do
            formatEstimatedTokensPerSecond True 42
                `shouldBe` "~42 ◈/s"
            formatEstimatedTokensPerSecond False 42
                `shouldBe` "42 ◈/s"

        it "joins usage and rate" do
            formatUsageWithRate emptyTokenUsage (Just 42)
                `shouldBe` "42 ◈/s"
            formatUsageWithRate
                TokenUsage
                    { inputTokens = 1200
                    , outputTokens = 340
                    , cachedTokens = 0
                    }
                (Just 42)
                `shouldBe` "1.2k ↓ · 340 ↑ · 42 ◈/s"

withTempDir :: String -> (FilePath -> IO a) -> IO a
withTempDir prefix action = do
    tmp <- getTemporaryDirectory
    bracket (mkdtemp (tmp <> "/" <> prefix)) removeDirectoryRecursive action

readPackagedCatalog :: IO ModelCatalog
readPackagedCatalog = do
    path <- packagedModelCatalogPath
    bytes <- LBS.readFile path
    case decodeModelConfig "models.default.json" bytes of
        Left err -> fail (Text.unpack err)
        Right catalog -> pure catalog
