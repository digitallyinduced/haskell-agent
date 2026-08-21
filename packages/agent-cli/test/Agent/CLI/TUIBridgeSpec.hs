module Agent.CLI.TUIBridgeSpec (spec) where

import Agent.CLI.AgentViewport (AgentEntry(..), AgentTarget(..))
import Agent.CLI.TUI.Bridge
import Agent.CLI.UI.Model
import Agent.Loop (LoopEvent(..), emptyTurnOutput)
import Agent.Subagents (SubagentId(..))
import Agent.ToolDispatch (functionToolCall)
import Test.Hspec

spec :: Spec
spec = describe "fullscreen TUI bridge" do
    it "follows retained output events but not draft-only events" do
        eventFollows (UiSystemMessage "copied") `shouldBe` True
        eventFollows (UiErrorMessage "failed") `shouldBe` True
        eventFollows (UiSetDraft "draft" 5) `shouldBe` False

    it "starts, refreshes, and clears native terminal progress" do
        let running = reduceUi (UiLoop TurnStarted) initialUiState
            refresh = running { uiFrame = 0 }
            toolCall =
                functionToolCall
                    "tool-1"
                    "run_terminal_cmd"
                    "{\"command\":\"sleep 1\"}"
            continuing =
                reduceUi
                    (UiLoop
                        (TurnFinished
                            (emptyTurnOutput "r1" [toolCall] Nothing)))
                    running
            finished =
                reduceUi
                    (UiLoop
                        (TurnFinished
                            (emptyTurnOutput "r2" [] Nothing)))
                    running
        nativeProgressSignal (UiLoop TurnStarted) running
            `shouldBe` Just True
        nativeProgressSignal UiTick refresh
            `shouldBe` Just True
        nativeProgressSignal
            (UiLoop (TurnFinished (emptyTurnOutput "r1" [] Nothing)))
            finished
            `shouldBe` Just False
        nativeProgressSignal
            (UiLoop (TurnFinished (emptyTurnOutput "r1" [toolCall] Nothing)))
            continuing
            `shouldBe` Just True
        nativeProgressSignal (UiTurnEnded BlockCancelled) running
            `shouldBe` Just False

    it "moves through history and restores the original draft" do
        historyMove 1 ["new", "old"] Nothing "draft" ""
            `shouldBe` ("new", Just 0, "draft")
        historyMove 1 ["new", "old"] (Just 0) "new" "draft"
            `shouldBe` ("old", Just 1, "draft")
        historyMove (-1) ["new", "old"] (Just 0) "new" "draft"
            `shouldBe` ("draft", Nothing, "draft")

    it "falls back to root when the selected agent disappears" do
        let child = AgentChild (SubagentId "child")
            root = AgentEntry AgentRoot "/root" "active" []
        normalizeAgentSelection child [root] `shouldBe` AgentRoot
        normalizeAgentSelection AgentRoot [root] `shouldBe` AgentRoot
