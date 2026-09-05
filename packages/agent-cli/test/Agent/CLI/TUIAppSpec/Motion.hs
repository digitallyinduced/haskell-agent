module Agent.CLI.TUIAppSpec.Motion (spec) where

import Agent.CLI.AgentViewport (AgentEntry(..))
import Agent.CLI.TUIAppSpec.AgentFixtures
import Agent.CLI.TUI.App
    ( advanceCompletionFlashes
    , completionFlashTransitions
    , completionRequiresRedraw
    , elapsedMillisSince
    , motionDemandFor
    , motionDemandForTerminalFocus
    , motionModeForTerminalFocus
    , nativeProgressKeepaliveDue
    , nextMotionSchedule
    , turnCompletionRequiresRedraw
    , uiEventRestartsMotionSchedule
    )
import Agent.CLI.TUI.Types (TerminalFocus(..))
import Agent.Loop (LoopEvent(..), emptyTurnOutput)
import Agent.ToolDispatch (ToolCallKind(..), ToolCallResult(..), functionToolCall)
import Agent.TUI.Model
import Agent.TUI.Motion
import qualified Data.Map.Strict as Map
import Test.Hspec

spec :: Spec
spec = do
    describe "motion demand" do
        it "distinguishes foreground, waiting, background, and static modes" do
            let idle =
                    reduceUi
                        (UiUserSubmitted "done")
                        initialUiState
                running =
                    reduceUi (UiLoop TurnStarted) idle
            motionDemandFor MotionFull False False False running
                `shouldBe` MotionFast
            motionDemandFor MotionFull True False False running
                `shouldBe` MotionSlow
            motionDemandFor MotionFull False True False idle
                `shouldBe` MotionSlow
            motionDemandFor MotionFull False False False idle
                `shouldBe` MotionNone
            motionDemandFor MotionFull False False False initialUiState
                `shouldBe` MotionSlow
            motionDemandFor MotionReduced False False False initialUiState
                `shouldBe` MotionNone
            motionDemandFor MotionReduced False False False running
                `shouldBe` MotionSlow
            motionDemandFor MotionOff False False False running
                `shouldBe` MotionSlow

        it "keeps semantic countdown updates active in every motion mode" do
            let countdown =
                    reduceUi
                        (UiRetryCountdown
                            "Provider unavailable.\n"
                            60000
                            ", or choose another provider.")
                        initialUiState
            motionDemandFor MotionFull False False False countdown
                `shouldBe` MotionSlow
            motionDemandFor MotionReduced False False False countdown
                `shouldBe` MotionSlow
            motionDemandFor MotionOff False False False countdown
                `shouldBe` MotionSlow

        it "suppresses cosmetic motion and slows cadence while unfocused" do
            let idle =
                    reduceUi
                        (UiUserSubmitted "done")
                        initialUiState
                running =
                    reduceUi (UiLoop TurnStarted) idle
            motionDemandForTerminalFocus
                TerminalFocused
                MotionFull
                False
                False
                False
                running
                `shouldBe` MotionFast
            motionDemandForTerminalFocus
                TerminalUnfocused
                MotionFull
                False
                True
                True
                idle
                `shouldBe` MotionNone
            motionDemandForTerminalFocus
                TerminalUnfocused
                MotionFull
                False
                False
                False
                running
                `shouldBe` MotionSlow
            motionModeForTerminalFocus TerminalFocused MotionFull
                `shouldBe` MotionFull
            motionModeForTerminalFocus TerminalFocusUnknown MotionReduced
                `shouldBe` MotionReduced
            motionModeForTerminalFocus TerminalUnfocused MotionFull
                `shouldBe` MotionOff

        it "bumps the scheduler generation on demand or timer boundaries" do
            nextMotionSchedule
                False
                MotionSlow
                160000
                (MotionSlow, 160000, 4)
                `shouldBe` (MotionSlow, 160000, 4)
            nextMotionSchedule
                True
                MotionSlow
                160000
                (MotionSlow, 160000, 4)
                `shouldBe` (MotionSlow, 160000, 5)
            nextMotionSchedule
                False
                MotionFast
                80000
                (MotionSlow, 160000, 4)
                `shouldBe` (MotionFast, 80000, 5)
            nextMotionSchedule
                False
                MotionSlow
                400000
                (MotionSlow, 500000, 4)
                `shouldBe` (MotionSlow, 400000, 5)

        it "requests one unfocused redraw when a running turn becomes idle" do
            let running = reduceUi (UiLoop TurnStarted) initialUiState
                finished =
                    reduceUi
                        (UiLoop
                            (TurnFinished
                                (emptyTurnOutput "response-1" [] Nothing)))
                        running
                continuing =
                    reduceUi
                        (UiLoop
                            (TurnFinished
                                (emptyTurnOutput
                                    "response-1"
                                    [functionToolCall "call-1" "read_file" "{}"]
                                    Nothing)))
                        running
            turnCompletionRequiresRedraw running finished `shouldBe` True
            turnCompletionRequiresRedraw running continuing `shouldBe` False
            turnCompletionRequiresRedraw finished finished `shouldBe` False

        it "requests an unfocused redraw when any child agent finishes" do
            let runningChild = childEntry 1
                sibling = childEntry 2
                finishedChild =
                    runningChild { agentStatus = "completed" }
                stillStreaming =
                    runningChild
                        { agentTranscript = ["assistant: still working"] }
            completionRequiresRedraw
                initialUiState
                [rootEntry, runningChild, sibling]
                initialUiState
                [rootEntry, finishedChild, sibling]
                `shouldBe` True
            completionRequiresRedraw
                initialUiState
                [rootEntry, runningChild, sibling]
                initialUiState
                [rootEntry, stillStreaming, sibling]
                `shouldBe` False
            completionRequiresRedraw
                initialUiState
                [rootEntry, runningChild, sibling]
                initialUiState
                [rootEntry, sibling]
                `shouldBe` True

        it "retains sub-millisecond time across clock samples" do
            elapsedMillisSince 1000000 1499999
                `shouldBe` (0, 1000000)
            elapsedMillisSince 1234567 3234999
                `shouldBe` (2, 3234567)
            elapsedMillisSince 4000000 3000000
                `shouldBe` (0, 4000000)

        it "restarts cadence when turn, notice, and promoted-input timers start" do
            let idle =
                    reduceUi
                        (UiUserSubmitted "done")
                        initialUiState
                turnStarted =
                    reduceUi (UiLoop TurnStarted) idle
                turnFinished =
                    reduceUi
                        (UiLoop
                            (TurnFinished
                                (emptyTurnOutput
                                    "response-1"
                                    []
                                    Nothing)))
                        turnStarted
                notice =
                    reduceUi
                        (UiSetNotice
                            (Just (successNotice "saved")))
                        idle
                warning =
                    reduceUi
                        (UiLoop (WarningRaised "Codex usage is low"))
                        turnStarted
                promoted =
                    reduceUi (UiInputPromoted "urgent") turnStarted
            uiEventRestartsMotionSchedule
                (UiLoop TurnStarted)
                idle
                turnStarted
                Map.empty
                `shouldBe` True
            uiEventRestartsMotionSchedule
                (UiLoop
                    (TurnFinished
                        (emptyTurnOutput "response-1" [] Nothing)))
                turnStarted
                turnFinished
                Map.empty
                `shouldBe` True
            uiEventRestartsMotionSchedule
                (UiSetNotice (Just (successNotice "saved")))
                idle
                notice
                Map.empty
                `shouldBe` True
            uiEventRestartsMotionSchedule
                (UiLoop (WarningRaised "Codex usage is low"))
                turnStarted
                warning
                Map.empty
                `shouldBe` True
            uiEventRestartsMotionSchedule
                (UiInputPromoted "urgent")
                turnStarted
                promoted
                Map.empty
                `shouldBe` True
            uiEventRestartsMotionSchedule
                (UiLoop (ActivityUpdated "still working"))
                turnStarted
                (reduceUi
                    (UiLoop (ActivityUpdated "still working"))
                    turnStarted)
                Map.empty
                `shouldBe` False

        it "refreshes native progress only after each five-second bucket" do
            let running =
                    advanceUiTime 5000 $
                        reduceUi (UiLoop TurnStarted) initialUiState
            nativeProgressKeepaliveDue False 0 running
                `shouldBe` True
            nativeProgressKeepaliveDue False 1 running
                `shouldBe` False
            nativeProgressKeepaliveDue True 0 running
                `shouldBe` False

        it "self-schedules completion flashes but disables them in off mode" do
            let idle =
                    reduceUi
                        (UiUserSubmitted "done")
                        initialUiState
            motionDemandFor MotionFull False False True idle
                `shouldBe` MotionFast
            motionDemandFor MotionReduced False False True idle
                `shouldBe` MotionSlow
            motionDemandFor MotionOff False False True idle
                `shouldBe` MotionNone

    describe "completion flashes" do
        it "detects only live-to-terminal block transitions" do
            let call =
                    functionToolCall
                        "tool-1"
                        "run_terminal_cmd"
                        "{\"command\":\"true\"}"
                running =
                    reduceUi
                        (UiLoop (ToolStarted call))
                        (reduceUi (UiLoop TurnStarted) initialUiState)
                completed =
                    reduceUi
                        (UiLoop
                            (ToolFinished
                                ToolCallResult
                                    { callId = "tool-1"
                                    , output = "exit: 0"
                                    , callKind = FunctionCallKind
                                    }))
                        running
            completionFlashTransitions running completed
                `shouldBe` [BlockId 1]
            completionFlashTransitions completed completed
                `shouldBe` []

        it "does not schedule completion flashes for inspection blocks" do
            let call =
                    functionToolCall
                        "inspect-1"
                        "read_file"
                        "{\"target_file\":\"README.md\"}"
                running =
                    reduceUi
                        (UiLoop (ToolStarted call))
                        (reduceUi (UiLoop TurnStarted) initialUiState)
                completed =
                    reduceUi
                        (UiLoop
                            (ToolFinished
                                ToolCallResult
                                    { callId = "inspect-1"
                                    , output = "contents"
                                    , callKind = FunctionCallKind
                                    }))
                        running
            completionFlashTransitions running completed
                `shouldBe` []

        it "ignores assistant streams and unsuccessful terminal states" do
            let assistantRunning =
                    reduceUi
                        (UiLoop (TextDelta "answer"))
                        (reduceUi (UiLoop TurnStarted) initialUiState)
                assistantComplete =
                    reduceUi
                        (UiLoop
                            (TurnFinished
                                (emptyTurnOutput
                                    "response-1"
                                    []
                                    (Just "answer"))))
                        assistantRunning
                call =
                    functionToolCall
                        "tool-2"
                        "run_terminal_cmd"
                        "{\"command\":\"false\"}"
                toolRunning =
                    reduceUi
                        (UiLoop (ToolStarted call))
                        (reduceUi (UiLoop TurnStarted) initialUiState)
                toolFailed =
                    reduceUi
                        (UiLoop
                            (ToolFinished
                                ToolCallResult
                                    { callId = "tool-2"
                                    , output = "Error: failed"
                                    , callKind = FunctionCallKind
                                    }))
                        toolRunning
            completionFlashTransitions
                assistantRunning
                assistantComplete
                `shouldBe` []
            completionFlashTransitions toolRunning toolFailed
                `shouldBe` []

        it "expires completion flashes from elapsed milliseconds" do
            let active = Map.singleton (BlockId 7) 400
            advanceCompletionFlashes 399 active
                `shouldBe` Map.singleton (BlockId 7) 1
            advanceCompletionFlashes 400 active
                `shouldBe` Map.empty
