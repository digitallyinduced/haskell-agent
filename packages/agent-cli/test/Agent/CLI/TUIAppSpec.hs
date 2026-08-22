module Agent.CLI.TUIAppSpec (spec) where

import Agent.CLI.AgentViewport (AgentEntry(..), AgentTarget(..))
import Agent.CLI.TUI.App
    ( advanceCompletionFlashes
    , agentEntryWindow
    , agentPaneEntryLimit
    , agentPaneVisible
    , completionFlashTransitions
    , conversationScrollbarRenderer
    , elapsedMillisSince
    , fullscreenVtyConfig
    , motionDemandFor
    , nativeProgressKeepaliveDue
    , nextMotionSchedule
    , repositoryHeaderText
    , uiEventRestartsMotionSchedule
    )
import Agent.Loop (LoopEvent(..), emptyTurnOutput)
import Brick
    ( VScrollbarRenderer(..)
    , hLimit
    , renderWidget
    , vLimit
    )
import Agent.Subagents (SubagentId(..))
import Agent.ToolDispatch
    ( ToolCallKind(..)
    , ToolCallResult(..)
    , functionToolCall
    )
import Agent.TUI.Model
import Agent.TUI.Motion
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Graphics.Vty as V
import Test.Hspec

spec :: Spec
spec = do
    describe "fullscreenVtyConfig" do
        it "maps enhanced-keyboard Shift+Enter sequences before Vty decodes them" do
            V.configInputMap fullscreenVtyConfig
                `shouldMatchList`
                    [ ( Nothing
                      , "\ESC[27;2;13~"
                      , V.EvKey V.KEnter [V.MShift]
                      )
                    , ( Nothing
                      , "\ESC[13;2u"
                      , V.EvKey V.KEnter [V.MShift]
                      )
                    ]

    describe "repositoryHeaderText" do
        it "puts the git state before the full checkout path" do
            repositoryHeaderText
                "detached"
                "~/digitallyinduced/haskell-agent"
                `shouldBe`
                    "detached  ~/digitallyinduced/haskell-agent"

        it "still renders a path when git state is unavailable" do
            repositoryHeaderText "" "~/scratch"
                `shouldBe` "~/scratch"

    describe "Agents pane layout" do
        it "hides below the responsive breakpoint and without children" do
            agentPaneVisible 71 20 [rootEntry, childEntry 1]
                `shouldBe` False
            agentPaneVisible 72 20 [rootEntry, childEntry 1]
                `shouldBe` True
            agentPaneVisible 120 9 [rootEntry, childEntry 1]
                `shouldBe` False
            agentPaneVisible 120 20 [rootEntry]
                `shouldBe` False

        it "centers the selected row and reports hidden rows on both sides" do
            let entries = rootEntry : map childEntry [1 .. 6]
                selected = AgentChild (SubagentId "agent-4")
                (above, shown, below) =
                    agentEntryWindow 3 selected entries
            above `shouldBe` 3
            map (.agentTarget) shown
                `shouldBe`
                    [ AgentChild (SubagentId "agent-3")
                    , selected
                    , AgentChild (SubagentId "agent-5")
                    ]
            below `shouldBe` 1

        it "reserves height for truncation indicators and pane chrome" do
            let availableHeight = 15
                entries = rootEntry : map childEntry [1 .. 20]
                selected = AgentChild (SubagentId "agent-10")
                entryLimit = agentPaneEntryLimit availableHeight
                (above, shown, below) =
                    agentEntryWindow entryLimit selected entries
                indicatorRows =
                    fromEnum (above > 0) + fromEnum (below > 0)
                renderedRows =
                    length shown + indicatorRows + 7
            entryLimit `shouldBe` 6
            renderedRows `shouldSatisfy` (<= availableHeight)

    describe "conversation scrollbar" do
        it "uses a visible trough that repaints old thumb cells" do
            let renderCell widget =
                    V.picImage $
                        renderWidget Nothing
                            [hLimit 1 (vLimit 1 widget)]
                            (1, 1)
            renderCell
                (conversationScrollbarRenderer @()).renderVScrollbarTrough
                `shouldBe` V.char V.defAttr '│'
            renderCell
                (conversationScrollbarRenderer @()).renderVScrollbar
                `shouldBe` V.char V.defAttr '┃'

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

rootEntry :: AgentEntry
rootEntry = AgentEntry
    { agentTarget = AgentRoot
    , agentPath = "/root"
    , agentStatus = "active"
    , agentSteps = []
    , agentTranscript = []
    }

childEntry :: Int -> AgentEntry
childEntry index = AgentEntry
    { agentTarget = AgentChild (SubagentId name)
    , agentPath = "/root/" <> name
    , agentStatus = "running"
    , agentSteps = []
    , agentTranscript = []
    }
  where
    name :: Text
    name = "agent-" <> Text.pack (show index)
