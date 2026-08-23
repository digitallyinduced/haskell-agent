module Agent.TUI.ModelSpec (spec) where

import Agent.TUI.Model
import Agent.Loop
    ( LoopEvent(..)
    , emptyTurnOutput
    )
import Agent.ToolDispatch
    ( ToolCallResult(..)
    , ToolCallKind(..)
    , functionToolCall
    )
import qualified Data.Foldable as Foldable
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = describe "fullscreen UI reducer" do
    it "retains user, reasoning, and assistant blocks across a turn" do
        let state =
                apply
                    [ UiUserSubmitted "hello"
                    , UiLoop TurnStarted
                    , UiLoop (ReasoningDelta "checking")
                    , UiLoop (TextDelta "answer")
                    , UiLoop (TurnFinished (emptyTurnOutput "r1" [] (Just "answer")))
                    ]
            blocks = Foldable.toList state.uiBlocks
        map (.blockKind) blocks
            `shouldBe` [BlockUser, BlockThinking, BlockAssistant]
        map (.blockBody) blocks
            `shouldBe` ["hello", "checking", "answer"]
        state.uiRunning `shouldBe` False

    it "retains a draft composed while a turn is running" do
        let state =
                apply
                    [ UiUserSubmitted "first request"
                    , UiLoop TurnStarted
                    , UiSetDraft "follow-up while busy" 20
                    , UiLoop (ReasoningDelta "checking")
                    , UiLoop
                        (TurnFinished
                            (emptyTurnOutput "r1" [] (Just "done")))
                    , UiSetAwaitingInput True
                    ]
        state.uiDraft `shouldBe` "follow-up while busy"
        state.uiCursor `shouldBe` 20
        state.uiAwaitingInput `shouldBe` True

    it "discards partial output and updates effort when restarting a turn" do
        let initialPrompt =
                initialUiState.uiPrompt
                    { promptEffort = "low"
                    }
            state =
                apply
                    [ UiSetPrompt initialPrompt
                    , UiUserSubmitted "try this"
                    , UiLoop TurnStarted
                    , UiLoop (ReasoningDelta "partial thought")
                    , UiLoop (TextDelta "partial answer")
                    , UiSetPromptEffort "high"
                    , UiTurnRestarted
                    ]
            blocks = Foldable.toList state.uiBlocks
        map (.blockBody) blocks `shouldBe` ["try this"]
        state.uiPrompt.promptEffort `shouldBe` "high"
        state.uiRunning `shouldBe` False
        state.uiActivity `shouldBe` "Restarting…"
        state.uiNotice
            `shouldBe` Just
                (progressNotice "Restarting current turn…")

    it "clears a cancellation progress notice when the turn ends" do
        let state =
                apply
                    [ UiLoop TurnStarted
                    , UiSetNotice (Just (progressNotice "Cancelling…"))
                    , UiTurnEnded BlockCancelled
                    ]
        state.uiRunning `shouldBe` False
        state.uiActivity `shouldBe` "Ready"
        state.uiNotice `shouldBe` Nothing

    it "preserves a transient notice when the turn ends" do
        let notice = warningNotice "Connection recovered"
            state =
                apply
                    [ UiLoop TurnStarted
                    , UiSetNotice (Just notice)
                    , UiTurnEnded BlockFailed
                    ]
        state.uiNotice `shouldBe` Just notice

    it "shows provider warnings without replacing live activity" do
        let state =
                apply
                    [ UiLoop TurnStarted
                    , UiLoop (ActivityUpdated "Writing…")
                    , UiLoop
                        (WarningRaised
                            "Codex usage is low: primary 8% left.")
                    ]
        state.uiActivity `shouldBe` "Writing…"
        state.uiNotice
            `shouldBe`
                Just
                    (warningNotice
                        "Codex usage is low: primary 8% left.")

    it "matches tool completion by call id" do
        let call = functionToolCall "c1" "run_terminal_cmd" "{\"command\":\"git status\"}"
            result = ToolCallResult
                { callId = "c1"
                , output = "exit: 0\nclean"
                , callKind = FunctionCallKind
                }
            state =
                apply
                    [ UiLoop TurnStarted
                    , UiLoop (ToolStarted call)
                    , UiLoop (ToolFinished result)
                    ]
            blocks = Foldable.toList state.uiBlocks
        case blocks of
            [block] -> do
                block.blockKind `shouldBe` BlockShell
                block.blockState `shouldBe` BlockComplete
                block.blockBody `shouldBe` "exit: 0\nclean"
            _ -> expectationFailure "expected one completed tool block"

    it "renders write_stdin as a shell block" do
        let call = functionToolCall "c1" "write_stdin" "{\"session_id\":3}"
            state = apply [UiLoop TurnStarted, UiLoop (ToolStarted call)]
            blocks = Foldable.toList state.uiBlocks
        case blocks of
            [block] -> do
                block.blockKind `shouldBe` BlockShell
                block.blockTitle `shouldBe` "Continued session 3"
            _ -> expectationFailure "expected one running shell block"

    it "renders the public Grok terminal alias as a shell block" do
        let call = functionToolCall
                "c1"
                "run_terminal_command"
                "{\"command\":\"git status\"}"
            state = apply [UiLoop TurnStarted, UiLoop (ToolStarted call)]
        case Foldable.toList state.uiBlocks of
            [block] -> do
                block.blockKind `shouldBe` BlockShell
                block.blockTitle `shouldBe` "$ git status"
            _ -> expectationFailure "expected one running shell block"

    it "applies tool output snapshots only to the matching running block" do
        let first = functionToolCall "c1" "run_terminal_cmd" "{\"command\":\"first\"}"
            second = functionToolCall "c2" "run_terminal_cmd" "{\"command\":\"second\"}"
            state =
                apply
                    [ UiLoop TurnStarted
                    , UiLoop (ToolStarted first)
                    , UiLoop (ToolStarted second)
                    , UiLoop (ToolOutputUpdated "c1" "first output")
                    ]
            blocks = Foldable.toList state.uiBlocks
        map (.blockBody) blocks `shouldBe` ["first output", ""]
        map (.blockState) blocks `shouldBe` [BlockRunning, BlockRunning]

    it "replaces a live snapshot with the final tool result" do
        let call = functionToolCall "c1" "run_terminal_cmd" "{\"command\":\"work\"}"
            result = ToolCallResult
                { callId = "c1"
                , output = "exit: 0\nfinal output"
                , callKind = FunctionCallKind
                }
            state =
                apply
                    [ UiLoop TurnStarted
                    , UiLoop (ToolStarted call)
                    , UiLoop (ToolOutputUpdated "c1" "partial output")
                    , UiLoop (ToolFinished result)
                    ]
        case Foldable.toList state.uiBlocks of
            [block] -> do
                block.blockBody `shouldBe` "exit: 0\nfinal output"
                block.blockState `shouldBe` BlockComplete
            _ -> expectationFailure "expected one completed tool block"

    it "ignores tool output snapshots received after completion" do
        let call = functionToolCall "c1" "run_terminal_cmd" "{\"command\":\"work\"}"
            result = ToolCallResult
                { callId = "c1"
                , output = "exit: 0\nfinal output"
                , callKind = FunctionCallKind
                }
            state =
                apply
                    [ UiLoop TurnStarted
                    , UiLoop (ToolStarted call)
                    , UiLoop (ToolFinished result)
                    , UiLoop (ToolOutputUpdated "c1" "late output")
                    ]
        case Foldable.toList state.uiBlocks of
            [block] -> do
                block.blockBody `shouldBe` "exit: 0\nfinal output"
                block.blockState `shouldBe` BlockComplete
            _ -> expectationFailure "expected one completed tool block"

    it "preserves tool folding while live snapshots arrive" do
        let call = functionToolCall "c1" "run_terminal_cmd" "{\"command\":\"work\"}"
            started =
                apply
                    [ UiLoop TurnStarted
                    , UiLoop (ToolStarted call)
                    ]
            expanded = reduceUi UiToggleSelected started
            updated =
                reduceUi
                    (UiLoop (ToolOutputUpdated "c1" "live output"))
                    expanded
        case Foldable.toList updated.uiBlocks of
            [block] -> do
                block.blockExpanded `shouldBe` True
                block.blockBody `shouldBe` "live output"
            _ -> expectationFailure "expected one running tool block"

    it "uses elapsed milliseconds without advancing idle turn time" do
        let emptyIdle =
                advanceUiTime 67 $
                    advanceUiTime 33 $
                        apply [UiSystemMessage "startup status"]
            nonEmptyIdle =
                advanceUiTime 100 $
                    reduceUi (UiUserSubmitted "hello") initialUiState
            compactedHistory =
                reduceUi (UiHistory "compacted summary") initialUiState
            running =
                advanceUiTime 100 $
                    advanceUiTime 67 $
                        advanceUiTime 33 $
                            apply [UiLoop TurnStarted]
            finished =
                reduceUi
                    (UiLoop
                        (TurnFinished
                            (emptyTurnOutput "r1" [] Nothing)))
                    running
            after = advanceUiTime 100 finished
        emptyIdle.uiElapsedMillis `shouldBe` 0
        conversationIsEmpty emptyIdle `shouldBe` True
        conversationIsEmpty nonEmptyIdle `shouldBe` False
        conversationIsEmpty compactedHistory `shouldBe` False
        nonEmptyIdle.uiElapsedMillis `shouldBe` 0
        running.uiElapsedMillis `shouldBe` 200
        after.uiElapsedMillis `shouldBe` 200

    it "briefly settles on Finished before returning to Ready" do
        let finished =
                apply
                    [ UiLoop TurnStarted
                    , UiLoop
                        (TurnFinished
                            (emptyTurnOutput "r1" [] Nothing))
                    ]
            awaiting = reduceUi (UiSetAwaitingInput True) finished
            almostSettled = advanceUiTime 999 awaiting
            settled = advanceUiTime 1 almostSettled
        finished.uiActivity `shouldBe` "Finished"
        finished.uiCompletionRemainingMillis `shouldBe` 1000
        awaiting.uiActivity `shouldBe` "Finished"
        almostSettled.uiActivity `shouldBe` "Finished"
        settled.uiActivity `shouldBe` "Ready"
        settled.uiCompletionRemainingMillis `shouldBe` 0

    it "expires transient notices but keeps progress notices visible" do
        let transient =
                reduceUi
                    (UiSetNotice
                        (Just (successNotice "Copied selected block.")))
                    initialUiState
            visible = advanceUiTime 2999 transient
            expired = advanceUiTime 1 visible
            progress =
                reduceUi
                    (UiSetNotice (Just (progressNotice "Cancelling…")))
                    initialUiState
            stillProgress = advanceUiTime 40000 progress
        visible.uiNotice
            `shouldBe` Just (successNotice "Copied selected block.")
        uiNextDeadlineMillis transient `shouldBe` Just 3000
        uiNextDeadlineMillis visible `shouldBe` Just 1
        expired.uiNotice `shouldBe` Nothing
        stillProgress.uiNotice
            `shouldBe` Just (progressNotice "Cancelling…")
        stillProgress.uiNoticeElapsedMillis `shouldBe` 0
        uiNextDeadlineMillis stillProgress `shouldBe` Nothing

    it "does not let replacement notices inherit elapsed time" do
        let almostExpired =
                advanceUiTime 2999 $
                    reduceUi
                        (UiSetNotice (Just (successNotice "Saved.")))
                        initialUiState
            replaced = reduceUi (UiInputPromoted "send next") almostExpired
            stillVisible = advanceUiTime 1 replaced
        replaced.uiNoticeElapsedMillis `shouldBe` 0
        stillVisible.uiNotice
            `shouldBe`
                Just
                    (warningNotice
                        "Cancelling the current turn; sending this prompt next…")

    it "shows a search-replace diff while the tool is running" do
        let call =
                functionToolCall
                    "edit-1"
                    "search_replace"
                    "{\"file_path\":\"A.hs\",\"old_string\":\"old\",\"new_string\":\"new\"}"
            blocks =
                Foldable.toList $
                    (.uiBlocks) $
                        apply
                            [ UiLoop TurnStarted
                            , UiLoop (ToolStarted call)
                            ]
        case blocks of
            [block] -> do
                block.blockKind `shouldBe` BlockEdit
                block.blockBody `shouldSatisfy` Text.isInfixOf "-old"
                block.blockBody `shouldSatisfy` Text.isInfixOf "+new"
            _ -> expectationFailure "expected one running edit block"

    it "formats collaboration result JSON for display" do
        let call =
                functionToolCall
                    "c1"
                    "collaboration.spawn_agent"
                    "{\"task_name\":\"reviewer\",\"message\":\"review\"}"
            result = ToolCallResult
                { callId = "c1"
                , output = "{\"task_name\":\"/root/reviewer\",\"nickname\":null}"
                , callKind = FunctionCallKind
                }
            blocks = Foldable.toList $ (.uiBlocks) $ apply
                [ UiLoop TurnStarted
                , UiLoop (ToolStarted call)
                , UiLoop (ToolFinished result)
                ]
        case blocks of
            [block] -> do
                block.blockTitle `shouldBe` "Spawned agent reviewer"
                block.blockBody `shouldBe` "Agent: /root/reviewer"
            _ -> expectationFailure "expected one collaboration tool block"

    it "only marks structured cancellation results as cancelled" do
        toolStateFor "exit: cancelled\npartial output"
            `shouldBe` BlockCancelled
        toolStateFor "Error: Command cancelled"
            `shouldBe` BlockCancelled
        toolStateFor "exit: 0\ncancellation complete"
            `shouldBe` BlockComplete
        toolStateFor "exit: 0\ncancelled work item"
            `shouldBe` BlockComplete

    it "moves selection and toggles folding" do
        let initial =
                apply
                    [ UiUserSubmitted "one"
                    , UiUserSubmitted "two"
                    , UiMoveSelection (-1)
                    ]
            toggled = reduceUi UiToggleSelected initial
            blocks = Foldable.toList toggled.uiBlocks
        selectedBlockIndex initial `shouldBe` 0
        case blocks of
            block : _ -> block.blockExpanded `shouldBe` False
            [] -> expectationFailure "expected selected blocks"

    it "selects a clicked block and follows only at the tail" do
        let populated =
                apply
                    [ UiUserSubmitted "one"
                    , UiUserSubmitted "two"
                    , UiUserSubmitted "three"
                    ]
            blocks = Foldable.toList populated.uiBlocks
        case blocks of
            first : _ : lastBlock : [] -> do
                let firstSelected =
                        reduceUi (UiSelectBlock first.blockId) populated
                    lastSelected =
                        reduceUi (UiSelectBlock lastBlock.blockId) firstSelected
                firstSelected.uiSelectedBlock
                    `shouldBe` Just first.blockId
                firstSelected.uiFollow `shouldBe` False
                lastSelected.uiSelectedBlock
                    `shouldBe` Just lastBlock.blockId
                lastSelected.uiFollow `shouldBe` True
                let resumed = reduceUi (UiSetFollow True) firstSelected
                resumed.uiSelectedBlock
                    `shouldBe` Just lastBlock.blockId
                resumed.uiFollow `shouldBe` True
            _ -> expectationFailure "expected three blocks"

    it "queues follow-ups in order and preserves the draft typed behind them" do
        let afterFirstStarted =
                apply
                    [ UiLoop TurnStarted
                    , UiSetDraft "first follow-up" 15
                    , UiInputQueued "first follow-up"
                    , UiSetDraft "second follow-up" 16
                    , UiInputQueued "second follow-up"
                    , UiSetDraft "draft for later" 15
                    , UiQueuedInputStarted
                    , UiUserSubmitted "first follow-up"
                    ]
            afterSecondStarted =
                reduceUi
                    (UiUserSubmitted "second follow-up")
                    (reduceUi UiQueuedInputStarted afterFirstStarted)
        Foldable.toList afterFirstStarted.uiQueuedInputs
            `shouldBe` ["second follow-up"]
        afterFirstStarted.uiDraft `shouldBe` "draft for later"
        afterFirstStarted.uiCursor `shouldBe` 15
        map (.blockBody) (Foldable.toList afterFirstStarted.uiBlocks)
            `shouldBe` ["first follow-up"]
        Foldable.toList afterSecondStarted.uiQueuedInputs `shouldBe` []
        afterSecondStarted.uiDraft `shouldBe` "draft for later"
        map (.blockBody) (Foldable.toList afterSecondStarted.uiBlocks)
            `shouldBe` ["first follow-up", "second follow-up"]

    it "promotes a send-now draft ahead of existing queued inputs" do
        let state =
                apply
                    [ UiLoop TurnStarted
                    , UiInputQueued "later"
                    , UiSetDraft "send now" 8
                    , UiInputPromoted "send now"
                    ]
        Foldable.toList state.uiQueuedInputs
            `shouldBe` ["send now", "later"]
        state.uiDraft `shouldBe` ""
        state.uiCursor `shouldBe` 0
        state.uiNotice
            `shouldBe`
                Just
                    (warningNotice
                        "Cancelling the current turn; sending this prompt next…")

    it "clears only the draft that was submitted immediately" do
        let state =
                apply
                    [ UiSetAwaitingInput True
                    , UiSetDraft "send this" 9
                    , UiDraftSubmitted
                    , UiUserSubmitted "send this"
                    ]
        state.uiDraft `shouldBe` ""
        state.uiCursor `shouldBe` 0
        state.uiAwaitingInput `shouldBe` False

    it "stays busy between a model tool request and the next model round" do
        let call =
                functionToolCall
                    "busy-tool"
                    "run_terminal_cmd"
                    "{\"command\":\"sleep 1\"}"
            requestingTool =
                apply
                    [ UiLoop TurnStarted
                    , UiLoop
                        (TurnFinished
                            (emptyTurnOutput "r1" [call] Nothing))
                    ]
            afterTool =
                reduceUi
                    (UiLoop
                        (ToolFinished
                            (ToolCallResult
                                { callId = "busy-tool"
                                , output = "exit: 0"
                                , callKind = FunctionCallKind
                                })))
                    (reduceUi (UiLoop (ToolStarted call)) requestingTool)
            finished =
                reduceUi
                    (UiLoop
                        (TurnFinished
                            (emptyTurnOutput "r2" [] (Just "done"))))
                    (reduceUi (UiLoop TurnStarted) afterTool)
        requestingTool.uiRunning `shouldBe` True
        requestingTool.uiActivity `shouldBe` "Running tools…"
        afterTool.uiRunning `shouldBe` True
        finished.uiRunning `shouldBe` False
        finished.uiActivity `shouldBe` "Finished"
        finished.uiCompletionRemainingMillis `shouldBe` 1000

    it "deletes the previous word for command/option-backspace" do
        deleteWordBefore "hello brave world" 17
            `shouldBe` ("hello brave ", 12)
        deleteWordBefore "hello brave   " 14
            `shouldBe` ("hello ", 6)
        deleteWordBefore "one two three" 7
            `shouldBe` ("one three", 4)

    it "deletes to the start of the current line for ctrl-u" do
        deleteToLineStart "first\nsecond line" 12
            `shouldBe` ("first\n line", 6)
        deleteToLineStart "single line" 6
            `shouldBe` (" line", 0)

apply :: [UiEvent] -> UiState
apply events = foldl (flip reduceUi) initialUiState events

toolStateFor :: Text -> BlockState
toolStateFor output =
    let call =
            functionToolCall
                "cancel-test"
                "run_terminal_cmd"
                "{\"command\":\"echo test\"}"
        result = ToolCallResult
            { callId = "cancel-test"
            , output
            , callKind = FunctionCallKind
            }
        blocks = Foldable.toList $
            (.uiBlocks) $
                apply
                    [ UiLoop TurnStarted
                    , UiLoop (ToolStarted call)
                    , UiLoop (ToolFinished result)
                    ]
    in case blocks of
        [block] -> block.blockState
        _ -> BlockFailed
