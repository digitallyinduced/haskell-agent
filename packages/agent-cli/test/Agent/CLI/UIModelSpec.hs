module Agent.CLI.UIModelSpec (spec) where

import Agent.CLI.UI.Model
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

    it "tracks elapsed tenths only while a turn is running" do
        let idle = apply [UiTick, UiTick]
            running = apply [UiLoop TurnStarted, UiTick, UiTick, UiTick]
            finished =
                reduceUi
                    (UiLoop
                        (TurnFinished
                            (emptyTurnOutput "r1" [] Nothing)))
                    running
            after = reduceUi UiTick finished
        idle.uiElapsedTenths `shouldBe` 0
        idle.uiFrame `shouldBe` 0
        running.uiElapsedTenths `shouldBe` 3
        after.uiElapsedTenths `shouldBe` 3

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
        finished.uiActivity `shouldBe` "Ready"

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
