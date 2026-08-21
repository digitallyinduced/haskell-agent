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
