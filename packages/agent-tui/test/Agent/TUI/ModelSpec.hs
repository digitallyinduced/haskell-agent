module Agent.TUI.ModelSpec (spec) where

import Agent.TUI.Model
import Agent.TUI.Presentation
    ( TodoDisplayLine(..)
    , TodoDisplayStatus(..)
    )
import Agent.Loop
    ( LoopEvent(..)
    , TokenUsage(..)
    , TurnOutput(..)
    , emptyTurnOutput
    , liveTokenRateMinMillis
    )
import Agent.ToolDispatch
    ( ToolCallResult(..)
    , ToolCallKind(..)
    , customToolCall
    , functionToolCall
    )
import Agent.Telemetry (TurnTelemetry(..))
import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = describe "fullscreen UI reducer" do
    it "cycles across all four permission choices" do
        let shown = reduceUi (UiPermissionShown "write a file") initialUiState
            moved count = iterate (reduceUi (UiPermissionMoved 1)) shown !! count
        fmap (.permissionIndex) shown.uiPermission `shouldBe` Just 0
        fmap (.permissionIndex) (moved 1).uiPermission `shouldBe` Just 1
        fmap (.permissionIndex) (moved 2).uiPermission `shouldBe` Just 2
        fmap (.permissionIndex) (moved 3).uiPermission `shouldBe` Just 3
        fmap (.permissionIndex) (moved 4).uiPermission `shouldBe` Just 0

    it "reuses one recap block instead of stacking spinners" do
        let started = reduceUi UiRecapStarted initialUiState
            ready =
                reduceUi
                    (UiRecapReady "We fixed auth retries in billing/retry.rs.")
                    started
            blocks = Foldable.toList ready.uiBlocks
        map (.blockKind) (Foldable.toList started.uiBlocks)
            `shouldBe` [BlockRecap]
        map (.blockKind) blocks `shouldBe` [BlockRecap]
        map (.blockBody) blocks
            `shouldBe` ["We fixed auth retries in billing/retry.rs."]
        map (.blockState) blocks `shouldBe` [BlockComplete]

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

    it "shows compact provider telemetry in the completion status" do
        let telemetry = TurnTelemetry
                { telemetryDurationMs = Just 1250
                , telemetryApiDurationMs = Just 1100
                , telemetryCostUsd = Just 0.0125
                , telemetryStopReason = Just "end_turn"
                , telemetryProviderTurns = Just 2
                , telemetryModels = Map.empty
                , telemetryStructuredOutput = Nothing
                }
            output =
                (emptyTurnOutput "r1" [] Nothing)
                    { providerTelemetry = Just telemetry }
            state =
                apply
                    [ UiLoop TurnStarted
                    , UiLoop (TurnFinished output)
                    ]
        state.uiActivity
            `shouldBe`
                "Finished · $0.0125 · 1.2s · 2 provider turns · stop end_turn"

    it "timestamps only newly appended user and assistant messages" do
        let existing =
                reduceUi (UiUserSubmitted "old prompt") initialUiState
            appended =
                applyFrom
                    existing
                    [ UiLoop TurnStarted
                    , UiLoop (ReasoningDelta "thinking")
                    , UiLoop (TextDelta "answer")
                    ]
            stamped =
                timestampNewMessageBlocks
                    (length existing.uiBlocks)
                    "1:53 PM"
                    appended
            blocks = Foldable.toList stamped.uiBlocks
        map (.blockTimestamp) blocks
            `shouldBe` ["", "", "1:53 PM"]

    it "selects and expands a clicked reasoning block in one action" do
        let before =
                apply
                    [ UiLoop TurnStarted
                    , UiLoop (ReasoningDelta "one\ntwo\nthree\nfour")
                    ]
        case Foldable.find
            ((== BlockThinking) . (.blockKind))
            before.uiBlocks of
            Nothing -> expectationFailure "expected a reasoning block"
            Just reasoning -> do
                let expanded =
                        reduceUi (UiActivateBlock reasoning.blockId) before
                    collapsed =
                        reduceUi
                            (UiActivateBlock reasoning.blockId)
                            expanded
                    selectedBlock :: UiState -> Maybe UiBlock
                    selectedBlock state =
                        Foldable.find
                            ((== reasoning.blockId) . (.blockId))
                            state.uiBlocks
                fmap (.blockExpanded) (selectedBlock before)
                    `shouldBe` Just False
                expanded.uiSelectedBlock `shouldBe` Just reasoning.blockId
                fmap (.blockExpanded) (selectedBlock expanded)
                    `shouldBe` Just True
                fmap (.blockExpanded) (selectedBlock collapsed)
                    `shouldBe` Just False

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

    it "updates the prompt model and account together during a provider switch" do
        let initialPrompt =
                initialUiState.uiPrompt
                    { promptModel = "fabel"
                    , promptEffort = "high"
                    , promptAccount = "Claude subscription"
                    }
            before =
                apply
                    [ UiSetPrompt initialPrompt
                    , UiSetDraft "half typed prompt" 7
                    ]
            after =
                reduceUi
                    (UiSetPromptTarget "gpt-5.6-sol" "OpenAI account")
                    before
        after.uiPrompt
            `shouldBe`
                initialPrompt
                    { promptModel = "gpt-5.6-sol"
                    , promptAccount = "OpenAI account"
                    }
        after.uiDraft `shouldBe` "half typed prompt"
        after.uiCursor `shouldBe` 7

    it "tracks current context occupancy independently of session totals" do
        let state =
                reduceUi
                    (UiSetContextUsage (Just 197000) (Just 272000))
                    initialUiState
        state.uiContextTokens `shouldBe` Just 197000
        state.uiContextWindow `shouldBe` Just 272000

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

    it "keeps block positions and selection stable across restart, append, and clear" do
        let restarted =
                apply
                    [ UiUserSubmitted "try this"
                    , UiLoop TurnStarted
                    , UiLoop (ReasoningDelta "partial thought")
                    , UiLoop (TextDelta "partial answer")
                    , UiTurnRestarted
                    ]
            appended =
                reduceUi
                    (UiSystemMessage "retrying")
                    restarted
            toggled = reduceUi UiToggleSelected appended
            cleared = reduceUi UiConversationCleared toggled
            repopulated =
                reduceUi
                    (UiUserSubmitted "fresh start")
                    cleared
        map (.blockId) (Foldable.toList restarted.uiBlocks)
            `shouldBe` [BlockId 1]
        restarted.uiSelectedBlock `shouldBe` Just (BlockId 1)
        restarted.uiSelectedBlockIndex `shouldBe` Just 0
        selectedBlockIndex restarted `shouldBe` 0
        restarted.uiNextBlockId `shouldBe` 4
        map (.blockId) (Foldable.toList appended.uiBlocks)
            `shouldBe` [BlockId 1, BlockId 4]
        appended.uiSelectedBlock `shouldBe` Just (BlockId 4)
        appended.uiSelectedBlockIndex `shouldBe` Just 1
        selectedBlockIndex appended `shouldBe` 1
        map (.blockExpanded) (Foldable.toList toggled.uiBlocks)
            `shouldBe` [True, False]
        cleared.uiBlocks `shouldBe` mempty
        cleared.uiSelectedBlock `shouldBe` Nothing
        cleared.uiSelectedBlockIndex `shouldBe` Nothing
        cleared.uiBlockIndices `shouldBe` mempty
        cleared.uiNextBlockId `shouldBe` 1
        map (.blockId) (Foldable.toList repopulated.uiBlocks)
            `shouldBe` [BlockId 1]
        repopulated.uiSelectedBlock `shouldBe` Just (BlockId 1)
        repopulated.uiSelectedBlockIndex `shouldBe` Just 0

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

    it "updates provider capacity in the prompt chrome" do
        let state =
                apply
                    [ UiLoop
                        ProviderLimitUpdated
                            { providerLimitText = "Weekly limit left: 8%"
                            , providerLimitWarning = True
                            }
                    ]
        state.uiPrompt.promptLimitStatus
            `shouldBe`
                Just PromptLimitStatus
                    { promptLimitText = "Weekly limit left: 8%"
                    , promptLimitWarning = True
                    }

    it "separates a partial response from its automatic retry" do
        let message =
                "Connection interrupted the response; restarting automatically."
            state =
                apply
                    [ UiLoop TurnStarted
                    , UiLoop (TextDelta "partial")
                    , UiLoop (ResponseRestarted message)
                    , UiLoop (TextDelta "complete")
                    ]
            blocks = Foldable.toList state.uiBlocks
        map (.blockBody) blocks `shouldBe` ["partial", "complete"]
        map (.blockState) blocks
            `shouldBe` [BlockComplete, BlockStreaming]
        state.uiActivity `shouldBe` "Writing…"
        state.uiNotice `shouldBe` Just (warningNotice message)

    it "discards every response attempt when restarting effort after recovery" do
        let initialPrompt =
                initialUiState.uiPrompt
                    { promptEffort = "low"
                    }
            state =
                apply
                    [ UiSetPrompt initialPrompt
                    , UiUserSubmitted "try this"
                    , UiLoop TurnStarted
                    , UiLoop (TextDelta "failed partial")
                    , UiLoop
                        (ResponseRestarted
                            "Connection interrupted; restarting.")
                    , UiLoop (TextDelta "retried partial")
                    , UiSetPromptEffort "high"
                    , UiTurnRestarted
                    ]
            blocks = Foldable.toList state.uiBlocks
        map (.blockBody) blocks `shouldBe` ["try this"]
        state.uiPrompt.promptEffort `shouldBe` "high"
        state.uiRunning `shouldBe` False
        state.uiActivity `shouldBe` "Restarting…"

    it "uses the retry boundary for a non-streaming response fallback" do
        let state =
                apply
                    [ UiLoop TurnStarted
                    , UiLoop (TextDelta "failed partial")
                    , UiLoop
                        (ResponseRestarted
                            "Connection interrupted; restarting.")
                    , UiLoop
                        (TurnFinished
                            (emptyTurnOutput
                                "r1" [] (Just "complete retry")))
                    ]
        map (.blockBody) (Foldable.toList state.uiBlocks)
            `shouldBe` ["failed partial", "complete retry"]

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

    it "updates an early tool start in place" do
        let early = functionToolCall "c1" "Task" "{}"
            canonical =
                functionToolCall
                    "c1"
                    "Agent"
                    "{\"description\":\"review the patch\"}"
            state =
                apply
                    [ UiLoop TurnStarted
                    , UiLoop (ToolStarted early)
                    , UiLoop (ToolUpdated canonical)
                    ]
        case Foldable.toList state.uiBlocks of
            [block] -> do
                block.blockTitle `shouldBe` "Spawned agent review the patch"
                block.blockDetail `shouldBe` ""
                block.blockState `shouldBe` BlockRunning
            _ -> expectationFailure "expected one updated tool block"
        Foldable.toList state.uiToolCalls
            `shouldBe` [(0, canonical)]

    it "repaints a running shell call as command arguments stream" do
        let early = functionToolCall "c1" "shell_command" ""
            preview =
                functionToolCall
                    "c1"
                    "shell_command"
                    "{\"command\":\"git status\"}"
            state =
                apply
                    [ UiLoop TurnStarted
                    , UiLoop (ToolStarted early)
                    , UiLoop (ToolArgumentsUpdated preview)
                    ]
        case Foldable.toList state.uiBlocks of
            [block] -> do
                block.blockKind `shouldBe` BlockShell
                block.blockTitle `shouldBe` "$ git status"
                block.blockState `shouldBe` BlockRunning
                state.uiActivity `shouldBe` "$ git status"
            _ -> expectationFailure "expected one updated shell block"
        Foldable.toList state.uiToolCalls
            `shouldBe` [(0, preview)]

    it "makes repeated tool starts idempotent by call id" do
        let early = functionToolCall "c1" "Task" "{}"
            canonical =
                functionToolCall
                    "c1"
                    "Agent"
                    "{\"description\":\"review the patch\"}"
            state =
                apply
                    [ UiLoop TurnStarted
                    , UiLoop (ToolStarted early)
                    , UiLoop (ToolStarted canonical)
                    ]
        case Foldable.toList state.uiBlocks of
            [block] -> do
                block.blockTitle `shouldBe` "Spawned agent review the patch"
                block.blockState `shouldBe` BlockRunning
            _ -> expectationFailure "expected one updated tool block"
        Foldable.toList state.uiToolCalls
            `shouldBe` [(0, canonical)]

    it "retracts a tool and repairs later tool positions" do
        let first = functionToolCall "c1" "Task" "{}"
            second = functionToolCall "c2" "Read" "{\"file_path\":\"README.md\"}"
            running =
                apply
                    [ UiLoop TurnStarted
                    , UiLoop (ToolStarted first)
                    , UiSystemMessage "between"
                    , UiLoop (ToolStarted second)
                    ]
            retracted =
                reduceUi (UiLoop (ToolRetracted "c1")) running
            finished =
                reduceUi
                    (UiLoop
                        (ToolFinished
                            ToolCallResult
                                { callId = "c2"
                                , output = "contents"
                                , callKind = FunctionCallKind
                                }))
                    retracted
        map (.blockBody) (Foldable.toList finished.uiBlocks)
            `shouldBe` ["between", "contents"]
        map (.blockState) (Foldable.toList finished.uiBlocks)
            `shouldBe` [BlockComplete, BlockComplete]
        finished.uiToolCalls `shouldBe` mempty

    it "removes a completed tool when its provider message is retracted" do
        let call = functionToolCall "c1" "Task" "{}"
            completed =
                apply
                    [ UiLoop TurnStarted
                    , UiLoop (ToolStarted call)
                    , UiLoop
                        (ToolFinished
                            ToolCallResult
                                { callId = "c1"
                                , output = "done"
                                , callKind = FunctionCallKind
                                })
                    ]
            retracted =
                reduceUi (UiLoop (ToolRetracted "c1")) completed
        retracted.uiBlocks `shouldBe` mempty
        retracted.uiToolCalls `shouldBe` mempty

    it "discards only blocks from the current response attempt" do
        let state =
                apply
                    [ UiUserSubmitted "hello"
                    , UiLoop TurnStarted
                    , UiLoop (TextDelta "partial")
                    , UiLoop
                        (ToolStarted
                            (functionToolCall "c1" "Task" "{}"))
                    , UiLoop ResponseAttemptDiscarded
                    ]
        map (.blockBody) (Foldable.toList state.uiBlocks)
            `shouldBe` ["hello"]
        state.uiToolCalls `shouldBe` mempty

    it "settles running tools when a response restarts" do
        let state =
                apply
                    [ UiLoop TurnStarted
                    , UiLoop
                        (ToolStarted
                            (functionToolCall "c1" "Task" "{}"))
                    , UiLoop (ResponseRestarted "retrying")
                    ]
        map (.blockState) (Foldable.toList state.uiBlocks)
            `shouldBe` [BlockFailed]
        state.uiToolCalls `shouldBe` mempty

    it "renders write_stdin as a shell block" do
        let call = functionToolCall "c1" "write_stdin" "{\"session_id\":3}"
            state = apply [UiLoop TurnStarted, UiLoop (ToolStarted call)]
            blocks = Foldable.toList state.uiBlocks
        case blocks of
            [block] -> do
                block.blockKind `shouldBe` BlockShell
                block.blockTitle `shouldBe` "Continued session 3"
            _ -> expectationFailure "expected one running shell block"

    it "keeps empty background-shell polls on the original command block" do
        let command =
                functionToolCall
                    "shell-1"
                    "shell_command"
                    "{\"command\":\"slow\"}"
            poll1 =
                functionToolCall
                    "poll-1"
                    "write_stdin"
                    "{\"session_id\":6,\"yield_time_ms\":1}"
            poll2 =
                functionToolCall
                    "poll-2"
                    "write_stdin"
                    "{\"session_id\":6,\"chars\":null}"
            running callId output =
                ToolCallResult
                    { callId
                    , output =
                        "Process still running.\nsession_id: 6\n" <> output
                    , callKind = FunctionCallKind
                    }
            finished =
                ToolCallResult
                    { callId = "poll-2"
                    , output = "Exit code: 0\nthird\n"
                    , callKind = FunctionCallKind
                    }
            commandRunning =
                apply
                    [ UiLoop TurnStarted
                    , UiLoop (ToolStarted command)
                    , UiLoop (ToolFinished (running "shell-1" "first\n"))
                    ]
            waiting =
                applyFrom
                    commandRunning
                    [ UiLoop TurnStarted
                    , UiLoop (ToolStarted poll1)
                    ]
            polled =
                applyFrom
                    waiting
                    [ UiLoop (ToolFinished (running "poll-1" "second\n"))
                    , UiLoop TurnStarted
                    , UiLoop (ToolStarted poll2)
                    , UiLoop (ToolFinished finished)
                    ]
        length waiting.uiBlocks `shouldBe` 1
        waiting.uiActivity `shouldBe` "Waiting for background terminal…"
        Map.lookup "poll-1" waiting.uiShellPolls `shouldBe` Just 6
        case Foldable.toList polled.uiBlocks of
            [block] -> do
                block.blockTitle `shouldBe` "$ slow"
                block.blockBody `shouldBe` "first\nsecond\nthird\n"
                block.blockState `shouldBe` BlockComplete
            _ -> expectationFailure "expected one coalesced shell block"
        polled.uiToolCalls `shouldBe` mempty
        polled.uiShellProcesses `shouldBe` mempty
        polled.uiShellPolls `shouldBe` mempty

    it "keeps the command tracked when an empty poll itself is cancelled" do
        let command =
                functionToolCall
                    "shell-1"
                    "shell_command"
                    "{\"command\":\"slow\"}"
            poll =
                functionToolCall
                    "poll-1"
                    "write_stdin"
                    "{\"session_id\":6}"
            state =
                apply
                    [ UiLoop TurnStarted
                    , UiLoop (ToolStarted command)
                    , UiLoop
                        (ToolFinished
                            ToolCallResult
                                { callId = "shell-1"
                                , output =
                                    "Process still running.\nsession_id: 6\nfirst\n"
                                , callKind = FunctionCallKind
                                })
                    , UiLoop TurnStarted
                    , UiLoop (ToolStarted poll)
                    , UiLoop
                        (ToolFinished
                            ToolCallResult
                                { callId = "poll-1"
                                , output =
                                    "Error: Poll cancelled; session 6 is still running"
                                , callKind = FunctionCallKind
                                })
                    ]
        Map.lookup 6 state.uiShellProcesses `shouldBe` Just (BlockId 1)
        state.uiShellPolls `shouldBe` mempty
        case Foldable.toList state.uiBlocks of
            [block] -> block.blockState `shouldBe` BlockRunning
            _ -> expectationFailure "expected one tracked shell block"

    it "coalesces a write_stdin call once its streamed arguments identify a poll" do
        let command =
                functionToolCall
                    "shell-1"
                    "shell_command"
                    "{\"command\":\"slow\"}"
            earlyPoll = functionToolCall "poll-1" "write_stdin" ""
            poll =
                functionToolCall
                    "poll-1"
                    "write_stdin"
                    "{\"session_id\":6}"
            commandRunning =
                apply
                    [ UiLoop TurnStarted
                    , UiLoop (ToolStarted command)
                    , UiLoop
                        (ToolFinished
                            ToolCallResult
                                { callId = "shell-1"
                                , output =
                                    "Process still running.\nsession_id: 6\n"
                                , callKind = FunctionCallKind
                                })
                    , UiLoop TurnStarted
                    , UiLoop (ToolStarted earlyPoll)
                    ]
            coalesced =
                reduceUi (UiLoop (ToolArgumentsUpdated poll)) commandRunning
        length commandRunning.uiBlocks `shouldBe` 2
        length coalesced.uiBlocks `shouldBe` 1
        Map.lookup "poll-1" coalesced.uiShellPolls `shouldBe` Just 6

    it "keeps a background command as the owner after terminal input" do
        let command =
                functionToolCall
                    "shell-1"
                    "shell_command"
                    "{\"command\":\"slow\"}"
            input =
                functionToolCall
                    "input-1"
                    "write_stdin"
                    "{\"session_id\":6,\"chars\":\"yes\\n\"}"
            running callId output =
                ToolCallResult
                    { callId
                    , output =
                        "Process still running.\nsession_id: 6\n" <> output
                    , callKind = FunctionCallKind
                    }
            state =
                apply
                    [ UiLoop TurnStarted
                    , UiLoop (ToolStarted command)
                    , UiLoop (ToolFinished (running "shell-1" "first\n"))
                    , UiLoop TurnStarted
                    , UiLoop (ToolStarted input)
                    , UiLoop (ToolFinished (running "input-1" "second\n"))
                    ]
        Map.lookup 6 state.uiShellProcesses `shouldBe` Just (BlockId 1)
        case Foldable.toList state.uiBlocks of
            [commandBlock, inputBlock] -> do
                commandBlock.blockState `shouldBe` BlockRunning
                commandBlock.blockBody `shouldBe` "first\n"
                inputBlock.blockState `shouldBe` BlockComplete
            _ -> expectationFailure "expected the command and input blocks"

    it "settles a background command when terminal input observes its exit" do
        let command =
                functionToolCall
                    "shell-1"
                    "shell_command"
                    "{\"command\":\"slow\"}"
            input =
                functionToolCall
                    "input-1"
                    "write_stdin"
                    "{\"session_id\":6,\"chars\":\"yes\\n\"}"
            running =
                ToolCallResult
                    { callId = "shell-1"
                    , output =
                        "Process still running.\nsession_id: 6\nfirst\n"
                    , callKind = FunctionCallKind
                    }
            runScenario output =
                apply
                    [ UiLoop TurnStarted
                    , UiLoop (ToolStarted command)
                    , UiLoop (ToolFinished running)
                    , UiLoop TurnStarted
                    , UiLoop (ToolStarted input)
                    , UiLoop
                        (ToolFinished
                            ToolCallResult
                                { callId = "input-1"
                                , output
                                , callKind = FunctionCallKind
                                })
                    ]
        mapM_
            (\(output, expectedState) -> do
                let state = runScenario output
                state.uiShellProcesses `shouldBe` mempty
                case Foldable.toList state.uiBlocks of
                    [commandBlock, inputBlock] -> do
                        commandBlock.blockState `shouldBe` expectedState
                        inputBlock.blockState `shouldBe` expectedState
                    _ ->
                        expectationFailure
                            "expected the command and input blocks")
            [ ("Exit code: 0\ndone\n", BlockComplete)
            , ("Exit code: 1\nfailed\n", BlockFailed)
            , ("Error: Command cancelled\n", BlockCancelled)
            ]

    it "stores GHCi source separately from its compact shell heading" do
        let call =
                functionToolCall
                    "c1"
                    "run_ghci"
                    "{\"expression\":\"do { putStrLn \\\"one\\\"; putStrLn \\\"two\\\" }\"}"
            state = apply [UiLoop TurnStarted, UiLoop (ToolStarted call)]
        case Foldable.toList state.uiBlocks of
            [block] -> do
                block.blockKind `shouldBe` BlockShell
                block.blockTitle `shouldBe` "$ ghci"
                block.blockDetail
                    `shouldBe` "do { putStrLn \"one\"; putStrLn \"two\" }"
                state.uiActivity `shouldBe` "$ ghci"
            _ -> expectationFailure "expected one running GHCi block"

    it "stores exec source as JavaScript code" do
        let source = "const answer = await tools.read_file({target_file: \"A.hs\"});"
            call = customToolCall "c1" "exec" source
            state = apply [UiLoop TurnStarted, UiLoop (ToolStarted call)]
        case Foldable.toList state.uiBlocks of
            [block] -> do
                block.blockKind `shouldBe` BlockShell
                block.blockTitle `shouldBe` "$ exec"
                block.blockDetail `shouldBe` source
                blockCodeLanguage block `shouldBe` Just "javascript"
            _ -> expectationFailure "expected one running exec block"

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

    it "tracks active tool calls by their exact block positions" do
        let first =
                functionToolCall
                    "c1"
                    "run_terminal_cmd"
                    "{\"command\":\"first\"}"
            second =
                functionToolCall
                    "c2"
                    "run_terminal_cmd"
                    "{\"command\":\"second\"}"
            running =
                apply
                    [ UiUserSubmitted "run both"
                    , UiLoop TurnStarted
                    , UiLoop (ToolStarted first)
                    , UiSystemMessage "between tools"
                    , UiLoop (ToolStarted second)
                    ]
            firstUpdated =
                reduceUi
                    (UiLoop (ToolOutputUpdated "c1" "first output"))
                    running
            secondFinished =
                reduceUi
                    (UiLoop
                        (ToolFinished
                            ToolCallResult
                                { callId = "c2"
                                , output = "exit: 0\nsecond output"
                                , callKind = FunctionCallKind
                                }))
                    firstUpdated
            blocks = Foldable.toList secondFinished.uiBlocks
        Foldable.toList running.uiToolCalls
            `shouldBe` [(1, first), (3, second)]
        map (.blockBody) blocks
            `shouldBe`
                [ "run both"
                , "first output"
                , "between tools"
                , "exit: 0\nsecond output"
                ]
        map (.blockState) blocks
            `shouldBe`
                [ BlockComplete
                , BlockRunning
                , BlockComplete
                , BlockComplete
                ]
        Foldable.toList secondFinished.uiToolCalls
            `shouldBe` [(1, first)]

    it "drops tool block positions when the conversation is cleared" do
        let call =
                functionToolCall
                    "c1"
                    "run_terminal_cmd"
                    "{\"command\":\"work\"}"
            result = ToolCallResult
                { callId = "c1"
                , output = "exit: 0\nlate final output"
                , callKind = FunctionCallKind
                }
            started =
                apply
                    [ UiLoop TurnStarted
                    , UiLoop (ToolStarted call)
                    ]
            cleared = reduceUi UiConversationCleared started
            reused =
                reduceUi
                    (UiUserSubmitted "replacement block")
                    cleared
            afterLateEvents =
                applyFrom
                    reused
                    [ UiLoop (ToolOutputUpdated "c1" "late live output")
                    , UiLoop (ToolFinished result)
                    ]
        cleared.uiToolCalls `shouldBe` mempty
        case Foldable.toList afterLateEvents.uiBlocks of
            [block] -> do
                block.blockKind `shouldBe` BlockUser
                block.blockBody `shouldBe` "replacement block"
                block.blockState `shouldBe` BlockComplete
            _ -> expectationFailure "expected one replacement block"

    it "drops tool block positions when the current turn is restarted" do
        let call =
                functionToolCall
                    "c1"
                    "run_terminal_cmd"
                    "{\"command\":\"work\"}"
            result = ToolCallResult
                { callId = "c1"
                , output = "exit: 0\nlate final output"
                , callKind = FunctionCallKind
                }
            started =
                apply
                    [ UiUserSubmitted "run work"
                    , UiLoop TurnStarted
                    , UiLoop (ToolStarted call)
                    ]
            restarted = reduceUi UiTurnRestarted started
            reused =
                reduceUi
                    (UiSystemMessage "replacement block")
                    restarted
            afterLateEvents =
                applyFrom
                    reused
                    [ UiLoop (ToolOutputUpdated "c1" "late live output")
                    , UiLoop (ToolFinished result)
                    ]
        restarted.uiToolCalls `shouldBe` mempty
        case Foldable.toList afterLateEvents.uiBlocks of
            [userBlock, replacementBlock] -> do
                userBlock.blockBody `shouldBe` "run work"
                replacementBlock.blockKind `shouldBe` BlockSystem
                replacementBlock.blockBody `shouldBe` "replacement block"
                replacementBlock.blockState `shouldBe` BlockComplete
            _ -> expectationFailure "expected retained and replacement blocks"

    it "drops tool block positions when a running turn ends" do
        let call =
                functionToolCall
                    "c1"
                    "run_terminal_cmd"
                    "{\"command\":\"work\"}"
            result = ToolCallResult
                { callId = "c1"
                , output = "exit: 0\nlate final output"
                , callKind = FunctionCallKind
                }
            ended =
                reduceUi (UiTurnEnded BlockCancelled) $
                    apply
                        [ UiLoop TurnStarted
                        , UiLoop (ToolStarted call)
                        ]
            afterLateEvents =
                applyFrom
                    ended
                    [ UiLoop (ToolOutputUpdated "c1" "late live output")
                    , UiLoop (ToolFinished result)
                    ]
        ended.uiToolCalls `shouldBe` mempty
        case Foldable.toList afterLateEvents.uiBlocks of
            [block] -> do
                block.blockBody `shouldBe` ""
                block.blockState `shouldBe` BlockCancelled
            _ -> expectationFailure "expected one cancelled tool block"

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

    it "reports live and completed tokens per second" do
        let call = functionToolCall "c1" "read_file" "{\"target_file\":\"A.hs\"}"
            streaming =
                advanceUiTime liveTokenRateMinMillis $
                    apply
                        [ UiLoop TurnStarted
                        , UiLoop (TextDelta "abcdefghijklmnop")
                        ]
            lastDelta =
                reduceUi (UiLoop (TextDelta "qrst")) streaming
            awaitingCompletion = advanceUiTime 1600 lastDelta
            reported usage tools =
                (emptyTurnOutput "r1" tools (Just "abcdefghijklmnop"))
                    { tokenUsage = usage }
            usage =
                TokenUsage
                    { inputTokens = 20
                    , outputTokens = 80
                    , cachedTokens = 0
                    }
            finished =
                reduceUi
                    (UiLoop (TurnFinished (reported usage [])))
                    awaitingCompletion
            duringTools =
                reduceUi (UiLoop (ToolStarted call)) $
                    reduceUi
                        (UiLoop (TurnFinished (reported usage [call])))
                        awaitingCompletion
            afterNext = reduceUi (UiLoop TurnStarted) finished
        uiTokensPerSecond streaming `shouldBe` Just 10
        uiTokensPerSecondEstimated streaming `shouldBe` True
        uiTokensPerSecond finished `shouldBe` Just 40
        uiTokensPerSecondEstimated finished `shouldBe` False
        finished.uiGenerating `shouldBe` False
        finished.uiGenerationMillis `shouldBe` liveTokenRateMinMillis
        finished.uiResponseMillis `shouldBe` 2000
        uiTokensPerSecond duringTools `shouldBe` Just 40
        duringTools.uiGenerating `shouldBe` False
        duringTools.uiGenerationMillis `shouldBe` liveTokenRateMinMillis
        (advanceUiTime 5000 duringTools).uiGenerationMillis
            `shouldBe` liveTokenRateMinMillis
        uiTokensPerSecond afterNext `shouldBe` Just 40
        afterNext.uiGenerationMillis `shouldBe` 0
        afterNext.uiResponseMillis `shouldBe` 0

    it "drops the live estimate when completed token metadata is missing" do
        let streaming =
                advanceUiTime liveTokenRateMinMillis $
                    foldl
                        (flip reduceUi)
                        initialUiState
                            { uiLastTokensPerSecond = Just 42 }
                        [ UiLoop TurnStarted
                        , UiLoop (TextDelta "abcdefghijklmnop")
                        ]
            finished =
                reduceUi
                    (UiLoop
                        (TurnFinished
                            (emptyTurnOutput "r1" [] (Just "abcdefghijklmnop"))))
                    streaming
        uiTokensPerSecond streaming `shouldBe` Just 10
        uiTokensPerSecondEstimated streaming `shouldBe` True
        uiTokensPerSecond finished `shouldBe` Nothing
        uiTokensPerSecondEstimated finished `shouldBe` False

    it "excludes time to first token only from the live estimate" do
        let waiting =
                advanceUiTime 5000 $
                    reduceUi (UiLoop TurnStarted) initialUiState
            streaming =
                advanceUiTime liveTokenRateMinMillis $
                    reduceUi
                        (UiLoop (TextDelta "abcdefghijklmnop"))
                        waiting
            turn =
                (emptyTurnOutput "r1" [] (Just "abcdefghijklmnop"))
                    { tokenUsage =
                        TokenUsage
                            { inputTokens = 20
                            , outputTokens = 108
                            , cachedTokens = 0
                            }
                    }
            finished = reduceUi (UiLoop (TurnFinished turn)) streaming
        waiting.uiGenerating `shouldBe` False
        waiting.uiGenerationMillis `shouldBe` 0
        uiTokensPerSecond waiting `shouldBe` Nothing
        streaming.uiGenerationMillis `shouldBe` liveTokenRateMinMillis
        uiTokensPerSecond streaming `shouldBe` Just 10
        streaming.uiResponseMillis `shouldBe` 5400
        uiTokensPerSecond finished `shouldBe` Just 20
        uiTokensPerSecondEstimated finished `shouldBe` False

    it "keeps turn elapsed time but resets generation timing when a response restarts" do
        let streaming =
                advanceUiTime 800 $
                    apply
                        [ UiLoop TurnStarted
                        , UiLoop (TextDelta "abcdefghijklmnop")
                        ]
            restarted =
                reduceUi (UiLoop (ResponseRestarted "retrying")) streaming
            waiting = advanceUiTime 1200 restarted
        streaming.uiElapsedMillis `shouldBe` 800
        restarted.uiElapsedMillis `shouldBe` 800
        restarted.uiGenerationMillis `shouldBe` 0
        restarted.uiGenerationChars `shouldBe` 0
        restarted.uiResponseMillis `shouldBe` 0
        restarted.uiGenerating `shouldBe` False
        restarted.uiActivity `shouldBe` "Retrying response…"
        waiting.uiElapsedMillis `shouldBe` 2000
        waiting.uiGenerationMillis `shouldBe` 0
        waiting.uiResponseMillis `shouldBe` 1200

    it "drops last tok/s when the conversation is cleared" do
        let streaming =
                advanceUiTime liveTokenRateMinMillis $
                    apply
                        [ UiLoop TurnStarted
                        , UiLoop (TextDelta "abcdefghijklmnop")
                        ]
            finished =
                reduceUi
                    (UiLoop
                        (TurnFinished
                            ((emptyTurnOutput "r1" [] (Just "abcdefghijklmnop"))
                                { tokenUsage =
                                    TokenUsage
                                        { inputTokens = 20
                                        , outputTokens = 80
                                        , cachedTokens = 0
                                        }
                                })))
                    (reduceUi (UiLoop (TextDelta "qrst")) streaming)
            cleared = reduceUi UiConversationCleared finished
        uiTokensPerSecond finished `shouldBe` Just 200
        uiTokensPerSecond cleared `shouldBe` Nothing
        cleared.uiLastTokensPerSecond `shouldBe` Nothing
        cleared.uiGenerationChars `shouldBe` 0
        cleared.uiGenerating `shouldBe` False

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

    it "updates retry errors in place until the deadline reaches now" do
        let started =
                reduceUi
                    (UiRetryCountdown
                        "Provider unavailable.\n"
                        61000
                        ", or choose another provider.")
                    initialUiState
            afterSecond = advanceUiTime 1000 started
            finished = advanceUiTime 60000 afterSecond
            bodies =
                map (.blockBody) . Foldable.toList . (.uiBlocks)
        bodies started
            `shouldBe`
                [ "Provider unavailable.\n\
                  \Try again in 1m01s, or choose another provider."
                ]
        bodies afterSecond
            `shouldBe`
                [ "Provider unavailable.\n\
                  \Try again in 1m00s, or choose another provider."
                ]
        bodies finished
            `shouldBe`
                [ "Provider unavailable.\n\
                  \Try again now, or choose another provider."
                ]
        uiNeedsTick started `shouldBe` True
        uiNextDeadlineMillis started `shouldBe` Just 1000
        finished.uiRetryCountdown `shouldBe` Nothing
        uiNeedsTick finished `shouldBe` False

    it "keeps worktree-relative diffs after successful edits" do
        let workspace =
                "/Users/marc/.haskell-agent/worktrees/haskell-agent/wt"
            path = workspace <> "/nix/modules/telegram.nix"
            call =
                functionToolCall
                    "edit-abs"
                    "search_replace"
                    ("{\"file_path\":\""
                        <> path
                        <> "\",\"old_string\":\"old\",\"new_string\":\"new\"}")
            started =
                apply
                    [ UiSetRepository "main" "~/project" workspace
                    , UiLoop TurnStarted
                    , UiLoop (ToolStarted call)
                    ]
            finished =
                reduceUi
                    (UiLoop
                        (ToolFinished
                            ToolCallResult
                                { callId = "edit-abs"
                                , output =
                                    "The file "
                                        <> path
                                        <> " has been updated successfully."
                                , callKind = FunctionCallKind
                                }))
                    started
        case Foldable.toList started.uiBlocks of
            [block] ->
                block.blockTitle
                    `shouldBe` "Edited nix/modules/telegram.nix"
            _ -> expectationFailure "expected one running edit block"
        case Foldable.toList finished.uiBlocks of
            [block] -> do
                block.blockBody
                    `shouldBe` "  -old\n  +new"
                block.blockState `shouldBe` BlockComplete
            _ -> expectationFailure "expected one completed edit block"

    it "retains inspection tools as a distinct completed block kind" do
        let call =
                functionToolCall
                    "read-1"
                    "Read"
                    "{\"file_path\":\"src/Main.hs\"}"
            result =
                ToolCallResult
                    { callId = "read-1"
                    , output = "module Main where"
                    , callKind = FunctionCallKind
                    }
            state =
                apply
                    [ UiLoop TurnStarted
                    , UiLoop (ToolStarted call)
                    , UiLoop (ToolFinished result)
                    ]
        case Foldable.toList state.uiBlocks of
            [block] -> do
                block.blockKind `shouldBe` BlockInspect
                block.blockState `shouldBe` BlockComplete
            _ -> expectationFailure "expected one completed inspection block"

    it "shows an apply_patch diff while running and after completion" do
        let call =
                customToolCall
                    "patch-1"
                    "apply_patch"
                    "*** Begin Patch\n\
                    \*** Update File: A.hs\n\
                    \@@\n\
                    \-old\n\
                    \+new\n\
                    \*** End Patch"
            started =
                apply
                    [ UiLoop TurnStarted
                    , UiLoop (ToolStarted call)
                    ]
            finished =
                reduceUi
                    (UiLoop
                        (ToolFinished
                            ToolCallResult
                                { callId = "patch-1"
                                , output = "Updated A.hs"
                                , callKind = CustomCallKind
                                }))
                    started
        case Foldable.toList started.uiBlocks of
            [block] -> do
                block.blockKind `shouldBe` BlockEdit
                block.blockBody `shouldSatisfy` Text.isInfixOf "-old"
                block.blockBody `shouldSatisfy` Text.isInfixOf "+new"
            _ -> expectationFailure "expected one running edit block"
        case Foldable.toList finished.uiBlocks of
            [block] -> do
                block.blockBody `shouldBe` "  -old\n  +new"
                block.blockState `shouldBe` BlockComplete
            _ -> expectationFailure "expected one completed edit block"

    it "replaces a preview with the error when an edit fails" do
        let call =
                functionToolCall
                    "edit-failed"
                    "search_replace"
                    "{\"file_path\":\"A.hs\",\"old_string\":\"old\",\"new_string\":\"new\"}"
            failed =
                apply
                    [ UiLoop TurnStarted
                    , UiLoop (ToolStarted call)
                    , UiLoop
                        (ToolFinished
                            ToolCallResult
                                { callId = "edit-failed"
                                , output = "Error: stale edit"
                                , callKind = FunctionCallKind
                                })
                    ]
        case Foldable.toList failed.uiBlocks of
            [block] -> do
                block.blockBody `shouldBe` "Error: stale edit"
                block.blockState `shouldBe` BlockFailed
            _ -> expectationFailure "expected one failed edit block"

    it "keeps todo_write in a live panel instead of scrollback" do
        let call =
                functionToolCall
                    "todo-1"
                    "todo_write"
                    "{\"todos\":[{\"id\":\"1\",\"content\":\"Find and clone repos\",\"status\":\"pending\"}]}"
            result = ToolCallResult
                { callId = "todo-1"
                , output =
                    "- [completed] 1: Find and clone repos\n\
                    \- [in_progress] 2: Investigate Grok Build"
                , callKind = FunctionCallKind
                }
            started =
                apply
                    [ UiLoop TurnStarted
                    , UiLoop (ToolStarted call)
                    ]
            finished =
                apply
                    [ UiLoop TurnStarted
                    , UiLoop (ToolStarted call)
                    , UiLoop (ToolFinished result)
                    ]
            done =
                apply
                    [ UiLoop TurnStarted
                    , UiLoop (ToolStarted call)
                    , UiLoop (ToolFinished result
                        { output = "- [completed] 1: Find and clone repos"
                        })
                    ]
        Foldable.toList started.uiBlocks `shouldBe` []
        started.uiActivity `shouldBe` "todo_write"
        visibleTodoList started `shouldBe` []
        Foldable.toList finished.uiBlocks `shouldBe` []
        map (.todoLineText) (visibleTodoList finished)
            `shouldBe` ["Find and clone repos", "Investigate Grok Build"]
        map (.todoLineStatus) (visibleTodoList finished)
            `shouldBe` [TodoDisplayCompleted, TodoDisplayInProgress]
        Foldable.toList done.uiBlocks `shouldBe` []
        visibleTodoList done `shouldBe` []
        done.uiTodos
            `shouldBe` [TodoDisplayLine TodoDisplayCompleted "Find and clone repos"]

    it "hides pending todos once the turn is idle" do
        let call =
                functionToolCall
                    "todo-1"
                    "todo_write"
                    "{\"todos\":[{\"id\":\"1\",\"content\":\"Inspect Model.hs\"}]}"
            result = ToolCallResult
                { callId = "todo-1"
                , output =
                    "- [completed] 1: Open the file\n\
                    \- [pending] 2: Inspect Model.hs"
                , callKind = FunctionCallKind
                }
            running =
                apply
                    [ UiLoop TurnStarted
                    , UiLoop (ToolStarted call)
                    , UiLoop (ToolFinished result)
                    ]
            idle =
                apply
                    [ UiLoop TurnStarted
                    , UiLoop (ToolStarted call)
                    , UiLoop (ToolFinished result)
                    , UiLoop (TurnFinished (emptyTurnOutput "r1" [] Nothing))
                    ]
        map (.todoLineText) (visibleTodoList running)
            `shouldBe` ["Open the file", "Inspect Model.hs"]
        visibleTodoList idle `shouldBe` []
        map (.todoLineText) idle.uiTodos
            `shouldBe` ["Open the file", "Inspect Model.hs"]

    it "does not let ordinary tool output clobber the live todo list" do
        let todoCall =
                functionToolCall
                    "todo-1"
                    "todo_write"
                    "{\"todos\":[{\"id\":\"1\",\"content\":\"Keep this list\"}]}"
            todoResult = ToolCallResult
                { callId = "todo-1"
                , output = "- [in_progress] 1: Keep this list"
                , callKind = FunctionCallKind
                }
            shellCall =
                functionToolCall "shell-1" "run_terminal_cmd" "{\"command\":\"ls\"}"
            shellResult = ToolCallResult
                { callId = "shell-1"
                , output = "- [pending] 99: spoofed checklist from stdout"
                , callKind = FunctionCallKind
                }
            state =
                apply
                    [ UiLoop TurnStarted
                    , UiLoop (ToolStarted todoCall)
                    , UiLoop (ToolFinished todoResult)
                    , UiLoop (ToolStarted shellCall)
                    , UiLoop (ToolFinished shellResult)
                    ]
        map (.todoLineText) (visibleTodoList state)
            `shouldBe` ["Keep this list"]
        map (.blockKind) (Foldable.toList state.uiBlocks)
            `shouldBe` [BlockShell]

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

    it "shows steering as part of the active turn and clears the draft" do
        let state =
                apply
                    [ UiLoop TurnStarted
                    , UiSetDraft "keep the schema" 15
                    , UiInputSteered "keep the schema"
                    ]
        state.uiRunning `shouldBe` True
        state.uiDraft `shouldBe` ""
        Foldable.toList state.uiQueuedInputs `shouldBe` []
        map (.blockBody) (Foldable.toList state.uiBlocks)
            `shouldBe` ["keep the schema"]
        (.noticeText) <$> state.uiNotice
            `shouldBe` Just "Steering the current turn…"

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

applyFrom :: UiState -> [UiEvent] -> UiState
applyFrom = foldl (flip reduceUi)

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
