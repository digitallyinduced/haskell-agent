module Agent.CLI.TurnSpec (spec) where

import Agent.CLI.Turn
    ( grokFirstTurnPrefix
    , grokFrameLastUserInput
    , grokUserQuery
    , restorePlanStateAfterIncomplete
    , takeGrokFirstTurnContext
    )
import Agent.CLI.TurnState
import Agent.CLI.Compaction (AutomaticCompactionBoundary(..))
import Agent.Error (ApiError(..))
import Agent.Loop
    ( ImageAttachment(..)
    , LoopError(..)
    , LoopEvent(..)
    , LoopExecution(..)
    , LoopProgress(..)
    , TokenUsage(..)
    , TurnAttachment(..)
    , TurnCompletion(..)
    , TurnInput(..)
    , TurnOutput(..)
    , emptyTurnOutput
    , userMessageWithAttachments
    )
import Agent.Responses.LoopBackend (toolResultToItem, turnInputsToItems)
import Agent.Responses.Types
    ( FunctionCall(..)
    , FunctionCallOutput(..)
    , ItemStatus(..)
    , ResponseContentPart(..)
    , ResponseItem(..)
    , ResponseMessage(..)
    , MessageContent(..)
    , ResponseRole(..)
    )
import Agent.ToolDispatch
    ( ToolCallKind(..)
    , ToolCallResult(..)
    , functionToolCall
    )
import Agent.Tools.PlanMode
    ( PlanModeEnv(..)
    , PlanModeState(..)
    , activatePlanMode
    , newPlanModeEnv
    )
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Calendar (fromGregorian)
import System.OsPath (unsafeEncodeUtf)
import Test.Hspec

spec :: Spec
spec = do
    describe "turnInputsWithContext" do
        it "orders plan-mode, task-plan, startup, and submitted inputs" do
            turnInputsWithContext
                (Just "plan reminder")
                (Just "task plan")
                (Just "startup instructions")
                [UserMessage "fix it"]
                `shouldBe`
                    [ UserMessage "plan reminder"
                    , UserMessage "task plan"
                    , UserMessage "startup instructions"
                    , UserMessage "fix it"
                    ]

    describe "uncommittedDisplayItems" do
        it "normalizes failed text, tools, and retry attempts for history only" do
            let call =
                    functionToolCall
                        "c1"
                        "shell_command"
                        "{\"command\":\"git status\"}"
                execution =
                    (uncommittedExecution prepared)
                        { executionUncommittedDisplayEvents =
                            [ TextDelta "first attempt"
                            , ToolStarted call
                            , ToolOutputUpdated "c1" "running"
                            , ToolFinished
                                (ToolCallResult
                                    "c1"
                                    "clean"
                                    FunctionCallKind)
                            , ResponseRestarted "retrying"
                            , TextDelta "second attempt"
                            ]
                        }
            case uncommittedDisplayItems execution of
                [ MessageItem first
                    , FunctionCallItem storedCall
                    , FunctionCallOutputItem output
                    , boundary
                    , MessageItem second
                    ] -> do
                        first.content
                            `shouldBe`
                                MessageContentParts
                                    [ OutputTextPart
                                        "first attempt"
                                        Nothing
                                        Nothing
                                    ]
                        storedCall.callId `shouldBe` "c1"
                        output.callId `shouldBe` "c1"
                        output.status `shouldBe` Just ItemCompleted
                        boundary `shouldSatisfy` isDisplayAttemptBoundary
                        second.content
                            `shouldBe`
                                MessageContentParts
                                    [ OutputTextPart
                                        "second attempt"
                                        Nothing
                                        Nothing
                                    ]
                other ->
                    expectationFailure
                        ("unexpected display projection: " <> show other)

        it "keeps reused tool call ids scoped to their retry attempt" do
            let firstCall =
                    functionToolCall "same" "shell_command"
                        "{\"command\":\"first\"}"
                secondCall =
                    functionToolCall "same" "shell_command"
                        "{\"command\":\"second\"}"
                execution =
                    (uncommittedExecution prepared)
                        { executionUncommittedDisplayEvents =
                            [ ToolStarted firstCall
                            , ToolFinished
                                (ToolCallResult
                                    "same"
                                    "first output"
                                    FunctionCallKind)
                            , ResponseRestarted "retrying"
                            , ToolStarted secondCall
                            , ToolOutputUpdated "same" "second output"
                            ]
                        }
                items = uncommittedDisplayItems execution
                calls =
                    [ call
                    | FunctionCallItem call <- items
                    ]
                outputs =
                    [ output
                    | FunctionCallOutputItem output <- items
                    ]
            map (.arguments) calls
                `shouldBe`
                    [ "{\"command\":\"first\"}"
                    , "{\"command\":\"second\"}"
                    ]
            map (.status) outputs
                `shouldBe` [Just ItemCompleted, Just ItemIncomplete]
            length (filter isDisplayAttemptBoundary items)
                `shouldBe` 1

    describe "finishConversation" do
        it "rolls a restarted turn back and restores consumed startup" do
            let patch = finishConversation prepared ConversationRestarted
                final = applyConversationPatch patch runningState
            final.conversationPreviousResponseId `shouldBe` Just "resp-newer"
            final.conversationTranscript `shouldBe` history
            final.conversationStartupContext
                `shouldBe` Just "startup instructions\n\nnewer skills"
            final.conversationGrokFirstTurnContext
                `shouldBe` Just "grok environment"
            final.conversationUsage `shouldBe` priorUsage
            final.conversationLastAssistant `shouldBe` Just "old answer"

        it "uses the same restoration for interrupted IO" do
            finishConversation prepared ConversationInterrupted
                `shouldBe`
                    finishConversation
                        prepared
                        ConversationProviderUnavailable

        it "retains the interrupted turn's items and invalidates the response chain on cancel" do
            let retained =
                    inputOnlyTurnItems prepared
                        <> [functionCallItem "c1" "shell" "{}" Nothing]
                final = applyConversationPatch
                    (finishConversation prepared (ConversationCancelled retained))
                    runningState
            final.conversationPreviousResponseId `shouldBe` Nothing
            final.conversationTranscript `shouldBe` history <> retained
            final.conversationStartupContext `shouldBe` Just "newer skills"
            final.conversationGrokFirstTurnContext `shouldBe` Nothing
            final.conversationUsage `shouldBe` priorUsage
            final.conversationLastAssistant `shouldBe` Just "old answer"

        it "uses the same checkpoint transition for terminal failures" do
            let retained = inputOnlyTurnItems prepared
            finishConversation prepared (ConversationFailed retained)
                `shouldBe`
                    finishConversation prepared (ConversationCancelled retained)

        it "retains failed multimodal input exactly once for retry" do
            let image = ImageAttachment "image/png" "png-bytes"
                multimodalPrepared = PreparedTurn
                    { preparedBeforeItems = history
                    , preparedConsumedStartup = Nothing
                    , preparedConsumedGrokContext = Nothing
                    , preparedTurnInputs =
                        [ userMessageWithAttachments
                            "inspect this"
                            [ImageAttachmentItem image]
                        ]
                    }
                final = applyConversationPatch
                    (finishConversation
                        multimodalPrepared
                        (ConversationFailed
                            (inputOnlyTurnItems multimodalPrepared)))
                    runningState
            final.conversationTranscript
                `shouldBe`
                    history
                        <> turnInputsToItems
                            [ userMessageWithAttachments
                                "inspect this"
                                [ImageAttachmentItem image]
                            ]

        it "restores one-shot context without changing conversation state for retry" do
            let final = applyConversationPatch
                    (finishConversation
                        prepared
                        ConversationProviderUnavailable)
                    runningState
            final.conversationPreviousResponseId `shouldBe` Just "resp-newer"
            final.conversationTranscript `shouldBe` mutatedTranscript
            final.conversationStartupContext
                `shouldBe` Just "startup instructions\n\nnewer skills"
            final.conversationGrokFirstTurnContext
                `shouldBe` Just "grok environment"
            final.conversationUsage `shouldBe` priorUsage
            final.conversationLastAssistant `shouldBe` Just "old answer"

        it "does not overwrite newer Grok context while restoring a retry" do
            let state = runningState
                    { conversationGrokFirstTurnContext =
                        Just "newer environment"
                    }
                final = applyConversationPatch
                    (finishConversation
                        prepared
                        ConversationProviderUnavailable)
                    state
            final.conversationGrokFirstTurnContext
                `shouldBe` Just "newer environment"

        it "commits successful metadata without rewriting backend state" do
            let usage = TokenUsage
                    { inputTokens = 7
                    , outputTokens = 3
                    , cachedTokens = 2
                    }
                final = applyConversationPatch
                    (finishConversation prepared
                        (ConversationCompleted
                            "resp-new"
                            usage
                            (Just "new answer")))
                    runningState
            final.conversationPreviousResponseId `shouldBe` Just "resp-newer"
            final.conversationTranscript `shouldBe` mutatedTranscript
            final.conversationStartupContext `shouldBe` Just "newer skills"
            final.conversationGrokFirstTurnContext `shouldBe` Nothing
            final.conversationUsage `shouldBe` TokenUsage
                { inputTokens = 17
                , outputTokens = 7
                , cachedTokens = 3
                }
            final.conversationLastAssistant `shouldBe` Just "new answer"

    describe "turnNewItems" do
        it "returns the suffix when the backend extends prior history" do
            let after = history <> turnInputsToItems [UserMessage "new"]
            turnNewItems history after
                `shouldBe` turnInputsToItems [UserMessage "new"]

        it "uses the complete replacement after compaction" do
            let replacement = turnInputsToItems [UserMessage "compacted"]
            turnNewItems history replacement `shouldBe` replacement

    describe "automatic compaction boundary" do
        it "keeps the checkpoint plus pending input after failure or cancel" do
            let checkpoint =
                    turnInputsToItems [UserMessage "compacted checkpoint"]
                pending = [UserMessage "current request"]
                committed =
                    rebasePreparedTurn
                        (Just AutomaticCompactionBoundary
                            { automaticCompactionHistory =
                                checkpoint <> turnInputsToItems pending
                            , automaticCompactionPendingInputs = []
                            })
                        prepared
                expected = checkpoint <> turnInputsToItems pending
                retained =
                    interruptedTurnItems
                        committed
                        (uncommittedExecution committed)
                        TurnAbortedByUser
                cancelled =
                    applyConversationPatch
                        (finishConversation committed
                            (ConversationCancelled retained))
                        runningState
                failed =
                    applyConversationPatch
                        (finishConversation committed
                            (ConversationFailed retained))
                        runningState
            retained `shouldBe` []
            cancelled.conversationTranscript `shouldBe` expected
            failed.conversationTranscript `shouldBe` expected
            inputOnlyTurnItems committed `shouldBe` []
            committed.preparedConsumedGrokContext `shouldBe` Nothing

        it "persists only the post-checkpoint suffix after success" do
            let checkpoint =
                    turnInputsToItems [UserMessage "compacted checkpoint"]
                pending = [UserMessage "current request"]
                response =
                    turnInputsToItems [UserMessage "assistant response"]
                committed =
                    rebasePreparedTurn
                        (Just AutomaticCompactionBoundary
                            { automaticCompactionHistory =
                                checkpoint <> turnInputsToItems pending
                            , automaticCompactionPendingInputs = []
                            })
                        prepared
                completed =
                    checkpoint <> turnInputsToItems pending <> response
            turnNewItems committed.preparedBeforeItems completed
                `shouldBe` response
            turnReplacesTranscript committed.preparedBeforeItems completed
                `shouldBe` False

        it "leaves ordinary turns unchanged when no compaction committed" do
            rebasePreparedTurn Nothing prepared `shouldBe` prepared

    describe "interruptedTurnItems" do
        it "retains only the prepared inputs while nothing committed" do
            interruptedTurnItems
                prepared
                (uncommittedExecution prepared)
                TurnAbortedByUser
                `shouldBe` inputOnlyTurnItems prepared

        it "keeps committed steps and queued results and closes unanswered calls on cancel" do
            let inputs = inputOnlyTurnItems prepared
                calls =
                    [ functionCallItem "c1" "read" "{\"path\":\"a\"}" Nothing
                    , functionCallItem "c2" "shell" "{\"cmd\":\"ls\"}" Nothing
                    ]
                result = ToolCallResult "c1" "contents of a" FunctionCallKind
                execution = LoopExecution
                    { executionState =
                        history <> inputs <> [assistantMessage "checking"] <> calls
                    , executionPendingInputs = [CompletedTool result]
                    , executionUncommittedAssistantText = Nothing
                    , executionUncommittedDisplayEvents = []
                    , executionProviderTelemetry = []
                    , executionProgress = ResponseCommitted
                    , executionResult = Left (LoopCancelled [result])
                    }
            interruptedTurnItems prepared execution TurnAbortedByUser
                `shouldBe`
                    inputs
                        <> [assistantMessage "checking"]
                        <> calls
                        <> [ toolResultToItem result
                           , toolResultToItem
                                (ToolCallResult
                                    "c2"
                                    "Tool `shell` was interrupted: the user cancelled the turn. It was not run, or was stopped before finishing and may have partially executed."
                                    FunctionCallKind)
                           ]
                        <> turnInputsToItems [UserMessage turnAbortedNote]

        it "drops a call cut off mid-arguments by an incomplete response but keeps complete ones" do
            let inputs = inputOnlyTurnItems prepared
                complete = functionCallItem "c-ok" "read" "{\"path\":\"a\"}" Nothing
                truncated =
                    functionCallItem "c-cut" "shell" "{\"cmd\":\"ls"
                        (Just ItemIncomplete)
                turn =
                    (emptyTurnOutput "resp-cut"
                        [ functionToolCall "c-ok" "read" "{\"path\":\"a\"}"
                        , functionToolCall "c-cut" "shell" "{\"cmd\":\"ls"
                        ]
                        (Just "partial"))
                        { completion = TurnIncomplete "max_output_tokens" Nothing }
                execution = LoopExecution
                    { executionState =
                        history <> inputs
                            <> [assistantMessage "partial", complete, truncated]
                    , executionPendingInputs = []
                    , executionUncommittedAssistantText = Nothing
                    , executionUncommittedDisplayEvents = []
                    , executionProviderTelemetry = []
                    , executionProgress = ResponseCommitted
                    , executionResult = Left (LoopIncomplete turn)
                    }
            interruptedTurnItems
                prepared
                execution
                (TurnAbortedByFailure "the response was cut off (max_output_tokens)")
                `shouldBe`
                    inputs
                        <> [assistantMessage "partial", complete]
                        <> [ toolResultToItem
                                (ToolCallResult
                                    "c-ok"
                                    "Tool `read` was not executed: the response was cut off (max_output_tokens)."
                                    FunctionCallKind)
                           ]

        it "drops a call whose arguments never became valid JSON even without a status" do
            let inputs = inputOnlyTurnItems prepared
                truncated =
                    functionCallItem "c-cut" "shell" "{\"cmd\":\"ls" Nothing
                turn =
                    (emptyTurnOutput "resp-cut"
                        [functionToolCall "c-cut" "shell" "{\"cmd\":\"ls"]
                        Nothing)
                        { completion = TurnIncomplete "max_output_tokens" Nothing }
                execution = LoopExecution
                    { executionState = history <> inputs <> [truncated]
                    , executionPendingInputs = []
                    , executionUncommittedAssistantText = Nothing
                    , executionUncommittedDisplayEvents = []
                    , executionProviderTelemetry = []
                    , executionProgress = ResponseCommitted
                    , executionResult = Left (LoopIncomplete turn)
                    }
            interruptedTurnItems
                prepared
                execution
                (TurnAbortedByFailure "the response was cut off (max_output_tokens)")
                `shouldBe` inputs

        it "keeps queued results after a transport failure without adding the aborted note" do
            let inputs = inputOnlyTurnItems prepared
                call = functionCallItem "c1" "read" "{}" Nothing
                result = ToolCallResult "c1" "done" FunctionCallKind
                execution = LoopExecution
                    { executionState = history <> inputs <> [call]
                    , executionPendingInputs = [CompletedTool result]
                    , executionUncommittedAssistantText = Nothing
                    , executionUncommittedDisplayEvents = []
                    , executionProviderTelemetry = []
                    , executionProgress = ResponseCommitted
                    , executionResult =
                        Left (LoopTransport (ConnectionError "down"))
                    }
            interruptedTurnItems
                prepared
                execution
                (TurnAbortedByFailure "the provider request failed")
                `shouldBe` inputs <> [call, toolResultToItem result]

        it "falls back to the prepared inputs when committed state does not extend the prepared history" do
            let rebased = prepared
                    { preparedBeforeItems =
                        turnInputsToItems [UserMessage "compacted"]
                    , preparedTurnInputs = [UserMessage "pending"]
                    }
                execution = LoopExecution
                    { executionState =
                        history
                            <> inputOnlyTurnItems prepared
                            <> [functionCallItem "c1" "read" "{}" Nothing]
                    , executionPendingInputs = []
                    , executionUncommittedAssistantText = Nothing
                    , executionUncommittedDisplayEvents = []
                    , executionProviderTelemetry = []
                    , executionProgress = ResponseCommitted
                    , executionResult =
                        Left (LoopTransport (ConnectionError "down"))
                    }
            interruptedTurnItems rebased execution TurnAbortedByUser
                `shouldBe` turnInputsToItems [UserMessage "pending"]

        it "does not add the aborted note when the model produced nothing" do
            let execution = LoopExecution
                    { executionState = history <> inputOnlyTurnItems prepared
                    , executionPendingInputs = []
                    , executionUncommittedAssistantText = Nothing
                    , executionUncommittedDisplayEvents = []
                    , executionProviderTelemetry = []
                    , executionProgress = ResponseCommitted
                    , executionResult = Left (LoopCancelled [])
                    }
            interruptedTurnItems prepared execution TurnAbortedByUser
                `shouldBe` inputOnlyTurnItems prepared

    describe "turnAbortedNote" do
        it "is recognised as generated context rather than user steering" do
            isTurnAbortedNote turnAbortedNote `shouldBe` True
            isTurnAbortedNote "  <turn_aborted>custom</turn_aborted>"
                `shouldBe` True
            isTurnAbortedNote "fix it" `shouldBe` False

    describe "restorePlanStateAfterIncomplete" do
        it "undoes an agent-initiated plan-mode entry after cancellation" do
            plan <- newPlanModeEnv (unsafeEncodeUtf "/tmp/turn-plan") Nothing
            activatePlanMode plan
            restorePlanStateAfterIncomplete plan PlanInactive
            readIORef plan.planStateRef `shouldReturn` PlanInactive

        it "restores pending and already-active modes exactly" do
            plan <- newPlanModeEnv (unsafeEncodeUtf "/tmp/turn-plan") Nothing
            writeIORef plan.planStateRef PlanActive
            restorePlanStateAfterIncomplete plan PlanPending
            readIORef plan.planStateRef `shouldReturn` PlanPending
            restorePlanStateAfterIncomplete plan PlanActive
            readIORef plan.planStateRef `shouldReturn` PlanActive

    describe "Grok user-message framing" do
        it "wraps a request in user_query tags" do
            grokUserQuery "fix it [2026-08-23 16:23 CEST]"
                `shouldBe`
                    "<user_query>\nfix it [2026-08-23 16:23 CEST]\n</user_query>"

        it "wraps only the last user payload and preserves synthetic context" do
            let inputs =
                    [ UserMessage "<system-reminder>rules</system-reminder>"
                    , UserMessage "<skill>instructions</skill>"
                    , UserMessage "actual request"
                    ]
            grokFrameLastUserInput inputs `shouldBe`
                [ UserMessage "<system-reminder>rules</system-reminder>"
                , UserMessage "<skill>instructions</skill>"
                , UserMessage "<user_query>\nactual request\n</user_query>"
                ]

        it "wraps multimodal user text without changing images" do
            let image = ImageAttachment "image/png" "png"
            grokFrameLastUserInput
                [ userMessageWithAttachments
                    "describe this"
                    [ImageAttachmentItem image]
                ]
                `shouldBe`
                    [ userMessageWithAttachments
                        "<user_query>\ndescribe this\n</user_query>"
                        [ImageAttachmentItem image]
                    ]

        it "renders first-turn environment and optional git status" do
            let prefix =
                    grokFirstTurnPrefix
                        "darwin 25.0"
                        "/bin/zsh"
                        (unsafeEncodeUtf "/tmp/project")
                        (fromGregorian 2026 8 23)
                        (Just " M src/Main.hs")
            prefix `shouldSatisfy` Text.isInfixOf "<user_info>"
            prefix `shouldSatisfy` Text.isInfixOf "OS Version: darwin 25.0"
            prefix `shouldSatisfy` Text.isInfixOf "Shell: zsh"
            prefix `shouldSatisfy` Text.isInfixOf "Workspace Path: /tmp/project"
            prefix `shouldSatisfy` Text.isInfixOf
                "Today's date: Sunday Aug 23, 2026"
            prefix `shouldSatisfy` Text.isInfixOf "Prefer using relative paths"
            prefix `shouldSatisfy` Text.isInfixOf "<git_status>"
            prefix `shouldSatisfy` Text.isInfixOf " M src/Main.hs"

        it "consumes persisted first-turn context before loading it fresh" do
            contextRef <- newIORef (Just "persisted environment")
            takeGrokFirstTurnContext
                contextRef
                (expectationFailure "loaded fresh context" >> pure "fresh")
                `shouldReturn` "persisted environment"
            takeGrokFirstTurnContext contextRef (pure "fresh environment")
                `shouldReturn` "fresh environment"

history :: [ResponseItem]
history = turnInputsToItems [UserMessage "earlier"]

mutatedTranscript :: [ResponseItem]
mutatedTranscript =
    history <> turnInputsToItems [UserMessage "backend-mutated suffix"]

priorUsage :: TokenUsage
priorUsage = TokenUsage
    { inputTokens = 10
    , outputTokens = 4
    , cachedTokens = 1
    }

prepared :: PreparedTurn
prepared = PreparedTurn
    { preparedBeforeItems = history
    , preparedConsumedStartup = Just "startup instructions"
    , preparedConsumedGrokContext = Just "grok environment"
    , preparedTurnInputs =
        [ UserMessage "startup instructions [2026-08-23 13:10 CEST]"
        , UserMessage "build failed [2026-08-23 13:10 CEST]"
        ]
    }

runningState :: ConversationState
runningState = ConversationState
    { conversationPreviousResponseId = Just "resp-newer"
    , conversationTranscript = mutatedTranscript
    , conversationStartupContext = Just "newer skills"
    , conversationGrokFirstTurnContext = Nothing
    , conversationUsage = priorUsage
    , conversationLastAssistant = Just "old answer"
    }

-- | A loop run that failed before any response committed.
uncommittedExecution :: PreparedTurn -> LoopExecution
uncommittedExecution turn = LoopExecution
    { executionState = turn.preparedBeforeItems
    , executionPendingInputs = turn.preparedTurnInputs
    , executionUncommittedAssistantText = Nothing
    , executionUncommittedDisplayEvents = []
    , executionProviderTelemetry = []
    , executionProgress = NoResponseCommitted
    , executionResult = Left (LoopTransport (ConnectionError "down"))
    }

functionCallItem :: Text -> Text -> Text -> Maybe ItemStatus -> ResponseItem
functionCallItem callId name arguments status =
    FunctionCallItem FunctionCall
        { itemId = Just ("fc-" <> callId)
        , callId
        , name
        , namespace = Nothing
        , provider = Nothing
        , arguments
        , encryptedFunctionArgs = Nothing
        , status
        }

assistantMessage :: Text -> ResponseItem
assistantMessage text =
    MessageItem ResponseMessage
        { messageId = Just ("msg-" <> text)
        , content = MessageContentParts [OutputTextPart text Nothing Nothing]
        , role = RoleAssistant
        , status = Nothing
        , phase = Nothing
        , passthrough = Nothing
        }
