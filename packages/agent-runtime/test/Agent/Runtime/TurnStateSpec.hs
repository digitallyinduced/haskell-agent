module Agent.Runtime.TurnStateSpec (spec, managedRecoveryRoundTrip) where

import Agent.Error (ApiError(..))
import Agent.Cancel (newCancelFlag, requestCancel)
import Agent.Json (rawJsonBytes)
import qualified Agent.Json.Decode as Json
import Agent.Loop
import Agent.Responses.LoopBackend (toolResultToItem, turnInputsToItems)
import Agent.Responses.Types
import Agent.Responses.Types.Items (responseItemDecoder)
import Agent.Runtime.Compaction (AutomaticCompactionBoundary(..))
import Agent.Runtime.TurnState
import Agent.ToolDispatch
import Agent.Tools.Types
    ( AppTool, ApprovalRule(..), ToolExecutionPolicy(..)
    , jsonAppToolWithExecution, mkToolRegistry, withAsyncToolCalls
    )
import Control.Concurrent (newEmptyMVar, putMVar, takeMVar, threadDelay, tryReadMVar)
import Control.Concurrent.Async (wait, withAsync)
import Control.Exception.Safe (finally)
import Data.IORef
import qualified Data.Text as Text
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "frontend-neutral turn policy" do
    it "keeps failed streamed output out of retryable model inputs" do
        let execution = failedExecution
                { executionUncommittedAssistantText = Just "partial answer"
                , executionUncommittedDisplayEvents =
                    [TextDelta "partial answer", ResponseAttemptFailed]
                }
        uncommittedDisplayItems execution `shouldSatisfy` (not . null)
        interruptedTurnItems prepared execution (TurnAbortedByFailure "offline")
            `shouldBe` inputOnlyTurnItems prepared
        execution.executionState `shouldBe` history

    it "retains committed tool results pending a failed continuation" do
        let result = ToolCallResult
                { callId = "call-1"
                , output = "already executed"
                , callKind = FunctionCallKind
                , toolResultMode = BlockingToolCall
                , toolResultImages = []
                , toolResultOutcome = Nothing
                }
            committed = inputOnlyTurnItems prepared
            execution = failedExecution
                { executionState = history <> committed
                , executionProgress = ResponseCommitted
                , executionPendingInputs = [CompletedTool result]
                }
        interruptedTurnItems prepared execution (TurnAbortedByFailure "offline")
            `shouldBe` committed <> [toolResultToItem result]

    it "retains explicit interrupted-work context without promoting display activity" do
        let recovery = turnInputsToItems
                [UserMessage "<turn_aborted>\n<interrupted_work>\nAssistant reported: PR #86 opened.\n</interrupted_work>\n</turn_aborted>"]
            committed = inputOnlyTurnItems prepared <> recovery
            execution = failedExecution
                { executionState = history <> committed
                , executionProgress = ResponseCommitted
                , executionPendingInputs = []
                , executionUncommittedAssistantText = Just "unfinished answer"
                , executionUncommittedDisplayEvents = [TextDelta "unfinished answer"]
                }
            retained = interruptedTurnItems prepared execution TurnAbortedByUser
            resumed = applyConversationPatch
                (finishConversation prepared (ConversationFailed retained))
                runningState
        retained `shouldBe` committed
        resumed.conversationTranscript `shouldBe` history <> committed
        resumed.conversationPreviousResponseId `shouldBe` Nothing
        uncommittedDisplayItems execution `shouldSatisfy` (not . null)

    it "invalidates a failed response chain without losing newer usage" do
        let retained = inputOnlyTurnItems prepared
            state = applyConversationPatch
                (finishConversation prepared (ConversationFailed retained))
                runningState
        state.conversationPreviousResponseId `shouldBe` Nothing
        state.conversationTranscript `shouldBe` history <> retained
        state.conversationUsage `shouldBe` runningState.conversationUsage
        state.conversationStartupContext `shouldBe` Just "newer skills"

    it "merges restored startup context with concurrently refreshed skills" do
        let state = applyConversationPatch
                (finishConversation prepared ConversationInterrupted)
                runningState
        state.conversationStartupContext
            `shouldBe` Just "startup instructions\n\nnewer skills"
        state.conversationUsage `shouldBe` runningState.conversationUsage

    it "rebases recovery onto an installed compaction checkpoint" do
        let compacted = turnInputsToItems [UserMessage "summary"]
            pending = [UserMessage "continue"]
            boundary = AutomaticCompactionBoundary compacted pending
            rebased = rebasePreparedTurn (Just boundary) prepared
            patch = finishConversation rebased ConversationRestarted
        rebased.preparedBeforeItems `shouldBe` compacted
        rebased.preparedTurnInputs `shouldBe` pending
        rebased.preparedConsumedStartup `shouldBe` Nothing
        rebased.preparedConsumedGrokContext `shouldBe` Nothing
        patch.patchTranscript `shouldBe` SetField compacted
        patch.patchStartupContext `shouldBe` KeepStartup

history :: [ResponseItem]
history = turnInputsToItems [UserMessage "earlier"]

prepared :: PreparedTurn
prepared = PreparedTurn
    { preparedBeforeItems = history
    , preparedConsumedStartup = Just "startup instructions"
    , preparedConsumedGrokContext = Just "environment"
    , preparedTurnInputs = [UserMessage "fix it"]
    }

runningState :: ConversationState
runningState = ConversationState
    { conversationPreviousResponseId = Just "response-newer"
    , conversationTranscript = history
    , conversationStartupContext = Just "newer skills"
    , conversationGrokFirstTurnContext = Nothing
    , conversationUsage = TokenUsage 10 4 1
    , conversationLastAssistant = Just "old answer"
    }

failedExecution :: LoopExecution
failedExecution = LoopExecution
    { executionState = history
    , executionPendingInputs = prepared.preparedTurnInputs
    , executionProgress = NoResponseCommitted
    , executionUncommittedAssistantText = Nothing
    , executionUncommittedDisplayEvents = []
    , executionProviderTelemetry = []
    , executionResult = Left (LoopTransport (ConnectionError "offline"))
    }

-- Exercise the real manager and interruption policy, then resume solely from
-- the supplied persistence adapter's restored transcript. The adapter belongs
-- to the session integration specs so this kernel fixture stays frontend-neutral.
managedRecoveryRoundTrip
    :: ([ResponseItem] -> [ResponseItem] -> IO [ResponseItem])
    -> Bool
    -> IO ()
managedRecoveryRoundTrip roundTrip precommit = do
    blocked <- newEmptyMVar
    joined <- newEmptyMVar
    invocations <- newIORef (0 :: Int)
    let mode = if precommit then AsyncToolCall else BlockingToolCall
        first = withToolCallMode mode (functionToolCall "saved" "save" "{}")
        second = withToolCallMode mode (functionToolCall "blocked" "block" "{}")
        -- TurnSequential makes B's start a deterministic acknowledgement
        -- that A has completed through the manager, not only the UI.
        makeTool name action =
            (if precommit then withAsyncToolCalls else id) $
                jsonAppToolWithExecution name "" [] AlwaysReadOnly
                    TurnSequential (noArgsTool name action)
        tools =
            [ makeTool "save" do
                modifyIORef' invocations (+ 1)
                pure (Right "file saved exactly once")
            , makeTool "block" $
                (putMVar blocked () >> threadDelay maxBound
                    >> pure (Right "must not finish"))
                    `finally` putMVar joined ()
            ]
        backend = backendWithCallbacks \state _ inputs callbacks ->
            if precommit
                then do
                    callbacks.onAsyncToolCall first
                    callbacks.onAsyncToolCall second
                    await "blocked precommit tool" (takeMVar blocked)
                    callbacks.onLoopEvent (TextDelta "untrusted unfinished answer")
                    pure (Left (ConnectionError "stream failed before commit"))
                else pure $ Right BackendResult
                    { backendOutput =
                        emptyTurnOutput "committed-tools" [first, second] Nothing
                    , backendState = advanceBackendSnapshot state
                        (state.backendItems <> turnInputsToItems inputs
                            <> recoveryCallItems) Nothing
                    }
    config <- recoveryConfig backend tools history
    execution <- if precommit
        then await "failed provider cleanup"
            (runLoopInputsDetailed config Nothing prepared.preparedTurnInputs)
        else withAsync
            (runLoopInputsDetailed config Nothing prepared.preparedTurnInputs)
            \running -> do
                await "blocked committed tool" (takeMVar blocked)
                requestCancel config.loopCancel
                await "cancelled tool cleanup" (wait running)
    tryReadMVar joined `shouldReturn` Just ()
    readIORef invocations `shouldReturn` 1
    let abort = if precommit
            then TurnAbortedByFailure "offline"
            else TurnAbortedByUser
        retained = interruptedTurnItems prepared execution abort
        updated = applyConversationPatch
            (finishConversation prepared (ConversationFailed retained))
            runningState
    updated.conversationPreviousResponseId `shouldBe` Nothing
    show retained `shouldNotContain` "untrusted unfinished answer"
    if precommit
        then do
            show retained `shouldContain` "file saved exactly once"
            retained `shouldSatisfy` all (\case MessageItem{} -> True; _ -> False)
            show retained `shouldContain` "saved"
            show retained `shouldContain` "blocked"
            show retained `shouldContain` "save"
            show retained `shouldContain` "unknown outcomes"
            show retained `shouldNotContain` "was not executed"
        else do
            let outputs = [output | FunctionCallOutputItem output <- retained]
            map (.callId) outputs `shouldBe` ["saved", "blocked"]
            case outputs of
                [saved, unfinished] -> do
                    Json.decodeEither Json.text (rawJsonBytes saved.output)
                        `shouldBe` Right "file saved exactly once"
                    saved.localOutcome `shouldBe` Just ToolSucceeded
                    case Json.decodeEither Json.text (rawJsonBytes unfinished.output) of
                        Left err -> expectationFailure (show err)
                        Right text -> do
                            text `shouldSatisfy` (not . Text.isInfixOf "was not executed")
                            text `shouldSatisfy` Text.isInfixOf "partially"
                _ -> expectationFailure "expected exactly one output per tool"
    restored <- roundTrip updated.conversationTranscript
        (uncommittedDisplayItems execution)
    restored `shouldBe` updated.conversationTranscript
    seen <- newIORef []
    let resumed = Backend \state previous inputs _ -> do
            previous `shouldBe` Nothing
            inputs `shouldBe` [UserMessage "go"]
            writeIORef seen state.backendItems
            pure $ Right BackendResult
                { backendOutput = emptyTurnOutput "resumed" [] (Just "verified saved file")
                , backendState = advanceBackendSnapshot state
                    (state.backendItems <> turnInputsToItems inputs) Nothing
                }
    resumeConfig <- recoveryConfig resumed tools restored
    continued <- runLoopInputsDetailed resumeConfig Nothing [UserMessage "go"]
    continued.executionResult `shouldSatisfy` either (const False) (const True)
    readIORef seen `shouldReturn` restored
    readIORef invocations `shouldReturn` 1

recoveryConfig :: Backend -> [AppTool] -> [ResponseItem] -> IO LoopConfig
recoveryConfig backend tools items = do
    cancel <- newCancelFlag
    state <- newIORef (advanceBackendSnapshot emptyBackendSnapshot items Nothing)
    registry <- either (fail . Text.unpack) pure (mkToolRegistry tools)
    pure LoopConfig
        { loopBackend = backend
        , loopBackendState = BackendStateStore
            { readBackendState = readIORef state
            , commitBackendState = \snapshot -> writeIORef state snapshot >> pure snapshot
            }
        , loopTools = registry
        , loopDispatch = defaultLoopDispatch
        , loopMaxTurns = defaultLoopMaxTurns
        , loopOnEvent = const (pure ())
        , loopApprove = const (pure (Right True))
        , loopReadSteering = pure []
        , loopCommitSteering = const (pure ())
        , loopInterrupt = pure ()
        , loopCancel = cancel
        }

recoveryCallItems :: [ResponseItem]
recoveryCallItems = either (error . show) id $
    Json.decodeEither (Json.list responseItemDecoder)
        "[{\"type\":\"function_call\",\"call_id\":\"saved\",\"name\":\"save\",\"arguments\":\"{}\"},{\"type\":\"function_call\",\"call_id\":\"blocked\",\"name\":\"block\",\"arguments\":\"{}\"}]"

await :: String -> IO a -> IO a
await label action = timeout 5000000 action
    >>= maybe (fail ("timed out waiting for " <> label)) pure
