module Agent.Runtime.TurnStateSpec (spec) where

import Agent.Error (ApiError(..))
import Agent.Loop
    ( LoopError(..)
    , LoopEvent(..)
    , LoopExecution(..)
    , LoopProgress(..)
    , TokenUsage(..)
    , TurnInput(..)
    )
import Agent.Responses.LoopBackend (toolResultToItem, turnInputsToItems)
import Agent.Responses.Types (ResponseItem)
import Agent.Runtime.Compaction (AutomaticCompactionBoundary(..))
import Agent.Runtime.TurnState
import Agent.ToolDispatch (ToolCallKind(..), ToolCallResult(..))
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
        let result = ToolCallResult "call-1" "already executed" FunctionCallKind
            committed = inputOnlyTurnItems prepared
            execution = failedExecution
                { executionState = history <> committed
                , executionProgress = ResponseCommitted
                , executionPendingInputs = [CompletedTool result]
                }
        interruptedTurnItems prepared execution (TurnAbortedByFailure "offline")
            `shouldBe` committed <> [toolResultToItem result]

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
