module Agent.Runtime.TurnEngineSpec (spec) where

import Agent.Cancel (newCancelFlag)
import Agent.Error (ApiError(..))
import Agent.Loop hiding (TurnCompleted)
import Agent.Responses.LoopBackend (turnInputsToItems)
import Agent.Responses.Types (ResponseItem)
import Agent.Runtime.Compaction (AutomaticCompactionBoundary(..))
import Agent.Runtime.TurnEngine
import Agent.Runtime.TurnState
import Agent.Tools.Types (mkToolRegistry)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = describe "turn engine" do
    it "prioritizes an explicit restart over cancellation, failure, and success" do
        mapM_ (\result -> do
            let final = finalizeTurn policy (Just "high") Nothing prepared
                    execution { executionResult = result }
            dispositionName final `shouldBe` "restart"
            modelItems final.finalizedModelItems `shouldBe` []
            displayItems final.finalizedDisplayItems `shouldBe` []
            final.finalizedPatch.patchTranscript `shouldBe` SetField history
            final.finalizedPatch.patchStartupContext
                `shouldBe` RestoreStartup "startup"
            ) [Left (LoopCancelled []), Left failure, Right success]

    it "classifies cancellation separately from provider failure" do
        let final = finish execution { executionResult = Left (LoopCancelled []) }
        dispositionName final `shouldBe` "cancel"
        modelItems final.finalizedModelItems `shouldBe` inputOnlyTurnItems prepared
        final.finalizedPatch.patchPreviousResponseId `shouldBe` SetField Nothing

    it "offers unavailable-provider recovery only before any response commits" do
        let unavailablePolicy = policy { providerUnavailable = const True }
            before = finalizeTurn unavailablePolicy Nothing Nothing prepared execution
            after = finalizeTurn unavailablePolicy Nothing Nothing prepared
                execution { executionProgress = ResponseCommitted }
        dispositionName before `shouldBe` "unavailable"
        modelItems before.finalizedModelItems `shouldBe` []
        before.finalizedPatch.patchStartupContext `shouldBe` RestoreStartup "startup"
        dispositionName after `shouldBe` "failure"
        after.finalizedPatch.patchPreviousResponseId `shouldBe` SetField Nothing

    it "does not mistake failure after visible output for provider unavailability" do
        let final = finalizeTurn
                policy { providerUnavailable = const True } Nothing Nothing prepared
                execution
                    { executionResult = Left (LoopTransportAfterOutput offline)
                    , executionUncommittedDisplayEvents =
                        [TextDelta "partial", ResponseAttemptFailed]
                    }
        dispositionName final `shouldBe` "failure"
        displayItems final.finalizedDisplayItems `shouldSatisfy` (not . null)
        modelItems final.finalizedModelItems `shouldBe` inputOnlyTurnItems prepared

    it "normalizes only the success patch and retains canonical model items" do
        let suffix = turnInputsToItems [UserMessage "canonical suffix"]
            final = finish execution
                { executionResult = Right success
                , executionProgress = ResponseCommitted
                , executionState = history <> suffix
                }
        dispositionName final `shouldBe` "success"
        final.finalizedPatch.patchLastAssistant `shouldBe` SetField (Just "answer")
        final.finalizedPatch.patchUsageDelta `shouldBe` success.tokenUsage
        modelItems final.finalizedModelItems `shouldBe` suffix
        displayItems final.finalizedDisplayItems `shouldBe` []
        case final.finalizedDisposition of
            TurnCompleted result -> result.finalText `shouldBe` Just "  answer  "
            _ -> expectationFailure "expected successful disposition"

    it "rebases failed retention onto the installed compaction prefix" do
        let summary = turnInputsToItems [UserMessage "summary"]
            pending = [UserMessage "continue"]
            boundary = AutomaticCompactionBoundary summary pending
            final = finalizeTurn policy Nothing (Just boundary) prepared execution
        final.finalizedPrepared.preparedBeforeItems `shouldBe` summary
        final.finalizedPrepared.preparedConsumedStartup `shouldBe` Nothing
        modelItems final.finalizedModelItems `shouldBe` turnInputsToItems pending
        final.finalizedPatch.patchTranscript
            `shouldBe` SetField (summary <> turnInputsToItems pending)
        final.finalizedPatch.patchStartupContext `shouldBe` KeepStartup

    it "finalizes a real scripted loop failure without exposing partial text to the model" do
        let backend = Backend \_ _ _ emit -> do
                emit TurnStarted
                emit (TextDelta "partial streamed answer")
                pure (Left offline)
        config <- safeConfig backend
        observed <- runLoopInputsDetailed config Nothing prepared.preparedTurnInputs
        let final = finish observed
        dispositionName final `shouldBe` "failure"
        observed.executionProgress `shouldBe` NoResponseCommitted
        modelItems final.finalizedModelItems `shouldBe` inputOnlyTurnItems prepared
        displayItems final.finalizedDisplayItems `shouldSatisfy` (not . null)
        final.finalizedPatch.patchTranscript
            `shouldBe` SetField (history <> inputOnlyTurnItems prepared)

policy :: TurnPolicy
policy = TurnPolicy
    { providerUnavailable = const False
    , normalizeAssistant = Text.strip
    }

finish :: LoopExecution -> FinalizedTurn
finish = finalizeTurn policy Nothing Nothing prepared

dispositionName :: FinalizedTurn -> Text
dispositionName final = case final.finalizedDisposition of
    TurnRestarted _ -> "restart"
    TurnCancelled _ -> "cancel"
    TurnProviderUnavailable _ -> "unavailable"
    TurnFailed _ -> "failure"
    TurnCompleted _ -> "success"

history :: [ResponseItem]
history = turnInputsToItems [UserMessage "earlier"]

prepared :: PreparedTurn
prepared = PreparedTurn history (Just "startup") Nothing [UserMessage "fix it"]

offline :: ApiError
offline = ConnectionError "offline"

failure :: LoopError
failure = LoopTransport offline

success :: LoopResult
success = LoopResult
    { finalResponseId = "response-1"
    , finalText = Just "  answer  "
    , turnsUsed = 1
    , tokenUsage = TokenUsage 10 4 1
    }

execution :: LoopExecution
execution = LoopExecution
    { executionState = history
    , executionPendingInputs = prepared.preparedTurnInputs
    , executionProgress = NoResponseCommitted
    , executionUncommittedAssistantText = Nothing
    , executionUncommittedDisplayEvents = []
    , executionProviderTelemetry = []
    , executionResult = Left failure
    }

-- No external services, process tools, or approvals are admitted by this host.
safeConfig :: Backend -> IO LoopConfig
safeConfig backend = do
    cancel <- newCancelFlag
    state <- newIORef (initialBackendSnapshot history)
    tools <- either (fail . Text.unpack) pure (mkToolRegistry [])
    pure LoopConfig
        { loopBackend = backend
        , loopBackendState = BackendStateStore
            { readBackendState = readIORef state
            , commitBackendState = \snapshot -> do
                writeIORef state snapshot
                pure snapshot
            }
        , loopTools = tools
        , loopDispatch = defaultLoopDispatch
        , loopMaxTurns = 2
        , loopOnEvent = const (pure ())
        , loopApprove = const (pure ToolApprovalRejected)
        , loopReadSteering = pure []
        , loopCommitSteering = const (pure ())
        , loopInterrupt = pure ()
        , loopCancel = cancel
        }
