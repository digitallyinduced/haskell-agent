module Agent.Runtime.TurnExecutionSpec (spec) where

import Agent.Cancel (newCancelFlag, requestCancel)
import Agent.Error (ApiError(..))
import Agent.Loop
import Agent.Responses.LoopBackend (turnInputsToItems)
import Agent.Responses.Types
    ( ResponseItem(..), FunctionCall(..), ItemStatus(..) )
import Agent.Runtime.Compaction (AutomaticCompactionBoundary(..))
import Agent.Runtime.TurnEngine qualified as Engine
import Agent.Runtime.TurnExecution
import Agent.Runtime.TurnState
import Agent.ToolDispatch (functionToolCall, noArgsTool)
import Agent.Tools.Types
    ( ApprovalRule(..), ToolExecutionPolicy(..), ToolRegistry
    , jsonAppToolWithExecution, mkToolRegistry )
import Control.Concurrent.Async (cancel, wait, withAsync)
import Control.Concurrent.MVar
    ( newEmptyMVar, putMVar, readMVar, takeMVar )
import Control.Exception.Safe (finally, onException)
import Control.Monad (void)
import Data.Either (isLeft)
import Data.IORef
    ( IORef, modifyIORef', newIORef, readIORef, writeIORef )
import Data.Text (Text)
import Data.Text qualified as Text
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "prepared turn execution" do
    it "preserves the direct-loop result, checkpoint, and event order on success" do
        baseline <- observe direct successBackend
        extracted <- observe shared successBackend
        extracted `shouldBe` baseline
        let (execution, _, _) = extracted
        execution.executionResult `shouldSatisfy` (not . isLeft)

    it "preserves provider failure before any response commits" do
        baseline <- observe direct (Backend \_ _ _ _ -> pure (Left offline))
        extracted <- observe shared (Backend \_ _ _ _ -> pure (Left offline))
        extracted `shouldBe` baseline
        let (execution, _, _) = extracted
        execution.executionResult `shouldBe` Left (LoopTransport offline)

    it "retains earlier attempts when a later attempt with reused call ids is discarded" do
        baseline <- observe direct failedAttemptsBackend
        extracted <- observe shared failedAttemptsBackend
        extracted `shouldBe` baseline
        let (execution, _, _) = extracted
            final = finalize Nothing prepared execution
            display = Text.pack (show (Engine.displayItems final.finalizedDisplayItems))
        display `shouldSatisfy` Text.isInfixOf "retained first attempt"
        display `shouldSatisfy` (not . Text.isInfixOf "discarded second attempt")
        [call.arguments | FunctionCallItem call <-
            Engine.displayItems final.finalizedDisplayItems]
            `shouldBe` ["{\"attempt\":1}"]
        Engine.modelItems final.finalizedModelItems
            `shouldBe` inputOnlyTurnItems prepared
        -- Simulate the existing persistence projection and resume it into a
        -- fresh backend store. Only the model patch may become provider input.
        case final.finalizedPatch.patchTranscript of
            KeepField -> expectationFailure "failure must invalidate the transcript"
            SetField resumedHistory -> do
                seen <- newIORef []
                final.finalizedPatch.patchPreviousResponseId
                    `shouldBe` SetField Nothing
                let resumedPrevious = case final.finalizedPatch.patchPreviousResponseId of
                        KeepField -> Just "previous-response"
                        SetField value -> value
                let resumed = PreparedTurn resumedHistory Nothing Nothing
                        [UserMessage "continue"]
                    backend = Backend \snapshot previous inputs emit -> do
                        writeIORef seen snapshot.backendItems
                        (successBackendWithPrevious Nothing).submitTurn snapshot previous inputs emit
                (config, _) <- configFor resumedHistory backend
                void (executePreparedTurn
                    (request resumed config)
                        { executionPreviousResponseId = resumedPrevious }
                    (pure Nothing) unexpected)
                readIORef seen `shouldReturn`
                    (history <> inputOnlyTurnItems prepared)
                Engine.displayItems final.finalizedDisplayItems
                    `shouldSatisfy` (not . null)

    it "retains committed tool results when the next provider submission fails" do
        let run runner = do
                count <- newIORef (0 :: Int)
                calls <- newIORef (0 :: Int)
                let backend = Backend \snapshot _ inputs _ -> do
                        n <- readIORef count
                        modifyIORef' count (+ 1)
                        if n == 0
                            then pure (Right (toolResponse snapshot inputs))
                            else pure (Left offline)
                (config, _) <- configFor history backend
                tools <- guardedTools (modifyIORef' calls (+ 1))
                result <- runner prepared config
                    { loopTools = tools
                    , loopApprove = const (pure (Right True))
                    }
                readIORef calls `shouldReturn` 1
                pure result
        baseline <- run direct
        extracted <- run shared
        extracted `shouldBe` baseline
        extracted.executionProgress `shouldBe` ResponseCommitted
        let final = finalize Nothing prepared extracted
            retained = Engine.modelItems final.finalizedModelItems
        retained `shouldSatisfy` any (\case
            FunctionCallOutputItem _ -> True
            _ -> False)
        final.finalizedPatch.patchPreviousResponseId `shouldBe` SetField Nothing

    it "cancels and joins a pending approval without invoking its tool" do
        baseline <- cancelledApproval direct
        extracted <- cancelledApproval shared
        extracted `shouldBe` baseline
        case extracted.executionResult of
            Left (LoopCancelled _) -> pure ()
            other -> expectationFailure ("expected cancellation, got " <> show other)

    it "observes the installed compaction boundary and retries only its prepared inputs" do
        boundaryRef <- newIORef Nothing
        let summary = turnInputsToItems [UserMessage "installed summary"]
            pending = [UserMessage "post-compaction continuation"]
            boundary = AutomaticCompactionBoundary summary pending
            backend = Backend \_ _ _ _ -> do
                writeIORef boundaryRef (Just boundary)
                pure (Left offline)
        (config, _) <- configFor history backend
        executed <- executePreparedTurn (request prepared config)
            (readIORef boundaryRef) unexpected
        executed.executedCompaction `shouldBe` Just boundary
        let final = finalize executed.executedCompaction prepared executed.executedLoop
        final.finalizedPatch.patchTranscript
            `shouldBe` SetField (summary <> turnInputsToItems pending)
        final.finalizedPatch.patchStartupContext `shouldBe` KeepStartup
        seen <- newIORef []
        let retryBackend = Backend \snapshot previous inputs emit -> do
                writeIORef seen inputs
                successBackend.submitTurn snapshot previous inputs emit
        (retryConfig, _) <- configFor summary retryBackend
        void (shared final.finalizedPrepared retryConfig)
        readIORef seen `shouldReturn` pending

    it "reports a rebased exceptional rollback once before rethrowing" do
        logRef <- newIORef ([] :: [Text])
        events <- newIORef []
        (config0, _) <- configFor history successBackend
        let summary = turnInputsToItems [UserMessage "summary"]
            boundary = AutomaticCompactionBoundary summary [UserMessage "pending"]
            config = config0
                { loopBackendState = config0.loopBackendState
                    { readBackendState = do
                        modifyIORef' logRef (<> ["loop"])
                        ioError (userError "state unavailable")
                    }
                }
            readBoundary = do
                modifyIORef' logRef (<> ["boundary"])
                pure (Just boundary)
            rollback event = do
                modifyIORef' logRef (<> ["rollback"])
                modifyIORef' events (<> [event.exceptionalPatch])
                event.exceptionalCompaction `shouldBe` Just boundary
        executePreparedTurn (request prepared config) readBoundary rollback
            `shouldThrow` anyIOException
        readIORef logRef `shouldReturn` ["loop", "boundary", "rollback"]
        readIORef events `shouldReturn`
            [finishConversation (rebasePreparedTurn (Just boundary) prepared)
                ConversationInterrupted]

    it "does not turn a post-loop checkpoint-read failure into loop rollback" do
        rollbacks <- newIORef (0 :: Int)
        (config, _) <- configFor history successBackend
        executePreparedTurn (request prepared config)
            (ioError (userError "checkpoint read failed"))
            (\_ -> modifyIORef' rollbacks (+ 1))
            `shouldThrow` anyIOException
        readIORef rollbacks `shouldReturn` 0

    it "delivers rollback after async cancellation has joined provider cleanup" do
        started <- newEmptyMVar
        blocked <- newEmptyMVar
        logRef <- newIORef ([] :: [Text])
        let backend = Backend \_ _ _ _ ->
                (putMVar started () >> takeMVar blocked >> pure (Left offline))
                    `finally` modifyIORef' logRef (<> ["provider closed"])
        (config, _) <- configFor history backend
        withAsync
            (executePreparedTurn (request prepared config) (pure Nothing)
                (\_ -> modifyIORef' logRef (<> ["rollback"]))) \running -> do
            timeout probeMicros (readMVar started) `shouldReturn` Just ()
            timeout probeMicros (cancel running) `shouldReturn` Just ()
        readIORef logRef `shouldReturn` ["provider closed", "rollback"]

type Runner = PreparedTurn -> LoopConfig -> IO LoopExecution

-- Baseline is the pre-extraction call site, not another call through the
-- extracted function. Both hosts retain the same pure finalization policy.
direct :: Runner
direct prepared config =
    runLoopInputsDetailed config (Just "previous-response") prepared.preparedTurnInputs
        `onException` pure ()

shared :: Runner
shared prepared config =
    (.executedLoop) <$> executePreparedTurn
        (request prepared config) (pure Nothing) unexpected

request :: PreparedTurn -> LoopConfig -> PreparedExecution
request prepared config = PreparedExecution
    { executionConfig = config
    , executionPreviousResponseId = Just "previous-response"
    , executionPreparedTurn = prepared
    }

unexpected :: ExceptionalTurn -> IO ()
unexpected _ = expectationFailure "unexpected exceptional rollback"

observe :: Runner -> Backend -> IO (LoopExecution, BackendSnapshot, [LoopEvent])
observe runner backend = do
    events <- newIORef []
    (config, state) <- configFor history backend
    execution <- runner prepared config
        { loopOnEvent = \event -> modifyIORef' events (<> [event]) }
    (,,) execution <$> readIORef state <*> readIORef events

configFor :: [ResponseItem] -> Backend -> IO (LoopConfig, IORef BackendSnapshot)
configFor initial backend = do
    cancelFlag <- newCancelFlag
    state <- newIORef (initialBackendSnapshot initial)
    tools <- either (fail . Text.unpack) pure (mkToolRegistry [])
    pure (LoopConfig
        { loopBackend = backend
        , loopBackendState = BackendStateStore
            { readBackendState = readIORef state
            , commitBackendState = \snapshot -> writeIORef state snapshot >> pure snapshot
            }
        , loopTools = tools
        , loopReadTools = Nothing
        , loopDispatch = defaultLoopDispatch
        , loopMaxTurns = 3
        , loopOnEvent = const (pure ())
        , loopApprove = const (pure (Right False))
        , loopReadSteering = pure []
        , loopCommitSteering = const (pure ())
        , loopInterrupt = pure ()
        , loopCancel = cancelFlag
        }, state)

successBackend :: Backend
successBackend = successBackendWithPrevious (Just "previous-response")

successBackendWithPrevious :: Maybe Text -> Backend
successBackendWithPrevious expectedPrevious = Backend \snapshot previous inputs emit -> do
    previous `shouldBe` expectedPrevious
    emit TurnStarted
    emit (TextDelta "answer")
    pure (Right BackendResult
        { backendOutput = emptyTurnOutput "response-success" [] (Just "answer")
        , backendState = advanceBackendSnapshot snapshot
            (snapshot.backendItems <> turnInputsToItems inputs) Nothing
        })

failedAttemptsBackend :: Backend
failedAttemptsBackend = Backend \_ _ _ emit -> do
    emit TurnStarted
    emit (TextDelta "retained first attempt")
    emit (ToolStarted (functionToolCall "reused" "guarded" "{\"attempt\":1}"))
    emit (ResponseRestarted "retry")
    emit (TextDelta "discarded second attempt")
    emit (ToolStarted (functionToolCall "reused" "guarded" "{\"attempt\":2}"))
    emit ResponseAttemptDiscarded
    pure (Left offline)

toolResponse :: BackendSnapshot -> [TurnInput] -> BackendResult
toolResponse snapshot inputs = BackendResult
    { backendOutput = emptyTurnOutput "response-tool"
        [functionToolCall "call-1" "guarded" "{}"] Nothing
    , backendState = advanceBackendSnapshot snapshot
        (snapshot.backendItems <> turnInputsToItems inputs <>
            [FunctionCallItem FunctionCall
                { callId = "call-1"
                , name = "guarded"
                , arguments = "{}"
                , itemId = Nothing
                , namespace = Nothing
                , provider = Nothing
                , encryptedFunctionArgs = Nothing
                , status = Just ItemCompleted
                , async = Nothing
                }]) Nothing
    }

guardedTools :: IO () -> IO ToolRegistry
guardedTools action =
    either (fail . Text.unpack) pure (mkToolRegistry
        [jsonAppToolWithExecution "guarded" "" [] AlwaysReadOnly TurnSequential
            (noArgsTool "guarded" (action >> pure (Right "tool result")))])

cancelledApproval :: Runner -> IO LoopExecution
cancelledApproval runner = do
    started <- newEmptyMVar
    released <- newEmptyMVar
    closed <- newIORef False
    invoked <- newIORef (0 :: Int)
    (config0, _) <- configFor history
        (Backend \snapshot _ inputs _ -> pure (Right (toolResponse snapshot inputs)))
    tools <- guardedTools (modifyIORef' invoked (+ 1))
    let config = config0
            { loopTools = tools
            , loopApprove = \_ ->
                (putMVar started () >> takeMVar released >> pure (Right True))
                    `finally` writeIORef closed True
            }
    withAsync (runner prepared config) \running -> do
        timeout probeMicros (readMVar started) `shouldReturn` Just ()
        requestCancel config.loopCancel
        result <- timeout probeMicros (wait running)
        readIORef closed `shouldReturn` True
        readIORef invoked `shouldReturn` 0
        maybe (fail "cancelled approval did not finish") pure result

finalize :: Maybe AutomaticCompactionBoundary -> PreparedTurn -> LoopExecution -> Engine.FinalizedTurn
finalize boundary prepared =
    Engine.finalizeTurn (Engine.TurnPolicy (const False) Text.strip)
        Nothing boundary prepared

history :: [ResponseItem]
history = turnInputsToItems [UserMessage "earlier"]

prepared :: PreparedTurn
prepared = PreparedTurn history (Just "startup") Nothing [UserMessage "fix it"]

offline :: ApiError
offline = ConnectionError "offline"

probeMicros :: Int
probeMicros = 5000000
