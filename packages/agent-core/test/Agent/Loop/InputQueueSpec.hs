module Agent.Loop.InputQueueSpec (spec) where

import Agent.Loop.InputBudget (logicalTextBytes, logicalTurnInputBytes)
import Agent.Loop.InputQueue
import qualified Agent.Loop.InputQueue.Model as Model
import Agent.Loop
    ( FileAttachment(..), ImageAttachment(..), TurnAttachment(..), TurnInput(..)
    , userMessageWithAttachments
    )
import Agent.ToolDispatch
    ( ToolCallKind(..), ToolCallResult(..), ToolCallMode(..)
    , ToolResultImage(..), ToolResultFile(..)
    )
import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (withAsync, cancel, wait, waitCatch)
import Control.Concurrent.STM (atomically, orElse, retry)
import Control.Exception.Safe (tryAny)
import qualified Data.ByteString as ByteString
import Data.Either (isLeft)
import qualified Data.Text as Text
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = do
  describe "pending input pure lifecycle" do
    it "retains the count budget through drain and retry, releasing it on commit" do
        let full = foldl (\s _ -> fst (Model.enqueueInput (UserMessage "x") s))
                Model.emptyPendingState [1 .. pendingInputCountLimit]
            (inflight, batch) = Model.drain full
            retried = fst (Model.requeue batch inflight)
            (again, retryBatch) = Model.drain retried
            committed = fst (Model.commit retryBatch again)
        Model.retainedCount inflight `shouldBe` pendingInputCountLimit
        Model.retainedBytes inflight `shouldBe` pendingInputCountLimit
        snd (Model.enqueueInput (UserMessage "") inflight) `shouldSatisfy` isLeft
        Model.batchInputs retryBatch `shouldBe` replicate pendingInputCountLimit (UserMessage "x")
        Model.retainedCount committed `shouldBe` 0
        Model.retainedBytes committed `shouldBe` 0
        snd (Model.enqueueInput (UserMessage "new") committed) `shouldBe` Right ()

    it "counts UTF-8 bytes in drained and newly queued inputs" do
        let large = userMessageWithAttachments "é"
                [FileAttachmentItem (FileAttachment Nothing ""
                    (ByteString.replicate (pendingInputByteLimit - 4) 0))]
            (initial, accepted) = Model.enqueueInput large Model.emptyPendingState
            (inflight, batch) = Model.drain initial
            (full, acceptedLast) = Model.enqueueInput (UserMessage "é") inflight
            committed = fst (Model.commit batch full)
        accepted `shouldBe` Right ()
        acceptedLast `shouldBe` Right ()
        Model.retainedBytes full `shouldBe` pendingInputByteLimit
        snd (Model.enqueueInput (UserMessage "x") full) `shouldSatisfy` isLeft
        Model.retainedBytes committed `shouldBe` 2
        Model.retainedCount committed `shouldBe` 1
        queuedInputs committed `shouldBe` [UserMessage "é"]

    it "invalidates in-flight batches on clear without releasing new-epoch budget" do
        let old = fst (Model.enqueueInput (UserMessage "old") Model.emptyPendingState)
            (inflight, batch) = Model.drain old
            cleared = fst (Model.clearPendingState inflight)
            fresh = fst (Model.enqueueInput (UserMessage "fresh") cleared)
            staleFailure = fst (Model.requeue batch fresh)
            staleSuccess = fst (Model.commit batch fresh)
        map queuedInputs [cleared, staleFailure, staleSuccess]
            `shouldBe` [[], [UserMessage "fresh"], [UserMessage "fresh"]]
        map Model.retainedCount [cleared, staleFailure, staleSuccess]
            `shouldBe` [0, 1, 1]
        map Model.retainedBytes [cleared, staleFailure, staleSuccess]
            `shouldBe` [0, 5, 5]

    it "requeues ordered messages but only the newest in-flight MCP snapshot" do
        let old = fst (Model.enqueueInput (UserMessage "old") Model.emptyPendingState)
            notice = fst (Model.enqueueNotice PendingMcpNotice (UserMessage "connecting") old)
            completion = fst (Model.enqueueNotice PendingSubagentNotice (UserMessage "done") notice)
            (inflight, batch) = Model.drain completion
            newer = fst (Model.enqueueNotice PendingMcpNotice (UserMessage "settled") inflight)
            latest = fst (Model.enqueueNotice PendingMcpNotice (UserMessage "latest") newer)
            current = fst (Model.enqueueInput (UserMessage "new") latest)
            retried = fst (Model.requeue batch current)
            expected = map UserMessage ["old", "done", "latest", "new"]
            (drained, retryBatch) = Model.drain retried
            committed = fst (Model.commit retryBatch drained)
        queuedInputs retried `shouldBe` expected
        Model.retainedCount retried `shouldBe` 4
        Model.retainedBytes retried `shouldBe` sum (map logicalTurnInputBytes expected)
        queuedInputs committed `shouldBe` []
        Model.retainedBytes committed `shouldBe` 0

    it "preserves the old notice on rejected replacement and resets omission reporting at drain and clear" do
        let old = fst (Model.enqueueNotice PendingMcpNotice (UserMessage "old") Model.emptyPendingState)
            oversized = userMessageWithAttachments ""
                [FileAttachmentItem (FileAttachment Nothing ""
                    (ByteString.replicate (pendingInputByteLimit + 1) 0))]
            omit = Model.enqueueNotice PendingMcpNotice oversized
            (reported, first) = omit old
            (suppressed, second) = omit reported
            (inflight, batch) = Model.drain suppressed
            (reportedAgain, afterDrain) = omit inflight
            retried = fst (Model.requeue batch reportedAgain)
            (_, afterRetry) = omit retried
            (_, afterClear) = omit (fst (Model.clearPendingState retried))
        first `shouldBe` Left "Root input queue is full; one or more background notices were omitted."
        second `shouldBe` Right ()
        afterDrain `shouldBe` first
        afterRetry `shouldBe` Right ()
        afterClear `shouldBe` first
        queuedInputs suppressed `shouldBe` [UserMessage "old"]
        queuedInputs retried `shouldBe` [UserMessage "old"]
        Model.retainedBytes retried `shouldBe` 3

    it "commits only the drained batch, leaving arrivals for the next submission" do
        let old = fst (Model.enqueueNotice PendingMcpNotice (UserMessage "old") Model.emptyPendingState)
            (inflight, batch) = Model.drain old
            current = fst (Model.enqueueNotice PendingMcpNotice (UserMessage "new") inflight)
            committed = fst (Model.commit batch current)
        queuedInputs committed `shouldBe` [UserMessage "new"]
        Model.retainedCount committed `shouldBe` 1
        Model.retainedBytes committed `shouldBe` 3

  describe "logicalTurnInputBytes" do
    it "bounds recovery context by the same retained input budget" do
        let oversized = RecoveryMessage (Text.replicate (pendingInputByteLimit + 1) "x")
        snd (Model.enqueueInput oversized Model.emptyPendingState) `shouldSatisfy` isLeft

    it "counts native file content and UTF-8 metadata against the retained queue budget" do
        let metadata = logicalTextBytes "über.pdf" + logicalTextBytes "application/pdf"
            contentLength = pendingInputByteLimit - metadata - 4
            file = ToolResultFile "über.pdf" "application/pdf"
                (ByteString.replicate contentLength 0)
            result = ToolCallResultWithFiles
                "id" "ok" FunctionCallKind BlockingToolCall [] Nothing [file]
            input = CompletedTool result
            (queued, accepted) = Model.enqueueInput input Model.emptyPendingState
            (inflight, batch) = Model.drain queued
            retried = fst (Model.requeue batch inflight)
            committed = fst (Model.commit batch inflight)
        logicalTurnInputBytes input `shouldBe` pendingInputByteLimit
        accepted `shouldBe` Right ()
        map Model.retainedBytes [queued, inflight, retried]
            `shouldBe` replicate 3 pendingInputByteLimit
        snd (Model.enqueueInput input inflight) `shouldSatisfy` isLeft
        Model.retainedBytes committed `shouldBe` 0
        snd (Model.enqueueInput input committed) `shouldBe` Right ()

    it "counts ordered attachments and rich tool-result images" do
        let attached =
                userMessageWithAttachments "é"
                    [ ImageAttachmentItem
                        (ImageAttachment "image/png" "abc")
                    , FileAttachmentItem
                        (FileAttachment (Just "a") "text/plain" "xy")
                    ]
            richResult =
                CompletedTool
                    (ToolCallResult
                        { callId = "id"
                        , toolResultMode = BlockingToolCall
                        , toolResultOutcome = Nothing
                        , output = "ok"
                        , callKind = FunctionCallKind
                        , toolResultImages =
                            [ ToolResultImage
                                "data:image/png;base64,abc"
                                (Just "high")
                            ]
                        })
        logicalTurnInputBytes attached `shouldBe`
            logicalTextBytes "é"
                + logicalTextBytes "image/png" + 3
                + logicalTextBytes "a"
                + logicalTextBytes "text/plain" + 2
        logicalTurnInputBytes richResult `shouldBe`
            logicalTextBytes "id"
                + logicalTextBytes "ok"
                + logicalTextBytes "data:image/png;base64,abc"
                + logicalTextBytes "high"


  describe "InputQueue STM composition" do
    it "rolls back queue admission when the surrounding transaction retries" do
        inputs <- newInputQueue
        atomically $
            (enqueueSteeringInputsSTM inputs [UserMessage "rolled back"] >> retry)
                `orElse` pure ()
        readSteeringInputs inputs `shouldReturn` []
        hasSteeringInputWake inputs `shouldReturn` False
        enqueueSteeringInputs inputs
            (replicate steeringInputCountLimit (UserMessage "accepted"))
            `shouldReturn` Right ()

    it "observes input accepted in the same transaction" do
        inputs <- newInputQueue
        atomically do
            result <- enqueueSteeringInputsSTM inputs [UserMessage "accepted"]
            awaitSteeringInputReady inputs
            pure result
          `shouldReturn` Right ()
        readSteeringInputs inputs `shouldReturn` [UserMessage "accepted"]

  describe "InputQueue pending batch ownership" do
    it "restores the batch when evaluating the submission result fails" do
        inputs <- newPendingInputs
        enqueuePendingInput inputs (UserMessage "retained") `shouldReturn` Right ()
        result <- tryAny $ withPendingInputBatch inputs \_ ->
            pure (error "submission result failure" :: Either Text.Text ())
        result `shouldSatisfy` isLeft
        timeout 1000000
            (withPendingInputBatch inputs
                (\queued -> pure (Right queued :: Either Text.Text [TurnInput])))
            `shouldReturn` Just (Right [UserMessage "retained"])

    it "keeps submission ownership across reset and cancellation of a waiting submitter" do
        inputs <- newPendingInputs
        enqueuePendingInput inputs (UserMessage "old") `shouldReturn` Right ()
        entered <- newEmptyMVar
        finish <- newEmptyMVar
        let first queued = do
                queued `shouldBe` [UserMessage "old"]
                putMVar entered ()
                takeMVar finish
                pure (Left "retry" :: Either Text.Text ())
        withAsync (withPendingInputBatch inputs first) \owner -> do
            takeMVar entered
            clearPendingInputs inputs
            enqueuePendingInput inputs (UserMessage "new") `shouldReturn` Right ()
            waiting <- newEmptyMVar
            submitted <- newEmptyMVar
            let second = do
                    putMVar waiting ()
                    withPendingInputBatch inputs \_ -> do
                        putMVar submitted ()
                        pure (Right () :: Either Text.Text ())
            withAsync second \waiter -> do
                takeMVar waiting
                timeout 20000 (takeMVar submitted) `shouldReturn` Nothing
                cancel waiter
                waitCatch waiter >>= (`shouldSatisfy` isLeft)
            putMVar finish ()
            wait owner `shouldReturn` Left "retry"
        withPendingInputBatch inputs
            (\queued -> pure (Right queued :: Either Text.Text [TurnInput]))
            `shouldReturn` Right [UserMessage "new"]

  describe "InputQueue steering" do
    it "observes idle readiness without consuming the wake or pending input" do
        steering <- newInputQueue
        let ready = atomically $
                (awaitSteeringInputReady steering >> pure True) `orElse` pure False
            completion = UserMessage "completed"
        ready `shouldReturn` False
        enqueueBackgroundCompletion steering "build" completion `shouldReturn` Right True
        ready `shouldReturn` True
        ready `shouldReturn` True
        hasSteeringInputWake steering `shouldReturn` True
        readSteeringInputs steering `shouldReturn` [completion]
        atomically (awaitSteeringInput steering)
        ready `shouldReturn` False
        readSteeringInputs steering `shouldReturn` [completion]

    it "retains completions beyond the count limit and delivers bounded batches" do
        steering <- newInputQueue
        let guidance = replicate steeringInputCountLimit (UserMessage "guidance")
            completion = UserMessage "completed"
        enqueueSteeringInputs steering guidance `shouldReturn` Right ()
        suppressUserSteeringWake steering
        enqueueBackgroundCompletion steering "first" completion `shouldReturn` Right True
        enqueueBackgroundCompletion steering "second" completion `shouldReturn` Right True
        enqueueBackgroundCompletion steering "first" completion `shouldReturn` Right False
        suppressUserSteeringWake steering
        hasBackgroundCompletions steering `shouldReturn` True
        hasSteeringInputWake steering `shouldReturn` True
        atomically (awaitSteeringInput steering)
        hasSteeringInputWake steering `shouldReturn` False
        readSteeringInputs steering `shouldReturn` guidance
        commitSteeringInputs steering 1
        readSteeringInputs steering `shouldReturn` (drop 1 guidance <> [completion])
        hasSteeringInputWake steering `shouldReturn` True
        commitSteeringInputs steering steeringInputCountLimit
        readSteeringInputs steering `shouldReturn` [completion]
        commitSteeringInputs steering 1
        hasBackgroundCompletions steering `shouldReturn` False

    it "retains completions beyond the byte limit until guidance is acknowledged" do
        steering <- newInputQueue
        -- Binary payload accounting is O(1), avoiding a 64 MiB interpreted
        -- character fold in GHCi while exercising the same byte-budget limit.
        let guidance = userMessageWithAttachments ""
                [FileAttachmentItem (FileAttachment Nothing ""
                    (ByteString.replicate steeringInputByteLimit 0))]
            completion = UserMessage "completed"
        enqueueSteeringInputs steering [guidance] `shouldReturn` Right ()
        enqueueBackgroundCompletion steering "build" completion `shouldReturn` Right True
        readSteeringInputs steering `shouldReturn` [guidance]
        commitSteeringInputs steering 1
        readSteeringInputs steering `shouldReturn` [completion]

    it "promotes deferred completions when an earlier notice is explicitly collected" do
        steering <- newInputQueue
        let guidance = replicate (steeringInputCountLimit - 1) (UserMessage "guidance")
            first = UserMessage "first completed"
            second = UserMessage "second completed"
        enqueueSteeringInputs steering guidance `shouldReturn` Right ()
        enqueueBackgroundCompletion steering "first" first `shouldReturn` Right True
        enqueueBackgroundCompletion steering "second" second `shouldReturn` Right True
        dismissBackgroundCompletion steering "first"
        readSteeringInputs steering `shouldReturn` (guidance <> [second])
        dismissBackgroundCompletion steering "second"
        readSteeringInputs steering `shouldReturn` guidance
        hasBackgroundCompletions steering `shouldReturn` False

    it "dismisses deferred notices and isolates them across conversation resets" do
        steering <- newInputQueue
        publish <- prepareBackgroundCompletion steering
        let guidance = replicate steeringInputCountLimit (UserMessage "guidance")
            completion = UserMessage "completed"
        enqueueSteeringInputs steering guidance `shouldReturn` Right ()
        publish "dismissed" completion `shouldReturn` Right True
        dismissBackgroundCompletion steering "dismissed"
        hasBackgroundCompletions steering `shouldReturn` False
        publish "old-conversation" completion `shouldReturn` Right True
        clearSteeringInputs steering
        publish "late-completion" completion `shouldReturn` Right False
        readSteeringInputs steering `shouldReturn` []
        hasBackgroundCompletions steering `shouldReturn` False
        hasSteeringInputWake steering `shouldReturn` False

    it "wakes passive waits without consuming guidance or its idle notification" do
        steering <- newInputQueue
        let ready = atomically $
                (awaitUserSteering steering >> pure True) `orElse` pure False
            guidance = [UserMessage "how long will it take?"]
        ready `shouldReturn` False
        enqueueBackgroundCompletion steering "build" (UserMessage "completed")
            `shouldReturn` Right True
        ready `shouldReturn` False
        enqueueSteeringInputs steering guidance `shouldReturn` Right ()
        ready `shouldReturn` True
        ready `shouldReturn` True
        hasSteeringInputWake steering `shouldReturn` True
        readSteeringInputs steering `shouldReturn` (UserMessage "completed" : guidance)
        atomically (awaitSteeringInput steering)
        ready `shouldReturn` True
        commitSteeringInputs steering 2
        ready `shouldReturn` False

    it "interrupts only for user guidance beyond the submitted prefix" do
        steering <- newInputQueue
        let ready count = atomically $
                (awaitUserSteeringAfter steering count >> pure True)
                    `orElse` pure False
            first = UserMessage "use the existing schema"
            second = UserMessage "also add tests"
            completion = UserMessage "build completed"
        ready 0 `shouldReturn` False
        enqueueSteeringInputs steering [first] `shouldReturn` Right ()
        ready 0 `shouldReturn` True
        ready 1 `shouldReturn` False
        enqueueBackgroundCompletion steering "build" completion
            `shouldReturn` Right True
        ready 1 `shouldReturn` False
        enqueueSteeringInputs steering [second] `shouldReturn` Right ()
        ready 1 `shouldReturn` True
        ready 2 `shouldReturn` True
        ready 3 `shouldReturn` False
        ready 1 `shouldReturn` True
        readSteeringInputs steering `shouldReturn` [first, completion, second]
        hasSteeringInputWake steering `shouldReturn` True
        commitSteeringInputs steering 2
        ready 0 `shouldReturn` True
        ready 1 `shouldReturn` False

    it "retains dismissed submitted notices without hiding or acknowledging new guidance" do
        steering <- newInputQueue
        let completion = UserMessage "build completed"
            guidance = UserMessage "change the implementation"
        enqueueBackgroundCompletion steering "build" completion
            `shouldReturn` Right True
        submitted <- reserveSteeringInputs steering
        submitted `shouldBe` [completion]
        dismissBackgroundCompletion steering "build"
        enqueueSteeringInputs steering [guidance] `shouldReturn` Right ()
        atomically
            ((awaitUserSteeringAfter steering (length submitted) >> pure True)
                `orElse` pure False)
            `shouldReturn` True
        readSteeringInputs steering `shouldReturn` [completion, guidance]
        commitSteeringInputs steering (length submitted)
        readSteeringInputs steering `shouldReturn` [guidance]
        hasBackgroundCompletions steering `shouldReturn` False

    it "allows dismissal beyond the reserved prefix and releases reservations on reset" do
        steering <- newInputQueue
        let completion = UserMessage "submitted completion"
        enqueueBackgroundCompletion steering "submitted" completion
            `shouldReturn` Right True
        reserveSteeringInputs steering `shouldReturn` [completion]
        enqueueBackgroundCompletion steering "unsubmitted" (UserMessage "later completion")
            `shouldReturn` Right True
        dismissBackgroundCompletion steering "unsubmitted"
        readSteeringInputs steering `shouldReturn` [completion]
        clearSteeringInputs steering
        enqueueBackgroundCompletion steering "new-conversation" completion
            `shouldReturn` Right True
        dismissBackgroundCompletion steering "new-conversation"
        readSteeringInputs steering `shouldReturn` []

    it "reserves idle-turn and final-close snapshots against dismissal" do
        steering <- newInputQueue
        let completion = UserMessage "completed"
        enqueueBackgroundCompletion steering "idle" completion
            `shouldReturn` Right True
        readSteeringTurn steering `shouldReturn` ("", [completion])
        dismissBackgroundCompletion steering "idle"
        readSteeringInputs steering `shouldReturn` [completion]
        commitSteeringInputs steering 1
        enqueueBackgroundCompletion steering "closing" completion
            `shouldReturn` Right True
        closeSteeringInputs steering `shouldReturn` [completion]
        dismissBackgroundCompletion steering "closing"
        readSteeringInputs steering `shouldReturn` [completion]
        commitSteeringInputs steering 1
        readSteeringInputs steering `shouldReturn` []

    it "retains the remaining reservation after a partial acknowledgement" do
        steering <- newInputQueue
        let first = UserMessage "first completion"
            second = UserMessage "second completion"
            guidance = UserMessage "new guidance"
        enqueueBackgroundCompletion steering "first" first
            `shouldReturn` Right True
        enqueueBackgroundCompletion steering "second" second
            `shouldReturn` Right True
        reserveSteeringInputs steering `shouldReturn` [first, second]
        commitSteeringInputs steering 1
        dismissBackgroundCompletion steering "second"
        enqueueSteeringInputs steering [guidance] `shouldReturn` Right ()
        commitSteeringInputs steering 1
        readSteeringInputs steering `shouldReturn` [guidance]

    it "broadcasts new guidance beyond a submitted prefix to concurrent waits" do
        steering <- newInputQueue
        enqueueSteeringInputs steering [UserMessage "already submitted"]
            `shouldReturn` Right ()
        let completed (Just (Right ())) = True
            completed _ = False
        withAsync (atomically (awaitUserSteeringAfter steering 1)) \first ->
            withAsync (atomically (awaitUserSteeringAfter steering 1)) \second -> do
                enqueueSteeringInputs steering [UserMessage "new instruction"]
                    `shouldReturn` Right ()
                timeout 1000000 (waitCatch first) >>= (`shouldSatisfy` completed)
                timeout 1000000 (waitCatch second) >>= (`shouldSatisfy` completed)
                readSteeringInputs steering `shouldReturn`
                    [UserMessage "already submitted", UserMessage "new instruction"]

    it "broadcasts arriving guidance to concurrent passive waits" do
        steering <- newInputQueue
        let completed (Just (Right ())) = True
            completed _ = False
        withAsync (atomically (awaitUserSteering steering)) \first ->
            withAsync (atomically (awaitUserSteering steering)) \second -> do
                enqueueSteeringInputs steering [UserMessage "status please"]
                    `shouldReturn` Right ()
                timeout 1000000 (waitCatch first) >>= (`shouldSatisfy` completed)
                timeout 1000000 (waitCatch second) >>= (`shouldSatisfy` completed)
                readSteeringInputs steering `shouldReturn` [UserMessage "status please"]

    it "snapshots idle guidance as turn text without consuming or duplicating inputs" do
        steering <- newInputQueue
        let guidance = [UserMessage "make a pr", UserMessage "include tests"]
        enqueueSteeringInputs steering guidance `shouldReturn` Right ()
        atomically (awaitSteeringInput steering)
        readSteeringTurn steering `shouldReturn`
            ("make a pr\n\ninclude tests", guidance)
        -- A failed attempt can read the same inputs again. Only the normal
        -- provider acknowledgement removes them.
        readSteeringTurn steering `shouldReturn`
            ("make a pr\n\ninclude tests", guidance)
        readSteeringInputs steering `shouldReturn` guidance
        commitSteeringInputs steering (length guidance)
        readSteeringTurn steering `shouldReturn` ("", [])

    it "preserves attachment guidance text but excludes background notices from user text" do
        steering <- newInputQueue
        let attached = userMessageWithAttachments "inspect this"
                [ImageAttachmentItem (ImageAttachment "image/png" "abc")]
            background = UserMessage "background tool completed"
        enqueueBackgroundCompletion steering "tool" background
            `shouldReturn` Right True
        readSteeringTurn steering `shouldReturn` ("", [background])
        enqueueSteeringInputs steering [attached] `shouldReturn` Right ()
        readSteeringTurn steering `shouldReturn`
            ("inspect this", [background, attached])

    it "does not wake for an empty enqueue" do
        steering <- newInputQueue
        enqueueSteeringInputs steering [] `shouldReturn` Right ()
        readSteeringInputs steering `shouldReturn` []
        hasSteeringInputWake steering `shouldReturn` False
        timeout 10000 (atomically (awaitSteeringInput steering))
            `shouldReturn` Nothing

    it "bounds, commits, and admits steering inputs in order" do
        steering <- newInputQueue
        let queued =
                [ UserMessage (Text.pack (show index))
                | index <- [1 .. steeringInputCountLimit]
                ]
        enqueueSteeringInputs steering queued `shouldReturn` Right ()
        enqueueSteeringInputs steering [UserMessage "overflow"]
            `shouldReturn`
                Left
                    "Steering queue is full; wait for the active turn to consume guidance."
        commitSteeringInputs steering 2
        enqueueSteeringInputs steering
            [UserMessage "new-1", UserMessage "new-2"]
            `shouldReturn` Right ()
        readSteeringInputs steering `shouldReturn`
            drop 2 queued <> [UserMessage "new-1", UserMessage "new-2"]

    it "hands late guidance to a finishing loop and closes only when idle" do
        steering <- newInputQueue
        enqueueSteeringInputs steering [UserMessage "use unit 2"]
            `shouldReturn` Right ()
        closeSteeringInputs steering `shouldReturn` [UserMessage "use unit 2"]
        enqueueSteeringInputs steering [UserMessage "and 3"]
            `shouldReturn` Right ()
        commitSteeringInputs steering 2
        closeSteeringInputs steering `shouldReturn` []
        enqueueSteeringInputs steering [UserMessage "too late"]
            `shouldReturn`
                Left "The turn has already answered; submit guidance as a new turn."
        readSteeringInputs steering `shouldReturn` []
        clearSteeringInputs steering
        enqueueSteeringInputs steering [UserMessage "new conversation"]
            `shouldReturn` Right ()

    it "wakes for ordinary input, retains it until commit, and does not hot-loop" do
        steering <- newInputQueue
        enqueueSteeringInputs steering [UserMessage "make a pr"]
            `shouldReturn` Right ()
        hasSteeringInputWake steering `shouldReturn` True
        timeout 100000 (atomically (awaitSteeringInput steering))
            `shouldReturn` Just ()
        readSteeringInputs steering `shouldReturn` [UserMessage "make a pr"]
        hasSteeringInputWake steering `shouldReturn` False
        timeout 10000 (atomically (awaitSteeringInput steering))
            `shouldReturn` Nothing
        commitSteeringInputs steering 1
        readSteeringInputs steering `shouldReturn` []

    it "preserves the wake for guidance arriving after the active turn snapshot" do
        steering <- newInputQueue
        enqueueSteeringInputs steering [UserMessage "first"]
            `shouldReturn` Right ()
        snapshot <- readSteeringInputs steering
        enqueueSteeringInputs steering [UserMessage "late"]
            `shouldReturn` Right ()
        commitSteeringInputs steering (length snapshot)
        timeout 100000 (atomically (awaitSteeringInput steering))
            `shouldReturn` Just ()
        readSteeringInputs steering `shouldReturn` [UserMessage "late"]

    it "does not wake for inputs already committed by the active turn" do
        steering <- newInputQueue
        enqueueSteeringInputs steering [UserMessage "consumed"]
            `shouldReturn` Right ()
        commitSteeringInputs steering 1
        hasSteeringInputWake steering `shouldReturn` False
        timeout 10000 (atomically (awaitSteeringInput steering))
            `shouldReturn` Nothing

    it "wakes again for new guidance after a previous wake was consumed" do
        steering <- newInputQueue
        enqueueSteeringInputs steering [UserMessage "first"]
            `shouldReturn` Right ()
        atomically (awaitSteeringInput steering)
        enqueueSteeringInputs steering [UserMessage "second"]
            `shouldReturn` Right ()
        timeout 100000 (atomically (awaitSteeringInput steering))
            `shouldReturn` Just ()
        readSteeringInputs steering `shouldReturn`
            [UserMessage "first", UserMessage "second"]
        clearSteeringInputs steering
        readSteeringInputs steering `shouldReturn` []
        hasSteeringInputWake steering `shouldReturn` False

    it "suppresses cancelled guidance wakes without dropping input or future wakes" do
        steering <- newInputQueue
        enqueueSteeringInputs steering [UserMessage "cancelled guidance"]
            `shouldReturn` Right ()
        suppressUserSteeringWake steering
        hasSteeringInputWake steering `shouldReturn` False
        readSteeringInputs steering `shouldReturn`
            [UserMessage "cancelled guidance"]
        enqueueSteeringInputs steering [UserMessage "new guidance"]
            `shouldReturn` Right ()
        hasSteeringInputWake steering `shouldReturn` True

    it "preserves background wakes when suppressing cancelled guidance" do
        steering <- newInputQueue
        enqueueBackgroundCompletion steering "task-1" (UserMessage "completed")
            `shouldReturn` Right True
        suppressUserSteeringWake steering
        hasSteeringInputWake steering `shouldReturn` True

    it "deduplicates keyed background completions and dismisses them" do
        steering <- newInputQueue
        enqueueSteeringInputs steering [UserMessage "ordinary"]
            `shouldReturn` Right ()
        timeout 100000 (atomically (awaitSteeringInput steering))
            `shouldReturn` Just ()

        enqueueBackgroundCompletion
            steering
            "task-1"
            (UserMessage "completed")
            `shouldReturn` Right True
        enqueueBackgroundCompletion
            steering
            "task-1"
            (UserMessage "duplicate")
            `shouldReturn` Right False
        hasBackgroundCompletions steering `shouldReturn` True
        timeout 100000 (atomically (awaitSteeringInput steering))
            `shouldReturn` Just ()
        timeout 10000 (atomically (awaitSteeringInput steering))
            `shouldReturn` Nothing
        readSteeringInputs steering `shouldReturn`
            [UserMessage "ordinary", UserMessage "completed"]

        dismissBackgroundCompletion steering "task-1"
        hasBackgroundCompletions steering `shouldReturn` False
        readSteeringInputs steering `shouldReturn` [UserMessage "ordinary"]

queuedInputs :: Model.InputQueueState -> [TurnInput]
queuedInputs = Model.batchInputs . snd . Model.drain
