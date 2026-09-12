module Agent.CLI.PendingInputsSpec (spec) where

import Agent.CLI.InputBudget
    ( logicalTextBytes
    , logicalTurnInputBytes
    )
import qualified Agent.CLI.PendingInputs.Model as Model
import Control.Concurrent.Async (withAsync, cancel, waitCatch)
import Agent.CLI.PendingInputs
    ( PendingNoticeKind(..)
    , clearPendingInputs
    , enqueuePendingInput
    , enqueuePendingNotice
    , newPendingInputs
    , pendingInputByteLimit
    , pendingInputCountLimit
    , withPendingInputs
    )
import Agent.CLI.SteeringInputs
    ( awaitSteeringInput
    , clearSteeringInputs
    , commitSteeringInputs
    , dismissBackgroundCompletion
    , enqueueBackgroundCompletion
    , enqueueSteeringInputs
    , hasBackgroundCompletions
    , hasSteeringInputWake
    , newSteeringInputs
    , readSteeringInputs
    , readSteeringTurn
    , steeringInputCountLimit
    , suppressUserSteeringWake
    )
import Agent.Error (ApiError(..))
import Agent.Loop
    ( Backend(..)
    , BackendCallbacks(..)
    , BackendResult(..)
    , FileAttachment(..)
    , ImageAttachment(..)
    , TurnAttachment(..)
    , TurnInput(..)
    , emptyBackendSnapshot
    , emptyTurnOutput
    , userMessageWithAttachments
    , backendWithCallbacks
    )
import Agent.ToolDispatch
    ( ToolCallKind(..)
    , ToolCallResult(..)
    , ToolCallMode(..)
    , ToolResultImage(..)
    )
import Control.Concurrent
    ( forkIO
    , newEmptyMVar
    , putMVar
    , takeMVar
    )
import Control.Exception.Safe (tryAny)
import Control.Concurrent.STM (atomically)
import Data.Either (isLeft)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
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
        let large = UserMessage (Text.replicate (pendingInputByteLimit `div` 2 - 1) "é")
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
            oversized = UserMessage (Text.replicate (pendingInputByteLimit + 1) "x")
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

  describe "withPendingInputs" do
    it "preserves recovery callbacks and requeues when a callback throws" do
        pending <- newPendingInputs
        enqueuePendingInput pending (UserMessage "old") `shouldReturn` Right ()
        seen <- newIORef []
        checkpoints <- newIORef []
        let backend = withPendingInputs pending $ backendWithCallbacks
                \state _ inputs callbacks -> do
                    modifyIORef' seen (<> [inputs])
                    callbacks.onRecoveryCheckpoint "checkpoint"
                    pure $ Right BackendResult
                        { backendOutput = emptyTurnOutput "ok" [] Nothing
                        , backendState = state
                        }
            callbacks = BackendCallbacks
                { onLoopEvent = const (pure ())
                , onAsyncToolCall = const (pure ())
                , onRecoveryCheckpoint = \value -> do
                    modifyIORef' checkpoints (<> [value])
                    enqueuePendingInput pending (UserMessage "arrival") `shouldReturn` Right ()
                    ioError (userError "callback failed")
                , onCompletedResponseItem = \_ _ -> pure ()
                , onCancellationMode = const (pure ())
                }
        result <- tryAny $ backend.submitTurnWithCallbacks emptyBackendSnapshot Nothing
            [UserMessage "parent"] callbacks
        result `shouldSatisfy` isLeft
        readIORef checkpoints `shouldReturn` ["checkpoint"]
        _ <- backend.submitTurn emptyBackendSnapshot Nothing [] (const (pure ()))
        _ <- backend.submitTurn emptyBackendSnapshot Nothing [] (const (pure ()))
        readIORef seen `shouldReturn`
            [ [UserMessage "old", UserMessage "parent"]
            , [UserMessage "old", UserMessage "arrival"]
            , []
            ]

    it "requeues after asynchronous cancellation and releases the lifecycle lock" do
        pending <- newPendingInputs
        enqueuePendingInput pending (UserMessage "old") `shouldReturn` Right ()
        entered <- newEmptyMVar
        blocked <- newEmptyMVar
        let backend = withPendingInputs pending $ Backend
                \_ _ _ _ -> putMVar entered () >> takeMVar blocked
        timeout 2000000 (withAsync
            (backend.submitTurn emptyBackendSnapshot Nothing [] (const (pure ())))
            \worker -> do
                takeMVar entered
                enqueuePendingInput pending (UserMessage "new") `shouldReturn` Right ()
                cancel worker
                waitCatch worker >>= (\result -> result `shouldSatisfy` isLeft))
            `shouldReturn` Just ()
        seen <- newIORef []
        let retry = withPendingInputs pending $ Backend
                \state _ inputs _ -> do
                    writeIORef seen inputs
                    pure $ Right BackendResult
                        { backendOutput = emptyTurnOutput "ok" [] Nothing
                        , backendState = state
                        }
        timeout 2000000
            (retry.submitTurn emptyBackendSnapshot Nothing [] (const (pure ())) >> pure ())
            `shouldReturn` Just ()
        readIORef seen `shouldReturn` [UserMessage "old", UserMessage "new"]
        _ <- retry.submitTurn emptyBackendSnapshot Nothing [] (const (pure ()))
        readIORef seen `shouldReturn` []

    it "commits queued inputs after a successful submission" do
        pending <- newPendingInputs
        enqueuePendingInput pending (UserMessage "child result")
        seen <- newIORef []
        let backend = withPendingInputs pending $ Backend
                \state _ inputs _ -> do
                    writeIORef seen inputs
                    pure $ Right BackendResult
                        { backendOutput =
                            emptyTurnOutput "response" [] Nothing
                        , backendState = state
                        }
        _ <- backend.submitTurn emptyBackendSnapshot Nothing
            [UserMessage "parent"] (const (pure ()))
        readIORef seen `shouldReturn`
            [UserMessage "child result", UserMessage "parent"]

    it "requeues inputs when the backend returns an error" do
        let queued = [UserMessage "child result"]
        pending <- newPendingInputs
        mapM_ (enqueuePendingInput pending) queued
        let backend = withPendingInputs pending $ Backend
                \_ _ _ _ -> pure (Left (ConnectionError "offline"))
        _ <- backend.submitTurn emptyBackendSnapshot Nothing
            [UserMessage "parent"] (const (pure ()))
        seen <- newIORef []
        let retry = withPendingInputs pending $ Backend
                \state _ inputs _ -> do
                    writeIORef seen inputs
                    pure $ Right BackendResult
                        { backendOutput = emptyTurnOutput "response" [] Nothing
                        , backendState = state
                        }
        _ <- retry.submitTurn emptyBackendSnapshot Nothing [] (const (pure ()))
        readIORef seen `shouldReturn` queued

    it "requeues inputs when submission is interrupted by an exception" do
        let queued = [UserMessage "child result"]
        pending <- newPendingInputs
        mapM_ (enqueuePendingInput pending) queued
        let backend = withPendingInputs pending $ Backend
                \_ _ _ _ -> ioError (userError "interrupted")
        result <- tryAny $
            backend.submitTurn emptyBackendSnapshot Nothing
                [UserMessage "parent"] (const (pure ()))
        result `shouldSatisfy` isLeft
        seen <- newIORef []
        let retry = withPendingInputs pending $ Backend
                \state _ inputs _ -> do
                    writeIORef seen inputs
                    pure $ Right BackendResult
                        { backendOutput = emptyTurnOutput "response" [] Nothing
                        , backendState = state
                        }
        _ <- retry.submitTurn emptyBackendSnapshot Nothing [] (const (pure ()))
        readIORef seen `shouldReturn` queued

    it "prepends drained inputs ahead of concurrent enqueues when requeuing" do
        pending <- newPendingInputs
        enqueuePendingInput pending (UserMessage "old")
        entered <- newEmptyMVar
        release <- newEmptyMVar
        let backend = withPendingInputs pending $ Backend
                \_ _ _ _ -> do
                    putMVar entered ()
                    takeMVar release
                    pure (Left (ConnectionError "offline"))
        done <- newEmptyMVar
        _ <- forkIO $ backend.submitTurn emptyBackendSnapshot Nothing [] (const (pure ())) >>= putMVar done
        takeMVar entered
        enqueuePendingInput pending (UserMessage "new")
        putMVar release ()
        _ <- takeMVar done
        seen <- newIORef []
        let retry = withPendingInputs pending $ Backend
                \state _ inputs _ -> do
                    writeIORef seen inputs
                    pure $ Right BackendResult
                        { backendOutput = emptyTurnOutput "response" [] Nothing
                        , backendState = state
                        }
        _ <- retry.submitTurn emptyBackendSnapshot Nothing [] (const (pure ()))
        readIORef seen `shouldReturn`
            [UserMessage "old", UserMessage "new"]

    it "clears queued inputs" do
        pending <- newPendingInputs
        enqueuePendingInput pending (UserMessage "stale")
        clearPendingInputs pending
        seen <- newIORef []
        let backend = withPendingInputs pending $ Backend
                \state _ inputs _ -> do
                    writeIORef seen inputs
                    pure $ Right BackendResult
                        { backendOutput = emptyTurnOutput "response" [] Nothing
                        , backendState = state
                        }
        _ <- backend.submitTurn emptyBackendSnapshot Nothing [] (const (pure ()))
        readIORef seen `shouldReturn` []

    it "does not resurrect a batch cleared during an in-flight failure" do
        pending <- newPendingInputs
        enqueuePendingInput pending (UserMessage "stale")
        entered <- newEmptyMVar
        release <- newEmptyMVar
        let backend = withPendingInputs pending $ Backend
                \_ _ _ _ -> do
                    putMVar entered ()
                    takeMVar release
                    pure (Left (ConnectionError "offline"))
        done <- newEmptyMVar
        _ <- forkIO $ backend.submitTurn emptyBackendSnapshot Nothing [] (const (pure ())) >>= putMVar done
        takeMVar entered
        clearPendingInputs pending
        putMVar release ()
        _ <- takeMVar done
        seen <- newIORef []
        let retry = withPendingInputs pending $ Backend
                \state _ inputs _ -> do
                    writeIORef seen inputs
                    pure $ Right BackendResult
                        { backendOutput = emptyTurnOutput "response" [] Nothing
                        , backendState = state
                        }
        _ <- retry.submitTurn emptyBackendSnapshot Nothing [] (const (pure ()))
        readIORef seen `shouldReturn` []

    it "serializes concurrent submission batches" do
        pending <- newPendingInputs
        enqueuePendingInput pending (UserMessage "first")
        entered <- newEmptyMVar
        release <- newEmptyMVar
        seen <- newIORef []
        let backend = withPendingInputs pending $ Backend
                \state _ inputs _ -> do
                    modifyIORef' seen (<> [inputs])
                    putMVar entered ()
                    takeMVar release
                    pure $ Left (ConnectionError "offline")
        done1 <- newEmptyMVar
        done2 <- newEmptyMVar
        _ <- forkIO $ backend.submitTurn emptyBackendSnapshot Nothing [] (const (pure ())) >>= putMVar done1
        takeMVar entered
        enqueuePendingInput pending (UserMessage "second")
        _ <- forkIO $ backend.submitTurn emptyBackendSnapshot Nothing [] (const (pure ())) >>= putMVar done2
        putMVar release ()
        _ <- takeMVar done1
        -- The second submission cannot overtake the first batch.
        takeMVar entered
        putMVar release ()
        _ <- takeMVar done2
        readIORef seen `shouldReturn`
            [ [UserMessage "first"]
            , [UserMessage "first", UserMessage "second"]
            ]

    it "rejects explicit inputs after the count budget" do
        pending <- newPendingInputs
        accepted <- mapM
            (enqueuePendingInput pending . UserMessage . Text.pack . show)
            [1 .. pendingInputCountLimit]
        accepted `shouldSatisfy` all (== Right ())
        enqueuePendingInput pending (UserMessage "overflow")
            `shouldReturn`
                Left
                    "Root input queue is full; wait for the root agent to consume pending messages."

    it "coalesces queued MCP snapshots to the latest value" do
        pending <- newPendingInputs
        enqueuePendingNotice pending PendingMcpNotice
            (UserMessage "connecting")
            `shouldReturn` Right ()
        enqueuePendingNotice pending PendingMcpNotice
            (UserMessage "settled")
            `shouldReturn` Right ()
        seen <- newIORef []
        let backend = withPendingInputs pending $ Backend
                \state _ inputs _ -> do
                    writeIORef seen inputs
                    pure $ Right BackendResult
                        { backendOutput = emptyTurnOutput "ok" [] Nothing
                        , backendState = state
                        }
        _ <- backend.submitTurn emptyBackendSnapshot Nothing [] (const (pure ()))
        readIORef seen `shouldReturn` [UserMessage "settled"]

    it "keeps the previous MCP snapshot when its replacement is rejected" do
        pending <- newPendingInputs
        enqueuePendingNotice pending PendingMcpNotice
            (UserMessage "settled")
            `shouldReturn` Right ()
        enqueuePendingNotice pending PendingMcpNotice
            (UserMessage (Text.replicate (pendingInputByteLimit + 1) "x"))
            `shouldReturn`
                Left
                    "Root input queue is full; one or more background notices were omitted."
        seen <- newIORef []
        let backend = withPendingInputs pending $ Backend
                \state _ inputs _ -> do
                    writeIORef seen inputs
                    pure $ Right BackendResult
                        { backendOutput = emptyTurnOutput "ok" [] Nothing
                        , backendState = state
                        }
        _ <- backend.submitTurn emptyBackendSnapshot Nothing [] (const (pure ()))
        readIORef seen `shouldReturn` [UserMessage "settled"]

    it "drops a stale drained MCP snapshot after an in-flight failure" do
        pending <- newPendingInputs
        enqueuePendingNotice pending PendingMcpNotice
            (UserMessage "connecting")
            `shouldReturn` Right ()
        entered <- newEmptyMVar
        release <- newEmptyMVar
        let failing = withPendingInputs pending $ Backend
                \_ _ _ _ -> do
                    putMVar entered ()
                    takeMVar release
                    pure (Left (ConnectionError "offline"))
        done <- newEmptyMVar
        _ <- forkIO $
            failing.submitTurn emptyBackendSnapshot Nothing [] (const (pure ())) >>= putMVar done
        takeMVar entered
        enqueuePendingNotice pending PendingMcpNotice
            (UserMessage "settled")
            `shouldReturn` Right ()
        putMVar release ()
        _ <- takeMVar done
        seen <- newIORef []
        let retry = withPendingInputs pending $ Backend
                \state _ inputs _ -> do
                    writeIORef seen inputs
                    pure $ Right BackendResult
                        { backendOutput = emptyTurnOutput "ok" [] Nothing
                        , backendState = state
                        }
        _ <- retry.submitTurn emptyBackendSnapshot Nothing [] (const (pure ()))
        readIORef seen `shouldReturn` [UserMessage "settled"]

    it "reports synthetic overflow once without exceeding the queue bound" do
        pending <- newPendingInputs
        mapM_
            (enqueuePendingInput pending . UserMessage . Text.pack . show)
            [1 .. pendingInputCountLimit]
        enqueuePendingNotice pending PendingSubagentNotice
            (UserMessage "omitted one")
            `shouldReturn`
                Left
                    "Root input queue is full; one or more background notices were omitted."
        enqueuePendingNotice pending PendingSubagentNotice
            (UserMessage "omitted two")
            `shouldReturn` Right ()
        seen <- newIORef []
        let backend = withPendingInputs pending $ Backend
                \state _ inputs _ -> do
                    writeIORef seen inputs
                    pure $ Right BackendResult
                        { backendOutput = emptyTurnOutput "ok" [] Nothing
                        , backendState = state
                        }
        _ <- backend.submitTurn emptyBackendSnapshot Nothing [] (const (pure ()))
        inputs <- readIORef seen
        length inputs `shouldBe` pendingInputCountLimit
        inputs `shouldSatisfy`
            all (`notElem` [UserMessage "omitted one", UserMessage "omitted two"])

  describe "SteeringInputs" do
    it "snapshots idle guidance as turn text without consuming or duplicating inputs" do
        steering <- newSteeringInputs
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
        steering <- newSteeringInputs
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
        steering <- newSteeringInputs
        enqueueSteeringInputs steering [] `shouldReturn` Right ()
        readSteeringInputs steering `shouldReturn` []
        hasSteeringInputWake steering `shouldReturn` False
        timeout 10000 (atomically (awaitSteeringInput steering))
            `shouldReturn` Nothing

    it "bounds, commits, and admits steering inputs in order" do
        steering <- newSteeringInputs
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

    it "wakes for ordinary input, retains it until commit, and does not hot-loop" do
        steering <- newSteeringInputs
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
        steering <- newSteeringInputs
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
        steering <- newSteeringInputs
        enqueueSteeringInputs steering [UserMessage "consumed"]
            `shouldReturn` Right ()
        commitSteeringInputs steering 1
        hasSteeringInputWake steering `shouldReturn` False
        timeout 10000 (atomically (awaitSteeringInput steering))
            `shouldReturn` Nothing

    it "wakes again for new guidance after a previous wake was consumed" do
        steering <- newSteeringInputs
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
        steering <- newSteeringInputs
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
        steering <- newSteeringInputs
        enqueueBackgroundCompletion steering "task-1" (UserMessage "completed")
            `shouldReturn` Right True
        suppressUserSteeringWake steering
        hasSteeringInputWake steering `shouldReturn` True

    it "deduplicates keyed background completions and dismisses them" do
        steering <- newSteeringInputs
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

queuedInputs :: Model.PendingState -> [TurnInput]
queuedInputs = Model.batchInputs . snd . Model.drain
