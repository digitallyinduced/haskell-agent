module Agent.LoopSpec (spec) where

import Agent.Cancel (newCancelFlag, requestCancel)
import Agent.Error (ApiError(..))
import Agent.Loop
import Agent.Responses.Types (ResponseItem(..), TaggedObject(..))
import Agent.ToolArgs (objectArgs, reqText)
import Agent.ToolDispatch
import Agent.Tools.Types
    ( ApprovalRule(..)
    , ToolExecutionPolicy(..)
    , ToolRegistry
    , jsonAppToolWithExecution
    , mkToolRegistry
    , toolExecutionPolicyFor
    )
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (wait, withAsync)
import Control.Concurrent.MVar
    ( newEmptyMVar
    , putMVar
    , readMVar
    , takeMVar
    , tryReadMVar
    )
import qualified Control.Exception as Exception
import Data.Aeson (FromJSON(..))
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as Text
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "runLoop" do
    it "shows image metadata without exposing attachment bytes" do
        let image = ImageAttachment "image/png" "secret-image-bytes"
            rendered = show image
        rendered `shouldContain` "image/png"
        rendered `shouldContain` "imageByteLength = 18"
        rendered `shouldContain` "<redacted>"
        rendered `shouldNotContain` "secret-image-bytes"

    it "combines TokenUsage component-wise" do
        TokenUsage 10 4 6 <> TokenUsage 3 2 1
            `shouldBe` TokenUsage 13 6 7

    it "uses emptyTokenUsage as the TokenUsage monoidal identity" do
        let usage = TokenUsage 10 4 6
        (mempty <> usage, usage <> mempty)
            `shouldBe` (usage, usage)

    it "threads previous_response_id and sends only CompletedTool on the follow-up" do
        submissions <- newIORef []
        backend <- scriptedBackend submissions
            [ Right $ emptyTurnOutput "resp-1"
                [functionToolCall "c1" "echo" "{\"message\":\"hi\"}"]
                (Just "calling echo")
            , Right $ emptyTurnOutput "resp-2" [] (Just "done")
            ]
        config <- testConfig backend
        result <- runLoop config Nothing "hello"
        result `shouldBe` Right LoopResult
            { finalResponseId = "resp-2"
            , finalText = Just "done"
            , turnsUsed = 2
            , tokenUsage = emptyTokenUsage
            }
        seen <- readIORef submissions
        seen `shouldBe`
            [ (Nothing, [UserMessage "hello"])
            , (Just "resp-1", [CompletedTool (functionResult "c1" "echo:hi")])
            ]

    it "accepts multimodal first turns via runLoopInputs" do
        submissions <- newIORef []
        backend <- scriptedBackend submissions
            [ Right $ emptyTurnOutput "resp-m" [] (Just "saw it")
            ]
        let image = ImageAttachment "image/png" "abc"
            inputs =
                [ UserMultimodal
                    { userText = "see this"
                    , userImages = [image]
                    }
                ]
        config <- testConfig backend
        result <- runLoopInputs config Nothing inputs
        result `shouldBe` Right LoopResult
            { finalResponseId = "resp-m"
            , finalText = Just "saw it"
            , turnsUsed = 1
            , tokenUsage = emptyTokenUsage
            }
        seen <- readIORef submissions
        seen `shouldBe` [(Nothing, inputs)]

    it "serializes loopOnEvent across parallel tool calls" do
        inFlight <- newIORef (0 :: Int)
        maxInFlight <- newIORef (0 :: Int)
        submissions <- newIORef []
        backend <- scriptedBackend submissions
            [ Right $ emptyTurnOutput "resp-1"
                [ functionToolCall "c1" "a" "{}"
                , functionToolCall "c2" "b" "{}"
                ]
                Nothing
            , Right $ emptyTurnOutput "resp-2" [] (Just "ok")
            ]
        let onEvent _ = do
                now <- atomicModifyIORef' inFlight \n -> (n + 1, n + 1)
                atomicModifyIORef' maxInFlight \seen -> (max seen now, ())
                threadDelay 30000
                atomicModifyIORef' inFlight \n -> (n - 1, ())
            handlers =
                [ noArgsTool "a" (pure (Right "ok"))
                , noArgsTool "b" (pure (Right "ok"))
                ]
        config0 <- testConfig backend
        let config = config0
                { loopTools = registryFromHandlers handlers
                , loopOnEvent = onEvent
                }
        result <- runLoop config Nothing "go"
        result `shouldBe` Right LoopResult
            { finalResponseId = "resp-2"
            , finalText = Just "ok"
            , turnsUsed = 2
            , tokenUsage = emptyTokenUsage
            }
        readIORef maxInFlight `shouldReturn` 1

    it "delivers events off the backend thread and flushes before returning" do
        sinkStarted <- newEmptyMVar
        releaseSink <- newEmptyMVar
        backendEntered <- newEmptyMVar
        let backend = Backend \_state _prev _inputs _onEvent -> do
                putMVar backendEntered ()
                pure $ Right BackendResult
                    { backendOutput =
                        emptyTurnOutput "resp-1" [] (Just "done")
                    , backendState = []
                    }
            onEvent = \case
                TurnStarted -> do
                    putMVar sinkStarted ()
                    takeMVar releaseSink
                _ -> pure ()
        config0 <- testConfig backend
        let config = config0 { loopOnEvent = onEvent }
        withAsync (runLoop config Nothing "go") \running -> do
            takeMVar sinkStarted
            timeout 1000000 (takeMVar backendEntered)
                `shouldReturn` Just ()
            timeout 100000 (wait running)
                `shouldReturn` Nothing
            putMVar releaseSink ()
            wait running `shouldReturn` Right LoopResult
                { finalResponseId = "resp-1"
                , finalText = Just "done"
                , turnsUsed = 1
                , tokenUsage = emptyTokenUsage
                }

    it "bounds queued events when the sink falls behind" do
        sinkStarted <- newEmptyMVar
        releaseSink <- newEmptyMVar
        backendStarted <- newEmptyMVar
        backendFinished <- newEmptyMVar
        let backend = Backend \_state _prev _inputs onEvent -> do
                putMVar backendStarted ()
                mapM_ (const (onEvent (TextDelta "x"))) [1 .. 300 :: Int]
                putMVar backendFinished ()
                pure $ Right BackendResult
                    { backendOutput =
                        emptyTurnOutput "resp-1" [] (Just "done")
                    , backendState = []
                    }
            onEvent = \case
                TurnStarted -> do
                    putMVar sinkStarted ()
                    takeMVar releaseSink
                _ -> pure ()
        config0 <- testConfig backend
        let config = config0 { loopOnEvent = onEvent }
        withAsync (runLoop config Nothing "go") \running -> do
            takeMVar sinkStarted
            takeMVar backendStarted
            timeout 100000 (takeMVar backendFinished)
                `shouldReturn` Nothing
            putMVar releaseSink ()
            timeout 1000000 (takeMVar backendFinished)
                `shouldReturn` Just ()
            wait running `shouldReturn` Right LoopResult
                { finalResponseId = "resp-1"
                , finalText = Just "done"
                , turnsUsed = 1
                , tokenUsage = emptyTokenUsage
                }

    it "dispatches consecutive parallel-safe tool calls concurrently" do
        firstStarted <- newEmptyMVar
        secondStarted <- newEmptyMVar
        release <- newEmptyMVar
        let blocked started = do
                putMVar started ()
                readMVar release
                pure (Right "ok")
            handlers =
                [ noArgsTool "a" (blocked firstStarted)
                , noArgsTool "b" (blocked secondStarted)
                ]
        submissions <- newIORef []
        backend <- scriptedBackend submissions
            [ Right $ emptyTurnOutput "resp-1"
                [ functionToolCall "c1" "a" "{}"
                , functionToolCall "c2" "b" "{}"
                ]
                Nothing
            , Right $ emptyTurnOutput "resp-2" [] (Just "ok")
            ]
        config0 <- testConfig backend
        withAsync
            (runLoop
                config0 { loopTools = registryFromHandlers handlers }
                Nothing
                "go")
            \running -> do
                bothStarted <- timeout concurrencyProbeMicros do
                    takeMVar firstStarted
                    takeMVar secondStarted
                bothStarted `shouldBe` Just ()
                putMVar release ()
                wait running `shouldReturn` Right LoopResult
                    { finalResponseId = "resp-2"
                    , finalText = Just "ok"
                    , turnsUsed = 2
                    , tokenUsage = emptyTokenUsage
                    }

    it "preserves order between consecutive turn-sequential calls" do
        firstStarted <- newEmptyMVar
        secondStarted <- newEmptyMVar
        releaseFirst <- newEmptyMVar
        let first = do
                putMVar firstStarted ()
                takeMVar releaseFirst
                pure (Right "first")
            second = putMVar secondStarted () >> pure (Right "second")
            tools =
                [ (TurnSequential, noArgsTool "first" first)
                , (TurnSequential, noArgsTool "second" second)
                ]
        submissions <- newIORef []
        backend <- scriptedBackend submissions
            [ Right $ emptyTurnOutput "resp-1"
                [ functionToolCall "c1" "first" "{}"
                , functionToolCall "c2" "second" "{}"
                ]
                Nothing
            , Right $ emptyTurnOutput "resp-2" [] (Just "ok")
            ]
        config0 <- testConfig backend
        withAsync
            (runLoop
                config0 { loopTools = registryFromPolicies tools }
                Nothing
                "go")
            \running -> do
                timeout concurrencyProbeMicros (takeMVar firstStarted)
                    `shouldReturn` Just ()
                tryReadMVar secondStarted `shouldReturn` Nothing
                putMVar releaseFirst ()
                timeout concurrencyProbeMicros (takeMVar secondStarted)
                    `shouldReturn` Just ()
                wait running `shouldReturn` Right LoopResult
                    { finalResponseId = "resp-2"
                    , finalText = Just "ok"
                    , turnsUsed = 2
                    , tokenUsage = emptyTokenUsage
                    }

    it "evaluates approvals serially before parallel-safe handlers" do
        firstApprovalStarted <- newEmptyMVar
        secondApprovalStarted <- newEmptyMVar
        releaseFirstApproval <- newEmptyMVar
        submissions <- newIORef []
        backend <- scriptedBackend submissions
            [ Right $ emptyTurnOutput "resp-1"
                [ functionToolCall "c1" "a" "{}"
                , functionToolCall "c2" "b" "{}"
                ]
                Nothing
            , Right $ emptyTurnOutput "resp-2" [] (Just "ok")
            ]
        config0 <- testConfig backend
        let approve :: ToolCall -> IO (Either Text Bool)
            approve call
                | call.name == "a" = do
                    putMVar firstApprovalStarted ()
                    takeMVar releaseFirstApproval
                    pure (Right True)
                | otherwise =
                    putMVar secondApprovalStarted () >> pure (Right True)
            handlers =
                [ noArgsTool "a" (pure (Right "a"))
                , noArgsTool "b" (pure (Right "b"))
                ]
            config = config0
                { loopTools = registryFromHandlers handlers
                , loopApprove = approve
                }
        withAsync (runLoop config Nothing "go") \running -> do
            timeout concurrencyProbeMicros (takeMVar firstApprovalStarted)
                `shouldReturn` Just ()
            tryReadMVar secondApprovalStarted `shouldReturn` Nothing
            putMVar releaseFirstApproval ()
            timeout concurrencyProbeMicros (takeMVar secondApprovalStarted)
                `shouldReturn` Just ()
            wait running `shouldReturn` Right LoopResult
                { finalResponseId = "resp-2"
                , finalText = Just "ok"
                , turnsUsed = 2
                , tokenUsage = emptyTokenUsage
                }

    it "keeps sequential calls as barriers around parallel-safe batches" do
        firstSafeStarted <- newEmptyMVar
        secondSafeStarted <- newEmptyMVar
        sequentialStarted <- newEmptyMVar
        finalSafeStarted <- newEmptyMVar
        releaseSafe <- newEmptyMVar
        releaseSequential <- newEmptyMVar
        let blockedSafe started = do
                putMVar started ()
                readMVar releaseSafe
                pure (Right "safe")
            sequential = do
                putMVar sequentialStarted ()
                takeMVar releaseSequential
                pure (Right "sequential")
            finalSafe = putMVar finalSafeStarted () >> pure (Right "final")
            tools =
                [ (ParallelSafe, noArgsTool "safe-a" (blockedSafe firstSafeStarted))
                , (ParallelSafe, noArgsTool "safe-b" (blockedSafe secondSafeStarted))
                , (TurnSequential, noArgsTool "sequential" sequential)
                , (ParallelSafe, noArgsTool "safe-c" finalSafe)
                ]
        submissions <- newIORef []
        backend <- scriptedBackend submissions
            [ Right $ emptyTurnOutput "resp-1"
                [ functionToolCall "c1" "safe-a" "{}"
                , functionToolCall "c2" "safe-b" "{}"
                , functionToolCall "c3" "sequential" "{}"
                , functionToolCall "c4" "safe-c" "{}"
                ]
                Nothing
            , Right $ emptyTurnOutput "resp-2" [] (Just "ok")
            ]
        config0 <- testConfig backend
        withAsync
            (runLoop
                config0 { loopTools = registryFromPolicies tools }
                Nothing
                "go")
            \running -> do
                safeBatchStarted <- timeout concurrencyProbeMicros do
                    takeMVar firstSafeStarted
                    takeMVar secondSafeStarted
                safeBatchStarted `shouldBe` Just ()
                tryReadMVar sequentialStarted `shouldReturn` Nothing
                tryReadMVar finalSafeStarted `shouldReturn` Nothing

                putMVar releaseSafe ()
                timeout concurrencyProbeMicros (takeMVar sequentialStarted)
                    `shouldReturn` Just ()
                tryReadMVar finalSafeStarted `shouldReturn` Nothing

                putMVar releaseSequential ()
                timeout concurrencyProbeMicros (takeMVar finalSafeStarted)
                    `shouldReturn` Just ()
                wait running `shouldReturn` Right LoopResult
                    { finalResponseId = "resp-2"
                    , finalText = Just "ok"
                    , turnsUsed = 2
                    , tokenUsage = emptyTokenUsage
                    }

    it "treats unknown tools as sequential" do
        toolExecutionPolicyFor
            (registryFromHandlers [noArgsTool "known" (pure (Right "ok"))])
            (functionToolCall "c1" "unknown" "{}")
            `shouldBe` TurnSequential

    it "returns a denial as tool output when approval is refused" do
        submissions <- newIORef []
        backend <- scriptedBackend submissions
            [ Right $ emptyTurnOutput "resp-1"
                [functionToolCall "c1" "echo" "{\"message\":\"nope\"}"]
                Nothing
            , Right $ emptyTurnOutput "resp-2" [] (Just "understood")
            ]
        config0 <- testConfig backend
        let config = config0 { loopApprove = \_ -> pure (Right False) }
        result <- runLoop config Nothing "please"
        result `shouldBe` Right LoopResult
            { finalResponseId = "resp-2"
            , finalText = Just "understood"
            , turnsUsed = 2
            , tokenUsage = emptyTokenUsage
            }
        seen <- readIORef submissions
        case seen of
            [_, (Just "resp-1", [CompletedTool denied])] ->
                denied.output `shouldBe` "Tool call rejected by user."
            other -> expectationFailure ("unexpected submissions: " <> show other)

    it "returns LoopMaxTurns when the model keeps calling tools" do
        backend <- endlessToolsBackend
        config0 <- testConfig backend
        let config = config0 { loopMaxTurns = 1 }
        result <- runLoop config Nothing "loop forever"
        case result of
            Left (LoopMaxTurns turn) -> do
                turn.responseId `shouldBe` "resp-1"
                turn.toolCalls `shouldNotBe` []
            other -> expectationFailure ("expected LoopMaxTurns, got " <> show other)

    it "keeps looping after a handler exception" do
        submissions <- newIORef []
        backend <- scriptedBackend submissions
            [ Right $ emptyTurnOutput "resp-1"
                [functionToolCall "c1" "explode" "{}"]
                Nothing
            , Right $ emptyTurnOutput "resp-2" [] (Just "survived")
            ]
        let handlers = [noArgsTool "explode" (error "boom")]
        config0 <- testConfig backend
        result <- runLoop config0 { loopTools = registryFromHandlers handlers } Nothing "go"
        result `shouldBe` Right LoopResult
            { finalResponseId = "resp-2"
            , finalText = Just "survived"
            , turnsUsed = 2
            , tokenUsage = emptyTokenUsage
            }
        seen <- readIORef submissions
        case seen of
            [_, (_, [CompletedTool crashed])] ->
                crashed.output `shouldSatisfy` Text.isInfixOf "crashed"
            other -> expectationFailure ("unexpected submissions: " <> show other)

    it "surfaces a transport Left as LoopTransport" do
        submissions <- newIORef []
        backend <- scriptedBackend submissions
            [Left (ConnectionError "down")]
        config <- testConfig backend
        result <- runLoop config Nothing "hello"
        result `shouldBe` Left (LoopTransport (ConnectionError "down"))

    it "returns explicit backend state and progress after success" do
        submissions <- newIORef []
        backend <- scriptedBackend submissions
            [Right (emptyTurnOutput "resp-1" [] (Just "done"))]
        config <- testConfig backend
        execution <- runLoopInputsDetailed config Nothing [UserMessage "hello"]
        length execution.executionState `shouldBe` 1
        execution.executionProgress `shouldBe` ResponseCommitted
        execution.executionResult `shouldBe` Right LoopResult
            { finalResponseId = "resp-1"
            , finalText = Just "done"
            , turnsUsed = 1
            , tokenUsage = emptyTokenUsage
            }

    it "returns the last committed state after a later transport failure" do
        submissions <- newIORef []
        backend <- scriptedBackend submissions
            [ Right $ emptyTurnOutput "resp-1"
                [functionToolCall "c1" "echo" "{\"message\":\"hi\"}"]
                Nothing
            , Left (ConnectionError "down")
            ]
        config <- testConfig backend
        execution <- runLoopInputsDetailed config Nothing [UserMessage "hello"]
        length execution.executionState `shouldBe` 1
        execution.executionProgress `shouldBe` ResponseCommitted
        execution.executionResult
            `shouldBe` Left (LoopTransport (ConnectionError "down"))

    it "does not commit backend state for a transport failure" do
        submissions <- newIORef []
        backend <- scriptedBackend submissions [Left (ConnectionError "down")]
        config <- testConfig backend
        execution <- runLoopInputsDetailed config Nothing [UserMessage "hello"]
        execution.executionState `shouldBe` []
        execution.executionProgress `shouldBe` NoResponseCommitted
        execution.executionResult
            `shouldBe` Left (LoopTransport (ConnectionError "down"))

    it "retains committed state when a later callback throws" do
        submissions <- newIORef []
        backend <- scriptedBackend submissions
            [Right (emptyTurnOutput "resp-1" [] (Just "done"))]
        config0 <- testConfig backend
        let config = config0
                { loopOnEvent = \case
                    TurnFinished _ ->
                        Exception.throwIO (userError "renderer exploded")
                    _ -> pure ()
                }
        execution <- runLoopInputsDetailed config Nothing [UserMessage "hello"]
        length execution.executionState `shouldBe` 1
        execution.executionProgress `shouldBe` ResponseCommitted
        execution.executionResult
            `shouldBe` Left (LoopUnexpected "user error (renderer exploded)")

    it "marks a transport failure after streamed output as interrupted" do
        let backend = Backend \_state _prev _inputs onEvent -> do
                onEvent (TextDelta "partial")
                pure (Left (ConnectionError "down"))
        config <- testConfig backend
        result <- runLoop config Nothing "hello"
        result `shouldBe`
            Left (LoopTransportAfterOutput (ConnectionError "down"))

    it "turns synchronous backend exceptions into a failed turn" do
        config <- testConfig $ Backend \_state _prev _inputs _onEvent ->
            Exception.throwIO (userError "backend exploded")
        result <- runLoop config Nothing "hello"
        result `shouldBe`
            Left (LoopUnexpected "user error (backend exploded)")

    it "turns synchronous approval exceptions into a failed turn" do
        submissions <- newIORef []
        backend <- scriptedBackend submissions
            [ Right $ emptyTurnOutput "resp-1"
                [functionToolCall "c1" "echo" "{\"message\":\"hi\"}"]
                Nothing
            ]
        config0 <- testConfig backend
        let config = config0
                { loopApprove = \_ ->
                    Exception.throwIO (userError "approval exploded")
                }
        result <- runLoop config Nothing "hello"
        result `shouldBe`
            Left (LoopUnexpected "user error (approval exploded)")

    it "does not turn asynchronous backend cancellation into a failed turn" do
        config <- testConfig $ Backend \_state _prev _inputs _onEvent ->
            Exception.throwIO Exception.ThreadKilled
        runLoop config Nothing "hello"
            `shouldThrow` (== Exception.ThreadKilled)

    it "does not detach asynchronous event-sink cancellation" do
        submissions <- newIORef []
        backend <- scriptedBackend submissions
            [Right (emptyTurnOutput "resp-1" [] (Just "done"))]
        config0 <- testConfig backend
        let config = config0
                { loopOnEvent = \_ ->
                    Exception.throwIO Exception.ThreadKilled
                }
        timeout 1000000
            (runLoop config Nothing "hello"
                `shouldThrow` (== Exception.ThreadKilled))
            `shouldReturn` Just ()

    it "emits TurnStarted and TurnFinished around each backend submit" do
        events <- newIORef []
        submissions <- newIORef []
        backend <- scriptedBackend submissions
            [ Right $ emptyTurnOutput "resp-1" [] (Just "hi")
            ]
        config0 <- testConfig backend
        let config = config0
                { loopOnEvent = \event -> modifyIORef' events (event :)
                }
        _ <- runLoop config Nothing "hello"
        seen <- reverse <$> readIORef events
        seen `shouldBe`
            [ TurnStarted
            , TurnFinished (emptyTurnOutput "resp-1" [] (Just "hi"))
            ]

    it "emits ToolStarted and ToolFinished around each dispatched call" do
        events <- newIORef []
        submissions <- newIORef []
        backend <- scriptedBackend submissions
            [ Right $ emptyTurnOutput "resp-1"
                [functionToolCall "c1" "echo" "{\"message\":\"hi\"}"]
                Nothing
            , Right $ emptyTurnOutput "resp-2" [] (Just "done")
            ]
        config0 <- testConfig backend
        let config = config0
                { loopOnEvent = \event -> modifyIORef' events (event :)
                }
        _ <- runLoop config Nothing "hello"
        seen <- reverse <$> readIORef events
        seen `shouldBe`
            [ TurnStarted
            , TurnFinished $ emptyTurnOutput "resp-1"
                [functionToolCall "c1" "echo" "{\"message\":\"hi\"}"]
                Nothing
            , ToolStarted (functionToolCall "c1" "echo" "{\"message\":\"hi\"}")
            , ToolFinished (functionResult "c1" "echo:hi")
            , TurnStarted
            , TurnFinished (emptyTurnOutput "resp-2" [] (Just "done"))
            ]

    it "emits correlated tool output snapshots before completion" do
        events <- newIORef []
        submissions <- newIORef []
        backend <- scriptedBackend submissions
            [ Right $ emptyTurnOutput "resp-1"
                [functionToolCall "c1" "stream" "{\"message\":\"hi\"}"]
                Nothing
            , Right $ emptyTurnOutput "resp-2" [] (Just "done")
            ]
        config0 <- testConfig backend
        let config = config0
                { loopTools = registryFromHandlers
                    [ typedStreamingTool "stream" \emit EchoArgs{message} -> do
                        emit ("partial:" <> message)
                        pure (Right ("complete:" <> message))
                    ]
                , loopOnEvent = \event -> modifyIORef' events (event :)
                }
        _ <- runLoop config Nothing "hello"
        seen <- reverse <$> readIORef events
        seen `shouldContain`
            [ ToolStarted
                (functionToolCall "c1" "stream" "{\"message\":\"hi\"}")
            , ToolOutputUpdated "c1" "partial:hi"
            , ToolFinished (functionResult "c1" "complete:hi")
            ]


    it "returns LoopCancelled when the cancel flag is set during tools" do
        submissions <- newIORef []
        backend <- scriptedBackend submissions
            [ Right $ emptyTurnOutput "resp-1"
                [functionToolCall "c1" "slow" "{}"]
                Nothing
            ]
        config0 <- testConfig backend
        let cancel = case config0 of
                LoopConfig{loopCancel = c} -> c
            handlers =
                [ noArgsTool "slow" do
                    requestCancel cancel
                    threadDelay 10000
                    pure (Right "should-not-continue")
                ]
            config = config0 { loopTools = registryFromHandlers handlers }
        result <- runLoop config Nothing "go"
        case result of
            Left (LoopCancelled results) ->
                results `shouldNotBe` []
            other -> expectationFailure ("expected LoopCancelled, got " <> show other)

    it "does not render a rejected tool when approval cancels the turn" do
        events <- newIORef []
        submissions <- newIORef []
        backend <- scriptedBackend submissions
            [ Right $ emptyTurnOutput "resp-1"
                [functionToolCall "c1" "echo" "{\"message\":\"hi\"}"]
                Nothing
            ]
        config0 <- testConfig backend
        let cancel = case config0 of
                LoopConfig{loopCancel = c} -> c
            config = config0
                { loopApprove = \_ -> do
                    requestCancel cancel
                    pure (Right False)
                , loopOnEvent = \event -> modifyIORef' events (event :)
                }
        result <- runLoop config Nothing "go"
        result `shouldBe` Left (LoopCancelled [])
        seen <- reverse <$> readIORef events
        seen `shouldBe`
            [ TurnStarted
            , TurnFinished $ emptyTurnOutput "resp-1"
                [functionToolCall "c1" "echo" "{\"message\":\"hi\"}"]
                Nothing
            ]

    it "stops preparing a parallel batch after approval cancels" do
        approvals <- newIORef ([] :: [Text])
        submissions <- newIORef []
        backend <- scriptedBackend submissions
            [ Right $ emptyTurnOutput "resp-1"
                [ functionToolCall "c1" "echo" "{\"message\":\"one\"}"
                , functionToolCall "c2" "echo" "{\"message\":\"two\"}"
                ]
                Nothing
            ]
        config0 <- testConfig backend
        let cancel = case config0 of
                LoopConfig{loopCancel = c} -> c
            config = config0
                { loopApprove = \call -> do
                    modifyIORef' approvals (<> [call.callId])
                    requestCancel cancel
                    pure (Right False)
                }
        result <- runLoop config Nothing "go"
        result `shouldBe` Left (LoopCancelled [])
        readIORef approvals `shouldReturn` ["c1"]

    it "returns LoopCancelled when cancel arrives during submitTurn" do
        started <- newEmptyMVar
        config0 <- testConfig $ Backend \state _prev _inputs _onEvent -> do
            putMVar started ()
            threadDelay 2000000
            pure $ Right BackendResult
                { backendOutput =
                    emptyTurnOutput "resp-slow" [] (Just "too late")
                , backendState = state <> [stateMarker]
                }
        let cancel = case config0 of
                LoopConfig{loopCancel = c} -> c
        _ <- forkIO do
            takeMVar started
            requestCancel cancel
        result <- runLoop config0 Nothing "go"
        result `shouldBe` Left (LoopCancelled [])

    it "does not clear a cancel requested before the loop starts" do
        submissions <- newIORef []
        backend <- scriptedBackend submissions
            [Right (emptyTurnOutput "resp-too-late" [] (Just "too late"))]
        config <- testConfig backend
        requestCancel config.loopCancel
        result <- runLoop config Nothing "go"
        result `shouldBe` Left (LoopCancelled [])
        readIORef submissions `shouldReturn` []

    it "sums token usage across model steps in one user turn" do
        submissions <- newIORef []
        backend <- scriptedBackend submissions
            [ Right TurnOutput
                { responseId = "resp-1"
                , toolCalls = [functionToolCall "c1" "echo" "{\"message\":\"hi\"}"]
                , assistantText = Just "calling"
                , tokenUsage = TokenUsage 10 4 2
                }
            , Right TurnOutput
                { responseId = "resp-2"
                , toolCalls = []
                , assistantText = Just "done"
                , tokenUsage = TokenUsage 12 6 0
                }
            ]
        config <- testConfig backend
        result <- runLoop config Nothing "hello"
        result `shouldBe` Right LoopResult
            { finalResponseId = "resp-2"
            , finalText = Just "done"
            , turnsUsed = 2
            , tokenUsage = TokenUsage 22 10 2
            }

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

testConfig :: Backend -> IO LoopConfig
testConfig backend = do
    cancel <- newCancelFlag
    state <- newIORef []
    pure LoopConfig
        { loopBackend = backend
        , loopBackendState = BackendStateStore
            { readBackendState = readIORef state
            , commitBackendState = writeIORef state
            }
        , loopTools = registryFromHandlers
            [ typedTool "echo" $ \EchoArgs { message } ->
                pure (Right ("echo:" <> message))
            ]
        , loopDispatch = defaultLoopDispatch
        , loopMaxTurns = defaultLoopMaxTurns
        , loopOnEvent = \_ -> pure ()
        , loopApprove = \_ -> pure (Right True)
        , loopCancel = cancel
        }

registryFromHandlers :: [ToolHandler] -> ToolRegistry
registryFromHandlers =
    registryFromPolicies . map (\handler -> (ParallelSafe, handler))

registryFromPolicies :: [(ToolExecutionPolicy, ToolHandler)] -> ToolRegistry
registryFromPolicies tools =
    either (error . Text.unpack) id $ mkToolRegistry
        [ jsonAppToolWithExecution
            (handlerName handler)
            ""
            []
            AlwaysReadOnly
            execution
            handler
        | (execution, handler) <- tools
        ]

concurrencyProbeMicros :: Int
concurrencyProbeMicros = 5000000

data EchoArgs = EchoArgs { message :: Text }

instance FromJSON EchoArgs where
    parseJSON = objectArgs $ \object -> EchoArgs <$> reqText object "message"

functionResult :: Text -> Text -> ToolCallResult
functionResult callId output = ToolCallResult
    { callId
    , output
    , callKind = FunctionCallKind
    }

scriptedBackend
    :: IORef [(Maybe Text, [TurnInput])]
    -> [Either ApiError TurnOutput]
    -> IO Backend
scriptedBackend submissions answers = do
    remaining <- newIORef answers
    pure $ Backend \state prev inputs _onEvent -> do
        modifyIORef' submissions (++ [(prev, inputs)])
        atomicModifyIORef' remaining \case
            [] -> ([], Left (ConnectionError "scripted backend exhausted"))
            next : rest ->
                ( rest
                , fmap
                    (\output -> BackendResult
                        { backendOutput = output
                        , backendState = state <> [stateMarker]
                        })
                    next
                )

endlessToolsBackend :: IO Backend
endlessToolsBackend = do
    counter <- newIORef (0 :: Int)
    pure $ Backend \state _prev _inputs _onEvent -> do
        n <- atomicModifyIORef' counter \i -> (i + 1, i + 1)
        let responseId = "resp-" <> Text.pack (show n)
        pure $ Right BackendResult
            { backendOutput = emptyTurnOutput responseId
                [functionToolCall "c1" "echo" "{\"message\":\"again\"}"]
                Nothing
            , backendState = state <> [stateMarker]
            }

stateMarker :: ResponseItem
stateMarker = UnknownResponseItem TaggedObject
    { tag = "test_state"
    , fields = mempty
    }
