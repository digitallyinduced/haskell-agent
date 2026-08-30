module Agent.LoopSpec (spec) where

import qualified Agent.Json.Decode as Json
import Agent.Cancel (newCancelFlag, requestCancel)
import Agent.Error (ApiError(..))
import Agent.Loop
import Agent.Responses.Types (ResponseItem(..), TaggedObject(..))
import Agent.ToolArgs (objectArgs, reqText)
import Agent.ToolDispatch
import Agent.Tools.Scheduling
    ( ToolAccess(..)
    , ToolResource(..)
    , ToolResourceClaim(..)
    , ToolSchedulingPlan(..)
    , schedulingPlansConflict
    )
import Agent.Tools.Types
    ( AppTool
    , ApprovalRule(..)
    , ToolExecutionPolicy(..)
    , ToolRegistry
    , jsonAppToolWithExecution
    , mkToolRegistry
    , toolExecutionPolicyFor
    , withToolResourceClaims
    )
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (cancel, wait, withAsync)
import Control.Concurrent.MVar
    ( newEmptyMVar
    , putMVar
    , readMVar
    , takeMVar
    , tryReadMVar
    )
import qualified Control.Exception as Exception
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as Text
import System.Timeout (timeout)
import System.OsPath (unsafeEncodeUtf)
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

    it "shows file metadata without exposing file bytes" do
        let file = FileAttachment (Just "report.pdf") "application/pdf" "secret-file-bytes"
            rendered = show file
        rendered `shouldContain` "report.pdf"
        rendered `shouldContain` "application/pdf"
        rendered `shouldContain` "fileByteLength = 17"
        rendered `shouldContain` "<redacted>"
        rendered `shouldNotContain` "secret-file-bytes"

    it "normalizes an empty attachment list to a text-only message" do
        userMessageWithAttachments "hello" []
            `shouldBe` UserMessage "hello"

    it "combines TokenUsage component-wise" do
        TokenUsage 10 4 6 <> TokenUsage 3 2 1
            `shouldBe` TokenUsage 13 6 7

    it "uses emptyTokenUsage as the TokenUsage monoidal identity" do
        let usage = TokenUsage 10 4 6
        (mempty <> usage, usage <> mempty)
            `shouldBe` (usage, usage)

    it "estimates tokens from streamed characters and reports tokens/sec" do
        estimateTokensFromChars 0 `shouldBe` 0
        estimateTokensFromChars 1 `shouldBe` 1
        estimateTokensFromChars 4 `shouldBe` 1
        estimateTokensFromChars 16 `shouldBe` 4
        tokensPerSecond 0 1000 `shouldBe` Nothing
        tokensPerSecond 100 0 `shouldBe` Nothing
        tokensPerSecond 100 1000 `shouldBe` Just 100
        tokensPerSecond 40 2000 `shouldBe` Just 20
        generationTokensPerSecond 80 1000 `shouldBe` Just 80
        generationTokensPerSecond 0 1000 `shouldBe` Nothing
        liveTokensPerSecond 16 (liveTokenRateMinMillis - 1) `shouldBe` Nothing
        liveTokensPerSecond 16 liveTokenRateMinMillis
            `shouldBe` tokensPerSecond 4 liveTokenRateMinMillis

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
                [ userMessageWithAttachments
                    "see this"
                    [ImageAttachmentItem image]
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

    it "accepts file attachments in multimodal turns" do
        submissions <- newIORef []
        backend <- scriptedBackend submissions
            [ Right $ emptyTurnOutput "resp-f" [] (Just "saw files")
            ]
        let image = ImageAttachment "image/png" "abc"
            file = FileAttachment (Just "notes.txt") "text/plain" "file-bytes"
            inputs =
                [ userMessageWithAttachments
                    "see this"
                    [ ImageAttachmentItem image
                    , FileAttachmentItem file
                    ]
                ]
        config <- testConfig backend
        result <- runLoopInputs config Nothing inputs
        result `shouldBe` Right LoopResult
            { finalResponseId = "resp-f"
            , finalText = Just "saw files"
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
                mapM_ (const (onEvent (WarningRaised "x"))) [1 .. 300 :: Int]
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
            timeout 3000000 (takeMVar backendFinished)
                `shouldReturn` Just ()
            wait running `shouldReturn` Right LoopResult
                { finalResponseId = "resp-1"
                , finalText = Just "done"
                , turnsUsed = 1
                , tokenUsage = emptyTokenUsage
                }

    it "wakes a producer blocked on a full queue when the sink fails" do
        sinkStarted <- newEmptyMVar
        releaseSink <- newEmptyMVar
        backendStarted <- newEmptyMVar
        backendFinished <- newEmptyMVar
        let backend = Backend \_state _prev _inputs onEvent -> do
                putMVar backendStarted ()
                mapM_ (const (onEvent (WarningRaised "queued")))
                    [1 .. 300 :: Int]
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
                    Exception.throwIO (userError "renderer exploded")
                _ -> pure ()
        config0 <- testConfig backend
        let config = config0 { loopOnEvent = onEvent }
        withAsync (runLoop config Nothing "go") \running -> do
            takeMVar sinkStarted
            takeMVar backendStarted
            timeout 100000 (takeMVar backendFinished)
                `shouldReturn` Nothing
            putMVar releaseSink ()
            timeout 1000000 (wait running)
                `shouldReturn`
                    Just
                        (Left
                            (LoopUnexpected
                                "user error (renderer exploded)"))

    it "coalesces adjacent deltas while preserving event boundaries" do
        sinkStarted <- newEmptyMVar
        releaseSink <- newEmptyMVar
        backendFinished <- newEmptyMVar
        events <- newIORef []
        let backend = Backend \_state _prev _inputs onEvent -> do
                onEvent (TextDelta "a")
                onEvent (TextDelta "b")
                onEvent (WarningRaised "boundary")
                onEvent (ReasoningDelta "r1")
                onEvent (ReasoningDelta "r2")
                onEvent (TextDelta "c")
                onEvent (TextDelta "d")
                putMVar backendFinished ()
                pure $ Right BackendResult
                    { backendOutput =
                        emptyTurnOutput "resp-1" [] (Just "done")
                    , backendState = []
                    }
            onEvent event = do
                modifyIORef' events (event :)
                case event of
                    TurnStarted -> do
                        putMVar sinkStarted ()
                        takeMVar releaseSink
                    _ -> pure ()
        config0 <- testConfig backend
        let config = config0 { loopOnEvent = onEvent }
        withAsync (runLoop config Nothing "go") \running -> do
            takeMVar sinkStarted
            takeMVar backendFinished
            putMVar releaseSink ()
            wait running `shouldReturn` Right LoopResult
                { finalResponseId = "resp-1"
                , finalText = Just "done"
                , turnsUsed = 1
                , tokenUsage = emptyTokenUsage
                }
        reverse <$> readIORef events `shouldReturn`
            [ TurnStarted
            , TextDelta "ab"
            , WarningRaised "boundary"
            , ReasoningDelta "r1r2"
            , TextDelta "cd"
            , TurnFinished (emptyTurnOutput "resp-1" [] (Just "done"))
            ]

    it "backpressures a coalesced text tail by logical payload bytes" do
        sinkStarted <- newEmptyMVar
        releaseSink <- newEmptyMVar
        backendFinished <- newEmptyMVar
        deliveredChars <- newIORef (0 :: Int)
        let chunk = Text.replicate (1024 * 1024) "x"
            backend = Backend \_state _prev _inputs onEvent -> do
                -- The chunks cross the conservative 8 MiB logical-byte
                -- budget while TurnStarted blocks the consumer, even though
                -- they would otherwise occupy only one TBQueue node.
                mapM_ (onEvent . TextDelta) [chunk, chunk, chunk]
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
                TextDelta text ->
                    modifyIORef' deliveredChars (+ Text.length text)
                _ -> pure ()
        config0 <- testConfig backend
        let config = config0 { loopOnEvent = onEvent }
        withAsync (runLoop config Nothing "go") \running -> do
            takeMVar sinkStarted
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
        readIORef deliveredChars
            `shouldReturn` 3 * Text.length chunk

    it "backpressures and coalesces provider-native child output" do
        sinkStarted <- newEmptyMVar
        releaseSink <- newEmptyMVar
        backendFinished <- newEmptyMVar
        deliveredChars <- newIORef (0 :: Int)
        let chunk = Text.replicate (1024 * 1024) "x"
            backend = Backend \_state _prev _inputs onEvent -> do
                mapM_
                    (onEvent . NativeAgentOutput "child")
                    [chunk, chunk, chunk]
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
                NativeAgentOutput "child" output ->
                    modifyIORef' deliveredChars (+ Text.length output)
                _ -> pure ()
        config0 <- testConfig backend
        let config = config0 { loopOnEvent = onEvent }
        withAsync (runLoop config Nothing "go") \running -> do
            takeMVar sinkStarted
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
        readIORef deliveredChars
            `shouldReturn` 3 * Text.length chunk

    it "keeps only the latest adjacent tool-output snapshot per call" do
        sinkStarted <- newEmptyMVar
        releaseSink <- newEmptyMVar
        backendFinished <- newEmptyMVar
        events <- newIORef []
        let backend = Backend \_state _prev _inputs onEvent -> do
                onEvent (ToolOutputUpdated "c1" "a")
                onEvent (ToolOutputUpdated "c1" "ab")
                onEvent (ToolOutputUpdated "c2" "x")
                onEvent (ToolOutputUpdated "c2" "xy")
                onEvent (ToolOutputUpdated "c1" "abc")
                putMVar backendFinished ()
                pure $ Right BackendResult
                    { backendOutput =
                        emptyTurnOutput "resp-1" [] (Just "done")
                    , backendState = []
                    }
            onEvent event = do
                modifyIORef' events (event :)
                case event of
                    TurnStarted -> do
                        putMVar sinkStarted ()
                        takeMVar releaseSink
                    _ -> pure ()
        config0 <- testConfig backend
        let config = config0 { loopOnEvent = onEvent }
        withAsync (runLoop config Nothing "go") \running -> do
            takeMVar sinkStarted
            takeMVar backendFinished
            putMVar releaseSink ()
            wait running `shouldReturn` Right LoopResult
                { finalResponseId = "resp-1"
                , finalText = Just "done"
                , turnsUsed = 1
                , tokenUsage = emptyTokenUsage
                }
        reverse <$> readIORef events `shouldReturn`
            [ TurnStarted
            , ToolOutputUpdated "c1" "ab"
            , ToolOutputUpdated "c2" "xy"
            , ToolOutputUpdated "c1" "abc"
            , TurnFinished (emptyTurnOutput "resp-1" [] (Just "done"))
            ]

    it "bounds a single oversized live tool-output snapshot" do
        delivered <- newIORef Nothing
        let oversized =
                Text.replicate (3 * 1024 * 1024) "x" <> "newest-tail"
            backend = Backend \_state _prev _inputs onEvent -> do
                onEvent (ToolOutputUpdated "large" oversized)
                pure $ Right BackendResult
                    { backendOutput =
                        emptyTurnOutput "resp-1" [] (Just "done")
                    , backendState = []
                    }
            onEvent = \case
                ToolOutputUpdated "large" output ->
                    writeIORef delivered (Just output)
                _ -> pure ()
        config0 <- testConfig backend
        result <- runLoop config0 { loopOnEvent = onEvent } Nothing "go"
        result `shouldBe` Right LoopResult
            { finalResponseId = "resp-1"
            , finalText = Just "done"
            , turnsUsed = 1
            , tokenUsage = emptyTokenUsage
            }
        readIORef delivered >>= \case
            Nothing -> expectationFailure "missing tool-output update"
            Just output -> do
                Text.length output `shouldSatisfy` (<= 2 * 1024 * 1024)
                output `shouldSatisfy`
                    Text.isPrefixOf "[earlier tool output truncated]"
                output `shouldSatisfy` Text.isSuffixOf "newest-tail"

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

    it "runs disjoint resource writes concurrently" do
        firstStarted <- newEmptyMVar
        secondStarted <- newEmptyMVar
        release <- newEmptyMVar
        let blocked started = do
                putMVar started ()
                readMVar release
                pure (Right "ok")
            tools =
                [ resourceTool "first" "file:a" (blocked firstStarted)
                , resourceTool "second" "file:b" (blocked secondStarted)
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
                config0 { loopTools = registryFromTools tools }
                Nothing
                "go")
            \running -> do
                (timeout concurrencyProbeMicros do
                        takeMVar firstStarted
                        takeMVar secondStarted)
                    `shouldReturn` Just ()
                putMVar release ()
                result <- wait running
                result `shouldSatisfy` either (const False) (const True)

    it "returns concurrent tool results in model order" do
        firstStarted <- newEmptyMVar
        secondFinished <- newEmptyMVar
        releaseFirst <- newEmptyMVar
        let first = do
                putMVar firstStarted ()
                takeMVar releaseFirst
                pure (Right "first")
            second = do
                putMVar secondFinished ()
                pure (Right "second")
            tools =
                [ resourceTool "first" "file:a" first
                , resourceTool "second" "file:b" second
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
                config0 { loopTools = registryFromTools tools }
                Nothing
                "go")
            \running -> do
                timeout concurrencyProbeMicros (takeMVar firstStarted)
                    `shouldReturn` Just ()
                timeout concurrencyProbeMicros (takeMVar secondFinished)
                    `shouldReturn` Just ()
                putMVar releaseFirst ()
                result <- wait running
                result `shouldSatisfy` either (const False) (const True)
        seen <- readIORef submissions
        seen `shouldBe`
            [ (Nothing, [UserMessage "go"])
            , (Just "resp-1",
                [ CompletedTool (functionResult "c1" "first")
                , CompletedTool (functionResult "c2" "second")
                ])
            ]

    it "serializes conflicting resource writes" do
        firstStarted <- newEmptyMVar
        secondStarted <- newEmptyMVar
        releaseFirst <- newEmptyMVar
        let first = do
                putMVar firstStarted ()
                takeMVar releaseFirst
                pure (Right "first")
            second = putMVar secondStarted () >> pure (Right "second")
            tools =
                [ resourceTool "first" "file:a" first
                , resourceTool "second" "file:a" second
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
                config0 { loopTools = registryFromTools tools }
                Nothing
                "go")
            \running -> do
                timeout concurrencyProbeMicros (takeMVar firstStarted)
                    `shouldReturn` Just ()
                tryReadMVar secondStarted `shouldReturn` Nothing
                putMVar releaseFirst ()
                timeout concurrencyProbeMicros (takeMVar secondStarted)
                    `shouldReturn` Just ()
                result <- wait running
                result `shouldSatisfy` either (const False) (const True)

    it "lets an independent later call bypass a blocked conflicting call" do
        firstStarted <- newEmptyMVar
        conflictingStarted <- newEmptyMVar
        independentStarted <- newEmptyMVar
        releaseFirst <- newEmptyMVar
        let first = do
                putMVar firstStarted ()
                takeMVar releaseFirst
                pure (Right "first")
            conflicting =
                putMVar conflictingStarted () >> pure (Right "conflicting")
            independent =
                putMVar independentStarted () >> pure (Right "independent")
            tools =
                [ resourceTool "first" "file:a" first
                , resourceTool "conflicting" "file:a" conflicting
                , resourceTool "independent" "file:b" independent
                ]
        submissions <- newIORef []
        backend <- scriptedBackend submissions
            [ Right $ emptyTurnOutput "resp-1"
                [ functionToolCall "c1" "first" "{}"
                , functionToolCall "c2" "conflicting" "{}"
                , functionToolCall "c3" "independent" "{}"
                ]
                Nothing
            , Right $ emptyTurnOutput "resp-2" [] (Just "ok")
            ]
        config0 <- testConfig backend
        withAsync
            (runLoop
                config0 { loopTools = registryFromTools tools }
                Nothing
                "go")
            \running -> do
                timeout concurrencyProbeMicros (takeMVar firstStarted)
                    `shouldReturn` Just ()
                timeout concurrencyProbeMicros (takeMVar independentStarted)
                    `shouldReturn` Just ()
                tryReadMVar conflictingStarted `shouldReturn` Nothing
                putMVar releaseFirst ()
                timeout concurrencyProbeMicros (takeMVar conflictingStarted)
                    `shouldReturn` Just ()
                result <- wait running
                result `shouldSatisfy` either (const False) (const True)

    it "evaluates dynamic-call approvals serially in model order" do
        approvalOrder <- newIORef []
        releaseFirst <- newEmptyMVar
        firstStarted <- newEmptyMVar
        let first = do
                putMVar firstStarted ()
                takeMVar releaseFirst
                pure (Right "first")
            tools =
                [ resourceTool "first" "file:a" first
                , resourceTool "conflicting" "file:a" (pure (Right "second"))
                , resourceTool "independent" "file:b" (pure (Right "third"))
                ]
        submissions <- newIORef []
        backend <- scriptedBackend submissions
            [ Right $ emptyTurnOutput "resp-1"
                [ functionToolCall "c1" "first" "{}"
                , functionToolCall "c2" "conflicting" "{}"
                , functionToolCall "c3" "independent" "{}"
                ]
                Nothing
            , Right $ emptyTurnOutput "resp-2" [] (Just "ok")
            ]
        config0 <- testConfig backend
        let config = config0
                { loopTools = registryFromTools tools
                , loopApprove = \call -> do
                    modifyIORef' approvalOrder (<> [call.name])
                    pure (Right True)
                }
        withAsync (runLoop config Nothing "go") \running -> do
            timeout concurrencyProbeMicros (takeMVar firstStarted)
                `shouldReturn` Just ()
            readIORef approvalOrder
                `shouldReturn` ["first", "conflicting", "independent"]
            putMVar releaseFirst ()
            result <- wait running
            result `shouldSatisfy` either (const False) (const True)

    it "does not resolve resources for a rejected tool call" do
        resolverCalls <- newIORef (0 :: Int)
        submissions <- newIORef []
        backend <- scriptedBackend submissions
            [ Right $ emptyTurnOutput "resp-1"
                [functionToolCall "c1" "guarded" "{}"]
                Nothing
            , Right $ emptyTurnOutput "resp-2" [] (Just "ok")
            ]
        config0 <- testConfig backend
        let tool =
                withToolResourceClaims
                    (\_ -> do
                        modifyIORef' resolverCalls (+ 1)
                        pure (Right []))
                    (jsonAppToolWithExecution
                        "guarded"
                        ""
                        []
                        AlwaysReadOnly
                        TurnSequential
                        (noArgsTool "guarded" (pure (Right "unexpected"))))
            config = config0
                { loopTools = registryFromTools [tool]
                , loopApprove = \_ -> pure (Right False)
                }
        result <- runLoop config Nothing "go"
        result `shouldSatisfy` either (const False) (const True)
        readIORef resolverCalls `shouldReturn` 0

    it "detects overlapping filesystem resource claims" do
        let root = unsafeEncodeUtf "/workspace/src"
            file = unsafeEncodeUtf "/workspace/src/Main.hs"
            other = unsafeEncodeUtf "/workspace/test/Spec.hs"
            readTree =
                ToolResourceClaims
                    [ToolResourceClaim ToolRead (ToolPathTree root)]
            writeFile path =
                ToolResourceClaims
                    [ToolResourceClaim ToolWrite (ToolPath path)]
        schedulingPlansConflict readTree (writeFile file) `shouldBe` True
        schedulingPlansConflict readTree (writeFile other) `shouldBe` False
        schedulingPlansConflict
            (ToolResourceClaims
                [ToolResourceClaim ToolRead ToolAllPaths])
            (writeFile other)
            `shouldBe` True

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

    it "defaults to a 2000-turn budget" do
        defaultLoopMaxTurns `shouldBe` 2000

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

    it "keeps looping after a handler returns a validation error" do
        submissions <- newIORef []
        backend <- scriptedBackend submissions
            [ Right $ emptyTurnOutput "resp-1"
                [functionToolCall "c1" "shell_command"
                    "{\"timeout_ms\":1000,\"yield_time_ms\":1000}"]
                Nothing
            , Right $ emptyTurnOutput "resp-2" [] (Just "retried")
            ]
        let handlers =
                [ noArgsTool "shell_command" $
                    pure (Left
                        "timeout_ms and yield_time_ms are mutually exclusive")
                ]
        config0 <- testConfig backend
        result <- runLoop
            config0 { loopTools = registryFromHandlers handlers }
            Nothing
            "go"
        result `shouldBe` Right LoopResult
            { finalResponseId = "resp-2"
            , finalText = Just "retried"
            , turnsUsed = 2
            , tokenUsage = emptyTokenUsage
            }
        seen <- readIORef submissions
        case seen of
            [_, (Just "resp-1", [CompletedTool failed])] ->
                failed.output `shouldBe`
                    "Error: timeout_ms and yield_time_ms are mutually exclusive"
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

    it "exposes tool results awaiting submission after a later transport failure" do
        submissions <- newIORef []
        backend <- scriptedBackend submissions
            [ Right $ emptyTurnOutput "resp-1"
                [functionToolCall "c1" "echo" "{\"message\":\"hi\"}"]
                Nothing
            , Left (ConnectionError "down")
            ]
        config <- testConfig backend
        execution <- runLoopInputsDetailed config Nothing [UserMessage "hello"]
        execution.executionPendingInputs `shouldBe`
            [CompletedTool (ToolCallResult "c1" "echo:hi" FunctionCallKind)]

    it "exposes the initial inputs while nothing has committed" do
        submissions <- newIORef []
        backend <- scriptedBackend submissions [Left (ConnectionError "down")]
        config <- testConfig backend
        execution <- runLoopInputsDetailed config Nothing [UserMessage "hello"]
        execution.executionPendingInputs `shouldBe` [UserMessage "hello"]

    it "leaves nothing pending once a response commits without tool calls" do
        submissions <- newIORef []
        backend <- scriptedBackend submissions
            [Right (emptyTurnOutput "resp-1" [] (Just "done"))]
        config <- testConfig backend
        execution <- runLoopInputsDetailed config Nothing [UserMessage "hello"]
        execution.executionPendingInputs `shouldBe` []

    it "leaves nothing pending after an incomplete response" do
        submissions <- newIORef []
        backend <- scriptedBackend submissions
            [ Right (emptyTurnOutput "resp-1"
                [functionToolCall "c1" "echo" "{\"message\":\"hi\"}"]
                Nothing)
                { completion = TurnIncomplete
                    { incompleteReason = "max_output_tokens"
                    , incompleteReasoningTokens = Nothing
                    }
                }
            ]
        config <- testConfig backend
        execution <- runLoopInputsDetailed config Nothing [UserMessage "hello"]
        execution.executionProgress `shouldBe` ResponseCommitted
        execution.executionPendingInputs `shouldBe` []
        case execution.executionResult of
            Left (LoopIncomplete turn) -> turn.responseId `shouldBe` "resp-1"
            other -> expectationFailure ("expected LoopIncomplete, got " <> show other)

    it "keeps completed tool results pending when cancelled during the next model step" do
        started <- newEmptyMVar
        calls <- newIORef (0 :: Int)
        config0 <- testConfig $ Backend \state _prev _inputs _onEvent -> do
            call <- atomicModifyIORef' calls \n -> (n + 1, n + 1)
            if call == 1
                then pure $ Right BackendResult
                    { backendOutput = emptyTurnOutput "resp-1"
                        [functionToolCall "c1" "echo" "{\"message\":\"hi\"}"]
                        Nothing
                    , backendState = state <> [stateMarker]
                    }
                else do
                    putMVar started ()
                    threadDelay maxBound
                    error "cancel should stop the backend"
        let cancelFlag = config0.loopCancel
        _ <- forkIO do
            takeMVar started
            requestCancel cancelFlag
        execution <- runLoopInputsDetailed config0 Nothing [UserMessage "go"]
        execution.executionResult `shouldBe` Left (LoopCancelled [])
        execution.executionProgress `shouldBe` ResponseCommitted
        execution.executionPendingInputs `shouldBe`
            [CompletedTool (ToolCallResult "c1" "echo:hi" FunctionCallKind)]

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

    it "retains committed state when the event pump fails during commit" do
        commitStarted <- newEmptyMVar
        sinkCanFail <- newEmptyMVar
        state <- newIORef []
        let committedState = [stateMarker]
            backend = Backend \_state _prev _inputs _onEvent ->
                pure $ Right BackendResult
                    { backendOutput =
                        emptyTurnOutput "resp-1" [] (Just "done")
                    , backendState = committedState
                    }
        config0 <- testConfig backend
        let config = config0
                { loopBackendState = BackendStateStore
                    { readBackendState = readIORef state
                    , commitBackendState = \newState -> do
                        putMVar commitStarted ()
                        Exception.uninterruptibleMask_ do
                            takeMVar sinkCanFail
                            -- Give the outer race time to cancel the loop
                            -- before the masked commit returns.
                            threadDelay 30000
                            writeIORef state newState
                    }
                , loopOnEvent = \case
                    TurnStarted -> do
                        takeMVar commitStarted
                        putMVar sinkCanFail ()
                        Exception.throwIO (userError "renderer exploded")
                    _ -> pure ()
                }
        execution <- runLoopInputsDetailed config Nothing [UserMessage "hello"]
        readIORef state `shouldReturn` committedState
        execution.executionState `shouldBe` committedState
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

    it "treats a transport failure after a discarded attempt as pre-output" do
        let backend = Backend \_state _prev _inputs onEvent -> do
                onEvent (TextDelta "partial")
                onEvent ResponseAttemptDiscarded
                pure (Left (ConnectionError "down"))
        config <- testConfig backend
        result <- runLoop config Nothing "hello"
        result `shouldBe` Left (LoopTransport (ConnectionError "down"))

    it "turns synchronous backend exceptions into a failed turn" do
        config <- testConfig $ Backend \_state _prev _inputs _onEvent ->
            Exception.throwIO (userError "backend exploded")
        result <- runLoop config Nothing "hello"
        result `shouldBe`
            Left (LoopUnexpected "user error (backend exploded)")

    it "keeps looping after a synchronous approval exception" do
        submissions <- newIORef []
        backend <- scriptedBackend submissions
            [ Right $ emptyTurnOutput "resp-1"
                [functionToolCall "c1" "echo" "{\"message\":\"hi\"}"]
                Nothing
            , Right $ emptyTurnOutput "resp-2" [] (Just "recovered")
            ]
        config0 <- testConfig backend
        let config = config0
                { loopApprove = \_ ->
                    Exception.throwIO (userError "approval exploded")
                }
        result <- runLoop config Nothing "hello"
        result `shouldBe` Right LoopResult
            { finalResponseId = "resp-2"
            , finalText = Just "recovered"
            , turnsUsed = 2
            , tokenUsage = emptyTokenUsage
            }
        seen <- readIORef submissions
        case seen of
            [_, (Just "resp-1", [CompletedTool failed])] ->
                failed.output `shouldBe`
                    "Tool echo could not be prepared: user error (approval exploded)"
            other -> expectationFailure ("unexpected submissions: " <> show other)

    it "does not turn asynchronous approval cancellation into tool output" do
        submissions <- newIORef []
        backend <- scriptedBackend submissions
            [ Right $ emptyTurnOutput "resp-1"
                [functionToolCall "c1" "echo" "{\"message\":\"hi\"}"]
                Nothing
            ]
        config0 <- testConfig backend
        let config = config0
                { loopApprove = \_ ->
                    Exception.throwIO Exception.ThreadKilled
                }
        runLoop config Nothing "hello"
            `shouldThrow` (== Exception.ThreadKilled)

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

    it "can be cancelled while the event sink is running" do
        sinkStarted <- newEmptyMVar
        submissions <- newIORef []
        backend <- scriptedBackend submissions
            [Right (emptyTurnOutput "resp-1" [] (Just "done"))]
        config0 <- testConfig backend
        let config = config0
                { loopOnEvent = \_ -> do
                    putMVar sinkStarted ()
                    threadDelay maxBound
                }
        withAsync (runLoop config Nothing "hello") \running -> do
            takeMVar sinkStarted
            timeout 1000000 (cancel running) `shouldReturn` Just ()

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
                    [ typedStreamingTool "stream" echoArgsDecoder \emit EchoArgs{message} -> do
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


    it "cancels an in-flight tool when the cancel flag is set" do
        started <- newEmptyMVar
        stopped <- newEmptyMVar
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
                    Exception.finally
                        (putMVar started ()
                            >> threadDelay maxBound
                            >> pure (Right "should-not-continue"))
                        (putMVar stopped ())
                ]
            config = config0 { loopTools = registryFromHandlers handlers }
        withAsync (runLoop config Nothing "go") \running -> do
            takeMVar started
            requestCancel cancel
            timeout 1000000 (wait running)
                `shouldReturn` Just (Left (LoopCancelled []))
            tryReadMVar stopped `shouldReturn` Just ()

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

    it "retains assistant text streamed before submitTurn is cancelled" do
        started <- newEmptyMVar
        config0 <- testConfig $ Backend \_state _prev _inputs onEvent -> do
            onEvent (TextDelta "visible ")
            onEvent (TextDelta "partial")
            putMVar started ()
            threadDelay 2000000
            error "cancel should stop the backend"
        let cancelFlag = config0.loopCancel
        _ <- forkIO do
            takeMVar started
            requestCancel cancelFlag
        execution <- runLoopInputsDetailed config0 Nothing [UserMessage "go"]
        execution.executionResult `shouldBe` Left (LoopCancelled [])
        execution.executionUncommittedAssistantText
            `shouldBe` Just "visible partial"

    it "reports only assistant text streamed since the last committed response" do
        started <- newEmptyMVar
        calls <- newIORef (0 :: Int)
        config0 <- testConfig $ Backend \state _prev _inputs onEvent -> do
            call <- atomicModifyIORef' calls \n -> (n + 1, n + 1)
            if call == 1
                then do
                    onEvent (TextDelta "committed text")
                    pure $ Right BackendResult
                        { backendOutput = emptyTurnOutput "resp-1"
                            [functionToolCall "c1" "echo" "{\"message\":\"hi\"}"]
                            (Just "committed text")
                        , backendState = state <> [stateMarker]
                        }
                else do
                    onEvent (TextDelta "dropped attempt")
                    onEvent (ResponseRestarted "reconnecting")
                    onEvent (TextDelta "partial ")
                    onEvent (TextDelta "answer")
                    putMVar started ()
                    threadDelay maxBound
                    error "cancel should stop the backend"
        let cancelFlag = config0.loopCancel
        _ <- forkIO do
            takeMVar started
            requestCancel cancelFlag
        execution <- runLoopInputsDetailed config0 Nothing [UserMessage "go"]
        execution.executionResult `shouldBe` Left (LoopCancelled [])
        execution.executionProgress `shouldBe` ResponseCommitted
        execution.executionUncommittedAssistantText
            `shouldBe` Just "dropped attempt\n\npartial answer"

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
                , completion = TurnCompleted
                }
            , Right TurnOutput
                { responseId = "resp-2"
                , toolCalls = []
                , assistantText = Just "done"
                , tokenUsage = TokenUsage 12 6 0
                , completion = TurnCompleted
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

    it "continues after a reasoning-only completion until the model answers" do
        events <- newIORef []
        submissions <- newIORef []
        backend <- scriptedBackend submissions
            [ Right $ emptyTurnOutput "resp-1" [] Nothing
            , Right $ emptyTurnOutput "resp-2" [] (Just "done")
            ]
        config0 <- testConfig backend
        let config = config0
                { loopOnEvent = \event -> modifyIORef' events (event :)
                }
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
            , (Just "resp-1", [])
            ]
        reverse <$> readIORef events `shouldReturn`
            [ TurnStarted
            , TurnFinished (emptyTurnOutput "resp-1" [] Nothing)
            , TurnStarted
            , TurnFinished (emptyTurnOutput "resp-2" [] (Just "done"))
            ]

    it "continues after whitespace-only assistant text" do
        submissions <- newIORef []
        backend <- scriptedBackend submissions
            [ Right $ emptyTurnOutput "resp-1" [] (Just "  \n")
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
            , (Just "resp-1", [])
            ]

    it "can call tools after a reasoning-only continuation" do
        submissions <- newIORef []
        backend <- scriptedBackend submissions
            [ Right $ emptyTurnOutput "resp-1" [] Nothing
            , Right $ emptyTurnOutput "resp-2"
                [functionToolCall "c1" "echo" "{\"message\":\"hi\"}"]
                Nothing
            , Right $ emptyTurnOutput "resp-3" [] (Just "done")
            ]
        config <- testConfig backend
        result <- runLoop config Nothing "hello"
        result `shouldBe` Right LoopResult
            { finalResponseId = "resp-3"
            , finalText = Just "done"
            , turnsUsed = 3
            , tokenUsage = emptyTokenUsage
            }
        seen <- readIORef submissions
        case seen of
            [ (Nothing, [UserMessage "hello"])
                , (Just "resp-1", [])
                , (Just "resp-2", [CompletedTool echoed])
                ] ->
                    echoed.output `shouldBe` "echo:hi"
            other -> expectationFailure ("unexpected submissions: " <> show other)

    it "resets the empty-completion allowance after a tool continuation" do
        submissions <- newIORef []
        backend <- scriptedBackend submissions
            [ Right $ emptyTurnOutput "resp-1" [] Nothing
            , Right $ emptyTurnOutput "resp-2" [] Nothing
            , Right $ emptyTurnOutput "resp-3"
                [functionToolCall "c1" "echo" "{\"message\":\"hi\"}"]
                Nothing
            , Right $ emptyTurnOutput "resp-4" [] Nothing
            , Right $ emptyTurnOutput "resp-5" [] Nothing
            , Right $ emptyTurnOutput "resp-6" [] (Just "done")
            ]
        config <- testConfig backend
        result <- runLoop config Nothing "hello"
        result `shouldBe` Right LoopResult
            { finalResponseId = "resp-6"
            , finalText = Just "done"
            , turnsUsed = 6
            , tokenUsage = emptyTokenUsage
            }
        length <$> readIORef submissions `shouldReturn` 6

    it "stops after repeated empty completions and warns" do
        events <- newIORef []
        submissions <- newIORef []
        backend <- scriptedBackend submissions
            [ Right $ emptyTurnOutput "resp-1" [] Nothing
            , Right $ emptyTurnOutput "resp-2" [] Nothing
            , Right $ emptyTurnOutput "resp-3" [] Nothing
            ]
        config0 <- testConfig backend
        let config = config0
                { loopOnEvent = \event -> modifyIORef' events (event :)
                }
        result <- runLoop config Nothing "hello"
        result `shouldBe` Right LoopResult
            { finalResponseId = "resp-3"
            , finalText = Nothing
            , turnsUsed = 3
            , tokenUsage = emptyTokenUsage
            }
        length <$> readIORef submissions `shouldReturn` 3
        reverse <$> readIORef events `shouldReturn`
            [ TurnStarted
            , TurnFinished (emptyTurnOutput "resp-1" [] Nothing)
            , TurnStarted
            , TurnFinished (emptyTurnOutput "resp-2" [] Nothing)
            , TurnStarted
            , TurnFinished (emptyTurnOutput "resp-3" [] Nothing)
            , WarningRaised
                "The model produced no assistant text or tool calls after reasoning; stopping."
            ]

    it "injects pending steering at the next committed model boundary" do
        submissions <- newIORef []
        pending <- newIORef []
        baseBackend <- scriptedBackend submissions
            [ Right (emptyTurnOutput "resp-1" [] (Just "initial answer"))
            , Right (emptyTurnOutput "resp-2" [] (Just "revised answer"))
            ]
        calls <- newIORef (0 :: Int)
        let backend = Backend \state previous inputs onEvent -> do
                call <- atomicModifyIORef' calls \count ->
                    (count + 1, count)
                if call == 0
                    then writeIORef pending ["use the existing schema"]
                    else pure ()
                baseBackend.submitTurn state previous inputs onEvent
        config0 <- testConfig backend
        let config = config0
                { loopReadSteering =
                    map UserMessage <$> readIORef pending
                , loopCommitSteering = \count ->
                    atomicModifyIORef' pending \messages ->
                        (drop count messages, ())
                }
        result <- runLoop config Nothing "start"
        result `shouldBe` Right LoopResult
            { finalResponseId = "resp-2"
            , finalText = Just "revised answer"
            , turnsUsed = 2
            , tokenUsage = emptyTokenUsage
            }
        readIORef submissions `shouldReturn`
            [ (Nothing, [UserMessage "start"])
            , (Just "resp-1", [UserMessage "use the existing schema"])
            ]
        readIORef pending `shouldReturn` []

    it "commits terminal incomplete responses without running their tools" do
        submissions <- newIORef []
        calls <- newIORef ([] :: [Text])
        pending <- newIORef ["keep this guidance"]
        backend <- scriptedBackend submissions
            [ Right TurnOutput
                { responseId = "resp-incomplete"
                , toolCalls =
                    [functionToolCall "c1" "echo" "{\"message\":\"unsafe\"}"]
                , assistantText = Just "partial"
                , tokenUsage = TokenUsage 120 32768 0
                , completion = TurnIncomplete
                    { incompleteReason = "max_output_tokens"
                    , incompleteReasoningTokens = Just 32000
                    }
                }
            ]
        config0 <- testConfig backend
        let config = config0
                { loopOnEvent = \case
                    ToolStarted call -> modifyIORef' calls (<> [call.callId])
                    _ -> pure ()
                , loopReadSteering = map UserMessage <$> readIORef pending
                , loopCommitSteering = \count ->
                    atomicModifyIORef' pending \messages ->
                        (drop count messages, ())
                }
        execution <- runLoopInputsDetailed config Nothing [UserMessage "hello"]
        execution.executionProgress `shouldBe` ResponseCommitted
        execution.executionResult `shouldBe`
            Left
                (LoopIncomplete TurnOutput
                    { responseId = "resp-incomplete"
                    , toolCalls =
                        [functionToolCall
                            "c1" "echo" "{\"message\":\"unsafe\"}"]
                    , assistantText = Just "partial"
                    , tokenUsage = TokenUsage 120 32768 0
                    , completion = TurnIncomplete
                        { incompleteReason = "max_output_tokens"
                        , incompleteReasoningTokens = Just 32000
                        }
                    })
        readIORef calls `shouldReturn` []
        readIORef submissions `shouldReturn`
            [ (Nothing
              , [ UserMessage "hello"
                , UserMessage "keep this guidance"
                ])
            ]
        readIORef pending `shouldReturn` ["keep this guidance"]

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
            [ typedTool "echo" echoArgsDecoder $ \EchoArgs { message } ->
                pure (Right ("echo:" <> message))
            ]
        , loopDispatch = defaultLoopDispatch
        , loopMaxTurns = defaultLoopMaxTurns
        , loopOnEvent = \_ -> pure ()
        , loopApprove = \_ -> pure (Right True)
        , loopReadSteering = pure []
        , loopCommitSteering = \_ -> pure ()
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

registryFromTools :: [AppTool] -> ToolRegistry
registryFromTools =
    either (error . Text.unpack) id . mkToolRegistry

resourceTool :: Text -> Text -> IO (Either Text Text) -> AppTool
resourceTool name resource action =
    withToolResourceClaims
        (\_ ->
            pure $ Right
                [ ToolResourceClaim ToolWrite
                    (ToolNamedResource resource)
                ])
        (jsonAppToolWithExecution
            name
            ""
            []
            AlwaysReadOnly
            TurnSequential
            (noArgsTool name action))

concurrencyProbeMicros :: Int
concurrencyProbeMicros = 5000000

data EchoArgs = EchoArgs { message :: Text }

echoArgsDecoder :: Json.Decoder EchoArgs
echoArgsDecoder = objectArgs $ \object -> EchoArgs <$> reqText object "message"

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
    }
