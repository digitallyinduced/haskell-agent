module Agent.Loop.EventDeliverySpec (spec) where

import Agent.Loop
import Agent.Loop.Fixtures
import Agent.ToolDispatch
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (wait, withAsync)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import qualified Control.Exception as Exception
import Data.IORef
import qualified Data.Text as Text
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = do
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
                    , backendState = emptyBackendSnapshot
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
                    , backendState = emptyBackendSnapshot
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
                    , backendState = emptyBackendSnapshot
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
                    , backendState = emptyBackendSnapshot
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
                    , backendState = emptyBackendSnapshot
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
                    , backendState = emptyBackendSnapshot
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
                    , backendState = emptyBackendSnapshot
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

    it "keeps only the latest adjacent tool-argument snapshot per call" do
        sinkStarted <- newEmptyMVar
        releaseSink <- newEmptyMVar
        backendFinished <- newEmptyMVar
        events <- newIORef []
        let preview callId arguments =
                ToolArgumentsUpdated
                    (functionToolCall callId "apply_patch" arguments)
            backend = Backend \_state _prev _inputs onEvent -> do
                onEvent (preview "c1" "a")
                onEvent (preview "c1" "ab")
                onEvent (preview "c2" "x")
                onEvent (preview "c2" "xy")
                onEvent (preview "c1" "abc")
                putMVar backendFinished ()
                pure $ Right BackendResult
                    { backendOutput =
                        emptyTurnOutput "resp-1" [] (Just "done")
                    , backendState = emptyBackendSnapshot
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
            , preview "c1" "ab"
            , preview "c2" "xy"
            , preview "c1" "abc"
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
                    , backendState = emptyBackendSnapshot
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
