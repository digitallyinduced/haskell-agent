module Agent.CLI.ConnectivitySpec (spec) where

import Agent.CLI.Connectivity
    ( RecoveryWatcher(..)
    , reconnectDelayMicros
    , transientRetryDelayMicros
    , withConnectionRecovery
    , withConnectionRecoveryUsing
    , withConnectionRecoveryUsingWatcher
    )
import Agent.CLI.PendingInputs
    ( enqueuePendingInput
    , newPendingInputs
    , withPendingInputs
    )
import Agent.Error (ApiError(..), ErrorType(..))
import Agent.ToolDispatch (functionToolCall)
import Agent.Loop
    ( Backend(..)
    , BackendResult(..)
    , LoopEvent(..)
    , TurnInput(..)
    , emptyBackendSnapshot
    , emptyTurnOutput
    )
import Control.Concurrent.Async (cancel, wait, withAsync)
import Control.Concurrent.STM
    ( TVar
    , atomically
    , check
    , modifyTVar'
    , newEmptyTMVarIO
    , newTVarIO
    , putTMVar
    , readTVar
    , readTVarIO
    , retry
    , takeTMVar
    )
import Control.Exception.Safe (finally)
import Data.IORef
import Data.Word (Word64)
import Test.Hspec

spec :: Spec
spec = describe "withConnectionRecovery" do
    it "restarts a stale in-flight submission when the network recovers" do
        generation <- newTVarIO (0 :: Word64)
        started <- newEmptyTMVarIO
        attempts <- newIORef (0 :: Int)
        waits <- newIORef []
        events <- newIORef []
        let watcher = recoveryWatcher generation
            backend =
                withConnectionRecoveryUsingWatcher
                    (\delay -> modifyIORef' waits (<> [delay]))
                    watcher
                    (Backend \state _ _ onEvent -> do
                        attempt <- atomicModifyIORef' attempts
                            \n -> (n + 1, n + 1)
                        if attempt == 1
                            then do
                                onEvent (TextDelta "partial")
                                atomically (putTMVar started ())
                                atomically retry
                            else
                                pure $
                                    Right BackendResult
                                        { backendOutput =
                                            emptyTurnOutput
                                                "response" [] (Just "done")
                                        , backendState = state
                                        })

        withAsync
            (backend.submitTurn emptyBackendSnapshot Nothing []
                (\event -> modifyIORef' events (<> [event]))) \running -> do
            atomically (takeTMVar started)
            atomically (modifyTVar' generation (+ 1))
            wait running `shouldReturn`
                Right BackendResult
                    { backendOutput =
                        emptyTurnOutput "response" [] (Just "done")
                    , backendState = emptyBackendSnapshot
                    }

        readIORef attempts `shouldReturn` 2
        readIORef waits `shouldReturn` []
        readIORef events `shouldReturn`
            [ TextDelta "partial"
            , ActivityUpdated
                "Internet connection restored; reconnecting…"
            , ResponseRestarted
                "Connection interrupted the response; restarting automatically. The new attempt may repeat partial output shown above."
            ]

    it "wakes a connection-error backoff as soon as the network recovers" do
        generation <- newTVarIO (0 :: Word64)
        waiting <- newEmptyTMVarIO
        attempts <- newIORef (0 :: Int)
        events <- newIORef []
        let watcher = recoveryWatcher generation
            waitForTimer _ = do
                atomically (putTMVar waiting ())
                atomically retry
            backend =
                withConnectionRecoveryUsingWatcher waitForTimer watcher $
                    Backend \state _ _ _ -> do
                        attempt <- atomicModifyIORef' attempts
                            \n -> (n + 1, n + 1)
                        pure $
                            if attempt == 1
                                then Left (ConnectionError "offline")
                                else Right BackendResult
                                    { backendOutput =
                                        emptyTurnOutput
                                            "response" [] (Just "done")
                                    , backendState = state
                                    }

        withAsync
            (backend.submitTurn emptyBackendSnapshot Nothing []
                (\event -> modifyIORef' events (<> [event]))) \running -> do
            atomically (takeTMVar waiting)
            atomically (modifyTVar' generation (+ 1))
            wait running `shouldReturn`
                Right BackendResult
                    { backendOutput =
                        emptyTurnOutput "response" [] (Just "done")
                    , backendState = emptyBackendSnapshot
                    }

        readIORef attempts `shouldReturn` 2
        readIORef events `shouldReturn`
            [ ActivityUpdated
                "Connection lost; waiting for internet. Retrying automatically in 1s (Esc or Ctrl-C to cancel)…"
            , ActivityUpdated
                "Internet connection restored; reconnecting…"
            ]

    it "does not treat the watcher's initial generation as a recovery" do
        generation <- newTVarIO (7 :: Word64)
        attempts <- newIORef (0 :: Int)
        let backend =
                withConnectionRecoveryUsingWatcher
                    (const (pure ()))
                    (recoveryWatcher generation)
                    (Backend \state _ _ _ -> do
                        modifyIORef' attempts (+ 1)
                        pure $
                            Right BackendResult
                                { backendOutput =
                                    emptyTurnOutput
                                        "response" [] (Just "done")
                                , backendState = state
                                })
        result <- backend.submitTurn emptyBackendSnapshot Nothing []
            (const (pure ()))
        result `shouldBe`
            Right BackendResult
                { backendOutput =
                    emptyTurnOutput "response" [] (Just "done")
                , backendState = emptyBackendSnapshot
                }
        readIORef attempts `shouldReturn` 1

    it "cancels and joins the recovery wait with its owner" do
        generation <- newTVarIO (0 :: Word64)
        backendStarted <- newEmptyTMVarIO
        watcherStopped <- newEmptyTMVarIO
        let baseWatcher = recoveryWatcher generation
            watcher = RecoveryWatcher do
                waitForRecovery <-
                    baseWatcher.armRecoveryWatcher
                pure $
                    waitForRecovery
                        `finally` atomically (putTMVar watcherStopped ())
            backend =
                withConnectionRecoveryUsingWatcher
                    (const (pure ()))
                    watcher
                    (Backend \_ _ _ _ -> do
                        atomically (putTMVar backendStarted ())
                        atomically retry)

        withAsync
            (backend.submitTurn emptyBackendSnapshot Nothing []
                (const (pure ()))) \running -> do
            atomically (takeTMVar backendStarted)
            cancel running
            atomically (takeTMVar watcherStopped)

    it "retains polling-only recovery when no path monitor is supplied" do
        let backend =
                withConnectionRecovery $
                    Backend \state _ _ _ ->
                        pure $
                            Right BackendResult
                                { backendOutput =
                                    emptyTurnOutput
                                        "response" [] (Just "done")
                                , backendState = state
                                }
        result <- backend.submitTurn emptyBackendSnapshot Nothing []
            (const (pure ()))
        result `shouldBe`
            Right BackendResult
                { backendOutput =
                    emptyTurnOutput "response" [] (Just "done")
                , backendState = emptyBackendSnapshot
                }

    it "retries the same submission until connectivity returns" do
        attempts <- newIORef (0 :: Int)
        waits <- newIORef []
        seen <- newIORef []
        events <- newIORef []
        let backend = withConnectionRecoveryUsing
                (\delay -> modifyIORef' waits (<> [delay]))
                (Backend \state previous inputs _ -> do
                    modifyIORef' seen (<> [(previous, inputs)])
                    attempt <- atomicModifyIORef' attempts \n -> (n + 1, n + 1)
                    pure $
                        if attempt < 3
                            then Left (ConnectionError "offline")
                            else Right BackendResult
                                { backendOutput =
                                    emptyTurnOutput
                                        "response" [] (Just "done")
                                , backendState = state
                                })
            inputs = [UserMessage "continue"]
        result <- backend.submitTurn emptyBackendSnapshot (Just "previous") inputs
            (\event -> modifyIORef' events (<> [event]))

        result `shouldBe`
            Right BackendResult
                { backendOutput =
                    emptyTurnOutput "response" [] (Just "done")
                , backendState = emptyBackendSnapshot
                }
        readIORef waits `shouldReturn` [1_000_000, 2_000_000]
        readIORef seen `shouldReturn`
            [ (Just "previous", inputs)
            , (Just "previous", inputs)
            , (Just "previous", inputs)
            ]
        readIORef events `shouldReturn`
            [ ActivityUpdated
                "Connection lost; waiting for internet. Retrying automatically in 1s (Esc or Ctrl-C to cancel)…"
            , ActivityUpdated "Checking internet connection…"
            , ActivityUpdated
                "Connection lost; waiting for internet. Retrying automatically in 2s (Esc or Ctrl-C to cancel)…"
            , ActivityUpdated "Checking internet connection…"
            ]

    it "does not retry non-transient provider errors" do
        waits <- newIORef []
        let expected =
                ProviderError InvalidRequestError "bad request" Nothing
            backend = withConnectionRecoveryUsing
                (\delay -> modifyIORef' waits (<> [delay]))
                (Backend \_ _ _ _ -> pure (Left expected))
        result <- backend.submitTurn emptyBackendSnapshot Nothing [] (const (pure ()))
        result `shouldBe` Left expected
        readIORef waits `shouldReturn` []

    it "recovers from a WebSocket frame parse disconnect" do
        attempts <- newIORef (0 :: Int)
        let backend = withConnectionRecoveryUsing
                (const (pure ()))
                (Backend \state _ _ _ -> do
                    attempt <- atomicModifyIORef' attempts \n -> (n + 1, n + 1)
                    pure $
                        if attempt == 1
                            then Left $ ConnectionError
                                "WebSocket receive error: ParseException \"not enough bytes\""
                            else Right BackendResult
                                { backendOutput =
                                    emptyTurnOutput
                                        "response" [] (Just "done")
                                , backendState = state
                                })
        result <- backend.submitTurn emptyBackendSnapshot Nothing [] (const (pure ()))
        result `shouldBe`
            Right BackendResult
                { backendOutput =
                    emptyTurnOutput "response" [] (Just "done")
                , backendState = emptyBackendSnapshot
                }
        readIORef attempts `shouldReturn` 2

    it "does not repeat a restart boundary the backend already emitted" do
        attempts <- newIORef (0 :: Int)
        events <- newIORef []
        let backend = withConnectionRecoveryUsing
                (const (pure ()))
                (Backend \state _ _ onEvent -> do
                    attempt <- atomicModifyIORef' attempts
                        \n -> (n + 1, n + 1)
                    if attempt == 1
                        then do
                            onEvent (TextDelta "partial")
                            onEvent (ResponseRestarted "inner restart")
                            pure (Left (ConnectionError "dropped"))
                        else
                            pure (Right
                                BackendResult
                                    { backendOutput =
                                        emptyTurnOutput
                                            "response" [] (Just "done")
                                    , backendState = state
                                    }))
        result <- backend.submitTurn emptyBackendSnapshot Nothing []
            (\event -> modifyIORef' events (<> [event]))
        result `shouldBe`
            Right BackendResult
                { backendOutput =
                    emptyTurnOutput "response" [] (Just "done")
                , backendState = emptyBackendSnapshot
                }
        readIORef attempts `shouldReturn` 2
        readIORef events `shouldReturn`
            [ TextDelta "partial"
            , ResponseRestarted "inner restart"
            , ActivityUpdated
                "Connection lost; waiting for internet. Retrying automatically in 1s (Esc or Ctrl-C to cancel)…"
            , ActivityUpdated "Checking internet connection…"
            ]

    it "restarts a submission after visible output streamed" do
        attempts <- newIORef (0 :: Int)
        waits <- newIORef []
        events <- newIORef []
        let backend = withConnectionRecoveryUsing
                (\delay -> modifyIORef' waits (<> [delay]))
                (Backend \state _ _ onEvent -> do
                    attempt <- atomicModifyIORef' attempts
                        \n -> (n + 1, n + 1)
                    if attempt == 1
                        then do
                            onEvent (TextDelta "partial")
                            pure (Left (ConnectionError "dropped"))
                        else do
                            onEvent (TextDelta "complete")
                            pure (Right
                                BackendResult
                                    { backendOutput =
                                        emptyTurnOutput
                                            "response"
                                            []
                                            (Just "complete")
                                    , backendState = state
                                    }))
        result <- backend.submitTurn emptyBackendSnapshot Nothing []
            (\event -> modifyIORef' events (<> [event]))
        result `shouldBe`
            Right BackendResult
                { backendOutput =
                    emptyTurnOutput "response" [] (Just "complete")
                , backendState = emptyBackendSnapshot
                }
        readIORef attempts `shouldReturn` 2
        readIORef waits `shouldReturn` [1_000_000]
        readIORef events `shouldReturn`
            [ TextDelta "partial"
            , ActivityUpdated
                "Connection lost; waiting for internet. Retrying automatically in 1s (Esc or Ctrl-C to cancel)…"
            , ActivityUpdated "Checking internet connection…"
            , ResponseRestarted
                "Connection interrupted the response; restarting automatically. The new attempt may repeat partial output shown above."
            , TextDelta "complete"
            ]

    it "restarts a submission after a streamed tool call was announced" do
        attempts <- newIORef (0 :: Int)
        events <- newIORef []
        let call = functionToolCall "call-1" "shell" "{}"
            backend = withConnectionRecoveryUsing
                (\_ -> pure ())
                (Backend \state _ _ onEvent -> do
                    attempt <- atomicModifyIORef' attempts
                        \n -> (n + 1, n + 1)
                    if attempt == 1
                        then do
                            onEvent (ToolStarted call)
                            pure (Left (ConnectionError "dropped"))
                        else pure (Right
                            BackendResult
                                { backendOutput =
                                    emptyTurnOutput "response" [] (Just "done")
                                , backendState = state
                                }))
        _ <- backend.submitTurn emptyBackendSnapshot Nothing []
            (\event -> modifyIORef' events (<> [event]))
        readIORef attempts `shouldReturn` 2
        recorded <- readIORef events
        [message | ResponseRestarted message <- recorded]
            `shouldBe`
                [ "Connection interrupted the response; restarting automatically. The new attempt may repeat partial output shown above."
                ]

    it "does not duplicate queued inputs across reconnect attempts" do
        attempts <- newIORef (0 :: Int)
        pending <- newPendingInputs
        enqueuePendingInput pending (UserMessage "queued")
        seen <- newIORef []
        let backend =
                withPendingInputs pending $
                    withConnectionRecoveryUsing
                        (const (pure ()))
                        (Backend \state _ inputs _ -> do
                            modifyIORef' seen (<> [inputs])
                            attempt <- atomicModifyIORef' attempts
                                \n -> (n + 1, n + 1)
                            pure $
                                if attempt == 1
                                    then Left (ConnectionError "offline")
                                    else Right BackendResult
                                        { backendOutput =
                                            emptyTurnOutput
                                                "response" [] (Just "done")
                                        , backendState = state
                                        })
            expected = [UserMessage "queued", UserMessage "current"]
        result <- backend.submitTurn emptyBackendSnapshot Nothing [UserMessage "current"]
            (const (pure ()))
        result `shouldBe`
            Right BackendResult
                { backendOutput =
                    emptyTurnOutput "response" [] (Just "done")
                , backendState = emptyBackendSnapshot
                }
        readIORef seen `shouldReturn` [expected, expected]

    it "restarts partial output after a transient provider error" do
        attempts <- newIORef (0 :: Int)
        waits <- newIORef []
        events <- newIORef []
        let backend = withConnectionRecoveryUsing
                (\delay -> modifyIORef' waits (<> [delay]))
                (Backend \state _ _ onEvent -> do
                    attempt <- atomicModifyIORef' attempts
                        \n -> (n + 1, n + 1)
                    if attempt == 1
                        then do
                            onEvent (ReasoningDelta "partial thought")
                            pure $
                                Left
                                    (ProviderError
                                        ApiErrorType
                                        "internal error"
                                        Nothing)
                        else
                            pure $
                                Right BackendResult
                                    { backendOutput =
                                        emptyTurnOutput
                                            "response" [] (Just "done")
                                    , backendState = state
                                    })
        result <- backend.submitTurn emptyBackendSnapshot Nothing []
            (\event -> modifyIORef' events (<> [event]))
        result `shouldBe`
            Right BackendResult
                { backendOutput =
                    emptyTurnOutput "response" [] (Just "done")
                , backendState = emptyBackendSnapshot
                }
        readIORef attempts `shouldReturn` 2
        readIORef waits `shouldReturn` [3_000_000]
        readIORef events `shouldReturn`
            [ ReasoningDelta "partial thought"
            , ActivityUpdated
                "Provider returned a temporary error; retrying automatically in 3s (attempt 1/2; Esc or Ctrl-C to cancel)…"
            , ActivityUpdated
                "Retrying provider request (attempt 1/2)…"
            , ResponseRestarted
                "Provider interrupted the response; restarting automatically. The new attempt may repeat partial output shown above."
            ]

    it "stops after two transient provider retries" do
        attempts <- newIORef (0 :: Int)
        waits <- newIORef []
        let expected =
                ProviderError ApiErrorType "internal error" Nothing
            backend = withConnectionRecoveryUsing
                (\delay -> modifyIORef' waits (<> [delay]))
                (Backend \_ _ _ _ -> do
                    modifyIORef' attempts (+ 1)
                    pure (Left expected))
        result <- backend.submitTurn emptyBackendSnapshot Nothing [] (const (pure ()))
        result `shouldBe` Left expected
        readIORef attempts `shouldReturn` 3
        readIORef waits `shouldReturn` [3_000_000, 6_000_000]

    it "honors a bounded provider Retry-After" do
        transientRetryDelayMicros
            (ProviderError ServiceUnavailableError "later" (Just 9))
            1
            `shouldBe` 9_000_000
        transientRetryDelayMicros
            (ProviderError ServiceUnavailableError "later" (Just 120))
            1
            `shouldBe` 60_000_000

    it "backs off to a bounded polling interval" do
        map reconnectDelayMicros [1 .. 7]
            `shouldBe`
                [ 1_000_000
                , 2_000_000
                , 4_000_000
                , 8_000_000
                , 15_000_000
                , 15_000_000
                , 15_000_000
                ]

recoveryWatcher :: TVar Word64 -> RecoveryWatcher
recoveryWatcher generation = RecoveryWatcher do
    baseline <- readTVarIO generation
    pure $ atomically do
            current <- readTVar generation
            check (current > baseline)
