module Agent.CLI.ConnectivitySpec (spec) where

import Agent.CLI.Connectivity
    ( reconnectDelayMicros
    , transientRetryDelayMicros
    , withConnectionRecoveryUsing
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
    , emptyTurnOutput
    )
import Data.IORef
import Test.Hspec

spec :: Spec
spec = describe "withConnectionRecovery" do
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
        result <- backend.submitTurn [] (Just "previous") inputs
            (\event -> modifyIORef' events (<> [event]))

        result `shouldBe`
            Right BackendResult
                { backendOutput =
                    emptyTurnOutput "response" [] (Just "done")
                , backendState = []
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
        result <- backend.submitTurn [] Nothing [] (const (pure ()))
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
        result <- backend.submitTurn [] Nothing [] (const (pure ()))
        result `shouldBe`
            Right BackendResult
                { backendOutput =
                    emptyTurnOutput "response" [] (Just "done")
                , backendState = []
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
        result <- backend.submitTurn [] Nothing []
            (\event -> modifyIORef' events (<> [event]))
        result `shouldBe`
            Right BackendResult
                { backendOutput =
                    emptyTurnOutput "response" [] (Just "done")
                , backendState = []
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
        result <- backend.submitTurn [] Nothing []
            (\event -> modifyIORef' events (<> [event]))
        result `shouldBe`
            Right BackendResult
                { backendOutput =
                    emptyTurnOutput "response" [] (Just "complete")
                , backendState = []
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
        _ <- backend.submitTurn [] Nothing []
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
        result <- backend.submitTurn [] Nothing [UserMessage "current"]
            (const (pure ()))
        result `shouldBe`
            Right BackendResult
                { backendOutput =
                    emptyTurnOutput "response" [] (Just "done")
                , backendState = []
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
        result <- backend.submitTurn [] Nothing []
            (\event -> modifyIORef' events (<> [event]))
        result `shouldBe`
            Right BackendResult
                { backendOutput =
                    emptyTurnOutput "response" [] (Just "done")
                , backendState = []
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
        result <- backend.submitTurn [] Nothing [] (const (pure ()))
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
