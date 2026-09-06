module Agent.OpenAI.LoopBackendSpec.TransportSpec (spec) where

import Agent.Error
    ( ApiError(..)
    , ErrorType(..)
    )
import Agent.Loop
import Agent.OpenAI.LoopBackend
import Agent.OpenAI.WebSocketClient
    ( newCodexTurnState
    , readCodexTurnState
    , recordCodexTurnState
    )
import Agent.Responses.Types
import Agent.ToolDispatch
import Control.Retry (constantDelay, limitRetries)
import Data.IORef
import Test.Hspec
import Agent.OpenAI.LoopBackendSpec.Fixtures

spec :: Spec
spec = do
    describe "openAiBackendWithTransportFallback" do
        it "switches permanently to fallback after a pre-output connection error" do
            fallbackActive <- newIORef False
            primaryCalls <- newIORef (0 :: Int)
            fallbackCalls <- newIORef (0 :: Int)
            transcript <- newIORef []
            let primary = Backend \_state _previous _inputs _onEvent -> do
                    modifyIORef' primaryCalls (+ 1)
                    pure (Left (ConnectionError
                        "WebSocket receive error: ParseException \"not enough bytes\""))
                fallback = Backend \state _previous _inputs _onEvent -> do
                    modifyIORef' fallbackCalls (+ 1)
                    pure $ Right BackendResult
                        { backendOutput =
                            emptyTurnOutput "resp-http" [] (Just "ok")
                        , backendState = state
                        }
                backend =
                    openAiBackendWithTransportFallback
                        fallbackActive primary fallback
            first <- submitWithState transcript backend Nothing
                [UserMessage "one"] (const (pure ()))
            second <- submitWithState transcript backend (Just "resp-http")
                [UserMessage "two"] (const (pure ()))
            first `shouldBe` Right (emptyTurnOutput "resp-http" [] (Just "ok"))
            second `shouldBe` Right (emptyTurnOutput "resp-http" [] (Just "ok"))
            readIORef fallbackActive `shouldReturn` True
            readIORef primaryCalls `shouldReturn` 1
            readIORef fallbackCalls `shouldReturn` 2

        it "replays over the fallback behind a restart boundary after visible output" do
            fallbackActive <- newIORef False
            fallbackCalls <- newIORef (0 :: Int)
            transcript <- newIORef []
            events <- newIORef []
            let primary = Backend \_state _previous _inputs onEvent -> do
                    onEvent (TextDelta "partial")
                    pure (Left (ConnectionError "socket closed"))
                fallback = Backend \state _previous _inputs onEvent -> do
                    modifyIORef' fallbackCalls (+ 1)
                    onEvent (TextDelta "replayed")
                    pure $ Right BackendResult
                        { backendOutput =
                            emptyTurnOutput "resp-http" [] (Just "replayed")
                        , backendState = state
                        }
                backend =
                    openAiBackendWithTransportFallback
                        fallbackActive primary fallback
            result <- submitWithState transcript backend Nothing
                [UserMessage "one"] (\event -> modifyIORef' events (<> [event]))
            fmap (.assistantText) result `shouldBe` Right (Just "replayed")
            readIORef fallbackActive `shouldReturn` True
            readIORef fallbackCalls `shouldReturn` 1
            readIORef events `shouldReturn`
                [ TextDelta "partial"
                , ResponseRestarted
                    "Connection interrupted the response; retrying over the HTTP transport. The new attempt may repeat partial output shown above."
                , TextDelta "replayed"
                ]

        it "does not repeat a restart boundary the primary already emitted" do
            fallbackActive <- newIORef False
            transcript <- newIORef []
            events <- newIORef []
            let primary = Backend \_state _previous _inputs onEvent -> do
                    onEvent (TextDelta "partial")
                    onEvent (ResponseRestarted "inner restart")
                    pure (Left (ConnectionError "socket closed"))
                fallback = Backend \state _previous _inputs _onEvent ->
                    pure $ Right BackendResult
                        { backendOutput =
                            emptyTurnOutput "resp-http" [] (Just "ok")
                        , backendState = state
                        }
                backend =
                    openAiBackendWithTransportFallback
                        fallbackActive primary fallback
            result <- submitWithState transcript backend Nothing
                [UserMessage "one"] (\event -> modifyIORef' events (<> [event]))
            fmap (.assistantText) result `shouldBe` Right (Just "ok")
            readIORef events `shouldReturn`
                [TextDelta "partial", ResponseRestarted "inner restart"]

        it "replays over the fallback transport after only hidden output streamed" do
            fallbackActive <- newIORef False
            primaryCalls <- newIORef (0 :: Int)
            fallbackCalls <- newIORef (0 :: Int)
            transcript <- newIORef []
            let connectionFailure = ConnectionError "socket closed"
                outputEvent = ResponseOutputItemDoneEvent
                    { item = assistantItem "partial"
                    , outputIndex = Just 0
                    , sequenceNumber = Nothing
                    }
                sendPrimary _request _previous onEvent = do
                    modifyIORef' primaryCalls (+ 1)
                    onEvent outputEvent
                    pure (Left connectionFailure)
                primary = openAiBackendWithRetryPolicies
                    (constantDelay 0 <> limitRetries 3)
                    (limitRetries 0)
                    sendPrimary
                    (pure baseParams)
                fallback = Backend \state _previous _inputs _onEvent -> do
                    modifyIORef' fallbackCalls (+ 1)
                    pure $ Right BackendResult
                        { backendOutput =
                            emptyTurnOutput "resp-http" [] (Just "duplicate")
                        , backendState = state
                        }
                backend =
                    openAiBackendWithTransportFallback
                        fallbackActive primary fallback
            result <- submitWithState transcript backend Nothing
                [UserMessage "one"] (const (pure ()))
            -- The dead socket committed nothing, so the hidden partial item
            -- does not block the replay; only visible deltas would.
            fmap (.assistantText) result `shouldBe` Right (Just "duplicate")
            readIORef fallbackActive `shouldReturn` True
            readIORef primaryCalls `shouldReturn` 1
            readIORef fallbackCalls `shouldReturn` 1

        it "closes an announced tool block before replaying over the fallback" do
            fallbackActive <- newIORef False
            events <- newIORef []
            transcript <- newIORef []
            let call = functionToolCall "fc-1" "shell" "{}"
                sendPrimary _request _previous onEvent = do
                    onEvent ResponseOutputItemAddedEvent
                        { item = functionCallItem "fc-1" "shell" "{}"
                        , outputIndex = Just 0
                        , sequenceNumber = Nothing
                        }
                    pure (Left (ConnectionError "socket closed"))
                primary = openAiBackendWithRetryPolicies
                    (constantDelay 0 <> limitRetries 3)
                    (limitRetries 0)
                    sendPrimary
                    (pure baseParams)
                fallback = Backend \state _previous _inputs onEvent -> do
                    onEvent (TextDelta "replayed")
                    pure $ Right BackendResult
                        { backendOutput =
                            emptyTurnOutput "resp-http" [] (Just "replayed")
                        , backendState = state
                        }
                backend =
                    openAiBackendWithTransportFallback
                        fallbackActive primary fallback
            result <- submitWithState transcript backend Nothing
                [UserMessage "one"] (\event -> modifyIORef' events (<> [event]))
            fmap (.assistantText) result `shouldBe` Right (Just "replayed")
            recorded <- readIORef events
            [() | ToolStarted started <- recorded, started.callId == call.callId]
                `shouldBe` [()]
            dropWhile (/= ResponseAttemptDiscarded) recorded
                `shouldBe` [ResponseAttemptDiscarded, TextDelta "replayed"]

        it "falls back immediately after a websocket connection-limit error" do
            fallbackActive <- newIORef False
            primaryCalls <- newIORef (0 :: Int)
            fallbackCalls <- newIORef (0 :: Int)
            events <- newIORef []
            primaryTranscript <- newIORef []
            let sendPrimary _request _previous _onEvent = do
                    modifyIORef' primaryCalls (+ 1)
                    pure (Left (ProviderError WebSocketConnectionLimitReached
                        "too many websocket connections" Nothing))
                primary = openAiBackendWithRetryPolicy
                    (constantDelay 0 <> limitRetries 3)
                    sendPrimary
                    (pure baseParams)
                fallback = Backend \state _previous _inputs _onEvent -> do
                    modifyIORef' fallbackCalls (+ 1)
                    pure $ Right BackendResult
                        { backendOutput =
                            emptyTurnOutput "resp-http" [] (Just "ok")
                        , backendState = state
                        }
                backend =
                    openAiBackendWithTransportFallback
                        fallbackActive primary fallback
            result <- submitWithState primaryTranscript backend Nothing
                [UserMessage "one"] (modifyIORef' events . (:))
            result `shouldBe` Right (emptyTurnOutput "resp-http" [] (Just "ok"))
            readIORef events `shouldReturn` []
            readIORef primaryCalls `shouldReturn` 1
            readIORef fallbackCalls `shouldReturn` 1

        it "falls back after an exhausted websocket upgrade rejection" do
            fallbackActive <- newIORef False
            primaryCalls <- newIORef (0 :: Int)
            fallbackCalls <- newIORef (0 :: Int)
            transcript <- newIORef []
            let primary = Backend \_state _previous _inputs _onEvent -> do
                    modifyIORef' primaryCalls (+ 1)
                    pure (Left (HttpError 403
                        "WebSocket handshake returned HTTP 403"))
                fallback = Backend \state _previous _inputs _onEvent -> do
                    modifyIORef' fallbackCalls (+ 1)
                    pure $ Right BackendResult
                        { backendOutput =
                            emptyTurnOutput "resp-http" [] (Just "ok")
                        , backendState = state
                        }
                backend =
                    openAiBackendWithTransportFallback
                        fallbackActive primary fallback
            result <- submitWithState transcript backend Nothing
                [UserMessage "one"] (const (pure ()))
            result `shouldBe` Right (emptyTurnOutput "resp-http" [] (Just "ok"))
            readIORef fallbackActive `shouldReturn` True
            readIORef primaryCalls `shouldReturn` 1
            readIORef fallbackCalls `shouldReturn` 1

        it "does not mistake a logical HTTP 403 for a websocket upgrade failure" do
            fallbackActive <- newIORef False
            fallbackCalls <- newIORef (0 :: Int)
            transcript <- newIORef []
            let primary = Backend \_state _previous _inputs _onEvent ->
                    pure (Left (HttpError 403 "model access denied"))
                fallback = Backend \state _previous _inputs _onEvent -> do
                    modifyIORef' fallbackCalls (+ 1)
                    pure $ Right BackendResult
                        { backendOutput =
                            emptyTurnOutput "resp-http" [] (Just "ok")
                        , backendState = state
                        }
                backend =
                    openAiBackendWithTransportFallback
                        fallbackActive primary fallback
            result <- submitWithState transcript backend Nothing
                [UserMessage "one"] (const (pure ()))
            result `shouldBe` Left (HttpError 403 "model access denied")
            readIORef fallbackActive `shouldReturn` False
            readIORef fallbackCalls `shouldReturn` 0

        it "keeps websocket handshake 401 on the credential recovery path" do
            isOpenAiWebSocketTransportFailure
                (HttpError 401 "WebSocket handshake returned HTTP 401")
                `shouldBe` False

        it "preserves non-transport provider failures" do
            fallbackActive <- newIORef False
            fallbackCalls <- newIORef (0 :: Int)
            transcript <- newIORef []
            let primary = Backend \_state _previous _inputs _onEvent ->
                    pure (Left (ProviderError InvalidRequestError
                        "bad request" Nothing))
                fallback = Backend \state _previous _inputs _onEvent -> do
                    modifyIORef' fallbackCalls (+ 1)
                    pure $ Right BackendResult
                        { backendOutput =
                            emptyTurnOutput "resp-http" [] (Just "ok")
                        , backendState = state
                        }
                backend =
                    openAiBackendWithTransportFallback
                        fallbackActive primary fallback
            result <- submitWithState transcript backend Nothing
                [UserMessage "one"] (const (pure ()))
            result `shouldBe` Left (ProviderError InvalidRequestError
                "bad request" Nothing)
            readIORef fallbackActive `shouldReturn` False
            readIORef fallbackCalls `shouldReturn` 0

    describe "withCodexTurnStateScope" do
        it "resets on a new prompt and preserves tool continuations" do
            turnState <- newCodexTurnState
            recordCodexTurnState turnState "stale"
            observed <- newIORef []
            transcript <- newIORef []
            let rawBackend =
                    Backend \state _previous _inputs _onEvent -> do
                        value <- readCodexTurnState turnState
                        modifyIORef' observed (<> [value])
                        pure $ Right BackendResult
                            { backendOutput =
                                emptyTurnOutput "resp-state" [] (Just "ok")
                            , backendState = state
                            }
                backend =
                    withCodexTurnStateScope
                        (pure turnState)
                        rawBackend
            _ <- submitWithState transcript backend Nothing
                [UserMessage "new turn"] (const (pure ()))
            recordCodexTurnState turnState "current"
            _ <- submitWithState transcript backend (Just "resp-state")
                [CompletedTool (functionResult "call-1" "done")]
                (const (pure ()))

            readIORef observed
                `shouldReturn` [Nothing, Just "current"]
