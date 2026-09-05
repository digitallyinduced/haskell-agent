module Agent.OpenAI.LoopBackendSpec.StateSpec (spec) where

import Agent.Error
    ( ApiError(..)
    , ErrorType(..)
    )
import Agent.Loop
import Agent.OpenAI.LoopBackend
import Agent.Responses.Request (stripReplayedItemStatus)
import Agent.Responses.Types
import Agent.ToolDispatch
import Control.Retry (constantDelay, limitRetries)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.IORef
import qualified Data.Text as Text
import Test.Hspec
import Agent.OpenAI.LoopBackendSpec.Fixtures

spec :: Spec
spec = do
    describe "statelessResponsesBackend" do
        it "replays the local transcript on tool follow-ups" do
            seen <- newIORef []
            events <- newIORef []
            remaining <- newIORef
                [ testResponse "resp-1"
                    [functionCallItem "c1" "read_file" "{\"target_file\":\"README.md\"}"]
                , testResponse "resp-2" [assistantItem "done"]
                ]
            transcript <- newIORef []
            let backend = statelessResponsesBackend
                    (scriptedStatelessSend seen remaining)
                    (pure baseParams)

            first <- submitWithState transcript backend Nothing [UserMessage "read it"]
                (modifyIORef' events . (:))
            first `shouldBe` Right (emptyTurnOutput "resp-1"
                [functionToolCall "c1" "read_file" "{\"target_file\":\"README.md\"}"]
                Nothing)

            second <- submitWithState transcript backend (Just "resp-1")
                [CompletedTool (functionResult "c1" "file contents")]
                (const (pure ()))
            second `shouldBe` Right (emptyTurnOutput "resp-2" [] (Just "done"))

            requests <- readIORef seen
            -- Replayed provider items lose their lifecycle status on the wire.
            map inputItems requests `shouldBe`
                [ turnInputsToItems [UserMessage "read it"]
                , map stripReplayedItemStatus
                    (turnInputsToItems [UserMessage "read it"]
                        <> [functionCallItem "c1" "read_file"
                            "{\"target_file\":\"README.md\"}"]
                        <> turnInputsToItems
                            [CompletedTool (functionResult "c1" "file contents")])
                ]
            readIORef transcript `shouldReturn`
                ( turnInputsToItems [UserMessage "read it"]
                    <> [functionCallItem "c1" "read_file"
                        "{\"target_file\":\"README.md\"}"]
                    <> turnInputsToItems
                        [CompletedTool (functionResult "c1" "file contents")]
                    <> [assistantItem "done"]
                )
            reverse <$> readIORef events `shouldReturn` [TextDelta "call"]

        it "leaves the transcript unchanged when the transport fails" do
            seen <- newIORef []
            remaining <- newIORef [testResponse "resp-1" [assistantItem "hi"]]
            transcript <- newIORef []
            let send request onEvent = do
                    n <- length <$> readIORef seen
                    if n == 0
                        then do
                            modifyIORef' seen (++ [request])
                            pure (Left (ConnectionError "boom"))
                        else scriptedStatelessSend seen remaining request onEvent
                backend = statelessResponsesBackend send (pure baseParams)

            failed <- submitWithState transcript backend Nothing [UserMessage "hi"] (const (pure ()))
            failed `shouldBe` Left (ConnectionError "boom")

            recovered <- submitWithState transcript backend Nothing [UserMessage "hi"] (const (pure ()))
            recovered `shouldBe` Right (emptyTurnOutput "resp-1" [] (Just "hi"))
            map inputItems <$> readIORef seen `shouldReturn`
                [ turnInputsToItems [UserMessage "hi"]
                , turnInputsToItems [UserMessage "hi"]
                ]

        it "re-reads request params without dropping the transcript" do
            seen <- newIORef []
            remaining <- newIORef
                [ testResponse "resp-1" [assistantItem "one"]
                , testResponse "resp-2" [assistantItem "two"]
                ]
            paramsRef <- newIORef (withEffort "low" baseParams)
            transcript <- newIORef []
            let backend = statelessResponsesBackend
                    (scriptedStatelessSend seen remaining)
                    (readIORef paramsRef)
            _ <- submitWithState transcript backend Nothing [UserMessage "one"] (const (pure ()))
            writeIORef paramsRef (withEffort "high" baseParams)
            _ <- submitWithState transcript backend (Just "resp-1") [UserMessage "two"]
                (const (pure ()))
            requests <- readIORef seen
            map reasoningEffort requests `shouldBe` [Just "low", Just "high"]
            map inputItems requests `shouldBe`
                [ turnInputsToItems [UserMessage "one"]
                , map stripReplayedItemStatus
                    (turnInputsToItems [UserMessage "one"]
                        <> [assistantItem "one"]
                        <> turnInputsToItems [UserMessage "two"])
                ]

    describe "openAiBackendWith" do
        it "continues a reasoning-only incomplete response by response id" do
            seenPrevious <- newIORef []
            remaining <- newIORef
                [ reasoningIncompleteResponse
                , testResponse "resp-final" [assistantItem "done"]
                ]
            let send _request previous _onEvent = do
                    modifyIORef' seenPrevious (<> [previous])
                    atomicModifyIORef' remaining \case
                        response : rest -> (rest, Right response)
                        [] -> error "unexpected extra response submission"
                backend = openAiBackendWith send (pure baseParams)
            config <- loopConfig backend

            result <- runLoop config Nothing "investigate"

            result `shouldBe` Right LoopResult
                { finalResponseId = "resp-final"
                , finalText = Just "done"
                , turnsUsed = 2
                , tokenUsage = TokenUsage
                    { inputTokens = 64000
                    , outputTokens = 128000
                    , cachedTokens = 0
                    }
                }
            readIORef seenPrevious `shouldReturn`
                [Nothing, Just "resp-reasoning-incomplete"]
            readIORef remaining `shouldReturn` []

        it "shows raw reasoning only when explicitly enabled" do
            let send _request _previous onEvent = do
                    onEvent (deltaEvent EventReasoningTextDelta "raw")
                    onEvent (deltaEvent EventReasoningSummaryTextDelta "summary")
                    pure $ Right (testResponse "resp-1" [assistantItem "ok"])
                collect showRawReasoning = do
                    events <- newIORef []
                    transcript <- newIORef []
                    let backend =
                            openAiBackendWithReasoningVisibility
                                showRawReasoning
                                send
                                (pure baseParams)
                    _ <- submitWithState transcript backend Nothing
                        [UserMessage "hello"]
                        (modifyIORef' events . (:))
                    reverse <$> readIORef events

            collect False `shouldReturn` [ReasoningDelta "summary"]
            collect True `shouldReturn`
                [ReasoningDelta "raw", ReasoningDelta "summary"]

        it "sends only the new items and threads previous_response_id" do
            seen <- newIORef []
            events <- newIORef []
            transcript <- newIORef []
            let backend = openAiBackendWith (recordingSend seen) (pure baseParams)
            result <- submitWithState transcript backend (Just "resp-prev")
                [UserMessage "hello"]
                (modifyIORef' events . (:))
            result `shouldBe` Right (emptyTurnOutput "resp-1" [] (Just "ok"))
            [(request, previous)] <- readIORef seen
            previous `shouldBe` Just "resp-prev"
            request.model `shouldBe` Just "gpt-5.6-luna"
            request.input `shouldBe` Just (ResponseInputItems (turnInputsToItems [UserMessage "hello"]))
            reverse <$> readIORef events `shouldReturn` [TextDelta "ok"]

        it "does not accumulate prior turns on the OpenAI transport" do
            seen <- newIORef []
            transcript <- newIORef []
            let backend = openAiBackendWith (recordingSend seen) (pure baseParams)
            _ <- submitWithState transcript backend Nothing [UserMessage "one"] (const (pure ()))
            _ <- submitWithState transcript backend (Just "resp-1")
                [CompletedTool (functionResult "c1" "out")]
                (const (pure ()))
            requests <- readIORef seen
            map (inputItems . fst) requests `shouldBe`
                [ turnInputsToItems [UserMessage "one"]
                , turnInputsToItems [CompletedTool (functionResult "c1" "out")]
                ]
            length <$> readIORef transcript `shouldReturn` 4

        it "re-reads request params on each turn so effort can change" do
            seen <- newIORef []
            paramsRef <- newIORef (withEffort "low" baseParams)
            transcript <- newIORef []
            let backend = openAiBackendWith (recordingSend seen) (readIORef paramsRef)
            _ <- submitWithState transcript backend Nothing [UserMessage "one"] (const (pure ()))
            writeIORef paramsRef (withEffort "high" baseParams)
            _ <- submitWithState transcript backend (Just "resp-1") [UserMessage "two"] (const (pure ()))
            map (reasoningEffort . fst) <$> readIORef seen
                `shouldReturn` [Just "low", Just "high"]

        it "replays the local transcript when previous_response_id is missing" do
            seen <- newIORef []
            let seed = turnInputsToItems [UserMessage "old"]
            transcript <- newIORef seed
            let send request previous onEvent = do
                    modifyIORef' seen (++ [(request, previous)])
                    case previous of
                        Just _ ->
                            pure $ Left $ ProviderError PreviousResponseNotFound
                                "previous_response_id was not found" Nothing
                        Nothing -> do
                            onEvent (deltaEvent EventOutputTextDelta "ok")
                            pure $ Right (testResponse "resp-2" [assistantItem "ok"])
                backend = openAiBackendWith send (pure baseParams)
            result <- submitWithState transcript backend (Just "resp-missing")
                [UserMessage "new"]
                (const (pure ()))
            result `shouldBe` Right (emptyTurnOutput "resp-2" [] (Just "ok"))
            requests <- readIORef seen
            map snd requests `shouldBe` [Just "resp-missing", Nothing]
            map (inputItems . fst) requests `shouldBe`
                [ turnInputsToItems [UserMessage "new"]
                , seed <> turnInputsToItems [UserMessage "new"]
                ]

        it "drops xAI checkpoints when replaying after a provider switch" do
            seen <- newIORef []
            let xaiCheckpoint =
                    ContextCompactionItemValue ContextCompactionItem
                        { itemId = Just "xai-context"
                        , encryptedContent = Just "opaque-xai"
                        }
                retained = turnInputsToItems [UserMessage "old"]
                seed =
                    [ xaiCheckpoint
                    , compactionCheckpointOriginItem "xai"
                    ]
                        <> retained
            transcript <- newIORef seed
            let backend =
                    openAiBackendWith (recordingSend seen) (pure baseParams)
            _ <- submitWithState transcript backend Nothing
                [UserMessage "new"]
                (const (pure ()))
            [(request, previous)] <- readIORef seen
            previous `shouldBe` Nothing
            inputItems request `shouldBe`
                retained <> turnInputsToItems [UserMessage "new"]

        it "starts a fresh chain when inherited cache retention is rejected" do
            seen <- newIORef []
            let seed = turnInputsToItems [UserMessage "old"]
                cacheError = ProviderError
                    (UnknownErrorType "invalid_parameter")
                    "prompt_cache_retention is not supported on this model \
                    \(code: invalid_parameter)"
                    Nothing
            transcript <- newIORef seed
            let send request previous onEvent = do
                    modifyIORef' seen (++ [(request, previous)])
                    case previous of
                        Just _ -> pure (Left cacheError)
                        Nothing -> do
                            onEvent (deltaEvent EventOutputTextDelta "ok")
                            pure $ Right
                                (testResponse "resp-fresh" [assistantItem "ok"])
                backend = openAiBackendWith send (pure baseParams)
            result <- submitWithState transcript backend (Just "resp-cache")
                [CompletedTool (functionResult "c1" "tool output")]
                (const (pure ()))
            result `shouldBe`
                Right (emptyTurnOutput "resp-fresh" [] (Just "ok"))
            requests <- readIORef seen
            map snd requests `shouldBe` [Just "resp-cache", Nothing]
            map (inputItems . fst) requests `shouldBe`
                [ turnInputsToItems
                    [CompletedTool (functionResult "c1" "tool output")]
                , seed <> turnInputsToItems
                    [CompletedTool (functionResult "c1" "tool output")]
                ]

        it "replays a call and output when continuation tool state is lost" do
            seen <- newIORef []
            let callId = "call_lost"
                seed =
                    turnInputsToItems [UserMessage "old"]
                        <> [functionCallItem callId "read_file" "{}"]
                chainError = ProviderError
                    InvalidRequestError
                    "No tool output found for function call call_lost."
                    Nothing
                resultInput =
                    [CompletedTool (functionResult callId "file contents")]
            transcript <- newIORef seed
            let send request previous onEvent = do
                    modifyIORef' seen (++ [(request, previous)])
                    case previous of
                        Just _ -> pure (Left chainError)
                        Nothing -> do
                            onEvent (deltaEvent EventOutputTextDelta "ok")
                            pure $ Right
                                (testResponse "resp-fresh" [assistantItem "ok"])
                backend = openAiBackendWith send (pure baseParams)
            result <- submitWithState transcript backend (Just "resp-call")
                resultInput
                (const (pure ()))
            result `shouldBe`
                Right (emptyTurnOutput "resp-fresh" [] (Just "ok"))
            requests <- readIORef seen
            map snd requests `shouldBe` [Just "resp-call", Nothing]
            map (inputItems . fst) requests `shouldBe`
                [ turnInputsToItems resultInput
                , map stripReplayedItemStatus
                    (seed <> turnInputsToItems resultInput)
                ]

        it "starts a fresh ordinary-function chain for native computer history" do
            seen <- newIORef []
            let legacyCall = ComputerCall
                    { computerCallItemId = Just "native-item"
                    , computerCallId = "native-call"
                    , computerActions = [ScreenshotAction]
                    , pendingSafetyChecks = []
                    , computerCallStatus = Just ItemCompleted
                    , computerCallExtra = KeyMap.empty
                    }
            transcript <- newIORef [ComputerCallItem legacyCall]
            let backend = openAiBackendWith
                    (recordingSend seen)
                    (pure baseParams)
            _ <- submitWithState transcript backend (Just "resp-native")
                [UserMessage "continue"]
                (const (pure ()))
            [(request, previous)] <- readIORef seen
            previous `shouldBe` Nothing
            case inputItems request of
                [FunctionCallItem function, MessageItem{}] -> do
                    function.name `shouldBe` computerFunctionName
                    function.namespace `shouldBe` Nothing
                other -> expectationFailure
                    ("legacy native item reached Codex: " <> show other)

        it "starts a fresh chain for the earlier Lite computer fallback" do
            seen <- newIORef []
            let legacyCall = FunctionCall
                    { itemId = Just "legacy-item"
                    , callId = "legacy-call"
                    , name = legacyComputerFunctionName
                    , namespace = Just computerFunctionNamespace
                    , provider = Nothing
                    , arguments =
                        "{\"actions\":[{\"type\":\"screenshot\"}]}"
                    , encryptedFunctionArgs = Nothing
                    , status = Just ItemCompleted
                    , async = Nothing
                    }
            transcript <- newIORef [FunctionCallItem legacyCall]
            let backend = openAiBackendWith
                    (recordingSend seen)
                    (pure baseParams)
            _ <- submitWithState transcript backend (Just "resp-legacy-lite")
                [UserMessage "continue"]
                (const (pure ()))
            [(request, previous)] <- readIORef seen
            previous `shouldBe` Nothing
            case inputItems request of
                [FunctionCallItem function, MessageItem{}] -> do
                    function.itemId `shouldBe` Nothing
                    function.name `shouldBe` computerFunctionName
                    function.namespace `shouldBe` Nothing
                other -> expectationFailure
                    ("legacy Lite computer item reached Codex: "
                        <> show other)

        it "strips explicitly requested cache retention and starts a fresh chain" do
            seen <- newIORef []
            let seed = turnInputsToItems [UserMessage "old"]
                cacheError = ProviderError
                    (UnknownErrorType "invalid_parameter")
                    "prompt_cache_retention is not supported on this model \
                    \(code: invalid_parameter)"
                    Nothing
                params = withPromptCacheRetention (Just "24h") baseParams
                send request previous _onEvent = do
                    modifyIORef' seen (++ [(request, previous)])
                    case previous of
                        Just _ -> pure (Left cacheError)
                        Nothing ->
                            pure $ Right
                                (testResponse "resp-fresh" [assistantItem "ok"])
            transcript <- newIORef seed
            let backend = openAiBackendWith send (pure params)
            result <- submitWithState transcript backend (Just "resp-cache")
                [UserMessage "new"]
                (const (pure ()))
            result `shouldBe`
                Right (emptyTurnOutput "resp-fresh" [] (Just "ok"))
            requests <- readIORef seen
            map snd requests `shouldBe` [Just "resp-cache", Nothing]
            map ((.promptCacheRetention) . fst) requests
                `shouldBe` [Nothing, Nothing]

        it "replays retained messages before an automatic compaction checkpoint" do
            seen <- newIORef []
            let checkpoint = compactionItem "opaque"
                history =
                    turnInputsToItems [UserMessage "old"]
                        <> [checkpoint]
                        <> turnInputsToItems [UserMessage "recent"]
            transcript <- newIORef history
            let backend = openAiBackendWith
                    (recordingSend seen)
                    (pure baseParams)
            _ <- submitWithState transcript backend Nothing [UserMessage "new"] (const (pure ()))
            [(request, previous)] <- readIORef seen
            previous `shouldBe` Nothing
            inputItems request `shouldBe`
                turnInputsToItems [UserMessage "old"]
                    <> [checkpoint]
                    <> turnInputsToItems [UserMessage "recent"]
                    <> turnInputsToItems [UserMessage "new"]

        it "retries transient Codex server errors before visible output" do
            attempts <- newIORef (0 :: Int)
            transcript <- newIORef []
            events <- newIORef []
            let serverError = ProviderError ApiErrorType
                    "An error occurred while processing your request. (code: server_error)"
                    Nothing
                send _request _previous _onEvent = do
                    modifyIORef' attempts (+ 1)
                    attempt <- readIORef attempts
                    pure if attempt < 3
                        then Left serverError
                        else Right (testResponse "resp-retried" [assistantItem "ok"])
                backend = openAiBackendWithRetryPolicy
                    (constantDelay 0 <> limitRetries 3)
                    send
                    (pure baseParams)
            result <- submitWithState transcript backend Nothing [UserMessage "one"]
                (modifyIORef' events . (:))
            result `shouldBe` Right (emptyTurnOutput "resp-retried" [] (Just "ok"))
            readIORef attempts `shouldReturn` 3
            observedEvents <- reverse <$> readIORef events
            observedEvents `shouldBe`
                [ ActivityUpdated
                    "Codex server error; retrying in 0s (attempt 1)…"
                , ActivityUpdated
                    "Retrying Codex request (attempt 1)…"
                , ActivityUpdated
                    "Codex server error; retrying in 0s (attempt 2)…"
                , ActivityUpdated
                    "Retrying Codex request (attempt 2)…"
                ]

        it "does not retry after visible output was streamed" do
            attempts <- newIORef (0 :: Int)
            transcript <- newIORef []
            events <- newIORef []
            let serverError = ProviderError ApiErrorType "server error" Nothing
                send _request _previous onEvent = do
                    modifyIORef' attempts (+ 1)
                    onEvent (deltaEvent EventOutputTextDelta "partial")
                    pure (Left serverError)
                backend = openAiBackendWithRetryPolicy
                    (constantDelay 0 <> limitRetries 3)
                    send
                    (pure baseParams)
            result <- submitWithState transcript backend Nothing [UserMessage "one"]
                (modifyIORef' events . (:))
            result `shouldBe` Left serverError
            readIORef attempts `shouldReturn` 1
            observedEvents <- readIORef events
            reverse observedEvents `shouldBe` [TextDelta "partial"]

        it "blocks credential failover after a tool call was streamed" do
            transcript <- newIORef []
            events <- newIORef []
            let exhausted = ProviderError UsageLimitReached
                    "usage exhausted" (Just 120)
                send _request _previous onEvent = do
                    onEvent ResponseOutputItemAddedEvent
                        { item = functionCallItem "fc-1" "shell" "{}"
                        , outputIndex = Just 0
                        , sequenceNumber = Nothing
                        }
                    pure (Left exhausted)
                backend = openAiBackendWithRetryPolicy
                    (constantDelay 0 <> limitRetries 3)
                    send
                    (pure baseParams)
            result <- submitWithState transcript backend Nothing [UserMessage "one"]
                (modifyIORef' events . (:))
            result `shouldBe` Left (ProviderError
                (UnknownErrorType "replay_unsafe")
                ( "provider failed after model output; refusing to replay: "
                    <> Text.pack (show exhausted)
                )
                Nothing)
            recorded <- reverse <$> readIORef events
            [() | ToolStarted started <- recorded, started.callId == "fc-1"]
                `shouldBe` [()]
            filter (not . isToolStartedEvent) recorded `shouldBe` []

        it "resubmits after a mid-response socket drop behind a restart boundary" do
            attempts <- newIORef (0 :: Int)
            transcript <- newIORef []
            events <- newIORef []
            let send _request _previous onEvent = do
                    modifyIORef' attempts (+ 1)
                    attempt <- readIORef attempts
                    if attempt == 1
                        then do
                            onEvent (deltaEvent EventOutputTextDelta "partial")
                            pure (Left (ConnectionError
                                "WebSocket closed by server (1012): "))
                        else do
                            onEvent (deltaEvent EventOutputTextDelta "complete")
                            pure (Right
                                (testResponse "resp-replayed"
                                    [assistantItem "complete"]))
                backend = openAiBackendWithRetryPolicies
                    (constantDelay 0 <> limitRetries 3)
                    (constantDelay 0 <> limitRetries 5)
                    send
                    (pure baseParams)
            result <- submitWithState transcript backend Nothing [UserMessage "one"]
                (modifyIORef' events . (:))
            result `shouldBe`
                Right (emptyTurnOutput "resp-replayed" [] (Just "complete"))
            readIORef attempts `shouldReturn` 2
            reverse <$> readIORef events `shouldReturn`
                [ TextDelta "partial"
                , ActivityUpdated
                    "Connection lost mid-response (WebSocket closed by server (1012): ); reconnecting in 0s (attempt 1)…"
                , ResponseRestarted
                    "Connection interrupted the response; restarting automatically. The new attempt may repeat partial output shown above."
                , ActivityUpdated "Reconnecting to Codex (attempt 1)…"
                , TextDelta "complete"
                ]

        it "does not replay after admitting a completed async tool call" do
            attempts <- newIORef (0 :: Int)
            admitted <- newIORef []
            let connectionFailure = ConnectionError "socket closed"
                asyncCall =
                    FunctionCallItem FunctionCall
                        { itemId = Nothing
                        , callId = "fc-async"
                        , name = "shell"
                        , namespace = Nothing
                        , provider = Nothing
                        , arguments = "{}"
                        , encryptedFunctionArgs = Nothing
                        , status = Just ItemCompleted
                        , async = Just True
                        }
                send _request _previous onEvent = do
                    modifyIORef' attempts (+ 1)
                    onEvent ResponseOutputItemDoneEvent
                        { item = asyncCall
                        , outputIndex = Just 0
                        , sequenceNumber = Nothing
                        }
                    pure (Left connectionFailure)
                backend = openAiBackendWithRetryPolicies
                    (constantDelay 0 <> limitRetries 3)
                    (constantDelay 0 <> limitRetries 5)
                    send
                    (pure baseParams)
            result <- backend.submitTurnWithCallbacks
                emptyBackendSnapshot
                Nothing
                [UserMessage "one"]
                BackendCallbacks
                    { onLoopEvent = const (pure ())
                    , onAsyncToolCall =
                        \call -> modifyIORef' admitted (call.callId :)
                    }
            result `shouldBe` Left (ProviderError
                (UnknownErrorType "replay_unsafe_websocket_transport")
                ( "provider failed after asynchronous tool call; "
                    <> "refusing to replay: "
                    <> Text.pack (show connectionFailure)
                )
                Nothing)
            readIORef attempts `shouldReturn` 1
            readIORef admitted `shouldReturn` ["fc-async"]

        it "discards a tool-only partial attempt before reconnecting" do
            attempts <- newIORef (0 :: Int)
            transcript <- newIORef []
            events <- newIORef []
            let send _request _previous onEvent = do
                    modifyIORef' attempts (+ 1)
                    attempt <- readIORef attempts
                    if attempt == 1
                        then do
                            onEvent ResponseOutputItemAddedEvent
                                { item = functionCallItem "fc-1" "shell" "{}"
                                , outputIndex = Just 0
                                , sequenceNumber = Nothing
                                }
                            pure (Left (ProviderError
                                WebSocketConnectionLimitReached
                                "too many websocket connections"
                                Nothing))
                        else pure (Right
                            (testResponse "resp-replayed" [assistantItem "ok"]))
                backend = openAiBackendWithRetryPolicies
                    (constantDelay 0 <> limitRetries 3)
                    (constantDelay 0 <> limitRetries 5)
                    send
                    (pure baseParams)
            result <- submitWithState transcript backend Nothing [UserMessage "one"]
                (modifyIORef' events . (:))
            result `shouldBe`
                Right (emptyTurnOutput "resp-replayed" [] (Just "ok"))
            readIORef attempts `shouldReturn` 2
            recorded <- reverse <$> readIORef events
            [() | ToolStarted started <- recorded, started.callId == "fc-1"]
                `shouldBe` [()]
            filter (not . isToolStartedEvent) recorded `shouldBe`
                [ ActivityUpdated
                    "Connection lost mid-response (Codex connection limit reached); reconnecting in 0s (attempt 1)…"
                , ResponseAttemptDiscarded
                , ActivityUpdated "Reconnecting to Codex (attempt 1)…"
                ]

        it "keeps a streamed apply_patch preview across reconnects" do
            attempts <- newIORef (0 :: Int)
            transcript <- newIORef []
            events <- newIORef []
            let patch = "*** Begin Patch\n"
                send _request _previous onEvent = do
                    modifyIORef' attempts (+ 1)
                    attempt <- readIORef attempts
                    if attempt == 1
                        then do
                            onEvent ResponseOutputItemAddedEvent
                                { item =
                                    customCallItem
                                        "patch-1"
                                        "apply_patch"
                                        ""
                                , outputIndex = Just 0
                                , sequenceNumber = Nothing
                                }
                            onEvent ResponseCustomToolInputDeltaEvent
                                { delta = Just patch
                                , streamItemId = Nothing
                                , streamCallId = Just "patch-1"
                                , streamOutputIndex = Just 0
                                , sequenceNumber = Nothing
                                }
                            pure (Left (ConnectionError "socket closed"))
                        else pure (Right
                            (testResponse "resp-replayed" [assistantItem "ok"]))
                backend = openAiBackendWithRetryPolicies
                    (constantDelay 0 <> limitRetries 3)
                    (constantDelay 0 <> limitRetries 5)
                    send
                    (pure baseParams)
            result <- submitWithState transcript backend Nothing [UserMessage "one"]
                (modifyIORef' events . (:))
            result `shouldBe`
                Right (emptyTurnOutput "resp-replayed" [] (Just "ok"))
            readIORef attempts `shouldReturn` 2
            recorded <- reverse <$> readIORef events
            let previews =
                    [call | ToolArgumentsUpdated call <- recorded]
            previews `shouldBe`
                [customToolCall "patch-1" "apply_patch" patch]
            length [() | ResponseRestarted _ <- recorded] `shouldBe` 1
            [() | ResponseAttemptDiscarded <- recorded] `shouldBe` []

        it "reports the transport failure once the reconnect policy is exhausted" do
            attempts <- newIORef (0 :: Int)
            transcript <- newIORef []
            events <- newIORef []
            let send _request _previous onEvent = do
                    modifyIORef' attempts (+ 1)
                    onEvent (deltaEvent EventOutputTextDelta "partial")
                    pure (Left (ConnectionError "socket closed"))
                backend = openAiBackendWithRetryPolicies
                    (constantDelay 0 <> limitRetries 3)
                    (constantDelay 0 <> limitRetries 2)
                    send
                    (pure baseParams)
            result <- submitWithState transcript backend Nothing [UserMessage "one"]
                (modifyIORef' events . (:))
            -- Not replay-unsafe: the transport fallback may still replay it.
            result `shouldBe` Left (ConnectionError "socket closed")
            readIORef attempts `shouldReturn` 3
            recorded <- reverse <$> readIORef events
            length (filter (== TextDelta "partial") recorded) `shouldBe` 3
            length [() | ResponseRestarted _ <- recorded] `shouldBe` 2
            last recorded `shouldBe` TextDelta "partial"

        it "does not reconnect before any output streamed" do
            attempts <- newIORef (0 :: Int)
            transcript <- newIORef []
            let send _request _previous _onEvent = do
                    modifyIORef' attempts (+ 1)
                    pure (Left (ConnectionError "socket closed"))
                backend = openAiBackendWithRetryPolicies
                    (constantDelay 0 <> limitRetries 3)
                    (constantDelay 0 <> limitRetries 5)
                    send
                    (pure baseParams)
            result <- submitWithState transcript backend Nothing [UserMessage "one"]
                (const (pure ()))
            -- Pre-output failures belong to the connection-recovery sender and
            -- the transport fallback, which replay without a boundary.
            result `shouldBe` Left (ConnectionError "socket closed")
            readIORef attempts `shouldReturn` 1
