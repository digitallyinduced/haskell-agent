module Agent.OpenAI.LoopBackendSpec (spec) where

import Agent.Error
    ( ApiError(..)
    , ErrorType(..)
    , isInlineRetryableProviderError
    )
import Agent.InterAgentMessage
import Agent.Loop
import Agent.OpenAI.LoopBackend
import Agent.Responses.LoopBackend (streamOutputObserved)
import Agent.Responses.Types
import Agent.ToolDispatch
import Control.Concurrent.Async (wait, withAsync)
import Control.Concurrent.MVar
    ( newEmptyMVar
    , putMVar
    , readMVar
    , takeMVar
    )
import Control.Retry (constantDelay, limitRetries)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as Text
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = do
    describe "withRequestInput" do
        it "repairs legacy assistant input_text from compacted sessions" do
            let legacySummary = MessageItem ResponseMessage
                    { messageId = Nothing
                    , content = MessageContentParts
                        [InputTextPart "Compacted conversation summary:\nold" Nothing KeyMap.empty]
                    , role = RoleAssistant
                    , status = Nothing
                    , phase = Nothing
                    , extraFields = KeyMap.empty
                    }
                request = withRequestInput baseParams [legacySummary]
            inputItems request `shouldBe`
                [ MessageItem ResponseMessage
                    { messageId = Nothing
                    , content = MessageContentParts
                        [OutputTextPart
                            "Compacted conversation summary:\nold"
                            Nothing
                            Nothing
                            KeyMap.empty]
                    , role = RoleAssistant
                    , status = Nothing
                    , phase = Nothing
                    , extraFields = KeyMap.empty
                    }
                ]

    describe "turnInputsToItems" do
        it "encodes a user message as RoleUser input_text" do
            case turnInputsToItems [UserMessage "hello"] of
                [MessageItem message] -> do
                    message.role `shouldBe` RoleUser
                    message.content `shouldBe`
                        MessageContentParts [InputTextPart "hello" Nothing KeyMap.empty]
                other -> expectationFailure ("expected one user message, got " <> show other)

        it "preserves encrypted collaboration payloads as agent_message content" do
            let message = InterAgentMessage
                    { messageAuthor = "/root"
                    , messageRecipient = "/root/worker"
                    , messageType = NewTaskMessage
                    , messageContent = EncryptedInterAgentContent "gAAAAA-ciphertext"
                    }
            case turnInputsToItems [AgentMessage message] of
                [item] -> Aeson.toJSON item `shouldBe` Aeson.object
                    [ "type" Aeson..= ("agent_message" :: Text)
                    , "author" Aeson..= ("/root" :: Text)
                    , "recipient" Aeson..= ("/root/worker" :: Text)
                    , "content" Aeson..=
                        [ Aeson.object
                            [ "type" Aeson..= ("input_text" :: Text)
                            , "text" Aeson..=
                                ("Message Type: NEW_TASK\n\
                                \Task name: /root/worker\n\
                                \Sender: /root\n\
                                \Payload:\n" :: Text)
                            ]
                        , Aeson.object
                            [ "type" Aeson..= ("encrypted_content" :: Text)
                            , "encrypted_content" Aeson..=
                                ("gAAAAA-ciphertext" :: Text)
                            ]
                        ]
                    ]
                other ->
                    expectationFailure ("expected one agent_message, got " <> show other)

        it "encodes multimodal turns as input_text plus input_image data URLs" do
            let image = ImageAttachment "image/png" "png-bytes"
            case turnInputsToItems
                    [UserMultimodal "see this" [image]] of
                [MessageItem message] -> do
                    message.role `shouldBe` RoleUser
                    case message.content of
                        MessageContentParts
                            [ InputTextPart text Nothing _
                            , InputImagePart
                                { detail
                                , fileId
                                , imageUrl
                                }
                            ] -> do
                            text `shouldBe` "see this"
                            detail `shouldBe` Just "auto"
                            fileId `shouldBe` Nothing
                            imageUrl `shouldBe`
                                Just "data:image/png;base64,cG5nLWJ5dGVz"
                        other ->
                            expectationFailure
                                ("expected text+image parts, got " <> show other)
                other ->
                    expectationFailure ("expected one user message, got " <> show other)

        it "encodes function results as function_call_output strings" do
            let items = turnInputsToItems
                    [CompletedTool (functionResult "c1" "echoed")]
            case items of
                [FunctionCallOutputItem output] -> do
                    output.callId `shouldBe` "c1"
                    output.output `shouldBe` Aeson.String "echoed"
                    itemType output `shouldBe` "function_call_output"
                other -> expectationFailure ("expected function output, got " <> show other)

        it "encodes custom results as custom_tool_call_output strings" do
            let items = turnInputsToItems
                    [CompletedTool (customResult "c2" "patched")]
            case items of
                [CustomToolCallOutputItem output] -> do
                    output.callId `shouldBe` "c2"
                    output.output `shouldBe` Aeson.String "patched"
                    itemType output `shouldBe` "custom_tool_call_output"
                other -> expectationFailure ("expected custom output, got " <> show other)

    describe "responseToTurnOutput" do
        it "collects function and custom tool calls and assistant text" do
            let turn = responseToTurnOutput $ testResponse "resp-9"
                    [ functionCallItem "fc1" "shell_command" "{\"command\":\"ls\"}"
                    , assistantItem "working"
                    , customCallItem "cc1" "apply_patch" "*** Begin Patch\n*** End Patch"
                    ]
            turn `shouldBe` emptyTurnOutput "resp-9"
                [ functionToolCall "fc1" "shell_command" "{\"command\":\"ls\"}"
                , customToolCall "cc1" "apply_patch" "*** Begin Patch\n*** End Patch"
                ]
                (Just "working")

        it "marks encrypted collaboration arguments and honors plaintext override" do
            let encrypted = responseToTurnOutput $ testResponse "resp-encrypted"
                    [functionCallItemWithExtras "fc1" "spawn_agent"
                        "{\"task_name\":\"worker\",\"message\":\"gAAAAA\"}"
                        (KeyMap.fromList
                            [(Key.fromText "namespace", Aeson.String "collaboration")])]
                plaintext = responseToTurnOutput $ testResponse "resp-plaintext"
                    [functionCallItemWithExtras "fc2" "spawn_agent"
                        "{\"task_name\":\"worker\",\"message\":\"hello\"}"
                        (KeyMap.fromList
                            [ (Key.fromText "namespace", Aeson.String "collaboration")
                            , (Key.fromText "encrypted_function_args", Aeson.Array mempty)
                            ])]
            map (.argumentsEncrypted) encrypted.toolCalls `shouldBe` [True]
            map (.argumentsEncrypted) plaintext.toolCalls `shouldBe` [False]

        it "joins multiple assistant messages" do
            let turn = responseToTurnOutput $ testResponse "resp-text"
                    [ assistantItem "first"
                    , functionCallItem "fc1" "echo" "{}"
                    , assistantItem "second"
                    ]
            turn.assistantText `shouldBe` Just "first\nsecond"

        it "copies provider usage including cached input tokens" do
            let turn = responseToTurnOutput $ testResponseWithUsage "resp-u"
                    [assistantItem "ok"]
                    (Aeson.object
                        [ "input_tokens" Aeson..= (120 :: Int)
                        , "output_tokens" Aeson..= (30 :: Int)
                        , "total_tokens" Aeson..= (150 :: Int)
                        , "input_tokens_details" Aeson..= Aeson.object
                            [ "cached_tokens" Aeson..= (80 :: Int)
                            ]
                        ])
            turn.tokenUsage `shouldBe` TokenUsage
                { inputTokens = 120
                , outputTokens = 30
                , cachedTokens = 80
                }

    describe "streamEventToLoopEvent" do
        it "maps output_text.delta and reasoning deltas" do
            streamEventToLoopEvent (deltaEvent EventOutputTextDelta "hello")
                `shouldBe` Just (TextDelta "hello")
            streamEventToLoopEvent (deltaEvent EventReasoningTextDelta "think")
                `shouldBe` Just (ReasoningDelta "think")
            streamEventToLoopEvent (deltaEvent EventReasoningSummaryTextDelta "sum")
                `shouldBe` Just (ReasoningDelta "sum")

        it "surfaces unknown provider events as visible activity warnings" do
            streamEventToLoopEvent
                (OtherResponseStreamEvent
                    { otherEventType =
                        StreamEventUnknown "response.future.done"
                    , sequenceNumber = Just 42
                    , eventExtraFields = KeyMap.empty
                    })
                `shouldBe`
                    Just
                        (ActivityUpdated
                            "Warning: unsupported provider event response.future.done")

        it "surfaces low Codex usage as a persistent warning" do
            streamEventToLoopEvent
                (codexRateLimitsEvent $ Aeson.object
                    [ "allowed" Aeson..= True
                    , "limit_reached" Aeson..= False
                    , "primary" Aeson..= Aeson.object
                        [ "used_percent" Aeson..= (91.5 :: Double)
                        , "window_minutes" Aeson..= (300 :: Int)
                        ]
                    , "secondary" Aeson..= Aeson.object
                        [ "used_percent" Aeson..= (93 :: Int)
                        , "window_minutes" Aeson..= (10080 :: Int)
                        ]
                    ])
                `shouldBe`
                    Just
                        (WarningRaised
                            "Codex usage is low: primary 8.5% left · secondary 7% left. Check /usage for reset details.")

        it "surfaces reached or disallowed Codex usage without ending the stream" do
            streamEventToLoopEvent
                (codexRateLimitsEvent $ Aeson.object
                    [ "allowed" Aeson..= False
                    , "limit_reached" Aeson..= True
                    ])
                `shouldBe`
                    Just
                        (WarningRaised
                            "Codex usage limit reached. Check /usage for reset details.")

        it "silently consumes healthy and malformed Codex usage snapshots" do
            streamEventToLoopEvent
                (codexRateLimitsEvent $ Aeson.object
                    [ "primary" Aeson..= Aeson.object
                        [ "used_percent" Aeson..= (42 :: Int)
                        ]
                    ])
                `shouldBe` Nothing
            streamEventToLoopEvent
                (OtherResponseStreamEvent
                    { otherEventType = EventCodexRateLimits
                    , sequenceNumber = Nothing
                    , eventExtraFields =
                        KeyMap.singleton "rate_limits" (Aeson.String "invalid")
                    })
                `shouldBe` Nothing

        it "ignores empty deltas and unrelated events" do
            streamEventToLoopEvent (deltaEvent EventOutputTextDelta "")
                `shouldBe` Nothing
            streamEventToLoopEvent (deltaEvent EventOutputTextDone "done")
                `shouldBe` Nothing
            streamEventToLoopEvent
                (OtherResponseStreamEvent
                    { otherEventType = EventCodexResponseMetadata
                    , sequenceNumber = Nothing
                    , eventExtraFields = KeyMap.singleton
                        "metadata"
                        (Aeson.object ["request_id" Aeson..= ("req-1" :: Text)])
                    })
                `shouldBe` Nothing
            streamEventToLoopEvent (ResponseOutputItemDoneEvent
                { item = assistantItem "x"
                , outputIndex = Just 0
                , sequenceNumber = Nothing
                , eventExtraFields = KeyMap.empty
                }) `shouldBe` Nothing

    describe "streamOutputObserved" do
        it "treats terminal lifecycle events as replay-unsafe" do
            streamOutputObserved
                (ResponseCompletedEvent (Aeson.object []) Nothing KeyMap.empty)
                `shouldBe` True
            streamOutputObserved
                (ResponseDoneEvent (Aeson.object []) Nothing KeyMap.empty)
                `shouldBe` True
            streamOutputObserved
                (ResponseIncompleteEvent (Aeson.object []) Nothing KeyMap.empty)
                `shouldBe` True

        it "treats failed lifecycle events as output only when output is present" do
            streamOutputObserved
                (ResponseFailedEvent
                    (Aeson.object ["output" Aeson..= [Aeson.object []]])
                    Nothing
                    KeyMap.empty)
                `shouldBe` True
            streamOutputObserved
                (ResponseFailedEvent (Aeson.object []) Nothing KeyMap.empty)
                `shouldBe` False

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
            map inputItems requests `shouldBe`
                [ turnInputsToItems [UserMessage "read it"]
                , turnInputsToItems [UserMessage "read it"]
                    <> [functionCallItem "c1" "read_file"
                        "{\"target_file\":\"README.md\"}"]
                    <> turnInputsToItems
                        [CompletedTool (functionResult "c1" "file contents")]
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
                , turnInputsToItems [UserMessage "one"]
                    <> [assistantItem "one"]
                    <> turnInputsToItems [UserMessage "two"]
                ]

    describe "openAiBackendWith" do
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

    describe "openAiBackendWithConnectionRecovery" do
        it "serializes a reusable-socket failure across concurrent senders" do
            currentCalls <- newIORef (0 :: Int)
            freshCalls <- newIORef (0 :: Int)
            firstCurrentStarted <- newEmptyMVar
            laterCurrentStarted <- newEmptyMVar
            releaseCurrent <- newEmptyMVar
            secondSenderStarted <- newEmptyMVar
            healthy <- newConnectionHealth True
            let connectionFailure = ConnectionError "socket closed"
                response = testResponse "resp-fresh" [assistantItem "ok"]
                sendCurrent _request _previous _onEvent = do
                    callNumber <- atomicModifyIORef' currentCalls \n ->
                        let next = n + 1
                        in (next, next)
                    if callNumber == 1
                        then putMVar firstCurrentStarted ()
                        else putMVar laterCurrentStarted ()
                    readMVar releaseCurrent
                    pure (Left connectionFailure)
                sendFresh _failure _request _previous _onEvent = do
                    modifyIORef' freshCalls (+ 1)
                    pure (Right response)
                sender =
                    openAiAuxiliaryResponseSenderWithConnectionRecovery
                        healthy
                        sendCurrent
                        sendFresh
            withAsync
                (sender baseParams Nothing (const (pure ())))
                \firstSender -> do
                    takeMVar firstCurrentStarted
                    withAsync
                        ( putMVar secondSenderStarted ()
                            >> sender baseParams Nothing (const (pure ()))
                        )
                        \secondSender -> do
                            takeMVar secondSenderStarted
                            overlappingCurrent <- timeout
                                concurrencyProbeMicros
                                (takeMVar laterCurrentStarted)
                            overlappingCurrent `shouldBe` Nothing
                            putMVar releaseCurrent ()
                            firstResult <- wait firstSender
                            secondResult <- wait secondSender
                            firstResult `shouldBe` Right response
                            secondResult `shouldBe` Right response
            readConnectionHealth healthy `shouldReturn` False
            readIORef currentCalls `shouldReturn` 1
            readIORef freshCalls `shouldReturn` 2

        it "marks a throwing reusable connection unhealthy and releases ownership" do
            currentCalls <- newIORef (0 :: Int)
            freshCalls <- newIORef (0 :: Int)
            healthy <- newConnectionHealth True
            let response = testResponse "resp-fresh" [assistantItem "ok"]
                sendCurrent _request _previous _onEvent = do
                    modifyIORef' currentCalls (+ 1)
                    ioError (userError "socket read failed")
                sendFresh _failure _request _previous _onEvent = do
                    modifyIORef' freshCalls (+ 1)
                    pure (Right response)
                sender =
                    openAiAuxiliaryResponseSenderWithConnectionRecovery
                        healthy
                        sendCurrent
                        sendFresh
            sender baseParams Nothing (const (pure ()))
                `shouldThrow` anyIOException
            readConnectionHealth healthy `shouldReturn` False
            sender baseParams Nothing (const (pure ()))
                `shouldReturn` Right response
            readIORef currentCalls `shouldReturn` 1
            readIORef freshCalls `shouldReturn` 1

        it "keeps auxiliary requests on the healthy reusable connection" do
            currentCalls <- newIORef (0 :: Int)
            freshCalls <- newIORef (0 :: Int)
            healthy <- newConnectionHealth True
            let response = testResponse "resp-current" [assistantItem "ok"]
                sendCurrent _request _previous _onEvent = do
                    modifyIORef' currentCalls (+ 1)
                    pure (Right response)
                sendFresh _failure _request _previous _onEvent = do
                    modifyIORef' freshCalls (+ 1)
                    pure (Right (testResponse "resp-fresh" [assistantItem "no"]))
                sender =
                    openAiAuxiliaryResponseSenderWithConnectionRecovery
                        healthy
                        sendCurrent
                        sendFresh
            sender baseParams Nothing (const (pure ()))
                `shouldReturn` Right response
            readConnectionHealth healthy `shouldReturn` True
            readIORef currentCalls `shouldReturn` 1
            readIORef freshCalls `shouldReturn` 0

        it "exposes the same current/fresh recovery to auxiliary requests" do
            currentCalls <- newIORef (0 :: Int)
            freshCalls <- newIORef (0 :: Int)
            failures <- newIORef []
            healthy <- newConnectionHealth True
            let connectionFailure = ConnectionError "socket closed"
            let sendCurrent _request _previous _onEvent = do
                    modifyIORef' currentCalls (+ 1)
                    pure (Left connectionFailure)
                sendFresh failure _request _previous _onEvent = do
                    modifyIORef' freshCalls (+ 1)
                    modifyIORef' failures (<> [failure])
                    pure $ Right (testResponse "resp-fresh" [assistantItem "ok"])
                sender =
                    openAiAuxiliaryResponseSenderWithConnectionRecovery
                        healthy
                        sendCurrent
                        sendFresh
            first <- sender baseParams Nothing (const (pure ()))
            second <- sender baseParams Nothing (const (pure ()))
            first `shouldBe`
                Right (testResponse "resp-fresh" [assistantItem "ok"])
            second `shouldBe`
                Right (testResponse "resp-fresh" [assistantItem "ok"])
            readConnectionHealth healthy `shouldReturn` False
            readIORef currentCalls `shouldReturn` 1
            readIORef freshCalls `shouldReturn` 2
            readIORef failures `shouldReturn`
                [Just connectionFailure, Nothing]

        it "does not replay auxiliary requests after an output item arrived" do
            freshCalls <- newIORef (0 :: Int)
            healthy <- newConnectionHealth True
            let connectionFailure = ConnectionError "socket closed"
                outputEvent = ResponseOutputItemDoneEvent
                    { item = assistantItem "partial"
                    , outputIndex = Just 0
                    , sequenceNumber = Nothing
                    , eventExtraFields = KeyMap.empty
                    }
                sendCurrent _request _previous onEvent = do
                    onEvent outputEvent
                    pure (Left connectionFailure)
                sendFresh _failure _request _previous _onEvent = do
                    modifyIORef' freshCalls (+ 1)
                    pure $ Right (testResponse "resp-fresh" [assistantItem "ok"])
                sender =
                    openAiAuxiliaryResponseSenderWithConnectionRecovery
                        healthy
                        sendCurrent
                        sendFresh
            sender baseParams Nothing (const (pure ()))
                `shouldReturn` Left
                    (replayUnsafeAuxiliaryFailure connectionFailure)
            readConnectionHealth healthy `shouldReturn` False
            readIORef freshCalls `shouldReturn` 0

        it "does not replay after a terminal auxiliary response arrived" do
            freshCalls <- newIORef (0 :: Int)
            healthy <- newConnectionHealth True
            let connectionFailure = ConnectionError "decode failed"
                completedEvent = ResponseCompletedEvent
                    { responseValue = Aeson.toJSON
                        (testResponse "resp-completed" [assistantItem "done"])
                    , sequenceNumber = Nothing
                    , eventExtraFields = KeyMap.empty
                    }
                sendCurrent _request _previous onEvent = do
                    onEvent completedEvent
                    pure (Left connectionFailure)
                sendFresh _failure _request _previous _onEvent = do
                    modifyIORef' freshCalls (+ 1)
                    pure $ Right
                        (testResponse "resp-fresh" [assistantItem "duplicate"])
                sender =
                    openAiAuxiliaryResponseSenderWithConnectionRecovery
                        healthy
                        sendCurrent
                        sendFresh
            sender baseParams Nothing (const (pure ()))
                `shouldReturn` Left
                    (replayUnsafeAuxiliaryFailure connectionFailure)
            readIORef freshCalls `shouldReturn` 0

        it "does not replay a failed auxiliary response with partial output" do
            freshCalls <- newIORef (0 :: Int)
            healthy <- newConnectionHealth True
            let connectionFailure = ConnectionError "failed response"
                failedEvent = ResponseFailedEvent
                    { responseValue = Aeson.toJSON
                        (testResponse "resp-failed" [assistantItem "partial"])
                    , sequenceNumber = Nothing
                    , eventExtraFields = KeyMap.empty
                    }
                sendCurrent _request _previous onEvent = do
                    onEvent failedEvent
                    pure (Left connectionFailure)
                sendFresh _failure _request _previous _onEvent = do
                    modifyIORef' freshCalls (+ 1)
                    pure $ Right
                        (testResponse "resp-fresh" [assistantItem "duplicate"])
                sender =
                    openAiAuxiliaryResponseSenderWithConnectionRecovery
                        healthy
                        sendCurrent
                        sendFresh
            sender baseParams Nothing (const (pure ()))
                `shouldReturn` Left
                    (replayUnsafeAuxiliaryFailure connectionFailure)
            readIORef freshCalls `shouldReturn` 0

        it "marks output from an initially fresh auxiliary connection as replay-unsafe" do
            currentCalls <- newIORef (0 :: Int)
            freshCalls <- newIORef (0 :: Int)
            healthy <- newConnectionHealth False
            let connectionFailure = ConnectionError "fresh socket closed"
                outputEvent = ResponseOutputItemDoneEvent
                    { item = assistantItem "partial"
                    , outputIndex = Just 0
                    , sequenceNumber = Nothing
                    , eventExtraFields = KeyMap.empty
                    }
                sendCurrent _request _previous _onEvent = do
                    modifyIORef' currentCalls (+ 1)
                    pure (Left (ConnectionError "unexpected current request"))
                sendFresh _failure _request _previous onEvent = do
                    modifyIORef' freshCalls (+ 1)
                    onEvent outputEvent
                    pure (Left connectionFailure)
                sender =
                    openAiAuxiliaryResponseSenderWithConnectionRecovery
                        healthy
                        sendCurrent
                        sendFresh
            sender baseParams Nothing (const (pure ()))
                `shouldReturn` Left
                    (replayUnsafeAuxiliaryFailure connectionFailure)
            readIORef currentCalls `shouldReturn` 0
            readIORef freshCalls `shouldReturn` 1

        it "marks fresh auxiliary output after a pre-output current failure" do
            freshCalls <- newIORef (0 :: Int)
            healthy <- newConnectionHealth True
            let currentFailure = ConnectionError "current socket closed"
                freshFailure = ConnectionError "fresh socket closed"
                outputEvent = ResponseOutputItemDoneEvent
                    { item = assistantItem "partial"
                    , outputIndex = Just 0
                    , sequenceNumber = Nothing
                    , eventExtraFields = KeyMap.empty
                    }
                sendCurrent _request _previous _onEvent =
                    pure (Left currentFailure)
                sendFresh failure _request _previous onEvent = do
                    failure `shouldBe` Just currentFailure
                    modifyIORef' freshCalls (+ 1)
                    onEvent outputEvent
                    pure (Left freshFailure)
                sender =
                    openAiAuxiliaryResponseSenderWithConnectionRecovery
                        healthy
                        sendCurrent
                        sendFresh
            sender baseParams Nothing (const (pure ()))
                `shouldReturn` Left
                    (replayUnsafeAuxiliaryFailure freshFailure)
            readConnectionHealth healthy `shouldReturn` False
            readIORef freshCalls `shouldReturn` 1

        it "replays on a fresh connection when the reusable socket dies before output" do
            currentCalls <- newIORef (0 :: Int)
            freshCalls <- newIORef (0 :: Int)
            healthy <- newConnectionHealth True
            transcript <- newIORef []
            let sendCurrent _request _previous _onEvent = do
                    modifyIORef' currentCalls (+ 1)
                    pure $ Left $ ConnectionError "socket closed"
                sendFresh _failure _request _previous _onEvent = do
                    modifyIORef' freshCalls (+ 1)
                    pure $ Right (testResponse "resp-fresh" [assistantItem "ok"])
                backend = openAiBackendWithConnectionRecovery
                    healthy sendCurrent sendFresh (pure baseParams)
            first <- submitWithState transcript backend Nothing [UserMessage "one"] (const (pure ()))
            second <- submitWithState transcript backend (Just "resp-fresh")
                [UserMessage "two"] (const (pure ()))
            first `shouldBe` Right (emptyTurnOutput "resp-fresh" [] (Just "ok"))
            second `shouldBe` Right (emptyTurnOutput "resp-fresh" [] (Just "ok"))
            readConnectionHealth healthy `shouldReturn` False
            readIORef currentCalls `shouldReturn` 1
            readIORef freshCalls `shouldReturn` 2

        it "still replays after an informational Codex rate-limit warning" do
            freshCalls <- newIORef (0 :: Int)
            healthy <- newConnectionHealth True
            transcript <- newIORef []
            events <- newIORef []
            let rateLimitsEvent =
                    codexRateLimitsEvent $ Aeson.object
                        [ "primary" Aeson..= Aeson.object
                            [ "used_percent" Aeson..= (92 :: Int)
                            ]
                        ]
                sendCurrent _request _previous onEvent = do
                    onEvent rateLimitsEvent
                    pure $ Left $ ConnectionError "socket closed"
                sendFresh _failure _request _previous _onEvent = do
                    modifyIORef' freshCalls (+ 1)
                    pure $ Right (testResponse "resp-fresh" [assistantItem "ok"])
                backend = openAiBackendWithConnectionRecovery
                    healthy sendCurrent sendFresh (pure baseParams)
            result <- submitWithState transcript backend Nothing [UserMessage "one"]
                (modifyIORef' events . (:))
            result `shouldBe` Right (emptyTurnOutput "resp-fresh" [] (Just "ok"))
            reverse <$> readIORef events `shouldReturn`
                [ WarningRaised
                    "Codex usage is low: primary 8% left. Check /usage for reset details."
                ]
            readConnectionHealth healthy `shouldReturn` False
            readIORef freshCalls `shouldReturn` 1

        it "does not replay after loop-visible output was already streamed" do
            freshCalls <- newIORef (0 :: Int)
            healthy <- newConnectionHealth True
            transcript <- newIORef []
            events <- newIORef []
            let sendCurrent _request _previous onEvent = do
                    onEvent (deltaEvent EventOutputTextDelta "partial")
                    pure $ Left $ ConnectionError "socket closed"
                sendFresh _failure _request _previous _onEvent = do
                    modifyIORef' freshCalls (+ 1)
                    pure $ Right (testResponse "resp-fresh" [assistantItem "ok"])
                streamingBackend = openAiBackendWithConnectionRecovery
                    healthy sendCurrent sendFresh (pure baseParams)
            result <- submitWithState transcript streamingBackend Nothing [UserMessage "one"]
                (modifyIORef' events . (:))
            result `shouldBe` Left (ConnectionError "socket closed")
            reverse <$> readIORef events `shouldReturn` [TextDelta "partial"]
            readConnectionHealth healthy `shouldReturn` False
            readIORef freshCalls `shouldReturn` 0

        it "does not treat provider errors as a dead connection" do
            freshCalls <- newIORef (0 :: Int)
            healthy <- newConnectionHealth True
            transcript <- newIORef []
            let sendCurrent _request _previous _onEvent =
                    pure $ Left $ ProviderError InvalidRequestError "bad request" Nothing
                sendFresh _failure _request _previous _onEvent = do
                    modifyIORef' freshCalls (+ 1)
                    pure $ Right (testResponse "resp-fresh" [assistantItem "ok"])
                providerErrorBackend = openAiBackendWithConnectionRecovery
                    healthy sendCurrent sendFresh (pure baseParams)
            result <- submitWithState transcript providerErrorBackend Nothing
                [UserMessage "one"] (const (pure ()))
            result `shouldBe` Left (ProviderError InvalidRequestError "bad request" Nothing)
            readConnectionHealth healthy `shouldReturn` True
            readIORef freshCalls `shouldReturn` 0

        it "reconnects immediately after a websocket connection-limit error" do
            currentCalls <- newIORef (0 :: Int)
            freshCalls <- newIORef (0 :: Int)
            healthy <- newConnectionHealth True
            transcript <- newIORef []
            let sendCurrent _request _previous _onEvent = do
                    modifyIORef' currentCalls (+ 1)
                    pure $ Left $ ProviderError WebSocketConnectionLimitReached
                        "too many websocket connections" Nothing
                sendFresh _failure _request _previous _onEvent = do
                    modifyIORef' freshCalls (+ 1)
                    pure $ Right (testResponse "resp-fresh" [assistantItem "ok"])
                backend = openAiBackendWithConnectionRecovery
                    healthy sendCurrent sendFresh (pure baseParams)
            result <- submitWithState transcript backend Nothing
                [UserMessage "one"] (const (pure ()))
            result `shouldBe` Right (emptyTurnOutput "resp-fresh" [] (Just "ok"))
            readConnectionHealth healthy `shouldReturn` False
            readIORef currentCalls `shouldReturn` 1
            readIORef freshCalls `shouldReturn` 1

        it "reacquires a credential after an in-band usage-limit error" do
            freshCalls <- newIORef (0 :: Int)
            healthy <- newConnectionHealth True
            transcript <- newIORef []
            let sendCurrent _request _previous _onEvent =
                    pure $ Left $ ProviderError UsageLimitReached
                        "usage limit reached" (Just 120)
                sendFresh _failure _request _previous _onEvent = do
                    modifyIORef' freshCalls (+ 1)
                    pure $ Right (testResponse "resp-fresh" [assistantItem "ok"])
                backend = openAiBackendWithConnectionRecovery
                    healthy sendCurrent sendFresh (pure baseParams)
            result <- submitWithState transcript backend Nothing
                [UserMessage "one"] (const (pure ()))
            result `shouldBe` Right (emptyTurnOutput "resp-fresh" [] (Just "ok"))
            readConnectionHealth healthy `shouldReturn` False
            readIORef freshCalls `shouldReturn` 1

        it "reports the exhausted reusable connection before replaying on a fresh account" do
            healthy <- newConnectionHealth True
            transcript <- newIORef (turnInputsToItems [UserMessage "old"])
            failures <- newIORef []
            seen <- newIORef []
            let exhausted = ProviderError UsageLimitReached
                    "usage limit reached" (Just 120)
                sendCurrent _request _previous _onEvent =
                    pure (Left exhausted)
                sendFresh failure request previous _onEvent = do
                    modifyIORef' failures (<> [failure])
                    modifyIORef' seen (<> [(inputItems request, previous)])
                    case previous of
                        Just _ ->
                            pure $ Left $ ProviderError PreviousResponseNotFound
                                "previous_response_id belongs to another account"
                                Nothing
                        Nothing ->
                            pure $ Right $
                                testResponse "resp-fresh" [assistantItem "ok"]
                backend = openAiBackendWithConnectionRecovery
                    healthy sendCurrent sendFresh (pure baseParams)
            result <- submitWithState transcript backend (Just "resp-old")
                [UserMessage "new"] (const (pure ()))
            result `shouldBe`
                Right (emptyTurnOutput "resp-fresh" [] (Just "ok"))
            readIORef failures `shouldReturn` [Just exhausted, Nothing]
            readIORef seen `shouldReturn`
                [ (turnInputsToItems [UserMessage "new"], Just "resp-old")
                , ( turnInputsToItems [UserMessage "old"]
                        <> turnInputsToItems [UserMessage "new"]
                  , Nothing
                  )
                ]

    describe "openAiResponseSenderWithRetryPolicy" do
        it "retries a pre-output fresh connection failure" $
            shouldRetryFreshAuxiliaryFailure
                (ConnectionError "fresh socket closed")

        it "retries a pre-output fresh websocket-limit failure" $
            shouldRetryFreshAuxiliaryFailure
                (ProviderError WebSocketConnectionLimitReached
                    "too many websocket connections"
                    Nothing)

        it "returns the final pre-output failure after exhausting retries" do
            attempts <- newIORef (0 :: Int)
            let failure = ConnectionError "still offline"
                send _request _previous _onEvent = do
                    modifyIORef' attempts (+ 1)
                    pure (Left failure)
                sender =
                    openAiResponseSenderWithRetryPolicy
                        (constantDelay 0 <> limitRetries 1)
                        isAuxiliaryOutputEvent
                        send
            sender baseParams Nothing (const (pure ()))
                `shouldReturn` Left failure
            readIORef attempts `shouldReturn` 2

        it "marks every post-output auxiliary failure as non-retryable" do
            mapM_ shouldMarkPostOutputAuxiliaryFailure
                [ ConnectionError "socket closed"
                , ProviderError WebSocketConnectionLimitReached
                    "too many websocket connections"
                    Nothing
                , ProviderError OverloadedError "busy" Nothing
                , ProviderError ServiceUnavailableError "unavailable" Nothing
                , ProviderError ApiErrorType "server error" Nothing
                , ProviderError UsageLimitReached "usage exhausted" (Just 120)
                , HttpError 503 "unavailable"
                ]

    describe "openAiBackendWithTransportFallback" do
        it "allows only one concurrent primary failure before fallback activation" do
            fallbackActive <- newTransportFallbackState False
            primaryCalls <- newIORef (0 :: Int)
            fallbackCalls <- newIORef (0 :: Int)
            firstPrimaryStarted <- newEmptyMVar
            laterPrimaryStarted <- newEmptyMVar
            releasePrimary <- newEmptyMVar
            secondTurnStarted <- newEmptyMVar
            firstTranscript <- newIORef []
            secondTranscript <- newIORef []
            let primary = Backend \_state _previous _inputs _onEvent -> do
                    callNumber <- atomicModifyIORef' primaryCalls \n ->
                        let next = n + 1
                        in (next, next)
                    if callNumber == 1
                        then putMVar firstPrimaryStarted ()
                        else putMVar laterPrimaryStarted ()
                    readMVar releasePrimary
                    pure (Left (ConnectionError "socket closed"))
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
                submit transcript message =
                    submitWithState transcript backend Nothing
                        [UserMessage message] (const (pure ()))
                expected =
                    Right (emptyTurnOutput "resp-http" [] (Just "ok"))
            withAsync (submit firstTranscript "one") \firstTurn -> do
                takeMVar firstPrimaryStarted
                withAsync
                    (putMVar secondTurnStarted () >> submit secondTranscript "two")
                    \secondTurn -> do
                        takeMVar secondTurnStarted
                        overlappingPrimary <- timeout
                            concurrencyProbeMicros
                            (takeMVar laterPrimaryStarted)
                        overlappingPrimary `shouldBe` Nothing
                        putMVar releasePrimary ()
                        firstResult <- wait firstTurn
                        secondResult <- wait secondTurn
                        firstResult `shouldBe` expected
                        secondResult `shouldBe` expected
            readTransportFallbackState fallbackActive `shouldReturn` True
            readIORef primaryCalls `shouldReturn` 1
            readIORef fallbackCalls `shouldReturn` 2

        it "keeps fallback active when the fallback backend throws" do
            fallbackActive <- newTransportFallbackState False
            primaryCalls <- newIORef (0 :: Int)
            fallbackCalls <- newIORef (0 :: Int)
            transcript <- newIORef []
            let primary = Backend \_state _previous _inputs _onEvent -> do
                    modifyIORef' primaryCalls (+ 1)
                    pure (Left (ConnectionError "socket closed"))
                fallback = Backend \state _previous _inputs _onEvent -> do
                    callNumber <- atomicModifyIORef' fallbackCalls \n ->
                        let next = n + 1
                        in (next, next)
                    if callNumber == 1
                        then ioError (userError "fallback failed")
                        else pure $ Right BackendResult
                            { backendOutput =
                                emptyTurnOutput "resp-http" [] (Just "ok")
                            , backendState = state
                            }
                backend =
                    openAiBackendWithTransportFallback
                        fallbackActive primary fallback
                submit =
                    submitWithState transcript backend Nothing
                        [UserMessage "one"] (const (pure ()))
            submit `shouldThrow` anyIOException
            readTransportFallbackState fallbackActive `shouldReturn` True
            submit `shouldReturn`
                Right (emptyTurnOutput "resp-http" [] (Just "ok"))
            readIORef primaryCalls `shouldReturn` 1
            readIORef fallbackCalls `shouldReturn` 2

        it "switches permanently to fallback after a pre-output connection error" do
            fallbackActive <- newTransportFallbackState False
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
            readTransportFallbackState fallbackActive `shouldReturn` True
            readIORef primaryCalls `shouldReturn` 1
            readIORef fallbackCalls `shouldReturn` 2

        it "does not replay a failed turn after model output was exposed" do
            fallbackActive <- newTransportFallbackState False
            fallbackCalls <- newIORef (0 :: Int)
            transcript <- newIORef []
            let primary = Backend \_state _previous _inputs onEvent -> do
                    onEvent (TextDelta "partial")
                    pure (Left (ConnectionError "socket closed"))
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
            result `shouldBe` Left (ConnectionError "socket closed")
            readTransportFallbackState fallbackActive `shouldReturn` True
            readIORef fallbackCalls `shouldReturn` 0

        it "falls back immediately after a websocket connection-limit error" do
            fallbackActive <- newTransportFallbackState False
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

        it "preserves non-transport provider failures" do
            fallbackActive <- newTransportFallbackState False
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
            readTransportFallbackState fallbackActive `shouldReturn` False
            readIORef fallbackCalls `shouldReturn` 0

--------------------------------------------------------------------------------
-- Fixtures
--------------------------------------------------------------------------------

concurrencyProbeMicros :: Int
concurrencyProbeMicros = 100_000

submitWithState
    :: IORef [ResponseItem]
    -> Backend
    -> Maybe Text
    -> [TurnInput]
    -> (LoopEvent -> IO ())
    -> IO (Either ApiError TurnOutput)
submitWithState stateRef backend previous inputs onEvent = do
    state <- readIORef stateRef
    result <- backend.submitTurn state previous inputs onEvent
    case result of
        Left err -> pure (Left err)
        Right BackendResult{..} -> do
            writeIORef stateRef backendState
            pure (Right backendOutput)

baseParams :: ResponseCreateParams
baseParams = withModel (Just "gpt-5.6-luna") defaultResponseCreateParams

withModel :: Maybe Text -> ResponseCreateParams -> ResponseCreateParams
withModel nextModel ResponseCreateParams { model = _, .. } =
    ResponseCreateParams { model = nextModel, .. }

withPromptCacheRetention
    :: Maybe Text -> ResponseCreateParams -> ResponseCreateParams
withPromptCacheRetention nextRetention
        ResponseCreateParams { promptCacheRetention = _, .. } =
    ResponseCreateParams { promptCacheRetention = nextRetention, .. }

withEffort :: Text -> ResponseCreateParams -> ResponseCreateParams
withEffort effort ResponseCreateParams { reasoning = _, .. } =
    ResponseCreateParams
        { reasoning = Just ReasoningConfig
            { context = Nothing
            , effort = Just effort
            , generateSummary = Nothing
            , reasoningMode = Nothing
            , summary = Nothing
            , extraFields = KeyMap.empty
            }
        , ..
        }

reasoningEffort :: ResponseCreateParams -> Maybe Text
reasoningEffort request = request.reasoning >>= (.effort)

recordingSend
    :: IORef [(ResponseCreateParams, Maybe Text)]
    -> ResponseCreateParams
    -> Maybe Text
    -> (ResponseStreamEvent -> IO ())
    -> IO (Either ApiError Response)
recordingSend seen request previous onEvent = do
    modifyIORef' seen (++ [(request, previous)])
    onEvent (deltaEvent EventOutputTextDelta "ok")
    pure $ Right (testResponse "resp-1" [assistantItem "ok"])

scriptedStatelessSend
    :: IORef [ResponseCreateParams]
    -> IORef [Response]
    -> ResponseCreateParams
    -> (ResponseStreamEvent -> IO ())
    -> IO (Either ApiError Response)
scriptedStatelessSend seen remaining request onEvent = do
    modifyIORef' seen (++ [request])
    next <- atomicModifyIORef' remaining \case
        [] -> ([], Nothing)
        response : rest -> (rest, Just response)
    case next of
        Nothing -> pure (Left (ConnectionError "scripted backend exhausted"))
        Just response -> do
            onEvent (deltaEvent EventOutputTextDelta "call")
            pure (Right response)

functionResult :: Text -> Text -> ToolCallResult
functionResult callId output = ToolCallResult
    { callId
    , output
    , callKind = FunctionCallKind
    }

customResult :: Text -> Text -> ToolCallResult
customResult callId output = ToolCallResult
    { callId
    , output
    , callKind = CustomCallKind
    }

functionCallItem :: Text -> Text -> Text -> ResponseItem
functionCallItem callId name arguments =
    functionCallItemWithExtras callId name arguments KeyMap.empty

functionCallItemWithExtras
    :: Text
    -> Text
    -> Text
    -> Aeson.Object
    -> ResponseItem
functionCallItemWithExtras callId name arguments extraFields =
    FunctionCallItem FunctionCall
    { itemId = Nothing
    , callId
    , name
    , arguments
    , status = Just ItemCompleted
    , extraFields
    }

customCallItem :: Text -> Text -> Text -> ResponseItem
customCallItem callId name input = CustomToolCallItem CustomToolCall
    { itemId = Nothing
    , callId
    , name
    , input
    , status = Just ItemCompleted
    , extraFields = KeyMap.empty
    }

assistantItem :: Text -> ResponseItem
assistantItem text = MessageItem ResponseMessage
    { messageId = Nothing
    , content = MessageContentParts [OutputTextPart text Nothing Nothing KeyMap.empty]
    , role = RoleAssistant
    , status = Just ItemCompleted
    , phase = Nothing
    , extraFields = KeyMap.empty
    }

compactionItem :: Text -> ResponseItem
compactionItem _ = KnownResponseItem ItemCompaction TaggedObject
    { tag = "compaction"
    , fields = KeyMap.empty
    }

deltaEvent :: StreamEventType -> Text -> ResponseStreamEvent
deltaEvent otherEventType delta = OtherResponseStreamEvent
    { otherEventType
    , sequenceNumber = Nothing
    , eventExtraFields = KeyMap.fromList [(Key.fromText "delta", Aeson.String delta)]
    }

codexRateLimitsEvent :: Aeson.Value -> ResponseStreamEvent
codexRateLimitsEvent rateLimits = OtherResponseStreamEvent
    { otherEventType = EventCodexRateLimits
    , sequenceNumber = Nothing
    , eventExtraFields = KeyMap.singleton "rate_limits" rateLimits
    }

testResponse :: Text -> [ResponseItem] -> Response
testResponse responseId output = testResponseWithUsage responseId output Aeson.Null

testResponseWithUsage :: Text -> [ResponseItem] -> Aeson.Value -> Response
testResponseWithUsage responseId output usage = case Aeson.fromJSON $ Aeson.object $
    [ "id" Aeson..= responseId
    , "created_at" Aeson..= (0 :: Int)
    , "model" Aeson..= ("test-model" :: Text)
    , "status" Aeson..= ("completed" :: Text)
    , "output" Aeson..= output
    ] <> usageField
  of
    Aeson.Success response -> response
    Aeson.Error err -> error err
  where
    usageField = case usage of
        Aeson.Null -> []
        value -> ["usage" Aeson..= value]

itemType :: Aeson.ToJSON a => a -> Text
itemType value = case Aeson.toJSON value of
    Aeson.Object object -> case KeyMap.lookup "type" object of
        Just (Aeson.String tag) -> tag
        _ -> ""
    _ -> ""

inputItems :: ResponseCreateParams -> [ResponseItem]
inputItems request = case request.input of
    Just (ResponseInputItems items) -> items
    _ -> []

shouldRetryFreshAuxiliaryFailure :: ApiError -> Expectation
shouldRetryFreshAuxiliaryFailure initialFailure = do
    healthy <- newConnectionHealth False
    currentCalls <- newIORef (0 :: Int)
    freshCalls <- newIORef (0 :: Int)
    let response = testResponse "resp-retried" [assistantItem "ok"]
        sendCurrent _request _previous _onEvent = do
            modifyIORef' currentCalls (+ 1)
            pure (Left (ConnectionError "unexpected reusable request"))
        sendFresh _failure _request _previous _onEvent = do
            attempt <- atomicModifyIORef' freshCalls \count ->
                let next = count + 1
                in (next, next)
            pure $
                if attempt == 1
                    then Left initialFailure
                    else Right response
        reconnecting =
            openAiAuxiliaryResponseSenderWithConnectionRecovery
                healthy
                sendCurrent
                sendFresh
        sender =
            openAiResponseSenderWithRetryPolicy
                (constantDelay 0 <> limitRetries 1)
                isAuxiliaryOutputEvent
                reconnecting
    sender baseParams Nothing (const (pure ()))
        `shouldReturn` Right response
    readIORef currentCalls `shouldReturn` 0
    readIORef freshCalls `shouldReturn` 2

shouldMarkPostOutputAuxiliaryFailure :: ApiError -> Expectation
shouldMarkPostOutputAuxiliaryFailure failure = do
    healthy <- newConnectionHealth True
    currentCalls <- newIORef (0 :: Int)
    freshCalls <- newIORef (0 :: Int)
    let outputEvent = ResponseOutputItemDoneEvent
            { item = assistantItem "partial"
            , outputIndex = Just 0
            , sequenceNumber = Nothing
            , eventExtraFields = KeyMap.empty
            }
        sendCurrent _request _previous onEvent = do
            modifyIORef' currentCalls (+ 1)
            onEvent outputEvent
            pure (Left failure)
        sendFresh _failure _request _previous _onEvent = do
            modifyIORef' freshCalls (+ 1)
            pure $ Right (testResponse "resp-fresh" [assistantItem "duplicate"])
        sender =
            openAiAuxiliaryResponseSenderWithConnectionRecovery
                healthy
                sendCurrent
                sendFresh
        expected = replayUnsafeAuxiliaryFailure failure
    sender baseParams Nothing (const (pure ()))
        `shouldReturn` Left expected
    isInlineRetryableProviderError expected `shouldBe` False
    readIORef currentCalls `shouldReturn` 1
    readIORef freshCalls `shouldReturn` 0

isAuxiliaryOutputEvent :: ResponseStreamEvent -> Bool
isAuxiliaryOutputEvent = streamOutputObserved

replayUnsafeAuxiliaryFailure :: ApiError -> ApiError
replayUnsafeAuxiliaryFailure failure =
    ProviderError (UnknownErrorType "replay_unsafe")
        ( "provider failed after auxiliary response output; refusing to replay: "
            <> Text.pack (show failure)
        )
        Nothing
