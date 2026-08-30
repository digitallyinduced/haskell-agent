module Agent.OpenAI.LoopBackendSpec (spec) where

import Agent.Cancel (newCancelFlag)
import Agent.Error
    ( ApiError(..)
    , ErrorType(..)
    , isInlineRetryableProviderError
    )
import qualified Agent.Responses.Codec as ResponsesCodec
import Agent.InterAgentMessage
import Agent.Loop
import Agent.OpenAI.LoopBackend
import Agent.OpenAI.WebSocketClient
    ( newCodexTurnState
    , readCodexTurnState
    , recordCodexTurnState
    )
import Agent.Responses.LoopBackend
    ( responseNeedsLoopContinuation
    , streamOutputObserved
    )
import Agent.Responses.Request (stripReplayedItemStatus)
import Agent.Responses.Types
import Agent.ToolDispatch
import Agent.Tools.FileSystem.ReadFile (readFileToolWithSpeculation)
import Agent.Tools.FileSystem.ReadFileSpeculation
    ( newReadFileSpeculation
    , waitForReadFileSpeculation
    )
import Agent.Tools.Speculation
    ( ToolSpeculationRuntime
    , closeToolSpeculationRuntime
    , newToolSpeculationRuntime
    , observeToolArgumentEvent
    , retainToolSpeculation
    , takeToolSpeculation
    , waitForToolSpeculation
    )
import Agent.Tools.Types (ToolRegistry, defaultToolEnv, mkToolRegistry)
import Control.Exception (bracket)
import Control.Retry (constantDelay, limitRetries)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.OsPath (unsafeEncodeUtf)
import Test.Hspec

spec :: Spec
spec = do
    describe "withRequestInput" do
        it "repairs legacy assistant input_text from compacted sessions" do
            let legacySummary = MessageItem ResponseMessage
                    { messageId = Nothing
                    , content = MessageContentParts
                        [InputTextPart "Compacted conversation summary:\nold" Nothing]
                    , role = RoleAssistant
                    , status = Nothing
                    , phase = Nothing
                    , passthrough = Nothing
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
                            ]
                    , role = RoleAssistant
                    , status = Nothing
                    , phase = Nothing
                    , passthrough = Nothing
                    }
                ]

    describe "turnInputsToItems" do
        it "encodes a user message as RoleUser input_text" do
            case turnInputsToItems [UserMessage "hello"] of
                [MessageItem message] -> do
                    message.role `shouldBe` RoleUser
                    message.content `shouldBe`
                        MessageContentParts [InputTextPart "hello" Nothing]
                other -> expectationFailure ("expected one user message, got " <> show other)

        it "preserves encrypted collaboration payloads as agent_message content" do
            let message = InterAgentMessage
                    { messageAuthor = "/root"
                    , messageRecipient = "/root/worker"
                    , messageType = NewTaskMessage
                    , messageContent = EncryptedInterAgentContent "gAAAAA-ciphertext"
                    }
            case turnInputsToItems [AgentMessage message] of
                [item@(AgentMessageItem encoded)] -> do
                    encoded.author `shouldBe` Just "/root"
                    encoded.recipient `shouldBe` Just "/root/worker"
                    encoded.content `shouldBe`
                        [ InputTextPart
                            "Message Type: NEW_TASK\n\
                            \Task name: /root/worker\n\
                            \Sender: /root\n\
                            \Payload:\n"
                            Nothing
                        , EncryptedContentPart "gAAAAA-ciphertext"
                        ]
                    Aeson.toJSON item `shouldBe` Aeson.object
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
                    [ userMessageWithAttachments
                        "see this"
                        [ImageAttachmentItem image]
                    ] of
                [MessageItem message] -> do
                    message.role `shouldBe` RoleUser
                    case message.content of
                        MessageContentParts
                            [ InputTextPart text Nothing
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

        it "encodes multimodal files as input_image plus input_file parts" do
            let image = ImageAttachment "image/png" "png-bytes"
                file = FileAttachment (Just "notes.txt") "text/plain" "file-bytes"
            case turnInputsToItems
                    [ userMessageWithAttachments
                        "see this"
                        [ ImageAttachmentItem image
                        , FileAttachmentItem file
                        ]
                    ] of
                [MessageItem message] -> do
                    message.role `shouldBe` RoleUser
                    case message.content of
                        MessageContentParts parts ->
                            parts `shouldSatisfy` \ps ->
                                any isInputFile ps && any isInputImage ps
                        other ->
                            expectationFailure
                                ("expected text+image+file parts, got " <> show other)
                other ->
                    expectationFailure ("expected one user message, got " <> show other)

        it "encodes function results as function_call_output strings" do
            let items = turnInputsToItems
                    [CompletedTool (functionResult "c1" "echoed")]
            case items of
                [FunctionCallOutputItem output] -> do
                    output.callId `shouldBe` "c1"
                    Aeson.toJSON output.output `shouldBe` Aeson.String "echoed"
                    itemType output `shouldBe` "function_call_output"
                other -> expectationFailure ("expected function output, got " <> show other)

        it "encodes custom results as custom_tool_call_output strings" do
            let items = turnInputsToItems
                    [CompletedTool (customResult "c2" "patched")]
            case items of
                [CustomToolCallOutputItem output] -> do
                    output.callId `shouldBe` "c2"
                    Aeson.toJSON output.output `shouldBe` Aeson.String "patched"
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
                worktree = responseToTurnOutput $ testResponse "resp-worktree"
                    [functionCallItemWithExtras "fc3" "spawn_agent_in_worktree"
                        "{\"task_name\":\"worker\",\"message\":\"gAAAAA\"}"
                        (KeyMap.fromList
                            [(Key.fromText "namespace", Aeson.String "collaboration")])]
            map (.argumentsEncrypted) encrypted.toolCalls `shouldBe` [True]
            map (.argumentsEncrypted) plaintext.toolCalls `shouldBe` [False]
            map (.argumentsEncrypted) worktree.toolCalls `shouldBe` [False]

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

        it "preserves incomplete reason and hidden reasoning usage" do
            let response =
                    (testResponseWithUsage "resp-incomplete" []
                        (Aeson.object
                            [ "input_tokens" Aeson..= (120 :: Int)
                            , "output_tokens" Aeson..= (32768 :: Int)
                            , "total_tokens" Aeson..= (32888 :: Int)
                            , "output_tokens_details" Aeson..= Aeson.object
                                [ "reasoning_tokens" Aeson..= (32000 :: Int)
                                ]
                            ]))
                        { status = ResponseIncomplete
                        , incompleteDetails = Just IncompleteDetails
                            { reason = "max_output_tokens"
                            }
                        }
                turn = responseToTurnOutput response
            turn.completion `shouldBe` TurnIncomplete
                { incompleteReason = "max_output_tokens"
                , incompleteReasoningTokens = Just 32000
                }
            turn.tokenUsage.outputTokens `shouldBe` 32768

        it "continues reasoning-only max-output stops on the response chain" do
            let turn = responseToTurnOutput reasoningIncompleteResponse
            turn.completion `shouldBe` TurnCompleted
            turn.toolCalls `shouldBe` []
            turn.assistantText `shouldBe` Nothing
            turn.tokenUsage.outputTokens `shouldBe` 128000

        it "continues only successful empty response steps" do
            let cancelled = testResponse "resp-cancelled" []
            responseNeedsLoopContinuation reasoningIncompleteResponse
                `shouldBe` True
            responseNeedsLoopContinuation (testResponse "resp-empty" [])
                `shouldBe` True
            responseNeedsLoopContinuation
                (cancelled
                    { status = ResponseCancelled
                    , incompleteDetails = Nothing
                    })
                `shouldBe` False

        it "keeps filtered or partially actionable incomplete output terminal" do
            let incomplete reason output =
                    (testResponse "resp-incomplete" output)
                        { status = ResponseIncomplete
                        , incompleteDetails = Just IncompleteDetails
                            { reason
                            }
                        }
                filtered = responseToTurnOutput
                    (incomplete "content_filter" [reasoningItem "rs-filtered"])
                partialCall = responseToTurnOutput
                    (incomplete "max_output_tokens"
                        [ reasoningItem "rs-partial"
                        , functionCallItem "call-partial" "echo" "{}"
                        ])
            filtered.completion `shouldBe` TurnIncomplete
                { incompleteReason = "content_filter"
                , incompleteReasoningTokens = Nothing
                }
            partialCall.completion `shouldBe` TurnIncomplete
                { incompleteReason = "max_output_tokens"
                , incompleteReasoningTokens = Nothing
                }

    describe "streamEventToLoopEvent" do
        it "maps output and summary deltas but hides raw reasoning" do
            streamEventToLoopEvent (deltaEvent EventOutputTextDelta "hello")
                `shouldBe` Just (TextDelta "hello")
            streamEventToLoopEvent (deltaEvent EventReasoningTextDelta "think")
                `shouldBe` Nothing
            streamEventToLoopEvent (deltaEvent EventReasoningSummaryTextDelta "sum")
                `shouldBe` Just (ReasoningDelta "sum")

        it "surfaces unknown provider events as visible activity warnings" do
            streamEventToLoopEvent
                (OtherResponseStreamEvent
                    { otherEventType =
                        StreamEventUnknown "response.future.done"
                    , sequenceNumber = Just 42
                    , eventDelta = Nothing
                    , streamItemId = Nothing
                    , streamOutputIndex = Nothing
                    , summaryIndex = Nothing
                    , turnState = Nothing
                    })
                `shouldBe`
                    Just
                        (ActivityUpdated
                            "Warning: unsupported provider event response.future.done")
            streamEventToLoopEvent
                (OtherResponseStreamEvent
                    { otherEventType =
                        StreamEventUnknown "response.future.done"
                    , sequenceNumber = Just 42
                    , eventDelta = Just "partial"
                    , streamItemId = Nothing
                    , streamOutputIndex = Nothing
                    , summaryIndex = Nothing
                    , turnState = Nothing
                    })
                `shouldBe`
                    Just
                        (ActivityUpdated
                            "Warning: unsupported provider event response.future.done")

        it "surfaces unparsed websocket frames as warnings without marking output" do
            let event = OtherResponseStreamEvent
                    { otherEventType =
                        StreamEventUnknown unparsedStreamEventTypeText
                    , sequenceNumber = Nothing
                    , eventDelta =
                        Just "key \"call_id\" not found: {\"type\":\"response.output_item.added\"}"
                    , streamItemId = Nothing
                    , streamOutputIndex = Nothing
                    , summaryIndex = Nothing
                    , turnState = Nothing
                    }
            streamEventToLoopEvent event
                `shouldBe`
                    Just (ActivityUpdated
                        ("Warning: unsupported provider event "
                            <> unparsedStreamEventTypeText))
            streamOutputObserved event `shouldBe` False

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
                    , eventDelta = Nothing
                    , streamItemId = Nothing
                    , streamOutputIndex = Nothing
                    , summaryIndex = Nothing
                    , turnState = Nothing
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
                    , eventDelta = Nothing
                    , streamItemId = Nothing
                    , streamOutputIndex = Nothing
                    , summaryIndex = Nothing
                    , turnState = Nothing
                    })
                `shouldBe` Nothing
            streamEventToLoopEvent (ResponseOutputItemDoneEvent
                { item = assistantItem "x"
                , outputIndex = Just 0
                , sequenceNumber = Nothing
                }) `shouldBe` Nothing

    describe "streamOutputObserved" do
        it "treats terminal lifecycle events as replay-unsafe" do
            streamOutputObserved
                (ResponseCompletedEvent (testResponse "completed" []) Nothing)
                `shouldBe` True
            streamOutputObserved
                (ResponseDoneEvent (testResponse "done" []) Nothing)
                `shouldBe` True
            streamOutputObserved
                (ResponseIncompleteEvent (testResponse "incomplete" []) Nothing)
                `shouldBe` False
            streamOutputObserved
                (ResponseIncompleteEvent
                    (testResponse "incomplete-output" [assistantItem "x"])
                    Nothing)
                `shouldBe` True

        it "treats failed lifecycle events as output only when output is present" do
            streamOutputObserved
                (ResponseFailedEvent
                    (testResponse "failed-output" [assistantItem "x"])
                    Nothing)
                `shouldBe` True
            streamOutputObserved
                (ResponseFailedEvent (testResponse "failed" []) Nothing)
                `shouldBe` False

        it "treats function-call argument events as replay-unsafe" do
            streamOutputObserved
                ResponseFunctionCallArgumentsDeltaEvent
                    { delta = Just "{}"
                    , streamItemId = Just "fc_1"
                    , streamOutputIndex = Just 0
                    , sequenceNumber = Nothing
                    }
                `shouldBe` True
            streamOutputObserved
                ResponseFunctionCallArgumentsDoneEvent
                    { arguments = Just "{}"
                    , functionName = Just "read_file"
                    , streamItemId = Just "fc_1"
                    , streamOutputIndex = Just 0
                    , sequenceNumber = Nothing
                    }
                `shouldBe` True

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

        it "retains read_file work started from streamed argument deltas" do
            withSystemTempDirectory "openai-read-speculation" \dir -> do
                Text.writeFile (dir </> "streamed.txt") "streamed"
                env <- defaultToolEnv (unsafeEncodeUtf dir)
                cache <- newReadFileSpeculation env
                bracket
                    (newToolSpeculationRuntime
                        [readFileToolWithSpeculation env (Just cache)])
                    closeToolSpeculationRuntime
                    \runtime -> do
                        transcript <- newIORef []
                        let callId = "call-streamed"
                            arguments =
                                "{\"target_file\":\"streamed.txt\"}"
                            addedCall =
                                FunctionCallItem FunctionCall
                                    { itemId = Nothing
                                    , callId
                                    , name = "read_file"
                                    , namespace = Nothing
                                    , arguments = ""
                                    , encryptedFunctionArgs = Nothing
                                    , status = Nothing
                                    , provider = Nothing
                                    }
                            finalCall =
                                functionCallItem
                                    callId
                                    "read_file"
                                    arguments
                            send _request _previous onEvent = do
                                onEvent ResponseOutputItemAddedEvent
                                    { item = addedCall
                                    , outputIndex = Just 0
                                    , sequenceNumber = Nothing
                                    }
                                onEvent ResponseFunctionCallArgumentsDeltaEvent
                                    { delta = Just arguments
                                    , streamItemId = Nothing
                                    , streamOutputIndex = Just 0
                                    , sequenceNumber = Nothing
                                    }
                                waitForToolSpeculation runtime
                                waitForReadFileSpeculation cache
                                pure $ Right $
                                    testResponse "resp-streamed" [finalCall]
                            backend =
                                openAiBackendWith
                                    send
                                    (pure baseParams)
                        result <- submitWithState
                            transcript
                            backend
                            Nothing
                            [UserMessage "read it"]
                            (observeArguments runtime)
                        result `shouldBe`
                            Right
                                (emptyTurnOutput
                                    "resp-streamed"
                                    [ functionToolCall
                                        callId
                                        "read_file"
                                        arguments
                                    ]
                                    Nothing)
                        retainToolSpeculation runtime
                            [ functionToolCall callId "read_file" arguments ]
                        takeToolSpeculation
                            runtime
                            (functionToolCall
                                callId
                                "read_file"
                                arguments)
                            `shouldReturn` Just (Right "1→streamed")

        it "binds arguments.done speculation from the final response item" do
            withSystemTempDirectory "openai-read-done-speculation" \dir -> do
                Text.writeFile (dir </> "done.txt") "done"
                env <- defaultToolEnv (unsafeEncodeUtf dir)
                cache <- newReadFileSpeculation env
                bracket
                    (newToolSpeculationRuntime
                        [readFileToolWithSpeculation env (Just cache)])
                    closeToolSpeculationRuntime
                    \runtime -> do
                        transcript <- newIORef []
                        let itemId = "item-done"
                            callId = "call-done"
                            arguments =
                                "{\"target_file\":\"done.txt\"}"
                            finalCall =
                                FunctionCallItem FunctionCall
                                    { itemId = Just itemId
                                    , callId
                                    , name = "read_file"
                                    , namespace = Nothing
                                    , arguments
                                    , encryptedFunctionArgs = Nothing
                                    , status = Just ItemCompleted
                                    , provider = Nothing
                                    }
                            send _request _previous onEvent = do
                                onEvent ResponseFunctionCallArgumentsDoneEvent
                                    { arguments = Just arguments
                                    , functionName = Just "read_file"
                                    , streamItemId = Just itemId
                                    , streamOutputIndex = Nothing
                                    , sequenceNumber = Nothing
                                    }
                                waitForToolSpeculation runtime
                                waitForReadFileSpeculation cache
                                pure $ Right $
                                    testResponse "resp-done" [finalCall]
                            backend =
                                openAiBackendWith
                                    send
                                    (pure baseParams)
                            call =
                                functionToolCall
                                    callId
                                    "read_file"
                                    arguments
                        result <- submitWithState
                            transcript
                            backend
                            Nothing
                            [UserMessage "read it"]
                            (observeArguments runtime)
                        result `shouldBe`
                            Right
                                (emptyTurnOutput
                                    "resp-done"
                                    [call]
                                    Nothing)
                        retainToolSpeculation runtime [call]
                        takeToolSpeculation runtime call
                            `shouldReturn` Just (Right "1→done")


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

        it "blocks credential failover after hidden output was streamed" do
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
            filter (not . isToolStartedEvent) recorded `shouldBe`
                [ shellArgumentsStarted
                , ActivityUpdated "Writing shell call…"
                ]

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

        it "discards a hidden partial attempt before reconnecting" do
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
                [ shellArgumentsStarted
                , ActivityUpdated "Writing shell call…"
                , ActivityUpdated
                    "Connection lost mid-response (Codex connection limit reached); reconnecting in 0s (attempt 1)…"
                , ResponseAttemptDiscarded
                , ActivityUpdated "Reconnecting to Codex (attempt 1)…"
                ]

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

    describe "openAiBackendWithConnectionRecovery" do
        it "keeps auxiliary requests on the healthy reusable connection" do
            currentCalls <- newIORef (0 :: Int)
            freshCalls <- newIORef (0 :: Int)
            healthy <- newIORef True
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
            readIORef healthy `shouldReturn` True
            readIORef currentCalls `shouldReturn` 1
            readIORef freshCalls `shouldReturn` 0

        it "exposes the same current/fresh recovery to auxiliary requests" do
            currentCalls <- newIORef (0 :: Int)
            freshCalls <- newIORef (0 :: Int)
            failures <- newIORef []
            healthy <- newIORef True
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
            readIORef healthy `shouldReturn` False
            readIORef currentCalls `shouldReturn` 1
            readIORef freshCalls `shouldReturn` 2
            readIORef failures `shouldReturn`
                [Just connectionFailure, Nothing]

        it "does not replay auxiliary requests after an output item arrived" do
            freshCalls <- newIORef (0 :: Int)
            healthy <- newIORef True
            let connectionFailure = ConnectionError "socket closed"
                outputEvent = ResponseOutputItemDoneEvent
                    { item = assistantItem "partial"
                    , outputIndex = Just 0
                    , sequenceNumber = Nothing
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
            readIORef healthy `shouldReturn` False
            readIORef freshCalls `shouldReturn` 0

        it "does not replay after a terminal auxiliary response arrived" do
            freshCalls <- newIORef (0 :: Int)
            healthy <- newIORef True
            let connectionFailure = ConnectionError "decode failed"
                completedEvent = ResponseCompletedEvent
                    { responseValue =
                        testResponse "resp-completed" [assistantItem "done"]
                    , sequenceNumber = Nothing
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
            healthy <- newIORef True
            let connectionFailure = ConnectionError "failed response"
                failedEvent = ResponseFailedEvent
                    { responseValue =
                        testResponse "resp-failed" [assistantItem "partial"]
                    , sequenceNumber = Nothing
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
            healthy <- newIORef False
            let connectionFailure = ConnectionError "fresh socket closed"
                outputEvent = ResponseOutputItemDoneEvent
                    { item = assistantItem "partial"
                    , outputIndex = Just 0
                    , sequenceNumber = Nothing
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
            healthy <- newIORef True
            let currentFailure = ConnectionError "current socket closed"
                freshFailure = ConnectionError "fresh socket closed"
                outputEvent = ResponseOutputItemDoneEvent
                    { item = assistantItem "partial"
                    , outputIndex = Just 0
                    , sequenceNumber = Nothing
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
            readIORef healthy `shouldReturn` False
            readIORef freshCalls `shouldReturn` 1

        it "replays on a fresh connection when the reusable socket dies before output" do
            currentCalls <- newIORef (0 :: Int)
            freshCalls <- newIORef (0 :: Int)
            healthy <- newIORef True
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
            readIORef healthy `shouldReturn` False
            readIORef currentCalls `shouldReturn` 1
            readIORef freshCalls `shouldReturn` 2

        it "still replays after an informational Codex rate-limit warning" do
            freshCalls <- newIORef (0 :: Int)
            healthy <- newIORef True
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
                , ProviderLimitUpdated
                    { providerLimitText = "5h limit left: 8%"
                    , providerLimitWarning = True
                    }
                ]
            readIORef healthy `shouldReturn` False
            readIORef freshCalls `shouldReturn` 1

        it "reconnects behind a restart boundary after loop-visible output streamed" do
            freshCalls <- newIORef (0 :: Int)
            healthy <- newIORef True
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
            -- The dead socket committed nothing, so the backend closes the
            -- partial attempt with a visible boundary and resubmits the same
            -- request; the sender then dials a fresh connection.
            result `shouldBe` Right (emptyTurnOutput "resp-fresh" [] (Just "ok"))
            reverse <$> readIORef events `shouldReturn`
                [ TextDelta "partial"
                , ActivityUpdated
                    "Connection lost mid-response (socket closed); reconnecting in 1s (attempt 1)…"
                , ResponseRestarted
                    "Connection interrupted the response; restarting automatically. The new attempt may repeat partial output shown above."
                , ActivityUpdated "Reconnecting to Codex (attempt 1)…"
                ]
            readIORef healthy `shouldReturn` False
            readIORef freshCalls `shouldReturn` 1

        it "does not treat provider errors as a dead connection" do
            freshCalls <- newIORef (0 :: Int)
            healthy <- newIORef True
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
            readIORef healthy `shouldReturn` True
            readIORef freshCalls `shouldReturn` 0

        it "reconnects immediately after a websocket connection-limit error" do
            currentCalls <- newIORef (0 :: Int)
            freshCalls <- newIORef (0 :: Int)
            healthy <- newIORef True
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
            readIORef healthy `shouldReturn` False
            readIORef currentCalls `shouldReturn` 1
            readIORef freshCalls `shouldReturn` 1

        it "reacquires a credential after an in-band usage-limit error" do
            freshCalls <- newIORef (0 :: Int)
            healthy <- newIORef True
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
            readIORef healthy `shouldReturn` False
            readIORef freshCalls `shouldReturn` 1

        it "reports the exhausted reusable connection before replaying on a fresh account" do
            healthy <- newIORef True
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

        it "retains WebSocket provenance without making replay safe" do
            let failure =
                    replayUnsafeAuxiliaryFailure
                        (ConnectionError
                            "WebSocket receive error: ParseException \"not enough bytes\"")
            isOpenAiReplayUnsafeWebSocketTransportFailure failure
                `shouldBe` True
            -- Callers use the ordinary predicate only when replaying the same
            -- logical request over HTTP is safe.
            isOpenAiWebSocketTransportFailure failure `shouldBe` False

        it "does not label post-output provider failures as transport failures" do
            let failure =
                    replayUnsafeAuxiliaryFailure
                        (ProviderError ApiErrorType "server error" Nothing)
            isOpenAiReplayUnsafeWebSocketTransportFailure failure
                `shouldBe` False

    describe "isOpenAiWebSocketTransportFailure" do
        it "recognizes an exact WebSocket handshake 403" do
            isOpenAiWebSocketTransportFailure
                (HttpError 403 "WebSocket handshake returned HTTP 403")
                `shouldBe` True

        it "does not hide application permission or authentication errors" do
            isOpenAiWebSocketTransportFailure
                (HttpError 403 "model access denied")
                `shouldBe` False
            isOpenAiWebSocketTransportFailure
                (HttpError 401 "WebSocket handshake returned HTTP 401")
                `shouldBe` False

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

--------------------------------------------------------------------------------
-- Fixtures
--------------------------------------------------------------------------------

observeArguments :: ToolSpeculationRuntime -> LoopEvent -> IO ()
observeArguments runtime = \case
    ToolArgumentEvent event -> observeToolArgumentEvent runtime event
    _ -> pure ()

isToolStartedEvent :: LoopEvent -> Bool
isToolStartedEvent = \case
    ToolStarted _ -> True
    _ -> False

shellArgumentsStarted :: LoopEvent
shellArgumentsStarted =
    ToolArgumentEvent ToolArgumentsStarted
        { argumentStreamRefs = [ToolCallStreamOutput 0]
        , argumentStreamCallId = "fc-1"
        , argumentStreamName = Just "shell"
        , argumentStreamArguments = "{}"
        }

submitWithState
    :: IORef [ResponseItem]
    -> Backend
    -> Maybe Text
    -> [TurnInput]
    -> (LoopEvent -> IO ())
    -> IO (Either ApiError TurnOutput)
submitWithState stateRef backend previous inputs onEvent = do
    state <- readIORef stateRef
    result <- backend.submitTurn
        (initialBackendSnapshot state) previous inputs onEvent
    case result of
        Left err -> pure (Left err)
        Right BackendResult{..} -> do
            writeIORef stateRef backendState.backendItems
            pure (Right backendOutput)

loopConfig :: Backend -> IO LoopConfig
loopConfig backend = do
    state <- newIORef emptyBackendSnapshot
    cancel <- newCancelFlag
    pure LoopConfig
        { loopBackend = backend
        , loopBackendState = BackendStateStore
            { readBackendState = readIORef state
            , commitBackendState = \snapshot -> do
                writeIORef state snapshot
                pure snapshot
            }
        , loopTools = emptyRegistry
        , loopDispatch = defaultLoopDispatch
        , loopMaxTurns = defaultLoopMaxTurns
        , loopOnEvent = const (pure ())
        , loopApprove = const (pure (Right True))
        , loopReadSteering = pure []
        , loopCommitSteering = const (pure ())
        , loopInterrupt = pure ()
        , loopCancel = cancel
        }

emptyRegistry :: ToolRegistry
emptyRegistry =
    either (error . Text.unpack) id (mkToolRegistry [])

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
functionCallItemWithExtras callId name arguments metadataFields =
    FunctionCallItem FunctionCall
    { itemId = Nothing
    , callId
    , name
    , namespace = case KeyMap.lookup "namespace" metadataFields of
        Just (Aeson.String value) -> Just value
        _ -> Nothing
    , provider = case KeyMap.lookup "provider" metadataFields of
        Just (Aeson.String value) -> Just value
        _ -> Nothing
    , arguments
    , encryptedFunctionArgs =
        case KeyMap.lookup "encrypted_function_args" metadataFields of
            Just (Aeson.Array _) -> Just []
            _ -> Nothing
    , status = Just ItemCompleted
    }

customCallItem :: Text -> Text -> Text -> ResponseItem
customCallItem callId name input = CustomToolCallItem CustomToolCall
    { itemId = Nothing
    , callId
    , name
    , namespace = Nothing
    , input
    , status = Just ItemCompleted
    }

assistantItem :: Text -> ResponseItem
assistantItem text = MessageItem ResponseMessage
    { messageId = Nothing
    , content = MessageContentParts [OutputTextPart text Nothing Nothing]
    , role = RoleAssistant
    , status = Just ItemCompleted
    , phase = Nothing
    , passthrough = Nothing
    }

reasoningItem :: Text -> ResponseItem
reasoningItem itemId = ReasoningItemValue ReasoningItem
    { itemId = Just itemId
    , summary = []
    , content = Nothing
    , encryptedContent = Just "opaque"
    , status = Just ItemCompleted
    }

reasoningIncompleteResponse :: Response
reasoningIncompleteResponse =
    (testResponseWithUsage
        "resp-reasoning-incomplete"
        [reasoningItem "rs-1"]
        (Aeson.object
            [ "input_tokens" Aeson..= (64000 :: Int)
            , "output_tokens" Aeson..= (128000 :: Int)
            , "total_tokens" Aeson..= (192000 :: Int)
            , "output_tokens_details" Aeson..= Aeson.object
                [ "reasoning_tokens" Aeson..= (53 :: Int)
                ]
            ]))
        { status = ResponseIncomplete
        , incompleteDetails = Just IncompleteDetails
            { reason = "max_output_tokens"
            }
        }

compactionItem :: Text -> ResponseItem
compactionItem _ = CompactionItemValue CompactionItem
    { itemId = Nothing
    , encryptedContent = Nothing
    }

deltaEvent :: StreamEventType -> Text -> ResponseStreamEvent
deltaEvent otherEventType delta = OtherResponseStreamEvent
    { otherEventType
    , sequenceNumber = Nothing
    , eventDelta = Just delta
    , streamItemId = Nothing
    , streamOutputIndex = Nothing
    , summaryIndex = Nothing
    , turnState = Nothing
    }

codexRateLimitsEvent :: Aeson.Value -> ResponseStreamEvent
codexRateLimitsEvent rateLimits = ResponseCodexRateLimitsEvent
    { rateLimits = CodexRateLimits
        { allowed = boolField "allowed" rateLimits
        , limitReached = boolField "limit_reached" rateLimits
        , primaryUsedPercent = percentField "primary" rateLimits
        , secondaryUsedPercent = percentField "secondary" rateLimits
        }
    , sequenceNumber = Nothing
    }
  where
    boolField name (Aeson.Object object) =
        case KeyMap.lookup name object of
            Just (Aeson.Bool value) -> Just value
            _ -> Nothing
    boolField _ _ = Nothing
    percentField name (Aeson.Object object) =
        case KeyMap.lookup name object of
            Just (Aeson.Object window) ->
                case KeyMap.lookup "used_percent" window of
                    Just (Aeson.Number value) -> Just (realToFrac value)
                    _ -> Nothing
            _ -> Nothing
    percentField _ _ = Nothing

testResponse :: Text -> [ResponseItem] -> Response
testResponse responseId output = testResponseWithUsage responseId output Aeson.Null

testResponseWithUsage :: Text -> [ResponseItem] -> Aeson.Value -> Response
testResponseWithUsage responseId output usage =
    case ResponsesCodec.decodeResponse
        (LBS.toStrict (Aeson.encode (Aeson.object $
    [ "id" Aeson..= responseId
    , "created_at" Aeson..= (0 :: Int)
    , "model" Aeson..= ("test-model" :: Text)
    , "status" Aeson..= ("completed" :: Text)
    , "output" Aeson..= output
    ] <> usageField))) of
        Right response -> response
        Left err -> error err
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
    healthy <- newIORef False
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
    healthy <- newIORef True
    currentCalls <- newIORef (0 :: Int)
    freshCalls <- newIORef (0 :: Int)
    let outputEvent = ResponseOutputItemDoneEvent
            { item = assistantItem "partial"
            , outputIndex = Just 0
            , sequenceNumber = Nothing
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
    ProviderError replayUnsafeType
        ( "provider failed after auxiliary response output; refusing to replay: "
            <> Text.pack (show failure)
        )
        Nothing
  where
    replayUnsafeType
        | isOpenAiWebSocketTransportFailure failure =
            UnknownErrorType "replay_unsafe_websocket_transport"
        | otherwise = UnknownErrorType "replay_unsafe"

isInputFile :: ResponseContentPart -> Bool
isInputFile = \case
    InputFilePart{} -> True
    _ -> False

isInputImage :: ResponseContentPart -> Bool
isInputImage = \case
    InputImagePart{} -> True
    _ -> False
