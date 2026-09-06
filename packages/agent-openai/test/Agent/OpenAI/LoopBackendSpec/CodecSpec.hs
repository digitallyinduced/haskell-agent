module Agent.OpenAI.LoopBackendSpec.CodecSpec (spec) where

import Agent.InterAgentMessage
import Agent.Loop
import Agent.OpenAI.LoopBackend
import Agent.Responses.LoopBackend
    ( responseNeedsLoopContinuation
    , streamOutputObserved
    )
import Agent.Responses.Types
import Agent.ToolDispatch
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import Test.Hspec
import Agent.OpenAI.LoopBackendSpec.Fixtures

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

isInputFile :: ResponseContentPart -> Bool
isInputFile = \case
    InputFilePart{} -> True
    _ -> False

isInputImage :: ResponseContentPart -> Bool
isInputImage = \case
    InputImagePart{} -> True
    _ -> False
