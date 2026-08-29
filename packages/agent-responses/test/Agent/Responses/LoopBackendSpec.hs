module Agent.Responses.LoopBackendSpec (spec) where

import Agent.Error (ApiError(..), ErrorType(..))
import Agent.Loop
    ( Backend(..)
    , FileAttachment(..)
    , ImageAttachment(..)
    , LoopEvent(..)
    , TurnInput(..)
    )
import Agent.Provider
    ( Credential(..)
    , BillingMode(..)
    , FailedCredential(..)
    , Provider(..)
    , tokenProvider
    )
import Agent.Json (rawJsonFromEncoding)
import Agent.Responses.LoopBackend
    ( newStreamEventToLoopEvents
    , statelessResponsesBackend
    , statelessResponsesBackendWithRawReasoning
    , tokenProviderStatelessResponsesBackend
    , turnInputsToItems
    , responseItemToToolCall
    , toolResultToItem
    , withRequestInput
    )
import Agent.Responses.Types
    ( MessageContent(..)
    , ComputerAction(..)
    , ComputerCall(..)
    , ComputerCallOutput(..)
    , CustomToolCall(..)
    , CustomToolCallOutput(..)
    , FunctionCall(..)
    , FunctionCallOutput(..)
    , InternalChatMetadata(..)
    , ItemStatus(..)
    , LocalShellCall(..)
    , ReasoningItem(..)
    , ResponseContentPart(..)
    , ResponseItem(..)
    , ResponseMessage(..)
    , ResponseRole(..)
    , ResponseStreamEvent(..)
    , StreamEventType(..)
    , ResponseInput(..)
    , ResponseCreateParams(..)
    , TaggedObject(..)
    , defaultResponseCreateParams
    )
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson.Key as Key
import Data.IORef
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Agent.ToolDispatch (ToolCall(..), ToolCallKind(..), ToolCallResult(..))
import Test.Hspec

spec :: Spec
spec = do
    backendSpec
    streamProjectionSpec

backendSpec :: Spec
backendSpec = describe "tokenProviderStatelessResponsesBackend" do
    it "round-trips native computer calls through structured screenshot output" do
        let call = ComputerCall
                { computerCallItemId = Just "item-1"
                , computerCallId = "call-1"
                , computerActions = [ClickAction 20 30 "left", TypeAction "secret"]
                , pendingSafetyChecks = []
                , computerCallStatus = Nothing
                , computerCallExtra = KeyMap.empty
                }
        case responseItemToToolCall (ComputerCallItem call) of
            Just projected -> do
                projected.callId `shouldBe` "call-1"
                projected.name `shouldBe` "computer"
                projected.callKind `shouldBe` ComputerCallKind
                projected.argumentsEncrypted `shouldBe` True
            Nothing -> expectationFailure "computer call was not projected"
        let outputValue = ComputerCallOutput
                { computerOutputItemId = Nothing
                , computerOutputCallId = "ignored"
                , screenshotDataUrl = "data:image/png;base64,AA=="
                , acknowledgedChecks = []
                , computerOutputStatus = Nothing
                , computerOutputExtra = KeyMap.empty
                }
            encodedJson = Aeson.toJSON outputValue
            encoded = TextEncoding.decodeUtf8 $ LBS.toStrict $ Aeson.encode
                outputValue
        (jsonField "output" encodedJson >>= jsonField "detail")
            `shouldBe` Just (Aeson.String "original")
        case toolResultToItem ToolCallResult
                { callId = "call-1"
                , output = encoded
                , callKind = ComputerCallKind
                } of
            ComputerCallOutputItem output -> do
                output.computerOutputCallId `shouldBe` "call-1"
                output.screenshotDataUrl `shouldBe` "data:image/png;base64,AA=="
            other -> expectationFailure ("unexpected output: " <> show other)
    it "encodes file attachments as input_file parts" do
        let image = ImageAttachment "image/png" "png-bytes"
            file = FileAttachment (Just "notes.txt") "text/plain" "file-bytes"
        case turnInputsToItems
                [UserMultimodalFiles "see this" [image] [file]] of
            [MessageItem message] -> do
                message.role `shouldBe` RoleUser
                case message.content of
                    MessageContentParts parts ->
                        parts `shouldSatisfy` \ps ->
                            any isInputFile ps && any isInputImage ps
                    _ -> expectationFailure "expected multimodal message parts"
            other -> expectationFailure ("unexpected items: " <> show other)

    it "reacquires a credential after an authentication rejection" do
        attempts <- newIORef []
        let first = credential "first"
            second = credential "second"
            provider = tokenProvider SubscriptionBilled \failed ->
                pure $ Right $ case failed of
                Nothing -> first
                Just FailedCredential{} -> second
            send current _params _onEvent = do
                modifyIORef' attempts (<> [current])
                pure $ Left $
                    if current == first
                        then ProviderError AuthenticationError "rejected" Nothing
                        else ConnectionError "stopped after failover"
            backend =
                tokenProviderStatelessResponsesBackend provider send
                    (pure defaultResponseCreateParams)

        result <- backend.submitTurn [] Nothing [UserMessage "hello"]
            (const (pure ()))

        result `shouldBe` Left (ConnectionError "stopped after failover")
        readIORef attempts `shouldReturn` [first, second]

    it "forwards raw provider reasoning text to the UI loop" do
        events <- newIORef []
        let send _params onStreamEvent = do
                onStreamEvent OtherResponseStreamEvent
                    { otherEventType = EventReasoningTextDelta
                    , sequenceNumber = Just 1
                    , eventDelta = Just "checking the implementation"
                    , streamItemId = Nothing
                    , streamOutputIndex = Nothing
                    , summaryIndex = Nothing
                    , turnState = Nothing
                    }
                pure (Left (ConnectionError "stop after reasoning"))
            backend =
                statelessResponsesBackend send
                    (pure defaultResponseCreateParams)

        result <- backend.submitTurn [] Nothing [UserMessage "hello"]
            (\event -> modifyIORef' events (<> [event]))

        result `shouldBe` Left (ConnectionError "stop after reasoning")
        readIORef events `shouldReturn`
            [ReasoningDelta "checking the implementation"]

    it "can hide raw reasoning while retaining reasoning summaries" do
        events <- newIORef []
        let send _params onStreamEvent = do
                onStreamEvent OtherResponseStreamEvent
                    { otherEventType = EventReasoningTextDelta
                    , sequenceNumber = Just 1
                    , eventDelta = Just "raw"
                    , streamItemId = Nothing
                    , streamOutputIndex = Nothing
                    , summaryIndex = Nothing
                    , turnState = Nothing
                    }
                onStreamEvent OtherResponseStreamEvent
                    { otherEventType = EventReasoningSummaryTextDelta
                    , sequenceNumber = Just 2
                    , eventDelta = Just "summary"
                    , streamItemId = Nothing
                    , streamOutputIndex = Nothing
                    , summaryIndex = Nothing
                    , turnState = Nothing
                    }
                pure (Left (ConnectionError "stop after reasoning"))
            backend =
                statelessResponsesBackendWithRawReasoning False send
                    (pure defaultResponseCreateParams)

        _ <- backend.submitTurn [] Nothing [UserMessage "hello"]
            (\event -> modifyIORef' events (<> [event]))

        readIORef events `shouldReturn` [ReasoningDelta "summary"]

    it "separates multiple reasoning summary parts for Markdown rendering" do
        events <- newIORef []
        let send _params onStreamEvent = do
                onStreamEvent ResponseReasoningSummaryPartAddedEvent
                    { streamItemId = Just "reasoning-1"
                    , streamOutputIndex = Just 0
                    , summaryIndex = Just 0
                    , partValue = Nothing
                    , sequenceNumber = Just 1

                    }
                onStreamEvent OtherResponseStreamEvent
                    { otherEventType = EventReasoningSummaryTextDelta
                    , sequenceNumber = Just 2
                    , eventDelta = Just "**Inspecting dependencies**"
                    , streamItemId = Nothing
                    , streamOutputIndex = Nothing
                    , summaryIndex = Nothing
                    , turnState = Nothing
                    }
                onStreamEvent ResponseReasoningSummaryPartAddedEvent
                    { streamItemId = Just "reasoning-1"
                    , streamOutputIndex = Just 0
                    , summaryIndex = Just 1
                    , partValue = Nothing
                    , sequenceNumber = Just 3

                    }
                onStreamEvent OtherResponseStreamEvent
                    { otherEventType = EventReasoningSummaryTextDelta
                    , sequenceNumber = Just 4
                    , eventDelta = Just "**Planning the fix**"
                    , streamItemId = Nothing
                    , streamOutputIndex = Nothing
                    , summaryIndex = Nothing
                    , turnState = Nothing
                    }
                pure (Left (ConnectionError "stop after reasoning"))
            backend =
                statelessResponsesBackendWithRawReasoning False send
                    (pure defaultResponseCreateParams)

        _ <- backend.submitTurn [] Nothing [UserMessage "hello"]
            (\event -> modifyIORef' events (<> [event]))

        readIORef events `shouldReturn`
            [ ReasoningDelta "**Inspecting dependencies**"
            , ReasoningDelta "\n\n"
            , ReasoningDelta "**Planning the fix**"
            ]

    it "preserves request input prefixes when adding transcript items" do
        let prefix = UnknownResponseItem TaggedObject
                { tag = "additional_tools"

                }
            params = paramsWithInputItems [prefix]
            request = withRequestInput params (turnInputsToItems [UserMessage "hello"])
        case request.input of
            Just (ResponseInputItems (first : second : _)) -> do
                first `shouldBe` prefix
                second `shouldSatisfy` isUserMessage
            _ -> expectationFailure "expected preserved input prefix"

    it "replaces arbitrary prior input instead of replaying it as a prefix" do
        let stale = turnInputsToItems [UserMessage "stale"]
            fresh = turnInputsToItems [UserMessage "fresh"]
            params = paramsWithInputItems stale
            request = withRequestInput params fresh
        request.input `shouldBe` Just (ResponseInputItems fresh)

    it "preserves only developer items marked as base instructions" do
        let additional = UnknownResponseItem TaggedObject
                { tag = "additional_tools"

                }
            unmarkedDeveloper = developerMessage
                "not base"
                ["some.other.kind"]
            markedDeveloper = developerMessage
                "base"
                ["other", "model.base_instructions"]
            stale = case turnInputsToItems [UserMessage "stale"] of
                [item] -> item
                _ -> error "expected one stale user item"
            fresh = turnInputsToItems [UserMessage "fresh"]
            params = paramsWithInputItems
                [ additional
                , markedDeveloper
                , unmarkedDeveloper
                , stale
                ]
            request = withRequestInput params fresh
        request.input `shouldBe` Just
            (ResponseInputItems (additional : markedDeveloper : fresh))

    it "strips image detail hints from Lite messages and tool outputs" do
        let additional = UnknownResponseItem TaggedObject
                { tag = "additional_tools"

                }
            imageMessage = MessageItem ResponseMessage
                { messageId = Nothing
                , content = MessageContentParts
                    [ InputImagePart
                        { detail = Just "high"
                        , fileId = Nothing
                        , imageUrl = Just "data:image/png;base64,AA=="
                        , promptCacheBreakpoint = Nothing

                        }
                    ]
                , role = RoleUser
                , status = Nothing
                , phase = Nothing
                , passthrough = Nothing

                }
            toolOutputValue = rawJsonFromEncoding (Aeson.toEncoding (Aeson.object
                [ "type" Aeson..= ("input_image" :: Text.Text)
                , "detail" Aeson..= ("high" :: Text.Text)
                , "image_url" Aeson..= ("data:image/png;base64,AA==" :: Text.Text)
                ]))
            toolOutput = FunctionCallOutputItem FunctionCallOutput
                { itemId = Nothing
                , callId = "call-1"
                , name = Nothing
                , namespace = Nothing
                , provider = Nothing
                , output = toolOutputValue
                , status = Nothing

                }
            params = paramsWithInputItems [additional]
            request = withRequestInput params [imageMessage, toolOutput]
        case request.input of
            Just (ResponseInputItems
                [ _
                , MessageItem ResponseMessage
                    { content = MessageContentParts [InputImagePart{detail}]
                    }
                , FunctionCallOutputItem FunctionCallOutput{output}
                ]) -> do
                    detail `shouldBe` Nothing
                    output `shouldBe` toolOutputValue
            other -> expectationFailure
                ("unexpected normalized Lite input: " <> show other)

    it "appends an empty assistant message after a trailing reasoning item" do
        let reasoning = ReasoningItemValue ReasoningItem
                { itemId = Just "rs-1"
                , summary = []
                , content = Nothing
                , encryptedContent = Nothing
                , status = Nothing

                }
            user = turnInputsToItems [UserMessage "hello"]
            params = defaultResponseCreateParams
            request = withRequestInput params (user <> [reasoning])
        case request.input of
            Just (ResponseInputItems items) -> do
                length items `shouldBe` 3
                last items `shouldSatisfy` isEmptyAssistantFollowup
            _ -> expectationFailure "expected request input items"

    it "does not invent a follow-up when reasoning already has a successor" do
        let reasoning = ReasoningItemValue ReasoningItem
                { itemId = Just "rs-1"
                , summary = []
                , content = Nothing
                , encryptedContent = Nothing
                , status = Nothing

                }
            items = turnInputsToItems [UserMessage "hello"] <> [reasoning]
                <> turnInputsToItems [UserMessage "continue"]
            request = withRequestInput defaultResponseCreateParams items
        request.input `shouldBe` Just (ResponseInputItems items)

    -- Responses Lite rejects a replayed reasoning item's lifecycle status as
    -- an unknown parameter (@input[N].status@), and Codex never sends the
    -- field on replayed messages, calls, outputs, or reasoning. Items whose
    -- status Codex does send on input keep it.
    it "drops provider lifecycle status from replayed transcript items" do
        let reasoning = ReasoningItemValue ReasoningItem
                { itemId = Just "rs-1"
                , summary = []
                , content = Nothing
                , encryptedContent = Just "opaque"
                , status = Just ItemCompleted
                }
            assistant = MessageItem ResponseMessage
                { messageId = Just "msg-1"
                , content = MessageContentParts
                    [OutputTextPart "done" Nothing Nothing]
                , role = RoleAssistant
                , status = Just ItemCompleted
                , phase = Nothing
                , passthrough = Nothing
                }
            call = FunctionCallItem FunctionCall
                { itemId = Just "fc-1"
                , callId = "call-1"
                , name = "shell"
                , namespace = Nothing
                , provider = Nothing
                , arguments = "{}"
                , encryptedFunctionArgs = Nothing
                , status = Just ItemCompleted
                }
            output = FunctionCallOutputItem FunctionCallOutput
                { itemId = Nothing
                , callId = "call-1"
                , name = Nothing
                , namespace = Nothing
                , provider = Nothing
                , output = rawJsonFromEncoding
                    (Aeson.toEncoding ("ok" :: Text.Text))
                , status = Just ItemIncomplete
                }
            customOutput = CustomToolCallOutputItem CustomToolCallOutput
                { itemId = Nothing
                , callId = "call-2"
                , name = Nothing
                , output = rawJsonFromEncoding
                    (Aeson.toEncoding ("ok" :: Text.Text))
                , status = Just ItemCompleted
                }
            customCall = CustomToolCallItem CustomToolCall
                { itemId = Just "ctc-1"
                , callId = "call-2"
                , name = "apply_patch"
                , namespace = Nothing
                , input = "*** Begin Patch"
                , status = Just ItemCompleted
                }
            shell = LocalShellCallItem LocalShellCall
                { itemId = Just "lsh-1"
                , callId = Just "call-3"
                , status = Just ItemCompleted
                , action = Nothing
                }
            history =
                turnInputsToItems [UserMessage "hello"]
                    <> [reasoning, assistant, call, output]
                    <> [customCall, customOutput, shell]
            request = withRequestInput defaultResponseCreateParams history
        map encodedStatus (requestInputItems request)
            `shouldBe`
                [ Nothing
                , Nothing
                , Nothing
                , Nothing
                , Nothing
                , Just (Aeson.String "completed")
                , Nothing
                , Just (Aeson.String "completed")
                ]
        -- Everything else on the replayed items survives untouched.
        map encodedField (requestInputItems request) !! 1
            `shouldBe` Just (Aeson.String "opaque")

-- | Streamed tool calls are announced immediately. Shell argument deltas
-- repaint that call with the partial command, while other tools retain coarse
-- activity updates.
streamProjectionSpec :: Spec
streamProjectionSpec = describe "newStreamEventToLoopEvents" do
    it "publishes a streamed function call immediately" do
        projectEvent <- newStreamEventToLoopEvents False
        events <- projectEvent (functionCallAdded "fc-1" "call-1" "shell_command")
        events `shouldBe`
            [ ToolStarted
                (functionToolCall "call-1" "shell_command" "")
            ]

    it "repaints a streamed shell call with its partial command" do
        projectEvent <- newStreamEventToLoopEvents False
        _ <- projectEvent (functionCallAdded "fc-1" "call-1" "shell_command")
        first <- projectEvent
            (argumentsDelta "fc-1" "{\"command\":\"git sta")
        first `shouldBe`
            [ ToolArgumentsUpdated
                (functionToolCall
                    "call-1"
                    "shell_command"
                    "{\"command\":\"git sta\"}")
            ]
        second <- projectEvent (argumentsDelta "fc-1" "tus\"}")
        second `shouldBe`
            [ ToolArgumentsUpdated
                (functionToolCall
                    "call-1"
                    "shell_command"
                    "{\"command\":\"git status\"}")
            ]

    it "replaces streamed tool metadata with the canonical done item" do
        projectEvent <- newStreamEventToLoopEvents False
        _ <- projectEvent (functionCallAdded "fc-1" "call-1" "shell_command")
        events <- projectEvent
            (functionCallDone
                "fc-1"
                "call-1"
                "shell_command"
                "{\"command\":\"git status\"}")
        events `shouldBe`
            [ ToolUpdated
                (functionToolCall
                    "call-1"
                    "shell_command"
                    "{\"command\":\"git status\"}")
            ]

    it "publishes a streamed custom tool call immediately" do
        projectEvent <- newStreamEventToLoopEvents False
        events <- projectEvent
            (customToolCallAdded "ct-1" "call-9" "apply_patch")
        events `shouldBe`
            [ ToolStarted ToolCall
                { callId = "call-9"
                , name = "apply_patch"
                , arguments = ""
                , callKind = CustomCallKind
                , argumentsEncrypted = False
                }
            , ActivityUpdated "Writing apply_patch call…"
            ]

    it "updates a streamed custom tool from its done item" do
        projectEvent <- newStreamEventToLoopEvents False
        _ <- projectEvent
            (customToolCallAdded "ct-1" "call-9" "apply_patch")
        events <- projectEvent
            (customToolCallDone
                "ct-1"
                "call-9"
                "apply_patch"
                "*** Begin Patch")
        events `shouldBe`
            [ ToolUpdated ToolCall
                { callId = "call-9"
                , name = "apply_patch"
                , arguments = "*** Begin Patch"
                , callKind = CustomCallKind
                , argumentsEncrypted = False
                }
            ]

    it "does not replace a shell preview with coarse argument activity" do
        projectEvent <- newStreamEventToLoopEvents False
        _ <- projectEvent (functionCallAdded "fc-1" "call-1" "shell_command")
        quiet <- projectEvent
            (argumentsDelta "fc-1" (Text.replicate 100 "x"))
        quiet `shouldBe` []
        loud <- projectEvent
            (argumentsDelta "fc-1" (Text.replicate 9900 "y"))
        loud `shouldBe` []

    it "warns once per runaway argument window" do
        projectEvent <- newStreamEventToLoopEvents False
        _ <- projectEvent (functionCallAdded "fc-1" "call-1" "shell_command")
        let bigDelta = Text.replicate 60000 "z"
        first <- projectEvent (argumentsDelta "fc-1" bigDelta)
        first `shouldBe` []
        second <- projectEvent (argumentsDelta "fc-1" bigDelta)
        second `shouldBe`
            [ WarningRaised
                ("The model has streamed 120k chars of shell_command "
                    <> "arguments in one response; it may be stuck in a "
                    <> "repetition loop.")
            ]
        third <- projectEvent (argumentsDelta "fc-1" bigDelta)
        third `shouldBe` []

    it "counts custom tool input as argument streaming" do
        projectEvent <- newStreamEventToLoopEvents False
        _ <- projectEvent (customToolCallAdded "ct-1" "call-9" "apply_patch")
        loud <- projectEvent
            (customInputDelta "ct-1" "call-9" (Text.replicate 10000 "p"))
        loud `shouldBe`
            [ActivityUpdated "Writing apply_patch call… (10k chars)"]

    it "keeps plain deltas mapped through the pure projection" do
        projectEvent <- newStreamEventToLoopEvents False
        events <- projectEvent OtherResponseStreamEvent
            { otherEventType = EventOutputTextDelta
            , sequenceNumber = Just 1
            , eventDelta = Just "hi"
                    , streamItemId = Nothing
                    , streamOutputIndex = Nothing
                    , summaryIndex = Nothing
                    , turnState = Nothing
            }
        events `shouldBe` [TextDelta "hi"]

functionToolCall :: Text.Text -> Text.Text -> Text.Text -> ToolCall
functionToolCall functionCallId functionName functionArguments = ToolCall
    { callId = functionCallId
    , name = functionName
    , arguments = functionArguments
    , callKind = FunctionCallKind
    , argumentsEncrypted = False
    }

functionCallAdded :: Text.Text -> Text.Text -> Text.Text -> ResponseStreamEvent
functionCallAdded functionItemId functionCallId functionName =
    ResponseOutputItemAddedEvent
        { item = FunctionCallItem FunctionCall
            { itemId = Just functionItemId
            , callId = functionCallId
            , name = functionName
            , namespace = Nothing
            , provider = Nothing
            , arguments = ""
            , encryptedFunctionArgs = Nothing
            , status = Nothing

            }
        , outputIndex = Just 0
        , sequenceNumber = Just 1

        }

functionCallDone
    :: Text.Text
    -> Text.Text
    -> Text.Text
    -> Text.Text
    -> ResponseStreamEvent
functionCallDone functionItemId functionCallId functionName functionArguments =
    ResponseOutputItemDoneEvent
        { item = FunctionCallItem FunctionCall
            { itemId = Just functionItemId
            , callId = functionCallId
            , name = functionName
            , namespace = Nothing
            , provider = Nothing
            , arguments = functionArguments
            , encryptedFunctionArgs = Nothing
            , status = Nothing

            }
        , outputIndex = Just 0
        , sequenceNumber = Just 2

        }

customToolCallAdded :: Text.Text -> Text.Text -> Text.Text -> ResponseStreamEvent
customToolCallAdded customItemId customCallId customName =
    ResponseOutputItemAddedEvent
        { item = CustomToolCallItem CustomToolCall
            { itemId = Just customItemId
            , callId = customCallId
            , name = customName
            , namespace = Nothing
            , input = ""
            , status = Nothing

            }
        , outputIndex = Just 0
        , sequenceNumber = Just 1

        }

customToolCallDone
    :: Text.Text
    -> Text.Text
    -> Text.Text
    -> Text.Text
    -> ResponseStreamEvent
customToolCallDone customItemId customCallId customName customInput =
    ResponseOutputItemDoneEvent
        { item = CustomToolCallItem CustomToolCall
            { itemId = Just customItemId
            , callId = customCallId
            , name = customName
            , namespace = Nothing
            , input = customInput
            , status = Nothing

            }
        , outputIndex = Just 0
        , sequenceNumber = Just 2

        }

argumentsDelta :: Text.Text -> Text.Text -> ResponseStreamEvent
argumentsDelta deltaItemId deltaText =
    ResponseFunctionCallArgumentsDeltaEvent
        { delta = Just deltaText
        , streamItemId = Just deltaItemId
        , streamOutputIndex = Just 0
        , sequenceNumber = Nothing

        }

customInputDelta :: Text.Text -> Text.Text -> Text.Text -> ResponseStreamEvent
customInputDelta deltaItemId deltaCallId deltaText =
    ResponseCustomToolInputDeltaEvent
        { delta = Just deltaText
        , streamItemId = Just deltaItemId
        , streamCallId = Just deltaCallId
        , streamOutputIndex = Just 0
        , sequenceNumber = Nothing

        }

-- | 'input' is also a field on 'CustomToolCall', so a record update on
-- 'ResponseCreateParams' is ambiguous here. Rebuild from the constructor.
requestInputItems :: ResponseCreateParams -> [ResponseItem]
requestInputItems request = case request.input of
    Just (ResponseInputItems items) -> items
    _ -> []

encodedStatus :: ResponseItem -> Maybe Aeson.Value
encodedStatus item = case Aeson.toJSON item of
    Aeson.Object fields -> KeyMap.lookup "status" fields
    _ -> Nothing

encodedField :: ResponseItem -> Maybe Aeson.Value
encodedField item = case Aeson.toJSON item of
    Aeson.Object fields -> KeyMap.lookup "encrypted_content" fields
    _ -> Nothing

paramsWithInputItems :: [ResponseItem] -> ResponseCreateParams
paramsWithInputItems items = case defaultResponseCreateParams of
    ResponseCreateParams{..} ->
        ResponseCreateParams
            { input = Just (ResponseInputItems items)
            , ..
            }

credential :: String -> Credential
credential label = Credential
    { accessToken = "token-" <> Text.pack label
    , accountId = Text.pack label
    , leaseId = Nothing
    , provider = OpenRouterProvider
    }

isInputFile :: ResponseContentPart -> Bool
isInputFile = \case
    InputFilePart{} -> True
    _ -> False

isInputImage :: ResponseContentPart -> Bool
isInputImage = \case
    InputImagePart{} -> True
    _ -> False

isUserMessage :: ResponseItem -> Bool
isUserMessage = \case
    MessageItem message ->
        message.role == RoleUser
            && case message.content of
                MessageContentParts [InputTextPart{ text = value }] ->
                    value == "hello"
                _ -> False
    _ -> False

isEmptyAssistantFollowup :: ResponseItem -> Bool
isEmptyAssistantFollowup = \case
    MessageItem message ->
        message.role == RoleAssistant
            && case message.content of
                MessageContentParts [OutputTextPart{ text = value }] ->
                    Text.null value
                _ -> False
    _ -> False

developerMessage :: Text.Text -> [Text.Text] -> ResponseItem
developerMessage messageText contentItemKinds =
    MessageItem ResponseMessage
        { messageId = Nothing
        , content = MessageContentParts
            [InputTextPart messageText Nothing]
        , role = RoleDeveloper
        , status = Nothing
        , phase = Nothing
        , passthrough = Just InternalChatMetadata
            { turnId = Nothing
            , createTime = Nothing
            , contentItemKinds = Just contentItemKinds
            , executedToolCalls = Nothing

            }

        }

jsonField :: Text.Text -> Aeson.Value -> Maybe Aeson.Value
jsonField fieldName = \case
    Aeson.Object object -> KeyMap.lookup (Key.fromText fieldName) object
    _ -> Nothing
