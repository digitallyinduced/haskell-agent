module Agent.Responses.LoopBackendSpec (spec) where

import Agent.Error (ApiError(..), ErrorType(..))
import Agent.Loop
    ( Backend(..)
    , BackendCallbacks(..)
    , BackendResult(..)
    , BackendSnapshot(..)
    , FileAttachment(..)
    , ImageAttachment(..)
    , LoopEvent(..)
    , TurnAttachment(..)
    , TurnInput(..)
    , advanceBackendSnapshot
    , emptyBackendSnapshot
    , userMessageWithAttachments
    )
import Agent.Provider
    ( Credential(..)
    , BillingMode(..)
    , FailedCredential(..)
    , Provider(..)
    , tokenProvider
    )
import Agent.Json (rawJsonFromEncoding)
import qualified Agent.Json.Decode as Json
import qualified Agent.Responses.Codec as Codec
import Agent.Responses.GenericBackend (genericResponsesBackendWith)
import Agent.Responses.LoopBackend
    ( emptyStreamProjectionState
    , newStreamEventToLoopEvents
    , statelessResponsesBackend
    , statelessResponsesBackendWithRawReasoning
    , tokenProviderStatelessResponsesBackend
    , turnInputsToItems
    , responseItemToToolCall
    , toolResultToItem
    , withRequestInput
    , streamEventToLoopEventsStep
    )
import Agent.Responses.Types
    ( MessageContent(..)
    , CodexRateLimits(..)
    , CompactionItem(..)
    , ComputerAction(..)
    , ComputerCall(..)
    , ComputerCallOutput(..)
    , CustomToolCall(..)
    , CustomToolCallOutput(..)
    , CustomTool(..)
    , FunctionCall(..)
    , FunctionCallOutput(..)
    , FunctionTool(..)
    , InternalChatMetadata(..)
    , ItemStatus(..)
    , LocalShellCall(..)
    , ReasoningItem(..)
    , ResponseContentPart(..)
    , ResponseItem(..)
    , ResponseMessage(..)
    , ResponseRole(..)
    , Response
    , ResponseTool(..)
    , ResponseStreamEvent(..)
    , StreamEventType(..)
    , ResponseInput(..)
    , ResponseCreateParams(..)
    , TaggedObject(..)
    , computerFunctionNamespace
    , computerFunctionName
    , compactionCheckpointOriginItem
    , defaultResponseCreateParams
    , legacyComputerFunctionName
    )
import Agent.Responses.Types.Items (responseItemDecoder)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Either (isLeft)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Agent.ToolDispatch
    ( ToolCall(..)
    , ToolCallKind(..)
    , ToolCallMode(..)
    , ToolCallResult(..)
    , ToolResultImage(..)
    , toolCallMode
    )
import Test.Hspec

spec :: Spec
spec = do
    wireAsyncSpec
    backendSpec
    streamProjectionSpec

wireAsyncSpec :: Spec
wireAsyncSpec = describe "Responses async wire fields" do
    it "decodes async function and custom calls without changing legacy defaults" do
        let decodeItem value =
                Json.decodeEither responseItemDecoder
                    (LBS.toStrict (Aeson.encode value))
            functionValue :: Maybe Bool -> Aeson.Value
            functionValue flag = Aeson.object
                ( [ "type" Aeson..= ("function_call" :: Text.Text)
                  , "call_id" Aeson..= ("function-call" :: Text.Text)
                  , "name" Aeson..= ("read_file" :: Text.Text)
                  , "arguments" Aeson..= ("{}" :: Text.Text)
                  ]
                    <> maybe [] (\value -> ["async" Aeson..= value]) flag
                )
            customValue :: Maybe Bool -> Aeson.Value
            customValue flag = Aeson.object
                ( [ "type" Aeson..= ("custom_tool_call" :: Text.Text)
                  , "call_id" Aeson..= ("custom-call" :: Text.Text)
                  , "name" Aeson..= ("apply_patch" :: Text.Text)
                  , "input" Aeson..= ("patch" :: Text.Text)
                  ]
                    <> maybe [] (\value -> ["async" Aeson..= value]) flag
                )
        decodeItem (functionValue (Just True))
            `shouldSatisfy` \case
                Right (FunctionCallItem FunctionCall{async = Just True}) -> True
                _ -> False
        decodeItem (functionValue (Just False))
            `shouldSatisfy` \case
                Right (FunctionCallItem FunctionCall{async = Just False}) -> True
                _ -> False
        decodeItem (functionValue Nothing)
            `shouldSatisfy` \case
                Right (FunctionCallItem FunctionCall{async = Nothing}) -> True
                _ -> False
        decodeItem (customValue (Just True))
            `shouldSatisfy` \case
                Right (CustomToolCallItem CustomToolCall{async = Just True}) -> True
                _ -> False
        decodeItem (customValue (Just False))
            `shouldSatisfy` \case
                Right (CustomToolCallItem CustomToolCall{async = Just False}) -> True
                _ -> False
        decodeItem (customValue Nothing)
            `shouldSatisfy` \case
                Right (CustomToolCallItem CustomToolCall{async = Nothing}) -> True
                _ -> False

    it "encodes async tools explicitly and omits absent capability" do
        let function flag = FunctionToolValue FunctionTool
                { name = "read_file"
                , description = Nothing
                , parameters = Nothing
                , strict = Nothing
                , async = flag
                }
            custom flag = CustomToolValue CustomTool
                { name = "apply_patch"
                , description = Nothing
                , format = Nothing
                , async = flag
                }
            hasAsync expected = \case
                Aeson.Object object ->
                    KeyMap.lookup "async" object == expected
                _ -> False
        Aeson.toJSON (function (Just True))
            `shouldSatisfy` hasAsync (Just (Aeson.Bool True))
        Aeson.toJSON (custom (Just True))
            `shouldSatisfy` hasAsync (Just (Aeson.Bool True))
        Aeson.toJSON (function Nothing) `shouldSatisfy` hasAsync Nothing
        Aeson.toJSON (custom Nothing) `shouldSatisfy` hasAsync Nothing

backendSpec :: Spec
backendSpec = describe "tokenProviderStatelessResponsesBackend" do
    it "preserves checkpoint provenance for provider-aware projection" do
        let request =
                withRequestInput
                    defaultResponseCreateParams
                    [ CompactionItemValue CompactionItem
                        { itemId = Just "cmp-1"
                        , encryptedContent = Just "opaque"
                        }
                    , compactionCheckpointOriginItem "xai"
                    ]
        request.input `shouldBe` Just
            (ResponseInputItems
                [ CompactionItemValue CompactionItem
                    { itemId = Just "cmp-1"
                    , encryptedContent = Just "opaque"
                    }
                , compactionCheckpointOriginItem "xai"
                ])

    it "rejects computer coordinates outside the platform Int range" do
        let tooLarge = toInteger (maxBound :: Int) + 1
            payload = LBS.toStrict $ Aeson.encode $ Aeson.object
                [ "type" Aeson..= ("computer_call" :: Text.Text)
                , "call_id" Aeson..= ("call-overflow" :: Text.Text)
                , "actions" Aeson..=
                    [ Aeson.object
                        [ "type" Aeson..= ("click" :: Text.Text)
                        , "x" Aeson..= tooLarge
                        , "y" Aeson..= (0 :: Int)
                        ]
                    ]
                ]
        Json.decodeEither responseItemDecoder payload
            `shouldSatisfy` isLeft

    it "routes the reserved ordinary computer function to the harness" do
        let call = FunctionCall
                { itemId = Nothing
                , callId = "call-1"
                , name = computerFunctionName
                , namespace = Just "functions"
                , provider = Nothing
                , arguments =
                    "{\"actions\":[{\"type\":\"type\",\"text\":\"secret\"}]}"
                , encryptedFunctionArgs = Nothing
                , status = Nothing
                , async = Nothing
                }
        case responseItemToToolCall (FunctionCallItem call) of
            Just projected -> do
                projected.callId `shouldBe` "call-1"
                projected.name `shouldBe` "computer"
                projected.callKind `shouldBe` ComputerFunctionCallKind
                projected.argumentsEncrypted `shouldBe` True
            Nothing -> expectationFailure "computer call was not projected"

    it "routes the standard computer function without a namespace" do
        let call = FunctionCall
                { itemId = Nothing
                , callId = "call-standard"
                , name = computerFunctionName
                , namespace = Nothing
                , provider = Nothing
                , arguments =
                    "{\"actions\":[{\"type\":\"screenshot\"}]}"
                , encryptedFunctionArgs = Nothing
                , status = Nothing
                , async = Nothing
                }
        case responseItemToToolCall (FunctionCallItem call) of
            Just projected -> do
                projected.name `shouldBe` "computer"
                projected.callKind `shouldBe` ComputerFunctionCallKind
            Nothing -> expectationFailure
                "standard computer function was not routed"

    it "projects async mode only for calls marked async true" do
        let function flag = FunctionCallItem FunctionCall
                { itemId = Nothing
                , callId = "function-call"
                , name = "read_file"
                , namespace = Nothing
                , provider = Nothing
                , arguments = "{}"
                , encryptedFunctionArgs = Nothing
                , status = Nothing
                , async = flag
                }
            custom flag = CustomToolCallItem CustomToolCall
                { itemId = Nothing
                , callId = "custom-call"
                , name = "apply_patch"
                , namespace = Nothing
                , input = "patch"
                , status = Nothing
                , async = flag
                }
            mode item = toolCallMode <$> responseItemToToolCall item
        mode (function (Just True)) `shouldBe` Just AsyncToolCall
        mode (function (Just False)) `shouldBe` Just BlockingToolCall
        mode (function Nothing) `shouldBe` Just BlockingToolCall
        mode (custom (Just True)) `shouldBe` Just AsyncToolCall
        mode (custom Nothing) `shouldBe` Just BlockingToolCall

    it "marks only async tool results on continuation items" do
        let blocking = toolResultToItem ToolCallResult
                { callId = "blocking-call"
                , output = "done"
                , callKind = FunctionCallKind
                }
            asynchronous = toolResultToItem AsyncToolCallResult
                { callId = "async-call"
                , output = "done"
                , callKind = CustomCallKind
                }
        blocking `shouldSatisfy` \case
            FunctionCallOutputItem FunctionCallOutput{async = Nothing} -> True
            _ -> False
        asynchronous `shouldSatisfy` \case
            CustomToolCallOutputItem CustomToolCallOutput{async = Just True} ->
                True
            _ -> False

    it "returns computer results as text plus a fresh user screenshot" do
        let encoded = TextEncoding.decodeUtf8 $ LBS.toStrict $ Aeson.encode
                ComputerCallOutput
                { computerOutputItemId = Nothing
                , computerOutputCallId = "ignored"
                , screenshotDataUrl = "data:image/png;base64,AA=="
                , acknowledgedChecks = []
                , computerOutputStatus = Nothing
                , computerOutputExtra = KeyMap.empty
                }
            result = ToolCallResult
                { callId = "call-1"
                , output = encoded
                , callKind = ComputerFunctionCallKind
                }
        case turnInputsToItems [CompletedTool result] of
            [FunctionCallOutputItem output, MessageItem observation] -> do
                Aeson.toJSON output.output `shouldBe`
                    Aeson.String "Computer action completed."
                case observation.content of
                    MessageContentParts
                        [InputTextPart{}, InputImagePart{imageUrl, detail}] -> do
                            imageUrl `shouldBe`
                                Just "data:image/png;base64,AA=="
                            detail `shouldBe` Just "auto"
                    other -> expectationFailure
                        ("expected screenshot observation, got " <> show other)
            other -> expectationFailure
                ("unexpected computer continuation: " <> show other)

    it "returns native accessibility state beside the screenshot" do
        let encoded = TextEncoding.decodeUtf8 $ LBS.toStrict $ Aeson.encode
                ComputerCallOutput
                    { computerOutputItemId = Nothing
                    , computerOutputCallId = "ignored"
                    , screenshotDataUrl = "data:image/jpeg;base64,AA=="
                    , acknowledgedChecks = []
                    , computerOutputStatus = Nothing
                    , computerOutputExtra = KeyMap.singleton
                        "accessibility_state"
                        (Aeson.String
                            "app=\"TextEdit\"\n[1] AXButton \"Save\"")
                    }
            result = ToolCallResult
                { callId = "call-native"
                , output = encoded
                , callKind = ComputerFunctionCallKind
                }
        case turnInputsToItems [CompletedTool result] of
            FunctionCallOutputItem output : _ ->
                Aeson.toJSON output.output `shouldBe` Aeson.String
                    "Computer action completed.\n\nCurrent macOS accessibility state:\napp=\"TextEdit\"\n[1] AXButton \"Save\""
            other -> expectationFailure
                ("unexpected native computer continuation: " <> show other)

    it "keeps computer output before its observation in standard and Lite requests" do
        let encoded = TextEncoding.decodeUtf8 $ LBS.toStrict $ Aeson.encode
                ComputerCallOutput
                    { computerOutputItemId = Nothing
                    , computerOutputCallId = "ignored"
                    , screenshotDataUrl = "data:image/png;base64,AA=="
                    , acknowledgedChecks = []
                    , computerOutputStatus = Nothing
                    , computerOutputExtra = KeyMap.empty
                    }
            inputs =
                turnInputsToItems
                    [ CompletedTool ToolCallResult
                        { callId = "call-function"
                        , output = encoded
                        , callKind = ComputerFunctionCallKind
                        }
                    ]
            standard = withRequestInput defaultResponseCreateParams inputs
            additional =
                UnknownResponseItem TaggedObject
                    { tag = "additional_tools" }
            lite =
                withRequestInput
                    defaultResponseCreateParams
                        { input = Just (ResponseInputItems [additional]) }
                    inputs
            assertContinuation expectedDetail = \case
                [ FunctionCallOutputItem output
                    , MessageItem ResponseMessage
                        { role = RoleUser
                        , content =
                            MessageContentParts
                                [ InputTextPart{}
                                    , InputImagePart{detail}
                                    ]
                        }
                    ] -> do
                        Aeson.toJSON output.output `shouldBe`
                            Aeson.String "Computer action completed."
                        detail `shouldBe` expectedDetail
                other -> expectationFailure
                    ("unexpected computer continuation: " <> show other)
        case standard.input of
            Just (ResponseInputItems continuation) ->
                assertContinuation (Just "auto") continuation
            other -> expectationFailure
                ("unexpected standard continuation: " <> show other)
        case lite.input of
            Just (ResponseInputItems (_ : continuation)) ->
                assertContinuation Nothing continuation
            other -> expectationFailure
                ("unexpected Lite continuation: " <> show other)

    it "does not reuse a screenshot when the latest computer call fails" do
        let successful = TextEncoding.decodeUtf8 $ LBS.toStrict $ Aeson.encode
                ComputerCallOutput
                    { computerOutputItemId = Nothing
                    , computerOutputCallId = "ignored"
                    , screenshotDataUrl = "data:image/png;base64,STALE"
                    , acknowledgedChecks = []
                    , computerOutputStatus = Nothing
                    , computerOutputExtra = KeyMap.empty
                    }
            inputs =
                [ CompletedTool ToolCallResult
                    { callId = "call-success"
                    , output = successful
                    , callKind = ComputerFunctionCallKind
                    }
                , CompletedTool ToolCallResult
                    { callId = "call-failed"
                    , output = "Computer input failed after changing the UI."
                    , callKind = ComputerFunctionCallKind
                    }
                ]
        case turnInputsToItems inputs of
            [ FunctionCallOutputItem first
                , FunctionCallOutputItem second
                ] -> do
                    Aeson.toJSON first.output `shouldBe`
                        Aeson.String "Computer action completed."
                    Aeson.toJSON second.output `shouldBe`
                        Aeson.String
                            "Computer input failed after changing the UI."
            other -> expectationFailure
                ("stale screenshot was reused: " <> show other)

    it "normalizes native computer calls and outputs at the wire boundary" do
        let call =
                ComputerCall
                    { computerCallItemId = Just "native-item"
                    , computerCallId = "native-call"
                    , computerActions = [TypeAction "secret"]
                    , pendingSafetyChecks = []
                    , computerCallStatus = Just ItemCompleted
                    , computerCallExtra = KeyMap.empty
                    }
            output =
                ComputerCallOutput
                    { computerOutputItemId = Just "native-output"
                    , computerOutputCallId = "native-call"
                    , screenshotDataUrl = "data:image/png;base64,NATIVE"
                    , acknowledgedChecks = []
                    , computerOutputStatus = Just ItemCompleted
                    , computerOutputExtra = KeyMap.empty
                    }
            request =
                withRequestInput
                    defaultResponseCreateParams
                    [ComputerCallItem call, ComputerCallOutputItem output]
        case responseItemToToolCall (ComputerCallItem call) of
            Just projected -> do
                projected.name `shouldBe` "computer"
                projected.callKind `shouldBe` ComputerCallKind
                projected.argumentsEncrypted `shouldBe` True
            Nothing -> expectationFailure "native computer call was not projected"
        case request.input of
            Just
                (ResponseInputItems
                    [ FunctionCallItem function
                        , FunctionCallOutputItem functionOutput
                        , MessageItem ResponseMessage
                            { role = RoleUser
                            , content =
                                MessageContentParts
                                    [ InputTextPart{}
                                        , InputImagePart{imageUrl}
                                        ]
                            }
                        ]) -> do
                            function.itemId `shouldBe` Nothing
                            function.name `shouldBe` computerFunctionName
                            function.namespace `shouldBe` Nothing
                            function.callId `shouldBe` "native-call"
                            functionOutput.itemId `shouldBe` Nothing
                            functionOutput.callId `shouldBe` "native-call"
                            Aeson.toJSON functionOutput.output `shouldBe`
                                Aeson.String "Computer action completed."
                            imageUrl `shouldBe`
                                Just "data:image/png;base64,NATIVE"
            other -> expectationFailure
                ("legacy computer history reached the wire: " <> show other)

    it "normalizes the earlier Lite computer namespace" do
        let call =
                FunctionCall
                    { itemId = Just "legacy-function-item"
                    , callId = "legacy-function-call"
                    , name = legacyComputerFunctionName
                    , namespace = Just computerFunctionNamespace
                    , provider = Nothing
                    , arguments =
                        "{\"actions\":[{\"type\":\"screenshot\"}]}"
                    , encryptedFunctionArgs = Nothing
                    , status = Just ItemCompleted
                    , async = Nothing
                    }
            output =
                FunctionCallOutput
                    { itemId = Just "legacy-output-item"
                    , callId = "legacy-function-call"
                    , name = Nothing
                    , namespace = Nothing
                    , provider = Nothing
                    , output =
                        rawJsonFromEncoding . Aeson.toEncoding $
                            [ Aeson.object
                                [ "type" Aeson..=
                                    ("input_image" :: Text.Text)
                                , "image_url" Aeson..=
                                    ( "data:image/jpeg;base64,LITE"
                                        :: Text.Text
                                    )
                                , "detail" Aeson..=
                                    ("original" :: Text.Text)
                                ]
                            ]
                    , status = Nothing
                    , async = Nothing
                    }
            request =
                withRequestInput
                    defaultResponseCreateParams
                    [FunctionCallItem call, FunctionCallOutputItem output]
        case request.input of
            Just
                (ResponseInputItems
                    [ FunctionCallItem function
                        , FunctionCallOutputItem functionOutput
                        , MessageItem ResponseMessage
                            { content =
                                MessageContentParts
                                    [ InputTextPart{}
                                        , InputImagePart{imageUrl}
                                        ]
                            }
                        ]) -> do
                            function.itemId `shouldBe` Nothing
                            function.name `shouldBe` computerFunctionName
                            function.namespace `shouldBe` Nothing
                            functionOutput.itemId `shouldBe` Nothing
                            Aeson.toJSON functionOutput.output `shouldBe`
                                Aeson.String "Computer action completed."
                            imageUrl `shouldBe`
                                Just "data:image/jpeg;base64,LITE"
            other -> expectationFailure
                ("legacy Lite history reached the wire: " <> show other)

    it "returns rejected legacy computer calls as ordinary text output" do
        case
            toolResultToItem
                ToolCallResult
                    { callId = "call-failed"
                    , output = "Tool call rejected by user."
                    , callKind = ComputerCallKind
                    } of
            FunctionCallOutputItem output -> do
                output.callId `shouldBe` "call-failed"
                Aeson.toJSON output.output `shouldBe`
                    Aeson.String "Tool call rejected by user."
            other -> expectationFailure ("unexpected output: " <> show other)

    it "does not text-truncate large computer screenshots" do
        let screenshot =
                "data:image/png;base64,"
                    <> Text.replicate (2 * 1024 * 1024) "A"
            encoded =
                TextEncoding.decodeUtf8 . LBS.toStrict . Aeson.encode $
                    ComputerCallOutput
                        { computerOutputItemId = Nothing
                        , computerOutputCallId = "ignored"
                        , screenshotDataUrl = screenshot
                        , acknowledgedChecks = []
                        , computerOutputStatus = Nothing
                        , computerOutputExtra = KeyMap.empty
                        }
        case
            turnInputsToItems
                [ CompletedTool
                    ToolCallResult
                        { callId = "call-large"
                        , output = encoded
                        , callKind = ComputerCallKind
                        }
                ] of
            [FunctionCallOutputItem{}, MessageItem observation] ->
                case observation.content of
                    MessageContentParts
                        [InputTextPart{}, InputImagePart{imageUrl}] ->
                            imageUrl `shouldBe` Just screenshot
                    other -> expectationFailure
                        ("expected large screenshot, got " <> show other)
            other -> expectationFailure
                ("unexpected large continuation: " <> show other)

    it "encodes file attachments as input_file parts" do
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
                    MessageContentParts
                        [InputTextPart{}, InputImagePart{}, InputFilePart{}] ->
                            pure ()
                    _ -> expectationFailure "expected multimodal message parts"
            other -> expectationFailure ("unexpected items: " <> show other)

    it "encodes rich function outputs as image content followed by the hint" do
        case toolResultToItem ToolCallResultWithImages
                { callId = "image-call"
                , output = "saved under generated_images"
                , callKind = FunctionCallKind
                , toolResultImages =
                    [ ToolResultImage
                        { imageUrl = "data:image/png;base64,AA=="
                        , imageDetail = Just "high"
                        }
                    ]
                } of
            FunctionCallOutputItem output ->
                Aeson.toJSON output.output `shouldBe` Aeson.toJSON
                    [ InputImagePart
                        { detail = Just "high"
                        , fileId = Nothing
                        , imageUrl = Just "data:image/png;base64,AA=="
                        , promptCacheBreakpoint = Nothing
                        }
                    , InputTextPart
                        { text = "saved under generated_images"
                        , promptCacheBreakpoint = Nothing
                        }
                    ]
            other -> expectationFailure
                ("expected function output, got " <> show other)

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

        result <- backend.submitTurn emptyBackendSnapshot Nothing [UserMessage "hello"]
            (const (pure ()))

        result `shouldBe` Left (ConnectionError "stopped after failover")
        readIORef attempts `shouldReturn` [first, second]

    it "replaces obsolete history after a server compaction checkpoint" do
        let oldItems = turnInputsToItems [UserMessage "old context"]
            checkpoint = CompactionItemValue CompactionItem
                { itemId = Just "compact-1"
                , encryptedContent = Just "opaque"
                }
            answer = MessageItem ResponseMessage
                { messageId = Just "message-1"
                , content = MessageContentParts
                    [OutputTextPart "continued" Nothing Nothing]
                , role = RoleAssistant
                , status = Nothing
                , phase = Nothing
                , passthrough = Nothing
                }
            send _params _onEvent =
                pure (Right (responseWithOutput [checkpoint, answer]))
            backend =
                statelessResponsesBackend send
                    (pure defaultResponseCreateParams)
            snapshot =
                advanceBackendSnapshot
                    emptyBackendSnapshot
                    oldItems
                    Nothing

        result <- backend.submitTurn
            snapshot
            Nothing
            [UserMessage "new input"]
            (const (pure ()))

        fmap (.backendState.backendItems) result
            `shouldBe` Right [checkpoint, answer]

    it "retains generic history while dropping an unreplayable checkpoint" do
        let oldItems = turnInputsToItems [UserMessage "old context"]
            newItems = turnInputsToItems [UserMessage "new input"]
            checkpoint = CompactionItemValue CompactionItem
                { itemId = Just "compact-generic"
                , encryptedContent = Just "opaque"
                }
            answer = MessageItem ResponseMessage
                { messageId = Just "message-generic"
                , content = MessageContentParts
                    [OutputTextPart "continued" Nothing Nothing]
                , role = RoleAssistant
                , status = Nothing
                , phase = Nothing
                , passthrough = Nothing
                }
            send _params _onEvent =
                pure (Right (responseWithOutput [checkpoint, answer]))
            backend =
                genericResponsesBackendWith send
                    (pure defaultResponseCreateParams)
            snapshot =
                advanceBackendSnapshot
                    emptyBackendSnapshot
                    oldItems
                    Nothing

        result <- backend.submitTurn
            snapshot
            Nothing
            [UserMessage "new input"]
            (const (pure ()))

        fmap (.backendState.backendItems) result
            `shouldBe`
                Right
                    (oldItems <> newItems <> [answer])

    it "preserves retained history when only the request has a checkpoint" do
        let retained = turnInputsToItems [UserMessage "retained context"]
            checkpoint = CompactionItemValue CompactionItem
                { itemId = Just "compact-old"
                , encryptedContent = Just "opaque"
                }
            existingItems = retained <> [checkpoint]
            newItems = turnInputsToItems [UserMessage "new input"]
            answer = MessageItem ResponseMessage
                { messageId = Just "message-ordinary"
                , content = MessageContentParts
                    [OutputTextPart "continued" Nothing Nothing]
                , role = RoleAssistant
                , status = Nothing
                , phase = Nothing
                , passthrough = Nothing
                }
            send _params _onEvent =
                pure (Right (responseWithOutput [answer]))
            backend =
                statelessResponsesBackend send
                    (pure defaultResponseCreateParams)
            snapshot =
                advanceBackendSnapshot
                    emptyBackendSnapshot
                    existingItems
                    Nothing

        result <- backend.submitTurn
            snapshot
            Nothing
            [UserMessage "new input"]
            (const (pure ()))

        fmap (.backendState.backendItems) result
            `shouldBe` Right (existingItems <> newItems <> [answer])

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

        result <- backend.submitTurn emptyBackendSnapshot Nothing [UserMessage "hello"]
            (\event -> modifyIORef' events (<> [event]))

        result `shouldBe` Left (ConnectionError "stop after reasoning")
        readIORef events `shouldReturn`
            [ReasoningDelta "checking the implementation"]

    it "announces async calls only when a completed output item arrives" do
        announced <- newIORef []
        let partialCall = FunctionCall
                { itemId = Just "async-item"
                , callId = "async-call"
                , name = "read_file"
                , namespace = Nothing
                , provider = Nothing
                , arguments = ""
                , encryptedFunctionArgs = Nothing
                , status = Nothing
                , async = Just True
                }
            completedCall = partialCall
                { arguments = "{\"path\":\"README.md\"}"
                , status = Just ItemCompleted
                }
            blockingCall = FunctionCall
                { itemId = Just "blocking-item"
                , callId = "blocking-call"
                , name = "read_file"
                , namespace = Nothing
                , provider = Nothing
                , arguments = "{\"path\":\"README.md\"}"
                , encryptedFunctionArgs = Nothing
                , status = Just ItemCompleted
                , async = Nothing
                }
            send _params onStreamEvent = do
                onStreamEvent ResponseOutputItemAddedEvent
                    { item = FunctionCallItem partialCall
                    , outputIndex = Just 0
                    , sequenceNumber = Just 1
                    }
                onStreamEvent ResponseFunctionCallArgumentsDeltaEvent
                    { delta = Just "{\"path\":\"README.md\"}"
                    , streamItemId = Just "async-item"
                    , streamOutputIndex = Just 0
                    , sequenceNumber = Just 2
                    }
                onStreamEvent ResponseOutputItemDoneEvent
                    { item = FunctionCallItem completedCall
                    , outputIndex = Just 0
                    , sequenceNumber = Just 3
                    }
                onStreamEvent ResponseOutputItemDoneEvent
                    { item = FunctionCallItem blockingCall
                    , outputIndex = Just 1
                    , sequenceNumber = Just 4
                    }
                pure (Left (ConnectionError "stop after calls"))
            backend =
                statelessResponsesBackend send
                    (pure defaultResponseCreateParams)

        result <- backend.submitTurnWithCallbacks
            emptyBackendSnapshot
            Nothing
            [UserMessage "hello"]
            BackendCallbacks
                { onLoopEvent = const (pure ())
                , onAsyncToolCall =
                    \call -> modifyIORef' announced (<> [call])
                }

        result `shouldBe` Left (ConnectionError "stop after calls")
        calls <- readIORef announced
        map (.callId) calls `shouldBe` ["async-call"]
        map toolCallMode calls `shouldBe` [AsyncToolCall]
        map (.arguments) calls `shouldBe` ["{\"path\":\"README.md\"}"]

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

        _ <- backend.submitTurn emptyBackendSnapshot Nothing [UserMessage "hello"]
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

        _ <- backend.submitTurn emptyBackendSnapshot Nothing [UserMessage "hello"]
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
                , async = Nothing

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
                    Aeson.toJSON output `shouldBe` Aeson.object
                        [ "type" Aeson..= ("input_image" :: Text.Text)
                        , "image_url" Aeson..=
                            ("data:image/png;base64,AA==" :: Text.Text)
                        ]
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
                , async = Nothing
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
                , async = Nothing
                }
            customOutput = CustomToolCallOutputItem CustomToolCallOutput
                { itemId = Nothing
                , callId = "call-2"
                , name = Nothing
                , output = rawJsonFromEncoding
                    (Aeson.toEncoding ("ok" :: Text.Text))
                , status = Just ItemCompleted
                , async = Nothing
                }
            customCall = CustomToolCallItem CustomToolCall
                { itemId = Just "ctc-1"
                , callId = "call-2"
                , name = "apply_patch"
                , namespace = Nothing
                , input = "*** Begin Patch"
                , status = Just ItemCompleted
                , async = Nothing
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

-- | Streamed tool calls are announced immediately. Safe argument deltas
-- repaint the call, while sensitive tools retain coarse activity updates.
streamProjectionSpec :: Spec
streamProjectionSpec = describe "newStreamEventToLoopEvents" do
    it "starts a pure projection attempt without retaining reused tool ids" do
        let (firstState, _) =
                streamEventToLoopEventsStep False
                    emptyStreamProjectionState
                    (functionCallAdded "fc-1" "call-1" "shell_command")
            (_, firstEvents) =
                streamEventToLoopEventsStep False firstState
                    (argumentsDelta "fc-1" "{\"command\":\"pwd\"}")
            (secondState, _) =
                streamEventToLoopEventsStep False
                    emptyStreamProjectionState
                    (functionCallAdded "fc-1" "call-2" "apply_patch")
            (_, secondEvents) =
                streamEventToLoopEventsStep False secondState
                    (argumentsDelta "fc-1" "{}")
        firstEvents `shouldBe`
            [ ToolArgumentsUpdated
                (functionToolCall
                    "call-1"
                    "shell_command"
                    "{\"command\":\"pwd\"}")
            ]
        secondEvents `shouldSatisfy` \case
            [ToolArgumentsUpdated call] ->
                call.callId == "call-2" && call.name == "apply_patch"
            _ -> False

    it "leaves Codex capacity display to the authoritative usage endpoint" do
        projectEvent <- newStreamEventToLoopEvents False
        events <- projectEvent ResponseCodexRateLimitsEvent
            { rateLimits = CodexRateLimits
                { allowed = Just True
                , limitReached = Just False
                , primaryUsedPercent = Just 12
                , secondaryUsedPercent = Just 79
                }
            , sequenceNumber = Nothing
            }
        events `shouldBe` []

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

    it "batches a long shell tail and flushes it when arguments finish" do
        projectEvent <- newStreamEventToLoopEvents False
        _ <- projectEvent (functionCallAdded "fc-1" "call-1" "shell_command")
        let firstCommand = Text.replicate 116 "a"
            firstArguments = "{\"command\":\"" <> firstCommand
            batchedSuffix = Text.replicate 63 "b" <> "c"
            tailSuffix = "tail"
            completeCommand = firstCommand <> batchedSuffix <> tailSuffix
            completeArguments =
                "{\"command\":\"" <> completeCommand <> "\"}"
        first <- projectEvent (argumentsDelta "fc-1" firstArguments)
        first `shouldBe`
            [ ToolArgumentsUpdated
                (functionToolCall
                    "call-1"
                    "shell_command"
                    ("{\"command\":\"" <> firstCommand <> "\"}"))
            ]
        quiet <- projectEvent
            (argumentsDelta "fc-1" (Text.replicate 63 "b"))
        quiet `shouldBe` []
        batched <- projectEvent (argumentsDelta "fc-1" "c")
        batched `shouldBe`
            [ ToolArgumentsUpdated
                (functionToolCall
                    "call-1"
                    "shell_command"
                    ( "{\"command\":\""
                        <> firstCommand
                        <> batchedSuffix
                        <> "\"}"
                    ))
            ]
        tailEvents <- projectEvent
            (argumentsDelta "fc-1" (tailSuffix <> "\"}"))
        tailEvents `shouldBe` []
        flushed <- projectEvent
            (functionArgumentsDone
                (Just "fc-1")
                (Just 0)
                (Just completeArguments))
        flushed `shouldBe`
            [ ToolArgumentsUpdated
                (functionToolCall
                    "call-1"
                    "shell_command"
                    completeArguments)
            ]

    it "repaints an ordinary JSON tool call as its arguments arrive" do
        projectEvent <- newStreamEventToLoopEvents False
        _ <- projectEvent (functionCallAdded "fc-1" "call-1" "read_file")
        first <- projectEvent
            (argumentsDelta "fc-1" "{\"target_file\":\"src/Ma")
        first `shouldBe`
            [ ToolArgumentsUpdated
                (functionToolCall
                    "call-1"
                    "read_file"
                    "{\"target_file\":\"src/Ma")
            ]
        second <- projectEvent (argumentsDelta "fc-1" "in.hs\"}")
        second `shouldBe`
            [ ToolArgumentsUpdated
                (functionToolCall
                    "call-1"
                    "read_file"
                    "{\"target_file\":\"src/Main.hs\"}")
            ]

    it "routes parallel argument streams by output index" do
        projectEvent <- newStreamEventToLoopEvents False
        _ <- projectEvent
            (functionCallAddedAt 0 "fc-read" "call-read" "read_file")
        _ <- projectEvent
            (functionCallAddedAt 1 "fc-grep" "call-grep" "grep")
        events <- projectEvent
            (argumentsDeltaAt
                Nothing
                (Just 0)
                "{\"target_file\":\"src/Main.hs\"}")
        events `shouldBe`
            [ ToolArgumentsUpdated
                (functionToolCall
                    "call-read"
                    "read_file"
                    "{\"target_file\":\"src/Main.hs\"}")
            ]

    it "publishes a structured prefix eagerly, then batches tiny deltas" do
        projectEvent <- newStreamEventToLoopEvents False
        _ <- projectEvent (functionCallAdded "fc-1" "call-1" "read_file")
        batches <- mapM
            (\_ -> projectEvent (argumentsDelta "fc-1" "x"))
            [1 :: Int .. 8064]
        let previews =
                [ call.arguments
                | ToolArgumentsUpdated call <- concat batches
                ]
        take 3 previews `shouldBe` ["x", "xx", "xxx"]
        length previews `shouldSatisfy` (< 170)
        last previews `shouldBe` Text.replicate 8064 "x"

    it "flushes a pending function argument tail on arguments done" do
        projectEvent <- newStreamEventToLoopEvents False
        _ <- projectEvent (functionCallAdded "fc-1" "call-1" "read_file")
        let prefix = Text.replicate 128 "p"
            complete = prefix <> "canonical"
        _ <- projectEvent (argumentsDelta "fc-1" prefix)
        quiet <- projectEvent (argumentsDelta "fc-1" "tail")
        quiet `shouldBe` []
        flushed <- projectEvent
            (functionArgumentsDone (Just "fc-1") (Just 0) (Just complete))
        flushed `shouldBe`
            [ ToolArgumentsUpdated
                (functionToolCall "call-1" "read_file" complete)
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

    it "preserves streamed arguments across a sparse function done item" do
        projectEvent <- newStreamEventToLoopEvents False
        _ <- projectEvent (functionCallAdded "fc-1" "call-1" "read_file")
        let prefix = Text.replicate 128 "p"
            tailText = "tail"
        _ <- projectEvent (argumentsDelta "fc-1" prefix)
        quiet <- projectEvent (argumentsDelta "fc-1" tailText)
        quiet `shouldBe` []
        events <- projectEvent
            (functionCallDone "fc-1" "call-1" "read_file" "")
        events `shouldBe`
            [ ToolUpdated
                (functionToolCall "call-1" "read_file" (prefix <> tailText))
            ]

    it "publishes a streamed custom tool call immediately" do
        projectEvent <- newStreamEventToLoopEvents False
        events <- projectEvent
            (customToolCallAdded "ct-1" "call-9" "apply_patch")
        events `shouldBe`
            [ToolStarted (customToolCall "call-9" "apply_patch" "")]

    it "repaints streamed apply_patch input as it arrives" do
        projectEvent <- newStreamEventToLoopEvents False
        _ <- projectEvent
            (customToolCallAdded "ct-1" "call-9" "apply_patch")
        let prefix = "*** Begin Patch\n*** Update File: A.hs\n"
        first <- projectEvent
            (customInputDelta "ct-1" "call-9" prefix)
        first `shouldBe`
            [ ToolArgumentsUpdated
                (customToolCall
                    "call-9"
                    "apply_patch"
                    prefix)
            ]
        let continuation =
                "@@\n-old\n+new\n" <> Text.replicate 256 "x"
        second <- projectEvent
            (customInputDelta "ct-1" "call-9" continuation)
        second `shouldBe`
            [ ToolArgumentsUpdated
                (customToolCall
                    "call-9"
                    "apply_patch"
                    (prefix <> continuation))
            ]

    it "batches tiny apply_patch deltas without losing their content" do
        projectEvent <- newStreamEventToLoopEvents False
        _ <- projectEvent
            (customToolCallAdded "ct-1" "call-9" "apply_patch")
        batches <- mapM
            (\_ -> projectEvent (customInputDelta "ct-1" "call-9" "x"))
            [1 :: Int .. 8193]
        let previews =
                [ call.arguments
                | ToolArgumentsUpdated call <- concat batches
                ]
        length previews `shouldSatisfy` (< 40)
        last previews `shouldBe` Text.replicate 8193 "x"

    it "bounds a live apply_patch preview" do
        projectEvent <- newStreamEventToLoopEvents False
        _ <- projectEvent
            (customToolCallAdded "ct-1" "call-9" "apply_patch")
        let retained = Text.replicate (64 * 1024) "p"
        first <- projectEvent
            (customInputDelta
                "ct-1"
                "call-9"
                (retained <> "not retained"))
        first `shouldBe`
            [ ToolArgumentsUpdated
                (customToolCall "call-9" "apply_patch" retained)
            ]
        second <- projectEvent
            (customInputDelta "ct-1" "call-9" "still not retained")
        second `shouldBe` []

    it "flushes accumulated custom input when input done omits it" do
        projectEvent <- newStreamEventToLoopEvents False
        _ <- projectEvent
            (customToolCallAdded "ct-1" "call-9" "apply_patch")
        let prefix = "*** Begin Patch\n"
            tailText = "*** End Patch\n"
        _ <- projectEvent (customInputDelta "ct-1" "call-9" prefix)
        quiet <- projectEvent
            (customInputDelta "ct-1" "call-9" tailText)
        quiet `shouldBe` []
        flushed <- projectEvent
            (customInputStreamDone
                (Just "ct-1")
                (Just "call-9")
                (Just 0)
                Nothing)
        flushed `shouldBe`
            [ ToolArgumentsUpdated
                (customToolCall
                    "call-9"
                    "apply_patch"
                    (prefix <> tailText))
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
            [ ToolUpdated
                (customToolCall "call-9" "apply_patch" "*** Begin Patch")
            ]

    it "preserves streamed input across a sparse custom done item" do
        projectEvent <- newStreamEventToLoopEvents False
        _ <- projectEvent
            (customToolCallAdded "ct-1" "call-9" "apply_patch")
        let prefix = "*** Begin Patch\n"
            tailText = "*** End Patch\n"
        _ <- projectEvent (customInputDelta "ct-1" "call-9" prefix)
        quiet <- projectEvent
            (customInputDelta "ct-1" "call-9" tailText)
        quiet `shouldBe` []
        events <- projectEvent
            (customToolCallDone "ct-1" "call-9" "apply_patch" "")
        events `shouldBe`
            [ ToolUpdated
                (customToolCall
                    "call-9"
                    "apply_patch"
                    (prefix <> tailText))
            ]

    it "retains the ordinary done projection for native computer calls" do
        projectEvent <- newStreamEventToLoopEvents False
        let item = ComputerCallItem ComputerCall
                { computerCallItemId = Just "native-item"
                , computerCallId = "native-call"
                , computerActions = [TypeAction "secret"]
                , pendingSafetyChecks = []
                , computerCallStatus = Just ItemCompleted
                , computerCallExtra = KeyMap.empty
                }
        events <- projectEvent ResponseOutputItemDoneEvent
            { item
            , outputIndex = Just 0
            , sequenceNumber = Just 2
            }
        case responseItemToToolCall item of
            Just call -> events `shouldBe` [ToolUpdated call]
            Nothing -> expectationFailure "native computer call was not projected"

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

    it "repaints streamed custom tool input" do
        projectEvent <- newStreamEventToLoopEvents False
        _ <- projectEvent
            (customToolCallAdded "ct-1" "call-9" "large_custom_tool")
        let arguments = Text.replicate 10000 "p"
        loud <- projectEvent
            (customInputDelta "ct-1" "call-9" arguments)
        loud `shouldBe`
            [ ToolArgumentsUpdated
                (customToolCall
                    "call-9"
                    "large_custom_tool"
                    arguments)
            ]

    it "keeps sensitive computer arguments out of live previews" do
        projectEvent <- newStreamEventToLoopEvents False
        _ <- projectEvent
            (functionCallAdded "fc-1" "call-1" computerFunctionName)
        loud <- projectEvent
            (argumentsDelta "fc-1" (Text.replicate 10000 "s"))
        loud `shouldBe`
            [ ActivityUpdated
                ("Writing " <> computerFunctionName <> " call… (10k chars)")
            ]

    it "keeps encrypted collaboration arguments out of live previews" do
        projectEvent <- newStreamEventToLoopEvents False
        _ <- projectEvent encryptedCollaborationCallAdded
        loud <- projectEvent
            (argumentsDelta "fc-secret" (Text.replicate 10000 "s"))
        loud `shouldBe`
            [ ActivityUpdated
                "Writing collaboration.spawn_agent call… (10k chars)"
            ]

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

customToolCall :: Text.Text -> Text.Text -> Text.Text -> ToolCall
customToolCall customCallId customName customInput = ToolCall
    { callId = customCallId
    , name = customName
    , arguments = customInput
    , callKind = CustomCallKind
    , argumentsEncrypted = False
    }

functionCallAdded :: Text.Text -> Text.Text -> Text.Text -> ResponseStreamEvent
functionCallAdded functionItemId functionCallId functionName =
    functionCallAddedAt 0 functionItemId functionCallId functionName

functionCallAddedAt
    :: Int
    -> Text.Text
    -> Text.Text
    -> Text.Text
    -> ResponseStreamEvent
functionCallAddedAt index functionItemId functionCallId functionName =
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
            , async = Nothing

            }
        , outputIndex = Just index
        , sequenceNumber = Just 1

        }

encryptedCollaborationCallAdded :: ResponseStreamEvent
encryptedCollaborationCallAdded =
    ResponseOutputItemAddedEvent
        { item = FunctionCallItem FunctionCall
            { itemId = Just "fc-secret"
            , callId = "call-secret"
            , name = "spawn_agent"
            , namespace = Just "collaboration"
            , provider = Nothing
            , arguments = ""
            , encryptedFunctionArgs = Just ["message"]
            , status = Nothing
            , async = Nothing

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
            , async = Nothing

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
            , async = Nothing

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
            , async = Nothing

            }
        , outputIndex = Just 0
        , sequenceNumber = Just 2

        }

argumentsDelta :: Text.Text -> Text.Text -> ResponseStreamEvent
argumentsDelta deltaItemId deltaText =
    argumentsDeltaAt (Just deltaItemId) (Just 0) deltaText

argumentsDeltaAt
    :: Maybe Text.Text
    -> Maybe Int
    -> Text.Text
    -> ResponseStreamEvent
argumentsDeltaAt deltaItemId outputIndex deltaText =
    ResponseFunctionCallArgumentsDeltaEvent
        { delta = Just deltaText
        , streamItemId = deltaItemId
        , streamOutputIndex = outputIndex
        , sequenceNumber = Nothing

        }

functionArgumentsDone
    :: Maybe Text.Text
    -> Maybe Int
    -> Maybe Text.Text
    -> ResponseStreamEvent
functionArgumentsDone deltaItemId outputIndex functionArguments =
    ResponseFunctionCallArgumentsDoneEvent
        { arguments = functionArguments
        , functionName = Nothing
        , streamItemId = deltaItemId
        , streamOutputIndex = outputIndex
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

customInputStreamDone
    :: Maybe Text.Text
    -> Maybe Text.Text
    -> Maybe Int
    -> Maybe Text.Text
    -> ResponseStreamEvent
customInputStreamDone deltaItemId deltaCallId outputIndex completeInput =
    ResponseCustomToolInputDoneEvent
        { inputText = completeInput
        , streamItemId = deltaItemId
        , streamCallId = deltaCallId
        , streamOutputIndex = outputIndex
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

isUserMessage :: ResponseItem -> Bool
isUserMessage = \case
    MessageItem message ->
        message.role == RoleUser
            && case message.content of
                MessageContentParts [InputTextPart{ text = value }] ->
                    value == "hello"
                _ -> False
    _ -> False

responseWithOutput :: [ResponseItem] -> Response
responseWithOutput output =
    either (error . Text.unpack) id
        . Codec.decodeResponse
        . LBS.toStrict
        . Aeson.encode $
        Aeson.object
            [ "id" Aeson..= ("resp-compacted" :: Text.Text)
            , "created_at" Aeson..= (0 :: Int)
            , "model" Aeson..= ("grok-4.6" :: Text.Text)
            , "status" Aeson..= ("completed" :: Text.Text)
            , "output" Aeson..= output
            ]

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
