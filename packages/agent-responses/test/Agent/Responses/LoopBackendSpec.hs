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
import Agent.Responses.LoopBackend
    ( statelessResponsesBackend
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
    , FunctionCall(..)
    , FunctionCallOutput(..)
    , InternalChatMetadata(..)
    , ItemStatus(..)
    , ResponseContentPart(..)
    , ResponseItem(..)
    , ResponseMessage(..)
    , ResponseRole(..)
    , SafetyCheck(..)
    , ResponseStreamEvent(..)
    , StreamEventType(..)
    , ResponseInput(..)
    , ResponseCreateParams(..)
    , TaggedObject(..)
    , computerFunctionName
    , computerFunctionNamespace
    , defaultResponseCreateParams
    , legacyComputerFunctionName
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
spec = describe "tokenProviderStatelessResponsesBackend" do
    it "routes the ordinary computer function to the computer harness" do
        let call = FunctionCall
                { itemId = Nothing
                , callId = "call-function"
                , name = computerFunctionName
                , namespace = Just "functions"
                , arguments =
                    "{\"actions\":[{\"type\":\"type\",\"text\":\"secret\"}]}"
                , encryptedFunctionArgs = Nothing
                , status = Nothing
                , extraFields = KeyMap.empty
                }
        case responseItemToToolCall (FunctionCallItem call) of
            Just projected -> do
                projected.name `shouldBe` "computer"
                projected.callKind `shouldBe` ComputerFunctionCallKind
                projected.argumentsEncrypted `shouldBe` True
            Nothing -> expectationFailure
                "computer function was not routed"

    it "routes the standard Responses computer function without a namespace" do
        let call = FunctionCall
                { itemId = Nothing
                , callId = "call-standard"
                , name = computerFunctionName
                , namespace = Nothing
                , arguments = "{\"actions\":[{\"type\":\"screenshot\"}]}"
                , encryptedFunctionArgs = Nothing
                , status = Nothing
                , extraFields = KeyMap.empty
                }
        case responseItemToToolCall (FunctionCallItem call) of
            Just projected -> do
                projected.name `shouldBe` "computer"
                projected.callKind `shouldBe` ComputerFunctionCallKind
            Nothing -> expectationFailure
                "standard computer function was not routed"

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
                { callId = "call-function"
                , output = encoded
                , callKind = ComputerFunctionCallKind
                }

        case turnInputsToItems [CompletedTool result] of
            [FunctionCallOutputItem output, MessageItem observation] -> do
                output.output `shouldBe`
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
                    first.output `shouldBe`
                        Aeson.String "Computer action completed."
                    second.output `shouldBe`
                        Aeson.String
                            "Computer input failed after changing the UI."
            other -> expectationFailure
                ("stale screenshot was reused: " <> show other)

    it "keeps computer continuation ordering in standard and Lite requests" do
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
            additional = UnknownResponseItem TaggedObject
                { tag = "additional_tools"
                , fields = KeyMap.empty
                }
            lite = withRequestInput
                defaultResponseCreateParams
                    { input = Just (ResponseInputItems [additional])
                    }
                inputs
        case standard.input of
            Just (ResponseInputItems
                [ FunctionCallOutputItem FunctionCallOutput{output}
                , MessageItem ResponseMessage
                    { role = RoleUser
                    , content = MessageContentParts
                        [InputTextPart{}, InputImagePart{detail}]
                    }
                ]) -> do
                    output `shouldBe`
                        Aeson.String "Computer action completed."
                    detail `shouldBe` Just "auto"
            other -> expectationFailure
                ("unexpected standard continuation: " <> show other)
        case lite.input of
            Just (ResponseInputItems
                [ _
                , FunctionCallOutputItem FunctionCallOutput{output}
                , MessageItem ResponseMessage
                    { role = RoleUser
                    , content = MessageContentParts
                        [InputTextPart{}, InputImagePart{detail}]
                    }
                ]) -> do
                    output `shouldBe`
                        Aeson.String "Computer action completed."
                    detail `shouldBe` Nothing
            other -> expectationFailure
                ("unexpected Lite continuation: " <> show other)

    it "normalizes legacy native computer calls to ordinary function output" do
        let call = ComputerCall
                { computerCallItemId = Just "item-1"
                , computerCallId = "call-1"
                , computerActions =
                    [ ClickAction 20 30 "left" []
                    , TypeAction "secret"
                    ]
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
        let encoded = TextEncoding.decodeUtf8 $ LBS.toStrict $ Aeson.encode
                ComputerCallOutput
                    { computerOutputItemId = Nothing
                    , computerOutputCallId = "ignored"
                    , screenshotDataUrl = "data:image/png;base64,AA=="
                    , acknowledgedChecks = []
                    , computerOutputStatus = Nothing
                    , computerOutputExtra = KeyMap.empty
                    }
        case turnInputsToItems
                [ CompletedTool ToolCallResult
                    { callId = "call-1"
                    , output = encoded
                    , callKind = ComputerCallKind
                    }
                ] of
            [FunctionCallOutputItem output, MessageItem observation] -> do
                output.callId `shouldBe` "call-1"
                output.output `shouldBe`
                    Aeson.String "Computer action completed."
                case observation.content of
                    MessageContentParts
                        [InputTextPart{}, InputImagePart{imageUrl}] ->
                            imageUrl `shouldBe`
                                Just "data:image/png;base64,AA=="
                    other -> expectationFailure
                        ("expected legacy screenshot observation, got "
                            <> show other)
            other -> expectationFailure
                ("unexpected normalized output: " <> show other)

    it "returns rejected legacy computer calls as ordinary text output" do
        case toolResultToItem ToolCallResult
                { callId = "call-failed"
                , output = "Tool call rejected by user."
                , callKind = ComputerCallKind
                } of
            FunctionCallOutputItem output -> do
                output.callId `shouldBe` "call-failed"
                output.output `shouldBe`
                    Aeson.String "Tool call rejected by user."
            other -> expectationFailure ("unexpected output: " <> show other)

    it "uses the durationless computer wait action shape" do
        Aeson.toJSON WaitAction `shouldBe`
            Aeson.object ["type" Aeson..= ("wait" :: Text.Text)]

    it "preserves unknown native computer protocol fields" do
        let safety = SafetyCheck
                { safetyCheckId = "safe-1"
                , safetyCheckCode = Just "confirm"
                , safetyCheckMessage = Just "Confirm account change"
                , safetyCheckExtra =
                    KeyMap.singleton "provider_safety"
                        (Aeson.String "retained")
                }
            call = ComputerCall
                { computerCallItemId = Just "item-1"
                , computerCallId = "call-1"
                , computerActions = [ClickAction 20 30 "right" ["shift"]]
                , pendingSafetyChecks = [safety]
                , computerCallStatus = Nothing
                , computerCallExtra =
                    KeyMap.singleton "provider_call"
                        (Aeson.String "retained")
                }
            output = ComputerCallOutput
                { computerOutputItemId = Just "output-1"
                , computerOutputCallId = "call-1"
                , screenshotDataUrl = "data:image/png;base64,AA=="
                , acknowledgedChecks = [safety]
                , computerOutputStatus = Nothing
                , computerOutputExtra =
                    KeyMap.singleton "provider_output"
                        (Aeson.String "retained")
                }
        Aeson.eitherDecode (Aeson.encode call) `shouldBe` Right call
        Aeson.eitherDecode (Aeson.encode output) `shouldBe` Right output

    it "does not text-truncate large computer screenshot continuations" do
        let screenshot =
                "data:image/png;base64," <> Text.replicate (2 * 1024 * 1024) "A"
            encoded = TextEncoding.decodeUtf8 $ LBS.toStrict $ Aeson.encode
                ComputerCallOutput
                    { computerOutputItemId = Nothing
                    , computerOutputCallId = "ignored"
                    , screenshotDataUrl = screenshot
                    , acknowledgedChecks = []
                    , computerOutputStatus = Nothing
                    , computerOutputExtra = KeyMap.empty
                    }
        case turnInputsToItems
                [ CompletedTool ToolCallResult
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
                    , eventExtraFields =
                        KeyMap.singleton "delta"
                            (Aeson.String "checking the implementation")
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
                    , eventExtraFields =
                        KeyMap.singleton "delta" (Aeson.String "raw")
                    }
                onStreamEvent OtherResponseStreamEvent
                    { otherEventType = EventReasoningSummaryTextDelta
                    , sequenceNumber = Just 2
                    , eventExtraFields =
                        KeyMap.singleton "delta" (Aeson.String "summary")
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
                    , eventExtraFields = KeyMap.empty
                    }
                onStreamEvent OtherResponseStreamEvent
                    { otherEventType = EventReasoningSummaryTextDelta
                    , sequenceNumber = Just 2
                    , eventExtraFields =
                        KeyMap.singleton "delta"
                            (Aeson.String "**Inspecting dependencies**")
                    }
                onStreamEvent ResponseReasoningSummaryPartAddedEvent
                    { streamItemId = Just "reasoning-1"
                    , streamOutputIndex = Just 0
                    , summaryIndex = Just 1
                    , partValue = Nothing
                    , sequenceNumber = Just 3
                    , eventExtraFields = KeyMap.empty
                    }
                onStreamEvent OtherResponseStreamEvent
                    { otherEventType = EventReasoningSummaryTextDelta
                    , sequenceNumber = Just 4
                    , eventExtraFields =
                        KeyMap.singleton "delta"
                            (Aeson.String "**Planning the fix**")
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
                , fields = KeyMap.empty
                }
            params = defaultResponseCreateParams
                { input = Just (ResponseInputItems [prefix])
                }
            request = withRequestInput params (turnInputsToItems [UserMessage "hello"])
        case request.input of
            Just (ResponseInputItems (first : second : _)) -> do
                first `shouldBe` prefix
                second `shouldSatisfy` isUserMessage
            _ -> expectationFailure "expected preserved input prefix"

    it "never re-emits legacy native computer history on the wire" do
        let call = ComputerCall
                { computerCallItemId = Just "native-item"
                , computerCallId = "native-call"
                , computerActions = [ScreenshotAction]
                , pendingSafetyChecks = []
                , computerCallStatus = Just ItemCompleted
                , computerCallExtra = KeyMap.empty
                }
            output = ComputerCallOutput
                { computerOutputItemId = Just "native-output"
                , computerOutputCallId = "native-call"
                , screenshotDataUrl = "data:image/png;base64,AA=="
                , acknowledgedChecks = []
                , computerOutputStatus = Just ItemCompleted
                , computerOutputExtra = KeyMap.empty
                }
            request = withRequestInput defaultResponseCreateParams
                [ComputerCallItem call, ComputerCallOutputItem output]
        case request.input of
            Just (ResponseInputItems
                [ FunctionCallItem function
                , FunctionCallOutputItem functionOutput
                , MessageItem ResponseMessage
                    { role = RoleUser
                    , content = MessageContentParts
                        [InputTextPart{}, InputImagePart{imageUrl}]
                    }
                ]) -> do
                    function.itemId `shouldBe` Nothing
                    function.name `shouldBe` computerFunctionName
                    function.namespace `shouldBe` Nothing
                    function.callId `shouldBe` "native-call"
                    functionOutput.itemId `shouldBe` Nothing
                    functionOutput.callId `shouldBe` "native-call"
                    functionOutput.output `shouldBe`
                        Aeson.String "Computer action completed."
                    imageUrl `shouldBe` Just "data:image/png;base64,AA=="
            other -> expectationFailure
                ("legacy computer history reached the wire: " <> show other)

    it "normalizes the earlier Lite computer fallback history" do
        let call = FunctionCall
                { itemId = Just "legacy-function-item"
                , callId = "legacy-function-call"
                , name = legacyComputerFunctionName
                , namespace = Just computerFunctionNamespace
                , arguments = "{\"actions\":[{\"type\":\"screenshot\"}]}"
                , encryptedFunctionArgs = Nothing
                , status = Just ItemCompleted
                , extraFields = KeyMap.empty
                }
            output = FunctionCallOutput
                { itemId = Just "legacy-output-item"
                , callId = "legacy-function-call"
                , name = Nothing
                , namespace = Nothing
                , output = Aeson.toJSON
                    [ Aeson.object
                        [ "type" Aeson..= ("input_image" :: Text.Text)
                        , "image_url" Aeson..=
                            ("data:image/jpeg;base64,LITE" :: Text.Text)
                        , "detail" Aeson..= ("original" :: Text.Text)
                        ]
                    ]
                , status = Nothing
                , extraFields = KeyMap.empty
                }
            request = withRequestInput defaultResponseCreateParams
                [FunctionCallItem call, FunctionCallOutputItem output]
        case request.input of
            Just (ResponseInputItems
                [ FunctionCallItem function
                , FunctionCallOutputItem functionOutput
                , MessageItem ResponseMessage
                    { role = RoleUser
                    , content = MessageContentParts
                        [InputTextPart{}, InputImagePart{imageUrl}]
                    }
                ]) -> do
                    function.itemId `shouldBe` Nothing
                    function.name `shouldBe` computerFunctionName
                    function.namespace `shouldBe` Nothing
                    functionOutput.itemId `shouldBe` Nothing
                    functionOutput.output `shouldBe`
                        Aeson.String "Computer action completed."
                    imageUrl `shouldBe`
                        Just "data:image/jpeg;base64,LITE"
            other -> expectationFailure
                ("legacy Lite computer history reached the wire: "
                    <> show other)

    it "keeps incomplete legacy computer output incomplete and image-free" do
        let output = ComputerCallOutput
                { computerOutputItemId = Just "native-output"
                , computerOutputCallId = "native-call"
                , screenshotDataUrl = "data:image/png;base64,PLACEHOLDER"
                , acknowledgedChecks = []
                , computerOutputStatus = Just ItemIncomplete
                , computerOutputExtra = KeyMap.empty
                }
            request = withRequestInput defaultResponseCreateParams
                [ComputerCallOutputItem output]
        case request.input of
            Just (ResponseInputItems [FunctionCallOutputItem functionOutput]) -> do
                functionOutput.status `shouldBe` Just ItemIncomplete
                functionOutput.output `shouldBe`
                    Aeson.String "Computer action did not complete."
            other -> expectationFailure
                ("incomplete legacy output became an observation: " <> show other)

    it "places all legacy function outputs before the final observation" do
        let call callId = ComputerCall
                { computerCallItemId = Nothing
                , computerCallId = callId
                , computerActions = [ScreenshotAction]
                , pendingSafetyChecks = []
                , computerCallStatus = Just ItemCompleted
                , computerCallExtra = KeyMap.empty
                }
            output callId screenshot = ComputerCallOutput
                { computerOutputItemId = Nothing
                , computerOutputCallId = callId
                , screenshotDataUrl = screenshot
                , acknowledgedChecks = []
                , computerOutputStatus = Just ItemCompleted
                , computerOutputExtra = KeyMap.empty
                }
            request = withRequestInput defaultResponseCreateParams
                [ ComputerCallItem (call "call-1")
                , ComputerCallItem (call "call-2")
                , ComputerCallOutputItem
                    (output "call-1" "data:image/png;base64,FIRST")
                , ComputerCallOutputItem
                    (output "call-2" "data:image/png;base64,LATEST")
                ]
        case request.input of
            Just (ResponseInputItems
                [ FunctionCallItem firstCall
                , FunctionCallItem secondCall
                , FunctionCallOutputItem firstOutput
                , FunctionCallOutputItem secondOutput
                , MessageItem ResponseMessage
                    { role = RoleUser
                    , content = MessageContentParts
                        [InputTextPart{}, InputImagePart{imageUrl}]
                    }
                ]) -> do
                    firstCall.callId `shouldBe` "call-1"
                    secondCall.callId `shouldBe` "call-2"
                    firstOutput.callId `shouldBe` "call-1"
                    secondOutput.callId `shouldBe` "call-2"
                    imageUrl `shouldBe`
                        Just "data:image/png;base64,LATEST"
            other -> expectationFailure
                ("legacy outputs were interleaved: " <> show other)

    it "replaces arbitrary prior input instead of replaying it as a prefix" do
        let stale = turnInputsToItems [UserMessage "stale"]
            fresh = turnInputsToItems [UserMessage "fresh"]
            params = defaultResponseCreateParams
                { input = Just (ResponseInputItems stale)
                }
            request = withRequestInput params fresh
        request.input `shouldBe` Just (ResponseInputItems fresh)

    it "preserves only developer items marked as base instructions" do
        let additional = UnknownResponseItem TaggedObject
                { tag = "additional_tools"
                , fields = KeyMap.empty
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
            params = defaultResponseCreateParams
                { input = Just (ResponseInputItems
                    [ additional
                    , markedDeveloper
                    , unmarkedDeveloper
                    , stale
                    ])
                }
            request = withRequestInput params fresh
        request.input `shouldBe` Just
            (ResponseInputItems (additional : markedDeveloper : fresh))

    it "strips image detail hints from Lite messages and tool outputs" do
        let additional = UnknownResponseItem TaggedObject
                { tag = "additional_tools"
                , fields = KeyMap.empty
                }
            imageMessage = MessageItem ResponseMessage
                { messageId = Nothing
                , content = MessageContentParts
                    [ InputImagePart
                        { detail = Just "high"
                        , fileId = Nothing
                        , imageUrl = Just "data:image/png;base64,AA=="
                        , promptCacheBreakpoint = Nothing
                        , extraFields = KeyMap.empty
                        }
                    ]
                , role = RoleUser
                , status = Nothing
                , phase = Nothing
                , passthrough = Nothing
                , extraFields = KeyMap.empty
                }
            toolOutput = FunctionCallOutputItem FunctionCallOutput
                { itemId = Nothing
                , callId = "call-1"
                , name = Nothing
                , namespace = Nothing
                , output = Aeson.object
                    [ "type" Aeson..= ("input_image" :: Text.Text)
                    , "detail" Aeson..= ("high" :: Text.Text)
                    , "image_url" Aeson..= ("data:image/png;base64,AA==" :: Text.Text)
                    ]
                , status = Nothing
                , extraFields = KeyMap.empty
                }
            params = defaultResponseCreateParams
                { input = Just (ResponseInputItems [additional])
                }
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
                    jsonField "detail" output `shouldBe` Nothing
            other -> expectationFailure
                ("unexpected normalized Lite input: " <> show other)

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

developerMessage :: Text.Text -> [Text.Text] -> ResponseItem
developerMessage messageText contentItemKinds =
    MessageItem ResponseMessage
        { messageId = Nothing
        , content = MessageContentParts
            [InputTextPart messageText Nothing KeyMap.empty]
        , role = RoleDeveloper
        , status = Nothing
        , phase = Nothing
        , passthrough = Just InternalChatMetadata
            { turnId = Nothing
            , createTime = Nothing
            , contentItemKinds = Just contentItemKinds
            , executedToolCalls = Nothing
            , extraFields = KeyMap.empty
            }
        , extraFields = KeyMap.empty
        }

jsonField :: Text.Text -> Aeson.Value -> Maybe Aeson.Value
jsonField fieldName = \case
    Aeson.Object object -> KeyMap.lookup (Key.fromText fieldName) object
    _ -> Nothing
