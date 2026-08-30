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
    )
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson.Key as Key
import Data.Foldable (toList)
import Data.IORef
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Agent.ToolDispatch (ToolCall(..), ToolCallKind(..), ToolCallResult(..))
import Test.Hspec

spec :: Spec
spec = describe "tokenProviderStatelessResponsesBackend" do
    it "routes the reserved computer function namespace to the computer harness" do
        let call = FunctionCall
                { itemId = Nothing
                , callId = "call-function"
                , name = computerFunctionName
                , namespace = Just computerFunctionNamespace
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
                "reserved computer function was not projected"

    it "returns reserved computer screenshots as multimodal function output" do
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

        case toolResultToItem result of
            FunctionCallOutputItem output ->
                case output.output of
                    Aeson.Array values -> case toList values of
                        [image] -> do
                            jsonField "type" image
                                `shouldBe` Just (Aeson.String "input_image")
                            jsonField "image_url" image `shouldBe`
                                Just (Aeson.String "data:image/png;base64,AA==")
                            jsonField "detail" image
                                `shouldBe` Just (Aeson.String "original")
                        other -> expectationFailure
                            ("expected one screenshot part, got " <> show other)
                    other -> expectationFailure
                        ("expected multimodal function output, got " <> show other)
            other -> expectationFailure ("unexpected output: " <> show other)

    it "round-trips native computer calls through structured screenshot output" do
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
        case toolResultToItem ToolCallResult
                { callId = "call-1"
                , output = encoded
                , callKind = ComputerCallKind
                } of
            ComputerCallOutputItem output -> do
                output.computerOutputCallId `shouldBe` "call-1"
                output.screenshotDataUrl `shouldBe` "data:image/png;base64,AA=="
            other -> expectationFailure ("unexpected output: " <> show other)

    it "marks rejected or failed computer calls incomplete" do
        case toolResultToItem ToolCallResult
                { callId = "call-failed"
                , output = "Tool call rejected by user."
                , callKind = ComputerCallKind
                } of
            ComputerCallOutputItem output -> do
                output.computerOutputCallId `shouldBe` "call-failed"
                output.computerOutputStatus `shouldBe` Just ItemIncomplete
                output.screenshotDataUrl `shouldSatisfy`
                    ("data:image/png;base64," `Text.isPrefixOf`)
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
        case toolResultToItem ToolCallResult
                { callId = "call-large"
                , output = encoded
                , callKind = ComputerCallKind
                } of
            ComputerCallOutputItem output -> do
                output.computerOutputCallId `shouldBe` "call-large"
                output.screenshotDataUrl `shouldBe` screenshot
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
