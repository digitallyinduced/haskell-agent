module Agent.OpenAI.LoopBackendSpec (spec) where

import Agent.Error (ApiError(..), ErrorType(..))
import Agent.InterAgentMessage
import Agent.Loop
import Agent.OpenAI.LoopBackend
import Agent.Responses.Types
import Agent.ToolDispatch
import Control.Retry (constantDelay, limitRetries)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.IORef
import Data.Text (Text)
import Test.Hspec

spec :: Spec
spec = do
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

        it "ignores empty deltas and unrelated events" do
            streamEventToLoopEvent (deltaEvent EventOutputTextDelta "")
                `shouldBe` Nothing
            streamEventToLoopEvent (deltaEvent EventOutputTextDone "done")
                `shouldBe` Nothing
            streamEventToLoopEvent (ResponseOutputItemDoneEvent
                { item = assistantItem "x"
                , outputIndex = 0
                , sequenceNumber = Nothing
                , eventExtraFields = KeyMap.empty
                }) `shouldBe` Nothing

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
                    transcript

            first <- backend.submitTurn Nothing [UserMessage "read it"]
                (modifyIORef' events . (:))
            first `shouldBe` Right (emptyTurnOutput "resp-1"
                [functionToolCall "c1" "read_file" "{\"target_file\":\"README.md\"}"]
                Nothing)

            second <- backend.submitTurn (Just "resp-1")
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
                backend = statelessResponsesBackend send (pure baseParams) transcript

            failed <- backend.submitTurn Nothing [UserMessage "hi"] (const (pure ()))
            failed `shouldBe` Left (ConnectionError "boom")

            recovered <- backend.submitTurn Nothing [UserMessage "hi"] (const (pure ()))
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
                    transcript
            _ <- backend.submitTurn Nothing [UserMessage "one"] (const (pure ()))
            writeIORef paramsRef (withEffort "high" baseParams)
            _ <- backend.submitTurn (Just "resp-1") [UserMessage "two"]
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
            let backend = openAiBackendWith (recordingSend seen) (pure baseParams) transcript
            result <- backend.submitTurn (Just "resp-prev")
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
            let backend = openAiBackendWith (recordingSend seen) (pure baseParams) transcript
            _ <- backend.submitTurn Nothing [UserMessage "one"] (const (pure ()))
            _ <- backend.submitTurn (Just "resp-1")
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
            let backend = openAiBackendWith (recordingSend seen) (readIORef paramsRef) transcript
            _ <- backend.submitTurn Nothing [UserMessage "one"] (const (pure ()))
            writeIORef paramsRef (withEffort "high" baseParams)
            _ <- backend.submitTurn (Just "resp-1") [UserMessage "two"] (const (pure ()))
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
                backend = openAiBackendWith send (pure baseParams) transcript
            result <- backend.submitTurn (Just "resp-missing")
                [UserMessage "new"]
                (const (pure ()))
            result `shouldBe` Right (emptyTurnOutput "resp-2" [] (Just "ok"))
            requests <- readIORef seen
            map snd requests `shouldBe` [Just "resp-missing", Nothing]
            map (inputItems . fst) requests `shouldBe`
                [ turnInputsToItems [UserMessage "new"]
                , seed <> turnInputsToItems [UserMessage "new"]
                ]

        it "retries transient Codex server errors before visible output" do
            attempts <- newIORef (0 :: Int)
            transcript <- newIORef []
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
                    transcript
            result <- backend.submitTurn Nothing [UserMessage "one"] (const (pure ()))
            result `shouldBe` Right (emptyTurnOutput "resp-retried" [] (Just "ok"))
            readIORef attempts `shouldReturn` 3

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
                    transcript
            result <- backend.submitTurn Nothing [UserMessage "one"]
                (modifyIORef' events . (:))
            result `shouldBe` Left serverError
            readIORef attempts `shouldReturn` 1
            observedEvents <- readIORef events
            reverse observedEvents `shouldBe` [TextDelta "partial"]

    describe "openAiBackendWithConnectionRecovery" do
        it "replays on a fresh connection when the reusable socket dies before output" do
            currentCalls <- newIORef (0 :: Int)
            freshCalls <- newIORef (0 :: Int)
            healthy <- newIORef True
            transcript <- newIORef []
            let sendCurrent _request _previous _onEvent = do
                    modifyIORef' currentCalls (+ 1)
                    pure $ Left $ ConnectionError "socket closed"
                sendFresh _request _previous _onEvent = do
                    modifyIORef' freshCalls (+ 1)
                    pure $ Right (testResponse "resp-fresh" [assistantItem "ok"])
                backend = openAiBackendWithConnectionRecovery
                    healthy sendCurrent sendFresh (pure baseParams) transcript
            first <- backend.submitTurn Nothing [UserMessage "one"] (const (pure ()))
            second <- backend.submitTurn (Just "resp-fresh")
                [UserMessage "two"] (const (pure ()))
            first `shouldBe` Right (emptyTurnOutput "resp-fresh" [] (Just "ok"))
            second `shouldBe` Right (emptyTurnOutput "resp-fresh" [] (Just "ok"))
            readIORef healthy `shouldReturn` False
            readIORef currentCalls `shouldReturn` 1
            readIORef freshCalls `shouldReturn` 2

        it "does not replay after loop-visible output was already streamed" do
            freshCalls <- newIORef (0 :: Int)
            healthy <- newIORef True
            transcript <- newIORef []
            events <- newIORef []
            let sendCurrent _request _previous onEvent = do
                    onEvent (deltaEvent EventOutputTextDelta "partial")
                    pure $ Left $ ConnectionError "socket closed"
                sendFresh _request _previous _onEvent = do
                    modifyIORef' freshCalls (+ 1)
                    pure $ Right (testResponse "resp-fresh" [assistantItem "ok"])
                streamingBackend = openAiBackendWithConnectionRecovery
                    healthy sendCurrent sendFresh (pure baseParams) transcript
            result <- streamingBackend.submitTurn Nothing [UserMessage "one"]
                (modifyIORef' events . (:))
            result `shouldBe` Left (ConnectionError "socket closed")
            reverse <$> readIORef events `shouldReturn` [TextDelta "partial"]
            readIORef healthy `shouldReturn` False
            readIORef freshCalls `shouldReturn` 0

        it "does not treat provider errors as a dead connection" do
            freshCalls <- newIORef (0 :: Int)
            healthy <- newIORef True
            transcript <- newIORef []
            let sendCurrent _request _previous _onEvent =
                    pure $ Left $ ProviderError InvalidRequestError "bad request" Nothing
                sendFresh _request _previous _onEvent = do
                    modifyIORef' freshCalls (+ 1)
                    pure $ Right (testResponse "resp-fresh" [assistantItem "ok"])
                providerErrorBackend = openAiBackendWithConnectionRecovery
                    healthy sendCurrent sendFresh (pure baseParams) transcript
            result <- providerErrorBackend.submitTurn Nothing
                [UserMessage "one"] (const (pure ()))
            result `shouldBe` Left (ProviderError InvalidRequestError "bad request" Nothing)
            readIORef healthy `shouldReturn` True
            readIORef freshCalls `shouldReturn` 0

--------------------------------------------------------------------------------
-- Fixtures
--------------------------------------------------------------------------------

baseParams :: ResponseCreateParams
baseParams = defaultResponseCreateParams { model = Just "gpt-5.6-luna" }

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

deltaEvent :: StreamEventType -> Text -> ResponseStreamEvent
deltaEvent otherEventType delta = OtherResponseStreamEvent
    { otherEventType
    , sequenceNumber = Nothing
    , eventExtraFields = KeyMap.fromList [(Key.fromText "delta", Aeson.String delta)]
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
