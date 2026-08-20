module Agent.OpenAI.LoopBackendSpec (spec) where

import Agent.Error (ApiError(..), ErrorType(..))
import Agent.Loop
import Agent.OpenAI.LoopBackend
import Agent.OpenAI.Responses.Types
import Agent.ToolDispatch
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
            turn `shouldBe` TurnOutput
                { responseId = "resp-9"
                , toolCalls =
                    [ functionToolCall "fc1" "shell_command" "{\"command\":\"ls\"}"
                    , customToolCall "cc1" "apply_patch" "*** Begin Patch\n*** End Patch"
                    ]
                , assistantText = Just "working"
                }

        it "joins multiple assistant messages" do
            let turn = responseToTurnOutput $ testResponse "resp-text"
                    [ assistantItem "first"
                    , functionCallItem "fc1" "echo" "{}"
                    , assistantItem "second"
                    ]
            turn.assistantText `shouldBe` Just "first\nsecond"

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

    describe "openAiBackendWith" do
        it "sends only the new items and threads previous_response_id" do
            seen <- newIORef []
            events <- newIORef []
            transcript <- newIORef []
            let backend = openAiBackendWith (recordingSend seen) (pure baseParams) transcript
            result <- backend.submitTurn (Just "resp-prev")
                [UserMessage "hello"]
                (modifyIORef' events . (:))
            result `shouldBe` Right TurnOutput
                { responseId = "resp-1"
                , toolCalls = []
                , assistantText = Just "ok"
                }
            [(request, previous)] <- readIORef seen
            previous `shouldBe` Just "resp-prev"
            request.model `shouldBe` Just "gpt-5.1-codex"
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
            result `shouldBe` Right TurnOutput
                { responseId = "resp-2"
                , toolCalls = []
                , assistantText = Just "ok"
                }
            requests <- readIORef seen
            map snd requests `shouldBe` [Just "resp-missing", Nothing]
            map (inputItems . fst) requests `shouldBe`
                [ turnInputsToItems [UserMessage "new"]
                , seed <> turnInputsToItems [UserMessage "new"]
                ]

--------------------------------------------------------------------------------
-- Fixtures
--------------------------------------------------------------------------------

baseParams :: ResponseCreateParams
baseParams = defaultResponseCreateParams { model = Just "gpt-5.1-codex" }

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
functionCallItem callId name arguments = FunctionCallItem FunctionCall
    { itemId = Nothing
    , callId
    , name
    , arguments
    , status = Just ItemCompleted
    , extraFields = KeyMap.empty
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
testResponse responseId output = case Aeson.fromJSON $ Aeson.object
    [ "id" Aeson..= responseId
    , "created_at" Aeson..= (0 :: Int)
    , "model" Aeson..= ("test-model" :: Text)
    , "status" Aeson..= ("completed" :: Text)
    , "output" Aeson..= output
    ] of
    Aeson.Success response -> response
    Aeson.Error err -> error err

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
