module Agent.Responses.ChatCompletionsSpec (spec) where

import Agent.Error (ApiError(..), ErrorType(InvalidRequestError))
import Agent.Responses.ChatCompletions
import Agent.Responses.Types
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = describe "buildChatRequest" do
    it "projects instructions, text input, and function tools" do
        let tool = FunctionToolValue FunctionTool
                { name = "read_file"
                , description = Just "Read a file"
                , parameters = Just $ Aeson.object
                    [ "type" Aeson..= ("object" :: String)
                    ]
                , strict = Just True
                , extraFields = KeyMap.empty
                }
            request = defaultResponseCreateParams
                { model = Just "must-not-escape"
                , instructions = Just "Be concise."
                , input = Just (ResponseInputText "Read README.md")
                , tools = Just [tool]
                , store = Just True
                , previousResponseId = Just "resp_previous"
                }
        buildChatRequest options request `shouldBe` Right (Aeson.object
            [ "model" Aeson..= ("apple-foundationmodel" :: String)
            , "messages" Aeson..=
                [ Aeson.object
                    [ "role" Aeson..= ("system" :: String)
                    , "content" Aeson..= expectedInstructions
                    ]
                , Aeson.object
                    [ "role" Aeson..= ("user" :: String)
                    , "content" Aeson..= ("Read README.md" :: String)
                    ]
                ]
            , "stream" Aeson..= False
            , "parallel_tool_calls" Aeson..= False
            , "tools" Aeson..=
                [ Aeson.object
                    [ "type" Aeson..= ("function" :: String)
                    , "function" Aeson..= Aeson.object
                        [ "name" Aeson..= ("read_file" :: String)
                        , "description" Aeson..= ("Read a file" :: String)
                        , "parameters" Aeson..= Aeson.object
                            ["type" Aeson..= ("object" :: String)]
                        , "strict" Aeson..= True
                        ]
                    ]
                ]
            ])

    it "reinforces object arguments when no base instructions are present" do
        let tool = FunctionToolValue FunctionTool
                { name = "list_dir"
                , description = Just "List a directory"
                , parameters = Just $ Aeson.object
                    [ "type" Aeson..= ("object" :: String)
                    , "properties" Aeson..= Aeson.object
                        [ "target_directory" Aeson..= Aeson.object
                            [ "type" Aeson..= ("string" :: String)
                            ]
                        ]
                    , "required" Aeson..= (["target_directory"] :: [Text])
                    ]
                , strict = Just True
                , extraFields = KeyMap.empty
                }
            request = defaultResponseCreateParams
                { input = Just (ResponseInputText "call list_dir")
                , tools = Just [tool]
                }
            reinforced = reinforceFunctionSchemas request
        reinforced.instructions `shouldSatisfy`
            maybe False (Text.isInfixOf "Never use an array")
        reinforceFunctionSchemas reinforced `shouldBe` reinforced
        case buildChatRequest options request of
            Right (Aeson.Object object)
                | Just (Aeson.Array messages) <- KeyMap.lookup "messages" object
                , Aeson.Object system : _ <- toList messages
                , Just (Aeson.String content) <- KeyMap.lookup "content" system -> do
                    content `shouldSatisfy`
                        Text.isInfixOf "Never use an array"
                    content `shouldSatisfy`
                        Text.isInfixOf "\"name\":\"list_dir\""
                    content `shouldSatisfy`
                        Text.isInfixOf
                            "\"required\":[\"target_directory\"]"
            result ->
                expectationFailure
                    ("expected schema instructions, got " <> show result)

    it "replays function calls and outputs as Chat Completions messages" do
        let request = case defaultResponseCreateParams of
                ResponseCreateParams{..} -> ResponseCreateParams
                    { input = Just $ ResponseInputItems
                        [ messageItem RoleUser "Use the tool."
                        , FunctionCallItem FunctionCall
                            { itemId = Nothing
                            , callId = "call_1"
                            , name = "read_file"
                            , namespace = Nothing
                            , arguments = "{\"path\":\"README.md\"}"
                            , encryptedFunctionArgs = Nothing
                            , status = Just ItemCompleted
                            , extraFields = KeyMap.empty
                            }
                        , FunctionCallOutputItem FunctionCallOutput
                            { itemId = Nothing
                            , callId = "call_1"
                            , name = Just "read_file"
                            , namespace = Nothing
                            , output = Aeson.String "contents"
                            , status = Just ItemCompleted
                            , extraFields = KeyMap.empty
                            }
                        ]
                    , ..
                    }
            expected = Aeson.toJSON
                [ Aeson.object
                    [ "role" Aeson..= ("user" :: String)
                    , "content" Aeson..= ("Use the tool." :: String)
                    ]
                , Aeson.object
                    [ "role" Aeson..= ("assistant" :: String)
                    , "content" Aeson..= Aeson.Null
                    , "tool_calls" Aeson..=
                        [ Aeson.object
                            [ "id" Aeson..= ("call_1" :: String)
                            , "type" Aeson..= ("function" :: String)
                            , "function" Aeson..= Aeson.object
                                [ "name" Aeson..= ("read_file" :: String)
                                , "arguments" Aeson..=
                                    ("{\"path\":\"README.md\"}" :: String)
                                ]
                            ]
                        ]
                    ]
                , Aeson.object
                    [ "role" Aeson..= ("tool" :: String)
                    , "tool_call_id" Aeson..= ("call_1" :: String)
                    , "content" Aeson..= ("contents" :: String)
                    ]
                ]
        case buildChatRequest options request of
            Right (Aeson.Object object) ->
                KeyMap.lookup "messages" object `shouldBe` Just expected
            result ->
                expectationFailure
                    ("expected a JSON request object, got " <> show result)

    it "replaces empty tool output with a non-empty sentinel" do
        let request = case defaultResponseCreateParams of
                ResponseCreateParams{..} -> ResponseCreateParams
                    { input = Just $ ResponseInputItems
                        [ FunctionCallItem FunctionCall
                            { itemId = Nothing
                            , callId = "call_empty"
                            , name = "run_terminal_cmd"
                            , namespace = Nothing
                            , arguments = "{}"
                            , encryptedFunctionArgs = Nothing
                            , status = Just ItemCompleted
                            , extraFields = KeyMap.empty
                            }
                        , FunctionCallOutputItem FunctionCallOutput
                            { itemId = Nothing
                            , callId = "call_empty"
                            , name = Just "run_terminal_cmd"
                            , namespace = Nothing
                            , output = Aeson.String "  "
                            , status = Just ItemCompleted
                            , extraFields = KeyMap.empty
                            }
                        ]
                    , ..
                    }
            expected = Aeson.toJSON
                [ Aeson.object
                    [ "role" Aeson..= ("assistant" :: String)
                    , "content" Aeson..= Aeson.Null
                    , "tool_calls" Aeson..=
                        [ Aeson.object
                            [ "id" Aeson..= ("call_empty" :: String)
                            , "type" Aeson..= ("function" :: String)
                            , "function" Aeson..= Aeson.object
                                [ "name" Aeson..=
                                    ("run_terminal_cmd" :: String)
                                , "arguments" Aeson..= ("{}" :: String)
                                ]
                            ]
                        ]
                    ]
                , Aeson.object
                    [ "role" Aeson..= ("tool" :: String)
                    , "tool_call_id" Aeson..= ("call_empty" :: String)
                    , "content" Aeson..= ("(no output)" :: String)
                    ]
                ]
        case buildChatRequest options request of
            Right (Aeson.Object object) ->
                KeyMap.lookup "messages" object `shouldBe` Just expected
            result ->
                expectationFailure
                    ("expected a JSON request object, got " <> show result)

    it "rejects messages inserted before a pending tool result" do
        let request = case defaultResponseCreateParams of
                ResponseCreateParams{..} -> ResponseCreateParams
                    { input = Just $ ResponseInputItems
                        [ FunctionCallItem FunctionCall
                            { itemId = Nothing
                            , callId = "call_pending"
                            , name = "read_file"
                            , namespace = Nothing
                            , arguments = "{}"
                            , encryptedFunctionArgs = Nothing
                            , status = Just ItemCompleted
                            , extraFields = KeyMap.empty
                            }
                        , messageItem RoleUser "skip the result"
                        ]
                    , ..
                    }
        case buildChatRequest options request of
            Left (ProviderError{errorType = InvalidRequestError}) ->
                pure ()
            result ->
                expectationFailure
                    ("expected invalid tool ordering, got " <> show result)

    it "normalizes function calls into canonical Responses items" do
        let message = Aeson.object
                [ "role" Aeson..= ("assistant" :: String)
                , "content" Aeson..= Aeson.Null
                , "tool_calls" Aeson..=
                    [ Aeson.object
                        [ "id" Aeson..= ("call_42" :: String)
                        , "type" Aeson..= ("function" :: String)
                        , "function" Aeson..= Aeson.object
                            [ "name" Aeson..= ("read_file" :: String)
                            , "arguments" Aeson..=
                                ("{\"path\":\"README.md\"}" :: String)
                            ]
                        ]
                    ]
                ]
        case normalizeChatCompletion
            (chatPayload "tool_calls" message) of
            Left err ->
                expectationFailure
                    ("expected a normalized response, got " <> show err)
            Right response ->
                case response.output of
                    [FunctionCallItem call] ->
                        (call.callId, call.name, call.arguments)
                            `shouldBe`
                                ( "call_42"
                                , "read_file"
                                , "{\"path\":\"README.md\"}"
                                )
                    output ->
                        expectationFailure
                            ("expected one function call, got " <> show output)

    it "rejects output truncated by the local model" do
        let message = Aeson.object
                [ "role" Aeson..= ("assistant" :: String)
                , "content" Aeson..= ("partial" :: String)
                ]
        normalizeChatCompletion (chatPayload "length" message)
            `shouldBe`
                Left
                    (ProviderError
                        InvalidRequestError
                        "Apple Intelligence stopped at its output-token limit"
                        Nothing)
  where
    expectedInstructions :: Text
    expectedInstructions =
        "Be concise.\n\n\
        \## Function Argument Schemas\n\
        \When calling a function, function.arguments must be a JSON-encoded \
        \object matching that function's schema. Never use an array as the \
        \top-level arguments value. Include every required field.\n\
        \[{\"name\":\"read_file\",\"parameters\":{\"type\":\"object\"}}]"
    options = ChatCompletionsOptions
        { chatBaseUrl = "http://127.0.0.1:8000/v1"
        , chatModel = "apple-foundationmodel"
        , chatBearerToken = Nothing
        , chatRequestTimeoutSeconds = 60
        }

chatPayload :: String -> Aeson.Value -> Aeson.Value
chatPayload finishReason message =
    Aeson.object
        [ "id" Aeson..= ("chatcmpl_test" :: String)
        , "created" Aeson..= (1 :: Int)
        , "model" Aeson..= ("apple-foundationmodel" :: String)
        , "choices" Aeson..=
            [ Aeson.object
                [ "finish_reason" Aeson..= finishReason
                , "message" Aeson..= message
                ]
            ]
        ]

messageItem :: ResponseRole -> Text -> ResponseItem
messageItem role value = MessageItem ResponseMessage
    { messageId = Nothing
    , content = MessageContentText value
    , role
    , status = Just ItemCompleted
    , phase = Nothing
    , passthrough = Nothing
    , extraFields = KeyMap.empty
    }
