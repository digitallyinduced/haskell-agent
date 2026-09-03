module Agent.Gemini.RequestSpec (spec) where

import Agent.Error (ApiError(..), ErrorType(..))
import Agent.Gemini.Request
import Agent.Responses.Types
import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Set as Set
import Data.Text (Text)
import Test.Hspec

spec :: Spec
spec = describe "Gemini request projection" do
    it "projects instructions, user text, and model normalization" do
        let params = defaultResponseCreateParams
                { model = Just " models/gemini-test "
                , instructions = Just "Be concise"
                , input = Just (ResponseInputItems [message RoleUser "hello"])
                }
        buildRequest "fallback" params `shouldBe`
            Right (GeminiRequest "gemini-test"
                (object
                    [ "contents" .=
                        [object
                            [ "role" .= ("user" :: Text)
                            , "parts" .= [object ["text" .= ("hello" :: Text)]]
                            ]]
                    , "systemInstruction" .= object
                        [ "parts" .= [object ["text" .= ("Be concise" :: Text)]] ]
                    ])
                Set.empty)

    it "emits function declarations and thinking config" do
        let tool = FunctionToolValue FunctionTool
                { name = "lookup"
                , description = Just "Look up a value"
                , parameters = Nothing
                , strict = Nothing
                }
            params :: ResponseCreateParams
            params = defaultResponseCreateParams
                { input = Just (ResponseInputText "use lookup")
                , tools = Just [tool]
                , reasoning = Just ReasoningConfig
                    { context = Nothing, effort = Just "high"
                    , summary = Nothing, generateSummary = Nothing
                    , reasoningMode = Nothing
                    }
                }
        case buildRequest "gemini-test" params of
            Right GeminiRequest{requestBody = Object body} -> do
                body `shouldSatisfy` member "tools"
                body `shouldSatisfy` member "generationConfig"
            other -> expectationFailure ("unexpected request: " <> show other)

    it "maps the portable hosted search tool to native Google Search" do
        let params :: ResponseCreateParams
            params = withTools
                (Just [KnownResponseTool ToolWebSearch])
                defaultResponseCreateParams
        fmap (.requestBody) (buildRequest "gemini-3.7-flash" params)
            `shouldBe`
                Right
                    (object
                        [ "contents" .= ([] :: [Value])
                        , "tools" .=
                            [object ["googleSearch" .= object []]]
                        ])

    it "omits every native tool when tool choice is none" do
        let params :: ResponseCreateParams
            params = withToolChoice
                (Just (ToolChoiceMode ToolChoiceNone))
                (withTools
                    (Just [KnownResponseTool ToolWebSearch])
                    defaultResponseCreateParams)
        fmap (.requestBody) (buildRequest "gemini-test" params)
            `shouldBe`
                Right (object ["contents" .= ([] :: [Value])])

    it "projects valid JSON arguments for ordinary function calls" do
        let params :: ResponseCreateParams
            params = withInput
                (Just (ResponseInputItems
                    [functionCall "lookup" "{\"query\":\"weather\"}"]))
                defaultResponseCreateParams
        fmap (.requestBody) (buildRequest "gemini-test" params)
            `shouldBe`
                Right
                    (object
                        [ "contents" .=
                            [ object
                                [ "role" .= ("model" :: Text)
                                , "parts" .=
                                    [ object
                                        [ "functionCall" .= object
                                            [ "id" .= ("call-1" :: Text)
                                            , "name" .= ("lookup" :: Text)
                                            , "args" .= object
                                                [ "query" .= ("weather" :: Text)
                                                ]
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        ])

    it "rejects malformed JSON arguments for ordinary function calls" do
        let params :: ResponseCreateParams
            params = withInput
                (Just (ResponseInputItems
                    [functionCall "lookup" "{not json"]))
                defaultResponseCreateParams
        buildRequest "gemini-test" params
            `shouldBe`
                Left
                    (ProviderError InvalidRequestError
                        "Gemini function call `lookup` arguments must be valid JSON"
                        Nothing)

    it "adapts freeform custom tools and preserves raw replay input" do
        let patch = "*** Begin Patch\n*** End Patch"
            custom = CustomToolValue CustomTool
                { name = "apply_patch"
                , description = Just "Apply a patch."
                , format = Nothing
                }
            call = functionCall "apply_patch" patch
            params = defaultResponseCreateParams
                { tools = Just [custom]
                , input = Just (ResponseInputItems [call])
                }
        case buildRequest "gemini-test" params of
            Left err -> expectationFailure
                ("unexpected request error: " <> show err)
            Right request -> do
                request.requestCustomToolNames
                    `shouldBe` Set.singleton "apply_patch"
                request.requestBody `shouldBe` object
                    [ "contents" .=
                        [ object
                            [ "role" .= ("model" :: Text)
                            , "parts" .=
                                [ object
                                    [ "functionCall" .= object
                                        [ "id" .= ("call-1" :: Text)
                                        , "name" .= ("apply_patch" :: Text)
                                        , "args" .= object
                                            ["input" .= (patch :: Text)]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    , "tools" .=
                        [ object
                            [ "functionDeclarations" .=
                                [ object
                                    [ "name" .= ("apply_patch" :: Text)
                                    , "description" .=
                                        ( "Apply a patch.\n\nPass the tool's complete raw input in the `input` field."
                                            :: Text
                                        )
                                    , "parametersJsonSchema" .= object
                                        [ "type" .= ("object" :: Text)
                                        , "properties" .= object
                                            [ "input" .= object
                                                [ "type" .= ("string" :: Text)
                                                , "description" .=
                                                    ( "Complete raw input for the custom tool."
                                                        :: Text
                                                    )
                                                ]
                                            ]
                                        , "required" .= ["input" :: Text]
                                        , "additionalProperties" .= False
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]

    it "maps disabled reasoning to the model's minimum thinking level" do
        let params :: ResponseCreateParams
            params = withReasoning
                (Just ReasoningConfig
                    { context = Nothing, effort = Just "none"
                    , summary = Nothing, generateSummary = Nothing
                    , reasoningMode = Nothing
                    })
                defaultResponseCreateParams
        fmap (.requestBody) (buildRequest "gemini-test" params)
            `shouldBe`
                Right
                    (object
                        [ "contents" .= ([] :: [Value])
                        , "generationConfig" .= object
                            [ "thinkingConfig" .= object
                                [ "thinkingLevel" .= ("LOW" :: Text)
                                , "includeThoughts" .= False
                                ]
                            ]
                        ])
        fmap (.requestBody) (buildRequest "gemini-3.5-flash-lite" params)
            `shouldBe`
                Right
                    (object
                        [ "contents" .= ([] :: [Value])
                        , "generationConfig" .= object
                            [ "thinkingConfig" .= object
                                [ "thinkingLevel" .= ("MINIMAL" :: Text)
                                , "includeThoughts" .= False
                                ]
                            ]
                        ])
  where
    message role text = MessageItem ResponseMessage
        { messageId = Nothing, role = role
        , content = MessageContentText text, status = Nothing
        , phase = Nothing, passthrough = Nothing
        }
    functionCall callName callArguments = FunctionCallItem FunctionCall
        { itemId = Just "item-1"
        , callId = "call-1"
        , name = callName
        , namespace = Nothing
        , provider = Just "gemini"
        , arguments = callArguments
        , encryptedFunctionArgs = Nothing
        , status = Just ItemCompleted
        }
    member key objectValue = KeyMap.member (Key.fromText key) objectValue

    withTools value ResponseCreateParams{..} =
        ResponseCreateParams { tools = value, .. }

    withInput value ResponseCreateParams{..} =
        ResponseCreateParams { input = value, .. }

    withToolChoice value ResponseCreateParams{..} =
        ResponseCreateParams { toolChoice = value, .. }

    withReasoning value ResponseCreateParams{..} =
        ResponseCreateParams { reasoning = value, .. }
