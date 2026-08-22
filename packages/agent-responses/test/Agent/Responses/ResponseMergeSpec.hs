module Agent.Responses.ResponseMergeSpec (spec) where

import qualified Agent.Responses.Types as Responses
import Agent.Responses.ResponseMerge
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import Data.Text (Text)
import Test.Hspec

spec :: Spec
spec = describe "mergeCompletedResponseOutput" do
    it "fills an empty completed output from streamed output_item.done items" do
        decodedOutput (mergeCompletedResponseOutput [toolCall] (completedResponse []))
            `shouldBe`
                [ Responses.FunctionCallItem
                    Responses.FunctionCall
                    { Responses.itemId = Just "fc_1"
                    , Responses.callId = "call_1"
                    , Responses.name = "echo_text"
                    , Responses.arguments = "{\"text\":\"ok\"}"
                    , Responses.status = Nothing
                    , Responses.extraFields = mempty
                    }
                ]

    it "keeps final output and appends streamed items missing from the completed event" do
        decodedOutput (mergeCompletedResponseOutput [toolCall] (completedResponse [assistantMessage]))
            `shouldBe`
                [ Responses.MessageItem
                    Responses.ResponseMessage
                    { Responses.messageId = Just "msg_1"
                    , Responses.role = Responses.RoleAssistant
                    , Responses.content = Responses.MessageContentParts
                        [ Responses.OutputTextPart
                            { Responses.text = "thinking..."
                            , Responses.annotations = Nothing
                            , Responses.logprobs = Nothing
                            , Responses.extraFields = mempty
                            }
                        ]
                    , Responses.status = Nothing
                    , Responses.phase = Nothing
                    , Responses.extraFields = mempty
                    }
                , Responses.FunctionCallItem
                    Responses.FunctionCall
                    { Responses.itemId = Just "fc_1"
                    , Responses.callId = "call_1"
                    , Responses.name = "echo_text"
                    , Responses.arguments = "{\"text\":\"ok\"}"
                    , Responses.status = Nothing
                    , Responses.extraFields = mempty
                    }
                ]

    it "does not duplicate streamed items already present in final output" do
        decodedOutput (mergeCompletedResponseOutput [toolCall] (completedResponse [toolCall]))
            `shouldBe`
                [ Responses.FunctionCallItem
                    Responses.FunctionCall
                    { Responses.itemId = Just "fc_1"
                    , Responses.callId = "call_1"
                    , Responses.name = "echo_text"
                    , Responses.arguments = "{\"text\":\"ok\"}"
                    , Responses.status = Nothing
                    , Responses.extraFields = mempty
                    }
                ]

decodedOutput :: Aeson.Value -> [Responses.ResponseItem]
decodedOutput value =
    case Aeson.fromJSON value of
        Aeson.Success (response :: Responses.Response) -> response.output
        Aeson.Error err -> error err

completedResponse :: [Aeson.Value] -> Aeson.Value
completedResponse output =
    Aeson.object
        [ "id" .= ("resp_1" :: Text)
        , "created_at" .= (0 :: Int)
        , "status" .= ("completed" :: Text)
        , "model" .= ("gpt-test" :: Text)
        , "output" .= output
        ]

assistantMessage :: Aeson.Value
assistantMessage =
    Aeson.object
        [ "type" .= ("message" :: Text)
        , "id" .= ("msg_1" :: Text)
        , "role" .= ("assistant" :: Text)
        , "content" .= [ Aeson.object
            [ "type" .= ("output_text" :: Text)
            , "text" .= ("thinking..." :: Text)
            ]
          ]
        ]

toolCall :: Aeson.Value
toolCall =
    Aeson.object
        [ "type" .= ("function_call" :: Text)
        , "id" .= ("fc_1" :: Text)
        , "call_id" .= ("call_1" :: Text)
        , "name" .= ("echo_text" :: Text)
        , "arguments" .= ("{\"text\":\"ok\"}" :: Text)
        ]
