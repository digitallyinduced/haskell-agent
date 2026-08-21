module Agent.OpenAI.ResponseMergeSpec (spec) where

import qualified Agent.Responses.Types as OpenAI
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
                [ OpenAI.FunctionCallItem
                    OpenAI.FunctionCall
                    { OpenAI.itemId = Just "fc_1"
                    , OpenAI.callId = "call_1"
                    , OpenAI.name = "echo_text"
                    , OpenAI.arguments = "{\"text\":\"ok\"}"
                    , OpenAI.status = Nothing
                    , OpenAI.extraFields = mempty
                    }
                ]

    it "keeps final output and appends streamed items missing from the completed event" do
        decodedOutput (mergeCompletedResponseOutput [toolCall] (completedResponse [assistantMessage]))
            `shouldBe`
                [ OpenAI.MessageItem
                    OpenAI.ResponseMessage
                    { OpenAI.messageId = Just "msg_1"
                    , OpenAI.role = OpenAI.RoleAssistant
                    , OpenAI.content = OpenAI.MessageContentParts
                        [ OpenAI.OutputTextPart
                            { OpenAI.text = "thinking..."
                            , OpenAI.annotations = Nothing
                            , OpenAI.logprobs = Nothing
                            , OpenAI.extraFields = mempty
                            }
                        ]
                    , OpenAI.status = Nothing
                    , OpenAI.phase = Nothing
                    , OpenAI.extraFields = mempty
                    }
                , OpenAI.FunctionCallItem
                    OpenAI.FunctionCall
                    { OpenAI.itemId = Just "fc_1"
                    , OpenAI.callId = "call_1"
                    , OpenAI.name = "echo_text"
                    , OpenAI.arguments = "{\"text\":\"ok\"}"
                    , OpenAI.status = Nothing
                    , OpenAI.extraFields = mempty
                    }
                ]

    it "does not duplicate streamed items already present in final output" do
        decodedOutput (mergeCompletedResponseOutput [toolCall] (completedResponse [toolCall]))
            `shouldBe`
                [ OpenAI.FunctionCallItem
                    OpenAI.FunctionCall
                    { OpenAI.itemId = Just "fc_1"
                    , OpenAI.callId = "call_1"
                    , OpenAI.name = "echo_text"
                    , OpenAI.arguments = "{\"text\":\"ok\"}"
                    , OpenAI.status = Nothing
                    , OpenAI.extraFields = mempty
                    }
                ]

decodedOutput :: Aeson.Value -> [OpenAI.ResponseItem]
decodedOutput value =
    case Aeson.fromJSON value of
        Aeson.Success (response :: OpenAI.Response) -> response.output
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
