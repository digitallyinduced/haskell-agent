module Agent.Responses.ResponseMergeSpec (spec) where

import qualified Agent.Responses.Types as Responses
import Agent.Responses.ResponseMerge
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck
    ( Arbitrary(..)
    , Gen
    , chooseInt
    , elements
    , listOf
    , vectorOf
    , (===)
    )

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
                    , Responses.namespace = Nothing
                    , Responses.arguments = "{\"text\":\"ok\"}"
                    , Responses.encryptedFunctionArgs = Nothing
                    , Responses.status = Nothing
                    , Responses.extraFields = mempty
                    }
                ]

    modifyMaxSuccess (const 300) $
        prop "overlaying object fragments is associative" $
            \(ObjectFragments (first, second, third)) ->
                mergeResponseFragments
                    [first, second, third]
                    === mergeResponseFragments
                        [ mergeResponseFragments [first, second]
                        , third
                        ]

    modifyMaxSuccess (const 300) $
        prop "later lifecycle fields win while unrelated fields survive" $
            \(GeneratedLifecycle (firstValue, secondValue)) ->
                let first =
                        Aeson.object
                            [ "id" .= ("first" :: Text)
                            , "model" .= ("test-model" :: Text)
                            , "stable" .= firstValue
                            ]
                    second =
                        Aeson.object
                            [ "id" .= ("second" :: Text)
                            , "vendor_field" .= secondValue
                            ]
                    expected =
                        Aeson.object
                            [ "id" .= ("second" :: Text)
                            , "model" .= ("test-model" :: Text)
                            , "stable" .= firstValue
                            , "vendor_field" .= secondValue
                            ]
                in mergeResponseFragments [first, second] === expected

    modifyMaxSuccess (const 300) $
        prop "merging identifiable streamed items is idempotent" $
            \(GeneratedItems items) ->
                let response = completedResponse []
                    once = mergeCompletedResponseOutput items response
                    twice = mergeCompletedResponseOutput items once
                in twice === once

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
                    , Responses.passthrough = Nothing
                    , Responses.extraFields = mempty
                    }
                , Responses.FunctionCallItem
                    Responses.FunctionCall
                    { Responses.itemId = Just "fc_1"
                    , Responses.callId = "call_1"
                    , Responses.name = "echo_text"
                    , Responses.namespace = Nothing
                    , Responses.arguments = "{\"text\":\"ok\"}"
                    , Responses.encryptedFunctionArgs = Nothing
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
                    , Responses.namespace = Nothing
                    , Responses.arguments = "{\"text\":\"ok\"}"
                    , Responses.encryptedFunctionArgs = Nothing
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

newtype ObjectFragments = ObjectFragments
    (Aeson.Value, Aeson.Value, Aeson.Value)

instance Show ObjectFragments where
    show (ObjectFragments values) = show values

instance Arbitrary ObjectFragments where
    arbitrary =
        ObjectFragments <$> ((,,) <$> genObject <*> genObject <*> genObject)
    shrink _ = []

genObject :: Gen Aeson.Value
genObject = do
    fields <- listOf $ do
        key <- elements ["id", "status", "model", "vendor_field"]
        value <- chooseInt (-10, 10)
        pure (key .= value)
    pure (Aeson.object fields)

newtype GeneratedItems = GeneratedItems [Aeson.Value]

instance Show GeneratedItems where
    show (GeneratedItems items) = show items

instance Arbitrary GeneratedItems where
    arbitrary = do
        identifiers <- vectorOf 6 (chooseInt (0, 20))
        let uniqueIdentifiers = nub identifiers
        pure $ GeneratedItems
            [ Aeson.object
                [ "type" .= ("function_call" :: Text)
                , "id" .= ("streamed-" <> Text.pack (show identifier))
                , "call_id" .= ("call-" <> Text.pack (show identifier))
                , "name" .= ("generated" :: Text)
                , "arguments" .= ("{}" :: Text)
                ]
            | identifier <- uniqueIdentifiers
            ]
    shrink _ = []

newtype GeneratedLifecycle = GeneratedLifecycle (Int, Int)
    deriving (Eq, Show)

instance Arbitrary GeneratedLifecycle where
    arbitrary = GeneratedLifecycle <$> ((,) <$> chooseInt (-100, 100) <*> chooseInt (-100, 100))
    shrink _ = []
