-- | Frontend projection checks stay with the frontend; backend behavior is
-- exercised independently by agent-computer-use-test.
module Agent.CLI.ComputerUseSpec (spec) where

import Agent.CLI.SessionAdmin (sessionToolEvent)
import Agent.Json (rawJsonFromEncoding)
import Agent.Responses.Types
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Test.Hspec

spec :: Spec
spec = describe "computer-use session projection" do
    it "rehydrates typed computer calls and outputs as native tool cards" do
        let call = ComputerCall
                { computerCallItemId = Nothing
                , computerCallId = "call-1"
                , computerActions = [ClickAction 20 30 "left" []]
                , pendingSafetyChecks = []
                , computerCallStatus = Nothing
                , computerCallExtra = KeyMap.empty
                }
            output = ComputerCallOutput
                { computerOutputItemId = Nothing
                , computerOutputCallId = "call-1"
                , screenshotDataUrl =
                    "data:image/png;base64,large-private-payload"
                , acknowledgedChecks = []
                , computerOutputStatus = Just ItemCompleted
                , computerOutputExtra = KeyMap.empty
                }
            encoded =
                TextEncoding.decodeUtf8 . LBS.toStrict . Aeson.encode $
                    [ sessionToolEvent (ComputerCallItem call)
                    , sessionToolEvent (ComputerCallOutputItem output)
                    ]
        encoded `shouldSatisfy`
            ("\"name\":\"computer\"" `Text.isInfixOf`)
        encoded `shouldSatisfy`
            ("\"output\":\"Screenshot captured\"" `Text.isInfixOf`)
        encoded `shouldSatisfy`
            (not . ("large-private-payload" `Text.isInfixOf`))

    it "redacts ordinary computer function arguments and screenshot output" do
        let call = FunctionCall
                { itemId = Nothing
                , callId = "call-function"
                , name = computerFunctionName
                , namespace = Just "functions"
                , arguments =
                    "{\"actions\":[{\"type\":\"type\",\
                    \\"text\":\"top secret\"}]}"
                , encryptedFunctionArgs = Nothing
                , provider = Nothing
                , status = Nothing
                , async = Just True
                }
            output = FunctionCallOutput
                { localOutcome = Nothing
                , itemId = Nothing
                , callId = "call-function"
                , name = Nothing
                , namespace = Nothing
                , output = rawJsonFromEncoding . Aeson.toEncoding $
                    [ Aeson.object
                        [ "type" Aeson..= ("input_image" :: Text.Text)
                        , "image_url" Aeson..=
                            ("data:image/png;base64,large-private-payload"
                                :: Text.Text)
                        ]
                    ]
                , provider = Nothing
                , status = Nothing
                , async = Just True
                }
            encoded =
                TextEncoding.decodeUtf8 . LBS.toStrict . Aeson.encode $
                    [ sessionToolEvent (FunctionCallItem call)
                    , sessionToolEvent (FunctionCallOutputItem output)
                    ]
        encoded `shouldSatisfy`
            ("\"name\":\"computer\"" `Text.isInfixOf`)
        encoded `shouldSatisfy`
            ("type 10 characters" `Text.isInfixOf`)
        encoded `shouldSatisfy`
            ("\"output\":\"Screenshot captured\"" `Text.isInfixOf`)
        encoded `shouldSatisfy`
            ("\"async\":true" `Text.isInfixOf`)
        encoded `shouldSatisfy`
            (not . ("top secret" `Text.isInfixOf`))
        encoded `shouldSatisfy`
            (not . ("large-private-payload" `Text.isInfixOf`))
