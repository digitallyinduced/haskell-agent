module Agent.CLI.TurnSpec (spec) where

import Agent.CLI.Turn (retainTurnInputs)
import Agent.Loop (TurnInput(..))
import Agent.Responses.LoopBackend (turnInputsToItems)
import Agent.Responses.Types
    ( ResponseContentPart(..)
    , ResponseItem(..)
    , ResponseMessage(..)
    , MessageContent(..)
    , ResponseRole(..)
    )
import qualified Data.Aeson.KeyMap as KeyMap
import Test.Hspec

spec :: Spec
spec = describe "retainTurnInputs" do
    it "keeps the exact turn inputs after existing history" do
        let history = turnInputsToItems [UserMessage "earlier"]
            inputs = [UserMessage "build failed [2026-08-23 13:10 CEST]"]
            retained = turnInputsToItems inputs
        retainTurnInputs history inputs
            `shouldBe` (history <> retained, retained)

    it "does not retain partial assistant or tool state" do
        let history = turnInputsToItems [UserMessage "earlier"]
            partialAssistant =
                MessageItem ResponseMessage
                    { messageId = Just "partial"
                    , content =
                        MessageContentParts
                            [ OutputTextPart
                                "partial answer"
                                Nothing
                                Nothing
                                KeyMap.empty
                            ]
                    , role = RoleAssistant
                    , status = Nothing
                    , phase = Nothing
                    , extraFields = KeyMap.empty
                    }
            inputs = [UserMessage "fix the failure"]
            (transcript, _) = retainTurnInputs history inputs
        transcript `shouldNotContain` [partialAssistant]
        transcript `shouldBe` history <> turnInputsToItems inputs
