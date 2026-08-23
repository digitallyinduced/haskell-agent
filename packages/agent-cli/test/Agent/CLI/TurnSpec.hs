module Agent.CLI.TurnSpec (spec) where

import Agent.CLI.Turn
    ( IncompleteTurnCheckpoint(..)
    , checkpointIncompleteTurn
    )
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
spec = describe "checkpointIncompleteTurn" do
    it "describes the complete input-only checkpoint transition" do
        let history = turnInputsToItems [UserMessage "earlier"]
            inputs = [UserMessage "build failed [2026-08-23 13:10 CEST]"]
            retained = turnInputsToItems inputs
        checkpointIncompleteTurn history inputs `shouldBe`
            IncompleteTurnCheckpoint
                { checkpointTranscript = history <> retained
                , checkpointTurnItems = retained
                , checkpointPreviousResponseId = Nothing
                }

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
            partialTranscript = history <> [partialAssistant]
            checkpoint = checkpointIncompleteTurn history inputs
        checkpoint.checkpointTranscript
            `shouldNotBe` partialTranscript <> turnInputsToItems inputs
        checkpoint.checkpointTranscript
            `shouldBe` history <> turnInputsToItems inputs
