module Agent.CLI.TurnSpec (spec) where

import Agent.CLI.Turn
    ( IncompleteTurnCheckpoint(..)
    , checkpointIncompleteTurn
    , restorePlanStateAfterIncomplete
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
import Agent.Tools.PlanMode
    ( PlanModeEnv(..)
    , PlanModeState(..)
    , activatePlanMode
    , newPlanModeEnv
    )
import qualified Data.Aeson.KeyMap as KeyMap
import Data.IORef (readIORef, writeIORef)
import System.OsPath (unsafeEncodeUtf)
import Test.Hspec

spec :: Spec
spec = do
    describe "checkpointIncompleteTurn" do
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

    describe "restorePlanStateAfterIncomplete" do
        it "undoes an agent-initiated plan-mode entry after cancellation" do
            plan <- newPlanModeEnv (unsafeEncodeUtf "/tmp/turn-plan") Nothing
            activatePlanMode plan
            restorePlanStateAfterIncomplete plan PlanInactive
            readIORef plan.planStateRef `shouldReturn` PlanInactive

        it "restores pending and already-active modes exactly" do
            plan <- newPlanModeEnv (unsafeEncodeUtf "/tmp/turn-plan") Nothing
            writeIORef plan.planStateRef PlanActive
            restorePlanStateAfterIncomplete plan PlanPending
            readIORef plan.planStateRef `shouldReturn` PlanPending
            restorePlanStateAfterIncomplete plan PlanActive
            readIORef plan.planStateRef `shouldReturn` PlanActive
