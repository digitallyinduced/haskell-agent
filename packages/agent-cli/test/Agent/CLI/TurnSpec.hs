module Agent.CLI.TurnSpec (spec) where

import Agent.CLI.Turn
    ( IncompleteTurnCheckpoint(..)
    , checkpointIncompleteTurn
    , grokFirstTurnPrefix
    , grokFrameLastUserInput
    , grokUserQuery
    , restorePlanStateAfterIncomplete
    )
import Agent.Loop (ImageAttachment(..), TurnInput(..))
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
import Data.Time.Calendar (fromGregorian)
import qualified Data.Text as Text
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

    describe "Grok user-message framing" do
        it "wraps a request in user_query tags" do
            grokUserQuery "fix it [2026-08-23 16:23 CEST]"
                `shouldBe`
                    "<user_query>\nfix it [2026-08-23 16:23 CEST]\n</user_query>"

        it "wraps only the last user payload and preserves synthetic context" do
            let inputs =
                    [ UserMessage "<system-reminder>rules</system-reminder>"
                    , UserMessage "<skill>instructions</skill>"
                    , UserMessage "actual request"
                    ]
            grokFrameLastUserInput inputs `shouldBe`
                [ UserMessage "<system-reminder>rules</system-reminder>"
                , UserMessage "<skill>instructions</skill>"
                , UserMessage "<user_query>\nactual request\n</user_query>"
                ]

        it "wraps multimodal user text without changing images" do
            let image = ImageAttachment "image/png" "png"
            grokFrameLastUserInput
                [UserMultimodal "describe this" [image]]
                `shouldBe`
                    [ UserMultimodal
                        "<user_query>\ndescribe this\n</user_query>"
                        [image]
                    ]

        it "renders first-turn environment and optional git status" do
            let prefix =
                    grokFirstTurnPrefix
                        "darwin 25.0"
                        "/bin/zsh"
                        (unsafeEncodeUtf "/tmp/project")
                        (fromGregorian 2026 8 23)
                        (Just " M src/Main.hs")
            prefix `shouldSatisfy` Text.isInfixOf "<user_info>"
            prefix `shouldSatisfy` Text.isInfixOf "OS Version: darwin 25.0"
            prefix `shouldSatisfy` Text.isInfixOf "Shell: zsh"
            prefix `shouldSatisfy` Text.isInfixOf "Workspace Path: /tmp/project"
            prefix `shouldSatisfy` Text.isInfixOf
                "Today's date: Sunday Aug 23, 2026"
            prefix `shouldSatisfy` Text.isInfixOf "Prefer using relative paths"
            prefix `shouldSatisfy` Text.isInfixOf "<git_status>"
            prefix `shouldSatisfy` Text.isInfixOf " M src/Main.hs"
