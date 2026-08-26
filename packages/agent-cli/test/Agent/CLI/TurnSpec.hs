module Agent.CLI.TurnSpec (spec) where

import Agent.CLI.Turn
    ( IncompleteTurnCheckpoint(..)
    , checkpointIncompleteTurn
    , grokFirstTurnPrefix
    , grokFrameLastUserInput
    , grokUserQuery
    , restorePlanStateAfterIncomplete
    )
import Agent.CLI.TurnState
import Agent.Loop
    ( ImageAttachment(..)
    , TokenUsage(..)
    , TurnInput(..)
    )
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
    describe "turnInputsWithContext" do
        it "orders plan, startup, and submitted inputs" do
            turnInputsWithContext
                (Just "plan reminder")
                (Just "startup instructions")
                [UserMessage "fix it"]
                `shouldBe`
                    [ UserMessage "plan reminder"
                    , UserMessage "startup instructions"
                    , UserMessage "fix it"
                    ]

    describe "finishConversation" do
        it "rolls a restarted turn back and restores consumed startup" do
            let patch = finishConversation prepared ConversationRestarted
                final = applyConversationPatch patch runningState
            final.conversationPreviousResponseId `shouldBe` Just "resp-newer"
            final.conversationTranscript `shouldBe` history
            final.conversationStartupContext
                `shouldBe` Just "startup instructions\n\nnewer skills"
            final.conversationUsage `shouldBe` priorUsage
            final.conversationLastAssistant `shouldBe` Just "old answer"

        it "uses the same restoration for interrupted IO" do
            finishConversation prepared ConversationInterrupted
                `shouldBe`
                    finishConversation
                        prepared
                        ConversationProviderUnavailable

        it "retains only inputs and invalidates the response chain on cancel" do
            let final = applyConversationPatch
                    (finishConversation prepared ConversationCancelled)
                    runningState
            final.conversationPreviousResponseId `shouldBe` Nothing
            final.conversationTranscript
                `shouldBe` history <> inputOnlyTurnItems prepared
            final.conversationStartupContext `shouldBe` Just "newer skills"
            final.conversationUsage `shouldBe` priorUsage
            final.conversationLastAssistant `shouldBe` Just "old answer"

        it "uses the same input-only checkpoint for terminal failures" do
            finishConversation prepared ConversationFailed
                `shouldBe` finishConversation prepared ConversationCancelled

        it "retains failed multimodal input exactly once for retry" do
            let image = ImageAttachment "image/png" "png-bytes"
                multimodalPrepared = PreparedTurn
                    { preparedBeforeItems = history
                    , preparedConsumedStartup = Nothing
                    , preparedTurnInputs =
                        [UserMultimodal "inspect this" [image]]
                    }
                final = applyConversationPatch
                    (finishConversation
                        multimodalPrepared
                        ConversationFailed)
                    runningState
            final.conversationTranscript
                `shouldBe`
                    history
                        <> turnInputsToItems
                            [UserMultimodal "inspect this" [image]]

        it "restores startup without changing conversation state for retry" do
            let final = applyConversationPatch
                    (finishConversation
                        prepared
                        ConversationProviderUnavailable)
                    runningState
            final.conversationPreviousResponseId `shouldBe` Just "resp-newer"
            final.conversationTranscript `shouldBe` mutatedTranscript
            final.conversationStartupContext
                `shouldBe` Just "startup instructions\n\nnewer skills"
            final.conversationUsage `shouldBe` priorUsage
            final.conversationLastAssistant `shouldBe` Just "old answer"

        it "commits successful metadata while preserving backend history" do
            let usage = TokenUsage
                    { inputTokens = 7
                    , outputTokens = 3
                    , cachedTokens = 2
                    }
                final = applyConversationPatch
                    (finishConversation prepared
                        (ConversationCompleted
                            "resp-new"
                            usage
                            (Just "new answer")))
                    runningState
            final.conversationPreviousResponseId `shouldBe` Just "resp-new"
            final.conversationTranscript `shouldBe` mutatedTranscript
            final.conversationStartupContext `shouldBe` Just "newer skills"
            final.conversationUsage `shouldBe` TokenUsage
                { inputTokens = 17
                , outputTokens = 7
                , cachedTokens = 3
                }
            final.conversationLastAssistant `shouldBe` Just "new answer"

    describe "turnNewItems" do
        it "returns the suffix when the backend extends prior history" do
            let after = history <> turnInputsToItems [UserMessage "new"]
            turnNewItems history after
                `shouldBe` turnInputsToItems [UserMessage "new"]

        it "uses the complete replacement after compaction" do
            let replacement = turnInputsToItems [UserMessage "compacted"]
            turnNewItems history replacement `shouldBe` replacement

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
                        , passthrough = Nothing
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

history :: [ResponseItem]
history = turnInputsToItems [UserMessage "earlier"]

mutatedTranscript :: [ResponseItem]
mutatedTranscript =
    history <> turnInputsToItems [UserMessage "backend-mutated suffix"]

priorUsage :: TokenUsage
priorUsage = TokenUsage
    { inputTokens = 10
    , outputTokens = 4
    , cachedTokens = 1
    }

prepared :: PreparedTurn
prepared = PreparedTurn
    { preparedBeforeItems = history
    , preparedConsumedStartup = Just "startup instructions"
    , preparedTurnInputs =
        [ UserMessage "startup instructions [2026-08-23 13:10 CEST]"
        , UserMessage "build failed [2026-08-23 13:10 CEST]"
        ]
    }

runningState :: ConversationState
runningState = ConversationState
    { conversationPreviousResponseId = Just "resp-newer"
    , conversationTranscript = mutatedTranscript
    , conversationStartupContext = Just "newer skills"
    , conversationUsage = priorUsage
    , conversationLastAssistant = Just "old answer"
    }
