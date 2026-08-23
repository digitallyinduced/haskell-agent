module Agent.CLI.TurnSpec (spec) where

import Agent.CLI.TurnState
import Agent.Loop
    ( TokenUsage(..)
    , TurnInput(..)
    )
import Agent.Responses.LoopBackend (turnInputsToItems)
import Agent.Responses.Types (ResponseItem)
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
