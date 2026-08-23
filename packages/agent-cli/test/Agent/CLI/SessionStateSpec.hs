module Agent.CLI.SessionStateSpec (spec) where

import Agent.CLI.SessionState
import Agent.CLI.TurnState
import Agent.Loop (TokenUsage(..), TurnInput(..), emptyTokenUsage)
import Agent.Responses.LoopBackend (turnInputsToItems)
import Test.Hspec

spec :: Spec
spec = describe "Agent.CLI.SessionState" do
    it "snapshots a turn and consumes startup context atomically" do
        let initial = testState
                { sessionConversation =
                    initialConversation
                        { conversationStartupContext = Just "instructions"
                        }
                }
            (started, snapshot) = beginSessionTurn initial
        snapshot.conversationStartupContext `shouldBe` Just "instructions"
        started.sessionConversation.conversationStartupContext
            `shouldBe` Nothing

    it "applies turn deltas without losing concurrently queued state" do
        let concurrent = testState
                { sessionConversation =
                    initialConversation
                        { conversationStartupContext = Just "new skills"
                        , conversationUsage = TokenUsage 3 2 1
                        }
                }
            patch = ConversationPatch
                { patchPreviousResponseId = SetField (Just "resp-1")
                , patchTranscript = KeepField
                , patchStartupContext = RestoreStartup "instructions"
                , patchUsageDelta = TokenUsage 7 5 2
                , patchLastAssistant = SetField (Just "answer")
                }
            updated =
                applySessionConversationPatch patch concurrent
                    |> (.sessionConversation)
        updated.conversationPreviousResponseId `shouldBe` Just "resp-1"
        updated.conversationStartupContext
            `shouldBe` Just "instructions\n\nnew skills"
        updated.conversationUsage `shouldBe` TokenUsage 10 7 3
        updated.conversationLastAssistant `shouldBe` Just "answer"

    it "installs compaction without disturbing unrelated conversation state" do
        let compacted = turnInputsToItems [UserMessage "summary"]
            initial = testState
                { sessionConversation =
                    initialConversation
                        { conversationPreviousResponseId = Just "resp-old"
                        , conversationStartupContext = Just "queued"
                        , conversationUsage = TokenUsage 8 4 2
                        , conversationLastAssistant = Just "answer"
                        }
                }
            updated =
                replaceSessionTranscript compacted initial
                    |> (.sessionConversation)
        updated.conversationPreviousResponseId `shouldBe` Nothing
        updated.conversationTranscript `shouldBe` compacted
        updated.conversationStartupContext `shouldBe` Just "queued"
        updated.conversationUsage `shouldBe` TokenUsage 8 4 2
        updated.conversationLastAssistant `shouldBe` Just "answer"

    it "resets all conversation-owned fields together" do
        let reset = resetSessionConversation (Just "fresh") testState
                |> (.sessionConversation)
        reset `shouldBe` initialConversation
            { conversationStartupContext = Just "fresh"
            }

testState :: SessionState
testState = SessionState initialConversation

initialConversation :: ConversationState
initialConversation = ConversationState
    { conversationPreviousResponseId = Nothing
    , conversationTranscript = []
    , conversationStartupContext = Nothing
    , conversationUsage = emptyTokenUsage
    , conversationLastAssistant = Nothing
    }

(|>) :: a -> (a -> b) -> b
value |> f = f value
