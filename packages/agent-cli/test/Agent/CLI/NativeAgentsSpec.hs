module Agent.CLI.NativeAgentsSpec (spec) where

import Agent.CLI.AgentViewport (AgentEntry(..), AgentTarget(..))
import Agent.CLI.NativeAgents
import Agent.Loop (LoopEvent(..), NativeAgentStatus(..))
import Agent.Responses.Types
    ( FunctionCall(..)
    , FunctionCallOutput(..)
    , ItemStatus(..)
    , ResponseItem(..)
    )
import Agent.TUI.Model (BlockState(..), UiBlock(..), UiState(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Foldable as Foldable
import Data.List (find)
import qualified Data.Map.Strict as Map
import Test.Hspec

spec :: Spec
spec = describe "provider-native agent tracking" do
    it "retains output that arrives before lifecycle metadata" do
        let beforeStart =
                applyNativeAgentEvent
                    (NativeAgentOutput "child" "working")
                    Map.empty
            afterStart =
                applyNativeAgentEvent
                    (NativeAgentStarted
                        "child"
                        Nothing
                        "Explore"
                        (Just "claude-sonnet"))
                    beforeStart
            view = afterStart Map.! "child"
        view.nativeAgentTranscript `shouldBe` ["working"]
        view.nativeAgentLabel `shouldBe` "Explore"
        view.nativeAgentModel `shouldBe` Just "claude-sonnet"

    it "builds nested display paths from provider parent identifiers" do
        let tracked =
                foldl
                    (flip applyNativeAgentEvent)
                    Map.empty
                    [ NativeAgentStarted
                        "parent" Nothing "Research / API" Nothing
                    , NativeAgentStarted
                        "child" (Just "parent") "Review" Nothing
                    ]
            entries = nativeAgentEntries tracked
            child =
                find ((== AgentNative "child") . (.agentTarget)) entries
        (.agentPath) <$> child
            `shouldBe` Just "/native/Research - API/Review"

    it "settles running children when an attempt is discarded" do
        let tracked =
                foldl
                    (flip applyNativeAgentEvent)
                    Map.empty
                    [ NativeAgentStarted "child" Nothing "Explore" Nothing
                    , NativeAgentOutput "child" "partial"
                    , ResponseAttemptDiscarded
                    ]
            view = tracked Map.! "child"
            states =
                map (.blockState)
                    (Foldable.toList view.nativeAgentConversation.uiBlocks)
        view.nativeAgentStatus `shouldBe` "cancelled"
        states `shouldSatisfy` all (/= BlockRunning)

    it "settles stale running children before the next turn starts" do
        let tracked =
                foldl
                    (flip applyNativeAgentEvent)
                    Map.empty
                    [ NativeAgentStarted "child" Nothing "Explore" Nothing
                    , NativeAgentOutput "child" "partial"
                    , TurnStarted
                    ]
            view = tracked Map.! "child"
            states =
                map (.blockState)
                    (Foldable.toList view.nativeAgentConversation.uiBlocks)
        view.nativeAgentStatus `shouldBe` "cancelled"
        states `shouldSatisfy` all (/= BlockRunning)

    it "creates and terminates a placeholder for reordered finish events" do
        let tracked =
                applyNativeAgentEvent
                    (NativeAgentFinished "late" NativeAgentFailed)
                    Map.empty
            view = tracked Map.! "late"
        view.nativeAgentStatus `shouldBe` "error"
        view.nativeAgentTranscript `shouldBe` []

    it "restores completed Claude-native agents from canonical tool items" do
        let provider =
                KeyMap.singleton "provider"
                    (Aeson.String "claude-code")
            call = FunctionCallItem FunctionCall
                { itemId = Nothing
                , callId = "agent-1"
                , name = "Agent"
                , namespace = Nothing
                , arguments =
                    "{\"description\":\"Review API\",\"model\":\"sonnet\"}"
                , encryptedFunctionArgs = Nothing
                , status = Just ItemCompleted
                , extraFields = provider
                }
            output = FunctionCallOutputItem FunctionCallOutput
                { itemId = Nothing
                , callId = "agent-1"
                , name = Nothing
                , namespace = Nothing
                , output = Aeson.String "review complete"
                , status = Just ItemCompleted
                , extraFields = provider
                }
            restored = restoreNativeAgents [call, output] Map.empty
            view = restored Map.! "agent-1"
        view.nativeAgentLabel `shouldBe` "Review API"
        view.nativeAgentModel `shouldBe` Just "sonnet"
        view.nativeAgentStatus `shouldBe` "done"
        view.nativeAgentTranscript `shouldBe` ["review complete"]

    it "does not restore unpaired or non-Claude canonical calls" do
        let claudeFields =
                KeyMap.singleton "provider"
                    (Aeson.String "claude-code")
            otherFields =
                KeyMap.singleton "provider"
                    (Aeson.String "openai")
            call identifier fields = FunctionCallItem FunctionCall
                { itemId = Nothing
                , callId = identifier
                , name = "Task"
                , namespace = Nothing
                , arguments = "{}"
                , encryptedFunctionArgs = Nothing
                , status = Just ItemCompleted
                , extraFields = fields
                }
            wrongOutput = FunctionCallOutputItem FunctionCallOutput
                { itemId = Nothing
                , callId = "claude-unpaired"
                , name = Nothing
                , namespace = Nothing
                , output = Aeson.String "not Claude metadata"
                , status = Just ItemCompleted
                , extraFields = otherFields
                }
            restored =
                restoreNativeAgents
                    [ call "claude-unpaired" claudeFields
                    , wrongOutput
                    , call "other" otherFields
                    ]
                    Map.empty
        restored `shouldBe` Map.empty
