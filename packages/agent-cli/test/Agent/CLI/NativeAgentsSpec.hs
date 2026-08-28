module Agent.CLI.NativeAgentsSpec (spec) where

import Agent.CLI.AgentViewport (AgentEntry(..), AgentTarget(..))
import Agent.CLI.NativeAgents
import Agent.Json (rawJsonFromEncoding)
import Agent.Loop (LoopEvent(..), NativeAgentStatus(..))
import Agent.Responses.Types
    ( FunctionCall(..)
    , FunctionCallOutput(..)
    , ItemStatus(..)
    , ResponseItem(..)
    )
import Agent.TUI.Model (BlockState(..), UiBlock(..), UiState(..))
import qualified Data.Aeson as Aeson
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
        let call = FunctionCallItem FunctionCall
                { itemId = Nothing
                , callId = "agent-1"
                , name = "Agent"
                , namespace = Nothing
                , provider = Just "claude-code"
                , arguments =
                    "{\"description\":\"Review API\",\"model\":\"sonnet\"}"
                , encryptedFunctionArgs = Nothing
                , status = Just ItemCompleted
                }
            output = FunctionCallOutputItem FunctionCallOutput
                { itemId = Nothing
                , callId = "agent-1"
                , name = Nothing
                , namespace = Nothing
                , provider = Just "claude-code"
                , output =
                    rawJsonFromEncoding (Aeson.toEncoding ("review complete" :: String))
                , status = Just ItemCompleted
                }
            restored = restoreNativeAgents [call, output] Map.empty
            view = restored Map.! "agent-1"
        view.nativeAgentLabel `shouldBe` "Review API"
        view.nativeAgentModel `shouldBe` Just "sonnet"
        view.nativeAgentStatus `shouldBe` "done"
        view.nativeAgentTranscript `shouldBe` ["review complete"]

    it "does not restore unpaired or non-Claude canonical calls" do
        let call identifier provider = FunctionCallItem FunctionCall
                { itemId = Nothing
                , callId = identifier
                , name = "Task"
                , namespace = Nothing
                , provider = Just provider
                , arguments = "{}"
                , encryptedFunctionArgs = Nothing
                , status = Just ItemCompleted
                }
            wrongOutput = FunctionCallOutputItem FunctionCallOutput
                { itemId = Nothing
                , callId = "claude-unpaired"
                , name = Nothing
                , namespace = Nothing
                , provider = Just "openai"
                , output = rawJsonFromEncoding
                    (Aeson.toEncoding ("not Claude metadata" :: String))
                , status = Just ItemCompleted
                }
            restored =
                restoreNativeAgents
                    [ call "claude-unpaired" "claude-code"
                    , wrongOutput
                    , call "other" "openai"
                    ]
                    Map.empty
        restored `shouldBe` Map.empty
