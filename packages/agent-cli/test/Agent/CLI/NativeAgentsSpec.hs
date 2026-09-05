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
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = describe "provider-native agent tracking" do
    it "retains output that arrives before lifecycle metadata" do
        let beforeStart =
                applyNativeAgentEvent
                    (NativeAgentOutput "child" "working")
                    emptyNativeAgentStore
            afterStart =
                applyNativeAgentEvent
                    (NativeAgentStarted
                        "child"
                        Nothing
                        "Explore"
                        (Just "claude-sonnet"))
                    beforeStart
            view = lookupView "child" afterStart
        nativeAgentTranscript view `shouldBe` ["working"]
        view.nativeAgentLabel `shouldBe` "Explore"
        view.nativeAgentModel `shouldBe` Just "claude-sonnet"

    it "builds nested display paths from provider parent identifiers" do
        let tracked =
                foldl
                    (flip applyNativeAgentEvent)
                    emptyNativeAgentStore
                    [ NativeAgentStarted
                        "parent" Nothing "Research / API" Nothing
                    , NativeAgentStarted
                        "child" (Just "parent") "Review" Nothing
                    ]
            entries = nativeAgentEntries AgentRoot tracked
            child =
                find ((== AgentNative "child") . (.agentTarget)) entries
        (.agentPath) <$> child
            `shouldBe` Just "/native/Research - API/Review"

    it "settles running children when an attempt is discarded" do
        let tracked =
                foldl
                    (flip applyNativeAgentEvent)
                    emptyNativeAgentStore
                    [ NativeAgentStarted "child" Nothing "Explore" Nothing
                    , NativeAgentOutput "child" "partial"
                    , ResponseAttemptDiscarded
                    ]
            view = lookupView "child" tracked
            states =
                map (.blockState)
                    (Foldable.toList
                        (nativeAgentConversation view).uiBlocks)
        view.nativeAgentStatus `shouldBe` "cancelled"
        states `shouldSatisfy` all (/= BlockRunning)

    it "settles stale running children before the next turn starts" do
        let tracked =
                foldl
                    (flip applyNativeAgentEvent)
                    emptyNativeAgentStore
                    [ NativeAgentStarted "child" Nothing "Explore" Nothing
                    , NativeAgentOutput "child" "partial"
                    , TurnStarted
                    ]
            view = lookupView "child" tracked
            states =
                map (.blockState)
                    (Foldable.toList
                        (nativeAgentConversation view).uiBlocks)
        view.nativeAgentStatus `shouldBe` "cancelled"
        states `shouldSatisfy` all (/= BlockRunning)

    it "creates and terminates a placeholder for reordered finish events" do
        let tracked =
                applyNativeAgentEvent
                    (NativeAgentFinished "late" NativeAgentFailed)
                    emptyNativeAgentStore
            view = lookupView "late" tracked
        view.nativeAgentStatus `shouldBe` "error"
        nativeAgentTranscript view `shouldBe` []

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
                , async = Nothing
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
                , async = Nothing
                }
            restored =
                restoreNativeAgents
                    (AgentNative "agent-1")
                    [call, output]
                    emptyNativeAgentStore
            view = lookupView "agent-1" restored
        view.nativeAgentLabel `shouldBe` "Review API"
        view.nativeAgentModel `shouldBe` Just "sonnet"
        view.nativeAgentStatus `shouldBe` "done"
        nativeAgentTranscript view `shouldBe` ["review complete"]

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
                , async = Nothing
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
                , async = Nothing
                }
            restored =
                restoreNativeAgents
                    AgentRoot
                    [ call "claude-unpaired" "claude-code"
                    , wrongOutput
                    , call "other" "openai"
                    ]
                    emptyNativeAgentStore
        nativeAgentStoreSize restored `shouldBe` 0

    it "restores a selected row beyond the bounded pending-output window" do
        let selectedId = "selected"
            selectedCall = FunctionCallItem FunctionCall
                { itemId = Nothing
                , callId = selectedId
                , name = "Agent"
                , namespace = Nothing
                , provider = Just "claude-code"
                , arguments = "{}"
                , encryptedFunctionArgs = Nothing
                , status = Just ItemCompleted
                , async = Nothing
                }
            output identifier = FunctionCallOutputItem FunctionCallOutput
                { itemId = Nothing
                , callId = identifier
                , name = Nothing
                , namespace = Nothing
                , provider = Just "claude-code"
                , output =
                    rawJsonFromEncoding
                        (Aeson.toEncoding ("done" :: String))
                , status = Just ItemCompleted
                , async = Nothing
                }
            newerUnpaired =
                [ output
                    (Text.pack ("unpaired-" <> show index))
                | index <- [1 .. nativeAgentMaxEntries + 16]
                ]
            restored =
                restoreNativeAgents
                    (AgentNative selectedId)
                    (selectedCall : output selectedId : newerUnpaired)
                    emptyNativeAgentStore
        nativeAgentLookup selectedId restored
            `shouldSatisfy` maybe False
                ((== "done") . (.nativeAgentStatus))

    it "bounds terminal rows and retained output" do
        let payload = Text.replicate (nativeAgentPreviewBytes `div` 2) "x"
            tracked =
                foldl
                    (flip applyNativeAgentEvent)
                    emptyNativeAgentStore
                    (concat
                        [ [ NativeAgentStarted identifier Nothing identifier Nothing
                          , NativeAgentOutput identifier payload
                          , NativeAgentFinished identifier NativeAgentCompleted
                          ]
                        | index <- [1 .. nativeAgentMaxEntries + 16]
                        , let identifier = Text.pack ("agent-" <> show index)
                        ])
        nativeAgentStoreSize tracked `shouldBe` nativeAgentMaxEntries
        nativeAgentStoreBytes tracked
            `shouldSatisfy` (<= nativeAgentAggregateBytes)

    it "bounds rows even when a provider never finishes older agents" do
        let tracked =
                foldl
                    (flip applyNativeAgentEvent)
                    emptyNativeAgentStore
                    [ NativeAgentStarted identifier Nothing identifier Nothing
                    | index <- [1 .. nativeAgentMaxEntries + 16]
                    , let identifier = Text.pack ("running-" <> show index)
                    ]
        nativeAgentStoreSize tracked `shouldBe` nativeAgentMaxEntries
        nativeAgentLookup "running-1" tracked `shouldBe` Nothing
        nativeAgentLookup
            (Text.pack ("running-" <> show (nativeAgentMaxEntries + 16)))
            tracked
            `shouldSatisfy` maybe False (const True)

    it "retains only a bounded tail of one oversized native output" do
        let suffix = "newest-tail"
            oversized =
                Text.replicate ((8 * 1024 * 1024 `div` 4) + 1) "x"
                    <> suffix
            tracked =
                applyNativeAgentEvent
                    (NativeAgentOutput "large" oversized)
                    emptyNativeAgentStore
            view = lookupView "large" tracked
            retained = Text.concat (nativeAgentTranscript view)
        nativeAgentStoreBytes tracked
            `shouldSatisfy` (<= 8 * 1024 * 1024)
        retained `shouldSatisfy` Text.isSuffixOf suffix
        retained `shouldSatisfy`
            Text.isInfixOf "[older native-agent output omitted]"

    it "materializes conversation state only for the selected native row" do
        let tracked =
                foldl
                    (flip applyNativeAgentEvent)
                    emptyNativeAgentStore
                    [ NativeAgentOutput "one" "first"
                    , NativeAgentOutput "two" "second"
                    ]
            entries = nativeAgentEntries (AgentNative "one") tracked
            one = find ((== AgentNative "one") . (.agentTarget)) entries
            two = find ((== AgentNative "two") . (.agentTarget)) entries
        (Foldable.toList . (.uiBlocks) . (.agentConversation) <$> one)
            `shouldSatisfy` maybe False (not . null)
        (Foldable.toList . (.uiBlocks) . (.agentConversation) <$> two)
            `shouldBe` Just []

    it "retains bounded live terminal rows while persistence catches up" do
        let pending =
                applyNativeAgentEvent
                    (NativeAgentFinished "stale" NativeAgentCompleted)
                    emptyNativeAgentStore
            restored = restoreNativeAgents AgentRoot [] pending
        nativeAgentLookup "stale" restored
            `shouldSatisfy` maybe False
                ((== "done") . (.nativeAgentStatus))

lookupView :: Text.Text -> NativeAgentStore -> NativeAgentView
lookupView identifier store =
    case nativeAgentLookup identifier store of
        Just view -> view
        Nothing -> error ("missing native agent: " <> Text.unpack identifier)
