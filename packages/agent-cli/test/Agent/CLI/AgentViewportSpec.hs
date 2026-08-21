module Agent.CLI.AgentViewportSpec (spec) where

import Agent.CLI.AgentViewport
import Agent.CLI.Picker (PickerKey(..))
import Agent.Subagents (SubagentId(..), SubagentStatus(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = do
    describe "renderAgentTree" do
        it "stays hidden until a child agent exists" do
            renderAgentTree False AgentRoot [rootEntry] `shouldBe` ""

        it "renders a deterministic filesystem hierarchy" do
            let entries =
                    [ child "beta" "/root/beta" "running"
                    , child "gamma" "/root/alpha/gamma" "done"
                    , rootEntry
                    , child "alpha" "/root/alpha" "running"
                    ]
            renderAgentTree False (AgentChild (SubagentId "gamma")) entries
                `shouldBe`
                    Text.intercalate "\n"
                        [ "agents"
                        , "  ▾ root  active"
                        , "  ├─ alpha  running"
                        , "› │  └─ gamma  done"
                        , "  └─ beta  running"
                        , "  viewing /root/alpha/gamma · /agents to switch"
                        ]

    describe "agent viewport selection" do
        let entries =
                [ rootEntry
                , child "alpha" "/root/alpha" "running"
                , child "beta" "/root/beta" "done"
                ]
            state = initialAgentViewportState AgentRoot entries

        it "wraps through agents with arrow keys" do
            up <- rightState (applyAgentViewportKey PickerKeyUp state)
            up.viewportIndex `shouldBe` 2
            down <- rightState (applyAgentViewportKey PickerKeyDown up)
            down.viewportIndex `shouldBe` 0

        it "confirms the currently previewed agent and preserves cancel" do
            moved <- rightState (applyAgentViewportKey PickerKeyDown state)
            applyAgentViewportKey PickerKeyConfirm moved
                `shouldBe` Left (Just (AgentChild (SubagentId "alpha")))
            applyAgentViewportKey PickerKeyCancel moved
                `shouldBe` Left Nothing

        it "selects clicked targets and preserves them across live refreshes" do
            let selected =
                    selectAgentTarget
                        (AgentChild (SubagentId "alpha"))
                        state
                refreshed =
                    refreshAgentViewportState
                        [ rootEntry
                        , child "alpha" "/root/alpha" "done"
                        , child "gamma" "/root/alpha/gamma" "running"
                        ]
                        selected
            (.agentTarget) <$> selectedAgentEntry refreshed
                `shouldBe` Just (AgentChild (SubagentId "alpha"))
            (.agentStatus) <$> selectedAgentEntry refreshed
                `shouldBe` Just "done"

        it "renders hierarchy and transcript panes" do
            let frame = renderAgentViewportFrameFor False 10 70 state
            frame `shouldSatisfy` Text.isInfixOf "hierarchy"
            frame `shouldSatisfy` Text.isInfixOf "transcript · /root"
            frame `shouldSatisfy` Text.isInfixOf "assistant: ready"

        it "keeps the selected child transcript visible after the picker" do
            let selected = AgentChild (SubagentId "alpha")
                panel = renderAgentViewportPanelFor False 70 selected entries
            panel `shouldSatisfy` Text.isInfixOf "transcript · /root/alpha"
            panel `shouldSatisfy` Text.isInfixOf "assistant: working"
            panel `shouldSatisfy` Text.isInfixOf "input routes to /root"

    describe "formatAgentStatus" do
        it "uses compact status labels" do
            map formatAgentStatus
                [ Pending
                , Running
                , Completed (Just "ok")
                , Errored "boom"
                , Interrupted
                ]
                `shouldBe` ["pending", "running", "done", "error", "interrupted"]

rootEntry :: AgentEntry
rootEntry =
    AgentEntry
        { agentTarget = AgentRoot
        , agentPath = "/root"
        , agentStatus = "active"
        , agentTranscript = ["user: hello", "assistant: ready"]
        }

child :: Text -> Text -> Text -> AgentEntry
child agentId path status =
    AgentEntry
        { agentTarget = AgentChild (SubagentId agentId)
        , agentPath = path
        , agentStatus = status
        , agentTranscript = ["assistant: working"]
        }

rightState :: Either (Maybe AgentTarget) AgentViewportState -> IO AgentViewportState
rightState result = case result of
    Right state -> pure state
    Left selected -> fail ("expected viewport state, got " <> show selected)
