module Agent.CLI.TUIAppSpec (spec) where

import Agent.CLI.AgentViewport (AgentEntry(..), AgentTarget(..))
import Agent.CLI.TUI.App
    ( agentEntryWindow
    , agentPaneEntryLimit
    , agentPaneVisible
    , fullscreenVtyConfig
    , repositoryHeaderText
    )
import Agent.Subagents (SubagentId(..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Graphics.Vty as V
import Test.Hspec

spec :: Spec
spec = do
    describe "fullscreenVtyConfig" do
        it "maps enhanced-keyboard Shift+Enter sequences before Vty decodes them" do
            V.configInputMap fullscreenVtyConfig
                `shouldMatchList`
                    [ ( Nothing
                      , "\ESC[27;2;13~"
                      , V.EvKey V.KEnter [V.MShift]
                      )
                    , ( Nothing
                      , "\ESC[13;2u"
                      , V.EvKey V.KEnter [V.MShift]
                      )
                    ]

    describe "repositoryHeaderText" do
        it "puts the git state before the full checkout path" do
            repositoryHeaderText
                "detached"
                "~/digitallyinduced/haskell-agent"
                `shouldBe`
                    "detached  ~/digitallyinduced/haskell-agent"

        it "still renders a path when git state is unavailable" do
            repositoryHeaderText "" "~/scratch"
                `shouldBe` "~/scratch"

    describe "Agents pane layout" do
        it "hides below the responsive breakpoint and without children" do
            agentPaneVisible 71 20 [rootEntry, childEntry 1]
                `shouldBe` False
            agentPaneVisible 72 20 [rootEntry, childEntry 1]
                `shouldBe` True
            agentPaneVisible 120 9 [rootEntry, childEntry 1]
                `shouldBe` False
            agentPaneVisible 120 20 [rootEntry]
                `shouldBe` False

        it "centers the selected row and reports hidden rows on both sides" do
            let entries = rootEntry : map childEntry [1 .. 6]
                selected = AgentChild (SubagentId "agent-4")
                (above, shown, below) =
                    agentEntryWindow 3 selected entries
            above `shouldBe` 3
            map (.agentTarget) shown
                `shouldBe`
                    [ AgentChild (SubagentId "agent-3")
                    , selected
                    , AgentChild (SubagentId "agent-5")
                    ]
            below `shouldBe` 1

        it "reserves height for truncation indicators and pane chrome" do
            let availableHeight = 15
                entries = rootEntry : map childEntry [1 .. 20]
                selected = AgentChild (SubagentId "agent-10")
                entryLimit = agentPaneEntryLimit availableHeight
                (above, shown, below) =
                    agentEntryWindow entryLimit selected entries
                indicatorRows =
                    fromEnum (above > 0) + fromEnum (below > 0)
                renderedRows =
                    length shown + indicatorRows + 7
            entryLimit `shouldBe` 6
            renderedRows `shouldSatisfy` (<= availableHeight)

rootEntry :: AgentEntry
rootEntry = AgentEntry
    { agentTarget = AgentRoot
    , agentPath = "/root"
    , agentStatus = "active"
    , agentSteps = []
    , agentTranscript = []
    }

childEntry :: Int -> AgentEntry
childEntry index = AgentEntry
    { agentTarget = AgentChild (SubagentId name)
    , agentPath = "/root/" <> name
    , agentStatus = "running"
    , agentSteps = []
    , agentTranscript = []
    }
  where
    name :: Text
    name = "agent-" <> Text.pack (show index)
