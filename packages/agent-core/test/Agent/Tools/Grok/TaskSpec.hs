module Agent.Tools.Grok.TaskSpec (spec) where

import Agent.Loop (LoopError(..), LoopResult(..), defaultLoopDispatch, emptyTokenUsage)
import Agent.InterAgentMessage (interAgentMessagePayload)
import Agent.OsPath (fromFilePath)
import Agent.Subagents
import Agent.ToolDispatch
    ( ToolCallResult(..)
    , dispatchToolCall
    , functionToolCall
    , noArgsTool
    )
import Agent.Tools.Grok.Task
import Agent.Subagents.TaskPath (taskPathRoot)
import Agent.Tools.MultiAgents (MultiAgentContext(..))
import Agent.Tools.Types (AppTool(..), AppToolKind(..))
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = describe "Agent.Tools.Grok.Task" do
    it "defaults run_in_background and spawns a background agent" do
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\_ _ prompt _ -> pure $ Right LoopResult
                { finalResponseId = "c"
                , finalText = Just ("done:" <> interAgentMessagePayload prompt)
                , turnsUsed = 1
                , tokenUsage = emptyTokenUsage
                })
            (\_ _ -> pure ())
        typesRef <- newIORef Map.empty
        let ctx = MultiAgentContext registry Nothing 0 taskPathRoot
                (pure Nothing) Nothing Nothing Nothing
            tool = taskTool (fromFilePath "/tmp") ctx typesRef
        result <- dispatchToolCall defaultLoopDispatch [tool.appToolHandler]
            (functionToolCall "c1" "task"
                "{\"prompt\":\"hello\",\"description\":\"test task\",\"model\":\"grok-4.5-mini\"}")
        result.output `shouldSatisfy` Text.isInfixOf "Subagent started in background"
        result.output `shouldSatisfy` Text.isInfixOf "subagent_id: agent-"
        specs <- Map.elems <$> readIORef typesRef
        map (\entry -> entry.modelOverride) specs `shouldBe` [Just "grok-4.5-mini"]
        closeSubagentRegistry registry

    it "filters explore tools to the Grok Build read-only set" do
        let tools =
                [ fake "read_file"
                , fake "search_replace"
                , fake "task"
                , fake "grep"
                , fake "run_terminal_cmd"
                ]
            names = map (.appToolName) (filterGrokToolsForType "explore" tools)
        names `shouldBe` ["read_file", "grep"]
        names `shouldNotContain` ["search_replace"]
        names `shouldNotContain` ["task"]

    it "filters task out of general-purpose children" do
        let tools = [fake "read_file", fake "task"]
            names = map (.appToolName) (filterGrokToolsForType "general-purpose" tools)
        names `shouldBe` ["read_file"]

    it "uses the host worktree hook for isolated children" do
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\_ _ _ _ -> pure $ Left LoopNoResponseId)
            (\_ _ -> pure ())
        typesRef <- newIORef Map.empty
        let createIsolated _ = pure (Right (fromFilePath "/tmp"))
            ctx = MultiAgentContext registry Nothing 0 taskPathRoot (pure Nothing)
                Nothing (Just createIsolated) Nothing
            tool = taskTool (fromFilePath "/tmp") ctx typesRef
        result <- dispatchToolCall defaultLoopDispatch [tool.appToolHandler]
            (functionToolCall "c1" "task"
                "{\"prompt\":\"x\",\"description\":\"y\",\"isolation\":\"worktree\"}")
        result.output `shouldSatisfy` Text.isInfixOf "worktree_path: /tmp"
        closeSubagentRegistry registry

fake :: Text -> AppTool
fake name = AppTool
    { appToolName = name
    , appToolDescription = name
    , appToolParameters = []
    , appToolHandler = noArgsTool name (pure (Right "ok"))
    , appToolKind = JsonFunction
    , appToolReadOnly = True
    , appToolIsReadOnlyCall = Nothing
    }
