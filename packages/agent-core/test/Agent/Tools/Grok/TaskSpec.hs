module Agent.Tools.Grok.TaskSpec (spec) where

import Agent.Loop (LoopError(..), LoopResult(..), defaultLoopDispatch, emptyTokenUsage)
import Agent.InterAgentMessage (interAgentMessagePayload)
import System.OsPath (OsPath, unsafeEncodeUtf)
import Agent.Subagents
import Agent.ToolDispatch
    ( ToolCallResult(..)
    , dispatchToolCall
    , functionToolCall
    , noArgsTool
    )
import Agent.Tools.Grok.Task
import Agent.Subagents.TaskPath (taskPathRoot)
import Agent.Tools.MultiAgents (MultiAgentContext(..), SubagentWorktree(..))
import Agent.Tools.Types
    ( AppTool(..)
    , ApprovalRule(..)
    , ToolExecutionPolicy(..)
    , ToolSchema(..)
    )
import Control.Concurrent.MVar
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Hspec

fromFilePath = unsafeEncodeUtf

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
                (pure Nothing) Nothing Nothing Nothing Nothing
            tool = taskTool (fromFilePath "/tmp") ctx typesRef
        result <- dispatchToolCall defaultLoopDispatch [tool.appToolHandler]
            (functionToolCall "c1" "task"
                "{\"prompt\":\"hello\",\"description\":\"test task\",\"model\":\"grok-4.5-mini\"}")
        result.output `shouldSatisfy` Text.isInfixOf "Subagent started in background"
        result.output `shouldSatisfy` Text.isInfixOf "subagent_id: agent-"
        specs <- Map.elems <$> readIORef typesRef
        map (\entry -> entry.modelOverride) specs `shouldBe` [Just "grok-4.5-mini"]
        closeSubagentRegistry registry

    it "records overrides before the child supervisor starts" do
        typesRef <- newIORef Map.empty
        observed <- newEmptyMVar
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (observeSpec typesRef observed)
            (\_ _ -> pure ())
        let ctx = MultiAgentContext registry Nothing 0 taskPathRoot
                (pure Nothing) Nothing Nothing Nothing Nothing
            tool = taskTool (fromFilePath "/tmp") ctx typesRef
        _ <- dispatchToolCall defaultLoopDispatch [tool.appToolHandler]
            (functionToolCall "c1" "task" raceArgs)
        takeMVar observed `shouldReturn` (Just "explore", Just "grok-4.5-mini")
        closeSubagentRegistry registry

    it "updates an agent type without discarding its overrides" do
        specsRef <- newIORef Map.empty
        let agentId = SubagentId "agent-1"
        recordAgentSpec specsRef agentId GrokSubagentSpec
            { agentType = "explore"
            , modelOverride = Just "grok-4.5-mini"
            , reasoningEffortOverride = Just "high"
            }
        recordAgentType specsRef agentId "plan"
        lookupAgentType specsRef agentId `shouldReturn` Just "plan"
        lookupAgentModel specsRef agentId
            `shouldReturn` Just "grok-4.5-mini"
        lookupAgentReasoningEffort specsRef agentId
            `shouldReturn` Just "high"

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

    it "keeps an isolated worktree until its child is closed" do
        cleaned <- newIORef False
        registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
            (\_ _ _ _ -> pure $ Left LoopNoResponseId)
            (\_ _ -> pure ())
        typesRef <- newIORef Map.empty
        let createIsolated = cleanupLease cleaned
            ctx = MultiAgentContext registry Nothing 0 taskPathRoot (pure Nothing)
                Nothing (Just createIsolated) Nothing Nothing
            tool = taskTool (fromFilePath "/tmp") ctx typesRef
        result <- dispatchToolCall defaultLoopDispatch [tool.appToolHandler]
            (functionToolCall "c1" "task"
                "{\"prompt\":\"x\",\"description\":\"y\",\"isolation\":\"worktree\"}")
        result.output `shouldSatisfy` Text.isInfixOf "worktree_path: /tmp/isolate"
        agents <- listAgents registry Nothing
        case agents of
            [(_, agentId, _)] -> do
                _ <- waitSubagents registry [agentId] 15000
                readIORef cleaned `shouldReturn` False
                _ <- closeSubagent registry agentId
                readIORef cleaned `shouldReturn` True
            _ -> expectationFailure "expected exactly one isolated child"
        closeSubagentRegistry registry

    it "cleans up worktrees after failed registry admission" do
        cleaned <- newIORef False
        registry <- closedRegistry
        typesRef <- newIORef Map.empty
        let ctx = MultiAgentContext registry Nothing 0 taskPathRoot
                (pure Nothing) Nothing (Just (cleanupLease cleaned)) Nothing Nothing
            tool = taskTool (fromFilePath "/tmp") ctx typesRef
        result <- dispatchToolCall defaultLoopDispatch [tool.appToolHandler]
            (functionToolCall "c1" "task" worktreeArgs)
        result.output `shouldSatisfy` Text.isInfixOf "registry is closed"
        readIORef cleaned `shouldReturn` True

fake :: Text -> AppTool
fake name = AppTool
    { appToolName = name
    , appToolDescription = name
    , appToolSchema = JsonFunctionSchema []
    , appToolHandler = noArgsTool name (pure (Right "ok"))
    , appToolApproval = AlwaysReadOnly
    , appToolExecution = ParallelSafe
    }

raceArgs :: Text
raceArgs =
    "{\"prompt\":\"hello\",\"description\":\"race test\",\
    \\"subagent_type\":\"explore\",\"model\":\"grok-4.5-mini\"}"

observeSpec
    :: GrokSubagentSpecs
    -> MVar (Maybe Text, Maybe Text)
    -> RunSubagent
observeSpec specs observed env _ _ _ = do
    agentType <- lookupAgentType specs env.subId
    agentModel <- lookupAgentModel specs env.subId
    putMVar observed (agentType, agentModel)
    pure $ Right LoopResult
        { finalResponseId = "c"
        , finalText = Just "done"
        , turnsUsed = 1
        , tokenUsage = emptyTokenUsage
        }

worktreeArgs :: Text
worktreeArgs =
    "{\"prompt\":\"x\",\"description\":\"cleanup\",\
    \\"isolation\":\"worktree\"}"

closedRegistry :: IO SubagentRegistry
closedRegistry = do
    registry <- newSubagentRegistry defaultSubagentConfig (fromFilePath "/tmp")
        (\_ _ _ _ -> pure $ Left LoopNoResponseId)
        (\_ _ -> pure ())
    closeSubagentRegistry registry
    pure registry

cleanupLease :: IORef Bool -> OsPath -> IO (Either Text SubagentWorktree)
cleanupLease cleaned _ =
    pure $ Right SubagentWorktree
        { subagentWorktreePath = fromFilePath "/tmp/isolate"
        , subagentWorktreeCleanup =
            writeIORef cleaned True >> pure (Right ())
        }
