module Agent.Tools.MultiAgentsExecutionSpec (spec) where

import qualified Agent.Loop as Agent
import Agent.Subagents
import Agent.Tools.MultiAgents (rootMultiAgentContext)
import Agent.Tools.MultiAgents.Execution
import qualified Agent.Tools.Types as Tools
import Agent.ToolDispatch (functionToolCall)
import Control.Exception.Safe (bracket, finally)
import Data.IORef
import System.OsPath (unsafeEncodeUtf)
import Test.Hspec

spec :: Spec
spec = describe "library child execution" do
    it "advertises collaboration tools and closes the provider before settlement" $
      withRegistry \registry -> do
        advertised <- newIORef []
        order <- newIORef ([] :: [String])
        execution <- fixture registry
        let configured = execution
                { withBackend = \tools action -> do
                    writeIORef advertised (map (.appToolName) tools)
                    action (answering, const (pure ()))
                        `finally` modifyIORef' order (<> ["provider-closed"])
                , finishTurn = \_ -> modifyIORef' order (<> ["settled"])
                }
        setSubagentRunner registry (childExecutionRunner (const (pure configured)) registry)
        Right identifier <- spawnSubagent registry Nothing 0 "task" Nothing
        (_, timedOut) <- waitSubagents registry [identifier] 1000
        timedOut `shouldBe` False
        readIORef advertised >>= (`shouldContain` ["spawn_agent", "wait_agent"])
        readIORef order `shouldReturn` ["provider-closed", "settled"]

    it "checks stop permission before contacting the provider and releases its scope" $
      withRegistry \registry -> do
        submitted <- newIORef False
        released <- newIORef False
        execution <- fixture registry
        let backend = Agent.Backend \snapshot _ _ _ -> do
                writeIORef submitted True
                pure (Right (Agent.BackendResult (Agent.emptyTurnOutput "response" [] (Just "done")) snapshot))
            configured = execution
                { shouldStop = pure True
                , withBackend = \_ action -> action (backend, const (pure ()))
                    `finally` writeIORef released True
                }
        setSubagentRunner registry (childExecutionRunner (const (pure configured)) registry)
        Right identifier <- spawnSubagent registry Nothing 0 "task" Nothing
        (_, timedOut) <- waitSubagents registry [identifier] 1000
        timedOut `shouldBe` False
        readIORef submitted `shouldReturn` False
        readIORef released `shouldReturn` True

    it "enforces the configured turn limit" $
      withRegistry \registry -> do
        outcome <- newIORef Nothing
        execution <- fixture registry
        let configured = execution
                { maxTurns = 1
                , withBackend = \_ action -> action
                    (Agent.Backend (\snapshot _ _ _ -> pure (Right (Agent.BackendResult
                        (Agent.emptyTurnOutput "response"
                            [functionToolCall "list" "list_agents" "{}"] Nothing) snapshot)))
                    , const (pure ()))
                , finishTurn = writeIORef outcome . Just
                }
        setSubagentRunner registry (childExecutionRunner (const (pure configured)) registry)
        Right identifier <- spawnSubagent registry Nothing 0 "task" Nothing
        _ <- waitSubagents registry [identifier] 1000
        readIORef outcome >>= \case
            Just (Left (Agent.LoopMaxTurns _)) -> pure ()
            _ -> expectationFailure "expected configured turn limit"

withRegistry :: (SubagentRegistry -> IO a) -> IO a
withRegistry = bracket
    (newSubagentRegistry defaultSubagentConfig (unsafeEncodeUtf ".")
        (\_ _ _ _ -> pure (Left Agent.LoopNoResponseId)) (\_ _ -> pure ()))
    closeSubagentRegistry

fixture :: SubagentRegistry -> IO ChildExecution
fixture registry = do
    root <- newRootSubagents NoticesToLoop registry
    tools <- either (fail . show) pure (Tools.mkToolRegistry [])
    pure ChildExecution
        { domainTools = tools
        , collaborationContext = rootMultiAgentContext root (unsafeEncodeUtf ".")
        , withBackend = \_ action -> action (answering, const (pure ()))
        , shouldStop = pure False
        , approveTool = const (pure Tools.ToolApprovalGranted)
        , maxTurns = 4
        , finishTurn = const (pure ())
        }

answering :: Agent.Backend
answering = Agent.Backend \snapshot _ _ _ ->
    pure (Right (Agent.BackendResult (Agent.emptyTurnOutput "response" [] (Just "done")) snapshot))
