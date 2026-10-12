module Agent.Tools.MultiAgentsExecutionSpec (spec) where

import qualified Agent.Loop as Agent
import Agent.Subagents
import Agent.Tools.MultiAgents (rootMultiAgentContext)
import Agent.Tools.MultiAgents.Execution
import qualified Agent.Tools.Types as Tools
import Agent.ToolDispatch (functionToolCall)
import Control.Exception.Safe (bracket, finally)
import Control.Concurrent.MVar
import System.Timeout (timeout)
import Data.IORef
import System.OsPath (unsafeEncodeUtf)
import Test.Hspec

spec :: Spec
spec = describe "library child execution" do
    it "releases execution resources on asynchronous cancellation without settling a result" $
      withRegistry \registry -> do
        started <- newEmptyMVar
        blocked <- newEmptyMVar
        released <- newIORef (0 :: Int)
        settled <- newIORef False
        execution <- fixture registry
        let configured = execution
                { withBackend = \_ _ -> putMVar started () >> takeMVar blocked >> fail "unreachable"
                , closeExecution = modifyIORef' released (+ 1)
                , finishTurn = \_ -> writeIORef settled True
                }
        setSubagentRunner registry (childExecutionRunner (const (pure configured)) registry)
        Right _ <- spawnSubagent registry Nothing 0 "task" Nothing
        timeout 1000000 (takeMVar started) `shouldReturn` Just ()
        timeout 1000000 (closeSubagentRegistry registry) `shouldReturn` Just ()
        readIORef released `shouldReturn` 1
        readIORef settled `shouldReturn` False

    it "releases execution resources when provider acquisition fails" $
      withRegistry \registry -> do
        released <- newIORef (0 :: Int)
        settled <- newIORef False
        execution <- fixture registry
        let configured = execution
                { withBackend = \_ _ -> fail "provider unavailable"
                , closeExecution = modifyIORef' released (+ 1)
                , finishTurn = \_ -> writeIORef settled True
                }
        setSubagentRunner registry (childExecutionRunner (const (pure configured)) registry)
        Right identifier <- spawnSubagent registry Nothing 0 "task" Nothing
        _ <- waitSubagents registry [identifier] 1000
        readIORef released `shouldReturn` 1
        readIORef settled `shouldReturn` False

    it "yields after durable tool output and resumes without an error status" $
      withRegistry \registry -> do
        persisted <- newIORef False
        requests <- newIORef (0 :: Int)
        execution <- fixture registry
        let backend = Agent.Backend \snapshot _ _ _ -> do
                count <- atomicModifyIORef' requests \n -> (n + 1, n)
                pure (Right (Agent.BackendResult
                    (if count == 0 then Agent.emptyTurnOutput "first"
                        [functionToolCall "list" "list_agents" "{}"] Nothing
                     else Agent.emptyTurnOutput "second" [] (Just "done")) snapshot))
            configured = execution
                { shouldYield = readIORef persisted
                , withBackend = \_ action -> action (backend, \case
                    Agent.ToolFinished _ -> writeIORef persisted True
                    _ -> pure ())
                }
        setSubagentRunner registry (childExecutionRunner (const (pure configured)) registry)
        Right identifier <- spawnSubagent registry Nothing 0 "task" Nothing
        _ <- waitSubagents registry [identifier] 1000
        getStatus registry identifier `shouldReturn` Interrupted
        readIORef requests `shouldReturn` 1
        writeIORef persisted False
        _ <- sendInput registry identifier "user reply" False
        _ <- waitSubagents registry [identifier] 1000
        getStatus registry identifier `shouldReturn` Completed (Just "done")

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
        , shouldYield = pure False
        , approveTool = const (pure Tools.ToolApprovalGranted)
        , withToolExecution = \_ action -> action
        , maxTurns = 4
        , finishTurn = const (pure ())
        , closeExecution = pure ()
        }

answering :: Agent.Backend
answering = Agent.Backend \snapshot _ _ _ ->
    pure (Right (Agent.BackendResult (Agent.emptyTurnOutput "response" [] (Just "done")) snapshot))
