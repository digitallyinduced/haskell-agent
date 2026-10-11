{-# LANGUAGE RankNTypes #-}
-- | Provider-independent child execution. A provider scopes its connection
-- through withBackend; the library owns loop construction and collaboration.
module Agent.Tools.MultiAgents.Execution
    ( ChildExecution (..)
    , childExecutionRunner
    ) where

import qualified Agent.Loop as Agent
import Agent.Subagents
import qualified Agent.Tools.MultiAgents as Multi
import qualified Agent.Tools.Types as Tools
import qualified Agent.ToolDispatch as Dispatch
import Control.Monad (when)
import Data.IORef (newIORef)
import qualified Data.Text as Text

data ChildExecution = ChildExecution
    { domainTools :: Tools.ToolRegistry
    , collaborationContext :: Multi.MultiAgentContext
    -- | Receives the complete tool set to advertise to the provider.
    , withBackend :: forall a. [Tools.AppTool]
        -> ((Agent.Backend, Agent.LoopEvent -> IO ()) -> IO a) -> IO a
    , shouldStop :: IO Bool
    , approveTool :: Dispatch.ToolCall -> IO Agent.ToolApproval
    , maxTurns :: Int
    , finishTurn :: Either Agent.LoopError Agent.LoopResult -> IO ()
    }

childExecutionRunner
    :: (SubagentSpawnEnv -> IO ChildExecution) -> SubagentRegistry -> RunSubagent
childExecutionRunner prepare registry environment _previous message onEvent = do
    execution@ChildExecution { withBackend } <- prepare environment
    tools <- either (fail . Text.unpack) pure $ Tools.mkToolRegistry
        (Tools.toolRegistryTools execution.domainTools
            <> Multi.multiAgentTools execution.collaborationContext)
    transcript <- newIORef Agent.emptyBackendSnapshot
    let checkStopped =
            execution.shouldStop >>= \stopped ->
                when stopped (fail "Subagent execution was stopped")
    result <- withBackend (Tools.toolRegistryTools tools) \(backend, persistEvent) ->
        runChildTurn registry environment ChildTurn
            { childBackend = Agent.backendWithCallbacks \snapshot previous inputs pending ->
                checkStopped >> backend.submitTurnWithCallbacks snapshot previous inputs pending
            , childTools = tools
            , childDispatch = Agent.defaultLoopDispatch
            , childApproval = \call -> checkStopped >> execution.approveTool call
            , childMaxTurns = execution.maxTurns
            , childOnEvent = \event -> persistEvent event >> onEvent event
            , childTranscript = transcript
            , childBackgroundTasks = Nothing
            }
            \loop -> Agent.runLoopInputs loop Nothing [Agent.AgentMessage message]
    execution.finishTurn result
    pure result
