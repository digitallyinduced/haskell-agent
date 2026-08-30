module Agent.GrokBuild.Dialect.Runtime
    ( GrokCodingTools(..)
    , GrokRuntimeControl(..)
    , newGrokCodingTools
    ) where

import Agent.GrokBuild.Dialect.Goal
    ( GoalRuntime
    , newGoalRuntime
    )
import Agent.GrokBuild.Dialect.Scheduler
    ( SchedulerRuntime
    , closeSchedulerRuntime
    , newSchedulerRuntime
    )
import Agent.GrokBuild.Dialect.Task (GrokSubagentSpecs)
import Agent.GrokBuild.Dialect.Tools
    ( closeGrokSession
    , grokTools
    , newGrokSession
    )
import Agent.GrokBuild.Dialect.Shell (resetGrokSessionTemp)
import Agent.GrokBuild.Dialect.Workflow
    ( WorkflowRuntime
    , newWorkflowRuntime
    )
import Agent.ResourceScope
    ( allocateResource
    , closeResourceScope
    , newResourceScope
    )
import Agent.Tools.Ghci
    ( closeGhciSession
    , newGhciSession
    , suspendGhciSession
    )
import Agent.Tools.MultiAgents (MultiAgentContext(..))
import Agent.Tools.PlanMode (PlanModeEnv, PlanModeHooks, newPlanModeEnv)
import Agent.Tools.Types (AppTool, ToolEnv(..))
import Control.Exception.Safe (onException)
import Control.Monad (forM)
import Data.Maybe (isNothing)
import System.OsPath (OsPath)

data GrokCodingTools = GrokCodingTools
    { grokAppTools :: ![AppTool]
    , grokPlanMode :: !PlanModeEnv
    , grokSuspendGhci :: !(IO ())
    , grokResetSessionTemp :: !(OsPath -> IO ())
    , grokClose :: !(IO ())
    , grokAgentTypes :: !GrokSubagentSpecs
    , grokRuntimeControl :: !GrokRuntimeControl
    }

-- | Session-local controls used by host slash commands. Optional runtimes are
-- absent when their backing tool is unavailable (for example in a child
-- agent), allowing the host command catalog to gate on real capability.
data GrokRuntimeControl = GrokRuntimeControl
    { grokGoalRuntime :: !GoalRuntime
    , grokSchedulerRuntime :: !(Maybe SchedulerRuntime)
    , grokWorkflowRuntime :: !(Maybe WorkflowRuntime)
    }

newGrokCodingTools
    :: ToolEnv
    -> Maybe PlanModeHooks
    -> Maybe MultiAgentContext
    -> GrokSubagentSpecs
    -> IO GrokCodingTools
newGrokCodingTools env hooks multi typesRef = do
    resources <- newResourceScope
    flip onException (closeResourceScope resources) do
        plan <- newPlanModeEnv env.toolCwd hooks
        (_, session) <- allocateResource resources
            (newGrokSession env)
            closeGrokSession
        (_, ghci) <- allocateResource resources
            (newGhciSession env)
            closeGhciSession
        goals <- newGoalRuntime
        let rootMulti = case multi of
                Just ctx | isNothing ctx.multiSelfId -> Just ctx
                _ -> Nothing
        scheduler <- forM rootMulti \ctx ->
            snd <$> allocateResource resources
                (newSchedulerRuntime env.toolCwd ctx typesRef)
                closeSchedulerRuntime
        workflows <- forM rootMulti \ctx ->
            newWorkflowRuntime env.toolCwd ctx typesRef
        let runtimeControl = GrokRuntimeControl
                { grokGoalRuntime = goals
                , grokSchedulerRuntime = scheduler
                , grokWorkflowRuntime = workflows
                }
        pure GrokCodingTools
            { grokAppTools =
                grokTools
                    session
                    ghci
                    plan
                    goals
                    scheduler
                    workflows
                    multi
                    typesRef
            , grokPlanMode = plan
            , grokSuspendGhci = suspendGhciSession ghci
            , grokResetSessionTemp = \tempDir -> do
                suspendGhciSession ghci
                resetGrokSessionTemp session tempDir
            , grokClose = closeResourceScope resources
            , grokAgentTypes = typesRef
            , grokRuntimeControl = runtimeControl
            }
