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
    , setSchedulerPaused
    )
import Agent.GrokBuild.Dialect.Task (GrokSubagentSpecs)
import Agent.GrokBuild.Dialect.Tools
    ( closeGrokSession
    , grokTools
    , newGrokSession
    )
import Agent.GrokBuild.Dialect.Shell (stopGrokBackgroundTasks)
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
import Agent.Tools.PlanMode
    ( PlanModeEnv
    , PlanModeHooks
    , newPlanModeEnv
    , withPlanModeLifecycle
    )
import Agent.Tools.Types (AppTool, ToolEnv(..))
import Agent.Subagents (interruptActiveSubagents)
import Control.Exception.Safe (onException)
import Control.Monad (forM, forM_)
import Data.Maybe (isNothing)

data GrokCodingTools = GrokCodingTools
    { grokAppTools :: ![AppTool]
    , grokPlanMode :: !PlanModeEnv
    , grokSuspendGhci :: !(IO ())
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
        let planHooks =
                fmap
                    (withPlanModeLifecycle
                        ( do
                            forM_ scheduler (`setSchedulerPaused` True)
                            stopGrokBackgroundTasks session
                            suspendGhciSession ghci
                            forM_ rootMulti
                                (interruptActiveSubagents . (.multiRegistry))
                            pure (Right ())
                        )
                        (forM_ scheduler (`setSchedulerPaused` False)))
                    hooks
        plan <- newPlanModeEnv env.toolCwd planHooks
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
            , grokClose = closeResourceScope resources
            , grokAgentTypes = typesRef
            , grokRuntimeControl = runtimeControl
            }
