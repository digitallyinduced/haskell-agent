module Agent.Codex.Dialect.Runtime
    ( CodexCodingTools(..)
    , newCodexCodingTools
    , newCodexCodingToolsWithTaskPlan
    ) where

import Agent.Codex.Dialect.Shell
    ( closeCodexShellSession
    , newCodexShellSession
    , resetCodexShellSession
    )
import Agent.Codex.Dialect.Tools (codexTools)
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
import Agent.Tools.MultiAgents (MultiAgentContext)
import Agent.Tools.PlanMode (PlanModeEnv, PlanModeHooks, newPlanModeEnv)
import Agent.Tools.TaskPlan (TaskPlanEnv, newTaskPlanEnv)
import Agent.Tools.Types (AppTool, ToolEnv(..))
import Control.Exception.Safe (onException)
import System.OsPath (OsPath)

data CodexCodingTools = CodexCodingTools
    { codexAppTools :: ![AppTool]
    , codexPlanMode :: !PlanModeEnv
    , codexTaskPlan :: !TaskPlanEnv
    , codexSuspendGhci :: !(IO ())
    , codexResetSessionTemp :: !(OsPath -> IO ())
    , codexClose :: !(IO ())
    }

newCodexCodingTools
    :: ToolEnv
    -> Maybe PlanModeHooks
    -> Maybe MultiAgentContext
    -> IO CodexCodingTools
newCodexCodingTools env hooks multi = do
    taskPlan <- newTaskPlanEnv Nothing Nothing
    newCodexCodingToolsWithTaskPlan env hooks taskPlan multi

newCodexCodingToolsWithTaskPlan
    :: ToolEnv
    -> Maybe PlanModeHooks
    -> TaskPlanEnv
    -> Maybe MultiAgentContext
    -> IO CodexCodingTools
newCodexCodingToolsWithTaskPlan env hooks taskPlan multi = do
    resources <- newResourceScope
    flip onException (closeResourceScope resources) do
        plan <- newPlanModeEnv env.toolCwd hooks
        (_, shellSession) <- allocateResource resources
            (newCodexShellSession env)
            closeCodexShellSession
        (_, ghci) <- allocateResource resources
            (newGhciSession env)
            closeGhciSession
        tools <- codexTools env shellSession ghci plan taskPlan multi
        pure CodexCodingTools
            { codexAppTools = tools
            , codexPlanMode = plan
            , codexTaskPlan = taskPlan
            , codexSuspendGhci = suspendGhciSession ghci
            , codexResetSessionTemp = \_tempDir -> do
                suspendGhciSession ghci
                resetCodexShellSession shellSession
            , codexClose = closeResourceScope resources
            }
