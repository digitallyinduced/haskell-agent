-- | Grok-build coding tools.
--
-- Wire names, JSON keys, and output phrasing are copied from
-- xai-org/grok-build @ crates/codegen/xai-grok-tools/src/implementations/grok_build.
-- Do not rename these to match Codex; Grok models are trained on this dialect.
module Agent.Tools.Grok
    ( grokTools
    , filterGrokToolsForType
    , newGrokSession
    , closeGrokSession
    , GrokSession
    ) where

import Agent.Tools.Ghci (GhciSession, runGhciTool)
import Agent.Tools.Grok.Grep (grepTool)
import Agent.Tools.Grok.ListDir (listDirTool)
import Agent.Tools.Grok.ReadFile (readFileTool)
import Agent.Tools.Grok.SearchReplace (searchReplaceTool)
import Agent.Tools.Grok.Shell
    ( GrokSession(..)
    , closeGrokSession
    , newGrokSession
    )
import Agent.Tools.Grok.Task
    ( GrokSubagentSpecs
    , filterGrokToolsForType
    , taskTool
    )
import Agent.Tools.Grok.TaskControl
    ( getTaskOutputTool
    , killTaskTool
    )
import Agent.Tools.Grok.Terminal (runTerminalCmdTool)
import Agent.Tools.MultiAgents (MultiAgentContext)
import Agent.Tools.PlanMode
    ( PlanModeEnv
    , askUserQuestionTool
    , enterPlanModeTool
    , exitPlanModeTool
    )
import Agent.Tools.Types (AppTool, ToolEnv(..))

-- Upstream: grok-build grok_build::{read_file, grep, list_dir, search_replace, bash,
-- get_task_output, kill_task, task, enter_plan_mode, exit_plan_mode, ask_user_question}.
-- Local extension: run_ghci (persistent GHCi with per-call purity approval).
grokTools
    :: GrokSession
    -> GhciSession
    -> PlanModeEnv
    -> Maybe MultiAgentContext
    -> GrokSubagentSpecs
    -> [AppTool]
grokTools session ghci planMode multi typesRef =
    let env = session.grokEnv
        base =
            [ readFileTool env
            , grepTool env
            , listDirTool env
            , searchReplaceTool env planMode
            , runTerminalCmdTool session
            , runGhciTool ghci
            , getTaskOutputTool session multi
            , killTaskTool session multi
            , enterPlanModeTool planMode
            , exitPlanModeTool planMode
            , askUserQuestionTool planMode
            ]
    in case multi of
        Nothing -> base
        Just ctx -> base ++ [taskTool env.toolCwd ctx typesRef]
