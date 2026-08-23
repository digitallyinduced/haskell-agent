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
import Agent.Tools.Grok.Monitor (monitorTool)
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
    , waitTasksTool
    )
import Agent.Tools.Grok.Terminal (runTerminalCmdTool)
import Agent.Tools.Grok.Todo (todoWriteTool)
import Agent.Tools.MultiAgents (MultiAgentContext)
import Agent.Tools.PlanMode
    ( PlanModeEnv
    , askUserQuestionTool
    , enterPlanModeTool
    , exitPlanModeTool
    )
import Agent.Tools.Types (AppTool, ToolEnv(..))

-- Core upstream parity: file tools, terminal/background lifecycle, progress,
-- monitor, subagents, and plan-mode interaction.
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
            , todoWriteTool session
            , getTaskOutputTool session multi
            , waitTasksTool session multi
            , killTaskTool session multi
            , monitorTool session
            , enterPlanModeTool planMode
            , exitPlanModeTool planMode
            , askUserQuestionTool planMode
            ]
    in case multi of
        Nothing -> base
        Just ctx -> base ++ [taskTool env.toolCwd ctx typesRef]
