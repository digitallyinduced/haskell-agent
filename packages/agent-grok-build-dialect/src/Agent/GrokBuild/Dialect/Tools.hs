-- | Grok-build coding tools.
--
-- Wire names, JSON keys, and output phrasing are copied from
-- xai-org/grok-build @ crates/codegen/xai-grok-tools/src/implementations/grok_build.
-- Do not rename these to match Codex; Grok models are trained on this dialect.
module Agent.GrokBuild.Dialect.Tools
    ( grokTools
    , filterGrokToolsForType
    , newGrokSession
    , closeGrokSession
    , GrokSession
    ) where

import Agent.Tools.Ghci (GhciSession, runGhciTool)
import Agent.Tools.FileSystem.Grep (grepTool)
import Agent.Tools.FileSystem.ListDir (listDirTool)
import Agent.Tools.FileSystem.ReadFile (readFileTool)
import Agent.GrokBuild.Dialect.Monitor (monitorTool)
import Agent.GrokBuild.Dialect.SearchReplace (searchReplaceTool)
import Agent.GrokBuild.Dialect.Shell
    ( GrokSession(..)
    , closeGrokSession
    , newGrokSession
    )
import Agent.GrokBuild.Dialect.Task
    ( GrokSubagentSpecs
    , filterGrokToolsForType
    , taskTool
    )
import Agent.GrokBuild.Dialect.TaskControl
    ( getTaskOutputTool
    , killTaskTool
    , waitTasksTool
    )
import Agent.GrokBuild.Dialect.Terminal (runTerminalCmdTool)
import Agent.GrokBuild.Dialect.Todo (todoWriteTool)
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
            [ runGhciTool ghci
            , readFileTool env
            , grepTool env
            , listDirTool env
            , searchReplaceTool env planMode
            , runTerminalCmdTool session
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
