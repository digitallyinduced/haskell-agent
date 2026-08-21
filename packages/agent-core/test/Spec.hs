module Main (main) where

import qualified Agent.CancelSpec as CancelSpec
import qualified Agent.ErrorSpec as ErrorSpec
import qualified Agent.JsonTextSpec as JsonTextSpec
import qualified Agent.LoopSpec as LoopSpec
import qualified Agent.ProjectInstructionsSpec as ProjectInstructionsSpec
import qualified Agent.SubagentsSpec as SubagentsSpec
import qualified Agent.Subagents.TaskPathSpec as TaskPathSpec
import qualified Agent.ToolArgsSpec as ToolArgsSpec
import qualified Agent.ToolDispatchSpec as ToolDispatchSpec
import qualified Agent.ToolDSLSpec as ToolDSLSpec
import qualified Agent.Tools.CodexSpec as CodexToolsSpec
import qualified Agent.Tools.DangerousSpec as DangerousSpec
import qualified Agent.Tools.GrokSpec as GrokToolsSpec
import qualified Agent.Tools.Grok.TaskSpec as GrokTaskSpec
import qualified Agent.Tools.GhciSpec as GhciSpec
import qualified Agent.Tools.IOSpec as IOSpec
import qualified Agent.Tools.MultiAgentsSpec as MultiAgentsSpec
import qualified Agent.Tools.PlanModeSpec as PlanModeSpec
import qualified Agent.Transport.WebSocketSpec as WebSocketSpec
import Test.Hspec (hspec)

main :: IO ()
main = hspec do
    CancelSpec.spec
    ErrorSpec.spec
    JsonTextSpec.spec
    LoopSpec.spec
    ProjectInstructionsSpec.spec
    SubagentsSpec.spec
    TaskPathSpec.spec
    ToolArgsSpec.spec
    ToolDispatchSpec.spec
    ToolDSLSpec.spec
    GrokToolsSpec.spec
    GrokTaskSpec.spec
    GhciSpec.spec
    IOSpec.spec
    MultiAgentsSpec.spec
    PlanModeSpec.spec
    CodexToolsSpec.spec
    DangerousSpec.spec
    WebSocketSpec.spec
