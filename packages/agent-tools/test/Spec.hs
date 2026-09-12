module Main (main) where

import qualified Agent.ToolDispatchSpec as ToolDispatchSpec
import qualified Agent.Tools.CodeMode.HostSpec as CodeModeHostSpec
import qualified Agent.Tools.CodeMode.ProtocolSpec as CodeModeProtocolSpec
import qualified Agent.Tools.DangerousSpec as DangerousSpec
import qualified Agent.Tools.FileSystem.GrepSpec as GrepSpec
import qualified Agent.Tools.FileSystem.ListDirSpec as ListDirSpec
import qualified Agent.Tools.FileSystem.ReadFileSpec as ReadFileSpec
import qualified Agent.Tools.GhciSpec as GhciSpec
import qualified Agent.Tools.IOSpec as IOSpec
import qualified Agent.Tools.LoopIntegrationSpec as LoopIntegrationSpec
import qualified Agent.Tools.MultiAgentsSpec as MultiAgentsSpec
import qualified Agent.Tools.OutputArtifactSpec as OutputArtifactSpec
import qualified Agent.Tools.OutputArtifactMemorySpec as OutputArtifactMemorySpec
import qualified Agent.Tools.OutputArtifact.RetrievalSpec as OutputArtifactRetrievalSpec
import qualified Agent.Tools.PlanModeSpec as PlanModeSpec
import qualified Agent.Tools.TaskPlanSpec as TaskPlanSpec
import qualified Agent.Tools.SecretSpec as SecretSpec
import qualified Agent.Tools.ShowImageSpec as ShowImageSpec
import qualified Agent.Tools.ViewImageSpec as ViewImageSpec
import qualified Agent.Tools.RenderChartSpec as RenderChartSpec
import Test.Hspec (hspec)

main :: IO ()
main = hspec do
    ToolDispatchSpec.spec
    GrepSpec.spec
    ListDirSpec.spec
    ReadFileSpec.spec
    GhciSpec.spec
    IOSpec.spec
    LoopIntegrationSpec.spec
    MultiAgentsSpec.spec
    OutputArtifactSpec.spec
    OutputArtifactMemorySpec.spec
    OutputArtifactRetrievalSpec.spec
    PlanModeSpec.spec
    TaskPlanSpec.spec
    SecretSpec.spec
    ShowImageSpec.spec
    ViewImageSpec.spec
    RenderChartSpec.spec
    CodeModeHostSpec.spec
    CodeModeProtocolSpec.spec
    DangerousSpec.spec
