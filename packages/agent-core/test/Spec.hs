module Main (main) where

import qualified Agent.CancelSpec as CancelSpec
import qualified Agent.ErrorSpec as ErrorSpec
import qualified Agent.LoopSpec as LoopSpec
import qualified Agent.ProjectInstructionsSpec as ProjectInstructionsSpec
import qualified Agent.ToolArgsSpec as ToolArgsSpec
import qualified Agent.ToolDispatchSpec as ToolDispatchSpec
import qualified Agent.ToolDSLSpec as ToolDSLSpec
import qualified Agent.Tools.CodexSpec as CodexToolsSpec
import qualified Agent.Tools.GrokSpec as GrokToolsSpec
import qualified Agent.Tools.GhciSpec as GhciSpec
import qualified Agent.Tools.IOSpec as IOSpec
import qualified Agent.Transport.WebSocketSpec as WebSocketSpec
import Test.Hspec (hspec)

main :: IO ()
main = hspec do
    CancelSpec.spec
    ErrorSpec.spec
    LoopSpec.spec
    ProjectInstructionsSpec.spec
    ToolArgsSpec.spec
    ToolDispatchSpec.spec
    ToolDSLSpec.spec
    GrokToolsSpec.spec
    GhciSpec.spec
    IOSpec.spec
    CodexToolsSpec.spec
    WebSocketSpec.spec
