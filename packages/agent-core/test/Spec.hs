module Main (main) where

import qualified Agent.Auth.JWTSpec as JWTSpec
import qualified Agent.CancelSpec as CancelSpec
import qualified Agent.DialectSpec as DialectSpec
import qualified Agent.ErrorSpec as ErrorSpec
import qualified Agent.Http.HeaderSpec as HttpHeaderSpec
import qualified Agent.JsonTextSpec as JsonTextSpec
import qualified Agent.LoopSpec as LoopSpec
import qualified Agent.OsPathSpec as OsPathSpec
import qualified Agent.ProjectInstructionsSpec as ProjectInstructionsSpec
import qualified Agent.Provider.OptionsSpec as ProviderOptionsSpec
import qualified Agent.ResourceScopeSpec as ResourceScopeSpec
import qualified Agent.RetrySpec as RetrySpec
import qualified Agent.SkillsSpec as SkillsSpec
import qualified Agent.SubagentsSpec as SubagentsSpec
import qualified Agent.Subagents.TaskPathSpec as TaskPathSpec
import qualified Agent.TextBufferSpec as TextBufferSpec
import qualified Agent.ToolArgsSpec as ToolArgsSpec
import qualified Agent.ToolDispatchSpec as ToolDispatchSpec
import qualified Agent.ToolDSLSpec as ToolDSLSpec
import qualified Agent.Tools.DangerousSpec as DangerousSpec
import qualified Agent.Tools.GhciSpec as GhciSpec
import qualified Agent.Tools.IOSpec as IOSpec
import qualified Agent.Tools.MultiAgentsSpec as MultiAgentsSpec
import qualified Agent.Tools.PlanModeSpec as PlanModeSpec
import qualified Agent.Transport.WebSocketSpec as WebSocketSpec
import Test.Hspec (hspec)

main :: IO ()
main = hspec do
    JWTSpec.spec
    CancelSpec.spec
    DialectSpec.spec
    ErrorSpec.spec
    HttpHeaderSpec.spec
    JsonTextSpec.spec
    LoopSpec.spec
    OsPathSpec.spec
    ProjectInstructionsSpec.spec
    ProviderOptionsSpec.spec
    ResourceScopeSpec.spec
    RetrySpec.spec
    SkillsSpec.spec
    SubagentsSpec.spec
    TaskPathSpec.spec
    TextBufferSpec.spec
    ToolArgsSpec.spec
    ToolDispatchSpec.spec
    ToolDSLSpec.spec
    GhciSpec.spec
    IOSpec.spec
    MultiAgentsSpec.spec
    PlanModeSpec.spec
    DangerousSpec.spec
    WebSocketSpec.spec
