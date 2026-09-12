module Main (main) where

import qualified Agent.Auth.JWTSpec as JWTSpec
import qualified Agent.ClientIdentitySpec as ClientIdentitySpec
import qualified Agent.CancelSpec as CancelSpec
import qualified Agent.ConcurrentSpec as ConcurrentSpec
import qualified Agent.ComputerUse.ProtocolSpec as ComputerUseProtocolSpec
import qualified Agent.DialectSpec as DialectSpec
import qualified Agent.ErrorSpec as ErrorSpec
import qualified Agent.Http.HeaderSpec as HttpHeaderSpec
import qualified Agent.JsonTextSpec as JsonTextSpec
import qualified Agent.LoopSpec as LoopSpec
import qualified Agent.OsPathSpec as OsPathSpec
import qualified Agent.ProjectInstructionsSpec as ProjectInstructionsSpec
import qualified Agent.Provider.OptionsSpec as ProviderOptionsSpec
import qualified Agent.ProviderSpec as ProviderSpec
import qualified Agent.ResourceScopeSpec as ResourceScopeSpec
import qualified Agent.RetrySpec as RetrySpec
import qualified Agent.SkillsSpec as SkillsSpec
import qualified Agent.SubagentsSpec as SubagentsSpec
import qualified Agent.Subagents.TaskPathSpec as TaskPathSpec
import qualified Agent.TextBufferSpec as TextBufferSpec
import qualified Agent.TelemetrySpec as TelemetrySpec
import qualified Agent.ToolArgsSpec as ToolArgsSpec
import qualified Agent.ToolDSLSpec as ToolDSLSpec
import qualified Agent.Tools.ResourceArbiterSpec as ResourceArbiterSpec
import qualified Agent.Transport.WebSocketSpec as WebSocketSpec
import qualified Agent.Transport.SSESpec as SSESpec
import Test.Hspec (hspec)

main :: IO ()
main = hspec do
    SSESpec.spec
    JWTSpec.spec
    ClientIdentitySpec.spec
    CancelSpec.spec
    ConcurrentSpec.spec
    ComputerUseProtocolSpec.spec
    DialectSpec.spec
    ErrorSpec.spec
    HttpHeaderSpec.spec
    JsonTextSpec.spec
    LoopSpec.spec
    OsPathSpec.spec
    ProjectInstructionsSpec.spec
    ProviderOptionsSpec.spec
    ProviderSpec.spec
    ResourceScopeSpec.spec
    RetrySpec.spec
    SkillsSpec.spec
    SubagentsSpec.spec
    TaskPathSpec.spec
    TextBufferSpec.spec
    TelemetrySpec.spec
    ToolArgsSpec.spec
    ToolDSLSpec.spec
    ResourceArbiterSpec.spec
    WebSocketSpec.spec
