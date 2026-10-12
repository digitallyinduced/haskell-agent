module Main (main) where

import qualified Agent.Auth.JWTSpec as JWTSpec
import qualified Agent.ClientIdentitySpec as ClientIdentitySpec
import qualified Agent.CancelSpec as CancelSpec
import qualified Agent.ConcurrentSpec as ConcurrentSpec
import qualified Agent.ComputerUse.ProtocolSpec as ComputerUseProtocolSpec
import qualified Agent.DialectSpec as DialectSpec
import qualified Agent.ErrorSpec as ErrorSpec
import qualified Agent.HistorySpec as HistorySpec
import qualified Agent.Loop.InputInboxSpec as InputInboxSpec
import qualified Agent.Loop.RecoverySpec as RecoverySpec
import qualified Agent.Loop.NativeSpec as NativeSpec
import qualified Agent.Tools.RegistrationSpec as RegistrationSpec
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
import qualified Agent.Skills.ToolSearchSpec as SkillsToolSearchSpec
import qualified Agent.SubagentsSpec as SubagentsSpec
import qualified Agent.Subagents.ChildSpec as SubagentChildSpec
import qualified Agent.Subagents.SuspensionSpec as SubagentSuspensionSpec
import qualified Agent.Subagents.RuntimeSpec as SubagentRuntimeSpec
import qualified Agent.Subagents.NotificationsSpec as SubagentNotificationsSpec
import qualified Agent.Subagents.RecoverySpec as SubagentRecoverySpec
import qualified Agent.DurableTranscriptSpec as DurableTranscriptSpec
import qualified Agent.Subagents.HistorySpec as SubagentHistorySpec
import qualified Agent.Subagents.LoopSpec as SubagentLoopSpec
import qualified Agent.Subagents.TaskPathSpec as TaskPathSpec
import qualified Agent.TextBufferSpec as TextBufferSpec
import qualified Agent.Transcription.ReceiveSpec as TranscriptionReceiveSpec
import qualified Agent.TelemetrySpec as TelemetrySpec
import qualified Agent.ToolArgsSpec as ToolArgsSpec
import qualified Agent.ToolDSLSpec as ToolDSLSpec
import qualified Agent.Tools.ResourceArbiterSpec as ResourceArbiterSpec
import qualified Agent.Transport.WebSocketSpec as WebSocketSpec
import qualified Agent.Transport.SSESpec as SSESpec
import qualified Agent.UuidSpec as UuidSpec
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
    HistorySpec.spec
    InputInboxSpec.spec
    RecoverySpec.spec
    NativeSpec.spec
    RegistrationSpec.spec
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
    SkillsToolSearchSpec.spec
    SubagentsSpec.spec
    SubagentChildSpec.spec
    SubagentSuspensionSpec.spec
    SubagentRuntimeSpec.spec
    SubagentNotificationsSpec.spec
    SubagentRecoverySpec.spec
    DurableTranscriptSpec.spec
    SubagentHistorySpec.spec
    SubagentLoopSpec.spec
    TaskPathSpec.spec
    TextBufferSpec.spec
    TranscriptionReceiveSpec.spec
    TelemetrySpec.spec
    ToolArgsSpec.spec
    ToolDSLSpec.spec
    ResourceArbiterSpec.spec
    UuidSpec.spec
    WebSocketSpec.spec
