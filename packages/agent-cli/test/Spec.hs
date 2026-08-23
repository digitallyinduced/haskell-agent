module Main (main) where

import Test.Hspec (hspec)

import qualified Agent.CLI.AgentSessionsSpec as AgentSessionsSpec
import qualified Agent.CLI.AgentViewportSpec as AgentViewportSpec
import qualified Agent.CLI.ApprovalSpec as ApprovalSpec
import qualified Agent.CLI.ArtifactSpec as ArtifactSpec
import qualified Agent.CLI.AuthSpec as AuthSpec
import qualified Agent.CLI.BtwSpec as BtwSpec
import qualified Agent.CLI.CancelWatchSpec as CancelWatchSpec
import qualified Agent.CLI.ClipboardSpec as ClipboardSpec
import qualified Agent.CLI.CommandSpec as CommandSpec
import qualified Agent.CLI.ConfigSpec as ConfigSpec
import qualified Agent.CLI.CompactionSpec as CompactionSpec
import qualified Agent.CLI.ConnectivitySpec as ConnectivitySpec
import qualified Agent.CLI.CredentialStoreSpec as CredentialStoreSpec
import qualified Agent.CLI.DialectsSpec as DialectsSpec
import qualified Agent.CLI.ErrorSpec as ErrorSpec
import qualified Agent.CLI.ImagePreviewSpec as ImagePreviewSpec
import qualified Agent.CLI.InputSpec as InputSpec
import qualified Agent.CLI.InterruptSpec as InterruptSpec
import qualified Agent.CLI.LoginSpec as LoginSpec
import qualified Agent.CLI.MarkdownSpec as MarkdownSpec
import qualified Agent.CLI.ModelPickerSpec as ModelPickerSpec
import qualified Agent.CLI.ModelsSpec as ModelsSpec
import qualified Agent.CLI.NotificationSpec as NotificationSpec
import qualified Agent.CLI.OptionsSpec as OptionsSpec
import qualified Agent.CLI.PendingInputsSpec as PendingInputsSpec
import qualified Agent.CLI.PermissionSpec as PermissionSpec
import qualified Agent.CLI.PickerSpec as PickerSpec
import qualified Agent.CLI.PlanSpec as PlanSpec
import qualified Agent.CLI.ProgressSpec as ProgressSpec
import qualified Agent.CLI.ProjectSpec as ProjectSpec
import qualified Agent.CLI.PromptSpec as PromptSpec
import qualified Agent.CLI.ProviderFallbackSpec as ProviderFallbackSpec
import qualified Agent.CLI.ProviderAvailabilitySpec as ProviderAvailabilitySpec
import qualified Agent.CLI.ProviderTransitionSpec as ProviderTransitionSpec
import qualified Agent.CLI.RequestSpec as RequestSpec
import qualified Agent.CLI.RenderSpec as RenderSpec
import qualified Agent.CLI.ReplStatusSpec as ReplStatusSpec
import qualified Agent.CLI.ResumeSpec as ResumeSpec
import qualified Agent.CLI.SecretSpec as SecretSpec
import qualified Agent.CLI.SessionSpec as SessionSpec
import qualified Agent.CLI.SessionTitleSpec as SessionTitleSpec
import qualified Agent.CLI.SkillsSpec as SkillsSpec
import qualified Agent.CLI.SubagentStoreSpec as SubagentStoreSpec
import qualified Agent.CLI.StyleSpec as StyleSpec
import qualified Agent.CLI.TimestampSpec as TimestampSpec
import qualified Agent.CLI.TurnSpec as TurnSpec
import qualified Agent.CLI.TerminalSpec as TerminalSpec
import qualified Agent.CLI.TextLayoutSpec as TextLayoutSpec
import qualified Agent.CLI.ToolsSpec as ToolsSpec
import qualified Agent.CLI.TUIAppSpec as TUIAppSpec
import qualified Agent.CLI.TUIBridgeSpec as TUIBridgeSpec
import qualified Agent.CLI.TUIComposerSpec as TUIComposerSpec
import qualified Agent.CLI.TUIImagePreviewSpec as TUIImagePreviewSpec
import qualified Agent.CLI.TUIScrollSpec as TUIScrollSpec
import qualified Agent.CLI.UsageSpec as UsageSpec
import qualified Agent.CLI.WorktreeSpec as WorktreeSpec

main :: IO ()
main = hspec do
    AgentViewportSpec.spec
    AgentSessionsSpec.spec
    ApprovalSpec.spec
    ArtifactSpec.spec
    AuthSpec.spec
    BtwSpec.spec
    CancelWatchSpec.spec
    ClipboardSpec.spec
    CommandSpec.spec
    ConfigSpec.spec
    CompactionSpec.spec
    ConnectivitySpec.spec
    CredentialStoreSpec.spec
    DialectsSpec.spec
    ErrorSpec.spec
    ImagePreviewSpec.spec
    InputSpec.spec
    InterruptSpec.spec
    LoginSpec.spec
    MarkdownSpec.spec
    ModelPickerSpec.spec
    ModelsSpec.spec
    NotificationSpec.spec
    OptionsSpec.spec
    PendingInputsSpec.spec
    PermissionSpec.spec
    PickerSpec.spec
    PlanSpec.spec
    ProgressSpec.spec
    ProjectSpec.spec
    PromptSpec.spec
    ProviderFallbackSpec.spec
    ProviderAvailabilitySpec.spec
    ProviderTransitionSpec.spec
    RequestSpec.spec
    RenderSpec.spec
    ReplStatusSpec.spec
    ResumeSpec.spec
    SecretSpec.spec
    StyleSpec.spec
    TimestampSpec.spec
    TurnSpec.spec
    TerminalSpec.spec
    TextLayoutSpec.spec
    SessionSpec.spec
    SessionTitleSpec.spec
    SkillsSpec.spec
    SubagentStoreSpec.spec
    ToolsSpec.spec
    TUIAppSpec.spec
    TUIBridgeSpec.spec
    TUIComposerSpec.spec
    TUIImagePreviewSpec.spec
    TUIScrollSpec.spec
    UsageSpec.spec
    WorktreeSpec.spec
