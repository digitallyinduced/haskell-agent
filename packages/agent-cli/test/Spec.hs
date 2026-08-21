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
import qualified Agent.CLI.CompactionSpec as CompactionSpec
import qualified Agent.CLI.CredentialStoreSpec as CredentialStoreSpec
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
import qualified Agent.CLI.ProviderTransitionSpec as ProviderTransitionSpec
import qualified Agent.CLI.RenderSpec as RenderSpec
import qualified Agent.CLI.ReplStatusSpec as ReplStatusSpec
import qualified Agent.CLI.ResumeSpec as ResumeSpec
import qualified Agent.CLI.SessionSpec as SessionSpec
import qualified Agent.CLI.SessionTitleSpec as SessionTitleSpec
import qualified Agent.CLI.SkillsSpec as SkillsSpec
import qualified Agent.CLI.SubagentStoreSpec as SubagentStoreSpec
import qualified Agent.CLI.StyleSpec as StyleSpec
import qualified Agent.CLI.TimestampSpec as TimestampSpec
import qualified Agent.CLI.TerminalSpec as TerminalSpec
import qualified Agent.CLI.ToolsSpec as ToolsSpec
import qualified Agent.CLI.UIModelSpec as UIModelSpec
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
    CompactionSpec.spec
    CredentialStoreSpec.spec
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
    ProviderTransitionSpec.spec
    RenderSpec.spec
    ReplStatusSpec.spec
    ResumeSpec.spec
    StyleSpec.spec
    TimestampSpec.spec
    TerminalSpec.spec
    SessionSpec.spec
    SessionTitleSpec.spec
    SkillsSpec.spec
    SubagentStoreSpec.spec
    ToolsSpec.spec
    UIModelSpec.spec
    UsageSpec.spec
    WorktreeSpec.spec
