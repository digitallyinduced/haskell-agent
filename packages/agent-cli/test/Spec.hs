module Main (main) where

import Test.Hspec (hspec)

import qualified Agent.CLI.AuthSpec as AuthSpec
import qualified Agent.CLI.CancelWatchSpec as CancelWatchSpec
import qualified Agent.CLI.ClipboardSpec as ClipboardSpec
import qualified Agent.CLI.CommandSpec as CommandSpec
import qualified Agent.CLI.ImagePreviewSpec as ImagePreviewSpec
import qualified Agent.CLI.InputSpec as InputSpec
import qualified Agent.CLI.InterruptSpec as InterruptSpec
import qualified Agent.CLI.MarkdownSpec as MarkdownSpec
import qualified Agent.CLI.ModelPickerSpec as ModelPickerSpec
import qualified Agent.CLI.ModelsSpec as ModelsSpec
import qualified Agent.CLI.OptionsSpec as OptionsSpec
import qualified Agent.CLI.PermissionSpec as PermissionSpec
import qualified Agent.CLI.PlanSpec as PlanSpec
import qualified Agent.CLI.ProjectSpec as ProjectSpec
import qualified Agent.CLI.PromptSpec as PromptSpec
import qualified Agent.CLI.RenderSpec as RenderSpec
import qualified Agent.CLI.ReplStatusSpec as ReplStatusSpec
import qualified Agent.CLI.ResumeSpec as ResumeSpec
import qualified Agent.CLI.SessionSpec as SessionSpec
import qualified Agent.CLI.SubagentStoreSpec as SubagentStoreSpec
import qualified Agent.CLI.StyleSpec as StyleSpec
import qualified Agent.CLI.TimestampSpec as TimestampSpec
import qualified Agent.CLI.ToolsSpec as ToolsSpec
import qualified Agent.CLI.WorktreeSpec as WorktreeSpec

main :: IO ()
main = hspec do
    AuthSpec.spec
    CancelWatchSpec.spec
    ClipboardSpec.spec
    CommandSpec.spec
    ImagePreviewSpec.spec
    InputSpec.spec
    InterruptSpec.spec
    MarkdownSpec.spec
    ModelPickerSpec.spec
    ModelsSpec.spec
    OptionsSpec.spec
    PermissionSpec.spec
    PlanSpec.spec
    ProjectSpec.spec
    PromptSpec.spec
    RenderSpec.spec
    ReplStatusSpec.spec
    ResumeSpec.spec
    StyleSpec.spec
    TimestampSpec.spec
    SessionSpec.spec
    SubagentStoreSpec.spec
    ToolsSpec.spec
    WorktreeSpec.spec
