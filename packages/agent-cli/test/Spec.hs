module Main (main) where

import Test.Hspec (hspec)

import qualified Agent.CLI.AuthSpec as AuthSpec
import qualified Agent.CLI.ClipboardSpec as ClipboardSpec
import qualified Agent.CLI.CommandSpec as CommandSpec
import qualified Agent.CLI.InputSpec as InputSpec
import qualified Agent.CLI.MarkdownSpec as MarkdownSpec
import qualified Agent.CLI.OptionsSpec as OptionsSpec
import qualified Agent.CLI.ProjectSpec as ProjectSpec
import qualified Agent.CLI.PromptSpec as PromptSpec
import qualified Agent.CLI.RenderSpec as RenderSpec
import qualified Agent.CLI.SessionSpec as SessionSpec
import qualified Agent.CLI.StyleSpec as StyleSpec
import qualified Agent.CLI.ToolsSpec as ToolsSpec
import qualified Agent.CLI.WorktreeSpec as WorktreeSpec

main :: IO ()
main = hspec do
    AuthSpec.spec
    ClipboardSpec.spec
    CommandSpec.spec
    InputSpec.spec
    MarkdownSpec.spec
    OptionsSpec.spec
    ProjectSpec.spec
    PromptSpec.spec
    RenderSpec.spec
    StyleSpec.spec
    SessionSpec.spec
    ToolsSpec.spec
    WorktreeSpec.spec
