module Main (main) where

import Test.Hspec (hspec)

import qualified Agent.CLI.AuthSpec as AuthSpec
import qualified Agent.CLI.CommandSpec as CommandSpec
import qualified Agent.CLI.MarkdownSpec as MarkdownSpec
import qualified Agent.CLI.OptionsSpec as OptionsSpec
import qualified Agent.CLI.PromptSpec as PromptSpec
import qualified Agent.CLI.RenderSpec as RenderSpec
import qualified Agent.CLI.ToolsSpec as ToolsSpec
import qualified Agent.CLI.WorktreeSpec as WorktreeSpec

main :: IO ()
main = hspec do
    AuthSpec.spec
    CommandSpec.spec
    MarkdownSpec.spec
    OptionsSpec.spec
    PromptSpec.spec
    RenderSpec.spec
    ToolsSpec.spec
    WorktreeSpec.spec
