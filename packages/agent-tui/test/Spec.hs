module Main (main) where

import qualified Agent.TUI.FencedCodeSpec as FencedCodeSpec
import qualified Agent.TUI.Markdown.InlineSpec as MarkdownInlineSpec
import qualified Agent.TUI.MarkdownSpec as MarkdownSpec
import qualified Agent.TUI.MotionSpec as MotionSpec
import qualified Agent.TUI.ModelSpec as ModelSpec
import qualified Agent.TUI.PresentationSpec as PresentationSpec
import qualified Agent.TUI.ThemeSpec as ThemeSpec
import qualified Agent.TUI.TextWidthSpec as TextWidthSpec
import Test.Hspec (hspec)

main :: IO ()
main = hspec do
    FencedCodeSpec.spec
    MarkdownInlineSpec.spec
    MarkdownSpec.spec
    MotionSpec.spec
    ModelSpec.spec
    PresentationSpec.spec
    ThemeSpec.spec
    TextWidthSpec.spec
