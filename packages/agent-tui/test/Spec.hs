module Main (main) where

import qualified Agent.TUI.AccentSpec as AccentSpec
import qualified Agent.TUI.FencedCodeSpec as FencedCodeSpec
import qualified Agent.TUI.Markdown.BlockSpec as MarkdownBlockSpec
import qualified Agent.TUI.Markdown.InlineSpec as MarkdownInlineSpec
import qualified Agent.TUI.MarkdownSpec as MarkdownSpec
import qualified Agent.TUI.MotionSpec as MotionSpec
import qualified Agent.TUI.ModelSpec as ModelSpec
import qualified Agent.TUI.ModelPropertySpec as ModelPropertySpec
import qualified Agent.TUI.PlanReviewSpec as PlanReviewSpec
import qualified Agent.TUI.PresentationSpec as PresentationSpec
import qualified Agent.TUI.QuestionnaireSpec as QuestionnaireSpec
import qualified Agent.TUI.ThemeSpec as ThemeSpec
import qualified Agent.TUI.TextWidthSpec as TextWidthSpec
import Test.Hspec (hspec)

main :: IO ()
main = hspec do
    AccentSpec.spec
    FencedCodeSpec.spec
    MarkdownBlockSpec.spec
    MarkdownInlineSpec.spec
    MarkdownSpec.spec
    MotionSpec.spec
    ModelSpec.spec
    ModelPropertySpec.spec
    PlanReviewSpec.spec
    PresentationSpec.spec
    QuestionnaireSpec.spec
    ThemeSpec.spec
    TextWidthSpec.spec
