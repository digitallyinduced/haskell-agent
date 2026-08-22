module Main (main) where

import qualified Agent.TUI.FencedCodeSpec as FencedCodeSpec
import qualified Agent.TUI.MarkdownSpec as MarkdownSpec
import qualified Agent.TUI.MotionSpec as MotionSpec
import qualified Agent.TUI.ModelSpec as ModelSpec
import qualified Agent.TUI.ThemeSpec as ThemeSpec
import Test.Hspec (hspec)

main :: IO ()
main = hspec do
    FencedCodeSpec.spec
    MarkdownSpec.spec
    MotionSpec.spec
    ModelSpec.spec
    ThemeSpec.spec
