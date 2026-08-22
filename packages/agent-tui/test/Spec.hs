module Main (main) where

import qualified Agent.TUI.MarkdownSpec as MarkdownSpec
import qualified Agent.TUI.ModelSpec as ModelSpec
import Test.Hspec (hspec)

main :: IO ()
main = hspec do
    MarkdownSpec.spec
    ModelSpec.spec
