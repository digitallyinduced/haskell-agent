module Main (main) where

import qualified Agent.Codex.Dialect.MultiAgentsLoopSpec as MultiAgentsLoopSpec
import qualified Agent.Codex.Dialect.MultiAgentsSpec as MultiAgentsSpec
import qualified Agent.Codex.DialectSpec as DialectSpec
import Test.Hspec (hspec)

main :: IO ()
main = hspec do
    DialectSpec.spec
    MultiAgentsSpec.spec
    MultiAgentsLoopSpec.spec
