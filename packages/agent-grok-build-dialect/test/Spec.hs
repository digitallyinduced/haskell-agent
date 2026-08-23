module Main (main) where

import qualified Agent.GrokBuild.DialectSpec as DialectSpec
import qualified Agent.GrokBuild.TaskSpec as TaskSpec
import Test.Hspec (hspec)

main :: IO ()
main = hspec do
    DialectSpec.spec
    TaskSpec.spec
