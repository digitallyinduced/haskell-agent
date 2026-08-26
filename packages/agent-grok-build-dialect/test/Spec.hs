module Main (main) where

import qualified Agent.GrokBuild.DialectSpec as DialectSpec
import qualified Agent.GrokBuild.RuntimeSpec as RuntimeSpec
import qualified Agent.GrokBuild.SearchReplaceSpec as SearchReplaceSpec
import qualified Agent.GrokBuild.TaskSpec as TaskSpec
import qualified Agent.GrokBuild.WebLspSpec as WebLspSpec
import Test.Hspec (hspec)

main :: IO ()
main = hspec do
    DialectSpec.spec
    RuntimeSpec.spec
    SearchReplaceSpec.spec
    TaskSpec.spec
    WebLspSpec.spec
