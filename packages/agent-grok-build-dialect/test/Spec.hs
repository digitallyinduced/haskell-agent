module Main (main) where

import qualified Agent.GrokBuild.DialectSpec as DialectSpec
import Test.Hspec (hspec)

main :: IO ()
main = hspec DialectSpec.spec
