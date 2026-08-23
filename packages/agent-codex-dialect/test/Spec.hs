module Main (main) where

import qualified Agent.Codex.DialectSpec as DialectSpec
import Test.Hspec (hspec)

main :: IO ()
main = hspec DialectSpec.spec
