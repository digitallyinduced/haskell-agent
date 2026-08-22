module Main (main) where

import qualified Agent.SyntaxSpec as SyntaxSpec
import Test.Hspec (hspec)

main :: IO ()
main = hspec SyntaxSpec.spec
