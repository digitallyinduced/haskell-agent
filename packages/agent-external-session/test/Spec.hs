module Main (main) where

import Test.Hspec (hspec)

import qualified Agent.CLI.ExternalSessionSpec as ExternalSessionSpec

main :: IO ()
main = hspec ExternalSessionSpec.spec
