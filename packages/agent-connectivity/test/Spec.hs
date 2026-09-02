module Main (main) where

import qualified Agent.ConnectivitySpec as ConnectivitySpec
import Test.Hspec (hspec)

main :: IO ()
main = hspec ConnectivitySpec.spec
