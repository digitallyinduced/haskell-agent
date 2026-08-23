module Main (main) where

import Test.Hspec (hspec)

import qualified Agent.Claude.AuthSpec as AuthSpec
import qualified Agent.Claude.LoopBackendSpec as LoopBackendSpec

main :: IO ()
main = hspec do
    AuthSpec.spec
    LoopBackendSpec.spec
