module Main (main) where

import Test.Hspec (hspec)

import qualified Agent.Claude.AuthSpec as AuthSpec
import qualified Agent.Claude.LoopBackendSpec as LoopBackendSpec
import qualified Agent.Claude.OptionsSpec as OptionsSpec

main :: IO ()
main = hspec do
    AuthSpec.spec
    OptionsSpec.spec
    LoopBackendSpec.spec
