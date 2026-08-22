module Main (main) where

import Test.Hspec (hspec)

import qualified Agent.ClaudeCode.AuthSpec as AuthSpec
import qualified Agent.ClaudeCode.LoopBackendSpec as LoopBackendSpec
import qualified Agent.ClaudeCode.StreamSpec as StreamSpec

main :: IO ()
main = hspec do
    AuthSpec.spec
    StreamSpec.spec
    LoopBackendSpec.spec
