module Main (main) where

import Test.Hspec (hspec)

import qualified Agent.XAI.AuthSpec as AuthSpec
import qualified Agent.XAI.ClientSpec as ClientSpec
import qualified Agent.XAI.FunctionalSpec as FunctionalSpec
import qualified Agent.XAI.LoopBackendSpec as LoopBackendSpec
import qualified Agent.XAI.ResponsesSpec as ResponsesSpec
import qualified Agent.XAI.UsageSpec as UsageSpec

main :: IO ()
main = hspec do
    AuthSpec.spec
    ClientSpec.spec
    FunctionalSpec.spec
    LoopBackendSpec.spec
    ResponsesSpec.spec
    UsageSpec.spec
