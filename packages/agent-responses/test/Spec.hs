module Main (main) where

import Test.Hspec (hspec)

import qualified Agent.Responses.ClientSpec as ClientSpec
import qualified Agent.Responses.GenericClientSpec as GenericClientSpec
import qualified Agent.Responses.LoopBackendSpec as LoopBackendSpec
import qualified Agent.Responses.ResponseMergeSpec as ResponseMergeSpec
import qualified Agent.Responses.SSESpec as SSESpec
import qualified Agent.Responses.StreamAssemblySpec as StreamAssemblySpec

main :: IO ()
main = hspec do
    ClientSpec.spec
    GenericClientSpec.spec
    LoopBackendSpec.spec
    ResponseMergeSpec.spec
    SSESpec.spec
    StreamAssemblySpec.spec
