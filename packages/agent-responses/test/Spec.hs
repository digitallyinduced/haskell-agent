module Main (main) where

import Test.Hspec (hspec)

import qualified Agent.Responses.LoopBackendSpec as LoopBackendSpec
import qualified Agent.Responses.ResponseMergeSpec as ResponseMergeSpec
import qualified Agent.Responses.SSESpec as SSESpec

main :: IO ()
main = hspec do
    LoopBackendSpec.spec
    ResponseMergeSpec.spec
    SSESpec.spec
