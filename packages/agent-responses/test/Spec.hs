module Main (main) where

import Test.Hspec (hspec)

import qualified Agent.Responses.LoopBackendSpec as LoopBackendSpec
import qualified Agent.Responses.ResponseMergeSpec as ResponseMergeSpec

main :: IO ()
main = hspec do
    LoopBackendSpec.spec
    ResponseMergeSpec.spec
