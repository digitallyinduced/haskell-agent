module Main (main) where

import Test.Hspec (hspec)

import qualified Agent.Responses.ResponseMergeSpec as ResponseMergeSpec

main :: IO ()
main = hspec ResponseMergeSpec.spec
