module Main (main) where

import Test.Hspec (hspec)

import qualified Agent.ClaudeCode.AuthSpec as AuthSpec
import qualified Agent.ClaudeCode.LoopBackendSpec as LoopBackendSpec
import qualified Agent.ClaudeCode.TranscriptSpec as TranscriptSpec

main :: IO ()
main = hspec do
    AuthSpec.spec
    TranscriptSpec.spec
    LoopBackendSpec.spec
