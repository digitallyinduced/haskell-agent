module Main (main) where

import Test.Hspec (hspec)

import qualified Agent.XAI.ContextTrimSpec as ContextTrimSpec
import qualified Agent.XAI.GrokFunctionalSpec as GrokFunctionalSpec
import qualified Agent.XAI.GrokLoginSpec as GrokLoginSpec
import qualified Agent.XAI.GrokSpec as GrokSpec
import qualified Agent.XAI.GrokTransportSpec as GrokTransportSpec

main :: IO ()
main = hspec do
    ContextTrimSpec.spec
    GrokFunctionalSpec.spec
    GrokLoginSpec.spec
    GrokSpec.spec
    GrokTransportSpec.spec
