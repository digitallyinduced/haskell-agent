module Main (main) where

import Agent.ComputerUseSpec qualified as ComputerUseSpec
import Agent.ComputerUse.SemanticSpec qualified as SemanticSpec
import Agent.ComputerUse.TransportSpec qualified as TransportSpec
import Test.Hspec (hspec)

main :: IO ()
main = hspec do
    ComputerUseSpec.spec
    SemanticSpec.spec
    TransportSpec.spec
