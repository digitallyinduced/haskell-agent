module Main (main) where

import Agent.ComputerUseSpec qualified as ComputerUseSpec
import Test.Hspec (hspec)

main :: IO ()
main = hspec ComputerUseSpec.spec
