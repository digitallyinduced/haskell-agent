module Main (main) where

import qualified Agent.TelegramSpec as TelegramSpec
import Test.Hspec (hspec)

main :: IO ()
main = hspec TelegramSpec.spec
