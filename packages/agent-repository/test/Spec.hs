module Main (main) where

import Test.Hspec (hspec)

import qualified Agent.CLI.RepositoryDeliverySpec as RepositoryDeliverySpec
import qualified Agent.CLI.RepositoryReviewSpec as RepositoryReviewSpec

main :: IO ()
main = hspec do
    RepositoryDeliverySpec.spec
    RepositoryReviewSpec.spec
