module Main (main) where

import Agent.Accounts.CredentialStoreSpec qualified as CredentialStore
import Agent.Accounts.SelectionSpec qualified as Selection
import Test.Hspec (hspec)

main :: IO ()
main = hspec do
    CredentialStore.spec
    Selection.spec
