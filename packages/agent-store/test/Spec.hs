{-# LANGUAGE BlockArguments #-}

module Main (main) where

import Test.Hspec (hspec)

import qualified Agent.Store.Postgres.ConfigSpec as ConfigSpec
import qualified Agent.Store.Postgres.CustomSpec as CustomSpec
import qualified Agent.Store.Postgres.ManagedSpec as ManagedSpec
import qualified Agent.Store.Postgres.ScopeSpec as ScopeSpec
import qualified Agent.Store.Postgres.SessionSpec as SessionSpec
import qualified Agent.Store.Postgres.SkillSpec as SkillSpec

main :: IO ()
main = hspec do
    ConfigSpec.spec
    ScopeSpec.spec
    CustomSpec.spec
    SessionSpec.spec
    SkillSpec.spec
    ManagedSpec.spec
