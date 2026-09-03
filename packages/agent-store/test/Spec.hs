{-# LANGUAGE BlockArguments #-}

module Main (main) where

import Test.Hspec (hspec)

import qualified Agent.Store.Postgres.ConfigSpec as ConfigSpec
import qualified Agent.Store.Postgres.CustomSpec as CustomSpec
import qualified Agent.Store.Postgres.ManagedSpec as ManagedSpec
import qualified Agent.Store.PoolCacheSpec as PoolCacheSpec
import qualified Agent.Store.Postgres.ScopeSpec as ScopeSpec
import qualified Agent.Store.Postgres.ServerTurnSpec as ServerTurnSpec
import qualified Agent.Store.Postgres.SessionSpec as SessionSpec
import qualified Agent.Store.Postgres.SkillSpec as SkillSpec
import qualified Agent.Store.Postgres.TenantSpec as TenantSpec

main :: IO ()
main = hspec do
    PoolCacheSpec.spec
    ConfigSpec.spec
    ScopeSpec.spec
    CustomSpec.spec
    ServerTurnSpec.spec
    SessionSpec.spec
    SkillSpec.spec
    TenantSpec.spec
    ManagedSpec.spec
