{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception.Safe (finally)
import Control.Monad (replicateM)
import Data.List (sort)
import qualified Data.Text as Text
import GHC.Clock (getMonotonicTimeNSec)
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import System.IO.Temp (withSystemTempDirectory)

import Agent.Store.Postgres
    ( closeStore
    , defaultManagedPostgresConfig
    , openStore
    , provisioningPool
    , scopePool
    , trustedPool
    )
import Agent.Store.Postgres.Connection (storePool)
import Agent.Store.Postgres.Custom
import Agent.Store.Postgres.Managed (stopManagedPostgres)
import Agent.Store.Postgres.Scope
    ( Scope(..)
    , ScopeDatabase(..)
    , ScopeKind(..)
    , mkScopeId
    , provisionScope
    )

main :: IO ()
main = do
    getArgs >>= \case
        [workload, tableCount, iterations, sampleCount] ->
            benchmark
                (parseWorkload workload)
                (read tableCount)
                (read iterations)
                (read sampleCount)
        _ ->
            fail
                "usage: custom-catalog-inspection-bench \
                \<sequential|pipeline> <table-count> <iterations> <samples>"

data Workload
    = Sequential
    | Pipeline

parseWorkload :: String -> Workload
parseWorkload = \case
    "sequential" -> Sequential
    "pipeline" -> Pipeline
    value -> error ("unknown workload: " <> value)

workloadName :: Workload -> String
workloadName = \case
    Sequential -> "sequential"
    Pipeline -> "pipeline"

benchmark :: Workload -> Int -> Int -> Int -> IO ()
benchmark workload tableCount iterations sampleCount =
    if tableCount < 0 || iterations <= 0 || sampleCount <= 0
        then fail "table count must be non-negative; iterations and samples must be positive"
        else
            withSystemTempDirectory "ha-bench" \stateDirectory -> do
                let config = defaultManagedPostgresConfig stateDirectory ""
                (openStore config
                    >>= either
                        (fail . show)
                        (run workload tableCount iterations sampleCount))
                    `finally` do
                        _ <- stopManagedPostgres config
                        pure ()

run workload tableCount iterations sampleCount store =
    finally
        (do
            scopeId <- either (fail . Text.unpack) pure $
                mkScopeId "0123456789abcdef0123456789abcdef"
            database <- provisionScope
                (storePool (provisioningPool store))
                (Scope RepositoryScope scopeId)
                >>= either (fail . Text.unpack) pure
            scoped <- scopePool store database.scopeDatabaseRole
                >>= either (fail . show) pure
            let pool = storePool scoped
                ddl = Text.intercalate ";"
                    [ "CREATE TABLE bench_" <> Text.pack (show index)
                        <> " (id bigint PRIMARY KEY, value text NOT NULL)"
                    | index <- [1 .. tableCount]
                    ]
            if tableCount > 0
                then do
                    _ <- executeCustom
                        (storePool (trustedPool store))
                        pool
                        database
                        (CustomAuditContext Nothing Nothing)
                        defaultQueryLimits
                        "catalog benchmark setup"
                        ddl
                        >>= either (fail . Text.unpack) pure
                    pure ()
                else pure ()
            samples <- replicateM sampleCount $
                measure $
                    replicateM iterations (inspect pool database)
                        >>= traverse (either (fail . Text.unpack) pure)
            let elapsed = sort [wall | (wall, _, _) <- samples]
                cpu = sort [cpuTime | (_, cpuTime, _) <- samples]
                checksum = sum [value | (_, _, value) <- samples]
            putStrLn $
                "workload=" <> workloadName workload
                    <> " tables=" <> show tableCount
                    <> " iterations=" <> show iterations
                    <> " samples=" <> show sampleCount
                    <> " median-wall-ms=" <> show (median elapsed / 1e6)
                    <> " median-cpu-ms=" <> show (median cpu / 1e9)
                    <> " checksum=" <> show checksum
        )
        (closeStore store)
  where
    inspect = case workload of
        Sequential -> inspectCustomSchemaSequential
        Pipeline -> inspectCustomSchema

    measure action = do
        wallStart <- getMonotonicTimeNSec
        cpuStart <- getCPUTime
        catalogs <- action
        cpuEnd <- getCPUTime
        wallEnd <- getMonotonicTimeNSec
        pure
            ( fromIntegral (wallEnd - wallStart) :: Double
            , fromIntegral (cpuEnd - cpuStart) :: Double
            , sum (map length catalogs)
            )

median :: [Double] -> Double
median values = values !! (length values `div` 2)
