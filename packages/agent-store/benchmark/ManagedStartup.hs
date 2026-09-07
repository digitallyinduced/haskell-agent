{-# LANGUAGE NumericUnderscores #-}

module Main (main) where

import Agent.Store.Postgres
    ( ManagedPostgresConfig
    , defaultManagedPostgresConfig
    , openStartupOwnerPool
    )
import Agent.Store.Postgres.Connection
    ( StorePool
    , closeStorePool
    , defaultPoolConfig
    , openStorePool
    , withSession
    )
import Agent.Store.Postgres.Managed
    ( ensureManagedPostgres
    , stopManagedPostgres
    )
import Agent.Store.Types (StoreError, renderStoreError)
import Control.Exception (evaluate)
import Control.Exception.Safe (bracket, finally)
import Control.Monad (forM, unless, void)
import Data.List (sort)
import qualified Data.Text as Text
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats
    ( RTSStats(..)
    , getRTSStats
    , getRTSStatsEnabled
    )
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Session as Session
import Hasql.Statement (Statement)
import qualified Hasql.Statement as Statement
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import System.Exit (die)
import System.IO.Temp (withSystemTempDirectory)
import System.Mem (performGC)
import Text.Printf (printf)

data Strategy
    = LegacyLifecycleCheck
    | WarmConnection
    deriving (Eq, Show)

data Sample = Sample
    { sampleElapsedMillis :: !Double
    , sampleCpuMillis :: !Double
    , sampleAllocatedBytes :: !Integer
    , sampleChecksum :: !Int
    }

main :: IO ()
main = do
    enabled <- getRTSStatsEnabled
    unless enabled $
        die "RTS statistics are disabled; run with +RTS -T"
    sampleCount <- parseArguments =<< getArgs
    withSystemTempDirectory "ham" \stateDirectory -> do
        let config = defaultManagedPostgresConfig stateDirectory ""
        (do
            -- Provision outside the measured warm-start region.
            _ <- requireStore =<< warmOpenAndQuery config
            _ <- measure (warmOpenAndQuery config)
            _ <- measure (legacyOpenAndQuery config)
            pairs <- forM [0 .. sampleCount - 1] \index ->
                if even index
                    then do
                        legacy <- measure (legacyOpenAndQuery config)
                        warm <- measure (warmOpenAndQuery config)
                        pure (legacy, warm)
                    else do
                        warm <- measure (warmOpenAndQuery config)
                        legacy <- measure (legacyOpenAndQuery config)
                        pure (legacy, warm)
            let (legacySamples, warmSamples) = unzip pairs
            printSample sampleCount LegacyLifecycleCheck
                (median legacySamples)
            printSample sampleCount WarmConnection
                (median warmSamples)
            )
            `finally` void (stopManagedPostgres config)

parseArguments :: [String] -> IO Int
parseArguments = \case
    [] -> pure 21
    [raw] ->
        case reads raw of
            [(value, "")]
                | value > 0 -> pure value
            _ -> die ("invalid sample count: " <> raw)
    _ -> die "usage: managed-startup-bench [SAMPLES]"

legacyOpenAndQuery :: ManagedPostgresConfig -> IO (Either StoreError Int)
legacyOpenAndQuery config =
    ensureManagedPostgres config >>= \case
        Left err -> pure (Left err)
        Right _ ->
            openAndQueryWith
                (openStorePool config defaultPoolConfig)

warmOpenAndQuery :: ManagedPostgresConfig -> IO (Either StoreError Int)
warmOpenAndQuery config =
    openAndQueryWith (openStartupOwnerPool config)

openAndQueryWith
    :: IO (Either StoreError StorePool)
    -> IO (Either StoreError Int)
openAndQueryWith acquirePool =
    acquirePool >>= \case
        Left err -> pure (Left err)
        Right pool ->
            bracket
                (pure pool)
                closeStorePool
                query
  where
    query pool =
        withSession
            pool
            (Session.statement () oneStatement)

oneStatement :: Statement () Int
oneStatement = Statement.preparable
    "SELECT 1::int4"
    Encoders.noParams
    (Decoders.singleRow $
        fromIntegral
            <$> Decoders.column (Decoders.nonNullable Decoders.int4))

requireStore :: Either StoreError a -> IO a
requireStore =
    either (die . Text.unpack . renderStoreError) pure

measure :: IO (Either StoreError Int) -> IO Sample
measure action = do
    performGC
    statsBefore <- getRTSStats
    cpuBefore <- getCPUTime
    elapsedBefore <- getMonotonicTimeNSec
    checksum <- requireStore =<< action
    forced <- evaluate checksum
    elapsedAfter <- getMonotonicTimeNSec
    cpuAfter <- getCPUTime
    -- Flush allocation counters after stopping the elapsed/CPU clocks. Small
    -- warm starts usually do not trigger a GC on their own.
    performGC
    statsAfter <- getRTSStats
    pure Sample
        { sampleElapsedMillis =
            fromIntegral (elapsedAfter - elapsedBefore) / 1_000_000
        , sampleCpuMillis =
            fromIntegral (cpuAfter - cpuBefore) / 1_000_000_000
        , sampleAllocatedBytes =
            fromIntegral
                (allocated_bytes statsAfter - allocated_bytes statsBefore)
        , sampleChecksum = forced
        }

median :: [Sample] -> Sample
median samples =
    Sample
        { sampleElapsedMillis =
            medianValue (map (.sampleElapsedMillis) samples)
        , sampleCpuMillis =
            medianValue (map (.sampleCpuMillis) samples)
        , sampleAllocatedBytes =
            medianValue (map (.sampleAllocatedBytes) samples)
        , sampleChecksum =
            medianValue (map (.sampleChecksum) samples)
        }

medianValue :: Ord a => [a] -> a
medianValue values =
    sort values !! (length values `div` 2)

printSample :: Int -> Strategy -> Sample -> IO ()
printSample sampleCount strategy sample =
    printf
        "managed-startup strategy=%s samples=%d elapsed-ms=%.3f \
        \cpu-ms=%.3f allocated-bytes=%d checksum=%d\n"
        (strategyLabel strategy)
        sampleCount
        sample.sampleElapsedMillis
        sample.sampleCpuMillis
        sample.sampleAllocatedBytes
        sample.sampleChecksum

strategyLabel :: Strategy -> String
strategyLabel = \case
    LegacyLifecycleCheck -> "legacy-lifecycle-check"
    WarmConnection -> "warm-connection"
