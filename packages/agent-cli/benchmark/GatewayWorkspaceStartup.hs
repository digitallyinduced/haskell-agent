{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- Compare serial and concurrent startup with actual Git worktree preparation
-- and an authoritative catalog request to the configured organization gateway.
-- No model request is sent. Every sample removes its own checkout and branch.
module Main (main) where

import Agent.CLI.GatewayModels
import Agent.CLI.Worktree
    ( createWorktreeWithFetch, removeWorktree, withPreparedWorktree
    , awaitPreparedWorktree, readyWorktreePath )
import Agent.Runtime.GatewayClient
    ( GatewayModel, loadGatewayCredential, newGatewayModelAccess )
import Control.Exception (evaluate)
import Control.Exception.Safe (bracket)
import Control.Monad (forM, forM_)
import Data.List (sort)
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Text as Text
import GHC.Clock (getMonotonicTimeNSec)
import System.Environment (getArgs, getEnv)
import System.IO (BufferMode(LineBuffering), hSetBuffering, stdout)
import System.OsPath (unsafeEncodeUtf, decodeFS)
import System.Posix.Temp (mkdtemp)
import System.Directory (removePathForcibly)
import System.Process (readProcess)
import Text.Read (readMaybe)

data StartupStrategy = SerialStartup | PreparedCatalog | PreparedWorkspace
    deriving (Eq, Show)

data StartupSample = StartupSample
    { elapsedMilliseconds :: !Double
    , localCheckoutMilliseconds :: !Double
    , synchronizedCheckoutMilliseconds :: !Double
    , catalogModels :: ![GatewayModel]
    , checkoutCommit :: !Text.Text
    }

strategyName :: StartupStrategy -> String
strategyName SerialStartup = "serial"
strategyName PreparedCatalog = "prepared-catalog"
strategyName PreparedWorkspace = "prepared-workspace"

median :: [Double] -> Double
median samples =
    let values = sort samples
        middle = length values `div` 2
    in if odd (length values)
        then values !! middle
        else (values !! (middle - 1) + values !! middle) / 2

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    arguments <- getArgs
    (repository, fetchLatest, sampleCount) <- case arguments of
        [path, mode, count]
            | mode `elem` ["local", "remote"]
            , Just samples <- (readMaybe count :: Maybe Int)
            , samples > 0 -> pure (unsafeEncodeUtf path, mode == "remote", samples)
        _ -> fail "usage: gateway-workspace-startup REPOSITORY local|remote SAMPLES"
    credential <- loadGatewayCredential >>= either (fail . Text.unpack)
        (maybe (fail "connect an organization gateway first") pure)
    temporaryRoot <- getEnv "TMPDIR"
    bracket
        (mkdtemp (temporaryRoot <> "/gateway-workspace-startup-"))
        removePathForcibly \temporaryDirectory -> do
            let create = createWorktreeWithFetch fetchLatest repository
                    (unsafeEncodeUtf temporaryDirectory)
                    >>= either (fail . Text.unpack) pure
                remove path = removeWorktree repository path
                    >>= either (fail . Text.unpack) pure
                consume result = either (fail . Text.unpack)
                    (\models -> evaluate (length models) >> pure models) result
                strategies = [SerialStartup, PreparedCatalog, PreparedWorkspace]
                run strategy = do
                    started <- getMonotonicTimeNSec
                    localReady <- newIORef 0
                    synchronized <- newIORef 0
                    let elapsed = do
                            now <- getMonotonicTimeNSec
                            pure (fromIntegral (now - started) / 1000000)
                        observeLocal = elapsed >>= writeIORef localReady
                        useReady path awaitModels = do
                            elapsed >>= writeIORef synchronized
                            filename <- decodeFS path
                            commit <- Text.strip . Text.pack
                                <$> readProcess "git" ["-C", filename, "rev-parse", "HEAD"] ""
                            models <- awaitModels >>= consume
                            pure (models, commit)
                        createLocal = do
                            path <- create
                            observeLocal
                            pure path
                    access <- newGatewayModelAccess credential
                    (models, commit) <- case strategy of
                        SerialStartup -> bracket createLocal remove \path ->
                            useReady path (withGatewayModelsForStartup access Right pure)
                        PreparedCatalog -> withPreparedGatewayModelsForStartup access \awaitModels ->
                            bracket createLocal remove \path -> useReady path awaitModels
                        PreparedWorkspace | not fetchLatest ->
                            withPreparedGatewayModelsForStartup access \awaitModels ->
                                bracket createLocal remove \path -> useReady path awaitModels
                        PreparedWorkspace -> withPreparedGatewayModelsForStartup access \awaitModels ->
                            withPreparedWorktree (const (pure ())) fetchLatest repository
                                (unsafeEncodeUtf temporaryDirectory) $
                                either (fail . Text.unpack) \pending -> do
                                    observeLocal
                                    bracket
                                        (awaitPreparedWorktree pending >>= either (fail . Text.unpack) pure)
                                        (remove . readyWorktreePath)
                                        (\ready -> useReady (readyWorktreePath ready) awaitModels)
                    total <- elapsed
                    local <- readIORef localReady
                    final <- readIORef synchronized
                    pure (StartupSample total local final models commit)
            rows <- forM [0 .. sampleCount - 1] \index -> do
                let offset = index `mod` length strategies
                    order = drop offset strategies <> take offset strategies
                results <- forM order \strategy -> do
                    sample <- run strategy
                    putStrLn (show (index, strategyName strategy,
                        sample.elapsedMilliseconds, sample.localCheckoutMilliseconds,
                        sample.synchronizedCheckoutMilliseconds, length sample.catalogModels))
                    pure (strategy, sample)
                case results of
                    (_, first) : remaining
                        | all (\(_, sample) ->
                            sample.catalogModels == first.catalogModels
                                && sample.checkoutCommit == first.checkoutCommit) remaining ->
                            pure results
                    _ -> fail "gateway catalog or upstream commit changed during paired sample"
            forM_ strategies \strategy -> do
                let samples = [sample | row <- rows, (mode, sample) <- row, mode == strategy]
                putStrLn (strategyName strategy
                    <> " median_ms=" <> show (median (map (.elapsedMilliseconds) samples))
                    <> " local_checkout_ms=" <> show (median (map (.localCheckoutMilliseconds) samples))
                    <> " synchronized_checkout_ms=" <> show (median (map (.synchronizedCheckoutMilliseconds) samples)))
