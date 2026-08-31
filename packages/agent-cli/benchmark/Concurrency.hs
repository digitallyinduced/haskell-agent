module Main (main) where

import Agent.Concurrent (mapConcurrentlyBounded)
import Agent.CLI.PendingInputs
    ( PendingInputs
    , enqueuePendingInput
    , newPendingInputs
    , withPendingInputs
    )
import Agent.Loop
    ( Backend(..)
    , BackendResult(..)
    , TurnInput(..)
    , emptyBackendSnapshot
    , emptyTurnOutput
    )
import Agent.Error (ApiError(..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (poll, withAsync)
import Control.Exception.Safe (SomeException, bracket, onException)
import qualified Control.Exception.Safe as Exception
import Control.Monad (forM_, when)
import qualified Data.ByteString as BS
import Data.List (sort)
import Data.IORef
    ( IORef
    , atomicModifyIORef'
    , modifyIORef'
    , newIORef
    , readIORef
    )
import qualified Data.Text as Text
import Data.Char (ord)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats
    ( RTSStats(..)
    , getRTSStats
    , getRTSStatsEnabled
    )
import System.CPUTime (getCPUTime)
import System.Directory
    ( createDirectoryIfMissing
    , getTemporaryDirectory
    , removePathForcibly
    )
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.Mem (performMajorGC)

data Sample = Sample
    { sampleElapsedMillis :: !Double
    , sampleCpuMillis :: !Double
    , sampleAllocatedBytes :: !Word
    }

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["pending-legacy", count, samples] ->
            benchmarkPendingInputs False (read count) (read samples)
        ["pending-seq", count, samples] ->
            benchmarkPendingInputs True (read count) (read samples)
        ["startup-accounts-serial", worktreeMillis, accountMillis, samples] ->
            benchmarkStartupOverlap
                False
                (read worktreeMillis)
                (read accountMillis)
                (read samples)
        ["startup-accounts-overlap", worktreeMillis, accountMillis, samples] ->
            benchmarkStartupOverlap
                True
                (read worktreeMillis)
                (read accountMillis)
                (read samples)
        _ -> benchmarkConcurrency args

benchmarkConcurrency :: [String] -> IO ()
benchmarkConcurrency args = do
    statsEnabled <- getRTSStatsEnabled
    when (not statsEnabled) $
        fail "run with +RTS -T"
    let (count, bytesPerFile, samples) = parseArgs args
    withInputFiles count bytesPerFile \paths -> do
        putStrLn
            ("count=" <> show count
                <> " bytes-per-file=" <> show bytesPerFile
                <> " samples=" <> show samples)
        benchmark "startup-serial" samples
            (serialDelayed count)
        benchmark "startup-bounded-8" samples
            (boundedDelayed count)
        benchmark "attachments-serial" samples
            (serialRead paths)
        benchmark "attachments-bounded-4" samples
            (boundedRead paths)

benchmarkPendingInputs :: Bool -> Int -> Int -> IO ()
benchmarkPendingInputs useSequence count sampleCount = do
    statsEnabled <- getRTSStatsEnabled
    when (not statsEnabled) $
        fail "run with +RTS -T"
    let inputs =
            [ UserMessage (Text.pack ("pending-" <> show index))
            | index <- [1 .. max 1 count]
            ]
    forM_ [False, True] \failFirst -> do
        legacyChecksum <- pendingLegacy inputs failFirst
        sequenceChecksum <- pendingSequence inputs failFirst
        when (legacyChecksum /= sequenceChecksum) $
            fail "pending queue implementations disagree on FIFO checksum"
        let action = if useSequence
                then pendingSequence inputs failFirst
                else pendingLegacy inputs failFirst
        samples <- mapM (const (measure action)) [1 .. max 1 sampleCount]
        let sample = medianSample samples
            label = if useSequence then "pending-seq" else "pending-legacy"
            scenario = if failFirst then "failure-retry" else "success"
        putStrLn
            ( label <> " scenario=" <> scenario <> " count=" <> show count
                <> " samples=" <> show sampleCount
                <> " elapsed-ms=" <> show sample.sampleElapsedMillis
                <> " cpu-ms=" <> show sample.sampleCpuMillis
                <> " allocated-bytes=" <> show sample.sampleAllocatedBytes
            )

benchmarkStartupOverlap :: Bool -> Int -> Int -> Int -> IO ()
benchmarkStartupOverlap useOverlap worktreeMillis accountMillis sampleCount = do
    statsEnabled <- getRTSStatsEnabled
    when (not statsEnabled) $
        fail "run with +RTS -T"
    let action =
            if useOverlap
                then overlapStartup worktreeMillis accountMillis
                else serialStartup worktreeMillis accountMillis
        label =
            if useOverlap
                then "startup-accounts-overlap"
                else "startup-accounts-serial"
    samples <- mapM (const (measure action)) [1 .. max 1 sampleCount]
    let sample = medianSample samples
    putStrLn
        ( label
            <> " worktree-ms=" <> show worktreeMillis
            <> " account-ms=" <> show accountMillis
            <> " samples=" <> show sampleCount
            <> " elapsed-ms=" <> show sample.sampleElapsedMillis
            <> " cpu-ms=" <> show sample.sampleCpuMillis
            <> " allocated-bytes=" <> show sample.sampleAllocatedBytes
        )

serialStartup :: Int -> Int -> IO Int
serialStartup worktreeMillis accountMillis = do
    threadDelay (worktreeMillis * 1000)
    threadDelay (accountMillis * 1000)
    pure 2

overlapStartup :: Int -> Int -> IO Int
overlapStartup worktreeMillis accountMillis =
    withAsync
        (threadDelay (accountMillis * 1000) >> pure 1)
        \accountWorker -> do
            threadDelay (worktreeMillis * 1000)
            poll accountWorker >>= \case
                Nothing -> do
                    -- The real startup retires an unfinished speculative
                    -- refresh and lets its established post-prompt probe run.
                    threadDelay (accountMillis * 1000)
                    pure 2
                Just (Left err) -> Exception.throwIO err
                Just (Right accountResult) -> pure (1 + accountResult)

medianSample :: [Sample] -> Sample
medianSample samples =
    Sample
        { sampleElapsedMillis = median (map (.sampleElapsedMillis) samples)
        , sampleCpuMillis = median (map (.sampleCpuMillis) samples)
        , sampleAllocatedBytes = median (map (.sampleAllocatedBytes) samples)
        }

pendingLegacy :: [TurnInput] -> Bool -> IO Int
pendingLegacy inputs failFirst = do
    pending <- newIORef ([] :: [TurnInput])
    mapM_ (\input -> atomicModifyIORef' pending \queued ->
        (queued <> [input], ())) inputs
    runLegacy pending failFirst

pendingSequence :: [TurnInput] -> Bool -> IO Int
pendingSequence inputs failFirst = do
    pending <- newPendingInputs
    mapM_ (enqueuePendingInput pending) inputs
    runSequence pending failFirst

runLegacy :: IORef [TurnInput] -> Bool -> IO Int
runLegacy pending failFirst = do
    seen <- newIORef []
    attempts <- newIORef (0 :: Int)
    let backend = legacyWithPending pending $ Backend
            \state _ submitted _ -> do
                modifyIORef' seen (<> [submitted])
                attempt <- atomicModifyIORef' attempts (\n -> (n + 1, n + 1))
                if failFirst && attempt == 1
                    then pure (Left (ConnectionError "benchmark"))
                    else pure $ Right BackendResult
                        { backendOutput = emptyTurnOutput "benchmark" [] Nothing
                        , backendState = state
                        }
    first <- backend.submitTurn emptyBackendSnapshot Nothing [] (const (pure ()))
    case first of
        Left _ | failFirst -> do
            _ <- backend.submitTurn emptyBackendSnapshot Nothing [] (const (pure ()))
            pure ()
        _ -> pure ()
    checksumInputs . concat <$> readIORef seen

runSequence :: PendingInputs -> Bool -> IO Int
runSequence pending failFirst = do
    seen <- newIORef []
    attempts <- newIORef (0 :: Int)
    let backend = withPendingInputs pending $ Backend
            \state _ submitted _ -> do
                modifyIORef' seen (<> [submitted])
                attempt <- atomicModifyIORef' attempts (\n -> (n + 1, n + 1))
                if failFirst && attempt == 1
                    then pure (Left (ConnectionError "benchmark"))
                    else pure $ Right BackendResult
                        { backendOutput = emptyTurnOutput "benchmark" [] Nothing
                        , backendState = state
                        }
    first <- backend.submitTurn emptyBackendSnapshot Nothing [] (const (pure ()))
    case first of
        Left _ | failFirst -> do
            _ <- backend.submitTurn emptyBackendSnapshot Nothing [] (const (pure ()))
            pure ()
        _ -> pure ()
    checksumInputs . concat <$> readIORef seen

legacyWithPending :: IORef [TurnInput] -> Backend -> Backend
legacyWithPending pending (Backend submit) =
    Backend \state previous inputs onEvent -> do
        queued <- atomicModifyIORef' pending (\xs -> ([], xs))
        let requeue = atomicModifyIORef' pending (\current ->
                (queued <> current, ()))
            prefixed = queued <> inputs
        result <- submit state previous prefixed onEvent `onException` requeue
        case result of
            Left _ -> requeue
            Right _ -> pure ()
        pure result

checksumInputs :: [TurnInput] -> Int
checksumInputs = foldl
    (\acc input ->
        Text.foldl' (\value character -> value * 131 + ord character)
            (acc * 17)
            (turnInputText input))
    17
  where
    turnInputText input = case input of
        UserMessage text -> text
        AgentMessage message -> Text.pack (show message)
        _ -> Text.pack (show input)

parseArgs :: [String] -> (Int, Int, Int)
parseArgs = \case
    [count, bytesPerFile, samples] ->
        (max 1 (read count), max 1 (read bytesPerFile), max 1 (read samples))
    _ -> (32, 16 * 1024, 7)

benchmark :: String -> Int -> IO Int -> IO ()
benchmark label count action = do
    samples <- mapM (const (measure action)) [1 .. max 1 count]
    let elapsed = median (map (.sampleElapsedMillis) samples)
        cpu = median (map (.sampleCpuMillis) samples)
        allocated = median (map (.sampleAllocatedBytes) samples)
    putStrLn
        (label
            <> " elapsed-ms=" <> show elapsed
            <> " cpu-ms=" <> show cpu
            <> " allocated-bytes=" <> show allocated)

measure :: IO Int -> IO Sample
measure action = do
    performMajorGC
    beforeStats <- getRTSStats
    beforeCpu <- getCPUTime
    beforeElapsed <- getMonotonicTimeNSec
    checksum <- action
    checksum `seq` pure ()
    afterElapsed <- getMonotonicTimeNSec
    afterCpu <- getCPUTime
    performMajorGC
    afterStats <- getRTSStats
    pure Sample
        { sampleElapsedMillis =
            fromIntegral (afterElapsed - beforeElapsed) / 1_000_000
        , sampleCpuMillis =
            fromIntegral (afterCpu - beforeCpu) / 1_000_000_000
        , sampleAllocatedBytes =
            fromIntegral
                (allocated_bytes afterStats - allocated_bytes beforeStats)
        }

serialDelayed :: Int -> IO Int
serialDelayed count = do
    values <- mapM (const delayedWork) [1 .. count]
    pure (sum values)

boundedDelayed :: Int -> IO Int
boundedDelayed count = do
    values <- mapConcurrentlyBounded 8 (const delayedWork) [1 .. count]
    pure (sum values)

delayedWork :: IO Int
delayedWork = threadDelay 20_000 >> pure 1

serialRead :: [FilePath] -> IO Int
serialRead paths =
    sumLengths <$> mapM BS.readFile paths

boundedRead :: [FilePath] -> IO Int
boundedRead paths =
    sumLengths <$> mapConcurrentlyBounded 4 BS.readFile paths

sumLengths :: [BS.ByteString] -> Int
sumLengths = foldr ((+) . BS.length) 0

withInputFiles :: Int -> Int -> ([FilePath] -> IO a) -> IO a
withInputFiles count bytesPerFile action = do
    root <- getTemporaryDirectory
    let directory = root </> "agent-cli-concurrency-bench"
        contents = BS.replicate bytesPerFile 97
        paths =
            [ directory </> ("attachment-" <> show index <> ".bin")
            | index <- [1 .. count]
            ]
    bracket
        (do
            removePathIfPresent directory
            createDirectoryIfMissing True directory
            mapM_ (`BS.writeFile` contents) paths
            pure paths)
        (const (removePathIfPresent directory))
        action

removePathIfPresent :: FilePath -> IO ()
removePathIfPresent path =
    removePathForcibly path `catchAny` const (pure ())

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Exception.catchAny

median :: Ord a => [a] -> a
median values =
    sort values !! (length values `div` 2)
