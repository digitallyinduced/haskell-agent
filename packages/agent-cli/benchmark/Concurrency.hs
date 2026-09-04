module Main (main) where

import Agent.CLI.Dialects (CodingTools(..), codingToolsFor)
import Agent.CLI.Options (defaultCliOptions)
import Agent.CLI.Skills (loadSkillsCatalogQuiet)
import Agent.Concurrent (mapConcurrentlyBounded)
import Agent.CLI.PendingInputs
    ( PendingInputs
    , enqueuePendingInput
    , newPendingInputs
    , withPendingInputs
    )
import Agent.Dialect (DialectId(CodexDialect), dialectForId)
import Agent.Loop
    ( Backend(..)
    , BackendResult(..)
    , TurnInput(..)
    , emptyBackendSnapshot
    , emptyTurnOutput
    )
import Agent.Error (ApiError(..))
import Agent.MCP
    ( McpFleet(..)
    , McpFleetLease(..)
    , McpProtocolPreference(..)
    , McpServerConfig(..)
    , McpSupervisor
    , acquireMcpFleetWithProgress
    , closeMcpSupervisor
    , mcpFleetTools
    , newMcpSupervisor
    , releaseMcpFleetLease
    )
import Agent.ResourceScope
    ( ResourceKey
    , allocateResource
    , allocateResourcesConcurrently
    , releaseResource
    , withResourceScope
    )
import Agent.Skills (Skill(..), SkillCatalog(..))
import Agent.Tools.Types (ToolEnv, defaultToolEnv)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (poll, withAsync)
import Control.Exception.Safe
    ( SomeException
    , bracket
    , bracketOnError
    , finally
    , onException
    )
import qualified Control.Exception.Safe as Exception
import Control.Monad (forM_, unless, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Char (ord)
import Data.IORef
    ( IORef
    , atomicModifyIORef'
    , modifyIORef'
    , newIORef
    , readIORef
    )
import Data.List (sort)
import qualified Data.Text as Text
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
import System.Environment (getArgs, getExecutablePath)
import System.FilePath ((</>))
import System.IO (hFlush, hIsEOF, stdin, stdout)
import System.Mem (performMajorGC)
import System.OsPath (OsPath, unsafeEncodeUtf)

data Sample = Sample
    { sampleElapsedMillis :: !Double
    , sampleCpuMillis :: !Double
    , sampleAllocatedBytes :: !Word
    }

data ToolStartupFixture = ToolStartupFixture
    { fixtureMcpConfigs :: ![McpServerConfig]
    , fixtureHome :: !OsPath
    , fixtureProject :: !OsPath
    , fixtureSkillCount :: !Int
    }

data LocalToolStartup = LocalToolStartup
    { localCoding :: !CodingTools
    , localSkills :: !SkillCatalog
    }

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["fake-mcp-server", delayMillis, serverIndex] ->
            runFakeMcpServer
                (max 0 (read delayMillis))
                (max 1 (read serverIndex))
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
        ["startup-tools-serial", servers, mcpDelayMillis, skills, samples] ->
            benchmarkStartupTools
                False
                (read servers)
                (read mcpDelayMillis)
                (read skills)
                (read samples)
        ["startup-tools-overlap", servers, mcpDelayMillis, skills, samples] ->
            benchmarkStartupTools
                True
                (read servers)
                (read mcpDelayMillis)
                (read skills)
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

benchmarkStartupTools :: Bool -> Int -> Int -> Int -> Int -> IO ()
benchmarkStartupTools
        useOverlap serverCount mcpDelayMillis skillCount sampleCount = do
    statsEnabled <- getRTSStatsEnabled
    when (not statsEnabled) $
        fail "run with +RTS -T"
    executable <- getExecutablePath
    let normalizedServers = max 1 serverCount
        normalizedDelay = max 0 mcpDelayMillis
        normalizedSkills = max 1 skillCount
        normalizedSamples = max 1 sampleCount
    withToolStartupFixture
        executable
        normalizedServers
        normalizedDelay
        normalizedSkills
        \fixture -> do
            serialChecksum <- runToolStartup False fixture
            overlapChecksum <- runToolStartup True fixture
            when (serialChecksum /= overlapChecksum) $
                fail "startup tool implementations disagree on checksum"
            let label =
                    if useOverlap
                        then "startup-tools-overlap"
                        else "startup-tools-serial"
            samples <-
                mapM
                    (const (measureToolStartup useOverlap fixture))
                    [1 .. normalizedSamples]
            let sample = medianSample samples
            putStrLn
                ( label
                    <> " servers=" <> show normalizedServers
                    <> " mcp-delay-ms=" <> show normalizedDelay
                    <> " skills=" <> show normalizedSkills
                    <> " samples=" <> show normalizedSamples
                    <> " elapsed-ms=" <> show sample.sampleElapsedMillis
                    <> " cpu-ms=" <> show sample.sampleCpuMillis
                    <> " allocated-bytes=" <> show sample.sampleAllocatedBytes
                )

runToolStartup :: Bool -> ToolStartupFixture -> IO Int
runToolStartup useOverlap fixture = do
    toolEnv <- defaultToolEnv fixture.fixtureProject
    mcpReleases <- newIORef (0 :: Int)
    localReleases <- newIORef (0 :: Int)
    bracket newMcpSupervisor closeMcpSupervisor \supervisor ->
        runToolStartupWithSupervisor
            useOverlap
            fixture
            supervisor
            toolEnv
            mcpReleases
            localReleases

measureToolStartup :: Bool -> ToolStartupFixture -> IO Sample
measureToolStartup useOverlap fixture = do
    toolEnv <- defaultToolEnv fixture.fixtureProject
    mcpReleases <- newIORef (0 :: Int)
    localReleases <- newIORef (0 :: Int)
    bracket newMcpSupervisor closeMcpSupervisor \supervisor ->
        measure
            (runToolStartupWithSupervisor
                useOverlap
                fixture
                supervisor
                toolEnv
                mcpReleases
                localReleases)

runToolStartupWithSupervisor
    :: Bool
    -> ToolStartupFixture
    -> McpSupervisor
    -> ToolEnv
    -> IORef Int
    -> IORef Int
    -> IO Int
runToolStartupWithSupervisor
        useOverlap fixture supervisor toolEnv mcpReleases localReleases = do
    let acquireMcp =
            acquireMcpFleetWithProgress
                supervisor
                (const (pure ()))
                fixture.fixtureMcpConfigs
        releaseMcp lease =
            releaseMcpFleetLease lease
                `finally` noteRelease mcpReleases
        acquireLocal = acquireLocalToolStartup fixture toolEnv
        releaseLocal local =
            releaseLocalToolStartup local
                `finally` noteRelease localReleases
    checksum <-
        if useOverlap
            then
                -- Mirrors the scoped MCP/local acquisition block in
                -- runAgentToolsRequest, including its early close actions.
                withResourceScope \resourceScope -> do
                    (mcpResource, localResource) <-
                        allocateResourcesConcurrently
                            resourceScope
                            acquireMcp
                            releaseMcp
                            acquireLocal
                            releaseLocal
                    consumeToolStartupResources
                        fixture
                        mcpReleases
                        localReleases
                        mcpResource
                        localResource
            else
                -- Use the same ownership and cleanup path while acquiring in
                -- the order used before startup acquisition was overlapped.
                withResourceScope \resourceScope -> do
                    mcpResource <-
                        allocateResource resourceScope acquireMcp releaseMcp
                    localResource <-
                        allocateResource
                            resourceScope
                            acquireLocal
                            releaseLocal
                    consumeToolStartupResources
                        fixture
                        mcpReleases
                        localReleases
                        mcpResource
                        localResource
    assertReleaseCounts mcpReleases localReleases
    pure checksum

consumeToolStartupResources
    :: ToolStartupFixture
    -> IORef Int
    -> IORef Int
    -> (ResourceKey, McpFleetLease)
    -> (ResourceKey, LocalToolStartup)
    -> IO Int
consumeToolStartupResources
        fixture mcpReleases localReleases
        (mcpKey, mcpLease) (localKey, local) = do
    result <- toolStartupChecksum fixture mcpLease local
    result `seq` pure ()
    releaseResource mcpKey
        `finally` releaseResource localKey
    assertReleaseCounts mcpReleases localReleases
    pure result

noteRelease :: IORef Int -> IO ()
noteRelease releases =
    atomicModifyIORef' releases \count -> (count + 1, ())

assertReleaseCounts :: IORef Int -> IORef Int -> IO ()
assertReleaseCounts mcpReleases localReleases = do
    mcpCount <- readIORef mcpReleases
    localCount <- readIORef localReleases
    unless (mcpCount == 1 && localCount == 1) $
        fail
            ("expected one MCP and one local cleanup, got "
                <> show mcpCount <> " and " <> show localCount)

acquireLocalToolStartup :: ToolStartupFixture -> ToolEnv -> IO LocalToolStartup
acquireLocalToolStartup fixture toolEnv =
    bracketOnError
        (codingToolsFor
            (dialectForId CodexDialect)
            toolEnv
            Nothing
            Nothing
            Nothing
            Nothing)
        (.codingClose)
        \localCoding -> do
            localSkills <-
                loadSkillsCatalogQuiet
                    defaultCliOptions
                    fixture.fixtureHome
                    fixture.fixtureProject
                    fixture.fixtureProject
            pure LocalToolStartup{..}

releaseLocalToolStartup :: LocalToolStartup -> IO ()
releaseLocalToolStartup startup =
    startup.localCoding.codingClose

toolStartupChecksum
    :: ToolStartupFixture
    -> McpFleetLease
    -> LocalToolStartup
    -> IO Int
toolStartupChecksum fixture mcpLease local = do
    let mcpToolCount =
            length (mcpFleetTools mcpLease.mcpLeaseFleet)
        codingToolCount =
            length local.localCoding.codingAppTools
        benchmarkSkillCount =
            length
                (filter
                    (Text.isPrefixOf "startup-bench-" . (.skillName))
                    local.localSkills.catalogSkills)
        expectedMcpTools = length fixture.fixtureMcpConfigs
    when (mcpToolCount /= expectedMcpTools) $
        fail
            ("expected " <> show expectedMcpTools
                <> " MCP tools, got " <> show mcpToolCount
                <> "; warnings: "
                <> show mcpLease.mcpLeaseFleet.mcpFleetWarnings)
    when (codingToolCount <= 0) $
        fail "expected coding tool construction to produce tools"
    when (benchmarkSkillCount /= fixture.fixtureSkillCount) $
        fail
            ("expected " <> show fixture.fixtureSkillCount
                <> " benchmark skills, got " <> show benchmarkSkillCount)
    pure
        (mcpToolCount * 1_000_000
            + codingToolCount * 10_000
            + benchmarkSkillCount)

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

withToolStartupFixture
    :: FilePath
    -> Int
    -> Int
    -> Int
    -> (ToolStartupFixture -> IO a)
    -> IO a
withToolStartupFixture
        executable serverCount mcpDelayMillis skillCount action = do
    temporary <- getTemporaryDirectory
    nonce <- getMonotonicTimeNSec
    let directory =
            temporary </> ("agent-cli-tool-startup-bench-" <> show nonce)
    bracket
        (do
            let home = directory </> "home"
                project = directory </> "project"
            createDirectoryIfMissing True home
            createDirectoryIfMissing True project
            writeBenchmarkSkills project skillCount
            let fixtureMcpConfigs =
                    [ fakeMcpConfig
                        executable
                        project
                        mcpDelayMillis
                        index
                    | index <- [1 .. serverCount]
                    ]
                fixtureHome = unsafeEncodeUtf home
                fixtureProject = unsafeEncodeUtf project
                fixtureSkillCount = skillCount
            pure ToolStartupFixture{..})
        (const (removePathIfPresent directory))
        action

writeBenchmarkSkills :: FilePath -> Int -> IO ()
writeBenchmarkSkills project skillCount =
    forM_ [1 .. skillCount] \index -> do
        let name = "startup-bench-" <> show index
            directory =
                project </> ".agents" </> "skills" </> name
        createDirectoryIfMissing True directory
        writeFile (directory </> "SKILL.md") $
            unlines
                [ "---"
                , "name: " <> name
                , "description: Representative startup benchmark skill "
                    <> show index
                , "---"
                , "# Instructions"
                , "Exercise the production skill discovery path."
                ]

fakeMcpConfig :: FilePath -> FilePath -> Int -> Int -> McpServerConfig
fakeMcpConfig executable project delayMillis index = McpServerConfig
    { mcpServerName = "startup-bench-" <> Text.pack (show index)
    , mcpServerUrl = Nothing
    , mcpServerCommand = executable
    , mcpServerArgs =
        ["fake-mcp-server", show delayMillis, show index]
    , mcpServerCwd = Just project
    , mcpServerEnv = []
    , mcpServerStartupTimeoutSeconds = 30
    , mcpServerRequestTimeoutSeconds = 30
    -- Keep the fixture on one deterministic initialize/tools-list handshake;
    -- Auto starts with the separate modern server/discover probe.
    , mcpServerProtocol = McpProtocolLegacy
    }

runFakeMcpServer :: Int -> Int -> IO ()
runFakeMcpServer delayMillis serverIndex = loop
  where
    loop = do
        eof <- hIsEOF stdin
        unless eof do
            request <- BS8.hGetLine stdin
            if "\"method\":\"initialize\"" `BS8.isInfixOf` request
                then do
                    threadDelay (delayMillis * 1000)
                    respond initializeResponse
                else
                    when
                        ("\"method\":\"tools/list\"" `BS8.isInfixOf` request)
                        (respond toolsResponse)
            loop

    respond response = do
        BS8.hPutStrLn stdout response
        hFlush stdout

    initializeResponse =
        "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"protocolVersion\":\"2025-11-25\",\"capabilities\":{},\"serverInfo\":{\"name\":\"startup-bench\",\"version\":\"1\"}}}"
    toolsResponse =
        BS8.pack
            ("{\"jsonrpc\":\"2.0\",\"id\":2,\"result\":{\"tools\":[{\"name\":\"read_"
                <> show serverIndex
                <> "\",\"description\":\"Read.\",\"inputSchema\":{\"type\":\"object\"},\"annotations\":{\"readOnlyHint\":true}}]}}")

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
