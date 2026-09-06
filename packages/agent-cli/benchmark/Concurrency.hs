module Main (main) where

import Agent.CLI.Dialects (CodingTools(..), codingToolsFor)
import Agent.CLI.Options (defaultCliOptions)
import Agent.CLI.PendingInputs
    ( PendingInputs
    , enqueuePendingInput
    , newPendingInputs
    , withPendingInputs
    )
import Agent.CLI.ModelConfig (loadModelCatalogAt)
import Agent.CLI.Project (loadProjectSettings, loadUserSettings)
import Agent.CLI.Session.History (detectGitBranch)
import Agent.CLI.Skills (loadSkillsCatalogQuiet)
import Agent.Concurrent (mapConcurrentlyBounded)
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
    , acquireMcpFleet
    , acquireMcpFleetProgressive
    , closeMcpSupervisor
    , mcpFleetStatuses
    , mcpFleetTools
    , newMcpSupervisor
    , releaseMcpFleetLease
    )
import Agent.OsPath (unsafeToFilePath)
import Agent.ProjectInstructions
    ( DiscoverOptions(..)
    , InstructionFile(..)
    , LoadedAgentsMd
    , defaultDiscoverOptions
    , discoverProjectInstructions
    , loadedInstructionFiles
    , loadedInstructionWarnings
    )
import Agent.ResourceScope
    ( allocateResource
    , allocateFourResourcesConcurrently
    , allocateResourcesConcurrently
    , withResourceScope
    )
import Agent.Skills (Skill(..), SkillCatalog(..))
import Agent.Tools.Types (ToolEnv, defaultToolEnv)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently, poll, withAsync)
import Control.Exception.Safe
    ( SomeException
    , bracket
    , bracketOnError
    , finally
    , onException
    )
import qualified Control.Exception.Safe as Exception
import Control.Monad (forM, forM_, unless, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Char (isDigit, isSpace, ord)
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

data ToolStartupStrategy
    = StartupSerial
    | StartupParentFullOverlap
    | StartupPreloadSkills

data StartupDagStrategy
    = StartupDagBaseline
    | StartupDagCurrent

data StartupDagContext = StartupDagContext
    { dagAgentsContext :: !LoadedAgentsMd
    , dagSkillsCatalog :: !SkillCatalog
    }

data StartupDagAuxResource = StartupDagAuxResource
    { dagAuxLabel :: !Text.Text
    , dagAuxChecksum :: !Int
    }

data StartupDagResources = StartupDagResources
    { dagMcpLease :: !McpFleetLease
    , dagCodingTools :: !CodingTools
    , dagWebResource :: !StartupDagAuxResource
    , dagLspResource :: !StartupDagAuxResource
    }

data StartupDagReleaseCounters = StartupDagReleaseCounters
    { dagMcpReleases :: !(IORef Int)
    , dagLocalReleases :: !(IORef Int)
    , dagWebReleases :: !(IORef Int)
    , dagLspReleases :: !(IORef Int)
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
                StartupSerial
                (read servers)
                (read mcpDelayMillis)
                (read skills)
                (read samples)
        ["startup-tools-parent", servers, mcpDelayMillis, skills, samples] ->
            benchmarkStartupTools
                StartupParentFullOverlap
                (read servers)
                (read mcpDelayMillis)
                (read skills)
                (read samples)
        ["startup-tools-preload-skills",
            servers, mcpDelayMillis, skills, samples] ->
            benchmarkStartupTools
                StartupPreloadSkills
                (read servers)
                (read mcpDelayMillis)
                (read skills)
                (read samples)
        ["startup-tools-paired", servers, mcpDelayMillis, skills, samples] ->
            benchmarkStartupToolsPaired
                (read servers)
                (read mcpDelayMillis)
                (read skills)
                (read samples)
        [ "startup-dag-paired"
            , servers
            , mcpDelayMillis
            , skills
            , auxDelayMillis
            , samples
            ] ->
                benchmarkStartupDagPaired
                    (read servers)
                    (read mcpDelayMillis)
                    (read skills)
                    (read auxDelayMillis)
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

benchmarkStartupTools
    :: ToolStartupStrategy -> Int -> Int -> Int -> Int -> IO ()
benchmarkStartupTools
        strategy serverCount mcpDelayMillis skillCount sampleCount = do
    statsEnabled <- getRTSStatsEnabled
    when (not statsEnabled) $
        fail "run with +RTS -T"
    executable <- getExecutablePath
    let normalizedServers = max 0 serverCount
        normalizedDelay = max 0 mcpDelayMillis
        normalizedSkills = max 0 skillCount
        normalizedSamples = max 1 sampleCount
    withToolStartupFixture
        executable
        normalizedServers
        normalizedDelay
        normalizedSkills
        \fixture -> do
            validateMcpFixture fixture
            serialChecksum <- runToolStartup StartupSerial fixture
            selectedChecksum <- runToolStartup strategy fixture
            when (serialChecksum /= selectedChecksum) $
                fail "startup tool implementations disagree on checksum"
            let label = case strategy of
                    StartupSerial -> "startup-tools-serial"
                    StartupParentFullOverlap -> "startup-tools-parent"
                    StartupPreloadSkills -> "startup-tools-preload-skills"
            samples <-
                mapM
                    (const (measureToolStartup strategy fixture))
                    [1 .. normalizedSamples]
            let sample = medianSample samples
            putStrLn
                ( label
                    <> " mcp-mode=progressive"
                    <> " mcp-servers=" <> show normalizedServers
                    <> " mcp-delay-ms=" <> show normalizedDelay
                    <> " skills=" <> show normalizedSkills
                    <> " samples=" <> show normalizedSamples
                    <> " elapsed-ms=" <> show sample.sampleElapsedMillis
                    <> " cpu-ms=" <> show sample.sampleCpuMillis
                    <> " allocated-bytes=" <> show sample.sampleAllocatedBytes
                )

benchmarkStartupToolsPaired :: Int -> Int -> Int -> Int -> IO ()
benchmarkStartupToolsPaired
        serverCount mcpDelayMillis skillCount sampleCount = do
    statsEnabled <- getRTSStatsEnabled
    when (not statsEnabled) $
        fail "run with +RTS -T"
    executable <- getExecutablePath
    let normalizedServers = max 0 serverCount
        normalizedDelay = max 0 mcpDelayMillis
        normalizedSkills = max 0 skillCount
        normalizedSamples = max 1 sampleCount
    withToolStartupFixture
        executable
        normalizedServers
        normalizedDelay
        normalizedSkills
        \fixture -> do
            validateMcpFixture fixture
            parentChecksum <-
                runToolStartup StartupParentFullOverlap fixture
            currentChecksum <- runToolStartup StartupPreloadSkills fixture
            when (parentChecksum /= currentChecksum) $
                fail "startup tool implementations disagree on checksum"
            pairedSamples <-
                forM [1 .. normalizedSamples] \index ->
                    if odd index
                        then do
                            parentSample <-
                                measureToolStartup
                                    StartupParentFullOverlap
                                    fixture
                            currentSample <-
                                measureToolStartup StartupPreloadSkills fixture
                            pure (parentSample, currentSample)
                        else do
                            currentSample <-
                                measureToolStartup StartupPreloadSkills fixture
                            parentSample <-
                                measureToolStartup
                                    StartupParentFullOverlap
                                    fixture
                            pure (parentSample, currentSample)
            let parentSample = medianSample (map fst pairedSamples)
                currentSample = medianSample (map snd pairedSamples)
                elapsedDelta =
                    median
                        [ current.sampleElapsedMillis
                            - parent.sampleElapsedMillis
                        | (parent, current) <- pairedSamples
                        ]
                cpuDelta =
                    median
                        [ current.sampleCpuMillis - parent.sampleCpuMillis
                        | (parent, current) <- pairedSamples
                        ]
                allocationDelta =
                    median
                        [ toInteger current.sampleAllocatedBytes
                            - toInteger parent.sampleAllocatedBytes
                        | (parent, current) <- pairedSamples
                        ]
                printSample label sample =
                    putStrLn
                        ( label
                            <> " mcp-mode=progressive"
                            <> " pairing=alternating"
                            <> " mcp-servers=" <> show normalizedServers
                            <> " mcp-delay-ms=" <> show normalizedDelay
                            <> " skills=" <> show normalizedSkills
                            <> " samples=" <> show normalizedSamples
                            <> " elapsed-ms="
                            <> show sample.sampleElapsedMillis
                            <> " cpu-ms=" <> show sample.sampleCpuMillis
                            <> " allocated-bytes="
                            <> show sample.sampleAllocatedBytes
                        )
            printSample "startup-tools-paired-parent" parentSample
            printSample "startup-tools-paired-current" currentSample
            putStrLn
                ( "startup-tools-paired-delta"
                    <> " mcp-mode=progressive"
                    <> " pairing=alternating"
                    <> " mcp-servers=" <> show normalizedServers
                    <> " mcp-delay-ms=" <> show normalizedDelay
                    <> " skills=" <> show normalizedSkills
                    <> " samples=" <> show normalizedSamples
                    <> " elapsed-ms=" <> show elapsedDelta
                    <> " cpu-ms=" <> show cpuDelta
                    <> " allocated-bytes=" <> show allocationDelta
                )

-- | Retained comparison for the startup frontier changed in this branch.
-- The baseline stages MCP/local, web/LSP, and context in three waves. The
-- current graph overlaps all four resources with concurrent context preload.
benchmarkStartupDagPaired :: Int -> Int -> Int -> Int -> Int -> IO ()
benchmarkStartupDagPaired
        serverCount mcpDelayMillis skillCount auxDelayMillis sampleCount = do
    statsEnabled <- getRTSStatsEnabled
    when (not statsEnabled) $
        fail "run with +RTS -T"
    executable <- getExecutablePath
    let normalizedServers = max 0 serverCount
        normalizedMcpDelay = max 0 mcpDelayMillis
        normalizedSkills = max 0 skillCount
        normalizedAuxDelay = max 0 auxDelayMillis
        normalizedSamples = max 1 sampleCount
    withStartupDagFixture
        executable
        normalizedServers
        normalizedMcpDelay
        normalizedSkills
        \fixture -> do
            validateMcpFixture fixture
            baselineChecksum <-
                runStartupDag
                    StartupDagBaseline
                    fixture
                    normalizedAuxDelay
            currentChecksum <-
                runStartupDag
                    StartupDagCurrent
                    fixture
                    normalizedAuxDelay
            when (baselineChecksum /= currentChecksum) $
                fail "startup DAG implementations disagree on checksum"
            pairedSamples <-
                forM [1 .. normalizedSamples] \index ->
                    if odd index
                        then do
                            baselineSample <-
                                measureStartupDag
                                    StartupDagBaseline
                                    fixture
                                    normalizedAuxDelay
                            currentSample <-
                                measureStartupDag
                                    StartupDagCurrent
                                    fixture
                                    normalizedAuxDelay
                            pure (baselineSample, currentSample)
                        else do
                            currentSample <-
                                measureStartupDag
                                    StartupDagCurrent
                                    fixture
                                    normalizedAuxDelay
                            baselineSample <-
                                measureStartupDag
                                    StartupDagBaseline
                                    fixture
                                    normalizedAuxDelay
                            pure (baselineSample, currentSample)
            let baselineSample = medianSample (map fst pairedSamples)
                currentSample = medianSample (map snd pairedSamples)
                elapsedDelta =
                    median
                        [ current.sampleElapsedMillis
                            - baseline.sampleElapsedMillis
                        | (baseline, current) <- pairedSamples
                        ]
                cpuDelta =
                    median
                        [ current.sampleCpuMillis - baseline.sampleCpuMillis
                        | (baseline, current) <- pairedSamples
                        ]
                allocationDelta =
                    median
                        [ toInteger current.sampleAllocatedBytes
                            - toInteger baseline.sampleAllocatedBytes
                        | (baseline, current) <- pairedSamples
                        ]
                dimensions =
                    " mcp-mode=progressive"
                        <> " pairing=alternating"
                        <> " baseline-topology=resources2+2-then-context1+1"
                        <> " current-topology=resources4||context2"
                        <> " shared-workspace-loaders=4"
                        <> " resource-branches=4"
                        <> " context-branches=2"
                        <> " context-kind=agents+filesystem-skills"
                        <> " mcp-servers=" <> show normalizedServers
                        <> " mcp-delay-ms=" <> show normalizedMcpDelay
                        <> " filesystem-skills=" <> show normalizedSkills
                        <> " agents-files=1"
                        <> " simulated-aux-resources=2"
                        <> " simulated-aux-kind=web-fetch+lsp"
                        <> " aux-delay-ms=" <> show normalizedAuxDelay
                        <> " samples=" <> show normalizedSamples
                printSample label sample =
                    putStrLn
                        ( label
                            <> dimensions
                            <> " elapsed-ms="
                            <> show sample.sampleElapsedMillis
                            <> " cpu-ms=" <> show sample.sampleCpuMillis
                            <> " allocated-bytes="
                            <> show sample.sampleAllocatedBytes
                        )
            printSample "startup-dag-paired-baseline" baselineSample
            printSample "startup-dag-paired-current" currentSample
            putStrLn
                ( "startup-dag-paired-delta"
                    <> dimensions
                    <> " elapsed-ms=" <> show elapsedDelta
                    <> " cpu-ms=" <> show cpuDelta
                    <> " allocated-bytes=" <> show allocationDelta
                )

runStartupDag
    :: StartupDagStrategy
    -> ToolStartupFixture
    -> Int
    -> IO Int
runStartupDag strategy fixture auxDelayMillis = do
    toolEnv <- defaultToolEnv fixture.fixtureProject
    releaseCounters <- newStartupDagReleaseCounters
    bracket newMcpSupervisor closeMcpSupervisor \supervisor ->
        runStartupDagWithSupervisor
            strategy
            fixture
            auxDelayMillis
            supervisor
            toolEnv
            releaseCounters
            (startupDagChecksum fixture)

measureStartupDag
    :: StartupDagStrategy
    -> ToolStartupFixture
    -> Int
    -> IO Sample
measureStartupDag strategy fixture auxDelayMillis = do
    toolEnv <- defaultToolEnv fixture.fixtureProject
    releaseCounters <- newStartupDagReleaseCounters
    performMajorGC
    beforeStats <- getRTSStats
    beforeCpu <- getCPUTime
    beforeElapsed <- getMonotonicTimeNSec
    bracket newMcpSupervisor closeMcpSupervisor \supervisor ->
        runStartupDagWithSupervisor
            strategy
            fixture
            auxDelayMillis
            supervisor
            toolEnv
            releaseCounters
            \resources context -> do
                checksum <- startupDagChecksum fixture resources context
                checksum `seq` pure ()
                afterElapsed <- getMonotonicTimeNSec
                afterCpu <- getCPUTime
                -- Capture prompt-ready latency and CPU before the accounting
                -- GC and before the resource scope performs cleanup.
                performMajorGC
                afterStats <- getRTSStats
                pure Sample
                    { sampleElapsedMillis =
                        fromIntegral (afterElapsed - beforeElapsed)
                            / 1_000_000
                    , sampleCpuMillis =
                        fromIntegral (afterCpu - beforeCpu)
                            / 1_000_000_000
                    , sampleAllocatedBytes =
                        fromIntegral
                            (allocated_bytes afterStats
                                - allocated_bytes beforeStats)
                    }

runStartupDagWithSupervisor
    :: StartupDagStrategy
    -> ToolStartupFixture
    -> Int
    -> McpSupervisor
    -> ToolEnv
    -> StartupDagReleaseCounters
    -> (StartupDagResources -> StartupDagContext -> IO a)
    -> IO a
runStartupDagWithSupervisor
        strategy
        fixture
        auxDelayMillis
        supervisor
        toolEnv
        releaseCounters
        continuation = do
    let acquireMcp =
            acquireMcpFleetProgressive
                supervisor
                (const (pure ()))
                fixture.fixtureMcpConfigs
        releaseMcp lease =
            releaseMcpFleetLease lease
                `finally` noteRelease releaseCounters.dagMcpReleases
        acquireLocal =
            codingToolsFor
                (dialectForId CodexDialect)
                toolEnv
                Nothing
                Nothing
                Nothing
                Nothing
        releaseLocal coding =
            coding.codingClose
                `finally` noteRelease releaseCounters.dagLocalReleases
        acquireAux label checksum = do
            threadDelay (auxDelayMillis * 1000)
            pure StartupDagAuxResource
                { dagAuxLabel = label
                , dagAuxChecksum = checksum
                }
        acquireWeb = acquireAux "web-fetch" 17
        acquireLsp = acquireAux "lsp" 29
        releaseWeb _ = noteRelease releaseCounters.dagWebReleases
        releaseLsp _ = noteRelease releaseCounters.dagLspReleases
        loadWorkspace = do
            ((projectSettings, userSettings), (catalogResult, branch)) <-
                concurrently
                    (concurrently
                        (loadProjectSettings fixture.fixtureProject)
                        (loadUserSettings fixture.fixtureHome))
                    (concurrently
                        (loadModelCatalogAt
                            fixture.fixtureHome
                            fixture.fixtureProject)
                        (detectGitBranch fixture.fixtureProject))
            catalog <- either (fail . Text.unpack) pure catalogResult
            projectSettings `seq`
                userSettings `seq`
                catalog `seq`
                branch `seq`
                pure ()
        loadAgents =
            discoverProjectInstructions
                (defaultDiscoverOptions
                    { discoverGlobalDir =
                        Just
                            (unsafeEncodeUtf
                                (unsafeToFilePath fixture.fixtureProject
                                    </> ".codex"))
                    , discoverRootMarkers =
                        [unsafeEncodeUtf ".startup-benchmark-root"]
                    })
                fixture.fixtureProject
        loadSkills =
            loadSkillsCatalogQuiet
                defaultCliOptions
                fixture.fixtureHome
                fixture.fixtureProject
                fixture.fixtureProject
        loadContextSequentially = do
            dagAgentsContext <- loadAgents
            dagSkillsCatalog <- loadSkills
            pure StartupDagContext{..}
        loadContextConcurrently = do
            (dagAgentsContext, dagSkillsCatalog) <-
                concurrently loadAgents loadSkills
            pure StartupDagContext{..}
        continueWith
                mcpLease
                codingTools
                webResource
                lspResource
                context =
            continuation
                StartupDagResources
                    { dagMcpLease = mcpLease
                    , dagCodingTools = codingTools
                    , dagWebResource = webResource
                    , dagLspResource = lspResource
                    }
                context
        runBaseline resourceScope = do
            ((_, mcpLease), (_, codingTools)) <-
                allocateResourcesConcurrently
                    resourceScope
                    acquireMcp
                    releaseMcp
                    acquireLocal
                    releaseLocal
            ((_, webResource), (_, lspResource)) <-
                allocateResourcesConcurrently
                    resourceScope
                    acquireWeb
                    releaseWeb
                    acquireLsp
                    releaseLsp
            context <- loadContextSequentially
            continueWith
                mcpLease
                codingTools
                webResource
                lspResource
                context
        runCurrent resourceScope = do
            (acquiredResources, context) <-
                concurrently
                    (allocateFourResourcesConcurrently
                        resourceScope
                        acquireMcp
                        releaseMcp
                        acquireLocal
                        releaseLocal
                        acquireWeb
                        releaseWeb
                        acquireLsp
                        releaseLsp)
                    loadContextConcurrently
            let ( (_, mcpLease)
                    , (_, codingTools)
                    , (_, webResource)
                    , (_, lspResource)
                    ) = acquiredResources
            continueWith
                mcpLease
                codingTools
                webResource
                lspResource
                context
    result <-
        withResourceScope \resourceScope -> do
            loadWorkspace
            case strategy of
                StartupDagBaseline -> runBaseline resourceScope
                StartupDagCurrent -> runCurrent resourceScope
    assertStartupDagReleaseCounts releaseCounters
    pure result

newStartupDagReleaseCounters :: IO StartupDagReleaseCounters
newStartupDagReleaseCounters = do
    dagMcpReleases <- newIORef 0
    dagLocalReleases <- newIORef 0
    dagWebReleases <- newIORef 0
    dagLspReleases <- newIORef 0
    pure StartupDagReleaseCounters{..}

assertStartupDagReleaseCounts :: StartupDagReleaseCounters -> IO ()
assertStartupDagReleaseCounts counters = do
    counts <-
        mapM readIORef
            [ counters.dagMcpReleases
            , counters.dagLocalReleases
            , counters.dagWebReleases
            , counters.dagLspReleases
            ]
    unless (counts == [1, 1, 1, 1]) $
        fail
            ("expected one MCP, local, web, and LSP cleanup, got "
                <> show counts)

startupDagChecksum
    :: ToolStartupFixture
    -> StartupDagResources
    -> StartupDagContext
    -> IO Int
startupDagChecksum fixture resources context = do
    statuses <- mcpFleetStatuses resources.dagMcpLease.mcpLeaseFleet
    let mcpServerCount = length statuses
        expectedMcpServers = length fixture.fixtureMcpConfigs
        mcpWarnings =
            resources.dagMcpLease.mcpLeaseFleet.mcpFleetWarnings
        codingToolCount =
            length resources.dagCodingTools.codingAppTools
        benchmarkSkillCount =
            length
                (filter
                    (Text.isPrefixOf "startup-bench-" . (.skillName))
                    context.dagSkillsCatalog.catalogSkills)
        instructionFiles =
            loadedInstructionFiles context.dagAgentsContext
        instructionWarnings =
            loadedInstructionWarnings context.dagAgentsContext
        instructionCharacters =
            sum (map (Text.length . (.instructionContent)) instructionFiles)
        webResource = resources.dagWebResource
        lspResource = resources.dagLspResource
    unless (null mcpWarnings) $
        fail ("MCP startup warnings: " <> show mcpWarnings)
    when (mcpServerCount /= expectedMcpServers) $
        fail
            ("expected " <> show expectedMcpServers
                <> " MCP servers, got " <> show mcpServerCount)
    when (codingToolCount <= 0) $
        fail "expected coding tool construction to produce tools"
    when (benchmarkSkillCount /= fixture.fixtureSkillCount) $
        fail
            ("expected " <> show fixture.fixtureSkillCount
                <> " benchmark skills, got " <> show benchmarkSkillCount)
    unless (null instructionWarnings) $
        fail ("AGENTS.md discovery warnings: " <> show instructionWarnings)
    when (length instructionFiles /= 1 || instructionCharacters <= 0) $
        fail
            ("expected one non-empty AGENTS.md file, got "
                <> show (length instructionFiles)
                <> " files and "
                <> show instructionCharacters
                <> " characters")
    when
        ( webResource.dagAuxLabel /= "web-fetch"
            || webResource.dagAuxChecksum /= 17
            || lspResource.dagAuxLabel /= "lsp"
            || lspResource.dagAuxChecksum /= 29
        )
        (fail "simulated web/LSP resources disagree")
    pure
        ( mcpServerCount * 1_000_000
            + codingToolCount * 10_000
            + benchmarkSkillCount * 100
            + length instructionFiles * 10
            + instructionCharacters
            + webResource.dagAuxChecksum
            + lspResource.dagAuxChecksum
        )

runToolStartup :: ToolStartupStrategy -> ToolStartupFixture -> IO Int
runToolStartup strategy fixture = do
    toolEnv <- defaultToolEnv fixture.fixtureProject
    mcpReleases <- newIORef (0 :: Int)
    localReleases <- newIORef (0 :: Int)
    bracket newMcpSupervisor closeMcpSupervisor \supervisor ->
        runToolStartupWithSupervisor
            strategy
            fixture
            supervisor
            toolEnv
            mcpReleases
            localReleases
            (toolStartupChecksum fixture)

measureToolStartup :: ToolStartupStrategy -> ToolStartupFixture -> IO Sample
measureToolStartup strategy fixture = do
    toolEnv <- defaultToolEnv fixture.fixtureProject
    mcpReleases <- newIORef (0 :: Int)
    localReleases <- newIORef (0 :: Int)
    performMajorGC
    beforeStats <- getRTSStats
    beforeCpu <- getCPUTime
    beforeElapsed <- getMonotonicTimeNSec
    bracket newMcpSupervisor closeMcpSupervisor \supervisor ->
        runToolStartupWithSupervisor
                strategy
                fixture
                supervisor
                toolEnv
                mcpReleases
                localReleases
                \mcpLease local -> do
                    checksum <- toolStartupChecksum fixture mcpLease local
                    checksum `seq` pure ()
                    afterElapsed <- getMonotonicTimeNSec
                    afterCpu <- getCPUTime
                    -- allocated_bytes is only current after a GC. Capture
                    -- prompt-ready latency and CPU first so this accounting
                    -- barrier and resource cleanup stay outside that interval.
                    performMajorGC
                    afterStats <- getRTSStats
                    pure Sample
                        { sampleElapsedMillis =
                            fromIntegral (afterElapsed - beforeElapsed)
                                / 1_000_000
                        , sampleCpuMillis =
                            fromIntegral (afterCpu - beforeCpu)
                                / 1_000_000_000
                        , sampleAllocatedBytes =
                            fromIntegral
                                (allocated_bytes afterStats
                                    - allocated_bytes beforeStats)
                        }

runToolStartupWithSupervisor
    :: ToolStartupStrategy
    -> ToolStartupFixture
    -> McpSupervisor
    -> ToolEnv
    -> IORef Int
    -> IORef Int
    -> (McpFleetLease -> LocalToolStartup -> IO a)
    -> IO a
runToolStartupWithSupervisor
        strategy fixture supervisor toolEnv mcpReleases localReleases
        continuation = do
    let acquireMcp =
            acquireMcpFleetProgressive
                supervisor
                (const (pure ()))
                fixture.fixtureMcpConfigs
        releaseMcp lease =
            releaseMcpFleetLease lease
                `finally` noteRelease mcpReleases
        acquireLocalWith skills =
            acquireLocalToolStartupWith toolEnv skills
        loadSkills =
            loadSkillsCatalogQuiet
                defaultCliOptions
                fixture.fixtureHome
                fixture.fixtureProject
                fixture.fixtureProject
        acquireLocal = acquireLocalWith loadSkills
        releaseLocal local =
            releaseLocalToolStartup local
                `finally` noteRelease localReleases
        loadWorkspace = do
            ((projectSettings, userSettings), (catalogResult, branch)) <-
                concurrently
                    (concurrently
                        (loadProjectSettings fixture.fixtureProject)
                        (loadUserSettings fixture.fixtureHome))
                    (concurrently
                        (loadModelCatalogAt
                            fixture.fixtureHome
                            fixture.fixtureProject)
                        (detectGitBranch fixture.fixtureProject))
            catalog <- either (fail . Text.unpack) pure catalogResult
            projectSettings `seq`
                userSettings `seq`
                catalog `seq`
                branch `seq`
                pure ()
        acquireSerially resourceScope selectedAcquireLocal = do
            (_, mcpLease) <-
                allocateResource resourceScope acquireMcp releaseMcp
            (_, local) <-
                allocateResource
                    resourceScope
                    selectedAcquireLocal
                    releaseLocal
            continuation mcpLease local
        acquireFullyConcurrently resourceScope selectedAcquireLocal = do
            ((_, mcpLease), (_, local)) <-
                allocateResourcesConcurrently
                    resourceScope
                    acquireMcp
                    releaseMcp
                    selectedAcquireLocal
                    releaseLocal
            continuation mcpLease local
    result <-
        withResourceScope \resourceScope ->
            case strategy of
                StartupSerial ->
                    loadWorkspace
                        >> acquireSerially resourceScope acquireLocal
                StartupParentFullOverlap ->
                    loadWorkspace
                        >> acquireFullyConcurrently resourceScope acquireLocal
                StartupPreloadSkills -> do
                    (_, preloadedSkills) <-
                        concurrently loadWorkspace loadSkills
                    acquireFullyConcurrently
                        resourceScope
                        (acquireLocalWith (pure preloadedSkills))
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

acquireLocalToolStartupWith
    :: ToolEnv
    -> IO SkillCatalog
    -> IO LocalToolStartup
acquireLocalToolStartupWith toolEnv acquireSkills =
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
            localSkills <- acquireSkills
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
    statuses <- mcpFleetStatuses mcpLease.mcpLeaseFleet
    let mcpServerCount = length statuses
        codingToolCount =
            length local.localCoding.codingAppTools
        benchmarkSkillCount =
            length
                (filter
                    (Text.isPrefixOf "startup-bench-" . (.skillName))
                    local.localSkills.catalogSkills)
        expectedMcpServers = length fixture.fixtureMcpConfigs
        warnings = mcpLease.mcpLeaseFleet.mcpFleetWarnings
    unless (null warnings) $
        fail ("MCP startup warnings: " <> show warnings)
    when (mcpServerCount /= expectedMcpServers) $
        fail
            ("expected " <> show expectedMcpServers
                <> " MCP servers, got " <> show mcpServerCount)
    when (codingToolCount <= 0) $
        fail "expected coding tool construction to produce tools"
    when (benchmarkSkillCount /= fixture.fixtureSkillCount) $
        fail
            ("expected " <> show fixture.fixtureSkillCount
                <> " benchmark skills, got " <> show benchmarkSkillCount)
    pure
        (mcpServerCount * 1_000_000
            + codingToolCount * 10_000
            + benchmarkSkillCount)

validateMcpFixture :: ToolStartupFixture -> IO ()
validateMcpFixture fixture =
    bracket newMcpSupervisor closeMcpSupervisor \supervisor ->
        bracket
            (acquireMcpFleet supervisor fixture.fixtureMcpConfigs)
            releaseMcpFleetLease
            \lease -> do
                let toolCount = length (mcpFleetTools lease.mcpLeaseFleet)
                    expected = length fixture.fixtureMcpConfigs
                    warnings = lease.mcpLeaseFleet.mcpFleetWarnings
                unless (null warnings) $
                    fail ("MCP fixture warnings: " <> show warnings)
                when (toolCount /= expected) $
                    fail
                        ("expected " <> show expected
                            <> " discovered MCP tools, got "
                            <> show toolCount)

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

withStartupDagFixture
    :: FilePath
    -> Int
    -> Int
    -> Int
    -> (ToolStartupFixture -> IO a)
    -> IO a
withStartupDagFixture
        executable serverCount mcpDelayMillis skillCount action =
    withToolStartupFixture
        executable
        serverCount
        mcpDelayMillis
        skillCount
        \fixture -> do
            let project = unsafeToFilePath fixture.fixtureProject
            writeFile
                (project </> ".startup-benchmark-root")
                ""
            writeFile
                (project </> "AGENTS.md")
                (unlines
                    [ "# Startup benchmark instructions"
                    , ""
                    , "Exercise the production project-instruction discovery path."
                    , "Keep benchmark resources scoped and startup work concurrent."
                    ])
            action fixture

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
                    respond request initializeResult
                else
                    when
                        ("\"method\":\"tools/list\"" `BS8.isInfixOf` request)
                        (respond request toolsResult)
            loop

    respond request result =
        case requestIdentifier request of
            Nothing -> fail "MCP benchmark request omitted a numeric id"
            Just identifier -> do
                BS8.hPutStrLn stdout
                    ("{\"jsonrpc\":\"2.0\",\"id\":"
                        <> identifier
                        <> ",\"result\":"
                        <> result
                        <> "}")
                hFlush stdout

    initializeResult =
        "{\"protocolVersion\":\"2025-11-25\",\"capabilities\":{},\"serverInfo\":{\"name\":\"startup-bench\",\"version\":\"1\"}}"
    toolsResult =
        BS8.pack
            ("{\"tools\":[{\"name\":\"read_"
                <> show serverIndex
                <> "\",\"description\":\"Read.\",\"inputSchema\":{\"type\":\"object\"},\"annotations\":{\"readOnlyHint\":true}}]}")

requestIdentifier :: BS8.ByteString -> Maybe BS8.ByteString
requestIdentifier request =
    case BS8.breakSubstring "\"id\":" request of
        (_, suffix)
            | BS8.null suffix -> Nothing
            | otherwise ->
                let identifier =
                        BS8.takeWhile isDigit
                            (BS8.dropWhile isSpace (BS8.drop 5 suffix))
                in if BS8.null identifier then Nothing else Just identifier

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
