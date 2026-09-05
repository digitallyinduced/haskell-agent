module Main (main) where

import Agent.CLI.ComputerUse.Linux.Portal
    ( CapturedPortalFrame(..)
    , PortalFrameState(..)
    , PortalPngFrame(..)
    , beginPortalCaptureRequestWith
    , publishPortalFrameWith
    , readPortalPngFrame
    , waitForPortalFrameAfter
    )
import Control.Concurrent
    ( MVar
    , newMVar
    , threadDelay
    , withMVar
    )
import Control.Concurrent.Async (race, withAsync)
import Control.Concurrent.STM
    ( TVar
    , newTVarIO
    , readTVarIO
    )
import Control.Exception.Safe
    ( bracket
    , catchAny
    , finally
    , mask
    , onException
    , tryAny
    )
import Control.Monad
    ( forM
    , replicateM
    , unless
    , void
    , when
    )
import qualified Data.ByteString as BS
import Data.IORef
    ( IORef
    , modifyIORef'
    , newIORef
    , readIORef
    )
import Data.List (sort)
import qualified Data.Text as Text
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats
    ( RTSStats(..)
    , getRTSStats
    , getRTSStatsEnabled
    )
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import System.IO
    ( Handle
    , hClose
    , hSetBinaryMode
    )
import System.Mem (performMajorGC)
import System.Posix.Process
    ( ProcessTimes(..)
    , getProcessTimes
    )
import qualified System.Posix.Signals as Posix
import System.Posix.Unistd
    ( SysVar(ClockTick)
    , getSysVar
    )
import System.Process
    ( CreateProcess(..)
    , ProcessHandle
    , StdStream(CreatePipe, Inherit, NoStream)
    , createProcess
    , getPid
    , interruptProcessGroupOf
    , proc
    , terminateProcess
    , waitForProcess
    )
import System.Timeout (timeout)
import Text.Printf (printf)
import Text.Read (readMaybe)

data Strategy
    = Continuous
    | RequestGated

data Workload
    = Idle !Int
    | Active !Int

data RunningCapture = RunningCapture
    { runningCaptureProcess :: !ProcessHandle
    , runningCaptureOutput :: !Handle
    }

data GenerationAwareCapture = GenerationAwareCapture
    { generationCaptureProcess :: !ProcessHandle
    , generationCaptureOutput :: !Handle
    , generationCaptureFrameState :: !(TVar PortalFrameState)
    , generationCaptureRequestLock :: !(MVar ())
    , generationCaptureRequestGeneration :: !(TVar Word64)
    , generationCaptureProcessLock :: !(MVar ())
    , generationCaptureTotals :: !(IORef FrameTotals)
    }

data FrameTotals = FrameTotals
    { frameTotalCount :: !Int
    , frameTotalBytes :: !Integer
    , frameTotalChecksum :: !Integer
    }

data Sample = Sample
    { sampleWallMillis :: !Double
    , sampleParentCpuMillis :: !Double
    , sampleChildCpuMillis :: !Double
    , sampleAllocatedBytes :: !Integer
    , sampleFrames :: !Int
    , samplePngBytes :: !Integer
    }

main :: IO ()
main = do
    statsEnabled <- getRTSStatsEnabled
    unless statsEnabled $
        fail "RTS statistics are disabled; run with +RTS -T."
    args <- getArgs
    (width, height, idleMillis, activeCaptures, sampleCount) <-
        parseArgs args
    clockTicks <- getSysVar ClockTick
    let dimensions = show width <> "x" <> show height
    putStrLn
        ( "source=videotestsrc-pattern-snow"
            <> " source-fps=60"
            <> " capture-max-fps=4"
            <> " dimensions=" <> dimensions
            <> " idle-ms=" <> show idleMillis
            <> " active-captures=" <> show activeCaptures
            <> " samples=" <> show sampleCount
            <> " build=optimized-O2"
            <> " pairing=alternating"
        )
    benchmarkWorkload
        clockTicks
        width
        height
        sampleCount
        (Idle idleMillis)
    benchmarkWorkload
        clockTicks
        width
        height
        sampleCount
        (Active activeCaptures)

parseArgs :: [String] -> IO (Int, Int, Int, Int, Int)
parseArgs = \case
    [widthArg, heightArg, idleArg, capturesArg, samplesArg] -> do
        width <- parsePositive "width" widthArg
        height <- parsePositive "height" heightArg
        idleMillis <- parsePositive "idle milliseconds" idleArg
        activeCaptures <- parsePositive "active capture count" capturesArg
        sampleCount <- parsePositive "sample count" samplesArg
        when (width > 32768 || height > 32768) $
            fail "Benchmark dimensions exceed the portal frame limit."
        when (toInteger width * toInteger height > 100000000) $
            fail "Benchmark pixel count exceeds the portal frame limit."
        pure (width, height, idleMillis, activeCaptures, sampleCount)
    _ ->
        fail
            ( "usage: portal-capture-bench WIDTH HEIGHT"
                <> " IDLE_MILLISECONDS ACTIVE_CAPTURES SAMPLES"
            )

parsePositive :: String -> String -> IO Int
parsePositive label raw =
    case readMaybe raw of
        Just value | value > 0 -> pure value
        _ -> fail (label <> " must be a positive integer.")

benchmarkWorkload
    :: Integer
    -> Int
    -> Int
    -> Int
    -> Workload
    -> IO ()
benchmarkWorkload clockTicks width height sampleCount workload = do
    pairedSamples <-
        forM [1 .. sampleCount] \index ->
            if odd index
                then do
                    continuous <-
                        measure clockTicks
                            (runStrategy Continuous workload width height)
                    gated <-
                        measure clockTicks
                            (runStrategy RequestGated workload width height)
                    pure (continuous, gated)
                else do
                    gated <-
                        measure clockTicks
                            (runStrategy RequestGated workload width height)
                    continuous <-
                        measure clockTicks
                            (runStrategy Continuous workload width height)
                    pure (continuous, gated)
    let continuous = medianSample (map fst pairedSamples)
        gated = medianSample (map snd pairedSamples)
        workloadName = case workload of
            Idle _ -> "idle"
            Active _ -> "active"
    printSample workloadName "continuous-baseline" sampleCount continuous
    printSample workloadName "request-gated" sampleCount gated
    printf
        ( "workload=%s strategy=request-gated"
            <> " paired-wall-delta-ms=%.3f"
            <> " paired-parent-cpu-delta-ms=%.3f"
            <> " paired-child-cpu-delta-ms=%.3f"
            <> " paired-allocated-delta-bytes=%d\n"
        )
        workloadName
        (median
            [ current.sampleWallMillis - baseline.sampleWallMillis
            | (baseline, current) <- pairedSamples
            ])
        (median
            [ current.sampleParentCpuMillis
                - baseline.sampleParentCpuMillis
            | (baseline, current) <- pairedSamples
            ])
        (median
            [ current.sampleChildCpuMillis
                - baseline.sampleChildCpuMillis
            | (baseline, current) <- pairedSamples
            ])
        (median
            [ current.sampleAllocatedBytes
                - baseline.sampleAllocatedBytes
            | (baseline, current) <- pairedSamples
            ])

printSample :: String -> String -> Int -> Sample -> IO ()
printSample workloadName strategyName sampleCount sample =
    printf
        ( "workload=%s strategy=%s samples=%d"
            <> " median-wall-ms=%.3f"
            <> " median-parent-cpu-ms=%.3f"
            <> " median-child-cpu-ms=%.3f"
            <> " median-allocated-bytes=%d"
            <> " median-frames=%d"
            <> " median-png-bytes=%d\n"
        )
        workloadName
        strategyName
        sampleCount
        sample.sampleWallMillis
        sample.sampleParentCpuMillis
        sample.sampleChildCpuMillis
        sample.sampleAllocatedBytes
        sample.sampleFrames
        sample.samplePngBytes

runStrategy :: Strategy -> Workload -> Int -> Int -> IO FrameTotals
runStrategy strategy workload width height =
    withSyntheticCapture width height \capture ->
        case strategy of
            Continuous -> runContinuousStrategy capture workload
            RequestGated -> runGenerationAwareStrategy capture workload

runContinuousStrategy :: RunningCapture -> Workload -> IO FrameTotals
runContinuousStrategy capture workload = do
    let readFrame = readFrameTotals capture.runningCaptureOutput
    initial <- readFrame
    workloadTotals <- case workload of
        Idle milliseconds ->
            drainFramesFor (milliseconds * 1000) readFrame
        Active captures ->
            combineFrameTotals <$> replicateM captures readFrame
    pure (addFrameTotals initial workloadTotals)

runGenerationAwareStrategy :: RunningCapture -> Workload -> IO FrameTotals
runGenerationAwareStrategy capture workload = do
    frameState <- newTVarIO PortalFramePending
    requestLock <- newMVar ()
    requestGeneration <- newTVarIO 0
    processLock <- newMVar ()
    totals <- newIORef emptyFrameTotals
    let generationCapture = GenerationAwareCapture
            { generationCaptureProcess = capture.runningCaptureProcess
            , generationCaptureOutput = capture.runningCaptureOutput
            , generationCaptureFrameState = frameState
            , generationCaptureRequestLock = requestLock
            , generationCaptureRequestGeneration = requestGeneration
            , generationCaptureProcessLock = processLock
            , generationCaptureTotals = totals
            }
    withAsync (runGenerationAwareReader generationCapture) \_reader -> do
        void (waitForGenerationFrame generationCapture 0)
        case workload of
            Idle milliseconds -> do
                threadDelay (milliseconds * 1000)
                readIORef totals
            Active captures ->
                void
                    (replicateM
                        captures
                        (captureGenerationAwareFrame generationCapture))
                    >> readIORef totals

runGenerationAwareReader :: GenerationAwareCapture -> IO ()
runGenerationAwareReader capture = loop 0
  where
    loop frameGeneration = do
        pngFrame <- readPortalPngFrame capture.generationCaptureOutput
        modifyIORef'
            capture.generationCaptureTotals
            (`addFrameTotals` portalFrameTotals pngFrame)
        published <-
            publishPortalFrameWith
                capture.generationCaptureProcessLock
                capture.generationCaptureRequestGeneration
                capture.generationCaptureFrameState
                (suspendGenerationCapture capture)
                frameGeneration
                pngFrame
        nextGeneration <-
            if published
                then pure frameGeneration
                else readTVarIO capture.generationCaptureRequestGeneration
        loop nextGeneration

captureGenerationAwareFrame
    :: GenerationAwareCapture
    -> IO CapturedPortalFrame
captureGenerationAwareFrame capture =
    withMVar capture.generationCaptureRequestLock \() ->
        mask \restore -> do
            baseline <-
                beginPortalCaptureRequestWith
                    capture.generationCaptureProcessLock
                    capture.generationCaptureRequestGeneration
                    capture.generationCaptureFrameState
                    (resumeGenerationCapture capture)
            restore (waitForGenerationFrame capture baseline)
                `finally`
                    withMVar
                        capture.generationCaptureProcessLock
                        (const (suspendGenerationCapture capture))

waitForGenerationFrame
    :: GenerationAwareCapture
    -> Word64
    -> IO CapturedPortalFrame
waitForGenerationFrame capture baseline =
    waitForPortalFrameAfter
        1_000_000
        baseline
        capture.generationCaptureFrameState
        >>= either (fail . Text.unpack) pure

resumeGenerationCapture :: GenerationAwareCapture -> IO ()
resumeGenerationCapture capture =
    signalCaptureProcess Posix.sigCONT capture.generationCaptureProcess

suspendGenerationCapture :: GenerationAwareCapture -> IO ()
suspendGenerationCapture capture =
    signalCaptureProcess Posix.sigSTOP capture.generationCaptureProcess

drainFramesFor :: Int -> IO FrameTotals -> IO FrameTotals
drainFramesFor microseconds readFrame = do
    totalsRef <- newIORef emptyFrameTotals
    void $
        race
            (threadDelay microseconds)
            (let loop = do
                    totals <- readFrame
                    modifyIORef' totalsRef (`addFrameTotals` totals)
                    loop
             in loop)
    readIORef totalsRef

readFrameTotals :: Handle -> IO FrameTotals
readFrameTotals input = do
    frame <- readPortalPngFrame input
    pure (portalFrameTotals frame)

portalFrameTotals :: PortalPngFrame -> FrameTotals
portalFrameTotals frame =
    let byteCount = toInteger (BS.length frame.portalPngFrameBytes)
        checksum =
            byteCount
                + toInteger frame.portalPngFrameWidth
                + toInteger frame.portalPngFrameHeight
    in FrameTotals
        { frameTotalCount = 1
        , frameTotalBytes = byteCount
        , frameTotalChecksum = checksum
        }

emptyFrameTotals :: FrameTotals
emptyFrameTotals =
    FrameTotals
        { frameTotalCount = 0
        , frameTotalBytes = 0
        , frameTotalChecksum = 0
        }

addFrameTotals :: FrameTotals -> FrameTotals -> FrameTotals
addFrameTotals left right =
    FrameTotals
        { frameTotalCount = left.frameTotalCount + right.frameTotalCount
        , frameTotalBytes = left.frameTotalBytes + right.frameTotalBytes
        , frameTotalChecksum =
            left.frameTotalChecksum + right.frameTotalChecksum
        }

combineFrameTotals :: [FrameTotals] -> FrameTotals
combineFrameTotals = foldr addFrameTotals emptyFrameTotals

withSyntheticCapture
    :: Int
    -> Int
    -> (RunningCapture -> IO value)
    -> IO value
withSyntheticCapture width height =
    bracket
        (startSyntheticCapture width height)
        stopSyntheticCapture

startSyntheticCapture :: Int -> Int -> IO RunningCapture
startSyntheticCapture width height = do
    let caps =
            "video/x-raw,width=" <> show width
                <> ",height=" <> show height
                <> ",framerate=60/1"
        command =
            (proc
                "gst-launch-1.0"
                [ "-q"
                , "videotestsrc"
                , "is-live=true"
                , "pattern=snow"
                , "!"
                , caps
                , "!"
                , "videorate"
                , "drop-only=true"
                , "max-rate=4"
                , "!"
                , "videoconvert"
                , "!"
                , "pngenc"
                , "snapshot=false"
                , "!"
                , "fdsink"
                , "fd=1"
                , "sync=false"
                ])
                { std_in = NoStream
                , std_out = CreatePipe
                , std_err = Inherit
                , create_group = True
                }
    createProcess command >>= \case
        (_, Just output, _, processHandle) -> do
            let capture =
                    RunningCapture
                        { runningCaptureProcess = processHandle
                        , runningCaptureOutput = output
                        }
            hSetBinaryMode output True
                `onException` stopSyntheticCapture capture
            pure capture
        _ -> fail "Unable to capture the synthetic GStreamer output."

stopSyntheticCapture :: RunningCapture -> IO ()
stopSyntheticCapture capture = do
    void $
        tryAny
            (signalCaptureProcess
                Posix.sigCONT
                capture.runningCaptureProcess)
    void $ tryAny (interruptProcessGroupOf capture.runningCaptureProcess)
    timeout 2_000_000 (waitForProcess capture.runningCaptureProcess)
        >>= \case
            Just _ -> pure ()
            Nothing -> do
                void $ tryAny (terminateProcess capture.runningCaptureProcess)
                timeout 2_000_000
                    (waitForProcess capture.runningCaptureProcess)
                    >>= \case
                        Just _ -> pure ()
                        Nothing -> do
                            getPid capture.runningCaptureProcess >>= \case
                                Nothing -> pure ()
                                Just processGroup ->
                                    void $
                                        tryAny
                                            (Posix.signalProcessGroup
                                                Posix.sigKILL
                                                processGroup)
                            void $
                                timeout 2_000_000
                                    (waitForProcess
                                        capture.runningCaptureProcess)
    hClose capture.runningCaptureOutput `catchAny` \_ -> pure ()

signalCaptureProcess :: Posix.Signal -> ProcessHandle -> IO ()
signalCaptureProcess signal processHandle =
    getPid processHandle >>= \case
        Nothing -> fail "The synthetic GStreamer process has exited."
        Just processGroup ->
            Posix.signalProcessGroup signal processGroup

measure :: Integer -> IO FrameTotals -> IO Sample
measure clockTicks action = do
    performMajorGC
    beforeStats <- getRTSStats
    beforeProcessTimes <- getProcessTimes
    beforeCpu <- getCPUTime
    beforeWall <- getMonotonicTimeNSec
    totals <- action
    totals.frameTotalChecksum `seq` pure ()
    afterWall <- getMonotonicTimeNSec
    afterCpu <- getCPUTime
    afterProcessTimes <- getProcessTimes
    performMajorGC
    afterStats <- getRTSStats
    let childTicks =
            childCpuTicks afterProcessTimes
                - childCpuTicks beforeProcessTimes
    pure Sample
        { sampleWallMillis =
            fromIntegral (afterWall - beforeWall) / 1_000_000
        , sampleParentCpuMillis =
            fromIntegral (afterCpu - beforeCpu) / 1_000_000_000
        , sampleChildCpuMillis =
            childTicks * 1000 / fromIntegral clockTicks
        , sampleAllocatedBytes =
            toInteger
                (allocated_bytes afterStats - allocated_bytes beforeStats)
        , sampleFrames = totals.frameTotalCount
        , samplePngBytes = totals.frameTotalBytes
        }

childCpuTicks :: ProcessTimes -> Double
childCpuTicks processTimes =
    realToFrac processTimes.childUserTime
        + realToFrac processTimes.childSystemTime

medianSample :: [Sample] -> Sample
medianSample samples =
    Sample
        { sampleWallMillis = median (map (.sampleWallMillis) samples)
        , sampleParentCpuMillis =
            median (map (.sampleParentCpuMillis) samples)
        , sampleChildCpuMillis =
            median (map (.sampleChildCpuMillis) samples)
        , sampleAllocatedBytes =
            median (map (.sampleAllocatedBytes) samples)
        , sampleFrames = median (map (.sampleFrames) samples)
        , samplePngBytes = median (map (.samplePngBytes) samples)
        }

median :: Ord value => [value] -> value
median values =
    sort values !! (length values `div` 2)
