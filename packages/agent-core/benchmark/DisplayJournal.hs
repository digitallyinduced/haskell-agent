module Main (main) where

import qualified Agent.Loop.DisplayJournal as New
import Agent.Loop.Output (LoopEvent(..))
import Agent.ToolDispatch
    ( ToolCall(..)
    , ToolCallKind(..)
    , ToolCallMode(..)
    , ToolCallResult(..)
    , functionToolCall
    , setToolCallArguments
    )
-- safe-exceptions does not export evaluate.
import Control.Exception (evaluate)
import Control.Monad (forM, forM_, unless)
import Data.Char (ord)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (inits, sort)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)
import qualified DisplayJournalBaseline as Old
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats (GCDetails(..), RTSStats(..), getRTSStats, getRTSStatsEnabled)
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import System.Exit (die)
import System.Mem (performGC)
import Text.Printf (printf)

data Mode = Old | New
data Workload
    = TextFailure
    | TextSuccess
    | TextAfterToolFailure
    | TextAfterToolSuccess
    | ToolsFailure
    | ToolsSuccess
    | RetryRetract

data Sample = Sample
    { wallMs :: !Double
    , cpuMs :: !Double
    , allocated :: !Integer
    }

main :: IO ()
main = getArgs >>= \case
    ["--verify"] -> validatePrefixes
    ["--retain", mode, tools, snapshots, size] -> do
        parsedTools <- positive tools
        parsedSnapshots <- positive snapshots
        parsedSize <- positive size
        retention mode parsedTools parsedSnapshots parsedSize
    args -> runBenchmark args

-- Independent process diagnostic, not a timed sample. The construction frame
-- must be gone before collection so the input list cannot keep dead snapshots
-- alive independently of the journal.
retention :: String -> Int -> Int -> Int -> IO ()
retention mode tools snapshots size = do
    enabled <- getRTSStatsEnabled
    unless enabled $ die "RTS stats disabled; use +RTS -T"
    (project, clear) <- retainedJournal mode tools snapshots size
    performGC
    admitted <- getRTSStats
    output <- project
    checksum <- evaluate (eventsChecksum output)
    performGC
    projected <- getRTSStats
    -- Keep both the journal and output alive across the second collection.
    again <- project >>= evaluate . eventsChecksum
    unless (again == checksum) $ die "retained projection changed"
    _ <- evaluate (eventsChecksum output)
    clear
    performGC
    cleared <- getRTSStats
    -- Keep output and the now-empty reference alive over the final collection.
    emptyOutput <- project
    unless (null emptyOutput) $ die "journal clear failed"
    _ <- evaluate (eventsChecksum output)
    printf "%s,retained,%d,%d,%d,%d,%d,%d,%d\n" mode tools snapshots size
        (gcdetails_live_bytes (gc admitted))
        (gcdetails_live_bytes (gc projected))
        (gcdetails_live_bytes (gc cleared))
        checksum

{-# NOINLINE retainedJournal #-}
retainedJournal :: String -> Int -> Int -> Int -> IO (IO [LoopEvent], IO ())
retainedJournal mode tools snapshots size = case mode of
    "old" -> build [] Old.recordDisplayEvent Old.displayEventsFromJournal
    "new" -> build New.emptyDisplayJournal New.recordDisplayEvent New.displayEventsFromJournal
    _ -> die "mode must be old or new"
  where
    build empty record project = do
        ref <- newIORef empty
        forM_ [1 .. tools] \identifier -> do
            let event = ToolStarted
                    (functionToolCall (Text.pack (show identifier)) "read_file" "{}")
            _ <- evaluate (eventsChecksum [event])
            modifyIORef' ref (record event)
        -- Generate and force each independent payload just before admission.
        -- No input list can root superseded snapshots at the collection point.
        forM_ [1 .. snapshots] \index -> do
            let identifier = Text.pack (show (1 + (index - 1) `mod` tools))
                payload = Text.take size
                    (Text.pack (show index) <> Text.replicate size "x")
                event = ToolOutputUpdated identifier payload
            _ <- evaluate (eventsChecksum [event])
            modifyIORef' ref (record event)
        pure (project <$> readIORef ref, writeIORef ref empty)

runBenchmark :: [String] -> IO ()
runBenchmark args = do
    enabled <- getRTSStatsEnabled
    unless enabled $ die "RTS stats disabled; use +RTS -T"
    case args of
        [modeArg, workloadArg, countArg, sizeArg, samplesArg] -> do
            mode <- case modeArg of
                "old" -> pure Old
                "new" -> pure New
                _ -> die "mode must be old or new"
            workload <- case workloadArg of
                "text-failure" -> pure TextFailure
                "text-success" -> pure TextSuccess
                "text-after-tool-failure" -> pure TextAfterToolFailure
                "text-after-tool-success" -> pure TextAfterToolSuccess
                "tools-failure" -> pure ToolsFailure
                "tools-success" -> pure ToolsSuccess
                "retry-retract" -> pure RetryRetract
                _ -> die "unknown workload"
            count <- positive countArg
            size <- positive sizeArg
            sampleCount <- positive samplesArg
            validatePrefixes
            -- Separate fixtures prevent validation from evaluating timed work.
            old <- runJournal Old workload (fixture workload count size 0)
            new <- runJournal New workload (fixture workload count size 0)
            unless (old == new) $ die "baseline/production output mismatch"
            samples <- forM [1 .. sampleCount] \sample -> do
                let inputs =
                        [ fixture workload count size (sample * repetitions + n)
                        | n <- [1 .. repetitions]
                        ]
                _ <- evaluate $ sum (map eventsChecksum inputs)
                measure $ runInputs mode workload inputs
            printf "%s,%s,%d,%d,%d,%.6f,%.6f,%d\n"
                modeArg workloadArg count size sampleCount
                (median (map (.wallMs) samples) / fromIntegral repetitions)
                (median (map (.cpuMs) samples) / fromIntegral repetitions)
                (median (map (.allocated) samples) `div` fromIntegral repetitions)
        _ -> die "usage: display-journal-bench old|new WORKLOAD COUNT BODY_BYTES SAMPLES"

-- Exercise removals at every position, shared update slots, discarded attempts,
-- reused call ids and text chunk boundaries, including observations before a
-- subsequent event could conceal an ordering bug. Kept outside timing.
validatePrefixes :: IO ()
validatePrefixes =
    forM_ [1 .. 64 :: Word64] \seed ->
        forM_ (inits (mixed seed)) \events -> do
            old <- runJournal Old TextFailure events
            new <- runJournal New TextFailure events
            unless (old == new) $
                die ("mixed-prefix mismatch: seed " <> show seed
                    <> ", prefix " <> show (length events))
  where
    mixed seed =
        map
            (\n -> choices !! fromIntegral ((n `div` 65536) `mod` fromIntegral (length choices)))
            (take 100 (drop 1 (iterate (\n -> n * 6364136223846793005 + 1442695040888963407) seed)))
    first = functionToolCall "same" "read_file" "{}"
    second = functionToolCall "other" "grep" "{\"x\":1}"
    choices =
        [ TextDelta "a"
        , ToolStarted first
        , ToolUpdated first
        , TextDelta ""
        , ToolArgumentsUpdated second
        , ToolOutputUpdated "same" "old"
        , ToolStarted second
        , ToolOutputUpdated "same" "new"
        , ToolRetracted "other"
        , TextDelta "b"
        , ResponseRestarted "retry"
        , ToolArgumentsUpdated first
        , ResponseAttemptDiscarded
        , ToolRetracted "same"
        , ToolFinished (finished "same" "first finish")
        , ToolStarted first
        , ToolFinished (finished "same" "second finish")
        , ResponseAttemptDiscarded
        , ToolFinished (finished "other" "other finish")
        ]

finished :: Text -> Text -> ToolCallResult
finished identifier output = ToolCallResult
    { callId = identifier
    , output
    , callKind = FunctionCallKind
    , toolResultMode = BlockingToolCall
    , toolResultImages = []
    , toolResultOutcome = Nothing
    }

positive :: String -> IO Int
positive raw = case reads raw of
    [(n, "")] | n > 0 -> pure n
    _ -> die ("expected positive integer: " <> raw)

repetitions :: Int
repetitions = 20

-- Exercise the same strict IORef admission/clear/discard operations as
-- recordVisibleLoopEvent. A failed turn forces retained projection; success
-- drops it, matching the loop's TurnFinished branch. This deliberately
-- excludes provider/event-pump timing, which LoopEvents.hs measures separately.
runJournal :: Mode -> Workload -> [LoopEvent] -> IO [LoopEvent]
runJournal mode workload events = case mode of
    Old -> run [] Old.recordDisplayEvent
        Old.discardCurrentDisplayAttempt Old.displayEventsFromJournal
    New -> run New.emptyDisplayJournal New.recordDisplayEvent
        New.discardCurrentDisplayAttempt New.displayEventsFromJournal
  where
    run empty record discard project = do
        ref <- newIORef empty
        mapM_
            (\event ->
                modifyIORef' ref $
                    case event of
                        ResponseAttemptDiscarded -> discard
                        _ -> record event)
            events
        case workload of
            TextSuccess -> writeIORef ref empty
            TextAfterToolSuccess -> writeIORef ref empty
            ToolsSuccess -> writeIORef ref empty
            _ -> pure ()
        project <$> readIORef ref

{-# NOINLINE runInputs #-}
runInputs :: Mode -> Workload -> [[LoopEvent]] -> IO Int
runInputs mode workload = go 0
  where
    go total [] = pure total
    go total (events : rest) = do
        output <- runJournal mode workload events
        checksum <- evaluate (eventsChecksum output)
        let total' = total + checksum
        total' `seq` go total' rest

fixture :: Workload -> Int -> Int -> Int -> [LoopEvent]
fixture workload count size salt = case workload of
    TextFailure -> texts
    TextSuccess -> texts
    TextAfterToolFailure -> ToolStarted (makeCall 1) : texts
    TextAfterToolSuccess -> ToolStarted (makeCall 1) : texts
    ToolsFailure -> tools
    ToolsSuccess -> tools
    RetryRetract ->
        tools
            <> [ResponseRestarted "retry"]
            <> tools
            <> map (ToolRetracted . (.callId)) calls
            <> [TextDelta body, ResponseAttemptDiscarded, TextDelta body]
  where
    body = Text.replicate size (Text.singleton (toEnum (97 + salt `mod` 26)))
    snapshot round =
        Text.replicate size (Text.singleton (toEnum (97 + (salt + round) `mod` 26)))
    texts = replicate count (TextDelta body)
    makeCall :: Int -> ToolCall
    makeCall n =
        functionToolCall
            (Text.pack ("call-" <> show salt <> "-" <> show n))
            "read_file"
            body
    calls = map makeCall [1 .. count]
    tools =
        map ToolStarted calls
            <> concat
                [ concat
                    [ [ if even round then ToolUpdated updated else ToolArgumentsUpdated updated
                      , ToolOutputUpdated call.callId (snapshot round)
                      ]
                    | call <- calls
                    , let updated = setToolCallArguments (snapshot round) call
                    ]
                | round <- [1 .. (16 :: Int)]
                ]
            <> map
                (\call -> ToolFinished (finished call.callId body))
                calls

eventsChecksum :: [LoopEvent] -> Int
eventsChecksum = foldl' (\n event -> n * 33 + eventChecksum event) 5381

eventChecksum :: LoopEvent -> Int
eventChecksum = \case
    TextDelta text -> 1 + textChecksum text
    ToolStarted call -> 2 + callChecksum call
    ToolUpdated call -> 3 + callChecksum call
    ToolArgumentsUpdated call -> 4 + callChecksum call
    ToolOutputUpdated identifier output -> 5 + textChecksum identifier + textChecksum output
    ToolFinished result -> 6 + textChecksum result.callId + textChecksum result.output
    ToolRetracted identifier -> 7 + textChecksum identifier
    ResponseRestarted reason -> 8 + textChecksum reason
    ResponseAttemptDiscarded -> 9
    _ -> error "unexpected benchmark event"

callChecksum :: ToolCall -> Int
callChecksum call =
    textChecksum call.callId + textChecksum call.name + textChecksum call.arguments

textChecksum :: Text -> Int
textChecksum = Text.foldl' (\n c -> n * 33 + ord c) 5381

measure :: IO Int -> IO Sample
measure action = do
    performGC
    before <- getRTSStats
    wallStart <- getMonotonicTimeNSec
    cpuStart <- getCPUTime
    result <- action
    _ <- evaluate result
    cpuEnd <- getCPUTime
    wallEnd <- getMonotonicTimeNSec
    performGC
    after <- getRTSStats
    pure Sample
        { wallMs = fromIntegral (wallEnd - wallStart) / 1e6
        , cpuMs = fromIntegral (cpuEnd - cpuStart) / 1e9
        , allocated = fromIntegral (after.allocated_bytes - before.allocated_bytes)
        }

median :: Ord a => [a] -> a
median values = sort values !! (length values `div` 2)
