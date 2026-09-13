-- Diagnostic benchmark for the actual viewport cache. "full" models the
-- fullscreen mailbox's complete size-accounting traversal; "visible" models
-- a consumer displaying only two steps. Neither retains returned snapshots.
-- The "-single" variants use single-word preview lines to detect Text slices.
module Main (main) where

import Agent.CLI.AgentViewport (AgentEntry(..), AgentStep(..))
import Agent.CLI.AgentViewport.Runtime
import Agent.Responses.Types
import Agent.Subagents (SubagentId(..), SubagentStatus(..))
import Control.Exception (evaluate)
import Control.Exception.Safe (bracket)
import Control.Monad (forM, unless)
import Data.IORef
import Data.List (foldl', sort)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Foreign.StablePtr (freeStablePtr, newStablePtr)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import System.Exit (die)
import System.Mem (performGC)
import Text.Printf (printf)

main :: IO ()
main = do
    enabled <- getRTSStatsEnabled
    unless enabled (die "run with +RTS -T")
    supplied <- getArgs
    let args = if null supplied
            then ["full-single", "8", "200", "65536", "5"]
            else supplied
    case args of
        [mode, agentsArg, itemsArg, bytesArg, samplesArg]
            | mode `elem` ["full", "visible", "full-single", "visible-single"] -> do
                agents <- positive agentsArg
                items <- positive itemsArg
                bytes <- positive bytesArg
                samples <- positive samplesArg
                results <- forM [1 .. samples] \salt ->
                    measure mode agents items bytes salt
                let median xs = sort xs !! (length xs `div` 2)
                    elapsed = median [e | (e, _, _, _, _) <- results]
                    cpu = median [c | (_, c, _, _, _) <- results]
                    allocated = median [a | (_, _, a, _, _) <- results]
                    live = median [l | (_, _, _, l, _) <- results]
                    checksum = sum [c | (_, _, _, _, c) <- results]
                printf "%s,%d,%d,%d,%.3f,%.3f,%d,%d,%d\n"
                    mode agents items bytes elapsed cpu allocated live checksum
        _ -> die "usage: viewport-retention MODE AGENTS ITEMS BYTES SAMPLES"
  where
    positive raw = case reads raw of
        [(n, "")] | n > 0 -> pure n
        _ -> die ("expected positive integer: " <> raw)

measure :: String -> Int -> Int -> Int -> Int
    -> IO (Double, Double, Integer, Integer, Int)
measure mode agents items bytes salt = do
    sources <- prepareSources ("-single" `Text.isSuffixOf` Text.pack mode)
        agents items bytes salt
    runtime <- newAgentViewportRuntime AgentViewportRuntimeConfig
        { viewportConfigShowRawReasoning = False
        , viewportConfigWorkspace = ""
        , viewportConfigReadRootTranscript = pure []
        , viewportConfigListChildren = pure
            [ AgentChildListing
                (Text.pack ("/root/" <> show index))
                (identifier index)
                (Completed Nothing)
            | index <- [1 .. agents]
            ]
        , viewportConfigReadChildSources =
            Map.map (AgentChildSource "benchmark" . pure) <$> readIORef sources
        , viewportConfigSelectChild = const (pure ())
        , viewportConfigReleaseChild = const (pure ())
        }
    -- Explicitly root the actual runtime/cache through the final collection.
    bracket (newStablePtr runtime) freeStablePtr \_ -> do
        performGC
        before <- getRTSStats
        start <- getMonotonicTimeNSec
        startCpu <- getCPUTime
        checksum <- consumeSnapshot mode runtime
        -- The loader closes over only this mutable source map. Clearing it
        -- simulates a child becoming cold without replacing cached previews.
        writeIORef sources Map.empty
        performGC
        performGC
        after <- getRTSStats
        end <- getMonotonicTimeNSec
        endCpu <- getCPUTime
        pure
            ( fromIntegral (end - start) / 1.0e6
            , fromIntegral (endCpu - startCpu) / 1.0e9
            , fromIntegral (after.allocated_bytes - before.allocated_bytes)
            , fromIntegral after.gc.gcdetails_live_bytes
            , checksum
            )

{-# NOINLINE consumeSnapshot #-}
consumeSnapshot :: String -> AgentViewportRuntime -> IO Int
consumeSnapshot mode runtime = do
    (_, entries) <- loadAgentSnapshot runtime False
    evaluate $ foldl' (\total entry ->
        total + checksumSteps
            (if "visible" `Text.isPrefixOf` Text.pack mode then take 2 entry.agentSteps
             else entry.agentSteps)) 0 entries

checksumSteps :: [AgentStep] -> Int
checksumSteps = foldl' (\total step ->
    total + Text.length step.agentStepTitle
        + maybe 0 Text.length step.agentStepDetail) 0

{-# NOINLINE prepareSources #-}
prepareSources :: Bool -> Int -> Int -> Int -> Int
    -> IO (IORef (Map.Map SubagentId [ResponseItem]))
prepareSources single agents items bytes salt = do
    let source = Map.fromList
            [ (identifier agent, map (message agent) [1 .. items])
            | agent <- [1 .. agents]
            ]
        message agent item = MessageItem ResponseMessage
            { messageId = Nothing
            , role = RoleAssistant
            , status = Just ItemCompleted
            , phase = Nothing
            , passthrough = Nothing
            , content = MessageContentText $
                Text.pack (show salt <> ":" <> show agent <> ":" <> show item)
                <> (if single then "\nCompleted\n"
                    else " completed coding task\nSecond preview line\n")
                <> Text.replicate bytes "x"
            }
        forceItem total (MessageItem message_) =
            case message_.content of
                MessageContentText text -> total + Text.length text
                _ -> total
        forceItem total _ = total
    _ <- evaluate (Map.foldl' (\total transcript ->
        foldl' forceItem total transcript) 0 source)
    newIORef source

identifier :: Int -> SubagentId
identifier = SubagentId . Text.pack . show
