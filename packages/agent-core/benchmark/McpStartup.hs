module Main (main) where

import Agent.MCP
    ( McpServerConfig(..)
    , McpServerStatus(..)
    , closeMcpFleet
    , mcpFleetStatuses
    , mcpFleetTools
    , startMcpFleet
    )
import Control.Exception.Safe (bracket)
import Control.Monad (forM)
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.List (sort)
import qualified Data.Text as Text
import GHC.Clock (getMonotonicTimeNSec)
import System.CPUTime (getCPUTime)
import System.Directory (getTemporaryDirectory, removeFile)
import System.Environment (getArgs)
import System.Exit (die)
import System.IO (hClose, openTempFile)
import System.Mem (performGC)
import System.Posix.Files (setFileMode)
import Text.Printf (printf)

data Workload
    = Sequential
    | LegacyToolInspection
    | LifecycleStatusInspection

data Sample = Sample
    { wallMillis :: !Double
    , cpuMillis :: !Double
    }

main :: IO ()
main =
    getArgs >>= \case
        [workloadArg, serverCountArg, delayMillisArg, sampleCountArg] -> do
            workload <- parseWorkload workloadArg
            serverCount <- parsePositive "server count" serverCountArg
            delayMillis <- parsePositive "delay milliseconds" delayMillisArg
            sampleCount <- parsePositive "sample count" sampleCountArg
            withFakeServer \script -> do
                let configs =
                        [ fakeConfig script delayMillis index
                        | index <- [1 .. serverCount]
                        ]
                samples <- forM [1 .. sampleCount] \_ -> do
                    performGC
                    measure (runWorkload workload configs)
                let medianSample = median samples
                printf
                    "%s,%d,%d,%.3f,%.3f\n"
                    workloadArg
                    serverCount
                    delayMillis
                    medianSample.wallMillis
                    medianSample.cpuMillis
        _ ->
            die $
                "usage: mcp-startup-bench WORKLOAD SERVERS DELAY_MS SAMPLES\n"
                    <> "workloads: sequential, legacy-tools, lifecycle-status"

parseWorkload :: String -> IO Workload
parseWorkload = \case
    "sequential" -> pure Sequential
    "legacy-tools" -> pure LegacyToolInspection
    "lifecycle-status" -> pure LifecycleStatusInspection
    other -> die ("unknown workload: " <> other)

parsePositive :: String -> String -> IO Int
parsePositive label raw =
    case reads raw of
        [(value, "")]
            | value > 0 -> pure value
        _ -> die ("invalid " <> label <> ": " <> raw)

runWorkload :: Workload -> [McpServerConfig] -> IO Int
runWorkload workload configs = case workload of
    Sequential ->
        bracket
            (mapM (startMcpFleet . pure) configs)
            (mapM_ closeMcpFleet)
            (pure . sum . map (length . mcpFleetTools))
    -- Representative pre-refactor inspection: consumers derived readiness
    -- from the fully materialized registration list after fleet startup.
    LegacyToolInspection ->
        bracket
            (startMcpFleet configs)
            closeMcpFleet
            (pure . length . mcpFleetTools)
    -- Refactored equivalent: inspect the explicit lifecycle snapshot after
    -- the same fleet startup with the same server inputs.
    LifecycleStatusInspection ->
        bracket
            (startMcpFleet configs)
            closeMcpFleet
            (\fleet -> do
                statuses <- mcpFleetStatuses fleet
                pure (sum (map (.mcpStatusToolCount) statuses)))

measure :: IO Int -> IO Sample
measure action = do
    wallBefore <- getMonotonicTimeNSec
    cpuBefore <- getCPUTime
    checksum <- action
    checksum `seq` pure ()
    cpuAfter <- getCPUTime
    wallAfter <- getMonotonicTimeNSec
    pure Sample
        { wallMillis =
            fromIntegral (wallAfter - wallBefore) / 1000000
        , cpuMillis =
            fromIntegral (cpuAfter - cpuBefore) / 1000000000
        }

median :: [Sample] -> Sample
median samples =
    Sample
        { wallMillis = middle (sort (map (.wallMillis) samples))
        , cpuMillis = middle (sort (map (.cpuMillis) samples))
        }

middle :: [a] -> a
middle values = values !! (length values `div` 2)

fakeConfig :: FilePath -> Int -> Int -> McpServerConfig
fakeConfig script delayMillis index = McpServerConfig
    { mcpServerName = "fake-" <> Text.pack (show index)
    , mcpServerCommand = script
    , mcpServerArgs = [show (fromIntegral delayMillis / 1000 :: Double)]
    , mcpServerCwd = Nothing
    , mcpServerEnv = []
    , mcpServerStartupTimeoutSeconds = 30
    , mcpServerRequestTimeoutSeconds = 30
    }

withFakeServer :: (FilePath -> IO a) -> IO a
withFakeServer action = do
    temporary <- getTemporaryDirectory
    bracket
        (do
            (path, handle) <- openTempFile temporary "agent-mcp-startup.sh"
            LBS8.hPutStr handle fakeServer
            hClose handle
            setFileMode path 0o700
            pure path)
        removeFile
        action

fakeServer :: LBS8.ByteString
fakeServer =
    "#!/bin/sh\n\
    \delay=\"$1\"\n\
    \while IFS= read -r line; do\n\
    \  case \"$line\" in\n\
    \    *'\"method\":\"initialize\"'*)\n\
    \      sleep \"$delay\"\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"protocolVersion\":\"2025-11-25\",\"capabilities\":{},\"serverInfo\":{\"name\":\"fake\",\"version\":\"1\"}}}'\n\
    \      ;;\n\
    \    *'\"method\":\"notifications/initialized\"'*) ;;\n\
    \    *'\"method\":\"tools/list\"'*)\n\
    \      printf '%s\\n' '{\"jsonrpc\":\"2.0\",\"id\":2,\"result\":{\"tools\":[]}}'\n\
    \      ;;\n\
    \  esac\n\
    \done\n"
