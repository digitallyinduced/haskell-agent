{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Agent.GrokBuild.Dialect.Workflow
    ( workflowRunSnapshotsWith
    , workflowTool
    , newWorkflowRuntime
    )
import Agent.InterAgentMessage (interAgentMessagePayload)
import Agent.Loop
    ( LoopResult(..)
    , defaultLoopDispatch
    , emptyTokenUsage
    )
import Agent.Subagents
    ( closeSubagentRegistry
    , defaultSubagentConfig
    , newSubagentRegistry
    , SubagentId
    , SubagentStatus(..)
    )
import Agent.Subagents.TaskPath (taskPathRoot)
import Agent.ToolDispatch
    ( dispatchToolCall
    , functionToolCall
    )
import Agent.Tools.MultiAgents (MultiAgentContext(..))
import Agent.Tools.Types (AppTool(..))
import Control.Concurrent (newMVar, threadDelay, withMVar)
import Control.Exception.Safe (bracket)
import Control.Monad (forM_)
import Data.IORef (newIORef)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.OsPath (unsafeEncodeUtf)
import Text.Printf (printf)

main :: IO ()
main =
    bracket newRegistry closeSubagentRegistry \registry -> do
        specs <- newIORef Map.empty
        runtime <- newWorkflowRuntime
            (unsafeEncodeUtf "/tmp")
            (rootContext registry)
            specs
        forM_ [1 .. 32 :: Int] \n -> do
            _ <- dispatchToolCall
                defaultLoopDispatch
                [(workflowTool runtime).appToolHandler]
                (functionToolCall (Text.pack ("call-" <> show n))
                    "workflow"
                    "{\"name\":\"deep-research\",\"args\":\"benchmark\"}")
            pure ()
        serialLock <- newMVar ()
        measure "workflow-snapshots-serial" $
            workflowRunSnapshotsWith
                (\agentId ->
                    withMVar serialLock \_ -> delayedStatus agentId)
                runtime
        measure "workflow-snapshots-bounded-8" $
            workflowRunSnapshotsWith delayedStatus runtime
  where
    delayedStatus :: SubagentId -> IO SubagentStatus
    delayedStatus _ = threadDelay 20_000 >> pure Running

    measure :: String -> IO [a] -> IO ()
    measure label action = do
        started <- getCurrentTime
        snapshots <- action
        length snapshots `seq` pure ()
        finished <- getCurrentTime
        printf "%s,%.3f\n" label
            (realToFrac (diffUTCTime finished started) * (1000 :: Double))

    newRegistry =
        newSubagentRegistry
            defaultSubagentConfig
            (unsafeEncodeUtf "/tmp")
            (\_ _ prompt _ -> pure (Right LoopResult
                { finalResponseId = "response"
                , finalText = Just ("done:" <> interAgentMessagePayload prompt)
                , turnsUsed = 1
                , tokenUsage = emptyTokenUsage
                }))
            (\_ _ -> pure ())

    rootContext registry = MultiAgentContext
        { multiRegistry = registry
        , multiCwd = unsafeEncodeUtf "/tmp"
        , multiSelfId = Nothing
        , multiDepth = 0
        , multiTaskPath = taskPathRoot
        , multiRootTurnId = pure Nothing
        , multiResumeFromDisk = Nothing
        , multiCreateWorktree = Nothing
        , multiPrepareSpawn = Nothing
        , multiSendToRoot = Nothing
        , multiSpawnModelGuidance = Nothing
        , multiAllowedChildModels = Nothing
        }
