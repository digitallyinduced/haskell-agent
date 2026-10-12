{-# LANGUAGE OverloadedStrings, OverloadedRecordDot, NamedFieldPuns, BlockArguments, RankNTypes #-}
-- | Low-level lifecycle operations for deterministic runtime tests. Hosts
-- should use the scoped interface in "Agent.Subagents.Runtime".
module Agent.Subagents.Runtime.Internal
    ( SubagentRuntime
    , RuntimeHandle (..)
    , RuntimeHooks (..)
    , newSubagentRuntime
    , withSubagentRuntimeInterval
    , withSubagentRuntimeHandle
    , closeSubagentRuntime
    , tickSubagentRuntime
    , tickSubagentRuntimeAt
    ) where

import Agent.Subagents
import Agent.Loop (LoopError (LoopNoResponseId))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel, poll, waitCatch, withAsync, link)
import Control.Concurrent.MVar
import qualified Control.Exception.Safe as Exception
import Control.Monad (forever, forM_, void, when)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)
import System.OsPath (OsPath)

data RuntimeHandle = RuntimeHandle
    { registry :: !SubagentRegistry
    , root :: !RootSubagents
    , withOperation :: forall a. IO a -> IO a
    -- ^ The operation guard captured when this tree acquired ownership.
    -- Hosts must use this guard, not callbacks prepared for a later root turn.
    }

-- | Callbacks own their resources for the full tree lifetime. They must not
-- capture a database connection or credentials scoped to an individual turn.
data RuntimeHooks = RuntimeHooks
    { withOwnership :: forall a. IO a -> IO (Maybe a)
    , withOperation :: forall a. IO a -> IO a
    -- ^ Reserve ownership for an operation, including all its external writes.
    -- The host defines failure/quarantine semantics; this is not a transaction.
    , initialize :: RuntimeHandle -> IO ()
    -- ^ Install runner/persistence callbacks and recover durable children.
    , pumpRunnable :: RuntimeHandle -> IO Bool
    -- ^ Process durable handoffs; False retires an explicitly cancelled tree.
    }

data Entry = Entry
    { owner :: !(Async ())
    , ready :: !(MVar (Either Exception.SomeException (Maybe RuntimeHandle)))
    , stop :: !(MVar ())
    , pump :: !(RuntimeHandle -> IO Bool)
    , borrowers :: !Int
    , lastActive :: !UTCTime
    }

data RuntimeState = RuntimeState
    { closed :: !Bool
    , entries :: !(Map Text Entry)
    }

data SubagentRuntime = SubagentRuntime
    { state :: !(MVar RuntimeState)
    , config :: !SubagentConfig
    , cwd :: !OsPath
    }

newSubagentRuntime :: SubagentConfig -> OsPath -> IO SubagentRuntime
newSubagentRuntime config cwd = do
    state <- newMVar RuntimeState { closed = False, entries = Map.empty }
    pure SubagentRuntime { state, config, cwd }

-- | Interval injection is internal, for fast tests of automatic scheduling.
withSubagentRuntimeInterval
    :: Int -> SubagentConfig -> OsPath
    -> (Text -> Exception.SomeException -> IO ())
    -> (SubagentRuntime -> IO a) -> IO a
withSubagentRuntimeInterval interval config cwd onFailure action =
    Exception.bracket (newSubagentRuntime config cwd) closeSubagentRuntime \runtime ->
        withAsync (forever (threadDelay interval >> tickSubagentRuntime runtime onFailure)) \scheduler -> do
            link scheduler
            action runtime

-- | Borrow a tree for an entire root turn. Returns Nothing when another
-- runtime owns it. Idle retirement cannot remove a borrowed tree.
withSubagentRuntimeHandle
    :: SubagentRuntime -> Text -> RuntimeHooks -> (RuntimeHandle -> IO a) -> IO (Maybe a)
withSubagentRuntimeHandle runtime key hooks action =
    Exception.bracket acquire release \entry -> do
        result <- readMVar entry.ready >>= either Exception.throwIO pure
        traverse action result
  where
    acquire = modifyMVar runtime.state \current -> do
        when current.closed (Exception.throwString "Subagent runtime is closed")
        existing <- case Map.lookup key current.entries of
            Nothing -> pure Nothing
            Just old -> poll old.owner >>= \finished ->
                pure case finished of
                    Nothing -> Just old
                    Just _ -> Nothing
        case existing of
            Just old -> do
                let borrowed = old { borrowers = old.borrowers + 1 }
                pure (current { entries = Map.insert key borrowed current.entries }, borrowed)
            Nothing -> do
                entry <- startEntry runtime.config runtime.cwd hooks
                let borrowed = entry { borrowers = 1 }
                pure (current { entries = Map.insert key borrowed current.entries }, borrowed)
    release entry = do
        now <- getCurrentTime
        modifyMVar_ runtime.state \current -> pure current
            { entries = Map.adjust (\latest ->
                if latest.ready == entry.ready
                    then latest { borrowers = latest.borrowers - 1, lastActive = now }
                    else latest) key current.entries
            }

-- Insertion runs under modifyMVar's acquisition protection. The runtime
-- retains every task handle and joins it before releasing external ownership.
startEntry :: SubagentConfig -> OsPath -> RuntimeHooks -> IO Entry
startEntry config cwd hooks@RuntimeHooks { withOwnership = own, withOperation = operation } = do
    lastActive <- getCurrentTime
    ready <- newEmptyMVar
    stop <- newEmptyMVar
    owner <- async $ (do
        result <- Exception.tryAny $
            own $
                Exception.bracket (newRuntimeHandle config cwd operation) shutdownRuntimeHandle \handle -> do
                    hooks.initialize handle
                    putMVar ready (Right (Just handle))
                    readMVar stop
        case result of
            Left failure -> void (tryPutMVar ready (Left failure))
            Right _ -> void (tryPutMVar ready (Right Nothing)))
        `Exception.finally` void (tryPutMVar ready (Right Nothing))
    pure Entry { owner, ready, stop, pump = hooks.pumpRunnable, borrowers = 0, lastActive }

newRuntimeHandle :: SubagentConfig -> OsPath -> (forall a. IO a -> IO a) -> IO RuntimeHandle
newRuntimeHandle config cwd operation = do
    registry <- newSubagentRegistry config cwd
        (\_ _ _ _ -> pure (Left LoopNoResponseId))
        (\_ _ -> pure ())
    root <- newRootSubagents NoticesToHost registry
        `Exception.onException` closeSubagentRegistry registry
    pure RuntimeHandle { registry, root, withOperation = operation }

-- Runtime teardown is not user cancellation: preserve the last durable
-- boundary rather than writing Closed/Interrupted for resumable children.
shutdownRuntimeHandle :: RuntimeHandle -> IO ()
shutdownRuntimeHandle handle = do
    setSubagentOnSettled handle.registry (\_ _ -> pure ())
    setSubagentOnComplete handle.registry (\_ _ -> pure ())
    stopSubagentRegistry handle.registry

tickSubagentRuntime :: SubagentRuntime -> (Text -> Exception.SomeException -> IO ()) -> IO ()
tickSubagentRuntime runtime onFailure =
    getCurrentTime >>= \now -> tickSubagentRuntimeAt runtime now onFailure

-- | Retire idle trees after one minute without cancelling durable sleepers.
tickSubagentRuntimeAt
    :: SubagentRuntime -> UTCTime -> (Text -> Exception.SomeException -> IO ()) -> IO ()
tickSubagentRuntimeAt runtime now onFailure = do
    snapshot <- readMVar runtime.state
    forM_ (Map.toList snapshot.entries) \(key, entry) ->
      Exception.handleAny (onFailure key) do
        available <- tryReadMVar entry.ready
        case available of
            Just (Right (Just handle)) -> do
                alive <- poll entry.owner
                case alive of
                    Nothing -> do
                        keep <- entry.pump handle
                        if keep
                            then void (resumeDueSubagents handle.registry now)
                            else do
                                void (tryPutMVar entry.stop ())
                                void (waitCatch entry.owner)
                        agents <- listAgents handle.registry Nothing
                        let active = any (\(_, _, status) -> status == Running || status == Pending) agents
                        modifyMVar_ runtime.state \current -> case Map.lookup key current.entries of
                            Just latest | latest.ready == entry.ready ->
                                if active || latest.borrowers > 0
                                    then pure current { entries = Map.insert key
                                        (latest { lastActive = now }) current.entries }
                                    else if diffUTCTime now latest.lastActive >= 60
                                        then do
                                            void (tryPutMVar latest.stop ())
                                            -- Exclude new acquisition until children
                                            -- join and ownership is released.
                                            void (waitCatch latest.owner)
                                            pure current { entries = Map.delete key current.entries }
                                        else pure current
                            _ -> pure current
                    Just _ -> forgetFinished key entry
            _ -> do
                finished <- poll entry.owner
                case finished of
                    Just _ -> forgetFinished key entry
                    Nothing -> pure ()
  where
    forgetFinished key entry = modifyMVar_ runtime.state \current ->
        pure current { entries = case Map.lookup key current.entries of
            Just latest | latest.ready == entry.ready -> Map.delete key current.entries
            _ -> current.entries
        }

closeSubagentRuntime :: SubagentRuntime -> IO ()
closeSubagentRuntime runtime = do
    owned <- modifyMVar runtime.state \current ->
        pure (RuntimeState True Map.empty, Map.elems current.entries)
    forM_ owned \entry -> void (tryPutMVar entry.stop ())
    -- Cancellation also covers initialization blocked in external acquisition.
    forM_ owned \entry -> cancel entry.owner
    forM_ owned \entry -> void (waitCatch entry.owner)
