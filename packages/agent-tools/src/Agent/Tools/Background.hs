-- | Exactly-once delivery for managed background task completions.
--
-- Dialect runtimes publish a model-facing reminder when a managed process
-- finishes. A later explicit result read may race that publication; the gate
-- serializes publication and dismissal before the next provider submission,
-- so the pending input contains either the reminder or the explicit result.
module Agent.Tools.Background
    ( CompletionGate
    , BackgroundTaskStatus(..)
    , consumeCompletion
    , dismissBackgroundTaskNotice
    , newCompletionGate
    , publishBackgroundTaskNotice
    , publishCompletion
    , readBackgroundTasks
    , registerBackgroundTask
    , removeBackgroundTask
    , setBackgroundTaskHooks
    , suppressCompletion
    , systemReminder
    ) where

import Agent.Tools.Types
    ( BackgroundTaskHooks(..)
    , BackgroundTaskNotice
    , BackgroundTaskStatus(..)
    , ToolEnv(..)
    )
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Control.Exception.Safe (tryAny)
import Control.Monad (void)
import Data.IORef (atomicModifyIORef', readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (getCurrentTime)

-- | Snapshot only: never consumes task output or completion notifications.
readBackgroundTasks :: ToolEnv -> IO [BackgroundTaskStatus]
readBackgroundTasks env =
    sortOn (\task -> (task.taskStartedAt, task.taskKey)) . Map.elems
        <$> readIORef env.toolBackgroundTasks

-- | Register before starting the worker, so an immediate completion cannot
-- remove its entry before insertion. Repeated registration preserves its age.
registerBackgroundTask :: ToolEnv -> Text -> Text -> Bool -> IO ()
registerBackgroundTask env key label autoResume = do
    startedAt <- getCurrentTime
    let status = BackgroundTaskStatus key label startedAt autoResume
    atomicModifyIORef' env.toolBackgroundTasks \tasks ->
        (Map.insertWith (\_ existing -> existing) key status tasks, ())

-- | Idempotent removal on completion, failed start, cancellation, or shutdown.
removeBackgroundTask :: ToolEnv -> Text -> IO ()
removeBackgroundTask env key =
    atomicModifyIORef' env.toolBackgroundTasks \tasks ->
        (Map.delete key tasks, ())

setBackgroundTaskHooks :: ToolEnv -> BackgroundTaskHooks -> IO ()
setBackgroundTaskHooks env = writeIORef env.toolBackgroundTaskHooks

publishBackgroundTaskNotice
    :: ToolEnv
    -> BackgroundTaskNotice
    -> IO Bool
publishBackgroundTaskNotice env notice = do
    hooks <- readIORef env.toolBackgroundTaskHooks
    tryAny (hooks.backgroundTaskCompleted notice)
        >>= either (const (pure False)) pure

dismissBackgroundTaskNotice :: ToolEnv -> Text -> IO ()
dismissBackgroundTaskNotice env key = do
    hooks <- readIORef env.toolBackgroundTaskHooks
    void $ tryAny (hooks.backgroundTaskDismissed key)

data CompletionState
    = CompletionPending
    | CompletionPublished
    | CompletionConsumed
    | CompletionSuppressed

newtype CompletionGate = CompletionGate (MVar CompletionState)

newCompletionGate :: IO CompletionGate
newCompletionGate = CompletionGate <$> newMVar CompletionPending

-- | Publish once. The action reports whether it actually enqueued a notice
-- and runs while the gate is held so a racing explicit consumer cannot
-- dismiss the key before it has actually been enqueued.
publishCompletion :: CompletionGate -> IO Bool -> IO ()
publishCompletion (CompletionGate stateVar) publish =
    modifyMVar stateVar \case
        CompletionPending -> do
            published <- publish
            pure
                ( if published
                    then CompletionPublished
                    else CompletionSuppressed
                , ()
                )
        state -> pure (state, ())

-- | Mark an explicit result as authoritative. If a completion reminder won
-- the race, remove it before the next model submission.
consumeCompletion :: CompletionGate -> IO () -> IO ()
consumeCompletion (CompletionGate stateVar) dismiss =
    modifyMVar stateVar \case
        CompletionPublished ->
            dismiss >> pure (CompletionConsumed, ())
        CompletionPending ->
            pure (CompletionConsumed, ())
        state -> pure (state, ())

-- | Disable a completion during reset, close, or explicit kill.
suppressCompletion :: CompletionGate -> IO () -> IO ()
suppressCompletion (CompletionGate stateVar) dismiss =
    modifyMVar stateVar \case
        CompletionPublished ->
            dismiss >> pure (CompletionSuppressed, ())
        CompletionPending ->
            pure (CompletionSuppressed, ())
        state -> pure (state, ())

systemReminder :: Text -> Text
systemReminder body =
    "<system-reminder>\n"
        <> neutralizeReminderTags body
        <> "\n</system-reminder>"

neutralizeReminderTags :: Text -> Text
neutralizeReminderTags =
    Text.replace "<system-reminder" "&lt;system-reminder"
        . Text.replace "</system-reminder" "&lt;/system-reminder"
        . Text.replace "<system_reminder" "&lt;system_reminder"
        . Text.replace "</system_reminder" "&lt;/system_reminder"
