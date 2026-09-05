-- | Managed shell processes for the OpenAI/Codex tool surface.
--
-- Each command still starts in a fresh shell. Commands that outlive their
-- initial yield are retained under a numeric session id, report completion
-- automatically, and may still receive input through @write_stdin@.
module Agent.Codex.Dialect.Shell
    ( CodexShellSession
    , CodexShellResult(..)
    , newCodexShellSession
    , resetCodexShellSession
    , closeCodexShellSession
    , startCodexShellCommand
    , continueCodexShellCommand
    ) where

import Agent.Cancel (waitCancel)
import Agent.Tools.Background
    ( CompletionGate
    , consumeCompletion
    , dismissBackgroundTaskNotice
    , newCompletionGate
    , publishBackgroundTaskNotice
    , publishCompletion
    , suppressCompletion
    , systemReminder
    )
import Agent.Tools.IO
    ( CommandResult(..)
    , RunningCommand(..)
    , RunningOutputCursor
    , formatCommandResult
    , initialRunningOutputCursor
    , interruptShellCommand
    , runningLiveOutput
    , runningOutputSince
    , startShellCommandWithInputAndCompletion
    , stopShellCommand
    , writeShellCommandInput
    )
import Agent.Tools.Types
    ( BackgroundTaskNotice(..)
    , ToolEnv(..)
    )
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (mapConcurrently_, race, withAsync)
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar
    , newEmptyMVar
    , newMVar
    , putMVar
    , readMVar
    , tryReadMVar
    , tryPutMVar
    , withMVar
    )
import Control.Exception.Safe (SomeException, mask, onException, try)
import Control.Monad (forM, void, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT, throwE)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import System.OsPath (OsPath)

data CodexShellResult
    = CodexShellFinished !CommandResult
    | CodexShellRunning
        { codexShellSessionId :: !Int
        , codexShellStdout :: !Text
        , codexShellStderr :: !Text
        }

data ManagedCommand = ManagedCommand
    { managedId :: !Int
    , managedRunning :: !RunningCommand
    , managedLock :: !(MVar ())
    , managedCursor :: !(MVar RunningOutputCursor)
    , managedCompletion :: !CompletionGate
    , managedYielded :: !(MVar Bool)
    }

data SessionStore = SessionStore
    { storeNextId :: !Int
    , storeCommands :: !(Map Int ManagedCommand)
    }

data CodexShellSession = CodexShellSession
    { sessionEnv :: !ToolEnv
    , sessionCommands :: !(MVar (Maybe SessionStore))
    }

maxManagedCommands :: Int
maxManagedCommands = 64

maxRetainedCompletedCommands :: Int
maxRetainedCompletedCommands = 64

newCodexShellSession :: ToolEnv -> IO CodexShellSession
newCodexShellSession env = do
    commands <- newMVar $ Just SessionStore
        { storeNextId = 0
        , storeCommands = Map.empty
        }
    pure CodexShellSession
        { sessionEnv = env
        , sessionCommands = commands
        }

-- | Stop and join every retained command. Closing is idempotent.
closeCodexShellSession :: CodexShellSession -> IO ()
closeCodexShellSession session = do
    commands <- modifyMVar session.sessionCommands \case
        Nothing -> pure (Nothing, [])
        Just current -> pure (Nothing, Map.elems current.storeCommands)
    stopManagedCommands session commands

-- | Stop and forget retained commands while keeping the shell session open.
-- Preserve the id counter so a stale id from the previous conversation can
-- never alias a newly started command.
resetCodexShellSession :: CodexShellSession -> IO ()
resetCodexShellSession session = do
    commands <- modifyMVar session.sessionCommands \case
        Nothing -> pure (Nothing, [])
        Just current ->
            pure
                ( Just current { storeCommands = Map.empty }
                , Map.elems current.storeCommands
                )
    stopManagedCommands session commands

stopManagedCommands :: CodexShellSession -> [ManagedCommand] -> IO ()
stopManagedCommands session commands =
    mapConcurrently_
        (\task ->
            void $ try @_ @SomeException $
                stopManagedCommand session task)
        commands

-- | Start a fresh shell command and wait up to the requested yield. If the
-- command is still alive it remains owned by the session and is returned under
-- a numeric id.
startCodexShellCommand
    :: CodexShellSession
    -> OsPath
    -> Text
    -> Int
    -> (Text -> Text -> IO ())
    -> IO (Either Text CodexShellResult)
startCodexShellCommand session workdir command yieldMs onSnapshot =
    mask \restore -> do
        startManagedCommand session workdir command >>= \case
            Left err -> pure (Left err)
            Right (commandId, task) ->
                restore
                    (waitForInitialYield
                        session commandId task yieldMs onSnapshot)
                    `onException` do
                        removeCommand session commandId
                        stopManagedCommand session task

continueCodexShellCommand
    :: CodexShellSession
    -> Int
    -> Text
    -> Int
    -> IO (Either Text CodexShellResult)
continueCodexShellCommand session commandId input yieldMs =
    runExceptT do
        task <- lookupManagedCommand session commandId
        ExceptT $
            withMVar task.managedLock \() ->
                runExceptT $
                    continueLocked session commandId task input yieldMs

continueLocked
    :: CodexShellSession
    -> Int
    -> ManagedCommand
    -> Text
    -> Int
    -> ExceptT Text IO CodexShellResult
continueLocked session commandId task input yieldMs = do
    -- Re-check after taking the command-specific lock: reset/close may have
    -- removed this command between the initial lookup and lock acquisition.
    void $ lookupManagedCommand session commandId
    lift (tryReadMVar task.managedRunning.runningResult) >>= \case
        Just result ->
            ExceptT $ finishCommand session commandId task result
        Nothing -> do
            inputResult <- lift $ runExceptT $ writeContinuationInput task input
            case inputResult of
                Right () ->
                    ExceptT $ waitForContinuation session commandId task yieldMs
                -- A process can exit between the result check and its stdin
                -- write. Return its completed result when available.
                Left err ->
                    lift (tryReadMVar task.managedRunning.runningResult)
                        >>= \case
                            Just result ->
                                ExceptT $ finishCommand session commandId task result
                            Nothing -> throwE err

writeContinuationInput :: ManagedCommand -> Text -> ExceptT Text IO ()
writeContinuationInput task input
    | Text.null input = pure ()
    | input == "\ETX" =
        lift $ interruptShellCommand task.managedRunning
    | otherwise =
        ExceptT $ writeShellCommandInput task.managedRunning input

waitForInitialYield
    :: CodexShellSession
    -> Int
    -> ManagedCommand
    -> Int
    -> (Text -> Text -> IO ())
    -> IO (Either Text CodexShellResult)
waitForInitialYield session commandId task yieldMs onSnapshot =
    withAsync sampleSnapshots \_sampler -> do
        stopped <- race
            (waitCancel session.sessionEnv.toolCancel)
            (race
                (threadDelay (max 1 yieldMs * 1000))
                (readMVar task.managedRunning.runningResult))
        case stopped of
            Left () -> do
                removeCommand session commandId
                stopManagedCommand session task
                result <- readMVar task.managedRunning.runningResult
                pure (Right (CodexShellFinished result { commandCancelled = True }))
            Right (Left ()) ->
                do
                    running <- runningResult commandId task
                    void $ tryPutMVar task.managedYielded True
                    pure running
            Right (Right result) -> do
                void $ tryPutMVar task.managedYielded False
                finishCommand session commandId task result
  where
    sampleSnapshots = go Nothing
    go previous = do
        threadDelay 100000
        snapshot <- runningLiveOutput task.managedRunning
        when (Just snapshot /= previous && snapshot /= ("", "")) $
            uncurry onSnapshot snapshot
        go (Just snapshot)

waitForContinuation
    :: CodexShellSession
    -> Int
    -> ManagedCommand
    -> Int
    -> IO (Either Text CodexShellResult)
waitForContinuation session commandId task yieldMs = do
    stopped <- race
        (waitCancel session.sessionEnv.toolCancel)
        (race
            (threadDelay (max 1 yieldMs * 1000))
            (readMVar task.managedRunning.runningResult))
    case stopped of
        Left () ->
            pure $ Left $
                "Error: Wait cancelled; session "
                    <> Text.pack (show commandId)
                    <> " is still running"
        Right (Left ()) ->
            runningResult commandId task
        Right (Right result) ->
            finishCommand session commandId task result

finishCommand
    :: CodexShellSession
    -> Int
    -> ManagedCommand
    -> CommandResult
    -> IO (Either Text CodexShellResult)
finishCommand session commandId task result = do
    (out, err) <- takeRunningOutput task
    consumeManagedCompletion session task
    removeCommand session commandId
    stopShellCommand task.managedRunning
    pure $ Right $ CodexShellFinished result
        { commandStdout = out
        , commandStderr = err
        }

runningResult :: Int -> ManagedCommand -> IO (Either Text CodexShellResult)
runningResult commandId task = do
    (out, err) <- takeRunningOutput task
    pure $ Right CodexShellRunning
        { codexShellSessionId = commandId
        , codexShellStdout = out
        , codexShellStderr = err
        }

takeRunningOutput :: ManagedCommand -> IO (Text, Text)
takeRunningOutput task =
  modifyMVar task.managedCursor \cursor -> do
    (output, nextCursor) <- runningOutputSince task.managedRunning cursor
    pure (nextCursor, output)

startManagedCommand
    :: CodexShellSession
    -> OsPath
    -> Text
    -> IO (Either Text (Int, ManagedCommand))
startManagedCommand session workdir command =
    do
        (stale, result) <-
            modifyMVar session.sessionCommands \case
                Nothing ->
                    pure
                        ( Nothing
                        , ( []
                          , Left
                                "Cannot start command: managed shell session is closed."
                          )
                        )
                Just current -> do
                    (store, completedToRelease, activeCount) <-
                        compactSessionStore current
                    if activeCount >= maxManagedCommands
                        then
                            pure
                                ( Just store
                                , ( completedToRelease
                                  , Left
                                        "Cannot start command: managed shell session is full."
                                  )
                                )
                        else do
                            let commandId = store.storeNextId + 1
                                key = codexCompletionKey commandId
                            completion <- newCompletionGate
                            cursor <- newMVar initialRunningOutputCursor
                            yielded <- newEmptyMVar
                            runningVar <- newEmptyMVar
                            let publish result = do
                                    didYield <- readMVar yielded
                                    when didYield $
                                      publishCompletion completion do
                                        running <- readMVar runningVar
                                        cursorAtYield <- readMVar cursor
                                        ((out, err), _) <-
                                            runningOutputSince
                                                running
                                                cursorAtYield
                                        publishBackgroundTaskNotice
                                            session.sessionEnv
                                            (codexCompletionNotice
                                                commandId
                                                command
                                                result
                                                    { commandStdout = out
                                                    , commandStderr = err
                                                    })
                            startShellCommandWithInputAndCompletion
                                session.sessionEnv
                                workdir
                                command
                                publish >>= \case
                                Left err ->
                                    pure
                                        ( Just store
                                        , (completedToRelease, Left err)
                                        )
                                Right running -> do
                                    putMVar runningVar running
                                    prepared <-
                                        (do
                                            task <-
                                                ManagedCommand commandId running
                                                    <$> newMVar ()
                                                    <*> pure cursor
                                                    <*> pure completion
                                                    <*> pure yielded
                                            pure (commandId, task))
                                            `onException` do
                                                void $ tryPutMVar yielded False
                                                suppressCompletion completion $
                                                    dismissBackgroundTaskNotice
                                                        session.sessionEnv
                                                        key
                                                stopShellCommand running
                                    let (commandId, task) = prepared
                                    pure
                                        ( Just store
                                            { storeNextId = commandId
                                            , storeCommands =
                                                Map.insert
                                                    commandId
                                                    task
                                                    store.storeCommands
                                            }
                                        , ( completedToRelease
                                          , Right (commandId, task)
                                          )
                                        )
        mapM_
            (\task ->
                void $ try @_ @SomeException $
                    stopShellCommand task.managedRunning)
            stale
        pure result

-- | Completed commands remain queryable for a bounded window but do not
-- consume the live-process quota. Older completed entries are reaped when a
-- later command starts; their already-queued completion reminders remain.
compactSessionStore
    :: SessionStore
    -> IO (SessionStore, [ManagedCommand], Int)
compactSessionStore store = do
    classified <- forM (Map.toAscList store.storeCommands) \entry@(_, task) -> do
        completed <- maybe False (const True)
            <$> tryReadMVar task.managedRunning.runningResult
        pure (completed, entry)
    let running =
            [entry | (False, entry) <- classified]
        completed =
            [entry | (True, entry) <- classified]
        evictedCount =
            max 0 (length completed - maxRetainedCompletedCommands)
        (evicted, retainedCompleted) =
            splitAt evictedCount completed
        retained = Map.fromList (running <> retainedCompleted)
    pure
        ( store { storeCommands = retained }
        , map snd evicted
        , length running
        )

stopManagedCommand :: CodexShellSession -> ManagedCommand -> IO ()
stopManagedCommand session task = do
    void $ tryPutMVar task.managedYielded False
    suppressManagedCompletion session task
    stopShellCommand task.managedRunning

consumeManagedCompletion :: CodexShellSession -> ManagedCommand -> IO ()
consumeManagedCompletion session task =
    consumeCompletion task.managedCompletion $
        dismissBackgroundTaskNotice
            session.sessionEnv
            (codexCompletionKey task.managedId)

suppressManagedCompletion :: CodexShellSession -> ManagedCommand -> IO ()
suppressManagedCompletion session task =
    suppressCompletion task.managedCompletion $
        dismissBackgroundTaskNotice
            session.sessionEnv
            (codexCompletionKey task.managedId)

codexCompletionKey :: Int -> Text
codexCompletionKey commandId =
    "codex-shell:" <> Text.pack (show commandId)

codexCompletionNotice
    :: Int
    -> Text
    -> CommandResult
    -> BackgroundTaskNotice
codexCompletionNotice commandId command result =
    BackgroundTaskNotice
        { noticeKey = codexCompletionKey commandId
        , noticeBody = systemReminder $
            "Background shell session "
                <> Text.pack (show commandId)
                <> " completed.\n\
                \The result is delivered automatically; do not call \
                \write_stdin merely to poll this session.\n\
                \Command:\n"
                <> boundedCompletionText 2048 command
                <> "\nResult:\n"
                <> boundedCompletionText
                    (32 * 1024)
                    (formatCommandResult result)
        }

boundedCompletionText :: Int -> Text -> Text
boundedCompletionText limit text
    | Text.length text <= limit = text
    | otherwise =
        let marker = "\n[...truncated...]\n"
            side = max 0 ((limit - Text.length marker) `div` 2)
        in Text.take side text <> marker <> Text.takeEnd side text

lookupCommand :: CodexShellSession -> Int -> IO (Maybe ManagedCommand)
lookupCommand session commandId =
    withMVar session.sessionCommands \case
        Nothing -> pure Nothing
        Just store -> pure (Map.lookup commandId store.storeCommands)

lookupManagedCommand
    :: CodexShellSession
    -> Int
    -> ExceptT Text IO ManagedCommand
lookupManagedCommand session commandId =
    lift (lookupCommand session commandId) >>= \case
        Nothing -> throwE (unknownSession commandId)
        Just task -> pure task

removeCommand :: CodexShellSession -> Int -> IO ()
removeCommand session commandId =
    modifyMVar session.sessionCommands \case
        Nothing -> pure (Nothing, ())
        Just store ->
            pure
                ( Just store
                    { storeCommands = Map.delete commandId store.storeCommands }
                , ()
                )

unknownSession :: Int -> Text
unknownSession commandId =
    "Unknown session_id: " <> Text.pack (show commandId)
