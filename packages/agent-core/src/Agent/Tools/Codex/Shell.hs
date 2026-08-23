-- | Managed shell processes for the OpenAI/Codex tool surface.
--
-- Each command still starts in a fresh shell. Commands that outlive their
-- initial yield can be retained under a numeric session id and later polled or
-- written to with @write_stdin@.
module Agent.Tools.Codex.Shell
    ( CodexShellSession
    , CodexShellResult(..)
    , newCodexShellSession
    , closeCodexShellSession
    , startCodexShellCommand
    , continueCodexShellCommand
    ) where

import Agent.Cancel (waitCancel)
import Agent.Tools.IO
    ( CommandResult(..)
    , RunningCommand(..)
    , RunningOutputCursor
    , initialRunningOutputCursor
    , interruptShellCommand
    , runningLiveOutput
    , runningOutputSince
    , startShellCommandWithInput
    , stopShellCommand
    , writeShellCommandInput
    )
import Agent.Tools.Types (ToolEnv(..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (mapConcurrently_, race, withAsync)
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar
    , newMVar
    , readMVar
    , tryReadMVar
    , withMVar
    )
import Control.Exception.Safe (SomeException, mask, onException, try)
import Control.Monad (void, when)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
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
    { managedRunning :: !RunningCommand
    , managedLock :: !(MVar ())
    , managedCursor :: !(IORef RunningOutputCursor)
    }

data SessionStore = SessionStore
    { storeCommands :: !(Map Int ManagedCommand)
    }

data CodexShellSession = CodexShellSession
    { sessionEnv :: !ToolEnv
    , sessionCommands :: !(MVar (Maybe SessionStore))
    , sessionNextId :: !(IORef Int)
    }

maxManagedCommands :: Int
maxManagedCommands = 64

newCodexShellSession :: ToolEnv -> IO CodexShellSession
newCodexShellSession env = do
    commands <- newMVar $ Just SessionStore
        { storeCommands = Map.empty }
    nextId <- newIORef 0
    pure CodexShellSession
        { sessionEnv = env
        , sessionCommands = commands
        , sessionNextId = nextId
        }

-- | Stop and join every retained command. Closing is idempotent.
closeCodexShellSession :: CodexShellSession -> IO ()
closeCodexShellSession session = do
    commands <- modifyMVar session.sessionCommands \case
        Nothing -> pure (Nothing, [])
        Just current -> pure (Nothing, Map.elems current.storeCommands)
    mapConcurrently_
        (\task ->
            void $ try @_ @SomeException $
                stopShellCommand task.managedRunning)
        commands

-- | Start a fresh shell command and wait up to the requested yield. If the
-- command is still alive it remains owned by the session and is returned under
-- a numeric id.
startCodexShellCommand
    :: CodexShellSession
    -> OsPath
    -> String
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
                        stopShellCommand task.managedRunning

continueCodexShellCommand
    :: CodexShellSession
    -> Int
    -> Text
    -> Int
    -> IO (Either Text CodexShellResult)
continueCodexShellCommand session commandId input yieldMs = do
    lookupCommand session commandId >>= \case
        Nothing -> pure (Left (unknownSession commandId))
        Just task ->
            withMVar task.managedLock \() -> do
                current <- lookupCommand session commandId
                case current of
                    Nothing -> pure (Left (unknownSession commandId))
                    Just _ -> do
                        tryReadMVar task.managedRunning.runningResult >>= \case
                            Just result ->
                                finishCommand session commandId task result
                            Nothing -> do
                                inputResult <-
                                    if Text.null input
                                        then pure (Right ())
                                        else if input == "\ETX"
                                            then interruptShellCommand task.managedRunning
                                                >> pure (Right ())
                                            else writeShellCommandInput
                                                task.managedRunning
                                                input
                                case inputResult of
                                    Right () ->
                                        waitForContinuation
                                            session commandId task yieldMs
                                    Left err ->
                                        completedAfterInputFailure
                                            task
                                            yieldMs >>= \case
                                                Just result ->
                                                    finishCommand
                                                        session
                                                        commandId
                                                        task
                                                        result
                                                Nothing -> pure (Left err)

-- A child may close stdin just before the supervisor publishes its completed
-- result. Give that handoff a short grace period so a late write returns the
-- command's final output rather than a transient broken-pipe error.
completedAfterInputFailure
    :: ManagedCommand
    -> Int
    -> IO (Maybe CommandResult)
completedAfterInputFailure task yieldMs =
    race
        (threadDelay (graceMs * 1000))
        (readMVar task.managedRunning.runningResult) >>= \case
            Left () -> pure Nothing
            Right result -> pure (Just result)
  where
    graceMs = max 100 (min 1000 yieldMs)

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
            Left () ->
                removeCommand session commandId
                    >> stopShellCommand task.managedRunning
                    >> pure (Left "Error: Command cancelled")
            Right (Left ()) ->
                runningResult commandId task
            Right (Right result) ->
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
                "Error: Poll cancelled; session "
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
takeRunningOutput task = do
    cursor <- readIORef task.managedCursor
    (output, nextCursor) <- runningOutputSince task.managedRunning cursor
    writeIORef task.managedCursor nextCursor
    pure output

nextCommandId :: CodexShellSession -> IO Int
nextCommandId session =
    atomicModifyIORef' session.sessionNextId \current ->
        let next = current + 1
        in (next, next)

startManagedCommand
    :: CodexShellSession
    -> OsPath
    -> String
    -> IO (Either Text (Int, ManagedCommand))
startManagedCommand session workdir command =
    modifyMVar session.sessionCommands \case
        Nothing ->
            pure
                ( Nothing
                , Left "Cannot start command: managed shell session is closed."
                )
        Just store
            | Map.size store.storeCommands >= maxManagedCommands ->
                pure
                    ( Just store
                    , Left "Cannot start command: managed shell session is full."
                    )
            | otherwise ->
                startShellCommandWithInput
                    session.sessionEnv
                    workdir
                    command >>= \case
                        Left err -> pure (Just store, Left err)
                        Right running -> do
                            prepared <-
                                (do
                                    commandId <- nextCommandId session
                                    task <- ManagedCommand running
                                        <$> newMVar ()
                                        <*> newIORef initialRunningOutputCursor
                                    pure (commandId, task))
                                    `onException` stopShellCommand running
                            let (commandId, task) = prepared
                            pure
                                ( Just store
                                    { storeCommands =
                                        Map.insert
                                            commandId
                                            task
                                            store.storeCommands
                                    }
                                , Right (commandId, task)
                                )

lookupCommand :: CodexShellSession -> Int -> IO (Maybe ManagedCommand)
lookupCommand session commandId =
    withMVar session.sessionCommands \case
        Nothing -> pure Nothing
        Just store -> pure (Map.lookup commandId store.storeCommands)

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
