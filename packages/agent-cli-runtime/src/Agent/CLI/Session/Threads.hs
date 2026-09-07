-- | Runtime-owned in-process session workers. The legacy namespace matches
-- the persistence and locking modules; this module has no frontend dependency.
module Agent.CLI.Session.Threads
    ( SessionThreadManager
    , newSessionThreadManager
    , launchSessionThread
    , launchSessionThreadNotifying
    , prepareSessionThreadWait
    , sessionThreadStatus
    , closeSessionThreadManager
    ) where

import Agent.CLI.Error (formatException)
import Agent.CLI.SessionLock (sessionLockIsActive, sessionLockPath, sessionActivitySnapshot)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
    ( Async, AsyncCancelled, asyncWithUnmask, cancel, poll, waitCatch )
import Control.Concurrent.MVar
    ( MVar, modifyMVar, modifyMVar_, newEmptyMVar, newMVar, putMVar, readMVar, takeMVar )
import Control.Exception.Safe (mask, tryAny, SomeException, fromException)
import Control.Monad (void)
import Data.Maybe (isJust)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import System.OsPath (OsPath, unsafeEncodeUtf, (</>))

data ManagedSessionThread
    = ManagedSessionThreadRunning !(Async Text)
    | ManagedSessionThreadCompleted
    | ManagedSessionThreadFailed !Text

data SessionThreadManagerState = SessionThreadManagerState
    { threadManagerClosed :: !Bool
    , managedThreads :: !(Map Text ManagedSessionThread)
    }

data SessionThreadManager = SessionThreadManager
    { threadManagerRoot :: !OsPath
    , threadManagerState :: !(MVar SessionThreadManagerState)
    }

newSessionThreadManager :: OsPath -> IO SessionThreadManager
newSessionThreadManager root = do
    state <- newMVar SessionThreadManagerState
        { threadManagerClosed = False
        , managedThreads = Map.empty
        }
    pure SessionThreadManager
        { threadManagerRoot = root
        , threadManagerState = state
        }

-- | Start one in-process background turn. The task is registered before it is
-- allowed to execute, so shutdown can always cancel and join every live turn.
launchSessionThread
    :: SessionThreadManager
    -> Text
    -> IO (Either Text ())
    -> IO (Either Text Text)
launchSessionThread manager sessionId action =
    launchSessionThreadNotifying manager sessionId (const (pure ())) action

-- | Enqueue one completion notification for this particular turn. The sink
-- runs in the tracked worker while the manager lock is held, immediately
-- before publishing the terminal state. It must be nonblocking and must not
-- call back into the manager. This makes notification delivery and readiness
-- for a follow-up atomic to callers, without an unowned notification thread.
-- Rejected launches and manager shutdown do not emit completion notices.
launchSessionThreadNotifying
    :: SessionThreadManager
    -> Text
    -> (Text -> IO ())
    -> IO (Either Text ())
    -> IO (Either Text Text)
launchSessionThreadNotifying manager sessionId notify action =
    mask \_ -> do
        launched <- modifyMVar manager.threadManagerState \state ->
            if state.threadManagerClosed
                then pure (state, Left "agent session manager is closed")
                else case Map.lookup sessionId state.managedThreads of
                    Just (ManagedSessionThreadRunning _) ->
                        pure
                            ( state
                            , Left
                                ("session " <> sessionId
                                    <> " is already running")
                            )
                    _ -> do
                        gate <- newEmptyMVar
                        started <- tryAny $
                            asyncWithUnmask \unmask -> do
                                takeMVar gate
                                result <- tryAny (unmask action)
                                let terminal = case result of
                                        Left err ->
                                            ManagedSessionThreadFailed
                                                (formatException err)
                                        Right (Left err) ->
                                            ManagedSessionThreadFailed err
                                        Right (Right ()) ->
                                            ManagedSessionThreadCompleted
                                let status = case terminal of
                                        ManagedSessionThreadFailed err -> "failed (" <> err <> ")"
                                        _ -> "completed"
                                modifyMVar_ manager.threadManagerState \current ->
                                    if current.threadManagerClosed
                                        then pure current
                                        else do
                                            -- A notification failure must not
                                            -- replace the child turn's outcome.
                                            void (tryAny (notify status))
                                            pure current
                                                { managedThreads =
                                                    Map.insert
                                                        sessionId
                                                        terminal
                                                        current.managedThreads
                                                }
                                pure status
                        case started of
                            Left err ->
                                pure
                                    ( state
                                    , Left
                                        ("failed to start agent session: "
                                            <> formatException err)
                                    )
                            Right worker -> do
                                let running = state
                                        { managedThreads =
                                            Map.insert
                                                sessionId
                                                (ManagedSessionThreadRunning worker)
                                                state.managedThreads
                                        }
                                putMVar gate ()
                                pure
                                    ( running
                                    , Right ("started session " <> sessionId)
                                    )
        pure launched

sessionThreadStatus :: SessionThreadManager -> Text -> IO Text
sessionThreadStatus manager sessionId =
    modifyMVar manager.threadManagerState \state ->
        case Map.lookup sessionId state.managedThreads of
            Nothing -> do
                locked <- lockIsActive
                pure (state, if locked then "running" else "idle")
            Just (ManagedSessionThreadRunning worker) ->
                poll worker >>= \case
                    Nothing -> pure (state, "running")
                    Just (Right status) ->
                        settle state
                            (if status == "completed"
                                then ManagedSessionThreadCompleted
                                else ManagedSessionThreadFailed status)
                            status
                    Just (Left err) ->
                        let message = "failed (" <> formatException err <> ")"
                        in settle state
                            (ManagedSessionThreadFailed message)
                            message
            Just ManagedSessionThreadCompleted ->
                terminalStatus state "completed"
            Just (ManagedSessionThreadFailed err) ->
                terminalStatus state ("failed (" <> err <> ")")
  where
    lockIsActive =
        sessionLockIsActive
            (sessionLockPath
                (manager.threadManagerRoot
                    </> unsafeEncodeUtf (Text.unpack sessionId)))
    -- Persist the terminal outcome instead of deleting it, so repeated status
    -- polls stay observable. The background worker itself records the same
    -- terminal constructor on exit (launchSessionThread); deleting it here
    -- destroyed that record, making a failed session report "idle" on the
    -- second poll (and never report its failure at all when a poll landed
    -- while the session lock was still held). A still-active lock only masks
    -- the outcome as "running" for this poll; the retained record surfaces the
    -- real status once the lock clears.
    settle state record terminal = do
        locked <- lockIsActive
        pure
            ( state
                { managedThreads =
                    Map.insert sessionId record state.managedThreads
                }
            , if locked then "running" else terminal
            )
    terminalStatus state terminal = do
        locked <- lockIsActive
        pure (state, if locked then "running" else terminal)

-- | Waiting on the captured Async (not repeated map lookups) prevents a quick
-- resume from moving the waiter onto a different run. The waiter never owns or
-- cancels the target. External interactive sessions use the turn activity lock,
-- not the lifetime lock, which remains held while the prompt is idle.
prepareSessionThreadWait :: SessionThreadManager -> Text -> IO (IO Text)
prepareSessionThreadWait manager sessionId = do
    state <- readMVar manager.threadManagerState
    case Map.lookup sessionId state.managedThreads of
        Just (ManagedSessionThreadRunning worker) ->
            pure $ either waitExceptionStatus id <$> waitCatch worker
        _ -> do
            (generation, active) <- sessionActivitySnapshot sessionDir
            if active
                then pure (waitExternal generation)
                else pure $ pure $ case Map.lookup sessionId state.managedThreads of
                    Just ManagedSessionThreadCompleted -> "completed"
                    Just (ManagedSessionThreadFailed err) -> "failed (" <> err <> ")"
                    _ -> "idle"
  where
    sessionDir = manager.threadManagerRoot </> unsafeEncodeUtf (Text.unpack sessionId)
    waitExternal generation = do
        (current, active) <- sessionActivitySnapshot sessionDir
        if active && (generation == current || generation == Nothing)
            then threadDelay 100000 >> waitExternal current
            else pure "idle"

waitExceptionStatus :: SomeException -> Text
waitExceptionStatus err
    | isJust (fromException err :: Maybe AsyncCancelled) = "cancelled"
    | otherwise = "failed (" <> formatException err <> ")"

closeSessionThreadManager :: SessionThreadManager -> IO ()
closeSessionThreadManager manager = do
    workers <- modifyMVar manager.threadManagerState \state ->
        let running =
                [ worker
                | ManagedSessionThreadRunning worker <-
                    Map.elems state.managedThreads
                ]
        in pure
            ( state
                { threadManagerClosed = True
                , managedThreads = Map.empty
                }
            , running
            )
    mapM_ cancel workers
    mapM_ (void . waitCatch) workers
