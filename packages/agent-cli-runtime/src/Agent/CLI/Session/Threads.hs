-- | Filesystem-lock and text compatibility adapter for runtime-owned workers.
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
import Agent.CLI.SessionLock
    ( SessionWaitSnapshot(..)
    , sessionLockIsActive
    , sessionLockPath
    , sessionWaitSnapshot
    )
import Agent.Runtime.SessionOwner
import Control.Concurrent (threadDelay)
import Control.Exception.Safe (tryAny)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import System.OsPath (OsPath, unsafeEncodeUtf, (</>))

data SessionThreadManager = SessionThreadManager
    { threadManagerRoot :: !OsPath
    , threadManagerOwner :: !SessionOwner
    }

-- | Bound concurrent in-process background turns. This is an admission limit,
-- not a queue: rejected calls never execute later without another request.
newSessionThreadManager :: OsPath -> IO SessionThreadManager
newSessionThreadManager root = SessionThreadManager root <$> newSessionOwner 64

launchSessionThread
    :: SessionThreadManager
    -> Text
    -> IO (Either Text ())
    -> IO (Either Text Text)
launchSessionThread manager sessionId =
    launchSessionThreadNotifying manager sessionId (const (pure ()))

-- | The sink runs before terminal publication and must be nonblocking and
-- must not call back into this manager.
launchSessionThreadNotifying
    :: SessionThreadManager
    -> Text
    -> (Text -> IO ())
    -> IO (Either Text ())
    -> IO (Either Text Text)
launchSessionThreadNotifying manager sessionId notify action = do
    launched <- tryAny $
        submitSessionTurn manager.threadManagerOwner sessionId
            (notify . outcomeText)
            -- Preserve the legacy user-facing exception formatting.
            (tryAny action >>= pure . either (Left . formatException) id)
    pure $ case launched of
        Left err -> Left ("failed to start agent session: " <> formatException err)
        Right (Left OwnerClosed) -> Left "agent session manager is closed"
        Right (Left SessionBusy) -> Left ("session " <> sessionId <> " is already running")
        Right (Left OwnerAtCapacity) -> Left "agent session manager is at capacity"
        Right (Right ()) -> Right ("started session " <> sessionId)

sessionThreadStatus :: SessionThreadManager -> Text -> IO Text
sessionThreadStatus manager sessionId = do
    (_, snapshot) <- sessionOwnerSnapshot manager.threadManagerOwner
    locked <- sessionLockIsActive (sessionLockPath (sessionDirectory manager sessionId))
    pure $ if locked
        then "running"
        else case Map.lookup sessionId snapshot of
            Nothing -> "idle"
            Just SessionRunning -> "running"
            Just (SessionFinished outcome) -> outcomeText outcome

-- | The owner captures this generation, so a later retry cannot move a waiter
-- onto a different run. External sessions retain the activity-lock fallback.
prepareSessionThreadWait :: SessionThreadManager -> Text -> IO (IO Text)
prepareSessionThreadWait manager sessionId = do
    captured <- prepareSessionWait manager.threadManagerOwner sessionId
    case captured of
        Just (SessionRunning, wait) -> pure (outcomeText <$> wait)
        _ -> do
            snapshot <- sessionWaitSnapshot sessionDir
            locked <- sessionLockIsActive (sessionLockPath sessionDir)
            if snapshot.waitActivityActive
                then pure (waitActivityThenInbox snapshot.waitActivityGeneration)
                else if snapshot.waitInboxPending > 0 && locked
                    then pure waitUntilQuiet
                    else pure $ maybe (pure "idle") (fmap outcomeText . snd) captured
  where
    sessionDir = sessionDirectory manager sessionId
    waitActivityThenInbox generation = do
        _ <- waitExternal generation
        snapshot <- sessionWaitSnapshot sessionDir
        locked <- sessionLockIsActive (sessionLockPath sessionDir)
        if snapshot.waitInboxPending > 0 && locked
            then waitUntilQuiet
            else pure "idle"
    waitExternal generation = do
        snapshot <- sessionWaitSnapshot sessionDir
        if snapshot.waitActivityActive
            && (generation == snapshot.waitActivityGeneration || generation == Nothing)
            then threadDelay 100000 >> waitExternal snapshot.waitActivityGeneration
            else pure ("idle" :: Text)
    waitUntilQuiet = do
        snapshot <- sessionWaitSnapshot sessionDir
        locked <- sessionLockIsActive (sessionLockPath sessionDir)
        if snapshot.waitActivityActive
                || (snapshot.waitInboxPending > 0 && locked)
            then threadDelay 100000 >> waitUntilQuiet
            else pure ("idle" :: Text)

sessionDirectory :: SessionThreadManager -> Text -> OsPath
sessionDirectory manager sessionId =
    manager.threadManagerRoot </> unsafeEncodeUtf (Text.unpack sessionId)

outcomeText :: SessionOutcome -> Text
outcomeText SessionCompleted = "completed"
outcomeText (SessionFailed err) = "failed (" <> err <> ")"
outcomeText SessionCancelled = "cancelled"

closeSessionThreadManager :: SessionThreadManager -> IO ()
closeSessionThreadManager = closeSessionOwner . (.threadManagerOwner)
