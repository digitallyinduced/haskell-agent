-- | Lifetime ownership for persisted sessions.
--
-- Lock files are permanent coordination points. The operating system releases
-- the advisory lock when the owning process exits, including after a crash.
module Agent.Runtime.SessionLock
    ( SessionLock
    , SessionWaitSnapshot(..)
    , acquireSessionLock
    , acquireSessionActivityLock
    , adjustSessionInboxPending
    , releaseSessionLock
    , sessionActivityLockPath
    , sessionActivityGeneration
    , sessionActivitySnapshot
    , sessionInboxPending
    , sessionLockFilePath
    , sessionLockIsActive
    , sessionLockPath
    , sessionWaitSnapshot
    ) where

import Agent.Runtime.Error (formatException)
import Agent.PrivateFileLock (withPrivateFileLock)
import Agent.FileRetry (writeLazyFileAtomically)
import Agent.OsPath (unsafeToFilePath)
import Control.Exception.Safe (SomeException, bracketOnError, mask_, try, tryAny)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Text (Text)
import qualified System.FileLock as FileLock
import qualified System.FilePath as FilePath
import System.OsPath (OsPath, unsafeEncodeUtf, (</>))
import System.IO.Error (isDoesNotExistError, catchIOError)
import Text.Read (readMaybe)

data SessionLock = SessionLock
    { lockFilePath :: !FilePath
    , sessionLockHandle :: !FileLock.FileLock
    }

-- | Cross-process view of work that waiters must drain: the current turn, if
-- any, plus accepted inbox messages that have not yet started as a turn.
data SessionWaitSnapshot = SessionWaitSnapshot
    { waitActivityGeneration :: !(Maybe Integer)
    , waitActivityActive :: !Bool
    , waitInboxPending :: !Integer
    }

sessionLockFilePath :: SessionLock -> FilePath
sessionLockFilePath lock = lock.lockFilePath

sessionLockPath :: OsPath -> FilePath
sessionLockPath sessionDir =
    unsafeToFilePath sessionDir FilePath.</> ".agent-running.lock"

sessionActivityLockPath :: OsPath -> FilePath
sessionActivityLockPath sessionDir =
    unsafeToFilePath sessionDir FilePath.</> ".agent-turn-running.lock"

acquireSessionLock :: OsPath -> Text -> IO (Either Text SessionLock)
acquireSessionLock sessionDir sessionId =
    acquireLockAt (sessionLockPath sessionDir) sessionId

acquireSessionActivityLock
    :: OsPath
    -> Text
    -> IO (Either Text SessionLock)
acquireSessionActivityLock sessionDir sessionId = do
    result <- tryAny $
        withPrivateFileLock (activityAdmissionPath sessionDir) $
            bracketOnError
                (acquireLockAt (sessionActivityLockPath sessionDir) sessionId)
                (either (const (pure ())) releaseSessionLock)
                \case
                    Left err -> pure (Left err)
                    Right lock -> do
                        generation <- sessionActivityGeneration sessionDir
                        writeLazyFileAtomically (activityGenerationPath sessionDir) 0o600
                            (LBS.pack (show (maybe 1 (+ 1) generation)))
                        pure (Right lock)
    pure $ case result of
        Left err -> Left
            ("failed to mark session activity " <> sessionId <> ": "
                <> formatException err)
        Right acquired -> acquired

-- | Observe activity and its generation under the same short-lived admission
-- gate used by writers. In particular, never observe an acquired activity lock
-- before that turn's generation has been published. Releasing an activity lock
-- needs no gate: it cannot change the generation, and any following acquisition
-- must wait until this snapshot has completed.
sessionActivitySnapshot :: OsPath -> IO (Maybe Integer, Bool)
sessionActivitySnapshot sessionDir = do
    snapshot <- sessionWaitSnapshot sessionDir
    pure (snapshot.waitActivityGeneration, snapshot.waitActivityActive)

sessionWaitSnapshot :: OsPath -> IO SessionWaitSnapshot
sessionWaitSnapshot sessionDir =
    withPrivateFileLock (activityAdmissionPath sessionDir) $ do
        generation <- sessionActivityGeneration sessionDir
        active <- sessionLockIsActive (sessionActivityLockPath sessionDir)
        pending <- readSessionInboxPending sessionDir
        pure SessionWaitSnapshot
            { waitActivityGeneration = generation
            , waitActivityActive = active
            , waitInboxPending = pending
            }

-- | Accepted owner-inbox messages that have not yet started as a turn.
sessionInboxPending :: OsPath -> IO Integer
sessionInboxPending sessionDir =
    withPrivateFileLock (activityAdmissionPath sessionDir) $
        readSessionInboxPending sessionDir

adjustSessionInboxPending :: OsPath -> Integer -> IO Integer
adjustSessionInboxPending sessionDir delta =
    withPrivateFileLock (activityAdmissionPath sessionDir) $ do
        current <- readSessionInboxPending sessionDir
        let next = max 0 (current + delta)
        writeLazyFileAtomically
            (sessionInboxPendingPath sessionDir)
            0o600
            (LBS.pack (show next))
        pure next

readSessionInboxPending :: OsPath -> IO Integer
readSessionInboxPending sessionDir =
    (do
        bytes <- BS.readFile
            (unsafeToFilePath (sessionInboxPendingPath sessionDir))
        case readMaybe (BS.unpack bytes) of
            Just value | value >= 0 -> pure value
            _ -> ioError (userError "invalid session inbox pending count"))
        `catchIOError` \err ->
            if isDoesNotExistError err then pure 0 else ioError err

sessionInboxPendingPath :: OsPath -> OsPath
sessionInboxPendingPath sessionDir =
    sessionDir </> unsafeEncodeUtf ".agent-inbox-pending"

-- A monotonically increasing generation distinguishes rapid consecutive turns
-- even if a cross-process waiter never observes the unlocked interval. The
-- activity lock serializes writers; atomic replacement prevents partial reads.
sessionActivityGeneration :: OsPath -> IO (Maybe Integer)
sessionActivityGeneration sessionDir =
    (do
        bytes <- BS.readFile (unsafeToFilePath (activityGenerationPath sessionDir))
        case readMaybe (BS.unpack bytes) of
            Just value | value > 0 -> pure (Just value)
            _ -> ioError (userError "invalid session activity generation"))
        `catchIOError` \err ->
            if isDoesNotExistError err then pure Nothing else ioError err

activityGenerationPath :: OsPath -> OsPath
activityGenerationPath sessionDir =
    sessionDir </> unsafeEncodeUtf ".agent-turn-generation"

activityAdmissionPath :: OsPath -> OsPath
activityAdmissionPath sessionDir =
    sessionDir </> unsafeEncodeUtf ".agent-turn-admission.lock"

acquireLockAt :: FilePath -> Text -> IO (Either Text SessionLock)
acquireLockAt path sessionId = mask_ do
    -- Do not expose a successfully acquired lock to async interruption before
    -- it has been wrapped in the value whose owner will release it.
    try @_ @SomeException
        (FileLock.tryLockFile path FileLock.Exclusive) >>= \case
            Left err -> pure $ Left
                ("failed to lock session " <> sessionId <> ": "
                    <> formatException err)
            Right Nothing -> pure $ Left
                ("session " <> sessionId <> " is already running")
            Right (Just lock) -> pure $ Right SessionLock
                { lockFilePath = path
                , sessionLockHandle = lock
                }

releaseSessionLock :: SessionLock -> IO ()
releaseSessionLock lock = do
    _ <- try @_ @SomeException
        (FileLock.unlockFile lock.sessionLockHandle)
    pure ()

sessionLockIsActive :: FilePath -> IO Bool
sessionLockIsActive path = mask_ $
    try @_ @SomeException
        (FileLock.tryLockFile path FileLock.Exclusive) >>= \case
            Left _ -> pure True
            Right Nothing -> pure True
            Right (Just lock) -> FileLock.unlockFile lock >> pure False
