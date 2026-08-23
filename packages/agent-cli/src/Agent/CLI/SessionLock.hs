-- | Lifetime ownership for persisted sessions.
--
-- Lock files are permanent coordination points. The operating system releases
-- the advisory lock when the owning process exits, including after a crash.
module Agent.CLI.SessionLock
    ( SessionLock
    , acquireSessionLock
    , releaseSessionLock
    , sessionLockFilePath
    , sessionLockIsActive
    , sessionLockPath
    ) where

import Agent.CLI.Error (formatException)
import Agent.OsPath (unsafeToFilePath)
import Control.Exception.Safe (SomeException, try)
import Data.Text (Text)
import qualified System.FileLock as FileLock
import qualified System.FilePath as FilePath
import System.OsPath (OsPath)

data SessionLock = SessionLock
    { lockFilePath :: !FilePath
    , sessionLockHandle :: !FileLock.FileLock
    }

sessionLockFilePath :: SessionLock -> FilePath
sessionLockFilePath lock = lock.lockFilePath

sessionLockPath :: OsPath -> FilePath
sessionLockPath sessionDir =
    unsafeToFilePath sessionDir FilePath.</> ".agent-running.lock"

acquireSessionLock :: OsPath -> Text -> IO (Either Text SessionLock)
acquireSessionLock sessionDir sessionId = do
    let path = sessionLockPath sessionDir
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
sessionLockIsActive path =
    try @_ @SomeException
        (FileLock.tryLockFile path FileLock.Exclusive) >>= \case
            Left _ -> pure True
            Right Nothing -> pure True
            Right (Just lock) -> FileLock.unlockFile lock >> pure False
