-- | Blocking cross-process locks for private harness state.
module Agent.CLI.PrivateFileLock
    ( withPrivateFileLock
    , withPrivateSharedFileLock
    , withPrivateSharedFileLockAfterGate
    , withPrivateSharedFileLocksAfterGate
    ) where

import Agent.OsPath (unsafeToFilePath)
import Control.Exception.Safe
    ( bracket
    , finally
    , mask
    , onException
    )
import Control.Monad (when)
import Data.IORef (atomicModifyIORef', newIORef)
import qualified System.FileLock as FileLock
import System.Directory.OsPath (createDirectoryIfMissing)
import System.OsPath (OsPath, takeDirectory)
import System.Posix.Files (setFileMode)
import System.Posix.IO
    ( OpenFileFlags(..)
    , OpenMode(ReadWrite)
    , closeFd
    , defaultFileFlags
    , openFd
    )

-- | Run an action while holding an exclusive lock on a private lock file.
--
-- Lock files are permanent coordination points. Their parent directories and
-- contents remain accessible only to the owning user, and the operating system
-- releases the lock if the process exits.
withPrivateFileLock :: OsPath -> IO a -> IO a
withPrivateFileLock path =
    withPrivateFileLockMode path FileLock.Exclusive

-- | Run an action while holding a shared lock on a private lock file.
withPrivateSharedFileLock :: OsPath -> IO a -> IO a
withPrivateSharedFileLock path =
    withPrivateFileLockMode path FileLock.Shared

withPrivateFileLockMode
    :: OsPath
    -> FileLock.SharedExclusive
    -> IO a
    -> IO a
withPrivateFileLockMode path mode action = do
    preparePrivateLockFile path
    FileLock.withFileLock (unsafeToFilePath path) mode \_ -> do
        setFileMode (unsafeToFilePath path) 0o600
        action

-- | Acquire an exclusive admission gate, acquire the target shared lock, then
-- release the gate before running the action. Writers hold the same gate while
-- waiting for the target's exclusive lock, so later long-running readers
-- cannot bypass a cross-process writer. Existing short readers can still take
-- the target lock directly and finish work required to release an older lease.
withPrivateSharedFileLockAfterGate
    :: OsPath
    -> OsPath
    -> IO a
    -> IO a
withPrivateSharedFileLockAfterGate gatePath path =
    withPrivateSharedFileLocksAfterGate gatePath [path]

-- | Like 'withPrivateSharedFileLockAfterGate', but acquire every shared lock
-- before releasing the admission gate. This supports compatibility locks that
-- must remain held together for the whole action.
withPrivateSharedFileLocksAfterGate
    :: OsPath
    -> [OsPath]
    -> IO a
    -> IO a
withPrivateSharedFileLocksAfterGate gatePath paths action = do
    preparePrivateLockFile gatePath
    mapM_ preparePrivateLockFile paths
    mask \restore -> do
        gateLock <-
            FileLock.lockFile
                (unsafeToFilePath gatePath)
                FileLock.Exclusive
        gateHeld <- newIORef True
        let releaseGate = do
                held <- atomicModifyIORef' gateHeld \current ->
                    (False, current)
                when held $
                    FileLock.unlockFile gateLock
                        `onException`
                            atomicModifyIORef' gateHeld (const (True, ()))
        (do
            sharedLocks <-
                acquireSharedLocks paths
                    `onException` releaseGate
            (do
                mapM_
                    (\path ->
                        setFileMode (unsafeToFilePath path) 0o600)
                    paths
                releaseGate
                restore action)
                `finally` releaseFileLocks sharedLocks)
            `finally` releaseGate
  where
    acquireSharedLocks [] = pure []
    acquireSharedLocks (path : remaining) = do
        lock <-
            FileLock.lockFile
                (unsafeToFilePath path)
                FileLock.Shared
        rest <-
            acquireSharedLocks remaining
                `onException` FileLock.unlockFile lock
        pure (lock : rest)

    releaseFileLocks =
        foldr
            (\lock remaining ->
                FileLock.unlockFile lock `finally` remaining)
            (pure ())
            . reverse

preparePrivateLockFile :: OsPath -> IO ()
preparePrivateLockFile path = do
    let directory = takeDirectory path
        filePath = unsafeToFilePath path
    createDirectoryIfMissing True directory
    setFileMode (unsafeToFilePath directory) 0o700
    -- 'filelock' creates a missing file with ordinary open permissions.
    -- Pre-create it with its private mode so there is no wider-permission
    -- window before the chmod below.
    bracket
        (openFd filePath ReadWrite
            defaultFileFlags { creat = Just 0o600, cloexec = True })
        closeFd
        (const (setFileMode filePath 0o600))
