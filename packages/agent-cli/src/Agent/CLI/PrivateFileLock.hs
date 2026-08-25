-- | Blocking cross-process locks for private harness state.
module Agent.CLI.PrivateFileLock
    ( withPrivateFileLock
    ) where

import Agent.OsPath (unsafeToFilePath)
import Control.Exception.Safe (bracket)
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
withPrivateFileLock path action = do
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
    FileLock.withFileLock filePath FileLock.Exclusive \_ -> do
        setFileMode filePath 0o600
        action
