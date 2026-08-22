-- | Retry filesystem operations that temporarily fail because another
-- process or thread holds the GHC/macOS file lock.
module Agent.FileRetry
    ( appendLazyFileRetryingOpen
    , retryOnFileBusy
    , writeLazyFileAtomically
    ) where

import Control.Exception.Safe (bracket, bracketOnError, impureThrow, throwIO, tryIO)
import Control.Retry
    ( RetryPolicyM
    , exponentialBackoff
    , limitRetries
    , retrying
    )
import qualified Data.ByteString.Lazy as LBS
import System.Directory.OsPath (removeFile, renameFile)
import System.IO
    ( IOMode(AppendMode)
    , hClose
    , openBinaryFile
    , openBinaryTempFile
    )
import System.IO.Error (isAlreadyInUseError)
import System.OsPath (OsPath, decodeUtf, takeDirectory, takeFileName, unsafeEncodeUtf)
import System.Posix.Files (setFileMode)
import System.Posix.Types (FileMode)

fileBusyRetryPolicy :: RetryPolicyM IO
fileBusyRetryPolicy = exponentialBackoff 1000 <> limitRetries 5

-- | Retry an 'IO' action after 1, 2, 4, 8, and 16 milliseconds when it
-- fails with 'isAlreadyInUseError'. Other exceptions are rethrown immediately.
retryOnFileBusy :: IO a -> IO a
retryOnFileBusy action = do
    result <- retrying fileBusyRetryPolicy shouldRetry (const (tryIO action))
    either throwIO pure result
  where
    shouldRetry _ = pure . either isAlreadyInUseError (const False)

-- | Append bytes while retrying only acquisition of the append handle.
-- Once writing begins it is never replayed, so a partial write cannot be
-- duplicated by the retry policy.
appendLazyFileRetryingOpen :: OsPath -> LBS.ByteString -> IO ()
appendLazyFileRetryingOpen path bytes =
    bracket
        (retryOnFileBusy (openBinaryFile (decodeUtfPath path) AppendMode))
        hClose
        (`LBS.hPut` bytes)

-- | Atomically replace a file using a unique temporary file in the same
-- directory. Unique names prevent concurrent writers from renaming or
-- deleting each other's temporary files.
writeLazyFileAtomically
    :: OsPath
    -> FileMode
    -> LBS.ByteString
    -> IO ()
writeLazyFileAtomically path mode bytes =
    bracketOnError acquire cleanup \(temporary, handle) -> do
        LBS.hPut handle bytes
        hClose handle
        setFileMode temporary mode
        retryOnFileBusy (renameFile (unsafeEncodeUtf temporary) path)
  where
    acquire =
        retryOnFileBusy $
            openBinaryTempFile
                (decodeUtfPath (takeDirectory path))
                (decodeUtfPath (takeFileName path) <> ".tmp")
    cleanup (temporary, handle) = do
        _ <- tryIO (hClose handle)
        _ <- tryIO (removeFile (unsafeEncodeUtf temporary))
        pure ()

decodeUtfPath :: OsPath -> FilePath
decodeUtfPath = either impureThrow id . decodeUtf
