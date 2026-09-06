{-# LANGUAGE ForeignFunctionInterface #-}
-- | Non-creating probes compatible with the filelock package's flock leases.
module Agent.CLI.Worktree.ReadOnlyLock (withExistingReadOnlyLock) where

import Control.Exception.Safe (bracket, tryIO)
import Data.Text (Text)
import Foreign.C.Types (CInt(..))
import System.IO.Error (isDoesNotExistError)
import System.OsPath (OsPath, decodeUtf)
import qualified System.Posix.IO as Posix
import qualified System.Posix.Files as Posix
import System.Posix.Types (Fd(..))

-- | Hold an existing regular lock throughout the callback, without O_CREAT
-- or write access. An absent lock runs the callback without creating a fence;
-- callers must treat this as observation only and validate parent directories.
-- All unsafe nodes, contention and I/O failures fail closed. Callback failures
-- also fail closed; asynchronous exceptions propagate and release the fd.
withExistingReadOnlyLock :: OsPath -> IO a -> IO (Either Text a)
withExistingReadOnlyLock path action = do
    result <- tryIO do
        decoded <- decodeUtf path
        status <- tryIO (Posix.getSymbolicLinkStatus decoded)
        case status of
            Left err
                | isDoesNotExistError err -> Right <$> action
                | otherwise -> pure unavailable
            Right before
                | not (Posix.isRegularFile before) -> pure unavailable
                | otherwise -> bracket
                    (Posix.openFd decoded Posix.ReadOnly
                        Posix.defaultFileFlags { Posix.nonBlock = True })
                    Posix.closeFd
                    (\fd@(Fd rawFd) -> do
                        after <- Posix.getFdStatus fd
                        if not (Posix.isRegularFile after)
                            || Posix.fileID before /= Posix.fileID after
                            || Posix.deviceID before /= Posix.deviceID after
                            then pure unavailable
                            else do
                                locked <- c_flock rawFd 6
                                if locked /= 0 then pure unavailable else Right <$> action)
    pure (either (const unavailable) id result)
  where
    unavailable = Left "existing lock is busy, unsafe or cannot be verified"

-- LOCK_EX (2) | LOCK_NB (4), identical on supported Linux/macOS targets.
-- fcntl record locks are not compatible with filelock's flock leases.
foreign import ccall unsafe "flock" c_flock :: CInt -> CInt -> IO CInt
