{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
-- | Crash-safe per-checkout records. A missing record means /not enrolled/,
-- never permission to collect a generated-looking directory.
module Agent.CLI.Worktree.Registry
    ( WorktreeRecord(..)
    , readRecord
    , writeRecord
    , modifyRecord
    , recordPath
    ) where

import Agent.CLI.Worktree.Snapshot (WorktreeSnapshot)
import Agent.OsPath (unsafeToFilePath)
import Control.Exception.Safe (bracket, tryAny, displayException)
import Control.Monad (when)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict', encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import qualified System.Directory as Directory
import qualified System.FileLock as FileLock
import qualified System.FilePath as FP
import System.IO (openBinaryTempFile, hClose)
import System.IO.Error (catchIOError, isDoesNotExistError)
import System.OsPath (OsPath, takeDirectory, takeFileName, (</>), unsafeEncodeUtf)
import System.Posix.IO (openFd, closeFd, defaultFileFlags, OpenMode(ReadOnly))
import System.Posix.Unistd (fileSynchronise)

data WorktreeRecord = WorktreeRecord
    { recordVersion :: !Int
    , recordCheckout :: !FilePath
    , recordCommonDir :: !FilePath
    , recordLastActivity :: !UTCTime
    , recordProtected :: !Bool
    , recordState :: !Text
    , recordSnapshot :: !(Maybe WorktreeSnapshot)
    } deriving (Eq, Show, Generic, FromJSON, ToJSON)

recordPath :: OsPath -> OsPath -> OsPath
recordPath root checkout =
    root </> unsafeEncodeUtf ".registry"
        </> takeFileName (takeDirectory checkout)
        </> (takeFileName checkout <> unsafeEncodeUtf ".json")

readRecord :: OsPath -> OsPath -> IO (Either Text (Maybe WorktreeRecord))
readRecord root checkout = catching do
    let path = unsafeToFilePath (recordPath root checkout)
    validateRegistryPaths root checkout
    exists <- Directory.doesPathExist path
    if not exists then pure (Right Nothing) else do
        bytes <- BS.readFile path
        pure case eitherDecodeStrict' bytes of
            Left err -> Left ("invalid worktree registry: " <> Text.pack err)
            Right record
                | record.recordVersion /= 1 -> Left "unsupported worktree registry version"
                | record.recordCheckout /= unsafeToFilePath checkout ->
                    Left "worktree registry checkout identity mismatch"
                | otherwise -> Right (Just record)

-- Callers hold a worktree lease. The short registry lock also serializes
-- activity updates from multiple shared lease holders.
writeRecord :: OsPath -> OsPath -> WorktreeRecord -> IO (Either Text ())
writeRecord root checkout record =
    modifyRecord root checkout (const (Right (Just record)))

modifyRecord
    :: OsPath -> OsPath
    -> (Maybe WorktreeRecord -> Either Text (Maybe WorktreeRecord))
    -> IO (Either Text ())
modifyRecord root checkout change = catching do
    let destination = unsafeToFilePath (recordPath root checkout)
        directory = FP.takeDirectory destination
    validateRegistryPaths root checkout
    Directory.createDirectoryIfMissing True directory
    -- Persist every newly-created directory entry too. A record's immediate
    -- parent fsync alone cannot make .registry or its repo directory durable.
    syncAncestors directory
    FileLock.withFileLock (destination <> ".lock") FileLock.Exclusive $ \_ ->
        readRecord root checkout >>= \case
            Left err -> pure (Left err)
            Right old -> case change old of
                Left err -> pure (Left err)
                Right Nothing -> pure (Right ())
                Right (Just record) -> do
                    -- Flush data before atomic rename, then flush the directory
                    -- entry before allowing destructive worktree removal.
                    bracket (openBinaryTempFile directory ".record-") cleanup $ \(temp, handle) -> do
                        LBS.hPut handle (encode record)
                        hClose handle
                        sync temp
                        Directory.renameFile temp destination
                        sync directory
                    pure (Right ())
  where
    cleanup (temp, handle) = do
        _ <- tryAny (hClose handle)
        _ <- tryAny (Directory.removeFile temp)
        pure ()
    sync path =
        bracket (openFd path ReadOnly defaultFileFlags) closeFd fileSynchronise
    syncAncestors path = do
        sync path
        let parent = FP.takeDirectory path
        when (parent /= path) (syncAncestors parent)

-- Never follow redirected metadata, including dangling symlinks which
-- doesPathExist reports as absent. Missing parents are valid before enrollment.
validateRegistryPaths :: OsPath -> OsPath -> IO ()
validateRegistryPaths root checkout = mapM_ rejectLink
    [ unsafeToFilePath root
    , unsafeToFilePath root FP.</> ".registry"
    , FP.takeDirectory destination
    , destination
    , destination <> ".lock"
    ]
  where
    destination = unsafeToFilePath (recordPath root checkout)
    rejectLink path = do
        linked <- Directory.pathIsSymbolicLink path `catchIOError` \err ->
            if isDoesNotExistError err then pure False else ioError err
        when linked (ioError (userError "symlinked worktree registry path"))

catching :: IO (Either Text a) -> IO (Either Text a)
catching action = tryAny action >>= \case
    Left err -> pure (Left (Text.pack (displayException err)))
    Right result -> pure result
