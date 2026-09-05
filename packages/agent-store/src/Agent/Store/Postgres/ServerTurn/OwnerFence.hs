{-# LANGUAGE OverloadedStrings #-}

{- | Host-level action locks which survive managed PostgreSQL restarts.

The managed database is reachable only through its private local Unix socket,
so every server process shares this lock directory. Lock files deliberately
remain in place: replacing an inode while another process holds it would split
the lock domain.
-}
module Agent.Store.Postgres.ServerTurn.OwnerFence (
    OwnerActionFileLock,
    canonicalOwnerInstanceId,
    acquireSharedActionLock,
    tryAcquireExclusiveActionLock,
    releaseActionLock,
    withAvailableExclusiveActionLocks,
) where

import Agent.Store.Types (StoreError (..))
import Control.Exception.Safe (displayException, finally, mask, mask_, tryAny)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.UUID.Types as UUID
import qualified System.FileLock as FileLock
import System.FilePath ((</>))

data OwnerActionFileLock = OwnerActionFileLock
    { ownerActionFileLockInstanceId :: !Text
    , ownerActionFileLockHandle :: !FileLock.FileLock
    }

canonicalOwnerInstanceId :: Text -> Either StoreError Text
canonicalOwnerInstanceId raw =
    case UUID.fromText raw of
        Nothing ->
            Left . StoreDataError $
                "server turn owner instance id is not a UUID"
        Just ownerId -> Right (UUID.toText ownerId)

acquireSharedActionLock ::
    FilePath ->
    Text ->
    IO (Either StoreError OwnerActionFileLock)
acquireSharedActionLock directory rawInstanceId =
    mask_ $
        case canonicalOwnerInstanceId rawInstanceId of
            Left err -> pure (Left err)
            Right instanceId ->
                fmap (fmap (OwnerActionFileLock instanceId)) $
                    captureLockFailure "acquire shared server action lock" $
                        FileLock.lockFile
                            (actionLockPath directory instanceId)
                            FileLock.Shared

tryAcquireExclusiveActionLock ::
    FilePath ->
    Text ->
    IO (Either StoreError (Maybe OwnerActionFileLock))
tryAcquireExclusiveActionLock directory rawInstanceId =
    mask_ $
        case canonicalOwnerInstanceId rawInstanceId of
            Left err -> pure (Left err)
            Right instanceId ->
                fmap (fmap (fmap (OwnerActionFileLock instanceId))) $
                    captureLockFailure "acquire exclusive server action lock" $
                        FileLock.tryLockFile
                            (actionLockPath directory instanceId)
                            FileLock.Exclusive

releaseActionLock :: OwnerActionFileLock -> IO ()
releaseActionLock actionLock =
    mask_ $
        FileLock.unlockFile actionLock.ownerActionFileLockHandle

withAvailableExclusiveActionLocks ::
    FilePath ->
    [Text] ->
    ([Text] -> IO (Either StoreError value)) ->
    IO (Either StoreError value)
withAvailableExclusiveActionLocks directory instanceIds action =
    mask \restore ->
        acquireAll [] instanceIds >>= \case
            Left err -> pure (Left err)
            Right locks ->
                restore
                    (action (map (.ownerActionFileLockInstanceId) locks))
                    `finally` mapM_ releaseActionLock locks
  where
    acquireAll acquired = \case
        [] -> pure (Right (reverse acquired))
        instanceId : remaining ->
            tryAcquireExclusiveActionLock directory instanceId >>= \case
                Left err -> do
                    mapM_ releaseActionLock acquired
                    pure (Left err)
                Right Nothing -> acquireAll acquired remaining
                Right (Just actionLock) ->
                    acquireAll (actionLock : acquired) remaining

actionLockPath :: FilePath -> Text -> FilePath
actionLockPath directory instanceId =
    directory </> Text.unpack instanceId <> ".lock"

captureLockFailure :: Text -> IO value -> IO (Either StoreError value)
captureLockFailure operation action =
    tryAny action >>= \case
        Left err ->
            pure . Left . StoreProcessError $
                "Could not "
                    <> operation
                    <> ": "
                    <> Text.pack (displayException err)
        Right value -> pure (Right value)
