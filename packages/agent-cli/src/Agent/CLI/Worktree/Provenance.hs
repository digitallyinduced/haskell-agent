-- | Read-only saved-session evidence for adopting older managed worktrees.
--
-- Session metadata is authoritative; checkout names, filesystem mtimes and
-- stale pre-migration JSON files are deliberately not activity clocks.
module Agent.CLI.Worktree.Provenance
    ( WorktreeActivity
    , loadWorktreeActivity
    , worktreeActivityFromListing
    , buildWorktreeActivity
    , protectActiveSessions
    , existingSessionLockActive
    ) where

import Agent.CLI.Session (listSessions, sessionDirForId)
import Agent.CLI.SessionLock (sessionLockPath, sessionActivityLockPath)
import Agent.CLI.Session.Types (SessionMeta(..))
import Agent.CLI.Worktree.ReadOnlyLock (withExistingReadOnlyLock)
import Agent.Store.Postgres.Connection (StorePool)
import Control.Exception.Safe (tryAny, tryIO)
import Control.Monad (foldM)
import Data.List (isPrefixOf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime)
import System.IO.Error (isDoesNotExistError)
import qualified System.Posix.Files as Posix
import System.OsPath
    ( OsPath, decodeUtf, isAbsolute, normalise, splitDirectories, takeDirectory, unsafeEncodeUtf, (</>) )

-- | A failed timestamp/provenance check poisons that checkout even if another
-- session for it is valid. Omitting that failure could hide newer activity.
type WorktreeActivity = Map OsPath (Either Text UTCTime)

-- | Reads all non-deleted sessions, including archived sessions and sessions
-- from other provider/gateway routes. The store uses a read-only transaction;
-- this function never imports legacy sessions or initializes/migrates storage.
-- The caller owns the pool and must not initialize it through a writing path
-- merely to run a dry run.
loadWorktreeActivity :: StorePool -> OsPath -> IO (Either Text WorktreeActivity)
loadWorktreeActivity pool root =
    tryAny (listSessions pool root) >>= \case
        Left _ -> pure (Left "saved-session activity unavailable; adoption deferred")
        Right listing@(sessions, _) -> case worktreeActivityFromListing root listing of
            Left err -> pure (Left err)
            Right activity -> Right <$> protectActiveSessions
                (takeDirectory (normalise root) </> unsafeEncodeUtf "sessions")
                root sessions activity

-- | Pre-upgrade agents do not hold worktree leases, but they do hold session
-- lifetime/turn locks. Probe every matching session, not just the newest one.
-- This is a read-only observation, not a fence against an old binary starting
-- after the final probe; such binaries must be stopped before explicit GC.
protectActiveSessions :: OsPath -> OsPath -> [SessionMeta] -> WorktreeActivity -> IO WorktreeActivity
protectActiveSessions sessionsRoot root sessions initial = foldM protect initial sessions
  where
    protect activity meta = case Map.keys (buildWorktreeActivity root [meta]) of
        [] -> pure activity
        checkout : _ -> do
            blocked <- case sessionDirForId sessionsRoot meta.metaId of
                Left _ -> pure True
                Right dir -> do
                    rootUnsafe <- uncertainSessionDirectory sessionsRoot
                    if rootUnsafe then pure True else do
                        dirUnsafe <- uncertainSessionDirectory dir
                        if dirUnsafe then pure True else do
                            lifetime <- existingSessionLockActive (sessionLockPath dir)
                            turn <- existingSessionLockActive (sessionActivityLockPath dir)
                            pure (lifetime || turn)
            pure if blocked
                then Map.insert checkout (Left "saved session is active or its session locks cannot be verified") activity
                else activity

-- Do not follow a substituted sessions root or session directory. Missing
-- directories are normal for imported/archived metadata; all other failures
-- and non-directory nodes are uncertain and retain the checkout.
uncertainSessionDirectory :: OsPath -> IO Bool
uncertainSessionDirectory path = do
    result <- tryIO (decodeUtf path >>= Posix.getSymbolicLinkStatus)
    pure case result of
        Left err -> not (isDoesNotExistError err)
        Right status -> not (Posix.isDirectory status)

-- | Open without O_CREAT: unlike tryLockFile this cannot create missing locks
-- even if a session directory disappears concurrently. Any non-ENOENT error,
-- non-regular lock or failed flock retains the checkout. Closing releases the
-- successful probe lock without modifying the file.
existingSessionLockActive :: FilePath -> IO Bool
existingSessionLockActive path =
    either (const True) (const False) <$>
        withExistingReadOnlyLock (unsafeEncodeUtf path) (pure ())

-- | Do not silently use a partial listing: the undecodable session could be
-- the newest session for any checkout. Avoid exposing session content in GC
-- diagnostics.
worktreeActivityFromListing
    :: OsPath -> ([SessionMeta], [Text]) -> Either Text WorktreeActivity
worktreeActivityFromListing root (sessions, warnings)
    | not (null warnings) =
        Left "saved-session metadata is incomplete or incompatible; adoption deferred"
    | not (isAbsolute root) || containsTraversal root =
        Left "managed worktree root is not an absolute, traversal-free path"
    | otherwise = Right (buildWorktreeActivity root sessions)

-- | Use the latest persisted activity across every session rooted at a
-- checkout or one of its subdirectories. A saved session is evidence, not
-- sufficient ownership proof by itself: GC additionally verifies the managed
-- path and reciprocal linked-worktree Git metadata under its maintenance lease.
buildWorktreeActivity :: OsPath -> [SessionMeta] -> WorktreeActivity
buildWorktreeActivity rawRoot sessions = Map.fromListWith newest
    [ (checkout, activity meta)
    | meta <- sessions
    , Just checkout <- [checkoutFor meta.metaCwd]
    ]
  where
    root = normalise rawRoot
    rootParts = splitDirectories root
    checkoutFor cwd
        | not (isAbsolute root && isAbsolute cwd) = Nothing
        | not (rootParts `isPrefixOf` splitDirectories (normalise cwd)) = Nothing
        | otherwise = case drop (length rootParts) (splitDirectories (normalise cwd)) of
            repository : checkout : _ -> Just (root </> repository </> checkout)
            _ -> Nothing
    activity meta
        | containsTraversal meta.metaCwd =
            Left "saved-session path contains traversal; activity uncertain"
        -- Session schema 1 records cwd and updatedAt with these semantics.
        -- The storage listing independently rejects incompatible versions.
        | meta.metaVersion /= 1 || Text.null (Text.strip meta.metaId) =
            Left "saved-session provenance is invalid"
        | meta.metaUpdatedAt < meta.metaCreatedAt =
            Left "saved-session activity predates session creation"
        | otherwise = Right meta.metaUpdatedAt
    newest (Left err) _ = Left err
    newest _ (Left err) = Left err
    newest (Right first) (Right second) = Right (max first second)

containsTraversal :: OsPath -> Bool
containsTraversal = any (== unsafeEncodeUtf "..") . splitDirectories
