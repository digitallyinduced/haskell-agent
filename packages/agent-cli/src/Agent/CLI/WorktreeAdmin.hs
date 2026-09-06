-- | Standalone recovery administration. Adoption reads existing session metadata
-- without starting, migrating, or importing the database.
module Agent.CLI.WorktreeAdmin (runWorktreeAdmin, renderWorktreeCleanupReport) where

import Agent.CLI.Config (HarnessConfig(..), WorktreeConfig(..), loadHarnessConfig)
import Agent.CLI.Options (WorktreeCommand(..))
import Agent.CLI.SessionAdmin (managedPostgresConfigForHome)
import Agent.CLI.Worktree.Provenance (WorktreeActivity, loadWorktreeActivity)
import Agent.CLI.Worktree
    ( WorktreeCleanupReport(..)
    , enrollWorktree
    , gcWorktreesWithActivity
    , isUnderWorktreeRoot
    , protectWorktree
    , restoreManagedWorktree
    , worktreeRoot
    )
import Agent.OsPath (unsafeToFilePath)
import Agent.Store.Postgres.Connection (closeStorePool, defaultPoolConfig, openStorePool)
import Control.Exception.Safe (bracket, tryAny)
import Control.Monad (unless)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory.OsPath (getCurrentDirectory, getHomeDirectory, makeAbsolute)
import System.Exit (exitFailure)
import System.IO (stderr)
import System.OsPath (OsPath)

runWorktreeAdmin :: WorktreeCommand -> IO ()
runWorktreeAdmin command = do
    home <- getHomeDirectory
    let root = worktreeRoot home
        administer requested action message = do
            path <- makeAbsolute requested
            unless (isUnderWorktreeRoot root path) $
                failCommand "path is outside the managed worktree root"
            action path >>= either failCommand (const $
                Text.putStrLn (message <> ": " <> Text.pack (unsafeToFilePath path)))
    case command of
        WorktreeGC dryRun overrideDays -> do
            config <- loadHarnessConfig home >>= either failCommand pure
            cwd <- getCurrentDirectory
            let days = fromMaybe config.configWorktree.worktreeInactiveDays overrideDays
            report <- gcWorktreesWithActivity (readExistingActivity home root) root days dryRun [cwd]
            Text.putStr (renderWorktreeCleanupReport dryRun days report)
            unless (null report.cleanupFailures) exitFailure
        WorktreeEnroll path ->
            administer path (enrollWorktree root)
                "Enrolled (ignored untracked files are NOT recoverable)"
        WorktreeRestore path ->
            administer path (restoreManagedWorktree root) "Checkout available"
        WorktreeProtect path ->
            administer path (\p -> protectWorktree root p True) "Protected"
        WorktreeUnprotect path ->
            administer path (\p -> protectWorktree root p False) "Unprotected"

failCommand :: Text -> IO a
failCommand message = Text.hPutStrLn stderr ("worktree: " <> message) >> exitFailure

-- Open only an ordinary pool: withStoreForHome would start PostgreSQL and run
-- migrations/imports, which are forbidden side effects for a dry run.
readExistingActivity :: OsPath -> OsPath -> IO (Either Text WorktreeActivity)
readExistingActivity home root = do
    result <- tryAny do
        config <- managedPostgresConfigForHome home
        bracket (openStorePool config defaultPoolConfig)
            (either (const (pure ())) closeStorePool)
            (either (const (pure unavailable)) (\pool -> loadWorktreeActivity pool root))
    pure (either (const unavailable) id result)
  where
    unavailable = Left "saved-session database unavailable; adoption deferred"

renderWorktreeCleanupReport :: Bool -> Int -> WorktreeCleanupReport -> Text
renderWorktreeCleanupReport dryRun days report = Text.unlines $
    [ (if dryRun then "Dry run" else "Collection pass")
        <> " — inactivity expiry: " <> tshow days <> " days"
    , "HEAD incorporated into the resolved default branch: 24 hours of inactivity."
    , "Ignored untracked files are NOT backed up or restored."
    , if dryRun then "Automatic adoption is simulated; no registry or snapshot is written."
        else "Verified existing agent worktrees are adopted using saved-session activity."
    ]
    <> [ "eligible\t" <> pathText path <> "\testimated bytes: " <> tshow bytes
       | (path, bytes) <- report.cleanupEligible ]
    <> [ "retained\t" <> pathText path <> "\t" <> reason
       | (path, reason) <- report.cleanupRetained ]
    <> [ "collected\t" <> pathText path | path <- report.cleanupRemoved ]
    <> [ "failed\t" <> pathText path <> "\t" <> reason
       | (path, reason) <- report.cleanupFailures ]
    <> [ tshow (length report.cleanupEligible) <> " eligible, "
        <> tshow (length report.cleanupRemoved) <> " collected, "
        <> tshow (length report.cleanupRetained) <> " retained, "
        <> tshow (length report.cleanupFailures) <> " failed."
       , "Eligible checkout gross apparent bytes: " <> tshow (sum (map snd report.cleanupEligible))
           <> " (excludes snapshot overhead and APFS sharing; not guaranteed net disk savings)."
       ]
  where
    -- Escape control characters in filenames so each decision stays on one line.
    pathText = Text.pack . show . unsafeToFilePath
    tshow :: Show a => a -> Text
    tshow = Text.pack . show
