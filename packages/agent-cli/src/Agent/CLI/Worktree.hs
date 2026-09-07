-- | Create isolated git worktrees under @~/.haskell-agent/worktrees@.
module Agent.CLI.Worktree
    ( createWorktree
    , createWorktreeWithFetch
    , createManagedWorktree
    , createManagedWorktreeWithProgress
    , createManagedWorktreeFromConfigWithProgress
    , removeWorktree
    , cleanupStaleWorktrees
    , gcWorktrees
    , gcWorktreesWithActivity
    , worktreeInactive
    , enrollWorktree
    , protectWorktree
    , restoreManagedWorktree
    , WorktreeCleanupReport(..)
    , WorktreeLease
    , acquireWorktreeLease
    , releaseWorktreeLease
    , isUnderWorktreeRoot
    , worktreeProgressMessage
    , worktreePath
    , worktreeRoot
    , WorktreeProgress(..)
    ) where

import Agent.CLI.Config
    ( HarnessConfig(..)
    , WorktreeConfig(..)
    , loadHarnessConfig
    )
import Agent.OsPath (unsafeToFilePath)
import Agent.CLI.Worktree.Registry
import Agent.CLI.Worktree.ReadOnlyLock (withExistingReadOnlyLock)
import qualified Agent.CLI.Worktree.Snapshot as Snapshot
import Control.Applicative ((<|>))
import Control.Exception.Safe
    ( SomeException
    , displayException
    , finally
    , mask
    , onException
    , tryAny
    , catchIO
    )
import Control.Monad (foldM, void, when, unless)
import Data.IORef (newIORef, readIORef, modifyIORef')
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
    ( ExceptT(..)
    , runExceptT
    , throwE
    , withExceptT
    )
import qualified Data.ByteString as ByteString
import Data.Char (isHexDigit)
import Data.List (isPrefixOf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime(..), getCurrentTime, nominalDiffTimeToSeconds, diffUTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Numeric (showHex)
import System.IO.Error (isDoesNotExistError)
import System.Directory.OsPath
    ( createDirectoryIfMissing
    , doesDirectoryExist
    , doesFileExist
    , doesPathExist
    , listDirectory
    , pathIsSymbolicLink
    , removePathForcibly
    )
import System.Entropy (getEntropy)
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import System.Timeout (timeout)
import System.Exit (ExitCode(..))
import qualified System.FileLock as FileLock
import System.OsPath
    ( OsPath
    , equalFilePath
    , normalise
    , splitDirectories
    , takeDirectory
    , takeFileName
    , unsafeEncodeUtf
    , (</>)
    )
import System.Process (CreateProcess(..), proc, readCreateProcessWithExitCode)

data WorktreeProgress
    = WorktreeInspectingRepository
    | WorktreeCheckingRemote !Text
    | WorktreeFetchingRemote !Text !Text
    | WorktreeCreating
    deriving (Eq, Show)

worktreeProgressMessage :: WorktreeProgress -> Text
worktreeProgressMessage = \case
    WorktreeInspectingRepository -> "Inspecting Git repository…"
    WorktreeCheckingRemote remote ->
        "Checking Git remote " <> remote <> "…"
    WorktreeFetchingRemote remote remoteHead ->
        "Fetching latest from " <> remote <> "/" <> branchName remoteHead <> "…"
    WorktreeCreating -> "Creating worktree…"
  where
    branchName ref =
        maybe ref id (Text.stripPrefix "refs/heads/" ref)

data WorktreeCleanupReport = WorktreeCleanupReport
    { cleanupRemoved :: ![OsPath]
    , cleanupFailures :: ![(OsPath, Text)]
    , cleanupEligible :: ![(OsPath, Integer)]
    , cleanupRetained :: ![(OsPath, Text)]
    }
    deriving (Eq, Show)

instance Semigroup WorktreeCleanupReport where
    left <> right = WorktreeCleanupReport
        { cleanupRemoved = left.cleanupRemoved <> right.cleanupRemoved
        , cleanupFailures = left.cleanupFailures <> right.cleanupFailures
        , cleanupEligible = left.cleanupEligible <> right.cleanupEligible
        , cleanupRetained = left.cleanupRetained <> right.cleanupRetained
        }

instance Monoid WorktreeCleanupReport where
    mempty = WorktreeCleanupReport [] [] [] []

data WorktreeLease = WorktreeLease FileLock.FileLock (Maybe (OsPath, OsPath))

-- | @~/.haskell-agent/worktrees@ given the user's home directory.
worktreeRoot :: OsPath -> OsPath
worktreeRoot home =
    home </> unsafeEncodeUtf ".haskell-agent" </> unsafeEncodeUtf "worktrees"

-- | True when @path@ is @root@ or a subdirectory of it.
-- Both paths should already be absolute (or otherwise comparable).
isUnderWorktreeRoot :: OsPath -> OsPath -> Bool
isUnderWorktreeRoot root path =
    equalFilePath root path
        || splitDirectories root `isPrefixOf` splitDirectories path

-- | @root/repo/YYYY-MM-DD-\<hex8\>@.
worktreePath :: OsPath -> OsPath -> Day -> String -> OsPath
worktreePath root repoName day hex8 =
    root </> repoName </> unsafeEncodeUtf (formatDay day <> "-" <> hex8)

-- | Add a new worktree of @source@ under @root@ using the current @HEAD@.
-- @root@ is injected so tests can use a temp directory instead of the real
-- home.
createWorktree :: OsPath -> OsPath -> IO (Either Text OsPath)
createWorktree = createWorktreeWithFetch False

-- | Add a new worktree, optionally fetching the selected remote's current
-- default branch and using that commit as the base.
createWorktreeWithFetch
    :: Bool -> OsPath -> OsPath -> IO (Either Text OsPath)
createWorktreeWithFetch =
    createWorktreeWithFetchProgress (const (pure ()))

createWorktreeWithFetchProgress
    :: (WorktreeProgress -> IO ())
    -> Bool
    -> OsPath
    -> OsPath
    -> IO (Either Text OsPath)
createWorktreeWithFetchProgress report fetchLatest source root = runExceptT do
    lift (report WorktreeInspectingRepository)
    repo <- gitToplevel source
    repoName <- gitRepositoryName repo
    base <-
        if fetchLatest
            then fetchLatestUpstream report repo
            else pure Nothing
    lift (report WorktreeCreating)
    now <- lift getCurrentTime
    let day = utctDay now
        start = posixMicros now
    lift (createDirectoryIfMissing True (root </> repoName))
    withGitWorktreeLock repo $
        addUnique repo root repoName day start base 0

-- | Create a worktree using the current machine-wide policy under the
-- supplied home. Interactive and subagent creation use this fresh-read entry
-- point; initial CLI startup uses its already validated snapshot below.
createManagedWorktree :: OsPath -> OsPath -> IO (Either Text OsPath)
createManagedWorktree =
    createManagedWorktreeWithProgress (const (pure ()))

createManagedWorktreeWithProgress
    :: (WorktreeProgress -> IO ())
    -> OsPath
    -> OsPath
    -> IO (Either Text OsPath)
createManagedWorktreeWithProgress report home source =
    loadHarnessConfig home >>= \case
        Left err -> pure (Left err)
        Right config ->
            createManagedWorktreeFromConfigWithProgress
                report
                config
                home
                source

-- | Create an initial managed worktree from an already validated startup
-- configuration snapshot. Later interactive worktree creation deliberately
-- continues through 'createManagedWorktreeWithProgress' so it observes
-- configuration changes made after startup.
createManagedWorktreeFromConfigWithProgress
    :: (WorktreeProgress -> IO ())
    -> HarnessConfig
    -> OsPath
    -> OsPath
    -> IO (Either Text OsPath)
createManagedWorktreeFromConfigWithProgress report config home source = do
    created <- createWorktreeWithFetchProgress
        report
        config.configWorktree.worktreeFetchLatestUpstream
        source
        (worktreeRoot home)
    case created of
        Left err -> pure (Left err)
        Right path -> enrollWorktree (worktreeRoot home) path >>= \case
            Left err -> pure (Left ("Created checkout but enrollment failed; retained safely: " <> err))
            Right () -> pure (Right path)

-- | Remove a managed worktree and the branch created for it.
removeWorktree :: OsPath -> OsPath -> IO (Either Text ())
removeWorktree source path = runExceptT do
    repo <- gitToplevel source
    withGitWorktreeLock repo do
        void $ ExceptT $
            git repo ["worktree", "remove", "--force", unsafeToFilePath path]
        void $ ExceptT $
            git repo ["branch", "-D", unsafeToFilePath (takeFileName path)]

-- | Take a shared lease when @path@ is inside one of our managed worktrees.
-- Cleanup takes the corresponding exclusive lock, so multiple live sessions
-- may share a checkout while automatic cleanup cannot remove it.
acquireWorktreeLease
    :: OsPath
    -> OsPath
    -> IO (Either Text (Maybe WorktreeLease))
acquireWorktreeLease root path = mask $ \restore ->
    case managedWorktreePath root path of
        Nothing -> pure (Right Nothing)
        Just managed -> do
            let lockPath = worktreeLeasePath root managed
            result <- tryAny do
                linked <- pathIsSymbolicLink root
                when linked (ioError (userError "symlinked managed worktree root"))
                createDirectoryIfMissing True (takeDirectory lockPath)
                FileLock.tryLockFile
                    (unsafeToFilePath lockPath)
                    FileLock.Shared
            case result of
                Left exception ->
                    pure $ Left
                        ("failed to lease managed worktree "
                            <> pathText managed
                            <> ": "
                            <> Text.pack (displayException exception))
                Right Nothing ->
                    pure $ Left
                        ("managed worktree is being cleaned up: "
                            <> pathText managed)
                Right (Just lock) -> do
                    activity <- restore (touchWorktree root managed)
                        `onException` FileLock.unlockFile lock
                    case activity of
                        Left err -> FileLock.unlockFile lock >> pure (Left err)
                        Right () -> pure (Right (Just (WorktreeLease lock (Just (root, managed)))))

releaseWorktreeLease :: WorktreeLease -> IO ()
releaseWorktreeLease (WorktreeLease lock activity) =
    (case activity of
        Nothing -> pure ()
        Just (root, path) -> void (touchWorktree root path))
    `finally` FileLock.unlockFile lock

-- | Updating activity never enrolls legacy checkouts.
touchWorktree :: OsPath -> OsPath -> IO (Either Text ())
touchWorktree root path = readRecord root path >>= \case
    Left err -> pure (Left err)
    Right Nothing -> pure (Right ())
    Right (Just _) -> do
        now <- getCurrentTime
        modifyRecord root path (Right . fmap (\r -> r { recordLastActivity = now }))

-- | Explicit consent to the ignored-file exclusion and inactivity policy.
-- Enrollment starts the inactivity clock now, not at the old checkout date.
enrollWorktree :: OsPath -> OsPath -> IO (Either Text ())
enrollWorktree root path = catchingWorktree $ withExclusiveManaged root path $ runExceptT do
    common <- inspectManagedIdentity root path
    now <- lift getCurrentTime
    ExceptT $ modifyRecord root path $ \case
        Just record
            | record.recordCommonDir == unsafeToFilePath common ->
                Right (Just record)
            | otherwise -> Left "enrolled repository identity changed"
        Nothing -> Right (Just WorktreeRecord
            { recordVersion = 1
            , recordCheckout = unsafeToFilePath path
            , recordCommonDir = unsafeToFilePath common
            , recordLastActivity = now
            , recordProtected = False
            , recordState = "present"
            , recordSnapshot = Nothing
            })

protectWorktree :: OsPath -> OsPath -> Bool -> IO (Either Text ())
protectWorktree root path protected = catchingWorktree $
    withExclusiveManaged root path $
        modifyRecord root path $ \case
            Nothing -> Left "worktree is not enrolled; enroll explicitly first"
            Just record -> Right (Just record { recordProtected = protected })

-- | Resume only restores absent enrolled checkouts. It never overwrites a path
-- or rewinds the old branch; snapshot restoration uses a detached HEAD.
restoreManagedWorktree :: OsPath -> OsPath -> IO (Either Text ())
restoreManagedWorktree root requested =
    case managedWorktreePath root requested of
        Nothing -> pure (Right ())
        Just path -> catchingWorktree do
            exists <- doesPathExist path
            if exists then mask $ \restore ->
                acquireWorktreeLease root path >>= \case
                    Left err -> pure (Left err)
                    Right Nothing -> pure (Left "managed checkout identity changed during resume")
                    Right (Just lease) ->
                        restore (do
                            stillExists <- doesPathExist path
                            if not stillExists
                                then pure (Left "checkout was collected during resume; retry to restore")
                                else readRecord root path >>= \case
                                    Left err -> pure (Left err)
                                    Right (Just record) | record.recordState /= "present" ->
                                        pure (Left ("checkout exists in interrupted maintenance state "
                                            <> record.recordState <> "; refusing to overwrite it"))
                                    _ -> pure (Right ()))
                        `finally` releaseWorktreeLease lease
            else
                withExclusiveManaged root path $ runExceptT do
                    record <- ExceptT (readRecord root path) >>= maybe
                        (throwE "missing worktree has no enrolled recovery record") pure
                    unless (record.recordState `elem` ["collecting", "collected", "restoring"])
                        (throwE "checkout disappeared outside collection; refusing a potentially stale snapshot")
                    snapshot <- maybe
                        (throwE "missing worktree has no verified recovery snapshot")
                        pure record.recordSnapshot
                    let common = unsafeEncodeUtf record.recordCommonDir
                    ExceptT $ withGitWorktreeLockAt common $ runExceptT do
                        links <- lift $ mapM pathIsSymbolicLink [root, takeDirectory path]
                        when (or links) (throwE "symlinked restoration parent")
                        -- Persist before restoration. Interrupted restore leaves
                        -- a directory that future resumes will not overwrite.
                        ExceptT $ writeRecord root path record { recordState = "restoring" }
                        ExceptT $ Snapshot.restoreSnapshot common path snapshot
                        now <- lift getCurrentTime
                        ExceptT $ writeRecord root path record
                            { recordState = "present", recordLastActivity = now }

withExclusiveManaged :: OsPath -> OsPath -> IO (Either Text a) -> IO (Either Text a)
withExclusiveManaged root path action = mask $ \restore ->
    if managedWorktreePath root path /= Just path
        || any (== unsafeEncodeUtf "..") (splitDirectories path)
        then pure (Left "expected a managed checkout root, not a subdirectory or traversal")
        else tryExclusiveWorktreeLease root path >>= \case
            Left err -> pure (Left err)
            Right Nothing -> pure (Left "worktree is active or another maintenance operation holds its lease")
            Right (Just lease) -> restore action `finally` releaseWorktreeLease lease

inspectManagedIdentity :: OsPath -> OsPath -> ExceptT Text IO OsPath
inspectManagedIdentity root path = do
    unless (managedWorktreePath root path == Just path)
        (throwE "not a managed checkout root")
    links <- lift $ mapM pathIsSymbolicLink [root, takeDirectory path, path]
    when (or links) (throwE "symlinked managed checkout or repository directory")
    top <- gitToplevel path
    unless (equalFilePath (normalise top) (normalise path))
        (throwE "checkout does not own its Git working directory")
    common <- gitCommonDir path
    gitDir <- Text.strip <$> ExceptT
        (git path ["rev-parse", "--path-format=absolute", "--git-dir"])
    when (gitDir == pathText common) (throwE "primary checkout cannot be collected")
    let admin = unsafeEncodeUtf (Text.unpack gitDir)
        dotGit = path </> unsafeEncodeUtf ".git"
        backpointer = admin </> unsafeEncodeUtf "gitdir"
    unless (equalFilePath (takeDirectory admin) (common </> unsafeEncodeUtf "worktrees"))
        (throwE "linked Git administration is outside the repository worktree registry")
    metadataLinks <- lift $ mapM pathIsSymbolicLink
        [dotGit, common, takeDirectory admin, admin, backpointer]
    when (or metadataLinks) (throwE "symlinked linked Git metadata")
    regular <- lift $ mapM doesFileExist [dotGit, backpointer]
    unless (and regular) (throwE "missing linked Git metadata")
    reciprocal <- Text.strip <$> lift (Text.readFile (unsafeToFilePath backpointer))
    unless (equalFilePath (normalise (unsafeEncodeUtf (Text.unpack reciprocal))) (normalise dotGit))
        (throwE "linked Git metadata points at another checkout")
    pure common

-- The dry-run must not create even maintenance lock files. Existing lock
-- files use the same OS advisory locks as filelock; closing the read-only
-- handle releases the probe. An absent lock is only an observational result:
-- a real pass always acquires the normal lease and checks everything again.
withReadOnlyLock :: OsPath -> IO (Either Text a) -> IO (Either Text a)
withReadOnlyLock path action = do
    linked <- isLinkIfPresent path
    parentLinked <- isLinkIfPresent (takeDirectory path)
    if linked || parentLinked then pure (Left "symlinked maintenance lock")
        else withExistingReadOnlyLock path action >>= pure . either Left id
  where
    isLinkIfPresent file = pathIsSymbolicLink file `catchIO` \err ->
        if isDoesNotExistError err then pure False else ioError err

catchingWorktree :: IO (Either Text a) -> IO (Either Text a)
catchingWorktree action = tryAny action >>= \case
    Left err -> pure (Left (exceptionText err))
    Right result -> pure result

-- | Snapshot-backed GC. Actual passes are bounded to eight eligible checkouts
-- and sixty seconds; each snapshot/removal has a thirty-second deadline.
-- Dry runs do not create snapshots, registry entries, or recovery refs.
-- This compatibility entry point has no legacy provenance; production callers
-- supply the saved-session reader through 'gcWorktreesWithActivity'.
gcWorktrees :: OsPath -> Int -> Bool -> [OsPath] -> IO WorktreeCleanupReport
gcWorktrees root = gcWorktreesWithActivity (pure (Right Map.empty)) root

-- | The injected reader is also re-run immediately before collection, under
-- the checkout lease, so newly saved session activity cannot be overwritten
-- by the pass's discovery snapshot.
gcWorktreesWithActivity
    :: IO (Either Text (Map OsPath (Either Text UTCTime)))
    -> OsPath -> Int -> Bool -> [OsPath] -> IO WorktreeCleanupReport
gcWorktreesWithActivity loadActivity root days dryRun protected = do
    exists <- doesDirectoryExist root
    if not exists then pure mempty else do
        result <- tryAny do
            linked <- pathIsSymbolicLink root
            when linked (ioError (userError "symlinked managed worktree root"))
            started <- getCurrentTime
            discovered <- timeout (30 * 1000000) (discoverManagedPaths root)
            candidates <- maybe
                (ioError (userError "worktree discovery deadline exceeded; no checkouts collected"))
                pure discovered
            activity <- timeout (30 * 1000000) loadActivity
            attempts <- newIORef (0 :: Int)
            foldM (visit started attempts (maybe (Left "session activity discovery deadline exceeded") id activity))
                mempty candidates
        pure $ either (cleanupFailure root) id result
  where
    retained path reason = mempty { cleanupRetained = [(path, reason)] }
    visit started attempts activity report path = do
        now <- getCurrentTime
        attempted <- readIORef attempts
        let exhausted = not dryRun &&
                (attempted >= 8 || diffUTCTime now started >= 60)
        if exhausted then pure (report <> retained path "pass budget exhausted") else do
            result <- timeout (30 * 1000000) $ catchingWorktree $
                checkoutLock path $ runExceptT do
                    when (any (isUnderWorktreeRoot path . normalise) protected)
                        (throwE "current session")
                    existing <- ExceptT (readRecord root path)
                    -- Persistent protection and recovery states take priority
                    -- over missing provenance and never get silently replaced.
                    case existing of
                        Just record | record.recordProtected -> throwE "protected"
                        Just record | record.recordState /= "present" ->
                            throwE ("state: " <> record.recordState)
                        _ -> pure ()
                    record <- resolveRecord path activity existing
                    isRecent <- lift (recent path now record)
                    if isRecent then pure (retained path "recent activity") else do
                        lift $ modifyIORef' attempts (+ 1)
                        common <- inspectManagedIdentity root path
                        unless (unsafeToFilePath common == record.recordCommonDir)
                            (throwE "enrolled repository identity changed")
                        ExceptT $ repositoryLock common $ runExceptT do
                            ExceptT $ Snapshot.checkSnapshotSupported path
                            if dryRun then do
                                bytes <- lift (estimateCheckoutBytes path)
                                pure mempty { cleanupEligible = [(path, bytes)] }
                            else do
                                latest <- lift loadActivity
                                refreshed <- resolveRecord path latest existing
                                recheckedAt <- lift getCurrentTime
                                recheckedRecent <- lift (recent path recheckedAt refreshed)
                                when recheckedRecent
                                    (throwE "recent activity")
                                unless (refreshed.recordCommonDir == record.recordCommonDir)
                                    (throwE "repository identity changed during adoption")
                                snapshot <- ExceptT $ Snapshot.createSnapshot path
                                finalActivity <- lift loadActivity
                                finalRecord <- resolveRecord path finalActivity existing
                                finalAt <- lift getCurrentTime
                                finalRecent <- lift (recent path finalAt finalRecord)
                                when finalRecent
                                    (throwE "recent activity")
                                finalCommon <- inspectManagedIdentity root path
                                unless (finalCommon == common && finalRecord.recordCommonDir == record.recordCommonDir)
                                    (throwE "repository identity changed during snapshot")
                                -- Adoption is persisted with the original saved
                                -- activity, never an artificial 'now' timestamp.
                                let saved = finalRecord { recordSnapshot = Just snapshot, recordState = "collecting" }
                                ExceptT $ writeRecord root path saved
                                removalAt <- lift getCurrentTime
                                removalRecent <- lift (recent path removalAt finalRecord)
                                when removalRecent do
                                    ExceptT $ writeRecord root path saved { recordState = "present" }
                                    throwE "recent activity or default-branch ancestry changed before removal"
                                verified <- lift $ Snapshot.verifySnapshotUnchanged path snapshot
                                case verified of
                                    Right () -> pure ()
                                    Left err -> do
                                        ExceptT $ writeRecord root path saved { recordState = "present" }
                                        throwE err
                                void $ ExceptT $ git common ["worktree", "remove", "--force", unsafeToFilePath path]
                                ExceptT $ writeRecord root path saved { recordState = "collected" }
                                pure mempty { cleanupRemoved = [path] }
            pure $ report <> case result of
                Nothing -> retained path "maintenance deadline exceeded; recovery record retained"
                Just (Left err) -> retained path err
                Just (Right one) -> one
    recent path now record
        | worktreeInactive days False now record.recordLastActivity = pure False
        | not (worktreeInactive days True now record.recordLastActivity) = pure True
        | otherwise = not <$> headInDefaultBranch path
    checkoutLock path
        | dryRun = withReadOnlyLock (worktreeLeasePath root path)
        | otherwise = withExclusiveManaged root path
    repositoryLock common
        | dryRun = withReadOnlyLock (common </> unsafeEncodeUtf "haskell-agent-worktree.lock")
        | otherwise = withGitWorktreeLockAt common
    resolveRecord path activity existing = do
        evidence <- either throwE pure activity
        case (existing, Map.lookup path evidence) of
            (_, Just (Left err)) -> throwE err
            (Just record, savedActivity) -> pure record
                { recordLastActivity = maybe record.recordLastActivity
                    (either (const record.recordLastActivity) (max record.recordLastActivity))
                    savedActivity }
            (Nothing, Nothing) -> throwE "uncertain ownership: no saved-session provenance"
            (Nothing, Just (Right lastActivity)) -> do
                common <- inspectManagedIdentity root path
                repository <- gitRepositoryName path
                unless (repository == takeFileName (takeDirectory path))
                    (throwE "uncertain ownership: managed repository directory does not match Git identity")
                pure WorktreeRecord
                    { recordVersion = 1
                    , recordCheckout = unsafeToFilePath path
                    , recordCommonDir = unsafeToFilePath common
                    , recordLastActivity = lastActivity
                    , recordProtected = False
                    , recordState = "present"
                    , recordSnapshot = Nothing
                    }

-- | Inactivity, not commit age or time since merge. Exact ancestry proof permits
-- the one-day fast path; uncertainty keeps the configured normal threshold.
worktreeInactive :: Int -> Bool -> UTCTime -> UTCTime -> Bool
worktreeInactive days incorporated now lastActivity =
    diffUTCTime now lastActivity >= fromIntegral threshold * 86400
  where
    threshold = if incorporated then min 1 (max 0 days) else max 0 days

-- | Resolve only an existing remote-default symbolic ref, never infer the
-- default from a familiar branch name or from the current checkout. No fetch,
-- network request or ref mutation is performed by maintenance. Stale/missing
-- tracking refs can miss merges; squash/rebase equivalence is not ancestry.
-- This proof is recomputed after snapshotting so newer commits cannot inherit
-- an older HEAD's eligibility. Final snapshot verification also checks HEAD.
headInDefaultBranch :: OsPath -> IO Bool
headInDefaultBranch path = do
    result <- runExceptT do
        graftPath <- Text.strip <$> ExceptT
            (git path ["rev-parse", "--path-format=absolute", "--git-path", "info/grafts"])
        grafts <- lift $ Directory.doesPathExist (Text.unpack graftPath)
        when grafts (throwE "grafted history is not merge evidence")
        remote <- selectUpstreamRemote path >>= maybe (throwE "no default remote") pure
        let prefix = "refs/remotes/" <> remote <> "/"
        target <- Text.strip <$> ExceptT
            (git path ["symbolic-ref", "--quiet", Text.unpack (prefix <> "HEAD")])
        unless (prefix `Text.isPrefixOf` target && target /= prefix <> "HEAD")
            (throwE "default branch is outside the selected remote")
        base <- Text.strip <$> ExceptT
            (git path ["--no-replace-objects", "rev-parse", "--verify", Text.unpack (target <> "^{commit}")])
        headCommit <- Text.strip <$> ExceptT
            (git path ["--no-replace-objects", "rev-parse", "--verify", "HEAD^{commit}"])
        void $ ExceptT
            (git path ["--no-replace-objects", "merge-base", "--is-ancestor", Text.unpack headCommit, Text.unpack base])
    pure $ either (const False) (const True) result

discoverManagedPaths :: OsPath -> IO [OsPath]
discoverManagedPaths root = do
    entries <- listDirectory root
    fmap concat $ mapM discover entries
  where
    discover entry
        | "." `isPrefixOf` unsafeToFilePath entry = pure []
        | otherwise = do
            let repo = root </> entry
            linked <- pathIsSymbolicLink repo
            directory <- doesDirectoryExist repo
            if linked || not directory then pure [] else do
                children <- listDirectory repo
                pure [repo </> child | child <- children, isManagedWorktreeName child]

-- Apparent bytes, including ignored cache data, without following symlinks.
-- This is an estimate rather than filesystem-block accounting.
estimateCheckoutBytes :: OsPath -> IO Integer
estimateCheckoutBytes path = walk (unsafeToFilePath path)
  where
    walk file = do
        linked <- Directory.pathIsSymbolicLink file
        if linked then pure 0 else do
            directory <- Directory.doesDirectoryExist file
            if directory then do
                children <- Directory.listDirectory file
                sum <$> mapM (walk . (file FilePath.</>)) children
            else Directory.getFileSize file

-- | Bounded snapshot-backed cleanup. The second argument is inactivity days.
cleanupStaleWorktrees
    :: OsPath
    -> Int
    -> [OsPath]
    -> IO WorktreeCleanupReport
cleanupStaleWorktrees root days protected = gcWorktrees root days False protected

tryExclusiveWorktreeLease
    :: OsPath
    -> OsPath
    -> IO (Either Text (Maybe WorktreeLease))
tryExclusiveWorktreeLease root candidate = do
    let lockPath = worktreeLeasePath root candidate
    result <- tryAny do
        linked <- pathIsSymbolicLink root
        when linked (ioError (userError "symlinked managed worktree root"))
        createDirectoryIfMissing True (takeDirectory lockPath)
        FileLock.tryLockFile
            (unsafeToFilePath lockPath)
            FileLock.Exclusive
    pure case result of
        Left exception ->
            Left
                ("failed to lock stale worktree: "
                    <> exceptionText exception)
        Right Nothing -> Right Nothing
        Right (Just lock) -> Right (Just (WorktreeLease lock Nothing))

managedWorktreePath :: OsPath -> OsPath -> Maybe OsPath
managedWorktreePath rawRoot rawPath =
    let root = normalise rawRoot
        path = normalise rawPath
        rootParts = splitDirectories root
        pathParts = splitDirectories path
    in case drop (length rootParts) pathParts of
        repository : checkout : _
            | rootParts `isPrefixOf` pathParts
            , isManagedWorktreeName checkout ->
                Just (root </> repository </> checkout)
        _ -> Nothing

worktreeLeasePath :: OsPath -> OsPath -> OsPath
worktreeLeasePath root managed =
    root
        </> unsafeEncodeUtf ".locks"
        </> takeFileName (takeDirectory managed)
        </> (takeFileName managed <> unsafeEncodeUtf ".lock")

isManagedWorktreeName :: OsPath -> Bool
isManagedWorktreeName path = case managedWorktreeDay path of
    Just _ -> True
    Nothing -> False

managedWorktreeDay :: OsPath -> Maybe Day
managedWorktreeDay path =
    case unsafeToFilePath path of
        year1 : year2 : year3 : year4 : '-' :
                month1 : month2 : '-' : day1 : day2 : '-' : suffix ->
            let date =
                    [ year1, year2, year3, year4, '-'
                    , month1, month2, '-', day1, day2
                    ]
            in if length suffix == 8 && all isHexDigit suffix
                then parseTimeM True defaultTimeLocale "%Y-%m-%d" date
                else Nothing
        _ -> Nothing

cleanupFailure :: OsPath -> SomeException -> WorktreeCleanupReport
cleanupFailure path exception =
    mempty
        { cleanupFailures =
            [(path, Text.pack (displayException exception))]
        }

exceptionText :: SomeException -> Text
exceptionText = Text.pack . displayException

pathText :: OsPath -> Text
pathText = Text.pack . unsafeToFilePath

addUnique
    :: OsPath
    -> OsPath
    -> OsPath
    -> Day
    -> Integer
    -> Maybe Text
    -> Int
    -> ExceptT Text IO OsPath
addUnique repo root repoName day start base attempt
    | attempt >= 32 =
        throwE "could not pick a unique worktree path"
    | otherwise = do
        let path = worktreePath root repoName day (hex8 (start + fromIntegral attempt))
        exists <- lift (doesPathExist path)
        if exists
            then addUnique repo root repoName day start base (attempt + 1)
            else do
                let branch = unsafeToFilePath (takeFileName path)
                    addArgs = case base of
                        Nothing ->
                            ["worktree", "add", unsafeToFilePath path]
                        Just commit ->
                            [ "worktree", "add", "-b", branch
                            , unsafeToFilePath path, Text.unpack commit
                            ]
                added <- lift $ mask \restore ->
                    restore (git repo addArgs)
                        `onException` cleanupWorktreeCandidate repo path
                case added of
                    Left err
                        | branchTaken err ->
                            addUnique
                                repo root repoName day start base (attempt + 1)
                        | otherwise -> do
                            lift (cleanupWorktreeCandidate repo path)
                            throwE err
                    Right _ -> pure path

cleanupWorktreeCandidate :: OsPath -> OsPath -> IO ()
cleanupWorktreeCandidate repo path = do
    exists <- doesPathExist path
    if not exists
        then pure ()
        else do
            _ <- git repo ["worktree", "remove", "--force", unsafeToFilePath path]
            _ <- tryAny (removePathForcibly path)
            _ <- git repo ["worktree", "prune"]
            _ <- git repo ["branch", "-D", unsafeToFilePath (takeFileName path)]
            pure ()

gitToplevel :: OsPath -> ExceptT Text IO OsPath
gitToplevel source = do
    path <- withExceptT
        (\err -> "--worktree requires a git repository (" <> Text.strip err <> ")")
        (ExceptT (git source ["rev-parse", "--show-toplevel"]))
    pure (unsafeEncodeUtf (Text.unpack (Text.strip path)))

gitRepositoryName :: OsPath -> ExceptT Text IO OsPath
gitRepositoryName repo = do
    path <- gitCommonDir repo
    pure $
        if takeFileName path == unsafeEncodeUtf ".git"
            then takeFileName (takeDirectory path)
            else takeFileName path

gitCommonDir :: OsPath -> ExceptT Text IO OsPath
gitCommonDir repo = do
    commonDir <- ExceptT $
        git repo ["rev-parse", "--path-format=absolute", "--git-common-dir"]
    pure (unsafeEncodeUtf (Text.unpack (Text.strip commonDir)))

-- | Serialize mutations of Git's shared worktree administration directory.
-- Fetches use private refs and can remain concurrent, but 'git worktree add'
-- and 'remove' update the common @.git/worktrees@ state.
withGitWorktreeLock
    :: OsPath
    -> ExceptT Text IO a
    -> ExceptT Text IO a
withGitWorktreeLock repo action = do
    commonDir <- gitCommonDir repo
    ExceptT $ withGitWorktreeLockAt commonDir (runExceptT action)

withGitWorktreeLockAt :: OsPath -> IO a -> IO a
withGitWorktreeLockAt commonDir action =
    FileLock.withFileLock
        (unsafeToFilePath
            (commonDir </> unsafeEncodeUtf "haskell-agent-worktree.lock"))
        FileLock.Exclusive
        (const action)

-- | Fetch and return the commit at the selected remote's advertised default
-- branch, or use the local @HEAD@ when the repository has no remotes. The
-- current branch's configured remote wins, followed by conventional @upstream@
-- and @origin@ names, then a sole remaining remote.
fetchLatestUpstream
    :: (WorktreeProgress -> IO ())
    -> OsPath
    -> ExceptT Text IO (Maybe Text)
fetchLatestUpstream report repo = do
    selectUpstreamRemote repo >>= \case
        Nothing -> pure Nothing
        Just remote -> do
            lift (report (WorktreeCheckingRemote remote))
            remoteHead <- remoteDefaultBranch repo remote
            lift (report (WorktreeFetchingRemote remote remoteHead))
            localRef <- lift freshFetchRef
            commit <-
                ExceptT $
                    runExceptT (fetchIntoRef repo remote remoteHead localRef)
                        `finally` cleanupFetchRef repo localRef
            pure (Just commit)

fetchIntoRef :: OsPath -> Text -> Text -> Text -> ExceptT Text IO Text
fetchIntoRef repo remote remoteHead localRef = do
    let refspec = remoteHead <> ":" <> localRef
        context action err =
            "failed to " <> action <> " from git remote "
                <> quote remote <> ": " <> Text.strip err
    -- An empty refmap prevents Git from also updating the configured
    -- remote-tracking ref, which would reintroduce a shared ref-lock race.
    void $
        withExceptT (context "fetch the latest default branch") $
            ExceptT $
                git repo
                    [ "fetch"
                    , "--no-tags"
                    , "--no-write-fetch-head"
                    , "--refmap="
                    , Text.unpack remote
                    , Text.unpack refspec
                    ]
    commit <-
        withExceptT (context "resolve the fetched default branch") $
            ExceptT $
                git repo
                    [ "rev-parse"
                    , "--verify"
                    , Text.unpack (localRef <> "^{commit}")
                    ]
    pure (Text.strip commit)

freshFetchRef :: IO Text
freshFetchRef = do
    bytes <- ByteString.unpack <$> getEntropy 16
    pure $
        "refs/haskell-agent/worktree-fetches/"
            <> Text.pack (concatMap hexByte bytes)
  where
    hexByte byte =
        let encoded = showHex byte ""
        in replicate (2 - length encoded) '0' <> encoded

cleanupFetchRef :: OsPath -> Text -> IO ()
cleanupFetchRef repo localRef =
    void $
        tryAny $
            git repo ["update-ref", "-d", Text.unpack localRef]

selectUpstreamRemote :: OsPath -> ExceptT Text IO (Maybe Text)
selectUpstreamRemote repo = do
    output <- ExceptT (git repo ["remote"])
    let remotes = filter (not . Text.null) (map Text.strip (Text.lines output))
    configured <- lift (configuredBranchRemote repo)
    case
        listToMaybe
            [ remote
            | remote <- maybe [] pure configured <> ["upstream", "origin"]
            , remote `elem` remotes
            ]
        <|> case remotes of
            [remote] -> Just remote
            _ -> Nothing
      of
        Just remote -> pure (Just remote)
        Nothing
            | null remotes -> pure Nothing
            | otherwise ->
                throwE
                    ( "could not choose an upstream git remote; configure the "
                        <> "current branch's remote or name one 'upstream' or 'origin'"
                    )

configuredBranchRemote :: OsPath -> IO (Maybe Text)
configuredBranchRemote repo =
    git repo ["branch", "--show-current"] >>= \case
        Right rawBranch
            | not (Text.null (Text.strip rawBranch)) ->
                git repo
                    [ "config"
                    , "--get"
                    , "branch." <> Text.unpack (Text.strip rawBranch) <> ".remote"
                    ] >>= \case
                        Right rawRemote ->
                            let remote = Text.strip rawRemote
                            in pure $
                                if Text.null remote || remote == "."
                                    then Nothing
                                    else Just remote
                        Left _ -> pure Nothing
        _ -> pure Nothing

remoteDefaultBranch :: OsPath -> Text -> ExceptT Text IO Text
remoteDefaultBranch repo remote = do
    output <-
        withExceptT
            (\err ->
                "failed to inspect git remote " <> quote remote
                    <> ": " <> Text.strip err)
            (ExceptT
                (git repo
                    [ "ls-remote"
                    , "--symref"
                    , Text.unpack remote
                    , "HEAD"
                    ]))
    case
        [ ref
        | line <- Text.lines output
        , ["ref:", ref, "HEAD"] <- [Text.words line]
        , "refs/heads/" `Text.isPrefixOf` ref
        ]
      of
        ref : _ -> pure ref
        [] ->
            throwE
                ( "git remote " <> quote remote
                    <> " did not advertise a default branch"
                )

quote :: Text -> Text
quote value = "'" <> value <> "'"

git :: OsPath -> [String] -> IO (Either Text Text)
git dir args = do
    (code, out, err) <-
        readCreateProcessWithExitCode
            (proc "git" args) { cwd = Just (unsafeToFilePath dir) }
            ""
    case code of
        ExitSuccess -> pure (Right (Text.pack out))
        ExitFailure _ ->
            pure $ Left $
                let stderrText = Text.strip (Text.pack err)
                    stdoutText = Text.strip (Text.pack out)
                    message
                        | Text.null stderrText = stdoutText
                        | otherwise = stderrText
                in if Text.null message
                    then "git " <> Text.pack (unwords args) <> " failed"
                    else message

branchTaken :: Text -> Bool
branchTaken err =
    "already used by worktree" `Text.isInfixOf` err
        || "already checked out" `Text.isInfixOf` err
        || "already exists" `Text.isInfixOf` err

posixMicros :: UTCTime -> Integer
posixMicros t =
    floor (nominalDiffTimeToSeconds (utcTimeToPOSIXSeconds t) * 1000000)

hex8 :: Integer -> String
hex8 n =
    let s = showHex (n `mod` 0x100000000) ""
    in replicate (8 - length s) '0' <> s

formatDay :: Day -> String
formatDay = formatTime defaultTimeLocale "%Y-%m-%d"
