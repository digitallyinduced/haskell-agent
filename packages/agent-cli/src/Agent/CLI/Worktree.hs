-- | Create isolated git worktrees under @~/.haskell-agent/worktrees@.
module Agent.CLI.Worktree
    ( createWorktree
    , createWorktreeWithFetch
    , createManagedWorktree
    , createManagedWorktreeWithProgress
    , createManagedWorktreeFromConfigWithProgress
    , removeWorktree
    , cleanupStaleWorktrees
    , defaultWorktreeKeepCount
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
import Control.Applicative ((<|>))
import Control.Exception.Safe
    ( SomeException
    , displayException
    , finally
    , mask
    , onException
    , tryAny
    )
import Control.Monad (filterM, foldM, void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
    ( ExceptT(..)
    , runExceptT
    , throwE
    , withExceptT
    )
import qualified Data.ByteString as ByteString
import Data.Char (isHexDigit)
import Data.List (isPrefixOf, sortOn)
import Data.Maybe (listToMaybe)
import Data.Ord (Down(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime(..), getCurrentTime, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Numeric (showHex)
import System.Directory.OsPath
    ( createDirectoryIfMissing
    , doesDirectoryExist
    , doesPathExist
    , listDirectory
    , pathIsSymbolicLink
    , removePathForcibly
    )
import System.Entropy (getEntropy)
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

-- | Match Codex Desktop's default per-repository retention. Worktrees beyond
-- this count are only removed when they are inactive, clean, and their HEAD is
-- reachable from another branch or tag.
defaultWorktreeKeepCount :: Int
defaultWorktreeKeepCount = 15

data WorktreeCleanupReport = WorktreeCleanupReport
    { cleanupRemoved :: ![OsPath]
    , cleanupFailures :: ![(OsPath, Text)]
    }
    deriving (Eq, Show)

instance Semigroup WorktreeCleanupReport where
    left <> right = WorktreeCleanupReport
        { cleanupRemoved = left.cleanupRemoved <> right.cleanupRemoved
        , cleanupFailures = left.cleanupFailures <> right.cleanupFailures
        }

instance Monoid WorktreeCleanupReport where
    mempty = WorktreeCleanupReport [] []

newtype WorktreeLease = WorktreeLease FileLock.FileLock

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
createManagedWorktreeFromConfigWithProgress report config home source =
    createWorktreeWithFetchProgress
        report
        config.configWorktree.worktreeFetchLatestUpstream
        source
        (worktreeRoot home)

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
acquireWorktreeLease root path =
    case managedWorktreePath root path of
        Nothing -> pure (Right Nothing)
        Just managed -> do
            let lockPath = worktreeLeasePath root managed
            result <- tryAny do
                createDirectoryIfMissing True (takeDirectory lockPath)
                FileLock.tryLockFile
                    (unsafeToFilePath lockPath)
                    FileLock.Shared
            pure case result of
                Left exception ->
                    Left
                        ("failed to lease managed worktree "
                            <> pathText managed
                            <> ": "
                            <> Text.pack (displayException exception))
                Right Nothing ->
                    Left
                        ("managed worktree is being cleaned up: "
                            <> pathText managed)
                Right (Just lock) -> Right (Just (WorktreeLease lock))

releaseWorktreeLease :: WorktreeLease -> IO ()
releaseWorktreeLease (WorktreeLease lock) = do
    _ <- tryAny (FileLock.unlockFile lock)
    pure ()

-- | Remove old managed worktrees without risking unique Git work.
--
-- Retention is applied per repository directory. A candidate must use our
-- generated path and branch name, be a linked worktree with no tracked or
-- untracked changes, have no active lease, and have a HEAD reachable from
-- another local branch, remote-tracking branch, or tag. Removal deliberately
-- omits @--force@. The generated branch is then deleted with @-d@, leaving it
-- intact without treating that data-safe fallback as a cleanup failure
-- whenever Git's own merged-branch check is stricter than ours.
-- Worktrees created on the current UTC day are never considered stale; this
-- also closes the interval between checkout creation and lease acquisition.
cleanupStaleWorktrees
    :: OsPath
    -> Int
    -> [OsPath]
    -> IO WorktreeCleanupReport
cleanupStaleWorktrees root requestedKeep protected = do
    exists <- doesDirectoryExist root
    if not exists
        then pure mempty
        else do
            today <- utctDay <$> getCurrentTime
            let cleanupLockPath =
                    root </> unsafeEncodeUtf ".cleanup.lock"
            lockResult <- tryAny $
                FileLock.tryLockFile
                    (unsafeToFilePath cleanupLockPath)
                    FileLock.Exclusive
            case lockResult of
                Left exception ->
                    pure $ cleanupFailure root exception
                Right Nothing ->
                    -- Another process is already doing the same best-effort GC.
                    pure mempty
                Right (Just lock) ->
                    runCleanup today `finally` FileLock.unlockFile lock
  where
    keep = max 1 requestedKeep
    protectedManaged =
        [ normalise path
        | path <- protected
        ]

    runCleanup today = do
        discovered <- discoverStaleCandidates root keep today
        case discovered of
            Left failures ->
                pure mempty { cleanupFailures = failures }
            Right (candidates, discoveryFailures) -> do
                cleaned <- foldM cleanupOne mempty candidates
                pure $
                    cleaned
                        <> mempty { cleanupFailures = discoveryFailures }

    cleanupOne report candidate
        | any (isUnderWorktreeRoot candidate) protectedManaged =
            pure report
        | otherwise = do
            result <- tryAny (cleanupCandidate root candidate)
            pure $ report <> case result of
                Left exception -> cleanupFailure candidate exception
                Right candidateReport -> candidateReport

discoverStaleCandidates
    :: OsPath
    -> Int
    -> Day
    -> IO (Either [(OsPath, Text)] ([OsPath], [(OsPath, Text)]))
discoverStaleCandidates root keep today = do
    listed <- tryAny (listDirectory root)
    case listed of
        Left exception ->
            pure (Left [(root, exceptionText exception)])
        Right entries ->
            Right <$> foldM discoverRepository ([], []) entries
  where
    discoverRepository (candidates, failures) entry = do
        let repository = root </> entry
        isDirectory <- doesDirectoryExist repository
        if not isDirectory
            then pure (candidates, failures)
            else do
                listed <- tryAny (listDirectory repository)
                case listed of
                    Left exception ->
                        pure
                            ( candidates
                            , failures <> [(repository, exceptionText exception)]
                            )
                    Right children -> do
                        directories <- filterM
                            (doesDirectoryExist . (repository </>))
                            children
                        let managed =
                                sortOn
                                    ( Down
                                        . unsafeToFilePath
                                        . takeFileName
                                        . fst
                                    )
                                    [ (repository </> child, day)
                                    | child <- directories
                                    , Just day <- [managedWorktreeDay child]
                                    ]
                            stale =
                                [ path
                                | (path, day) <- drop keep managed
                                , day < today
                                ]
                        pure (candidates <> stale, failures)

cleanupCandidate :: OsPath -> OsPath -> IO WorktreeCleanupReport
cleanupCandidate root candidate = do
    leaseResult <- tryExclusiveWorktreeLease root candidate
    case leaseResult of
        Left err ->
            pure mempty
                { cleanupFailures = [(candidate, err)]
                }
        Right Nothing ->
            pure mempty
        Right (Just lease) ->
            cleanupWithLease `finally` releaseWorktreeLease lease
  where
    cleanupWithLease = do
        hasGitMetadata <-
            doesPathExist (candidate </> unsafeEncodeUtf ".git")
        -- A generated-looking directory can survive an interrupted
        -- @git worktree remove@ after its .git marker is gone. Without
        -- that ownership marker, preserve it rather than warning on
        -- @git rev-parse@ or recursively deleting arbitrary files.
        if hasGitMetadata
            then cleanupGitWorktree
            else pure mempty

    cleanupGitWorktree =
        inspectCleanupCandidate candidate >>= \case
            Left err ->
                pure mempty
                    { cleanupFailures = [(candidate, err)]
                    }
            Right Nothing ->
                pure mempty
            Right (Just (commonDir, branch)) ->
                withGitWorktreeLockAt commonDir $
                    git commonDir
                        [ "worktree"
                        , "remove"
                        , unsafeToFilePath candidate
                        ] >>= \case
                            Left err ->
                                pure mempty
                                    { cleanupFailures = [(candidate, err)]
                                    }
                            Right _ -> do
                                -- The worktree is the resource governed by the
                                -- retention policy. Branch deletion is best
                                -- effort: @-d@ can reject a branch that is known
                                -- to be reachable from another ref but is not
                                -- merged into the repository's current branch.
                                -- Retaining it is safe and should not surface as
                                -- a startup warning.
                                _ <- git commonDir
                                    [ "branch"
                                    , "-d"
                                    , "--"
                                    , Text.unpack branch
                                    ]
                                pure WorktreeCleanupReport
                                    { cleanupRemoved = [candidate]
                                    , cleanupFailures = []
                                    }

inspectCleanupCandidate
    :: OsPath
    -> IO (Either Text (Maybe (OsPath, Text)))
inspectCleanupCandidate candidate = runExceptT do
    candidateLink <- lift (pathIsSymbolicLink candidate)
    repositoryLink <- lift (pathIsSymbolicLink (takeDirectory candidate))
    if candidateLink || repositoryLink
        then pure Nothing
        else do
            topLevel <- Text.strip <$> ExceptT
                (git candidate
                    [ "rev-parse"
                    , "--path-format=absolute"
                    , "--show-toplevel"
                    ])
            let topLevelPath = unsafeEncodeUtf (Text.unpack topLevel)
            if not
                (equalFilePath
                    (normalise candidate)
                    (normalise topLevelPath))
                then pure Nothing
                else do
                    gitDir <- Text.strip <$> ExceptT
                        (git candidate
                            [ "rev-parse"
                            , "--path-format=absolute"
                            , "--git-dir"
                            ])
                    commonDirText <- Text.strip <$> ExceptT
                        (git candidate
                            [ "rev-parse"
                            , "--path-format=absolute"
                            , "--git-common-dir"
                            ])
                    if gitDir == commonDirText
                        then pure Nothing
                        else do
                            branch <- Text.strip <$> ExceptT
                                (git candidate
                                    ["branch", "--show-current"])
                            let expected = Text.pack
                                    (unsafeToFilePath
                                        (takeFileName candidate))
                            if Text.null branch || branch /= expected
                                then pure Nothing
                                else do
                                    status <- ExceptT $
                                        git candidate
                                            [ "status"
                                            , "--porcelain=v1"
                                            , "--untracked-files=all"
                                            , "--ignore-submodules=none"
                                            ]
                                    if not (Text.null status)
                                        then pure Nothing
                                        else do
                                            refs <- ExceptT $
                                                git candidate
                                                    [ "for-each-ref"
                                                    , "--format=%(refname)"
                                                    , "--contains=HEAD"
                                                    , "refs/heads"
                                                    , "refs/remotes"
                                                    , "refs/tags"
                                                    ]
                                            let ownRef =
                                                    "refs/heads/" <> branch
                                                otherRefs =
                                                    filter
                                                        (\ref ->
                                                            not (Text.null ref)
                                                                && ref /= ownRef)
                                                        (Text.lines refs)
                                            pure $
                                                if null otherRefs
                                                    then Nothing
                                                    else Just
                                                        ( unsafeEncodeUtf
                                                            (Text.unpack
                                                                commonDirText)
                                                        , branch
                                                        )

tryExclusiveWorktreeLease
    :: OsPath
    -> OsPath
    -> IO (Either Text (Maybe WorktreeLease))
tryExclusiveWorktreeLease root candidate = do
    let lockPath = worktreeLeasePath root candidate
    result <- tryAny do
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
        Right (Just lock) -> Right (Just (WorktreeLease lock))

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
