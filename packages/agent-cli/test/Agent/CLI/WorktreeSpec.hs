module Agent.CLI.WorktreeSpec (spec) where

import Agent.CLI.Config
import Agent.CLI.Worktree
import Agent.CLI.Worktree.Registry
import Control.Concurrent.Async (mapConcurrently)
import Control.Exception.Safe (bracket)
import Control.Monad (void)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (dropWhileEnd, isInfixOf)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime, NominalDiffTime, getCurrentTime, addUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import qualified System.Directory as Directory
import System.Directory.OsPath
    ( createDirectoryIfMissing
    , doesDirectoryExist
    , doesFileExist
    )
import System.Exit (ExitCode(..))
import qualified System.FilePath as FilePath
import System.OsPath
    ( OsPath
    , addTrailingPathSeparator
    , decodeUtf
    , takeDirectory
    , takeFileName
    , unsafeEncodeUtf
    , (</>)
    )
import System.Posix.Temp (mkdtemp)
import System.Process (CreateProcess(..), proc, readCreateProcessWithExitCode)
import Test.Hspec

fromFilePath = unsafeEncodeUtf
toFilePath path = either (error . show) id (decodeUtf path)

spec :: Spec
spec = describe "Agent.CLI.Worktree" do
    describe "worktreePath" do
        it "builds root/repo/YYYY-MM-DD-hex" do
            worktreePath
                (fromFilePath "/tmp/root")
                (fromFilePath "my-repo")
                (fromGregorian 2026 8 20)
                "abcd1234"
                `shouldBe` fromFilePath "/tmp/root/my-repo/2026-08-20-abcd1234"

    describe "worktreeRoot" do
        it "is ~/.haskell-agent/worktrees" do
            worktreeRoot (fromFilePath "/home/marc")
                `shouldBe` fromFilePath "/home/marc/.haskell-agent/worktrees"

    describe "isUnderWorktreeRoot" do
        it "matches the root and its subdirectories" do
            let root = fromFilePath "/home/marc/.haskell-agent/worktrees"
            isUnderWorktreeRoot root root `shouldBe` True
            isUnderWorktreeRoot root
                (root </> fromFilePath "haskell-agent"
                    </> fromFilePath "2026-08-20-abcd")
                `shouldBe` True
            isUnderWorktreeRoot
                (addTrailingPathSeparator root)
                (root </> fromFilePath "haskell-agent"
                    </> fromFilePath "2026-08-20-abcd")
                `shouldBe` True
            isUnderWorktreeRoot root (fromFilePath "/home/marc/src/haskell-agent")
                `shouldBe` False
            isUnderWorktreeRoot root (root <> fromFilePath "-extra") `shouldBe` False

    describe "createWorktree" do
        it "adds a worktree under the injected root on a new branch" $
            withTempGitRepo \repo ->
            withTempDir "agent-home-" \home -> do
                path <- expectRight =<< createWorktree repo (worktreeRoot home)
                let parent = worktreeRoot home </> takeFileName repo
                isUnderWorktreeRoot parent path `shouldBe` True
                toFilePath (takeFileName path) `shouldSatisfy` ("-" `isInfixOf`)
                inside <- git path ["rev-parse", "--is-inside-work-tree"]
                inside `shouldBe` "true"
                sourceBranch <- git repo ["rev-parse", "--abbrev-ref", "HEAD"]
                worktreeBranch <- git path ["rev-parse", "--abbrev-ref", "HEAD"]
                worktreeBranch `shouldNotBe` sourceBranch
                worktreeBranch `shouldBe` toFilePath (takeFileName path)
                sourceHead <- git repo ["rev-parse", "HEAD"]
                git path ["rev-parse", "HEAD"] `shouldReturn` sourceHead

        it "rejects a directory that is not a git checkout" $
            withTempDir "agent-not-git-" \dir -> do
                let root = dir </> fromFilePath "worktrees"
                result <- createWorktree dir root
                case result of
                    Left err -> err `shouldSatisfy` Text.isInfixOf "--worktree"
                    Right path ->
                        expectationFailure ("expected failure, got " <> toFilePath path)
                doesDirectoryExist root `shouldReturn` False

        it "uses local HEAD when default fetching has no remote" $
            withTempGitRepo \repo ->
            withTempDir "agent-home-" \home -> do
                sourceHead <- git repo ["rev-parse", "HEAD"]
                path <- expectRight =<< createManagedWorktree home repo
                git path ["rev-parse", "HEAD"] `shouldReturn` sourceHead

        it "fails closed when a configured remote cannot be fetched" $
            withTempGitRepo \repo ->
            withTempDir "agent-home-" \home -> do
                _ <- git repo
                    [ "remote"
                    , "add"
                    , "origin"
                    , toFilePath (repo </> fromFilePath "missing.git")
                    ]
                let root = worktreeRoot home
                result <- createWorktreeWithFetch True repo root
                result `shouldSatisfy` \case
                    Left err ->
                        "failed to inspect git remote" `Text.isInfixOf` err
                    Right _ -> False
                doesDirectoryExist root `shouldReturn` False

        it "fetches and branches from the remote's latest default commit by default" $
            withTempRemoteRepo \repo updater ->
            withTempDir "agent-home-" \home -> do
                _ <- git repo
                    ["symbolic-ref", "refs/remotes/origin/HEAD", "refs/remotes/origin/master"]
                stale <- git repo ["rev-parse", "refs/remotes/origin/master"]
                latest <- git updater ["rev-parse", "HEAD"]
                stale `shouldNotBe` latest

                progressRef <- newIORef []
                path <- expectRight
                    =<< createManagedWorktreeWithProgress
                        (\progress ->
                            modifyIORef' progressRef (<> [progress]))
                        home
                        repo

                git path ["rev-parse", "HEAD"] `shouldReturn` latest
                git repo ["rev-parse", "refs/remotes/origin/master"]
                    `shouldReturn` stale
                readFile (toFilePath (path </> fromFilePath "README"))
                    `shouldReturn` "latest\n"
                doesFileExist (path </> fromFilePath "LOCAL")
                    `shouldReturn` False
                git path ["rev-parse", "--abbrev-ref", "HEAD"]
                    `shouldReturn` toFilePath (takeFileName path)
                temporaryFetchRefs repo `shouldReturn` ""
                progress <- readIORef progressRef
                progress `shouldBe`
                    [ WorktreeInspectingRepository
                    , WorktreeFetchingRemote "origin" "refs/heads/master"
                    , WorktreeCreating
                    ]
                map worktreeProgressMessage progress `shouldBe`
                    [ "Inspecting Git repository…"
                    , "Fetching latest from origin/master…"
                    , "Creating worktree…"
                    ]

        it "discovers a missing default-branch reference and reuses it on the next creation" $
            withTempRemoteRepo \repo updater ->
            withTempDir "agent-home-" \home -> do
                latest <- git updater ["rev-parse", "HEAD"]
                progressRef <- newIORef []
                let report progress = modifyIORef' progressRef (<> [progress])
                first <- expectRight
                    =<< createManagedWorktreeWithProgress report home repo
                git first ["rev-parse", "HEAD"] `shouldReturn` latest
                git repo ["symbolic-ref", "refs/remotes/origin/HEAD"]
                    `shouldReturn` "refs/remotes/origin/master"
                readIORef progressRef `shouldReturn`
                    [ WorktreeInspectingRepository
                    , WorktreeCheckingRemote "origin"
                    , WorktreeFetchingRemote "origin" "refs/heads/master"
                    , WorktreeCreating
                    ]
                modifyIORef' progressRef (const [])
                second <- expectRight
                    =<< createManagedWorktreeWithProgress report home repo
                git second ["rev-parse", "HEAD"] `shouldReturn` latest
                readIORef progressRef `shouldReturn`
                    [ WorktreeInspectingRepository
                    , WorktreeFetchingRemote "origin" "refs/heads/master"
                    , WorktreeCreating
                    ]
                temporaryFetchRefs repo `shouldReturn` ""

        it "rediscovers the default branch when the cached branch was deleted" $
            withTempRemoteRepo \repo updater ->
            withTempDir "agent-home-" \home -> do
                _ <- git repo
                    ["symbolic-ref", "refs/remotes/origin/HEAD", "refs/remotes/origin/master"]
                remotePath <- fromFilePath <$> git repo ["remote", "get-url", "origin"]
                _ <- git updater ["push", "origin", "HEAD:refs/heads/main"]
                _ <- git remotePath ["symbolic-ref", "HEAD", "refs/heads/main"]
                _ <- git updater ["push", "origin", "--delete", "master"]
                latest <- git updater ["rev-parse", "HEAD"]
                progressRef <- newIORef []
                path <- expectRight =<< createManagedWorktreeWithProgress
                    (\progress -> modifyIORef' progressRef (<> [progress]))
                    home repo
                git path ["rev-parse", "HEAD"] `shouldReturn` latest
                git repo ["symbolic-ref", "refs/remotes/origin/HEAD"]
                    `shouldReturn` "refs/remotes/origin/main"
                readIORef progressRef `shouldReturn`
                    [ WorktreeInspectingRepository
                    , WorktreeFetchingRemote "origin" "refs/heads/master"
                    , WorktreeCheckingRemote "origin"
                    , WorktreeFetchingRemote "origin" "refs/heads/main"
                    , WorktreeCreating
                    ]
                temporaryFetchRefs repo `shouldReturn` ""

        it "rejects a cached default-branch reference pointing outside the selected remote" $
            withTempRemoteRepo \repo updater ->
            withTempDir "agent-home-" \home -> do
                _ <- git repo
                    ["symbolic-ref", "refs/remotes/origin/HEAD", "refs/remotes/other/master"]
                latest <- git updater ["rev-parse", "HEAD"]
                progressRef <- newIORef []
                path <- expectRight =<< createManagedWorktreeWithProgress
                    (\progress -> modifyIORef' progressRef (<> [progress]))
                    home repo
                git path ["rev-parse", "HEAD"] `shouldReturn` latest
                git repo ["symbolic-ref", "refs/remotes/origin/HEAD"]
                    `shouldReturn` "refs/remotes/origin/master"
                readIORef progressRef `shouldReturn`
                    [ WorktreeInspectingRepository
                    , WorktreeCheckingRemote "origin"
                    , WorktreeFetchingRemote "origin" "refs/heads/master"
                    , WorktreeCreating
                    ]

        it "does not rediscover the cached default branch after an unrelated fetch failure" $
            withTempRemoteRepo \repo _ ->
            withTempDir "agent-home-" \home -> do
                _ <- git repo
                    ["symbolic-ref", "refs/remotes/origin/HEAD", "refs/remotes/origin/master"]
                _ <- git repo
                    ["remote", "set-url", "origin", toFilePath (repo </> fromFilePath "missing.git")]
                progressRef <- newIORef []
                result <- createManagedWorktreeWithProgress
                    (\progress -> modifyIORef' progressRef (<> [progress]))
                    home repo
                result `shouldSatisfy` \case
                    Left err -> "failed to fetch" `Text.isInfixOf` err
                    Right _ -> False
                readIORef progressRef `shouldReturn`
                    [ WorktreeInspectingRepository
                    , WorktreeFetchingRemote "origin" "refs/heads/master"
                    ]
                git repo ["symbolic-ref", "refs/remotes/origin/HEAD"]
                    `shouldReturn` "refs/remotes/origin/master"
                temporaryFetchRefs repo `shouldReturn` ""
                doesDirectoryExist (worktreeRoot home) `shouldReturn` False

        it "uses local HEAD when latest-upstream fetching is disabled" $
            withTempRemoteRepo \repo updater ->
            withTempDir "agent-home-" \home -> do
                local <- git repo ["rev-parse", "HEAD"]
                latest <- git updater ["rev-parse", "HEAD"]
                local `shouldNotBe` latest
                let config = defaultHarnessConfig
                        { configWorktree = defaultHarnessConfig.configWorktree
                            { worktreeFetchLatestUpstream = False
                            }
                        }
                saveHarnessConfig home config `shouldReturn` Right ()

                path <- expectRight =<< createManagedWorktree home repo

                git path ["rev-parse", "HEAD"] `shouldReturn` local
                doesFileExist (path </> fromFilePath "LOCAL")
                    `shouldReturn` True

        it "keeps startup worktree policy on its snapshot while later creation reloads config" $
            withTempRemoteRepo \repo updater ->
            withTempDir "agent-home-" \home -> do
                local <- git repo ["rev-parse", "HEAD"]
                latest <- git updater ["rev-parse", "HEAD"]
                local `shouldNotBe` latest
                let initialConfig = defaultHarnessConfig
                        { configWorktree = defaultHarnessConfig.configWorktree
                            { worktreeFetchLatestUpstream = False
                            }
                        }

                -- Model a config edit after startup captured its snapshot.
                saveHarnessConfig home initialConfig `shouldReturn` Right ()
                startupConfig <- loadHarnessConfig home >>= \case
                    Left err -> do
                        expectationFailure
                            ("failed to load startup config: "
                                <> Text.unpack err)
                        pure defaultHarnessConfig
                    Right config -> pure config
                saveHarnessConfig home defaultHarnessConfig
                    `shouldReturn` Right ()
                startupPath <- expectRight
                    =<< createManagedWorktreeFromConfigWithProgress
                        (const (pure ()))
                        startupConfig
                        home
                        repo
                git startupPath ["rev-parse", "HEAD"] `shouldReturn` local

                laterPath <- expectRight =<< createManagedWorktree home repo
                git laterPath ["rev-parse", "HEAD"] `shouldReturn` latest

        it "uses isolated fetch refs for concurrent worktree creation" $
            withTempRemoteRepo \repo updater ->
            withTempDir "agent-home-" \home -> do
                latest <- git updater ["rev-parse", "HEAD"]
                paths <-
                    mapM expectRight
                        =<< mapConcurrently
                            (\_ ->
                                createWorktreeWithFetch
                                    True
                                    repo
                                    (worktreeRoot home))
                            [1 :: Int .. 4]

                mapM_ (\path ->
                    git path ["rev-parse", "HEAD"] `shouldReturn` latest) paths
                temporaryFetchRefs repo `shouldReturn` ""

        it "creates two distinct worktrees for the same repo" $
            withTempGitRepo \repo ->
            withTempDir "agent-home-" \home -> do
                first <- expectRight =<< createWorktree repo (worktreeRoot home)
                second <- expectRight =<< createWorktree repo (worktreeRoot home)
                first `shouldNotBe` second
                doesDirectoryExist first `shouldReturn` True
                doesDirectoryExist second `shouldReturn` True

        it "keeps linked worktrees grouped under the original repository name" $
            withTempGitRepo \repo ->
            withTempDir "agent-home-" \home -> do
                first <- expectRight =<< createWorktree repo (worktreeRoot home)
                second <- expectRight =<< createWorktree first (worktreeRoot home)
                let parent = worktreeRoot home </> takeFileName repo
                isUnderWorktreeRoot parent second `shouldBe` True

        it "removes the worktree and its generated branch" $
            withTempGitRepo \repo ->
            withTempDir "agent-home-" \home -> do
                path <- expectRight =<< createWorktree repo (worktreeRoot home)
                let branch = toFilePath (takeFileName path)
                removeWorktree repo path `shouldReturn` Right ()
                doesDirectoryExist path `shouldReturn` False
                branches <- git repo ["branch", "--list", branch]
                branches `shouldBe` ""

    describe "cleanupStaleWorktrees" do
        it "never collects worktrees created on the current UTC day" $
            withTempGitRepo \repo ->
            withTempDir "agent-home-" \home -> do
                let root = worktreeRoot home
                day <- formatTime defaultTimeLocale "%Y-%m-%d"
                    <$> getCurrentTime
                first <- addManagedWorktree repo root (day <> "-00000001")
                second <- addManagedWorktree repo root (day <> "-00000002")

                report <- cleanupStaleWorktrees root 1 []

                report.cleanupRemoved `shouldBe` []
                doesDirectoryExist first `shouldReturn` True
                doesDirectoryExist second `shouldReturn` True

        it "never adopts legacy worktrees based on age or count without session provenance" $
            withTempGitRepo \repo ->
            withTempDir "agent-home-" \home -> do
                let root = worktreeRoot home
                older <- addManagedWorktree repo root "2026-08-20-00000001"
                newer <- addManagedWorktree repo root "2026-08-21-00000002"

                report <- cleanupStaleWorktrees root 1 []

                report.cleanupFailures `shouldBe` []
                report.cleanupRemoved `shouldBe` []
                report.cleanupRetained `shouldContain` [(older, "uncertain ownership: no saved-session provenance")]
                doesDirectoryExist older `shouldReturn` True
                doesDirectoryExist newer `shouldReturn` True
                git repo ["branch", "--list", "--format=%(refname:short)", "2026-08-20-00000001"]
                    `shouldReturn` "2026-08-20-00000001"

        it "retains unenrolled unique work even when another ref contains it" $
            withTempGitRepo \repo ->
            withTempDir "agent-home-" \home -> do
                let root = worktreeRoot home
                    branch = "2026-08-20-00000001"
                older <- addManagedWorktree repo root branch
                _ <- addManagedWorktree repo root "2026-08-21-00000002"
                writeFile
                    (toFilePath (older </> fromFilePath "README"))
                    "reachable elsewhere\n"
                _ <- git older ["add", "README"]
                _ <- git older ["commit", "-m", "reachable work"]
                _ <- git older ["branch", "retained-copy"]

                report <- cleanupStaleWorktrees root 1 []

                report.cleanupRemoved `shouldBe` []
                report.cleanupFailures `shouldBe` []
                doesDirectoryExist older `shouldReturn` True
                git repo ["branch", "--list", "--format=%(refname:short)", branch]
                    `shouldReturn` branch

        it "preserves stale worktrees with uncommitted files" $
            withTempGitRepo \repo ->
            withTempDir "agent-home-" \home -> do
                let root = worktreeRoot home
                older <- addManagedWorktree repo root "2026-08-20-00000001"
                _ <- addManagedWorktree repo root "2026-08-21-00000002"
                writeFile
                    (toFilePath (older </> fromFilePath "notes.txt"))
                    "keep me\n"

                report <- cleanupStaleWorktrees root 1 []

                report.cleanupRemoved `shouldBe` []
                report.cleanupFailures `shouldBe` []
                doesDirectoryExist older `shouldReturn` True

        it "preserves stale worktrees whose commit is not reachable elsewhere" $
            withTempGitRepo \repo ->
            withTempDir "agent-home-" \home -> do
                let root = worktreeRoot home
                older <- addManagedWorktree repo root "2026-08-20-00000001"
                _ <- addManagedWorktree repo root "2026-08-21-00000002"
                writeFile
                    (toFilePath (older </> fromFilePath "README"))
                    "unique\n"
                _ <- git older ["add", "README"]
                _ <- git older ["commit", "-m", "unique work"]

                report <- cleanupStaleWorktrees root 1 []

                report.cleanupRemoved `shouldBe` []
                report.cleanupFailures `shouldBe` []
                doesDirectoryExist older `shouldReturn` True

        it "preserves managed-looking directories without Git metadata" $
            withTempDir "agent-home-" \home -> do
                let root = worktreeRoot home
                    repository = root </> fromFilePath "repository"
                    older =
                        repository
                            </> fromFilePath "2026-08-20-00000001"
                    newer =
                        repository
                            </> fromFilePath "2026-08-21-00000002"
                    sentinel = older </> fromFilePath "unfinished-removal"
                createDirectoryIfMissing True older
                createDirectoryIfMissing True newer
                writeFile (toFilePath sentinel) "preserve me\n"

                report <- cleanupStaleWorktrees root 1 []

                report.cleanupRemoved `shouldBe` []
                report.cleanupFailures `shouldBe` []
                doesFileExist sentinel `shouldReturn` True

        it "preserves stale worktrees with an active shared lease" $
            withTempGitRepo \repo ->
            withTempDir "agent-home-" \home -> do
                let root = worktreeRoot home
                older <- addManagedWorktree repo root "2026-08-20-00000001"
                _ <- addManagedWorktree repo root "2026-08-21-00000002"
                lease <- acquireWorktreeLease root older >>= \case
                    Right (Just value) -> pure value
                    _ -> expectationFailure
                        "expected a managed worktree lease"
                        >> fail "missing worktree lease"

                fmap (.cleanupRemoved) (cleanupStaleWorktrees root 1 [])
                    `shouldReturn` []
                doesDirectoryExist older `shouldReturn` True

                releaseWorktreeLease lease
                fmap (.cleanupRemoved) (cleanupStaleWorktrees root 1 [])
                    `shouldReturn` []
                doesDirectoryExist older `shouldReturn` True

        it "preserves explicitly protected stale worktrees" $
            withTempGitRepo \repo ->
            withTempDir "agent-home-" \home -> do
                let root = worktreeRoot home
                older <- addManagedWorktree repo root "2026-08-20-00000001"
                _ <- addManagedWorktree repo root "2026-08-21-00000002"

                report <- cleanupStaleWorktrees
                    root
                    1
                    [older </> fromFilePath "nested/current-directory"]

                report.cleanupRemoved `shouldBe` []
                doesDirectoryExist older `shouldReturn` True

        it "ignores symlinked managed-looking candidates" $
            withTempGitRepo \repo ->
            withTempDir "agent-home-" \home -> do
                let root = worktreeRoot home
                    parent = root </> takeFileName repo
                    branch = "2026-08-20-00000001"
                    candidate = parent </> fromFilePath branch
                    actual =
                        home
                            </> fromFilePath "outside"
                            </> fromFilePath branch
                createDirectoryIfMissing True parent
                createDirectoryIfMissing True (home </> fromFilePath "outside")
                _ <- git repo
                    ["worktree", "add", "-b", branch, toFilePath actual]
                Directory.createDirectoryLink
                    (toFilePath actual)
                    (toFilePath candidate)
                _ <- addManagedWorktree
                    repo
                    root
                    "2026-08-21-00000002"

                report <- cleanupStaleWorktrees root 1 []

                report.cleanupRemoved `shouldBe` []
                doesDirectoryExist actual `shouldReturn` True

    describe "merged worktree inactivity" do
        it "uses the exact inclusive 24-hour inactivity boundary for incorporated HEADs" do
            now <- getCurrentTime
            worktreeInactive 7 True now (addUTCTime (-86400 + 0.001) now) `shouldBe` False
            worktreeInactive 7 True now (addUTCTime (-86400) now) `shouldBe` True
            worktreeInactive 7 True now (addUTCTime (-86400 - 0.001) now) `shouldBe` True
            worktreeInactive 7 True now (addUTCTime 1 now) `shouldBe` False

        it "uses configured inactivity for unproven HEADs without a commit-date clock" do
            now <- getCurrentTime
            worktreeInactive 7 False now (addUTCTime (-86400) now) `shouldBe` False
            worktreeInactive 7 False now (addUTCTime (-7 * 86400 + 0.001) now) `shouldBe` False
            worktreeInactive 7 False now (addUTCTime (-7 * 86400) now) `shouldBe` True
            worktreeInactive 14 False now (addUTCTime (-8 * 86400) now) `shouldBe` False
            worktreeInactive 14 True now (addUTCTime (-86400) now) `shouldBe` True

        mapM_ (\branch ->
            it ("recognizes a normal merge into the resolved " <> branch <> " default branch") $
                withMergedWorktree branch \_ root path -> do
                    activity <- savedActivityDaysAgo path 2
                    report <- gcWorktreesWithActivity activity root 7 True []
                    map fst report.cleanupEligible `shouldBe` [path]
                    report.cleanupRetained `shouldBe` []
                    readRecord root path `shouldReturn` Right Nothing
            ) ["master", "main", "release/stable"]

        it "retains a merged checkout with less than 24 hours of inactivity" $
            withMergedWorktree "main" \_ root path -> do
                now <- getCurrentTime
                let activity = pure (Right (Map.singleton path (Right (addUTCTime (-23 * 3600) now))))
                report <- gcWorktreesWithActivity activity root 7 True []
                report.cleanupEligible `shouldBe` []
                report.cleanupRetained `shouldBe` [(path, "recent activity")]

        it "new commits after the merged head disqualify the short expiry" $
            withMergedWorktree "main" \_ root path -> do
                _ <- git path ["commit", "--allow-empty", "-m", "after merge"]
                activity <- savedActivityDaysAgo path 2
                report <- gcWorktreesWithActivity activity root 7 True []
                report.cleanupEligible `shouldBe` []
                report.cleanupRetained `shouldBe` [(path, "recent activity")]

        it "an absent default-branch target keeps the normal expiry" $
            withMergedWorktree "main" \repo root path -> do
                _ <- git repo ["update-ref", "-d", "refs/remotes/origin/main"]
                activity <- savedActivityDaysAgo path 2
                report <- gcWorktreesWithActivity activity root 7 True []
                report.cleanupEligible `shouldBe` []
                report.cleanupRetained `shouldBe` [(path, "recent activity")]

        it "does not guess a default branch from local branch names" $
            withMergedWorktree "master" \repo root path -> do
                _ <- git repo ["symbolic-ref", "--delete", "refs/remotes/origin/HEAD"]
                activity <- savedActivityDaysAgo path 2
                report <- gcWorktreesWithActivity activity root 7 True []
                report.cleanupEligible `shouldBe` []
                report.cleanupRetained `shouldBe` [(path, "recent activity")]

        it "keeps the normal expiry when legacy grafts make ancestry uncertain" $
            withMergedWorktree "main" \repo root path -> do
                writeFile (toFilePath (repo </> fromFilePath ".git/info/grafts")) ""
                activity <- savedActivityDaysAgo path 2
                report <- gcWorktreesWithActivity activity root 7 True []
                report.cleanupEligible `shouldBe` []
                report.cleanupRetained `shouldBe` [(path, "recent activity")]

        it "does not mistake squash-equivalent content for an incorporated HEAD" $
            withTempGitRepo \repo ->
            withTempDir "agent-home-" \home -> do
                let root = worktreeRoot home
                _ <- git repo ["branch", "-M", "main"]
                path <- addManagedWorktree repo root "2026-08-20-00000001"
                writeFile (toFilePath (path </> fromFilePath "feature")) "feature\n"
                _ <- git path ["add", "feature"]
                _ <- git path ["commit", "-m", "feature"]
                _ <- git repo ["merge", "--squash", toFilePath (takeFileName path)]
                _ <- git repo ["commit", "-m", "squashed feature"]
                configureDefaultRef repo "main"
                activity <- savedActivityDaysAgo path 2
                report <- gcWorktreesWithActivity activity root 7 True []
                report.cleanupEligible `shouldBe` []
                report.cleanupRetained `shouldBe` [(path, "recent activity")]
                oldActivity <- savedActivityDaysAgo path 8
                oldReport <- gcWorktreesWithActivity oldActivity root 7 True []
                map fst oldReport.cleanupEligible `shouldBe` [path]

        it "snapshots dirty merged checkouts before collecting at two days" $
            withMergedWorktree "main" \_ root path -> do
                let file name = toFilePath (path </> fromFilePath name)
                writeFile (file "README") "staged\n"
                _ <- git path ["add", "README"]
                writeFile (file "README") "unstaged\n"
                writeFile (file "notes") "untracked\n"
                writeFile (file ".gitignore") "cache\n"
                writeFile (file "cache") "discarded\n"
                headBefore <- git path ["rev-parse", "HEAD"]
                activity <- savedActivityDaysAgo path 2
                report <- gcWorktreesWithActivity activity root 7 False []
                report.cleanupRemoved `shouldBe` [path]
                restoreManagedWorktree root path `shouldReturn` Right ()
                git path ["rev-parse", "HEAD"] `shouldReturn` headBefore
                git path ["show", ":README"] `shouldReturn` "staged"
                readFile (file "README") `shouldReturn` "unstaged\n"
                readFile (file "notes") `shouldReturn` "untracked\n"
                Directory.doesFileExist (file "cache") `shouldReturn` False

        it "keeps active merged checkouts even after 24 hours" $
            withMergedWorktree "main" \_ root path -> do
                activity <- savedActivityDaysAgo path 2
                bracket (acquireWorktreeLease root path) releaseLease $ \_ -> do
                    report <- gcWorktreesWithActivity activity root 7 True []
                    report.cleanupEligible `shouldBe` []
                    map snd report.cleanupRetained `shouldSatisfy` any (Text.isInfixOf "existing lock is busy")
                doesDirectoryExist path `shouldReturn` True

        it "keeps protected merged checkouts even after 24 hours" $
            withMergedWorktree "main" \_ root path -> do
                enrollWorktree root path `shouldReturn` Right ()
                now <- getCurrentTime
                modifyRecord root path (Right . fmap (\record ->
                    record { recordLastActivity = addUTCTime (-2 * 86400) now }))
                    `shouldReturn` Right ()
                protectWorktree root path True `shouldReturn` Right ()
                activity <- savedActivityDaysAgo path 2
                report <- gcWorktreesWithActivity activity root 7 False []
                report.cleanupRemoved `shouldBe` []
                report.cleanupRetained `shouldBe` [(path, "protected")]

        mapM_ (\readNumber ->
            it ("rechecks incorporated HEAD on activity read " <> show readNumber) $
                withMergedWorktree "main" \_ root path -> do
                    saved <- savedActivityDaysAgo path 2
                    reads <- newIORef (0 :: Int)
                    let activity = do
                            modifyIORef' reads (+1)
                            count <- readIORef reads
                            if count == readNumber
                                then void (git path ["commit", "--allow-empty", "-m", "concurrent new commit"])
                                else pure ()
                            saved
                    report <- gcWorktreesWithActivity activity root 7 False []
                    report.cleanupRemoved `shouldBe` []
                    report.cleanupRetained `shouldBe` [(path, "recent activity")]
                    doesDirectoryExist path `shouldReturn` True
            ) [2, 3]

    describe "snapshot-backed worktree GC" do
        it "simulates verified legacy adoption without registry or lock writes" $
            withTempGitRepo \repo ->
            withTempDir "agent-home-" \home -> do
                let root = worktreeRoot home
                path <- addManagedWorktree repo root "2026-08-20-00000001"
                now <- getCurrentTime
                let activity = pure (Right (Map.singleton path (Right (addUTCTime (-8 * 86400) now))))
                writeFile (toFilePath (path </> fromFilePath ".gitignore")) "cache\n"
                writeFile (toFilePath (path </> fromFilePath "cache")) (replicate 10000 'x')
                before <- git repo ["show-ref"]
                report <- gcWorktreesWithActivity activity root 7 True []
                report.cleanupRetained `shouldBe` []
                map fst report.cleanupEligible `shouldBe` [path]
                sum (map snd report.cleanupEligible) `shouldSatisfy` (>= 10000)
                readRecord root path `shouldReturn` Right Nothing
                doesDirectoryExist (root </> fromFilePath ".registry") `shouldReturn` False
                doesDirectoryExist (root </> fromFilePath ".locks") `shouldReturn` False
                doesDirectoryExist (root </> fromFilePath ".leases") `shouldReturn` False
                doesFileExist (repo </> fromFilePath ".git/haskell-agent-worktree.lock") `shouldReturn` False
                git repo ["show-ref"] `shouldReturn` before

        it "adopts and collects legacy dirty work with its original activity and restores it" $
            withTempGitRepo \repo ->
            withTempDir "agent-home-" \home -> do
                let root = worktreeRoot home
                path <- addManagedWorktree repo root "2026-08-20-00000001"
                now <- getCurrentTime
                let old = addUTCTime (-8 * 86400) now
                    activity = pure (Right (Map.singleton path (Right old)))
                    file name = toFilePath (path </> fromFilePath name)
                writeFile (file "README") "staged\n"
                _ <- git path ["add", "README"]
                writeFile (file "README") "unstaged\n"
                writeFile (file "notes") "untracked\n"
                writeFile (file ".gitignore") "cache\n"
                writeFile (file "cache") "excluded\n"
                report <- gcWorktreesWithActivity activity root 7 False []
                report.cleanupRemoved `shouldBe` [path]
                readRecord root path >>= \case
                    Right (Just record) -> do
                        record.recordLastActivity `shouldBe` old
                        record.recordState `shouldBe` "collected"
                    other -> expectationFailure (show other)
                restoreManagedWorktree root path `shouldReturn` Right ()
                git path ["show", ":README"] `shouldReturn` "staged"
                readFile (file "README") `shouldReturn` "unstaged\n"
                readFile (file "notes") `shouldReturn` "untracked\n"
                Directory.doesFileExist (file "cache") `shouldReturn` False

        it "retains legacy unknown activity and recent session activity without adoption" $
            withTempGitRepo \repo ->
            withTempDir "agent-home-" \home -> do
                let root = worktreeRoot home
                old <- addManagedWorktree repo root "2026-08-20-00000001"
                recent <- addManagedWorktree repo root "2026-08-20-00000002"
                now <- getCurrentTime
                let activity = pure (Right (Map.fromList
                        [(old, Left "unknown session activity"), (recent, Right now)]))
                report <- gcWorktreesWithActivity activity root 7 False []
                report.cleanupRemoved `shouldBe` []
                report.cleanupRetained `shouldMatchList` [(old, "unknown session activity"), (recent, "recent activity")]
                readRecord root old `shouldReturn` Right Nothing
                readRecord root recent `shouldReturn` Right Nothing

        it "dry-run lease probes retain an active legacy checkout without touching its activity" $
            withTempGitRepo \repo ->
            withTempDir "agent-home-" \home -> do
                let root = worktreeRoot home
                path <- addManagedWorktree repo root "2026-08-20-00000001"
                now <- getCurrentTime
                let activity = pure (Right (Map.singleton path (Right (addUTCTime (-8 * 86400) now))))
                bracket (acquireWorktreeLease root path) releaseLease $ \_ -> do
                    report <- gcWorktreesWithActivity activity root 7 True []
                    report.cleanupEligible `shouldBe` []
                    map snd report.cleanupRetained `shouldSatisfy` any (Text.isInfixOf "existing lock is busy")
                readRecord root path `shouldReturn` Right Nothing

        it "rechecks session activity under the lease before adopting or snapshotting" $
            withTempGitRepo \repo ->
            withTempDir "agent-home-" \home -> do
                let root = worktreeRoot home
                path <- addManagedWorktree repo root "2026-08-20-00000001"
                now <- getCurrentTime
                reads <- newIORef (0 :: Int)
                let activity = do
                        count <- readIORef reads
                        modifyIORef' reads (+1)
                        pure (Right (Map.singleton path (Right (if count == 0 then addUTCTime (-8 * 86400) now else now))))
                report <- gcWorktreesWithActivity activity root 7 False []
                report.cleanupRemoved `shouldBe` []
                report.cleanupRetained `shouldBe` [(path, "recent activity")]
                readRecord root path `shouldReturn` Right Nothing
                git repo ["for-each-ref", "--format=%(refname)", "refs/haskell-agent/worktree-snapshots"]
                    `shouldReturn` ""

        it "rejects copied linked metadata whose reciprocal pointer names another checkout" $
            withTempGitRepo \repo ->
            withTempDir "agent-home-" \home -> do
                let root = worktreeRoot home
                path <- addManagedWorktree repo root "2026-08-20-00000001"
                now <- getCurrentTime
                let impostor = takeDirectory path </> fromFilePath "2026-08-20-00000002"
                    activity = pure (Right (Map.singleton impostor (Right (addUTCTime (-8 * 86400) now))))
                createDirectoryIfMissing True impostor
                Directory.copyFile (toFilePath (path </> fromFilePath ".git"))
                    (toFilePath (impostor </> fromFilePath ".git"))
                report <- gcWorktreesWithActivity activity root 7 True []
                report.cleanupEligible `shouldBe` []
                report.cleanupRetained `shouldContain` [(impostor, "linked Git metadata points at another checkout")]
                readRecord root impostor `shouldReturn` Right Nothing

        it "retains legacy snapshot safety failures rather than adopting them" $
            withTempGitRepo \repo ->
            withTempDir "agent-home-" \home -> do
                let root = worktreeRoot home
                path <- addManagedWorktree repo root "2026-08-20-00000001"
                now <- getCurrentTime
                let activity = pure (Right (Map.singleton path (Right (addUTCTime (-8 * 86400) now))))
                _ <- git path ["update-index", "--assume-unchanged", "README"]
                report <- gcWorktreesWithActivity activity root 7 False []
                report.cleanupRemoved `shouldBe` []
                report.cleanupRetained `shouldSatisfy` (not . null)
                readRecord root path `shouldReturn` Right Nothing

        it "concurrent adopters collect once and keep a restorable snapshot" $
            withTempGitRepo \repo ->
            withTempDir "agent-home-" \home -> do
                let root = worktreeRoot home
                path <- addManagedWorktree repo root "2026-08-20-00000001"
                now <- getCurrentTime
                let activity = pure (Right (Map.singleton path (Right (addUTCTime (-8 * 86400) now))))
                reports <- mapConcurrently (\_ -> gcWorktreesWithActivity activity root 7 False []) [1 :: Int .. 4]
                concatMap (.cleanupRemoved) reports `shouldBe` [path]
                restoreManagedWorktree root path `shouldReturn` Right ()
                doesDirectoryExist path `shouldReturn` True

        it "bounds eligible attempts even when every candidate fails preflight" $
            withTempGitRepo \repo ->
            withTempDir "agent-home-" \home -> do
                let root = worktreeRoot home
                paths <- mapM (\n -> do
                    path <- addManagedWorktree repo root ("2026-08-20-0000000" <> show n)
                    enrollWorktree root path `shouldReturn` Right ()
                    ageRecord root path
                    _ <- git path ["update-index", "--assume-unchanged", "README"]
                    pure path) [1 :: Int .. 9]
                report <- gcWorktrees root 7 False []
                report.cleanupRemoved `shouldBe` []
                length (filter ((== "pass budget exhausted") . snd) report.cleanupRetained) `shouldBe` 1
                mapM_ (\path -> doesDirectoryExist path `shouldReturn` True) paths

        it "enrollment starts a fresh inactivity window" $
            withTempGitRepo \repo ->
            withTempDir "agent-home-" \home -> do
                let root = worktreeRoot home
                path <- addManagedWorktree repo root "2026-08-20-00000001"
                enrollWorktree root path `shouldReturn` Right ()
                report <- gcWorktrees root 7 False []
                report.cleanupRemoved `shouldBe` []
                report.cleanupRetained `shouldBe` [(path, "recent activity")]

        it "resuming an existing nested cwd refreshes activity before the runtime lease" $
            withTempGitRepo \repo ->
            withTempDir "agent-home-" \home -> do
                let root = worktreeRoot home
                path <- addManagedWorktree repo root "2026-08-20-00000001"
                let nested = path </> fromFilePath "nested"
                createDirectoryIfMissing True nested
                enrollWorktree root path `shouldReturn` Right ()
                ageRecord root path
                restoreManagedWorktree root nested `shouldReturn` Right ()
                report <- gcWorktrees root 7 False []
                report.cleanupRetained `shouldBe` [(path, "recent activity")]

        it "racing resume and collection retains a usable checkout or a restorable snapshot" $
            withTempGitRepo \repo ->
            withTempDir "agent-home-" \home -> do
                let root = worktreeRoot home
                path <- addManagedWorktree repo root "2026-08-20-00000001"
                enrollWorktree root path `shouldReturn` Right ()
                ageRecord root path
                _ <- mapConcurrently id
                    [ void (gcWorktrees root 7 False [])
                    , void (restoreManagedWorktree root path)
                    ]
                restoreManagedWorktree root path `shouldReturn` Right ()
                doesDirectoryExist path `shouldReturn` True
                report <- gcWorktrees root 7 False []
                report.cleanupRetained `shouldBe` [(path, "recent activity")]

        it "dry runs preserve refs, registry, index, and checkout while estimating ignored bytes" $
            withTempGitRepo \repo ->
            withTempDir "agent-home-" \home -> do
                let root = worktreeRoot home
                path <- addManagedWorktree repo root "2026-08-20-00000001"
                writeFile (toFilePath (path </> fromFilePath ".gitignore")) "cache\n"
                writeFile (toFilePath (path </> fromFilePath "cache")) (replicate 10000 'x')
                enrollWorktree root path `shouldReturn` Right ()
                ageRecord root path
                before <- readRecord root path
                refs <- git repo ["show-ref"]
                status <- git path ["status", "--porcelain"]
                report <- gcWorktrees root 7 True []
                map fst report.cleanupEligible `shouldBe` [path]
                sum (map snd report.cleanupEligible) `shouldSatisfy` (>= 10000)
                readRecord root path `shouldReturn` before
                git repo ["show-ref"] `shouldReturn` refs
                git path ["status", "--porcelain"] `shouldReturn` status
                doesDirectoryExist path `shouldReturn` True

        it "collects dirty unique work and restores staged, unstaged and untracked files, not ignored data" $
            withTempGitRepo \repo ->
            withTempDir "agent-home-" \home -> do
                let root = worktreeRoot home
                path <- addManagedWorktree repo root "2026-08-20-00000001"
                let file name = toFilePath (path </> fromFilePath name)
                writeFile (file "README") "unique commit\n"
                _ <- git path ["add", "README"]
                _ <- git path ["commit", "-m", "unique"]
                headBefore <- git path ["rev-parse", "HEAD"]
                writeFile (file "README") "staged\n"
                _ <- git path ["add", "README"]
                writeFile (file "README") "unstaged\n"
                writeFile (file "notes") "untracked\n"
                writeFile (file ".gitignore") "cache\n"
                writeFile (file "cache") "excluded"
                status <- git path ["status", "--porcelain", "--untracked-files=all"]
                enrollWorktree root path `shouldReturn` Right ()
                ageRecord root path
                report <- gcWorktrees root 7 False []
                report.cleanupRetained `shouldBe` []
                report.cleanupRemoved `shouldBe` [path]
                doesDirectoryExist path `shouldReturn` False
                let originalBranch = toFilePath (takeFileName path)
                _ <- git repo ["branch", "-f", originalBranch, "HEAD"]
                movedBranch <- git repo ["rev-parse", originalBranch]
                restoreManagedWorktree root path `shouldReturn` Right ()
                git repo ["rev-parse", originalBranch] `shouldReturn` movedBranch
                git path ["rev-parse", "HEAD"] `shouldReturn` headBefore
                git path ["status", "--porcelain", "--untracked-files=all"] `shouldReturn` status
                git path ["show", ":README"] `shouldReturn` "staged"
                readFile (file "README") `shouldReturn` "unstaged\n"
                readFile (file "notes") `shouldReturn` "untracked\n"
                Directory.doesFileExist (file "cache") `shouldReturn` False

        it "persistent protection and live leases independently prevent collection" $
            withTempGitRepo \repo ->
            withTempDir "agent-home-" \home -> do
                let root = worktreeRoot home
                path <- addManagedWorktree repo root "2026-08-20-00000001"
                enrollWorktree root path `shouldReturn` Right ()
                ageRecord root path
                protectWorktree root path True `shouldReturn` Right ()
                report <- gcWorktrees root 7 False []
                report.cleanupRetained `shouldBe` [(path, "protected")]
                protectWorktree root path False `shouldReturn` Right ()
                bracket (acquireWorktreeLease root path) releaseLease $ \lease -> do
                    case lease of
                        Right (Just _) -> pure ()
                        _ -> expectationFailure "expected active lease"
                    blocked <- gcWorktrees root 0 False []
                    blocked.cleanupRemoved `shouldBe` []
                fresh <- gcWorktrees root 7 False []
                fresh.cleanupRetained `shouldBe` [(path, "recent activity")]

        it "concurrent collectors only remove a checkout once and restoration remains available" $
            withTempGitRepo \repo ->
            withTempDir "agent-home-" \home -> do
                let root = worktreeRoot home
                path <- addManagedWorktree repo root "2026-08-20-00000001"
                enrollWorktree root path `shouldReturn` Right ()
                ageRecord root path
                reports <- mapConcurrently (\_ -> gcWorktrees root 7 False []) [1 :: Int .. 4]
                concatMap (.cleanupRemoved) reports `shouldBe` [path]
                restoreManagedWorktree root path `shouldReturn` Right ()
                doesDirectoryExist path `shouldReturn` True

        it "an interrupted maintenance state never overwrites an existing checkout" $
            withTempGitRepo \repo ->
            withTempDir "agent-home-" \home -> do
                let root = worktreeRoot home
                path <- addManagedWorktree repo root "2026-08-20-00000001"
                enrollWorktree root path `shouldReturn` Right ()
                writeFile (toFilePath (path </> fromFilePath "sentinel")) "preserve"
                modifyRecord root path (Right . fmap (\r -> r { recordState = "restoring" }))
                    `shouldReturn` Right ()
                restoreManagedWorktree root path
                    >>= (`shouldSatisfy` either (const True) (const False))
                readFile (toFilePath (path </> fromFilePath "sentinel")) `shouldReturn` "preserve"
                report <- gcWorktrees root 0 False []
                report.cleanupRemoved `shouldBe` []
                report.cleanupRetained `shouldBe` [(path, "state: restoring")]

        it "a missing legacy checkout is never recreated from a guessed branch" $
            withTempDir "agent-home-" \home -> do
                let root = worktreeRoot home
                    path = root </> fromFilePath "repository" </> fromFilePath "2026-08-20-00000001"
                createDirectoryIfMissing True (takeDirectory path)
                restoreManagedWorktree root path
                    >>= (`shouldSatisfy` either (const True) (const False))
                doesDirectoryExist path `shouldReturn` False

        it "rejects a symlinked registry without modifying redirected metadata" $
            withTempGitRepo \repo ->
            withTempDir "agent-home-" \home -> do
                let root = worktreeRoot home
                    outside = home </> fromFilePath "outside-registry"
                path <- addManagedWorktree repo root "2026-08-20-00000001"
                createDirectoryIfMissing True outside
                Directory.createDirectoryLink (toFilePath outside)
                    (toFilePath (root </> fromFilePath ".registry"))
                enrollWorktree root path
                    >>= (`shouldSatisfy` either (const True) (const False))
                report <- gcWorktrees root 0 False []
                report.cleanupRemoved `shouldBe` []
                Directory.listDirectory (toFilePath outside) `shouldReturn` []
                doesDirectoryExist path `shouldReturn` True

        it "rejects a symlinked managed root before leases or activity writes" $
            withTempGitRepo \repo ->
            withTempDir "agent-home-" \home -> do
                let root = worktreeRoot home
                    alias = home </> fromFilePath "alias"
                path <- addManagedWorktree repo root "2026-08-20-00000001"
                Directory.createDirectoryLink (toFilePath root) (toFilePath alias)
                let aliasedPath = alias </> takeFileName repo </> takeFileName path
                rejected <- either (const True) (const False)
                    <$> acquireWorktreeLease alias aliasedPath
                rejected `shouldBe` True
                report <- gcWorktrees alias 0 False []
                report.cleanupRemoved `shouldBe` []
                report.cleanupFailures `shouldSatisfy` (not . null)
                doesDirectoryExist (root </> fromFilePath ".leases") `shouldReturn` False

        it "a corrupt registry fails closed and cannot be silently replaced by enrollment" $
            withTempGitRepo \repo ->
            withTempDir "agent-home-" \home -> do
                let root = worktreeRoot home
                path <- addManagedWorktree repo root "2026-08-20-00000001"
                enrollWorktree root path `shouldReturn` Right ()
                writeFile (toFilePath (recordPath root path)) "{broken"
                report <- gcWorktrees root 0 False []
                report.cleanupRemoved `shouldBe` []
                enrollWorktree root path >>= (`shouldSatisfy` either (const True) (const False))
                doesDirectoryExist path `shouldReturn` True

withMergedWorktree :: String -> (OsPath -> OsPath -> OsPath -> IO a) -> IO a
withMergedWorktree branch action =
    withTempGitRepo \repo ->
    withTempDir "agent-home-" \home -> do
        let root = worktreeRoot home
        _ <- git repo ["branch", "-M", branch]
        path <- addManagedWorktree repo root "2026-08-20-00000001"
        writeFile (toFilePath (path </> fromFilePath "feature")) "feature\n"
        _ <- git path ["add", "feature"]
        _ <- git path ["commit", "-m", "feature"]
        _ <- git repo ["merge", "--no-ff", "-m", "merge feature", toFilePath (takeFileName path)]
        configureDefaultRef repo branch
        action repo root path

configureDefaultRef :: OsPath -> String -> IO ()
configureDefaultRef repo branch = do
    _ <- git repo ["remote", "add", "origin", toFilePath repo]
    _ <- git repo ["update-ref", "refs/remotes/origin/" <> branch, "HEAD"]
    void $ git repo ["symbolic-ref", "refs/remotes/origin/HEAD", "refs/remotes/origin/" <> branch]

savedActivityDaysAgo :: OsPath -> NominalDiffTime -> IO (IO (Either Text (Map.Map OsPath (Either Text UTCTime))))
savedActivityDaysAgo path days = do
    now <- getCurrentTime
    pure (pure (Right (Map.singleton path (Right (addUTCTime (-days * 86400) now)))))

ageRecord :: OsPath -> OsPath -> IO ()
ageRecord root path = do
    now <- getCurrentTime
    modifyRecord root path (Right . fmap (\record ->
        record { recordLastActivity = addUTCTime (-8 * 86400) now }))
        `shouldReturn` Right ()

releaseLease :: Either Text (Maybe WorktreeLease) -> IO ()
releaseLease (Right (Just lease)) = releaseWorktreeLease lease
releaseLease _ = pure ()

expectRight :: Either Text OsPath -> IO OsPath
expectRight = \case
    Right path -> pure path
    Left err -> do
        expectationFailure ("expected Right, got Left " <> Text.unpack err)
        pure (fromFilePath "")

withTempGitRepo :: (OsPath -> IO a) -> IO a
withTempGitRepo action =
    withTempDir "agent-git-" \dir -> do
        _ <- git dir ["init"]
        _ <- git dir ["config", "user.email", "test@example.com"]
        _ <- git dir ["config", "user.name", "Test"]
        _ <- git dir ["config", "commit.gpgsign", "false"]
        writeFile (toFilePath (dir </> fromFilePath "README")) "hello\n"
        _ <- git dir ["add", "README"]
        _ <- git dir ["commit", "-m", "init"]
        action dir

withTempRemoteRepo :: (OsPath -> OsPath -> IO a) -> IO a
withTempRemoteRepo action =
    withTempDir "agent-remote-" \root -> do
        let bare = root </> fromFilePath "origin.git"
            repo = root </> fromFilePath "source"
            updater = root </> fromFilePath "updater"
        _ <- git root
            [ "init", "--bare", "--initial-branch=master", toFilePath bare ]
        Directory.createDirectory (toFilePath repo)
        _ <- git repo ["init", "--initial-branch=master"]
        configureGit repo
        writeFile (toFilePath (repo </> fromFilePath "README")) "initial\n"
        _ <- git repo ["add", "README"]
        _ <- git repo ["commit", "-m", "initial"]
        _ <- git repo ["remote", "add", "origin", toFilePath bare]
        _ <- git repo ["push", "-u", "origin", "master"]

        _ <- git root ["clone", toFilePath bare, toFilePath updater]
        configureGit updater
        writeFile (toFilePath (updater </> fromFilePath "README")) "latest\n"
        _ <- git updater ["add", "README"]
        _ <- git updater ["commit", "-m", "latest"]
        _ <- git updater ["push", "origin", "master"]

        _ <- git repo ["switch", "-c", "local-work"]
        writeFile (toFilePath (repo </> fromFilePath "LOCAL")) "local\n"
        _ <- git repo ["add", "LOCAL"]
        _ <- git repo ["commit", "-m", "local"]
        action repo updater

configureGit :: OsPath -> IO ()
configureGit repo = do
    _ <- git repo ["config", "user.email", "test@example.com"]
    _ <- git repo ["config", "user.name", "Test"]
    _ <- git repo ["config", "commit.gpgsign", "false"]
    pure ()

temporaryFetchRefs :: OsPath -> IO String
temporaryFetchRefs repo =
    git repo
        [ "for-each-ref"
        , "--format=%(refname)"
        , "refs/haskell-agent/worktree-fetches/"
        ]

addManagedWorktree :: OsPath -> OsPath -> String -> IO OsPath
addManagedWorktree repo root name = do
    let parent = root </> takeFileName repo
        path = parent </> fromFilePath name
    createDirectoryIfMissing True parent
    _ <- git repo ["worktree", "add", "-b", name, toFilePath path]
    pure path

withTempDir :: String -> (OsPath -> IO a) -> IO a
withTempDir prefix action = do
    tmp <- Directory.getTemporaryDirectory
    bracket
        (mkdtemp (tmp FilePath.</> prefix <> "XXXXXX"))
        Directory.removeDirectoryRecursive
        (action . fromFilePath)

git :: OsPath -> [String] -> IO String
git dir args = do
    (code, out, err) <-
        readCreateProcessWithExitCode
            (proc "git" args) { cwd = Just (toFilePath dir) } ""
    case code of
        ExitSuccess -> pure (trim out)
        ExitFailure _ -> fail (unwords ("git" : args) <> ": " <> trim err)

trim :: String -> String
trim = dropWhileEnd isSpaceChar . dropWhile isSpaceChar
  where
    isSpaceChar c = c == ' ' || c == '\n' || c == '\r' || c == '\t'
