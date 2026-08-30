module Agent.CLI.WorktreeSpec (spec) where

import Agent.CLI.Config
import Agent.CLI.Worktree
import Control.Concurrent.Async (mapConcurrently)
import Control.Exception.Safe (bracket)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (dropWhileEnd, isInfixOf)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (getCurrentTime)
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
                    , WorktreeCheckingRemote "origin"
                    , WorktreeFetchingRemote "origin" "refs/heads/master"
                    , WorktreeCreating
                    ]
                map worktreeProgressMessage progress `shouldBe`
                    [ "Inspecting Git repository…"
                    , "Checking Git remote origin…"
                    , "Fetching latest from origin/master…"
                    , "Creating worktree…"
                    ]

        it "uses local HEAD when latest-upstream fetching is disabled" $
            withTempRemoteRepo \repo updater ->
            withTempDir "agent-home-" \home -> do
                local <- git repo ["rev-parse", "HEAD"]
                latest <- git updater ["rev-parse", "HEAD"]
                local `shouldNotBe` latest
                let config = defaultHarnessConfig
                        { configWorktree = WorktreeConfig
                            { worktreeFetchLatestUpstream = False
                            }
                        }
                saveHarnessConfig home config `shouldReturn` Right ()

                path <- expectRight =<< createManagedWorktree home repo

                git path ["rev-parse", "HEAD"] `shouldReturn` local
                doesFileExist (path </> fromFilePath "LOCAL")
                    `shouldReturn` True

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

        it "removes clean managed worktrees beyond the per-repository retention" $
            withTempGitRepo \repo ->
            withTempDir "agent-home-" \home -> do
                let root = worktreeRoot home
                older <- addManagedWorktree repo root "2026-08-20-00000001"
                newer <- addManagedWorktree repo root "2026-08-21-00000002"

                report <- cleanupStaleWorktrees root 1 []

                report.cleanupFailures `shouldBe` []
                report.cleanupRemoved `shouldBe` [older]
                doesDirectoryExist older `shouldReturn` False
                doesDirectoryExist newer `shouldReturn` True
                git repo ["branch", "--list", "2026-08-20-00000001"]
                    `shouldReturn` ""

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
                    `shouldReturn` [older]
                doesDirectoryExist older `shouldReturn` False

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
