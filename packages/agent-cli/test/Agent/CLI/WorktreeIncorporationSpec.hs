module Agent.CLI.WorktreeIncorporationSpec (spec) where

import Agent.CLI.Worktree.Incorporation
import Control.Exception.Safe (bracket)
import Control.Monad (void)
import Data.Either (isLeft)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory (getTemporaryDirectory, removePathForcibly)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.OsPath (unsafeEncodeUtf)
import System.Posix.Files (setFileMode)
import System.Posix.Temp (mkdtemp)
import System.Process (CreateProcess(..), proc, readCreateProcessWithExitCode)
import Test.Hspec

spec :: Spec
spec = describe "worktree incorporation" do
    it "accepts incorporation into a non-default branch" $ withRepository $ \repository -> do
        feature repository
        void $ git repository ["checkout", "-b", "integration"]
        commit repository "integration" "integration"
        void $ git repository ["checkout", "feature"]
        inspect repository [] `shouldReturn` Right "incorporated into refs/heads/integration"

    it "retains a branch published only to its own remote tracking reference" $
        withRepository $ \repository -> do
            feature repository
            headCommit <- git repository ["rev-parse", "HEAD"]
            void $ git repository ["update-ref", "refs/remotes/origin/feature", Text.unpack headCommit]
            inspect repository [] >>= (`shouldSatisfy` isLeft)

    it "retains an unmerged detached head with an identical feature reference" $
        withRepository $ \repository -> do
            feature repository
            void $ git repository ["checkout", "--detach"]
            inspect repository [] >>= (`shouldSatisfy` isLeft)

    it "accepts a detached checkout at the default branch tip" $
        withRepository $ \repository -> do
            void $ git repository ["checkout", "--detach"]
            inspect repository [] `shouldReturn` Right "incorporated into refs/heads/main"

    it "does not treat recovery refs as incorporation" $ withRepository $ \repository -> do
        feature repository
        headCommit <- git repository ["rev-parse", "HEAD"]
        void $ git repository ["update-ref", "refs/haskell-agent/reclaimed/test", Text.unpack headCommit]
        inspect repository [] >>= (`shouldSatisfy` isLeft)

    it "accepts an exact merged squash PR with matching resulting content" $
        withRepository $ \repository -> do
            request <- squash repository
            inspect repository [request]
                `shouldReturn` Right "merged pull request incorporated into refs/heads/main"

    it "retains post-PR commits even when a PR was merged" $ withRepository $ \repository -> do
        request <- squash repository
        commit repository "subsequent" "subsequent"
        inspect repository [request] >>= (`shouldSatisfy` isLeft)

    it "accepts squash integration alongside unrelated target changes" $
        withRepository $ \repository -> do
            feature repository
            headCommit <- git repository ["rev-parse", "HEAD"]
            void $ git repository ["checkout", "main"]
            commit repository "unrelated" "unrelated"
            void $ git repository ["merge", "--squash", "feature"]
            void $ git repository ["commit", "-m", "Squash integration"]
            mergeCommit <- git repository ["rev-parse", "HEAD"]
            void $ git repository ["checkout", "feature"]
            inspect repository [MergedPullRequest headCommit mergeCommit "main"]
                `shouldReturn` Right "merged pull request incorporated into refs/heads/main"

    it "accepts verified rebase integration" $ withRepository $ \repository -> do
        feature repository
        headCommit <- git repository ["rev-parse", "HEAD"]
        void $ git repository ["checkout", "main"]
        commit repository "unrelated" "unrelated"
        void $ git repository ["cherry-pick", Text.unpack headCommit]
        mergeCommit <- git repository ["rev-parse", "HEAD"]
        void $ git repository ["checkout", "feature"]
        inspect repository [MergedPullRequest headCommit mergeCommit "main"]
            `shouldReturn` Right "merged pull request incorporated into refs/heads/main"

    it "preserves whitespace and pathspec characters in content checks" $
        withRepository $ \repository -> do
            feature repository
            commit repository " \n[work]* " "unique content"
            headCommit <- git repository ["rev-parse", "HEAD"]
            void $ git repository ["checkout", "main"]
            void $ git repository ["merge", "--squash", "feature"]
            writeFile (repository </> " \n[work]* ") "omitted content"
            void $ git repository ["add", "--all"]
            void $ git repository ["commit", "-m", "Incomplete integration"]
            mergeCommit <- git repository ["rev-parse", "HEAD"]
            void $ git repository ["checkout", "feature"]
            inspect repository [MergedPullRequest headCommit mergeCommit "main"]
                >>= (`shouldSatisfy` isLeft)

    it "does not confuse matching content with a matching executable mode" $
        withRepository $ \repository -> do
            feature repository
            setFileMode (repository </> "feature") 0o755
            void $ git repository ["add", "feature"]
            void $ git repository ["commit", "-m", "Executable mode"]
            headCommit <- git repository ["rev-parse", "HEAD"]
            void $ git repository ["checkout", "main"]
            void $ git repository ["merge", "--squash", "feature"]
            setFileMode (repository </> "feature") 0o644
            void $ git repository ["add", "feature"]
            void $ git repository ["commit", "-m", "Missing executable mode"]
            mergeCommit <- git repository ["rev-parse", "HEAD"]
            void $ git repository ["checkout", "feature"]
            inspect repository [MergedPullRequest headCommit mergeCommit "main"]
                >>= (`shouldSatisfy` isLeft)

    it "retains a merged PR whose resulting content omits work" $ withRepository $ \repository -> do
        feature repository
        headCommit <- git repository ["rev-parse", "HEAD"]
        void $ git repository ["checkout", "main"]
        commit repository "other" "other"
        mergeCommit <- git repository ["rev-parse", "HEAD"]
        void $ git repository ["checkout", "feature"]
        inspect repository [MergedPullRequest headCommit mergeCommit "main"]
            >>= (`shouldSatisfy` isLeft)

    it "retains a PR merge commit no surviving target branch contains" $
        withRepository $ \repository -> do
            request <- squash repository
            void $ git repository ["branch", "-f", "main", "main^"]
            inspect repository [request] >>= (`shouldSatisfy` isLeft)

    it "does not let replacement objects manufacture ancestry" $
        withRepository $ \repository -> do
            feature repository
            headCommit <- git repository ["rev-parse", "HEAD"]
            void $ git repository ["checkout", "main"]
            commit repository "other" "other"
            mainCommit <- git repository ["rev-parse", "HEAD"]
            void $ git repository ["replace", Text.unpack headCommit, Text.unpack mainCommit]
            void $ git repository ["checkout", "feature"]
            inspect repository [] >>= (`shouldSatisfy` isLeft)

inspect :: FilePath -> [MergedPullRequest] -> IO (Either Text Text)
inspect repository requests =
    inspectIncorporationWith (const (pure (Right requests))) (unsafeEncodeUtf repository)

feature :: FilePath -> IO ()
feature repository = do
    void $ git repository ["checkout", "-b", "feature"]
    commit repository "feature" "feature"

squash :: FilePath -> IO MergedPullRequest
squash repository = do
    feature repository
    headCommit <- git repository ["rev-parse", "HEAD"]
    void $ git repository ["checkout", "main"]
    void $ git repository ["merge", "--squash", "feature"]
    void $ git repository ["commit", "-m", "Squash integration"]
    mergeCommit <- git repository ["rev-parse", "HEAD"]
    void $ git repository ["checkout", "feature"]
    pure (MergedPullRequest headCommit mergeCommit "main")

withRepository :: (FilePath -> IO a) -> IO a
withRepository action = do
    temporary <- getTemporaryDirectory
    bracket (mkdtemp (temporary </> "worktree-incorporation-")) removePathForcibly $ \repository -> do
        void $ git repository ["init", "-b", "main"]
        void $ git repository ["config", "user.name", "Worktree Test"]
        void $ git repository ["config", "user.email", "worktree@example.invalid"]
        void $ git repository ["config", "commit.gpgsign", "false"]
        void $ git repository ["config", "core.filemode", "true"]
        commit repository "initial" "initial"
        action repository

commit :: FilePath -> FilePath -> String -> IO ()
commit repository name content = do
    writeFile (repository </> name) content
    void $ git repository ["add", "--", name]
    void $ git repository ["commit", "-m", name]

git :: FilePath -> [String] -> IO Text
git repository arguments = do
    (code, output, errors) <- readCreateProcessWithExitCode
        (proc "git" ("-c" : "core.hooksPath=/dev/null" : arguments))
            { cwd = Just repository } ""
    case code of
        ExitSuccess -> pure (Text.strip (Text.pack output))
        _ -> fail errors
