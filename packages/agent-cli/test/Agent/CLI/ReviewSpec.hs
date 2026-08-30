module Agent.CLI.ReviewSpec (spec) where

import Agent.CLI.Review
import Agent.OsPath (fromText, unsafeToFilePath)
import Control.Exception.Safe (bracket)
import Control.Monad (unless)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified System.Directory as Directory
import System.Exit (ExitCode(..))
import qualified System.FilePath as FilePath
import System.OsPath (OsPath, (</>))
import System.Posix.Temp (mkdtemp)
import System.Process
    ( CreateProcess(..)
    , proc
    , readCreateProcessWithExitCode
    )
import Test.Hspec

spec :: Spec
spec = describe "Agent.CLI.Review" do
    describe "reviewPrompt" do
        it "renders an uncommitted review with all worktree categories" do
            let prompt = reviewPrompt ReviewUncommitted
            prompt `shouldSatisfy` Text.isInfixOf "staged, unstaged, and untracked"
            prompt `shouldSatisfy` Text.isInfixOf "file and line range"
            prompt `shouldSatisfy` Text.isInfixOf "Do not modify files"

        it "treats a base branch as a literal ref" do
            let prompt = reviewPrompt (ReviewBaseBranch "release`candidate")
            prompt `shouldSatisfy` Text.isInfixOf "merge base"
            prompt `shouldSatisfy` Text.isInfixOf "`release\\`candidate`"
            prompt `shouldSatisfy` Text.isInfixOf "literal Git ref"

        it "renders commit and custom targets" do
            reviewPrompt (ReviewCommitTarget "abc123")
                `shouldSatisfy` Text.isInfixOf "single Git commit `abc123`"
            reviewPrompt (ReviewCustom "Check the parser")
                `shouldSatisfy`
                    Text.isInfixOf
                        "<review_instructions>\nCheck the parser\n</review_instructions>"

    describe "Git discovery" do
        it "rejects a directory outside Git" $
            withTempDir "agent-review-not-git-" \dir -> do
                listReviewBranches dir
                    `shouldReturn` Left "not a Git repository"
                listReviewCommits dir 20
                    `shouldReturn` Left "not a Git repository"

        it "lists non-current local branches" $
            withTempGitRepo \repo -> do
                git repo ["branch", "base"]
                git repo ["branch", "another"]
                branches <- expectRight =<< listReviewBranches repo
                map (.reviewBranchName) branches
                    `shouldMatchList` ["another", "base"]

        it "lists recent commits newest first and respects the limit" $
            withTempGitRepo \repo -> do
                writeUtf8 (repo </> fromText "second") "two\n"
                git repo ["add", "second"]
                git repo ["commit", "-m", "second subject"]
                commits <- expectRight =<< listReviewCommits repo 1
                case commits of
                    [commit] -> do
                        commit.reviewCommitSubject `shouldBe` "second subject"
                        Text.length commit.reviewCommitHash `shouldBe` 40
                        commit.reviewCommitShortHash
                            `shouldSatisfy`
                                (`Text.isPrefixOf` commit.reviewCommitHash)
                    _ ->
                        expectationFailure
                            ("expected one commit, got " <> show commits)

        it "returns no commits for an unborn repository" $
            withTempDir "agent-review-empty-" \repo -> do
                git repo ["init"]
                listReviewCommits repo 20 `shouldReturn` Right []

expectRight :: Either Text a -> IO a
expectRight = \case
    Right value -> pure value
    Left err -> do
        expectationFailure ("expected Right, got Left " <> Text.unpack err)
        fail "unreachable"

withTempGitRepo :: (OsPath -> IO a) -> IO a
withTempGitRepo action =
    withTempDir "agent-review-git-" \repo -> do
        git repo ["init", "-b", "main"]
        git repo ["config", "user.email", "test@example.com"]
        git repo ["config", "user.name", "Test"]
        git repo ["config", "commit.gpgsign", "false"]
        writeUtf8 (repo </> fromText "README") "hello\n"
        git repo ["add", "README"]
        git repo ["commit", "-m", "initial subject"]
        action repo

withTempDir :: String -> (OsPath -> IO a) -> IO a
withTempDir prefix action = do
    root <- Directory.getTemporaryDirectory
    bracket
        (fromText . Text.pack
            <$> mkdtemp (root FilePath.</> prefix <> "XXXXXX"))
        (Directory.removeDirectoryRecursive . unsafeToFilePath)
        action

git :: OsPath -> [String] -> IO ()
git cwd arguments = do
    (exitCode, _stdout, stderr) <-
        readCreateProcessWithExitCode
            (proc "git" arguments)
                { cwd = Just (unsafeToFilePath cwd) }
            ""
    unless (exitCode == ExitSuccess) $
        expectationFailure
            ("git " <> unwords arguments <> " failed: " <> stderr)

writeUtf8 :: OsPath -> Text -> IO ()
writeUtf8 path = TextIO.writeFile (unsafeToFilePath path)
