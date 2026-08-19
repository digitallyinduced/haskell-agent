module Agent.CLI.WorktreeSpec (spec) where

import Agent.CLI.Worktree
import Control.Exception (bracket)
import Data.List (dropWhileEnd, isInfixOf, isPrefixOf)
import Data.Time.Calendar (fromGregorian)
import System.Directory
    ( doesDirectoryExist
    , getTemporaryDirectory
    , removeDirectoryRecursive
    )
import System.Exit (ExitCode(..))
import System.FilePath (addTrailingPathSeparator, takeFileName, (</>))
import System.Posix.Temp (mkdtemp)
import System.Process (CreateProcess(..), proc, readCreateProcessWithExitCode)
import Test.Hspec

spec :: Spec
spec = describe "Agent.CLI.Worktree" do
    describe "worktreePath" do
        it "builds root/repo/YYYY-MM-DD-hex" do
            worktreePath "/tmp/root" "my-repo" (fromGregorian 2026 8 20) "abcd1234"
                `shouldBe` "/tmp/root" </> "my-repo" </> "2026-08-20-abcd1234"

    describe "worktreeRoot" do
        it "is ~/.haskell-agent/worktrees" do
            worktreeRoot "/home/marc"
                `shouldBe` "/home/marc" </> ".haskell-agent" </> "worktrees"

    describe "createWorktree" do
        it "adds a worktree under the injected root on a new branch" $
            withTempGitRepo \repo ->
            withTempDir "agent-home-" \home -> do
                path <- expectRight =<< createWorktree repo (worktreeRoot home)
                let parent = worktreeRoot home </> takeFileName repo
                path `shouldSatisfy` (addTrailingPathSeparator parent `isPrefixOf`)
                takeFileName path `shouldSatisfy` ("-" `isInfixOf`)
                inside <- git path ["rev-parse", "--is-inside-work-tree"]
                inside `shouldBe` "true"
                sourceBranch <- git repo ["rev-parse", "--abbrev-ref", "HEAD"]
                worktreeBranch <- git path ["rev-parse", "--abbrev-ref", "HEAD"]
                worktreeBranch `shouldNotBe` sourceBranch
                worktreeBranch `shouldBe` takeFileName path

        it "rejects a directory that is not a git checkout" $
            withTempDir "agent-not-git-" \dir -> do
                result <- createWorktree dir (dir </> "worktrees")
                case result of
                    Left err -> err `shouldSatisfy` ("--worktree" `isInfixOf`)
                    Right path -> expectationFailure ("expected failure, got " <> path)

        it "creates two distinct worktrees for the same repo" $
            withTempGitRepo \repo ->
            withTempDir "agent-home-" \home -> do
                first <- expectRight =<< createWorktree repo (worktreeRoot home)
                second <- expectRight =<< createWorktree repo (worktreeRoot home)
                first `shouldNotBe` second
                doesDirectoryExist first `shouldReturn` True
                doesDirectoryExist second `shouldReturn` True

expectRight :: Either String FilePath -> IO FilePath
expectRight = \case
    Right path -> pure path
    Left err -> do
        expectationFailure ("expected Right, got Left " <> err)
        pure ""

withTempGitRepo :: (FilePath -> IO a) -> IO a
withTempGitRepo action =
    withTempDir "agent-git-" \dir -> do
        _ <- git dir ["init"]
        _ <- git dir ["config", "user.email", "test@example.com"]
        _ <- git dir ["config", "user.name", "Test"]
        _ <- git dir ["config", "commit.gpgsign", "false"]
        writeFile (dir </> "README") "hello\n"
        _ <- git dir ["add", "README"]
        _ <- git dir ["commit", "-m", "init"]
        action dir

withTempDir :: String -> (FilePath -> IO a) -> IO a
withTempDir prefix action = do
    tmp <- getTemporaryDirectory
    bracket
        (mkdtemp (tmp </> prefix <> "XXXXXX"))
        removeDirectoryRecursive
        action

git :: FilePath -> [String] -> IO String
git dir args = do
    (code, out, err) <-
        readCreateProcessWithExitCode (proc "git" args) { cwd = Just dir } ""
    case code of
        ExitSuccess -> pure (trim out)
        ExitFailure _ -> fail (unwords ("git" : args) <> ": " <> trim err)

trim :: String -> String
trim = dropWhileEnd isSpaceChar . dropWhile isSpaceChar
  where
    isSpaceChar c = c == ' ' || c == '\n' || c == '\r' || c == '\t'
