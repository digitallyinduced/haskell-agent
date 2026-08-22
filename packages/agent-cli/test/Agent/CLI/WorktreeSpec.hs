module Agent.CLI.WorktreeSpec (spec) where

import Agent.CLI.Worktree
import System.OsPath (OsPath, decodeUtf, unsafeEncodeUtf)
import Control.Exception (bracket)
import Data.List (dropWhileEnd, isInfixOf)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Calendar (fromGregorian)
import qualified System.Directory as Directory
import System.Directory.OsPath (doesDirectoryExist)
import System.Exit (ExitCode(..))
import qualified System.FilePath as FilePath
import System.OsPath (addTrailingPathSeparator, takeFileName, (</>))
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

        it "rejects a directory that is not a git checkout" $
            withTempDir "agent-not-git-" \dir -> do
                let root = dir </> fromFilePath "worktrees"
                result <- createWorktree dir root
                case result of
                    Left err -> err `shouldSatisfy` Text.isInfixOf "--worktree"
                    Right path ->
                        expectationFailure ("expected failure, got " <> toFilePath path)
                doesDirectoryExist root `shouldReturn` False

        it "creates two distinct worktrees for the same repo" $
            withTempGitRepo \repo ->
            withTempDir "agent-home-" \home -> do
                first <- expectRight =<< createWorktree repo (worktreeRoot home)
                second <- expectRight =<< createWorktree repo (worktreeRoot home)
                first `shouldNotBe` second
                doesDirectoryExist first `shouldReturn` True
                doesDirectoryExist second `shouldReturn` True

        it "removes the worktree and its generated branch" $
            withTempGitRepo \repo ->
            withTempDir "agent-home-" \home -> do
                path <- expectRight =<< createWorktree repo (worktreeRoot home)
                let branch = toFilePath (takeFileName path)
                removeWorktree repo path `shouldReturn` Right ()
                doesDirectoryExist path `shouldReturn` False
                branches <- git repo ["branch", "--list", branch]
                branches `shouldBe` ""

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
