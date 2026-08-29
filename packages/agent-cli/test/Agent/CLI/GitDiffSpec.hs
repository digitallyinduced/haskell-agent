module Agent.CLI.GitDiffSpec (spec) where

import Agent.CLI.GitDiff
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
import System.Posix.Files (setFileMode)
import System.Posix.Temp (mkdtemp)
import System.Process
    ( CreateProcess(..)
    , proc
    , readCreateProcessWithExitCode
    )
import Test.Hspec

spec :: Spec
spec = describe "Agent.CLI.GitDiff" do
    it "reports a directory outside Git without failing" $
        withTempDir "agent-diff-not-git-" \dir ->
            getGitDiff dir `shouldReturn` Right GitDiffNotRepository

    it "returns an empty diff for an unchanged repository" $
        withTempGitRepo \repo ->
            getGitDiff repo `shouldReturn` Right (GitDiffOutput "")

    it "includes tracked changes and NUL-delimited untracked paths" $
        withTempGitRepo \repo -> do
            writeUtf8 (repo </> fromText "README") "changed\n"
            writeUtf8 (repo </> fromText "new file.txt") "new\n"
            writeUtf8 (repo </> fromText "line\nbreak.txt") "odd\n"

            result <- getGitDiff repo
            diff <- expectDiff result
            diff `shouldSatisfy` Text.isInfixOf "-hello"
            diff `shouldSatisfy` Text.isInfixOf "changed"
            diff `shouldSatisfy` Text.isInfixOf "new file.txt"
            diff `shouldSatisfy` Text.isInfixOf "break.txt"
            diff `shouldSatisfy` Text.isInfixOf "odd"

    it "does not execute configured clean or textconv helpers" $
        withTempGitRepo \repo -> do
            let marker = repo </> fromText "helper-ran"
                script = repo </> fromText "evil-helper"
            writeFile (unsafeToFilePath script) $
                "#!/bin/sh\n"
                    <> "touch "
                    <> shellQuote (unsafeToFilePath marker)
                    <> "\ncat\n"
            setFileMode (unsafeToFilePath script) 0o700
            writeUtf8 (repo </> fromText ".gitattributes")
                "*.bad filter=evil diff=evil\n"
            writeUtf8 (repo </> fromText "tracked.bad") "old\n"
            git repo ["add", ".gitattributes", "tracked.bad"]
            git repo ["commit", "-m", "add filtered file"]
            git repo ["config", "filter.evil.clean", unsafeToFilePath script]
            git repo ["config", "filter.evil.process", unsafeToFilePath script]
            git repo ["config", "filter.evil.required", "true"]
            git repo ["config", "diff.evil.textconv", unsafeToFilePath script]
            writeUtf8 (repo </> fromText "tracked.bad") "new\n"

            result <- getGitDiff repo
            _ <- expectDiff result
            Directory.doesPathExist (unsafeToFilePath marker)
                `shouldReturn` False

expectDiff :: Either Text GitDiffResult -> IO Text
expectDiff = \case
    Right (GitDiffOutput diff) -> pure diff
    other -> do
        expectationFailure ("expected GitDiffOutput, got " <> show other)
        pure ""

withTempGitRepo :: (OsPath -> IO a) -> IO a
withTempGitRepo action =
    withTempDir "agent-git-diff-" \repo -> do
        git repo ["init"]
        git repo ["config", "user.email", "test@example.com"]
        git repo ["config", "user.name", "Test"]
        git repo ["config", "commit.gpgsign", "false"]
        writeUtf8 (repo </> fromText "README") "hello\n"
        git repo ["add", "README"]
        git repo ["commit", "-m", "initial"]
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

shellQuote :: String -> String
shellQuote value =
    "'" <> concatMap quote value <> "'"
  where
    quote '\'' = "'\\''"
    quote char = [char]
