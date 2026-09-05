module Agent.CLI.GitDiffSpec (spec) where

import Agent.CLI.GitDiff
import Agent.OsPath (fromText, unsafeToFilePath)
import Control.Exception.Safe (bracket)
import Control.Monad (unless)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified System.Directory as Directory
import System.Environment (lookupEnv, setEnv, unsetEnv)
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
            git repo ["add", "README"]
            writeUtf8 (repo </> fromText "new file.txt") "new\n"
            writeUtf8 (repo </> fromText "line\nbreak.txt") "odd\n"

            result <- getGitDiff repo
            diff <- expectDiff result
            diff `shouldSatisfy` Text.isInfixOf "-hello"
            diff `shouldSatisfy` Text.isInfixOf "changed"
            diff `shouldSatisfy` Text.isInfixOf "new file.txt"
            diff `shouldSatisfy` Text.isInfixOf "break.txt"
            diff `shouldSatisfy` Text.isInfixOf "odd"

    it "includes staged files in an unborn repository" $
        withTempDir "agent-git-diff-unborn-" \repo -> do
            git repo ["init"]
            writeUtf8 (repo </> fromText "first") "staged\n"
            git repo ["add", "first"]

            diff <- expectDiff =<< getGitDiff repo
            diff `shouldSatisfy` Text.isInfixOf "first"
            diff `shouldSatisfy` Text.isInfixOf "+staged"

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

    it "renders repository-controlled terminal escapes inert" $
        withTempGitRepo \repo -> do
            writeUtf8
                (repo </> fromText "README")
                "hello\n\ESC]52;c;ZXZpbA==\BEL\rhidden\n"

            diff <- expectDiff =<< getGitDiff repo
            diff `shouldSatisfy` (not . Text.elem '\ESC')
            diff `shouldSatisfy` (not . Text.elem '\BEL')
            diff `shouldSatisfy` (not . Text.elem '\r')
            diff `shouldSatisfy` Text.isInfixOf "␛]52;c;ZXZpbA==␇"

    it "finishes both inspections and prefers the tracked failure" $
        withFakeGit
            [ "rev-parse) printf 'true\\n';;"
            , "config) exit 1;;"
            , "diff) printf tracked >&2; exit 2;;"
            , "ls-files) : > listed; printf untracked >&2; exit 2;;"
            ] \repo -> do
                result <- getGitDiff repo
                result `shouldSatisfy` isFailureContaining "tracked"
                result `shouldSatisfy` (not . isFailureContaining "untracked")
                Directory.doesFileExist
                    (unsafeToFilePath (repo </> fromText "listed"))
                    `shouldReturn` True

    it "finishes all untracked diffs and returns the first failure" $
        withFakeGit
            [ "rev-parse) printf 'true\\n';;"
            , "config) exit 1;;"
            , "ls-files) printf 'first\\000second\\000';;"
            , "diff)"
            , "  case \"$*\" in"
            , "    *--no-index*)"
            , "      for path do :; done"
            , "      printf '%s\\n' \"$path\" >> inspected"
            , "      printf '%s failed' \"$path\" >&2; exit 2;;"
            , "    *) exit 0;;"
            , "  esac;;"
            ] \repo -> do
                getGitDiff repo >>= (`shouldSatisfy` isFailureContaining "first failed")
                readFile (unsafeToFilePath (repo </> fromText "inspected"))
                    `shouldReturn` "first\nsecond\n"

    it "includes both staged and unstaged changes in an unborn repository" $
        withTempDir "agent-git-diff-unborn-both-" \repo -> do
            git repo ["init"]
            writeUtf8 (repo </> fromText "first") "staged\n"
            git repo ["add", "first"]
            writeUtf8 (repo </> fromText "first") "unstaged\n"
            diff <- expectDiff =<< getGitDiff repo
            diff `shouldSatisfy` Text.isInfixOf "+staged"
            diff `shouldSatisfy` Text.isInfixOf "-staged"
            diff `shouldSatisfy` Text.isInfixOf "+unstaged"

isFailureContaining :: Text -> Either Text a -> Bool
isFailureContaining expected = \case
    Left err -> expected `Text.isInfixOf` err
    Right _ -> False

-- The fixture uses only shell builtins and restores PATH before cleanup.
withFakeGit :: [String] -> (OsPath -> IO a) -> IO a
withFakeGit cases action =
    withTempDir "agent-git-diff-fake-" \repo -> do
        let executable = unsafeToFilePath (repo </> fromText "git")
        writeFile executable $ unlines
            ( [ "#!/bin/sh"
              , "while [ \"$1\" = -c ]; do shift 2; done"
              , "command=$1; shift"
              , "case \"$command\" in"
              ]
                <> cases
                <> ["*) exit 99;;", "esac"]
            )
        setFileMode executable 0o700
        bracket
            (lookupEnv "PATH")
            (maybe (unsetEnv "PATH") (setEnv "PATH"))
            \_ -> do
                setEnv "PATH" (unsafeToFilePath repo)
                action repo

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
