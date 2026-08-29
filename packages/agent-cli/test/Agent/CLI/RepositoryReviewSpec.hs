module Agent.CLI.RepositoryReviewSpec (spec) where

import Agent.CLI.RepositoryReview
import Control.Concurrent.MVar
    ( newEmptyMVar
    , putMVar
    , takeMVar
    )
import Control.Exception.Safe (bracket)
import qualified Data.ByteString.Char8 as BS8
import Data.Either (isLeft)
import Data.IORef
    ( modifyIORef'
    , newIORef
    , readIORef
    )
import qualified Data.Text as Text
import System.Directory
    ( createDirectory
    , doesFileExist
    , getTemporaryDirectory
    , removePathForcibly
    )
import System.Exit (ExitCode(..))
import System.IO (hClose, openTempFile)
import System.Process
    ( CreateProcess(..)
    , proc
    , readCreateProcessWithExitCode
    )
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "repository review service" do
    it "snapshots unusual paths and parses diff hunk coordinates" $
        withRepository \root -> do
            appendFile (root <> "/tracked.txt") "second\n"
            let unusual = "space and\nλ.txt"
            writeFile (root <> "/" <> unusual) "untracked\n"

            snapshot <- expectRight =<< repositorySnapshot root
            snapshot.snapshotHead `shouldSatisfy` (/= Nothing)
            snapshot.snapshotFiles `shouldSatisfy`
                any (\file ->
                    file.repositoryFilePath == "tracked.txt"
                        && file.repositoryFileWorktreeStatus == 'M')
            snapshot.snapshotFiles `shouldSatisfy`
                any (\file ->
                    file.repositoryFilePath == unusual
                        && file.repositoryFileIndexStatus == '?')
            untrackedDiff <- expectRight
                =<< repositoryDiff
                    root
                    snapshot.snapshotId
                    RepositoryWorktreeDiff
                    unusual
            untrackedDiff.repositoryDiffPatch `shouldSatisfy`
                BS8.isInfixOf "+untracked"

            diff <- expectRight
                =<< repositoryDiff
                    root
                    snapshot.snapshotId
                    RepositoryWorktreeDiff
                    "tracked.txt"
            diff.repositoryDiffPatch `shouldSatisfy`
                BS8.isInfixOf "+second"
            diff.repositoryDiffHunks `shouldBe`
                [ DiffHunk
                    { hunkOldStart = 1
                    , hunkOldCount = 1
                    , hunkNewStart = 1
                    , hunkNewCount = 2
                    , hunkHeader = ""
                    }
                ]

    it "rejects a stale snapshot before mutating the index" $
        withRepository \root -> do
            appendFile (root <> "/tracked.txt") "pending\n"
            snapshot <- expectRight =<< repositorySnapshot root
            appendFile (root <> "/tracked.txt") "newer\n"

            mutateRepository root snapshot.snapshotId (StagePath "tracked.txt")
                >>= \case
                    Left (StaleRepositorySnapshot _ _) -> pure ()
                    result -> expectationFailure ("unexpected result: " <> show result)

            staged <- git root ["diff", "--cached", "--name-only"]
            staged `shouldBe` ""

    it "stages, unstages, restores, and commits under fingerprint guards" $
        withRepository \root -> do
            appendFile (root <> "/tracked.txt") "pending\n"
            initial <- expectRight =<< repositorySnapshot root

            staged <- expectRight
                =<< mutateRepository
                    root
                    initial.snapshotId
                    (StagePath "tracked.txt")
            staged.snapshotFiles `shouldSatisfy`
                any (\file ->
                    file.repositoryFilePath == "tracked.txt"
                        && file.repositoryFileIndexStatus == 'M'
                        && file.repositoryFileWorktreeStatus == ' ')

            unstagedAfterPath <- expectRight
                =<< mutateRepository
                    root
                    staged.snapshotId
                    (UnstagePath "tracked.txt")
            restored <- expectRight
                =<< mutateRepository
                    root
                    unstagedAfterPath.snapshotId
                    (RestorePath "tracked.txt")
            restored.snapshotFiles `shouldBe` []
            readFile (root <> "/tracked.txt") `shouldReturn` "first\n"

            appendFile (root <> "/tracked.txt") "commit me\n"
            beforeCommit <- expectRight =<< repositorySnapshot root
            afterStage <- expectRight
                =<< mutateRepository
                    root
                    beforeCommit.snapshotId
                    (StagePath "tracked.txt")
            committed <- expectRight
                =<< commitRepository root afterStage.snapshotId "review commit\n"
            committed.snapshotFiles `shouldBe` []
            Text.strip <$> git root ["log", "-1", "--pretty=%B"]
                `shouldReturn` "review commit"

    it "applies reviewed patches and refuses to delete untracked files" $
        withRepository \root -> do
            appendFile (root <> "/tracked.txt") "patch\n"
            before <- expectRight =<< repositorySnapshot root
            diff <- expectRight
                =<< repositoryDiff
                    root
                    before.snapshotId
                    RepositoryWorktreeDiff
                    "tracked.txt"
            staged <- expectRight
                =<< mutateRepository
                    root
                    before.snapshotId
                    (StagePatch diff.repositoryDiffPatch)
            git root ["diff", "--cached", "--name-only"]
                `shouldReturn` "tracked.txt\n"

            unstageDiff <- expectRight
                =<< repositoryDiff
                    root
                    staged.snapshotId
                    RepositoryStagedDiff
                    "tracked.txt"
            _unstagedAfterPatch <- expectRight
                =<< mutateRepository
                    root
                    staged.snapshotId
                    (UnstagePatch unstageDiff.repositoryDiffPatch)
            git root ["diff", "--cached", "--name-only"] `shouldReturn` ""

            writeFile (root <> "/untracked.txt") "keep\n"
            current <- expectRight =<< repositorySnapshot root
            result <- mutateRepository
                root
                current.snapshotId
                (RestorePath "untracked.txt")
            result `shouldSatisfy` isLeft
            doesFileExist (root <> "/untracked.txt") `shouldReturn` True

    it "streams an explicit argv check and reports its exit status" $
        withRepository \root -> do
            snapshot <- expectRight =<< repositorySnapshot root
            chunks <- newIORef []
            terminal <- newEmptyMVar
            check <- expectRight
                =<< startRepositoryCheck
                    root
                    snapshot.snapshotId
                    "/bin/sh"
                    [ "-c"
                    , "printf stdout-value; printf stderr-value >&2; exit 7"
                    ]
                    (\stream bytes ->
                        modifyIORef' chunks (<> [(stream, bytes)]))
                    (\cancelled exitCode ->
                        putMVar terminal (cancelled, exitCode))
            waitRepositoryCheck check
            takeMVar terminal `shouldReturn` (False, ExitFailure 7)
            readIORef chunks >>= (`shouldMatchList`
                [ (RepositoryCheckStdout, "stdout-value")
                , (RepositoryCheckStderr, "stderr-value")
                ])

    it "cancels and joins a running check" $
        withRepository \root -> do
            snapshot <- expectRight =<< repositorySnapshot root
            terminal <- newEmptyMVar
            check <- expectRight
                =<< startRepositoryCheck
                    root
                    snapshot.snapshotId
                    "/bin/sleep"
                    ["30"]
                    (\_ _ -> pure ())
                    (\cancelled exitCode ->
                        putMVar terminal (cancelled, exitCode))
            cancelRepositoryCheck check
            waitRepositoryCheck check
            result <- timeout 2_000_000 (takeMVar terminal)
            result `shouldSatisfy` \case
                Just (True, ExitFailure _) -> True
                _ -> False

withRepository :: (FilePath -> IO value) -> IO value
withRepository action = withTempDirectory "repository-review" \root -> do
    _ <- git root ["init", "-q"]
    _ <- git root ["config", "user.name", "Repository Review Test"]
    _ <- git root ["config", "user.email", "review@example.test"]
    writeFile (root <> "/tracked.txt") "first\n"
    _ <- git root ["add", "tracked.txt"]
    _ <- git root ["commit", "-q", "-m", "initial"]
    action root

withTempDirectory :: String -> (FilePath -> IO value) -> IO value
withTempDirectory template action = do
    base <- getTemporaryDirectory
    bracket
        (do
            (path, handle) <- openTempFile base template
            hClose handle
            removePathForcibly path
            createDirectory path
            pure path)
        removePathForcibly
        action

git :: FilePath -> [String] -> IO Text.Text
git root arguments = do
    (exitCode, output, errors) <-
        readCreateProcessWithExitCode
            (proc "git" arguments) { cwd = Just root }
            ""
    case exitCode of
        ExitSuccess -> pure (Text.pack output)
        ExitFailure code ->
            expectationFailure
                ("git exited " <> show code <> ": " <> errors)
                >> pure ""

expectRight :: (HasCallStack, Show error) => Either error value -> IO value
expectRight = \case
    Left err -> expectationFailure (show err) >> fail "unreachable"
    Right value -> pure value
