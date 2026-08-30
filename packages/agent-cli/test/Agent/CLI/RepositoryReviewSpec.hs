module Agent.CLI.RepositoryReviewSpec (spec) where

import Agent.CLI.RepositoryReview
import Control.Concurrent.MVar
    ( newEmptyMVar
    , putMVar
    , takeMVar
    )
import Control.Exception.Safe (bracket, throwString)
import Control.Monad (forM_)
import qualified Data.ByteString.Char8 as BS8
import Data.Either (isLeft)
import Data.IORef
    ( atomicModifyIORef'
    , modifyIORef'
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
                    (StagePatch "tracked.txt" diff.repositoryDiffPatch)
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
                    (UnstagePatch
                        "tracked.txt"
                        unstageDiff.repositoryDiffPatch)
            git root ["diff", "--cached", "--name-only"] `shouldReturn` ""

            writeFile (root <> "/untracked.txt") "keep\n"
            current <- expectRight =<< repositorySnapshot root
            result <- mutateRepository
                root
                current.snapshotId
                (RestorePath "untracked.txt")
            result `shouldSatisfy` isLeft
            doesFileExist (root <> "/untracked.txt") `shouldReturn` True

    it "accepts an unchanged subset of reviewed hunks" $
        withRepository \root -> do
            writeFile
                (root <> "/tracked.txt")
                (unlines (map (("line " <>) . show) [1 :: Int .. 12]))
            _ <- git root ["add", "tracked.txt"]
            _ <- git root ["commit", "-q", "-m", "lines"]
            writeFile
                (root <> "/tracked.txt")
                (unlines
                    [ if number `elem` [2, 11]
                        then "changed " <> show number
                        else "line " <> show number
                    | number <- [1 :: Int .. 12]
                    ])
            snapshot <- expectRight =<< repositorySnapshot root
            diff <- expectRight
                =<< repositoryDiff root snapshot.snapshotId
                    RepositoryWorktreeDiff "tracked.txt"
            let selected = firstPatchHunk diff.repositoryDiffPatch
            selected `shouldSatisfy` (/= diff.repositoryDiffPatch)
            _ <- expectRight
                =<< mutateRepository root snapshot.snapshotId
                    (StagePatch "tracked.txt" selected)
            stagedPatch <- git root ["diff", "--cached"]
            stagedPatch `shouldSatisfy` Text.isInfixOf "+changed 2"
            stagedPatch `shouldNotSatisfy` Text.isInfixOf "+changed 11"

    it "rejects pathspec magic, globs, and traversal without broad mutation" $
        withRepository \root -> do
            writeFile (root <> "/second.txt") "second\n"
            _ <- git root ["add", "second.txt"]
            _ <- git root ["commit", "-q", "-m", "second"]
            appendFile (root <> "/tracked.txt") "pending\n"
            appendFile (root <> "/second.txt") "pending\n"

            forM_ ["*", ":!tracked.txt", "../tracked.txt", "a/../tracked.txt"] $
                \path -> do
                    snapshot <- expectRight =<< repositorySnapshot root
                    mutateRepository root snapshot.snapshotId (StagePath path)
                        `shouldReturnSatisfying` isInvalidRequest
                    git root ["diff", "--cached", "--name-only"]
                        `shouldReturn` ""

    it "binds patch mutations to one reviewed path and exact hunk content" $
        withRepository \root -> do
            writeFile (root <> "/second.txt") "second\n"
            _ <- git root ["add", "second.txt"]
            _ <- git root ["commit", "-q", "-m", "second"]
            appendFile (root <> "/tracked.txt") "tracked change\n"
            appendFile (root <> "/second.txt") "second change\n"
            snapshot <- expectRight =<< repositorySnapshot root
            trackedDiff <- expectRight
                =<< repositoryDiff root snapshot.snapshotId
                    RepositoryWorktreeDiff "tracked.txt"
            secondDiff <- expectRight
                =<< repositoryDiff root snapshot.snapshotId
                    RepositoryWorktreeDiff "second.txt"

            mutateRepository root snapshot.snapshotId
                (StagePatch "tracked.txt" secondDiff.repositoryDiffPatch)
                `shouldReturnSatisfying` isInvalidRequest
            mutateRepository root snapshot.snapshotId
                (StagePatch
                    "tracked.txt"
                    (trackedDiff.repositoryDiffPatch
                        <> secondDiff.repositoryDiffPatch))
                `shouldReturnSatisfying` isInvalidRequest
            mutateRepository root snapshot.snapshotId
                (StagePatch
                    "tracked.txt"
                    (trackedDiff.repositoryDiffPatch <> "+injected\n"))
                `shouldReturnSatisfying` isInvalidRequest
            git root ["diff", "--cached", "--name-only"] `shouldReturn` ""

            writeFile (root <> "/untracked.txt") "keep\n"
            untrackedSnapshot <- expectRight =<< repositorySnapshot root
            untrackedDiff <- expectRight
                =<< repositoryDiff root untrackedSnapshot.snapshotId
                    RepositoryWorktreeDiff "untracked.txt"
            mutateRepository root untrackedSnapshot.snapshotId
                (RestorePatch
                    "untracked.txt"
                    untrackedDiff.repositoryDiffPatch)
                `shouldReturnSatisfying` isInvalidRequest
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

    it "drains large stdout and stderr streams without pipe deadlock" $
        withRepository \root -> do
            snapshot <- expectRight =<< repositorySnapshot root
            byteCounts <- newIORef (0, 0)
            terminal <- newEmptyMVar
            check <- expectRight
                =<< startRepositoryCheck
                    root
                    snapshot.snapshotId
                    "/bin/sh"
                    [ "-c"
                    , "/usr/bin/yes o | /usr/bin/head -c 1048576; "
                        <> "/usr/bin/yes e | /usr/bin/head -c 1048576 >&2"
                    ]
                    (\stream bytes ->
                        atomicModifyIORef' byteCounts \(out, err) ->
                            ( case stream of
                                RepositoryCheckStdout ->
                                    (out + BS8.length bytes, err)
                                RepositoryCheckStderr ->
                                    (out, err + BS8.length bytes)
                            , ()
                            ))
                    (\cancelled exitCode ->
                        putMVar terminal (cancelled, exitCode))
            waitRepositoryCheck check
            takeMVar terminal `shouldReturn` (False, ExitSuccess)
            readIORef byteCounts `shouldReturn` (1048576, 1048576)

    it "reports one terminal result when an output callback throws" $
        withRepository \root -> do
            snapshot <- expectRight =<< repositorySnapshot root
            terminalCount <- newIORef (0 :: Int)
            terminal <- newEmptyMVar
            check <- expectRight
                =<< startRepositoryCheck
                    root
                    snapshot.snapshotId
                    "/bin/sh"
                    ["-c", "printf callback-value; exit 9"]
                    (\_ _ -> throwString "callback failed")
                    (\cancelled exitCode -> do
                        modifyIORef' terminalCount (+ 1)
                        putMVar terminal (cancelled, exitCode))
            waitRepositoryCheck check
            takeMVar terminal `shouldReturn` (False, ExitFailure 9)
            readIORef terminalCount `shouldReturn` 1

    it "cancels and joins a running check" $
        withRepository \root -> do
            snapshot <- expectRight =<< repositorySnapshot root
            terminal <- newEmptyMVar
            terminalCount <- newIORef (0 :: Int)
            check <- expectRight
                =<< startRepositoryCheck
                    root
                    snapshot.snapshotId
                    "/bin/sh"
                    [ "-c"
                    , "trap '' TERM; "
                        <> "(trap '' TERM; exec /bin/sleep 30) & wait"
                    ]
                    (\_ _ -> pure ())
                    (\cancelled exitCode -> do
                        modifyIORef' terminalCount (+ 1)
                        putMVar terminal (cancelled, exitCode))
            cancelRepositoryCheck check
            waitRepositoryCheck check
            result <- timeout 2_000_000 (takeMVar terminal)
            result `shouldSatisfy` \case
                Just (True, ExitFailure _) -> True
                _ -> False
            readIORef terminalCount `shouldReturn` 1

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

isInvalidRequest
    :: Either RepositoryError RepositorySnapshot
    -> Bool
isInvalidRequest = \case
    Left (InvalidRepositoryRequest _) -> True
    _ -> False

shouldReturnSatisfying
    :: (HasCallStack, Show value)
    => IO value
    -> (value -> Bool)
    -> Expectation
shouldReturnSatisfying action predicate =
    action >>= (`shouldSatisfy` predicate)

firstPatchHunk :: BS8.ByteString -> BS8.ByteString
firstPatchHunk patch =
    let lines' = BS8.lines patch
        isHunk = BS8.isPrefixOf "@@ "
        (header, body) = break isHunk lines'
    in case body of
        [] -> patch
        first:rest ->
            let (hunkBody, _) = break isHunk rest
            in BS8.unlines (header <> [first] <> hunkBody)
