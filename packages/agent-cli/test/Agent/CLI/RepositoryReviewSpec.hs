module Agent.CLI.RepositoryReviewSpec (spec) where

import Agent.CLI.RepositoryReview
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
    ( async
    , cancel
    , waitCatch
    )
import Control.Concurrent.MVar
    ( newEmptyMVar
    , putMVar
    , takeMVar
    )
import Control.Exception.Safe (bracket, throwString, tryAny)
import Control.Monad (forM_, when)
import qualified Data.ByteString.Char8 as BS8
import Data.Char (isLower, toUpper)
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
    , doesDirectoryExist
    , doesFileExist
    , findExecutable
    , getTemporaryDirectory
    , removeFile
    , removePathForcibly
    )
import System.Exit (ExitCode(..))
import System.Environment (getEnv, lookupEnv, setEnv, unsetEnv)
import System.IO (hClose, openTempFile)
import System.Process
    ( CreateProcess(..)
    , createProcess
    , proc
    , readCreateProcessWithExitCode
    , waitForProcess
    )
import System.Timeout (timeout)
import System.Posix.Files
    ( createNamedPipe
    , createSymbolicLink
    , ownerExecuteMode
    , ownerReadMode
    , ownerWriteMode
    , setFileMode
    , unionFileModes
    )
import System.Posix.Signals (nullSignal, signalProcess)
import Test.Hspec

spec :: Spec
spec = describe "repository review service" do
    it "rejects repository-local Git and disables configured helpers" $
        withRepository \root -> do
            let bin = root <> "/bin"
                fakeGit = bin <> "/git"
                gitMarker = root <> "/repository-git-ran"
                fsmonitor = root <> "/fsmonitor"
                fsmonitorMarker = root <> "/fsmonitor-ran"
                subdirectory = root <> "/nested"
            createDirectory bin
            createDirectory subdirectory
            writeFile (subdirectory <> "/.git") "not a repository boundary\n"
            writeFile fakeGit $
                "#!/bin/sh\nprintf ran > "
                    <> shellQuote gitMarker
                    <> "\nexec /usr/bin/git \"$@\"\n"
            writeFile fsmonitor $
                "#!/bin/sh\nprintf ran > "
                    <> shellQuote fsmonitorMarker
                    <> "\nexit 0\n"
            mapM_ (`setFileMode` 0o700) [fakeGit, fsmonitor]
            originalPath <- getEnv "PATH"
            bracket
                (setEnv "PATH" (bin <> ":" <> originalPath))
                (\_ -> setEnv "PATH" originalPath)
                \_ -> do
                    repositorySnapshot root
                        `shouldReturnSatisfying` isLeft
                    repositorySnapshot subdirectory
                        `shouldReturnSatisfying` isLeft
                    doesFileExist gitMarker `shouldReturn` False
            let caseVariantBin = changePathCase root <> "/bin"
            caseVariantExists <- doesDirectoryExist caseVariantBin
            when caseVariantExists $
                bracket
                    (setEnv "PATH" (caseVariantBin <> ":" <> originalPath))
                    (\_ -> setEnv "PATH" originalPath)
                    \_ -> do
                        repositorySnapshot root
                            `shouldReturnSatisfying` isLeft
                        doesFileExist gitMarker `shouldReturn` False

            _ <- git root ["config", "core.fsmonitor", fsmonitor]
            _ <- expectRight =<< repositorySnapshot root
            doesFileExist fsmonitorMarker `shouldReturn` False

    it "isolates snapshots from clean filters and injected Git config" $
        withRepository \root -> do
            let filterScript = root <> "/clean-filter"
                filterMarker = root <> "/clean-filter-ran"
                injectedConfig = root <> "/injected.gitconfig"
                injectedScript = root <> "/injected-fsmonitor"
                injectedMarker = root <> "/injected-fsmonitor-ran"
            writeFile filterScript $
                "#!/bin/sh\nprintf ran > "
                    <> shellQuote filterMarker
                    <> "\ncat\n"
            setFileMode filterScript 0o700
            writeFile (root <> "/.gitattributes") $
                "tracked.txt filter=pwn\n*.filtered filter=pwn\n"
            _ <- git root ["config", "filter.pwn.clean", filterScript]
            _ <- git root ["config", "filter.pwn.required", "true"]
            appendFile (root <> "/tracked.txt") "changed\n"
            writeFile (root <> "/untracked.filtered") "untracked\n"
            snapshot <- expectRight =<< repositorySnapshot root
            _ <- expectRight
                =<< repositoryDiff
                    root
                    snapshot.snapshotId
                    RepositoryWorktreeDiff
                    "tracked.txt"
            doesFileExist filterMarker `shouldReturn` False

            writeFile injectedScript $
                "#!/bin/sh\nprintf ran > "
                    <> shellQuote injectedMarker
                    <> "\nexit 0\n"
            setFileMode injectedScript 0o700
            writeFile injectedConfig $
                "[core]\n\tfsmonitor = "
                    <> injectedScript
                    <> "\n"
            originalGlobal <- lookupEnv "GIT_CONFIG_GLOBAL"
            bracket
                (setEnv "GIT_CONFIG_GLOBAL" injectedConfig)
                (\_ -> case originalGlobal of
                    Nothing -> unsetEnv "GIT_CONFIG_GLOBAL"
                    Just value -> setEnv "GIT_CONFIG_GLOBAL" value)
                \_ -> do
                    _ <- expectRight =<< repositorySnapshot root
                    doesFileExist injectedMarker `shouldReturn` False

    it "keeps isolated Git metadata outside the repository when TMPDIR is inside it" $
        withRepository \root -> do
            originalTemporaryDirectory <- lookupEnv "TMPDIR"
            bracket
                (setEnv "TMPDIR" root)
                (\_ -> case originalTemporaryDirectory of
                    Nothing -> unsetEnv "TMPDIR"
                    Just value -> setEnv "TMPDIR" value)
                \_ -> do
                    first <- expectRight =<< repositorySnapshot root
                    second <- expectRight =<< repositorySnapshot root
                    first.snapshotFiles `shouldBe` []
                    second.snapshotFiles `shouldBe` []
                    second.snapshotId `shouldBe` first.snapshotId

    it "preserves repository-local and configured exclude rules" $
        withRepository \root -> do
            let repositoryExclude = root <> "/.git/info/exclude"
                configuredExclude = root <> "/.git/configured-excludes"
            appendFile repositoryExclude
                ( "repository-secret.env\n"
                    <> "repository-wins.env\n"
                    <> "!repository-unignored.env\n"
                )
            writeFile configuredExclude
                ( "configured-secret.env\n"
                    <> "!repository-wins.env\n"
                    <> "repository-unignored.env\n"
                )
            _ <- git root
                [ "config"
                , "core.excludesFile"
                , ".git/configured-excludes"
                ]
            writeFile (root <> "/repository-secret.env") "secret\n"
            writeFile (root <> "/configured-secret.env") "secret\n"
            writeFile (root <> "/repository-wins.env") "secret\n"
            writeFile (root <> "/repository-unignored.env") "visible\n"
            snapshot <- expectRight =<< repositorySnapshot root
            map (.repositoryFilePath) snapshot.snapshotFiles
                `shouldBe` ["repository-unignored.env"]

    it "fails closed without blocking on a special exclude file" $
        withRepository \root -> do
            let repositoryExclude = root <> "/.git/info/exclude"
            removeFile repositoryExclude
            createNamedPipe repositoryExclude
                (ownerReadMode `unionFileModes` ownerWriteMode)
            result <- timeout 2_000_000 (repositorySnapshot root)
            result `shouldSatisfy` \case
                Just (Left (RepositoryCommandFailed _ _ _)) -> True
                _ -> False

    it "bounds repository-local config include stalls" $
        withRepository \root -> do
            let includedConfig = root <> "/.git/included-config"
            _ <- git root ["config", "include.path", includedConfig]
            createNamedPipe includedConfig
                (ownerReadMode `unionFileModes` ownerWriteMode)
            result <- timeout 3_000_000 (repositorySnapshot root)
            result `shouldSatisfy` \case
                Just (Left _) -> True
                _ -> False

    it "rejects untracked special files and fingerprints symlinks without following them" $
        withRepository \root -> do
            let untrackedPipe = root <> "/untracked-pipe"
                hiddenPipe = root <> "/.git/hidden-pipe"
                link = root <> "/pipe-link"
            createNamedPipe untrackedPipe
                (ownerReadMode `unionFileModes` ownerWriteMode)
            blocked <- timeout specialFileRegressionTimeoutMicros
                (repositorySnapshot root)
            blocked `shouldSatisfy` \case
                Just (Left (InvalidRepositoryRequest _)) -> True
                Just (Right _) -> True
                _ -> False
            removeFile untrackedPipe

            createNamedPipe hiddenPipe
                (ownerReadMode `unionFileModes` ownerWriteMode)
            createSymbolicLink ".git/hidden-pipe" link
            linked <- timeout specialFileRegressionTimeoutMicros
                (repositorySnapshot root)
            linked `shouldSatisfy` \case
                Just (Right snapshot) ->
                    any
                        ((== "pipe-link") . (.repositoryFilePath))
                        snapshot.snapshotFiles
                _ -> False
            case linked of
                Just (Right snapshot) ->
                    timeout specialFileRegressionTimeoutMicros
                        (repositoryDiff
                            root
                            snapshot.snapshotId
                            RepositoryWorktreeDiff
                            "pipe-link")
                        `shouldReturnSatisfying` \case
                            Just (Left (InvalidRepositoryRequest _)) -> True
                            _ -> False
                _ -> expectationFailure
                    "symlink snapshot was unavailable for diff regression"

    it "distinguishes an unborn HEAD from a broken HEAD" do
        withTempDirectory "repository-review-unborn" \root -> do
            _ <- git root ["init", "-q"]
            unborn <- expectRight =<< repositorySnapshot root
            unborn.snapshotHead `shouldBe` Nothing

            writeFile (root <> "/.git/HEAD") "not-a-valid-head\n"
            repositorySnapshot root `shouldReturnSatisfying` \case
                Left _ -> True
                Right _ -> False

    it "canonicalizes symlink aliases to one repository identity" $
        withRepository \root ->
            withTempDirectory "repository-review-alias" \container -> do
                let alias = container <> "/alias"
                createSymbolicLink root alias
                direct <- expectRight =<< repositorySnapshot root
                linked <- expectRight =<< repositorySnapshot alias
                linked.snapshotRoot `shouldBe` direct.snapshotRoot
                linked.snapshotId `shouldBe` direct.snapshotId

    it "honors the common-directory advisory transaction lock" $
        withRepository \root -> do
            pythonExecutable <-
                maybe (fail "python3 not found") pure
                    =<< findExecutable "python3"
            let lockPath =
                    root
                        <> "/.git/haskell-agent-worktree.lock"
                readyPath = root <> "/advisory-lock-ready"
                releasePath = root <> "/advisory-lock-release"
                script =
                    "import fcntl,os,time\n"
                        <> "f=open(" <> show lockPath <> ",'w')\n"
                        <> "fcntl.flock(f,fcntl.LOCK_EX)\n"
                        <> "open(" <> show readyPath <> ",'w').close()\n"
                        <> "while not os.path.exists("
                        <> show releasePath
                        <> "):\n time.sleep(0.01)\n"
            (_, _, _, locker) <-
                createProcess (proc pythonExecutable ["-c", script])
            _ <- awaitFileContents readyPath 200
            pending <- async (repositorySnapshot root)
            blocked <- timeout 200_000 (waitCatch pending)
            blocked `shouldSatisfy` \case
                Nothing -> True
                Just _ -> False
            writeFile releasePath ""
            _ <- waitForProcess locker
            waitCatch pending `shouldReturnSatisfying` \case
                Right (Right _) -> True
                _ -> False

    it "includes untracked file mode in the worktree fingerprint" $
        withRepository \root -> do
            let path = root <> "/untracked-mode.txt"
            writeFile path "same content\n"
            before <- expectRight =<< repositorySnapshot root
            setFileMode path
                (ownerReadMode
                    `unionFileModes` ownerWriteMode
                    `unionFileModes` ownerExecuteMode)
            after <- expectRight =<< repositorySnapshot root
            after.snapshotWorktreeFingerprint
                `shouldNotBe` before.snapshotWorktreeFingerprint

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

    it "rejects commit when the reviewed index fingerprint changed" $
        withRepository \root -> do
            appendFile (root <> "/tracked.txt") "reviewed\n"
            before <- expectRight =<< repositorySnapshot root
            staged <- expectRight
                =<< mutateRepository root before.snapshotId
                    (StagePath "tracked.txt")
            writeFile (root <> "/other.txt") "external\n"
            _ <- git root ["add", "other.txt"]

            commitRepository root staged.snapshotId "must not commit\n"
                `shouldReturnSatisfying` \case
                    Left (StaleRepositorySnapshot _ _) -> True
                    _ -> False
            git root ["log", "-1", "--pretty=%B"]
                `shouldReturn` "initial\n\n"

    it "applies reviewed patches and refuses to delete untracked files" $
        withRepository \root -> do
            appendFile (root <> "/tracked.txt") "patch\n"
            before <- expectRight =<< repositorySnapshot root
            staged <- expectRight
                =<< mutateRepository
                    root
                    before.snapshotId
                    (StageHunks "tracked.txt" [0])
            git root ["diff", "--cached", "--name-only"]
                `shouldReturn` "tracked.txt\n"

            _unstagedAfterPatch <- expectRight
                =<< mutateRepository
                    root
                    staged.snapshotId
                    (UnstageHunks "tracked.txt" [0])
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
            _ <- expectRight
                =<< mutateRepository root snapshot.snapshotId
                    (StageHunks "tracked.txt" [0])
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

            forM_
                [ "*"
                , ":!tracked.txt"
                , ":(glob)**"
                , ":(top)tracked.txt"
                , "../tracked.txt"
                , "a/../tracked.txt"
                ] $
                \path -> do
                    snapshot <- expectRight =<< repositorySnapshot root
                    mutateRepository root snapshot.snapshotId (StagePath path)
                        `shouldReturnSatisfying` isInvalidRequest
                    git root ["diff", "--cached", "--name-only"]
                        `shouldReturn` ""

    it "rejects invalid hunk selections and destructive selected patches" $
        withRepository \root -> do
            writeFile (root <> "/second.txt") "second\n"
            _ <- git root ["add", "second.txt"]
            _ <- git root ["commit", "-q", "-m", "second"]
            appendFile (root <> "/tracked.txt") "tracked change\n"
            appendFile (root <> "/second.txt") "second change\n"
            snapshot <- expectRight =<< repositorySnapshot root
            mutateRepository root snapshot.snapshotId
                (StageHunks "tracked.txt" [99])
                `shouldReturnSatisfying` isInvalidRequest
            mutateRepository root snapshot.snapshotId
                (StageHunks "not-in-snapshot.txt" [0])
                `shouldReturnSatisfying` isInvalidRequest
            mutateRepository root snapshot.snapshotId
                (StageHunks "tracked.txt" [0, 0])
                `shouldReturnSatisfying` isInvalidRequest
            git root ["diff", "--cached", "--name-only"] `shouldReturn` ""

            writeFile (root <> "/untracked.txt") "keep\n"
            untrackedSnapshot <- expectRight =<< repositorySnapshot root
            mutateRepository root untrackedSnapshot.snapshotId
                (RestoreHunks "untracked.txt" [0])
                `shouldReturnSatisfying` isInvalidRequest
            doesFileExist (root <> "/untracked.txt") `shouldReturn` True

            removeFile (root <> "/tracked.txt")
            deletedSnapshot <- expectRight =<< repositorySnapshot root
            mutateRepository root deletedSnapshot.snapshotId
                (StageHunks "tracked.txt" [0])
                `shouldReturnSatisfying` isInvalidRequest

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
                    , "yes o | head -c 1048576; "
                        <> "yes e | head -c 1048576 >&2"
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

    it "joins a check descendant that retains output after its leader exits" $
        withRepository \root -> do
            snapshot <- expectRight =<< repositorySnapshot root
            let pidFile = root <> "/check-background-child.pid"
            terminal <- newEmptyMVar
            check <- expectRight
                =<< startRepositoryCheck
                    root
                    snapshot.snapshotId
                    "/bin/sh"
                    [ "-c"
                    , "/bin/sh -c 'trap \"\" TERM; echo $$ > "
                        <> shellQuote pidFile
                        <> "; exec sleep 30' & "
                        <> "while [ ! -s "
                        <> shellQuote pidFile
                        <> " ]; do sleep 0.01; done; exit 0"
                    ]
                    (\_ _ -> pure ())
                    (\cancelled exitCode ->
                        putMVar terminal (cancelled, exitCode))
            childPid <- awaitFileContents pidFile 200
            timeout 2_000_000 (waitRepositoryCheck check)
                `shouldReturn` Just ()
            takeMVar terminal `shouldReturn` (False, ExitSuccess)
            awaitProcessGone (Text.strip childPid) 200
                `shouldReturn` True

    it "joins a successful commit hook descendant that retains Git output" $
        withRepository \root -> do
            pythonExecutable <-
                maybe (fail "python3 not found") pure
                    =<< findExecutable "python3"
            appendFile (root <> "/tracked.txt") "hook completion\n"
            before <- expectRight =<< repositorySnapshot root
            staged <- expectRight
                =<< mutateRepository root before.snapshotId
                    (StagePath "tracked.txt")
            let pidFile = root <> "/completed-hook-child.pid"
                hook = root <> "/.git/hooks/pre-commit"
            writeFile hook
                ( "#!" <> pythonExecutable <> "\n"
                    <> "import os, signal, time\n"
                    <> "signal.signal(signal.SIGTERM, signal.SIG_IGN)\n"
                    <> "pid = os.fork()\n"
                    <> "if pid:\n"
                    <> "    with open(" <> show pidFile
                    <> ", 'w') as stream:\n"
                    <> "        stream.write(str(pid))\n"
                    <> "    os._exit(0)\n"
                    <> "time.sleep(30)\n"
                )
            setFileMode hook
                (ownerReadMode
                    `unionFileModes` ownerWriteMode
                    `unionFileModes` ownerExecuteMode)

            result <- timeout 30_000_000
                (commitRepository root staged.snapshotId "completed hook\n")
            result `shouldSatisfy` \case
                Just (Right _) -> True
                _ -> False
            childPid <- awaitFileContents pidFile 200
            awaitProcessGone (Text.strip childPid) 200
                `shouldReturn` True

    it "kills commit-hook descendants when the commit worker is cancelled" $
        withRepository \root -> do
            appendFile (root <> "/tracked.txt") "hook cancellation\n"
            before <- expectRight =<< repositorySnapshot root
            staged <- expectRight
                =<< mutateRepository root before.snapshotId
                    (StagePath "tracked.txt")
            let pidFile = root <> "/hook-child.pid"
                hook = root <> "/.git/hooks/pre-commit"
            writeFile hook
                ( "#!/bin/sh\n"
                    <> "trap '' TERM\n"
                    <> "/bin/sh -c 'trap \"\" TERM; echo $$ > "
                    <> shellQuote pidFile
                    <> "; exec sleep 30' &\n"
                    <> "wait\n"
                )
            setFileMode hook
                (ownerReadMode
                    `unionFileModes` ownerWriteMode
                    `unionFileModes` ownerExecuteMode)

            worker <- async
                (commitRepository root staged.snapshotId "blocked hook\n")
            childPid <- awaitFileContents pidFile 200
            cancel worker
            _ <- waitCatch worker
            childGone <- awaitProcessGone (Text.strip childPid) 200
            childGone `shouldBe` True
            git root ["log", "-1", "--pretty=%B"] `shouldReturn` "initial\n\n"

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
                        <> "(trap '' TERM; exec sleep 30) & wait"
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
    appendFile
        (root <> "/.git/config")
        "\n[user]\n\tname = Repository Review Test\n\temail = review@example.test\n"
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

awaitFileContents :: FilePath -> Int -> IO Text.Text
awaitFileContents path attempts
    | attempts <= 0 =
        expectationFailure ("timed out waiting for " <> path) >> pure ""
    | otherwise =
        doesFileExist path >>= \case
            True -> Text.pack <$> readFile path
            False -> threadDelay 10_000 >> awaitFileContents path (attempts - 1)

processExists :: Text.Text -> IO Bool
processExists pid =
    case reads (Text.unpack pid) of
        [(processId, "")] -> do
            result <- tryAny (signalProcess nullSignal processId)
            pure (either (const False) (const True) result)
        _ -> pure False

awaitProcessGone :: Text.Text -> Int -> IO Bool
awaitProcessGone pid attempts =
    processExists pid >>= \case
        False -> pure True
        True
            | attempts <= 0 -> pure False
            | otherwise ->
                threadDelay 10_000 >> awaitProcessGone pid (attempts - 1)

shellQuote :: String -> String
shellQuote value = "'" <> concatMap escape value <> "'"
  where
    escape '\'' = "'\\''"
    escape character = [character]

changePathCase :: FilePath -> FilePath
changePathCase = go
  where
    go [] = []
    go (character : rest)
        | isLower character = toUpper character : rest
        | otherwise = character : go rest

-- A loaded Nix check can substantially delay the Git subprocesses that run
-- before special paths are inspected. Keep this as a deadlock guard rather
-- than a scheduler-sensitive performance assertion.
specialFileRegressionTimeoutMicros :: Int
specialFileRegressionTimeoutMicros = 75_000_000
