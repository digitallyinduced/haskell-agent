module Agent.CLI.RepositoryReview
    ( RepositorySnapshot(..)
    , RepositoryFile(..)
    , RepositoryDiff(..)
    , RepositoryDiffKind(..)
    , DiffHunk(..)
    , RepositoryMutation(..)
    , RepositoryCheck
    , RepositoryCheckStream(..)
    , RepositoryError(..)
    , repositorySnapshot
    , repositoryDiff
    , mutateRepository
    , commitRepository
    , startRepositoryCheck
    , cancelRepositoryCheck
    , waitRepositoryCheck
    , repositoryErrorText
    ) where

import Control.Concurrent (threadDelay)
import Control.Applicative ((<|>))
import Control.Concurrent.Async
    ( Async
    , asyncWithUnmask
    , cancel
    , poll
    , wait
    , waitCatch
    , withAsync
    )
import Control.Concurrent.MVar
    ( MVar
    , modifyMVar
    , newMVar
    , withMVar
    )
import Control.Exception.Safe
    ( SomeException
    , bracket
    , finally
    , isAsyncException
    , mask
    , onException
    , throwIO
    , throwString
    , tryAny
    , tryIO
    )
import Control.Monad (foldM, unless, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Char (isDigit, toLower)
import Data.List (find, nub, sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.IORef
    ( IORef
    , newIORef
    , readIORef
    , writeIORef
    )
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Text.Encoding.Error (lenientDecode)
import System.Exit (ExitCode(..))
import System.Environment (getEnvironment)
import qualified System.FileLock as FileLock
import System.IO (Handle, hClose)
import System.IO.Error (isDoesNotExistError)
import System.IO.Unsafe (unsafePerformIO)
import System.Directory
    ( canonicalizePath
    , createDirectory
    , doesDirectoryExist
    , getTemporaryDirectory
    , removePathForcibly
    )
import System.FilePath
    ( isAbsolute
    , makeRelative
    , normalise
    , splitDirectories
    , (</>)
    )
import System.Posix.Signals
    ( Signal
    , sigKILL
    , sigTERM
    , signalProcessGroup
    )
import System.Posix.Files
    ( fileMode
    , getFdStatus
    , getSymbolicLinkStatus
    , isRegularFile
    , isSymbolicLink
    , readSymbolicLink
    , setFileMode
    )
import System.Posix.IO
    ( OpenMode(ReadOnly)
    , cloexec
    , closeFd
    , defaultFileFlags
    , dup
    , fdToHandle
    , nonBlock
    , nofollow
    , openFd
    )
import System.Posix.Temp (mkdtemp)
import System.Posix.Types (FileMode, ProcessID)
import System.Process
    ( CreateProcess(..)
    , ProcessHandle
    , StdStream(CreatePipe)
    , createProcess
    , getPid
    , proc
    , terminateProcess
    , waitForProcess
    )
import System.Timeout (timeout)

import Agent.CLI.ProcessSecurity
    ( canonicalPathOutside
    , resolveExecutableOutside
    , sanitizeSearchPathOutside
    )

data RepositorySnapshot = RepositorySnapshot
    { snapshotId :: !Text
    , snapshotRoot :: !FilePath
    , snapshotHead :: !(Maybe Text)
    , snapshotIndexFingerprint :: !Text
    , snapshotWorktreeFingerprint :: !Text
    , snapshotFiles :: ![RepositoryFile]
    } deriving (Eq, Show)

data RepositoryDiffKind
    = RepositoryWorktreeDiff
    | RepositoryStagedDiff
    deriving (Eq, Show)

data RepositoryFile = RepositoryFile
    { repositoryFilePath :: !FilePath
    , repositoryFileOriginalPath :: !(Maybe FilePath)
    , repositoryFileIndexStatus :: !Char
    , repositoryFileWorktreeStatus :: !Char
    } deriving (Eq, Show)

data RepositoryDiff = RepositoryDiff
    { repositoryDiffPatch :: !BS.ByteString
    , repositoryDiffBinary :: !Bool
    , repositoryDiffHunks :: ![DiffHunk]
    } deriving (Eq, Show)

data DiffHunk = DiffHunk
    { hunkOldStart :: !Int
    , hunkOldCount :: !Int
    , hunkNewStart :: !Int
    , hunkNewCount :: !Int
    , hunkHeader :: !Text
    } deriving (Eq, Show)

data RepositoryMutation
    = StagePath !FilePath
    | UnstagePath !FilePath
    | RestorePath !FilePath
    | StageHunks !FilePath ![Int]
    | UnstageHunks !FilePath ![Int]
    | RestoreHunks !FilePath ![Int]
    deriving (Eq, Show)

data RepositoryCheckStream
    = RepositoryCheckStdout
    | RepositoryCheckStderr
    deriving (Eq, Show)

data RepositoryCheck = RepositoryCheck
    { repositoryCheckProcess :: !ProcessHandle
    , repositoryCheckProcessGroup :: !(Maybe ProcessID)
    , repositoryCheckWorker :: !(Async ())
    , repositoryCheckCancelled :: !(IORef Bool)
    }

data RepositoryError
    = NotARepository !Text
    | StaleRepositorySnapshot !Text !Text
    | InvalidRepositoryRequest !Text
    | RepositoryCommandFailed !Text !Int !Text
    deriving (Eq, Show)

repositorySnapshot :: FilePath -> IO (Either RepositoryError RepositorySnapshot)
repositorySnapshot requested = runRepositoryRead requested snapshotAtRoot

repositoryDiff
    :: FilePath
    -> Text
    -> RepositoryDiffKind
    -> FilePath
    -> IO (Either RepositoryError RepositoryDiff)
repositoryDiff requested expected kind path =
    withRepositoryMutationLock requested \root ->
        repositoryDiffAtRoot root expected kind path

repositoryDiffAtRoot
    :: FilePath
    -> Text
    -> RepositoryDiffKind
    -> FilePath
    -> IO (Either RepositoryError RepositoryDiff)
repositoryDiffAtRoot root expected kind path = do
        case validateRepositoryPath root path of
            Left err -> pure (Left err)
            Right () -> snapshotAtRoot root >>= \case
                Left err -> pure (Left err)
                Right snapshot
                    | snapshot.snapshotId /= expected ->
                        pure
                            (Left
                                (StaleRepositorySnapshot
                                    expected
                                    snapshot.snapshotId))
                    | not (snapshotContainsPath snapshot path) ->
                        pure
                            (Left
                                (InvalidRepositoryRequest
                                    "file path is not present in the reviewed snapshot"))
                    | otherwise -> do
                        let untracked =
                                kind == RepositoryWorktreeDiff
                                    && any
                                        (\file ->
                                            file.repositoryFilePath == path
                                                && file.repositoryFileIndexStatus
                                                    == '?')
                                        snapshot.snapshotFiles
                            arguments
                                | untracked =
                                    [ "--literal-pathspecs"
                                    , "diff"
                                    , "--no-index"
                                    , "--binary"
                                    , "--no-ext-diff"
                                    , "--no-textconv"
                                    , "--no-color"
                                    , "--"
                                    , "/dev/null"
                                    , path
                                    ]
                                | otherwise =
                                    [ "--literal-pathspecs"
                                    , "diff"
                                    , "--binary"
                                    , "--no-ext-diff"
                                    , "--no-textconv"
                                    , "--no-color"
                                    ]
                                        <> case kind of
                                            RepositoryWorktreeDiff -> []
                                            RepositoryStagedDiff -> ["--cached"]
                                        <> ["--", path]
                        result <-
                            if untracked
                                then
                                    validateUntrackedDiffPath root path >>= \case
                                        Left err -> pure (Left err)
                                        Right () -> runGitDiff root arguments
                                else runGitDiff root arguments
                        after <- snapshotAtRoot root
                        pure do
                            patch <- result
                            when
                                (BS.length patch > maxRepositoryPatchBytes)
                                (Left
                                    (InvalidRepositoryRequest
                                        "repository diff exceeds the 64 MiB limit"))
                            let hunks = parseDiffHunks patch
                            when
                                (length hunks > maxRepositoryDiffHunks)
                                (Left
                                    (InvalidRepositoryRequest
                                        "repository diff has too many hunks"))
                            latest <- after
                            if latest.snapshotId /= expected
                                then
                                    Left
                                        (StaleRepositorySnapshot
                                            expected
                                            latest.snapshotId)
                                else
                                    Right RepositoryDiff
                                        { repositoryDiffPatch = patch
                                        , repositoryDiffBinary =
                                            "GIT binary patch"
                                                `BS8.isInfixOf` patch
                                                || "Binary files "
                                                    `BS8.isInfixOf` patch
                                        , repositoryDiffHunks =
                                            hunks
                                        }

mutateRepository
    :: FilePath
    -> Text
    -> RepositoryMutation
    -> IO (Either RepositoryError RepositorySnapshot)
mutateRepository requested expected mutation =
    withRepositoryMutationLock requested \root -> do
        checked <- requireSnapshot root expected
        case checked of
            Left err -> pure (Left err)
            Right snapshot -> do
                result <- runMutation root snapshot mutation
                case result of
                    Left err -> pure (Left err)
                    Right () -> snapshotAtRoot root

commitRepository
    :: FilePath
    -> Text
    -> Text
    -> IO (Either RepositoryError RepositorySnapshot)
commitRepository requested expected rawMessage =
    withRepositoryMutationLock requested \root -> do
        checked <- requireSnapshot root expected
        case checked of
            Left err -> pure (Left err)
            Right _
                | Text.null (Text.strip rawMessage) ->
                    pure (Left (InvalidRepositoryRequest "commit message is empty"))
                | Text.length rawMessage > maxRepositoryCommitCharacters ->
                    pure
                        (Left
                            (InvalidRepositoryRequest
                                "commit message exceeds the 8 MiB character limit"))
                | Text.any (== '\NUL') rawMessage ->
                    pure
                        (Left
                            (InvalidRepositoryRequest
                                "commit message contains a NUL byte"))
                | otherwise -> do
                    -- Revalidate immediately before handing control to Git.
                    -- The repository lock excludes other API mutations; an
                    -- unrelated external writer remains outside that lock.
                    latest <- requireSnapshot root expected
                    case latest of
                        Left err -> pure (Left err)
                        Right _ -> do
                            committed <- runGit root
                                ["commit", "--file=-"]
                                (TextEncoding.encodeUtf8 rawMessage)
                            case committed of
                                Left err -> pure (Left err)
                                Right _ -> snapshotAtRoot root

startRepositoryCheck
    :: FilePath
    -> Text
    -> FilePath
    -> [String]
    -> (RepositoryCheckStream -> BS.ByteString -> IO ())
    -> (Bool -> ExitCode -> IO ())
    -> IO (Either RepositoryError RepositoryCheck)
startRepositoryCheck requested expected executable arguments onOutput onExit =
    runRepositoryRead requested \root -> do
        checked <- requireSnapshot root expected
        case checked of
            Left err -> pure (Left err)
            Right _
                | null executable || '\NUL' `elem` executable ->
                    pure
                        (Left
                            (InvalidRepositoryRequest
                                "check executable is invalid"))
                | length executable > maxRepositoryArgumentCharacters ->
                    pure
                        (Left
                            (InvalidRepositoryRequest
                                "check executable exceeds the 1 MiB character limit"))
                | length arguments > maxRepositoryCheckArguments ->
                    pure
                        (Left
                            (InvalidRepositoryRequest
                                "check has too many arguments"))
                | any (elem '\NUL') arguments ->
                    pure
                        (Left
                            (InvalidRepositoryRequest
                                "check argument contains a NUL byte"))
                | any null arguments ->
                    pure
                        (Left
                            (InvalidRepositoryRequest
                                "check argument is empty"))
                | any
                    ((> maxRepositoryArgumentCharacters) . length)
                    arguments ->
                        pure
                            (Left
                                (InvalidRepositoryRequest
                                    "check argument exceeds the 1 MiB character limit"))
                | sum (map (toInteger . length) arguments)
                    > toInteger maxRepositoryCheckTotalCharacters ->
                        pure
                            (Left
                                (InvalidRepositoryRequest
                                    "check arguments exceed the 8 MiB total limit"))
                | otherwise -> do
                    -- Keep asynchronous cancellation masked across process
                    -- acquisition and construction of its owning worker.
                    -- Once this returns, every resource is reachable through
                    -- RepositoryCheck and cancellation is owned by that
                    -- worker's finalizer.
                    created <- trySynchronous (mask \_ -> do
                        (maybeOutput, maybeError, process, processGroup) <-
                            createCheckProcess root executable arguments
                        cancelled <- newIORef False
                        let cleanup = do
                                closeQuietly maybeOutput
                                closeQuietly maybeError
                                signalCheckProcessGroup
                                    sigKILL processGroup process
                                _ <- tryAny (waitForProcess process)
                                pure ()
                        worker <- asyncWithUnmask
                            (\unmask ->
                                unmask
                                    (withAsync
                                        (drainCheckOutput
                                            RepositoryCheckStdout
                                            maybeOutput)
                                        \outputReader ->
                                            withAsync
                                                (drainCheckOutput
                                                    RepositoryCheckStderr
                                                    maybeError)
                                                \errorReader -> do
                                                    exitCode <-
                                                        waitForProcess process
                                                    -- The requested leader is
                                                    -- complete. Any process
                                                    -- still in its dedicated
                                                    -- group is a residual
                                                    -- descendant and must not
                                                    -- outlive the check.
                                                    signalCheckProcessGroup
                                                        sigKILL
                                                        processGroup
                                                        process
                                                    -- Reader failures
                                                    -- (including callback
                                                    -- failures) must not
                                                    -- suppress the one terminal
                                                    -- callback. An escaped
                                                    -- process outside the
                                                    -- group still cannot hold
                                                    -- this worker indefinitely.
                                                    drained <- timeout
                                                        processPipeTeardownMicros
                                                        ((,)
                                                            <$> waitCatch outputReader
                                                            <*> waitCatch errorReader)
                                                    case drained of
                                                        Just _ -> pure ()
                                                        Nothing -> do
                                                            closeQuietly
                                                                maybeOutput
                                                            closeQuietly
                                                                maybeError
                                                            cancel outputReader
                                                            cancel errorReader
                                                    wasCancelled <-
                                                        readIORef cancelled
                                                    onExit
                                                        wasCancelled
                                                        exitCode)
                                    `finally` cleanup)
                                `onException` cleanup
                        pure RepositoryCheck
                            { repositoryCheckProcess = process
                            , repositoryCheckProcessGroup = processGroup
                            , repositoryCheckWorker = worker
                            , repositoryCheckCancelled = cancelled
                            })
                    pure case created of
                        Left exception ->
                            Left
                                (RepositoryCommandFailed
                                    (renderCommand executable arguments)
                                    (-1)
                                    (Text.pack (show exception)))
                        Right check -> Right check
  where
    drainCheckOutput stream handle =
        let drain callbackEnabled = do
                bytes <- BS.hGetSome handle (64 * 1024)
                if BS.null bytes
                    then pure ()
                    else do
                        nextEnabled <-
                            if callbackEnabled
                                then
                                    trySynchronous (onOutput stream bytes) >>= \case
                                        Left _ -> pure False
                                        Right () -> pure True
                                else pure False
                        -- A failed foreign callback is disabled for this
                        -- stream, but the pipe remains actively drained.
                        drain nextEnabled
        in drain True `finally` closeQuietly handle

cancelRepositoryCheck :: RepositoryCheck -> IO ()
cancelRepositoryCheck check = do
    writeIORef check.repositoryCheckCancelled True
    signalCheckProcessGroup
        sigTERM
        check.repositoryCheckProcessGroup
        check.repositoryCheckProcess
    threadDelay 250_000
    poll check.repositoryCheckWorker >>= \case
        Nothing ->
            signalCheckProcessGroup
                sigKILL
                check.repositoryCheckProcessGroup
                check.repositoryCheckProcess
        Just _ -> pure ()
    -- Cancellation owns process teardown: do not return while descendants,
    -- pipe readers, or the terminal callback are still active.
    waitRepositoryCheck check

waitRepositoryCheck :: RepositoryCheck -> IO ()
waitRepositoryCheck check = do
    _ <- waitCatch check.repositoryCheckWorker
    pure ()

signalCheckProcessGroup
    :: Signal
    -> Maybe ProcessID
    -> ProcessHandle
    -> IO ()
signalCheckProcessGroup signal processGroup process = do
    processId <- maybe (getPid process) (pure . Just) processGroup
    case processId of
        Nothing -> do
            _ <- tryAny (terminateProcess process)
            pure ()
        Just pid -> do
            _ <- tryAny (signalProcessGroup signal pid)
            pure ()

createCheckProcess
    :: FilePath
    -> FilePath
    -> [String]
    -> IO (Handle, Handle, ProcessHandle, Maybe ProcessID)
createCheckProcess root executable arguments = mask \_ -> do
    (maybeInput, maybeOutput, maybeError, process) <-
        createProcess
            (proc executable arguments)
                { cwd = Just root
                , std_in = CreatePipe
                , std_out = CreatePipe
                , std_err = CreatePipe
                , close_fds = True
                , create_group = True
                }
    case (maybeInput, maybeOutput, maybeError) of
        (Just input, Just output, Just errors) -> do
            let cleanupWithoutGroup = do
                    closeQuietly input
                    closeQuietly output
                    closeQuietly errors
                    _ <- tryAny (terminateProcess process)
                    _ <- tryAny (waitForProcess process)
                    pure ()
            processGroup <- getPid process
                `onException` cleanupWithoutGroup
            let cleanup = do
                    closeQuietly input
                    closeQuietly output
                    closeQuietly errors
                    signalCheckProcessGroup sigKILL processGroup process
                    _ <- tryAny (waitForProcess process)
                    pure ()
            -- Checks have no stdin API. Closing immediately prevents an
            -- inherited terminal or pipe from leaving a check blocked.
            (hClose input >> pure (output, errors, process, processGroup))
                `onException` cleanup
        _ -> do
            _ <- tryAny (terminateProcess process)
            _ <- tryAny (waitForProcess process)
            fail "could not create check output pipes"

snapshotAtRoot :: FilePath -> IO (Either RepositoryError RepositorySnapshot)
snapshotAtRoot root =
    withIsolatedRepositoryGit root \headOid environment -> do
        indexResult <-
            runGitWithEnvironment
                environment root ["ls-files", "--stage", "-z"] BS.empty
        statusResult <-
            runGitWithEnvironment
                environment
                root
                [ "status"
                , "--porcelain=v1"
                , "-z"
                , "--untracked-files=all"
                , "--ignore-submodules=all"
                ]
                BS.empty
        diffResult <-
            runGitWithEnvironment
                environment
                root
                [ "diff"
                , "--binary"
                , "--no-ext-diff"
                , "--no-textconv"
                , "--no-color"
                , "--ignore-submodules=all"
                ]
                BS.empty
        case (indexResult, statusResult, diffResult) of
            (Right indexBytes, Right statusBytes, Right diffBytes) -> do
                worktreeMaterial <-
                    appendUntrackedHashesWith
                        (runGitWithEnvironment environment)
                        root
                        statusBytes
                        diffBytes
                indexHash <- hashMaterialWith
                    (runGitWithEnvironment environment)
                    root
                    indexBytes
                worktreeHash <- case worktreeMaterial of
                    Left err -> pure (Left err)
                    Right material ->
                        hashMaterialWith
                            (runGitWithEnvironment environment)
                            root
                            material
                pure do
                    indexFingerprint <- indexHash
                    worktreeFingerprint <- worktreeHash
                    let headFingerprint = fromMaybe "unborn" headOid
                        identity = Text.intercalate ":"
                            [ headFingerprint
                            , indexFingerprint
                            , worktreeFingerprint
                            ]
                    files <- parsePorcelain statusBytes
                    pure RepositorySnapshot
                        { snapshotId = identity
                        , snapshotRoot = root
                        , snapshotHead = headOid
                        , snapshotIndexFingerprint = indexFingerprint
                        , snapshotWorktreeFingerprint = worktreeFingerprint
                        , snapshotFiles = files
                        }
            (Left err, _, _) -> pure (Left err)
            (_, Left err, _) -> pure (Left err)
            (_, _, Left err) -> pure (Left err)

repositoryHead
    :: FilePath
    -> IO (Either RepositoryError (Maybe Text))
repositoryHead root =
    runGit root ["rev-parse", "--verify", "HEAD"] BS.empty >>= \case
        Right oid -> pure (Right (Just (decodeTrimmed oid)))
        Left headError ->
            -- A symbolic HEAD whose branch ref does not exist is the normal
            -- unborn-repository case. Detached/corrupt HEAD and command
            -- failures retain their original error instead of silently
            -- becoming "unborn".
            runGit root ["symbolic-ref", "--quiet", "HEAD"] BS.empty >>= \case
                Right reference ->
                    runGit root
                        [ "show-ref"
                        , "--verify"
                        , "--quiet"
                        , Text.unpack (decodeTrimmed reference)
                        ]
                        BS.empty >>= \case
                            Left (RepositoryCommandFailed _ 1 _) ->
                                pure (Right Nothing)
                            Left referenceError ->
                                pure (Left referenceError)
                            Right _ -> pure (Left headError)
                Left _ -> pure (Left headError)

requireSnapshot
    :: FilePath
    -> Text
    -> IO (Either RepositoryError RepositorySnapshot)
requireSnapshot root expected = do
    current <- snapshotAtRoot root
    pure do
        snapshot <- current
        if snapshot.snapshotId == expected
            then Right snapshot
            else Left (StaleRepositorySnapshot expected snapshot.snapshotId)

runMutation
    :: FilePath
    -> RepositorySnapshot
    -> RepositoryMutation
    -> IO (Either RepositoryError ())
runMutation root snapshot = \case
    StagePath path ->
        checkedPath path $
            applyPathPatch RepositoryWorktreeDiff ["--cached"] path
    UnstagePath path ->
        checkedPath path $
            applyPathPatch RepositoryStagedDiff ["--cached", "--reverse"] path
    RestorePath path ->
        checkedPath path $
            if isUntracked path
                then pure
                    (Left
                        (InvalidRepositoryRequest
                            "restore never deletes an untracked file"))
                else applyPathPatch
                    RepositoryWorktreeDiff ["--reverse"] path
    StageHunks path hunks ->
        checkedPath path $
            applyReviewedHunks
                RepositoryWorktreeDiff ["--cached"] path hunks
    UnstageHunks path hunks ->
        checkedPath path $
            applyReviewedHunks
                RepositoryStagedDiff ["--cached", "--reverse"] path hunks
    RestoreHunks path hunks ->
        checkedPath path $
            if isUntracked path
                then pure
                    (Left
                        (InvalidRepositoryRequest
                            "restore never deletes an untracked file"))
                else applyReviewedHunks
                    RepositoryWorktreeDiff ["--reverse"] path hunks
  where
    checkedPath path action
        = case validateRepositoryPath root path of
            Left err -> pure (Left err)
            Right ()
                | not (snapshotContainsPath snapshot path) ->
                    pure
                        (Left
                            (InvalidRepositoryRequest
                                "file path is not present in the reviewed snapshot"))
                | otherwise -> action
    isUntracked path = any
        (\file ->
            file.repositoryFilePath == path
                && file.repositoryFileIndexStatus == '?')
        snapshot.snapshotFiles
    applyPathPatch kind flags path =
        repositoryDiffAtRoot root snapshot.snapshotId kind path >>= \case
            Left err -> pure (Left err)
            Right diff
                | BS.null diff.repositoryDiffPatch -> pure (Right ())
                | otherwise ->
                    applyPatch
                        root snapshot.snapshotId flags diff.repositoryDiffPatch
    applyReviewedHunks kind flags path indices =
        repositoryDiffAtRoot root snapshot.snapshotId kind path >>= \case
            Left err -> pure (Left err)
            Right reviewed ->
                case selectReviewedHunks
                    snapshot path reviewed.repositoryDiffPatch indices of
                    Left err -> pure (Left err)
                    Right patch ->
                        applyPatch root snapshot.snapshotId flags patch

applyPatch
    :: FilePath
    -> Text
    -> [String]
    -> BS.ByteString
    -> IO (Either RepositoryError ())
applyPatch root expected flags patch
    | BS.null patch =
        pure (Left (InvalidRepositoryRequest "patch is empty"))
    | otherwise = do
        -- The in-process lock serializes bridge mutations. Revalidate as close
        -- as possible to the single atomic git-apply invocation; an unrelated
        -- external git writer cannot be locked by this API.
        checked <- requireSnapshot root expected
        case checked of
            Left err -> pure (Left err)
            Right _ ->
                withIsolatedRepositoryGit root \_ environment ->
                    voidResult
                        <$> runGitWithEnvironment
                            environment
                            root
                            (["apply", "--recount"] <> flags <> ["-"])
                            patch

validateRepositoryPath
    :: FilePath
    -> FilePath
    -> Either RepositoryError ()
validateRepositoryPath root path
    | null path
        || path == "."
        || isAbsolute path
        || normalise path /= path
        || any (`elem` [".", ".."]) (splitDirectories path)
        || ':' `isPrefixOfPath` path
        || any (`elem` ("*?[" :: String)) path
        || '\NUL' `elem` path =
            Left
                (InvalidRepositoryRequest
                    "file path must be a normalized literal repository-relative path")
    | let relative = makeRelative (normalise root) (normalise (root </> path))
    , isAbsolute relative
        || relative == ".."
        || case splitDirectories relative of
            "..":_ -> True
            _ -> False =
            Left
                (InvalidRepositoryRequest
                    "file path escapes the repository root")
    | otherwise = Right ()
  where
    isPrefixOfPath character value =
        case value of
            first:_ -> first == character
            [] -> False

selectReviewedHunks
    :: RepositorySnapshot
    -> FilePath
    -> BS.ByteString
    -> [Int]
    -> Either RepositoryError BS.ByteString
selectReviewedHunks snapshot path reviewed indices
    | null indices || length indices /= length (nub indices) =
        Left (InvalidRepositoryRequest "hunk selection is empty or duplicated")
    | length indices > maxRepositorySelectedHunks =
        Left (InvalidRepositoryRequest "too many selected hunks")
    | BS.length reviewed > maxRepositoryPatchBytes =
        Left (InvalidRepositoryRequest "repository patch exceeds the 64 MiB limit")
    | isDestructivePatch =
        Left
            (InvalidRepositoryRequest
                "selected-hunk mutation does not allow deletion or rename patches")
    | otherwise = case textPatchSections reviewed of
        Nothing ->
            Left
                (InvalidRepositoryRequest
                    "selected hunks require a non-binary text diff")
        Just (header, hunks)
            | any (\index -> index < 0 || index >= length hunks) indices ->
                Left (InvalidRepositoryRequest "hunk index is out of range")
            | otherwise ->
                Right
                    (BS8.unlines
                        (header <> concatMap (hunks !!) (sort indices)))
  where
    file = find
        (\entry -> entry.repositoryFilePath == path)
        snapshot.snapshotFiles
    isDestructivePatch =
        maybe False
            (\entry -> isJust entry.repositoryFileOriginalPath)
            file
            || any
                (`BS8.isInfixOf` reviewed)
                [ "deleted file mode "
                , "rename from "
                , "rename to "
                , "+++ /dev/null"
                ]

textPatchSections
    :: BS.ByteString
    -> Maybe ([BS.ByteString], [[BS.ByteString]])
textPatchSections patch = do
    let lines' = BS8.lines patch
        (header, body) = break isHunkHeader lines'
    unless (not (null header) && startsDiff header) Nothing
    hunks <- splitHunks body
    pure (header, hunks)
  where
    isHunkHeader = BS8.isPrefixOf "@@ "
    startsDiff (first:_) = BS8.isPrefixOf "diff --git " first
    startsDiff [] = False
    splitHunks [] = Nothing
    splitHunks lines' =
        let collect (current, completed) line
                | isHunkHeader line =
                    ( [line]
                    , if null current
                        then completed
                        else reverse current : completed
                    )
                | null current = ([], completed)
                | otherwise = (line : current, completed)
            (lastHunk, reversed) = foldl' collect ([], []) lines'
            result = reverse
                (if null lastHunk
                    then reversed
                    else reverse lastHunk : reversed)
        in if null result then Nothing else Just result

appendUntrackedHashesWith
    :: (FilePath
        -> [String]
        -> BS.ByteString
        -> IO (Either RepositoryError BS.ByteString))
    -> FilePath
    -> BS.ByteString
    -> BS.ByteString
    -> IO (Either RepositoryError BS.ByteString)
appendUntrackedHashesWith run root statusBytes diffBytes =
    foldM addHash (Right (statusBytes <> diffBytes)) untracked
  where
    untracked =
        [ file.repositoryFilePath
        | file <- either (const []) id (parsePorcelain statusBytes)
        , file.repositoryFileIndexStatus == '?'
        ]
    addHash (Left err) _ = pure (Left err)
    addHash (Right material) path = do
        fingerprintUntrackedPath run root path >>= \case
            Left err -> pure (Left err)
            Right (contentHash, mode) ->
                pure
                    (Right
                        ( material
                            <> contentHash
                            <> BS8.pack (show mode)
                            <> "\NUL"
                        ))

fingerprintUntrackedPath
    :: (FilePath
        -> [String]
        -> BS.ByteString
        -> IO (Either RepositoryError BS.ByteString))
    -> FilePath
    -> FilePath
    -> IO (Either RepositoryError (BS.ByteString, FileMode))
fingerprintUntrackedPath run root path = do
    let absolutePath = root </> path
    trySynchronous (getSymbolicLinkStatus absolutePath) >>= \case
        Left exception -> pure (Left (fingerprintError exception))
        Right status
            | isRegularFile status ->
                readUntrackedRegularFile absolutePath >>= \case
                    Left err -> pure (Left err)
                    Right (contents, openedMode) ->
                        hashContents contents openedMode
            | isSymbolicLink status ->
                trySynchronous
                    (TextEncoding.encodeUtf8 . Text.pack
                        <$> readSymbolicLink absolutePath) >>= \case
                            Left exception ->
                                pure (Left (fingerprintError exception))
                            Right contents ->
                                hashContents contents (fileMode status)
            | otherwise ->
                pure
                    (Left
                        (InvalidRepositoryRequest
                            "untracked special files cannot be reviewed"))
  where
    hashContents contents mode =
        run root
            [ "hash-object"
            , "--no-filters"
            , "--stdin"
            ]
            contents >>= \case
                Left err -> pure (Left err)
                Right contentHash -> pure (Right (contentHash, mode))
    fingerprintError exception =
        InvalidRepositoryRequest
            ("could not fingerprint untracked file: "
                <> Text.pack (show exception))

readUntrackedRegularFile
    :: FilePath
    -> IO (Either RepositoryError (BS.ByteString, FileMode))
readUntrackedRegularFile path =
    trySynchronous
        (bracket
            (openFd
                path
                ReadOnly
                defaultFileFlags
                    { nofollow = True
                    , nonBlock = True
                    , cloexec = True
                    })
            closeFd
            \descriptor -> do
                status <- getFdStatus descriptor
                unless (isRegularFile status) $
                    throwString "untracked file changed type while fingerprinting"
                duplicate <- dup descriptor
                handle <- fdToHandle duplicate
                    `onException` closeFd duplicate
                bytes <-
                    BS.hGet
                        handle
                        (maxRepositoryUntrackedFileBytes + 1)
                        `finally` hClose handle
                when
                    (BS.length bytes > maxRepositoryUntrackedFileBytes)
                    (throwString
                        "untracked file exceeds the review size limit")
                pure (bytes, fileMode status)) >>= \case
                    Left exception ->
                        pure
                            (Left
                                (InvalidRepositoryRequest
                                    ("could not fingerprint untracked file: "
                                        <> Text.pack (show exception))))
                    Right bytes -> pure (Right bytes)

hashMaterialWith
    :: (FilePath
        -> [String]
        -> BS.ByteString
        -> IO (Either RepositoryError BS.ByteString))
    -> FilePath
    -> BS.ByteString
    -> IO (Either RepositoryError Text)
hashMaterialWith run root bytes =
    fmap decodeTrimmed
        <$> run root ["hash-object", "--no-filters", "--stdin"] bytes

parsePorcelain :: BS.ByteString -> Either RepositoryError [RepositoryFile]
parsePorcelain bytes = go (filter (not . BS.null) (BS.split 0 bytes)) []
  where
    go [] acc = Right (reverse acc)
    go (entry:rest) acc
        | BS.length entry < 3 =
            Left (InvalidRepositoryRequest "git returned malformed status")
        | otherwise =
            let indexStatus = toStatus (BS.index entry 0)
                worktreeStatus = toStatus (BS.index entry 1)
                path = decodePath (BS.drop 3 entry)
                renamed = indexStatus `elem` ['R', 'C']
                    || worktreeStatus `elem` ['R', 'C']
            in if renamed
                then case rest of
                    [] ->
                        Left
                            (InvalidRepositoryRequest
                                "git returned an incomplete rename status")
                    original:remaining ->
                        go remaining
                            (RepositoryFile
                                { repositoryFilePath = path
                                , repositoryFileOriginalPath =
                                    Just (decodePath original)
                                , repositoryFileIndexStatus = indexStatus
                                , repositoryFileWorktreeStatus = worktreeStatus
                                }
                                : acc)
                else
                    go rest
                        (RepositoryFile
                            { repositoryFilePath = path
                            , repositoryFileOriginalPath = Nothing
                            , repositoryFileIndexStatus = indexStatus
                            , repositoryFileWorktreeStatus = worktreeStatus
                            }
                            : acc)
    toStatus byte
        | byte == 32 = ' '
        | otherwise = toEnum (fromIntegral byte)

parseDiffHunks :: BS.ByteString -> [DiffHunk]
parseDiffHunks =
    reverse
        . foldl' collect []
        . Text.lines
        . TextEncoding.decodeUtf8With lenientDecode
  where
    collect acc line =
        maybe acc (: acc) (parseHunkHeader line)

parseHunkHeader :: Text -> Maybe DiffHunk
parseHunkHeader line = do
    body <- Text.stripPrefix "@@ -" line
    let (oldRange, afterOld) = Text.breakOn " +" body
    newAndHeader <- Text.stripPrefix " +" afterOld
    let (newRange, afterNew) = Text.breakOn " @@" newAndHeader
    header <- Text.stripPrefix " @@" afterNew
    (oldStart, oldCount) <- parseRange oldRange
    (newStart, newCount) <- parseRange newRange
    pure DiffHunk
        { hunkOldStart = oldStart
        , hunkOldCount = oldCount
        , hunkNewStart = newStart
        , hunkNewCount = newCount
        , hunkHeader = Text.strip header
        }

parseRange :: Text -> Maybe (Int, Int)
parseRange text = case Text.splitOn "," text of
    [start] -> (, 1) <$> decimal start
    [start, count] -> (,) <$> decimal start <*> decimal count
    _ -> Nothing
  where
    decimal value
        | Text.null value || Text.any (not . isDigit) value = Nothing
        | otherwise = case reads (Text.unpack value) of
            [(number, "")] -> Just number
            _ -> Nothing

runRepositoryRead
    :: FilePath
    -> (FilePath -> IO (Either RepositoryError value))
    -> IO (Either RepositoryError value)
runRepositoryRead requested action = do
    withRepositoryMutationLock requested action

withRepositoryMutationLock
    :: FilePath
    -> (FilePath -> IO (Either RepositoryError value))
    -> IO (Either RepositoryError value)
withRepositoryMutationLock requested action = do
    rootResult <- repositoryRoot requested
    case rootResult of
        Left err -> pure (Left err)
        Right root -> do
            lock <- modifyMVar repositoryLocks \locks ->
                case Map.lookup root locks of
                    Just existing -> pure (locks, existing)
                    Nothing -> do
                        created <- newMVar ()
                        pure (Map.insert root created locks, created)
            withMVar lock \_ -> do
                lockPathResult <- repositoryAdvisoryLockPath root
                case lockPathResult of
                    Left err -> pure (Left err)
                    Right lockPath ->
                        trySynchronous
                            (FileLock.withFileLock
                                lockPath
                                FileLock.Exclusive
                                (const (action root))) >>= \case
                                    Left exception ->
                                        pure
                                            (Left
                                                (RepositoryCommandFailed
                                                    "repository advisory lock"
                                                    (-1)
                                                    (Text.pack
                                                        (show exception))))
                                    Right result -> pure result

repositoryAdvisoryLockPath
    :: FilePath
    -> IO (Either RepositoryError FilePath)
repositoryAdvisoryLockPath root =
    repositoryCommonDirectory root >>= \case
        Left err -> pure (Left err)
        Right commonDirectory ->
            pure
                (Right
                    (commonDirectory
                        </> "haskell-agent-worktree.lock"))

repositoryCommonDirectory
    :: FilePath
    -> IO (Either RepositoryError FilePath)
repositoryCommonDirectory root =
    runGit root ["rev-parse", "--git-common-dir"] BS.empty >>= \case
        Left err -> pure (Left err)
        Right commonDirectory -> do
            let commonPath = Text.unpack (decodeTrimmed commonDirectory)
            trySynchronous
                (canonicalizePath
                    (if isAbsolute commonPath
                        then commonPath
                        else root </> commonPath))
                >>= \case
                    Left exception ->
                        pure
                            (Left
                                (RepositoryCommandFailed
                                    "resolve repository common directory"
                                    (-1)
                                    (Text.pack (show exception))))
                    Right canonical ->
                        pure (Right canonical)

repositoryRoot :: FilePath -> IO (Either RepositoryError FilePath)
repositoryRoot requested
    | null requested =
        pure (Left (NotARepository "repository path is empty"))
    | otherwise =
        runGitWithTimeout
            repositoryDiscoveryTimeoutMicros
            requested
            ["rev-parse", "--show-toplevel"]
            BS.empty >>= \case
            Left err -> pure (Left (NotARepository (repositoryErrorText err)))
            Right rootBytes ->
                trySynchronous
                    (canonicalizePath
                        (Text.unpack (decodeTrimmed rootBytes))) >>= \case
                            Left exception ->
                                pure
                                    (Left
                                        (NotARepository
                                            (Text.pack (show exception))))
                            Right canonical -> pure (Right canonical)

snapshotContainsPath :: RepositorySnapshot -> FilePath -> Bool
snapshotContainsPath snapshot path =
    any (\file -> file.repositoryFilePath == path) snapshot.snapshotFiles

{-# NOINLINE repositoryLocks #-}
repositoryLocks :: MVar (Map FilePath (MVar ()))
repositoryLocks = unsafePerformIO (newMVar Map.empty)

runGit
    :: FilePath
    -> [String]
    -> BS.ByteString
    -> IO (Either RepositoryError BS.ByteString)
runGit =
    runGitWithTimeout maxRepositoryGitCommandMicros

runGitWithTimeout
    :: Int
    -> FilePath
    -> [String]
    -> BS.ByteString
    -> IO (Either RepositoryError BS.ByteString)
runGitWithTimeout timeoutMicros root arguments input = do
    result <-
        trySynchronous
            (timeout timeoutMicros
                (runProcessBytesWithEnvironment
                    []
                    root
                    "git"
                    (safeGitArguments <> arguments)
                    input))
    pure case result of
        Left exception ->
            Left
                (RepositoryCommandFailed
                    (renderCommand "git" arguments)
                    (-1)
                    (Text.pack (show exception)))
        Right Nothing ->
            Left
                (RepositoryCommandFailed
                    (renderCommand "git" arguments)
                    (-1)
                    "repository Git command timed out")
        Right (Just (exitCode, output, errors)) ->
            gitProcessResult arguments exitCode output errors

runGitWithEnvironment
    :: [(String, String)]
    -> FilePath
    -> [String]
    -> BS.ByteString
    -> IO (Either RepositoryError BS.ByteString)
runGitWithEnvironment environment root arguments input = do
    result <-
        trySynchronous
            (timeout maxRepositoryGitCommandMicros
                (runProcessBytesWithEnvironment
                    environment
                    root
                    "git"
                    (safeGitArguments <> arguments)
                    input))
    pure case result of
        Left exception ->
            Left
                (RepositoryCommandFailed
                    (renderCommand "git" arguments)
                    (-1)
                    (Text.pack (show exception)))
        Right Nothing ->
            Left
                (RepositoryCommandFailed
                    (renderCommand "git" arguments)
                    (-1)
                    "repository Git command timed out")
        Right (Just (exitCode, output, errors)) ->
            gitProcessResult arguments exitCode output errors

gitProcessResult
    :: [String]
    -> ExitCode
    -> BS.ByteString
    -> BS.ByteString
    -> Either RepositoryError BS.ByteString
gitProcessResult arguments exitCode output errors =
    case exitCode of
        ExitSuccess -> Right output
        ExitFailure code ->
            Left
                (RepositoryCommandFailed
                    (renderCommand "git" arguments)
                    code
                    (Text.strip
                        (TextEncoding.decodeUtf8With lenientDecode errors)))

runGitDiff
    :: FilePath
    -> [String]
    -> IO (Either RepositoryError BS.ByteString)
runGitDiff root arguments = do
    withIsolatedRepositoryGit root \_ environment ->
        runGitDiffWithEnvironment environment root arguments

runGitDiffWithEnvironment
    :: [(String, String)]
    -> FilePath
    -> [String]
    -> IO (Either RepositoryError BS.ByteString)
runGitDiffWithEnvironment environment root arguments = do
    result <-
        trySynchronous
            (timeout maxRepositoryDiffCommandMicros
                (runProcessBytesWithEnvironment
                    environment
                    root
                    "git"
                    (safeGitArguments <> arguments)
                    BS.empty))
    pure case result of
        Left exception ->
            Left
                (RepositoryCommandFailed
                    (renderCommand "git" arguments)
                    (-1)
                    (Text.pack (show exception)))
        Right Nothing ->
            Left
                (RepositoryCommandFailed
                    (renderCommand "git" arguments)
                    (-1)
                    "repository diff command timed out")
        Right (Just (exitCode, output, errors)) -> case exitCode of
            ExitSuccess -> Right output
            ExitFailure 1 -> Right output
            ExitFailure code ->
                Left
                    (RepositoryCommandFailed
                        (renderCommand "git" arguments)
                        code
                        (Text.strip
                            (TextEncoding.decodeUtf8With lenientDecode errors)))

validateUntrackedDiffPath
    :: FilePath
    -> FilePath
    -> IO (Either RepositoryError ())
validateUntrackedDiffPath root path =
    trySynchronous (getSymbolicLinkStatus (root </> path)) >>= \case
        Left exception ->
            pure
                (Left
                    (InvalidRepositoryRequest
                        ("could not inspect untracked diff path: "
                            <> Text.pack (show exception))))
        Right status
            | isRegularFile status -> pure (Right ())
            | otherwise ->
                pure
                    (Left
                        (InvalidRepositoryRequest
                            "untracked special-file diffs are not supported"))

runProcessBytesWithEnvironment
    :: [(String, String)]
    -> FilePath
    -> FilePath
    -> [String]
    -> BS.ByteString
    -> IO (ExitCode, BS.ByteString, BS.ByteString)
runProcessBytesWithEnvironment
    overrides
    workingDirectory
    executable
    arguments
    input =
    bracket start stop
        \(inputHandle, outputHandle, errorHandle, process, processGroup, completed) -> do
        withAsync
            (BS.hPut inputHandle input `finally` closeQuietly inputHandle)
            \inputWriter ->
                withAsync
                    (hGetBounded
                        maxRepositoryProcessOutputBytes
                        outputHandle)
                    \outputReader ->
                    withAsync
                        (hGetBounded
                            maxRepositoryProcessErrorBytes
                            errorHandle)
                        \errorReader -> do
                        exitCode <- waitForProcess process
                        -- The requested leader is complete. Kill any residual
                        -- member of its dedicated group before waiting for
                        -- pipe EOF; escaped groups are bounded by the timeout.
                        signalCheckProcessGroup
                            sigKILL processGroup process
                        _ <- wait inputWriter
                        drained <- timeout
                            processPipeTeardownMicros
                            ((,) <$> wait outputReader <*> wait errorReader)
                        (output, errors) <- case drained of
                            Just values -> pure values
                            Nothing ->
                                throwString
                                    "repository process descendants retained output pipes"
                        writeIORef completed True
                        pure (exitCode, output, errors)
  where
    start = mask \_ -> do
        resolvedExecutable <-
            resolveExecutableOutside workingDirectory executable
                >>= either (fail . Text.unpack) pure
        environment <-
            applyEnvironmentOverrides overrides
                <$> repositoryProcessEnvironment workingDirectory
        (maybeInput, maybeOutput, maybeError, process) <-
            createProcess
                (proc resolvedExecutable arguments)
                    { cwd = Just workingDirectory
                    , std_in = CreatePipe
                    , std_out = CreatePipe
                    , std_err = CreatePipe
                    , close_fds = True
                    , create_group = True
                    , env = Just environment
                    }
        let closePipes = do
                mapM_ closeQuietly maybeInput
                mapM_ closeQuietly maybeOutput
                mapM_ closeQuietly maybeError
            cleanupWithoutGroup = do
                closePipes
                _ <- tryAny (terminateProcess process)
                _ <- tryAny (waitForProcess process)
                pure ()
        case (maybeInput, maybeOutput, maybeError) of
            (Just inputHandle, Just outputHandle, Just errorHandle) -> do
                processGroup <- getPid process
                    `onException` cleanupWithoutGroup
                let cleanup = do
                        closePipes
                        signalCheckProcessGroup
                            sigKILL processGroup process
                        _ <- tryAny (waitForProcess process)
                        pure ()
                completed <- newIORef False
                    `onException` cleanup
                pure
                    ( inputHandle
                    , outputHandle
                    , errorHandle
                    , process
                    , processGroup
                    , completed
                    )
            _ -> do
                cleanupWithoutGroup
                fail "could not create process pipes"
    stop
        ( inputHandle
        , outputHandle
        , errorHandle
        , process
        , processGroup
        , completed
        ) = do
        closeQuietly inputHandle
        closeQuietly outputHandle
        closeQuietly errorHandle
        finished <- readIORef completed
        unless finished do
            signalCheckProcessGroup sigTERM processGroup process
            threadDelay 250_000
            -- The group can outlive its leader (for example, a Git hook that
            -- forks and exits), so always escalate the captured group.
            signalCheckProcessGroup sigKILL processGroup process
        _ <- tryAny (waitForProcess process)
        pure ()

withIsolatedRepositoryGit
    :: FilePath
    -> ( Maybe Text
        -> [(String, String)]
        -> IO (Either RepositoryError value)
       )
    -> IO (Either RepositoryError value)
withIsolatedRepositoryGit root action = do
    repositoryCommonDirectory root >>= \case
        Left err -> pure (Left err)
        Right commonDirectory -> do
            attempted <- trySynchronous $
                withPrivateTempDirectoryOutside
                    [root, commonDirectory]
                    "haskell-agent-review"
                    \gitDirectory ->
                        prepareIsolatedRepositoryGit root gitDirectory >>= \case
                            Left err -> pure (Left err)
                            Right (headOid, environment) ->
                                action headOid environment
            pure case attempted of
                Left exception ->
                    Left
                        (RepositoryCommandFailed
                            "prepare isolated repository"
                            (-1)
                            (Text.pack (show exception)))
                Right result -> result

prepareIsolatedRepositoryGit
    :: FilePath
    -> FilePath
    -> IO
        (Either
            RepositoryError
            (Maybe Text, [(String, String)]))
prepareIsolatedRepositoryGit root gitDirectory = do
    headResult <- repositoryHead root
    objectsResult <-
        runGit
            root
            [ "rev-parse"
            , "--path-format=absolute"
            , "--git-path"
            , "objects"
            ]
            BS.empty
    indexResult <-
        runGit
            root
            [ "rev-parse"
            , "--path-format=absolute"
            , "--git-path"
            , "index"
            ]
            BS.empty
    formatResult <-
        runGit root ["rev-parse", "--show-object-format"] BS.empty
    configResult <-
        runGit
            root
            [ "config"
            , "--local"
            , "--no-includes"
            , "--null"
            , "--list"
            ]
            BS.empty
    repositoryExcludeResult <-
        runGit
            root
            [ "rev-parse"
            , "--path-format=absolute"
            , "--git-path"
            , "info/exclude"
            ]
            BS.empty
    configuredExcludeResult <- configuredRepositoryExclude root
    case
        ( headResult
        , objectsResult
        , indexResult
        , formatResult
        , configResult
        , repositoryExcludeResult
        , configuredExcludeResult
        ) of
        ( Right headOid
            , Right objectsBytes
            , Right indexBytes
            , Right formatBytes
            , Right configBytes
            , Right repositoryExcludeBytes
            , Right configuredExcludeBytes
            ) -> do
                let objects = decodePath (stripLineEnding objectsBytes)
                    index = decodePath (stripLineEnding indexBytes)
                    objectFormat = decodeTrimmed formatBytes
                    repositoryExclude =
                        decodePath (stripLineEnding repositoryExcludeBytes)
                    configuredExcludeValue =
                        decodePath
                            (BS.takeWhile (/= 0) configuredExcludeBytes)
                    configuredExclude
                        | null configuredExcludeValue = ""
                        | isAbsolute configuredExcludeValue =
                            configuredExcludeValue
                        | otherwise = root </> configuredExcludeValue
                objectsExist <- doesDirectoryExist objects
                if not
                    ( isAbsolute objects
                        && isAbsolute index
                        && isAbsolute repositoryExclude
                        && '\NUL' `notElem` objects
                        && '\NUL' `notElem` index
                        && '\NUL' `notElem` repositoryExclude
                        && '\NUL' `notElem` configuredExclude
                        && objectsExist
                        && objectFormat `elem` ["sha1", "sha256"]
                    )
                    then pure (Left invalidStorage)
                    else
                        readRepositoryExcludeRules
                            repositoryExclude
                            configuredExclude >>= \case
                                Left err -> pure (Left err)
                                Right
                                    ( repositoryExcludeContents
                                        , configuredExcludeContents
                                        ) -> do
                                        installIsolatedRepositoryGit
                                            gitDirectory
                                            headOid
                                            objectFormat
                                            configBytes
                                            repositoryExcludeContents
                                            configuredExcludeContents
                                        pure
                                            (Right
                                                ( headOid
                                                , [ ("GIT_DIR", gitDirectory)
                                                  , ("GIT_WORK_TREE", root)
                                                  , ("GIT_INDEX_FILE", index)
                                                  , ("GIT_OBJECT_DIRECTORY", objects)
                                                  , ("GIT_CONFIG_GLOBAL", "/dev/null")
                                                  , ("GIT_CONFIG_SYSTEM", "/dev/null")
                                                  , ("GIT_CONFIG_NOSYSTEM", "1")
                                                  , ("GIT_CONFIG_COUNT", "1")
                                                  , ("GIT_CONFIG_KEY_0", "core.excludesFile")
                                                  , ( "GIT_CONFIG_VALUE_0"
                                                    , gitDirectory
                                                        </> "global-excludes"
                                                    )
                                                  , ("GIT_ATTR_NOSYSTEM", "1")
                                                  , ("GIT_OPTIONAL_LOCKS", "0")
                                                  ]
                                                ))
        (Left err, _, _, _, _, _, _) -> pure (Left err)
        (_, Left err, _, _, _, _, _) -> pure (Left err)
        (_, _, Left err, _, _, _, _) -> pure (Left err)
        (_, _, _, Left err, _, _, _) -> pure (Left err)
        (_, _, _, _, Left err, _, _) -> pure (Left err)
        (_, _, _, _, _, Left err, _) -> pure (Left err)
        (_, _, _, _, _, _, Left err) -> pure (Left err)
  where
    invalidStorage =
        RepositoryCommandFailed
            "prepare isolated repository"
            (-1)
            "repository storage paths are invalid"

configuredRepositoryExclude
    :: FilePath
    -> IO (Either RepositoryError BS.ByteString)
configuredRepositoryExclude root = do
    worktreeEnabled <-
        runGitOptionalConfig
            root
            [ "config"
            , "--local"
            , "--no-includes"
            , "--bool"
            , "--get"
            , "extensions.worktreeConfig"
            ]
    case worktreeEnabled of
        Left err -> pure (Left err)
        Right enabled -> do
            worktree <-
                if maybe False ((== "true") . decodeTrimmed) enabled
                    then
                        runGitOptionalConfig
                            root
                            (configPathArguments ["--worktree", "--no-includes"])
                    else pure (Right Nothing)
            local <-
                runGitOptionalConfig
                    root
                    (configPathArguments ["--local", "--no-includes"])
            selectHigherScope worktree local >>= \case
                Left err -> pure (Left err)
                Right (Just value) -> pure (Right value)
                Right Nothing ->
                    runGitOptionalConfig
                        root
                        (configPathArguments ["--global", "--includes"]) >>= \case
                            Left err -> pure (Left err)
                            Right (Just value) -> pure (Right value)
                            Right Nothing ->
                                runGitOptionalConfig
                                    root
                                    (configPathArguments
                                        ["--system", "--includes"]) >>= \case
                                            Left err -> pure (Left err)
                                            Right value ->
                                                pure (Right (fromMaybe "\NUL" value))
  where
    configPathArguments scope =
        ["config"]
            <> scope
            <> [ "--path"
               , "--null"
               , "--get"
               , "core.excludesFile"
               ]
    selectHigherScope (Left err) _ = pure (Left err)
    selectHigherScope _ (Left err) = pure (Left err)
    selectHigherScope (Right worktree) (Right local) =
        pure (Right (worktree <|> local))

runGitOptionalConfig
    :: FilePath
    -> [String]
    -> IO (Either RepositoryError (Maybe BS.ByteString))
runGitOptionalConfig root arguments = do
    result <-
        trySynchronous
            (timeout repositoryConfigTimeoutMicros
                (runProcessBytesWithEnvironment
                    []
                    root
                    "git"
                    (safeGitArguments <> arguments)
                    BS.empty))
    pure case result of
        Left exception ->
            Left
                (RepositoryCommandFailed
                    (renderCommand "git" arguments)
                    (-1)
                    (Text.pack (show exception)))
        Right Nothing ->
            Left
                (RepositoryCommandFailed
                    (renderCommand "git" arguments)
                    (-1)
                    "repository config resolution timed out")
        Right (Just (ExitSuccess, output, _)) -> Right (Just output)
        Right (Just (ExitFailure 1, _, _)) -> Right Nothing
        Right (Just (ExitFailure code, _, errors)) ->
            Left
                (RepositoryCommandFailed
                    (renderCommand "git" arguments)
                    code
                    (Text.strip
                        (TextEncoding.decodeUtf8With lenientDecode errors)))

installIsolatedRepositoryGit
    :: FilePath
    -> Maybe Text
    -> Text
    -> BS.ByteString
    -> BS.ByteString
    -> BS.ByteString
    -> IO ()
installIsolatedRepositoryGit
    gitDirectory
    headOid
    objectFormat
    configBytes
    repositoryExcludeContents
    configuredExcludeContents = do
    createDirectory (gitDirectory </> "refs")
    createDirectory (gitDirectory </> "refs" </> "heads")
    createDirectory (gitDirectory </> "info")
    BS.writeFile
        (gitDirectory </> "HEAD")
        (case headOid of
            Nothing -> "ref: refs/heads/isolated\n"
            Just oid -> TextEncoding.encodeUtf8 oid <> "\n")
    BS.writeFile
        (gitDirectory </> "info" </> "exclude")
        repositoryExcludeContents
    BS.writeFile
        (gitDirectory </> "global-excludes")
        configuredExcludeContents
    let formatVersion =
            if objectFormat == "sha256" then "1" else "0"
        extension =
            if objectFormat == "sha256"
                then
                    [ "[extensions]"
                    , "\tobjectformat = sha256"
                    ]
                else []
        config =
            unlines
                ( [ "[core]"
                  , "\trepositoryformatversion = " <> formatVersion
                  , "\tbare = false"
                  , "\thooksPath = /dev/null"
                  , "\tfsmonitor = false"
                  , "\tattributesFile = /dev/null"
                  ]
                    <> safeCoreConfigLines configBytes
                    <> extension
                )
    BS8.writeFile (gitDirectory </> "config") (BS8.pack config)
    mapM_
        (`setFileMode` 0o600)
        [ gitDirectory </> "HEAD"
        , gitDirectory </> "config"
        , gitDirectory </> "info" </> "exclude"
        , gitDirectory </> "global-excludes"
        ]

readRepositoryExcludeRules
    :: FilePath
    -> FilePath
    -> IO
        (Either
            RepositoryError
            (BS.ByteString, BS.ByteString))
readRepositoryExcludeRules repositoryExclude configuredExclude =
    trySynchronous
        (do
            repositoryRules <- readOptionalRegularFile repositoryExclude
            configuredRules <- readOptionalRegularFile configuredExclude
            when
                ( BS.length repositoryRules
                    + BS.length configuredRules
                    > maxRepositoryExcludeBytes
                )
                (throwString "repository exclude rules exceed the size limit")
            pure (repositoryRules, configuredRules)) >>= \case
                Left exception ->
                    pure
                        (Left
                            (RepositoryCommandFailed
                                "read repository exclude rules"
                                (-1)
                                (Text.pack (show exception))))
                Right rules -> pure (Right rules)

readOptionalRegularFile :: FilePath -> IO BS.ByteString
readOptionalRegularFile "" = pure BS.empty
readOptionalRegularFile requested = do
    tryIO
        (canonicalizePath requested) >>= \case
            Left exception
                | isDoesNotExistError exception -> pure BS.empty
                | otherwise -> throwIO exception
            Right path ->
                tryIO
                    (openFd
                        path
                        ReadOnly
                        defaultFileFlags
                            { nofollow = True
                            , cloexec = True
                            , nonBlock = True
                            }) >>= \case
                                Left exception
                                    | isDoesNotExistError exception ->
                                        pure BS.empty
                                    | otherwise -> throwIO exception
                                Right descriptor ->
                                    mask \restore -> do
                                        status <- getFdStatus descriptor
                                            `onException` closeFd descriptor
                                        unless (isRegularFile status) do
                                            closeFd descriptor
                                            throwString
                                                "repository exclude rules are not a regular file"
                                        handle <- fdToHandle descriptor
                                            `onException` closeFd descriptor
                                        restore
                                            (do
                                                bytes <-
                                                    BS.hGet
                                                        handle
                                                        ( maxRepositoryExcludeBytes
                                                            + 1
                                                        )
                                                when
                                                    (BS.length bytes
                                                        > maxRepositoryExcludeBytes)
                                                    (throwString
                                                        "repository exclude rules exceed the size limit")
                                                pure bytes)
                                            `finally` hClose handle

safeCoreConfigLines :: BS.ByteString -> [String]
safeCoreConfigLines bytes =
    [ "\t" <> field <> " = " <> value
    | field <-
        [ "filemode"
        , "ignorecase"
        , "symlinks"
        , "precomposeunicode"
        ]
    , Just value <- [booleanValue ("core." <> field)]
    ]
  where
    entries =
        [ (key, BS.drop 1 separatorAndValue)
        | entry <- BS.split 0 bytes
        , not (BS.null entry)
        , let (key, separatorAndValue) = BS8.break (== '\n') entry
        , not (BS.null separatorAndValue)
        ]
    booleanValue key =
        case
            [ map toLower (BS8.unpack value)
            | (entryKey, value) <- reverse entries
            , BS8.unpack entryKey == key
            ] of
                value : _
                    | value `elem` ["true", "yes", "on", "1"] ->
                        Just "true"
                    | value `elem` ["false", "no", "off", "0"] ->
                        Just "false"
                _ -> Nothing

withPrivateTempDirectoryOutside
    :: [FilePath]
    -> String
    -> (FilePath -> IO value)
    -> IO value
withPrivateTempDirectoryOutside boundaries template action = do
    configuredBase <- getTemporaryDirectory
    bracket
        (acquire (nub [configuredBase, "/tmp", "/var/tmp"]))
        removePathForcibly
        action
  where
    acquire [] =
        throwString
            "no private temporary directory is available outside repository storage"
    acquire (candidate : remaining) =
        trySynchronous (outsideAllBoundaries candidate >>= \case
            Nothing ->
                throwString
                    "temporary directory is inside repository storage"
            Just safeBase -> do
                directory <-
                    mkdtemp (safeBase </> template <> ".XXXXXX")
                (do
                    setFileMode directory 0o700
                    outsideAllBoundaries directory >>= \case
                        Nothing ->
                            throwString
                                "temporary directory resolved inside repository storage"
                        Just checkedDirectory -> pure checkedDirectory)
                    `onException` removePathForcibly directory) >>= \case
                    Left _ -> acquire remaining
                    Right directory -> pure directory

    outsideAllBoundaries candidate =
        foldM
            (\checked boundary ->
                case checked of
                    Nothing -> pure Nothing
                    Just path -> canonicalPathOutside boundary path)
            (Just candidate)
            boundaries

applyEnvironmentOverrides
    :: [(String, String)]
    -> [(String, String)]
    -> [(String, String)]
applyEnvironmentOverrides overrides inherited =
    overrides
        <> filter
            (\(name, _) -> name `notElem` map fst overrides)
            inherited

stripLineEnding :: BS.ByteString -> BS.ByteString
stripLineEnding = BS8.dropWhileEnd (`elem` ['\r', '\n'])

safeGitArguments :: [String]
safeGitArguments =
    [ "--no-replace-objects"
    , "-c"
    , "core.fsmonitor=false"
    , "-c"
    , "credential.helper="
    , "-c"
    , "core.sshCommand=false"
    , "-c"
    , "protocol.ext.allow=never"
    , "-c"
    , "commit.gpgSign=false"
    , "-c"
    , "tag.gpgSign=false"
    ]

repositoryProcessEnvironment :: FilePath -> IO [(String, String)]
repositoryProcessEnvironment root = do
    inherited <- getEnvironment
    let sanitized =
            filter
                (\(name, _) ->
                    name `notElem` blocked
                        && not ("GIT_CONFIG_KEY_" `Text.isPrefixOf` Text.pack name)
                        && not ("GIT_CONFIG_VALUE_" `Text.isPrefixOf` Text.pack name))
                inherited
    safePath <- case lookup "PATH" sanitized of
        Nothing -> pure Nothing
        Just value -> sanitizeSearchPathOutside root value
    let withPath =
            case safePath of
                Nothing -> filter ((/= "PATH") . fst) sanitized
                Just value ->
                    ("PATH", value) : filter ((/= "PATH") . fst) sanitized
    pure
        ( [ ("GIT_TERMINAL_PROMPT", "0")
          , ("GCM_INTERACTIVE", "never")
          , ("GIT_PAGER", "cat")
          , ("PAGER", "cat")
          , ("SSH_ASKPASS_REQUIRE", "never")
          ]
            <> withPath
        )
  where
    blocked =
        [ "GIT_TERMINAL_PROMPT"
        , "GCM_INTERACTIVE"
        , "GIT_PAGER"
        , "PAGER"
        , "SSH_ASKPASS_REQUIRE"
        , "GIT_DIR"
        , "GIT_WORK_TREE"
        , "GIT_INDEX_FILE"
        , "GIT_OBJECT_DIRECTORY"
        , "GIT_ALTERNATE_OBJECT_DIRECTORIES"
        , "GIT_COMMON_DIR"
        , "GIT_CONFIG_COUNT"
        , "GIT_CONFIG_PARAMETERS"
        , "GIT_CONFIG"
        , "GIT_CONFIG_GLOBAL"
        , "GIT_CONFIG_SYSTEM"
        , "GIT_CONFIG_NOSYSTEM"
        , "GIT_ATTR_NOSYSTEM"
        , "GIT_SSH"
        , "GIT_SSH_COMMAND"
        , "GIT_ASKPASS"
        , "GIT_PROXY_COMMAND"
        , "GIT_EXEC_PATH"
        , "GIT_EXTERNAL_DIFF"
        ]

hGetBounded :: Int -> Handle -> IO BS.ByteString
hGetBounded limit handle = go 0 False []
  where
    go total exceeded chunks = do
        chunk <- BS.hGetSome handle (64 * 1024)
        if BS.null chunk
            then
                if exceeded
                    then throwString "repository process output limit exceeded"
                    else pure (BS.concat (reverse chunks))
            else do
                let next = total + BS.length chunk
                if exceeded || next > limit
                    then go total True chunks
                    else go next False (chunk : chunks)

closeQuietly :: Handle -> IO ()
closeQuietly handle = do
    _ <- tryAny (hClose handle)
    pure ()

decodeTrimmed :: BS.ByteString -> Text
decodeTrimmed =
    Text.strip . TextEncoding.decodeUtf8With lenientDecode

decodePath :: BS.ByteString -> FilePath
decodePath = Text.unpack . TextEncoding.decodeUtf8With lenientDecode

renderCommand :: FilePath -> [String] -> Text
renderCommand executable arguments =
    Text.unwords (Text.pack executable : map (Text.pack . show) arguments)

repositoryErrorText :: RepositoryError -> Text
repositoryErrorText = \case
    NotARepository message -> message
    StaleRepositorySnapshot expected actual ->
        "repository changed (expected "
            <> expected
            <> ", actual "
            <> actual
            <> ")"
    InvalidRepositoryRequest message -> message
    RepositoryCommandFailed command code message ->
        command
            <> " exited "
            <> Text.pack (show code)
            <> if Text.null message then "" else ": " <> message

voidResult :: Either error value -> Either error ()
voidResult = fmap (const ())

maxRepositoryPatchBytes :: Int
maxRepositoryPatchBytes = 64 * 1024 * 1024

maxRepositoryDiffHunks :: Int
maxRepositoryDiffHunks = 100_000

maxRepositorySelectedHunks :: Int
maxRepositorySelectedHunks = 4096

maxRepositoryCommitCharacters :: Int
maxRepositoryCommitCharacters = 8 * 1024 * 1024

maxRepositoryCheckArguments :: Int
maxRepositoryCheckArguments = 4096

maxRepositoryArgumentCharacters :: Int
maxRepositoryArgumentCharacters = 1024 * 1024

maxRepositoryCheckTotalCharacters :: Int
maxRepositoryCheckTotalCharacters = 8 * 1024 * 1024

maxRepositoryProcessOutputBytes :: Int
maxRepositoryProcessOutputBytes = 64 * 1024 * 1024

maxRepositoryProcessErrorBytes :: Int
maxRepositoryProcessErrorBytes = 8 * 1024 * 1024

maxRepositoryExcludeBytes :: Int
maxRepositoryExcludeBytes = 16 * 1024 * 1024

maxRepositoryUntrackedFileBytes :: Int
maxRepositoryUntrackedFileBytes = 64 * 1024 * 1024

repositoryDiscoveryTimeoutMicros :: Int
repositoryDiscoveryTimeoutMicros = 1_000_000

maxRepositoryGitCommandMicros :: Int
maxRepositoryGitCommandMicros = 60_000_000

repositoryConfigTimeoutMicros :: Int
repositoryConfigTimeoutMicros = 2_000_000

maxRepositoryDiffCommandMicros :: Int
maxRepositoryDiffCommandMicros = 15_000_000

processPipeTeardownMicros :: Int
processPipeTeardownMicros = 1_000_000

trySynchronous :: IO value -> IO (Either SomeException value)
trySynchronous action =
    tryAny action >>= \case
        Left exception
            | isAsyncException exception -> throwIO exception
            | otherwise -> pure (Left exception)
        Right value -> pure (Right value)
