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
import Control.Concurrent.Async
    ( Async
    , asyncWithUnmask
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
    ( bracket
    , finally
    , onException
    , tryAny
    )
import Control.Monad (foldM, unless)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Char (isDigit)
import Data.List (nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
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
import System.IO (Handle, hClose)
import System.IO.Unsafe (unsafePerformIO)
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
import System.Posix.Types (ProcessID)
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
    | StagePatch !FilePath !BS.ByteString
    | UnstagePatch !FilePath !BS.ByteString
    | RestorePatch !FilePath !BS.ByteString
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
    runRepositoryRead requested \root -> do
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
                                    , "--no-color"
                                    ]
                                        <> case kind of
                                            RepositoryWorktreeDiff -> []
                                            RepositoryStagedDiff -> ["--cached"]
                                        <> ["--", path]
                        result <- runGitDiff root arguments
                        after <- snapshotAtRoot root
                        pure do
                            patch <- result
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
                                            parseDiffHunks patch
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
                | Text.any (== '\NUL') rawMessage ->
                    pure
                        (Left
                            (InvalidRepositoryRequest
                                "commit message contains a NUL byte"))
                | otherwise -> do
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
                | any (elem '\NUL') arguments ->
                    pure
                        (Left
                            (InvalidRepositoryRequest
                                "check argument contains a NUL byte"))
                | otherwise -> do
                    created <- tryAny do
                        (maybeOutput, maybeError, process) <-
                            createCheckProcess root executable arguments
                        processGroup <- getPid process
                        cancelled <- newIORef False
                        worker <-
                            asyncWithUnmask
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
                                                        _ <- wait outputReader
                                                        _ <- wait errorReader
                                                        wasCancelled <-
                                                            readIORef cancelled
                                                        onExit
                                                            wasCancelled
                                                            exitCode))
                                `onException` do
                                    closeQuietly maybeOutput
                                    closeQuietly maybeError
                                    signalCheckProcessGroup
                                        sigKILL processGroup process
                                    _ <- tryAny (waitForProcess process)
                                    pure ()
                        pure RepositoryCheck
                            { repositoryCheckProcess = process
                            , repositoryCheckProcessGroup = processGroup
                            , repositoryCheckWorker = worker
                            , repositoryCheckCancelled = cancelled
                            }
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
        let drain = do
                bytes <- BS.hGetSome handle (64 * 1024)
                if BS.null bytes
                    then pure ()
                    else onOutput stream bytes >> drain
        in drain `finally` closeQuietly handle

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
    -> IO (Handle, Handle, ProcessHandle)
createCheckProcess root executable arguments = do
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
            -- Checks have no stdin API. Closing immediately prevents an
            -- inherited terminal or pipe from leaving a check blocked.
            hClose input
            pure (output, errors, process)
        _ -> do
            _ <- tryAny (terminateProcess process)
            _ <- tryAny (waitForProcess process)
            fail "could not create check output pipes"

snapshotAtRoot :: FilePath -> IO (Either RepositoryError RepositorySnapshot)
snapshotAtRoot root = do
    headResult <- runGit root ["rev-parse", "--verify", "HEAD"] BS.empty
    let headOid = either (const Nothing) (Just . decodeTrimmed) headResult
    indexResult <- runGit root ["ls-files", "--stage", "-z"] BS.empty
    statusResult <- runGit root
        ["status", "--porcelain=v1", "-z", "--untracked-files=all"]
        BS.empty
    diffResult <- runGit root
        ["diff", "--binary", "--no-ext-diff", "--no-color"]
        BS.empty
    case (indexResult, statusResult, diffResult) of
        (Right indexBytes, Right statusBytes, Right diffBytes) -> do
            worktreeMaterial <- appendUntrackedHashes root statusBytes diffBytes
            indexHash <- hashMaterial root indexBytes
            worktreeHash <- case worktreeMaterial of
                Left err -> pure (Left err)
                Right material -> hashMaterial root material
            pure do
                indexFingerprint <- indexHash
                worktreeFingerprint <- worktreeHash
                let headFingerprint = fromMaybe "unborn" headOid
                    identity = Text.intercalate ":"
                        [headFingerprint, indexFingerprint, worktreeFingerprint]
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
    StagePatch path patch ->
        checkedPath path $
            applyReviewedPatch RepositoryWorktreeDiff ["--cached"] path patch
    UnstagePatch path patch ->
        checkedPath path $
            applyReviewedPatch
                RepositoryStagedDiff ["--cached", "--reverse"] path patch
    RestorePatch path patch ->
        checkedPath path $
            if isUntracked path
                then pure
                    (Left
                        (InvalidRepositoryRequest
                            "restore never deletes an untracked file"))
                else applyReviewedPatch
                    RepositoryWorktreeDiff ["--reverse"] path patch
  where
    checkedPath path action
        = case validateRepositoryPath root path of
            Left err -> pure (Left err)
            Right () -> action
    isUntracked path = any
        (\file ->
            file.repositoryFilePath == path
                && file.repositoryFileIndexStatus == '?')
        snapshot.snapshotFiles
    applyPathPatch kind flags path =
        repositoryDiff root snapshot.snapshotId kind path >>= \case
            Left err -> pure (Left err)
            Right diff
                | BS.null diff.repositoryDiffPatch -> pure (Right ())
                | otherwise ->
                    applyPatch
                        root snapshot.snapshotId flags diff.repositoryDiffPatch
    applyReviewedPatch kind flags path patch =
        repositoryDiff root snapshot.snapshotId kind path >>= \case
            Left err -> pure (Left err)
            Right reviewed ->
                case validateReviewedPatch
                    reviewed.repositoryDiffPatch patch of
                    Left err -> pure (Left err)
                    Right () ->
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
                voidResult
                    <$> runGit root
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

validateReviewedPatch
    :: BS.ByteString
    -> BS.ByteString
    -> Either RepositoryError ()
validateReviewedPatch reviewed supplied
    | BS.null supplied =
        Left (InvalidRepositoryRequest "patch is empty")
    | supplied == reviewed = Right ()
    | otherwise =
        case (textPatchSections reviewed, textPatchSections supplied) of
            (Just (reviewedHeader, reviewedHunks), Just (header, hunks))
                | header == reviewedHeader
                    && not (null hunks)
                    && length hunks == length (nub hunks)
                    && all (`elem` reviewedHunks) hunks -> Right ()
            _ ->
                Left
                    (InvalidRepositoryRequest
                        "patch is not an exact reviewed hunk set for the requested path")

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
                    , if null current then completed else current : completed
                    )
                | null current = ([], completed)
                | otherwise = (current <> [line], completed)
            (lastHunk, reversed) = foldl' collect ([], []) lines'
            result = reverse
                (if null lastHunk then reversed else lastHunk : reversed)
        in if null result then Nothing else Just result

appendUntrackedHashes
    :: FilePath
    -> BS.ByteString
    -> BS.ByteString
    -> IO (Either RepositoryError BS.ByteString)
appendUntrackedHashes root statusBytes diffBytes =
    foldM addHash (Right (statusBytes <> diffBytes)) untracked
  where
    untracked =
        [ file.repositoryFilePath
        | file <- either (const []) id (parsePorcelain statusBytes)
        , file.repositoryFileIndexStatus == '?'
        ]
    addHash (Left err) _ = pure (Left err)
    addHash (Right material) path = do
        result <- runGit root
            ["--literal-pathspecs", "hash-object", "--", path]
            BS.empty
        pure ((material <>) <$> result)

hashMaterial :: FilePath -> BS.ByteString -> IO (Either RepositoryError Text)
hashMaterial root bytes =
    fmap decodeTrimmed <$> runGit root ["hash-object", "--stdin"] bytes

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
    foldl' collect [] . Text.lines . TextEncoding.decodeUtf8With lenientDecode
  where
    collect acc line =
        maybe acc (\hunk -> acc <> [hunk]) (parseHunkHeader line)

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
    rootResult <- repositoryRoot requested
    case rootResult of
        Left err -> pure (Left err)
        Right root -> action root

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
            withMVar lock (const (action root))

repositoryRoot :: FilePath -> IO (Either RepositoryError FilePath)
repositoryRoot requested
    | null requested =
        pure (Left (NotARepository "repository path is empty"))
    | otherwise =
        runGit requested ["rev-parse", "--show-toplevel"] BS.empty >>= \case
            Left err -> pure (Left (NotARepository (repositoryErrorText err)))
            Right rootBytes -> pure (Right (Text.unpack (decodeTrimmed rootBytes)))

{-# NOINLINE repositoryLocks #-}
repositoryLocks :: MVar (Map FilePath (MVar ()))
repositoryLocks = unsafePerformIO (newMVar Map.empty)

runGit
    :: FilePath
    -> [String]
    -> BS.ByteString
    -> IO (Either RepositoryError BS.ByteString)
runGit root arguments input = do
    result <- tryAny (runProcessBytes root "git" arguments input)
    pure case result of
        Left exception ->
            Left
                (RepositoryCommandFailed
                    (renderCommand "git" arguments)
                    (-1)
                    (Text.pack (show exception)))
        Right (exitCode, output, errors) -> case exitCode of
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
    result <- tryAny (runProcessBytes root "git" arguments BS.empty)
    pure case result of
        Left exception ->
            Left
                (RepositoryCommandFailed
                    (renderCommand "git" arguments)
                    (-1)
                    (Text.pack (show exception)))
        Right (exitCode, output, errors) -> case exitCode of
            ExitSuccess -> Right output
            ExitFailure 1 -> Right output
            ExitFailure code ->
                Left
                    (RepositoryCommandFailed
                        (renderCommand "git" arguments)
                        code
                        (Text.strip
                            (TextEncoding.decodeUtf8With lenientDecode errors)))

runProcessBytes
    :: FilePath
    -> FilePath
    -> [String]
    -> BS.ByteString
    -> IO (ExitCode, BS.ByteString, BS.ByteString)
runProcessBytes workingDirectory executable arguments input =
    bracket start stop \(inputHandle, outputHandle, errorHandle, process) -> do
        withAsync
            (BS.hPut inputHandle input `finally` closeQuietly inputHandle)
            \inputWriter ->
                withAsync (BS.hGetContents outputHandle) \outputReader ->
                    withAsync (BS.hGetContents errorHandle) \errorReader -> do
                        exitCode <- waitForProcess process
                        _ <- wait inputWriter
                        output <- wait outputReader
                        errors <- wait errorReader
                        pure (exitCode, output, errors)
  where
    start = do
        (maybeInput, maybeOutput, maybeError, process) <-
            createProcess
                (proc executable arguments)
                    { cwd = Just workingDirectory
                    , std_in = CreatePipe
                    , std_out = CreatePipe
                    , std_err = CreatePipe
                    }
        case (maybeInput, maybeOutput, maybeError) of
            (Just inputHandle, Just outputHandle, Just errorHandle) ->
                pure (inputHandle, outputHandle, errorHandle, process)
            _ -> do
                terminateProcess process
                _ <- waitForProcess process
                fail "could not create process pipes"
    stop (inputHandle, outputHandle, errorHandle, process) = do
        closeQuietly inputHandle
        closeQuietly outputHandle
        closeQuietly errorHandle
        _ <- tryAny (terminateProcess process)
        _ <- tryAny (waitForProcess process)
        pure ()

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
