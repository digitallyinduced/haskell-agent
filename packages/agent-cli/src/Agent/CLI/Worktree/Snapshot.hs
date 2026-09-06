-- | Recovery objects for worktree collection. Callers own the repository lock
-- and exclusive checkout lease across capture, final verification and removal.
-- No command writes the live index or moves its branch. Working files are
-- stored as raw blobs (never through clean/smudge filters).
module Agent.CLI.Worktree.Snapshot
    ( WorktreeSnapshot(..)
    , createSnapshot
    , verifySnapshotUnchanged
    , restoreSnapshot
    , checkSnapshotSupported
    ) where

import Agent.OsPath (unsafeToFilePath)
import Control.Exception.Safe (bracket, displayException, onException, tryAny)
import Control.Monad (forM, forM_, unless, void, when)
import Data.Aeson (FromJSON, ToJSON)
import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.Char (toLower)
import Data.List (isPrefixOf, nub, sort)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import GHC.Generics (Generic)
import Numeric (showHex)
import qualified System.Directory as Dir
import System.Environment (getEnvironment)
import System.Entropy (getEntropy)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeDirectory, splitDirectories, isAbsolute)
import System.IO (IOMode(..), hClose, withBinaryFile)
import System.IO.Error (isDoesNotExistError, tryIOError)
import System.OsPath (OsPath)
import qualified System.Posix.Files as Posix
import qualified System.Posix.Files.ByteString as PosixBytes
import qualified System.Posix.IO as PosixIO
import System.Posix.Temp (mkdtemp)
import System.Posix.Types (FileMode)
import System.Posix.Unistd (fileSynchronise)
import System.Process
    ( CreateProcess(..), StdStream(..), proc, withCreateProcess, waitForProcess )

data WorktreeSnapshot = WorktreeSnapshot
    { snapshotHead :: !Text
    , snapshotIndexTree :: !Text
    , snapshotWorkTree :: !Text
    , snapshotRef :: !Text
    , snapshotBranch :: !Text
    } deriving (Eq, Show, Generic)

instance ToJSON WorktreeSnapshot
instance FromJSON WorktreeSnapshot

-- | Recovery refs never expire automatically. A failed capture may leave
-- harmless unreachable objects, but never authorizes deletion.
createSnapshot :: OsPath -> IO (Either Text WorktreeSnapshot)
createSnapshot path = result $ withScratch \tmp -> do
    let repo = unsafeToFilePath path
    (headOid, indexTree, workTree, branch) <- capture repo tmp
    nonce <- concatMap (\b -> let s = showHex b "" in replicate (2 - length s) '0' <> s)
        . BS.unpack <$> getEntropy 16
    let ref = "refs/haskell-agent/snapshots/" <> nonce
    indexCommit <- oid repo tmp [] ["commit-tree", indexTree, "-p", headOid]
        "haskell-agent snapshot index\n"
    commit <- oid repo tmp [] ["commit-tree", workTree, "-p", headOid, "-p", indexCommit]
        "haskell-agent snapshot working files (ignored untracked files excluded)\n"
    void $ git repo tmp [] ["update-ref", ref, commit, ""] ""
    let snapshot = WorktreeSnapshot (Text.pack headOid) (Text.pack indexTree)
            (Text.pack workTree) (Text.pack ref) (Text.pack branch)
    verifyObjects repo tmp snapshot
    checkUnchanged repo tmp snapshot
    pure snapshot

verifySnapshotUnchanged :: OsPath -> WorktreeSnapshot -> IO (Either Text ())
verifySnapshotUnchanged path snapshot = result $ withScratch \tmp -> do
    let repo = unsafeToFilePath path
    verifyObjects repo tmp snapshot
    checkUnchanged repo tmp snapshot

checkUnchanged :: FilePath -> FilePath -> WorktreeSnapshot -> IO ()
checkUnchanged repo tmp snapshot = do
    actual <- capture repo tmp
    unless (actual == (Text.unpack snapshot.snapshotHead,
        Text.unpack snapshot.snapshotIndexTree, Text.unpack snapshot.snapshotWorkTree,
        Text.unpack snapshot.snapshotBranch)) $
        fail "checkout changed during snapshot; retained"

-- | An existing destination, including an empty directory or dangling symlink,
-- is never reused. Failure leaves the partial checkout and recovery ref intact
-- for inspection; it never recursively deletes a potentially edited directory.
-- Always restores detached so an independently moved branch is untouched.
restoreSnapshot :: OsPath -> OsPath -> WorktreeSnapshot -> IO (Either Text ())
restoreSnapshot source destination snapshot = result $ withScratch \tmp -> do
    let repo = unsafeToFilePath source
        target = unsafeToFilePath destination
    verifyObjects repo tmp snapshot
    -- Atomic mkdir reserves the name against another restore.
    Dir.createDirectory target
    -- One --force repairs only this missing checkout's stale registration.
    -- Git still refuses a locked registration (which requires two --force).
    void $ git repo tmp [] ["worktree", "add", "--force", "--detach", "--no-checkout",
        "--", target, Text.unpack snapshot.snapshotHead] ""
    entries <- treeEntries repo tmp (Text.unpack snapshot.snapshotWorkTree)
    forM_ entries \(mode, object, name) -> do
        validatePath name
        let file = target </> name
        Dir.createDirectoryIfMissing True (takeDirectory file)
        checkParents target name
        exists <- pathExists file
        when exists $ fail "restore destination changed while restoring"
        bytes <- git repo tmp [] ["cat-file", "blob", object] ""
        case mode of
            "120000" -> PosixBytes.createSymbolicLink bytes (Text.encodeUtf8 (Text.pack file))
            "100644" -> writeExclusive file 0o644 bytes
            "100755" -> writeExclusive file 0o755 bytes
            _ -> fail "unsupported recovery file mode"
    void $ git target tmp [] ["read-tree", Text.unpack snapshot.snapshotIndexTree] ""
    (headOid, indexTree, workTree, _) <- capture target tmp
    unless ((headOid, indexTree, workTree) ==
        (Text.unpack snapshot.snapshotHead, Text.unpack snapshot.snapshotIndexTree,
         Text.unpack snapshot.snapshotWorkTree)) $
        fail "restored checkout verification failed; partial checkout retained"
    -- Registry may mark this checkout present only after its bytes and all
    -- newly created directory entries (including Git's private metadata) are
    -- durable. Symlinks are persisted through their containing directory.
    syncTree target
    syncPath (takeDirectory target)
    gitDir <- oid target tmp [] ["rev-parse", "--absolute-git-dir"] ""
    syncTree gitDir
    syncPath (takeDirectory gitDir)
    syncPath (takeDirectory (takeDirectory gitDir))

syncPath :: FilePath -> IO ()
syncPath path = bracket
    (PosixIO.openFd path PosixIO.ReadOnly PosixIO.defaultFileFlags
        { PosixIO.nofollow = True })
    PosixIO.closeFd fileSynchronise

syncTree :: FilePath -> IO ()
syncTree path = do
    status <- Posix.getSymbolicLinkStatus path
    if Posix.isDirectory status
        then do
            Dir.listDirectory path >>= mapM_ (syncTree . (path </>))
            syncPath path
        else unless (Posix.isSymbolicLink status) do
            unless (Posix.isRegularFile status) $
                fail "unsupported file appeared during restore durability check"
            syncPath path

-- O_EXCL also refuses a leaf created after the explicit existence check.
writeExclusive :: FilePath -> FileMode -> BS.ByteString -> IO ()
writeExclusive file mode bytes = bracket acquire hClose (`BS.hPut` bytes)
  where
    acquire = do
        fd <- PosixIO.openFd file PosixIO.WriteOnly PosixIO.defaultFileFlags
            { PosixIO.exclusive = True, PosixIO.nofollow = True, PosixIO.creat = Just mode }
        PosixIO.fdToHandle fd `onException` PosixIO.closeFd fd

-- | Read-only eligibility preflight. Only a private scratch copy of the index
-- is used; no Git objects, refs, live index or working files are written.
-- This is advisory: collection still recaptures and verifies under its lease.
checkSnapshotSupported :: OsPath -> IO (Either Text ())
checkSnapshotSupported path = result $ withScratch \tmp -> do
    let repo = unsafeToFilePath path
    (_, _, _, _, names) <- inspectCheckout repo tmp
    mapM_ (void . readWorkingFile repo) names

inspectCheckout :: FilePath -> FilePath -> IO (FilePath, String, String, BS.ByteString, [FilePath])
inspectCheckout repo tmp = do
    gitDir <- oid repo tmp [] ["rev-parse", "--absolute-git-dir"] ""
    forM_ ["index.lock", "MERGE_HEAD", "CHERRY_PICK_HEAD", "REVERT_HEAD",
        "rebase-merge", "rebase-apply", "sequencer", "BISECT_LOG", "config.worktree"] \name ->
        pathExists (gitDir </> name) >>= \present ->
            when present $ fail ("unsupported Git state: " <> name)
    headOid <- oid repo tmp [] ["rev-parse", "--verify", "HEAD^{commit}"] ""
    branch <- oid repo tmp [] ["rev-parse", "--symbolic-full-name", "HEAD"] ""
    -- Copy first: write-tree may refresh extensions, but only in this copy.
    let index = tmp </> "index"
        indexEnv = [("GIT_INDEX_FILE", index)]
    original <- BS.readFile (gitDir </> "index")
    BS.writeFile index original
    stages <- git repo tmp indexEnv ["ls-files", "--stage", "-z"] ""
    forM_ (nul stages) \entry -> do
        let header = B8.words (B8.takeWhile (/= '\t') entry)
        case header of
            [mode, _, "0"] | mode `elem` ["100644", "100755", "120000"] -> pure ()
            _ -> fail "unsupported Git index: conflict or submodule"
    flags <- git repo tmp indexEnv ["ls-files", "-v", "-z"] ""
    unless (all (BS.isPrefixOf "H ") (nul flags)) $
        fail "unsupported Git index: assume-unchanged or sparse checkout"
    visible <- git repo tmp indexEnv
        ["diff", "--cached", "--raw", "--no-ext-diff", "--ita-visible-in-index", "-z"] ""
    invisible <- git repo tmp indexEnv
        ["diff", "--cached", "--raw", "--no-ext-diff", "--ita-invisible-in-index", "-z"] ""
    unless (visible == invisible) $ fail "unsupported Git index: intent-to-add"
    names <- git repo tmp indexEnv ["ls-files", "--cached", "--others",
        "--exclude-standard", "-z"] "" >>= mapM utf8 . nul
    pure (gitDir, headOid, branch, original, sort (nub names))

capture :: FilePath -> FilePath -> IO (String, String, String, String)
capture repo tmp = do
    (gitDir, headOid, branch, original, names) <- inspectCheckout repo tmp
    indexTree <- oid repo tmp [("GIT_INDEX_FILE", tmp </> "index")] ["write-tree"] ""
    -- Build a completely fresh index from raw bytes; git add would apply
    -- filters and lose CRLF, working-tree-encoding and other exact contents.
    let workIndexEnv = [("GIT_INDEX_FILE", tmp </> "working-index")]
    void $ git repo tmp workIndexEnv ["read-tree", "--empty"] ""
    entries <- forM names \name ->
        readWorkingFile repo name >>= \case
            Nothing -> pure BS.empty
            Just (mode, bytes) -> do
                object <- oid repo tmp [] ["hash-object", "-w", "--no-filters", "--stdin"] bytes
                pure $ mode <> " " <> B8.pack object <> "\t"
                    <> Text.encodeUtf8 (Text.pack name) <> "\0"
    void $ git repo tmp workIndexEnv ["update-index", "-z", "--index-info"] (BS.concat entries)
    workTree <- oid repo tmp workIndexEnv ["write-tree"] ""
    after <- BS.readFile (gitDir </> "index")
    unless (original == after) $ fail "index changed during snapshot"
    headAfter <- oid repo tmp [] ["rev-parse", "--verify", "HEAD^{commit}"] ""
    unless (headOid == headAfter) $ fail "HEAD changed during snapshot"
    pure (headOid, indexTree, workTree, branch)

readWorkingFile :: FilePath -> FilePath -> IO (Maybe (BS.ByteString, BS.ByteString))
readWorkingFile repo name = do
    validatePath name
    checkParents repo name
    let path = repo </> name
    status <- tryIOError (Posix.getSymbolicLinkStatus path)
    case status of
        Left err | isDoesNotExistError err -> pure Nothing
        Left err -> ioError err
        Right st
            | Posix.isSymbolicLink st -> Just . ("120000",) <$>
                PosixBytes.readSymbolicLink (Text.encodeUtf8 (Text.pack path))
            | Posix.isRegularFile st -> Just .
                (if Posix.fileMode st .&. 0o111 /= 0 then "100755" else "100644",)
                <$> BS.readFile path
            | otherwise -> fail "unsupported working file: directory, nested repository or special file"

verifyObjects :: FilePath -> FilePath -> WorktreeSnapshot -> IO ()
verifyObjects repo tmp snapshot = do
    let ref = Text.unpack snapshot.snapshotRef
    unless ("refs/haskell-agent/snapshots/" `isPrefixOf` ref) $
        fail "invalid recovery ref"
    void $ git repo tmp [] ["check-ref-format", ref] ""
    forM_ [snapshot.snapshotHead, snapshot.snapshotIndexTree, snapshot.snapshotWorkTree] \value ->
        unless (Text.length value `elem` [40, 64] &&
            Text.all (`elem` ("0123456789abcdef" :: String)) value) $
            fail "invalid snapshot object id"
    commitTree <- oid repo tmp [] ["rev-parse", "--verify", ref <> "^{tree}"] ""
    parent <- oid repo tmp [] ["rev-parse", "--verify", ref <> "^1"] ""
    indexTree <- oid repo tmp [] ["rev-parse", "--verify", ref <> "^2^{tree}"] ""
    unless ((parent, indexTree, commitTree) ==
        (Text.unpack snapshot.snapshotHead, Text.unpack snapshot.snapshotIndexTree,
         Text.unpack snapshot.snapshotWorkTree)) $ fail "recovery ref does not match registry"
    void $ git repo tmp [] ["fsck", "--connectivity-only", "--no-reflogs", "--no-dangling", ref] ""
    -- Walk and read all recovery blobs, not just tree roots. Missing/corrupt
    -- objects must prevent collection before the original is removed.
    forM_ [snapshot.snapshotIndexTree, snapshot.snapshotWorkTree] \tree -> do
        entries <- treeEntries repo tmp (Text.unpack tree)
        forM_ entries \(_, object, name) -> do
            validatePath name
            bytes <- git repo tmp [] ["cat-file", "blob", object] ""
            hashed <- oid repo tmp [] ["hash-object", "--no-filters", "--stdin"] bytes
            unless (hashed == object) $ fail "corrupt snapshot blob"

treeEntries :: FilePath -> FilePath -> String -> IO [(String, String, FilePath)]
treeEntries repo tmp tree = do
    entries <- git repo tmp [] ["ls-tree", "-r", "-z", tree] ""
    forM (nul entries) \entry -> do
        let (header, name) = B8.break (== '\t') entry
        case B8.words header of
            [mode, "blob", object] | mode `elem` ["100644", "100755", "120000"] -> do
                path <- utf8 (BS.drop 1 name)
                pure (B8.unpack mode, B8.unpack object, path)
            _ -> fail "unsupported recovery tree entry"

validatePath :: FilePath -> IO ()
validatePath name =
    when (null name || isAbsolute name || any ((`elem` ["..", ".", ".git"]) . map toLower)
        (splitDirectories name)) $ fail "unsafe snapshot path"

checkParents :: FilePath -> FilePath -> IO ()
checkParents root name =
    forM_ (scanl (</>) root (dropLast (splitDirectories name))) \parent -> do
        status <- tryIOError (Posix.getSymbolicLinkStatus parent)
        case status of
            Left err | isDoesNotExistError err -> pure ()
            Left err -> ioError err
            Right st -> unless (Posix.isDirectory st && not (Posix.isSymbolicLink st)) $
                fail "unsupported working path: non-directory ancestor"
  where
    dropLast [] = []
    dropLast [_] = []
    dropLast (x : xs) = x : dropLast xs

nul :: BS.ByteString -> [BS.ByteString]
nul = filter (not . BS.null) . BS.split 0

utf8 :: BS.ByteString -> IO String
utf8 bytes = either (const (fail "unsupported non-UTF8 path")) (pure . Text.unpack)
    (Text.decodeUtf8' bytes)

pathExists :: FilePath -> IO Bool
pathExists path = tryIOError (Posix.getSymbolicLinkStatus path) >>= \case
    Right _ -> pure True
    Left err | isDoesNotExistError err -> pure False
    Left err -> ioError err

result :: IO a -> IO (Either Text a)
result action = either (Left . Text.pack . displayException) Right <$> tryAny action

withScratch :: (FilePath -> IO a) -> IO a
withScratch action = do
    root <- Dir.getTemporaryDirectory
    bracket (mkdtemp (root </> "agent-worktree-snapshot-")) Dir.removePathForcibly action

oid :: FilePath -> FilePath -> [(String, String)] -> [String] -> BS.ByteString -> IO String
oid repo tmp env args input = B8.unpack . B8.takeWhile (/= '\n') <$> git repo tmp env args input

-- Files instead of pipes keep binary I/O exact and avoid pipe deadlocks.
-- withCreateProcess scopes the child even when the caller's bounded pass is
-- cancelled. Scratch directories are private (mkdtemp mode 0700).
git :: FilePath -> FilePath -> [(String, String)] -> [String] -> BS.ByteString -> IO BS.ByteString
git repo tmp overrides args input = do
    inherited <- getEnvironment
    let environment = overrides <>
            [("GIT_OPTIONAL_LOCKS", "0"), ("GIT_NO_REPLACE_OBJECTS", "1"),
             ("GIT_NO_LAZY_FETCH", "1"), ("GIT_TERMINAL_PROMPT", "0"),
             ("GIT_AUTHOR_NAME", "haskell-agent"), ("GIT_AUTHOR_EMAIL", "snapshot@localhost"),
             ("GIT_COMMITTER_NAME", "haskell-agent"), ("GIT_COMMITTER_EMAIL", "snapshot@localhost")]
            <> filter (not . isPrefixOf "GIT_" . fst) inherited
        inputPath = tmp </> "stdin"
        outputPath = tmp </> "stdout"
        errorPath = tmp </> "stderr"
    BS.writeFile inputPath input
    code <- withBinaryFile inputPath ReadMode \stdin ->
        withBinaryFile outputPath WriteMode \stdout ->
        withBinaryFile errorPath WriteMode \stderr ->
        withCreateProcess (proc "git"
            (["-c", "core.hooksPath=/dev/null", "-c", "core.fsmonitor=false",
              "-c", "core.fsync=all", "-c", "core.fsyncMethod=fsync",
              "-C", repo] <> args))
            { env = Just environment, std_in = UseHandle stdin,
              std_out = UseHandle stdout, std_err = UseHandle stderr } \_ _ _ child ->
                waitForProcess child
    case code of
        ExitSuccess -> BS.readFile outputPath
        ExitFailure _ -> do
            err <- BS.readFile errorPath
            fail ("git " <> unwords (take 2 args) <> ": " <> B8.unpack err)
