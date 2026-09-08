-- | A conservative, read-only clean-checkout proof and recovery using existing
-- Git objects. The caller holds the checkout lease and repository lock through
-- inspection, preservation, final verification, and removal.
module Agent.CLI.Worktree.Clean
    ( CleanCheckout
    , inspectCleanCheckout
    , preserveCleanCheckout
    , verifyCleanCheckout
    ) where

import Agent.CLI.Worktree.Snapshot (WorktreeSnapshot(..))
import Agent.CLI.Worktree.Ignored (checkIgnoredPath)
import Agent.OsPath (unsafeToFilePath)
import Control.Exception.Safe (bracket, displayException, tryAny)
import Control.Monad (forM_, unless, void, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.List (isPrefixOf, nub)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Numeric (showHex)
import qualified System.Directory as Directory
import System.Environment (getEnvironment)
import System.Entropy (getEntropy)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeFileName)
import System.OsPath (OsPath)
import System.IO.Error (isDoesNotExistError, tryIOError)
import qualified System.Posix.Files as Posix
import System.Posix.Temp (mkdtemp)
import System.Process (CreateProcess(..), proc, readCreateProcessWithExitCode)

data CleanCheckout = CleanCheckout
    { cleanHead :: !String
    , cleanTree :: !String
    , cleanBranch :: !String
    , cleanIndex :: !BS.ByteString
    , cleanReflog :: !BS.ByteString
    , cleanOriginalHead :: !BS.ByteString
    , cleanCommitMessage :: !BS.ByteString
    , cleanFetchHead :: !BS.ByteString
    , cleanAutoMerge :: !BS.ByteString
    , cleanRebaseHead :: !BS.ByteString
    } deriving (Eq, Show)

inspectCleanCheckout :: OsPath -> IO (Either Text CleanCheckout)
inspectCleanCheckout = result . inspect . unsafeToFilePath

verifyCleanCheckout :: OsPath -> CleanCheckout -> IO (Either Text ())
verifyCleanCheckout path expected = result $ do
    actual <- inspect (unsafeToFilePath path)
    unless (actual == expected) $
        fail "checkout or private Git history changed during collection"

-- | Retain the legacy snapshot shape, but reuse the existing HEAD tree.
-- Reflog-only commits get permanent refs before checkout removal.
preserveCleanCheckout :: OsPath -> CleanCheckout -> IO (Either Text WorktreeSnapshot)
preserveCleanCheckout path expected = result $ do
    let repository = unsafeToFilePath path
    actual <- inspect repository
    unless (actual == expected) $ fail "checkout changed before recovery preservation"
    identifier <- concatMap (\byte -> let digits = showHex byte "" in
        replicate (2 - length digits) '0' <> digits) . BS.unpack <$> getEntropy 16
    fetchedObjects <- parseFetchedObjects expected.cleanFetchHead
    autoMerge <- validatePseudoReference repository "AUTO_MERGE" "tree" expected.cleanAutoMerge
    rebaseHead <- validatePseudoReference repository "REBASE_HEAD" "commit" expected.cleanRebaseHead
    let reference = "refs/haskell-agent/snapshots/" <> identifier
        history = nub $ filter (not . all (== '0')) $
            concatMap (take 2 . words . B8.unpack) (B8.lines expected.cleanReflog)
            <> words (B8.unpack expected.cleanOriginalHead)
            <> fetchedObjects
            <> rebaseHead
    forM_ history $ \object -> do
        unless (validObject object) $ fail "invalid private Git history object"
        void $ git repository ["cat-file", "-e", object <> "^{commit}"] ""
        void $ git repository ["update-ref",
            "refs/haskell-agent/reclaimed/" <> identifier <> "/" <> object, object, ""] ""
    forM_ autoMerge $ \object ->
        void $ git repository ["update-ref",
            "refs/haskell-agent/reclaimed/" <> identifier <> "/auto-merge", object, ""] ""
    forM_ [("commit-message", expected.cleanCommitMessage), ("fetched-heads", expected.cleanFetchHead)] $
        \(name, bytes) -> unless (BS.null bytes) $ do
            contents <- either (fail . show) (pure . Text.unpack) $ Text.decodeUtf8' bytes
            object <- stripped repository ["hash-object", "-w", "--no-filters", "--stdin"] contents
            void $ git repository ["update-ref",
                "refs/haskell-agent/reclaimed/" <> identifier <> "/" <> name, object, ""] ""
    indexCommit <- stripped repository ["commit-tree", expected.cleanTree,
        "-p", expected.cleanHead] "haskell-agent clean recovery index\n"
    commit <- stripped repository ["commit-tree", expected.cleanTree,
        "-p", expected.cleanHead, "-p", indexCommit] "haskell-agent clean recovery working files\n"
    void $ git repository ["update-ref", reference, commit, ""] ""
    void $ git repository ["cat-file", "-e", reference <> "^{commit}"] ""
    -- Unlike a full capture, no working-file blobs are written here. Prove that
    -- recovery's existing object closure is present before authorizing removal.
    void $ git repository (["rev-list", "--objects", "--missing=error", reference] <> history <> autoMerge) ""
    pure WorktreeSnapshot
        { snapshotHead = Text.pack expected.cleanHead
        , snapshotIndexTree = Text.pack expected.cleanTree
        , snapshotWorkTree = Text.pack expected.cleanTree
        , snapshotRef = Text.pack reference
        , snapshotBranch = Text.pack expected.cleanBranch
        }

inspect :: FilePath -> IO CleanCheckout
inspect repository = do
    administration <- stripped repository ["rev-parse", "--absolute-git-dir"] ""
    entries <- Directory.listDirectory administration
    let unsupported = filter (`notElem` ["HEAD", "index", "commondir", "gitdir", "logs",
            "ORIG_HEAD", "COMMIT_EDITMSG", "FETCH_HEAD", "AUTO_MERGE", "REBASE_HEAD", "refs"]) entries
    unless (null unsupported) $
        fail ("unsupported private Git state: " <> show unsupported)
    forM_ entries $ \entry -> do
        status <- Posix.getSymbolicLinkStatus (administration </> entry)
        when (Posix.isSymbolicLink status) $ fail ("symlinked private Git state: " <> entry)
    when ("refs" `elem` entries) $ do
        privateReferences <- Directory.listDirectory (administration </> "refs")
        unless (null privateReferences) $ fail ("unsupported private Git references: " <> show privateReferences)
    logsExist <- Directory.doesDirectoryExist (administration </> "logs")
    when logsExist $ do
        logs <- Directory.listDirectory (administration </> "logs")
        unless (all (== "HEAD") logs) $ fail ("unsupported private Git reflogs: " <> show (filter (/= "HEAD") logs))
    flags <- git repository ["ls-files", "-v", "-z"] ""
    unless (all ((== "H ") . take 2) (nul flags)) $
        fail "unsupported Git index: assume-unchanged or sparse checkout"
    stages <- git repository ["ls-files", "--stage", "-z"] ""
    forM_ (nul stages) $ \entry ->
        case words (takeWhile (/= '\t') entry) of
            [mode, _, "0"] | mode `elem` ["100644", "100755", "120000"] -> pure ()
            _ -> fail "unsupported Git index: conflict or submodule"
    symlinks <- stripped repository ["config", "--default=true", "--get", "core.symlinks"] ""
    unless (symlinks `elem` ["true", "yes", "on", "1"]) $
        fail "disabled symbolic links prevent exact clean recovery"
    names <- git repository ["ls-files", "-z"] ""
    attributes <- git repository ["check-attr", "-z", "--stdin",
        "filter", "working-tree-encoding", "text", "eol", "ident"] names
    checkAttributes (nul attributes)
    status <- git repository ["status", "--porcelain=v1", "-z",
        "--untracked-files=all", "--ignored=matching", "--ignore-submodules=none"] ""
    forM_ (nul status) $ \entry ->
        if take 3 entry == "!! " then checkIgnoredPath (git repository) repository (drop 3 entry)
        else fail "unique staged, unstaged, or untracked files"
    headObject <- stripped repository ["rev-parse", "--verify", "HEAD^{commit}"] ""
    treeObject <- stripped repository ["rev-parse", "--verify", "HEAD^{tree}"] ""
    -- Never trust stat-cache hits made with autocrlf or other historical
    -- settings. A fresh private index forces Git to compare actual working
    -- bytes with the existing tree in one process, without live-index writes.
    temporary <- Directory.getTemporaryDirectory
    bracket (mkdtemp (temporary </> "worktree-clean-inspection-"))
        Directory.removePathForcibly $ \scratch -> do
            let privateGit = gitWithEnvironment [("GIT_INDEX_FILE", scratch </> "index")] repository
            void $ privateGit ["read-tree", treeObject] ""
            void $ privateGit ["update-index", "--really-refresh"] ""
            void $ privateGit ["diff-files", "--quiet", "--no-ext-diff", "--no-textconv"] ""
    branch <- stripped repository ["rev-parse", "--symbolic-full-name", "HEAD"] ""
    index <- BS.readFile (administration </> "index")
    history <- readOptional (administration </> "logs" </> "HEAD")
    original <- readOptional (administration </> "ORIG_HEAD")
    message <- readOptional (administration </> "COMMIT_EDITMSG")
    fetched <- readOptional (administration </> "FETCH_HEAD")
    void $ parseFetchedObjects fetched
    autoMerge <- readOptional (administration </> "AUTO_MERGE")
    rebaseHead <- readOptional (administration </> "REBASE_HEAD")
    void $ validatePseudoReference repository "AUTO_MERGE" "tree" autoMerge
    void $ validatePseudoReference repository "REBASE_HEAD" "commit" rebaseHead
    pure (CleanCheckout headObject treeObject branch index history original message fetched autoMerge rebaseHead)
  where
    checkAttributes [] = pure ()
    checkAttributes (_ : _ : value : rest)
        | value `elem` ["unspecified", "unset"] = checkAttributes rest
        | otherwise = fail "working-file attributes prevent exact clean recovery"
    checkAttributes _ = fail "malformed Git attribute response"

readOptional :: FilePath -> IO BS.ByteString
readOptional path = do
    status <- tryIOError (Posix.getSymbolicLinkStatus path)
    case status of
        Left failure | isDoesNotExistError failure -> pure BS.empty
        Left failure -> ioError failure
        Right metadata -> do
            unless (Posix.isRegularFile metadata) $
                fail ("unsupported private Git history file: " <> takeFileName path)
            BS.readFile path

validObject :: String -> Bool
validObject object = length object `elem` [40, 64] &&
    all (`elem` ("0123456789abcdef" :: String)) object

-- Only inactive, direct pseudorefs are supported. Active operation markers
-- (MERGE_HEAD, rebase-merge, rebase-apply, sequencer, etc.) remain rejected.
-- Retain the exact bytes in the proof so changes invalidate removal.
validatePseudoReference :: FilePath -> String -> String -> BS.ByteString -> IO [String]
validatePseudoReference repository name expectedType bytes
    | BS.null bytes = pure []
    | otherwise = case B8.lines bytes of
        [line] | validObject (B8.unpack line) -> do
            let object = B8.unpack line
            actualType <- stripped repository ["cat-file", "-t", object] ""
            unless (actualType == expectedType) $
                fail ("unsupported " <> name <> " object type: " <> actualType)
            pure [object]
        _ -> fail ("unsupported " <> name <> " contents")

parseFetchedObjects :: BS.ByteString -> IO [String]
parseFetchedObjects = mapM parseEntry . B8.lines
  where
    parseEntry entry = case B8.split '\t' entry of
        object : disposition : descriptionFields
            | validObject (B8.unpack object)
            , disposition `elem` ["", "not-for-merge"]
            , not (null descriptionFields) -> pure (B8.unpack object)
        _ -> fail "unsupported FETCH_HEAD contents"

nul :: String -> [String]
nul [] = []
nul value = case break (== '\0') value of
    (entry, []) -> [entry]
    (entry, _ : rest) -> entry : nul rest

stripped :: FilePath -> [String] -> String -> IO String
stripped repository arguments input =
    Text.unpack . Text.strip . Text.pack <$> git repository arguments input

git :: FilePath -> [String] -> String -> IO String
git = gitWithEnvironment []

gitWithEnvironment :: [(String, String)] -> FilePath -> [String] -> String -> IO String
gitWithEnvironment supplied repository arguments input = do
    environment <- getEnvironment
    let overrides = supplied <>
            [ ("GIT_OPTIONAL_LOCKS", "0"), ("GIT_NO_REPLACE_OBJECTS", "1")
            , ("GIT_NO_LAZY_FETCH", "1")
            , ("GIT_TERMINAL_PROMPT", "0")
            , ("GIT_AUTHOR_NAME", "Agent worktree recovery")
            , ("GIT_AUTHOR_EMAIL", "worktree-recovery@localhost")
            , ("GIT_COMMITTER_NAME", "Agent worktree recovery")
            , ("GIT_COMMITTER_EMAIL", "worktree-recovery@localhost")
            ]
        settings = ["--no-optional-locks", "-c", "core.fsmonitor=false",
            "-c", "core.untrackedCache=false", "-c", "core.ignorestat=false",
            "-c", "core.autocrlf=false", "-c", "core.safecrlf=false",
            "-c", "core.sparseCheckout=false", "-c", "core.sparseCheckoutCone=false",
            "-c", "core.filemode=true", "-c", "core.hooksPath=/dev/null",
            "-c", "commit.gpgsign=false", "-c", "core.fsync=committed,reference"]
    (code, output, errors) <- readCreateProcessWithExitCode
        (proc "git" (settings <> arguments))
            { cwd = Just repository
            , env = Just (overrides <> filter (not . isPrefixOf "GIT_" . fst) environment)
            } input
    unless (code == ExitSuccess) $
        fail ("Git clean-checkout check failed: " <> take 500 errors)
    pure output

result :: IO a -> IO (Either Text a)
result action = either (Left . Text.pack . displayException) Right <$> tryAny action
