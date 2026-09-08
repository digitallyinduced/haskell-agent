module Agent.CLI.WorktreeCleanSpec (spec) where

import Agent.CLI.Worktree.Clean
import qualified Agent.CLI.Worktree.Snapshot as Snapshot
import Control.Exception.Safe (bracket)
import Control.Monad (void)
import qualified Data.ByteString as BS
import Data.Either (isLeft, isRight)
import Data.List (isInfixOf)
import qualified Data.Text as Text
import qualified System.Directory as Directory
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.OsPath (unsafeEncodeUtf)
import System.Posix.Temp (mkdtemp)
import System.Process (CreateProcess(..), proc, readCreateProcessWithExitCode)
import Test.Hspec

spec :: Spec
spec = describe "clean worktree recovery" $ do
    it "inspects without changing the live index or creating recovery refs" $
        withCheckout $ \repository checkout -> do
            administration <- git checkout ["rev-parse", "--absolute-git-dir"]
            before <- BS.readFile (administration </> "index")
            inspectCleanCheckout (unsafeEncodeUtf checkout) `shouldReturnSatisfy` isRight
            BS.readFile (administration </> "index") `shouldReturn` before
            git repository ["for-each-ref", "refs/haskell-agent"] `shouldReturn` ""
    it "retains unstaged, staged, and untracked work" $
        withCheckout $ \_ checkout -> do
            writeFile (checkout </> "source.txt") "changed\n"
            inspectCleanCheckout (unsafeEncodeUtf checkout) `shouldReturnSatisfy` isLeft
            void $ git checkout ["add", "source.txt"]
            inspectCleanCheckout (unsafeEncodeUtf checkout) `shouldReturnSatisfy` isLeft
            void $ git checkout ["reset", "--hard", "HEAD"]
            writeFile (checkout </> "notes.txt") "private work\n"
            inspectCleanCheckout (unsafeEncodeUtf checkout) `shouldReturnSatisfy` isLeft
    it "accepts unchanged LF bytes under autocrlf=input but retains converted CRLF bytes" $
        withCheckout $ \_ checkout -> do
            void $ git checkout ["config", "core.autocrlf", "input"]
            inspectCleanCheckout (unsafeEncodeUtf checkout) `shouldReturnSatisfy` isRight
            writeFile (checkout </> "source.txt") "original\r\n"
            -- Populate the normal index's stat cache under conversion settings.
            void $ git checkout ["add", "source.txt"]
            inspectCleanCheckout (unsafeEncodeUtf checkout) `shouldReturnSatisfy` isLeft
    it "retains ignored private files but permits repository-ignored output directories" $
        withCheckout $ \_ checkout -> do
            writeFile (checkout </> ".gitignore") "dist-newstyle/\n.env\n"
            void $ git checkout ["add", ".gitignore"]
            void $ git checkout ["commit", "-m", "Specify ignored paths"]
            Directory.createDirectory (checkout </> "dist-newstyle")
            writeFile (checkout </> "dist-newstyle" </> "output") "cache"
            inspectCleanCheckout (unsafeEncodeUtf checkout) `shouldReturnSatisfy` isRight
            writeFile (checkout </> ".env") "private configuration"
            inspectCleanCheckout (unsafeEncodeUtf checkout) `shouldReturnSatisfy` isLeft
    it "does not treat global ignore rules as repository authorization" $
        withCheckout $ \repository checkout -> do
            let exclusions = repository </> "global-excludes"
            writeFile exclusions "dist-newstyle/\n"
            void $ git checkout ["config", "core.excludesFile", exclusions]
            Directory.createDirectory (checkout </> "dist-newstyle")
            writeFile (checkout </> "dist-newstyle" </> "output") "private"
            inspectCleanCheckout (unsafeEncodeUtf checkout) `shouldReturnSatisfy` isLeft
    it "retains byte transformations and assume-unchanged files" $
        withCheckout $ \_ checkout -> do
            writeFile (checkout </> ".gitattributes") "source.txt text\n"
            void $ git checkout ["add", ".gitattributes"]
            void $ git checkout ["commit", "-m", "Specify text conversion"]
            inspectCleanCheckout (unsafeEncodeUtf checkout) `shouldReturnSatisfy` isLeft
            void $ git checkout ["rm", ".gitattributes"]
            void $ git checkout ["commit", "-m", "Remove text conversion"]
            void $ git checkout ["update-index", "--assume-unchanged", "source.txt"]
            inspectCleanCheckout (unsafeEncodeUtf checkout) `shouldReturnSatisfy` isLeft
    it "retains private Git operation state" $
        withCheckout $ \_ checkout -> do
            administration <- git checkout ["rev-parse", "--absolute-git-dir"]
            writeFile (administration </> "MERGE_HEAD") "unfinished"
            inspectCleanCheckout (unsafeEncodeUtf checkout) `shouldReturnSatisfy` isLeft
    it "detects edits after the initial proof" $
        withCheckout $ \_ checkout -> do
            proof <- inspectCleanCheckout (unsafeEncodeUtf checkout) >>= requireRight
            writeFile (checkout </> "source.txt") "changed after inspection"
            verifyCleanCheckout (unsafeEncodeUtf checkout) proof `shouldReturnSatisfy` isLeft
    it "does not authorize recovery when an existing blob object is missing" $
        withCheckout $ \repository checkout -> do
            proof <- inspectCleanCheckout (unsafeEncodeUtf checkout) >>= requireRight
            object <- git checkout ["rev-parse", "HEAD:source.txt"]
            Directory.removeFile (repository </> ".git" </> "objects" </>
                take 2 object </> drop 2 object)
            preserveCleanCheckout (unsafeEncodeUtf checkout) proof `shouldReturnSatisfy` isLeft
    it "retains worktree-private references" $
        withCheckout $ \_ checkout -> do
            void $ git checkout ["update-ref", "refs/worktree/private", "HEAD"]
            inspectCleanCheckout (unsafeEncodeUtf checkout) `shouldReturnSatisfy` isLeft
    it "preserves FETCH_HEAD objects and metadata before collection" $
        withCheckout $ \repository checkout -> do
            void $ git checkout ["fetch", repository, "main"]
            administration <- git checkout ["rev-parse", "--absolute-git-dir"]
            fetched <- readFile (administration </> "FETCH_HEAD")
            proof <- inspectCleanCheckout (unsafeEncodeUtf checkout) >>= requireRight
            snapshot <- preserveCleanCheckout (unsafeEncodeUtf checkout) proof >>= requireRight
            let identifier = last (Text.splitOn "/" snapshot.snapshotRef)
                reference = "refs/haskell-agent/reclaimed/" <> Text.unpack identifier <> "/fetched-heads"
            git repository ["cat-file", "blob", reference] `shouldReturn`
                Text.unpack (Text.strip (Text.pack fetched))
    it "retains malformed FETCH_HEAD contents" $
        withCheckout $ \_ checkout -> do
            administration <- git checkout ["rev-parse", "--absolute-git-dir"]
            writeFile (administration </> "FETCH_HEAD") "unknown fetched state\n"
            inspectCleanCheckout (unsafeEncodeUtf checkout) `shouldReturnSatisfy` isLeft
    it "preserves inactive AUTO_MERGE trees and REBASE_HEAD commits" $
        withCheckout $ \repository checkout -> do
            administration <- git checkout ["rev-parse", "--absolute-git-dir"]
            original <- git checkout ["rev-parse", "HEAD"]
            writeFile (checkout </> "source.txt") "abandoned resolution\n"
            void $ git checkout ["commit", "-am", "Temporary resolution"]
            tree <- git checkout ["rev-parse", "HEAD^{tree}"]
            commit <- git checkout ["rev-parse", "HEAD"]
            void $ git checkout ["reset", "--hard", original]
            writeFile (administration </> "AUTO_MERGE") (tree <> "\n")
            writeFile (administration </> "REBASE_HEAD") (commit <> "\n")
            proof <- inspectCleanCheckout (unsafeEncodeUtf checkout) >>= requireRight
            snapshot <- preserveCleanCheckout (unsafeEncodeUtf checkout) proof >>= requireRight
            let identifier = Text.unpack (last (Text.splitOn "/" snapshot.snapshotRef))
                prefix = "refs/haskell-agent/reclaimed/" <> identifier <> "/"
            git repository ["rev-parse", prefix <> "auto-merge"] `shouldReturn` tree
            git repository ["rev-parse", prefix <> commit] `shouldReturn` commit
            git repository ["show", prefix <> "auto-merge:source.txt"] `shouldReturn` "abandoned resolution"
            verifyCleanCheckout (unsafeEncodeUtf checkout) proof `shouldReturn` Right ()
            writeFile (administration </> "REBASE_HEAD") (original <> "\n")
            verifyCleanCheckout (unsafeEncodeUtf checkout) proof `shouldReturnSatisfy` isLeft
    it "retains active rebase state even with valid inactive pseudorefs" $
        withCheckout $ \_ checkout -> do
            administration <- git checkout ["rev-parse", "--absolute-git-dir"]
            commit <- git checkout ["rev-parse", "HEAD"]
            writeFile (administration </> "REBASE_HEAD") (commit <> "\n")
            Directory.createDirectory (administration </> "rebase-merge")
            outcome <- inspectCleanCheckout (unsafeEncodeUtf checkout)
            outcome `shouldSatisfy` either (Text.isInfixOf "rebase-merge") (const False)
    it "retains malformed and incorrectly typed inactive pseudorefs" $
        withCheckout $ \_ checkout -> do
            administration <- git checkout ["rev-parse", "--absolute-git-dir"]
            commit <- git checkout ["rev-parse", "HEAD"]
            writeFile (administration </> "AUTO_MERGE") (commit <> "\n")
            inspectCleanCheckout (unsafeEncodeUtf checkout) `shouldReturnSatisfy` isLeft
            Directory.removeFile (administration </> "AUTO_MERGE")
            writeFile (administration </> "REBASE_HEAD") "ref: refs/heads/main\n"
            inspectCleanCheckout (unsafeEncodeUtf checkout) `shouldReturnSatisfy` isLeft
    it "rechecks AUTO_MERGE changes before recovery preservation" $
        withCheckout $ \_ checkout -> do
            administration <- git checkout ["rev-parse", "--absolute-git-dir"]
            tree <- git checkout ["rev-parse", "HEAD^{tree}"]
            writeFile (administration </> "AUTO_MERGE") (tree <> "\n")
            proof <- inspectCleanCheckout (unsafeEncodeUtf checkout) >>= requireRight
            Directory.removeFile (administration </> "AUTO_MERGE")
            preserveCleanCheckout (unsafeEncodeUtf checkout) proof `shouldReturnSatisfy` isLeft
    it "creates legacy-compatible recovery using existing tree objects" $
        withCheckout $ \repository checkout -> do
            proof <- inspectCleanCheckout (unsafeEncodeUtf checkout) >>= requireRight
            snapshot <- preserveCleanCheckout (unsafeEncodeUtf checkout) proof >>= requireRight
            Snapshot.verifySnapshotUnchanged (unsafeEncodeUtf checkout) snapshot `shouldReturn` Right ()
            let restored = repository </> "restored"
            Snapshot.restoreSnapshot (unsafeEncodeUtf repository) (unsafeEncodeUtf restored) snapshot
                `shouldReturn` Right ()
            readFile (restored </> "source.txt") `shouldReturn` "original\n"
    it "preserves commits referenced only by the checkout reflog" $
        withCheckout $ \repository checkout -> do
            original <- git checkout ["rev-parse", "HEAD"]
            writeFile (checkout </> "source.txt") "temporary commit\n"
            void $ git checkout ["commit", "-am", "Create recoverable commit"]
            abandoned <- git checkout ["rev-parse", "HEAD"]
            void $ git checkout ["reset", "--hard", original]
            proof <- inspectCleanCheckout (unsafeEncodeUtf checkout) >>= requireRight
            void $ preserveCleanCheckout (unsafeEncodeUtf checkout) proof >>= requireRight
            references <- git repository ["for-each-ref", "--format=%(objectname)",
                "refs/haskell-agent/reclaimed"]
            references `shouldSatisfy` isInfixOf abandoned

shouldReturnSatisfy :: Show a => IO a -> (a -> Bool) -> Expectation
shouldReturnSatisfy action predicate = action >>= (`shouldSatisfy` predicate)

requireRight :: Show error => Either error a -> IO a
requireRight = either (fail . show) pure

withCheckout :: (FilePath -> FilePath -> IO a) -> IO a
withCheckout action = do
    temporary <- Directory.getTemporaryDirectory
    bracket (mkdtemp (temporary </> "worktree-clean-spec-"))
        Directory.removePathForcibly $ \directory -> do
            let repository = directory </> "repository"
                checkout = directory </> "checkout"
            Directory.createDirectory repository
            void $ git repository ["init", "-b", "main"]
            void $ git repository ["config", "user.name", "Worktree test"]
            void $ git repository ["config", "user.email", "worktree-test@localhost"]
            void $ git repository ["config", "commit.gpgsign", "false"]
            void $ git repository ["config", "core.autocrlf", "false"]
            void $ git repository ["config", "core.attributesFile", "/dev/null"]
            void $ git repository ["config", "core.excludesFile", "/dev/null"]
            writeFile (repository </> "source.txt") "original\n"
            void $ git repository ["add", "source.txt"]
            void $ git repository ["commit", "-m", "Initial commit"]
            void $ git repository ["worktree", "add", "--detach", checkout]
            action repository checkout

git :: FilePath -> [String] -> IO String
git directory arguments = do
    (code, output, errors) <- readCreateProcessWithExitCode
        (proc "git" arguments) { cwd = Just directory } ""
    unlessSuccess code errors
    pure (Text.unpack (Text.strip (Text.pack output)))
  where
    unlessSuccess ExitSuccess _ = pure ()
    unlessSuccess _ errors = fail errors
