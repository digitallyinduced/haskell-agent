module Agent.CLI.Worktree.SnapshotSpec (spec) where

import Agent.CLI.Worktree.Snapshot
import Control.Concurrent.Async (concurrently)
import Control.Exception.Safe (bracket)
import Control.Monad (void)
import qualified Data.ByteString as BS
import Data.Either (isLeft, isRight)
import qualified Data.Text as Text
import qualified System.Directory as Dir
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.OsPath (OsPath, unsafeEncodeUtf)
import qualified System.Posix.Files as Posix
import System.Posix.Temp (mkdtemp)
import System.Process (proc, readCreateProcessWithExitCode)
import Test.Hspec

spec :: Spec
spec = describe "worktree recovery snapshots" do
    it "preflights dirty checkouts without writing Git objects, refs or the live index" $
        fixture \repo checkout -> do
            writeFile (checkout </> "new") "not yet a Git object"
            writeFile (checkout </> "tracked") "unstaged"
            gitDir <- trim <$> gitOut checkout ["rev-parse", "--absolute-git-dir"]
            originalIndex <- BS.readFile (gitDir </> "index")
            objects <- gitOut repo ["count-objects", "-v"]
            refs <- gitOut repo ["show-ref"]
            checkSnapshotSupported (p checkout) `shouldReturn` Right ()
            gitOut repo ["count-objects", "-v"] `shouldReturn` objects
            gitOut repo ["show-ref"] `shouldReturn` refs
            BS.readFile (gitDir </> "index") `shouldReturn` originalIndex
            writeFile (gitDir </> "MERGE_HEAD") "pending"
            checkSnapshotSupported (p checkout) >>= (`shouldSatisfy` isLeft)

    it "round trips unique commits, index, raw bytes, symlinks, deletes and untracked names" $
        fixture \repo checkout -> do
            writeFile (checkout </> "tracked") "unique commit\n"
            git checkout ["commit", "-am", "unique"]
            unique <- gitOut checkout ["rev-parse", "HEAD"]
            writeFile (checkout </> "tracked") "staged\n"
            BS.writeFile (checkout </> "binary") (BS.pack [0, 128, 255, 10])
            git checkout ["add", "tracked", "binary"]
            writeFile (checkout </> "tracked") "unstaged\r\n"
            BS.writeFile (checkout </> "binary") (BS.pack [255, 0, 1, 13, 10])
            writeFile (checkout </> "untracked\n\tquote\"") "loose\n"
            writeFile (checkout </> ".env") "excluded secret\n"
            Posix.createSymbolicLink "missing-target" (checkout </> "link")
            writeFile (checkout </> "script") "#!/bin/sh\n"
            Posix.setFileMode (checkout </> "script") 0o755
            git checkout ["rm", "deleted"]
            -- Staged deletion with a replacement must remain untracked.
            writeFile (checkout </> "deleted") "replacement\n"
            gitDir <- trim <$> gitOut checkout ["rev-parse", "--absolute-git-dir"]
            originalIndex <- BS.readFile (gitDir </> "index")
            staged <- gitOut checkout ["diff", "--cached", "--binary"]
            unstaged <- gitOut checkout ["diff", "--binary"]
            snapshot <- expectRight =<< createSnapshot (p checkout)
            BS.readFile (gitDir </> "index") `shouldReturn` originalIndex
            gitOut checkout ["rev-parse", "HEAD"] `shouldReturn` unique
            verifySnapshotUnchanged (p checkout) snapshot `shouldReturn` Right ()
            git repo ["worktree", "remove", "--force", checkout]
            git repo ["branch", "-D", "session"]
            git repo ["gc", "--prune=now"]
            restoreSnapshot (p repo) (p checkout) snapshot `shouldReturn` Right ()
            gitOut checkout ["rev-parse", "HEAD"] `shouldReturn` unique
            gitOut checkout ["diff", "--cached", "--binary"] `shouldReturn` staged
            gitOut checkout ["diff", "--binary"] `shouldReturn` unstaged
            BS.readFile (checkout </> "binary") `shouldReturn` BS.pack [255, 0, 1, 13, 10]
            readFile (checkout </> "untracked\n\tquote\"") `shouldReturn` "loose\n"
            Posix.readSymbolicLink (checkout </> "link") `shouldReturn` "missing-target"
            executable <- Posix.fileMode <$> Posix.getFileStatus (checkout </> "script")
            executable `shouldBe` 0o100755
            Dir.doesPathExist (checkout </> ".env") `shouldReturn` False

    it "never applies clean or smudge filters when saving working bytes" $
        fixture \repo checkout -> do
            git repo ["config", "filter.lossy.clean", "tr a-z A-Z"]
            git repo ["config", "filter.lossy.smudge", "tr A-Z a-z"]
            writeFile (checkout </> ".gitattributes") "tracked filter=lossy\n"
            writeFile (checkout </> "tracked") "MiXeD\r\n"
            git checkout ["add", ".gitattributes", "tracked"]
            staged <- gitOut checkout ["show", ":tracked"]
            snapshot <- expectRight =<< createSnapshot (p checkout)
            git repo ["worktree", "remove", "--force", checkout]
            restoreSnapshot (p repo) (p checkout) snapshot `shouldReturn` Right ()
            BS.readFile (checkout </> "tracked") `shouldReturn` "MiXeD\r\n"
            gitOut checkout ["show", ":tracked"] `shouldReturn` staged

    it "detects edits, including newly created untracked files, but ignores ignored edits" $
        fixture \_ checkout -> do
            snapshot <- expectRight =<< createSnapshot (p checkout)
            writeFile (checkout </> ".env") "ignored"
            verifySnapshotUnchanged (p checkout) snapshot `shouldReturn` Right ()
            writeFile (checkout </> "new") "valuable"
            verifySnapshotUnchanged (p checkout) snapshot >>= (`shouldSatisfy` isLeft)
            Dir.removeFile (checkout </> "new")
            writeFile (checkout </> "tracked") "changed"
            verifySnapshotUnchanged (p checkout) snapshot >>= (`shouldSatisfy` isLeft)

    it "rejects incomplete recovery refs before creating a destination" $
        fixture \repo checkout -> do
            snapshot <- expectRight =<< createSnapshot (p checkout)
            git repo ["update-ref", "-d", Text.unpack snapshot.snapshotRef]
            let target = checkout <> "-restore"
            restoreSnapshot (p repo) (p target) snapshot >>= (`shouldSatisfy` isLeft)
            Dir.doesPathExist target `shouldReturn` False

    it "rejects missing recovery blobs before authorizing collection or restoration" $
        fixture \repo checkout -> do
            writeFile (checkout </> "precious") "only in this untracked file\n"
            snapshot <- expectRight =<< createSnapshot (p checkout)
            object <- trim <$> gitOut repo
                ["rev-parse", Text.unpack snapshot.snapshotWorkTree <> ":precious"]
            let objectPath = repo </> ".git" </> "objects" </> take 2 object </> drop 2 object
                target = checkout <> "-restore"
            Dir.removeFile objectPath
            verifySnapshotUnchanged (p checkout) snapshot >>= (`shouldSatisfy` isLeft)
            restoreSnapshot (p repo) (p target) snapshot >>= (`shouldSatisfy` isLeft)
            Dir.doesPathExist target `shouldReturn` False
            readFile (checkout </> "precious") `shouldReturn` "only in this untracked file\n"

    it "retains staged children beneath a replaced symlink parent" $
        fixture \repo checkout -> do
            Dir.createDirectory (checkout </> "directory")
            writeFile (checkout </> "directory" </> "child") "staged"
            git checkout ["add", "directory/child"]
            Dir.removePathForcibly (checkout </> "directory")
            Posix.createSymbolicLink repo (checkout </> "directory")
            createSnapshot (p checkout) >>= (`shouldSatisfy` isLeft)
            readFile (repo </> "tracked") `shouldReturn` "base\n"

    it "does not overwrite an existing directory, file or dangling symlink" $
        fixture \repo checkout -> do
            snapshot <- expectRight =<< createSnapshot (p checkout)
            let target = checkout <> "-restore"
            Dir.createDirectory target
            restoreSnapshot (p repo) (p target) snapshot >>= (`shouldSatisfy` isLeft)
            Dir.removeDirectory target
            writeFile target "keep"
            restoreSnapshot (p repo) (p target) snapshot >>= (`shouldSatisfy` isLeft)
            readFile target `shouldReturn` "keep"
            Dir.removeFile target
            Posix.createSymbolicLink "absent" target
            restoreSnapshot (p repo) (p target) snapshot >>= (`shouldSatisfy` isLeft)
            Posix.readSymbolicLink target `shouldReturn` "absent"

    it "restores detached without resetting a moved branch" $
        fixture \repo checkout -> do
            snapshot <- expectRight =<< createSnapshot (p checkout)
            git repo ["worktree", "remove", "--force", checkout]
            git repo ["commit", "--allow-empty", "-m", "new"]
            new <- gitOut repo ["rev-parse", "HEAD"]
            git repo ["branch", "-f", "session", "HEAD"]
            restoreSnapshot (p repo) (p checkout) snapshot `shouldReturn` Right ()
            gitOut repo ["rev-parse", "session"] `shouldReturn` new
            trim <$> gitOut checkout ["rev-parse", "--abbrev-ref", "HEAD"] `shouldReturn` "HEAD"

    it "repairs only the missing destination's stale unlocked registration" $
        fixture \repo checkout -> do
            writeFile (checkout </> "loose") "recover this"
            snapshot <- expectRight =<< createSnapshot (p checkout)
            -- Simulate an interrupted removal: files gone, registration left.
            Dir.removePathForcibly checkout
            restoreSnapshot (p repo) (p checkout) snapshot `shouldReturn` Right ()
            readFile (checkout </> "loose") `shouldReturn` "recover this"

    it "does not override a stale locked registration" $
        fixture \repo checkout -> do
            snapshot <- expectRight =<< createSnapshot (p checkout)
            git repo ["worktree", "lock", "--reason", "keep", checkout]
            Dir.removePathForcibly checkout
            restoreSnapshot (p repo) (p checkout) snapshot >>= (`shouldSatisfy` isLeft)
            registrations <- gitOut repo ["worktree", "list", "--porcelain"]
            registrations `shouldContain` "locked keep"

    it "reserves a destination against concurrent restores" $
        fixture \repo checkout -> do
            snapshot <- expectRight =<< createSnapshot (p checkout)
            let restore = restoreSnapshot (p repo) (p (checkout <> "-restore")) snapshot
            (left, right) <- concurrently restore restore
            length (filter isRight [left, right]) `shouldBe` 1
            length (filter isLeft [left, right]) `shouldBe` 1

    it "retains unsupported in-progress operations and special index states" $
        fixture \_ checkout -> do
            gitDir <- trim <$> gitOut checkout ["rev-parse", "--absolute-git-dir"]
            writeFile (gitDir </> "MERGE_HEAD") "pending"
            createSnapshot (p checkout) >>= (`shouldSatisfy` isLeft)
            Dir.removeFile (gitDir </> "MERGE_HEAD")
            git checkout ["update-index", "--assume-unchanged", "tracked"]
            createSnapshot (p checkout) >>= (`shouldSatisfy` isLeft)
            git checkout ["update-index", "--no-assume-unchanged", "tracked"]
            writeFile (checkout </> "intent") "not staged"
            git checkout ["add", "-N", "intent"]
            createSnapshot (p checkout) >>= (`shouldSatisfy` isLeft)

    it "retains nested repositories rather than losing their history" $
        fixture \_ checkout -> do
            Dir.createDirectory (checkout </> "nested")
            git (checkout </> "nested") ["init"]
            createSnapshot (p checkout) >>= (`shouldSatisfy` isLeft)

p :: FilePath -> OsPath
p = unsafeEncodeUtf

trim :: String -> String
trim = reverse . dropWhile (== '\n') . reverse

expectRight :: Show e => Either e a -> IO a
expectRight = either (fail . show) pure

fixture :: (FilePath -> FilePath -> IO a) -> IO a
fixture action = do
    tmp <- Dir.getTemporaryDirectory
    bracket (mkdtemp (tmp </> "snapshot-spec-")) Dir.removePathForcibly \root -> do
        let repo = root </> "repo"
            checkout = root </> "checkout"
        Dir.createDirectory repo
        git repo ["init"]
        git repo ["config", "user.name", "Snapshot Test"]
        git repo ["config", "user.email", "snapshot@example.invalid"]
        git repo ["config", "commit.gpgsign", "false"]
        writeFile (repo </> "tracked") "base\n"
        writeFile (repo </> "deleted") "delete me\n"
        writeFile (repo </> ".gitignore") ".env\n"
        git repo ["add", "."]
        git repo ["commit", "-m", "base"]
        git repo ["worktree", "add", "-b", "session", checkout]
        action repo checkout

git :: FilePath -> [String] -> IO ()
git repo = void . gitOut repo

gitOut :: FilePath -> [String] -> IO String
gitOut repo args = do
    (code, output, err) <- readCreateProcessWithExitCode
        (proc "git" (["-C", repo] <> args)) ""
    unlessSuccess code err
    pure output
  where
    unlessSuccess ExitSuccess _ = pure ()
    unlessSuccess _ err = fail err
