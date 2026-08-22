module Agent.CLI.ProjectSpec (spec) where

import Agent.CLI.Project
import System.OsPath (OsPath, decodeUtf, unsafeEncodeUtf)
import Control.Exception (bracket)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified System.Directory as Directory
import System.Directory.OsPath
    ( canonicalizePath
    , createDirectoryIfMissing
    , doesFileExist
    )
import System.Exit (ExitCode(..))
import qualified System.FilePath as FilePath
import System.OsPath ((</>))
import System.Posix.Files (fileMode, getFileStatus)
import System.Posix.Temp (mkdtemp)
import System.Posix.Types (FileMode)
import System.Process (CreateProcess(..), proc, readCreateProcessWithExitCode)
import Test.Hspec

fromFilePath = unsafeEncodeUtf
toFilePath path = either (error . show) id (decodeUtf path)

spec :: Spec
spec = describe "Agent.CLI.Project" do
    describe "projectSettingsPath" do
        it "is <project>/.haskell-agent/settings.json" do
            projectSettingsPath (fromFilePath "/tmp/repo")
                `shouldBe` fromFilePath "/tmp/repo/.haskell-agent/settings.json"

    describe "loadProjectSettings/saveProjectAutoApprove" do
        it "defaults when settings are missing" $
            withTempDir "agent-project-" \root -> do
                settings <- loadProjectSettings root
                settings `shouldBe` defaultProjectSettings

        it "round-trips auto-approve with private file mode" $
            withTempDir "agent-project-" \root -> do
                saveProjectAutoApprove root True
                let path = projectSettingsPath root
                doesFileExist path `shouldReturn` True
                modeOf path `shouldReturn` 0o600
                settings <- loadProjectSettings root
                settings.settingsAutoApprove `shouldBe` True
                bytes <- LBS.readFile (toFilePath path)
                Aeson.eitherDecode' bytes
                    `shouldBe` Right (defaultProjectSettings { settingsAutoApprove = True })

                saveProjectAutoApprove root False
                settings' <- loadProjectSettings root
                settings'.settingsAutoApprove `shouldBe` False

        it "ignores corrupt settings files" $
            withTempDir "agent-project-" \root -> do
                let dir = root </> fromFilePath ".haskell-agent"
                    path = projectSettingsPath root
                createDirectoryIfMissing True dir
                LBS.writeFile (toFilePath path) "{not-json"
                loadProjectSettings root `shouldReturn` defaultProjectSettings

    describe "resolveProjectRoot" do
        it "uses the git toplevel when available" $
            withTempDir "agent-git-" \root -> do
                git_ root ["init"]
                expected <- canonicalizePath root
                let nested = root </> fromFilePath "packages" </> fromFilePath "cli"
                createDirectoryIfMissing True nested
                resolveProjectRoot nested `shouldReturn` expected

        it "stays in a linked worktree instead of the primary checkout" $
            withTempDir "agent-wt-" \root -> do
                let mainRepo = root </> fromFilePath "main"
                    linked = root </> fromFilePath "linked"
                createDirectoryIfMissing True mainRepo
                git_ mainRepo ["init"]
                git_ mainRepo ["config", "user.email", "test@example.com"]
                git_ mainRepo ["config", "user.name", "Test"]
                git_ mainRepo ["config", "commit.gpgsign", "false"]
                git_ mainRepo ["commit", "--allow-empty", "-m", "init"]
                git_ mainRepo ["worktree", "add", "--detach", toFilePath linked]
                expected <- canonicalizePath linked
                resolveProjectRoot linked `shouldReturn` expected
                saveProjectAutoApprove expected True
                settings <- loadProjectSettings =<< resolveProjectRoot linked
                settings.settingsAutoApprove `shouldBe` True
                primarySettings <- loadProjectSettings =<< canonicalizePath mainRepo
                primarySettings.settingsAutoApprove `shouldBe` False

        it "falls back to cwd outside a git repo" $
            withTempDir "agent-nogit-" \root -> do
                expected <- canonicalizePath root
                resolveProjectRoot root `shouldReturn` expected

withTempDir :: String -> (OsPath -> IO a) -> IO a
withTempDir prefix action = do
    tmp <- Directory.getTemporaryDirectory
    bracket
        (mkdtemp (tmp FilePath.</> prefix))
        Directory.removeDirectoryRecursive
        (action . fromFilePath)

modeOf :: OsPath -> IO FileMode
modeOf path = do
    status <- getFileStatus (toFilePath path)
    pure (fileMode status `mod` 0o1000)

git_ :: OsPath -> [String] -> IO ()
git_ dir args = do
    (code, _out, err) <-
        readCreateProcessWithExitCode
            (proc "git" args) { cwd = Just (toFilePath dir) } ""
    case code of
        ExitSuccess -> pure ()
        ExitFailure _ -> expectationFailure ("git " <> unwords args <> ": " <> err)
