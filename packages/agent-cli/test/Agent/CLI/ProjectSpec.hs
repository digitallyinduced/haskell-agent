module Agent.CLI.ProjectSpec (spec) where

import Agent.CLI.Project
import Control.Exception (bracket)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import System.Directory
    ( canonicalizePath
    , createDirectoryIfMissing
    , doesFileExist
    , getTemporaryDirectory
    , removeDirectoryRecursive
    )
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.Posix.Files (fileMode, getFileStatus)
import System.Posix.Temp (mkdtemp)
import System.Posix.Types (FileMode)
import System.Process (CreateProcess(..), proc, readCreateProcessWithExitCode)
import Test.Hspec

spec :: Spec
spec = describe "Agent.CLI.Project" do
    describe "projectSettingsPath" do
        it "is <project>/.haskell-agent/settings.json" do
            projectSettingsPath "/tmp/repo"
                `shouldBe` "/tmp/repo" </> ".haskell-agent" </> "settings.json"

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
                bytes <- LBS.readFile path
                Aeson.eitherDecode' bytes
                    `shouldBe` Right (defaultProjectSettings { settingsAutoApprove = True })

                saveProjectAutoApprove root False
                settings' <- loadProjectSettings root
                settings'.settingsAutoApprove `shouldBe` False

        it "ignores corrupt settings files" $
            withTempDir "agent-project-" \root -> do
                let dir = root </> ".haskell-agent"
                    path = projectSettingsPath root
                createDirectoryIfMissing True dir
                LBS.writeFile path "{not-json"
                loadProjectSettings root `shouldReturn` defaultProjectSettings

    describe "resolveProjectRoot" do
        it "uses the git toplevel when available" $
            withTempDir "agent-git-" \root -> do
                git_ root ["init"]
                expected <- canonicalizePath root
                let nested = root </> "packages" </> "cli"
                createDirectoryIfMissing True nested
                resolveProjectRoot nested `shouldReturn` expected

        it "falls back to cwd outside a git repo" $
            withTempDir "agent-nogit-" \root -> do
                expected <- canonicalizePath root
                resolveProjectRoot root `shouldReturn` expected

withTempDir :: String -> (FilePath -> IO a) -> IO a
withTempDir prefix action = do
    tmp <- getTemporaryDirectory
    bracket (mkdtemp (tmp </> prefix)) removeDirectoryRecursive action

modeOf :: FilePath -> IO FileMode
modeOf path = do
    status <- getFileStatus path
    pure (fileMode status `mod` 0o1000)

git_ :: FilePath -> [String] -> IO ()
git_ dir args = do
    (code, _out, err) <-
        readCreateProcessWithExitCode (proc "git" args) { cwd = Just dir } ""
    case code of
        ExitSuccess -> pure ()
        ExitFailure _ -> expectationFailure ("git " <> unwords args <> ": " <> err)
