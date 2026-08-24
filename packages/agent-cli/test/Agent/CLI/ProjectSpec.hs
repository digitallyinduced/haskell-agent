module Agent.CLI.ProjectSpec (spec) where

import Agent.CLI.Project
import Agent.CLI.Models (ModelTarget(..))
import Agent.Dialect (DialectId(..))
import Agent.Provider (Provider(..))
import System.OsPath (OsPath, decodeUtf, unsafeEncodeUtf)
import Control.Exception (bracket)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.Text as Text
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

target
    :: Provider
    -> Text.Text
    -> Text.Text
    -> Text.Text
    -> DialectId
    -> ModelTarget
target provider connection model wireModel dialect = ModelTarget
    { targetProvider = provider
    , targetConnectionId = connection
    , targetModelId = model
    , targetWireModelId = wireModel
    , targetDialect = dialect
    }

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

        it "round-trips the last provider/model/dialect and preserves other settings" $
            withTempDir "agent-project-" \root -> do
                saveProjectAutoApprove root True
                saveProjectModel root
                    (target OpenAIProvider "openai"
                        "gpt-project" "gpt-project" CodexDialect)

                let path = projectSettingsPath root
                modeOf path `shouldReturn` 0o600
                settings <- loadProjectSettings root
                settings.settingsAutoApprove `shouldBe` True
                settings.settingsLastModel `shouldBe` Just ProjectModel
                    { projectModelTarget =
                        target OpenAIProvider "openai"
                            "gpt-project" "gpt-project" CodexDialect
                    }
                projectModelProvider settings `shouldBe` Just OpenAIProvider
                projectModelFor OpenAIProvider settings
                    `shouldBe` Just "gpt-project"
                projectDialectFor OpenAIProvider settings
                    `shouldBe` Just CodexDialect
                projectModelFor XAIProvider settings `shouldBe` Nothing
                projectDialectFor XAIProvider settings `shouldBe` Nothing

                saveProjectAutoApprove root False
                updated <- loadProjectSettings root
                updated.settingsAutoApprove `shouldBe` False
                projectModelFor OpenAIProvider updated
                    `shouldBe` Just "gpt-project"

        it "replaces the remembered provider/model without resetting approval" $
            withTempDir "agent-project-" \root -> do
                saveProjectAutoApprove root True
                saveProjectModel root
                    (target OpenAIProvider "openai"
                        "gpt-old" "gpt-old" CodexDialect)
                saveProjectModel root
                    (target XAIProvider "xai"
                        "grok-new" "grok-new" GrokBuildDialect)

                settings <- loadProjectSettings root
                settings.settingsAutoApprove `shouldBe` True
                projectModelProvider settings `shouldBe` Just XAIProvider
                projectModelFor XAIProvider settings `shouldBe` Just "grok-new"
                projectDialectFor XAIProvider settings
                    `shouldBe` Just GrokBuildDialect
                projectModelFor OpenAIProvider settings `shouldBe` Nothing

        it "remembers one successful account per provider without storing secrets" $
            withTempDir "agent-project-" \root -> do
                saveProjectAccount root OpenAIProvider
                    "managed:openai-1" "chatgpt-account-1"
                saveProjectAccount root XAIProvider
                    "managed:xai-1" "grok-account-1"

                settings <- loadProjectSettings root
                projectAccountFor OpenAIProvider settings
                    `shouldBe` Just ProjectAccount
                        { projectAccountProvider = OpenAIProvider
                        , projectAccountSelectionId = "managed:openai-1"
                        , projectAccountId = "chatgpt-account-1"
                        }
                projectAccountFor XAIProvider settings
                    `shouldBe` Just ProjectAccount
                        { projectAccountProvider = XAIProvider
                        , projectAccountSelectionId = "managed:xai-1"
                        , projectAccountId = "grok-account-1"
                        }
                bytes <- LBS.readFile
                    (toFilePath (projectSettingsPath root))
                Text.isInfixOf "secret" (Text.pack (LBS8.unpack bytes))
                    `shouldBe` False

                saveProjectAccount root OpenAIProvider
                    "managed:openai-2" "chatgpt-account-2"
                updated <- loadProjectSettings root
                projectAccountFor OpenAIProvider updated
                    `shouldBe` Just ProjectAccount
                        { projectAccountProvider = OpenAIProvider
                        , projectAccountSelectionId = "managed:openai-2"
                        , projectAccountId = "chatgpt-account-2"
                        }

        it "loads legacy OpenRouter settings with the old Grok dialect" $
            withTempDir "agent-project-" \root -> do
                let dir = root </> fromFilePath ".haskell-agent"
                    path = projectSettingsPath root
                createDirectoryIfMissing True dir
                LBS.writeFile
                    (toFilePath path)
                    "{\"version\":1,\"autoApprove\":true,\"lastModel\":{\"provider\":\"openrouter\",\"model\":\"openai/gpt-5.1\"}}"

                settings <- loadProjectSettings root
                settings.settingsAutoApprove `shouldBe` True
                projectModelFor OpenRouterProvider settings
                    `shouldBe` Just "openai/gpt-5.1"
                projectDialectFor OpenRouterProvider settings
                    `shouldBe` Just GrokBuildDialect
                fmap (.projectModelTarget.targetWireModelId)
                    settings.settingsLastModel
                    `shouldBe` Just "openai/gpt-5.1"
                fmap (.projectModelTarget.targetConnectionId)
                    settings.settingsLastModel
                    `shouldBe` Just "openrouter"

        it "loads legacy settings without a remembered model" $
            withTempDir "agent-project-" \root -> do
                let dir = root </> fromFilePath ".haskell-agent"
                    path = projectSettingsPath root
                createDirectoryIfMissing True dir
                LBS.writeFile
                    (toFilePath path)
                    "{\"version\":1,\"autoApprove\":true}"

                settings <- loadProjectSettings root
                settings.settingsAutoApprove `shouldBe` True
                settings.settingsLastModel `shouldBe` Nothing
                settings.settingsLastAccounts `shouldBe` []

        it "ignores an unknown remembered dialect without resetting approval" $
            withTempDir "agent-project-" \root -> do
                let dir = root </> fromFilePath ".haskell-agent"
                    path = projectSettingsPath root
                createDirectoryIfMissing True dir
                LBS.writeFile
                    (toFilePath path)
                    "{\"version\":1,\"autoApprove\":true,\"lastModel\":{\"provider\":\"openrouter\",\"model\":\"openai/gpt-5.1\",\"dialect\":\"retired\"}}"

                settings <- loadProjectSettings root
                settings.settingsAutoApprove `shouldBe` True
                settings.settingsLastModel `shouldBe` Nothing

        it "ignores an incompatible remembered dialect" $
            withTempDir "agent-project-" \root -> do
                let dir = root </> fromFilePath ".haskell-agent"
                    path = projectSettingsPath root
                createDirectoryIfMissing True dir
                LBS.writeFile
                    (toFilePath path)
                    "{\"version\":1,\"autoApprove\":true,\"lastModel\":{\"provider\":\"openai\",\"model\":\"gpt-5.6-luna\",\"dialect\":\"grok-build\"}}"

                settings <- loadProjectSettings root
                settings.settingsAutoApprove `shouldBe` True
                settings.settingsLastModel `shouldBe` Nothing

        it "ignores only a malformed remembered model" $
            withTempDir "agent-project-" \root -> do
                let dir = root </> fromFilePath ".haskell-agent"
                    path = projectSettingsPath root
                createDirectoryIfMissing True dir
                LBS.writeFile
                    (toFilePath path)
                    "{\"version\":1,\"autoApprove\":true,\"lastModel\":{\"provider\":\"retired\",\"model\":\"old\"}}"

                settings <- loadProjectSettings root
                settings.settingsAutoApprove `shouldBe` True
                settings.settingsLastModel `shouldBe` Nothing

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
