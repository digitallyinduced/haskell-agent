module Agent.CLI.McpAdminSpec (spec) where

import Agent.CLI.Config
    ( HarnessConfig(..)
    , McpServerConfig(..)
    , defaultHarnessConfig
    , loadHarnessConfig
    , saveHarnessConfig
    )
import Agent.CLI.McpAdmin
import Agent.MCP (McpProtocolPreference(..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently, poll, wait, withAsync)
import Control.Concurrent.MVar
    ( newEmptyMVar
    , putMVar
    , takeMVar
    )
import Control.Exception.Safe (bracket)
import qualified Data.Map.Strict as Map
import System.Directory.OsPath (getTemporaryDirectory)
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import System.OsPath (OsPath, decodeUtf, unsafeEncodeUtf)
import System.Posix.Temp (mkdtemp)
import Test.Hspec

spec :: Spec
spec = describe "Agent.CLI.McpAdmin" do
    it "redacts environment values and preserves their sorted keys" $
        withTempDir \home -> do
            Right initial <- listMcpAdminServers home
            Right added <- addMcpAdminServer home initial.mcpAdminRevision
                "docs" input
                    { mcpAdminInputEnv =
                        Map.fromList [("TOKEN", "top-secret"), ("A", "hidden")]
                    }
            added.mcpAdminValue.mcpAdminEnvKeys `shouldBe` ["A", "TOKEN"]
            show added.mcpAdminValue `shouldNotContain` "top-secret"
            Right listed <- listMcpAdminServers home
            listed.mcpAdminValue `shouldBe` [added.mcpAdminValue]

    it "rejects stale revisions without overwriting a concurrent edit" $
        withTempDir \home -> do
            Right initial <- listMcpAdminServers home
            Right added <- addMcpAdminServer home initial.mcpAdminRevision
                "docs" input
            stale <- setMcpAdminServerEnabled home initial.mcpAdminRevision
                "docs" False
            stale `shouldBe`
                Left (McpAdminConflict added.mcpAdminRevision)
            Right current <- readMcpAdminServer home "docs"
            current.mcpAdminValue.mcpAdminEnabled `shouldBe` True

    it "validates writes through the shared harness config validator" $
        withTempDir \home -> do
            Right initial <- listMcpAdminServers home
            result <- addMcpAdminServer home initial.mcpAdminRevision
                "broken" input { mcpAdminInputStartupTimeoutSeconds = 0 }
            result `shouldBe`
                Left (McpAdminInvalid
                    "MCP server 'broken' startupTimeoutSeconds must be positive")
            Right config <- loadHarnessConfig home
            config.configMcpServers `shouldBe` Map.empty

    it "supports edit, disable, and remove with successive revisions" $
        withTempDir \home -> do
            Right initial <- listMcpAdminServers home
            Right added <- addMcpAdminServer home initial.mcpAdminRevision
                "docs" input
            Right edited <- editMcpAdminServer home added.mcpAdminRevision
                "docs" input { mcpAdminInputCommand = "other" }
            edited.mcpAdminValue.mcpAdminEnabled `shouldBe` True
            Right disabled <- setMcpAdminServerEnabled home
                edited.mcpAdminRevision "docs" False
            restartMcpAdminServer home disabled.mcpAdminRevision "docs"
                (pure ())
                `shouldReturn` Right disabled
            Right removed <- removeMcpAdminServer home
                disabled.mcpAdminRevision "docs"
            listMcpAdminServers home `shouldReturn`
                Right McpAdminSnapshot
                    { mcpAdminRevision = removed.mcpAdminRevision
                    , mcpAdminValue = []
                    }

    it "allows exactly one writer for a shared snapshot revision" $
        withTempDir \home -> do
            Right initial <- listMcpAdminServers home
            (left, right) <- concurrently
                (addMcpAdminServer home initial.mcpAdminRevision "left" input)
                (addMcpAdminServer home initial.mcpAdminRevision "right" input)
            length (filter isSuccess [left, right]) `shouldBe` 1
            length (filter isConflict [left, right]) `shouldBe` 1
            Right final <- listMcpAdminServers home
            length final.mcpAdminValue `shouldBe` 1
            final.mcpAdminRevision `shouldNotBe` initial.mcpAdminRevision

    it "rejects edits that would erase remote HTTP configuration" $
        withTempDir \home -> do
            let remote = McpServerConfig
                    { mcpEnabled = True
                    , mcpUrl = Just "https://example.test/mcp"
                    , mcpCommand = ""
                    , mcpArgs = []
                    , mcpCwd = Nothing
                    , mcpEnv = Map.empty
                    , mcpStartupTimeoutSeconds = 30
                    , mcpRequestTimeoutSeconds = 60
                    , mcpOAuth = Nothing
                    , mcpProtocol = McpProtocolModern
                    }
            saveHarnessConfig home
                defaultHarnessConfig
                    { configMcpServers = Map.singleton "remote" remote }
                `shouldReturn` Right ()
            Right snapshot <- listMcpAdminServers home
            editMcpAdminServer home snapshot.mcpAdminRevision "remote" input
                `shouldReturn`
                    Left (McpAdminInvalid
                        "remote HTTP MCP servers cannot be edited through this API")
            Right config <- loadHarnessConfig home
            Map.lookup "remote" config.configMcpServers `shouldBe` Just remote

    it "linearizes revision validation with the supervisor restart" $
        withTempDir \home -> do
            Right initial <- listMcpAdminServers home
            Right added <- addMcpAdminServer home initial.mcpAdminRevision
                "docs" input
            entered <- newEmptyMVar
            release <- newEmptyMVar
            let restart = putMVar entered () >> takeMVar release
            withAsync
                (restartMcpAdminServer home added.mcpAdminRevision
                    "docs" restart)
                \restarting -> do
                    takeMVar entered
                    withAsync
                        (setMcpAdminServerEnabled home
                            added.mcpAdminRevision "docs" False)
                        \writing -> do
                            threadDelay 20000
                            poll writing >>= \case
                                Nothing -> pure ()
                                Just _ ->
                                    expectationFailure
                                        "writer bypassed restart lock"
                            putMVar release ()
                            wait restarting `shouldReturn` Right added
                            wait writing >>= \case
                                Left err -> expectationFailure (show err)
                                Right disabled ->
                                    disabled.mcpAdminValue.mcpAdminEnabled
                                        `shouldBe` False

    it "does not run the supervisor restart for a stale revision" $
        withTempDir \home -> do
            Right initial <- listMcpAdminServers home
            Right added <- addMcpAdminServer home initial.mcpAdminRevision
                "docs" input
            Right disabled <- setMcpAdminServerEnabled home
                added.mcpAdminRevision "docs" False
            result <- restartMcpAdminServer home added.mcpAdminRevision
                "docs" (expectationFailure "stale restart action ran")
            result `shouldBe`
                Left (McpAdminConflict disabled.mcpAdminRevision)

input :: McpAdminServerInput
input = McpAdminServerInput
    { mcpAdminInputCommand = "mcp-docs"
    , mcpAdminInputArgs = ["--stdio"]
    , mcpAdminInputCwd = Just "/tmp"
    , mcpAdminInputEnv = Map.empty
    , mcpAdminInputStartupTimeoutSeconds = 30
    , mcpAdminInputRequestTimeoutSeconds = 60
    }

withTempDir :: (OsPath -> IO a) -> IO a
withTempDir action = do
    tmp <- getTemporaryDirectory
    bracket
        (mkdtemp (filePath tmp FilePath.</> "mcp-admin-"))
        Directory.removeDirectoryRecursive
        (action . unsafeEncodeUtf)

filePath :: OsPath -> FilePath
filePath value = either (error . show) id (decodeUtf value)

isSuccess :: Either McpAdminError a -> Bool
isSuccess = either (const False) (const True)

isConflict :: Either McpAdminError a -> Bool
isConflict = \case
    Left (McpAdminConflict _) -> True
    _ -> False
