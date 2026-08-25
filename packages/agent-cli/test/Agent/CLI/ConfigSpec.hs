module Agent.CLI.ConfigSpec (spec) where

import Agent.CLI.Config
import Control.Exception.Safe (bracket)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import System.Directory.OsPath
    ( createDirectoryIfMissing
    , getTemporaryDirectory
    )
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import System.OsPath
    ( OsPath
    , decodeUtf
    , takeDirectory
    , unsafeEncodeUtf
    )
import System.Posix.Temp (mkdtemp)
import System.Posix.Files
    ( fileMode
    , getFileStatus
    )
import Test.Hspec

spec :: Spec
spec = describe "Agent.CLI.Config" do
    it "uses ~/.haskell-agent/config.json" do
        harnessConfigPath (path "/Users/test")
            `shouldBe` path "/Users/test/.haskell-agent/config.json"

    it "defaults when the global config is missing" $
        withTempDir "agent-config-" \home ->
            loadHarnessConfig home `shouldReturn` Right defaultHarnessConfig

    it "loads MCP servers with defaults and deterministic map ordering" $
        withTempDir "agent-config-" \home -> do
            writeConfig home
                "{\"version\":1,\"mcpServers\":{\"zeta\":{\"command\":\"z\"},\"alpha\":{\"command\":\"a\",\"args\":[\"one\"],\"cwd\":\"/tmp\",\"env\":{\"TOKEN\":\"secret\"},\"enabled\":false,\"startupTimeoutSeconds\":12,\"requestTimeoutSeconds\":34}}}"
            result <- loadHarnessConfig home
            case result of
                Left err -> expectationFailure (show err)
                Right config -> do
                    config.configMcpInitStrategy `shouldBe` McpInitAuto
                    Map.keys config.configMcpServers
                        `shouldBe` ["alpha", "zeta"]
                    Map.lookup "alpha" config.configMcpServers
                        `shouldBe` Just McpServerConfig
                            { mcpEnabled = False
                            , mcpCommand = "a"
                            , mcpArgs = ["one"]
                            , mcpCwd = Just "/tmp"
                            , mcpEnv = Map.fromList [("TOKEN", "secret")]
                            , mcpStartupTimeoutSeconds = 12
                            , mcpRequestTimeoutSeconds = 34
                            }
                    Map.lookup "zeta" config.configMcpServers
                        `shouldBe` Just McpServerConfig
                            { mcpEnabled = True
                            , mcpCommand = "z"
                            , mcpArgs = []
                            , mcpCwd = Nothing
                            , mcpEnv = Map.empty
                            , mcpStartupTimeoutSeconds = 30
                            , mcpRequestTimeoutSeconds = 60
                            }

    it "loads the MCP initialization strategy" $
        withTempDir "agent-config-" \home -> do
            writeConfig home
                "{\"mcpInitStrategy\":\"progressive\"}"
            result <- loadHarnessConfig home
            fmap (.configMcpInitStrategy) result
                `shouldBe` Right McpInitProgressive

    it "loads the concurrent agent limit" $
        withTempDir "agent-config-" \home -> do
            writeConfig home "{\"maxConcurrentAgents\":48}"
            result <- loadHarnessConfig home
            fmap (.configMaxConcurrentAgents) result
                `shouldBe` Right (Just 48)

    it "rejects a non-positive concurrent agent limit" $
        withTempDir "agent-config-" \home -> do
            writeConfig home "{\"maxConcurrentAgents\":0}"
            loadHarnessConfig home
                `shouldReturn` Left "maxConcurrentAgents must be at least 1"

    it "rejects unknown MCP initialization strategies" $
        withTempDir "agent-config-" \home -> do
            writeConfig home
                "{\"mcpInitStrategy\":\"eventually\"}"
            result <- loadHarnessConfig home
            result `shouldSatisfy` \case
                Left err ->
                    "unknown MCP initialization strategy"
                        `Text.isInfixOf` err
                Right _ -> False

    describe "useProgressiveMcp" do
        it "uses progressive startup for interactive auto mode" $
            useProgressiveMcp McpInitAuto False `shouldBe` True

        it "uses blocking startup for one-shot auto mode" $
            useProgressiveMcp McpInitAuto True `shouldBe` False

        it "honors explicit overrides" do
            useProgressiveMcp McpInitProgressive True `shouldBe` True
            useProgressiveMcp McpInitBlocking False `shouldBe` False

    it "rejects unsupported versions" $
        withTempDir "agent-config-" \home -> do
            writeConfig home "{\"version\":2}"
            loadHarnessConfig home
                `shouldReturn` Left
                    "Unsupported harness config version 2; expected 1"

    it "rejects empty commands and non-positive timeouts" $
        withTempDir "agent-config-" \home -> do
            writeConfig home
                "{\"mcpServers\":{\"broken\":{\"command\":\" \"}}}"
            loadHarnessConfig home
                `shouldReturn` Left "MCP server 'broken' has an empty command"

            writeConfig home
                "{\"mcpServers\":{\"broken\":{\"command\":\"ok\",\"startupTimeoutSeconds\":0}}}"
            loadHarnessConfig home
                `shouldReturn` Left
                    "MCP server 'broken' startupTimeoutSeconds must be positive"

    it "reports malformed JSON with the config path" $
        withTempDir "agent-config-" \home -> do
            writeConfig home "{not-json"
            result <- loadHarnessConfig home
            result `shouldSatisfy` \case
                Left err ->
                    "Invalid " `Text.isPrefixOf` err
                        && "config.json" `Text.isInfixOf` err
                Right _ -> False

    it "atomically saves a private, loadable MCP configuration" $
        withTempDir "agent-config-" \home -> do
            let server = McpServerConfig
                    { mcpEnabled = True
                    , mcpCommand = "nix"
                    , mcpArgs = ["run", "/tmp/seo-mcp"]
                    , mcpCwd = Just "/tmp"
                    , mcpEnv = Map.fromList [("TOKEN", "secret")]
                    , mcpStartupTimeoutSeconds = 90
                    , mcpRequestTimeoutSeconds = 45
                    }
                config = defaultHarnessConfig
                    { configMcpServers = Map.singleton "seo-mcp" server
                    , configMaxConcurrentAgents = Just 48
                    }
            saveHarnessConfig home config `shouldReturn` Right ()
            loadHarnessConfig home `shouldReturn` Right config
            status <- getFileStatus (filePath (harnessConfigPath home))
            fileMode status `shouldBe` 0o100600

    it "validates before saving" $
        withTempDir "agent-config-" \home -> do
            let broken = defaultHarnessConfig
                    { configMcpServers =
                        Map.singleton "broken" McpServerConfig
                            { mcpEnabled = True
                            , mcpCommand = ""
                            , mcpArgs = []
                            , mcpCwd = Nothing
                            , mcpEnv = Map.empty
                            , mcpStartupTimeoutSeconds = 30
                            , mcpRequestTimeoutSeconds = 60
                            }
                    }
            saveHarnessConfig home broken
                `shouldReturn`
                    Left "MCP server 'broken' has an empty command"

writeConfig :: OsPath -> LBS.ByteString -> IO ()
writeConfig home bytes = do
    let file = harnessConfigPath home
    createDirectoryIfMissing True (takeDirectory file)
    LBS.writeFile (filePath file) bytes

withTempDir :: String -> (OsPath -> IO a) -> IO a
withTempDir prefix action = do
    tmp <- getTemporaryDirectory
    bracket
        (mkdtemp (filePath tmp FilePath.</> prefix))
        Directory.removeDirectoryRecursive
        (action . path)

path :: FilePath -> OsPath
path = unsafeEncodeUtf

filePath :: OsPath -> FilePath
filePath value = either (error . show) id (decodeUtf value)
