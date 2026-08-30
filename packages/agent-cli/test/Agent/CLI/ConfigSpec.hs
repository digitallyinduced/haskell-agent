module Agent.CLI.ConfigSpec (spec) where

import Agent.CLI.Config
import Control.Exception.Safe (bracket)
import Agent.MCP (McpProtocolPreference(..))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
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

    it "defaults when the global config is empty" $
        withTempDir "agent-config-" \home -> do
            writeConfig home ""
            loadHarnessConfig home `shouldReturn` Right defaultHarnessConfig

    it "defaults when the global config is only whitespace" $
        withTempDir "agent-config-" \home -> do
            writeConfig home " \n\t"
            loadHarnessConfig home `shouldReturn` Right defaultHarnessConfig

    it "updates typed configuration without discarding unrelated fields" $
        withTempDir "agent-config-" \home -> do
            let original =
                    defaultHarnessConfig
                        { configMcpServers =
                            Map.singleton "kept" McpServerConfig
                                { mcpEnabled = True
                                , mcpUrl = Just "https://example.test/mcp"
                                , mcpCommand = ""
                                , mcpArgs = []
                                , mcpCwd = Nothing
                                , mcpEnv = Map.singleton "TOKEN" "secret"
                                , mcpStartupTimeoutSeconds = 30
                                , mcpRequestTimeoutSeconds = 60
                                , mcpOAuth = Nothing
                                , mcpProtocol = McpProtocolAuto
                                }
                        }
            saveHarnessConfig home original `shouldReturn` Right ()
            result <- updateHarnessConfig home \config ->
                Right config { configMaxConcurrentAgents = Just 12 }
            fmap (.configMaxConcurrentAgents) result `shouldBe` Right (Just 12)
            loaded <- loadHarnessConfig home
            fmap
                (Map.lookup "kept" . (.configMcpServers))
                loaded
                `shouldBe` Right (Map.lookup "kept" original.configMcpServers)

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
                            , mcpUrl = Nothing
                            , mcpCommand = "a"
                            , mcpArgs = ["one"]
                            , mcpCwd = Just "/tmp"
                            , mcpEnv = Map.fromList [("TOKEN", "secret")]
                            , mcpStartupTimeoutSeconds = 12
                            , mcpRequestTimeoutSeconds = 34
                            , mcpOAuth = Nothing
                            , mcpProtocol = McpProtocolAuto
                            }
                    Map.lookup "zeta" config.configMcpServers
                        `shouldBe` Just McpServerConfig
                            { mcpEnabled = True
                            , mcpUrl = Nothing
                            , mcpCommand = "z"
                            , mcpArgs = []
                            , mcpCwd = Nothing
                            , mcpEnv = Map.empty
                            , mcpStartupTimeoutSeconds = 30
                            , mcpRequestTimeoutSeconds = 60
                            , mcpOAuth = Nothing
                            , mcpProtocol = McpProtocolAuto
                            }

    it "loads remote MCP servers by URL" $
        withTempDir "agent-config-" \home -> do
            writeConfig home
                "{\"mcpServers\":{\"remote\":{\"url\":\"https://example.test/mcp\"}}}"
            result <- loadHarnessConfig home
            fmap (Map.lookup "remote" . (.configMcpServers)) result
                `shouldSatisfy` \case
                    Right (Just server) -> server.mcpUrl == Just "https://example.test/mcp"
                    _ -> False

    it "loads the optional per-server oauth object" $
        withTempDir "agent-config-" \home -> do
            writeConfig home
                "{\"mcpServers\":{\"remote\":{\"url\":\"https://example.test/mcp\",\"oauth\":{\"clientId\":\"cid\",\"clientSecret\":\"top-secret\",\"clientIdMetadataUrl\":\"https://app.example/client.json\",\"scopes\":[\"files:read\"]}}}}"
            result <- loadHarnessConfig home
            case result of
                Left err -> expectationFailure (Text.unpack err)
                Right config -> do
                    let oauth = Map.lookup "remote" config.configMcpServers >>= (.mcpOAuth)
                    oauth `shouldBe` Just McpOAuthConfig
                        { mcpOAuthClientId = Just "cid"
                        , mcpOAuthClientSecret = Just "top-secret"
                        , mcpOAuthClientIdMetadataUrl = Just "https://app.example/client.json"
                        , mcpOAuthScopes = ["files:read"]
                        }
                    show oauth `shouldSatisfy` (not . Text.isInfixOf "top-secret" . Text.pack)
                    show config `shouldSatisfy` (not . Text.isInfixOf "top-secret" . Text.pack)
                    saveHarnessConfig home config `shouldReturn` Right ()
                    loadHarnessConfig home `shouldReturn` Right config

    it "treats a missing oauth object and partial oauth keys as optional" $
        withTempDir "agent-config-" \home -> do
            writeConfig home
                "{\"mcpServers\":{\"remote\":{\"url\":\"https://example.test/mcp\",\"oauth\":{\"scopes\":[\"a\",\"b\"]}}}}"
            result <- loadHarnessConfig home
            fmap (\config -> Map.lookup "remote" config.configMcpServers >>= (.mcpOAuth)) result
                `shouldBe` Right (Just (McpOAuthConfig Nothing Nothing Nothing ["a", "b"]))

    it "validates the oauth object" $
        withTempDir "agent-config-" \home -> do
            writeConfig home
                "{\"mcpServers\":{\"remote\":{\"url\":\"https://example.test/mcp\",\"oauth\":{\"clientSecret\":\"s\"}}}}"
            loadHarnessConfig home
                `shouldReturn` Left "MCP server 'remote' oauth.clientSecret requires oauth.clientId"

            writeConfig home
                "{\"mcpServers\":{\"remote\":{\"url\":\"https://example.test/mcp\",\"oauth\":{\"clientIdMetadataUrl\":\"http://app.example/client.json\"}}}}"
            loadHarnessConfig home
                `shouldReturn` Left "MCP server 'remote' oauth.clientIdMetadataUrl must be an https URL with a path"

            writeConfig home
                "{\"mcpServers\":{\"remote\":{\"url\":\"https://example.test/mcp\",\"oauth\":{\"clientIdMetadataUrl\":\"https://app.example\"}}}}"
            loadHarnessConfig home
                `shouldReturn` Left "MCP server 'remote' oauth.clientIdMetadataUrl must be an https URL with a path"

            writeConfig home
                "{\"mcpServers\":{\"local\":{\"command\":\"srv\",\"oauth\":{\"clientId\":\"c\"}}}}"
            loadHarnessConfig home
                `shouldReturn` Left "MCP server 'local' oauth requires url"

            writeConfig home
                "{\"mcpServers\":{\"remote\":{\"url\":\"https://example.test/mcp\",\"oauth\":{\"clientId\":\" \"}}}}"
            loadHarnessConfig home
                `shouldReturn` Left "MCP server 'remote' oauth.clientId must not be empty"

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

    it "decodes LSP maps and retains opaque JSON options" $
        withTempDir "agent-config-" \home -> do
            writeConfig home
                "{\"lsp\":{\"enabled\":true,\"servers\":{\"hls\":{\"command\":\"haskell-language-server-wrapper\",\"args\":[\"--lsp\"],\"env\":{\"HLS_LOG\":\"warn\"},\"extensionToLanguage\":{\".hs\":\"haskell\"},\"initializationOptions\":{\"haskell\":{\"formattingProvider\":\"fourmolu\"}},\"settings\":[true,3],\"workspaceFolder\":\"src\",\"startupTimeoutMilliseconds\":1234,\"shutdownTimeoutMilliseconds\":5678}}}}"
            result <- loadHarnessConfig home
            case result of
                Left err -> expectationFailure (Text.unpack err)
                Right config -> do
                    case Map.lookup "hls" config.configLsp.lspServers of
                        Nothing -> expectationFailure "missing HLS config"
                        Just server -> do
                            server.lspArgs `shouldBe` ["--lsp"]
                            server.lspEnv
                                `shouldBe` Map.singleton "HLS_LOG" "warn"
                            server.lspExtensionToLanguage
                                `shouldBe` Map.singleton ".hs" "haskell"
                            server.lspInitializationOptions
                                `shouldSatisfy` isJust
                            server.lspSettings `shouldSatisfy` isJust
                            server.lspWorkspaceFolder `shouldBe` Just "src"
                            server.lspStartupTimeoutMilliseconds
                                `shouldBe` 1234
                            server.lspShutdownTimeoutMilliseconds
                                `shouldBe` 5678
                    saveHarnessConfig home config `shouldReturn` Right ()
                    loadHarnessConfig home `shouldReturn` Right config

    it "rejects non-text values in configured environment maps" $
        withTempDir "agent-config-" \home -> do
            writeConfig home
                "{\"mcpServers\":{\"broken\":{\"command\":\"ok\",\"env\":{\"TOKEN\":1}}}}"
            result <- loadHarnessConfig home
            result `shouldSatisfy` \case
                Left err -> "Invalid " `Text.isPrefixOf` err
                Right _ -> False

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
                `shouldReturn` Left "MCP server 'broken' must configure exactly one of url or command"

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
                    , mcpUrl = Nothing
                    , mcpCommand = "nix"
                    , mcpArgs = ["run", "/tmp/seo-mcp"]
                    , mcpCwd = Just "/tmp"
                    , mcpEnv = Map.fromList [("TOKEN", "secret")]
                    , mcpStartupTimeoutSeconds = 90
                    , mcpRequestTimeoutSeconds = 45
                    , mcpOAuth = Nothing
                    , mcpProtocol = McpProtocolAuto
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
                            , mcpUrl = Nothing
                            , mcpCommand = ""
                            , mcpArgs = []
                            , mcpCwd = Nothing
                            , mcpEnv = Map.empty
                            , mcpStartupTimeoutSeconds = 30
                            , mcpRequestTimeoutSeconds = 60
                            , mcpOAuth = Nothing
                            , mcpProtocol = McpProtocolAuto
                            }
                    }
            saveHarnessConfig home broken
                `shouldReturn`
                    Left "MCP server 'broken' must configure exactly one of url or command"

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
