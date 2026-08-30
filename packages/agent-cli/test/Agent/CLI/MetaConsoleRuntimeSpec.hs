module Agent.CLI.MetaConsoleRuntimeSpec (spec) where

import Agent.CLI.Config
    ( HarnessConfig(..)
    , LspConfig(..)
    , LspServerConfig(..)
    , McpOAuthConfig(..)
    , McpServerConfig(..)
    , defaultHarnessConfig
    )
import Agent.CLI.MetaConsole
    ( MetaAction(..)
    , MetaLspServer(..)
    , MetaMcpServer(..)
    )
import Agent.CLI.Runtime.MetaConsole
    ( MetaSecretValue(..)
    , applyMetaConfigActions
    )
import Agent.MCP (McpProtocolPreference(..))
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    , shouldSatisfy
    )

spec :: Spec
spec = describe "Meta Console host executor" do
    it "updates public MCP fields while preserving env and OAuth credentials" do
        let existing = McpServerConfig
                { mcpEnabled = True
                , mcpUrl = Just "https://old.example/mcp"
                , mcpCommand = ""
                , mcpArgs = []
                , mcpCwd = Nothing
                , mcpEnv = Map.singleton "TOKEN" "private"
                , mcpStartupTimeoutSeconds = 30
                , mcpRequestTimeoutSeconds = 60
                , mcpOAuth = Just McpOAuthConfig
                    { mcpOAuthClientId = Just "client"
                    , mcpOAuthClientSecret = Just "client-secret"
                    , mcpOAuthClientIdMetadataUrl = Nothing
                    , mcpOAuthScopes = ["old"]
                    }
                , mcpProtocol = McpProtocolAuto
                }
            config = defaultHarnessConfig
                { configMcpServers = Map.singleton "docs" existing
                }
            proposed = MetaMcpServer
                { metaMcpName = "docs"
                , metaMcpEnabled = False
                , metaMcpUrl = Just "https://new.example/mcp"
                , metaMcpCommand = Nothing
                , metaMcpArgs = []
                , metaMcpCwd = Nothing
                , metaMcpStartupTimeoutSeconds = 10
                , metaMcpRequestTimeoutSeconds = 20
                , metaMcpProtocol = McpProtocolModern
                , metaMcpOAuthScopes = Just ["read"]
                }
        applyMetaConfigActions [] [MetaUpsertMcp proposed] config
            `shouldSatisfy` \case
                Left _ -> False
                Right updated ->
                    case Map.lookup "docs" updated.configMcpServers of
                        Nothing -> False
                        Just server ->
                            server.mcpEnv == Map.singleton "TOKEN" "private"
                                && server.mcpOAuth
                                    == Just McpOAuthConfig
                                        { mcpOAuthClientId = Just "client"
                                        , mcpOAuthClientSecret =
                                            Just "client-secret"
                                        , mcpOAuthClientIdMetadataUrl = Nothing
                                        , mcpOAuthScopes = ["read"]
                                        }
                                && server.mcpUrl
                                    == Just "https://new.example/mcp"
                                && not server.mcpEnabled

    it "injects trusted secret input without putting a value in the action" do
        let server = McpServerConfig
                { mcpEnabled = True
                , mcpUrl = Nothing
                , mcpCommand = "docs-mcp"
                , mcpArgs = []
                , mcpCwd = Nothing
                , mcpEnv = Map.empty
                , mcpStartupTimeoutSeconds = 30
                , mcpRequestTimeoutSeconds = 60
                , mcpOAuth = Nothing
                , mcpProtocol = McpProtocolAuto
                }
            config = defaultHarnessConfig
                { configMcpServers = Map.singleton "docs" server
                }
            secret = MetaMcpSecretValue "docs" "API_TOKEN" "private-value"
        applyMetaConfigActions
            [secret]
            [MetaSetMcpSecretEnv "docs" "API_TOKEN"]
            config
            `shouldSatisfy` \case
                Left _ -> False
                Right updated ->
                    (Map.lookup "docs" updated.configMcpServers
                        >>= Map.lookup "API_TOKEN" . (.mcpEnv))
                        == Just "private-value"
        Text.pack (show secret)
            `shouldSatisfy` (not . Text.isInfixOf "private-value")

    it "never writes a redaction marker over an existing private MCP URL" do
        let existing = McpServerConfig
                { mcpEnabled = True
                , mcpUrl =
                    Just "https://user:password@example.com/mcp?token=private"
                , mcpCommand = ""
                , mcpArgs = []
                , mcpCwd = Nothing
                , mcpEnv = Map.empty
                , mcpStartupTimeoutSeconds = 30
                , mcpRequestTimeoutSeconds = 60
                , mcpOAuth = Nothing
                , mcpProtocol = McpProtocolAuto
                }
            config = defaultHarnessConfig
                { configMcpServers = Map.singleton "docs" existing
                }
            proposed = MetaMcpServer
                { metaMcpName = "docs"
                , metaMcpEnabled = True
                , metaMcpUrl =
                    Just "https://<redacted>@example.com/mcp?<redacted>"
                , metaMcpCommand = Nothing
                , metaMcpArgs = []
                , metaMcpCwd = Nothing
                , metaMcpStartupTimeoutSeconds = 15
                , metaMcpRequestTimeoutSeconds = 30
                , metaMcpProtocol = McpProtocolAuto
                , metaMcpOAuthScopes = Nothing
                }
        applyMetaConfigActions [] [MetaUpsertMcp proposed] config
            `shouldSatisfy` \case
                Left _ -> False
                Right updated ->
                    (Map.lookup "docs" updated.configMcpServers
                        >>= (.mcpUrl))
                        == existing.mcpUrl

    it "preserves opaque LSP env/settings fields during a public update" do
        let existing = LspServerConfig
                { lspCommand = "old-hls"
                , lspArgs = []
                , lspEnv = Map.singleton "HLS_TOKEN" "private"
                , lspExtensionToLanguage = Map.singleton "hs" "haskell"
                , lspInitializationOptions = Nothing
                , lspSettings = Nothing
                , lspWorkspaceFolder = Nothing
                , lspStartupTimeoutMilliseconds = 15000
                , lspShutdownTimeoutMilliseconds = 5000
                }
            config = defaultHarnessConfig
                { configLsp =
                    defaultHarnessConfig.configLsp
                        { lspServers = Map.singleton "haskell" existing
                        }
                }
            proposed = MetaLspServer
                { metaLspName = "haskell"
                , metaLspCommand = "haskell-language-server-wrapper"
                , metaLspArgs = ["--lsp"]
                , metaLspExtensionToLanguage =
                    Map.singleton "hs" "haskell"
                , metaLspWorkspaceFolder = Just "/workspace"
                , metaLspStartupTimeoutMilliseconds = 10000
                , metaLspShutdownTimeoutMilliseconds = 3000
                }
        applyMetaConfigActions [] [MetaUpsertLsp proposed] config
            `shouldSatisfy` \case
                Left _ -> False
                Right updated ->
                    case Map.lookup "haskell" updated.configLsp.lspServers of
                        Nothing -> False
                        Just server ->
                            server.lspEnv
                                == Map.singleton "HLS_TOKEN" "private"
                                && server.lspCommand
                                    == "haskell-language-server-wrapper"

    it "rejects an update targeting a missing server" do
        applyMetaConfigActions
            []
            [MetaSetMcpEnabled "missing" False]
            defaultHarnessConfig
            `shouldBe` Left "MCP server 'missing' is not configured"
