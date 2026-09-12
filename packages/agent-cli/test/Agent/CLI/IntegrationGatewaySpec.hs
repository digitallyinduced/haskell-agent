module Agent.CLI.IntegrationGatewaySpec (spec) where

import Agent.Runtime.GatewayClient (GatewayCredential(..))
import Agent.CLI.IntegrationGateway
    (availableIntegrationServerName, gatewayIntegrationMcpConfig, integrationEndpointServers)
import Agent.Integration.API (IntegrationEndpoint(..))
import Agent.MCP (McpServerConfig(..), McpToolServer(..))
import Test.Hspec

spec :: Spec
spec = describe "generic gateway integrations" do
    it "gives explicit device tools the canonical namespace and excludes only reserved remote names" do
        let config = gatewayIntegrationMcpConfig
                (GatewayCredential "https://gateway.example" "wss://gateway.example/ws" "token")
            endpoint = McpToolServer
                { toolServerInitialize = ioError (userError "not used by namespace test")
                , toolServerListTools = pure (Right [])
                , toolServerCallTool = \_ -> ioError (userError "not used by namespace test")
                , toolServerSubscribe = \_ -> pure (pure ())
                }
            (remote, local) = integrationEndpointServers []
                (CombinedOverlayIntegrationEndpoint config ["device_lookup"] endpoint)
        map (.mcpServerName) remote `shouldBe` ["integrations-2"]
        map (.mcpServerExcludedTools) remote `shouldBe` [["device_lookup"]]
        map fst local `shouldBe` ["integrations"]
        let (plainRemote, plainLocal) = integrationEndpointServers []
                (CombinedIntegrationEndpoint config endpoint)
        map (.mcpServerName) plainRemote `shouldBe` ["integrations"]
        map (.mcpServerExcludedTools) plainRemote `shouldBe` [[]]
        map fst plainLocal `shouldBe` ["integrations-2"]
        let (collisionRemote, collisionLocal) = integrationEndpointServers ["integrations"]
                (CombinedOverlayIntegrationEndpoint config ["device_lookup"] endpoint)
        map (.mcpServerName) collisionRemote `shouldBe` ["integrations-3"]
        map fst collisionLocal `shouldBe` ["integrations-2"]

    it "uses the aggregate endpoint and keeps credentials out of URLs and Show" do
        let config = gatewayIntegrationMcpConfig GatewayCredential
                { gatewayBaseUrl = "https://gateway.example/"
                , gatewayWebSocketUrl = "wss://gateway.example/ws"
                , gatewayAccessToken = "private-test-token"
                }
        config.mcpServerName `shouldBe` "integrations"
        config.mcpServerUrl `shouldBe` Just "https://gateway.example/mcp/integrations"
        config.mcpServerEnv `shouldBe` [("MCP_ACCESS_TOKEN", "private-test-token")]
        show config `shouldNotContain` "private-test-token"
        config.mcpServerCommand `shouldBe` ""

    it "keeps the built-in endpoint when configured server names collide" do
        availableIntegrationServerName
            ["integrations", "integrations-2", "another-server"]
            `shouldBe` "integrations-3"
