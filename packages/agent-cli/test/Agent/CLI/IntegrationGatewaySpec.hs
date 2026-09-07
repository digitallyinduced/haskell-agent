module Agent.CLI.IntegrationGatewaySpec (spec) where

import Agent.CLI.GatewayClient (GatewayCredential(..))
import Agent.CLI.IntegrationGateway (gatewayIntegrationMcpConfig)
import Agent.MCP (McpServerConfig(..))
import Test.Hspec

spec :: Spec
spec = describe "generic gateway integrations" do
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
