module Agent.CLI.IntegrationGatewaySpec (spec) where

import Agent.Runtime.GatewayClient (GatewayCredential(..))
import Agent.CLI.IntegrationGateway
    ( availableIntegrationServerName
    , gatewayIntegrationMcpConfig
    , integrationEndpointServers
    , sessionIntegrationAuthority
    , prepareSessionIntegrationRuntime
    )
import Agent.Integration.API
    ( IntegrationAuthority(..), IntegrationEndpoint(..), IntegrationRuntime(..)
    , newIntegrationSupervisor, closeIntegrationSupervisor, emptyIntegrationProvider )
import Agent.MCP (McpServerConfig(..), McpToolServer(..))
import Agent.Tools.Types (defaultToolEnv)
import Control.Concurrent.Async (wait, withAsync)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception.Safe (bracket)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Maybe (isJust, isNothing)
import System.OsPath (unsafeEncodeUtf)
import Test.Hspec

spec :: Spec
spec = describe "generic gateway integrations" do
    it "reuses an integration prepared during independent workspace startup" do
        toolEnv <- defaultToolEnv (unsafeEncodeUtf ".")
        started <- newEmptyMVar
        release <- newEmptyMVar
        acquired <- newIORef (0 :: Int)
        closed <- newIORef (0 :: Int)
        let provider env = do
                modifyIORef' acquired (+ 1)
                putMVar started ()
                takeMVar release
                fmap (fmap (\runtime -> runtime
                    { closeIntegrationRuntime = modifyIORef' closed (+ 1) }))
                    (emptyIntegrationProvider env)
        bracket (newIntegrationSupervisor provider toolEnv) closeIntegrationSupervisor \supervisor ->
            withAsync (prepareSessionIntegrationRuntime True Nothing supervisor) \worker -> do
                takeMVar started
                -- The host can prepare a workspace while acquisition is blocked.
                readIORef acquired `shouldReturn` 1
                putMVar release ()
                (fmap (fmap isJust) (wait worker)) `shouldReturn` Right True
                (fmap (fmap isJust) (prepareSessionIntegrationRuntime True Nothing supervisor))
                    `shouldReturn` Right True
                readIORef acquired `shouldReturn` 1
                readIORef closed `shouldReturn` 0
        readIORef closed `shouldReturn` 1

    it "does not acquire a withheld integration during early preparation" do
        toolEnv <- defaultToolEnv (unsafeEncodeUtf ".")
        acquired <- newIORef (0 :: Int)
        let provider env = modifyIORef' acquired (+ 1) >> emptyIntegrationProvider env
            credential = GatewayCredential "https://gateway.example" "wss://gateway.example/ws" "token"
        bracket (newIntegrationSupervisor provider toolEnv) closeIntegrationSupervisor \supervisor -> do
            (fmap (fmap isNothing) (prepareSessionIntegrationRuntime False (Just credential) supervisor))
                `shouldReturn` Right True
            readIORef acquired `shouldReturn` 0

    it "withholds a connected gateway's integrations without falling back to local ones" do
        let credential =
                GatewayCredential "https://gateway.example" "wss://gateway.example/ws" "token"
        sessionIntegrationAuthority True (Just credential)
            `shouldBe` Just (OrganizationIntegrationAuthority (gatewayIntegrationMcpConfig credential))
        sessionIntegrationAuthority False (Just credential) `shouldBe` Nothing
        sessionIntegrationAuthority True Nothing `shouldBe` Just LocalIntegrationAuthority
        sessionIntegrationAuthority False Nothing `shouldBe` Just LocalIntegrationAuthority

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
