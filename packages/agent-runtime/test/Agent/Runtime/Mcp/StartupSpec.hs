module Agent.Runtime.Mcp.StartupSpec (spec) where

import qualified Agent.MCP as MCP
import qualified System.OsPath as OsPath
import qualified Agent.MCP.Types as MCP
import Agent.Runtime.Config
import Agent.Runtime.Mcp.Startup
import Agent.Runtime.McpOAuthStore (mcpOAuthStorePath)
import Agent.OsPath (unsafeToFilePath)
import Control.Concurrent.Async (cancel, poll, wait, withAsync)
import Control.Concurrent.MVar
import Control.Exception.Safe (bracket, finally)
import Control.Monad (forM_)
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Data.Text (Text)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "MCP startup" do
    it "maps transport configuration and preserves explicit capability settings" do
        let configured = testConfig
                { mcpCwd = Just "/workspace/override"
                , mcpEnv = Map.fromList [("B", "two"), ("A", "one")]
                , mcpRoots = True
                , mcpSampling = True
                , mcpLogLevel = Just MCP.McpLogWarning
                , mcpStartupTimeoutSeconds = 12
                , mcpRequestTimeoutSeconds = 34
                }
            (servers, _) = resolveMcpConfiguration (configuration [("test", configured)])
        case servers of
            [server] -> do
                server.mcpServerName `shouldBe` "test"
                server.mcpServerCommand `shouldBe` "test-server"
                server.mcpServerArgs `shouldBe` ["--test"]
                server.mcpServerCwd `shouldBe` Just "/workspace/override"
                server.mcpServerEnv `shouldBe` [("A", "one"), ("B", "two")]
                server.mcpServerRootsEnabled `shouldBe` True
                server.mcpServerSamplingEnabled `shouldBe` True
                server.mcpServerLogLevel `shouldBe` Just MCP.McpLogWarning
                server.mcpServerStartupTimeoutSeconds `shouldBe` 12
                server.mcpServerRequestTimeoutSeconds `shouldBe` 34
                server.mcpServerProtocol `shouldBe` MCP.McpProtocolModern
            _ -> expectationFailure "expected one mapped MCP server"

    it "filters disabled and host-command servers without disabling remote MCP" do
        let request = (configuration
                [ ("command", testConfig)
                , ("disabled", testConfig { mcpEnabled = False })
                , ("remote", testConfig { mcpUrl = Just "https://example.test/mcp" })
                ]) { mcpConfigHostExtensions = False }
        map (.mcpServerName) (fst (resolveMcpConfiguration request))
            `shouldBe` ["remote"]
        null (fst (resolveMcpConfiguration request { mcpConfigToolsEnabled = False }))
            `shouldBe` True

    it "keeps connection credentials separate from legacy OAuth token files" do
        let url = "https://example.test/mcp"
            remote = testConfig { mcpUrl = Just url }
            connection = remote
                { mcpConnectionId = Just "connection"
                , mcpConnectionGeneration = Just "generation"
                , mcpDisplayName = Just "Display"
                }
            request = configuration
                [ ("connection", connection)
                , ("explicit", remote { mcpEnv = Map.singleton "MCP_OAUTH_TOKEN_FILE" "/custom/token" })
                , ("legacy", remote)
                ]
        case fst (resolveMcpConfiguration request) of
            [protected, explicit, legacy] -> do
                protected.mcpServerConnection `shouldBe`
                    Just (MCP.McpConnectionIdentity "connection" (Just "generation") (Just "Display"))
                lookup "MCP_OAUTH_TOKEN_FILE" protected.mcpServerEnv `shouldBe` Nothing
                lookup "MCP_OAUTH_TOKEN_FILE" explicit.mcpServerEnv `shouldBe` Just "/custom/token"
                lookup "MCP_OAUTH_TOKEN_FILE" legacy.mcpServerEnv `shouldBe`
                    Just (unsafeToFilePath (mcpOAuthStorePath request.mcpConfigHome url))
                legacy.mcpServerCwd `shouldBe` Just "/workspace"
            _ -> expectationFailure "expected three mapped remote MCP servers"

    it "resolves interactive and one-shot initialization policy" do
        forM_ [(McpInitAuto, False, True), (McpInitAuto, True, False),
               (McpInitProgressive, True, True), (McpInitBlocking, False, False)] $
            \(strategy, oneShot, expected) -> do
                let request = (configuration [])
                        { mcpConfigHarness = defaultHarnessConfig { configMcpInitStrategy = strategy }
                        , mcpConfigOneShot = oneShot
                        }
                snd (resolveMcpConfiguration request) `shouldBe` expected

    it "reports in-memory endpoints in the complete configuration" do
        let request = (memoryRequest False testServer)
                { mcpStartupTransportServers = [integrationsMcpConfig "remote"] }
        map (.mcpServerName) (startupServerConfigs request) `shouldBe` ["remote", "memory"]

    it "waits for the catalog in blocking mode and releases only the session lease" $ within $
        bracket MCP.newMcpSupervisor MCP.closeMcpSupervisor \supervisor -> do
            started <- newEmptyMVar
            unblock <- newEmptyMVar
            live <- newIORef False
            reports <- newIORef ([] :: [[Text]])
            let endpoint = testServer
                    { MCP.toolServerListTools =
                        putMVar started () >> takeMVar unblock >> pure (Right [])
                    }
                hooks = quietHooks
                    { mcpInstallHostHooks = writeIORef live True
                    , mcpClearHostHooks = writeIORef live False
                    , mcpReportBlocking = \names -> atomicModifyIORef' reports (\values -> (values <> [names], ()))
                    , mcpReportProgressive = \_ -> expectationFailure "blocking startup used progressive reports"
                    }
            withAsync
                (acquireMcpStartup supervisor (memoryRequest False endpoint) hooks
                    \fleet close -> pure (fleet, close)) \worker -> do
                takeMVar started
                isNothing <$> poll worker `shouldReturn` True
                readIORef live `shouldReturn` True
                putMVar unblock ()
                (fleet, close) <- wait worker
                readIORef reports >>= (`shouldSatisfy` any (elem "memory"))
                MCP.mcpFleetInstructions fleet `shouldReturn` [("memory", "Test instructions")]
                leaseCount supervisor `shouldReturn` 1
                close
                leaseCount supervisor `shouldReturn` 0
                readIORef live `shouldReturn` False
                -- A session lease does not own the process-wide fleet itself.
                MCP.mcpFleetInstructions fleet `shouldReturn` [("memory", "Test instructions")]

    it "returns a progressive fleet before its in-memory catalog settles" $ within $
        bracket MCP.newMcpSupervisor MCP.closeMcpSupervisor \supervisor -> do
            started <- newEmptyMVar
            unblock <- newEmptyMVar
            reported <- newEmptyMVar
            let endpoint = testServer
                    { MCP.toolServerListTools =
                        putMVar started () >> takeMVar unblock >> pure (Right [])
                    }
                hooks = quietHooks
                    { mcpReportBlocking = \_ -> expectationFailure "progressive startup used blocking reports"
                    , mcpReportProgressive = \statuses ->
                        if any (\status -> status.mcpStatusName == "memory") statuses
                            then do
                                _ <- tryPutMVar reported ()
                                pure ()
                            else pure ()
                    }
            close <- acquireMcpStartup supervisor (memoryRequest True endpoint) hooks
                \_ release -> pure release
            takeMVar started
            takeMVar reported
            leaseCount supervisor `shouldReturn` 1
            putMVar unblock ()
            close
            leaseCount supervisor `shouldReturn` 0

    it "supports progressive acquisition without in-memory endpoints" $ within $
        bracket MCP.newMcpSupervisor MCP.closeMcpSupervisor \supervisor -> do
            let request = emptyRequest { mcpStartupProgressive = True }
                hooks = quietHooks
                    { mcpReportBlocking = \_ -> expectationFailure "progressive startup used blocking reports" }
            close <- acquireMcpStartup supervisor request hooks
                \fleet release -> do
                    MCP.mcpFleetStatuses fleet `shouldReturn` []
                    pure release
            state <- readMVar supervisor.supervisorState
            map (.supervisorEntryProgressive) state.supervisorEntries `shouldBe` [True]
            close
            leaseCount supervisor `shouldReturn` 0

    it "clears partially installed hooks when hook installation fails" $
        bracket MCP.newMcpSupervisor MCP.closeMcpSupervisor \supervisor -> do
            live <- newIORef False
            let hooks = quietHooks
                    { mcpInstallHostHooks = writeIORef live True >> ioError (userError "install failed")
                    , mcpClearHostHooks = writeIORef live False
                    }
            acquireMcpStartup supervisor emptyRequest hooks (\_ _ -> pure ())
                `shouldThrow` anyIOException
            readIORef live `shouldReturn` False
            leaseCount supervisor `shouldReturn` 0

    it "clears hooks when fleet acquisition fails" $
        bracket MCP.newMcpSupervisor MCP.closeMcpSupervisor \supervisor -> do
            MCP.closeMcpSupervisor supervisor
            live <- newIORef False
            let hooks = quietHooks
                    { mcpInstallHostHooks = writeIORef live True
                    , mcpClearHostHooks = writeIORef live False
                    }
            acquireMcpStartup supervisor emptyRequest hooks (\_ _ -> pure ())
                `shouldThrow` (\(McpStartupError _) -> True)
            readIORef live `shouldReturn` False

    it "releases the lease before clearing hooks when completion fails" $
        bracket MCP.newMcpSupervisor MCP.closeMcpSupervisor \supervisor -> do
            cleared <- newIORef False
            let hooks = quietHooks
                    { mcpClearHostHooks = do
                        leaseCount supervisor `shouldReturn` 0
                        writeIORef cleared True
                    }
            acquireMcpStartup supervisor emptyRequest hooks
                (\_ _ -> ioError (userError "completion failed") :: IO ())
                `shouldThrow` anyIOException
            readIORef cleared `shouldReturn` True

    it "joins a cancelled blocking acquisition before clearing its host hooks" $ within $
        bracket MCP.newMcpSupervisor MCP.closeMcpSupervisor \supervisor -> do
            started <- newEmptyMVar
            blocked <- newEmptyMVar
            stopped <- newEmptyMVar
            cleared <- newIORef False
            let endpoint = testServer
                    { MCP.toolServerListTools =
                        (putMVar started () >> takeMVar blocked >> pure (Right []))
                            `finally` putMVar stopped ()
                    }
                hooks = quietHooks
                    { mcpClearHostHooks = do
                        tryReadMVar stopped `shouldReturn` Just ()
                        writeIORef cleared True
                    }
            withAsync
                (acquireMcpStartup supervisor (memoryRequest False endpoint) hooks
                    (\_ _ -> pure ())) \worker -> do
                takeMVar started
                cancel worker
            readIORef cleared `shouldReturn` True
            leaseCount supervisor `shouldReturn` 0

    it "releases an acquired lease when its completion callback is cancelled" $ within $
        bracket MCP.newMcpSupervisor MCP.closeMcpSupervisor \supervisor -> do
            started <- newEmptyMVar
            blocked <- newEmptyMVar
            cleared <- newIORef False
            let hooks = quietHooks
                    { mcpClearHostHooks = do
                        leaseCount supervisor `shouldReturn` 0
                        writeIORef cleared True
                    }
            withAsync
                (acquireMcpStartup supervisor emptyRequest hooks
                    (\_ _ -> putMVar started () >> takeMVar blocked :: IO ())) \worker -> do
                takeMVar started
                leaseCount supervisor `shouldReturn` 1
                cancel worker
            readIORef cleared `shouldReturn` True

configuration :: [(Text, McpServerConfig)] -> McpConfigurationRequest
configuration servers = McpConfigurationRequest
    { mcpConfigCwd = either (error . show) id (OsPath.encodeUtf "/workspace")
    , mcpConfigHome = either (error . show) id (OsPath.encodeUtf "/home/test")
    , mcpConfigHarness = defaultHarnessConfig { configMcpServers = Map.fromList servers }
    , mcpConfigToolsEnabled = True
    , mcpConfigHostExtensions = True
    , mcpConfigOneShot = False
    }

quietHooks :: McpStartupHooks
quietHooks = McpStartupHooks
    { mcpInstallHostHooks = pure ()
    , mcpClearHostHooks = pure ()
    , mcpReportBlocking = const (pure ())
    , mcpReportProgressive = const (pure ())
    }

emptyRequest :: McpStartupRequest
emptyRequest = McpStartupRequest [] [] False

memoryRequest :: Bool -> MCP.McpToolServer -> McpStartupRequest
memoryRequest progressive endpoint =
    McpStartupRequest [] [(integrationsMcpConfig "memory", endpoint)] progressive

testConfig :: McpServerConfig
testConfig = McpServerConfig
    { mcpEnabled = True
    , mcpUrl = Nothing
    , mcpConnectionId = Nothing
    , mcpConnectionCredentials = Nothing
    , mcpConnectionGeneration = Nothing
    , mcpDisplayName = Nothing
    , mcpCommand = "test-server"
    , mcpArgs = ["--test"]
    , mcpCwd = Nothing
    , mcpEnv = Map.empty
    , mcpStartupTimeoutSeconds = 30
    , mcpRequestTimeoutSeconds = 60
    , mcpOAuth = Nothing
    , mcpProtocol = MCP.McpProtocolModern
    , mcpRoots = False
    , mcpSampling = False
    , mcpLogLevel = Nothing
    }

testServer :: MCP.McpToolServer
testServer = MCP.McpToolServer
    { toolServerInitialize = pure MCP.McpServerInfo
        { serverInfoEra = MCP.McpEraModern
        , serverInfoProtocolVersion = "2026-07-28"
        , serverInfoName = Just "test"
        , serverInfoVersion = Just "1"
        , serverInfoTitle = Nothing
        , serverInfoIcons = []
        , serverInfoInstructions = Just "Test instructions"
        , serverInfoCapabilities = MCP.emptyServerCapabilities
            { MCP.capabilityTools = Just (MCP.McpListCapability False) }
        }
    , toolServerListTools = pure (Right [])
    , toolServerCallTool = \_ -> error "no test tools"
    , toolServerSubscribe = \_ -> pure (pure ())
    }

leaseCount :: MCP.McpSupervisor -> IO Int
leaseCount supervisor = do
    state <- readMVar supervisor.supervisorState
    pure (sum (map (.supervisorEntryLeases) state.supervisorEntries))

within :: IO () -> IO ()
within action =
    timeout 5000000 action `shouldReturn` Just ()
