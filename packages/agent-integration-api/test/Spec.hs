module Main (main) where

import Agent.Integration.API
import Agent.MCP (McpServerConfig(..), McpProtocolPreference(..), McpToolServer(..))
import Agent.Tools.Types (ToolEnv, defaultToolEnv)
import Control.Concurrent.Async (mapConcurrently_)
import Control.Exception.Safe (bracket)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Either (isLeft)
import System.IO.Temp (withSystemTempDirectory)
import System.OsPath (unsafeEncodeUtf)
import Test.Hspec

main :: IO ()
main = hspec do
    describe "provider-neutral integration ownership" do
        it "retains explicit exact device ownership without acquiring the ordinary provider" $
            withEnvironment \env -> do
                let provider _ toolEnv = fmap (fmap (\runtime -> runtime
                        { integrationRuntimeEndpoint =
                            LocalOverlayIntegrationEndpoint ["device_lookup"] testServer }))
                        (emptyIntegrationProvider toolEnv)
                bracket
                    (newIntegrationSupervisorWithOrganizationProvider
                        (\_ -> expectationFailure "ordinary provider invoked" >> emptyIntegrationProvider env)
                        (Just provider) env)
                    closeIntegrationSupervisor \supervisor -> do
                        acquired <- acquireIntegrationRuntime supervisor
                            (OrganizationIntegrationAuthority remoteConfig)
                        case acquired of
                            Right runtime -> case integrationRuntimeEndpoint runtime of
                                CombinedOverlayIntegrationEndpoint config names _ -> do
                                    config `shouldBe` remoteConfig
                                    names `shouldBe` ["device_lookup"]
                                _ -> expectationFailure "expected explicit device overlay"
                            Left _ -> expectationFailure "overlay unavailable"
        it "never acquires a local provider for an organization authority" $
            withEnvironment \env ->
                bracket
                    (newIntegrationSupervisor
                        (\_ -> expectationFailure "local provider invoked" >> emptyIntegrationProvider env)
                        env)
                    closeIntegrationSupervisor \supervisor -> do
                        acquired <- acquireIntegrationRuntime supervisor
                            (OrganizationIntegrationAuthority remoteConfig)
                        case acquired of
                            Left _ -> expectationFailure "remote runtime unavailable"
                            Right runtime -> case integrationRuntimeEndpoint runtime of
                                RemoteIntegrationEndpoint config -> config `shouldBe` remoteConfig
                                _ -> expectationFailure "wrong endpoint authority"
        it "combines only the explicitly supplied organization-local provider" $
            withEnvironment \env -> do
                starts <- newIORef []
                let provider config toolEnv = do
                        modifyIORef' starts (<> [config])
                        fmap (fmap (\runtime -> runtime
                            { integrationRuntimeEndpoint = LocalIntegrationEndpoint testServer }))
                            (emptyIntegrationProvider toolEnv)
                bracket
                    (newIntegrationSupervisorWithOrganizationProvider
                        (\_ -> expectationFailure "ordinary local provider invoked" >> emptyIntegrationProvider env)
                        (Just provider) env)
                    closeIntegrationSupervisor \supervisor -> do
                        mapConcurrently_
                            (\_ -> acquireIntegrationRuntime supervisor
                                (OrganizationIntegrationAuthority remoteConfig))
                            [1 .. 20 :: Int]
                        readIORef starts `shouldReturn` [remoteConfig]
                        acquired <- acquireIntegrationRuntime supervisor
                            (OrganizationIntegrationAuthority remoteConfig)
                        case acquired of
                            Right runtime -> case integrationRuntimeEndpoint runtime of
                                CombinedIntegrationEndpoint config server -> do
                                    config `shouldBe` remoteConfig
                                    fmap (either (const False) null) server.toolServerListTools
                                        `shouldReturn` True
                                _ -> expectationFailure "expected remote and authorized local endpoint"
                            Left _ -> expectationFailure "organization provider unavailable"
        it "retires each exact credential identity before replacement and on logout" $
            withEnvironment \env -> do
                events <- newIORef []
                let provider config toolEnv = do
                        modifyIORef' events (<> [("start", config)])
                        fmap (fmap (\runtime -> runtime
                            { closeIntegrationRuntime =
                                modifyIORef' events (<> [("close", config)]) }))
                            (emptyIntegrationProvider toolEnv)
                    rotated = remoteConfig
                        { mcpServerEnv = [("MCP_ACCESS_TOKEN", "different-test-token")] }
                    switched = rotated
                        { mcpServerUrl = Just "https://other.invalid/mcp/integrations" }
                bracket
                    (newIntegrationSupervisorWithOrganizationProvider
                        emptyIntegrationProvider (Just provider) env)
                    closeIntegrationSupervisor \supervisor -> do
                        let acquire config = acquireIntegrationRuntime supervisor
                                (OrganizationIntegrationAuthority config)
                        _ <- acquire remoteConfig
                        _ <- acquire rotated
                        _ <- acquire switched
                        resetIntegrationSupervisor supervisor
                        resetIntegrationSupervisor supervisor
                        readIORef events `shouldReturn`
                            [ ("start", remoteConfig), ("close", remoteConfig)
                            , ("start", rotated), ("close", rotated)
                            , ("start", switched), ("close", switched)
                            ]
                        _ <- acquire switched
                        _ <- acquireIntegrationRuntime supervisor LocalIntegrationAuthority
                        readIORef events `shouldReturn`
                            [ ("start", remoteConfig), ("close", remoteConfig)
                            , ("start", rotated), ("close", rotated)
                            , ("start", switched), ("close", switched)
                            , ("start", switched), ("close", switched)
                            ]
        it "fails closed without invoking the ordinary local provider on organization failure" $
            withEnvironment \env ->
                bracket
                    (newIntegrationSupervisorWithOrganizationProvider
                        (\_ -> expectationFailure "ordinary local fallback invoked" >> emptyIntegrationProvider env)
                        (Just (\_ _ -> pure (Left "organization integration unavailable"))) env)
                    closeIntegrationSupervisor \supervisor ->
                        acquireIntegrationRuntime supervisor
                            (OrganizationIntegrationAuthority remoteConfig)
                            `shouldReturnSatisfy` isLeft
        it "rejects a provider-supplied remote endpoint and closes it" $
            withEnvironment \env -> do
                closes <- newIORef (0 :: Int)
                let provider _ toolEnv = fmap (fmap (\runtime -> runtime
                        { integrationRuntimeEndpoint = RemoteIntegrationEndpoint remoteConfig
                        , closeIntegrationRuntime = modifyIORef' closes (+ 1) }))
                        (emptyIntegrationProvider toolEnv)
                bracket
                    (newIntegrationSupervisorWithOrganizationProvider
                        emptyIntegrationProvider (Just provider) env)
                    closeIntegrationSupervisor \supervisor -> do
                        acquireIntegrationRuntime supervisor
                            (OrganizationIntegrationAuthority remoteConfig)
                            `shouldReturnSatisfy` isLeft
                        readIORef closes `shouldReturn` 1
        it "never revives the previous identity when switching cleanup throws" $
            withEnvironment \env -> do
                starts <- newIORef (0 :: Int)
                let provider _ toolEnv = do
                        modifyIORef' starts (+ 1)
                        fmap (fmap (\runtime -> runtime
                            { closeIntegrationRuntime = ioError (userError "cleanup failed") }))
                            (emptyIntegrationProvider toolEnv)
                supervisor <- newIntegrationSupervisorWithOrganizationProvider
                    emptyIntegrationProvider (Just provider) env
                _ <- acquireIntegrationRuntime supervisor
                    (OrganizationIntegrationAuthority remoteConfig)
                acquireIntegrationRuntime supervisor LocalIntegrationAuthority
                    `shouldThrow` anyIOException
                acquireIntegrationRuntime supervisor
                    (OrganizationIntegrationAuthority remoteConfig)
                    `shouldReturnSatisfy` isLeft
                closeIntegrationSupervisor supervisor
                readIORef starts `shouldReturn` 1

        it "closes idempotently even when provider cleanup fails" $
            withEnvironment \env -> do
                let provider toolEnv = emptyIntegrationProvider toolEnv >>= \case
                        Left err -> pure (Left err)
                        Right runtime -> pure (Right runtime
                            {closeIntegrationRuntime = ioError (userError "cleanup failed")})
                supervisor <- newIntegrationSupervisor provider env
                _ <- acquireIntegrationRuntime supervisor LocalIntegrationAuthority
                closeIntegrationSupervisor supervisor `shouldThrow` anyIOException
                closeIntegrationSupervisor supervisor
        it "shares one local runtime and closes it exactly once" $
            withEnvironment \env -> do
                starts <- newIORef (0 :: Int)
                closes <- newIORef (0 :: Int)
                let provider toolEnv = do
                        modifyIORef' starts (+ 1)
                        emptyIntegrationProvider toolEnv >>= \case
                            Left err -> pure (Left err)
                            Right runtime -> pure (Right runtime
                                {closeIntegrationRuntime = modifyIORef' closes (+ 1)})
                supervisor <- newIntegrationSupervisor provider env
                mapConcurrently_
                    (\_ -> acquireIntegrationRuntime supervisor LocalIntegrationAuthority)
                    [1 .. 20 :: Int]
                readIORef starts `shouldReturn` 1
                closeIntegrationSupervisor supervisor
                closeIntegrationSupervisor supervisor
                readIORef closes `shouldReturn` 1
                acquired <- acquireIntegrationRuntime supervisor LocalIntegrationAuthority
                case acquired of
                    Left _ -> pure ()
                    Right _ -> expectationFailure "closed supervisor accepted acquisition"
        it "does not let a failed local provider affect remote acquisition" $
            withEnvironment \env ->
                bracket
                    (newIntegrationSupervisor (\_ -> pure (Left "unavailable")) env)
                    closeIntegrationSupervisor \supervisor -> do
                        _ <- acquireIntegrationRuntime supervisor LocalIntegrationAuthority
                        acquired <- acquireIntegrationRuntime supervisor
                            (OrganizationIntegrationAuthority remoteConfig)
                        case acquired of
                            Left _ -> expectationFailure "local failure affected remote"
                            Right runtime -> case integrationRuntimeEndpoint runtime of
                                RemoteIntegrationEndpoint config -> config `shouldBe` remoteConfig
                                _ -> expectationFailure "wrong endpoint authority"

shouldReturnSatisfy :: IO a -> (a -> Bool) -> Expectation
shouldReturnSatisfy action predicate = action >>= \value ->
    predicate value `shouldBe` True

testServer :: McpToolServer
testServer = McpToolServer
    { toolServerInitialize = ioError (userError "not used by ownership test")
    , toolServerListTools = pure (Right [])
    , toolServerCallTool = \_ -> ioError (userError "not used by ownership test")
    , toolServerSubscribe = \_ -> pure (pure ())
    }

withEnvironment :: (ToolEnv -> IO a) -> IO a
withEnvironment action = withSystemTempDirectory "integration-api-test" \root ->
    defaultToolEnv (unsafeEncodeUtf root) >>= action

remoteConfig :: McpServerConfig
remoteConfig = McpServerConfig
    { mcpServerName = "integrations"
    , mcpServerUrl = Just "https://gateway.invalid/mcp/integrations"
    , mcpServerCommand = ""
    , mcpServerArgs = []
    , mcpServerCwd = Nothing
    , mcpServerEnv = [("MCP_ACCESS_TOKEN", "redacted-test-token")]
    , mcpServerStartupTimeoutSeconds = 1
    , mcpServerRequestTimeoutSeconds = 1
    , mcpServerProtocol = McpProtocolLegacy
    , mcpServerRootsEnabled = False
    , mcpServerSamplingEnabled = False
    , mcpServerLogLevel = Nothing
    , mcpServerExcludedTools = []
    , mcpServerConnection = Nothing
    }
