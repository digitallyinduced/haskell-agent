module Main (main) where

import Agent.Integration.API
import Agent.MCP (McpServerConfig(..), McpProtocolPreference(..))
import Agent.Tools.Types (ToolEnv(..), defaultToolEnv)
import Control.Concurrent.Async (mapConcurrently_)
import Control.Exception.Safe (bracket)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import System.Directory.OsPath (doesDirectoryExist)
import System.IO.Temp (withSystemTempDirectory)
import System.OsPath (unsafeEncodeUtf)
import Test.Hspec

main :: IO ()
main = hspec do
    describe "provider-neutral integration ownership" do
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
                            Right runtime ->
                                integrationRuntimeRemoteServer runtime `shouldBe` Just remoteConfig
        it "removes scratch even when provider cleanup fails" $
            withEnvironment \env -> do
                let provider toolEnv = emptyIntegrationProvider toolEnv >>= \case
                        Left err -> pure (Left err)
                        Right runtime -> pure (Right runtime
                            {closeIntegrationRuntime = ioError (userError "cleanup failed")})
                supervisor <- newIntegrationSupervisor provider env
                _ <- acquireIntegrationRuntime supervisor LocalIntegrationAuthority
                closeIntegrationSupervisor supervisor `shouldThrow` anyIOException
                doesDirectoryExist (unsafeEncodeUtf
                    (integrationSupervisorArtifactDirectory supervisor)) `shouldReturn` False
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
        it "authorizes its stable scratch root without redirecting session writes" $
            withEnvironment \env ->
                bracket
                    (newIntegrationSupervisor emptyIntegrationProvider env)
                    closeIntegrationSupervisor \supervisor -> do
                        session <- defaultToolEnv (unsafeEncodeUtf ".")
                        writeIORef session.toolSessionTmp (Just (unsafeEncodeUtf "session-only"))
                        prepareIntegrationSupervisorForSession supervisor session
                        readIORef session.toolSessionTmp
                            `shouldReturn` Just (unsafeEncodeUtf "session-only")
                        root <- readIORef env.toolSessionTmp
                        roots <- readIORef session.toolAllowedRoots
                        case root of
                            Nothing -> expectationFailure "missing process scratch"
                            Just path -> do
                                roots `shouldContain` [path]
                                closeIntegrationSupervisor supervisor
                                doesDirectoryExist path `shouldReturn` False
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
                            Right runtime ->
                                integrationRuntimeRemoteServer runtime `shouldBe` Just remoteConfig

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
    }
