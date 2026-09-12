-- | Frontend-neutral MCP configuration and ownership during session startup.
module Agent.Runtime.Mcp.Startup
    ( McpConfigurationRequest(..)
    , resolveMcpConfiguration
    , McpStartupRequest(..)
    , McpStartupHooks(..)
    , McpStartupError(..)
    , startupServerConfigs
    , integrationsMcpConfig
    , acquireMcpStartup
    ) where

import qualified Agent.MCP as MCP
import Agent.OsPath (unsafeToFilePath)
import Agent.Runtime.Config
    ( HarnessConfig(..), McpServerConfig(..)
    , mcpServersForRuntime, mcpUsesConnectionCredentials, useProgressiveMcp )
import Agent.Runtime.McpOAuthStore (mcpOAuthStorePath)
import Control.Exception.Safe (Exception, SomeException, bracketOnError, finally, onException, throwIO, try)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import System.OsPath (OsPath)

data McpConfigurationRequest = McpConfigurationRequest
    { mcpConfigCwd :: OsPath
    , mcpConfigHome :: OsPath
    , mcpConfigHarness :: HarnessConfig
    , mcpConfigToolsEnabled :: Bool
    , mcpConfigHostExtensions :: Bool
    , mcpConfigOneShot :: Bool
    }

newtype McpStartupError = McpStartupError SomeException
    deriving (Show)

instance Exception McpStartupError

resolveMcpConfiguration :: McpConfigurationRequest -> ([MCP.McpServerConfig], Bool)
resolveMcpConfiguration McpConfigurationRequest{..} =
    ( map configure
        (mcpServersForRuntime mcpConfigToolsEnabled mcpConfigHostExtensions mcpConfigHarness)
    , useProgressiveMcp mcpConfigHarness.configMcpInitStrategy mcpConfigOneShot
    )
  where
    configure (label, config) = MCP.McpServerConfig
        { MCP.mcpServerName = label
        , MCP.mcpServerConnection = fmap (\identifier -> MCP.McpConnectionIdentity
            identifier config.mcpConnectionGeneration config.mcpDisplayName)
            (if mcpUsesConnectionCredentials config then config.mcpConnectionId else Nothing)
        , MCP.mcpServerUrl = config.mcpUrl
        , MCP.mcpServerCommand = Text.unpack config.mcpCommand
        , MCP.mcpServerArgs = map Text.unpack config.mcpArgs
        , MCP.mcpServerCwd = Just (maybe (unsafeToFilePath mcpConfigCwd) Text.unpack config.mcpCwd)
        , MCP.mcpServerEnv =
            [(Text.unpack name, Text.unpack value) | (name, value) <- Map.toAscList config.mcpEnv]
                <> case (mcpUsesConnectionCredentials config, config.mcpUrl) of
                    (False, Just url)
                        | Map.notMember "MCP_OAUTH_TOKEN_FILE" config.mcpEnv ->
                            [("MCP_OAUTH_TOKEN_FILE", unsafeToFilePath (mcpOAuthStorePath mcpConfigHome url))]
                    _ -> []
        , MCP.mcpServerStartupTimeoutSeconds = config.mcpStartupTimeoutSeconds
        , MCP.mcpServerRequestTimeoutSeconds = config.mcpRequestTimeoutSeconds
        , MCP.mcpServerProtocol = config.mcpProtocol
        , MCP.mcpServerRootsEnabled = config.mcpRoots
        , MCP.mcpServerSamplingEnabled = config.mcpSampling
        , MCP.mcpServerLogLevel = config.mcpLogLevel
        , MCP.mcpServerExcludedTools = []
        }

data McpStartupRequest = McpStartupRequest
    { mcpStartupTransportServers :: [MCP.McpServerConfig]
    , mcpStartupInMemoryServers :: [(MCP.McpServerConfig, MCP.McpToolServer)]
    , mcpStartupProgressive :: Bool
    }

-- | Hosts supply presentation and install their session-specific protocol
-- callbacks. Clearing must tolerate partially completed installation.
data McpStartupHooks = McpStartupHooks
    { mcpInstallHostHooks :: IO ()
    , mcpClearHostHooks :: IO ()
    , mcpReportBlocking :: [Text] -> IO ()
    , mcpReportProgressive :: [MCP.McpServerStatus] -> IO ()
    }

startupServerConfigs :: McpStartupRequest -> [MCP.McpServerConfig]
startupServerConfigs request =
    request.mcpStartupTransportServers <> map fst request.mcpStartupInMemoryServers

-- | The successful continuation transfers cleanup to the enclosing session
-- scope. If acquisition or that continuation fails (including cancellation),
-- release the lease before clearing hooks used by its workers.
acquireMcpStartup
    :: MCP.McpSupervisor
    -> McpStartupRequest
    -> McpStartupHooks
    -> (MCP.McpFleet -> IO () -> IO a)
    -> IO a
acquireMcpStartup supervisor request hooks finish =
    bracketOnError
        ((hooks.mcpInstallHostHooks >> acquireLease)
            `onException` hooks.mcpClearHostHooks)
        release
        (\lease -> finish lease.mcpLeaseFleet (release lease))
  where
    acquireLease = try (acquire request hooks) >>= either (throwIO . McpStartupError) pure
    release lease =
        MCP.releaseMcpFleetLease lease `finally` hooks.mcpClearHostHooks
    acquire McpStartupRequest{..} McpStartupHooks{..}
        | null mcpStartupInMemoryServers =
            if mcpStartupProgressive
                then MCP.acquireMcpFleetProgressive supervisor mcpReportProgressive mcpStartupTransportServers
                else MCP.acquireMcpFleetWithProgress supervisor mcpReportBlocking mcpStartupTransportServers
        | mcpStartupProgressive =
            MCP.acquireMcpFleetProgressiveWithInMemory supervisor mcpReportProgressive
                mcpStartupTransportServers mcpStartupInMemoryServers
        | otherwise =
            MCP.acquireMcpFleetWithInMemory supervisor mcpReportBlocking
                mcpStartupTransportServers mcpStartupInMemoryServers

integrationsMcpConfig :: Text -> MCP.McpServerConfig
integrationsMcpConfig serverName = MCP.McpServerConfig
    { MCP.mcpServerName = serverName
    , MCP.mcpServerUrl = Nothing
    , MCP.mcpServerCommand = ""
    , MCP.mcpServerArgs = []
    , MCP.mcpServerCwd = Nothing
    , MCP.mcpServerEnv = []
    , MCP.mcpServerStartupTimeoutSeconds = 10
    , MCP.mcpServerRequestTimeoutSeconds = 60
    , MCP.mcpServerProtocol = MCP.McpProtocolModern
    , MCP.mcpServerRootsEnabled = False
    , MCP.mcpServerSamplingEnabled = False
    , MCP.mcpServerLogLevel = Nothing
    , MCP.mcpServerExcludedTools = []
    , MCP.mcpServerConnection = Nothing
    }
