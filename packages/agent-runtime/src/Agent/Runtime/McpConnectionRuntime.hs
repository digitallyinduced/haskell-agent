-- | Resolve captured native connection identities against the shared catalog.
module Agent.Runtime.McpConnectionRuntime
    ( mcpConnectionCredentials
    , registerMcpConnectionRuntime
    , invalidateMcpConnectionRuntimes
    , observeMcpConnectionInfo
    , readMcpConnectionIcons
    ) where

import Agent.Runtime.Config (HarnessConfig(..), McpServerConfig(..), withHarnessConfigSnapshot, harnessConfigPath, mcpUsesConnectionCredentials)
import Agent.Runtime.McpConnectionCredentials (CredentialRuntime, mcpConnectionCredentialProviderWith, withMcpConnectionRefreshLock)
import Agent.OsPath (fromText)
import qualified Agent.MCP as MCP
import qualified Data.Map.Strict as Map
import Data.IORef
import Data.Unique (Unique, newUnique)
import Control.Exception.Safe (finally)
import System.Directory.OsPath (getHomeDirectory)
import System.IO.Unsafe (unsafePerformIO)
import System.OsPath (takeDirectory, (</>))
import qualified Data.Text as Text
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)

-- Process-local, bounded metadata cache. Identity, generation and endpoint
-- must all match; a removed/replaced connection cannot inherit another icon.
{-# NOINLINE connectionIcons #-}
connectionIcons :: IORef (Map.Map (Text.Text, Maybe Text.Text, Maybe Text.Text) [MCP.McpIcon])
connectionIcons = unsafePerformIO (newIORef Map.empty)

observeMcpConnectionInfo :: MCP.McpServerConfig -> MCP.McpServerInfo -> IO ()
observeMcpConnectionInfo config info = case config.mcpServerConnection of
    Nothing -> pure ()
    Just identity -> atomicModifyIORef' connectionIcons \current ->
        let key = (identity.mcpConnectionIdentifier,
                identity.mcpConnectionGeneration, config.mcpServerUrl)
            bounded = if Map.size current >= 256 then Map.empty else current
            icons = take 8 $ filter (\icon -> Text.length icon.iconSrc <= 262144)
                info.serverInfoIcons
        in (Map.insert key icons bounded, ())

readMcpConnectionIcons :: Text.Text -> Maybe Text.Text -> Text.Text -> IO [MCP.McpIcon]
readMcpConnectionIcons identifier generation endpoint =
    Map.findWithDefault [] (identifier, generation, Just endpoint) <$> readIORef connectionIcons

runtimeInvalidators :: IORef (Map.Map Unique (IO ()))
runtimeInvalidators = unsafePerformIO (newIORef Map.empty)
{-# NOINLINE runtimeInvalidators #-}

-- | Register a live native fleet owner; the returned action unregisters it.
registerMcpConnectionRuntime :: IO () -> IO (IO ())
registerMcpConnectionRuntime invalidate = do
    key <- newUnique
    atomicModifyIORef' runtimeInvalidators \current ->
        (Map.insert key invalidate current, ())
    pure $ atomicModifyIORef' runtimeInvalidators \current ->
        (Map.delete key current, ())

-- | Invoke outside catalog/store locks: shutdown may validate credentials.
-- Attempt every retirement even if one owner throws during cleanup.
invalidateMcpConnectionRuntimes :: IO ()
invalidateMcpConnectionRuntimes =
    readIORef runtimeInvalidators >>= foldr finally (pure ()) . Map.elems

-- | Explicit host metadata, not names or URLs, selects protected credentials.
-- Removed/stale identities remain authoritative and cannot fall back to
-- legacy tokens. The generation participates in fleet configuration equality.
mcpConnectionCredentials :: CredentialRuntime -> MCP.McpServerConfig -> IO (Maybe MCP.McpCredentialProvider)
mcpConnectionCredentials credentials runtime = case runtime.mcpServerConnection of
    Nothing -> pure Nothing
    Just identity -> do
        home <- getHomeDirectory
        let identifier = identity.mcpConnectionIdentifier
            valid = not (Text.null identifier) && Text.length identifier <= 128
                && Text.all (\c -> isAsciiLower c || isAsciiUpper c || isDigit c || c == '-') identifier
            provider = mcpConnectionCredentialProviderWith credentials identifier (gate identity)
            lockPath = takeDirectory (harnessConfigPath home) </>
                fromText ("mcp-refresh-" <> identifier <> ".lock")
        pure (Just (if valid then withMcpConnectionRefreshLock lockPath provider
            else MCP.McpCredentialProvider
                { MCP.mcpCredentialAccessToken = pure (Left "Invalid MCP connection identity")
                , MCP.mcpCredentialRefreshAccessToken = pure (Left "Invalid MCP connection identity")
                }))
  where
    gate identity action = do
        home <- getHomeDirectory
        result <- withHarnessConfigSnapshot home \_ catalog ->
            case Map.lookup runtime.mcpServerName catalog.configMcpServers of
                Just config
                    | config.mcpConnectionId == Just identity.mcpConnectionIdentifier
                    , mcpUsesConnectionCredentials config
                    , config.mcpConnectionGeneration == identity.mcpConnectionGeneration
                    , config.mcpUrl == runtime.mcpServerUrl
                    , config.mcpEnabled -> action
                _ -> pure (Left "MCP connection is disabled, removed, or has changed")
        pure (result >>= id)
