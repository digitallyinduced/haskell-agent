-- | Resolve captured native connection identities against the shared catalog.
module Agent.CLI.McpConnectionRuntime
    ( mcpConnectionCredentials
    , registerMcpConnectionRuntime
    , invalidateMcpConnectionRuntimes
    ) where

import Agent.CLI.Config (HarnessConfig(..), McpServerConfig(..), withHarnessConfigSnapshot, harnessConfigPath, mcpUsesConnectionCredentials)
import Agent.CLI.McpConnectionCredentials (mcpConnectionCredentialProviderWith, withMcpConnectionRefreshLock)
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
mcpConnectionCredentials :: MCP.McpServerConfig -> IO (Maybe MCP.McpCredentialProvider)
mcpConnectionCredentials runtime = case runtime.mcpServerConnection of
    Nothing -> pure Nothing
    Just identity -> do
        home <- getHomeDirectory
        let identifier = identity.mcpConnectionIdentifier
            valid = not (Text.null identifier) && Text.length identifier <= 128
                && Text.all (\c -> isAsciiLower c || isAsciiUpper c || isDigit c || c == '-') identifier
            provider = mcpConnectionCredentialProviderWith identifier (gate identity)
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
