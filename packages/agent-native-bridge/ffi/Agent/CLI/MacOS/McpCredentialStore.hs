-- | Install the platform credential implementation before the native runtime
-- can start MCP connections. Registration itself performs no Keychain access.
module Agent.CLI.MacOS.McpCredentialStore (ensureNativeMcpCredentialStore) where

import Agent.CLI.MacOS.McpKeychain
import Agent.CLI.McpConnectionCredentials
    ( McpCredentialStore(..), installMcpCredentialStore )
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar)
import Control.Monad (unless)
import System.IO.Unsafe (unsafePerformIO)

credentialStoreInstalled :: MVar Bool
credentialStoreInstalled = unsafePerformIO (newMVar False)
{-# NOINLINE credentialStoreInstalled #-}

ensureNativeMcpCredentialStore :: IO ()
ensureNativeMcpCredentialStore =
    modifyMVar_ credentialStoreInstalled \installed -> do
        unless installed $
            installMcpCredentialStore McpCredentialStore
                { credentialStoreLoad = readMcpKeychain
                , credentialStoreSave = writeMcpKeychain
                , credentialStoreDelete = deleteMcpKeychain
                }
        pure True
