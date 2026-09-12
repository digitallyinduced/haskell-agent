-- | Application-scoped credential ownership at the handle-free C ABI boundary.
-- All engines and catalog operations borrow the same fully initialized runtime.
-- Construction performs no Keychain access.
module Agent.CLI.MacOS.McpCredentialStore (nativeMcpCredentialRuntime) where

import Agent.CLI.MacOS.McpKeychain
import Agent.CLI.McpConnectionCredentials
    ( CredentialRuntime, McpCredentialStore(..), newCredentialRuntime )
import System.IO.Unsafe (unsafePerformIO)

-- The existing C ABI has no application handle. Keep the sole singleton here,
-- not in the credential implementation: there is no separate install step or
-- mutable store that an engine can observe before initialization completes.
nativeMcpCredentialRuntime :: CredentialRuntime
nativeMcpCredentialRuntime = unsafePerformIO $ newCredentialRuntime $ Just McpCredentialStore
    { credentialStoreLoad = readMcpKeychain
    , credentialStoreSave = writeMcpKeychain
    , credentialStoreDelete = deleteMcpKeychain
    }
{-# NOINLINE nativeMcpCredentialRuntime #-}
