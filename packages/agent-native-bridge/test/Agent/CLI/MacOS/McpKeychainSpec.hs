module Agent.CLI.MacOS.McpKeychainSpec (spec) where

import Agent.CLI.MacOS.McpKeychain
import Agent.CLI.MacOS.McpCredentialStore (nativeMcpCredentialRuntime)
import Agent.Runtime.McpConnectionCredentials (loadMcpConnectionRecord, deleteMcpConnectionRecord)
import Control.Concurrent.Async (concurrently)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Test.Hspec (Spec, describe, it, shouldSatisfy, shouldReturn)

-- These cases are rejected before Security.framework is called. Automated
-- tests must not alter the developer's login keychain or trigger consent UI.
spec :: Spec
spec = describe "MCP Keychain input boundary" do
    it "has a native backend on concurrent first use without a prior engine or install step" do
        concurrently
            (fmap (fmap (const ())) (loadMcpConnectionRecord nativeMcpCredentialRuntime ""))
            (deleteMcpConnectionRecord nativeMcpCredentialRuntime "")
            `shouldReturn`
                ( Left "Cannot read MCP credentials from Keychain. Unlock your login keychain and try again."
                , Left "Cannot remove MCP credentials from Keychain. Unlock your login keychain and try again."
                )
    it "rejects missing account identifiers without querying the login keychain" do
        readMcpKeychain "" >>= (`shouldSatisfy` isLeft)
        writeMcpKeychain "" "private-record" >>= (`shouldSatisfy` isLeft)
        deleteMcpKeychain "" >>= (`shouldSatisfy` isLeft)
    it "rejects embedded NUL identifiers and empty or oversized records" do
        readMcpKeychain "account\0suffix" >>= (`shouldSatisfy` isLeft)
        writeMcpKeychain "fixture-account" BS.empty >>= (`shouldSatisfy` isLeft)
        writeMcpKeychain "fixture-account" (BS.replicate (1024 * 1024 + 1) 1)
            >>= (`shouldSatisfy` isLeft)
        deleteMcpKeychain "account\0suffix" >>= (`shouldSatisfy` isLeft)
