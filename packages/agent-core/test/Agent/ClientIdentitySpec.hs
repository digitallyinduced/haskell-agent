module Agent.ClientIdentitySpec (spec) where

import Agent.ClientIdentity (gatewayUserAgent)
import Control.Exception.Safe (bracket)
import qualified Data.ByteString.Char8 as BS
import System.Environment (lookupEnv, setEnv, unsetEnv, withProgName)
import Test.Hspec

spec :: Spec
spec = describe "gatewayUserAgent" do
    it "identifies CLI requests, including CLI running on macOS" do
        value <- withProgName "agent-cli" gatewayUserAgent
        value `shouldSatisfy` BS.isPrefixOf "agent-cli/"
        value `shouldSatisfy` BS.isInfixOf "; commit "
    it "uses the explicit native RTS identity" do
        value <- withProgName "haskell-agent-macos" gatewayUserAgent
        value `shouldSatisfy` BS.isPrefixOf "haskell-agent-macos/"
    it "does not include an arbitrary executable name in the header" do
        value <- withProgName "private-name\r\nInjected: value" gatewayUserAgent
        value `shouldSatisfy` BS.isPrefixOf "agent-cli/"
        value `shouldSatisfy` BS.all (\c -> c >= ' ' && c <= '~')
    it "reports the same runtime version and build for each frontend" do
        cli <- withProgName "agent-cli" gatewayUserAgent
        native <- withProgName "haskell-agent-macos" gatewayUserAgent
        BS.dropWhile (/= '/') cli `shouldBe` BS.dropWhile (/= '/') native
    it "reads a frontend-owned build revision at runtime" do
        value <- withEnvironment "AGENT_BUILD_COMMIT" "abcdef12" gatewayUserAgent
        value `shouldSatisfy` BS.isInfixOf "; commit abcdef12)"
    it "rejects unsafe runtime revision text" do
        value <-
            withEnvironment
                "AGENT_BUILD_COMMIT"
                "bad\r\nInjected: value"
                gatewayUserAgent
        value `shouldSatisfy` BS.isInfixOf "; commit development)"

withEnvironment :: String -> String -> IO a -> IO a
withEnvironment name value =
    bracket
        (lookupEnv name <* setEnv name value)
        (maybe (unsetEnv name) (setEnv name))
        . const
