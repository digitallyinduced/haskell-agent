module Agent.ClientIdentitySpec (spec) where

import Agent.ClientIdentity (gatewayUserAgent)
import Data.ByteString.Char8 qualified as BS
import System.Environment (withProgName)
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
