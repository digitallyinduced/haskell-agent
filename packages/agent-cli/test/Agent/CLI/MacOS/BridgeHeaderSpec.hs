{-# LANGUAGE ForeignFunctionInterface #-}

module Agent.CLI.MacOS.BridgeHeaderSpec (spec) where

import Foreign.C.Types (CInt(..))
import Test.Hspec (Spec, describe, it, shouldReturn)

foreign import ccall "ha_image_attachment_abi_smoke"
    imageAttachmentAbiSmoke :: IO CInt

foreign import ccall "ha_gateway_abi_smoke"
    gatewayAbiSmoke :: IO CInt

foreign import ccall "ha_mcp_admin_abi_smoke"
    mcpAdminAbiSmoke :: IO CInt

foreign import ccall "ha_session_continuity_abi_smoke"
    sessionContinuityAbiSmoke :: IO CInt

foreign import ccall "ha_data_browser_abi_smoke"
    dataBrowserAbiSmoke :: IO CInt

spec :: Spec
spec = do
    describe "native image attachment ABI" do
        it "preserves the documented image struct layout and ordered buffers" do
            imageAttachmentAbiSmoke `shouldReturn` 0
        it "preserves typed gateway callbacks and synchronous validation" do
            gatewayAbiSmoke `shouldReturn` 0
        it "preserves the typed MCP argument and environment layouts" do
            mcpAdminAbiSmoke `shouldReturn` 0
        it "preserves typed data-browser callbacks and synchronous validation" do
            dataBrowserAbiSmoke `shouldReturn` 0
    describe "native session continuity ABI" do
        it "matches callback signatures and rejects invalid UTF-8 safely" do
            sessionContinuityAbiSmoke `shouldReturn` 0
