{-# LANGUAGE ForeignFunctionInterface #-}

module Agent.CLI.MacOS.BridgeHeaderSpec (spec) where

import Foreign.C.Types (CInt(..))
import Test.Hspec (Spec, describe, it, shouldReturn)

foreign import ccall "ha_image_attachment_abi_smoke"
    imageAttachmentAbiSmoke :: IO CInt

foreign import ccall "ha_gateway_abi_smoke"
    gatewayAbiSmoke :: IO CInt

spec :: Spec
spec = describe "native image attachment ABI" do
    it "preserves the documented image struct layout and ordered buffers" do
        imageAttachmentAbiSmoke `shouldReturn` 0
    it "preserves typed gateway callbacks and synchronous validation" do
        gatewayAbiSmoke `shouldReturn` 0
