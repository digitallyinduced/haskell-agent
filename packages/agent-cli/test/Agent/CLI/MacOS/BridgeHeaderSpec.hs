{-# LANGUAGE ForeignFunctionInterface #-}

module Agent.CLI.MacOS.BridgeHeaderSpec (spec) where

import Foreign.C.Types (CInt(..))
import Test.Hspec (Spec, describe, it, shouldReturn)

foreign import ccall "ha_image_attachment_abi_smoke"
    imageAttachmentAbiSmoke :: IO CInt

foreign import ccall "ha_interaction_option_abi_smoke"
    interactionOptionAbiSmoke :: IO CInt

spec :: Spec
spec = describe "native bridge struct ABI" do
    it "preserves the documented image struct layout and ordered buffers" do
        imageAttachmentAbiSmoke `shouldReturn` 0

    it "preserves the documented interaction option layout" do
        interactionOptionAbiSmoke `shouldReturn` 0
