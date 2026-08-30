{-# LANGUAGE ForeignFunctionInterface #-}

module Agent.CLI.MacOS.BridgeHeaderSpec (spec) where

import Foreign.C.Types (CInt(..))
import Test.Hspec (Spec, describe, it, shouldReturn)

foreign import ccall "ha_image_attachment_abi_smoke"
    imageAttachmentAbiSmoke :: IO CInt

foreign import ccall "ha_session_continuity_abi_smoke"
    sessionContinuityAbiSmoke :: IO CInt

spec :: Spec
spec = do
    describe "native image attachment ABI" do
        it "preserves the documented image struct layout and ordered buffers" do
            imageAttachmentAbiSmoke `shouldReturn` 0
    describe "native session continuity ABI" do
        it "matches callback signatures and rejects invalid UTF-8 safely" do
            sessionContinuityAbiSmoke `shouldReturn` 0
