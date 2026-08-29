{-# LANGUAGE ForeignFunctionInterface #-}

module Agent.CLI.MacOS.BridgeFFISpec (spec) where

import Foreign.C.Types (CInt(..))
import Test.Hspec (Spec, describe, it, shouldReturn)

foreign import ccall "ha_image_attachment_stage_smoke"
    imageAttachmentStageSmoke :: IO CInt

spec :: Spec
spec = describe "native image attachment staging" do
    it "accepts copied image buffers through the exported bridge" do
        imageAttachmentStageSmoke `shouldReturn` 0
