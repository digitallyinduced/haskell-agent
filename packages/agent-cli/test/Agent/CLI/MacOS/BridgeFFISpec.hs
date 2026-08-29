{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

module Agent.CLI.MacOS.BridgeFFISpec (spec) where

#ifdef darwin_HOST_OS
import Foreign.C.Types (CInt(..))
import Test.Hspec (Spec, describe, it, shouldReturn)

foreign import ccall "ha_image_attachment_stage_smoke"
    imageAttachmentStageSmoke :: IO CInt

foreign import ccall "ha_native_turn_options_stage_smoke"
    nativeTurnOptionsStageSmoke :: IO CInt
#else
import Test.Hspec (Spec, describe, it, pendingWith)
#endif

spec :: Spec
spec = describe "native bridge FFI" do
    it "accepts copied image buffers through the exported bridge" do
#ifdef darwin_HOST_OS
        imageAttachmentStageSmoke `shouldReturn` 0
#else
        pendingWith "the native bridge smoke test only links on macOS"
#endif

    it "stages typed turn options and validates interaction resolution" do
#ifdef darwin_HOST_OS
        nativeTurnOptionsStageSmoke `shouldReturn` 0
#else
        pendingWith "the native bridge smoke test only links on macOS"
#endif
