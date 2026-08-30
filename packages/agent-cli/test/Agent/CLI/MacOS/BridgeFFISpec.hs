{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

module Agent.CLI.MacOS.BridgeFFISpec (spec) where

#ifdef darwin_HOST_OS
import Foreign.C.Types (CInt(..))
import Test.Hspec (Spec, describe, it, shouldReturn)

foreign import ccall "ha_image_attachment_stage_smoke"
    imageAttachmentStageSmoke :: IO CInt

foreign import ccall "ha_learned_skill_admin_validation_smoke"
    learnedSkillAdminValidationSmoke :: IO CInt

#else
import Test.Hspec (Spec, describe, it, pendingWith)
#endif

spec :: Spec
spec = describe "native engine lifecycle smoke" do
    it "stages copied images and completes restart before destroy returns" do
#ifdef darwin_HOST_OS
        imageAttachmentStageSmoke `shouldReturn` 0
#else
        pendingWith "the native bridge smoke test only links on macOS"
#endif
    it "rejects invalid learned resource inputs through exported symbols" do
#ifdef darwin_HOST_OS
        learnedSkillAdminValidationSmoke `shouldReturn` 0
#else
        pendingWith "the native bridge smoke test only links on macOS"
#endif
