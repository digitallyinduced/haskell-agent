{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

module Agent.CLI.MacOS.BridgeFFISpec (spec) where

import qualified Agent.CLI.MacOS.Bridge as Bridge
#ifdef darwin_HOST_OS
import Foreign.C.Types (CInt(..))
import Test.Hspec (Spec, describe, it, shouldReturn)

foreign import ccall "ha_image_attachment_stage_smoke"
    imageAttachmentStageSmoke :: IO CInt

foreign import ccall "ha_repository_review_abi_smoke"
    repositoryReviewAbiSmoke :: IO CInt
#else
import Test.Hspec (Spec, describe, it, pendingWith, shouldReturn)
#endif

spec :: Spec
spec = describe "native image attachment staging" do
    it "accepts copied image buffers through the exported bridge" do
#ifdef darwin_HOST_OS
        imageAttachmentStageSmoke `shouldReturn` 0
#else
        pendingWith "the native bridge smoke test only links on macOS"
#endif

    it "blocks new repository workers until cancel-all returns" do
        Bridge.repositoryCancelAllAdmissionSmoke `shouldReturn` True

    it "does not self-deadlock on callback lifecycle calls" do
        Bridge.repositoryCancelAllReentrancySmoke `shouldReturn` True
        Bridge.repositoryCheckDestroyReentrancySmoke `shouldReturn` True

    it "classifies async cancellation as cancelled, not failure" do
        Bridge.repositoryCancelClassificationSmoke `shouldReturn` True

    it "does not retry a terminal callback that throws" do
        Bridge.repositoryTerminalThrowSmoke `shouldReturn` True

    it "validates the typed repository-review ABI from native code" do
#ifdef darwin_HOST_OS
        repositoryReviewAbiSmoke `shouldReturn` 0
#else
        pendingWith "the native bridge smoke test only links on macOS"
#endif
