module Agent.CLI.TUIImagePreviewSpec (spec) where

import Agent.CLI.TUI.ImagePreview
    ( TuiImagePreview(..)
    , prepareTuiImagePreview
    )
import Agent.Loop (ImageAttachment(..))
import Codec.Picture (PixelRGB8(..), encodePng, generateImage)
import qualified Data.ByteString.Lazy as LBS
import Test.Hspec

spec :: Spec
spec =
    describe "prepareTuiImagePreview" do
        it "decodes and samples an image into bounded terminal cells" do
            let source =
                    generateImage
                        (\x y ->
                            PixelRGB8
                                (fromIntegral (x * 40))
                                (fromIntegral (y * 80))
                                120)
                        4
                        2
                attachment = ImageAttachment
                    { imageMime = "image/png"
                    , imageBytes = LBS.toStrict (encodePng source)
                    }
            case prepareTuiImagePreview attachment of
                Left err -> expectationFailure (show err)
                Right preview -> do
                    preview.previewSourceWidth `shouldBe` 4
                    preview.previewSourceHeight `shouldBe` 2
                    length preview.previewCells `shouldBe` 1
                    map length preview.previewCells `shouldBe` [4]

        it "rejects invalid image bytes without crashing the TUI" do
            prepareTuiImagePreview
                ImageAttachment
                    { imageMime = "image/png"
                    , imageBytes = "not an image"
                    }
                `shouldSatisfy` isLeft
  where
    isLeft = \case
        Left _ -> True
        Right _ -> False
