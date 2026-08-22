module Agent.CLI.TUIImagePreviewSpec (spec) where

import Agent.CLI.TUI.ImagePreview
    ( NativePreviewPlacement(..)
    , TuiImagePreview(..)
    , nativePreviewPlacements
    , prepareTuiImagePreview
    , previewCellSize
    , previewImageAt
    )
import Agent.Loop (ImageAttachment(..))
import Codec.Picture
    ( PixelRGB8(..)
    , PixelRGBA8(..)
    , encodePng
    , generateImage
    , imageHeight
    , imageWidth
    , pixelAt
    )
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
                    previewCellSize 80 24 preview `shouldBe` (4, 1)

        it "uses the available preview area while preserving aspect ratio" do
            let source =
                    generateImage
                        (\_ _ -> PixelRGB8 10 20 30)
                        1600
                        900
                attachment = ImageAttachment
                    { imageMime = "image/png"
                    , imageBytes = LBS.toStrict (encodePng source)
                    }
            case prepareTuiImagePreview attachment of
                Left err -> expectationFailure (show err)
                Right preview -> do
                    previewCellSize 72 23 preview `shouldBe` (72, 20)
                    previewCellSize 48 12 preview `shouldBe` (42, 12)

        it "preserves thin screenshot details while downsampling" do
            let source =
                    generateImage
                        (\x y ->
                            if x `elem` [2, 3] || y `elem` [2, 3]
                                then PixelRGB8 0 0 0
                                else PixelRGB8 255 255 255)
                        1604
                        442
                attachment = ImageAttachment
                    { imageMime = "image/png"
                    , imageBytes = LBS.toStrict (encodePng source)
                    }
            case prepareTuiImagePreview attachment of
                Left err -> expectationFailure (show err)
                Right preview -> do
                    let image = previewImageAt 72 23 preview
                    imageWidth image `shouldBe` 72
                    imageHeight image `shouldBe` 19
                    pixelAt image 0 0 `shouldNotBe` PixelRGB8 255 255 255

        it "centers native Kitty placements over the fullscreen placeholder" do
            let source =
                    generateImage
                        (\_ _ -> PixelRGB8 10 20 30)
                        1604
                        442
                attachment = ImageAttachment
                    { imageMime = "image/png"
                    , imageBytes = LBS.toStrict (encodePng source)
                    }
            case prepareTuiImagePreview attachment of
                Left err -> expectationFailure (show err)
                Right preview ->
                    nativePreviewPlacements
                        2000000000
                        120
                        40
                        [(attachment, preview)]
                        `shouldBe`
                            [ NativePreviewPlacement
                                { nativePreviewImageId = 2000000000
                                , nativePreviewRow = 14
                                , nativePreviewColumn = 24
                                , nativePreviewColumns = 72
                                , nativePreviewRows = 10
                                , nativePreviewAttachment = attachment
                                }
                            ]

        it "composites alpha instead of exposing hidden transparent RGB" do
            let transparentSource =
                    generateImage
                        (\x _ ->
                            if x == 0
                                then PixelRGBA8 255 0 255 0
                                else PixelRGBA8 255 255 255 128)
                        2
                        1
                compositedSource =
                    generateImage
                        (\x _ ->
                            if x == 0
                                then PixelRGB8 0 43 54
                                else PixelRGB8 128 149 155)
                        2
                        1
                attachment source = ImageAttachment
                    { imageMime = "image/png"
                    , imageBytes = LBS.toStrict (encodePng source)
                    }
                transparentPreview =
                    prepareTuiImagePreview (attachment transparentSource)
                compositedPreview =
                    prepareTuiImagePreview (attachment compositedSource)
            case (transparentPreview, compositedPreview) of
                (Right transparent, Right composited) ->
                    transparent.previewSample == composited.previewSample
                        `shouldBe` True
                (Left err, _) -> expectationFailure (show err)
                (_, Left err) -> expectationFailure (show err)

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
