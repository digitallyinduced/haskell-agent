module Agent.CLI.TUIImagePreviewSpec (spec) where

import Agent.CLI.TUI.ImagePreview
    ( NativePreviewPlacement(..)
    , TuiImagePreview(..)
    , imageDimensions
    , nativePreviewPlacements
    , prepareTuiImagePreview
    , previewCountForWidth
    , previewCellSize
    , previewImageAt
    , sameNativePreviewLayout
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
import qualified Data.ByteString as BS
import Test.Hspec

spec :: Spec
spec = do
    describe "sameNativePreviewLayout" do
        let placement attachment =
                NativePreviewPlacement
                    { nativePreviewImageId = 7
                    , nativePreviewRow = 2
                    , nativePreviewColumn = 3
                    , nativePreviewColumns = 20
                    , nativePreviewRows = 6
                    , nativePreviewAttachment = attachment
                    }
        it "ignores attachment contents when geometry is unchanged" do
            sameNativePreviewLayout
                [placement (ImageAttachment "image/png" "first")]
                [placement (ImageAttachment "image/png" "second")]
                `shouldBe` True

        it "detects viewport movement" do
            let moved =
                    (placement (ImageAttachment "image/png" "bytes"))
                        { nativePreviewRow = 3
                        }
            sameNativePreviewLayout
                [placement (ImageAttachment "image/png" "bytes")]
                [moved]
                `shouldBe` False

    describe "imageDimensions" do
        it "reads PNG dimensions without decoding image data" do
            let headerOnlyPng = BS.pack
                    [ 137, 80, 78, 71, 13, 10, 26, 10
                    , 0, 0, 0, 13
                    , 73, 72, 68, 82
                    , 0, 0, 6, 64
                    , 0, 0, 3, 132
                    ]
            imageDimensions "image/png" headerOnlyPng
                `shouldBe` Right (1600, 900)

        it "reads JPEG dimensions from the first start-of-frame marker" do
            let jpegHeader = BS.pack
                    [ 0xff, 0xd8
                    , 0xff, 0xc0
                    , 0x00, 0x11
                    , 0x08
                    , 0x01, 0x2c
                    , 0x02, 0x80
                    ]
            imageDimensions "image/jpeg" jpegHeader
                `shouldBe` Right (640, 300)

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

        it "does not force the ANSI sample when sizing native previews" do
            let attachment = ImageAttachment
                    { imageMime = "image/png"
                    , imageBytes = ""
                    }
                preview = TuiImagePreview
                    { previewMime = "image/png"
                    , previewBytes = 0
                    , previewSourceWidth = 1600
                    , previewSourceHeight = 900
                    , previewSample =
                        error "native preview sizing forced the ANSI sample"
                    , previewKittyAttachment = attachment
                    }
            previewCellSize 72 23 preview `shouldBe` (72, 20)
            nativePreviewPlacements 100 120 40 [(attachment, preview)]
                `shouldBe`
                    [ NativePreviewPlacement
                        { nativePreviewImageId = 100
                        , nativePreviewRow = 9
                        , nativePreviewColumn = 24
                        , nativePreviewColumns = 72
                        , nativePreviewRows = 20
                        , nativePreviewAttachment = attachment
                        }
                    ]

        it "keeps every native placement inside narrow terminal bounds" do
            let attachment = ImageAttachment
                    { imageMime = "image/png"
                    , imageBytes = ""
                    }
                preview = TuiImagePreview
                    { previewMime = "image/png"
                    , previewBytes = 0
                    , previewSourceWidth = 1
                    , previewSourceHeight = 1
                    , previewSample =
                        error "native preview bounds forced the ANSI sample"
                    , previewKittyAttachment = attachment
                    }
                previews = replicate 3 (attachment, preview)
            previewCountForWidth 1 `shouldBe` 1
            previewCountForWidth 4 `shouldBe` 2
            previewCountForWidth 7 `shouldBe` 3
            mapM_
                (\columns ->
                    nativePreviewPlacements 100 columns 10 previews
                        `shouldSatisfy` all
                            (\placement ->
                                placement.nativePreviewColumn >= 0
                                    && placement.nativePreviewColumn
                                        + placement.nativePreviewColumns
                                        <= columns))
                [1 .. 12]

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
