-- | True-colour image previews for the retained fullscreen interface.
module Agent.CLI.TUI.ImagePreview
    ( TuiImagePreview(..)
    , prepareTuiImagePreview
    , previewCellSize
    , renderTuiImagePreview
    ) where

import Agent.Loop (ImageAttachment(..))
import Brick (Widget, raw)
import Codec.Picture
    ( Image
    , PixelRGB8(..)
    , PixelRGBA8(..)
    , convertRGBA8
    , decodeImage
    , generateImage
    , imageHeight
    , imageWidth
    , pixelAt
    )
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word8)
import qualified Graphics.Vty as V

data PreviewCell = PreviewCell
    { cellTop :: !PixelRGB8
    , cellBottom :: !PixelRGB8
    }
    deriving (Eq, Show)

-- | A bounded RGB sample. It is large enough to fill a typical terminal while
-- avoiding retention of another full decoded copy of a large screenshot.
data TuiImagePreview = TuiImagePreview
    { previewMime :: !Text
    , previewBytes :: !Int
    , previewSourceWidth :: !Int
    , previewSourceHeight :: !Int
    , previewSample :: !(Image PixelRGB8)
    }
    deriving (Eq)

instance Show TuiImagePreview where
    show preview =
        "TuiImagePreview { previewMime = "
            <> show preview.previewMime
            <> ", previewBytes = "
            <> show preview.previewBytes
            <> ", previewSourceWidth = "
            <> show preview.previewSourceWidth
            <> ", previewSourceHeight = "
            <> show preview.previewSourceHeight
            <> ", previewSampleSize = "
            <> show
                ( imageWidth preview.previewSample
                , imageHeight preview.previewSample
                )
            <> " }"

prepareTuiImagePreview :: ImageAttachment -> Either Text TuiImagePreview
prepareTuiImagePreview ImageAttachment{imageMime, imageBytes} = do
    dynamic <- firstText (decodeImage imageBytes)
    let image = convertRGBA8 dynamic
        sourceWidth = imageWidth image
        sourceHeight = imageHeight image
        (targetWidth, targetHeight) =
            previewSize sourceWidth sourceHeight
    pure TuiImagePreview
        { previewMime = imageMime
        , previewBytes = BS.length imageBytes
        , previewSourceWidth = sourceWidth
        , previewSourceHeight = sourceHeight
        , previewSample =
            generateImage
                (\x y -> samplePixel image targetWidth targetHeight x y)
                targetWidth
                targetHeight
        }

-- | Size the preview to the supplied terminal-cell bounds while preserving its
-- aspect ratio. One terminal row represents two sampled pixel rows.
previewCellSize :: Int -> Int -> TuiImagePreview -> (Int, Int)
previewCellSize maxColumns maxRows preview =
    let (pixelWidth, pixelHeight) =
            previewSizeWithin
                maxColumns
                (max 1 maxRows * 2)
                preview.previewSourceWidth
                preview.previewSourceHeight
    in (pixelWidth, (pixelHeight + 1) `div` 2)

renderTuiImagePreview :: Int -> Int -> TuiImagePreview -> Widget n
renderTuiImagePreview maxColumns maxRows preview =
    raw $
        V.vertCat
            [ V.horizCat
                [ renderCell PreviewCell
                    { cellTop =
                        samplePreviewPixel preview targetWidth targetHeight x topY
                    , cellBottom =
                        samplePreviewPixel preview targetWidth targetHeight x
                            (min (targetHeight - 1) (topY + 1))
                    }
                | x <- [0 .. targetWidth - 1]
                ]
            | topY <- [0, 2 .. targetHeight - 1]
            ]
  where
    (targetWidth, targetRows) =
        previewCellSize maxColumns maxRows preview
    targetHeight = max 1 (targetRows * 2)

renderCell :: PreviewCell -> V.Image
renderCell PreviewCell{cellTop, cellBottom} =
    V.char
        (pixelAttr cellTop cellBottom)
        '▀'

pixelAttr :: PixelRGB8 -> PixelRGB8 -> V.Attr
pixelAttr
    (PixelRGB8 topRed topGreen topBlue)
    (PixelRGB8 bottomRed bottomGreen bottomBlue) =
        V.withBackColor
            (V.withForeColor V.defAttr
                (V.rgbColor topRed topGreen topBlue))
            (V.rgbColor bottomRed bottomGreen bottomBlue)

previewSize :: Int -> Int -> (Int, Int)
previewSize =
    previewSizeWithin maxPreviewColumns maxPreviewPixelRows

previewSizeWithin :: Int -> Int -> Int -> Int -> (Int, Int)
previewSizeWithin maxWidth maxHeight width height
    | width <= 0 || height <= 0 = (1, 1)
    | otherwise =
        let scale :: Double
            scale =
                minimum
                    [ 1
                    , fromIntegral (max 1 maxWidth) / fromIntegral width
                    , fromIntegral (max 1 maxHeight) / fromIntegral height
                    ]
        in ( max 1 (floor (fromIntegral width * scale))
           , max 1 (floor (fromIntegral height * scale))
           )

samplePixel
    :: Image PixelRGBA8
    -> Int
    -> Int
    -> Int
    -> Int
    -> PixelRGB8
samplePixel image targetWidth targetHeight x y =
    compositePixel previewBackground (pixelAt image sourceX sourceY)
  where
    sourceX =
        min (imageWidth image - 1)
            (x * imageWidth image `div` targetWidth)
    sourceY =
        min (imageHeight image - 1)
            (y * imageHeight image `div` targetHeight)

samplePreviewPixel
    :: TuiImagePreview
    -> Int
    -> Int
    -> Int
    -> Int
    -> PixelRGB8
samplePreviewPixel preview targetWidth targetHeight x y =
    pixelAt sample sourceX sourceY
  where
    sample = preview.previewSample
    sourceX =
        min (imageWidth sample - 1)
            (x * imageWidth sample `div` targetWidth)
    sourceY =
        min (imageHeight sample - 1)
            (y * imageHeight sample `div` targetHeight)

-- Transparent PNG pixels can retain arbitrary RGB data. Dropping alpha with
-- 'convertRGB8' exposes those hidden colours as bright smears, so composite
-- onto the fullscreen canvas before rendering the sampled terminal cells.
compositePixel :: PixelRGB8 -> PixelRGBA8 -> PixelRGB8
compositePixel
    (PixelRGB8 backgroundRed backgroundGreen backgroundBlue)
    (PixelRGBA8 red green blue alpha) =
        PixelRGB8
            (blend red backgroundRed alpha)
            (blend green backgroundGreen alpha)
            (blend blue backgroundBlue alpha)

blend :: Word8 -> Word8 -> Word8 -> Word8
blend foreground background alpha =
    fromIntegral $
        ( fromIntegral foreground * alpha'
            + fromIntegral background * (255 - alpha')
            + 127
        ) `div` 255
  where
    alpha' = fromIntegral alpha :: Int

previewBackground :: PixelRGB8
previewBackground = PixelRGB8 0 43 54

maxPreviewColumns :: Int
maxPreviewColumns = 240

-- Two sampled pixel rows fit in one terminal row via the upper-half block.
maxPreviewPixelRows :: Int
maxPreviewPixelRows = 160

firstText :: Either String a -> Either Text a
firstText = either (Left . Text.pack) Right
