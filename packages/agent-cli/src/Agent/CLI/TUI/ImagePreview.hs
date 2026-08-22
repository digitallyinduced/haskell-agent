-- | Small true-colour thumbnails for the retained fullscreen interface.
module Agent.CLI.TUI.ImagePreview
    ( TuiImagePreview(..)
    , prepareTuiImagePreview
    , renderTuiImagePreview
    ) where

import Agent.Loop (ImageAttachment(..))
import Brick (Widget, raw)
import Codec.Picture
    ( Image
    , PixelRGB8(..)
    , convertRGB8
    , decodeImage
    , imageHeight
    , imageWidth
    , pixelAt
    )
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Graphics.Vty as V

data PreviewCell = PreviewCell
    { cellTop :: !PixelRGB8
    , cellBottom :: !PixelRGB8
    }
    deriving (Eq, Show)

-- | A bounded, terminal-cell representation. Keeping only sampled pixels
-- avoids retaining another full decoded copy of a large screenshot.
data TuiImagePreview = TuiImagePreview
    { previewMime :: !Text
    , previewBytes :: !Int
    , previewSourceWidth :: !Int
    , previewSourceHeight :: !Int
    , previewCells :: ![[PreviewCell]]
    }
    deriving (Eq, Show)

prepareTuiImagePreview :: ImageAttachment -> Either Text TuiImagePreview
prepareTuiImagePreview ImageAttachment{imageMime, imageBytes} = do
    dynamic <- firstText (decodeImage imageBytes)
    let image = convertRGB8 dynamic
        sourceWidth = imageWidth image
        sourceHeight = imageHeight image
        (targetWidth, targetHeight) =
            previewSize sourceWidth sourceHeight
    pure TuiImagePreview
        { previewMime = imageMime
        , previewBytes = BS.length imageBytes
        , previewSourceWidth = sourceWidth
        , previewSourceHeight = sourceHeight
        , previewCells =
            sampleCells image targetWidth targetHeight
        }

renderTuiImagePreview :: TuiImagePreview -> Widget n
renderTuiImagePreview preview =
    raw $
        V.vertCat
            [ V.horizCat (map renderCell row)
            | row <- preview.previewCells
            ]

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
previewSize width height
    | width <= 0 || height <= 0 = (1, 1)
    | otherwise =
        let scale :: Double
            scale =
                minimum
                    [ 1
                    , fromIntegral maxPreviewColumns / fromIntegral width
                    , fromIntegral maxPreviewPixelRows / fromIntegral height
                    ]
        in ( max 1 (floor (fromIntegral width * scale))
           , max 1 (floor (fromIntegral height * scale))
           )

sampleCells :: Image PixelRGB8 -> Int -> Int -> [[PreviewCell]]
sampleCells image targetWidth targetHeight =
    [ [ PreviewCell
            { cellTop = samplePixel image targetWidth targetHeight x topY
            , cellBottom =
                samplePixel image targetWidth targetHeight x
                    (min (targetHeight - 1) (topY + 1))
            }
      | x <- [0 .. targetWidth - 1]
      ]
    | topY <- [0, 2 .. targetHeight - 1]
    ]

samplePixel
    :: Image PixelRGB8
    -> Int
    -> Int
    -> Int
    -> Int
    -> PixelRGB8
samplePixel image targetWidth targetHeight x y =
    pixelAt image sourceX sourceY
  where
    sourceX =
        min (imageWidth image - 1)
            (x * imageWidth image `div` targetWidth)
    sourceY =
        min (imageHeight image - 1)
            (y * imageHeight image `div` targetHeight)

maxPreviewColumns :: Int
maxPreviewColumns = 24

-- Two sampled pixel rows fit in one terminal row via the upper-half block.
maxPreviewPixelRows :: Int
maxPreviewPixelRows = 12

firstText :: Either String a -> Either Text a
firstText = either (Left . Text.pack) Right
