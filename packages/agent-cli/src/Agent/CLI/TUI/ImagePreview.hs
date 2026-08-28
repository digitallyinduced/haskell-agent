{-# LANGUAGE BangPatterns #-}

-- | True-colour image previews for the retained fullscreen interface.
module Agent.CLI.TUI.ImagePreview
    ( NativePreviewPlacement(..)
    , TuiImagePreview(..)
    , nativePreviewPlacements
    , sameNativePreviewLayout
    , imageDimensions
    , prepareTuiImagePreview
    , previewCountForWidth
    , previewCellSize
    , previewImageAt
    , renderTuiImagePreview
    ) where

import Agent.CLI.ImagePreview (kittyCompatibleAttachment)
import Agent.Loop (ImageAttachment(..))
import Brick (Widget, raw)
import Codec.Picture
    ( DynamicImage(..)
    , Image
    , PixelRGB8(..)
    , PixelRGBA8(..)
    , convertRGBA8
    , decodeImage
    , dynamicMap
    , generateImage
    , imageData
    , imageHeight
    , imageWidth
    , pixelAt
    )
import qualified Data.ByteString as BS
import Data.Bits ((.|.), shiftL)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector.Storable as Vector
import Data.Word (Word8)
import qualified Graphics.Vty as V

data PreviewCell = PreviewCell
    { cellTop :: !PixelRGB8
    , cellBottom :: !PixelRGB8
    }
    deriving (Eq, Show)

-- | Compare only the layout that changes while scrolling. Attachments can be
-- large, so synchronization must not traverse their bytes on every reflow.
sameNativePreviewLayout
    :: [NativePreviewPlacement]
    -> [NativePreviewPlacement]
    -> Bool
sameNativePreviewLayout left right =
    length left == length right
        && and (zipWith samePlacement left right)
  where
    samePlacement a b =
        a.nativePreviewImageId == b.nativePreviewImageId
            && a.nativePreviewRow == b.nativePreviewRow
            && a.nativePreviewColumn == b.nativePreviewColumn
            && a.nativePreviewColumns == b.nativePreviewColumns
            && a.nativePreviewRows == b.nativePreviewRows

data NativePreviewPlacement = NativePreviewPlacement
    { nativePreviewImageId :: !Int
    , nativePreviewRow :: !Int
    , nativePreviewColumn :: !Int
    , nativePreviewColumns :: !Int
    , nativePreviewRows :: !Int
    , nativePreviewAttachment :: !ImageAttachment
    }
    deriving (Eq, Show)

-- | A bounded RGB sample. It is large enough to fill a typical terminal while
-- avoiding retention of another full decoded copy of a large screenshot.
data TuiImagePreview = TuiImagePreview
    { previewMime :: !Text
    , previewBytes :: !Int
    , previewSourceWidth :: !Int
    , previewSourceHeight :: !Int
    -- Keep the ANSI fallback sample lazy. Native Kitty previews only need the
    -- source dimensions and encoded attachment, and eagerly resampling a large
    -- screenshot here can stall the Brick event thread for over a second in
    -- the interpreted development loop.
    , previewSample :: Image PixelRGB8
    , previewKittyAttachment :: !ImageAttachment
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
    (sourceWidth, sourceHeight) <- imageDimensions imageMime imageBytes
    let (targetWidth, targetHeight) =
            previewSampleSize sourceWidth sourceHeight
    pure TuiImagePreview
        { previewMime = imageMime
        , previewBytes = BS.length imageBytes
        , previewSourceWidth = sourceWidth
        , previewSourceHeight = sourceHeight
        , previewSample =
            case decodeImage imageBytes of
                Left _ ->
                    generateImage
                        (\_ _ -> previewBackground)
                        1
                        1
                Right dynamic ->
                    sampleDynamicImage
                        dynamic
                        targetWidth
                        targetHeight
        , previewKittyAttachment =
            kittyCompatibleAttachment
                (ImageAttachment{imageMime, imageBytes})
        }

-- | Read common image dimensions without inflating the complete compressed
-- image. macOS clipboard screenshots arrive as PNG, so native terminal
-- previews can be placed after inspecting only the 24-byte PNG header.
-- Formats without a cheap parser retain the full-decoder fallback.
imageDimensions :: Text -> BS.ByteString -> Either Text (Int, Int)
imageDimensions mime bytes
    | normalized == "image/png" =
        pngDimensions bytes
    | normalized == "image/jpeg" || normalized == "image/jpg" =
        jpegDimensions bytes
    | otherwise = do
        dynamic <- firstText (decodeImage bytes)
        pure (dynamicWidth dynamic, dynamicHeight dynamic)
  where
    normalized = Text.toLower mime

pngDimensions :: BS.ByteString -> Either Text (Int, Int)
pngDimensions bytes
    | BS.length bytes < 24 =
        Left "invalid PNG header"
    | BS.take 8 bytes /= pngSignature =
        Left "invalid PNG signature"
    | BS.take 4 (BS.drop 12 bytes) /= "IHDR" =
        Left "PNG has no leading IHDR chunk"
    | width <= 0 || height <= 0 =
        Left "invalid PNG dimensions"
    | otherwise =
        Right (width, height)
  where
    width = word32beAt bytes 16
    height = word32beAt bytes 20

pngSignature :: BS.ByteString
pngSignature = BS.pack [137, 80, 78, 71, 13, 10, 26, 10]

jpegDimensions :: BS.ByteString -> Either Text (Int, Int)
jpegDimensions bytes
    | BS.length bytes < 4
        || BS.index bytes 0 /= 0xff
        || BS.index bytes 1 /= 0xd8 =
        Left "invalid JPEG header"
    | otherwise =
        scan 2
  where
    size = BS.length bytes

    scan offset
        | offset + 1 >= size =
            Left "JPEG dimensions not found"
        | BS.index bytes offset /= 0xff =
            scan (offset + 1)
        | otherwise =
            let markerOffset = skipFill (offset + 1)
            in if markerOffset >= size
                then Left "truncated JPEG marker"
                else
                    let marker = BS.index bytes markerOffset
                        segment = markerOffset + 1
                    in if isStartOfFrame marker
                        then frameDimensions segment
                        else if marker == 0xd9 || marker == 0xda
                            then Left "JPEG dimensions not found"
                            else if isStandaloneMarker marker
                                then scan segment
                                else
                                    if segment + 1 >= size
                                        then Left "truncated JPEG segment"
                                        else
                                            let segmentLength =
                                                    word16beAt bytes segment
                                            in if segmentLength < 2
                                                then Left
                                                    "invalid JPEG segment length"
                                                else scan
                                                    (segment + segmentLength)

    skipFill offset
        | offset < size && BS.index bytes offset == 0xff =
            skipFill (offset + 1)
        | otherwise = offset

    frameDimensions segment
        | segment + 6 >= size =
            Left "truncated JPEG frame"
        | word16beAt bytes segment < 7 =
            Left "invalid JPEG frame"
        | width <= 0 || height <= 0 =
            Left "invalid JPEG dimensions"
        | otherwise =
            Right (width, height)
      where
        height = word16beAt bytes (segment + 3)
        width = word16beAt bytes (segment + 5)

isStartOfFrame :: Word8 -> Bool
isStartOfFrame marker =
    marker `elem`
        [ 0xc0, 0xc1, 0xc2, 0xc3
        , 0xc5, 0xc6, 0xc7
        , 0xc9, 0xca, 0xcb
        , 0xcd, 0xce, 0xcf
        ]

isStandaloneMarker :: Word8 -> Bool
isStandaloneMarker marker =
    marker == 0x01 || marker >= 0xd0 && marker <= 0xd8

word16beAt :: BS.ByteString -> Int -> Int
word16beAt bytes offset =
    fromIntegral (BS.index bytes offset) `shiftL` 8
        .|. fromIntegral (BS.index bytes (offset + 1))

word32beAt :: BS.ByteString -> Int -> Int
word32beAt bytes offset =
    fromIntegral (BS.index bytes offset) `shiftL` 24
        .|. fromIntegral (BS.index bytes (offset + 1)) `shiftL` 16
        .|. fromIntegral (BS.index bytes (offset + 2)) `shiftL` 8
        .|. fromIntegral (BS.index bytes (offset + 3))

dynamicWidth :: DynamicImage -> Int
dynamicWidth = dynamicMap imageWidth

dynamicHeight :: DynamicImage -> Int
dynamicHeight = dynamicMap imageHeight

-- | Size the preview to the supplied terminal-cell bounds while preserving its
-- aspect ratio. One terminal row represents two sampled pixel rows.
previewCellSize :: Int -> Int -> TuiImagePreview -> (Int, Int)
previewCellSize maxColumns maxRows preview =
    let (pixelWidth, pixelHeight) = previewPixelSize maxColumns maxRows preview
    in (pixelWidth, (pixelHeight + 1) `div` 2)

-- | Match the centered Brick overlay layout with zero-based terminal-cell
-- placements for Kitty graphics. The caption occupies the row immediately
-- below each placement.
nativePreviewPlacements
    :: Int
    -> Int
    -> Int
    -> [(ImageAttachment, TuiImagePreview)]
    -> [NativePreviewPlacement]
nativePreviewPlacements imageIdBase terminalColumns terminalRows previews =
    zipWith place [imageIdBase ..] shownWithSizes
  where
    shown =
        takeLast
            (previewCountForWidth maxWidth)
            previews
    count = length shown
    maxWidth = viewportPreviewSize terminalColumns
    maxHeight = viewportPreviewSize terminalRows
    gaps = max 0 (count - 1) * previewGap
    segmentWidth = max 1 ((maxWidth - gaps) `div` max 1 count)
    previewHeight = max 1 (maxHeight - 1)
    sizes =
        [ previewCellSize segmentWidth previewHeight preview
        | (_, preview) <- shown
        ]
    contentWidth = segmentWidth * count + gaps
    contentHeight =
        case sizes of
            [] -> 0
            _ -> maximum (map snd sizes) + 1
    contentColumn = max 0 ((terminalColumns - contentWidth) `div` 2)
    contentRow = max 0 ((terminalRows - contentHeight) `div` 2)

    place imageId ((_attachment, preview), (columns, rows), index) =
        NativePreviewPlacement
            { nativePreviewImageId = imageId
            , nativePreviewRow = contentRow
            , nativePreviewColumn =
                contentColumn
                    + index * (segmentWidth + previewGap)
                    + max 0 ((segmentWidth - columns) `div` 2)
            , nativePreviewColumns = columns
            , nativePreviewRows = rows
            , nativePreviewAttachment = preview.previewKittyAttachment
            }

    shownWithSizes =
        zip3 shown sizes [0 ..]

-- | Resample the bounded source preview for the current terminal area. Box
-- filtering is important for screenshots: nearest-neighbour sampling can miss
-- thin text and rules completely, leaving an almost blank rectangle.
previewImageAt :: Int -> Int -> TuiImagePreview -> Image PixelRGB8
previewImageAt maxColumns maxRows preview =
    generateImage
        (sampleImageBox preview.previewSample targetWidth targetHeight)
        targetWidth
        targetHeight
  where
    (targetWidth, targetHeight) =
        previewPixelSize maxColumns maxRows preview

renderTuiImagePreview :: Int -> Int -> TuiImagePreview -> Widget n
renderTuiImagePreview maxColumns maxRows preview =
    raw $
        V.vertCat
            [ V.horizCat
                [ renderCell PreviewCell
                    { cellTop = pixelAt image x topY
                    , cellBottom = pixelAt image x
                        (min (targetHeight - 1) (topY + 1))
                    }
                | x <- [0 .. targetWidth - 1]
                ]
            | topY <- [0, 2 .. targetHeight - 1]
            ]
  where
    image = previewImageAt maxColumns maxRows preview
    targetWidth = imageWidth image
    targetHeight = imageHeight image

previewPixelSize :: Int -> Int -> TuiImagePreview -> (Int, Int)
previewPixelSize maxColumns maxRows preview =
    previewSizeWithin
        (min maxPreviewColumns (max 1 maxColumns))
        (min maxPreviewPixelRows (max 1 maxRows * 2))
        preview.previewSourceWidth
        preview.previewSourceHeight

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

previewSampleSize :: Int -> Int -> (Int, Int)
previewSampleSize =
    previewSizeWithin maxPreviewSampleColumns maxPreviewSamplePixelRows

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

sampleDynamicImage
    :: DynamicImage
    -> Int
    -> Int
    -> Image PixelRGB8
sampleDynamicImage dynamic targetWidth targetHeight =
    case dynamic of
        ImageRGB8 image ->
            generateImage
                (sampleRgbBox image targetWidth targetHeight)
                targetWidth
                targetHeight
        ImageRGBA8 image ->
            generateImage
                (sampleRgbaBox image targetWidth targetHeight)
                targetWidth
                targetHeight
        _ ->
            let image = convertRGBA8 dynamic
            in generateImage
                (sampleRgbaBox image targetWidth targetHeight)
                targetWidth
                targetHeight

sampleImageBox :: Image PixelRGB8 -> Int -> Int -> Int -> Int -> PixelRGB8
sampleImageBox image targetWidth targetHeight =
    sampleRgbBox image targetWidth targetHeight

sampleRgbaBox
    :: Image PixelRGBA8
    -> Int
    -> Int
    -> Int
    -> Int
    -> PixelRGB8
sampleRgbaBox image targetWidth targetHeight x y =
    finishAverage (goY 0 0 0 0 0)
  where
    sourceWidth = imageWidth image
    sourceHeight = imageHeight image
    pixels = imageData image
    (startX, endX, startY, endY) =
        boxBounds sourceWidth sourceHeight targetWidth targetHeight x y
    sampleColumns = min maxBoxSamplesPerAxis (endX - startX)
    sampleRows = min maxBoxSamplesPerAxis (endY - startY)
    PixelRGB8 backgroundRed backgroundGreen backgroundBlue = previewBackground

    goY !sampleY !red !green !blue !count
        | sampleY >= sampleRows = (red, green, blue, count)
        | otherwise =
            let sourceY = samplePosition startY endY sampleRows sampleY
                (!red', !green', !blue', !count') =
                    goX sourceY 0 red green blue count
            in goY (sampleY + 1) red' green' blue' count'

    goX !sourceY !sampleX !red !green !blue !count
        | sampleX >= sampleColumns = (red, green, blue, count)
        | otherwise =
            let sourceX = samplePosition startX endX sampleColumns sampleX
                base = (sourceY * sourceWidth + sourceX) * 4
                pixelRed = Vector.unsafeIndex pixels base
                pixelGreen = Vector.unsafeIndex pixels (base + 1)
                pixelBlue = Vector.unsafeIndex pixels (base + 2)
                alpha = Vector.unsafeIndex pixels (base + 3)
            in goX
                sourceY
                (sampleX + 1)
                (red + compositeChannel pixelRed backgroundRed alpha)
                (green + compositeChannel pixelGreen backgroundGreen alpha)
                (blue + compositeChannel pixelBlue backgroundBlue alpha)
                (count + 1)

sampleRgbBox
    :: Image PixelRGB8
    -> Int
    -> Int
    -> Int
    -> Int
    -> PixelRGB8
sampleRgbBox image targetWidth targetHeight x y =
    finishAverage (goY 0 0 0 0 0)
  where
    sourceWidth = imageWidth image
    sourceHeight = imageHeight image
    pixels = imageData image
    (startX, endX, startY, endY) =
        boxBounds sourceWidth sourceHeight targetWidth targetHeight x y
    sampleColumns = min maxBoxSamplesPerAxis (endX - startX)
    sampleRows = min maxBoxSamplesPerAxis (endY - startY)

    goY !sampleY !red !green !blue !count
        | sampleY >= sampleRows = (red, green, blue, count)
        | otherwise =
            let sourceY = samplePosition startY endY sampleRows sampleY
                (!red', !green', !blue', !count') =
                    goX sourceY 0 red green blue count
            in goY (sampleY + 1) red' green' blue' count'

    goX !sourceY !sampleX !red !green !blue !count
        | sampleX >= sampleColumns = (red, green, blue, count)
        | otherwise =
            let sourceX = samplePosition startX endX sampleColumns sampleX
                base = (sourceY * sourceWidth + sourceX) * 3
            in goX
                sourceY
                (sampleX + 1)
                (red + fromIntegral (Vector.unsafeIndex pixels base))
                (green + fromIntegral (Vector.unsafeIndex pixels (base + 1)))
                (blue + fromIntegral (Vector.unsafeIndex pixels (base + 2)))
                (count + 1)

boxBounds
    :: Int
    -> Int
    -> Int
    -> Int
    -> Int
    -> Int
    -> (Int, Int, Int, Int)
boxBounds sourceWidth sourceHeight targetWidth targetHeight x y =
    ( startX
    , max (startX + 1) ((x + 1) * sourceWidth `div` targetWidth)
    , startY
    , max (startY + 1) ((y + 1) * sourceHeight `div` targetHeight)
    )
  where
    startX = x * sourceWidth `div` targetWidth
    startY = y * sourceHeight `div` targetHeight

samplePosition :: Int -> Int -> Int -> Int -> Int
samplePosition start end count index =
    start + ((2 * index + 1) * (end - start)) `div` (2 * count)

finishAverage :: (Int, Int, Int, Int) -> PixelRGB8
finishAverage (red, green, blue, count) =
    PixelRGB8 (rounded red) (rounded green) (rounded blue)
  where
    rounded total =
        fromIntegral ((total + count `div` 2) `div` count)

compositeChannel :: Word8 -> Word8 -> Word8 -> Int
compositeChannel foreground background alpha =
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

-- The ANSI source sample is deliberately smaller than the maximum layout.
-- It may be upscaled in unusually large terminals, but keeping this bounded
-- avoids hundreds of thousands of interpreted sampling operations on paste.
maxPreviewSampleColumns :: Int
maxPreviewSampleColumns = 96

maxPreviewSamplePixelRows :: Int
maxPreviewSamplePixelRows = 64

-- Bound work per output pixel so large screenshots remain cheap in the
-- interpreted development loop while still sampling throughout each source
-- region instead of selecting one arbitrary pixel.
maxBoxSamplesPerAxis :: Int
maxBoxSamplesPerAxis = 3

maxShownPreviews :: Int
maxShownPreviews = 3

-- | Number of previews that can fit with at least one column per image and
-- the normal gap between them.
previewCountForWidth :: Int -> Int
previewCountForWidth availableWidth
    | availableWidth <= 0 = 0
    | otherwise =
        min
            maxShownPreviews
            ((availableWidth + previewGap) `div` (1 + previewGap))

previewGap :: Int
previewGap = 2

viewportPreviewSize :: Int -> Int
viewportPreviewSize available =
    max 1 (available * 3 `div` 5)

takeLast :: Int -> [a] -> [a]
takeLast count values =
    drop (max 0 (length values - count)) values

firstText :: Either String a -> Either Text a
firstText = either (Left . Text.pack) Right
