-- | Bounded image preparation for multimodal model requests.
module Agent.Image.Normalize
    ( NormalizedImage(..)
    , normalizeImageForPrompt
    , normalizeImageDataUrl
    ) where

import Codec.Picture
    ( DynamicImage(..)
    , Image
    , PixelRGB8(..)
    , PixelRGBA8(..)
    , PixelYCbCr8
    , convertRGB8
    , convertRGBA8
    , decodeImageWithMetadata
    , encodeJpegAtQuality
    , encodePng
    , imageHeight
    , imageWidth
    )
import qualified Codec.Picture.Metadata as Metadata
import Codec.Picture.Metadata.Exif
    ( ExifData(..)
    , ExifTag(TagOrientation)
    )
import Codec.Picture.Types
    ( Pixel
    , convertImage
    , dynamicMap
    , generateImage
    , pixelAt
    , pixelMap
    )
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Word (Word8)

-- These limits match the conservative preparation used by Grok Build. They
-- keep base64 expansion comfortably below the transport's request allowance
-- while retaining enough pixels for model-side tiling.
maximumImageBytes :: Int
maximumImageBytes = 1500000

maximumImageSide :: Int
maximumImageSide = 2000

maximumImagePixels :: Int
maximumImagePixels = 2408448

minimumRetrySide :: Int
minimumRetrySide = 512

jpegQualities :: [Word8]
jpegQualities = [88, 80, 72, 64, 56, 48, 40, 32]

data NormalizedImage = NormalizedImage
    { normalizedImageMime :: !Text
    , normalizedImageBytes :: !ByteString
    }

-- | Decode and, when necessary, resize or recompress an image before it is
-- serialized into a provider request. Small images remain byte-for-byte
-- unchanged. Formats JuicyPixels cannot decode (notably WebP) also remain
-- unchanged so normalization never makes a previously usable attachment
-- disappear.
normalizeImageForPrompt :: Text -> ByteString -> NormalizedImage
normalizeImageForPrompt mime bytes =
    case decodeImageWithMetadata bytes of
        Left _ -> original
        Right (dynamicImage, metadata)
            | imageAlreadyBounded dynamicImage -> original
            | dynamicImageHasAlpha dynamicImage ->
                firstFitting
                    (map (encodeRgbaCandidate rgbaImage)
                        dimensions)
                    original
            | otherwise ->
                firstFitting
                    (map (encodeRgbCandidate rgbImage)
                        dimensions)
                    original
          where
            orientation = metadataOrientation metadata
            rgbaImage =
                orientImage orientation (convertRGBA8 dynamicImage)
            rgbImage =
                orientImage orientation (convertRGB8 dynamicImage)
            dimensions =
                candidateDimensions
                    (if dynamicImageHasAlpha dynamicImage
                        then imageWidth rgbaImage
                        else imageWidth rgbImage)
                    (if dynamicImageHasAlpha dynamicImage
                        then imageHeight rgbaImage
                        else imageHeight rgbImage)
  where
    original = NormalizedImage mime bytes
    imageAlreadyBounded dynamicImage =
        ByteString.length bytes <= maximumImageBytes
            && dynamicImageWidth dynamicImage <= maximumImageSide
            && dynamicImageHeight dynamicImage <= maximumImageSide
            && toInteger (dynamicImageWidth dynamicImage)
                * toInteger (dynamicImageHeight dynamicImage)
                <= toInteger maximumImagePixels

-- | Normalize an inline base64 image while preserving malformed, remote, and
-- already-bounded URLs exactly. Tool results use this representation instead
-- of the raw attachment form used by user-authored turns.
normalizeImageDataUrl :: Text -> Text
normalizeImageDataUrl url =
    case parseImageDataUrl url of
        Nothing -> url
        Just (mime, bytes) ->
            case normalizeImageForPrompt mime bytes of
                NormalizedImage normalizedMime normalizedBytes
                    | normalizedMime == mime
                        && normalizedBytes == bytes ->
                            url
                    | otherwise ->
                        "data:"
                            <> normalizedMime
                            <> ";base64,"
                            <> TextEncoding.decodeUtf8
                                (Base64.encode normalizedBytes)

parseImageDataUrl :: Text -> Maybe (Text, ByteString)
parseImageDataUrl url = do
    let (metadata, payloadWithComma) = Text.breakOn "," url
        loweredMetadata = Text.toLower metadata
        base64Suffix = ";base64"
    guardMaybe (not (Text.null payloadWithComma))
    guardMaybe ("data:image/" `Text.isPrefixOf` loweredMetadata)
    guardMaybe (base64Suffix `Text.isSuffixOf` loweredMetadata)
    let mime =
            Text.drop 5
                (Text.dropEnd (Text.length base64Suffix) metadata)
    guardMaybe (not (Text.null mime))
    bytes <-
        either (const Nothing) Just
            (Base64.decode
                (TextEncoding.encodeUtf8 (Text.drop 1 payloadWithComma)))
    pure (mime, bytes)

guardMaybe :: Bool -> Maybe ()
guardMaybe condition
    | condition = Just ()
    | otherwise = Nothing

dynamicImageWidth :: DynamicImage -> Int
dynamicImageWidth = dynamicMap imageWidth

dynamicImageHeight :: DynamicImage -> Int
dynamicImageHeight = dynamicMap imageHeight

dynamicImageHasAlpha :: DynamicImage -> Bool
dynamicImageHasAlpha = \case
    ImageYA8 _ -> True
    ImageYA16 _ -> True
    ImageRGBA8 _ -> True
    ImageRGBA16 _ -> True
    _ -> False

metadataOrientation :: Metadata.Metadatas -> Int
metadataOrientation metadata =
    normalizeOrientation $
        case Metadata.lookup (Metadata.Exif TagOrientation) metadata of
            Just (ExifShort orientation) -> fromIntegral orientation
            Just (ExifLong orientation) -> fromIntegral orientation
            _ -> 1
  where
    normalizeOrientation orientation
        | orientation >= 1 && orientation <= 8 = orientation
        | otherwise = 1

-- EXIF orientation describes how stored pixels must be transformed for
-- display. Re-encoding drops the metadata, so apply that transform first.
orientImage :: Pixel pixel => Int -> Image pixel -> Image pixel
orientImage orientation source =
    case orientation of
        2 -> sameSize \x y -> pixelAt source (width - 1 - x) y
        3 -> sameSize \x y ->
            pixelAt source (width - 1 - x) (height - 1 - y)
        4 -> sameSize \x y -> pixelAt source x (height - 1 - y)
        5 -> swappedSize \x y -> pixelAt source y x
        6 -> swappedSize \x y -> pixelAt source y (height - 1 - x)
        7 -> swappedSize \x y ->
            pixelAt source (width - 1 - y) (height - 1 - x)
        8 -> swappedSize \x y -> pixelAt source (width - 1 - y) x
        _ -> source
  where
    width = imageWidth source
    height = imageHeight source
    sameSize sample = generateImage sample width height
    swappedSize sample = generateImage sample height width

candidateDimensions :: Int -> Int -> [(Int, Int)]
candidateDimensions sourceWidth sourceHeight =
    shrinkFrom (boundedDimensions sourceWidth sourceHeight)
  where
    shrinkFrom dimensions@(width, height)
        | max width height <= minimumRetrySide = [dimensions]
        | otherwise =
            dimensions : shrinkFrom nextDimensions
      where
        longest = max width height
        nextLongest =
            max minimumRetrySide ((longest * 3) `div` 4)
        scale =
            fromIntegral nextLongest / fromIntegral longest :: Double
        nextDimensions =
            ( max 1 (floor (fromIntegral width * scale))
            , max 1 (floor (fromIntegral height * scale))
            )

boundedDimensions :: Int -> Int -> (Int, Int)
boundedDimensions width height
    | width <= 0 || height <= 0 = (1, 1)
    | otherwise =
        constrainArea
            ( max 1 (floor (fromIntegral width * scale))
            , max 1 (floor (fromIntegral height * scale))
            )
  where
    pixelCount =
        fromIntegral width * fromIntegral height :: Double
    sideScale =
        fromIntegral maximumImageSide
            / fromIntegral (max width height)
    areaScale =
        sqrt (fromIntegral maximumImagePixels / pixelCount)
    scale = min 1 (min sideScale areaScale)

    constrainArea dimensions@(targetWidth, targetHeight)
        | toInteger targetWidth * toInteger targetHeight
            <= toInteger maximumImagePixels =
                dimensions
        | targetWidth >= targetHeight =
            (max 1 (maximumImagePixels `div` targetHeight), targetHeight)
        | otherwise =
            (targetWidth, max 1 (maximumImagePixels `div` targetWidth))

encodeRgbCandidate
    :: Image PixelRGB8
    -> (Int, Int)
    -> Maybe NormalizedImage
encodeRgbCandidate source (width, height) =
    let resized = resizeRgb8 width height source
        png = strictEncodePng resized
    in if ByteString.length png <= maximumImageBytes
        then Just (NormalizedImage "image/png" png)
        else encodeJpegCandidate resized

encodeRgbaCandidate
    :: Image PixelRGBA8
    -> (Int, Int)
    -> Maybe NormalizedImage
encodeRgbaCandidate source (width, height) =
    let resized = resizeRgba8 width height source
        png = strictEncodePng resized
    in if ByteString.length png <= maximumImageBytes
        then Just (NormalizedImage "image/png" png)
        else encodeJpegCandidate (pixelMap flattenAgainstWhite resized)

encodeJpegCandidate :: Image PixelRGB8 -> Maybe NormalizedImage
encodeJpegCandidate image =
    go jpegQualities
  where
    yCbCrImage = convertImage image :: Image PixelYCbCr8

    go = \case
        [] -> Nothing
        quality : remaining ->
            let jpeg =
                    LazyByteString.toStrict
                        (encodeJpegAtQuality quality yCbCrImage)
            in if ByteString.length jpeg <= maximumImageBytes
                then Just (NormalizedImage "image/jpeg" jpeg)
                else go remaining

strictEncodePng image =
    LazyByteString.toStrict (encodePng image)

firstFitting
    :: [Maybe NormalizedImage]
    -> NormalizedImage
    -> NormalizedImage
firstFitting candidates fallback =
    case candidates of
        [] -> fallback
        Nothing : remaining -> firstFitting remaining fallback
        Just image : _ -> image

resizeRgb8 :: Int -> Int -> Image PixelRGB8 -> Image PixelRGB8
resizeRgb8 targetWidth targetHeight source
    | targetWidth == imageWidth source
        && targetHeight == imageHeight source =
            source
    | otherwise =
        generateImage sample targetWidth targetHeight
  where
    sample x y =
        let (x0, x1, xFraction) =
                sourceSample (imageWidth source) targetWidth x
            (y0, y1, yFraction) =
                sourceSample (imageHeight source) targetHeight y
            PixelRGB8 r00 g00 b00 = pixelAt source x0 y0
            PixelRGB8 r10 g10 b10 = pixelAt source x1 y0
            PixelRGB8 r01 g01 b01 = pixelAt source x0 y1
            PixelRGB8 r11 g11 b11 = pixelAt source x1 y1
        in PixelRGB8
            (bilinear8 xFraction yFraction r00 r10 r01 r11)
            (bilinear8 xFraction yFraction g00 g10 g01 g11)
            (bilinear8 xFraction yFraction b00 b10 b01 b11)

resizeRgba8 :: Int -> Int -> Image PixelRGBA8 -> Image PixelRGBA8
resizeRgba8 targetWidth targetHeight source
    | targetWidth == imageWidth source
        && targetHeight == imageHeight source =
            source
    | otherwise =
        generateImage sample targetWidth targetHeight
  where
    sample x y =
        let (x0, x1, xFraction) =
                sourceSample (imageWidth source) targetWidth x
            (y0, y1, yFraction) =
                sourceSample (imageHeight source) targetHeight y
            PixelRGBA8 r00 g00 b00 a00 = pixelAt source x0 y0
            PixelRGBA8 r10 g10 b10 a10 = pixelAt source x1 y0
            PixelRGBA8 r01 g01 b01 a01 = pixelAt source x0 y1
            PixelRGBA8 r11 g11 b11 a11 = pixelAt source x1 y1
        in PixelRGBA8
            (bilinear8 xFraction yFraction r00 r10 r01 r11)
            (bilinear8 xFraction yFraction g00 g10 g01 g11)
            (bilinear8 xFraction yFraction b00 b10 b01 b11)
            (bilinear8 xFraction yFraction a00 a10 a01 a11)

sourceSample :: Int -> Int -> Int -> (Int, Int, Double)
sourceSample sourceLength targetLength targetIndex =
    (lower, upper, coordinate - fromIntegral lower)
  where
    rawCoordinate =
        (fromIntegral targetIndex + 0.5)
            * fromIntegral sourceLength
            / fromIntegral targetLength
            - 0.5
    coordinate =
        max 0 (min (fromIntegral (sourceLength - 1)) rawCoordinate)
    lower = floor coordinate
    upper = min (sourceLength - 1) (lower + 1)

bilinear8
    :: Double
    -> Double
    -> Word8
    -> Word8
    -> Word8
    -> Word8
    -> Word8
bilinear8 xFraction yFraction topLeft topRight bottomLeft bottomRight =
    fromIntegral (max 0 (min 255 (round value :: Int)))
  where
    top =
        (1 - xFraction) * fromIntegral topLeft
            + xFraction * fromIntegral topRight
    bottom =
        (1 - xFraction) * fromIntegral bottomLeft
            + xFraction * fromIntegral bottomRight
    value =
        (1 - yFraction) * top + yFraction * bottom

flattenAgainstWhite :: PixelRGBA8 -> PixelRGB8
flattenAgainstWhite (PixelRGBA8 red green blue alpha) =
    PixelRGB8
        (composite red alpha)
        (composite green alpha)
        (composite blue alpha)
  where
    composite channel opacity =
        fromIntegral
            ( (fromIntegral channel * fromIntegral opacity
                + 255 * (255 - fromIntegral opacity)
                + 127
              )
                `div` 255
                :: Int
            )
