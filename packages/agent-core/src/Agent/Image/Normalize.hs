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
    , encodeJpegAtQuality
    , encodePng
    , imageHeight
    , imageWidth
    )
import Codec.Picture.Bitmap (decodeBitmapWithMetadata)
import Codec.Picture.Jpg (decodeJpegWithMetadata)
import qualified Codec.Picture.Metadata as Metadata
import Codec.Picture.Metadata.Exif
    ( ExifData(..)
    , ExifTag(TagOrientation)
    )
import Codec.Picture.Png (decodePngWithMetadata)
import Codec.Picture.Types
    ( Pixel
    , convertImage
    , dynamicMap
    , generateImage
    , pixelAt
    , pixelMap
    )
import qualified Codec.Compression.Zlib.Internal as Zlib
import Control.Monad.ST.Lazy (runST)
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

-- Decoding is intentionally gated by dimensions read from the encoded image
-- header. This bounds the largest raster JuicyPixels may allocate even when a
-- tiny compressed input advertises decompressed dimensions of several
-- gigapixels.
maximumDecodedImageSide :: Integer
maximumDecodedImageSide = 8192

maximumDecodedImagePixels :: Integer
maximumDecodedImagePixels = 25000000

maximumEncodedImageBytes :: Int
maximumEncodedImageBytes = 20 * 1024 * 1024

maximumEncodedImageBase64Characters :: Int
maximumEncodedImageBase64Characters =
    4 * ((maximumEncodedImageBytes + 2) `div` 3)

-- Account for the inflater output, JuicyPixels' decoded raster, and the
-- normalized 8-bit RGB(A) raster. This still admits ordinary 24-megapixel
-- RGB photos while preventing high-bit-depth PNGs from creating an
-- unbounded multi-raster peak.
maximumDecodedImageWorkingBytes :: Integer
maximumDecodedImageWorkingBytes = 256 * 1024 * 1024

maximumPngChunkCount :: Int
maximumPngChunkCount = 4096

minimumRetrySide :: Int
minimumRetrySide = 512

jpegQualities :: [Word8]
jpegQualities = [88, 80, 72, 64, 56, 48, 40, 32]

data NormalizedImage = NormalizedImage
    { normalizedImageMime :: !Text
    , normalizedImageBytes :: !ByteString
    }

data EncodedImageFormat
    = EncodedPng
    | EncodedJpeg
    | EncodedBitmap

data EncodedImageHeader = EncodedImageHeader
    { encodedImageFormat :: !EncodedImageFormat
    , encodedImageWidth :: !Integer
    , encodedImageHeight :: !Integer
    , encodedImageWorkingBytes :: !Integer
    , encodedPngInflatedBytes :: !(Maybe Integer)
    }

-- | Decode and, when necessary, resize or recompress an image before it is
-- serialized into a provider request. Small images remain byte-for-byte
-- unchanged. Formats without a bounded header preflight also remain unchanged
-- so normalization never makes a previously usable attachment disappear.
normalizeImageForPrompt :: Text -> ByteString -> NormalizedImage
normalizeImageForPrompt mime bytes =
    case encodedImageHeader bytes of
        Nothing -> original
        Just header
            | imageAlreadyBounded (headerDimensions header) -> original
            | not (headerSafeToDecode bytes header) -> original
            | not (encodedPayloadSafeToDecode bytes header) -> original
            | otherwise ->
                case decodeSupportedImage header.encodedImageFormat bytes of
                    Left _ -> original
                    Right (dynamicImage, metadata)
                        | not (dynamicImageSafeToProcess dynamicImage) ->
                            original
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
    imageAlreadyBounded (width, height) =
        ByteString.length bytes <= maximumImageBytes
            && width <= toInteger maximumImageSide
            && height <= toInteger maximumImageSide
            && width * height <= toInteger maximumImagePixels

headerDimensions :: EncodedImageHeader -> (Integer, Integer)
headerDimensions header =
    (header.encodedImageWidth, header.encodedImageHeight)

headerSafeToDecode :: ByteString -> EncodedImageHeader -> Bool
headerSafeToDecode bytes header =
    ByteString.length bytes <= maximumEncodedImageBytes
        && dimensionsSafeToDecode (headerDimensions header)
        && header.encodedImageWorkingBytes
            <= maximumDecodedImageWorkingBytes

encodedPayloadSafeToDecode :: ByteString -> EncodedImageHeader -> Bool
encodedPayloadSafeToDecode bytes header =
    case
        ( header.encodedImageFormat
        , header.encodedPngInflatedBytes
        )
    of
        (EncodedPng, Just expectedBytes) ->
            case pngImageDataChunks bytes of
                Nothing -> False
                Just chunks ->
                    pngInflatesToExpectedSize expectedBytes chunks
        (EncodedPng, Nothing) -> False
        (_, _) -> True

decodeSupportedImage
    :: EncodedImageFormat
    -> ByteString
    -> Either String (DynamicImage, Metadata.Metadatas)
decodeSupportedImage imageFormat =
    case imageFormat of
        EncodedPng -> decodePngWithMetadata
        EncodedJpeg -> decodeJpegWithMetadata
        EncodedBitmap -> decodeBitmapWithMetadata

dimensionsSafeToDecode :: (Integer, Integer) -> Bool
dimensionsSafeToDecode (width, height) =
    width > 0
        && height > 0
        && width <= maximumDecodedImageSide
        && height <= maximumDecodedImageSide
        && width * height <= maximumDecodedImagePixels

dynamicImageSafeToProcess :: DynamicImage -> Bool
dynamicImageSafeToProcess dynamicImage =
    dimensionsSafeToDecode
        ( toInteger (dynamicImageWidth dynamicImage)
        , toInteger (dynamicImageHeight dynamicImage)
        )

-- Only formats with a bounded dimension preflight are admitted to their
-- format-specific decoder. Other and malformed formats stay byte-for-byte
-- unchanged, and a malformed file cannot fall through to a different
-- JuicyPixels decoder whose allocation was not preflighted.
encodedImageHeader :: ByteString -> Maybe EncodedImageHeader
encodedImageHeader bytes
    | ByteString.take 8 bytes == pngSignature =
        pngHeader bytes
    | ByteString.take 2 bytes == jpegSignature =
        fromDimensions EncodedJpeg (jpegDimensions bytes)
    | ByteString.take 2 bytes == "BM" =
        fromDimensions EncodedBitmap (bitmapDimensions bytes)
    | otherwise = Nothing
  where
    pngSignature = "\137PNG\r\n\SUB\n"
    jpegSignature = "\255\216"
    fromDimensions imageFormat dimensions = do
        (width, height) <- dimensions
        pure EncodedImageHeader
            { encodedImageFormat = imageFormat
            , encodedImageWidth = width
            , encodedImageHeight = height
            , encodedImageWorkingBytes = width * height * 12
            , encodedPngInflatedBytes = Nothing
            }

pngHeader :: ByteString -> Maybe EncodedImageHeader
pngHeader bytes = do
    guardMaybe (ByteString.length bytes >= 33)
    chunkLength <- word32Be bytes 8
    guardMaybe (chunkLength == 13)
    guardMaybe (ByteString.take 4 (ByteString.drop 12 bytes) == "IHDR")
    width <- word32Be bytes 16
    height <- word32Be bytes 20
    bitDepth <- byteAt bytes 24
    colorType <- byteAt bytes 25
    compressionMethod <- byteAt bytes 26
    filterMethod <- byteAt bytes 27
    interlaceMethod <- byteAt bytes 28
    guardMaybe (compressionMethod == 0)
    guardMaybe (filterMethod == 0)
    guardMaybe (interlaceMethod == 0 || interlaceMethod == 1)
    (samplesPerPixel, decodedBytesPerPixel) <-
        pngColorLayout bitDepth colorType
    (validWidth, validHeight) <- validDimensions width height
    let pixelCount = validWidth * validHeight
        inflatedBytes =
            pngInflatedByteCount
                validWidth
                validHeight
                samplesPerPixel
                bitDepth
                interlaceMethod
        workingBytes =
            inflatedBytes
                + pixelCount * (decodedBytesPerPixel + 4)
    pure EncodedImageHeader
        { encodedImageFormat = EncodedPng
        , encodedImageWidth = validWidth
        , encodedImageHeight = validHeight
        , encodedImageWorkingBytes = workingBytes
        , encodedPngInflatedBytes = Just inflatedBytes
        }

pngColorLayout
    :: Integer
    -> Integer
    -> Maybe (Integer, Integer)
pngColorLayout bitDepth colorType =
    case colorType of
        0 -> do
            guardMaybe (bitDepth `elem` [1, 2, 4, 8, 16])
            pure (1, if bitDepth == 16 then 2 else 1)
        2 -> do
            guardMaybe (bitDepth == 8 || bitDepth == 16)
            pure (3, if bitDepth == 16 then 6 else 3)
        3 -> do
            guardMaybe (bitDepth `elem` [1, 2, 4, 8])
            pure (1, 4)
        4 -> do
            guardMaybe (bitDepth == 8 || bitDepth == 16)
            pure (2, if bitDepth == 16 then 4 else 2)
        6 -> do
            guardMaybe (bitDepth == 8 || bitDepth == 16)
            pure (4, if bitDepth == 16 then 8 else 4)
        _ -> Nothing

pngInflatedByteCount
    :: Integer
    -> Integer
    -> Integer
    -> Integer
    -> Integer
    -> Integer
pngInflatedByteCount width height samples bitDepth interlaceMethod
    | interlaceMethod == 0 =
        height * (1 + scanlineBytes width)
    | otherwise =
        sum
            [ passHeight * (1 + scanlineBytes passWidth)
            | (xStart, yStart, xStep, yStep) <- adam7Passes
            , let passWidth = passLength width xStart xStep
            , let passHeight = passLength height yStart yStep
            , passWidth > 0
            , passHeight > 0
            ]
  where
    scanlineBytes passWidth =
        ceilingDivide
            (passWidth * samples * bitDepth)
            8
    adam7Passes =
        [ (0, 0, 8, 8)
        , (4, 0, 8, 8)
        , (0, 4, 4, 8)
        , (2, 0, 4, 4)
        , (0, 2, 2, 4)
        , (1, 0, 2, 2)
        , (0, 1, 1, 2)
        ]
    passLength total start step
        | total <= start = 0
        | otherwise = ceilingDivide (total - start) step

ceilingDivide :: Integer -> Integer -> Integer
ceilingDivide numerator denominator =
    (numerator + denominator - 1) `div` denominator

pngImageDataChunks :: ByteString -> Maybe [ByteString]
pngImageDataChunks bytes =
    go 33 0 False False []
  where
    totalBytes = toInteger (ByteString.length bytes)

    go offset chunkCount sawImageData endedImageData chunks = do
        guardMaybe (chunkCount < maximumPngChunkCount)
        guardMaybe (offset + 12 <= totalBytes)
        chunkLength <- word32Be bytes (fromInteger offset)
        let dataOffset = offset + 8
            nextOffset = dataOffset + chunkLength + 4
        guardMaybe (nextOffset <= totalBytes)
        let chunkType =
                ByteString.take 4
                    (ByteString.drop (fromInteger (offset + 4)) bytes)
            chunkData =
                ByteString.take (fromInteger chunkLength)
                    (ByteString.drop (fromInteger dataOffset) bytes)
        case chunkType of
            "IDAT" -> do
                guardMaybe (not endedImageData)
                go
                    nextOffset
                    (chunkCount + 1)
                    True
                    False
                    (chunkData : chunks)
            "IEND" -> do
                guardMaybe (chunkLength == 0)
                guardMaybe sawImageData
                guardMaybe (nextOffset == totalBytes)
                pure (reverse chunks)
            _ ->
                go
                    nextOffset
                    (chunkCount + 1)
                    sawImageData
                    (endedImageData || sawImageData)
                    chunks

pngInflatesToExpectedSize :: Integer -> [ByteString] -> Bool
pngInflatesToExpectedSize expectedBytes chunks =
    runST $
        drive
            (Zlib.decompressST
                Zlib.zlibFormat
                Zlib.defaultDecompressParams)
            chunks
            False
            0
  where
    drive stream remainingChunks endOfInputSupplied bytesSeen =
        case stream of
            Zlib.DecompressInputRequired supplyInput ->
                case nextNonEmptyChunk remainingChunks of
                    Just (inputChunk, rest) -> do
                        nextStream <- supplyInput inputChunk
                        drive
                            nextStream
                            rest
                            endOfInputSupplied
                            bytesSeen
                    Nothing
                        | endOfInputSupplied ->
                            pure False
                        | otherwise -> do
                            nextStream <- supplyInput ByteString.empty
                            drive nextStream [] True bytesSeen
            Zlib.DecompressOutputAvailable outputChunk next -> do
                let nextBytes =
                        bytesSeen
                            + toInteger (ByteString.length outputChunk)
                if nextBytes > expectedBytes
                    then pure False
                    else do
                        nextStream <- next
                        drive
                            nextStream
                            remainingChunks
                            endOfInputSupplied
                            nextBytes
            Zlib.DecompressStreamEnd unconsumed ->
                pure
                    ( ByteString.null unconsumed
                        && all ByteString.null remainingChunks
                        && bytesSeen == expectedBytes
                    )
            Zlib.DecompressStreamError _ ->
                pure False

    nextNonEmptyChunk = \case
        [] -> Nothing
        chunk : rest
            | ByteString.null chunk ->
                nextNonEmptyChunk rest
            | otherwise ->
                Just (chunk, rest)

bitmapDimensions :: ByteString -> Maybe (Integer, Integer)
bitmapDimensions bytes = do
    headerSize <- word32Le bytes 14
    case headerSize of
        12 -> do
            width <- word16Le bytes 18
            height <- word16Le bytes 20
            validDimensions width height
        size
            | size >= 40 -> do
                width <- signedWord32Le bytes 18
                height <- signedWord32Le bytes 22
                validDimensions width (abs height)
        _ -> Nothing

jpegDimensions :: ByteString -> Maybe (Integer, Integer)
jpegDimensions bytes = findMarker 2
  where
    scanLimit = min (ByteString.length bytes) (1024 * 1024)

    findMarker offset
        | offset + 1 >= scanLimit = Nothing
        | ByteString.index bytes offset /= 255 =
            findMarker (offset + 1)
        | otherwise =
            let markerOffset = skipFillBytes offset
            in if markerOffset >= scanLimit
                then Nothing
                else inspectMarker markerOffset

    skipFillBytes offset
        | offset < scanLimit
            && ByteString.index bytes offset == 255 =
                skipFillBytes (offset + 1)
        | otherwise = offset

    inspectMarker markerOffset =
        case ByteString.index bytes markerOffset of
            0 -> findMarker (markerOffset + 1)
            marker
                | jpegStartOfFrame marker -> do
                    segmentLength <- word16Be bytes (markerOffset + 1)
                    guardMaybe (segmentLength >= 7)
                    height <- word16Be bytes (markerOffset + 4)
                    width <- word16Be bytes (markerOffset + 6)
                    validDimensions width height
                | marker == 217 || marker == 218 -> Nothing
                | jpegStandaloneMarker marker ->
                    findMarker (markerOffset + 1)
                | otherwise -> do
                    segmentLength <- word16Be bytes (markerOffset + 1)
                    guardMaybe (segmentLength >= 2)
                    let next =
                            markerOffset + 1 + fromInteger segmentLength
                    guardMaybe (next > markerOffset && next <= scanLimit)
                    findMarker next

    jpegStartOfFrame marker =
        marker `elem`
            [ 192, 193, 194, 195
            , 197, 198, 199
            , 201, 202, 203
            , 205, 206, 207
            ]

    jpegStandaloneMarker marker =
        marker == 1
            || marker == 216
            || (marker >= 208 && marker <= 215)

validDimensions :: Integer -> Integer -> Maybe (Integer, Integer)
validDimensions width height = do
    guardMaybe (width > 0 && height > 0)
    pure (width, height)

word16Be :: ByteString -> Int -> Maybe Integer
word16Be bytes offset = do
    first <- byteAt bytes offset
    second <- byteAt bytes (offset + 1)
    pure (first * 256 + second)

word16Le :: ByteString -> Int -> Maybe Integer
word16Le bytes offset = do
    first <- byteAt bytes offset
    second <- byteAt bytes (offset + 1)
    pure (first + second * 256)

word32Be :: ByteString -> Int -> Maybe Integer
word32Be bytes offset = do
    first <- word16Be bytes offset
    second <- word16Be bytes (offset + 2)
    pure (first * 65536 + second)

word32Le :: ByteString -> Int -> Maybe Integer
word32Le bytes offset = do
    first <- word16Le bytes offset
    second <- word16Le bytes (offset + 2)
    pure (first + second * 65536)

signedWord32Le :: ByteString -> Int -> Maybe Integer
signedWord32Le bytes offset = do
    unsigned <- word32Le bytes offset
    pure
        (if unsigned >= 2147483648
            then unsigned - 4294967296
            else unsigned)

byteAt :: ByteString -> Int -> Maybe Integer
byteAt bytes offset
    | offset < 0 || offset >= ByteString.length bytes = Nothing
    | otherwise = Just (fromIntegral (ByteString.index bytes offset))

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
    let encodedPayload = Text.drop 1 payloadWithComma
    guardMaybe
        (Text.length encodedPayload
            <= maximumEncodedImageBase64Characters)
    let mime =
            Text.drop 5
                (Text.dropEnd (Text.length base64Suffix) metadata)
    guardMaybe (not (Text.null mime))
    bytes <-
        either (const Nothing) Just
            (Base64.decode
                (TextEncoding.encodeUtf8 encodedPayload))
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
            alpha =
                bilinearDouble xFraction yFraction
                    (fromIntegral a00)
                    (fromIntegral a10)
                    (fromIntegral a01)
                    (fromIntegral a11)
            premultiplied channel00 channel10 channel01 channel11 =
                bilinearDouble xFraction yFraction
                    (fromIntegral channel00 * fromIntegral a00)
                    (fromIntegral channel10 * fromIntegral a10)
                    (fromIntegral channel01 * fromIntegral a01)
                    (fromIntegral channel11 * fromIntegral a11)
            channel channel00 channel10 channel01 channel11
                | alpha <= 0 = 0
                | otherwise =
                    word8FromDouble
                        (premultiplied
                            channel00 channel10 channel01 channel11
                            / alpha)
        in if word8FromDouble alpha == 0
            then PixelRGBA8 0 0 0 0
            else PixelRGBA8
                (channel r00 r10 r01 r11)
                (channel g00 g10 g01 g11)
                (channel b00 b10 b01 b11)
                (word8FromDouble alpha)

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
    word8FromDouble $
        bilinearDouble xFraction yFraction
            (fromIntegral topLeft)
            (fromIntegral topRight)
            (fromIntegral bottomLeft)
            (fromIntegral bottomRight)

bilinearDouble
    :: Double
    -> Double
    -> Double
    -> Double
    -> Double
    -> Double
    -> Double
bilinearDouble
    xFraction
    yFraction
    topLeft
    topRight
    bottomLeft
    bottomRight =
        (1 - yFraction) * top + yFraction * bottom
  where
    top =
        (1 - xFraction) * topLeft + xFraction * topRight
    bottom =
        (1 - xFraction) * bottomLeft + xFraction * bottomRight

word8FromDouble :: Double -> Word8
word8FromDouble value =
    fromIntegral (max 0 (min 255 (round value :: Int)))

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
