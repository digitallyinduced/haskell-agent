-- | Transport-independent validation of bounded semantic computer responses.
-- Transport adapters retain responsibility for session identity and framing.
module Agent.ComputerUse.Semantic
    ( SemanticComputerResponse(..)
    , SemanticComputerResult(..)
    , validateSemanticComputerResponse
    ) where

import Agent.ComputerUse.Accessibility
import Agent.ComputerUse.Protocol
import Agent.ToolDispatch (ToolResultImage(..))
import Codec.Picture (DynamicImage, dynamicMap, imageHeight, imageWidth, pixelAt)
import Codec.Picture.Jpg (decodeJpeg)
import Codec.Picture.Png (decodePng)
import Control.Exception (evaluate)
import Control.Exception.Safe (tryAny)
import Control.Monad (guard, when)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Bits ((.&.), complement, shiftR, xor)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Word (Word8, Word32)

data SemanticComputerResponse = SemanticComputerResponse
    { semanticResultBytes :: !BS.ByteString
    , semanticAccessibilityBytes :: !BS.ByteString
    , semanticImageBytes :: !BS.ByteString
    -- | 0: absent, 1: PNG, 2: JPEG. Unknown formats are rejected.
    , semanticImageFormat :: !Int
    } deriving (Eq, Show)

data SemanticComputerResult = SemanticComputerResult
    { semanticResultValue :: !Aeson.Value
    , semanticAccessibility :: !(Maybe AccessibilityObservation)
    , semanticImage :: !(Maybe ToolResultImage)
    } deriving (Eq, Show)

validateSemanticComputerResponse
    :: SemanticComputerRequest
    -> AccessibilityDeltaState
    -> SemanticComputerResponse
    -> IO (Either Text (SemanticComputerResult, AccessibilityDeltaState))
validateSemanticComputerResponse request state response =
    case metadata of
        Left err -> pure (Left err)
        Right object -> do
            decodedImage <- decodeImage
                (semanticComputerRequestWantsScreenshot request)
                response.semanticImageFormat response.semanticImageBytes
            pure do
                image <- decodedImage
                let (observation, successor) = case request of
                        ListComputerTargets -> (Nothing, state)
                        _ -> let (value, next) =
                                    decodeAccessibility response.semanticAccessibilityBytes state
                             in (Just value, next)
                    objectWithAccessibility = maybe object
                        (\value -> KeyMap.insert "accessibility_state"
                            (Aeson.toJSON value) object) observation
                    freshEvidence = maybe False accessibilityIsFresh observation
                        || maybe False (const True) image
                result <- insertDefaultVerdict request freshEvidence objectWithAccessibility
                pure (SemanticComputerResult (Aeson.Object result) observation image, successor)
  where
    metadata = do
        when (BS.length response.semanticResultBytes > 1024 * 1024) $
            Left "The native computer result exceeds its bounded buffer."
        when (BS.length response.semanticAccessibilityBytes > 512 * 1024) $
            Left "The native computer accessibility exceeds its bounded buffer."
        when (BS.length response.semanticImageBytes > 16 * 1024 * 1024) $
            Left "The native computer image exceeds its bounded buffer."
        value <- case Aeson.eitherDecodeStrict' response.semanticResultBytes of
            Left err -> Left
                ("The native computer host returned invalid result JSON: " <> Text.pack err)
            Right decoded -> Right decoded
        object <- case value of
            Aeson.Object fields -> Right fields
            _ -> Left "The native computer host result must be a JSON object."
        when (containsDataImage value) $
            Left "The native computer host embedded screenshot data in result JSON."
        when (request == ListComputerTargets
                && not (BS.null response.semanticAccessibilityBytes)) $
            Left "The native computer host returned accessibility data for list_targets."
        pure object
    accessibilityIsFresh = \case
        AccessibilityFull{} -> True
        AccessibilityDelta{} -> True
        AccessibilityUnavailable{} -> False

insertDefaultVerdict
    :: SemanticComputerRequest -> Bool -> Aeson.Object -> Either Text Aeson.Object
insertDefaultVerdict request freshEvidence object
    | Just value <- KeyMap.lookup verdictKey object =
        case (Aeson.fromJSON value :: Aeson.Result ComputerUseVerdict) of
            Aeson.Success verdict -> do
                when (verdict.computerUseVerdictFreshObservation && not freshEvidence) $
                    Left
                        "The native computer host verdict claims a fresh observation without returning fresh accessibility or image evidence."
                case request of
                    ActOnComputerTarget{}
                        | ComputerUseObservation <- verdict.computerUseVerdictEffect ->
                            Left "The native computer host verdict cannot classify an input request as an observation."
                    _ -> Right ()
                Right object
            Aeson.Error err ->
                Left ("The native computer host returned an invalid verdict: " <> Text.pack err)
    | ActOnComputerTarget{} <- request =
        Right (KeyMap.insert verdictKey (Aeson.toJSON verdict) object)
    | otherwise = Right object
  where
    verdictKey = Key.fromText computerUseVerdictField
    verdict = case KeyMap.lookup "ok" object of
        Just (Aeson.Bool False) -> suspectedNoopComputerUseVerdict freshEvidence
        _ -> unverifiedComputerUseVerdict freshEvidence

decodeAccessibility
    :: BS.ByteString -> AccessibilityDeltaState
    -> (AccessibilityObservation, AccessibilityDeltaState)
decodeAccessibility bytes state
    | BS.null bytes = unavailableAccessibilityObservation
        "Native accessibility snapshot unavailable." state
    | otherwise = case decodeAccessibilitySnapshot bytes of
        Left err -> unavailableAccessibilityObservation
            ("The native computer host returned invalid accessibility JSON: " <> err) state
        Right snapshot -> advanceAccessibilityObservation state snapshot

decodeImage :: Bool -> Int -> BS.ByteString -> IO (Either Text (Maybe ToolResultImage))
decodeImage includeScreenshot format bytes
    | BS.null bytes && format == 0 = pure (Right Nothing)
    | not includeScreenshot =
        pure (Left "The native computer host returned an unrequested screenshot.")
    | BS.null bytes =
        pure (Left "The native computer host returned an empty screenshot.")
    | otherwise = do
        decodedMime <- imageMime format bytes
        pure do
            mime <- decodedMime
            pure (Just (ToolResultImage
                ("data:" <> mime <> ";base64,"
                    <> TextEncoding.decodeUtf8 (Base64.encode bytes))
                Nothing))

imageMime :: Int -> BS.ByteString -> IO (Either Text Text)
imageMime format bytes = case format of
    1 -> validateDecodedImage "image/png"
        "The native computer host returned malformed PNG data." (validPng bytes)
    2 -> validateDecodedImage "image/jpeg"
        "The native computer host returned malformed JPEG data." (validJpeg bytes)
    _ -> pure (Left "The native computer host returned an unsupported image format.")

validateDecodedImage :: Text -> Text -> Bool -> IO (Either Text Text)
validateDecodedImage mime malformed valid = do
    attempted <- tryAny (evaluate valid)
    pure case attempted of
        Right True -> Right mime
        Right False -> Left malformed
        Left _ -> Left malformed

containsDataImage :: Aeson.Value -> Bool
containsDataImage = \case
    Aeson.Object object -> any containsDataImage object
    Aeson.Array values -> any containsDataImage values
    Aeson.String value -> "data:image/" `Text.isPrefixOf` Text.toLower value
    _ -> False

validPng :: BS.ByteString -> Bool
validPng bytes = case pngEnvelope bytes of
    Nothing -> False
    Just dimensions -> decodedImageMatches dimensions (decodePng bytes)

pngEnvelope :: BS.ByteString -> Maybe (Int, Int)
pngEnvelope bytes = do
    guard (BS.length bytes >= 45)
    guard (BS.take 8 bytes == BS.pack [137, 80, 78, 71, 13, 10, 26, 10])
    validChunks True False Nothing (BS.drop 8 bytes)
  where
    validChunks firstChunk sawImageData dimensions remaining = do
        guard (BS.length remaining >= 12)
        guard (chunkLength <= BS.length remaining - 12)
        guard (chunkChecksum == crc32 checksumInput)
        if firstChunk
            then do
                guard (chunkType == "IHDR")
                guard (chunkLength == 13)
                headerDimensions <- validHeader chunkData
                validChunks False False (Just headerDimensions) rest
            else case chunkType of
                "IHDR" -> Nothing
                "IDAT" -> validChunks False True dimensions rest
                "IEND" -> do
                    guard (chunkLength == 0)
                    guard sawImageData
                    guard (BS.null rest)
                    dimensions
                _ -> validChunks False sawImageData dimensions rest
      where
        chunkLength = word32At remaining 0
        chunkType = BS.take 4 (BS.drop 4 remaining)
        chunkData = BS.take chunkLength (BS.drop 8 remaining)
        checksumInput = BS.take (4 + chunkLength) (BS.drop 4 remaining)
        chunkChecksum = fromIntegral (word32At remaining (8 + chunkLength))
        rest = BS.drop (12 + chunkLength) remaining
    validHeader header = do
        guard (BS.length header == 13)
        let width = word32At header 0
            height = word32At header 4
        guard (dimensionsSafeToDecode (width, height))
        guard (BS.index header 10 == 0)
        guard (BS.index header 11 == 0)
        guard (BS.index header 12 <= 1)
        pure (width, height)

validJpeg :: BS.ByteString -> Bool
validJpeg bytes = case jpegDimensions bytes of
    Nothing -> False
    Just dimensions -> decodedImageMatches dimensions (decodeJpeg bytes)

jpegDimensions :: BS.ByteString -> Maybe (Int, Int)
jpegDimensions bytes = do
    guard (BS.length bytes >= 14)
    guard (BS.take 2 bytes == BS.pack [255, 216])
    guard (BS.drop (BS.length bytes - 2) bytes == BS.pack [255, 217])
    validSegments Nothing (BS.drop 2 bytes)
  where
    validSegments dimensions remaining =
        case nextMarker remaining of
            Nothing -> Nothing
            Just (marker, afterMarker)
                | marker == 0xd9 -> Nothing
                | standaloneMarker marker -> validSegments dimensions afterMarker
                | BS.length afterMarker < 2 -> Nothing
                | segmentLength < 2 || segmentLength > BS.length afterMarker -> Nothing
                | marker == 0xda -> dimensions
                | startOfFrame marker -> do
                    guard (segmentLength >= 8)
                    let height = word16At afterMarker 3
                        width = word16At afterMarker 5
                    guard (dimensionsSafeToDecode (width, height))
                    validSegments (Just (width, height)) (BS.drop segmentLength afterMarker)
                | otherwise -> validSegments dimensions (BS.drop segmentLength afterMarker)
              where
                segmentLength = word16At afterMarker 0
    nextMarker remaining =
        case BS.elemIndex 255 remaining of
            Nothing -> Nothing
            Just markerStart ->
                case BS.uncons (BS.dropWhile (== 255) (BS.drop markerStart remaining)) of
                    Nothing -> Nothing
                    Just (0, rest) -> nextMarker rest
                    Just (marker, rest) -> Just (marker, rest)
    standaloneMarker marker =
        marker == 0x01 || marker == 0xd8 || (marker >= 0xd0 && marker <= 0xd7)
    startOfFrame marker =
        marker >= 0xc0 && marker <= 0xcf && marker `notElem` [0xc4, 0xc8, 0xcc]

decodedImageMatches :: (Int, Int) -> Either String DynamicImage -> Bool
decodedImageMatches expected = \case
    Left _ -> False
    Right image -> dynamicMap
        (\raster ->
            let width = imageWidth raster
                height = imageHeight raster
            in (width, height) == expected
                && (pixelAt raster (width - 1) (height - 1) `seq` True)) image

dimensionsSafeToDecode :: (Int, Int) -> Bool
dimensionsSafeToDecode (width, height) =
    width > 0 && height > 0 && width <= 8192 && height <= 8192
        && toInteger width * toInteger height <= 25000000

crc32 :: BS.ByteString -> Word32
crc32 = complement . BS.foldl' update maxBound
  where
    update :: Word32 -> Word8 -> Word32
    update checksum byte = advance 8 (checksum `xor` fromIntegral byte)
    advance :: Int -> Word32 -> Word32
    advance 0 value = value
    advance count value = advance (count - 1)
        (if value .&. 1 == 1
            then (value `shiftR` 1) `xor` 0xedb88320
            else value `shiftR` 1)

word16At :: BS.ByteString -> Int -> Int
word16At bytes offset =
    fromIntegral (BS.index bytes offset) * 256
        + fromIntegral (BS.index bytes (offset + 1))

word32At :: BS.ByteString -> Int -> Int
word32At bytes offset =
    fromIntegral (BS.index bytes offset) * 16777216
        + fromIntegral (BS.index bytes (offset + 1)) * 65536
        + fromIntegral (BS.index bytes (offset + 2)) * 256
        + fromIntegral (BS.index bytes (offset + 3))
