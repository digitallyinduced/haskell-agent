-- | Detect and encode a local image file for model-facing tool results.
--
-- Image types match the Responses image-input API. Magic bytes are inspected
-- rather than extensions; a second decoder check rejects corrupt data that
-- happens to have a supported prefix.
module Agent.Image.File
    ( maxImageFileBytes
    , supportedImageMime
    , readImageFileResult
    ) where

import Agent.OsPath (unsafeToFilePath)
import Agent.ToolDispatch (ToolHandlerResult(..), ToolResultImage(..))
import Codec.Picture (decodeGifImages, decodeImage)
import Control.Exception.Safe (tryIO)
import Data.Bits ((.|.), shiftL)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import System.IO (IOMode(ReadMode), withBinaryFile)
import System.OsPath (OsPath)

-- | Keep individual image results bounded even though the upstream API permits
-- a larger aggregate request. The model may inspect several images in one
-- turn, and tool-result images remain in the conversation history.
maxImageFileBytes :: Int
maxImageFileBytes = 20 * 1024 * 1024

supportedImageMime :: BS.ByteString -> Maybe Text
supportedImageMime bytes
    | prefix [0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a] = Just "image/png"
    | prefix [0xff, 0xd8, 0xff] = Just "image/jpeg"
    | prefix [0x47, 0x49, 0x46, 0x38, 0x37, 0x61]
        || prefix [0x47, 0x49, 0x46, 0x38, 0x39, 0x61] = Just "image/gif"
    | BS.length bytes >= 12
        && BS.take 4 bytes == "RIFF"
        && BS.take 4 (BS.drop 8 bytes) == "WEBP" = Just "image/webp"
    | otherwise = Nothing
  where
    prefix = (`BS.isPrefixOf` bytes) . BS.pack

-- | Read a workspace-resolved image and return it as a tool result the next
-- provider request can attach as @input_image@.
readImageFileResult
    :: OsPath
    -> Text
    -> IO (Either Text ToolHandlerResult)
readImageFileResult imagePath display =
    tryIO
        (withBinaryFile (unsafeToFilePath imagePath) ReadMode
            (\handle -> BS.hGet handle (maxImageFileBytes + 1))) >>= \case
        Left err ->
            pure . Left $
                "Failed to read " <> display <> ": " <> Text.pack (show err)
        Right bytes | BS.length bytes > maxImageFileBytes ->
            pure . Left $
                "Image is too large to view (the limit is "
                    <> Text.pack (show maxImageFileBytes)
                    <> " bytes). Downscale it first."
        Right bytes ->
            case supportedImageMime bytes of
                Nothing ->
                    pure . Left $
                        display
                            <> " is not a supported image. Supported formats: PNG, JPEG, WebP, non-animated GIF."
                Just mime
                    | not (validImage mime bytes) ->
                        pure . Left $
                            "Unable to process image: invalid or unsupported image data"
                    | otherwise ->
                        pure . Right $ ToolHandlerResult
                            { resultText = "Viewed image file: " <> display
                            , resultImages =
                                [ ToolResultImage
                                    { imageUrl =
                                        "data:" <> mime <> ";base64,"
                                            <> TextEncoding.decodeUtf8 (Base64.encode bytes)
                                    , imageDetail = Just "high"
                                    }
                                ]
                            }

validImage :: Text -> BS.ByteString -> Bool
validImage "image/webp" = validWebP
validImage "image/gif" =
    either (const False) ((== 1) . length) . decodeGifImages
validImage _ = either (const False) (const True) . decodeImage

-- JuicyPixels does not decode WebP. Validate its RIFF envelope and require a
-- standard WebP payload chunk; the provider performs the full pixel decode.
validWebP :: BS.ByteString -> Bool
validWebP bytes =
    BS.length bytes >= 20
        && BS.take 4 bytes == "RIFF"
        && BS.take 4 (BS.drop 8 bytes) == "WEBP"
        && riffSize bytes + 8 == BS.length bytes
        && BS.take 4 (BS.drop 12 bytes) `elem` ["VP8 ", "VP8L", "VP8X"]

riffSize :: BS.ByteString -> Int
riffSize bytes =
    fromIntegral (BS.index bytes 4)
        .|. (fromIntegral (BS.index bytes 5) `shiftL` 8)
        .|. (fromIntegral (BS.index bytes 6) `shiftL` 16)
        .|. (fromIntegral (BS.index bytes 7) `shiftL` 24)
