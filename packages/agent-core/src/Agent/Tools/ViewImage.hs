-- | Model-facing local image inspection.
--
-- Unlike 'Agent.Tools.ShowImage', this tool puts the selected image in the
-- next provider request so a vision-capable model can inspect it.
module Agent.Tools.ViewImage
    ( viewImageTool
    , viewImageToolName
    ) where

import Agent.Json.Decode (Decoder)
import Agent.OsPath (fromText, unsafeToFilePath)
import Agent.ToolArgs (objectArgs, optText, reqText)
import Agent.ToolDispatch
    ( ToolCall(..)
    , ToolHandlerResult(..)
    , ToolResultImage(..)
    , decodeToolArguments
    , toolArgumentsValue
    , typedRichToolWithCall
    )
import Agent.ToolDSL (PropertySchema(..), PropertyType(..))
import Agent.Tools.FileSystem (displayPathInWorkspace, resolveForRead)
import Agent.Tools.Scheduling
    ( ToolAccess(..)
    , ToolResource(..)
    , ToolResourceClaim(..)
    )
import Agent.Tools.Types
    ( AppTool
    , ToolEnv
    , ToolExecutionPolicy(..)
    , jsonTool
    , withToolResourceClaims
    )
import Codec.Picture (decodeGifImages, decodeImage)
import Control.Exception.Safe (tryIO)
import Data.Bits ((.|.), shiftL)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import System.Directory.OsPath (doesFileExist)
import System.IO (IOMode(ReadMode), withBinaryFile)

viewImageToolName :: Text
viewImageToolName = "view_image"

-- | Keep individual image results bounded even though the upstream API permits
-- a larger aggregate request. The model may call this tool repeatedly in one
-- turn, and tool-result images remain in the conversation history.
maxViewImageBytes :: Int
maxViewImageBytes = 20 * 1024 * 1024

data ViewImageArgs = ViewImageArgs
    { path :: !Text
    , detail :: !(Maybe Text)
    }

viewImageArgsDecoder :: Decoder ViewImageArgs
viewImageArgsDecoder = objectArgs \object ->
    ViewImageArgs
        <$> reqText object "path"
        <*> optText object "detail"

viewImageTool :: ToolEnv -> AppTool
viewImageTool env =
    withToolResourceClaims (viewImageClaims env) $
        jsonTool viewImageToolName viewImageDescription
            [ PropertySchema "path" PropertyString True $ Just
                "Local filesystem path to an image file."
            ]
            True
            ParallelSafe
            (typedRichToolWithCall viewImageToolName viewImageArgsDecoder
                (runViewImage env))

viewImageDescription :: Text
viewImageDescription =
    "View a local image file from the filesystem when visual inspection is needed. \
    \Use this for images already available on disk. The image is added to your context."

viewImageClaims
    :: ToolEnv
    -> ToolCall
    -> IO (Either Text [ToolResourceClaim])
viewImageClaims env call =
    case decodeToolArguments viewImageArgsDecoder (toolArgumentsValue call.arguments) of
        Left err -> pure (Left err)
        Right args ->
            resolveForRead env (fromText args.path)
                >>= pure . fmap (\path -> [ToolResourceClaim ToolRead (ToolPath path)])

runViewImage
    :: ToolEnv
    -> ToolCall
    -> ViewImageArgs
    -> IO (Either Text ToolHandlerResult)
runViewImage env _call args =
    case args.detail of
        Just detail | detail /= "high" ->
            pure . Left $
                "view_image.detail only supports `high` for this model, got `"
                    <> detail <> "`"
        _ ->
            resolveForRead env (fromText args.path) >>= \case
                Left err -> pure (Left err)
                Right imagePath -> do
                    display <- displayPathInWorkspace env imagePath
                    doesFileExist imagePath >>= \case
                        False -> pure (Left ("File not found: " <> display))
                        True -> readAndEncode display imagePath
  where
    readAndEncode display imagePath =
        tryIO
            (withBinaryFile (unsafeToFilePath imagePath) ReadMode
                (\handle -> BS.hGet handle (maxViewImageBytes + 1))) >>= \case
            Left err ->
                pure . Left $
                    "Failed to read " <> display <> ": " <> Text.pack (show err)
            Right bytes | BS.length bytes > maxViewImageBytes ->
                pure . Left $
                    "Image is too large to view (the limit is "
                        <> Text.pack (show maxViewImageBytes)
                        <> " bytes). Downscale it first."
            Right bytes ->
                case supportedImageMime bytes of
                    Nothing ->
                        pure . Left $
                            display <> " is not a supported image. Supported formats: PNG, JPEG, WebP, non-animated GIF."
                    Just mime
                        | not (validImage mime bytes) ->
                            pure . Left $ "Unable to process image: invalid or unsupported image data"
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

-- | Image types supported by the Responses image-input API. We inspect magic
-- bytes rather than extensions; the full decoder check above rejects corrupt
-- data that happens to have one of these prefixes.
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
