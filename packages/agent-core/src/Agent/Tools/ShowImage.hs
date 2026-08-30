-- | Host-mediated inline image display for model tool calls.
--
-- The model names an image file; the trusted host decides how to present it
-- (Kitty/iTerm2 graphics, an ANSI approximation, a chat photo, …). The image
-- is shown to the user only. It is never attached to the model context, so
-- the tool result stays a short text summary.
module Agent.Tools.ShowImage
    ( ImageDisplayHooks(..)
    , ImageDisplayRequest(..)
    , showImageTool
    , showImageToolName
    , sniffImageMime
    , maxShowImageBytes
    ) where

import Agent.Json.Decode (Decoder)
import Agent.Loop (ImageAttachment(..))
import Agent.OsPath (fromText, unsafeToFilePath)
import Agent.ToolArgs (objectArgs, optText, reqText)
import Agent.ToolDSL (PropertySchema(..), PropertyType(..))
import Agent.ToolDispatch
    ( ToolCall(..)
    , decodeToolArguments
    , toolArgumentsValue
    , typedToolWithCall
    )
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
import Control.Exception.Safe (tryIO)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory.OsPath (doesFileExist, getFileSize)

-- | One image the model asked the host to present.
data ImageDisplayRequest = ImageDisplayRequest
    { displayCallId :: !Text
      -- ^ The originating tool call, so a transcript UI can attach the image
      -- to the matching tool block.
    , displayPath :: !Text
      -- ^ Workspace-relative display path of the source file.
    , displayCaption :: !(Maybe Text)
    , displayImage :: !ImageAttachment
    }
    deriving (Eq, Show)

-- | Host presentation used by 'showImageTool'. A 'Left' reports why the host
-- could not present the image; the tool surfaces it to the model.
newtype ImageDisplayHooks = ImageDisplayHooks
    { showImage :: ImageDisplayRequest -> IO (Either Text ())
    }

showImageToolName :: Text
showImageToolName = "show_image"

-- | Larger files are rejected before they are read: terminal graphics
-- protocols transfer the encoded image inline, so multi-megabyte files stall
-- the UI without looking better.
maxShowImageBytes :: Int
maxShowImageBytes = 20 * 1024 * 1024

data ShowImageArgs = ShowImageArgs
    { path :: !Text
    , caption :: !(Maybe Text)
    }
    deriving (Eq, Show)

showImageArgsDecoder :: Decoder ShowImageArgs
showImageArgsDecoder = objectArgs \object ->
    ShowImageArgs
        <$> reqText object "path"
        <*> optText object "caption"

showImageTool :: ToolEnv -> ImageDisplayHooks -> AppTool
showImageTool env hooks =
    withToolResourceClaims (showImageClaims env) $
        jsonTool showImageToolName showImageDescription
            [ PropertySchema "path" PropertyString True $ Just
                "Path of the image file to display. Relative paths use the workspace; absolute paths may resolve within the workspace or session temp directory. PNG, JPEG, GIF, BMP, and TIFF are supported."
            , PropertySchema "caption" PropertyString False $ Just
                "Optional short caption shown with the image."
            ]
            True
            ParallelSafe
            (typedToolWithCall showImageToolName showImageArgsDecoder
                (runShowImage env hooks))

showImageDescription :: Text
showImageDescription =
    "Display an image file inline to the user in the conversation.\n\
    \\n\
    \- Use it to present screenshots, rendered previews, charts, icons, or other generated images\n\
    \- Supported formats: PNG, JPEG, GIF, BMP, TIFF. Convert SVG, PDF, or WebP to PNG first (for example with rsvg-convert or ImageMagick)\n\
    \- The image is shown to the user only; it is not added to your own context. Use read_file or the user's description when you need to inspect image contents yourself"

showImageClaims
    :: ToolEnv
    -> ToolCall
    -> IO (Either Text [ToolResourceClaim])
showImageClaims env call =
    case
        decodeToolArguments showImageArgsDecoder (toolArgumentsValue call.arguments)
            :: Either Text ShowImageArgs
    of
        Left err -> pure (Left err)
        Right args ->
            resolveForRead env (fromText args.path)
                >>= pure . fmap
                    (\path -> [ToolResourceClaim ToolRead (ToolPath path)])

runShowImage
    :: ToolEnv
    -> ImageDisplayHooks
    -> ToolCall
    -> ShowImageArgs
    -> IO (Either Text Text)
runShowImage env hooks call args =
    resolveForRead env (fromText args.path) >>= \case
        Left err -> pure (Left err)
        Right path -> do
            display <- displayPathInWorkspace env path
            exists <- doesFileExist path
            if not exists
                then pure (Left ("File not found: " <> display))
                else do
                    size <- getFileSize path
                    if size > toInteger maxShowImageBytes
                        then pure $ Left $
                            "Image is too large to display ("
                                <> formatByteCount (fromInteger size)
                                <> "; the limit is "
                                <> formatByteCount maxShowImageBytes
                                <> "). Downscale it first."
                        else readImage display path
  where
    readImage display path =
        tryIO (BS.readFile (unsafeToFilePath path)) >>= \case
            Left err ->
                pure (Left ("Failed to read " <> display <> ": " <> Text.pack (show err)))
            Right bytes ->
                case sniffImageMime bytes of
                    Nothing ->
                        pure $ Left $
                            display
                                <> " is not a supported image. Supported formats: PNG, JPEG, GIF, BMP, TIFF. Convert other formats (SVG, PDF, WebP) to PNG first."
                    Just mime -> do
                        let caption = normalizeCaption args.caption
                            request = ImageDisplayRequest
                                { displayCallId = call.callId
                                , displayPath = display
                                , displayCaption = caption
                                , displayImage = ImageAttachment
                                    { imageMime = mime
                                    , imageBytes = bytes
                                    }
                                }
                        hooks.showImage request >>= \case
                            Left err ->
                                pure (Left ("Could not display " <> display <> ": " <> err))
                            Right () ->
                                pure $ Right $
                                    Text.intercalate "\n" $
                                        [ "Displayed "
                                            <> display
                                            <> " to the user ("
                                            <> mime
                                            <> ", "
                                            <> formatByteCount (BS.length bytes)
                                            <> ")."
                                        ]
                                            <> [ "Caption: " <> text
                                               | Just text <- [caption]
                                               ]
                                            <> [ "The image is visible to the user only; it was not added to your context."
                                               ]

normalizeCaption :: Maybe Text -> Maybe Text
normalizeCaption raw =
    case Text.strip <$> raw of
        Just text | not (Text.null text) -> Just text
        _ -> Nothing

-- | Detect the encoded format from its magic bytes. File extensions are
-- ignored: models routinely write PNG data to @.jpg@ paths and vice versa.
sniffImageMime :: BS.ByteString -> Maybe Text
sniffImageMime bytes
    | hasPrefix [0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a] = Just "image/png"
    | hasPrefix [0xff, 0xd8, 0xff] = Just "image/jpeg"
    | hasPrefix [0x47, 0x49, 0x46, 0x38, 0x37, 0x61]
        || hasPrefix [0x47, 0x49, 0x46, 0x38, 0x39, 0x61] = Just "image/gif"
    | hasPrefix [0x42, 0x4d] && BS.length bytes >= 14 = Just "image/bmp"
    | hasPrefix [0x49, 0x49, 0x2a, 0x00]
        || hasPrefix [0x4d, 0x4d, 0x00, 0x2a] = Just "image/tiff"
    | otherwise = Nothing
  where
    hasPrefix prefix = BS.pack prefix `BS.isPrefixOf` bytes

formatByteCount :: Int -> Text
formatByteCount n
    | n < 1024 = Text.pack (show n) <> " B"
    | n < 1024 * 1024 = Text.pack (show (n `div` 1024)) <> " KB"
    | otherwise = Text.pack (show (n `div` (1024 * 1024))) <> " MB"
