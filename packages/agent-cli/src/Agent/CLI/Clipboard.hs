-- | Read images, text, and file paths from the system clipboard.
module Agent.CLI.Clipboard
    ( ClipboardContent(..)
    , readClipboard
    , readClipboardImage
    , readClipboardImages
    , readClipboardImagesImageFirst
    , readClipboardImagesForPaste
    , readClipboardText
    , nonEmptyClipboardImages
    , nonEmptyClipboardText
    , appendUniqueImageAttachments
    , loadImagesFromPastedText
    , formatImageSize
    ) where

import Agent.CLI.Clipboard.Linux
    ( readLinuxClipboardImage
    , readLinuxClipboardText
    )
import Agent.CLI.Clipboard.MacOS
    ( readMacClipboardImage
    , readMacClipboardPaths
    , readMacClipboardText
    )
import Agent.CLI.Error (formatException)
import Agent.Loop (ImageAttachment(..))
import Control.Exception.Safe (tryAny)
import Control.Monad (filterM)
import qualified Data.ByteString as BS
import Data.Char (toLower)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory (doesFileExist)
import System.FilePath (takeExtension)
import System.Info (os)

-- | What we could usefully take from the clipboard for /paste.
data ClipboardContent
    = ClipboardImage ImageAttachment
    | ClipboardText Text
    | ClipboardPaths [FilePath]
    | ClipboardEmpty
    deriving (Eq, Show)

-- | Prefer file paths (images first), then image bytes, then plain text.
readClipboard :: IO ClipboardContent
readClipboard = do
    images <- readClipboardImages
    case images of
        Right (img:_) -> pure (ClipboardImage img)
        _ -> do
            paths <- readClipboardPaths
            existing <- filterM doesFileExist paths
            if not (null existing)
                then pure (ClipboardPaths existing)
                else do
                    txt <- readClipboardText
                    case txt of
                        Right t | not (Text.null (Text.strip t)) ->
                            pure (ClipboardText (Text.stripEnd t))
                        _ -> pure ClipboardEmpty

-- | All clipboard images we can load (Finder file-list images or one bitmap).
readClipboardImages :: IO (Either Text [ImageAttachment])
readClipboardImages = do
    paths <- readClipboardPaths
    imagePaths <- filterM isImageFile paths
    case imagePaths of
        [] -> fmap (:[]) <$> readClipboardImageBytes
        ps -> do
            loaded <- mapM readImageFile ps
            case sequence loaded of
                Right images@(_:_) -> pure (Right images)
                Right [] -> pure (Left "no image found on the clipboard")
                Left err -> pure (Left err)

-- | Fast path for a terminal bracketed paste. Screenshots and copied browser
-- images usually expose bitmap data directly; checking Finder file coercions
-- first is especially expensive on macOS because AppleScript may spend close
-- to a second attempting to turn the bitmap into a file list.
readClipboardImagesImageFirst :: IO (Either Text [ImageAttachment])
readClipboardImagesImageFirst = do
    bitmap <- readClipboardImageBytes
    case bitmap of
        Right image -> pure (Right [image])
        Left bitmapError -> do
            paths <- readClipboardPaths
            imagePaths <- filterM isImageFile paths
            case imagePaths of
                [] -> pure (Left bitmapError)
                ps -> do
                    loaded <- mapM readImageFile ps
                    case sequence loaded of
                        Right images@(_:_) -> pure (Right images)
                        Right [] -> pure (Left bitmapError)
                        Left err -> pure (Left err)

-- | Read images for an explicit image-paste action and produce the richer
-- text/path diagnostics without probing the clipboard a second time.
readClipboardImagesForPaste :: IO (Either Text [ImageAttachment])
readClipboardImagesForPaste = do
    paths <- readClipboardPaths
    existingPaths <- filterM doesFileExist paths
    let imagePaths =
            filter (isImageExtension . takeExtension) existingPaths
    case imagePaths of
        _ : _ ->
            mapM readImageFile imagePaths >>= \loaded ->
                pure $ case sequence loaded of
                    Right images@(_ : _) -> Right images
                    _ -> Left (clipboardPathsError existingPaths)
        [] ->
            readClipboardImageBytes >>= \case
                Right image -> pure (Right [image])
                Left imageError
                    | not (null existingPaths) ->
                        pure (Left (clipboardPathsError existingPaths))
                    | otherwise ->
                        readClipboardText >>= \case
                            Right text
                                | not (Text.null (Text.strip text)) ->
                                    pure (Left clipboardTextError)
                            _ -> pure (Left imageError)

clipboardTextError :: Text
clipboardTextError =
    "clipboard has text, not an image (paste text normally into the prompt)"

clipboardPathsError :: [FilePath] -> Text
clipboardPathsError paths =
    "clipboard has file path(s), but no loadable image: "
        <> Text.intercalate ", " (map Text.pack paths)

-- | Prefer PNG; fall back to JPEG. Back-compat for one-image callers.
readClipboardImage :: IO (Either Text ImageAttachment)
readClipboardImage =
    readClipboardImages >>= \case
        Left err -> pure (Left err)
        Right [] -> pure (Left "no image found on the clipboard")
        Right (img:_) -> pure (Right img)

-- | Keep a successful, non-empty clipboard image read. This is used when a
-- terminal reports a bracketed paste: image-bearing clipboard contents should
-- become attachments, while ordinary text pastes should stay in the editor.
nonEmptyClipboardImages
    :: Either Text [ImageAttachment]
    -> Maybe [ImageAttachment]
nonEmptyClipboardImages = \case
    Right images@(_:_) -> Just images
    _ -> Nothing

-- | Keep successful clipboard text, including whitespace-only text. Explicit
-- Ctrl/Cmd+V should behave like a normal text paste whenever the clipboard
-- exposes text, falling back to image attachment only when it does not.
nonEmptyClipboardText :: Either Text Text -> Maybe Text
nonEmptyClipboardText = \case
    Right text | not (Text.null text) -> Just text
    _ -> Nothing

-- | Append only images whose bytes are not already attached. Comparing the
-- payload rather than the MIME label also suppresses a repeated paste if the
-- same clipboard bytes are reported with different metadata.
appendUniqueImageAttachments
    :: [ImageAttachment]
    -> [ImageAttachment]
    -> ([ImageAttachment], [ImageAttachment])
appendUniqueImageAttachments existing incoming =
    foldl' appendOne (existing, []) incoming
  where
    appendOne (allImages, added) image
        | any (sameImage image) allImages = (allImages, added)
        | otherwise = (allImages <> [image], added <> [image])
    sameImage left right = left.imageBytes == right.imageBytes

formatImageSize :: Int -> Text
formatImageSize n
    | n < 1024 = Text.pack (show n) <> " B"
    | n < 1024 * 1024 =
        Text.pack (show (n `div` 1024)) <> " KB"
    | otherwise =
        Text.pack (show (n `div` (1024 * 1024))) <> " MB"

-- | Native Cmd+V of a Finder image often inserts POSIX path(s) rather than
-- bitmap bytes. If the whole prompt (or every newline-separated token) is an
-- existing image file, load them as attachments; otherwise return 'Nothing'
-- so the prompt stays ordinary text.
loadImagesFromPastedText :: Text -> IO (Maybe [ImageAttachment])
loadImagesFromPastedText raw = do
    let stripped = Text.strip raw
        whole = normalizePastedPath stripped
        lines_ =
            filter (not . null)
                (map normalizePastedPath (Text.splitOn "\n" stripped))
    wholeOk <- if null whole then pure False else isImageFile whole
    if wholeOk
        then fmap toMaybe (readImageFile whole)
        else if length lines_ > 1
            then do
                allImages <- and <$> mapM isImageFile lines_
                if not allImages
                    then pure Nothing
                    else do
                        loaded <- mapM readImageFile lines_
                        pure $ case sequence loaded of
                            Right images@(_:_) -> Just images
                            _ -> Nothing
            else pure Nothing
  where
    toMaybe = \case
        Right img -> Just [img]
        Left _ -> Nothing

-- | Strip quotes and a @file://@ prefix so Finder / browser pastes match.
normalizePastedPath :: Text -> FilePath
normalizePastedPath raw =
    let stripped = unquote (Text.strip raw)
        unpacked = Text.unpack stripped
    in case unpacked of
        'f':'i':'l':'e':':':'/':'/':rest -> dropAuthority rest
        _ -> unpacked
  where
    unquote t
        | Text.length t >= 2
        , Text.head t == Text.last t
        , Text.head t `elem` ['"', '\''] =
            Text.init (Text.drop 1 t)
        | otherwise = t
    -- @file:///tmp/x.png@ → @/tmp/x.png@; @file://localhost/tmp/x.png@ too.
    dropAuthority rest =
        case rest of
            '/':_ -> rest
            _ ->
                case dropWhile (/= '/') rest of
                    [] -> rest
                    path -> path

--------------------------------------------------------------------------------
-- Platform readers
--------------------------------------------------------------------------------

readClipboardImageBytes :: IO (Either Text ImageAttachment)
readClipboardImageBytes
    | os == "darwin" = readMacClipboardImage
    | os == "linux" = readLinuxClipboardImage
    | otherwise =
        pure (Left "clipboard images are not supported on this platform yet")

readClipboardText :: IO (Either Text Text)
readClipboardText
    | os == "darwin" = readMacClipboardText
    | os == "linux" = readLinuxClipboardText
    | otherwise = pure (Left "clipboard text is not supported on this platform yet")

readClipboardPaths :: IO [FilePath]
readClipboardPaths
    | os == "darwin" = readMacClipboardPaths
    | otherwise = pure []

--------------------------------------------------------------------------------
-- Shared helpers
--------------------------------------------------------------------------------

isImageFile :: FilePath -> IO Bool
isImageFile path = do
    exists <- doesFileExist path
    pure (exists && isImageExtension (takeExtension path))

isImageExtension :: String -> Bool
isImageExtension ext =
    map toLower ext `elem` [".png", ".jpg", ".jpeg", ".gif", ".webp", ".bmp"]

readImageFile :: FilePath -> IO (Either Text ImageAttachment)
readImageFile path = do
    result <- tryAny (BS.readFile path)
    pure $ case result of
        Left ex -> Left (formatException ex)
        Right bytes
            | BS.null bytes -> Left ("empty image file: " <> Text.pack path)
            | otherwise ->
                Right ImageAttachment
                    { imageMime = mimeForPath path
                    , imageBytes = bytes
                    }

mimeForPath :: FilePath -> Text
mimeForPath path = case map toLower (takeExtension path) of
    ".jpg" -> "image/jpeg"
    ".jpeg" -> "image/jpeg"
    ".gif" -> "image/gif"
    ".webp" -> "image/webp"
    ".bmp" -> "image/bmp"
    _ -> "image/png"
