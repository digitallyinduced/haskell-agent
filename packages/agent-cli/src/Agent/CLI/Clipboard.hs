-- | Read images, text, and file paths from the system clipboard.
module Agent.CLI.Clipboard
    ( ClipboardContent(..)
    , readClipboard
    , readClipboardImage
    , readClipboardImages
    , loadImagesFromPastedText
    , formatImageSize
    ) where

import Agent.Loop (ImageAttachment(..))
import Control.Exception (SomeException, try)
import Control.Monad (filterM)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (toLower)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory
    ( doesFileExist
    , getTemporaryDirectory
    , removeFile
    )
import System.Exit (ExitCode(..))
import System.FilePath (takeExtension)
import System.IO (hClose, openBinaryTempFile)
import System.Info (os)
import System.Process (readProcessWithExitCode)

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

-- | Prefer PNG; fall back to JPEG. Back-compat for one-image callers.
readClipboardImage :: IO (Either Text ImageAttachment)
readClipboardImage =
    readClipboardImages >>= \case
        Left err -> pure (Left err)
        Right [] -> pure (Left "no image found on the clipboard")
        Right (img:_) -> pure (Right img)

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
    | os == "darwin" = runTextCmd "pbpaste" []
    | os == "linux" = readLinuxClipboardText
    | otherwise = pure (Left "clipboard text is not supported on this platform yet")

readClipboardPaths :: IO [FilePath]
readClipboardPaths
    | os == "darwin" = readMacClipboardPaths
    | otherwise = pure []

--------------------------------------------------------------------------------
-- macOS
--------------------------------------------------------------------------------

readMacClipboardImage :: IO (Either Text ImageAttachment)
readMacClipboardImage = do
    png <- readMacClipboardClass "«class PNGf»"
    case png of
        Right bytes | not (BS.null bytes) ->
            pure (Right (ImageAttachment "image/png" bytes))
        _ -> do
            jpg <- readMacClipboardClass "JPEG picture"
            case jpg of
                Right bytes | not (BS.null bytes) ->
                    pure (Right (ImageAttachment "image/jpeg" bytes))
                _ ->
                    pure (Left "no image found on the clipboard")

readMacClipboardPaths :: IO [FilePath]
readMacClipboardPaths = do
    (code, out, _) <- readProcessWithExitCode "osascript"
        [ "-e"
        , "try\n\
          \  set theFiles to the clipboard as list\n\
          \  set paths to {}\n\
          \  repeat with f in theFiles\n\
          \    try\n\
          \      set end of paths to POSIX path of f\n\
          \    end try\n\
          \  end repeat\n\
          \  set AppleScript's text item delimiters to linefeed\n\
          \  return paths as text\n\
          \on error\n\
          \  try\n\
          \    return POSIX path of (the clipboard as «class furl»)\n\
          \  on error\n\
          \    return \"\"\n\
          \  end try\n\
          \end try"
        ]
        ""
    pure $ case code of
        ExitSuccess -> filter (not . null) (lines out)
        ExitFailure _ -> []

readMacClipboardClass :: String -> IO (Either Text ByteString)
readMacClipboardClass typeClass = do
    tmpDir <- getTemporaryDirectory
    result <- try @SomeException do
        (path, handle) <- openBinaryTempFile tmpDir "agent-clipboard-.bin"
        hClose handle
        removeFile path
        let script =
                unlines
                    [ "try"
                    , "  set clipData to the clipboard as " <> typeClass
                    , "  set outFile to open for access POSIX file "
                        <> appleString path
                        <> " with write permission"
                    , "  set eof of outFile to 0"
                    , "  write clipData to outFile"
                    , "  close access outFile"
                    , "  return \"ok\""
                    , "on error errMsg"
                    , "  try"
                    , "    close access POSIX file " <> appleString path
                    , "  end try"
                    , "  error errMsg"
                    , "end try"
                    ]
        (code, _out, err) <- readProcessWithExitCode "osascript" ["-e", script] ""
        case code of
            ExitSuccess -> do
                bytes <- BS.readFile path
                _ <- try @SomeException (removeFile path)
                pure (Right bytes)
            ExitFailure _ -> do
                _ <- try @SomeException (removeFile path)
                pure (Left (clipboardErrorMessage typeClass err))
    case result of
        Left ex -> pure (Left (Text.pack (show ex)))
        Right value -> pure value

--------------------------------------------------------------------------------
-- Linux
--------------------------------------------------------------------------------

readLinuxClipboardImage :: IO (Either Text ImageAttachment)
readLinuxClipboardImage = do
    png <- firstRight
        [ runBytesCmd "wl-paste" ["-t", "image/png"]
        , runBytesCmd "xclip" ["-selection", "clipboard", "-t", "image/png", "-o"]
        ]
    case png of
        Right bytes | not (BS.null bytes) ->
            pure (Right (ImageAttachment "image/png" bytes))
        _ -> do
            jpg <- firstRight
                [ runBytesCmd "wl-paste" ["-t", "image/jpeg"]
                , runBytesCmd "xclip" ["-selection", "clipboard", "-t", "image/jpeg", "-o"]
                ]
            case jpg of
                Right bytes | not (BS.null bytes) ->
                    pure (Right (ImageAttachment "image/jpeg" bytes))
                Left err -> pure (Left err)
                Right _ -> pure (Left "no image found on the clipboard")

readLinuxClipboardText :: IO (Either Text Text)
readLinuxClipboardText =
    firstRight
        [ runTextCmd "wl-paste" ["-n"]
        , runTextCmd "xclip" ["-selection", "clipboard", "-o"]
        ]

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
    result <- try @SomeException (BS.readFile path)
    pure $ case result of
        Left ex -> Left (Text.pack (show ex))
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

runTextCmd :: FilePath -> [String] -> IO (Either Text Text)
runTextCmd cmd args = do
    result <- try @SomeException (readProcessWithExitCode cmd args "")
    pure $ case result of
        Left ex -> Left (Text.pack (show ex))
        Right (ExitSuccess, out, _) -> Right (Text.pack out)
        Right (ExitFailure _, _, err) ->
            Left (Text.strip (Text.pack err))

runBytesCmd :: FilePath -> [String] -> IO (Either Text ByteString)
runBytesCmd cmd args = do
    tmpDir <- getTemporaryDirectory
    result <- try @SomeException do
        (path, handle) <- openBinaryTempFile tmpDir "agent-clipboard-.bin"
        hClose handle
        removeFile path
        (code, _, err) <- readProcessWithExitCode "bash"
            [ "-c"
            , shellQuote cmd
                <> " "
                <> unwords (map shellQuote args)
                <> " > "
                <> shellQuote path
            ]
            ""
        case code of
            ExitSuccess -> do
                bytes <- BS.readFile path
                _ <- try @SomeException (removeFile path)
                if BS.null bytes
                    then pure (Left "no image found on the clipboard")
                    else pure (Right bytes)
            ExitFailure _ -> do
                _ <- try @SomeException (removeFile path)
                pure (Left (Text.strip (Text.pack err)))
    case result of
        Left ex -> pure (Left (Text.pack (show ex)))
        Right value -> pure value

shellQuote :: String -> String
shellQuote s = '\'' : go s
  where
    go [] = "'"
    go ('\'':xs) = "'\\''" <> go xs
    go (c:xs) = c : go xs

firstRight :: [IO (Either Text a)] -> IO (Either Text a)
firstRight [] = pure (Left "no image found on the clipboard")
firstRight (action:rest) = do
    result <- action
    case result of
        Right value -> pure (Right value)
        Left err -> do
            more <- firstRight rest
            case more of
                Right value -> pure (Right value)
                Left _ -> pure (Left err)

clipboardErrorMessage :: String -> String -> Text
clipboardErrorMessage typeClass err =
    let cleaned = Text.strip (Text.pack err)
        lower = Text.pack (map toLower typeClass)
    in if Text.null cleaned
        then "no image found on the clipboard (" <> lower <> ")"
        else "clipboard: " <> cleaned

appleString :: FilePath -> String
appleString path = "\"" <> escapeApple path <> "\""
  where
    escapeApple = concatMap \case
        '"' -> "\\\""
        '\\' -> "\\\\"
        c -> [c]
