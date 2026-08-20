-- | Read image bytes from the system clipboard (macOS).
module Agent.CLI.Clipboard
    ( readClipboardImage
    , formatImageSize
    ) where

import Agent.Loop (ImageAttachment(..))
import Control.Exception (SomeException, try)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (toLower)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory (getTemporaryDirectory, removeFile)
import System.Exit (ExitCode(..))
import System.IO (hClose, openBinaryTempFile)
import System.Info (os)
import System.Process (readProcessWithExitCode)

-- | Prefer PNG; fall back to JPEG. macOS only for now.
readClipboardImage :: IO (Either Text ImageAttachment)
readClipboardImage
    | os /= "darwin" =
        pure (Left "clipboard images are only supported on macOS for now")
    | otherwise = do
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

formatImageSize :: Int -> Text
formatImageSize n
    | n < 1024 = Text.pack (show n) <> " B"
    | n < 1024 * 1024 =
        Text.pack (show (n `div` 1024)) <> " KB"
    | otherwise =
        Text.pack (show (n `div` (1024 * 1024))) <> " MB"

-- | Dump a macOS clipboard type class to a temp file via osascript, then read it.
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
