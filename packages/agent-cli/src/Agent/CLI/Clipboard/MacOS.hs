-- | macOS clipboard readers backed by pbpaste and AppleScript.
module Agent.CLI.Clipboard.MacOS
    ( readMacClipboardImage
    , readMacClipboardPaths
    , readMacClipboardText
    ) where

import Agent.CLI.Error (formatException)
import Agent.Loop (ImageAttachment(..))
import Control.Exception.Safe (tryAny)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (toLower)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory (getTemporaryDirectory, removeFile)
import System.Exit (ExitCode(..))
import System.IO (hClose, openBinaryTempFile)
import System.Process (readProcessWithExitCode)

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

readMacClipboardText :: IO (Either Text Text)
readMacClipboardText = do
    result <- tryAny (readProcessWithExitCode "pbpaste" [] "")
    pure $ case result of
        Left ex -> Left (formatException ex)
        Right (ExitSuccess, out, _) -> Right (Text.pack out)
        Right (ExitFailure _, _, err) ->
            Left (Text.strip (Text.pack err))

readMacClipboardPaths :: IO [FilePath]
readMacClipboardPaths = do
    result <- tryAny $
        readProcessWithExitCode "osascript"
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
    pure $ case result of
        Right (ExitSuccess, out, _) ->
            filter (not . null) (lines out)
        Right (ExitFailure _, _, _) -> []
        Left _ -> []

readMacClipboardClass :: String -> IO (Either Text ByteString)
readMacClipboardClass typeClass = do
    tmpDir <- getTemporaryDirectory
    result <- tryAny do
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
                _ <- tryAny (removeFile path)
                pure (Right bytes)
            ExitFailure _ -> do
                _ <- tryAny (removeFile path)
                pure (Left (clipboardErrorMessage typeClass err))
    case result of
        Left ex -> pure (Left (formatException ex))
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
