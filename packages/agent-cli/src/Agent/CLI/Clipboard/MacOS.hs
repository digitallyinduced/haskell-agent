{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | macOS clipboard metadata and readers backed by pbpaste and AppleScript.
module Agent.CLI.Clipboard.MacOS
    ( readMacClipboardImage
    , readMacClipboardMayContainImages
    , readMacClipboardPaths
    , readMacClipboardText
    ) where

import Agent.CLI.Clipboard.Process (readClipboardProcessText)
import Agent.Runtime.Error (formatException)
import Agent.Loop (ImageAttachment(..))
import Control.Exception.Safe (bracket, finally, tryAny)
import Control.Monad (void)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (toLower)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory (getTemporaryDirectory, removeFile)
import System.Exit (ExitCode(..))
import System.IO
    ( IOMode(ReadMode)
    , hClose
    , openBinaryTempFile
    , withBinaryFile
    )
import System.Process (readProcessWithExitCode)

#if defined(darwin_HOST_OS)
import Foreign.C.Types (CInt(..))

foreign import ccall safe "agent_cli_clipboard_may_contain_images"
    clipboardMayContainImages :: IO CInt
#endif

-- | Check advertised types without coercing or reading clipboard payloads.
-- Inspection failures conservatively retain the existing image readers.
readMacClipboardMayContainImages :: IO Bool
#if defined(darwin_HOST_OS)
readMacClipboardMayContainImages = (/= 0) <$> clipboardMayContainImages
#else
readMacClipboardMayContainImages = pure True
#endif

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
    result <- tryAny (readClipboardProcessText "pbpaste" [])
    pure $ case result of
        Left ex -> Left (formatException ex)
        Right (ExitSuccess, out, _) -> Right out
        Right (ExitFailure _, _, err) ->
            Left (Text.strip err)

readMacClipboardPaths :: IO [FilePath]
readMacClipboardPaths = do
    result <- tryAny $
        readProcessWithExitCode "osascript"
            [ "-e"
            , unlines
                [ "try"
                , "  set theFiles to the clipboard as list"
                , "  set paths to {}"
                , "  repeat with f in theFiles"
                , "    try"
                , "      set end of paths to POSIX path of f"
                , "    end try"
                , "  end repeat"
                , "  set AppleScript's text item delimiters to linefeed"
                , "  return paths as text"
                , "on error"
                , "  try"
                , "    return POSIX path of (the clipboard as «class furl»)"
                , "  on error"
                , "    return \"\""
                , "  end try"
                , "end try"
                ]
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
    result <- tryAny $
        bracket
            (openBinaryTempFile tmpDir "agent-clipboard-.bin")
            (\(path, handle) ->
                hClose handle `finally` void (tryAny (removeFile path)))
            \(path, handle) -> do
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
                (code, _out, err) <-
                    readProcessWithExitCode "osascript" ["-e", script] ""
                case code of
                    ExitSuccess -> do
                        bytes <- withBinaryFile path ReadMode \input ->
                            BS.hGet input (maxClipboardImageBytes + 1)
                        if BS.length bytes > maxClipboardImageBytes
                            then pure (Left
                                "clipboard image exceeds the 20 MB limit")
                            else pure (Right bytes)
                    ExitFailure _ ->
                        pure (Left (clipboardErrorMessage typeClass err))
    case result of
        Left ex -> pure (Left (formatException ex))
        Right value -> pure value

maxClipboardImageBytes :: Int
maxClipboardImageBytes = 20 * 1024 * 1024

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
