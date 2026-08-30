-- | Linux clipboard readers backed by Wayland or X11 command-line tools.
module Agent.CLI.Clipboard.Linux
    ( readLinuxClipboardImage
    , readLinuxClipboardText
    ) where

import Agent.CLI.Error (formatException)
import Agent.Loop (ImageAttachment(..))
import Control.Exception.Safe (finally, tryAny)
import Control.Monad (void)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory
    ( findExecutable
    , getFileSize
    , getTemporaryDirectory
    , removeFile
    )
import System.Exit (ExitCode(..))
import System.IO (hClose, openBinaryTempFile)
import System.Process (readProcessWithExitCode)

readLinuxClipboardImage :: IO (Either Text ImageAttachment)
readLinuxClipboardImage = do
    tools <- findLinuxClipboardTools
    case tools of
        [] -> pure (Left linuxClipboardUnavailableError)
        _ -> do
            png <- firstRight
                [ runBytesCmd executable (linuxImageArgs tool "image/png")
                | tool <- tools
                , let executable = linuxClipboardExecutable tool
                ]
            case png of
                Right bytes | not (BS.null bytes) ->
                    pure (Right (ImageAttachment "image/png" bytes))
                _ -> do
                    jpg <- firstRight
                        [ runBytesCmd executable
                            (linuxImageArgs tool "image/jpeg")
                        | tool <- tools
                        , let executable = linuxClipboardExecutable tool
                        ]
                    case jpg of
                        Right bytes | not (BS.null bytes) ->
                            pure (Right (ImageAttachment "image/jpeg" bytes))
                        Left err -> pure (Left err)
                        Right _ -> pure (Left "no image found on the clipboard")

readLinuxClipboardText :: IO (Either Text Text)
readLinuxClipboardText = do
    tools <- findLinuxClipboardTools
    case tools of
        [] -> pure (Left linuxClipboardUnavailableError)
        _ ->
            firstRight
                [ runTextCmd executable (linuxTextArgs tool)
                | tool <- tools
                , let executable = linuxClipboardExecutable tool
                ]

data LinuxClipboardTool
    = WlPaste FilePath
    | Xclip FilePath

findLinuxClipboardTools :: IO [LinuxClipboardTool]
findLinuxClipboardTools = do
    wlPaste <- findExecutable "wl-paste"
    xclip <- findExecutable "xclip"
    pure $
        maybe [] (pure . WlPaste) wlPaste
            <> maybe [] (pure . Xclip) xclip

linuxClipboardExecutable :: LinuxClipboardTool -> FilePath
linuxClipboardExecutable = \case
    WlPaste executable -> executable
    Xclip executable -> executable

linuxImageArgs :: LinuxClipboardTool -> String -> [String]
linuxImageArgs tool mime = case tool of
    WlPaste _ -> ["-t", mime]
    Xclip _ -> ["-selection", "clipboard", "-t", mime, "-o"]

linuxTextArgs :: LinuxClipboardTool -> [String]
linuxTextArgs = \case
    WlPaste _ -> ["-n"]
    Xclip _ -> ["-selection", "clipboard", "-o"]

linuxClipboardUnavailableError :: Text
linuxClipboardUnavailableError =
    "no clipboard reader is available on this Linux host; install \
    \wl-clipboard (Wayland) or xclip (X11). On a remote server, upload the \
    \image and paste its server-side path instead."

runTextCmd :: FilePath -> [String] -> IO (Either Text Text)
runTextCmd cmd args = do
    result <- tryAny (readProcessWithExitCode cmd args "")
    pure $ case result of
        Left ex -> Left (formatException ex)
        Right (ExitSuccess, out, _) -> Right (Text.pack out)
        Right (ExitFailure _, _, err) ->
            Left (Text.strip (Text.pack err))

runBytesCmd :: FilePath -> [String] -> IO (Either Text ByteString)
runBytesCmd cmd args = do
    tmpDir <- getTemporaryDirectory
    result <- tryAny do
        (path, handle) <- openBinaryTempFile tmpDir "agent-clipboard-.bin"
        hClose handle
        let cleanup = void (tryAny (removeFile path))
        (do
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
                    size <- getFileSize path
                    if size > maxClipboardImageBytes
                        then pure (Left
                            "clipboard image exceeds the 20 MB limit")
                        else do
                            bytes <- BS.readFile path
                            if BS.null bytes
                                then pure (Left
                                    "no image found on the clipboard")
                                else pure (Right bytes)
                ExitFailure _ ->
                    pure (Left (Text.strip (Text.pack err))))
            `finally` cleanup
    case result of
        Left ex -> pure (Left (formatException ex))
        Right value -> pure value

maxClipboardImageBytes :: Integer
maxClipboardImageBytes = 20 * 1024 * 1024

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
