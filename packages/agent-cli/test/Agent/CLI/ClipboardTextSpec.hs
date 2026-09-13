module Agent.CLI.ClipboardTextSpec (spec) where

import Agent.CLI.Clipboard (readClipboardText)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (cancel, waitCatch, withAsync)
import Control.Exception.Safe (bracket)
import Data.Either (isLeft)
import qualified Data.Text as Text
import GHC.IO.Encoding (getLocaleEncoding, setLocaleEncoding)
import System.Directory
    ( Permissions(executable), createDirectory, doesFileExist, getPermissions
    , getTemporaryDirectory, removeFile, removePathForcibly, setPermissions
    )
import System.Environment (getEnv, lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.Info (os)
import System.IO (TextEncoding, hClose, latin1, openBinaryTempFile, utf8)
import System.Timeout (timeout)
import Test.Hspec

-- PATH contains only our fixtures, never the real clipboard readers. These
-- tests deliberately do not call the image/type probes or mutate the clipboard.
spec :: Spec
spec = sequential $ describe "clipboard text subprocesses" do
    it "preserves Unicode under the locale UTF-8 decoder" $
        withUtf8 $ withReader "printf '\\303\\251\\346\\227\\245\\360\\237\\230\\200\\n'" $
            readClipboardText `shouldReturn` Right "é日😀\n"

    it "retains the configured non-UTF-8 locale decoder" $
        withEncoding latin1 $ withReader "printf '\\377\\351'" $
            readClipboardText `shouldReturn` Right "ÿé"

    it "preserves empty output" $
        withReader "exit 0" $
            readClipboardText `shouldReturn` Right ""

    it "preserves whitespace, CRLF, and trailing newlines exactly" $
        withReader "printf '  one\\r\\ntwo\\n\\n'" $
            readClipboardText `shouldReturn` Right "  one\r\ntwo\n\n"

    it "rejects malformed output instead of silently replacing bytes" $
        withUtf8 $ withReader "printf '\\377'" do
            readClipboardText >>= (`shouldSatisfy` isLeft)

    it "rejects malformed stderr even when the command succeeds" $
        withUtf8 $ withReader "printf ok; printf '\\377' >&2" do
            readClipboardText >>= (`shouldSatisfy` isLeft)

    it "returns stripped stderr for a nonzero exit" $
        withReader "printf ignored; printf '  first error\\n\\n' >&2; exit 7" $
            readClipboardText `shouldReturn` Left "first error"

    it "preserves Unicode diagnostics" $
        withUtf8 $ withReader "printf ' \\303\\251\\346\\227\\245\\n' >&2; exit 1" $
            readClipboardText `shouldReturn` Left "é日"

    it "preserves an empty error for a nonzero exit with no stderr" $
        withReader "exit 1" $
            readClipboardText `shouldReturn` Left ""

    it "closes the child's empty stdin" $
        withReader "if IFS= read -r line; then printf unexpected; else printf eof; fi" $
            timeout 5000000 readClipboardText `shouldReturn` Just (Right "eof")

    it "returns an error when the executable is unavailable" $
        withReaders [] do
            readClipboardText >>= (`shouldSatisfy` isLeft)

    it "drains stdout and stderr together beyond pipe capacity" $
        withReader
            ("chunk=abcdefghijklmnopqrstuvwxyz0123456789\n"
                <> "i=0\nwhile [ \"$i\" -lt 8192 ]; do\n"
                <> "printf '%s' \"$chunk\"\nprintf '%s' \"$chunk\" >&2\n"
                <> "i=$((i + 1))\ndone") do
            timeout 10000000 readClipboardText `shouldReturn`
                Just (Right (Text.replicate 8192 "abcdefghijklmnopqrstuvwxyz0123456789"))

    it "propagates cancellation and terminates the child" $
        withReader
            ("trap 'printf stopped > \"${0%/*}/stopped\"; exit 0' TERM\n"
                <> "printf ready > \"${0%/*}/ready\"\n"
                <> "while :; do :; done") do
            directory <- getEnv "PATH"
            withAsync readClipboardText \reader -> do
                timeout 5000000 (awaitFile (directory </> "ready"))
                    `shouldReturn` Just ()
                timeout 5000000 (cancel reader) `shouldReturn` Just ()
                waitCatch reader >>= (`shouldSatisfy` isLeft)
                timeout 5000000 (awaitFile (directory </> "stopped"))
                    `shouldReturn` Just ()

    it "falls back from Wayland to X11 after an error" $
        linuxOnly $ withReaders
            [("wl-paste", "printf 'wayland failed' >&2; exit 1")
            ,("xclip", "printf 'x11 text\\n'")] $
                readClipboardText `shouldReturn` Right "x11 text\n"

    it "keeps the first error when both Linux readers fail" $
        linuxOnly $ withReaders
            [("wl-paste", "printf ' first error\\n' >&2; exit 1")
            ,("xclip", "printf 'second error' >&2; exit 2")] $
                readClipboardText `shouldReturn` Left "first error"

    it "accepts an empty successful Wayland read without falling back" $
        linuxOnly $ withReaders
            [("wl-paste", "exit 0"), ("xclip", "printf unexpected")] $
                readClipboardText `shouldReturn` Right ""

withUtf8 :: IO a -> IO a
withUtf8 = withEncoding utf8

withEncoding :: TextEncoding -> IO a -> IO a
withEncoding encoding action = bracket getLocaleEncoding setLocaleEncoding \_ -> do
    setLocaleEncoding encoding
    action

awaitFile :: FilePath -> IO ()
awaitFile path = do
    exists <- doesFileExist path
    if exists then pure () else threadDelay 10000 >> awaitFile path

linuxOnly :: IO () -> IO ()
linuxOnly action
    | os == "linux" = action
    | otherwise = pendingWith "Linux-only clipboard fallback"

withReader :: String -> IO () -> IO ()
withReader script = withReaders [("pbpaste", script), ("wl-paste", script)]

withReaders :: [(FilePath, String)] -> IO () -> IO ()
withReaders scripts action
    | os /= "darwin" && os /= "linux" = pendingWith "Unix clipboard subprocess fixture"
    | otherwise = do
        temporary <- getTemporaryDirectory
        bracket
            (do
                (directory, handle) <- openBinaryTempFile temporary "agent-clipboard-text-"
                hClose handle
                removeFile directory
                createDirectory directory
                pure directory)
            removePathForcibly
            \directory -> do
                mapM_ (\(name, body) -> do
                    let path = directory </> name
                    writeFile path ("#!/bin/sh\n" <> body <> "\n")
                    permissions <- getPermissions path
                    setPermissions path permissions { executable = True }) scripts
                bracket
                    (lookupEnv "PATH")
                    (maybe (unsetEnv "PATH") (setEnv "PATH"))
                    \_ -> setEnv "PATH" directory >> action
