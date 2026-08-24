module Agent.CLI.ClipboardSpec (spec) where

import Agent.CLI.Clipboard
import Agent.Loop (ImageAttachment(..))
import Control.Exception (bracket)
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import System.Directory (getTemporaryDirectory, removeFile)
import System.Info (os)
import System.IO (hClose, openBinaryTempFile)
import Test.Hspec

spec :: Spec
spec = do
    describe "formatImageSize" do
        it "formats bytes, kilobytes, and megabytes" do
            formatImageSize 500 `shouldBe` "500 B"
            formatImageSize 2048 `shouldBe` "2 KB"
            formatImageSize (3 * 1024 * 1024) `shouldBe` "3 MB"

    describe "readClipboardImage" do
        it "reports unsupported platforms clearly" do
            result <- readClipboardImage
            if os == "darwin"
                then case result of
                    Left err ->
                        err `shouldSatisfy` (not . Text.null)
                    Right ImageAttachment{imageMime, imageBytes} -> do
                        imageMime `shouldSatisfy` (`elem` ["image/png", "image/jpeg"])
                        BS.length imageBytes `shouldSatisfy` (> 0)
                else if os == "linux"
                    then case result of
                        Left err ->
                            err `shouldSatisfy` (not . Text.null)
                        Right ImageAttachment{imageMime, imageBytes} -> do
                            imageMime `shouldSatisfy` (`elem` ["image/png", "image/jpeg"])
                            BS.length imageBytes `shouldSatisfy` (> 0)
                    else result `shouldBe`
                        Left "clipboard images are not supported on this platform yet"

    describe "nonEmptyClipboardImages" do
        it "keeps only successful non-empty image reads" do
            let image = ImageAttachment "image/png" "png-bytes"
            nonEmptyClipboardImages (Right [image])
                `shouldBe` Just [image]
            nonEmptyClipboardImages (Right [])
                `shouldBe` Nothing
            nonEmptyClipboardImages (Left "not an image")
                `shouldBe` Nothing

    describe "nonEmptyClipboardText" do
        it "keeps successful text, including whitespace" do
            nonEmptyClipboardText (Right "hello") `shouldBe` Just "hello"
            nonEmptyClipboardText (Right "  ") `shouldBe` Just "  "
            nonEmptyClipboardText (Right "") `shouldBe` Nothing
            nonEmptyClipboardText (Left "not text") `shouldBe` Nothing

    describe "loadImagesFromPastedText" do
        it "returns Nothing for ordinary prompt text" do
            result <- loadImagesFromPastedText "fix the bug in Main.hs"
            result `shouldBe` Nothing

        it "loads a pasted image path as an attachment" do
            tmp <- getTemporaryDirectory
            bracket
                (do
                    (path, handle) <- openBinaryTempFile tmp "agent-paste-.png"
                    BS.hPut handle "png-bytes"
                    hClose handle
                    pure path)
                removeFile
                \path -> do
                    result <- loadImagesFromPastedText (Text.pack path)
                    quoted <- loadImagesFromPastedText ("\"" <> Text.pack path <> "\"")
                    filed <- loadImagesFromPastedText ("file://" <> Text.pack path)
                    let check label got = case got of
                            Just [ImageAttachment{imageMime, imageBytes}] -> do
                                imageMime `shouldBe` "image/png"
                                imageBytes `shouldBe` "png-bytes"
                            other ->
                                expectationFailure (label <> " unexpected: " <> show other)
                    check "path" result
                    check "quoted" quoted
                    check "file-url" filed
