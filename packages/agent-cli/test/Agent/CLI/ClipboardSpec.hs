module Agent.CLI.ClipboardSpec (spec) where

import Agent.CLI.Clipboard
import Agent.Loop (ImageAttachment(..))
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import System.Info (os)
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
