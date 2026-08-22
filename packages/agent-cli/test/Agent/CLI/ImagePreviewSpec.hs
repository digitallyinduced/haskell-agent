module Agent.CLI.ImagePreviewSpec (spec) where

import Agent.CLI.ImagePreview
import Agent.Loop (ImageAttachment(..))
import Codec.Picture
    ( PixelYCbCr8(..)
    , encodeJpegAtQuality
    , generateImage
    )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = do
    describe "parseImagePreviewProtocol" do
        it "detects Kitty, Ghostty, and WezTerm" do
            parseImagePreviewProtocol (Just "xterm-kitty") Nothing (Just "1") Nothing
                `shouldBe` PreviewKitty
            parseImagePreviewProtocol (Just "xterm-256color") (Just "ghostty") Nothing Nothing
                `shouldBe` PreviewKitty
            parseImagePreviewProtocol Nothing (Just "WezTerm") Nothing Nothing
                `shouldBe` PreviewKitty

        it "detects iTerm2" do
            parseImagePreviewProtocol Nothing (Just "iTerm.app") Nothing (Just "w0t0")
                `shouldBe` PreviewITerm
            parseImagePreviewProtocol (Just "xterm-256color") (Just "iTerm2") Nothing Nothing
                `shouldBe` PreviewITerm

        it "falls back when the terminal has no graphics protocol" do
            parseImagePreviewProtocol (Just "xterm-256color") (Just "Apple_Terminal") Nothing Nothing
                `shouldBe` PreviewUnsupported

    describe "preview size caps" do
        it "keeps the thumbnail inside a fraction of the pane" do
            previewRowsFor 24 `shouldBe` 6
            previewRowsFor 8 `shouldBe` 4
            previewColumnsFor 120 `shouldBe` 40
            previewColumnsFor 30 `shouldBe` 12

    describe "kittyImageSequence" do
        it "transmits and displays in one action while preserving source aspect ratio" do
            let seq_ = kittyImageSequence 7 20 6 "image/png" "png-bytes"
            seq_ `shouldSatisfy` Text.isInfixOf "\ESC_Ga=T,q=2,i=7,f=100,t=d,r=6,C=1,m=0;"
            seq_ `shouldSatisfy` (not . Text.isInfixOf "\ESC_Ga=p")
            seq_ `shouldSatisfy` (not . Text.isInfixOf ",c=")
            -- payload is base64 of the raw bytes
            seq_ `shouldSatisfy` Text.isInfixOf "cG5nLWJ5dGVz"

        it "transcodes JPEG attachments to PNG before transmission" do
            let source = generateImage (\_ _ -> PixelYCbCr8 20 128 128) 4 3
                jpeg =
                    LBS.toStrict (encodeJpegAtQuality 90 source)
                compatible =
                    kittyCompatibleAttachment
                        (ImageAttachment "image/jpeg" jpeg)
            compatible.imageMime `shouldBe` "image/png"
            BS.take 8 compatible.imageBytes
                `shouldBe` "\137PNG\r\n\SUB\n"

        it "chunks payloads larger than 4096 encoded bytes" do
            let bytes = BS.replicate 4000 65 -- 'A'; base64 expands past 4096
                seq_ = kittyImageSequence 2 10 4 "image/png" bytes
            Text.count ",m=1;" seq_ `shouldBe` 1
            seq_ `shouldSatisfy` Text.isInfixOf ",m=0;"
            Text.count "\ESC_Ga=T" seq_ `shouldBe` 1
            Text.count "\ESC_Gq=2,m=0;" seq_ `shouldBe` 1
            Text.count ",i=2," seq_ `shouldBe` 1

    describe "kittyPlacedImageSequence" do
        it "uses a stable placement id and explicit fullscreen cell rectangle" do
            let seq_ =
                    kittyPlacedImageSequence
                        2000000001
                        2000000001
                        72
                        19
                        "image/png"
                        "png-bytes"
            seq_ `shouldSatisfy` Text.isInfixOf
                "\ESC_Ga=T,q=2,i=2000000001,f=100,t=d,r=19,c=72,p=2000000001,z=1,C=1,m=0;"

        it "deletes only the requested image and frees its data" do
            kittyDeleteImageSequence 2000000001
                `shouldBe` "\ESC_Ga=d,d=I,q=2,i=2000000001\ESC\\"

    describe "itermImageSequence" do
        it "emits OSC 1337 inline file with cell size" do
            let seq_ = itermImageSequence 20 6 "png-bytes"
            seq_ `shouldBe`
                "\ESC]1337;File=inline=1;width=20;height=6;preserveAspectRatio=1:cG5nLWJ5dGVz\BEL"

    describe "wrapTmuxPassthrough" do
        it "doubles ESC so tmux forwards the inner sequence" do
            wrapTmuxPassthrough "\ESC]1337;File=inline=1:\BEL"
                `shouldBe` "\ESCPtmux;\ESC\ESC]1337;File=inline=1:\BEL\ESC\\"

    describe "positionImagePayload" do
        it "moves to a zero-based cell and restores Vty's cursor" do
            positionImagePayload 4 9 "image"
                `shouldBe` "\ESC7\ESC[5;10Himage\ESC8"

    describe "renderImagePreview" do
        let img = ImageAttachment "image/png" "png-bytes"
            muted = id
        it "returns Nothing for an empty list" do
            renderImagePreview PreviewKitty False muted 20 6 1 []
                `shouldBe` Nothing

        it "includes graphics plus a caption on Kitty" do
            case renderImagePreview PreviewKitty False muted 20 6 3 [img] of
                Nothing -> expectationFailure "expected a preview block"
                Just block -> do
                    block `shouldSatisfy` Text.isInfixOf "\ESC_Ga=T,q=2,i=3"
                    block `shouldSatisfy` Text.isInfixOf "image/png (9 B)"
                    -- six blank lines (rows=6) sit between the bitmap and the caption
                    block `shouldSatisfy` Text.isInfixOf (Text.replicate 6 "\n")

        it "wraps Kitty sequences for tmux" do
            case renderImagePreview PreviewKitty True muted 12 4 1 [img] of
                Nothing -> expectationFailure "expected a preview block"
                Just block -> do
                    block `shouldSatisfy` Text.isPrefixOf "\ESCPtmux;"
                    block `shouldSatisfy` Text.isInfixOf "\ESC\ESC_Ga=T"

        it "falls back to a caption without graphics" do
            case renderImagePreview PreviewUnsupported False muted 20 6 1 [img] of
                Nothing -> expectationFailure "expected a preview block"
                Just block -> do
                    block `shouldSatisfy` Text.isInfixOf "inline preview needs Kitty"
                    block `shouldSatisfy` (not . Text.isInfixOf "\ESC_G")
                    block `shouldSatisfy` (not . Text.isInfixOf "1337")
