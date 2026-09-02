module Agent.CLI.MailMimeSpec (spec) where

import Agent.CLI.Mail.Mime
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Either (isLeft)
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = describe "custom IMAP MIME decoding" do
    it "decodes multipart text and exposes real attachment bytes" do
        parsed <- parseFixture multipartFixture
        mailMimeTextBody 1024 parsed
            `shouldSatisfy` maybe False ("Hello €" `Text.isInfixOf`)
        case mailMimeAttachments parsed of
            [attachment] -> do
                attachment.parsedMailAttachmentFilename
                    `shouldBe` "invoice.pdf"
                attachment.parsedMailAttachmentBytes
                    `shouldBe` "%PDF"
                mailMimeAttachmentContent
                    4 attachment.parsedMailAttachmentId parsed
                    `shouldBe` Right attachment
                mailMimeAttachmentContent
                    3 attachment.parsedMailAttachmentId parsed
                    `shouldSatisfy` isLeft
            attachments ->
                expectationFailure
                    ("expected one attachment, got "
                        <> show (length attachments))

    it "falls back to bounded plain text for HTML-only messages" do
        parsed <- parseFixture htmlFixture
        mailMimeTextBody 5 parsed `shouldBe` Just "Hello"
        mailMimeTextBodyTruncated 5 parsed `shouldBe` True
        mailMimeTextBodyTruncated 11 parsed `shouldBe` False

    it "exposes attachment-disposition parts without filenames safely" do
        parsed <- parseFixture unnamedAttachmentFixture
        case mailMimeAttachments parsed of
            [attachment] -> do
                attachment.parsedMailAttachmentFilename
                    `shouldSatisfy` ("attachment-" `Text.isPrefixOf`)
                attachment.parsedMailAttachmentBytes `shouldBe` "data"
            attachments ->
                expectationFailure
                    ("expected one attachment, got "
                        <> show (length attachments))

parseFixture :: BS.ByteString -> IO ParsedMailMime
parseFixture bytes =
    case parseMailMime bytes of
        Left err -> expectationFailure (Text.unpack err) >> fail "invalid fixture"
        Right parsed -> pure parsed

multipartFixture :: BS.ByteString
multipartFixture = BS8.intercalate "\r\n"
    [ "MIME-Version: 1.0"
    , "Content-Type: multipart/mixed; boundary=\"outer\""
    , ""
    , "--outer"
    , "Content-Type: multipart/alternative; boundary=\"alternative\""
    , ""
    , "--alternative"
    , "Content-Type: text/plain; charset=utf-8"
    , "Content-Transfer-Encoding: quoted-printable"
    , ""
    , "Hello =E2=82=AC"
    , "--alternative"
    , "Content-Type: text/html; charset=utf-8"
    , ""
    , "<p>Hello €</p>"
    , "--alternative--"
    , "--outer"
    , "Content-Type: application/pdf; name=\"invoice.pdf\""
    , "Content-Disposition: attachment; filename=\"invoice.pdf\""
    , "Content-Transfer-Encoding: base64"
    , ""
    , "JVBERg=="
    , "--outer--"
    , ""
    ]

unnamedAttachmentFixture :: BS.ByteString
unnamedAttachmentFixture = BS8.intercalate "\r\n"
    [ "MIME-Version: 1.0"
    , "Content-Type: application/octet-stream"
    , "Content-Disposition: attachment"
    , "Content-Transfer-Encoding: base64"
    , ""
    , "ZGF0YQ=="
    , ""
    ]

htmlFixture :: BS.ByteString
htmlFixture = BS8.intercalate "\r\n"
    [ "MIME-Version: 1.0"
    , "Content-Type: text/html; charset=utf-8"
    , ""
    , "<p>Hello world</p>"
    , ""
    ]
