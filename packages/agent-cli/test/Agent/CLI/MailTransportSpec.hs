{-# LANGUAGE OverloadedStrings #-}

module Agent.CLI.MailTransportSpec (spec) where

import Agent.CLI.Mail.Tools (MailAttachment(..), MailMessage(..))
import Agent.CLI.Mail.Transport
    ( decodeImapMessageId
    , decodeGmailAttachmentRef
    , mailProviderStatusError
    , parseGmailMessageValue
    )
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.Either (isLeft)
import Data.Text (isInfixOf)
import Network.HTTP.Types (mkStatus)
import Test.Hspec

spec :: Spec
spec = describe "mail transport references" do
    it "does not force reconnection for provider policy or quota denials" do
        mailProviderStatusError (mkStatus 401 "Unauthorized")
            `shouldSatisfy` isInfixOf "must be reconnected"
        mailProviderStatusError (mkStatus 403 "Forbidden")
            `shouldNotSatisfy` isInfixOf "must be reconnected"

    it "rejects command injection in opaque IMAP message ids" do
        decodeImapMessageId "SU5CT1gNCm02NjYgTE9HT1VUADEANDI"
            `shouldSatisfy` isLeft
        decodeImapMessageId "SU5CT1gAMQA0Mg"
            `shouldBe` Right ("INBOX", "1", "42")

    it "skips Gmail text attachments when selecting the message body" do
        let value = Aeson.object
                [ "id" .= ("gmail-message" :: String)
                , "payload" .= Aeson.object
                    [ "mimeType" .= ("multipart/mixed" :: String)
                    , "parts" .=
                        [ Aeson.object
                            [ "mimeType" .= ("text/plain" :: String)
                            , "filename" .= ("notes.txt" :: String)
                            , "body" .= Aeson.object
                                [ "data" .= ("YXR0YWNobWVudA==" :: String) ]
                            ]
                        , Aeson.object
                            [ "mimeType" .= ("text/plain" :: String)
                            , "filename" .= ("" :: String)
                            , "body" .= Aeson.object
                                [ "data" .= ("cmVhbCBib2R5" :: String) ]
                            ]
                        ]
                    ]
                ]
        case parseGmailMessageValue 1024 value of
            Left err -> expectationFailure (show err)
            Right message ->
                message.mailMessageBody `shouldBe` Just "real body"

    it "falls back to bounded text from Gmail HTML-only messages" do
        let value = Aeson.object
                [ "id" .= ("gmail-html" :: String)
                , "payload" .= Aeson.object
                    [ "mimeType" .= ("text/html" :: String)
                    , "filename" .= ("" :: String)
                    , "body" .= Aeson.object
                        [ "data" .=
                            ("PHA-SGVsbG8gd29ybGQ8L3A-" :: String) ]
                    ]
                ]
        case parseGmailMessageValue 5 value of
            Left err -> expectationFailure (show err)
            Right message -> do
                message.mailMessageBody `shouldBe` Just "Hello"
                message.mailMessageBodyTruncated `shouldBe` True

    it "keeps Gmail attachment metadata in its opaque download reference" do
        let value = Aeson.object
                [ "id" .= ("gmail-attachment-message" :: String)
                , "payload" .= Aeson.object
                    [ "mimeType" .= ("multipart/mixed" :: String)
                    , "parts" .=
                        [ Aeson.object
                            [ "mimeType" .= ("application/pdf" :: String)
                            , "filename" .= ("invoice.pdf" :: String)
                            , "body" .= Aeson.object
                                [ "attachmentId" .=
                                    ("provider-attachment-id" :: String)
                                , "size" .= (123 :: Int)
                                ]
                            ]
                        ]
                    ]
                ]
        case parseGmailMessageValue 1024 value of
            Left err -> expectationFailure (show err)
            Right message ->
                case message.mailMessageAttachments of
                    [attachment] ->
                        decodeGmailAttachmentRef attachment.mailAttachmentId
                            `shouldBe`
                                ( "provider-attachment-id"
                                , Just "invoice.pdf"
                                , Just "application/pdf"
                                )
                    attachments -> expectationFailure $
                        "expected one Gmail attachment, got "
                            <> show (length attachments)
