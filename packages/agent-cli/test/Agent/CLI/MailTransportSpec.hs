{-# LANGUAGE OverloadedStrings #-}

module Agent.CLI.MailTransportSpec (spec) where

import Agent.CLI.Mail.Mime (renderMailDraftMime)
import Agent.CLI.Mail.Tools
    ( MailAttachment(..)
    , MailDraft(..)
    , MailDraftContent(..)
    , MailboxSummary(..)
    , MailMessage(..)
    )
import Agent.CLI.Mail.Transport
    ( decodeImapDraftId
    , decodeImapMessageId
    , decodeGmailAttachmentRef
    , imapUidHasFlag
    , mailProviderStatusError
    , parseGmailDraftValue
    , parseGmailMessageValue
    , parseGraphDraftValue
    , parseImapAppendUid
    , parseImapMailboxListLine
    , parseMailReplyRecipient
    , validateMailReplyRecipient
    )
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as BS8
import Data.Either (isLeft, isRight)
import qualified Data.Text as Text
import Data.Text (isInfixOf)
import qualified Data.Text.Encoding as TextEncoding
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
        decodeImapDraftId "imap-draft:SU5CT1gAMQA0Mg"
            `shouldBe` Right ("INBOX", "1", "42")
        decodeImapDraftId "SU5CT1gAMQA0Mg"
            `shouldSatisfy` isLeft

    it "renders draft MIME with encoded bodies and no injected header" do
        let rendered = renderMailDraftMime "sender@example.com" MailDraftContent
                { mailDraftTo = ["person@example.com"]
                , mailDraftCc = []
                , mailDraftBcc = []
                , mailDraftSubject = "Hello"
                , mailDraftBody = "draft\nbody"
                }
                Nothing
        rendered `shouldSatisfy` ("From: sender@example.com" `BS.isInfixOf`)
        rendered `shouldSatisfy` ("Content-Transfer-Encoding: base64" `BS.isInfixOf`)
        rendered `shouldSatisfy` ("ZHJhZnQKYm9keQ==" `BS.isInfixOf`)

    it "folds long recipient and Unicode subject headers safely" do
        let rendered = renderMailDraftMime "sender@example.com" MailDraftContent
                { mailDraftTo =
                    [ "person" <> Text.pack (show index) <> "@example.com"
                    | index <- [1 :: Int .. 12]
                    ]
                , mailDraftCc = []
                , mailDraftBcc = []
                , mailDraftSubject = Text.replicate 80 "ä"
                , mailDraftBody = ""
                }
                Nothing
        rendered `shouldSatisfy` (",\r\n\tperson" `BS.isInfixOf`)
        rendered `shouldSatisfy`
            ("?=\r\n\t=?UTF-8?B?" `BS.isInfixOf`)
        let encodedWords =
                filter ("=?UTF-8?B?" `BS.isPrefixOf`) (BS8.words rendered)
        encodedWords `shouldSatisfy` (not . null)
        all validEncodedWord encodedWords `shouldBe` True
        all ((<= 998) . BS.length . BS.takeWhile (/= 13))
            (BS.split 10 rendered)
            `shouldBe` True

    it "drops malformed derived reply headers instead of injecting them" do
        let rendered = renderMailDraftMime "sender@example.com" MailDraftContent
                { mailDraftTo = ["person@example.com"]
                , mailDraftCc = []
                , mailDraftBcc = []
                , mailDraftSubject = "Reply"
                , mailDraftBody = ""
                }
                (Just ("<safe@example.com>\r\nBcc: victim@example.com", Nothing))
        rendered `shouldNotSatisfy` ("In-Reply-To:" `BS.isInfixOf`)
        rendered `shouldNotSatisfy` ("victim@example.com" `BS.isInfixOf`)

    it "derives exactly one safe reply recipient from provider headers" do
        parseMailReplyRecipient
            (Just "Person <Person@Example.com>")
            (Just "fallback@example.com")
            `shouldBe` Right "person@example.com"
        parseMailReplyRecipient
            Nothing
            (Just "sender@example.com")
            `shouldBe` Right "sender@example.com"
        parseMailReplyRecipient
            (Just "first@example.com, second@example.com")
            Nothing
            `shouldSatisfy` isLeft
        parseMailReplyRecipient
            (Just "safe@example.com\r\nBcc: victim@example.com")
            Nothing
            `shouldSatisfy` isLeft
        validateMailReplyRecipient
            ["Person@Example.com"]
            "person@example.com"
            `shouldBe` Right ()
        validateMailReplyRecipient
            ["approved@example.com"]
            "different@example.com"
            `shouldSatisfy` isLeft

    it "recognizes only special-use Drafts mailboxes and stable APPENDUIDs" do
        fmap (.mailMailboxRole)
            (parseImapMailboxListLine
                "* LIST (\\HasNoChildren \\Drafts) \"/\" \"Entwürfe\"")
            `shouldBe` Just (Just "drafts")
        fmap (.mailMailboxRole)
            (parseImapMailboxListLine
                "* LIST (\\HasNoChildren) \"/\" \"Drafts\"")
            `shouldBe` Just Nothing
        parseImapAppendUid "m402 OK [APPENDUID 777 42] appended"
            `shouldBe` Just ("777", "42")
        parseImapAppendUid "m402 OK appended" `shouldBe` Nothing
        imapUidHasFlag "42" "\\Draft"
            [ "* 8 FETCH (UID 41 FLAGS (\\Draft))"
            , "* 9 FETCH (UID 42 FLAGS (\\Seen))"
            ]
            `shouldBe` False
        imapUidHasFlag "42" "\\Draft"
            ["* 9 FETCH (UID 42 FLAGS (\\Seen \\Draft))"]
            `shouldBe` True

    it "tags provider draft ids and rejects non-draft Graph responses" do
        let gmail = Aeson.object
                [ "id" .= ("gmail-resource" :: String)
                , "message" .= Aeson.object
                    [ "id" .= ("gmail-message" :: String)
                    , "threadId" .= ("gmail-thread" :: String)
                    ]
                ]
            graph isDraft = Aeson.object
                [ "id" .= ("graph-message" :: String)
                , "conversationId" .= ("graph-thread" :: String)
                , "isDraft" .= isDraft
                ]
        case parseGmailDraftValue gmail of
            Left err -> expectationFailure (show err)
            Right draft -> do
                draft.mailDraftId `shouldSatisfy`
                    ("gmail-draft:" `isInfixOf`)
                draft.mailDraftMessageId `shouldBe` Just "gmail-message"
        parseGraphDraftValue (graph False) `shouldSatisfy` isLeft
        case parseGraphDraftValue (graph True) of
            Left err -> expectationFailure (show err)
            Right draft -> do
                draft.mailDraftId `shouldSatisfy`
                    ("graph-draft:" `isInfixOf`)
                draft.mailDraftMessageId `shouldBe` Just "graph-message"

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

    it "exposes the effective Gmail Reply-To address for draft approval" do
        let header name value = Aeson.object
                [ "name" .= (name :: String)
                , "value" .= (value :: String)
                ]
            value = Aeson.object
                [ "id" .= ("gmail-reply-to" :: String)
                , "payload" .= Aeson.object
                    [ "mimeType" .= ("text/plain" :: String)
                    , "headers" .=
                        [ header "From" "Sender <sender@example.com>"
                        , header "Reply-To" "Replies <reply@example.com>"
                        ]
                    , "body" .= Aeson.object ["data" .= ("" :: String)]
                    ]
                ]
        case parseGmailMessageValue 1024 value of
            Left err -> expectationFailure (show err)
            Right message ->
                message.mailMessageReplyTo `shouldBe` Just "reply@example.com"

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

validEncodedWord :: BS.ByteString -> Bool
validEncodedWord word
    | not ("=?UTF-8?B?" `BS.isPrefixOf` word)
        || not ("?=" `BS.isSuffixOf` word)
        || BS.length word <= 12 = False
    | otherwise =
        case Base64.decode
            (BS.take (BS.length word - 12) (BS.drop 10 word)) of
            Left _ -> False
            Right bytes -> isRight (TextEncoding.decodeUtf8' bytes)
