module Main (main) where

import Agent.Mail.Imap
import Agent.Mail.Mime
import Agent.Mail.OAuth
import Agent.Mail.SecretCodec
import Agent.Mail.Transport
import Agent.Mail.Types
import qualified Agent.Mail.Types as MailTypes
import Data.Aeson (Result(..), Value(..), object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as AesonTypes
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Time (UTCTime(..), fromGregorian)
import Network.HTTP.Types (mkStatus)
import Test.Hspec

main :: IO ()
main = hspec do
    describe "email domain types" do
        it "requires fail-closed account status flags" do
            let partial = object
                    [ "account_id" .= ("account-ref" :: Text)
                    , "provider" .= ("gmail" :: Text)
                    , "email" .= ("person@example.com" :: Text)
                    ]
            (Aeson.fromJSON partial :: Result MailAccountSummary)
                `shouldSatisfy` \case
                    Error _ -> True
                    Success _ -> False
            let storedAccount = object
                    [ "id" .= ("account-ref" :: Text)
                    , "provider" .= ("gmail" :: Text)
                    , "email" .= ("person@example.com" :: Text)
                    , "created_at" .= fixedTime
                    , "updated_at" .= fixedTime
                    ]
            (Aeson.fromJSON storedAccount :: Result MailAccount)
                `shouldSatisfy` \case
                    Error _ -> True
                    Success _ -> False

        it "omits absent optional search fields instead of encoding null" do
            let request = MailSearchRequest
                    { mailSearchAccountId = "account-ref"
                    , mailSearchMailboxId = Nothing
                    , mailSearchQuery = Nothing
                    , mailSearchFrom = Nothing
                    , mailSearchTo = Nothing
                    , mailSearchSubject = Nothing
                    , mailSearchAfter = Nothing
                    , mailSearchBefore = Nothing
                    , mailSearchHasAttachments = Nothing
                    , mailSearchLimit = 20
                    }
            case Aeson.toJSON request of
                Object value -> do
                    KeyMap.member "account_id" value `shouldBe` True
                    KeyMap.member "limit" value `shouldBe` True
                    KeyMap.member "query" value `shouldBe` False
                    KeyMap.member "mailbox_id" value `shouldBe` False
                    KeyMap.member "has_attachments" value `shouldBe` False
                _ -> expectationFailure "expected a search object"

        it "rejects any draft result that claims an email was sent" do
            let unsafeResult = object
                    [ "draft_id" .= ("draft-ref" :: Text)
                    , "message_id" .= (Nothing :: Maybe Text)
                    , "thread_id" .= (Nothing :: Maybe Text)
                    , "warning" .= (Nothing :: Maybe Text)
                    , "saved" .= True
                    , "sent" .= True
                    ]
            (Aeson.fromJSON unsafeResult :: Result MailDraft)
                `shouldSatisfy` \case
                    Error _ -> True
                    Success _ -> False

        it "accepts only an affirmative send result" do
            Aeson.fromJSON (Aeson.toJSON MailSendResult)
                `shouldBe` Success MailSendResult
            let unconfirmed = object ["sent" .= False]
            (Aeson.fromJSON unconfirmed :: Result MailSendResult)
                `shouldSatisfy` \case
                    Error _ -> True
                    Success _ -> False

    describe "OAuth PKCE" do
        it "requests Microsoft account selection without changing Google consent or PKCE" do
            let url provider = mailOAuthAuthorizationUrl
                    (MailOAuthClient provider "client" Nothing "http://localhost:54321")
                    "state" "verifier"
            url MicrosoftProvider `shouldSatisfy`
                either (const False) (\value ->
                    "prompt=select_account" `Text.isInfixOf` value
                    && "code_challenge_method=S256" `Text.isInfixOf` value)
            url GmailProvider `shouldSatisfy`
                either (const False) (\value ->
                    "prompt=consent" `Text.isInfixOf` value
                    && not ("select_account" `Text.isInfixOf` value))

        it "matches the RFC 7636 S256 example" do
            mailOAuthPkceChallenge
                "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk"
                `shouldBe`
                    "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM"

        it "requests the Microsoft permission needed to send approved drafts" do
            Text.words (mailOAuthScopes MicrosoftProvider)
                `shouldContain` ["Mail.Send"]

    describe "Microsoft mailbox response diagnostics" do
        let decode status = decodeMailOAuthMailboxResponse MicrosoftProvider
                (mkStatus status "")
            diagnosticContains expected result =
                result `shouldSatisfy` either (Text.isInfixOf expected) (const False)
        it "decodes successful profile responses" do
            decode 200 "{\"mail\":\"person@example.com\"}" `shouldBe`
                Right (object ["mail" .= ("person@example.com" :: Text)])
        it "identifies unsupported mailboxes without exposing provider details" do
            let result = decode 404 "{\"error\":{\"code\":\"MailboxNotEnabledForRESTAPI\",\"message\":\"private-provider-detail\"}}"
            diagnosticContains "does not have a supported Outlook mailbox" result
            show result `shouldNotContain` "private-provider-detail"
        it "distinguishes authorization and permission failures" do
            diagnosticContains "rejected the mailbox authorization" (decode 401 "{}")
            diagnosticContains "consent and access policies" (decode 403 "{}")
        it "distinguishes throttling and service failures even with non-JSON bodies" do
            diagnosticContains "limiting mailbox requests" (decode 429 "private-body")
            diagnosticContains "temporarily unavailable" (decode 503 "private-body")
        it "does not expose unknown provider errors or malformed successful responses" do
            let result = decode 400 "{\"error\":{\"code\":\"Unknown\",\"message\":\"private-detail\"}}"
            diagnosticContains "could not verify this mailbox" result
            show result `shouldNotContain` "private-detail"
            decode 200 "private-invalid-json" `shouldSatisfy` either
                (not . Text.isInfixOf "private-invalid-json") (const False)
        it "preserves Gmail diagnostics" do
            decodeMailOAuthMailboxResponse GmailProvider (mkStatus 403 "") "{}"
                `shouldBe` Left "Mail provider did not return a usable mailbox identity."

    describe "explicit secret storage codec" do
        it "round trips the installed OAuth client parameter without exposing it in Show" do
            let secret = (validOAuthCredential "account-1").mailCredentialSecret
                    { MailTypes.mailOAuthClientSecret = Just "synthetic-desktop-parameter" }
            AesonTypes.parseEither
                parseMailSecretStorageValue
                (mailSecretStorageValue secret)
                `shouldBe` Right secret
            show secret `shouldNotContain` "synthetic-desktop-parameter"
            show secret `shouldNotContain` "access-token"
            show secret `shouldNotContain` "refresh-token"

        it "reads existing OAuth credentials without a client parameter" do
            let secret = (validOAuthCredential "account-1").mailCredentialSecret
                legacy = case mailSecretStorageValue secret of
                    Object value -> Object (KeyMap.delete "client_secret" value)
                    value -> value
            AesonTypes.parseEither parseMailSecretStorageValue legacy
                `shouldBe` Right secret

        it "round trips OAuth credentials with no installed client parameter" do
            let secret = (validOAuthCredential "account-1").mailCredentialSecret
            AesonTypes.parseEither
                parseMailSecretStorageValue
                (mailSecretStorageValue secret)
                `shouldBe` Right secret

        it "round trips only through the opt-in codec and redacts Show" do
            let secret = MailImapSecret
                    { mailSecretAccountId = "account-1"
                    , mailImapPassword = "do-not-log-this"
                    }
            AesonTypes.parseEither
                parseMailSecretStorageValue
                (mailSecretStorageValue secret)
                `shouldBe` Right secret
            show secret `shouldNotContain` "do-not-log-this"

        it "redacts attachment bytes and one-time download references" do
            let content = MailAttachmentContent
                    { mailDownloadedAttachmentFilename = Just "private.txt"
                    , mailDownloadedAttachmentContentType = Just "text/plain"
                    , mailDownloadedAttachmentBytes = "private attachment"
                    }
                download = MailAttachmentDownload
                    { mailAttachmentDownloadRef = "one-time-capability"
                    , mailAttachmentDownloadFilename = Just "private.txt"
                    , mailAttachmentDownloadContentType = Just "text/plain"
                    , mailAttachmentDownloadSizeBytes = 18
                    }
                parsed = ParsedMailAttachment
                    { parsedMailAttachmentId = "imap-part-capability"
                    , parsedMailAttachmentFilename = "private.txt"
                    , parsedMailAttachmentContentType = "text/plain"
                    , parsedMailAttachmentBytes = "decoded private attachment"
                    }
            show content `shouldNotContain` "private attachment"
            show content `shouldNotContain` "private.txt"
            show download `shouldNotContain` "one-time-capability"
            show download `shouldNotContain` "private.txt"
            show parsed `shouldNotContain` "decoded private attachment"
            show parsed `shouldNotContain` "imap-part-capability"
            show parsed `shouldNotContain` "private.txt"

    describe "bounded Gmail MIME traversal" do
        it "uses one traversal for nested bodies and attachments" do
            let payload = object
                    [ "parts" .=
                        [ object ["parts" .= [gmailTextPart]]
                        , gmailAttachmentPart
                        ]
                    ]
            case parseGmailMessageValue 1024
                (gmailMessageWithPayload payload) of
                Left _ -> expectationFailure "expected the Gmail message to parse"
                Right message -> do
                    message.mailMessageBody `shouldBe` Just "shared body"
                    length message.mailMessageAttachments `shouldBe` 1

        it "rejects MIME trees nested beyond 32 levels" do
            parseGmailMessageValue 1024
                (gmailMessageWithPayload (gmailNestedPart 32))
                `shouldSatisfy` \case
                    Right _ -> True
                    Left _ -> False
            parseGmailMessageValue 1024
                (gmailMessageWithPayload (gmailNestedPart 33))
                `shouldSatisfy` isFailure

        it "rejects MIME trees with more than 512 total parts" do
            parseGmailMessageValue 1024
                (gmailMessageWithPayload
                    (object ["parts" .= replicate 511 gmailEmptyPart]))
                `shouldSatisfy` \case
                    Right _ -> True
                    Left _ -> False
            parseGmailMessageValue 1024
                (gmailMessageWithPayload
                    (object ["parts" .= replicate 512 gmailEmptyPart]))
                `shouldSatisfy` isFailure

        it "allows 200 attachments but rejects an attachment overflow" do
            parseGmailMessageValue 1024
                (gmailMessageWithPayload
                    (object ["parts" .= replicate 200 gmailAttachmentPart]))
                `shouldSatisfy` \case
                    Right message ->
                        length message.mailMessageAttachments == 200
                    Left _ -> False
            parseGmailMessageValue 1024
                (gmailMessageWithPayload
                    (object ["parts" .= replicate 201 gmailAttachmentPart]))
                `shouldSatisfy` isFailure

        it "decodes RFC 2231 attachment filenames consistently" do
            let raw = TextEncoding.encodeUtf8
                    "Content-Type: application/octet-stream; name=\"\"; name*0*=utf-8''report%20; name*1*=final.pdf\r\n\
                    \Content-Disposition: inline\r\n\
                    \Content-Transfer-Encoding: 7bit\r\n\r\n\
                    \attachment bytes"
            case parseMailMime raw of
                Left err -> expectationFailure (Text.unpack err)
                Right parsed ->
                    map (.parsedMailAttachmentFilename)
                        (mailMimeAttachments parsed)
                        `shouldBe` ["report final.pdf"]

    describe "injected IMAP socket connector" do
        it "recursively includes bounded Microsoft child folders" do
            requests <- newIORef []
            let fetch endpoint remaining = do
                    modifyIORef' requests (<> [(endpoint, remaining)])
                    pure $ Right
                        if endpoint
                                == "https://graph.microsoft.com/v1.0/me/mailFolders"
                            then
                                ( [ (mailbox "inbox" "Inbox", False)
                                  , (mailbox "archive" "Archive", True)
                                  ]
                                , Just
                                    "https://graph.microsoft.com/v1.0/me/mailFolders?$skiptoken=next"
                                )
                            else if "$skiptoken=next" `Text.isSuffixOf` endpoint
                                then
                                    ([(mailbox "sent" "Sent", False)], Nothing)
                            else if "/archive/childFolders" `Text.isSuffixOf` endpoint
                                then ([(mailbox "year" "2026", True)], Nothing)
                            else if "/year/childFolders" `Text.isSuffixOf` endpoint
                                then
                                    ( [ (mailbox
                                            "september"
                                            "September", False)
                                      ]
                                    , Nothing
                                    )
                            else ([], Nothing)
            result <- graphListMailboxesWith 5 fetch
            fmap (map (.mailMailboxId)) result
                `shouldBe`
                    Right ["inbox", "archive", "sent", "year", "september"]
            fmap snd <$> readIORef requests `shouldReturn` [5, 3, 2, 1]

        it "continues IMAP attachment filtering beyond the first 50 hits" do
            fetched <- newIORef (0 :: Int)
            let fetch uid = do
                    modifyIORef' fetched (+ 1)
                    pure (searchSummary uid (uid == (61 :: Int)))
            found <- collectImapSearchMatches 100 1 fetch
                (.mailMessageSummaryHasAttachments)
                [1 .. 100 :: Int]
            fmap (map (.mailMessageSummaryId)) found `shouldBe` Right ["61"]
            readIORef fetched `shouldReturn` 61

        it "reports an incomplete bounded IMAP attachment scan" do
            fetched <- newIORef (0 :: Int)
            let fetch uid = do
                    modifyIORef' fetched (+ 1)
                    pure (searchSummary uid False)
            found <- collectImapSearchMatches 50 1 fetch
                (.mailMessageSummaryHasAttachments)
                [1 .. 100 :: Int]
            found `shouldBe` Left
                "The IMAP attachment-filtered search exceeded its bounded scan. Narrow the search and try again."
            readIORef fetched `shouldReturn` 50

        it "recognizes disposition and filename-only IMAP attachments" do
            imapBodyStructureHasAttachment
                "* 1 FETCH (BODYSTRUCTURE (\"APPLICATION\" \"PDF\" NIL NIL NIL \"BASE64\" 12 NIL (\"ATTACHMENT\" NIL)))"
                `shouldBe` True
            imapBodyStructureHasAttachment
                "* 1 FETCH (BODYSTRUCTURE (\"APPLICATION\" \"PDF\" (\"NAME\" \"report.pdf\") NIL NIL \"BASE64\" 12 NIL \"INLINE\"))"
                `shouldBe` True
            imapBodyStructureHasAttachment
                "* 1 FETCH (BODYSTRUCTURE (\"APPLICATION\" \"PDF\" (\"NAME*0*\" \"utf-8''report%20\") NIL NIL \"BASE64\" 12 NIL \"INLINE\"))"
                `shouldBe` True
            imapBodyStructureHasAttachment
                "* 1 FETCH (BODYSTRUCTURE (\"TEXT\" \"PLAIN\" (\"CHARSET\" \"UTF-8\") NIL NIL \"7BIT\" 12 1))"
                `shouldBe` False
            imapBodyStructureHasAttachment
                "* 1 FETCH (BODYSTRUCTURE (\"MESSAGE\" \"RFC822\" NIL NIL NIL \"7BIT\" 100 (NIL \"Subject\" ((\"Attachment\" NIL \"sender\" \"example.com\")) NIL NIL NIL NIL NIL NIL NIL) (\"TEXT\" \"PLAIN\" (\"CHARSET\" \"UTF-8\") NIL NIL \"7BIT\" 12 1) 1 NIL NIL))"
                `shouldBe` False

        it "builds one-shot Microsoft send and reply payloads from approved content" do
            let content = MailDraftContent
                    { mailDraftTo = ["to@example.com"]
                    , mailDraftCc = ["cc@example.com"]
                    , mailDraftBcc = ["bcc@example.com"]
                    , mailDraftSubject = "Approved subject"
                    , mailDraftBody = "Approved body"
                    }
                message = object
                    [ "toRecipients" .=
                        [object
                            [ "emailAddress" .= object
                                ["address" .= ("to@example.com" :: Text)]
                            ]]
                    , "ccRecipients" .=
                        [object
                            [ "emailAddress" .= object
                                ["address" .= ("cc@example.com" :: Text)]
                            ]]
                    , "bccRecipients" .=
                        [object
                            [ "emailAddress" .= object
                                ["address" .= ("bcc@example.com" :: Text)]
                            ]]
                    , "subject" .= ("Approved subject" :: Text)
                    , "body" .= object
                        [ "contentType" .= ("text" :: Text)
                        , "content" .= ("Approved body" :: Text)
                        ]
                    ]
            graphSendMailPayload content `shouldBe` object
                [ "message" .= message
                , "saveToSentItems" .= True
                ]
            graphReplyMailPayload content `shouldBe` object
                ["message" .= message]

        it "keeps IMAP draft replacement append-only" do
            let commands = imapUpdateDraftPostAppendCommands "42"
                transcript = Text.unwords (map snd commands)
            commands `shouldBe` []
            Text.toCaseFold transcript
                `shouldSatisfy` (not . Text.isInfixOf "store")
            Text.toCaseFold transcript
                `shouldSatisfy` (not . Text.isInfixOf "\\deleted")
            Text.toCaseFold transcript
                `shouldSatisfy` (not . Text.isInfixOf "expunge")

        it "does not run before settings validation" do
            invoked <- newIORef False
            result <- withMailImapConnectionUsing
                (\_ -> do
                    writeIORef invoked True
                    ioError (userError "must not run"))
                validImapSettings { mailImapHost = "" }
                "app-password"
                (\_ -> pure ())
            readIORef invoked `shouldReturn` False
            result `shouldSatisfy` isFailure

        it "receives the original settings and sanitizes connector errors" do
            received <- newIORef Nothing
            result <- withMailImapConnectionUsing
                (\settings -> do
                    writeIORef received (Just settings)
                    ioError (userError "sensitive connector detail"))
                validImapSettings
                "app-password"
                (\_ -> pure ())
            readIORef received `shouldReturn` Just validImapSettings
            result `shouldBe`
                (Left "Could not securely validate the IMAP account."
                    :: Either Text ())

        it "is used by the IMAP transport while preserving provider hooks" do
            invoked <- newIORef False
            let transport = mailTransportWithImapConnector
                    (\_ -> do
                        writeIORef invoked True
                        ioError (userError "connector failed"))
                    noOpTransportHooks
            result <- transport.mailTransportListMailboxes
                validImapCredential
                1
            readIORef invoked `shouldReturn` True
            result `shouldSatisfy` isFailure

        it "rejects a mismatched secret before opening a socket" do
            invoked <- newIORef False
            let transport = mailTransportWithImapConnector
                    (\_ -> do
                        writeIORef invoked True
                        ioError (userError "must not run"))
                    noOpTransportHooks
                mismatched = validImapCredential
                    { mailCredentialSecret = MailImapSecret
                        { mailSecretAccountId = "another-account"
                        , mailImapPassword = "app-password"
                        }
                    }
            result <- transport.mailTransportListMailboxes mismatched 1
            readIORef invoked `shouldReturn` False
            result `shouldBe`
                (Left "The email account credential is invalid."
                    :: Either Text [MailboxSummary])

        it "does not attempt SMTP for custom IMAP send requests" do
            invoked <- newIORef False
            let transport = mailTransportWithImapConnector
                    (\_ -> do
                        writeIORef invoked True
                        ioError (userError "must not run"))
                    noOpTransportHooks
                request = MailSendRequest
                    { mailSendAccountId = "account-1"
                    , mailSendDraftId = "imap-draft"
                    , mailSendContent = MailDraftContent
                        { mailDraftTo = ["recipient@example.com"]
                        , mailDraftCc = []
                        , mailDraftBcc = []
                        , mailDraftSubject = "Hello"
                        , mailDraftBody = "Body"
                        }
                    }
            result <- transport.mailTransportSend validImapCredential request
            readIORef invoked `shouldReturn` False
            result `shouldBe` Left
                "Custom IMAP accounts cannot send email because no SMTP connection is configured."

        it "rejects a refreshed OAuth credential for another account" do
            let original = validOAuthCredential "account-1"
                replacement = validOAuthCredential "account-2"
                transport = mailTransportWithHooks noOpTransportHooks
                    { mailTransportRefreshCredential =
                        const (pure (Right replacement))
                    }
            result <- transport.mailTransportListMailboxes original 1
            result `shouldBe`
                (Left "The email account credential is invalid."
                    :: Either Text [MailboxSummary])

gmailMessageWithPayload :: Value -> Value
gmailMessageWithPayload payload = object
    [ "id" .= ("gmail-message" :: Text)
    , "payload" .= payload
    ]

gmailEmptyPart :: Value
gmailEmptyPart = object []

gmailNestedPart :: Int -> Value
gmailNestedPart depth
    | depth <= 0 = gmailEmptyPart
    | otherwise = object ["parts" .= [gmailNestedPart (depth - 1)]]

gmailAttachmentPart :: Value
gmailAttachmentPart = object
    [ "filename" .= ("attachment.txt" :: Text)
    , "mimeType" .= ("text/plain" :: Text)
    , "body" .= object
        [ "attachmentId" .= ("provider-attachment" :: Text)
        , "size" .= (1 :: Int)
        ]
    ]

gmailTextPart :: Value
gmailTextPart = object
    [ "mimeType" .= ("text/plain" :: Text)
    , "body" .= object
        [ "data" .= ("c2hhcmVkIGJvZHk" :: Text)
        ]
    ]

validImapSettings :: MailImapSettings
validImapSettings = MailImapSettings
    { mailImapHost = "imap.example.com"
    , mailImapPort = 993
    , mailImapTLSMode = MailImplicitTLS
    , mailImapUsername = "person@example.com"
    }

validOAuthCredential :: Text -> MailCredential
validOAuthCredential accountId = MailCredential
    { mailCredentialAccount = MailAccount
        { mailAccountId = accountId
        , mailAccountProvider = GmailProvider
        , mailAccountEmail = "person@example.com"
        , mailAccountLabel = "Personal"
        , mailAccountEnabled = True
        , mailAccountState = MailConnected
        , mailAccountImapSettings = Nothing
        , mailAccountOAuthClientId = Just
            "client.apps.googleusercontent.com"
        , mailAccountCreatedAt = fixedTime
        , mailAccountUpdatedAt = fixedTime
        , mailAccountLastVerifiedAt = Just fixedTime
        , mailAccountLastErrorCode = Nothing
        }
    , mailCredentialSecret = MailOAuthSecret
        { mailSecretAccountId = accountId
        , mailOAuthAccessToken = "access-token"
        , mailOAuthRefreshToken = Just "refresh-token"
        , mailOAuthExpiresAt = Nothing
        , mailOAuthScopes =
            ["https://www.googleapis.com/auth/gmail.readonly"]
        , mailOAuthClientSecret = Nothing
        }
    }

validImapCredential :: MailCredential
validImapCredential = MailCredential
    { mailCredentialAccount = MailAccount
        { mailAccountId = "account-1"
        , mailAccountProvider = ImapProvider
        , mailAccountEmail = "person@example.com"
        , mailAccountLabel = "Personal"
        , mailAccountEnabled = True
        , mailAccountState = MailConnected
        , mailAccountImapSettings = Just validImapSettings
        , mailAccountOAuthClientId = Nothing
        , mailAccountCreatedAt = fixedTime
        , mailAccountUpdatedAt = fixedTime
        , mailAccountLastVerifiedAt = Just fixedTime
        , mailAccountLastErrorCode = Nothing
        }
    , mailCredentialSecret = MailImapSecret
        { mailSecretAccountId = "account-1"
        , mailImapPassword = "app-password"
        }
    }

noOpTransportHooks :: MailTransportHooks
noOpTransportHooks = MailTransportHooks
    { mailTransportRefreshCredential = pure . Right
    , mailTransportRecordAccountState = \_ _ _ -> pure (Right ())
    }

fixedTime :: UTCTime
fixedTime = UTCTime (fromGregorian 2026 1 1) 0

isFailure :: Either left value -> Bool
isFailure = \case
    Left _ -> True
    Right _ -> False

mailbox :: Text -> Text -> MailboxSummary
mailbox identifier name = MailboxSummary
    { mailMailboxId = identifier
    , mailMailboxName = name
    , mailMailboxRole = Nothing
    , mailMailboxUnreadCount = Nothing
    }

searchSummary :: Int -> Bool -> MailMessageSummary
searchSummary identifier hasAttachments = MailMessageSummary
    { mailMessageSummaryId = Text.pack (show identifier)
    , mailMessageSummaryThreadId = Nothing
    , mailMessageSummarySubject = Nothing
    , mailMessageSummaryFrom = Nothing
    , mailMessageSummaryReplyTo = Nothing
    , mailMessageSummaryTo = Nothing
    , mailMessageSummaryReceivedAt = Nothing
    , mailMessageSummarySnippet = Nothing
    , mailMessageSummaryHasAttachments = hasAttachments
    , mailMessageSummaryAttachmentCount = Nothing
    }
