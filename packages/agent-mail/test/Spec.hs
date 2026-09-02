module Main (main) where

import Agent.Mail.Contract
import Agent.Mail.Imap
import Agent.Mail.Mime
import Agent.Mail.OAuth
import Agent.Mail.SecretCodec
import Agent.Mail.Transport
import Agent.Mail.Types
import Data.Aeson (Result(..), Value(..), object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as AesonTypes
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime(..), fromGregorian)
import Test.Hspec

main :: IO ()
main = hspec do
    describe "email MCP contract" do
        it "keeps the canonical model-facing tool names" do
            map (.mailMcpToolName) mailMcpTools `shouldBe`
                [ "email_list_accounts"
                , "email_list_mailboxes"
                , "email_search"
                , "email_get"
                , "email_download_attachment"
                , "email_create_draft"
                , "email_update_draft"
                , "email_reply_draft"
                ]

        it "marks only draft mutations as requiring fresh approval" do
            map (.mailMcpToolName)
                (filter (.mailMcpToolRequiresFreshApproval) mailMcpTools)
                `shouldBe` mailDraftMutationToolNames

        it "round trips structured results only for the exact contract" do
            let accounts =
                    [ MailAccountSummary
                        "account-ref"
                        "gmail"
                        "person@example.com"
                        Nothing
                        True
                        True
                    ]
            decodeMailMcpResult (mailMcpSuccess accounts)
                `shouldBe` Right accounts
            let incompatible = object
                    [ "structuredContent" .= object
                        [ "contract" .= ("other" :: Text)
                        , "version" .= mailContractVersion
                        , "data" .= accounts
                        ]
                    , "isError" .= False
                    ]
            (decodeMailMcpResult incompatible
                :: Either Text [MailAccountSummary])
                `shouldBe` Left "Error in $: incompatible email MCP contract"

        it "publishes closed object schemas" do
            mailMcpToolDefinitions `shouldSatisfy` all closedSchema

        it "publishes an exact output envelope for every tool" do
            mailMcpToolDefinitions `shouldSatisfy` all hasOutputSchema

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

        it "does not expose gateway-supplied error text" do
            let result =
                    decodeMailMcpResult
                        (mailMcpFailure
                            "mailbox-controlled instructions and secret text")
                        :: Either Text [MailAccountSummary]
            result `shouldBe` Left "Error in $: email operation failed"

        it "rejects non-canonical gateway error payloads before normalizing them" do
            let oversized = object
                    [ "structuredContent" .= object
                        [ "contract" .= mailContractId
                        , "version" .= mailContractVersion
                        , "error" .= Text.replicate (16 * 1024 * 1024) " "
                        ]
                    , "isError" .= True
                    ]
            (decodeMailMcpResult oversized
                :: Either Text [MailAccountSummary])
                `shouldBe` Left
                    "Error in $: incompatible email MCP contract"

        it "rejects any draft result that claims an email was sent" do
            let unsafeResult = object
                    [ "content" .= ([] :: [Value])
                    , "structuredContent" .= object
                        [ "contract" .= mailContractId
                        , "version" .= mailContractVersion
                        , "data" .= object
                            [ "draft_id" .= ("draft-ref" :: Text)
                            , "message_id" .= (Nothing :: Maybe Text)
                            , "thread_id" .= (Nothing :: Maybe Text)
                            , "warning" .= (Nothing :: Maybe Text)
                            , "saved" .= True
                            , "sent" .= True
                            ]
                        ]
                    , "isError" .= False
                    ]
            (decodeMailMcpResult unsafeResult :: Either Text MailDraft)
                `shouldSatisfy` \case
                    Left _ -> True
                    Right _ -> False

    describe "OAuth PKCE" do
        it "matches the RFC 7636 S256 example" do
            mailOAuthPkceChallenge
                "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk"
                `shouldBe`
                    "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM"

    describe "explicit secret storage codec" do
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

    describe "injected IMAP socket connector" do
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

closedSchema :: Value -> Bool
closedSchema (Object tool) =
    case KeyMap.lookup "inputSchema" tool of
        Just (Object input) ->
            KeyMap.lookup "additionalProperties" input == Just (Bool False)
        _ -> False
closedSchema _ = False

hasOutputSchema :: Value -> Bool
hasOutputSchema (Object tool) =
    case KeyMap.lookup "outputSchema" tool of
        Just (Object output) -> KeyMap.member "oneOf" output
        _ -> False
hasOutputSchema _ = False

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
