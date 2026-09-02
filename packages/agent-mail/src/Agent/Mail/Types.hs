-- | Provider-neutral email values shared by local and gateway backends.
--
-- 'MailSecret' and 'MailCredential' are secret-bearing transport values and
-- intentionally have no ambient JSON instances.  The remaining public result
-- values are safe to encode in the first-party MCP protocol after callers
-- apply the normal mailbox-content trust warning and output limits.
module Agent.Mail.Types
    ( MailToolLimits(..)
    , defaultMailToolLimits
    , MailProvider(..)
    , mailProviderSlug
    , parseMailProvider
    , MailTLSMode(..)
    , mailTLSModeSlug
    , MailImapSettings(..)
    , MailAccountState(..)
    , mailAccountStateSlug
    , MailAccount(..)
    , MailSecret(..)
    , MailCredential(..)
    , MailAccountSummary(..)
    , MailboxSummary(..)
    , MailSearchRequest(..)
    , MailMessageSummary(..)
    , MailGetRequest(..)
    , MailMessage(..)
    , MailAttachment(..)
    , MailAttachmentRequest(..)
    , MailAttachmentContent(..)
    , MailAttachmentDownload(..)
    , MailDraftContent(..)
    , MailCreateDraftRequest(..)
    , MailUpdateDraftRequest(..)
    , MailReplyDraftRequest(..)
    , MailDraft(..)
    , MailTransport(..)
    , MailTransportHooks(..)
    , normalizeMailEmail
    , validateMailImapSettings
    , validateMailOAuthClientId
    , validateMailSearchRequest
    , validateMailGetRequest
    , validateMailAttachmentRequest
    , validateMailDraftContent
    , validateOpaqueMailReference
    ) where

import Control.Monad (when)
import Data.Aeson
    ( FromJSON(..)
    , ToJSON(..)
    , object
    , withObject
    , withText
    , (.:)
    , (.:?)
    , (.!=)
    , (.=)
    )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString as BS
import Data.Char
    ( isAlphaNum
    , isAscii
    , isControl
    , isHexDigit
    , isSpace
    )
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Time
    ( Day
    , UTCTime
    , defaultTimeLocale
    , parseTimeM
    )

data MailToolLimits = MailToolLimits
    { mailRequestTimeoutMicros :: !Int
    , mailMaximumSearchResults :: !Int
    , mailMaximumMailboxes :: !Int
    , mailMaximumBodyBytes :: !Int
    , mailMaximumAttachmentBytes :: !Int
    , mailMaximumDraftBodyBytes :: !Int
    , mailMaximumResultBytes :: !Int
    }
    deriving (Eq, Show)

defaultMailToolLimits :: MailToolLimits
defaultMailToolLimits = MailToolLimits
    { mailRequestTimeoutMicros = 15 * 1_000_000
    , mailMaximumSearchResults = 50
    , mailMaximumMailboxes = 200
    , mailMaximumBodyBytes = 48 * 1024
    , mailMaximumAttachmentBytes = 20 * 1024 * 1024
    , mailMaximumDraftBodyBytes = 128 * 1024
    , mailMaximumResultBytes = 96 * 1024
    }

data MailProvider
    = GmailProvider
    | MicrosoftProvider
    | ImapProvider
    deriving (Eq, Ord, Show)

mailProviderSlug :: MailProvider -> Text
mailProviderSlug = \case
    GmailProvider -> "gmail"
    MicrosoftProvider -> "microsoft"
    ImapProvider -> "imap"

parseMailProvider :: Text -> Maybe MailProvider
parseMailProvider raw =
    case Text.toCaseFold (Text.strip raw) of
        "gmail" -> Just GmailProvider
        "google" -> Just GmailProvider
        "microsoft" -> Just MicrosoftProvider
        "outlook" -> Just MicrosoftProvider
        "imap" -> Just ImapProvider
        _ -> Nothing

instance ToJSON MailProvider where
    toJSON = Aeson.String . mailProviderSlug

instance FromJSON MailProvider where
    parseJSON = withText "MailProvider" \raw ->
        maybe (fail "unknown mail provider") pure (parseMailProvider raw)

data MailTLSMode
    = MailImplicitTLS
    | MailStartTLS
    deriving (Eq, Ord, Show)

mailTLSModeSlug :: MailTLSMode -> Text
mailTLSModeSlug = \case
    MailImplicitTLS -> "tls"
    MailStartTLS -> "starttls"

instance ToJSON MailTLSMode where
    toJSON = Aeson.String . mailTLSModeSlug

instance FromJSON MailTLSMode where
    parseJSON = withText "MailTLSMode" \raw ->
        case Text.toCaseFold raw of
            "tls" -> pure MailImplicitTLS
            "ssl" -> pure MailImplicitTLS
            "implicit_tls" -> pure MailImplicitTLS
            "starttls" -> pure MailStartTLS
            _ -> fail "mail TLS mode must be tls or starttls"

data MailImapSettings = MailImapSettings
    { mailImapHost :: !Text
    , mailImapPort :: !Int
    , mailImapTLSMode :: !MailTLSMode
    , mailImapUsername :: !Text
    }
    deriving (Eq)

instance Show MailImapSettings where
    show _ = "MailImapSettings { <redacted> }"

instance ToJSON MailImapSettings where
    toJSON settings = object
        [ "host" .= settings.mailImapHost
        , "port" .= settings.mailImapPort
        , "tls_mode" .= settings.mailImapTLSMode
        , "username" .= settings.mailImapUsername
        ]

instance FromJSON MailImapSettings where
    parseJSON = withObject "MailImapSettings" \value ->
        MailImapSettings
            <$> value .: "host"
            <*> value .: "port"
            <*> value .: "tls_mode"
            <*> value .: "username"

data MailAccountState
    = MailConnected
    | MailNeedsReauthorization
    | MailConnectionError
    deriving (Eq, Ord, Show)

mailAccountStateSlug :: MailAccountState -> Text
mailAccountStateSlug = \case
    MailConnected -> "connected"
    MailNeedsReauthorization -> "needs_reauth"
    MailConnectionError -> "connection_error"

instance ToJSON MailAccountState where
    toJSON = Aeson.String . mailAccountStateSlug

instance FromJSON MailAccountState where
    parseJSON = withText "MailAccountState" \value ->
        case Text.toCaseFold value of
            "connected" -> pure MailConnected
            "needs_reauth" -> pure MailNeedsReauthorization
            "needs_reauthorization" ->
                pure MailNeedsReauthorization
            "connection_error" -> pure MailConnectionError
            "validation_failed" -> pure MailConnectionError
            _ -> fail "unknown mail account state"

data MailAccount = MailAccount
    { mailAccountId :: !Text
    , mailAccountProvider :: !MailProvider
    , mailAccountEmail :: !Text
    , mailAccountLabel :: !Text
    , mailAccountEnabled :: !Bool
    , mailAccountState :: !MailAccountState
    , mailAccountImapSettings :: !(Maybe MailImapSettings)
    , mailAccountOAuthClientId :: !(Maybe Text)
    , mailAccountCreatedAt :: !UTCTime
    , mailAccountUpdatedAt :: !UTCTime
    , mailAccountLastVerifiedAt :: !(Maybe UTCTime)
    , mailAccountLastErrorCode :: !(Maybe Text)
    }
    deriving (Eq)

instance Show MailAccount where
    show account =
        "MailAccount { mailAccountProvider = "
            <> show account.mailAccountProvider
            <> ", mailAccountEnabled = "
            <> show account.mailAccountEnabled
            <> ", mailAccountState = "
            <> show account.mailAccountState
            <> ", privateFields = <redacted> }"

instance ToJSON MailAccount where
    toJSON account = object
        [ "id" .= account.mailAccountId
        , "provider" .= account.mailAccountProvider
        , "email" .= account.mailAccountEmail
        , "label" .= account.mailAccountLabel
        , "enabled" .= account.mailAccountEnabled
        , "state" .= account.mailAccountState
        , "imap" .= account.mailAccountImapSettings
        , "oauth_client_id" .= account.mailAccountOAuthClientId
        , "created_at" .= account.mailAccountCreatedAt
        , "updated_at" .= account.mailAccountUpdatedAt
        , "last_verified_at" .= account.mailAccountLastVerifiedAt
        , "last_error_code" .= account.mailAccountLastErrorCode
        ]

instance FromJSON MailAccount where
    parseJSON = withObject "MailAccount" \value ->
        MailAccount
            <$> value .: "id"
            <*> value .: "provider"
            <*> value .: "email"
            <*> value .:? "label" .!= ""
            <*> value .: "enabled"
            <*> value .: "state"
            <*> value .:? "imap"
            <*> value .:? "oauth_client_id"
            <*> value .: "created_at"
            <*> value .: "updated_at"
            <*> value .:? "last_verified_at"
            <*> value .:? "last_error_code"

data MailSecret
    = MailOAuthSecret
        { mailSecretAccountId :: !Text
        , mailOAuthAccessToken :: !Text
        , mailOAuthRefreshToken :: !(Maybe Text)
        , mailOAuthExpiresAt :: !(Maybe UTCTime)
        , mailOAuthScopes :: ![Text]
        }
    | MailImapSecret
        { mailSecretAccountId :: !Text
        , mailImapPassword :: !Text
        }
    deriving (Eq)

instance Show MailSecret where
    show = \case
        MailOAuthSecret {} -> "MailOAuthSecret { <redacted> }"
        MailImapSecret {} -> "MailImapSecret { <redacted> }"

data MailCredential = MailCredential
    { mailCredentialAccount :: !MailAccount
    , mailCredentialSecret :: !MailSecret
    }
    deriving (Eq)

instance Show MailCredential where
    show credential =
        "MailCredential { mailCredentialAccount = "
            <> show credential.mailCredentialAccount
            <> ", mailCredentialSecret = <redacted> }"

data MailAccountSummary = MailAccountSummary
    { mailAccountId :: !Text
    , mailAccountProvider :: !Text
    , mailAccountEmail :: !Text
    , mailAccountLabel :: !(Maybe Text)
    , mailAccountEnabled :: !Bool
    , mailAccountVerified :: !Bool
    }
    deriving (Eq)

instance Show MailAccountSummary where
    show account =
        "MailAccountSummary { privateFields = <redacted>"
            <> ", mailAccountEnabled = "
            <> show account.mailAccountEnabled
            <> ", mailAccountVerified = "
            <> show account.mailAccountVerified
            <> " }"

instance ToJSON MailAccountSummary where
    toJSON account = object
        [ "account_id" .= account.mailAccountId
        , "provider" .= account.mailAccountProvider
        , "email" .= account.mailAccountEmail
        , "label" .= account.mailAccountLabel
        , "enabled" .= account.mailAccountEnabled
        , "verified" .= account.mailAccountVerified
        ]

instance FromJSON MailAccountSummary where
    parseJSON = withObject "MailAccountSummary" \value ->
        MailAccountSummary
            <$> value .: "account_id"
            <*> value .: "provider"
            <*> value .: "email"
            <*> value .: "label"
            <*> value .: "enabled"
            <*> value .: "verified"

data MailboxSummary = MailboxSummary
    { mailMailboxId :: !Text
    , mailMailboxName :: !Text
    , mailMailboxRole :: !(Maybe Text)
    , mailMailboxUnreadCount :: !(Maybe Int)
    }
    deriving (Eq)

instance Show MailboxSummary where
    show _ = "MailboxSummary { <redacted> }"

instance ToJSON MailboxSummary where
    toJSON mailbox = object
        [ "mailbox_id" .= mailbox.mailMailboxId
        , "name" .= mailbox.mailMailboxName
        , "role" .= mailbox.mailMailboxRole
        , "unread_count" .= mailbox.mailMailboxUnreadCount
        ]

instance FromJSON MailboxSummary where
    parseJSON = withObject "MailboxSummary" \value ->
        MailboxSummary
            <$> value .: "mailbox_id"
            <*> value .: "name"
            <*> value .: "role"
            <*> value .: "unread_count"

data MailSearchRequest = MailSearchRequest
    { mailSearchAccountId :: !Text
    , mailSearchMailboxId :: !(Maybe Text)
    , mailSearchQuery :: !(Maybe Text)
    , mailSearchFrom :: !(Maybe Text)
    , mailSearchTo :: !(Maybe Text)
    , mailSearchSubject :: !(Maybe Text)
    , mailSearchAfter :: !(Maybe Text)
    , mailSearchBefore :: !(Maybe Text)
    , mailSearchHasAttachments :: !(Maybe Bool)
    , mailSearchLimit :: !Int
    }
    deriving (Eq)

instance Show MailSearchRequest where
    show request =
        "MailSearchRequest { privateFields = <redacted>"
            <> ", mailSearchLimit = "
            <> show request.mailSearchLimit
            <> " }"

instance ToJSON MailSearchRequest where
    toJSON request = object $
        [ "account_id" .= request.mailSearchAccountId
        , "limit" .= request.mailSearchLimit
        ]
        <> optional "mailbox_id" request.mailSearchMailboxId
        <> optional "query" request.mailSearchQuery
        <> optional "from" request.mailSearchFrom
        <> optional "to" request.mailSearchTo
        <> optional "subject" request.mailSearchSubject
        <> optional "after" request.mailSearchAfter
        <> optional "before" request.mailSearchBefore
        <> optional "has_attachments" request.mailSearchHasAttachments
      where
        optional _ Nothing = []
        optional key (Just value) = [Key.fromText key .= value]

instance FromJSON MailSearchRequest where
    parseJSON = withObject "MailSearchRequest" \value ->
        MailSearchRequest
            <$> value .: "account_id"
            <*> value .:? "mailbox_id"
            <*> value .:? "query"
            <*> value .:? "from"
            <*> value .:? "to"
            <*> value .:? "subject"
            <*> value .:? "after"
            <*> value .:? "before"
            <*> value .:? "has_attachments"
            <*> value .:? "limit" .!= 20

data MailMessageSummary = MailMessageSummary
    { mailMessageSummaryId :: !Text
    , mailMessageSummaryThreadId :: !(Maybe Text)
    , mailMessageSummarySubject :: !(Maybe Text)
    , mailMessageSummaryFrom :: !(Maybe Text)
    , mailMessageSummaryReplyTo :: !(Maybe Text)
    , mailMessageSummaryTo :: !(Maybe Text)
    , mailMessageSummaryReceivedAt :: !(Maybe Text)
    , mailMessageSummarySnippet :: !(Maybe Text)
    , mailMessageSummaryHasAttachments :: !Bool
    , mailMessageSummaryAttachmentCount :: !(Maybe Int)
    }
    deriving (Eq)

instance Show MailMessageSummary where
    show _ = "MailMessageSummary { <redacted> }"

instance ToJSON MailMessageSummary where
    toJSON message = object
        [ "message_id" .= message.mailMessageSummaryId
        , "thread_id" .= message.mailMessageSummaryThreadId
        , "subject" .= message.mailMessageSummarySubject
        , "from" .= message.mailMessageSummaryFrom
        , "reply_to" .= message.mailMessageSummaryReplyTo
        , "to" .= message.mailMessageSummaryTo
        , "received_at" .= message.mailMessageSummaryReceivedAt
        , "snippet" .= message.mailMessageSummarySnippet
        , "has_attachments" .= message.mailMessageSummaryHasAttachments
        , "attachment_count" .= message.mailMessageSummaryAttachmentCount
        ]

instance FromJSON MailMessageSummary where
    parseJSON = withObject "MailMessageSummary" \value ->
        MailMessageSummary
            <$> value .: "message_id"
            <*> value .: "thread_id"
            <*> value .: "subject"
            <*> value .: "from"
            <*> value .: "reply_to"
            <*> value .: "to"
            <*> value .: "received_at"
            <*> value .: "snippet"
            <*> value .: "has_attachments"
            <*> value .: "attachment_count"

data MailGetRequest = MailGetRequest
    { mailGetAccountId :: !Text
    , mailGetMessageId :: !Text
    }
    deriving (Eq)

instance Show MailGetRequest where
    show _ = "MailGetRequest { <redacted> }"

instance ToJSON MailGetRequest where
    toJSON request = object
        [ "account_id" .= request.mailGetAccountId
        , "message_id" .= request.mailGetMessageId
        ]

instance FromJSON MailGetRequest where
    parseJSON = withObject "MailGetRequest" \value ->
        MailGetRequest
            <$> value .: "account_id"
            <*> value .: "message_id"

data MailAttachment = MailAttachment
    { mailAttachmentId :: !Text
    , mailAttachmentFilename :: !(Maybe Text)
    , mailAttachmentContentType :: !(Maybe Text)
    , mailAttachmentSizeBytes :: !(Maybe Int)
    }
    deriving (Eq)

instance Show MailAttachment where
    show attachment =
        "MailAttachment { privateFields = <redacted>"
            <> ", mailAttachmentSizeBytes = "
            <> show attachment.mailAttachmentSizeBytes
            <> " }"

instance ToJSON MailAttachment where
    toJSON attachment = object
        [ "attachment_id" .= attachment.mailAttachmentId
        , "filename" .= attachment.mailAttachmentFilename
        , "content_type" .= attachment.mailAttachmentContentType
        , "size_bytes" .= attachment.mailAttachmentSizeBytes
        ]

instance FromJSON MailAttachment where
    parseJSON = withObject "MailAttachment" \value ->
        MailAttachment
            <$> value .: "attachment_id"
            <*> value .: "filename"
            <*> value .: "content_type"
            <*> value .: "size_bytes"

data MailMessage = MailMessage
    { mailMessageId :: !Text
    , mailMessageThreadId :: !(Maybe Text)
    , mailMessageSubject :: !(Maybe Text)
    , mailMessageFrom :: !(Maybe Text)
    , mailMessageReplyTo :: !(Maybe Text)
    , mailMessageTo :: !(Maybe Text)
    , mailMessageCc :: !(Maybe Text)
    , mailMessageReceivedAt :: !(Maybe Text)
    , mailMessageSentAt :: !(Maybe Text)
    , mailMessageBody :: !(Maybe Text)
    , mailMessageBodyTruncated :: !Bool
    , mailMessageAttachments :: ![MailAttachment]
    }
    deriving (Eq)

instance Show MailMessage where
    show _ = "MailMessage { <redacted> }"

instance ToJSON MailMessage where
    toJSON message = object
        [ "message_id" .= message.mailMessageId
        , "thread_id" .= message.mailMessageThreadId
        , "subject" .= message.mailMessageSubject
        , "from" .= message.mailMessageFrom
        , "reply_to" .= message.mailMessageReplyTo
        , "to" .= message.mailMessageTo
        , "cc" .= message.mailMessageCc
        , "received_at" .= message.mailMessageReceivedAt
        , "sent_at" .= message.mailMessageSentAt
        , "body" .= message.mailMessageBody
        , "body_truncated" .= message.mailMessageBodyTruncated
        , "attachments" .= message.mailMessageAttachments
        ]

instance FromJSON MailMessage where
    parseJSON = withObject "MailMessage" \value ->
        MailMessage
            <$> value .: "message_id"
            <*> value .: "thread_id"
            <*> value .: "subject"
            <*> value .: "from"
            <*> value .: "reply_to"
            <*> value .: "to"
            <*> value .: "cc"
            <*> value .: "received_at"
            <*> value .: "sent_at"
            <*> value .: "body"
            <*> value .: "body_truncated"
            <*> value .: "attachments"

data MailAttachmentRequest = MailAttachmentRequest
    { mailAttachmentAccountId :: !Text
    , mailAttachmentMessageId :: !Text
    , mailAttachmentRequestId :: !Text
    }
    deriving (Eq)

instance Show MailAttachmentRequest where
    show _ = "MailAttachmentRequest { <redacted> }"

instance ToJSON MailAttachmentRequest where
    toJSON request = object
        [ "account_id" .= request.mailAttachmentAccountId
        , "message_id" .= request.mailAttachmentMessageId
        , "attachment_id" .= request.mailAttachmentRequestId
        ]

instance FromJSON MailAttachmentRequest where
    parseJSON = withObject "MailAttachmentRequest" \value ->
        MailAttachmentRequest
            <$> value .: "account_id"
            <*> value .: "message_id"
            <*> value .: "attachment_id"

data MailAttachmentContent = MailAttachmentContent
    { mailDownloadedAttachmentFilename :: !(Maybe Text)
    , mailDownloadedAttachmentContentType :: !(Maybe Text)
    , mailDownloadedAttachmentBytes :: !BS.ByteString
    }
    deriving (Eq)

instance Show MailAttachmentContent where
    show content =
        "MailAttachmentContent"
            <> " { mailDownloadedAttachmentFilename = <redacted>"
            <> ", mailDownloadedAttachmentContentType = <redacted>"
            <> ", mailDownloadedAttachmentBytes = <"
            <> show (BS.length content.mailDownloadedAttachmentBytes)
            <> " bytes> }"

-- | Result of the MCP control operation.  The bytes are fetched separately
-- over the authenticated same-origin gateway data plane.
data MailAttachmentDownload = MailAttachmentDownload
    { mailAttachmentDownloadRef :: !Text
    , mailAttachmentDownloadFilename :: !(Maybe Text)
    , mailAttachmentDownloadContentType :: !(Maybe Text)
    , mailAttachmentDownloadSizeBytes :: !Int
    }
    deriving (Eq)

instance Show MailAttachmentDownload where
    show download =
        "MailAttachmentDownload"
            <> " { mailAttachmentDownloadRef = <redacted>"
            <> ", mailAttachmentDownloadFilename = <redacted>"
            <> ", mailAttachmentDownloadContentType = <redacted>"
            <> ", mailAttachmentDownloadSizeBytes = "
            <> show download.mailAttachmentDownloadSizeBytes
            <> " }"

instance ToJSON MailAttachmentDownload where
    toJSON download = object
        [ "download_ref" .= download.mailAttachmentDownloadRef
        , "filename" .= download.mailAttachmentDownloadFilename
        , "content_type" .= download.mailAttachmentDownloadContentType
        , "size_bytes" .= download.mailAttachmentDownloadSizeBytes
        ]

instance FromJSON MailAttachmentDownload where
    parseJSON = withObject "MailAttachmentDownload" \value ->
        MailAttachmentDownload
            <$> value .: "download_ref"
            <*> value .: "filename"
            <*> value .: "content_type"
            <*> value .: "size_bytes"

data MailDraftContent = MailDraftContent
    { mailDraftTo :: ![Text]
    , mailDraftCc :: ![Text]
    , mailDraftBcc :: ![Text]
    , mailDraftSubject :: !Text
    , mailDraftBody :: !Text
    }
    deriving (Eq)

instance Show MailDraftContent where
    show _ = "MailDraftContent { <redacted> }"

instance ToJSON MailDraftContent where
    toJSON content = object
        [ "to" .= content.mailDraftTo
        , "cc" .= content.mailDraftCc
        , "bcc" .= content.mailDraftBcc
        , "subject" .= content.mailDraftSubject
        , "body" .= content.mailDraftBody
        ]

instance FromJSON MailDraftContent where
    parseJSON = withObject "MailDraftContent" \value ->
        MailDraftContent
            <$> value .:? "to" .!= []
            <*> value .:? "cc" .!= []
            <*> value .:? "bcc" .!= []
            <*> value .:? "subject" .!= ""
            <*> value .:? "body" .!= ""

data MailCreateDraftRequest = MailCreateDraftRequest
    { mailCreateDraftAccountId :: !Text
    , mailCreateDraftContent :: !MailDraftContent
    }
    deriving (Eq)

instance Show MailCreateDraftRequest where
    show _ = "MailCreateDraftRequest { <redacted> }"

instance ToJSON MailCreateDraftRequest where
    toJSON request =
        draftRequestValue
            request.mailCreateDraftAccountId
            request.mailCreateDraftContent
            []

instance FromJSON MailCreateDraftRequest where
    parseJSON = withObject "MailCreateDraftRequest" \value ->
        MailCreateDraftRequest
            <$> value .: "account_id"
            <*> parseJSON (Aeson.Object value)

data MailUpdateDraftRequest = MailUpdateDraftRequest
    { mailUpdateDraftAccountId :: !Text
    , mailUpdateDraftId :: !Text
    , mailUpdateDraftContent :: !MailDraftContent
    }
    deriving (Eq)

instance Show MailUpdateDraftRequest where
    show _ = "MailUpdateDraftRequest { <redacted> }"

instance ToJSON MailUpdateDraftRequest where
    toJSON request =
        draftRequestValue
            request.mailUpdateDraftAccountId
            request.mailUpdateDraftContent
            ["draft_id" .= request.mailUpdateDraftId]

instance FromJSON MailUpdateDraftRequest where
    parseJSON = withObject "MailUpdateDraftRequest" \value ->
        MailUpdateDraftRequest
            <$> value .: "account_id"
            <*> value .: "draft_id"
            <*> parseJSON (Aeson.Object value)

data MailReplyDraftRequest = MailReplyDraftRequest
    { mailReplyDraftAccountId :: !Text
    , mailReplyDraftMessageId :: !Text
    , mailReplyDraftTo :: ![Text]
    , mailReplyDraftBody :: !Text
    }
    deriving (Eq)

instance Show MailReplyDraftRequest where
    show _ = "MailReplyDraftRequest { <redacted> }"

instance ToJSON MailReplyDraftRequest where
    toJSON request = object
        [ "account_id" .= request.mailReplyDraftAccountId
        , "message_id" .= request.mailReplyDraftMessageId
        , "to" .= request.mailReplyDraftTo
        , "body" .= request.mailReplyDraftBody
        ]

instance FromJSON MailReplyDraftRequest where
    parseJSON = withObject "MailReplyDraftRequest" \value ->
        MailReplyDraftRequest
            <$> value .: "account_id"
            <*> value .: "message_id"
            <*> value .: "to"
            <*> value .:? "body" .!= ""

data MailDraft = MailDraft
    { mailDraftId :: !Text
    , mailDraftMessageId :: !(Maybe Text)
    , mailDraftThreadId :: !(Maybe Text)
    , mailDraftWarning :: !(Maybe Text)
    }
    deriving (Eq)

instance Show MailDraft where
    show _ = "MailDraft { <redacted> }"

instance ToJSON MailDraft where
    toJSON draft = object
        [ "draft_id" .= boundedOpaque draft.mailDraftId
        , "message_id" .= fmap boundedOpaque draft.mailDraftMessageId
        , "thread_id" .= fmap boundedOpaque draft.mailDraftThreadId
        , "warning" .= fmap boundedShortText draft.mailDraftWarning
        , "saved" .= True
        , "sent" .= False
        ]

instance FromJSON MailDraft where
    parseJSON = withObject "MailDraft" \value -> do
        draft <- MailDraft
            <$> value .: "draft_id"
            <*> value .: "message_id"
            <*> value .: "thread_id"
            <*> value .: "warning"
        saved <- value .: "saved"
        sent <- value .: "sent"
        when (not saved || sent) $
            fail "email draft result violated the no-send contract"
        pure draft

-- | Provider transport used by both standalone and gateway credential
-- stores. The credential is selected and authorized before it reaches this
-- layer.
data MailTransport = MailTransport
    { mailTransportListMailboxes
        :: !(MailCredential -> Int
            -> IO (Either Text [MailboxSummary]))
    , mailTransportSearch
        :: !(MailCredential -> MailSearchRequest
            -> IO (Either Text [MailMessageSummary]))
    , mailTransportGetMessage
        :: !(MailCredential -> MailGetRequest -> Int
            -> IO (Either Text MailMessage))
    , mailTransportDownloadAttachment
        :: !(MailCredential -> MailAttachmentRequest -> Int
            -> IO (Either Text MailAttachmentContent))
    , mailTransportCreateDraft
        :: !(MailCredential -> MailCreateDraftRequest
            -> IO (Either Text MailDraft))
    , mailTransportUpdateDraft
        :: !(MailCredential -> MailUpdateDraftRequest
            -> IO (Either Text MailDraft))
    , mailTransportReplyDraft
        :: !(MailCredential -> MailReplyDraftRequest
            -> IO (Either Text MailDraft))
    }

-- | Persistence hooks required by provider execution. Implementations must
-- perform compare-and-swap updates against the exact supplied account so a
-- late provider response cannot overwrite a reconnect or revocation.
data MailTransportHooks = MailTransportHooks
    { mailTransportRefreshCredential
        :: !(MailCredential -> IO (Either Text MailCredential))
    , mailTransportRecordAccountState
        :: !(MailAccount -> MailAccountState -> Maybe Text
            -> IO (Either Text ()))
    }

draftRequestValue
    :: Text
    -> MailDraftContent
    -> [AesonTypes.Pair]
    -> Aeson.Value
draftRequestValue accountId content extra = object $
    [ "account_id" .= accountId
    , "to" .= content.mailDraftTo
    , "cc" .= content.mailDraftCc
    , "bcc" .= content.mailDraftBcc
    , "subject" .= content.mailDraftSubject
    , "body" .= content.mailDraftBody
    ] <> extra

normalizeMailEmail :: Text -> Either Text Text
normalizeMailEmail raw =
    case Text.splitOn "@" (Text.toCaseFold (Text.strip raw)) of
        [local, domain]
            | validLocal local
            , validDomain domain ->
                Right (local <> "@" <> domain)
        _ -> Left "enter a valid email address"

validateMailImapSettings :: MailImapSettings -> Either Text ()
validateMailImapSettings settings = do
    let rawHost = settings.mailImapHost
        rawUsername = settings.mailImapUsername
        host = Text.strip rawHost
        username = Text.strip rawUsername
    when
        ( rawHost /= host
        || Text.null host
        || Text.length host > 253
        || Text.any (\character -> isControl character || isSpace character) rawHost
        || not (validHost host)
        )
        (Left "IMAP host is invalid")
    when
        (settings.mailImapPort < 1 || settings.mailImapPort > 65535)
        (Left "IMAP port must be between 1 and 65535")
    when
        ( rawUsername /= username
        || Text.null username
        || Text.length username > 320
        || Text.any
            (\character -> isControl character || isSpace character)
            rawUsername
        )
        (Left "IMAP username is invalid")

validateMailOAuthClientId :: MailProvider -> Text -> Either Text ()
validateMailOAuthClientId provider rawClientId
    | rawClientId /= clientId
        || Text.null clientId
        || Text.length clientId > 1024
        || Text.any isControl clientId =
            Left "OAuth client id is invalid"
    | provider == GmailProvider
        , Just prefix <- Text.stripSuffix googleClientIdSuffix clientId
        , not (Text.null prefix)
        , Text.all validGoogleCharacter prefix =
            Right ()
    | provider == MicrosoftProvider
        , map Text.length groups == [8, 4, 4, 4, 12]
        , all (Text.all isHexDigit) groups =
            Right ()
    | provider == GmailProvider =
        Left "Gmail OAuth client id must be an installed-app client id."
    | provider == MicrosoftProvider =
        Left "Microsoft OAuth client id must be a UUID."
    | otherwise =
        Left "Custom IMAP accounts do not use OAuth client ids."
  where
    clientId = Text.strip rawClientId
    groups = Text.splitOn "-" clientId
    googleClientIdSuffix = ".apps.googleusercontent.com"
    validGoogleCharacter character =
        isAscii character
            && (isAlphaNum character || character == '_' || character == '-')

validateMailDraftContent
    :: MailToolLimits
    -> MailDraftContent
    -> Either Text MailDraftContent
validateMailDraftContent limits content = do
    to <- traverse (validateDraftRecipient "to") content.mailDraftTo
    cc <- traverse (validateDraftRecipient "cc") content.mailDraftCc
    bcc <- traverse (validateDraftRecipient "bcc") content.mailDraftBcc
    when
        (length to + length cc + length bcc > maximumDraftRecipients)
        (Left "a draft may contain at most 100 recipients")
    subject <- validateDraftSubject content.mailDraftSubject
    body <- validateDraftBody limits content.mailDraftBody
    pure MailDraftContent
        { mailDraftTo = to
        , mailDraftCc = cc
        , mailDraftBcc = bcc
        , mailDraftSubject = subject
        , mailDraftBody = body
        }

validateMailSearchRequest
    :: MailToolLimits
    -> MailSearchRequest
    -> Either Text MailSearchRequest
validateMailSearchRequest limits request = do
    accountId <-
        validateOpaqueMailReference "account_id" request.mailSearchAccountId
    mailboxId <-
        traverse
            (validateOpaqueMailReference "mailbox_id")
            request.mailSearchMailboxId
    query <- traverse (validateSearchText "query") request.mailSearchQuery
    from <- traverse (validateSearchText "from") request.mailSearchFrom
    to <- traverse (validateSearchText "to") request.mailSearchTo
    subject <- traverse (validateSearchText "subject") request.mailSearchSubject
    after <- traverse validateIsoDay request.mailSearchAfter
    before <- traverse validateIsoDay request.mailSearchBefore
    when
        ( request.mailSearchLimit < 1
        || request.mailSearchLimit > limits.mailMaximumSearchResults
        )
        ( Left
            ( "limit must be between 1 and "
                <> Text.pack (show limits.mailMaximumSearchResults)
            )
        )
    when (not (validDateRange after before))
        (Left "after must not be later than before")
    pure request
        { mailSearchAccountId = accountId
        , mailSearchMailboxId = mailboxId
        , mailSearchQuery = query
        , mailSearchFrom = from
        , mailSearchTo = to
        , mailSearchSubject = subject
        }

validateMailGetRequest
    :: MailGetRequest
    -> Either Text MailGetRequest
validateMailGetRequest request =
    MailGetRequest
        <$> validateOpaqueMailReference
            "account_id"
            request.mailGetAccountId
        <*> validateOpaqueMailReference
            "message_id"
            request.mailGetMessageId

validateMailAttachmentRequest
    :: MailAttachmentRequest
    -> Either Text MailAttachmentRequest
validateMailAttachmentRequest request =
    MailAttachmentRequest
        <$> validateOpaqueMailReference
            "account_id"
            request.mailAttachmentAccountId
        <*> validateOpaqueMailReference
            "message_id"
            request.mailAttachmentMessageId
        <*> validateOpaqueMailReference
            "attachment_id"
            request.mailAttachmentRequestId

validateOpaqueMailReference :: Text -> Text -> Either Text Text
validateOpaqueMailReference field value
    | Text.null stripped = Left (field <> " must not be empty")
    | utf8Length stripped > maximumOpaqueReferenceBytes =
        Left (field <> " is too long")
    | Text.any isControl stripped =
        Left (field <> " contains control characters")
    | otherwise = Right stripped
  where
    stripped = Text.strip value

validateDraftRecipient :: Text -> Text -> Either Text Text
validateDraftRecipient field raw
    | Text.null value =
        Left (field <> " must not contain an empty recipient")
    | utf8Length value > maximumDraftRecipientBytes =
        Left (field <> " recipient is too long")
    | Text.any isControl value =
        Left (field <> " recipient contains control characters")
    | otherwise =
        case normalizeMailEmail value of
            Left _ ->
                Left (field <> " recipients must be bare email addresses")
            Right _ -> Right value
  where
    value = Text.strip raw

validateDraftSubject :: Text -> Either Text Text
validateDraftSubject value
    | utf8Length value > maximumDraftSubjectBytes =
        Left "subject is too long"
    | Text.any isControl value =
        Left "subject contains control characters"
    | otherwise = Right value

validateDraftBody :: MailToolLimits -> Text -> Either Text Text
validateDraftBody limits value
    | utf8Length value > limits.mailMaximumDraftBodyBytes =
        Left "body is too long"
    | Text.any invalid value =
        Left "body contains invalid control characters"
    | otherwise = Right value
  where
    invalid character =
        isControl character
            && character /= '\r'
            && character /= '\n'
            && character /= '\t'

validateSearchText :: Text -> Text -> Either Text Text
validateSearchText field value
    | Text.null stripped =
        Left (field <> " must not be empty when provided")
    | utf8Length value > maximumSearchTextBytes =
        Left (field <> " is too long")
    | Text.any isControl value =
        Left (field <> " contains control characters")
    | otherwise = Right value
  where
    stripped = Text.strip value

validateIsoDay :: Text -> Either Text Text
validateIsoDay value =
    case parseIsoDay value of
        Nothing -> Left "dates must use ISO format YYYY-MM-DD"
        Just _ -> Right value

validDateRange :: Maybe Text -> Maybe Text -> Bool
validDateRange after before =
    case (after >>= parseIsoDay, before >>= parseIsoDay) of
        (Just afterDay, Just beforeDay) -> afterDay <= beforeDay
        _ -> True

parseIsoDay :: Text -> Maybe Day
parseIsoDay value =
    parseTimeM True defaultTimeLocale "%F" (Text.unpack value)

utf8Length :: Text -> Int
utf8Length = BS.length . TextEncoding.encodeUtf8

truncateUtf8 :: Int -> Text -> Text
truncateUtf8 limit = decodePrefix . BS.take (max 0 limit) . TextEncoding.encodeUtf8
  where
    decodePrefix bytes =
        case TextEncoding.decodeUtf8' bytes of
            Right value -> value
            Left _
                | BS.null bytes -> ""
                | otherwise -> decodePrefix (BS.init bytes)

boundedOpaque :: Text -> Text
boundedOpaque = truncateUtf8 maximumOpaqueReferenceBytes

boundedShortText :: Text -> Text
boundedShortText = truncateUtf8 maximumShortTextBytes

validLocal :: Text -> Bool
validLocal local =
    not (Text.null local)
        && Text.length local <= 64
        && Text.all
            ( \character ->
                isAscii character
                    && ( isAlphaNum character
                        || character
                            `elem` (".!#$%&'*+/=?^_`{|}~-" :: String)
                       )
            )
            local

validDomain :: Text -> Bool
validDomain domain =
    Text.length domain <= 253
        && length labels >= 2
        && all validLabel labels
  where
    labels = Text.splitOn "." domain
    validLabel label =
        not (Text.null label)
            && Text.length label <= 63
            && Text.head label /= '-'
            && Text.last label /= '-'
            && Text.all
                ( \character ->
                    isAscii character
                        && (isAlphaNum character || character == '-')
                )
                label

validHost :: Text -> Bool
validHost host
    | Text.length host >= 3
        && Text.head host == '['
        && Text.last host == ']' =
            let address = Text.init (Text.tail host)
            in not (Text.null address)
                && Text.all
                    ( \character ->
                        isAscii character
                            && ( isAlphaNum character
                                || character == ':'
                                || character == '.'
                               )
                    )
                    address
    | otherwise = validDomain host

maximumOpaqueReferenceBytes, maximumSearchTextBytes
    , maximumDraftRecipientBytes, maximumDraftSubjectBytes
    , maximumDraftRecipients :: Int
maximumOpaqueReferenceBytes = 8192
maximumSearchTextBytes = 500
maximumDraftRecipientBytes = 320
maximumDraftSubjectBytes = 700
maximumDraftRecipients = 100
maximumShortTextBytes :: Int
maximumShortTextBytes = 2048
