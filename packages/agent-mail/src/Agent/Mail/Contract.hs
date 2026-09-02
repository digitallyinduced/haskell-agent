-- | Versioned first-party MCP contract for connected email accounts.
module Agent.Mail.Contract
    ( MailMcpTool(..)
    , mailContractId
    , mailContractVersion
    , mailContractMetadata
    , mailMcpTools
    , mailMcpToolDefinitions
    , mailListAccountsToolName
    , mailListMailboxesToolName
    , mailSearchToolName
    , mailGetToolName
    , mailDownloadAttachmentToolName
    , mailCreateDraftToolName
    , mailUpdateDraftToolName
    , mailReplyDraftToolName
    , mailDraftMutationToolNames
    , mailMcpSuccess
    , mailMcpFailure
    , decodeMailMcpResult
    ) where

import Data.Aeson
    ( FromJSON
    , ToJSON
    , Value(..)
    , object
    , withObject
    , (.:)
    , (.=)
    )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding

data MailMcpTool = MailMcpTool
    { mailMcpToolName :: !Text
    , mailMcpToolDescription :: !Text
    , mailMcpToolInputSchema :: !Value
    , mailMcpToolOutputSchema :: !Value
    , mailMcpToolReadOnly :: !Bool
    , mailMcpToolRequiresFreshApproval :: !Bool
    }
    deriving (Eq, Show)

mailContractId :: Text
mailContractId = "dev.haskell-agent.email"

mailContractVersion :: Text
mailContractVersion = "1"

mailContractMetadata :: Value
mailContractMetadata = object
    [ "contract" .= mailContractId
    , "version" .= mailContractVersion
    ]

mailListAccountsToolName, mailListMailboxesToolName, mailSearchToolName
    , mailGetToolName, mailDownloadAttachmentToolName
    , mailCreateDraftToolName, mailUpdateDraftToolName
    , mailReplyDraftToolName :: Text
mailListAccountsToolName = "email_list_accounts"
mailListMailboxesToolName = "email_list_mailboxes"
mailSearchToolName = "email_search"
mailGetToolName = "email_get"
mailDownloadAttachmentToolName = "email_download_attachment"
mailCreateDraftToolName = "email_create_draft"
mailUpdateDraftToolName = "email_update_draft"
mailReplyDraftToolName = "email_reply_draft"

mailDraftMutationToolNames :: [Text]
mailDraftMutationToolNames =
    [ mailCreateDraftToolName
    , mailUpdateDraftToolName
    , mailReplyDraftToolName
    ]

mailMcpTools :: [MailMcpTool]
mailMcpTools =
    [ readTool
        mailListAccountsToolName
        "List email accounts connected for the authenticated user."
        emptySchema
        (arraySchema accountSummarySchema 100)
    , readTool
        mailListMailboxesToolName
        "List mailboxes for one connected email account."
        (schema
            [ ("account_id", referenceProperty "Opaque account reference.") ]
            ["account_id"])
        (arraySchema mailboxSummarySchema 200)
    , readTool
        mailSearchToolName
        "Search a connected email account with bounded structured filters."
        searchSchema
        (arraySchema messageSummarySchema 50)
    , readTool
        mailGetToolName
        "Read one bounded plain-text email message and attachment metadata."
        (schema
            [ ("account_id", referenceProperty "Opaque account reference.")
            , ("message_id", referenceProperty "Opaque message reference.")
            ]
            ["account_id", "message_id"])
        messageSchema
    , readTool
        mailDownloadAttachmentToolName
        "Prepare a bounded attachment for authenticated same-origin download."
        (schema
            [ ("account_id", referenceProperty "Opaque account reference.")
            , ("message_id", referenceProperty "Opaque message reference.")
            , ("attachment_id", referenceProperty "Opaque attachment reference.")
            ]
            ["account_id", "message_id", "attachment_id"])
        attachmentDownloadSchema
    , writeTool
        mailCreateDraftToolName
        "Save a new mailbox draft. This never sends email."
        (schema draftProperties ["account_id"])
        draftResultSchema
    , writeTool
        mailUpdateDraftToolName
        "Replace an existing mailbox draft. This never sends email."
        (schema
            ( ("draft_id", referenceProperty "Opaque draft reference.")
                : draftProperties
            )
            ["account_id", "draft_id"])
        draftResultSchema
    , writeTool
        mailReplyDraftToolName
        "Save a reply draft for a source message. This never sends email."
        (schema
            [ ("account_id", referenceProperty "Opaque account reference.")
            , ("message_id", referenceProperty "Opaque source message reference.")
            , ("to", stringArrayProperty "Exactly one bare reply recipient." 1 1 320)
            , ("body", boundedStringProperty "Plain-text draft body." 131072)
            ]
            ["account_id", "message_id", "to"])
        draftResultSchema
    ]
  where
    readTool name description inputSchema outputDataSchema =
        MailMcpTool
            name
            description
            inputSchema
            (resultEnvelopeSchema outputDataSchema)
            True
            False
    writeTool name description inputSchema outputDataSchema =
        MailMcpTool
            name
            description
            inputSchema
            (resultEnvelopeSchema outputDataSchema)
            False
            True

mailMcpToolDefinitions :: [Value]
mailMcpToolDefinitions = fmap toolValue mailMcpTools
  where
    toolValue tool = object
        [ "name" .= tool.mailMcpToolName
        , "description" .= tool.mailMcpToolDescription
        , "inputSchema" .= tool.mailMcpToolInputSchema
        , "outputSchema" .= tool.mailMcpToolOutputSchema
        , "annotations" .= object
            [ "readOnlyHint" .= tool.mailMcpToolReadOnly
            , "destructiveHint" .= False
            , "idempotentHint" .= tool.mailMcpToolReadOnly
            , "openWorldHint" .= True
            ]
        , "_meta" .= object
            [ "dev.haskell-agent/email-contract" .= mailContractVersion
            , "dev.haskell-agent/fresh-approval"
                .= tool.mailMcpToolRequiresFreshApproval
            ]
        ]

mailMcpSuccess :: ToJSON value => value -> Value
mailMcpSuccess value =
    let structured = object
            [ "contract" .= mailContractId
            , "version" .= mailContractVersion
            , "data" .= value
            ]
        encoded =
            TextEncoding.decodeUtf8
                (LBS.toStrict (Aeson.encode structured))
    in object
        [ "content" .=
            [ object
                [ "type" .= ("text" :: Text)
                , "text" .= encoded
                ]
            ]
        , "structuredContent" .= structured
        , "isError" .= False
        ]

mailMcpFailure :: Text -> Value
mailMcpFailure _ = object
    [ "content" .=
        [ object
            [ "type" .= ("text" :: Text)
            , "text" .= bounded
            ]
        ]
    , "structuredContent" .= object
        [ "contract" .= mailContractId
        , "version" .= mailContractVersion
        , "error" .= bounded
        ]
    , "isError" .= True
    ]
  where
    bounded = "Email operation failed." :: Text

decodeMailMcpResult :: FromJSON value => Value -> Either Text value
decodeMailMcpResult =
    either (Left . Text.pack) Right . AesonTypes.parseEither parser
  where
    parser = withObject "Mail MCP result" \result -> do
        case KeyMap.lookup "isError" result of
            Just (Bool True) -> do
                structured <- result .: "structuredContent"
                withObject "Mail MCP error"
                    ( \payload -> do
                        contract <- payload .: "contract"
                        version <- payload .: "version"
                        message <- payload .: "error"
                        if contract /= mailContractId
                            || version /= mailContractVersion
                            || message /= ("Email operation failed." :: Text)
                            then fail "incompatible email MCP contract"
                            else fail "email operation failed"
                    )
                    structured
            Just (Bool False) -> do
                structured <- result .: "structuredContent"
                withObject "Mail MCP structured result"
                    ( \payload -> do
                        contract <- payload .: "contract"
                        version <- payload .: "version"
                        if contract /= mailContractId
                            || version /= mailContractVersion
                            then fail "incompatible email MCP contract"
                            else payload .: "data"
                    )
                    structured
            _ -> fail "invalid email MCP result status"

emptySchema :: Value
emptySchema = schema [] []

searchSchema :: Value
searchSchema = schema
    [ ("account_id", referenceProperty "Opaque account reference.")
    , ("mailbox_id", referenceProperty "Optional opaque mailbox reference.")
    , ("query", boundedStringProperty "Optional text query, at most 500 UTF-8 bytes." 500)
    , ("from", boundedStringProperty "Optional sender filter." 500)
    , ("to", boundedStringProperty "Optional recipient filter." 500)
    , ("subject", boundedStringProperty "Optional subject filter." 500)
    , ("after", isoDateProperty "Optional inclusive ISO date.")
    , ("before", isoDateProperty "Optional inclusive ISO date.")
    , ("has_attachments", object ["type" .= ("boolean" :: Text)])
    , ("limit", object
        [ "type" .= ("integer" :: Text)
        , "minimum" .= (1 :: Int)
        , "maximum" .= (50 :: Int)
        , "default" .= (20 :: Int)
        ])
    ]
    ["account_id"]

draftProperties :: [(Text, Value)]
draftProperties =
    [ ("account_id", referenceProperty "Opaque account reference.")
    , ("to", stringArrayProperty "Bare To recipient addresses." 0 100 320)
    , ("cc", stringArrayProperty "Bare Cc recipient addresses." 0 100 320)
    , ("bcc", stringArrayProperty "Bare Bcc recipient addresses." 0 100 320)
    , ("subject", boundedStringProperty "Draft subject." 700)
    , ("body", boundedStringProperty "Plain-text draft body." 131072)
    ]

schema :: [(Text, Value)] -> [Text] -> Value
schema properties required = object
    [ "type" .= ("object" :: Text)
    , "properties" .= object
        [ Key.fromText name .= value
        | (name, value) <- properties
        ]
    , "required" .= required
    , "additionalProperties" .= False
    ]

boundedStringProperty :: Text -> Int -> Value
boundedStringProperty description maximumLength = object
    [ "type" .= ("string" :: Text)
    , "description" .= description
    , "maxLength" .= maximumLength
    ]

referenceProperty :: Text -> Value
referenceProperty description = object
    [ "type" .= ("string" :: Text)
    , "description" .= description
    , "minLength" .= (1 :: Int)
    , "maxLength" .= (8192 :: Int)
    , "pattern" .= ("^[A-Za-z0-9_-]+$" :: Text)
    ]

downloadReferenceProperty :: Value
downloadReferenceProperty = object
    [ "type" .= ("string" :: Text)
    , "description" .= ("One-time download reference." :: Text)
    , "minLength" .= (1 :: Int)
    , "maxLength" .= (1024 :: Int)
    , "pattern" .= ("^[A-Za-z0-9_-]+$" :: Text)
    ]

isoDateProperty :: Text -> Value
isoDateProperty description = object
    [ "type" .= ("string" :: Text)
    , "description" .= description
    , "format" .= ("date" :: Text)
    , "maxLength" .= (10 :: Int)
    ]

stringArrayProperty :: Text -> Int -> Int -> Int -> Value
stringArrayProperty description minimumItems maximumItems maximumLength = object
    [ "type" .= ("array" :: Text)
    , "items" .= object
        [ "type" .= ("string" :: Text)
        , "maxLength" .= maximumLength
        ]
    , "minItems" .= minimumItems
    , "maxItems" .= maximumItems
    , "description" .= description
    ]

resultEnvelopeSchema :: Value -> Value
resultEnvelopeSchema dataSchema = object
    [ "oneOf" .=
        [ schema
            [ ("contract", constStringProperty mailContractId)
            , ("version", constStringProperty mailContractVersion)
            , ("data", dataSchema)
            ]
            ["contract", "version", "data"]
        , schema
            [ ("contract", constStringProperty mailContractId)
            , ("version", constStringProperty mailContractVersion)
            , ("error", constStringProperty "Email operation failed.")
            ]
            ["contract", "version", "error"]
        ]
    ]

accountSummarySchema :: Value
accountSummarySchema = schema
    [ ("account_id", referenceProperty "Opaque account reference.")
    , ("provider", boundedStringProperty "Email provider." 32)
    , ("email", boundedStringProperty "Verified mailbox address." 320)
    , ("label", nullableStringProperty 160)
    , ("enabled", booleanProperty)
    , ("verified", booleanProperty)
    ]
    ["account_id", "provider", "email", "label", "enabled", "verified"]

mailboxSummarySchema :: Value
mailboxSummarySchema = schema
    [ ("mailbox_id", referenceProperty "Opaque mailbox reference.")
    , ("name", boundedStringProperty "Mailbox name." 512)
    , ("role", nullableStringProperty 64)
    , ("unread_count", nullableIntegerProperty 0)
    ]
    ["mailbox_id", "name", "role", "unread_count"]

messageSummarySchema :: Value
messageSummarySchema = schema
    [ ("message_id", referenceProperty "Opaque message reference.")
    , ("thread_id", nullableReferenceProperty)
    , ("subject", nullableStringProperty 2048)
    , ("from", nullableStringProperty 2048)
    , ("reply_to", nullableStringProperty 2048)
    , ("to", nullableStringProperty 4096)
    , ("received_at", nullableStringProperty 128)
    , ("snippet", nullableStringProperty 4096)
    , ("has_attachments", booleanProperty)
    , ("attachment_count", nullableIntegerProperty 0)
    ]
    [ "message_id", "thread_id", "subject", "from", "reply_to", "to"
    , "received_at", "snippet", "has_attachments", "attachment_count"
    ]

messageSchema :: Value
messageSchema = schema
    [ ("message_id", referenceProperty "Opaque message reference.")
    , ("thread_id", nullableReferenceProperty)
    , ("subject", nullableStringProperty 2048)
    , ("from", nullableStringProperty 2048)
    , ("reply_to", nullableStringProperty 2048)
    , ("to", nullableStringProperty 4096)
    , ("cc", nullableStringProperty 4096)
    , ("received_at", nullableStringProperty 128)
    , ("sent_at", nullableStringProperty 128)
    , ("body", nullableStringProperty 49152)
    , ("body_truncated", booleanProperty)
    , ("attachments", arraySchema attachmentSchema 200)
    ]
    [ "message_id", "thread_id", "subject", "from", "reply_to", "to"
    , "cc", "received_at", "sent_at", "body", "body_truncated"
    , "attachments"
    ]

attachmentSchema :: Value
attachmentSchema = schema
    [ ("attachment_id", referenceProperty "Opaque attachment reference.")
    , ("filename", nullableStringProperty 1024)
    , ("content_type", nullableStringProperty 255)
    , ("size_bytes", nullableIntegerProperty 0)
    ]
    ["attachment_id", "filename", "content_type", "size_bytes"]

attachmentDownloadSchema :: Value
attachmentDownloadSchema = schema
    [ ("download_ref", downloadReferenceProperty)
    , ("filename", nullableStringProperty 1024)
    , ("content_type", nullableStringProperty 255)
    , ("size_bytes", object
        [ "type" .= ("integer" :: Text)
        , "minimum" .= (0 :: Int)
        , "maximum" .= (20 * 1024 * 1024 :: Int)
        ])
    ]
    ["download_ref", "filename", "content_type", "size_bytes"]

draftResultSchema :: Value
draftResultSchema = schema
    [ ("draft_id", referenceProperty "Opaque draft reference.")
    , ("message_id", nullableReferenceProperty)
    , ("thread_id", nullableReferenceProperty)
    , ("warning", nullableStringProperty 1024)
    , ("saved", constBooleanProperty True)
    , ("sent", constBooleanProperty False)
    ]
    ["draft_id", "message_id", "thread_id", "warning", "saved", "sent"]

arraySchema :: Value -> Int -> Value
arraySchema items maximumItems = object
    [ "type" .= ("array" :: Text)
    , "items" .= items
    , "maxItems" .= maximumItems
    ]

nullableStringProperty :: Int -> Value
nullableStringProperty maximumLength = object
    [ "type" .= (["string", "null"] :: [Text])
    , "maxLength" .= maximumLength
    ]

nullableReferenceProperty :: Value
nullableReferenceProperty = object
    [ "type" .= (["string", "null"] :: [Text])
    , "maxLength" .= (8192 :: Int)
    , "pattern" .= ("^[A-Za-z0-9_-]+$" :: Text)
    ]

nullableIntegerProperty :: Int -> Value
nullableIntegerProperty minimumValue = object
    [ "type" .= (["integer", "null"] :: [Text])
    , "minimum" .= minimumValue
    ]

booleanProperty :: Value
booleanProperty = object ["type" .= ("boolean" :: Text)]

constStringProperty :: Text -> Value
constStringProperty value = object
    [ "type" .= ("string" :: Text)
    , "const" .= value
    ]

constBooleanProperty :: Bool -> Value
constBooleanProperty value = object
    [ "type" .= ("boolean" :: Text)
    , "const" .= value
    ]
