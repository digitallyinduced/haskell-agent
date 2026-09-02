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
    , readTool
        mailListMailboxesToolName
        "List mailboxes for one connected email account."
        (schema
            [ ("account_id", stringProperty "Opaque account reference.") ]
            ["account_id"])
    , readTool
        mailSearchToolName
        "Search a connected email account with bounded structured filters."
        searchSchema
    , readTool
        mailGetToolName
        "Read one bounded plain-text email message and attachment metadata."
        (schema
            [ ("account_id", stringProperty "Opaque account reference.")
            , ("message_id", stringProperty "Opaque message reference.")
            ]
            ["account_id", "message_id"])
    , readTool
        mailDownloadAttachmentToolName
        "Prepare a bounded attachment for authenticated same-origin download."
        (schema
            [ ("account_id", stringProperty "Opaque account reference.")
            , ("message_id", stringProperty "Opaque message reference.")
            , ("attachment_id", stringProperty "Opaque attachment reference.")
            ]
            ["account_id", "message_id", "attachment_id"])
    , writeTool
        mailCreateDraftToolName
        "Save a new mailbox draft. This never sends email."
        (schema draftProperties ["account_id"])
    , writeTool
        mailUpdateDraftToolName
        "Replace an existing mailbox draft. This never sends email."
        (schema
            ( ("draft_id", stringProperty "Opaque draft reference.")
                : draftProperties
            )
            ["account_id", "draft_id"])
    , writeTool
        mailReplyDraftToolName
        "Save a reply draft for a source message. This never sends email."
        (schema
            [ ("account_id", stringProperty "Opaque account reference.")
            , ("message_id", stringProperty "Opaque source message reference.")
            , ("to", stringArrayProperty "Exactly one bare reply recipient.")
            , ("body", stringProperty "Plain-text draft body.")
            ]
            ["account_id", "message_id", "to"])
    ]
  where
    readTool name description inputSchema =
        MailMcpTool name description inputSchema True False
    writeTool name description inputSchema =
        MailMcpTool name description inputSchema False True

mailMcpToolDefinitions :: [Value]
mailMcpToolDefinitions = fmap toolValue mailMcpTools
  where
    toolValue tool = object
        [ "name" .= tool.mailMcpToolName
        , "description" .= tool.mailMcpToolDescription
        , "inputSchema" .= tool.mailMcpToolInputSchema
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
    let encoded =
            TextEncoding.decodeUtf8
                (LBS.toStrict (Aeson.encode value))
    in object
        [ "content" .=
            [ object
                [ "type" .= ("text" :: Text)
                , "text" .= encoded
                ]
            ]
        , "structuredContent" .= object
            [ "contract" .= mailContractId
            , "version" .= mailContractVersion
            , "data" .= value
            ]
        , "isError" .= False
        ]

mailMcpFailure :: Text -> Value
mailMcpFailure message = object
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
    bounded = Text.take 512 (Text.unwords (Text.words message))

decodeMailMcpResult :: FromJSON value => Value -> Either Text value
decodeMailMcpResult =
    either (Left . Text.pack) Right . AesonTypes.parseEither parser
  where
    parser = withObject "Mail MCP result" \result -> do
        case KeyMap.lookup "isError" result of
            Just (Bool True) -> do
                message <- case KeyMap.lookup "structuredContent" result of
                    Just structured ->
                        withObject "Mail MCP error" (.: "error") structured
                    Nothing -> fail "email operation failed"
                fail (Text.unpack message)
            _ -> do
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

emptySchema :: Value
emptySchema = schema [] []

searchSchema :: Value
searchSchema = schema
    [ ("account_id", stringProperty "Opaque account reference.")
    , ("mailbox_id", stringProperty "Optional opaque mailbox reference.")
    , ("query", stringProperty "Optional text query, at most 500 UTF-8 bytes.")
    , ("from", stringProperty "Optional sender filter.")
    , ("to", stringProperty "Optional recipient filter.")
    , ("subject", stringProperty "Optional subject filter.")
    , ("after", stringProperty "Optional inclusive ISO date.")
    , ("before", stringProperty "Optional inclusive ISO date.")
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
    [ ("account_id", stringProperty "Opaque account reference.")
    , ("to", stringArrayProperty "Bare To recipient addresses.")
    , ("cc", stringArrayProperty "Bare Cc recipient addresses.")
    , ("bcc", stringArrayProperty "Bare Bcc recipient addresses.")
    , ("subject", stringProperty "Draft subject.")
    , ("body", stringProperty "Plain-text draft body.")
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

stringProperty :: Text -> Value
stringProperty description = object
    [ "type" .= ("string" :: Text)
    , "description" .= description
    ]

stringArrayProperty :: Text -> Value
stringArrayProperty description = object
    [ "type" .= ("array" :: Text)
    , "items" .= object ["type" .= ("string" :: Text)]
    , "description" .= description
    ]
