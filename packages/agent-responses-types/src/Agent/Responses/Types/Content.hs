module Agent.Responses.Types.Content
    ( ResponseRole(..)
    , responseRoleDecoder
    , ItemStatus(..)
    , itemStatusDecoder
    , MessageContent(..)
    , messageContentDecoder
    , ResponseContentPart(..)
    , responseContentPartDecoder
    ) where

import qualified Data.Aeson as Aeson
import Data.Aeson hiding (TaggedObject)
import qualified Data.Hermes as Hermes
import Data.Text (Text)

import Agent.Responses.Types.Common

data ResponseRole = RoleUser | RoleAssistant | RoleSystem | RoleDeveloper | RoleUnknown !Text
    deriving stock (Eq, Show)

responseRoleText :: ResponseRole -> Text
responseRoleText = \case
    RoleUser -> "user"
    RoleAssistant -> "assistant"
    RoleSystem -> "system"
    RoleDeveloper -> "developer"
    RoleUnknown value -> value

instance ToJSON ResponseRole where
    toJSON = Aeson.String . responseRoleText

responseRoleDecoder :: Hermes.Decoder ResponseRole
responseRoleDecoder = fmap (\case
        "user" -> RoleUser
        "assistant" -> RoleAssistant
        "system" -> RoleSystem
        "developer" -> RoleDeveloper
        value -> RoleUnknown value) Hermes.text

data ItemStatus = ItemInProgress | ItemCompleted | ItemIncomplete | ItemStatusUnknown !Text
    deriving stock (Eq, Show)

itemStatusText :: ItemStatus -> Text
itemStatusText = \case
    ItemInProgress -> "in_progress"
    ItemCompleted -> "completed"
    ItemIncomplete -> "incomplete"
    ItemStatusUnknown value -> value

instance ToJSON ItemStatus where
    toJSON = Aeson.String . itemStatusText

itemStatusDecoder :: Hermes.Decoder ItemStatus
itemStatusDecoder = fmap (\case
        "in_progress" -> ItemInProgress
        "completed" -> ItemCompleted
        "incomplete" -> ItemIncomplete
        value -> ItemStatusUnknown value) Hermes.text

data MessageContent
    = MessageContentText !Text
    | MessageContentParts ![ResponseContentPart]
    deriving stock (Eq, Show)

instance ToJSON MessageContent where
    toJSON (MessageContentText value) = Aeson.String value
    toJSON (MessageContentParts value) = toJSON value

messageContentDecoder :: Hermes.Decoder MessageContent
messageContentDecoder =
    Hermes.getType >>= \case
        Hermes.VString -> MessageContentText <$> Hermes.text
        Hermes.VArray ->
            MessageContentParts <$> Hermes.list responseContentPartDecoder
        _ -> fail "MessageContent: expected string or array"

data ResponseContentPart
    = InputTextPart
        { text                  :: !Text
        , promptCacheBreakpoint :: !(Maybe Aeson.Value)
        , extraFields           :: !Aeson.Object
        }
    | InputImagePart
        { detail                :: !(Maybe Text)
        , fileId                :: !(Maybe Text)
        , imageUrl              :: !(Maybe Text)
        , promptCacheBreakpoint :: !(Maybe Aeson.Value)
        , extraFields           :: !Aeson.Object
        }
    | InputFilePart
        { detail                :: !(Maybe Text)
        , fileData              :: !(Maybe Text)
        , fileId                :: !(Maybe Text)
        , fileUrl               :: !(Maybe Text)
        , filename              :: !(Maybe Text)
        , promptCacheBreakpoint :: !(Maybe Aeson.Value)
        , extraFields           :: !Aeson.Object
        }
    | InputAudioPart
        { inputAudio  :: !Aeson.Value
        , extraFields :: !Aeson.Object
        }
    | OutputTextPart
        { text        :: !Text
        , annotations :: !(Maybe [Aeson.Value])
        , logprobs    :: !(Maybe [Aeson.Value])
        , extraFields :: !Aeson.Object
        }
    | RefusalPart
        { refusal     :: !Text
        , extraFields :: !Aeson.Object
        }
    | ReasoningTextPart
        { text        :: !Text
        , extraFields :: !Aeson.Object
        }
    | SummaryTextPart
        { text        :: !Text
        , extraFields :: !Aeson.Object
        }
    | EncryptedContentPart
        { encryptedContent :: !Text
        , extraFields      :: !Aeson.Object
        }
    | PlainTextPart
        { text        :: !Text
        , extraFields :: !Aeson.Object
        }
    | UnknownContentPart !TaggedObject
    deriving stock (Eq, Show)

instance ToJSON ResponseContentPart where
    toJSON InputTextPart { text, promptCacheBreakpoint, extraFields } = objectWith extraFields
        [ Just (field "type" ("input_text" :: Text))
        , Just (field "text" text)
        , optionalField "prompt_cache_breakpoint" promptCacheBreakpoint
        ]
    toJSON InputImagePart { detail, fileId, imageUrl, promptCacheBreakpoint, extraFields } = objectWith extraFields
        [ Just (field "type" ("input_image" :: Text))
        , optionalField "detail" detail
        , optionalField "file_id" fileId
        , optionalField "image_url" imageUrl
        , optionalField "prompt_cache_breakpoint" promptCacheBreakpoint
        ]
    toJSON InputFilePart { detail, fileData, fileId, fileUrl, filename, promptCacheBreakpoint, extraFields } = objectWith extraFields
        [ Just (field "type" ("input_file" :: Text))
        , optionalField "detail" detail
        , optionalField "file_data" fileData
        , optionalField "file_id" fileId
        , optionalField "file_url" fileUrl
        , optionalField "filename" filename
        , optionalField "prompt_cache_breakpoint" promptCacheBreakpoint
        ]
    toJSON InputAudioPart { inputAudio, extraFields } = objectWith extraFields
        [ Just (field "type" ("input_audio" :: Text))
        , Just (field "input_audio" inputAudio)
        ]
    toJSON OutputTextPart { text, annotations, logprobs, extraFields } = objectWith extraFields
        [ Just (field "type" ("output_text" :: Text))
        , Just (field "text" text)
        , optionalField "annotations" annotations
        , optionalField "logprobs" logprobs
        ]
    toJSON RefusalPart { refusal, extraFields } = objectWith extraFields
        [Just (field "type" ("refusal" :: Text)), Just (field "refusal" refusal)]
    toJSON ReasoningTextPart { text, extraFields } = objectWith extraFields
        [Just (field "type" ("reasoning_text" :: Text)), Just (field "text" text)]
    toJSON SummaryTextPart { text, extraFields } = objectWith extraFields
        [Just (field "type" ("summary_text" :: Text)), Just (field "text" text)]
    toJSON EncryptedContentPart { encryptedContent, extraFields } =
        objectWith extraFields
            [ Just (field "type" ("encrypted_content" :: Text))
            , Just (field "encrypted_content" encryptedContent)
            ]
    toJSON PlainTextPart { text, extraFields } = objectWith extraFields
        [Just (field "type" ("text" :: Text)), Just (field "text" text)]
    toJSON (UnknownContentPart tagged) = toJSON tagged

responseContentPartDecoder :: Hermes.Decoder ResponseContentPart
responseContentPartDecoder =
    Hermes.object do
        tag <- Hermes.atKey "type" Hermes.text
        Hermes.liftObjectDecoder $ Hermes.object $ case tag of
            "input_text" -> InputTextPart
                <$> Hermes.atKey "text" Hermes.text
                <*> optionalAtKey "prompt_cache_breakpoint" rawValue
                <*> pure mempty
            "input_image" -> InputImagePart
                <$> optionalAtKey "detail" Hermes.text
                <*> optionalAtKey "file_id" Hermes.text
                <*> optionalAtKey "image_url" Hermes.text
                <*> optionalAtKey "prompt_cache_breakpoint" rawValue
                <*> pure mempty
            "input_file" -> InputFilePart
                <$> optionalAtKey "detail" Hermes.text
                <*> optionalAtKey "file_data" Hermes.text
                <*> optionalAtKey "file_id" Hermes.text
                <*> optionalAtKey "file_url" Hermes.text
                <*> optionalAtKey "filename" Hermes.text
                <*> optionalAtKey "prompt_cache_breakpoint" rawValue
                <*> pure mempty
            "input_audio" -> InputAudioPart
                <$> Hermes.atKey "input_audio" rawValue
                <*> pure mempty
            "output_text" -> OutputTextPart
                <$> Hermes.atKey "text" Hermes.text
                <*> optionalAtKey "annotations" (Hermes.list rawValue)
                <*> optionalAtKey "logprobs" (Hermes.list rawValue)
                <*> pure mempty
            "refusal" -> RefusalPart
                <$> Hermes.atKey "refusal" Hermes.text
                <*> pure mempty
            "reasoning_text" -> ReasoningTextPart
                <$> Hermes.atKey "text" Hermes.text
                <*> pure mempty
            "summary_text" -> SummaryTextPart
                <$> Hermes.atKey "text" Hermes.text
                <*> pure mempty
            "encrypted_content" -> EncryptedContentPart
                <$> Hermes.atKey "encrypted_content" Hermes.text
                <*> pure mempty
            "text" -> PlainTextPart
                <$> Hermes.atKey "text" Hermes.text
                <*> pure mempty
            _ -> UnknownContentPart
                <$> (TaggedObject tag mempty <$ consumeObject)
  where
    rawValue = aesonValueDecoder
    consumeObject = pure ()
