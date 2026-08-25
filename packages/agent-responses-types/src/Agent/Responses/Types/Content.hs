module Agent.Responses.Types.Content
    ( ResponseRole(..)
    , ItemStatus(..)
    , MessageContent(..)
    , ResponseContentPart(..)
    ) where

import qualified Data.Aeson as Aeson
import Data.Aeson
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

instance FromJSON ResponseRole where
    parseJSON = withText "ResponseRole" $ pure . \case
        "user" -> RoleUser
        "assistant" -> RoleAssistant
        "system" -> RoleSystem
        "developer" -> RoleDeveloper
        value -> RoleUnknown value

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

instance FromJSON ItemStatus where
    parseJSON = withText "ItemStatus" $ pure . \case
        "in_progress" -> ItemInProgress
        "completed" -> ItemCompleted
        "incomplete" -> ItemIncomplete
        value -> ItemStatusUnknown value

data MessageContent
    = MessageContentText !Text
    | MessageContentParts ![ResponseContentPart]
    deriving stock (Eq, Show)

instance ToJSON MessageContent where
    toJSON (MessageContentText value) = Aeson.String value
    toJSON (MessageContentParts value) = toJSON value

instance FromJSON MessageContent where
    parseJSON (Aeson.String value) = pure (MessageContentText value)
    parseJSON value@(Aeson.Array _) = MessageContentParts <$> parseJSON value
    parseJSON value = fail ("MessageContent: expected string or array, got " <> show value)

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
    toJSON (UnknownContentPart tagged) = toJSON tagged

instance FromJSON ResponseContentPart where
    parseJSON value = withObject "ResponseContentPart" (\o -> do
        tag <- o .: "type"
        case (tag :: Text) of
            "input_text" -> InputTextPart
                <$> o .: "text"
                <*> o .:? "prompt_cache_breakpoint"
                <*> pure (without ["type", "text", "prompt_cache_breakpoint"] o)
            "input_image" -> InputImagePart
                <$> o .:? "detail"
                <*> o .:? "file_id"
                <*> o .:? "image_url"
                <*> o .:? "prompt_cache_breakpoint"
                <*> pure (without ["type", "detail", "file_id", "image_url", "prompt_cache_breakpoint"] o)
            "input_file" -> InputFilePart
                <$> o .:? "detail"
                <*> o .:? "file_data"
                <*> o .:? "file_id"
                <*> o .:? "file_url"
                <*> o .:? "filename"
                <*> o .:? "prompt_cache_breakpoint"
                <*> pure (without ["type", "detail", "file_data", "file_id", "file_url", "filename", "prompt_cache_breakpoint"] o)
            "input_audio" -> InputAudioPart
                <$> o .: "input_audio"
                <*> pure (without ["type", "input_audio"] o)
            "output_text" -> OutputTextPart
                <$> o .: "text"
                <*> o .:? "annotations"
                <*> o .:? "logprobs"
                <*> pure (without ["type", "text", "annotations", "logprobs"] o)
            "refusal" -> RefusalPart
                <$> o .: "refusal"
                <*> pure (without ["type", "refusal"] o)
            "reasoning_text" -> ReasoningTextPart
                <$> o .: "text"
                <*> pure (without ["type", "text"] o)
            "summary_text" -> SummaryTextPart
                <$> o .: "text"
                <*> pure (without ["type", "text"] o)
            "encrypted_content" -> EncryptedContentPart
                <$> o .: "encrypted_content"
                <*> pure (without ["type", "encrypted_content"] o)
            _ -> UnknownContentPart <$> parseJSON value) value
