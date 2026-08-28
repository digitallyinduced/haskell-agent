{-# OPTIONS_GHC -Wno-orphans #-}

-- | Telegram Bot API wire and media types.
module Agent.Telegram.Types.Wire
    ( TelegramVoice(..)
    , TelegramMedia(..)
    , TelegramMediaKind(..)
    , TelegramFileMedia(..)
    , TelegramPhotoSize(..)
    , TelegramDocument(..)
    , TelegramVideo(..)
    , TelegramVideoNote(..)
    , TelegramAudio(..)
    , TelegramAnimation(..)
    , TelegramSticker(..)
    , TelegramLocation(..)
    , TelegramContact(..)
    , TelegramVenue(..)
    , TelegramPoll(..)
    , TelegramDice(..)
    , TelegramUser(..)
    , TelegramMessageEntity(..)
    , TelegramChat(..)
    , TelegramChatMember(..)
    , TelegramChatMemberUpdated(..)
    , TelegramMessage(..)
    , TelegramReactionType(..)
    , TelegramMessageReaction(..)
    , TelegramCallbackQuery(..)
    , TelegramUpdate(..)
    , TelegramResponseParameters(..)
    , TelegramResponse(..)
    , TelegramClient(..)
    , telegramVoiceDecoder
    , telegramMediaDecoder
    , telegramUserDecoder
    , telegramChatMemberDecoder
    , telegramMessageDecoder
    , telegramUpdateDecoder
    , telegramResponseDecoder
    ) where

import Agent.Json (RawJson, rawJsonDecoder)
import qualified Agent.Json.Decode as Hermes
import Control.Applicative ((<|>))
import Data.Aeson
    ( ToJSON(..)
    , object
    , (.=)
    )
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Maybe (fromMaybe)
import qualified Network.HTTP.Client as Http

data TelegramVoice = TelegramVoice
    { voiceFileId :: !Text
    , voiceDuration :: !Int
    , voiceMimeType :: !(Maybe Text)
    , voiceFileSize :: !(Maybe Integer)
    } deriving (Eq, Show)

instance ToJSON TelegramVoice where
    toJSON voice = object
        [ "fileId" .= voice.voiceFileId
        , "duration" .= voice.voiceDuration
        , "mimeType" .= voice.voiceMimeType
        , "fileSize" .= voice.voiceFileSize
        ]

telegramVoiceDecoder :: Hermes.Decoder TelegramVoice
telegramVoiceDecoder = Hermes.object do
    snakeFileId <- Hermes.optionalKey "file_id" Hermes.text
    camelFileId <- Hermes.optionalKey "fileId" Hermes.text
    snakeMime <- Hermes.optionalKey "mime_type" Hermes.text
    camelMime <- Hermes.optionalKey "mimeType" Hermes.text
    snakeSize <- Hermes.optionalKey "file_size" integerDecoder
    camelSize <- Hermes.optionalKey "fileSize" integerDecoder
    duration <- defaultField "duration" 0 Hermes.int
    case snakeFileId <|> camelFileId of
        Nothing -> fail "missing Telegram voice file id"
        Just fileId ->
            pure $
                TelegramVoice
                    fileId
                    duration
                    (snakeMime <|> camelMime)
                    (snakeSize <|> camelSize)

data TelegramUser = TelegramUser
    { userId :: !Integer
    , userIsBot :: !Bool
    , userFirstName :: !(Maybe Text)
    , userLastName :: !(Maybe Text)
    , userUsername :: !(Maybe Text)
    } deriving (Eq, Show)

instance ToJSON TelegramUser where
    toJSON user = object
        [ "id" .= user.userId
        , "isBot" .= user.userIsBot
        , "firstName" .= user.userFirstName
        , "lastName" .= user.userLastName
        , "username" .= user.userUsername
        ]

telegramUserDecoder :: Hermes.Decoder TelegramUser
telegramUserDecoder = Hermes.object do
    snakeBot <- Hermes.optionalKey "is_bot" Hermes.bool
    camelBot <- Hermes.optionalKey "isBot" Hermes.bool
    snakeFirst <- Hermes.optionalKey "first_name" Hermes.text
    camelFirst <- Hermes.optionalKey "firstName" Hermes.text
    snakeLast <- Hermes.optionalKey "last_name" Hermes.text
    camelLast <- Hermes.optionalKey "lastName" Hermes.text
    TelegramUser
        <$> Hermes.atKey "id" integerDecoder
        <*> pure (fromMaybe False (snakeBot <|> camelBot))
        <*> pure (snakeFirst <|> camelFirst)
        <*> pure (snakeLast <|> camelLast)
        <*> Hermes.optionalKey "username" Hermes.text

data TelegramMessageEntity = TelegramMessageEntity
    { entityType :: !Text
    , entityOffset :: !Int
    , entityLength :: !Int
    , entityUser :: !(Maybe TelegramUser)
    } deriving (Eq, Show)

telegramMessageEntityDecoder :: Hermes.Decoder TelegramMessageEntity
telegramMessageEntityDecoder = Hermes.object $
    TelegramMessageEntity
        <$> Hermes.atKey "type" Hermes.text
        <*> Hermes.atKey "offset" Hermes.int
        <*> Hermes.atKey "length" Hermes.int
        <*> Hermes.optionalKey "user" telegramUserDecoder

data TelegramChat = TelegramChat
    { telegramChatId :: !Integer
    , telegramChatType :: !Text
    } deriving (Eq, Show)

telegramChatDecoder :: Hermes.Decoder TelegramChat
telegramChatDecoder = Hermes.object $
    TelegramChat
        <$> Hermes.atKey "id" integerDecoder
        <*> Hermes.atKey "type" Hermes.text

data TelegramChatMember = TelegramChatMember
    { chatMemberUser :: !TelegramUser
    , chatMemberStatus :: !Text
    , chatMemberIsMember :: !(Maybe Bool)
    } deriving (Eq, Show)

telegramChatMemberDecoder :: Hermes.Decoder TelegramChatMember
telegramChatMemberDecoder = Hermes.object $
    TelegramChatMember
        <$> Hermes.atKey "user" telegramUserDecoder
        <*> Hermes.atKey "status" Hermes.text
        <*> Hermes.optionalKey "is_member" Hermes.bool

data TelegramChatMemberUpdated = TelegramChatMemberUpdated
    { chatMemberUpdatedChat :: !TelegramChat
    , chatMemberUpdatedFrom :: !TelegramUser
    , chatMemberUpdatedOld :: !TelegramChatMember
    , chatMemberUpdatedNew :: !TelegramChatMember
    } deriving (Eq, Show)

telegramChatMemberUpdatedDecoder :: Hermes.Decoder TelegramChatMemberUpdated
telegramChatMemberUpdatedDecoder = Hermes.object $
    TelegramChatMemberUpdated
        <$> Hermes.atKey "chat" telegramChatDecoder
        <*> Hermes.atKey "from" telegramUserDecoder
        <*> Hermes.atKey "old_chat_member" telegramChatMemberDecoder
        <*> Hermes.atKey "new_chat_member" telegramChatMemberDecoder

data TelegramMessage = TelegramMessage
    { messageId :: !Integer
    , messageFrom :: !(Maybe TelegramUser)
    , messageChat :: !TelegramChat
    , messageThread :: !(Maybe Integer)
    , messageText :: !(Maybe Text)
    , messageCaption :: !(Maybe Text)
    , messageVoice :: !(Maybe TelegramVoice)
    , messageAudio :: !(Maybe TelegramAudio)
    , messageDocument :: !(Maybe TelegramDocument)
    , messagePhoto :: ![TelegramPhotoSize]
    , messageVideo :: !(Maybe TelegramVideo)
    , messageVideoNote :: !(Maybe TelegramVideoNote)
    , messageAnimation :: !(Maybe TelegramAnimation)
    , messageSticker :: !(Maybe TelegramSticker)
    , messageLocation :: !(Maybe TelegramLocation)
    , messageContact :: !(Maybe TelegramContact)
    , messageVenue :: !(Maybe TelegramVenue)
    , messagePoll :: !(Maybe TelegramPoll)
    , messageDice :: !(Maybe TelegramDice)
    , messageMediaGroupId :: !(Maybe Text)
    , messageForwardOrigin :: !(Maybe RawJson)
    , messageEditDate :: !(Maybe Integer)
    , messageReplyTo :: !(Maybe TelegramMessage)
    , messageNewChatMembers :: ![TelegramUser]
    , messageEntities :: ![TelegramMessageEntity]
    , messageCaptionEntities :: ![TelegramMessageEntity]
    } deriving (Eq, Show)

telegramMessageDecoder :: Hermes.Decoder TelegramMessage
telegramMessageDecoder = Hermes.object $
    TelegramMessage
        <$> Hermes.atKey "message_id" integerDecoder
        <*> Hermes.optionalKey "from" telegramUserDecoder
        <*> Hermes.atKey "chat" telegramChatDecoder
        <*> Hermes.optionalKey "message_thread_id" integerDecoder
        <*> Hermes.optionalKey "text" Hermes.text
        <*> Hermes.optionalKey "caption" Hermes.text
        <*> Hermes.optionalKey "voice" telegramVoiceDecoder
        <*> Hermes.optionalKey "audio" telegramAudioDecoder
        <*> Hermes.optionalKey "document" telegramDocumentDecoder
        <*> defaultField "photo" [] (Hermes.list telegramPhotoSizeDecoder)
        <*> Hermes.optionalKey "video" telegramVideoDecoder
        <*> Hermes.optionalKey "video_note" telegramVideoNoteDecoder
        <*> Hermes.optionalKey "animation" telegramAnimationDecoder
        <*> Hermes.optionalKey "sticker" telegramStickerDecoder
        <*> Hermes.optionalKey "location" telegramLocationDecoder
        <*> Hermes.optionalKey "contact" telegramContactDecoder
        <*> Hermes.optionalKey "venue" telegramVenueDecoder
        <*> Hermes.optionalKey "poll" telegramPollDecoder
        <*> Hermes.optionalKey "dice" telegramDiceDecoder
        <*> Hermes.optionalKey "media_group_id" Hermes.text
        <*> Hermes.optionalKey "forward_origin" rawJsonDecoder
        <*> Hermes.optionalKey "edit_date" integerDecoder
        <*> Hermes.optionalKey "reply_to_message" telegramMessageDecoder
        <*> defaultField "new_chat_members" [] (Hermes.list telegramUserDecoder)
        <*> defaultField "entities" [] (Hermes.list telegramMessageEntityDecoder)
        <*> defaultField "caption_entities" [] (Hermes.list telegramMessageEntityDecoder)

data TelegramReactionType = TelegramReactionType
    { reactionType :: !Text
    , reactionEmoji :: !(Maybe Text)
    , reactionCustomEmojiId :: !(Maybe Text)
    } deriving (Eq, Show)

telegramReactionTypeDecoder :: Hermes.Decoder TelegramReactionType
telegramReactionTypeDecoder = Hermes.object $
    TelegramReactionType
        <$> Hermes.atKey "type" Hermes.text
        <*> Hermes.optionalKey "emoji" Hermes.text
        <*> Hermes.optionalKey "custom_emoji_id" Hermes.text

data TelegramMessageReaction = TelegramMessageReaction
    { messageReactionChat :: !TelegramChat
    , messageReactionMessageId :: !Integer
    , messageReactionUser :: !(Maybe TelegramUser)
    , messageReactionOld :: ![TelegramReactionType]
    , messageReactionNew :: ![TelegramReactionType]
    } deriving (Eq, Show)

telegramMessageReactionDecoder :: Hermes.Decoder TelegramMessageReaction
telegramMessageReactionDecoder = Hermes.object $
    TelegramMessageReaction
        <$> Hermes.atKey "chat" telegramChatDecoder
        <*> Hermes.atKey "message_id" integerDecoder
        <*> Hermes.optionalKey "user" telegramUserDecoder
        <*> defaultField "old_reaction" [] (Hermes.list telegramReactionTypeDecoder)
        <*> defaultField "new_reaction" [] (Hermes.list telegramReactionTypeDecoder)

data TelegramPhotoSize = TelegramPhotoSize
    { photoFileId :: !Text
    , photoWidth :: !Int
    , photoHeight :: !Int
    , photoFileSize :: !(Maybe Integer)
    } deriving (Eq, Show)

telegramPhotoSizeDecoder :: Hermes.Decoder TelegramPhotoSize
telegramPhotoSizeDecoder = Hermes.object $
    TelegramPhotoSize
        <$> Hermes.atKey "file_id" Hermes.text
        <*> defaultField "width" 0 Hermes.int
        <*> defaultField "height" 0 Hermes.int
        <*> Hermes.optionalKey "file_size" integerDecoder

data TelegramDocument = TelegramDocument
    { documentFileId :: !Text
    , documentFileName :: !(Maybe Text)
    , documentMimeType :: !(Maybe Text)
    , documentFileSize :: !(Maybe Integer)
    } deriving (Eq, Show)

telegramDocumentDecoder :: Hermes.Decoder TelegramDocument
telegramDocumentDecoder = Hermes.object $
    TelegramDocument
        <$> Hermes.atKey "file_id" Hermes.text
        <*> Hermes.optionalKey "file_name" Hermes.text
        <*> Hermes.optionalKey "mime_type" Hermes.text
        <*> Hermes.optionalKey "file_size" integerDecoder

data TelegramVideo = TelegramVideo
    { videoFileId :: !Text
    , videoDuration :: !Int
    , videoMimeType :: !(Maybe Text)
    , videoFileName :: !(Maybe Text)
    , videoFileSize :: !(Maybe Integer)
    } deriving (Eq, Show)

telegramVideoDecoder :: Hermes.Decoder TelegramVideo
telegramVideoDecoder = Hermes.object $
    TelegramVideo
        <$> Hermes.atKey "file_id" Hermes.text
        <*> defaultField "duration" 0 Hermes.int
        <*> Hermes.optionalKey "mime_type" Hermes.text
        <*> Hermes.optionalKey "file_name" Hermes.text
        <*> Hermes.optionalKey "file_size" integerDecoder

data TelegramVideoNote = TelegramVideoNote
    { videoNoteFileId :: !Text
    , videoNoteDuration :: !Int
    , videoNoteLength :: !Int
    , videoNoteFileSize :: !(Maybe Integer)
    } deriving (Eq, Show)

telegramVideoNoteDecoder :: Hermes.Decoder TelegramVideoNote
telegramVideoNoteDecoder = Hermes.object $
    TelegramVideoNote
        <$> Hermes.atKey "file_id" Hermes.text
        <*> defaultField "duration" 0 Hermes.int
        <*> defaultField "length" 0 Hermes.int
        <*> Hermes.optionalKey "file_size" integerDecoder

data TelegramAudio = TelegramAudio
    { audioFileId :: !Text
    , audioDuration :: !Int
    , audioMimeType :: !(Maybe Text)
    , audioFileName :: !(Maybe Text)
    , audioFileSize :: !(Maybe Integer)
    } deriving (Eq, Show)

telegramAudioDecoder :: Hermes.Decoder TelegramAudio
telegramAudioDecoder = Hermes.object $
    TelegramAudio
        <$> Hermes.atKey "file_id" Hermes.text
        <*> defaultField "duration" 0 Hermes.int
        <*> Hermes.optionalKey "mime_type" Hermes.text
        <*> Hermes.optionalKey "file_name" Hermes.text
        <*> Hermes.optionalKey "file_size" integerDecoder

data TelegramAnimation = TelegramAnimation
    { animationFileId :: !Text
    , animationMimeType :: !(Maybe Text)
    , animationFileName :: !(Maybe Text)
    , animationFileSize :: !(Maybe Integer)
    } deriving (Eq, Show)

telegramAnimationDecoder :: Hermes.Decoder TelegramAnimation
telegramAnimationDecoder = Hermes.object $
    TelegramAnimation
        <$> Hermes.atKey "file_id" Hermes.text
        <*> Hermes.optionalKey "mime_type" Hermes.text
        <*> Hermes.optionalKey "file_name" Hermes.text
        <*> Hermes.optionalKey "file_size" integerDecoder

data TelegramSticker = TelegramSticker
    { stickerFileId :: !Text
    , stickerEmoji :: !(Maybe Text)
    , stickerIsAnimated :: !Bool
    , stickerFileSize :: !(Maybe Integer)
    } deriving (Eq, Show)

telegramStickerDecoder :: Hermes.Decoder TelegramSticker
telegramStickerDecoder = Hermes.object $
    TelegramSticker
        <$> Hermes.atKey "file_id" Hermes.text
        <*> Hermes.optionalKey "emoji" Hermes.text
        <*> defaultField "is_animated" False Hermes.bool
        <*> Hermes.optionalKey "file_size" integerDecoder

data TelegramLocation = TelegramLocation
    { locationLatitude :: !Double
    , locationLongitude :: !Double
    } deriving (Eq, Show)

telegramLocationDecoder :: Hermes.Decoder TelegramLocation
telegramLocationDecoder = Hermes.object $
    TelegramLocation
        <$> Hermes.atKey "latitude" Hermes.double
        <*> Hermes.atKey "longitude" Hermes.double

data TelegramContact = TelegramContact
    { contactPhoneNumber :: !Text
    , contactFirstName :: !Text
    , contactLastName :: !(Maybe Text)
    } deriving (Eq, Show)

telegramContactDecoder :: Hermes.Decoder TelegramContact
telegramContactDecoder = Hermes.object $
    TelegramContact
        <$> Hermes.atKey "phone_number" Hermes.text
        <*> Hermes.atKey "first_name" Hermes.text
        <*> Hermes.optionalKey "last_name" Hermes.text

data TelegramVenue = TelegramVenue
    { venueTitle :: !Text
    , venueAddress :: !Text
    } deriving (Eq, Show)

telegramVenueDecoder :: Hermes.Decoder TelegramVenue
telegramVenueDecoder = Hermes.object $
    TelegramVenue
        <$> Hermes.atKey "title" Hermes.text
        <*> Hermes.atKey "address" Hermes.text

data TelegramPoll = TelegramPoll
    { pollId :: !Text
    , pollQuestion :: !Text
    } deriving (Eq, Show)

telegramPollDecoder :: Hermes.Decoder TelegramPoll
telegramPollDecoder = Hermes.object $
    TelegramPoll
        <$> Hermes.atKey "id" Hermes.text
        <*> Hermes.atKey "question" Hermes.text

data TelegramDice = TelegramDice
    { diceEmoji :: !Text
    , diceValue :: !Int
    } deriving (Eq, Show)

telegramDiceDecoder :: Hermes.Decoder TelegramDice
telegramDiceDecoder = Hermes.object $
    TelegramDice
        <$> Hermes.atKey "emoji" Hermes.text
        <*> Hermes.atKey "value" Hermes.int

data TelegramMediaKind
    = TelegramMediaPhoto
    | TelegramMediaDocument
    | TelegramMediaVideo
    | TelegramMediaVideoNote
    | TelegramMediaAudio
    | TelegramMediaAnimation
    | TelegramMediaSticker
    | TelegramMediaLocation
    | TelegramMediaContact
    | TelegramMediaVenue
    | TelegramMediaPoll
    | TelegramMediaDice
    deriving (Eq, Show)

instance ToJSON TelegramMediaKind where
    toJSON = \case
        TelegramMediaPhoto -> "photo"
        TelegramMediaDocument -> "document"
        TelegramMediaVideo -> "video"
        TelegramMediaVideoNote -> "video_note"
        TelegramMediaAudio -> "audio"
        TelegramMediaAnimation -> "animation"
        TelegramMediaSticker -> "sticker"
        TelegramMediaLocation -> "location"
        TelegramMediaContact -> "contact"
        TelegramMediaVenue -> "venue"
        TelegramMediaPoll -> "poll"
        TelegramMediaDice -> "dice"

telegramMediaKindDecoder :: Hermes.Decoder TelegramMediaKind
telegramMediaKindDecoder = Hermes.withText \case
        "photo" -> pure TelegramMediaPhoto
        "document" -> pure TelegramMediaDocument
        "video" -> pure TelegramMediaVideo
        "video_note" -> pure TelegramMediaVideoNote
        "audio" -> pure TelegramMediaAudio
        "animation" -> pure TelegramMediaAnimation
        "sticker" -> pure TelegramMediaSticker
        "location" -> pure TelegramMediaLocation
        "contact" -> pure TelegramMediaContact
        "venue" -> pure TelegramMediaVenue
        "poll" -> pure TelegramMediaPoll
        "dice" -> pure TelegramMediaDice
        other -> fail ("unknown Telegram media kind: " <> Text.unpack other)

data TelegramFileMedia = TelegramFileMedia
    { fileMediaFileId :: !Text
    , fileMediaName :: !(Maybe Text)
    , fileMediaMimeType :: !(Maybe Text)
    , fileMediaFileSize :: !(Maybe Integer)
    , fileMediaDuration :: !(Maybe Int)
    } deriving (Eq, Show)

instance ToJSON TelegramFileMedia where
    toJSON media = object
        [ "fileId" .= media.fileMediaFileId
        , "name" .= media.fileMediaName
        , "mimeType" .= media.fileMediaMimeType
        , "fileSize" .= media.fileMediaFileSize
        , "duration" .= media.fileMediaDuration
        ]

telegramFileMediaDecoder :: Hermes.Decoder TelegramFileMedia
telegramFileMediaDecoder = Hermes.object do
    snakeId <- Hermes.optionalKey "file_id" Hermes.text
    camelId <- Hermes.optionalKey "fileId" Hermes.text
    snakeName <- Hermes.optionalKey "file_name" Hermes.text
    camelName <- Hermes.optionalKey "name" Hermes.text
    snakeMime <- Hermes.optionalKey "mime_type" Hermes.text
    camelMime <- Hermes.optionalKey "mimeType" Hermes.text
    snakeSize <- Hermes.optionalKey "file_size" integerDecoder
    camelSize <- Hermes.optionalKey "fileSize" integerDecoder
    case snakeId <|> camelId of
        Nothing -> fail "missing Telegram media file id"
        Just fileId ->
            TelegramFileMedia fileId
                (snakeName <|> camelName)
                (snakeMime <|> camelMime)
                (snakeSize <|> camelSize)
                <$> Hermes.optionalKey "duration" Hermes.int

data TelegramMedia = TelegramMedia
    { telegramMediaKind :: !TelegramMediaKind
    , telegramMediaFile :: !(Maybe TelegramFileMedia)
    , telegramMediaDescription :: !Text
    } deriving (Eq, Show)

instance ToJSON TelegramMedia where
    toJSON media = object
        [ "kind" .= media.telegramMediaKind
        , "file" .= media.telegramMediaFile
        , "description" .= media.telegramMediaDescription
        ]

telegramMediaDecoder :: Hermes.Decoder TelegramMedia
telegramMediaDecoder = Hermes.object $
        TelegramMedia
            <$> Hermes.atKey "kind" telegramMediaKindDecoder
            <*> Hermes.optionalKey "file" telegramFileMediaDecoder
            <*> defaultField "description" "" Hermes.text

data TelegramUpdate = TelegramUpdate
    { updateId :: !Integer
    , updateMessage :: !(Maybe TelegramMessage)
    , updateEditedMessage :: !(Maybe TelegramMessage)
    , updateMessageReaction :: !(Maybe TelegramMessageReaction)
    , updateCallbackQuery :: !(Maybe TelegramCallbackQuery)
    , updateMyChatMember :: !(Maybe TelegramChatMemberUpdated)
    } deriving (Eq, Show)

telegramUpdateDecoder :: Hermes.Decoder TelegramUpdate
telegramUpdateDecoder = Hermes.object $
        TelegramUpdate
            <$> Hermes.atKey "update_id" integerDecoder
            <*> Hermes.optionalKey "message" telegramMessageDecoder
            <*> Hermes.optionalKey "edited_message" telegramMessageDecoder
            <*> Hermes.optionalKey "message_reaction" telegramMessageReactionDecoder
            <*> Hermes.optionalKey "callback_query" telegramCallbackQueryDecoder
            <*> Hermes.optionalKey "my_chat_member" telegramChatMemberUpdatedDecoder

data TelegramCallbackQuery = TelegramCallbackQuery
    { callbackQueryId :: !Text
    , callbackQueryFrom :: !TelegramUser
    , callbackQueryMessage :: !(Maybe TelegramMessage)
    , callbackQueryData :: !(Maybe Text)
    } deriving (Eq, Show)

telegramCallbackQueryDecoder :: Hermes.Decoder TelegramCallbackQuery
telegramCallbackQueryDecoder = Hermes.object $
        TelegramCallbackQuery
            <$> Hermes.atKey "id" Hermes.text
            <*> Hermes.atKey "from" telegramUserDecoder
            <*> Hermes.optionalKey "message" telegramMessageDecoder
            <*> Hermes.optionalKey "data" Hermes.text

data TelegramResponse a = TelegramResponse
    { responseOk :: !Bool
    , responseResult :: !(Maybe a)
    , responseDescription :: !(Maybe Text)
    , responseErrorCode :: !(Maybe Int)
    , responseParameters :: !(Maybe TelegramResponseParameters)
    }

telegramResponseDecoder
    :: Hermes.Decoder a
    -> Hermes.Decoder (TelegramResponse a)
telegramResponseDecoder resultDecoder = Hermes.object $
        TelegramResponse
            <$> Hermes.atKey "ok" Hermes.bool
            <*> Hermes.optionalKey "result" resultDecoder
            <*> Hermes.optionalKey "description" Hermes.text
            <*> Hermes.optionalKey "error_code" Hermes.int
            <*> Hermes.optionalKey "parameters" telegramResponseParametersDecoder

data TelegramResponseParameters = TelegramResponseParameters
    { responseRetryAfter :: !(Maybe Int)
    } deriving (Eq, Show)

telegramResponseParametersDecoder :: Hermes.Decoder TelegramResponseParameters
telegramResponseParametersDecoder = Hermes.object $
    TelegramResponseParameters <$> Hermes.optionalKey "retry_after" Hermes.int

data TelegramClient = TelegramClient
    { clientToken :: !Text
    , clientManager :: !Http.Manager
    }


defaultField
    :: Text
    -> a
    -> Hermes.Decoder a
    -> Hermes.FieldsDecoder a
defaultField key fallback decoder =
    Hermes.defaultKey fallback key decoder

integerDecoder :: Hermes.Decoder Integer
integerDecoder = fromIntegral <$> Hermes.int

