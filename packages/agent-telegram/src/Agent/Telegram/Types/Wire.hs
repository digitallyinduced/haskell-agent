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
    ) where

import Data.Aeson
    ( FromJSON(..)
    , ToJSON(..)
    , Value
    , object
    , withObject
    , (.:)
    , (.:?)
    , (.!=)
    , (.=)
    )
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as Text
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

instance FromJSON TelegramVoice where
    parseJSON = withObject "TelegramVoice" \o ->
        TelegramVoice
            <$> (o .:? "file_id" >>= maybe (o .: "fileId") pure)
            <*> (o .:? "duration" .!= 0)
            <*> (o .:? "mime_type" >>= maybe (o .:? "mimeType") (pure . Just))
            <*> (o .:? "file_size" >>= maybe (o .:? "fileSize") (pure . Just))

data TelegramUser = TelegramUser
    { userId :: !Integer
    , userIsBot :: !Bool
    , userFirstName :: !(Maybe Text)
    , userLastName :: !(Maybe Text)
    , userUsername :: !(Maybe Text)
    } deriving (Eq, Show)

instance FromJSON TelegramUser where
    parseJSON = withObject "TelegramUser" \o ->
        TelegramUser
            <$> o .: "id"
            <*> (o .:? "is_bot" .!= False)
            <*> o .:? "first_name"
            <*> o .:? "last_name"
            <*> o .:? "username"

data TelegramChat = TelegramChat
    { telegramChatId :: !Integer
    , telegramChatType :: !Text
    } deriving (Eq, Show)

instance FromJSON TelegramChat where
    parseJSON = withObject "TelegramChat" \o ->
        TelegramChat <$> o .: "id" <*> o .: "type"

data TelegramChatMember = TelegramChatMember
    { chatMemberUser :: !TelegramUser
    , chatMemberStatus :: !Text
    , chatMemberIsMember :: !(Maybe Bool)
    } deriving (Eq, Show)

instance FromJSON TelegramChatMember where
    parseJSON = withObject "TelegramChatMember" \o ->
        TelegramChatMember
            <$> o .: "user"
            <*> o .: "status"
            <*> o .:? "is_member"

data TelegramChatMemberUpdated = TelegramChatMemberUpdated
    { chatMemberUpdatedChat :: !TelegramChat
    , chatMemberUpdatedFrom :: !TelegramUser
    , chatMemberUpdatedOld :: !TelegramChatMember
    , chatMemberUpdatedNew :: !TelegramChatMember
    } deriving (Eq, Show)

instance FromJSON TelegramChatMemberUpdated where
    parseJSON = withObject "TelegramChatMemberUpdated" \o ->
        TelegramChatMemberUpdated
            <$> o .: "chat"
            <*> o .: "from"
            <*> o .: "old_chat_member"
            <*> o .: "new_chat_member"

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
    , messageForwardOrigin :: !(Maybe Value)
    , messageEditDate :: !(Maybe Integer)
    , messageReplyTo :: !(Maybe TelegramMessage)
    , messageNewChatMembers :: ![TelegramUser]
    } deriving (Eq, Show)

instance FromJSON TelegramMessage where
    parseJSON = withObject "TelegramMessage" \o ->
        TelegramMessage
            <$> o .: "message_id"
            <*> o .:? "from"
            <*> o .: "chat"
            <*> o .:? "message_thread_id"
            <*> o .:? "text"
            <*> o .:? "caption"
            <*> o .:? "voice"
            <*> o .:? "audio"
            <*> o .:? "document"
            <*> (o .:? "photo" .!= [])
            <*> o .:? "video"
            <*> o .:? "video_note"
            <*> o .:? "animation"
            <*> o .:? "sticker"
            <*> o .:? "location"
            <*> o .:? "contact"
            <*> o .:? "venue"
            <*> o .:? "poll"
            <*> o .:? "dice"
            <*> o .:? "media_group_id"
            <*> o .:? "forward_origin"
            <*> o .:? "edit_date"
            <*> o .:? "reply_to_message"
            <*> (o .:? "new_chat_members" .!= [])

data TelegramReactionType = TelegramReactionType
    { reactionType :: !Text
    , reactionEmoji :: !(Maybe Text)
    , reactionCustomEmojiId :: !(Maybe Text)
    } deriving (Eq, Show)

instance FromJSON TelegramReactionType where
    parseJSON = withObject "TelegramReactionType" \o ->
        TelegramReactionType
            <$> o .: "type"
            <*> o .:? "emoji"
            <*> o .:? "custom_emoji_id"

data TelegramMessageReaction = TelegramMessageReaction
    { messageReactionChat :: !TelegramChat
    , messageReactionMessageId :: !Integer
    , messageReactionUser :: !(Maybe TelegramUser)
    , messageReactionOld :: ![TelegramReactionType]
    , messageReactionNew :: ![TelegramReactionType]
    } deriving (Eq, Show)

instance FromJSON TelegramMessageReaction where
    parseJSON = withObject "TelegramMessageReaction" \o ->
        TelegramMessageReaction
            <$> o .: "chat"
            <*> o .: "message_id"
            <*> o .:? "user"
            <*> (o .:? "old_reaction" .!= [])
            <*> (o .:? "new_reaction" .!= [])

data TelegramPhotoSize = TelegramPhotoSize
    { photoFileId :: !Text
    , photoWidth :: !Int
    , photoHeight :: !Int
    , photoFileSize :: !(Maybe Integer)
    } deriving (Eq, Show)

instance FromJSON TelegramPhotoSize where
    parseJSON = withObject "TelegramPhotoSize" \o ->
        TelegramPhotoSize
            <$> o .: "file_id"
            <*> o .:? "width" .!= 0
            <*> o .:? "height" .!= 0
            <*> o .:? "file_size"

data TelegramDocument = TelegramDocument
    { documentFileId :: !Text
    , documentFileName :: !(Maybe Text)
    , documentMimeType :: !(Maybe Text)
    , documentFileSize :: !(Maybe Integer)
    } deriving (Eq, Show)

instance FromJSON TelegramDocument where
    parseJSON = withObject "TelegramDocument" \o ->
        TelegramDocument
            <$> o .: "file_id"
            <*> o .:? "file_name"
            <*> o .:? "mime_type"
            <*> o .:? "file_size"

data TelegramVideo = TelegramVideo
    { videoFileId :: !Text
    , videoDuration :: !Int
    , videoMimeType :: !(Maybe Text)
    , videoFileName :: !(Maybe Text)
    , videoFileSize :: !(Maybe Integer)
    } deriving (Eq, Show)

instance FromJSON TelegramVideo where
    parseJSON = withObject "TelegramVideo" \o ->
        TelegramVideo
            <$> o .: "file_id"
            <*> o .:? "duration" .!= 0
            <*> o .:? "mime_type"
            <*> o .:? "file_name"
            <*> o .:? "file_size"

data TelegramVideoNote = TelegramVideoNote
    { videoNoteFileId :: !Text
    , videoNoteDuration :: !Int
    , videoNoteLength :: !Int
    , videoNoteFileSize :: !(Maybe Integer)
    } deriving (Eq, Show)

instance FromJSON TelegramVideoNote where
    parseJSON = withObject "TelegramVideoNote" \o ->
        TelegramVideoNote
            <$> o .: "file_id"
            <*> o .:? "duration" .!= 0
            <*> o .:? "length" .!= 0
            <*> o .:? "file_size"

data TelegramAudio = TelegramAudio
    { audioFileId :: !Text
    , audioDuration :: !Int
    , audioMimeType :: !(Maybe Text)
    , audioFileName :: !(Maybe Text)
    , audioFileSize :: !(Maybe Integer)
    } deriving (Eq, Show)

instance FromJSON TelegramAudio where
    parseJSON = withObject "TelegramAudio" \o ->
        TelegramAudio
            <$> o .: "file_id"
            <*> o .:? "duration" .!= 0
            <*> o .:? "mime_type"
            <*> o .:? "file_name"
            <*> o .:? "file_size"

data TelegramAnimation = TelegramAnimation
    { animationFileId :: !Text
    , animationMimeType :: !(Maybe Text)
    , animationFileName :: !(Maybe Text)
    , animationFileSize :: !(Maybe Integer)
    } deriving (Eq, Show)

instance FromJSON TelegramAnimation where
    parseJSON = withObject "TelegramAnimation" \o ->
        TelegramAnimation
            <$> o .: "file_id"
            <*> o .:? "mime_type"
            <*> o .:? "file_name"
            <*> o .:? "file_size"

data TelegramSticker = TelegramSticker
    { stickerFileId :: !Text
    , stickerEmoji :: !(Maybe Text)
    , stickerIsAnimated :: !Bool
    , stickerFileSize :: !(Maybe Integer)
    } deriving (Eq, Show)

instance FromJSON TelegramSticker where
    parseJSON = withObject "TelegramSticker" \o ->
        TelegramSticker
            <$> o .: "file_id"
            <*> o .:? "emoji"
            <*> o .:? "is_animated" .!= False
            <*> o .:? "file_size"

data TelegramLocation = TelegramLocation
    { locationLatitude :: !Double
    , locationLongitude :: !Double
    } deriving (Eq, Show)

instance FromJSON TelegramLocation where
    parseJSON = withObject "TelegramLocation" \o ->
        TelegramLocation <$> o .: "latitude" <*> o .: "longitude"

data TelegramContact = TelegramContact
    { contactPhoneNumber :: !Text
    , contactFirstName :: !Text
    , contactLastName :: !(Maybe Text)
    } deriving (Eq, Show)

instance FromJSON TelegramContact where
    parseJSON = withObject "TelegramContact" \o ->
        TelegramContact <$> o .: "phone_number" <*> o .: "first_name" <*> o .:? "last_name"

data TelegramVenue = TelegramVenue
    { venueTitle :: !Text
    , venueAddress :: !Text
    } deriving (Eq, Show)

instance FromJSON TelegramVenue where
    parseJSON = withObject "TelegramVenue" \o ->
        TelegramVenue <$> o .: "title" <*> o .: "address"

data TelegramPoll = TelegramPoll
    { pollId :: !Text
    , pollQuestion :: !Text
    } deriving (Eq, Show)

instance FromJSON TelegramPoll where
    parseJSON = withObject "TelegramPoll" \o ->
        TelegramPoll <$> o .: "id" <*> o .: "question"

data TelegramDice = TelegramDice
    { diceEmoji :: !Text
    , diceValue :: !Int
    } deriving (Eq, Show)

instance FromJSON TelegramDice where
    parseJSON = withObject "TelegramDice" \o ->
        TelegramDice <$> o .: "emoji" <*> o .: "value"

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

instance FromJSON TelegramMediaKind where
    parseJSON = Aeson.withText "TelegramMediaKind" \case
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

instance FromJSON TelegramFileMedia where
    parseJSON = withObject "TelegramFileMedia" \o ->
        TelegramFileMedia
            <$> (o .:? "file_id" >>= maybe (o .: "fileId") pure)
            <*> (o .:? "file_name" >>= maybe (o .:? "name") (pure . Just))
            <*> (o .:? "mime_type" >>= maybe (o .:? "mimeType") (pure . Just))
            <*> (o .:? "file_size" >>= maybe (o .:? "fileSize") (pure . Just))
            <*> o .:? "duration"

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

instance FromJSON TelegramMedia where
    parseJSON = withObject "TelegramMedia" \o ->
        TelegramMedia
            <$> o .: "kind"
            <*> o .:? "file"
            <*> o .:? "description" .!= ""

data TelegramUpdate = TelegramUpdate
    { updateId :: !Integer
    , updateMessage :: !(Maybe TelegramMessage)
    , updateEditedMessage :: !(Maybe TelegramMessage)
    , updateMessageReaction :: !(Maybe TelegramMessageReaction)
    , updateCallbackQuery :: !(Maybe TelegramCallbackQuery)
    , updateMyChatMember :: !(Maybe TelegramChatMemberUpdated)
    } deriving (Eq, Show)

instance FromJSON TelegramUpdate where
    parseJSON = withObject "TelegramUpdate" \o ->
        TelegramUpdate
            <$> o .: "update_id"
            <*> o .:? "message"
            <*> o .:? "edited_message"
            <*> o .:? "message_reaction"
            <*> o .:? "callback_query"
            <*> o .:? "my_chat_member"

data TelegramCallbackQuery = TelegramCallbackQuery
    { callbackQueryId :: !Text
    , callbackQueryFrom :: !TelegramUser
    , callbackQueryMessage :: !(Maybe TelegramMessage)
    , callbackQueryData :: !(Maybe Text)
    } deriving (Eq, Show)

instance FromJSON TelegramCallbackQuery where
    parseJSON = withObject "TelegramCallbackQuery" \o ->
        TelegramCallbackQuery
            <$> o .: "id"
            <*> o .: "from"
            <*> o .:? "message"
            <*> o .:? "data"

data TelegramResponse a = TelegramResponse
    { responseOk :: !Bool
    , responseResult :: !(Maybe a)
    , responseDescription :: !(Maybe Text)
    , responseErrorCode :: !(Maybe Int)
    , responseParameters :: !(Maybe TelegramResponseParameters)
    }

instance FromJSON a => FromJSON (TelegramResponse a) where
    parseJSON = withObject "TelegramResponse" \o ->
        TelegramResponse
            <$> o .: "ok"
            <*> o .:? "result"
            <*> o .:? "description"
            <*> o .:? "error_code"
            <*> o .:? "parameters"

data TelegramResponseParameters = TelegramResponseParameters
    { responseRetryAfter :: !(Maybe Int)
    } deriving (Eq, Show)

instance FromJSON TelegramResponseParameters where
    parseJSON = withObject "TelegramResponseParameters" \o ->
        TelegramResponseParameters <$> o .:? "retry_after"

data TelegramClient = TelegramClient
    { clientToken :: !Text
    , clientManager :: !Http.Manager
    }

