-- | Telegram message content and media extraction.
module Agent.Telegram.Classify.Media
    ( hasTelegramMedia
    , messageMediaAttachments
    , messageContentText
    ) where

import Agent.Telegram.Types
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as Text

hasTelegramMedia :: TelegramMessage -> Bool
hasTelegramMedia = not . null . messageMediaAttachments

messageMediaAttachments :: TelegramMessage -> [TelegramMedia]
messageMediaAttachments message =
    concat
        [ maybeToList photoAttachment
        , maybeToList documentAttachment
        , maybeToList audioAttachment
        , maybeToList videoAttachment
        , maybeToList videoNoteAttachment
        , maybeToList animationAttachment
        , maybeToList stickerAttachment
        ]
  where
    photoAttachment =
        case reverse message.messagePhoto of
            photo : _ ->
                Just TelegramMedia
                    { telegramMediaKind = TelegramMediaPhoto
                    , telegramMediaFile =
                        Just TelegramFileMedia
                            { fileMediaFileId = photo.photoFileId
                            , fileMediaName = Nothing
                            , fileMediaMimeType = Just "image/jpeg"
                            , fileMediaFileSize = photo.photoFileSize
                            , fileMediaDuration = Nothing
                            }
                    , telegramMediaDescription = "[Photo]"
                    }
            [] -> Nothing

    documentAttachment =
        fmap
            (\document -> TelegramMedia
                { telegramMediaKind = TelegramMediaDocument
                , telegramMediaFile =
                    Just TelegramFileMedia
                        { fileMediaFileId = document.documentFileId
                        , fileMediaName = document.documentFileName
                        , fileMediaMimeType = document.documentMimeType
                        , fileMediaFileSize = document.documentFileSize
                        , fileMediaDuration = Nothing
                        }
                , telegramMediaDescription =
                    "[Document: "
                        <> fromMaybe document.documentFileId document.documentFileName
                        <> "]"
                })
            message.messageDocument

    audioAttachment =
        fmap
            (\audio -> TelegramMedia
                { telegramMediaKind = TelegramMediaAudio
                , telegramMediaFile =
                    Just TelegramFileMedia
                        { fileMediaFileId = audio.audioFileId
                        , fileMediaName = audio.audioFileName
                        , fileMediaMimeType = audio.audioMimeType
                        , fileMediaFileSize = audio.audioFileSize
                        , fileMediaDuration = Just audio.audioDuration
                        }
                , telegramMediaDescription =
                    "[Audio file: "
                        <> fromMaybe "audio" audio.audioFileName
                        <> "]"
                })
            message.messageAudio

    videoAttachment =
        fmap
            (\video -> TelegramMedia
                { telegramMediaKind = TelegramMediaVideo
                , telegramMediaFile =
                    Just TelegramFileMedia
                        { fileMediaFileId = video.videoFileId
                        , fileMediaName = video.videoFileName
                        , fileMediaMimeType = video.videoMimeType
                        , fileMediaFileSize = video.videoFileSize
                        , fileMediaDuration = Just video.videoDuration
                        }
                , telegramMediaDescription =
                    "[Video file: "
                        <> fromMaybe "video" video.videoFileName
                        <> "]"
                })
            message.messageVideo

    videoNoteAttachment =
        fmap
            (\note -> TelegramMedia
                { telegramMediaKind = TelegramMediaVideoNote
                , telegramMediaFile =
                    Just TelegramFileMedia
                        { fileMediaFileId = note.videoNoteFileId
                        , fileMediaName = Nothing
                        , fileMediaMimeType = Just "video/mp4"
                        , fileMediaFileSize = note.videoNoteFileSize
                        , fileMediaDuration = Just note.videoNoteDuration
                        }
                , telegramMediaDescription = "[Video note]"
                })
            message.messageVideoNote

    animationAttachment =
        fmap
            (\animation -> TelegramMedia
                { telegramMediaKind = TelegramMediaAnimation
                , telegramMediaFile =
                    Just TelegramFileMedia
                        { fileMediaFileId = animation.animationFileId
                        , fileMediaName = animation.animationFileName
                        , fileMediaMimeType = animation.animationMimeType
                        , fileMediaFileSize = animation.animationFileSize
                        , fileMediaDuration = Nothing
                        }
                , telegramMediaDescription = "[Animation]"
                })
            message.messageAnimation

    stickerAttachment =
        fmap
            (\sticker -> TelegramMedia
                { telegramMediaKind = TelegramMediaSticker
                , telegramMediaFile =
                    Just TelegramFileMedia
                        { fileMediaFileId = sticker.stickerFileId
                        , fileMediaName = Nothing
                        , fileMediaMimeType = Just "application/octet-stream"
                        , fileMediaFileSize = sticker.stickerFileSize
                        , fileMediaDuration = Nothing
                        }
                , telegramMediaDescription =
                    "[Sticker"
                        <> maybe "" (\emoji -> " " <> emoji) sticker.stickerEmoji
                        <> "]"
                })
            message.messageSticker

messageContentText :: TelegramMessage -> Maybe Text
messageContentText message =
    case message.messageText <|> message.messageCaption of
        Just text -> Just text
        Nothing
            | Just _ <- message.messageVoice -> Just "[Voice message]"
            | Just audio <- message.messageAudio ->
                Just ("[Audio file: " <> fromMaybe "audio" audio.audioFileName <> "]")
            | Just document <- message.messageDocument ->
                Just ("[Document: " <> fromMaybe document.documentFileId document.documentFileName <> "]")
            | not (null message.messagePhoto) -> Just "[Photo]"
            | Just video <- message.messageVideo ->
                Just ("[Video file: " <> fromMaybe "video" video.videoFileName <> "]")
            | Just _ <- message.messageVideoNote -> Just "[Video note]"
            | Just _ <- message.messageAnimation -> Just "[Animation]"
            | Just sticker <- message.messageSticker ->
                Just
                    ("[Sticker"
                        <> maybe "" (\emoji -> " " <> emoji) sticker.stickerEmoji
                        <> "]")
            | Just location <- message.messageLocation ->
                Just
                    ("[Location: "
                        <> Text.pack (show location.locationLatitude)
                        <> ", "
                        <> Text.pack (show location.locationLongitude)
                        <> "]")
            | Just contact <- message.messageContact ->
                Just ("[Contact: " <> contact.contactFirstName <> "]")
            | Just venue <- message.messageVenue ->
                Just ("[Venue: " <> venue.venueTitle <> "]")
            | Just poll <- message.messagePoll ->
                Just ("[Poll: " <> poll.pollQuestion <> "]")
            | Just dice <- message.messageDice ->
                Just ("[Dice: " <> dice.diceEmoji <> " " <> Text.pack (show dice.diceValue) <> "]")
            | otherwise -> Nothing
