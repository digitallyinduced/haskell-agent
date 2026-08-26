-- | Pure Telegram update classification and durable pending-action helpers.
module Agent.Telegram.Classify
    ( TelegramUpdateAction(..)
    , storeUpdateAction
    , checkpointPendingVoiceTranscript
    , nextPendingAction
    , ambientGroupPrompt
    , classifyTelegramUpdate
    , classifyTelegramUpdateWithMode
    , groupJoinAuthorized
    , isAnonymousAdmin
    , telegramAnonymousAdminUserId
    , isAmbientGroupPrompt
    , reactionMessageText
    , telegramCommand
    , telegramCommandArguments
    , telegramReactionEmoji
    , telegramReplyText
    , telegramUserLabel
    , telegramReplyUserIdFromPrompt
    , recordSeenTelegramUsers
    , resolveTelegramUser
    , grantableTelegramUser
    , TelegramUserResolution(..)
    ) where

import Agent.Telegram.Types
import Control.Applicative ((<|>))
import Data.Char (isDigit)
import Data.List (nubBy)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, mapMaybe, maybeToList)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Read (readMaybe)

data TelegramUpdateAction
    = IgnoreUpdate
    | QueueTurn !Integer !TelegramChatKey !Text !(Maybe TelegramVoice)
    | QueueMediaTurn !TelegramPendingMediaTurn
    | QueueCallback !TelegramPendingCallback
    | AuthorizeGroupChat !Integer
    | RevokeGroupChat !Integer
    | ReviewGroupJoin !Integer !TelegramUser
    | LeaveUnauthorizedGroup !Integer
    deriving (Eq, Show)

storeUpdateAction
    :: Integer
    -> TelegramUpdateAction
    -> TelegramState
    -> TelegramState
storeUpdateAction updateId action current
    | updateAlreadyStored updateId current =
        advanceOffset current
    | otherwise =
        advanceOffset case action of
            IgnoreUpdate -> current
            ReviewGroupJoin _ _ -> current
            AuthorizeGroupChat chatId ->
                current
                    { authorizedGroupChatIds =
                        Set.insert chatId current.authorizedGroupChatIds
                    }
            RevokeGroupChat chatId ->
                current
                    { authorizedGroupChatIds =
                        Set.delete chatId current.authorizedGroupChatIds
                    }
            LeaveUnauthorizedGroup chatId ->
                enqueuePendingAction
                    (LeaveUnauthorizedChat
                        (TelegramPendingLeave
                            updateId
                            (TelegramChatKey chatId Nothing)))
                    (dropPendingChat chatId current)
                        { authorizedGroupChatIds =
                            Set.delete
                                chatId
                                current.authorizedGroupChatIds
                        }
            QueueTurn messageId key text voice ->
                authorizeGroupKey key $
                    enqueueIncomingAction
                        (RunPendingTurn
                            (TelegramPendingTurn
                                updateId messageId key text voice))
                        current
            QueueMediaTurn pending ->
                let prepared = pending
                        { pendingMediaUpdateId = updateId
                        }
                    replaced =
                        if pending.pendingMediaEdited
                            then removePendingMessage
                                pending.pendingMediaChat
                                pending.pendingMediaMessageId
                                current
                            else current
                in authorizeGroupKey pending.pendingMediaChat $
                    enqueueIncomingAction
                        (RunPendingMediaTurn prepared)
                        replaced
            QueueCallback pending ->
                maybe id authorizeGroupKey pending.pendingCallbackChat $
                    current
                        { pendingCallbacks =
                            Map.insert
                                pending.pendingCallbackUpdateId
                                pending
                                current.pendingCallbacks
                        }
  where
    advanceOffset state =
        state
            { nextUpdateId =
                Just (max (updateId + 1)
                    (fromMaybe 0 state.nextUpdateId))
            }

enqueueIncomingAction :: PendingChatAction -> TelegramState -> TelegramState
enqueueIncomingAction incoming state =
    case previousBatchableAction incoming state of
        Nothing -> enqueuePendingAction incoming state
        Just previous ->
            let merged = mergePendingActions previous incoming
            in enqueuePendingAction
                merged
                (deletePendingAction previous state)

previousBatchableAction
    :: PendingChatAction
    -> TelegramState
    -> Maybe PendingChatAction
previousBatchableAction incoming state = do
    guardBatchable incoming
    queue <- Map.lookup (pendingActionChatLocal incoming) state.pendingQueues
    (_, previous) <- Map.lookupMax queue
    guardBatchable previous
    if pendingActionUpdateIdLocal previous
            < pendingActionUpdateIdLocal incoming
        then Just previous
        else Nothing

guardBatchable :: PendingChatAction -> Maybe ()
guardBatchable = \case
    RunPendingTurn pending
        | pending.pendingTurnVoice == Nothing
        , telegramCommand pending.pendingTurnText == Nothing
        , not ("[Telegram reaction" `Text.isPrefixOf` pending.pendingTurnText) ->
            Just ()
    RunPendingMediaTurn pending
        | telegramCommand pending.pendingMediaText == Nothing ->
            Just ()
    _ -> Nothing

mergePendingActions
    :: PendingChatAction
    -> PendingChatAction
    -> PendingChatAction
mergePendingActions previous incoming =
    let (previousText, previousMedia, previousUser, _, _, previousGroup) =
            pendingActionParts previous
        (incomingText, incomingMedia, incomingUser, incomingMessageId,
            incomingEdited, incomingGroup) =
                pendingActionParts incoming
        mergedText =
            Text.intercalate
                pendingTurnSeparator
                (filter (not . Text.null)
                    [Text.strip previousText, Text.strip incomingText])
        mergedAttachments = previousMedia <> incomingMedia
        mergedUser =
            if incomingUser == 0 then previousUser else incomingUser
        mergedGroup = incomingGroup <|> previousGroup
        chat = pendingActionChatLocal incoming
        updateId = pendingActionUpdateIdLocal incoming
    in if null mergedAttachments && not incomingEdited
        then RunPendingTurn
            TelegramPendingTurn
                { pendingTurnUpdateId = updateId
                , pendingTurnMessageId = incomingMessageId
                , pendingTurnChat = chat
                , pendingTurnText = mergedText
                , pendingTurnVoice = Nothing
                }
        else RunPendingMediaTurn
            TelegramPendingMediaTurn
                { pendingMediaUpdateId = updateId
                , pendingMediaMessageId = incomingMessageId
                , pendingMediaChat = chat
                , pendingMediaUserId = mergedUser
                , pendingMediaText = mergedText
                , pendingMediaAttachments = mergedAttachments
                , pendingMediaEdited = incomingEdited
                , pendingMediaGroupId = mergedGroup
                }

pendingActionParts
    :: PendingChatAction
    -> (Text, [TelegramMedia], Integer, Integer, Bool, Maybe Text)
pendingActionParts = \case
    RunPendingTurn pending ->
        ( pending.pendingTurnText
        , []
        , 0
        , pending.pendingTurnMessageId
        , False
        , Nothing
        )
    RunPendingMediaTurn pending ->
        ( pending.pendingMediaText
        , pending.pendingMediaAttachments
        , pending.pendingMediaUserId
        , pending.pendingMediaMessageId
        , pending.pendingMediaEdited
        , pending.pendingMediaGroupId
        )
    DeliverReply pending ->
        (pending.pendingText, [], 0, fromMaybe 0 pending.pendingReplyToMessageId,
            False, Nothing)
    LeaveUnauthorizedChat _ ->
        ("", [], 0, 0, False, Nothing)

pendingActionUpdateIdLocal :: PendingChatAction -> Integer
pendingActionUpdateIdLocal = \case
    DeliverReply pending -> pending.pendingUpdateId
    RunPendingTurn pending -> pending.pendingTurnUpdateId
    RunPendingMediaTurn pending -> pending.pendingMediaUpdateId
    LeaveUnauthorizedChat pending -> pending.pendingLeaveUpdateId

pendingActionChatLocal :: PendingChatAction -> TelegramChatKey
pendingActionChatLocal = \case
    DeliverReply pending -> pending.pendingChat
    RunPendingTurn pending -> pending.pendingTurnChat
    RunPendingMediaTurn pending -> pending.pendingMediaChat
    LeaveUnauthorizedChat pending -> pending.pendingLeaveChat

removePendingMessage
    :: TelegramChatKey
    -> Integer
    -> TelegramState
    -> TelegramState
removePendingMessage key messageId state =
    state
        { pendingQueues =
            Map.update
                (\queue ->
                    let remaining =
                            Map.filter
                                (not . sameMessage)
                                queue
                    in if Map.null remaining then Nothing else Just remaining)
                key
                state.pendingQueues
        }
  where
    sameMessage = \case
        RunPendingTurn pending ->
            pending.pendingTurnMessageId == messageId
        RunPendingMediaTurn pending ->
            pending.pendingMediaMessageId == messageId
        DeliverReply _ -> False
        LeaveUnauthorizedChat _ -> False

telegramAnonymousAdminUserId :: Integer
telegramAnonymousAdminUserId = 1087968824

isAnonymousAdmin :: TelegramUser -> Bool
isAnonymousAdmin user =
    user.userId == telegramAnonymousAdminUserId
        || user.userUsername == Just "GroupAnonymousBot"

isGroupChatType :: Text -> Bool
isGroupChatType chatType =
    chatType == "group"
        || chatType == "supergroup"
        || chatType == "channel"

chatMemberIsPresent :: TelegramChatMember -> Bool
chatMemberIsPresent member =
    case member.chatMemberStatus of
        "creator" -> True
        "administrator" -> True
        "member" -> True
        "restricted" -> member.chatMemberIsMember == Just True
        _ -> False

chatMemberIsAdministrator :: TelegramChatMember -> Bool
chatMemberIsAdministrator member =
    member.chatMemberStatus == "creator"
        || member.chatMemberStatus == "administrator"

groupJoinAuthorized
    :: Set Integer
    -> TelegramUser
    -> [TelegramChatMember]
    -> Bool
groupJoinAuthorized allowedUsers actor admins =
    let adminIds =
            Set.fromList
                [ member.chatMemberUser.userId
                | member <- admins
                , chatMemberIsAdministrator member
                ]
    in if isAnonymousAdmin actor
        then any (`Set.member` allowedUsers) (Set.toList adminIds)
        else actor.userId `Set.member` allowedUsers
            && actor.userId `Set.member` adminIds

dropPendingChat :: Integer -> TelegramState -> TelegramState
dropPendingChat chatId state =
    state
        { pendingQueues =
            Map.filterWithKey
                (\key _ -> key.chatId /= chatId)
                state.pendingQueues
        }

authorizeGroupKey :: TelegramChatKey -> TelegramState -> TelegramState
authorizeGroupKey key state
    | key.chatId < 0 =
        state
            { authorizedGroupChatIds =
                Set.insert key.chatId state.authorizedGroupChatIds
            }
    | otherwise = state

classifyGroupJoin
    :: Set Integer
    -> Set Integer
    -> Integer
    -> TelegramUser
    -> TelegramUpdateAction
classifyGroupJoin allowedUsers authorizedGroups chatId actor
    | chatId `Set.member` authorizedGroups = IgnoreUpdate
    | actor.userId `Set.member` allowedUsers || isAnonymousAdmin actor =
        ReviewGroupJoin chatId actor
    | otherwise = LeaveUnauthorizedGroup chatId

classifyMyChatMember
    :: Set Integer
    -> Set Integer
    -> TelegramChatMemberUpdated
    -> TelegramUpdateAction
classifyMyChatMember allowedUsers authorizedGroups membership
    | not (isGroupChatType membership.chatMemberUpdatedChat.telegramChatType) =
        IgnoreUpdate
    | chatMemberIsPresent membership.chatMemberUpdatedNew =
        classifyGroupJoin
            allowedUsers
            authorizedGroups
            membership.chatMemberUpdatedChat.telegramChatId
            membership.chatMemberUpdatedFrom
    | chatMemberIsPresent membership.chatMemberUpdatedOld =
        RevokeGroupChat membership.chatMemberUpdatedChat.telegramChatId
    | otherwise = IgnoreUpdate

botAddedInMessage :: TelegramUser -> TelegramMessage -> Bool
botAddedInMessage bot message =
    any (\user -> user.userId == bot.userId) message.messageNewChatMembers

classifyBotAddedMessage
    :: Set Integer
    -> Set Integer
    -> TelegramMessage
    -> TelegramUpdateAction
classifyBotAddedMessage allowedUsers authorizedGroups message
    | not (isGroupChatType message.messageChat.telegramChatType) =
        IgnoreUpdate
    | otherwise =
        case message.messageFrom of
            Just sender ->
                classifyGroupJoin
                    allowedUsers
                    authorizedGroups
                    message.messageChat.telegramChatId
                    sender
            Nothing ->
                LeaveUnauthorizedGroup message.messageChat.telegramChatId

inboundGroupChatId :: TelegramUpdate -> Maybe Integer
inboundGroupChatId update =
    (update.updateMyChatMember >>= chatIdFromChat . (.chatMemberUpdatedChat))
        <|> (update.updateMessage >>= chatIdFromChat . (.messageChat))
        <|> (update.updateEditedMessage >>= chatIdFromChat . (.messageChat))
        <|> (update.updateMessageReaction >>= chatIdFromChat . (.messageReactionChat))
        <|> (update.updateCallbackQuery
            >>= (.callbackQueryMessage)
            >>= chatIdFromChat . (.messageChat))
  where
    chatIdFromChat chat =
        if isGroupChatType chat.telegramChatType
            then Just chat.telegramChatId
            else Nothing

isMembershipAction :: TelegramUpdateAction -> Bool
isMembershipAction = \case
    AuthorizeGroupChat _ -> True
    RevokeGroupChat _ -> True
    ReviewGroupJoin _ _ -> True
    LeaveUnauthorizedGroup _ -> True
    _ -> False

isAuthorizingGroupAction :: TelegramUpdateAction -> Bool
isAuthorizingGroupAction = \case
    QueueTurn _ _ text _ -> not (isAmbientGroupPrompt text)
    QueueMediaTurn pending ->
        not (isAmbientGroupPrompt pending.pendingMediaText)
    QueueCallback _ -> True
    _ -> False

enforceGroupAuthorization
    :: Set Integer
    -> TelegramUpdate
    -> TelegramUpdateAction
    -> TelegramUpdateAction
enforceGroupAuthorization authorizedGroups update action
    | isMembershipAction action = action
    | Just chatId <- inboundGroupChatId update
    , chatId `Set.notMember` authorizedGroups
    , not (isAuthorizingGroupAction action)
        || isJust update.updateMessageReaction =
        LeaveUnauthorizedGroup chatId
    | otherwise = action

classifyTelegramUpdate
    :: TelegramUser
    -> Set Integer
    -> TelegramUpdate
    -> TelegramUpdateAction
classifyTelegramUpdate bot allowedUsers =
    classifyTelegramUpdateWithMode bot allowedUsers Set.empty False

setActionUpdateId :: Integer -> TelegramUpdateAction -> TelegramUpdateAction
setActionUpdateId updateId = \case
    QueueMediaTurn pending ->
        QueueMediaTurn pending { pendingMediaUpdateId = updateId }
    action -> action

classifyTelegramReaction
    :: Set Integer
    -> TelegramUpdate
    -> TelegramUpdateAction
classifyTelegramReaction allowedUsers update =
    case update.updateMessageReaction of
        Just reaction
            | Just sender <- reaction.messageReactionUser
            , sender.userId `Set.member` allowedUsers ->
                QueueTurn
                    reaction.messageReactionMessageId
                    TelegramChatKey
                        { chatId = reaction.messageReactionChat.telegramChatId
                        , messageThreadId = Nothing
                        }
                    (reactionMessageText reaction)
                    Nothing
        _ -> IgnoreUpdate

classifyTelegramUpdateWithMode
    :: TelegramUser
    -> Set Integer
    -> Set Integer
    -> Bool
    -> TelegramUpdate
    -> TelegramUpdateAction
classifyTelegramUpdateWithMode bot allowedUsers authorizedGroups respondToAllGroupMessages update =
    enforceGroupAuthorization authorizedGroups update $
        case update of
            TelegramUpdate { updateMyChatMember = Just membership } ->
                classifyMyChatMember allowedUsers authorizedGroups membership
            TelegramUpdate { updateMessage = Just message }
                | botAddedInMessage bot message ->
                    classifyBotAddedMessage allowedUsers authorizedGroups message
            TelegramUpdate { updateMessage = Just message }
                | Just sender <- message.messageFrom
                , sender.userId `Set.member` allowedUsers ->
                    setActionUpdateId update.updateId
                        (classifyMessageLike
                            False
                            bot
                            sender
                            respondToAllGroupMessages
                            message)
            TelegramUpdate { updateEditedMessage = Just message }
                | Just sender <- message.messageFrom
                , sender.userId `Set.member` allowedUsers ->
                    setActionUpdateId update.updateId
                        (classifyMessageLike
                            True
                            bot
                            sender
                            respondToAllGroupMessages
                            message)
            TelegramUpdate { updateMessageReaction = Just _ } ->
                classifyTelegramReaction allowedUsers update
            TelegramUpdate { updateCallbackQuery = Just callback }
                | sender <- callback.callbackQueryFrom
                , sender.userId `Set.member` allowedUsers
                , Just callbackData <- callback.callbackQueryData ->
                    QueueCallback TelegramPendingCallback
                        { pendingCallbackUpdateId = update.updateId
                        , pendingCallbackQueryId = callback.callbackQueryId
                        , pendingCallbackUserId = sender.userId
                        , pendingCallbackChat =
                            (\message -> TelegramChatKey
                                { chatId = message.messageChat.telegramChatId
                                , messageThreadId = message.messageThread
                                })
                                <$> callback.callbackQueryMessage
                        , pendingCallbackMessageId =
                            (.messageId) <$> callback.callbackQueryMessage
                        , pendingCallbackData = callbackData
                        }
            _ -> IgnoreUpdate

classifyMessageLike
    :: Bool
    -> TelegramUser
    -> TelegramUser
    -> Bool
    -> TelegramMessage
    -> TelegramUpdateAction
classifyMessageLike edited bot sender respondToAllGroupMessages message =
    case message.messageChat.telegramChatType of
        "private" -> classifyPrivateMessage
        "group" -> classifyGroupMessage
        "supergroup" -> classifyGroupMessage
        _ -> IgnoreUpdate
  where
    key = TelegramChatKey
        { chatId = message.messageChat.telegramChatId
        , messageThreadId = message.messageThread
        }

    classifyPrivateMessage
        | Just voice <- message.messageVoice =
            queueVoice "[Voice message]" voice
        | hasTelegramMedia message =
            queueMedia
                (fromMaybe
                    (if edited then "[Edited Telegram media]" else "[Telegram media]")
                    (messageContentText message))
        | otherwise = queueMessage id

    queueVoice text voice =
        QueueTurn message.messageId key text (Just voice)

    queueMedia promptText =
        QueueMediaTurn
            TelegramPendingMediaTurn
                { pendingMediaUpdateId = message.messageId
                , pendingMediaMessageId = message.messageId
                , pendingMediaChat = key
                , pendingMediaUserId = sender.userId
                , pendingMediaText =
                    messageContextPrefix bot message <> promptText
                , pendingMediaAttachments = messageMediaAttachments message
                , pendingMediaEdited = edited
                , pendingMediaGroupId = message.messageMediaGroupId
                }

    classifyGroupMessage
        | Just rawText <- messageContentText message
        , Just target <- explicitCommandTarget (Text.strip rawText)
        , not (botUsernameMatches bot target) =
            IgnoreUpdate
        | Just rawText <- messageContentText message
        , telegramCommand rawText /= Nothing =
            if hasTelegramMedia message
                then queueMedia (attributeGroupText sender (Text.strip rawText))
                else queueText (attributeGroupText sender (Text.strip rawText))
        | messageRepliesToBot bot message =
            queueGroupReply
        | Just rawText <- messageContentText message
        , Just targetedText <- groupTextForBot bot rawText =
            if hasTelegramMedia message
                then queueMedia (attributeGroupText sender targetedText)
                else queueText (attributeGroupText sender targetedText)
        | respondToAllGroupMessages =
            queueAmbientGroupMessage
        | otherwise = IgnoreUpdate

    queueGroupReply =
        case message.messageVoice of
            Just voice ->
                QueueTurn message.messageId key
                    (messageContextPrefix bot message
                        <> attributeGroupMessage sender "[Voice message]")
                    (Just voice)
            Nothing
                | hasTelegramMedia message ->
                    queueMedia
                        (attributeGroupMessage sender
                            (fromMaybe
                                (if edited then "[Edited Telegram media]" else "[Telegram media]")
                                (messageContentText message)))
            Nothing
                | Just rawText <- messageContentText message ->
                    queueText (attributeGroupText sender rawText)
            Nothing -> IgnoreUpdate

    queueAmbientGroupMessage =
        case message.messageVoice of
            Just voice ->
                QueueTurn message.messageId key
                    (ambientGroupPrompt
                        (attributeGroupMessage sender "[Voice message]"))
                    (Just voice)
            Nothing
                | hasTelegramMedia message ->
                    queueMedia
                        (ambientGroupPrompt
                            (attributeGroupMessage sender
                                (fromMaybe
                                    (if edited
                                        then "[Edited Telegram media]"
                                        else "[Telegram media]")
                                    (messageContentText message))))
            Nothing
                | Just rawText <- messageContentText message ->
                    queueText
                        (ambientGroupPrompt
                            (attributeGroupText sender rawText))
            Nothing -> IgnoreUpdate

    queueMessage transform =
        case message.messageVoice of
            Just voice ->
                QueueTurn message.messageId key
                    (transform "[Voice message]")
                    (Just voice)
            Nothing
                | hasTelegramMedia message ->
                    queueMedia
                        (transform
                            (fromMaybe
                                (if edited then "[Edited Telegram media]" else "[Telegram media]")
                                (messageContentText message)))
            Nothing
                | Just rawText <- messageContentText message ->
                    queueText (transform rawText)
            Nothing -> IgnoreUpdate

    queueText rawText
        | Text.null clean = IgnoreUpdate
        | edited =
            QueueMediaTurn TelegramPendingMediaTurn
                { pendingMediaUpdateId = message.messageId
                , pendingMediaMessageId = message.messageId
                , pendingMediaChat = key
                , pendingMediaUserId = sender.userId
                , pendingMediaText = clean
                , pendingMediaAttachments = []
                , pendingMediaEdited = True
                , pendingMediaGroupId = message.messageMediaGroupId
                }
        | otherwise =
            QueueTurn message.messageId key clean Nothing
      where
        rewritten = rewriteGrantCommand message rawText
        clean
            | telegramCommand rewritten /= Nothing = Text.strip rewritten
            | otherwise =
                Text.strip (messageContextPrefix bot message <> rewritten)

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
            | Just _ <- message.messageVoice ->
                Just "[Voice message]"
            | Just audio <- message.messageAudio ->
                Just ("[Audio file: " <> fromMaybe "audio" audio.audioFileName <> "]")
            | Just document <- message.messageDocument ->
                Just ("[Document: " <> fromMaybe document.documentFileId document.documentFileName <> "]")
            | not (null message.messagePhoto) ->
                Just "[Photo]"
            | Just video <- message.messageVideo ->
                Just ("[Video file: " <> fromMaybe "video" video.videoFileName <> "]")
            | Just _ <- message.messageVideoNote ->
                Just "[Video note]"
            | Just _ <- message.messageAnimation ->
                Just "[Animation]"
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

messageContextPrefix :: TelegramUser -> TelegramMessage -> Text
messageContextPrefix bot message =
    forwarded <> replied <> mentioned
  where
    forwarded =
        case message.messageForwardOrigin of
            Nothing -> ""
            Just _ -> "[Forwarded Telegram message]\n"
    replied =
        case message.messageReplyTo of
            Just replyMessage ->
                "[Replying to Telegram message "
                    <> Text.pack (show replyMessage.messageId)
                    <> maybe
                        ""
                        (\user -> " from " <> telegramUserLabel user)
                        replyMessage.messageFrom
                    <> maybe
                        ""
                        (\content ->
                            ": "
                                <> Text.take 1000
                                    (Text.replace "\n" " " content))
                        (messageContentText replyMessage)
                    <> "]\n"
            Nothing -> ""
    mentioned =
        case mentionLabels bot message of
            [] -> ""
            labels ->
                "[Telegram mentions: "
                    <> Text.intercalate "; " labels
                    <> "]\n"

messageRepliesToBot :: TelegramUser -> TelegramMessage -> Bool
messageRepliesToBot bot message =
    case message.messageReplyTo >>= (.messageFrom) of
        Just repliedTo -> repliedTo.userId == bot.userId
        Nothing -> False

groupTextForBot :: TelegramUser -> Text -> Maybe Text
groupTextForBot bot rawText =
    case explicitCommandTarget clean of
        Just target
            | usernameMatches target -> Just clean
            | otherwise -> Nothing
        Nothing -> stripBotMention bot clean
  where
    clean = Text.strip rawText
    usernameMatches = botUsernameMatches bot

botUsernameMatches :: TelegramUser -> Text -> Bool
botUsernameMatches bot target =
    maybe False
        ((== Text.toCaseFold target) . Text.toCaseFold)
        bot.userUsername

explicitCommandTarget :: Text -> Maybe Text
explicitCommandTarget text = do
    firstWord <- case Text.words text of
        [] -> Nothing
        value : _ -> Just value
    command <- Text.stripPrefix "/" firstWord
    let (_, targetWithAt) = Text.breakOn "@" command
    Text.stripPrefix "@" targetWithAt

stripBotMention :: TelegramUser -> Text -> Maybe Text
stripBotMention bot text = do
    username <- bot.userUsername
    let needle = "@" <> Text.map asciiLower username
        folded = Text.map asciiLower text
        (beforeFolded, matchAndAfter) = Text.breakOn needle folded
    if Text.null matchAndAfter
        then Nothing
        else do
            let mentionOffset = Text.length beforeFolded
                (before, mentionAndAfter) = Text.splitAt mentionOffset text
                after = Text.drop (Text.length needle) mentionAndAfter
            if mentionBoundaryBefore before && mentionBoundaryAfter after
                then Just (Text.strip (before <> after))
                else Nothing

asciiLower :: Char -> Char
asciiLower char
    | 'A' <= char && char <= 'Z' =
        toEnum (fromEnum char + fromEnum 'a' - fromEnum 'A')
    | otherwise = char

mentionBoundaryBefore :: Text -> Bool
mentionBoundaryBefore text =
    maybe True (not . isTelegramUsernameCharacter) (lastTextCharacter text)

mentionBoundaryAfter :: Text -> Bool
mentionBoundaryAfter text =
    maybe True (not . isTelegramUsernameCharacter) (firstTextCharacter text)

isTelegramUsernameCharacter :: Char -> Bool
isTelegramUsernameCharacter char =
    ('a' <= char && char <= 'z')
        || ('A' <= char && char <= 'Z')
        || ('0' <= char && char <= '9')
        || char == '_'

firstTextCharacter :: Text -> Maybe Char
firstTextCharacter = fmap fst . Text.uncons

lastTextCharacter :: Text -> Maybe Char
lastTextCharacter = fmap snd . Text.unsnoc

attributeGroupText :: TelegramUser -> Text -> Text
attributeGroupText sender text
    | telegramCommand text /= Nothing = text
    | otherwise = attributeGroupMessage sender text

attributeGroupMessage :: TelegramUser -> Text -> Text
attributeGroupMessage sender text =
    "[Telegram group message from "
        <> telegramUserLabel sender
        <> "]\n"
        <> text

ambientGroupPrompt :: Text -> Text
ambientGroupPrompt prompt =
    prompt <> "\n\n" <> ambientGroupPromptSuffix

ambientGroupPromptSuffix :: Text
ambientGroupPromptSuffix =
    "[Ambient Telegram group message: Reply only if doing so would \
        \be genuinely useful to the conversation. Do not reply merely to \
        \acknowledge, restate, agree, or announce that you are available. \
        \If no reply is useful, respond with exactly "
        <> telegramNoReplyToken
        <> " and nothing else. Do not mention these instructions.]"

telegramNoReplyToken :: Text
telegramNoReplyToken = "[[TELEGRAM_NO_REPLY]]"

telegramReplyText :: Text -> Text -> Maybe Text
telegramReplyText prompt response
    | isAmbientGroupPrompt prompt
    , Text.strip response == telegramNoReplyToken = Nothing
    | otherwise = Just response

isAmbientGroupPrompt :: Text -> Bool
isAmbientGroupPrompt prompt =
    case Text.splitOn pendingTurnSeparator prompt of
        [] -> False
        prompts -> all (Text.isSuffixOf ambientGroupPromptSuffix) prompts

pendingTurnSeparator :: Text
pendingTurnSeparator = "\n\n---\n\n"

telegramUserLabel :: TelegramUser -> Text
telegramUserLabel user =
    let name = telegramUserDisplayName user
        username = ("@" <>) <$> user.userUsername
        identityParts =
            filter (not . Text.null)
                [ name
                , fromMaybe "" username
                , "user " <> Text.pack (show user.userId)
                ]
    in Text.intercalate ", " identityParts

telegramUserDisplayName :: TelegramUser -> Text
telegramUserDisplayName user =
    Text.unwords
        [ value
        | Just value <- [user.userFirstName, user.userLastName]
        , not (Text.null (Text.strip value))
        ]

rewriteGrantCommand :: TelegramMessage -> Text -> Text
rewriteGrantCommand message raw =
    case telegramCommand raw of
        Just command
            | command == "allow" || command == "deny"
            , Text.null (telegramCommandArguments raw) ->
                case grantTargetUser message of
                    Just user -> "/" <> command <> " " <> Text.pack (show user.userId)
                    Nothing -> raw
        _ -> raw

grantTargetUser :: TelegramMessage -> Maybe TelegramUser
grantTargetUser message =
    case message.messageReplyTo >>= (.messageFrom) of
        Just user | grantableTelegramUser user -> Just user
        _ ->
            case filter grantableTelegramUser (mentionedTelegramUsers message) of
                [user] -> Just user
                _ -> Nothing

mentionLabels :: TelegramUser -> TelegramMessage -> [Text]
mentionLabels bot message =
    filter (not . Text.null)
        [ label
        | label <- mapMaybe (mentionLabel bot) (messageMentionEntities message)
        ]

mentionLabel :: TelegramUser -> (Text, Maybe TelegramUser) -> Maybe Text
mentionLabel bot (snippet, mentionedUser)
    | Just user <- mentionedUser
    , user.userId == bot.userId =
        Nothing
    | Just user <- mentionedUser =
        Just (telegramUserLabel user)
    | botUsernameMatches bot mentionName =
        Nothing
    | Text.null mentionName =
        Nothing
    | otherwise =
        Just (Text.strip snippet)
  where
    mentionName =
        Text.strip (Text.dropWhile (== '@') (Text.strip snippet))

messageMentionEntities :: TelegramMessage -> [(Text, Maybe TelegramUser)]
messageMentionEntities message =
    let text = fromMaybe "" (message.messageText <|> message.messageCaption)
        entities = message.messageEntities <> message.messageCaptionEntities
    in
        [ (utf16Slice entity.entityOffset entity.entityLength text, entity.entityUser)
        | entity <- entities
        , entity.entityType == "mention" || entity.entityType == "text_mention"
        ]

mentionedTelegramUsers :: TelegramMessage -> [TelegramUser]
mentionedTelegramUsers message =
    nubBy (\left right -> left.userId == right.userId) $
        mapMaybe snd (messageMentionEntities message)

utf16Slice :: Int -> Int -> Text -> Text
utf16Slice offset length text =
    utf16Take length (utf16Drop offset text)

utf16Drop :: Int -> Text -> Text
utf16Drop n text
    | n <= 0 = text
    | otherwise = case Text.uncons text of
        Nothing -> ""
        Just (char, rest)
            | fromEnum char >= 0x10000 -> utf16Drop (n - 2) rest
            | otherwise -> utf16Drop (n - 1) rest

utf16Take :: Int -> Text -> Text
utf16Take n text
    | n <= 0 = ""
    | otherwise = case Text.uncons text of
        Nothing -> ""
        Just (char, rest)
            | fromEnum char >= 0x10000 ->
                if n >= 2
                    then Text.cons char (utf16Take (n - 2) rest)
                    else ""
            | otherwise -> Text.cons char (utf16Take (n - 1) rest)

grantableTelegramUser :: TelegramUser -> Bool
grantableTelegramUser user =
    not user.userIsBot
        && not (isAnonymousAdmin user)
        && user.userId > 0

stubTelegramUser :: Integer -> TelegramUser
stubTelegramUser userId =
    TelegramUser
        { userId
        , userIsBot = False
        , userFirstName = Nothing
        , userLastName = Nothing
        , userUsername = Nothing
        }

data TelegramUserResolution
    = ResolvedTelegramUser TelegramUser
    | AmbiguousTelegramUsers [TelegramUser]
    | UnresolvedTelegramUser Text
    deriving (Eq, Show)

resolveTelegramUser
    :: TelegramState
    -> Integer
    -> Maybe Integer
    -> Text
    -> TelegramUserResolution
resolveTelegramUser state chatId fallbackUserId rawQuery
    | Text.null query =
        case fallbackUserId of
            Just userId -> resolveById userId
            Nothing ->
                UnresolvedTelegramUser
                    "Reply to their message, mention them, or pass a name, \
                    \@username, or numeric Telegram user id."
    | Just userId <- parsePositiveUserId query = resolveById userId
    | otherwise =
        uniqueMatches
            (preferChatLocal
                (filter (userMatchesQuery query) candidates))
  where
    query = normalizeUserQuery rawQuery
    candidates =
        nubBy (\left right -> left.userId == right.userId)
            (Map.elems state.seenTelegramUsers)

    resolveById userId =
        case Map.lookup userId state.seenTelegramUsers of
            Just user -> grantResolved user
            Nothing -> grantResolved (stubTelegramUser userId)

    preferChatLocal matches =
        let local =
                filter
                    (\user ->
                        maybe
                            False
                            (Set.member user.userId)
                            (Map.lookup chatId state.seenUsersByChat))
                    matches
        in if null local then matches else local

    uniqueMatches = \case
        [user] -> grantResolved user
        [] ->
            UnresolvedTelegramUser
                ("I have not seen anyone matching "
                    <> rawQuery
                    <> " in this chat yet. Reply to one of their messages \
                       \with /allow, mention them, or have them send a \
                       \message here first.")
        users -> AmbiguousTelegramUsers users

    grantResolved user
        | grantableTelegramUser user = ResolvedTelegramUser user
        | otherwise =
            UnresolvedTelegramUser
                "That Telegram account cannot be added to the allowlist."

normalizeUserQuery :: Text -> Text
normalizeUserQuery raw =
    let stripped = Text.strip raw
        withoutAt = fromMaybe stripped (Text.stripPrefix "@" stripped)
        folded = Text.toCaseFold withoutAt
    in case Text.stripPrefix "user " folded of
        Just rest | not (Text.null rest) && Text.all isDigit rest -> rest
        _ -> folded

parsePositiveUserId :: Text -> Maybe Integer
parsePositiveUserId text = do
    userId <- readMaybe (Text.unpack text)
    if userId > 0 then Just userId else Nothing

userMatchesQuery :: Text -> TelegramUser -> Bool
userMatchesQuery query user =
    query == Text.pack (show user.userId)
        || maybe False ((== query) . Text.toCaseFold) user.userUsername
        || maybe False ((== query) . Text.toCaseFold . Text.strip) user.userFirstName
        || maybe False ((== query) . Text.toCaseFold . Text.strip) user.userLastName
        || (let name = Text.toCaseFold (telegramUserDisplayName user)
            in not (Text.null name) && name == query)

recordSeenTelegramUsers :: TelegramUpdate -> TelegramState -> TelegramState
recordSeenTelegramUsers update state =
    foldl' recordOne state (telegramObservedUsers update)
  where
    recordOne current (chatId, user) =
        current
            { seenTelegramUsers =
                Map.insert user.userId user current.seenTelegramUsers
            , seenUsersByChat =
                Map.insertWith
                    Set.union
                    chatId
                    (Set.singleton user.userId)
                    current.seenUsersByChat
            }

telegramObservedUsers :: TelegramUpdate -> [(Integer, TelegramUser)]
telegramObservedUsers update =
    nubBy
        (\(leftChat, leftUser) (rightChat, rightUser) ->
            leftChat == rightChat && leftUser.userId == rightUser.userId)
        (maybe [] usersFromMessage update.updateMessage
            <> maybe [] usersFromMessage update.updateEditedMessage
            <> maybe [] usersFromReaction update.updateMessageReaction
            <> maybe [] usersFromCallback update.updateCallbackQuery
            <> maybe [] usersFromMembership update.updateMyChatMember)

usersFromMessage :: TelegramMessage -> [(Integer, TelegramUser)]
usersFromMessage message =
    let chatId = message.messageChat.telegramChatId
        people =
            maybeToList message.messageFrom
                <> maybeToList (message.messageReplyTo >>= (.messageFrom))
                <> message.messageNewChatMembers
                <> mentionedTelegramUsers message
    in map (\user -> (chatId, user)) people

usersFromReaction :: TelegramMessageReaction -> [(Integer, TelegramUser)]
usersFromReaction reaction =
    map
        (\user -> (reaction.messageReactionChat.telegramChatId, user))
        (maybeToList reaction.messageReactionUser)

usersFromCallback :: TelegramCallbackQuery -> [(Integer, TelegramUser)]
usersFromCallback callback =
    (maybe
        []
        (\message -> [(message.messageChat.telegramChatId, callback.callbackQueryFrom)])
        callback.callbackQueryMessage)
        <> maybe [] usersFromMessage callback.callbackQueryMessage

usersFromMembership :: TelegramChatMemberUpdated -> [(Integer, TelegramUser)]
usersFromMembership membership =
    let chatId = membership.chatMemberUpdatedChat.telegramChatId
    in map
        (\user -> (chatId, user))
        [ membership.chatMemberUpdatedFrom
        , membership.chatMemberUpdatedOld.chatMemberUser
        , membership.chatMemberUpdatedNew.chatMemberUser
        ]

reactionMessageText :: TelegramMessageReaction -> Text
reactionMessageText reaction
    | null reaction.messageReactionNew =
        "[Telegram reaction removed from message "
            <> Text.pack (show reaction.messageReactionMessageId)
            <> "]"
    | otherwise =
        "[Telegram reaction on message "
            <> Text.pack (show reaction.messageReactionMessageId)
            <> "]: "
            <> Text.intercalate " "
                (map renderReaction reaction.messageReactionNew)
  where
    renderReaction value = case
        (value.reactionEmoji, value.reactionCustomEmojiId) of
            (Just emoji, _) -> emoji
            (_, Just customId) -> "custom-emoji:" <> customId
            _ -> value.reactionType

updateAlreadyStored :: Integer -> TelegramState -> Bool
updateAlreadyStored updateId state =
    maybe False (> updateId) state.nextUpdateId
        || any (Map.member updateId) state.pendingQueues
        || Map.member updateId state.pendingCallbacks

telegramCommand :: Text -> Maybe Text
telegramCommand text = do
    firstWord <- case Text.words text of
        [] -> Nothing
        value : _ -> Just value
    withoutSlash <- Text.stripPrefix "/" firstWord
    pure (Text.toLower (Text.takeWhile (/= '@') withoutSlash))

telegramCommandArguments :: Text -> Text
telegramCommandArguments text =
    case Text.words (Text.strip text) of
        _command : rest -> Text.unwords rest
        [] -> ""

telegramReplyUserIdFromPrompt :: Text -> Maybe Integer
telegramReplyUserIdFromPrompt prompt = do
    rest <- Text.stripPrefix
        "[Replying to Telegram message "
        (Text.takeWhile (/= '\n') (Text.stripStart prompt))
    let afterUser = snd (Text.breakOn ", user " rest)
    digits <- Text.stripPrefix ", user " afterUser
    parsePositiveUserId (Text.takeWhile isDigit digits)

checkpointPendingVoiceTranscript
    :: Integer
    -> Text
    -> TelegramState
    -> TelegramState
checkpointPendingVoiceTranscript updateId transcript state =
    state
        { pendingQueues =
            fmap (fmap checkpoint) state.pendingQueues
        }
  where
    checkpoint = \case
        RunPendingTurn current
            | current.pendingTurnUpdateId == updateId ->
                RunPendingTurn current
                    { pendingTurnText = transcript
                    , pendingTurnVoice = Nothing
                    }
        current -> current
nextPendingAction
    :: TelegramChatKey
    -> TelegramState
    -> Maybe PendingChatAction
nextPendingAction key state =
    snd <$> (Map.lookupMin =<< Map.lookup key state.pendingQueues)
telegramReactionEmoji :: Text -> Maybe Text
telegramReactionEmoji text =
    let candidate = Text.filter (/= '\xFE0F') (Text.strip text)
        normalized = if candidate == "♥" then "❤" else candidate
    in if normalized `Set.member` supportedTelegramReactions
        then Just normalized
        else Nothing

supportedTelegramReactions :: Set Text
supportedTelegramReactions = Set.fromList
    [ "👍", "👎", "❤", "🔥", "🥰", "👏", "😁", "🤔", "🤯", "😱"
    , "🤬", "😢", "🎉", "🤩", "🤮", "💩", "🙏", "👌", "🕊", "🤡"
    , "🥱", "🥴", "😍", "🐳", "🌚", "🌭", "💯", "🤣", "⚡"
    , "🍌", "🏆", "💔", "🤨", "😐", "🍓", "🍾", "💋", "🖕", "😈"
    , "😴", "😭", "🤓", "👻", "👀", "🎃", "🙈", "😇", "😨"
    , "🤝", "✍", "🤗", "🫡", "🎅", "🎄", "☃", "💅", "🤪", "🗿"
    , "🆒", "💘", "🙉", "🦄", "😘", "💊", "🙊", "😎", "👾", "🤷"
    , "😡"
    ]

