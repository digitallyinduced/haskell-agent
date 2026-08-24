{-# OPTIONS_GHC -Wno-orphans #-}

-- | Telegram wire types and durable queue state.
module Agent.Telegram.Types
    ( TelegramConfig(..)
    , TelegramApprovalMode(..)
    , TelegramSetupOptions(..)
    , defaultTelegramSetupOptions
    , TelegramCommand(..)
    , TelegramUsersCommand(..)
    , TelegramChatKey(..)
    , TelegramBinding(..)
    , TelegramState(..)
    , TelegramPendingTurn(..)
    , TelegramPendingReply(..)
    , TelegramPendingMediaTurn(..)
    , TelegramPendingCallback(..)
    , TelegramRetryMetadata(..)
    , TelegramDeadLetter(..)
    , TelegramCallbackBinding(..)
    , TelegramVoice(..)
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
    , TelegramMessage(..)
    , TelegramReactionType(..)
    , TelegramMessageReaction(..)
    , TelegramCallbackQuery(..)
    , TelegramUpdate(..)
    , TelegramResponseParameters(..)
    , TelegramResponse(..)
    , TelegramClient(..)
    , PendingChatAction(..)
    , emptyTelegramState
    , insertPendingAction
    , enqueuePendingAction
    , deletePendingAction
    ) where

import Agent.Provider (Provider, parseProvider, providerSlug)
import Data.Aeson
    ( FromJSON(..)
    , ToJSON(..)
    , object
    , withObject
    , (.:)
    , (.:?)
    , (.!=)
    , (.=)
    )
import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime)
import qualified Network.HTTP.Client as Http

data TelegramApprovalMode
    = TelegramApprovalPrompt
    | TelegramApprovalDeny
    | TelegramApprovalYolo
    deriving (Eq, Show)

instance ToJSON PendingChatAction where
    toJSON = \case
        DeliverReply pending -> object
            [ "kind" .= ("reply" :: Text)
            , "reply" .= pending
            ]
        RunPendingTurn pending -> object
            [ "kind" .= ("turn" :: Text)
            , "turn" .= pending
            ]
        RunPendingMediaTurn pending -> object
            [ "kind" .= ("media_turn" :: Text)
            , "mediaTurn" .= pending
            ]

instance FromJSON PendingChatAction where
    parseJSON = withObject "PendingChatAction" \o -> do
        kind <- o .: "kind"
        case (kind :: Text) of
            "reply" -> DeliverReply <$> o .: "reply"
            "turn" -> RunPendingTurn <$> o .: "turn"
            "media_turn" -> RunPendingMediaTurn <$> o .: "mediaTurn"
            _ -> fail ("unknown pending Telegram action: " <> Text.unpack kind)

instance ToJSON TelegramApprovalMode where
    toJSON = \case
        TelegramApprovalPrompt -> "prompt"
        TelegramApprovalDeny -> "deny"
        TelegramApprovalYolo -> "yolo"

instance FromJSON TelegramApprovalMode where
    parseJSON = Aeson.withText "TelegramApprovalMode" \case
        "prompt" -> pure TelegramApprovalPrompt
        "deny" -> pure TelegramApprovalDeny
        "yolo" -> pure TelegramApprovalYolo
        other -> fail ("unknown Telegram approval mode: " <> Text.unpack other)

data TelegramConfig = TelegramConfig
    { telegramProvider :: !Provider
    , telegramModel :: !(Maybe Text)
    , telegramCwd :: !FilePath
    , telegramEffort :: !(Maybe Text)
    , telegramApprovalMode :: !TelegramApprovalMode
    , telegramAllowedUsers :: !(Set Integer)
    , telegramRespondToAllGroupMessages :: !Bool
    } deriving (Eq, Show)

instance ToJSON TelegramConfig where
    toJSON config = object
        [ "provider" .= providerSlug config.telegramProvider
        , "model" .= config.telegramModel
        , "cwd" .= config.telegramCwd
        , "effort" .= config.telegramEffort
        , "approvalMode" .= config.telegramApprovalMode
        , "allowedUsers" .= Set.toList config.telegramAllowedUsers
        , "respondToAllGroupMessages"
            .= config.telegramRespondToAllGroupMessages
        ]

instance FromJSON TelegramConfig where
    parseJSON = withObject "TelegramConfig" \o -> do
        providerText <- o .: "provider"
        telegramProvider <- maybe
            (fail ("unknown provider: " <> Text.unpack providerText))
            pure
            (parseProvider providerText)
        legacyYolo <- o .:? "yolo" .!= False
        approvalMode <- o .:? "approvalMode" .!=
            (if legacyYolo
                then TelegramApprovalYolo
                else TelegramApprovalPrompt)
        TelegramConfig
            <$> pure telegramProvider
            <*> o .:? "model"
            <*> o .: "cwd"
            <*> o .:? "effort"
            <*> pure approvalMode
            <*> (Set.fromList <$> o .: "allowedUsers")
            <*> (o .:? "respondToAllGroupMessages" .!= False)

data TelegramSetupOptions = TelegramSetupOptions
    { setupProvider :: !(Maybe Provider)
    , setupModel :: !(Maybe Text)
    , setupCwd :: !(Maybe FilePath)
    , setupEffort :: !(Maybe Text)
    , setupApprovalMode :: !TelegramApprovalMode
    , setupAllowedUsers :: ![Integer]
    , setupRespondToAllGroupMessages :: !Bool
    , setupStart :: !Bool
    } deriving (Eq, Show)

defaultTelegramSetupOptions :: TelegramSetupOptions
defaultTelegramSetupOptions = TelegramSetupOptions
    { setupProvider = Nothing
    , setupModel = Nothing
    , setupCwd = Nothing
    , setupEffort = Nothing
    , setupApprovalMode = TelegramApprovalPrompt
    , setupAllowedUsers = []
    , setupRespondToAllGroupMessages = False
    , setupStart = False
    }

data TelegramCommand
    = TelegramSetup !TelegramSetupOptions
    | TelegramRun
    | TelegramStart
    | TelegramStop
    | TelegramStatus
    | TelegramUsers !TelegramUsersCommand
    | TelegramHelp
    | TelegramVersion
    deriving (Eq, Show)

data TelegramUsersCommand
    = TelegramUsersList
    | TelegramUsersAdd !Integer
    | TelegramUsersRemove !Integer
    deriving (Eq, Show)

data TelegramChatKey = TelegramChatKey
    { chatId :: !Integer
    , messageThreadId :: !(Maybe Integer)
    } deriving (Eq, Ord, Show)

instance ToJSON TelegramChatKey where
    toJSON key = object
        [ "chatId" .= key.chatId
        , "messageThreadId" .= key.messageThreadId
        ]

instance FromJSON TelegramChatKey where
    parseJSON = withObject "TelegramChatKey" \o ->
        TelegramChatKey <$> o .: "chatId" <*> o .:? "messageThreadId"

data TelegramBinding = TelegramBinding
    { bindingChat :: !TelegramChatKey
    , bindingSessionId :: !Text
    } deriving (Eq, Show)

instance ToJSON TelegramBinding where
    toJSON binding = object
        [ "chat" .= binding.bindingChat
        , "sessionId" .= binding.bindingSessionId
        ]

instance FromJSON TelegramBinding where
    parseJSON = withObject "TelegramBinding" \o ->
        TelegramBinding <$> o .: "chat" <*> o .: "sessionId"

data TelegramState = TelegramState
    { telegramStateVersion :: !Int
    , nextUpdateId :: !(Maybe Integer)
    , bindings :: !(Map TelegramChatKey Text)
    , pendingQueues :: !(Map TelegramChatKey (Map Integer PendingChatAction))
    , pendingCallbacks :: !(Map Integer TelegramPendingCallback)
    , callbackBindings :: !(Map Text TelegramCallbackBinding)
    , retryMetadata :: !(Map Text TelegramRetryMetadata)
    , deliveryCheckpoints :: !(Map Text Int)
    , deadLetters :: ![TelegramDeadLetter]
    , outboundMessageIds :: !(Map TelegramChatKey (Set Integer))
    } deriving (Eq, Show)

instance ToJSON TelegramState where
    toJSON state = object
        [ "version" .= state.telegramStateVersion
        , "nextUpdateId" .= state.nextUpdateId
        , "bindings" .=
            [ TelegramBinding key sessionId
            | (key, sessionId) <- Map.toList state.bindings
            ]
        , "pendingTurns" .=
            sortOn (.pendingTurnUpdateId)
                [ pending
                | queue <- Map.elems state.pendingQueues
                , RunPendingTurn pending <- Map.elems queue
                ]
        , "pendingReplies" .=
            sortOn (.pendingUpdateId)
                [ pending
                | queue <- Map.elems state.pendingQueues
                , DeliverReply pending <- Map.elems queue
                ]
        , "pendingMediaTurns" .=
            sortOn (.pendingMediaUpdateId)
                [ pending
                | queue <- Map.elems state.pendingQueues
                , RunPendingMediaTurn pending <- Map.elems queue
                ]
        , "pendingCallbacks" .= Map.elems state.pendingCallbacks
        , "callbackBindings" .= Map.elems state.callbackBindings
        , "retryMetadata" .= Map.toList state.retryMetadata
        , "deliveryCheckpoints" .= Map.toList state.deliveryCheckpoints
        , "deadLetters" .= state.deadLetters
        , "outboundMessages" .=
            [ TelegramOutboundMessages key (Set.toList messageIds)
            | (key, messageIds) <- Map.toList state.outboundMessageIds
            ]
        ]

instance FromJSON TelegramState where
    parseJSON = withObject "TelegramState" \o -> do
        storedVersion <- o .:? "version" .!= 1
        if storedVersion `elem` [1 :: Int, 2]
            then pure ()
            else fail
                ("unsupported Telegram state version: "
                    <> show storedVersion)
        nextUpdateId <- o .:? "nextUpdateId"
        storedBindings <- o .:? "bindings" .!= ([] :: [TelegramBinding])
        pendingTurns <- o .:? "pendingTurns" .!= ([] :: [TelegramPendingTurn])
        pendingReplies <- o .:? "pendingReplies" .!= ([] :: [TelegramPendingReply])
        pendingMediaTurns <-
            o .:? "pendingMediaTurns" .!= ([] :: [TelegramPendingMediaTurn])
        storedCallbacks <-
            o .:? "pendingCallbacks" .!= ([] :: [TelegramPendingCallback])
        storedCallbackBindings <-
            o .:? "callbackBindings" .!= ([] :: [TelegramCallbackBinding])
        storedRetries <-
            o .:? "retryMetadata" .!= ([] :: [(Text, TelegramRetryMetadata)])
        storedDeliveryCheckpoints <-
            o .:? "deliveryCheckpoints" .!= ([] :: [(Text, Int)])
        deadLetters <- o .:? "deadLetters" .!= []
        outboundMessages <-
            o .:? "outboundMessages" .!= ([] :: [TelegramOutboundMessages])
        pure TelegramState
            { telegramStateVersion = 2
            , nextUpdateId
            , bindings =
                foldr
                    (\binding ->
                        Map.insert binding.bindingChat binding.bindingSessionId)
                    Map.empty
                    storedBindings
            , pendingQueues =
                foldl'
                    (flip insertPendingAction)
                    Map.empty
                    ( map RunPendingTurn pendingTurns
                        <> map DeliverReply pendingReplies
                        <> map RunPendingMediaTurn pendingMediaTurns
                    )
            , pendingCallbacks =
                Map.fromList
                    [ (callback.pendingCallbackUpdateId, callback)
                    | callback <- storedCallbacks
                    ]
            , callbackBindings =
                Map.fromList
                    [ (binding.callbackBindingData, binding)
                    | binding <- storedCallbackBindings
                    ]
            , retryMetadata = Map.fromList storedRetries
            , deliveryCheckpoints = Map.fromList storedDeliveryCheckpoints
            , deadLetters
            , outboundMessageIds =
                Map.fromList
                    [ (messages.outboundChat, Set.fromList messages.outboundIds)
                    | messages <- outboundMessages
                    ]
            }

data TelegramOutboundMessages = TelegramOutboundMessages
    { outboundChat :: !TelegramChatKey
    , outboundIds :: ![Integer]
    } deriving (Eq, Show)

instance ToJSON TelegramOutboundMessages where
    toJSON messages = object
        [ "chat" .= messages.outboundChat
        , "messageIds" .= messages.outboundIds
        ]

instance FromJSON TelegramOutboundMessages where
    parseJSON = withObject "TelegramOutboundMessages" \o ->
        TelegramOutboundMessages <$> o .: "chat" <*> o .:? "messageIds" .!= []

data TelegramPendingTurn = TelegramPendingTurn
    { pendingTurnUpdateId :: !Integer
    , pendingTurnMessageId :: !Integer
    , pendingTurnChat :: !TelegramChatKey
    , pendingTurnText :: !Text
    , pendingTurnVoice :: !(Maybe TelegramVoice)
    } deriving (Eq, Show)

instance ToJSON TelegramPendingTurn where
    toJSON pending = object
        [ "updateId" .= pending.pendingTurnUpdateId
        , "messageId" .= pending.pendingTurnMessageId
        , "chat" .= pending.pendingTurnChat
        , "text" .= pending.pendingTurnText
        , "voice" .= pending.pendingTurnVoice
        ]

instance FromJSON TelegramPendingTurn where
    parseJSON = withObject "TelegramPendingTurn" \o ->
        TelegramPendingTurn
            <$> o .: "updateId"
            <*> o .:? "messageId" .!= 0
            <*> o .: "chat"
            <*> o .: "text"
            <*> o .:? "voice"

data TelegramPendingReply = TelegramPendingReply
    { pendingUpdateId :: !Integer
    , pendingChat :: !TelegramChatKey
    , pendingReplyToMessageId :: !(Maybe Integer)
    , pendingText :: !Text
    } deriving (Eq, Show)

instance ToJSON TelegramPendingReply where
    toJSON pending = object
        [ "updateId" .= pending.pendingUpdateId
        , "chat" .= pending.pendingChat
        , "replyToMessageId" .= pending.pendingReplyToMessageId
        , "text" .= pending.pendingText
        ]

instance FromJSON TelegramPendingReply where
    parseJSON = withObject "TelegramPendingReply" \o ->
        TelegramPendingReply
            <$> o .: "updateId"
            <*> o .: "chat"
            <*> o .:? "replyToMessageId"
            <*> o .: "text"

data TelegramPendingMediaTurn = TelegramPendingMediaTurn
    { pendingMediaUpdateId :: !Integer
    , pendingMediaMessageId :: !Integer
    , pendingMediaChat :: !TelegramChatKey
    , pendingMediaUserId :: !Integer
    , pendingMediaText :: !Text
    , pendingMediaAttachments :: ![TelegramMedia]
    , pendingMediaEdited :: !Bool
    , pendingMediaGroupId :: !(Maybe Text)
    } deriving (Eq, Show)

instance ToJSON TelegramPendingMediaTurn where
    toJSON pending = object
        [ "updateId" .= pending.pendingMediaUpdateId
        , "messageId" .= pending.pendingMediaMessageId
        , "chat" .= pending.pendingMediaChat
        , "userId" .= pending.pendingMediaUserId
        , "text" .= pending.pendingMediaText
        , "attachments" .= pending.pendingMediaAttachments
        , "edited" .= pending.pendingMediaEdited
        , "mediaGroupId" .= pending.pendingMediaGroupId
        ]

instance FromJSON TelegramPendingMediaTurn where
    parseJSON = withObject "TelegramPendingMediaTurn" \o ->
        TelegramPendingMediaTurn
            <$> o .: "updateId"
            <*> o .:? "messageId" .!= 0
            <*> o .: "chat"
            <*> o .:? "userId" .!= 0
            <*> o .:? "text" .!= ""
            <*> o .:? "attachments" .!= []
            <*> o .:? "edited" .!= False
            <*> o .:? "mediaGroupId"

data TelegramPendingCallback = TelegramPendingCallback
    { pendingCallbackUpdateId :: !Integer
    , pendingCallbackQueryId :: !Text
    , pendingCallbackUserId :: !Integer
    , pendingCallbackChat :: !(Maybe TelegramChatKey)
    , pendingCallbackMessageId :: !(Maybe Integer)
    , pendingCallbackData :: !Text
    } deriving (Eq, Show)

instance ToJSON TelegramPendingCallback where
    toJSON pending = object
        [ "updateId" .= pending.pendingCallbackUpdateId
        , "queryId" .= pending.pendingCallbackQueryId
        , "userId" .= pending.pendingCallbackUserId
        , "chat" .= pending.pendingCallbackChat
        , "messageId" .= pending.pendingCallbackMessageId
        , "data" .= pending.pendingCallbackData
        ]

instance FromJSON TelegramPendingCallback where
    parseJSON = withObject "TelegramPendingCallback" \o ->
        TelegramPendingCallback
            <$> o .: "updateId"
            <*> o .: "queryId"
            <*> o .: "userId"
            <*> o .:? "chat"
            <*> o .:? "messageId"
            <*> o .: "data"

data TelegramRetryMetadata = TelegramRetryMetadata
    { retryAttempts :: !Int
    , retryNextAt :: !(Maybe UTCTime)
    , retryLastError :: !(Maybe Text)
    } deriving (Eq, Show)

instance ToJSON TelegramRetryMetadata where
    toJSON retry = object
        [ "attempts" .= retry.retryAttempts
        , "nextAt" .= retry.retryNextAt
        , "lastError" .= retry.retryLastError
        ]

instance FromJSON TelegramRetryMetadata where
    parseJSON = withObject "TelegramRetryMetadata" \o ->
        TelegramRetryMetadata
            <$> o .:? "attempts" .!= 0
            <*> o .:? "nextAt"
            <*> o .:? "lastError"

data TelegramDeadLetter = TelegramDeadLetter
    { deadLetterUpdateId :: !Integer
    , deadLetterChat :: !(Maybe TelegramChatKey)
    , deadLetterError :: !Text
    , deadLetterFailedAt :: !UTCTime
    , deadLetterAction :: !(Maybe PendingChatAction)
    } deriving (Eq, Show)

instance ToJSON TelegramDeadLetter where
    toJSON dead = object
        [ "updateId" .= dead.deadLetterUpdateId
        , "chat" .= dead.deadLetterChat
        , "error" .= dead.deadLetterError
        , "failedAt" .= dead.deadLetterFailedAt
        , "action" .= dead.deadLetterAction
        ]

instance FromJSON TelegramDeadLetter where
    parseJSON = withObject "TelegramDeadLetter" \o ->
        TelegramDeadLetter
            <$> o .: "updateId"
            <*> o .:? "chat"
            <*> o .: "error"
            <*> o .: "failedAt"
            <*> o .:? "action"

data TelegramCallbackBinding = TelegramCallbackBinding
    { callbackBindingData :: !Text
    , callbackBindingRequestId :: !Text
    , callbackBindingChat :: !TelegramChatKey
    , callbackBindingUserId :: !Integer
    , callbackBindingMessageId :: !(Maybe Integer)
    , callbackBindingBridgeDirectory :: !FilePath
    , callbackBindingValue :: !Text
    , callbackBindingExpiresAt :: !UTCTime
    , callbackBindingConsumed :: !Bool
    } deriving (Eq, Show)

instance ToJSON TelegramCallbackBinding where
    toJSON binding = object
        [ "data" .= binding.callbackBindingData
        , "requestId" .= binding.callbackBindingRequestId
        , "chat" .= binding.callbackBindingChat
        , "userId" .= binding.callbackBindingUserId
        , "messageId" .= binding.callbackBindingMessageId
        , "bridgeDirectory" .= binding.callbackBindingBridgeDirectory
        , "value" .= binding.callbackBindingValue
        , "expiresAt" .= binding.callbackBindingExpiresAt
        , "consumed" .= binding.callbackBindingConsumed
        ]

instance FromJSON TelegramCallbackBinding where
    parseJSON = withObject "TelegramCallbackBinding" \o ->
        TelegramCallbackBinding
            <$> o .: "data"
            <*> o .: "requestId"
            <*> o .: "chat"
            <*> o .: "userId"
            <*> o .:? "messageId"
            <*> o .: "bridgeDirectory"
            <*> o .: "value"
            <*> o .: "expiresAt"
            <*> o .:? "consumed" .!= False

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
    } deriving (Eq, Show)

instance FromJSON TelegramUpdate where
    parseJSON = withObject "TelegramUpdate" \o ->
        TelegramUpdate
            <$> o .: "update_id"
            <*> o .:? "message"
            <*> o .:? "edited_message"
            <*> o .:? "message_reaction"
            <*> o .:? "callback_query"

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

data PendingChatAction
    = DeliverReply !TelegramPendingReply
    | RunPendingTurn !TelegramPendingTurn
    | RunPendingMediaTurn !TelegramPendingMediaTurn
    deriving (Eq, Show)

pendingActionUpdateId :: PendingChatAction -> Integer
pendingActionUpdateId = \case
    DeliverReply pending -> pending.pendingUpdateId
    RunPendingTurn pending -> pending.pendingTurnUpdateId
    RunPendingMediaTurn pending -> pending.pendingMediaUpdateId

pendingActionChat :: PendingChatAction -> TelegramChatKey
pendingActionChat = \case
    DeliverReply pending -> pending.pendingChat
    RunPendingTurn pending -> pending.pendingTurnChat
    RunPendingMediaTurn pending -> pending.pendingMediaChat

insertPendingAction
    :: PendingChatAction
    -> Map TelegramChatKey (Map Integer PendingChatAction)
    -> Map TelegramChatKey (Map Integer PendingChatAction)
insertPendingAction action =
    Map.alter
        (Just . Map.insert (pendingActionUpdateId action) action . fromMaybe Map.empty)
        (pendingActionChat action)

enqueuePendingAction :: PendingChatAction -> TelegramState -> TelegramState
enqueuePendingAction action state =
    state { pendingQueues = insertPendingAction action state.pendingQueues }

deletePendingAction :: PendingChatAction -> TelegramState -> TelegramState
deletePendingAction action state =
    state
        { pendingQueues =
            Map.update
                (\queue ->
                    let remaining = Map.delete (pendingActionUpdateId action) queue
                    in if Map.null remaining then Nothing else Just remaining)
                (pendingActionChat action)
                state.pendingQueues
        }

emptyTelegramState :: TelegramState
emptyTelegramState = TelegramState
    { telegramStateVersion = 2
    , nextUpdateId = Nothing
    , bindings = Map.empty
    , pendingQueues = Map.empty
    , pendingCallbacks = Map.empty
    , callbackBindings = Map.empty
    , retryMetadata = Map.empty
    , deliveryCheckpoints = Map.empty
    , deadLetters = []
    , outboundMessageIds = Map.empty
    }
