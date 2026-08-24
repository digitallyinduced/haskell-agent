{-# OPTIONS_GHC -Wno-orphans #-}

-- | Telegram wire types and durable queue state.
module Agent.Telegram.Types
    ( TelegramConfig(..)
    , TelegramSetupOptions(..)
    , defaultTelegramSetupOptions
    , TelegramCommand(..)
    , TelegramChatKey(..)
    , TelegramBinding(..)
    , TelegramState(..)
    , TelegramPendingTurn(..)
    , TelegramPendingReply(..)
    , TelegramVoice(..)
    , TelegramUser(..)
    , TelegramChat(..)
    , TelegramMessage(..)
    , TelegramReactionType(..)
    , TelegramMessageReaction(..)
    , TelegramUpdate(..)
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
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Network.HTTP.Client as Http

data TelegramConfig = TelegramConfig
    { telegramProvider :: !Provider
    , telegramModel :: !(Maybe Text)
    , telegramCwd :: !FilePath
    , telegramEffort :: !(Maybe Text)
    , telegramYolo :: !Bool
    , telegramAllowedUsers :: !(Set Integer)
    } deriving (Eq, Show)

instance ToJSON TelegramConfig where
    toJSON config = object
        [ "provider" .= providerSlug config.telegramProvider
        , "model" .= config.telegramModel
        , "cwd" .= config.telegramCwd
        , "effort" .= config.telegramEffort
        , "yolo" .= config.telegramYolo
        , "allowedUsers" .= Set.toList config.telegramAllowedUsers
        ]

instance FromJSON TelegramConfig where
    parseJSON = withObject "TelegramConfig" \o -> do
        providerText <- o .: "provider"
        telegramProvider <- maybe
            (fail ("unknown provider: " <> Text.unpack providerText))
            pure
            (parseProvider providerText)
        TelegramConfig
            <$> pure telegramProvider
            <*> o .:? "model"
            <*> o .: "cwd"
            <*> o .:? "effort"
            <*> (o .:? "yolo" .!= False)
            <*> (Set.fromList <$> o .: "allowedUsers")

data TelegramSetupOptions = TelegramSetupOptions
    { setupProvider :: !(Maybe Provider)
    , setupModel :: !(Maybe Text)
    , setupCwd :: !(Maybe FilePath)
    , setupEffort :: !(Maybe Text)
    , setupYolo :: !Bool
    , setupAllowedUser :: !(Maybe Integer)
    , setupStart :: !Bool
    } deriving (Eq, Show)

defaultTelegramSetupOptions :: TelegramSetupOptions
defaultTelegramSetupOptions = TelegramSetupOptions
    { setupProvider = Nothing
    , setupModel = Nothing
    , setupCwd = Nothing
    , setupEffort = Nothing
    , setupYolo = False
    , setupAllowedUser = Nothing
    , setupStart = False
    }

data TelegramCommand
    = TelegramSetup !TelegramSetupOptions
    | TelegramRun
    | TelegramStart
    | TelegramStop
    | TelegramStatus
    | TelegramHelp
    | TelegramVersion
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
    { nextUpdateId :: !(Maybe Integer)
    , bindings :: !(Map TelegramChatKey Text)
    , pendingQueues :: !(Map TelegramChatKey (Map Integer PendingChatAction))
    } deriving (Eq, Show)

instance ToJSON TelegramState where
    toJSON state = object
        [ "nextUpdateId" .= state.nextUpdateId
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
        ]

instance FromJSON TelegramState where
    parseJSON = withObject "TelegramState" \o -> do
        nextUpdateId <- o .:? "nextUpdateId"
        storedBindings <- o .:? "bindings" .!= ([] :: [TelegramBinding])
        pendingTurns <- o .:? "pendingTurns" .!= ([] :: [TelegramPendingTurn])
        pendingReplies <- o .:? "pendingReplies" .!= ([] :: [TelegramPendingReply])
        pure TelegramState
            { nextUpdateId
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
                    (map RunPendingTurn pendingTurns <> map DeliverReply pendingReplies)
            }

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
    } deriving (Eq, Show)

instance FromJSON TelegramUser where
    parseJSON = withObject "TelegramUser" \o -> TelegramUser <$> o .: "id"

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
    , messageVoice :: !(Maybe TelegramVoice)
    } deriving (Eq, Show)

instance FromJSON TelegramMessage where
    parseJSON = withObject "TelegramMessage" \o ->
        TelegramMessage
            <$> o .: "message_id"
            <*> o .:? "from"
            <*> o .: "chat"
            <*> o .:? "message_thread_id"
            <*> o .:? "text"
            <*> o .:? "voice"

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

data TelegramUpdate = TelegramUpdate
    { updateId :: !Integer
    , updateMessage :: !(Maybe TelegramMessage)
    , updateMessageReaction :: !(Maybe TelegramMessageReaction)
    } deriving (Eq, Show)

instance FromJSON TelegramUpdate where
    parseJSON = withObject "TelegramUpdate" \o ->
        TelegramUpdate
            <$> o .: "update_id"
            <*> o .:? "message"
            <*> o .:? "message_reaction"

data TelegramResponse a = TelegramResponse
    { responseOk :: !Bool
    , responseResult :: !(Maybe a)
    , responseDescription :: !(Maybe Text)
    }

instance FromJSON a => FromJSON (TelegramResponse a) where
    parseJSON = withObject "TelegramResponse" \o ->
        TelegramResponse <$> o .: "ok" <*> o .:? "result" <*> o .:? "description"

data TelegramClient = TelegramClient
    { clientToken :: !Text
    , clientManager :: !Http.Manager
    }

data PendingChatAction
    = DeliverReply !TelegramPendingReply
    | RunPendingTurn !TelegramPendingTurn
    deriving (Eq, Show)

pendingActionUpdateId :: PendingChatAction -> Integer
pendingActionUpdateId = \case
    DeliverReply pending -> pending.pendingUpdateId
    RunPendingTurn pending -> pending.pendingTurnUpdateId

pendingActionChat :: PendingChatAction -> TelegramChatKey
pendingActionChat = \case
    DeliverReply pending -> pending.pendingChat
    RunPendingTurn pending -> pending.pendingTurnChat

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
    { nextUpdateId = Nothing
    , bindings = Map.empty
    , pendingQueues = Map.empty
    }
