-- | Telegram durable queue state.
module Agent.Telegram.Types.State
    ( TelegramChatKey(..)
    , TelegramBinding(..)
    , TelegramState(..)
    , TelegramPendingTurn(..)
    , TelegramPendingReply(..)
    , TelegramPendingMediaTurn(..)
    , TelegramPendingLeave(..)
    , TelegramPendingCallback(..)
    , TelegramRetryMetadata(..)
    , TelegramDeadLetter(..)
    , TelegramCallbackBinding(..)
    , PendingChatAction(..)
    , emptyTelegramState
    , insertPendingAction
    , enqueuePendingAction
    , deletePendingAction
    ) where

import Agent.Telegram.Types.Wire (TelegramMedia, TelegramUser(..), TelegramVoice)
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
import Data.List (sort, sortOn)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime)

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
        LeaveUnauthorizedChat pending -> object
            [ "kind" .= ("leave" :: Text)
            , "leave" .= pending
            ]

instance FromJSON PendingChatAction where
    parseJSON = withObject "PendingChatAction" \o -> do
        kind <- o .: "kind"
        case (kind :: Text) of
            "reply" -> DeliverReply <$> o .: "reply"
            "turn" -> RunPendingTurn <$> o .: "turn"
            "media_turn" -> RunPendingMediaTurn <$> o .: "mediaTurn"
            "leave" -> LeaveUnauthorizedChat <$> o .: "leave"
            _ -> fail ("unknown pending Telegram action: " <> Text.unpack kind)

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
    , authorizedGroupChatIds :: !(Set Integer)
    , allowedUserIds :: !(Set Integer)
    , seenTelegramUsers :: !(Map Integer TelegramUser)
    , seenUsersByChat :: !(Map Integer (Set Integer))
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
        , "pendingLeaves" .=
            sortOn (.pendingLeaveUpdateId)
                [ pending
                | queue <- Map.elems state.pendingQueues
                , LeaveUnauthorizedChat pending <- Map.elems queue
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
        , "authorizedGroupChats" .= sort (Set.toList state.authorizedGroupChatIds)
        , "allowedUserIds" .= sort (Set.toList state.allowedUserIds)
        , "seenTelegramUsers" .=
            sortOn (.userId) (Map.elems state.seenTelegramUsers)
        , "seenUsersByChat" .=
            [ TelegramSeenChatUsers chatId (sort (Set.toList userIds))
            | (chatId, userIds) <- Map.toAscList state.seenUsersByChat
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
        pendingLeaves <-
            o .:? "pendingLeaves" .!= ([] :: [TelegramPendingLeave])
        storedAuthorized <- o .:? "authorizedGroupChats"
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
        storedAllowedUsers <-
            o .:? "allowedUserIds" .!= ([] :: [Integer])
        storedSeenUsers <-
            o .:? "seenTelegramUsers" .!= ([] :: [TelegramUser])
        storedSeenByChat <-
            o .:? "seenUsersByChat" .!= ([] :: [TelegramSeenChatUsers])
        let bindings =
                foldr
                    (\binding ->
                        Map.insert binding.bindingChat binding.bindingSessionId)
                    Map.empty
                    storedBindings
            authorizedGroupChatIds = case storedAuthorized of
                Just ids -> Set.fromList ids
                Nothing -> Set.fromList
                    [ key.chatId
                    | key <- Map.keys bindings
                    , key.chatId < 0
                    ]
        pure TelegramState
            { telegramStateVersion = 2
            , nextUpdateId
            , bindings
            , pendingQueues =
                foldl'
                    (flip insertPendingAction)
                    Map.empty
                    ( map RunPendingTurn pendingTurns
                        <> map DeliverReply pendingReplies
                        <> map RunPendingMediaTurn pendingMediaTurns
                        <> map LeaveUnauthorizedChat pendingLeaves
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
            , authorizedGroupChatIds
            , allowedUserIds = Set.fromList storedAllowedUsers
            , seenTelegramUsers =
                Map.fromList
                    [ (user.userId, user)
                    | user <- storedSeenUsers
                    ]
            , seenUsersByChat =
                Map.fromList
                    [ (seen.seenChatId, Set.fromList seen.seenChatUserIds)
                    | seen <- storedSeenByChat
                    ]
            }

data TelegramSeenChatUsers = TelegramSeenChatUsers
    { seenChatId :: !Integer
    , seenChatUserIds :: ![Integer]
    } deriving (Eq, Show)

instance ToJSON TelegramSeenChatUsers where
    toJSON seen = object
        [ "chatId" .= seen.seenChatId
        , "userIds" .= seen.seenChatUserIds
        ]

instance FromJSON TelegramSeenChatUsers where
    parseJSON = withObject "TelegramSeenChatUsers" \o ->
        TelegramSeenChatUsers <$> o .: "chatId" <*> (o .:? "userIds" .!= [])

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

data TelegramPendingLeave = TelegramPendingLeave
    { pendingLeaveUpdateId :: !Integer
    , pendingLeaveChat :: !TelegramChatKey
    } deriving (Eq, Show)

instance ToJSON TelegramPendingLeave where
    toJSON pending = object
        [ "updateId" .= pending.pendingLeaveUpdateId
        , "chat" .= pending.pendingLeaveChat
        ]

instance FromJSON TelegramPendingLeave where
    parseJSON = withObject "TelegramPendingLeave" \o ->
        TelegramPendingLeave
            <$> o .: "updateId"
            <*> o .: "chat"

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

data PendingChatAction
    = DeliverReply !TelegramPendingReply
    | RunPendingTurn !TelegramPendingTurn
    | RunPendingMediaTurn !TelegramPendingMediaTurn
    | LeaveUnauthorizedChat !TelegramPendingLeave
    deriving (Eq, Show)

pendingActionUpdateId :: PendingChatAction -> Integer
pendingActionUpdateId = \case
    DeliverReply pending -> pending.pendingUpdateId
    RunPendingTurn pending -> pending.pendingTurnUpdateId
    RunPendingMediaTurn pending -> pending.pendingMediaUpdateId
    LeaveUnauthorizedChat pending -> pending.pendingLeaveUpdateId

pendingActionChat :: PendingChatAction -> TelegramChatKey
pendingActionChat = \case
    DeliverReply pending -> pending.pendingChat
    RunPendingTurn pending -> pending.pendingTurnChat
    RunPendingMediaTurn pending -> pending.pendingMediaChat
    LeaveUnauthorizedChat pending -> pending.pendingLeaveChat

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
    , authorizedGroupChatIds = Set.empty
    , allowedUserIds = Set.empty
    , seenTelegramUsers = Map.empty
    , seenUsersByChat = Map.empty
    }
