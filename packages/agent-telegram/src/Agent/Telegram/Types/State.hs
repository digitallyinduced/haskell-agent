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
    , pendingReplyTarget
    , telegramStateDecoder
    ) where

import Agent.Json (rawJsonBytes, rawJsonDecoder)
import qualified Agent.Json.Decode as Hermes
import Agent.Telegram.Types.Wire
    ( TelegramMedia
    , TelegramUser(..)
    , TelegramVoice
    , telegramMediaDecoder
    , telegramUserDecoder
    , telegramVoiceDecoder
    )
import Data.Aeson
    ( ToJSON(..)
    , object
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

pendingChatActionDecoder :: Hermes.Decoder PendingChatAction
pendingChatActionDecoder = Hermes.object do
        kind <- Hermes.atKey "kind" Hermes.text
        case (kind :: Text) of
            "reply" -> DeliverReply
                <$> Hermes.atKey "reply" telegramPendingReplyDecoder
            "turn" -> RunPendingTurn
                <$> Hermes.atKey "turn" telegramPendingTurnDecoder
            "media_turn" -> RunPendingMediaTurn
                <$> Hermes.atKey "mediaTurn" telegramPendingMediaTurnDecoder
            "leave" -> LeaveUnauthorizedChat
                <$> Hermes.atKey "leave" telegramPendingLeaveDecoder
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

telegramChatKeyDecoder :: Hermes.Decoder TelegramChatKey
telegramChatKeyDecoder = Hermes.object $
    TelegramChatKey
        <$> Hermes.atKey "chatId" integerDecoder
        <*> Hermes.optionalKey "messageThreadId" integerDecoder

data TelegramBinding = TelegramBinding
    { bindingChat :: !TelegramChatKey
    , bindingSessionId :: !Text
    } deriving (Eq, Show)

instance ToJSON TelegramBinding where
    toJSON binding = object
        [ "chat" .= binding.bindingChat
        , "sessionId" .= binding.bindingSessionId
        ]

telegramBindingDecoder :: Hermes.Decoder TelegramBinding
telegramBindingDecoder = Hermes.object $
    TelegramBinding
        <$> Hermes.atKey "chat" telegramChatKeyDecoder
        <*> Hermes.atKey "sessionId" Hermes.text

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
    , latestInboundMessageIds :: !(Map TelegramChatKey Integer)
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
        , "latestInboundMessages" .=
            [ TelegramLatestInboundMessage key messageId
            | (key, messageId) <- Map.toAscList state.latestInboundMessageIds
            ]
        ]

telegramStateDecoder :: Hermes.Decoder TelegramState
telegramStateDecoder = Hermes.object do
        storedVersion <- defaultField "version" 1 Hermes.int
        if storedVersion `elem` [1 :: Int, 2]
            then pure ()
            else fail
                ("unsupported Telegram state version: "
                    <> show storedVersion)
        nextUpdateId <- Hermes.optionalKey "nextUpdateId" integerDecoder
        storedBindings <- defaultField "bindings" []
            (Hermes.list telegramBindingDecoder)
        pendingTurns <- defaultField "pendingTurns" []
            (Hermes.list telegramPendingTurnDecoder)
        pendingReplies <- defaultField "pendingReplies" []
            (Hermes.list telegramPendingReplyDecoder)
        pendingMediaTurns <-
            defaultField "pendingMediaTurns" []
                (Hermes.list telegramPendingMediaTurnDecoder)
        pendingLeaves <-
            defaultField "pendingLeaves" []
                (Hermes.list telegramPendingLeaveDecoder)
        storedAuthorized <- Hermes.optionalKey "authorizedGroupChats"
            (Hermes.list integerDecoder)
        storedCallbacks <-
            defaultField "pendingCallbacks" []
                (Hermes.list telegramPendingCallbackDecoder)
        storedCallbackBindings <-
            defaultField "callbackBindings" []
                (Hermes.list telegramCallbackBindingDecoder)
        storedRetries <-
            defaultField "retryMetadata" []
                (Hermes.list (pairDecoder Hermes.text telegramRetryMetadataDecoder))
        storedDeliveryCheckpoints <-
            defaultField "deliveryCheckpoints" []
                (Hermes.list (pairDecoder Hermes.text Hermes.int))
        deadLetters <- defaultField "deadLetters" []
            (Hermes.list telegramDeadLetterDecoder)
        outboundMessages <-
            defaultField "outboundMessages" []
                (Hermes.list telegramOutboundMessagesDecoder)
        storedAllowedUsers <-
            defaultField "allowedUserIds" [] (Hermes.list integerDecoder)
        storedSeenUsers <-
            defaultField "seenTelegramUsers" []
                (Hermes.list telegramUserDecoder)
        storedSeenByChat <-
            defaultField "seenUsersByChat" []
                (Hermes.list telegramSeenChatUsersDecoder)
        storedLatestInbound <-
            defaultField "latestInboundMessages" []
                (Hermes.list telegramLatestInboundMessageDecoder)
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
            , latestInboundMessageIds =
                Map.fromList
                    [ (latest.latestInboundChat, latest.latestInboundMessageId)
                    | latest <- storedLatestInbound
                    ]
            }

data TelegramLatestInboundMessage = TelegramLatestInboundMessage
    { latestInboundChat :: !TelegramChatKey
    , latestInboundMessageId :: !Integer
    } deriving (Eq, Show)

instance ToJSON TelegramLatestInboundMessage where
    toJSON latest = object
        [ "chat" .= latest.latestInboundChat
        , "messageId" .= latest.latestInboundMessageId
        ]

telegramLatestInboundMessageDecoder :: Hermes.Decoder TelegramLatestInboundMessage
telegramLatestInboundMessageDecoder = Hermes.object $
    TelegramLatestInboundMessage
        <$> Hermes.atKey "chat" telegramChatKeyDecoder
        <*> Hermes.atKey "messageId" integerDecoder

data TelegramSeenChatUsers = TelegramSeenChatUsers
    { seenChatId :: !Integer
    , seenChatUserIds :: ![Integer]
    } deriving (Eq, Show)

instance ToJSON TelegramSeenChatUsers where
    toJSON seen = object
        [ "chatId" .= seen.seenChatId
        , "userIds" .= seen.seenChatUserIds
        ]

telegramSeenChatUsersDecoder :: Hermes.Decoder TelegramSeenChatUsers
telegramSeenChatUsersDecoder = Hermes.object $
    TelegramSeenChatUsers
        <$> Hermes.atKey "chatId" integerDecoder
        <*> defaultField "userIds" [] (Hermes.list integerDecoder)

data TelegramOutboundMessages = TelegramOutboundMessages
    { outboundChat :: !TelegramChatKey
    , outboundIds :: ![Integer]
    } deriving (Eq, Show)

instance ToJSON TelegramOutboundMessages where
    toJSON messages = object
        [ "chat" .= messages.outboundChat
        , "messageIds" .= messages.outboundIds
        ]

telegramOutboundMessagesDecoder :: Hermes.Decoder TelegramOutboundMessages
telegramOutboundMessagesDecoder = Hermes.object $
    TelegramOutboundMessages
        <$> Hermes.atKey "chat" telegramChatKeyDecoder
        <*> defaultField "messageIds" [] (Hermes.list integerDecoder)

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

telegramPendingTurnDecoder :: Hermes.Decoder TelegramPendingTurn
telegramPendingTurnDecoder = Hermes.object $
        TelegramPendingTurn
            <$> Hermes.atKey "updateId" integerDecoder
            <*> defaultField "messageId" 0 integerDecoder
            <*> Hermes.atKey "chat" telegramChatKeyDecoder
            <*> Hermes.atKey "text" Hermes.text
            <*> Hermes.optionalKey "voice" telegramVoiceDecoder

data TelegramPendingReply = TelegramPendingReply
    { pendingUpdateId :: !Integer
    , pendingChat :: !TelegramChatKey
    , pendingReplyToMessageId :: !(Maybe Integer)
    , pendingEditMessageId :: !(Maybe Integer)
    , pendingText :: !Text
    } deriving (Eq, Show)

instance ToJSON TelegramPendingReply where
    toJSON pending = object
        [ "updateId" .= pending.pendingUpdateId
        , "chat" .= pending.pendingChat
        , "replyToMessageId" .= pending.pendingReplyToMessageId
        , "editMessageId" .= pending.pendingEditMessageId
        , "text" .= pending.pendingText
        ]

telegramPendingReplyDecoder :: Hermes.Decoder TelegramPendingReply
telegramPendingReplyDecoder = Hermes.object $
        TelegramPendingReply
            <$> Hermes.atKey "updateId" integerDecoder
            <*> Hermes.atKey "chat" telegramChatKeyDecoder
            <*> Hermes.optionalKey "replyToMessageId" integerDecoder
            <*> Hermes.optionalKey "editMessageId" integerDecoder
            <*> Hermes.atKey "text" Hermes.text

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

telegramPendingMediaTurnDecoder :: Hermes.Decoder TelegramPendingMediaTurn
telegramPendingMediaTurnDecoder = Hermes.object $
        TelegramPendingMediaTurn
            <$> Hermes.atKey "updateId" integerDecoder
            <*> defaultField "messageId" 0 integerDecoder
            <*> Hermes.atKey "chat" telegramChatKeyDecoder
            <*> defaultField "userId" 0 integerDecoder
            <*> defaultField "text" "" Hermes.text
            <*> defaultField "attachments" [] (Hermes.list telegramMediaDecoder)
            <*> defaultField "edited" False Hermes.bool
            <*> Hermes.optionalKey "mediaGroupId" Hermes.text

data TelegramPendingLeave = TelegramPendingLeave
    { pendingLeaveUpdateId :: !Integer
    , pendingLeaveChat :: !TelegramChatKey
    } deriving (Eq, Show)

instance ToJSON TelegramPendingLeave where
    toJSON pending = object
        [ "updateId" .= pending.pendingLeaveUpdateId
        , "chat" .= pending.pendingLeaveChat
        ]

telegramPendingLeaveDecoder :: Hermes.Decoder TelegramPendingLeave
telegramPendingLeaveDecoder = Hermes.object $
        TelegramPendingLeave
            <$> Hermes.atKey "updateId" integerDecoder
            <*> Hermes.atKey "chat" telegramChatKeyDecoder

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

telegramPendingCallbackDecoder :: Hermes.Decoder TelegramPendingCallback
telegramPendingCallbackDecoder = Hermes.object $
        TelegramPendingCallback
            <$> Hermes.atKey "updateId" integerDecoder
            <*> Hermes.atKey "queryId" Hermes.text
            <*> Hermes.atKey "userId" integerDecoder
            <*> Hermes.optionalKey "chat" telegramChatKeyDecoder
            <*> Hermes.optionalKey "messageId" integerDecoder
            <*> Hermes.atKey "data" Hermes.text

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

telegramRetryMetadataDecoder :: Hermes.Decoder TelegramRetryMetadata
telegramRetryMetadataDecoder = Hermes.object $
        TelegramRetryMetadata
            <$> defaultField "attempts" 0 Hermes.int
            <*> Hermes.optionalKey "nextAt" Hermes.utcTime
            <*> Hermes.optionalKey "lastError" Hermes.text

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

telegramDeadLetterDecoder :: Hermes.Decoder TelegramDeadLetter
telegramDeadLetterDecoder = Hermes.object $
        TelegramDeadLetter
            <$> Hermes.atKey "updateId" integerDecoder
            <*> Hermes.optionalKey "chat" telegramChatKeyDecoder
            <*> Hermes.atKey "error" Hermes.text
            <*> Hermes.atKey "failedAt" Hermes.utcTime
            <*> Hermes.optionalKey "action" pendingChatActionDecoder

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

telegramCallbackBindingDecoder :: Hermes.Decoder TelegramCallbackBinding
telegramCallbackBindingDecoder = Hermes.object $
        TelegramCallbackBinding
            <$> Hermes.atKey "data" Hermes.text
            <*> Hermes.atKey "requestId" Hermes.text
            <*> Hermes.atKey "chat" telegramChatKeyDecoder
            <*> Hermes.atKey "userId" integerDecoder
            <*> Hermes.optionalKey "messageId" integerDecoder
            <*> (Text.unpack <$> Hermes.atKey "bridgeDirectory" Hermes.text)
            <*> Hermes.atKey "value" Hermes.text
            <*> Hermes.atKey "expiresAt" Hermes.utcTime
            <*> defaultField "consumed" False Hermes.bool

data PendingChatAction
    = DeliverReply !TelegramPendingReply
    | RunPendingTurn !TelegramPendingTurn
    | RunPendingMediaTurn !TelegramPendingMediaTurn
    | LeaveUnauthorizedChat !TelegramPendingLeave
    deriving (Eq, Show)


defaultField
    :: Text
    -> a
    -> Hermes.Decoder a
    -> Hermes.FieldsDecoder a
defaultField key fallback decoder =
    Hermes.defaultKey fallback key decoder

integerDecoder :: Hermes.Decoder Integer
integerDecoder = fromIntegral <$> Hermes.int

pairDecoder
    :: Hermes.Decoder a
    -> Hermes.Decoder b
    -> Hermes.Decoder (a, b)
pairDecoder first second =
    rawJsonDecoder >>= \raw ->
        case Hermes.decodeEither
                ((,)
                    <$> Hermes.atPointer "/0" first
                    <*> Hermes.atPointer "/1" second)
                (rawJsonBytes raw) of
            Left err -> fail
                (Text.unpack (Hermes.jsonErrorMessage err))
            Right pair -> pure pair

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

-- | Use Telegram's visible reply chrome only when a newer inbound message is
-- visible in the same conversation. A response to the latest message reads
-- naturally as the next ordinary chat message.
pendingReplyTarget :: TelegramPendingReply -> TelegramState -> Maybe Integer
pendingReplyTarget pending state = do
    target <- pending.pendingReplyToMessageId
    case Map.lookup pending.pendingChat state.latestInboundMessageIds of
        Just latest | latest <= target -> Nothing
        -- Missing recency is possible for work restored from older state.
        -- Keep the target conservatively rather than misattribute the reply.
        _ -> Just target

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
    , latestInboundMessageIds = Map.empty
    }
