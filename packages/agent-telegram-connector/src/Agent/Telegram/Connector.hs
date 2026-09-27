-- | High-level, supervised Telegram conversation service.
--
-- Store operations are transaction boundaries, not event handlers: the inbox
-- commits a complete batch before its offset is acknowledged, execution claims
-- serialize all channels of one session, and delivery claims commit before IO.
module Agent.Telegram.Connector
    ( module Agent.Telegram.Connector.Session
    , TelegramConnector(..), InboxStore(..), ExecutionStore(..), DeliveryStore(..)
    , ConversationApplication(..), ConversationEvent(..)
    , DeliveryAttempt(..), DeliveryOutcome(..)
    , runTelegramConnector, runSessionConnector, pollConnectorUpdates, processConnectorUpdate
    , processConnectorExecution, processConnectorDelivery
    , classifyPrivateConversationUpdate, deliverConnectorMessage, prepareDeliveryPayload
    , runConversationQueue, ConversationQueue(..)
    ) where

import Agent.Telegram.Connector.Session
import qualified Agent.Telegram.Client as Telegram
import Agent.Telegram.Presentation (boundedTextPresentationFields, messageAddressFields)
import Agent.Telegram.Types.State (TelegramChatKey(..))
import Agent.Telegram.Types.Wire
import qualified Agent.Json.Decode as Json
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (mapConcurrently_)
import Control.Exception.Safe (SomeException, tryAny)
import Control.Monad (forever, forM_, void)
import Data.Aeson (Value, encode, eitherDecode, object, (.=), (.:), withObject)
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy as ByteString
import Data.Maybe (isJust)
import Data.Text (Text)

-- | Every callback owns its database connection. Do not share a connection
-- between the five concurrently supervised services.
data TelegramConnector context binding = TelegramConnector
    { connectorClient :: !TelegramClient
    , connectorInbox :: !(InboxStore context)
    , connectorApplication :: !(context -> ConversationApplication binding)
    , connectorExecutions :: !ExecutionStore
    , connectorBackend :: !SessionBackend
    , connectorDeliveries :: !DeliveryStore
    , connectorProgress :: !(IO ())
    , connectorReportFailure :: !(Text -> IO ())
    }

data InboxStore context = InboxStore
    { readOffset :: IO Integer
    -- | Atomically insert by update ID and advance offset with max(old,new).
    , persistBatch :: [(Integer, Value)] -> Integer -> IO ()
    -- | Own the claim, rollback failed work, quarantine invalid work, and mark
    -- processed only when the supplied action succeeds.
    , withNextUpdate :: (context -> Value -> IO ()) -> IO Bool
    }

data ExecutionStore = ExecutionStore
    { withNextExecution :: (SessionExecution -> IO ExecutionTransition) -> IO Bool
    }

data DeliveryStore = DeliveryStore
    { withNextDelivery :: (DeliveryAttempt -> IO DeliveryOutcome) -> IO Bool
    }

data ConversationApplication binding = ConversationApplication
    { authorizeConversation :: TelegramChatKey -> Integer -> IO (Maybe binding)
    -- | Only this narrowly scoped path receives an unlinked message, to handle
    -- the application's one-use account-link command. It must not run an agent.
    , linkConversation :: TelegramMessage -> IO ()
    -- | Persist the admitted event and its execution intent atomically.
    , recordConversationEvent :: binding -> Integer -> ConversationEvent -> IO ()
    }

data ConversationEvent
    = ConversationMessage !TelegramMessage
    | ConversationEdit !TelegramMessage
    | ConversationReaction !TelegramMessageReaction
    | ConversationCallback !TelegramCallbackQuery
    deriving (Eq, Show)

data DeliveryAttempt = DeliveryAttempt
    { deliveryChat :: !TelegramChatKey
    , deliveryReplyTo :: !(Maybe Integer)
    , deliveryContent :: !Text
    , deliveryKeyboard :: !(Maybe Value)
    }
    -- | Trusted, already prepared sendMessage payload persisted by the outbox.
    -- The store must authorize its exact recipient before releasing the claim.
    | PreparedDeliveryAttempt !Value
    deriving (Eq, Show)

data DeliveryOutcome
    = DeliveryAcknowledged !Integer
    | DeliveryRejected
    | DeliveryUncertain
    deriving (Eq, Show)

-- | All workers are children of this call. Failure recovery deliberately
-- records only a component name, never raw bearer-bearing HTTP exceptions.
runTelegramConnector :: TelegramConnector context binding -> IO ()
runTelegramConnector connector = mapConcurrently_ id
    [ service "polling" 250000 (pollConnectorUpdates connector.connectorClient connector.connectorInbox)
    , service "inbox" 250000 (void $ processConnectorUpdate connector.connectorInbox connector.connectorApplication)
    , service "execution" 1000000 (void $ processConnectorExecution connector.connectorExecutions connector.connectorBackend)
    , service "delivery" 1100000 (void $ processConnectorDelivery connector.connectorClient connector.connectorDeliveries)
    , service "progress" 4000000 connector.connectorProgress
    ]
  where
    service name interval action = forever do
        tryAny action >>= \case
            Left _ -> connector.connectorReportFailure name >> threadDelay 5000000
            Right () -> pure ()
        threadDelay interval

-- | Keep web/session execution available without Telegram credentials or when
-- bot verification is unavailable. Uses the same recovery and cancellation
-- policy as the execution child of the full connector.
runSessionConnector :: ExecutionStore -> SessionBackend -> (Text -> IO ()) -> IO ()
runSessionConnector store backend reportFailure = forever do
    tryAny (processConnectorExecution store backend) >>= \case
        Left _ -> reportFailure "execution" >> threadDelay 5000000
        Right _ -> pure ()
    threadDelay 1000000

pollConnectorUpdates :: TelegramClient -> InboxStore context -> IO ()
pollConnectorUpdates client store = do
    offset <- store.readOffset
    result <- Telegram.telegramRequestOnce client "getUpdates" (object
        [ "offset" .= offset
        , "timeout" .= (5 :: Int)
        , "limit" .= (100 :: Int)
        , "allowed_updates" .=
            (["message", "edited_message", "message_reaction", "callback_query"] :: [Text])
        ]) 15
    values <- case result of
        Left _ -> fail "Telegram polling unavailable"
        Right bytes -> case eitherDecode bytes >>= Aeson.parseEither
            (withObject "Telegram response" (.: "result")) of
                Left _ -> fail "Invalid Telegram update batch"
                Right entries -> pure (entries :: [Value])
    batch <- traverse (\value -> case Aeson.parseMaybe
        (withObject "Telegram update" (.: "update_id")) value of
            Just identifier | identifier >= 0 -> pure (identifier, value)
            _ -> fail "Invalid Telegram update identifier") values
    let next = foldr (\(identifier, _) -> max (identifier + 1)) offset batch
    store.persistBatch batch next

processConnectorUpdate :: InboxStore context -> (context -> ConversationApplication binding) -> IO Bool
processConnectorUpdate store makeApplication = store.withNextUpdate \context payload ->
    case Json.decodeEither telegramUpdateDecoder (ByteString.toStrict (encode payload)) of
        Left _ -> fail "Invalid Telegram update"
        Right update -> forM_ (classifyPrivateConversationUpdate update) \(key, actor, event) -> do
            let application = makeApplication context
            binding <- application.authorizeConversation key actor
            case binding of
                Just admitted -> application.recordConversationEvent admitted update.updateId event
                Nothing -> case event of
                    ConversationMessage message -> application.linkConversation message
                    _ -> pure ()

-- | A conservative application admission policy. Standalone group routing
-- retains its explicit allowlist policy; neither may infer authorization from
-- a callback token, a forwarded message, or a claimed sender-chat.
classifyPrivateConversationUpdate
    :: TelegramUpdate -> Maybe (TelegramChatKey, Integer, ConversationEvent)
classifyPrivateConversationUpdate update
    | Just message <- update.updateMessage = messageEvent ConversationMessage message
    | Just message <- update.updateEditedMessage = messageEvent ConversationEdit message
    | Just reaction <- update.updateMessageReaction
    , Just actor <- reaction.messageReactionUser
    , validIdentifier reaction.messageReactionMessageId
    , privateSender reaction.messageReactionChat actor =
        Just (TelegramChatKey reaction.messageReactionChat.telegramChatId Nothing,
            actor.userId, ConversationReaction reaction)
    | Just callback <- update.updateCallbackQuery
    , Just message <- callback.callbackQueryMessage
    , validIdentifier message.messageId
    , privateSender message.messageChat callback.callbackQueryFrom
    , isJust callback.callbackQueryData =
        Just (TelegramChatKey message.messageChat.telegramChatId message.messageThread,
            callback.callbackQueryFrom.userId, ConversationCallback callback)
    | otherwise = Nothing
  where
    messageEvent constructor message = do
        actor <- message.messageFrom
        if privateSender message.messageChat actor && validIdentifier message.messageId
            then Just (TelegramChatKey message.messageChat.telegramChatId message.messageThread,
                actor.userId, constructor message)
            else Nothing
    privateSender chat actor =
        chat.telegramChatType == "private" && validIdentifier chat.telegramChatId
            && chat.telegramChatId == actor.userId && not actor.userIsBot
    validIdentifier identifier = identifier > 0 && identifier <= 9223372036854775807

processConnectorExecution :: ExecutionStore -> SessionBackend -> IO Bool
processConnectorExecution store backend =
    store.withNextExecution (advanceSessionExecution backend)

processConnectorDelivery :: TelegramClient -> DeliveryStore -> IO Bool
processConnectorDelivery client store =
    store.withNextDelivery (deliverConnectorMessage client)

-- | Exactly one transport attempt. The store supplies one already segmented
-- part, retaining deployed chunk boundaries across upgrades.
deliverConnectorMessage :: TelegramClient -> DeliveryAttempt -> IO DeliveryOutcome
deliverConnectorMessage client attempt = do
    result <- Telegram.telegramRequestOnce client "sendMessage" (prepareDeliveryPayload attempt) 20
    pure $ case result of
        Left failure
            | Just code <- failure.telegramErrorCode
            , code >= 400 && code < 500 -> DeliveryRejected
        Left _ -> DeliveryUncertain
        Right bytes -> case eitherDecode bytes >>= Aeson.parseEither
            (withObject "Telegram response" \response ->
                response .: "result" >>= withObject "Telegram message" (.: "message_id")) of
                    Right identifier | identifier > 0 -> DeliveryAcknowledged identifier
                    _ -> DeliveryUncertain

-- | Freeze formatting before persistence; prepared payloads are never rendered
-- or segmented again when a pending outbox part resumes after an upgrade.
prepareDeliveryPayload :: DeliveryAttempt -> Value
prepareDeliveryPayload (PreparedDeliveryAttempt payload) = payload
prepareDeliveryPayload DeliveryAttempt { deliveryChat, deliveryReplyTo, deliveryContent, deliveryKeyboard } =
    object $ messageAddressFields deliveryChat deliveryReplyTo
        <> boundedTextPresentationFields 2000 deliveryContent
        <> ["link_preview_options" .= object ["is_disabled" .= True]]
        <> maybe [] (\keyboard -> ["reply_markup" .= keyboard]) deliveryKeyboard

-- | Durable per-conversation queue driver. Acquisition and completion live in
-- the store adapter, while exception propagation, retry waiting, and ordered
-- advancement are shared between local and remote integrations.
data ConversationQueue action = ConversationQueue
    { nextConversationAction :: IO (Maybe action)
    , waitConversationAction :: action -> IO ()
    , performConversationAction :: action -> IO ()
    , failConversationAction :: action -> SomeException -> IO (Maybe Int)
    }

runConversationQueue :: ConversationQueue action -> IO ()
runConversationQueue queue = queue.nextConversationAction >>= \case
    Nothing -> pure ()
    Just action -> do
        queue.waitConversationAction action
        tryAny (queue.performConversationAction action) >>= \case
            Left exception -> do
                delay <- queue.failConversationAction action exception
                forM_ delay threadDelay
            Right () -> pure ()
        runConversationQueue queue
