-- | Provider-neutral messages exchanged between a root agent and subagents.
--
-- OpenAI collaboration tools may return encrypted message fields. The opaque
-- payload must stay typed until the OpenAI adapter emits an @agent_message@
-- item with an @encrypted_content@ part; treating it as ordinary user text
-- prevents the child model from decrypting it.
module Agent.InterAgentMessage
    ( InterAgentMessage(..)
    , InterAgentMessageContent(..)
    , InterAgentMessageType(..)
    , encryptedInterAgentContent
    , interAgentMessagePayload
    , plainInterAgentContent
    , renderInterAgentMessage
    , renderInterAgentMessageHeader
    ) where

import Data.Text (Text)
import qualified Data.Text as Text

data InterAgentMessageType
    = NewTaskMessage
    | QueuedMessage
    | FollowUpMessage
    deriving (Eq, Show)

data InterAgentMessageContent
    = PlainInterAgentContent !Text
    | EncryptedInterAgentContent !Text
    deriving (Eq)

instance Show InterAgentMessageContent where
    show = \case
        PlainInterAgentContent text ->
            "PlainInterAgentContent " <> show text
        EncryptedInterAgentContent _ ->
            "EncryptedInterAgentContent <redacted>"

data InterAgentMessage = InterAgentMessage
    { messageAuthor :: !Text
    , messageRecipient :: !Text
    , messageType :: !InterAgentMessageType
    , messageContent :: !InterAgentMessageContent
    } deriving (Eq)

instance Show InterAgentMessage where
    show message =
        "InterAgentMessage { messageAuthor = " <> show message.messageAuthor
            <> ", messageRecipient = " <> show message.messageRecipient
            <> ", messageType = " <> show message.messageType
            <> ", messageContent = " <> show message.messageContent
            <> " }"

plainInterAgentContent :: Text -> InterAgentMessageContent
plainInterAgentContent = PlainInterAgentContent

encryptedInterAgentContent :: Text -> InterAgentMessageContent
encryptedInterAgentContent = EncryptedInterAgentContent

renderInterAgentMessageHeader :: InterAgentMessage -> Text
renderInterAgentMessageHeader message =
    Text.unlines $
        [ "Message Type: " <> messageTypeText message.messageType
        ]
        <> taskLine
        <> [ "Sender: " <> message.messageAuthor
           , "Payload:"
           ]
  where
    taskLine = case message.messageType of
        NewTaskMessage -> ["Task name: " <> message.messageRecipient]
        QueuedMessage -> []
        FollowUpMessage -> []

renderInterAgentMessage :: InterAgentMessage -> Text
renderInterAgentMessage message =
    renderInterAgentMessageHeader message <> interAgentMessagePayload message

interAgentMessagePayload :: InterAgentMessage -> Text
interAgentMessagePayload message = case message.messageContent of
    PlainInterAgentContent text -> text
    -- Non-OpenAI transports do not currently advertise encrypted
    -- collaboration fields. Preserve the token rather than dropping it if
    -- one nevertheless reaches this fallback.
    EncryptedInterAgentContent text -> text

messageTypeText :: InterAgentMessageType -> Text
messageTypeText = \case
    NewTaskMessage -> "NEW_TASK"
    QueuedMessage -> "MESSAGE"
    FollowUpMessage -> "FOLLOW_UP"
