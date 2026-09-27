-- | Application input orchestration, after connector admission. Capabilities
-- must be built in the inbox transaction and scoped to its authorized binding.
-- No media preparation runs for duplicates, edits, reactions or controls.
module Agent.Telegram.Connector.Input
    ( InputCapabilities(..), processConversationInput ) where

import Agent.Telegram.Connector (ConversationEvent(..))
import Agent.Telegram.Connector.Media
import Agent.Telegram.Types.Wire
import Control.Applicative ((<|>))
import Control.Monad (unless)
import Data.Text (Text)
import qualified Data.Text as Text

data InputCapabilities = InputCapabilities
    { inputAlreadyRecorded :: TelegramMessage -> IO Bool
    , inputTranscribe :: TelegramVoice -> IO (Either Text Text)
    , inputPrepareMedia :: TelegramMessage -> [TelegramMedia] -> IO (Either Text Text)
    , inputPublish :: TelegramMessage -> Text -> IO ()
    , inputNotice :: TelegramMessage -> Text -> IO ()
    , inputInvalidText :: Text
    , inputUnsupported :: Text
    , inputStart :: TelegramMessage -> IO ()
    , inputCancel :: TelegramMessage -> IO ()
    , inputEdit :: TelegramMessage -> IO ()
    , inputReaction :: TelegramMessageReaction -> IO ()
    , inputCallback :: TelegramCallbackQuery -> IO ()
    }

processConversationInput :: InputCapabilities -> ConversationEvent -> IO ()
processConversationInput capabilities = \case
    ConversationEdit message -> capabilities.inputEdit message
    ConversationReaction reaction -> capabilities.inputReaction reaction
    ConversationCallback callback -> capabilities.inputCallback callback
    ConversationMessage message ->
        capabilities.inputAlreadyRecorded message >>= \seen -> unless seen $
            if message.messageText == Just "/cancel"
                then capabilities.inputCancel message
                else if maybe False ((== ["/start"]) . take 1 . Text.words) message.messageText
                    then capabilities.inputStart message
                    else case message.messageVoice of
                        Just voice -> capabilities.inputTranscribe voice >>= publish message
                        Nothing -> case messageMediaAttachments message of
                            media@(_ : _) -> capabilities.inputPrepareMedia message media >>= publish message
                            [] -> case message.messageText <|> message.messageCaption of
                                Just content -> publish message (Right content)
                                Nothing -> capabilities.inputNotice message capabilities.inputUnsupported
  where
    publish message = \case
        Left problem -> capabilities.inputNotice message problem
        Right content
            | Text.null (Text.strip content) || Text.length content > 16000 || Text.any (== '\NUL') content ->
                capabilities.inputNotice message capabilities.inputInvalidText
            | otherwise -> capabilities.inputPublish message (Text.strip content)
