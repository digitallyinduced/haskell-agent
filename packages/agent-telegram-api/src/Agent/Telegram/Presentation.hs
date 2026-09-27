-- | Pure Telegram request preparation. Transport, retries, segmentation, and
-- authorization remain the caller's responsibility.
module Agent.Telegram.Presentation
    ( TelegramTextPresentation (..)
    , textPresentationFields
    , boundedTextPresentationFields
    , messageAddressFields
    , inlineKeyboardMarkup
    ) where

import Agent.Telegram.Markdown (markdownToTelegramHtml, telegramRenderedLength)
import Agent.Telegram.Types.State (TelegramChatKey (..))
import Data.Aeson (Value, object, (.=))
import Data.Aeson.Key (Key)
import Data.Text (Text)

data TelegramTextPresentation = PlainText | HtmlText | RichText
    deriving (Eq, Show)

textPresentationFields :: TelegramTextPresentation -> Text -> [(Key, Value)]
textPresentationFields PlainText content = ["text" .= content]
textPresentationFields HtmlText content =
    [ "text" .= markdownToTelegramHtml content
    , "parse_mode" .= ("HTML" :: Text)
    ]
textPresentationFields RichText content =
    ["rich_message" .= object ["html" .= markdownToTelegramHtml content]]

-- | Select plain text before transmission if formatting expands beyond the
-- caller's rendered Unicode-scalar budget. This neither splits nor truncates:
-- the caller must already bound the raw content. In particular, a budget of
-- 2000 scalars leaves room for supplementary characters under Telegram's
-- 4096 UTF-16-unit limit. Never use a failed send to select another payload.
boundedTextPresentationFields :: Int -> Text -> [(Key, Value)]
boundedTextPresentationFields maximumRenderedScalars content =
    textPresentationFields
        (if telegramRenderedLength content <= maximumRenderedScalars then HtmlText else PlainText)
        content

messageAddressFields :: TelegramChatKey -> Maybe Integer -> [(Key, Value)]
messageAddressFields key replyIdentifier =
    ["chat_id" .= key.chatId]
        <> maybe [] (\identifier -> ["message_thread_id" .= identifier]) key.messageThreadId
        <> maybe [] (\identifier ->
            ["reply_parameters" .= object
                [ "message_id" .= identifier
                , "allow_sending_without_reply" .= True
                ]]) replyIdentifier

inlineKeyboardMarkup :: [[(Text, Text)]] -> Value
inlineKeyboardMarkup rows = object
    ["inline_keyboard" .=
        [[object ["text" .= label, "callback_data" .= callbackData]
          | (label, callbackData) <- row] | row <- rows]]
