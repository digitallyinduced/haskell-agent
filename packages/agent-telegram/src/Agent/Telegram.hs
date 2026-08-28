-- | Dedicated Telegram gateway backed by persisted agent sessions.
module Agent.Telegram
    ( telegramMain
    , parseAllowedUsers
    , splitTelegramText
    , markdownToTelegramHtml
    , withTelegramProgressUsing
    , TelegramConfig(..)
    , TelegramCommand(..)
    , TelegramSetupOptions(..)
    , defaultTelegramSetupOptions
    , parseTelegramArgs
    , TelegramUsersCommand(..)
    , TelegramChatKey(..)
    , TelegramState(..)
    , TelegramPendingTurn(..)
    , TelegramPendingReply(..)
    , TelegramVoice(..)
    , TelegramUser(..)
    , TelegramMessage(..)
    , TelegramUpdate(..)
    , TelegramUpdateAction(..)
    , PendingChatAction(..)
    , emptyTelegramState
    , classifyTelegramUpdate
    , classifyTelegramUpdateWithMode
    , groupJoinAuthorized
    , isAnonymousAdmin
    , telegramAnonymousAdminUserId
    , storeUpdateAction
    , nextPendingAction
    , checkpointPendingVoiceTranscript
    , reactionMessageText
    , telegramReactionEmoji
    , telegramReplyText
    , telegramAgentPrompt
    , telegramCommandArguments
    , telegramReplyUserIdFromPrompt
    , telegramUserLabel
    , recordSeenTelegramUsers
    , recordLatestInboundMessage
    , resolveTelegramUser
    , TelegramUserResolution(..)
    , transcribeWithXAI
    , downloadTelegramMediaAttachmentsWith
    ) where

import Agent.Telegram.Internal.App
import Agent.Telegram.Internal.Support (withTelegramProgressUsing)
import Agent.Telegram.Internal.Text (parseAllowedUsers, splitTelegramText)
import Agent.Telegram.Internal.Turn
    ( downloadTelegramMediaAttachmentsWith
    , telegramAgentPrompt
    )
import Agent.Telegram.Markdown (markdownToTelegramHtml)
import Agent.Telegram.Types
import Agent.Telegram.Classify
import Agent.Telegram.Voice (transcribeWithXAI)
