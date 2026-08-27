-- | Resolution and durable observation of Telegram users.
module Agent.Telegram.Classify.User
    ( TelegramUserResolution(..)
    , resolveTelegramUser
    , grantableTelegramUser
    , recordSeenTelegramUsers
    , messageMentionEntities
    , mentionedTelegramUsers
    ) where

import Agent.Telegram.Types
import Control.Applicative ((<|>))
import Data.Char (isDigit)
import Data.List (foldl', nubBy)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Read (readMaybe)

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

telegramUserDisplayName :: TelegramUser -> Text
telegramUserDisplayName user =
    Text.unwords
        [ value
        | Just value <- [user.userFirstName, user.userLastName]
        , not (Text.null (Text.strip value))
        ]

grantableTelegramUser :: TelegramUser -> Bool
grantableTelegramUser user =
    not user.userIsBot
        && user.userId /= telegramAnonymousAdminUserId
        && user.userId > 0

telegramAnonymousAdminUserId :: Integer
telegramAnonymousAdminUserId = 1087968824

stubTelegramUser :: Integer -> TelegramUser
stubTelegramUser userId =
    TelegramUser
        { userId
        , userIsBot = False
        , userFirstName = Nothing
        , userLastName = Nothing
        , userUsername = Nothing
        }

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

mentionedTelegramUsers :: TelegramMessage -> [TelegramUser]
mentionedTelegramUsers message =
    nubBy (\left right -> left.userId == right.userId) $
        mapMaybe snd (messageMentionEntities message)

messageMentionEntities :: TelegramMessage -> [(Text, Maybe TelegramUser)]
messageMentionEntities message =
    let text = fromMaybe "" (message.messageText <|> message.messageCaption)
        entities = message.messageEntities <> message.messageCaptionEntities
    in
        [ (utf16Slice entity.entityOffset entity.entityLength text, entity.entityUser)
        | entity <- entities
        , entity.entityType == "mention" || entity.entityType == "text_mention"
        ]

utf16Slice :: Int -> Int -> Text -> Text
utf16Slice offset length = utf16Take length . utf16Drop offset

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
