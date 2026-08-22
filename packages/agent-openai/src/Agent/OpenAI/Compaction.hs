-- | Conversation compaction helpers shared by OpenAI remote compact and
-- xAI/OpenRouter local summarization.
module Agent.OpenAI.Compaction
    ( summaryPrefix
    , summarizationPrompt
    , estimateTokens
    , estimateItemsTokens
    , collectRecentUserTexts
    , buildLocalCompactedHistory
    , compactTranscriptAtLastCheckpoint
    , hasCompactionCheckpoint
    , assistantSummaryItem
    , userTextItem
    , isCompactSessionTurn
    , isClearSessionTurn
    , isNewSessionTurn
    , isTranscriptResetTurn
    , compactSessionUserText
    , clearSessionUserText
    , newSessionUserText
    ) where

import Agent.Responses.Types
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Maybe (mapMaybe)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding

-- | Marker prefix for compacted summary messages.
summaryPrefix :: Text
summaryPrefix = "Compacted conversation summary:"

-- | User-visible / persisted marker for a compact turn.
compactSessionUserText :: Maybe Text -> Text
compactSessionUserText focus = case focus of
    Just text | not (Text.null (Text.strip text)) ->
        "/compact " <> Text.strip text
    _ -> "/compact"

-- | Prompt used for local (Grok-style) summarization turns.
summarizationPrompt :: Maybe Text -> Text
summarizationPrompt focus =
    Text.unlines $
        [ "Summarize the conversation so far for a successor coding agent."
        , "The successor will only see this summary plus a few recent user messages;"
        , "it will not see prior tool calls or tool outputs."
        , "Preserve: the user's goals, important file paths, decisions made,"
        , "errors encountered and how they were fixed, and remaining work."
        , "Be concrete and concise. Do not call tools."
        ]
            <> case focus of
                Just text | not (Text.null (Text.strip text)) ->
                    [ ""
                    , "Additional focus from the user:"
                    , Text.strip text
                    ]
                _ -> []

estimateTokens :: Text -> Int
estimateTokens text = max 1 (Text.length text `div` 4)

estimateItemsTokens :: [ResponseItem] -> Int
estimateItemsTokens items =
    sum
        [ estimateTokens (TextEncoding.decodeUtf8 (LBS.toStrict (Aeson.encode item)))
        | item <- items
        ]

-- | Collect recent real user message texts (newest last), skipping /compact markers.
collectRecentUserTexts :: Int -> [ResponseItem] -> [Text]
collectRecentUserTexts keep items =
    reverse (take keep (reverse (mapMaybe userTextOf items)))
  where
    userTextOf = \case
        MessageItem message
            | message.role == RoleUser ->
                case messageText message of
                    Just text
                        | Text.isPrefixOf "/compact" (Text.strip text) -> Nothing
                        | otherwise -> Just text
                    Nothing -> Nothing
        _ -> Nothing

messageText :: ResponseMessage -> Maybe Text
messageText message = case message.content of
    MessageContentText text -> Just text
    MessageContentParts parts ->
        let texts =
                [ text
                | part <- parts
                , text <- case part of
                    InputTextPart { text } -> [text]
                    OutputTextPart { text } -> [text]
                    _ -> []
                ]
        in case texts of
            [] -> Nothing
            xs -> Just (Text.intercalate "\n" xs)

userTextItem :: Text -> ResponseItem
userTextItem text = MessageItem ResponseMessage
    { messageId = Nothing
    , content = MessageContentParts [InputTextPart text Nothing KeyMap.empty]
    , role = RoleUser
    , status = Nothing
    , phase = Nothing
    , extraFields = KeyMap.empty
    }

assistantSummaryItem :: Text -> ResponseItem
assistantSummaryItem summary =
    MessageItem ResponseMessage
        { messageId = Nothing
        , content =
            MessageContentParts
                [ OutputTextPart
                    (summaryPrefix <> "\n" <> Text.strip summary)
                    Nothing
                    Nothing
                    KeyMap.empty
                ]
        , role = RoleAssistant
        , status = Nothing
        , phase = Nothing
        , extraFields = KeyMap.empty
        }

-- | Grok-style local rebuild: recent user texts + assistant summary.
buildLocalCompactedHistory :: Int -> [ResponseItem] -> Text -> [ResponseItem]
buildLocalCompactedHistory keepRecent history summary =
    map userTextItem (collectRecentUserTexts keepRecent history)
        <> [assistantSummaryItem summary]

-- | Session turns that represent a compaction checkpoint.
isCompactSessionTurn :: Text -> Bool
isCompactSessionTurn text =
    let stripped = Text.strip text
    in stripped == "/compact" || Text.isPrefixOf "/compact " stripped

clearSessionUserText :: Text
clearSessionUserText = "/clear"

newSessionUserText :: Text
newSessionUserText = "/new"

isClearSessionTurn :: Text -> Bool
isClearSessionTurn text = Text.strip text == clearSessionUserText

isNewSessionTurn :: Text -> Bool
isNewSessionTurn text = Text.strip text == newSessionUserText

-- | Turns that replace the live transcript with their turnItems snapshot
-- (compact) or empty it (/clear, /new).
isTranscriptResetTurn :: Text -> Bool
isTranscriptResetTurn text =
    isCompactSessionTurn text
        || isClearSessionTurn text
        || isNewSessionTurn text

-- | An opaque server compaction item replaces every item before it.
compactTranscriptAtLastCheckpoint :: [ResponseItem] -> [ResponseItem]
compactTranscriptAtLastCheckpoint items = go [] (reverse items)
  where
    go _ [] = items
    go after (item : before) =
        case item of
            KnownResponseItem ItemCompaction _ -> item : after
            _ -> go (item : after) before

hasCompactionCheckpoint :: [ResponseItem] -> Bool
hasCompactionCheckpoint = any \case
    KnownResponseItem ItemCompaction _ -> True
    MessageItem message
        | message.role == RoleAssistant ->
            maybe False
                (Text.isPrefixOf summaryPrefix . Text.stripStart)
                (messageText message)
    _ -> False
