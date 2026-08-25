-- | Conversation compaction helpers shared by OpenAI remote compact and
-- xAI/OpenRouter local summarization.
module Agent.OpenAI.Compaction
    ( summaryPrefix
    , summarizationPrompt
    , remoteCompactionRetainedTokenBudget
    , remoteCompactionMaxStringLength
    , compactionTriggerItem
    , buildRemoteCompactionRequest
    , trimRemoteCompactionHistoryToFit
    , extractRemoteCompactionItem
    , buildRemoteCompactedHistory
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

import Agent.Responses.LoopBackend (withRequestInput)
import Agent.Responses.Types
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding

-- | Marker prefix for compacted summary messages.
summaryPrefix :: Text
summaryPrefix = "Compacted conversation summary:"

-- | Matches the retained-message budget used by Codex remote compaction v2.
remoteCompactionRetainedTokenBudget :: Int
remoteCompactionRetainedTokenBudget = 64_000

-- | Maximum length accepted for an individual string in a compaction input.
remoteCompactionMaxStringLength :: Int
remoteCompactionMaxStringLength = 1_048_576

maxRetainedAgentMessageTokens :: Int
maxRetainedAgentMessageTokens = 10_000

-- | The exact sentinel understood by the remote compaction v2 protocol.
compactionTriggerItem :: ResponseItem
compactionTriggerItem =
    CompactionTriggerItemValue CompactionTriggerItem
        { extraFields = KeyMap.empty
        }

-- | Build a normal streaming Responses request whose final input item asks the
-- model to emit an opaque compaction checkpoint.
buildRemoteCompactionRequest
    :: ResponseCreateParams
    -> [ResponseItem]
    -> ResponseCreateParams
buildRemoteCompactionRequest params history =
    case withRequestInput params (history <> [compactionTriggerItem]) of
        ResponseCreateParams{..} ->
            ResponseCreateParams
                { parallelToolCalls = Just True
                , previousResponseId = Nothing
                , store = Just False
                , stream = Just True
                , toolChoice = Just (ToolChoiceMode ToolChoiceAuto)
                , ..
                }

contextWindowTruncatedOutputMessage :: Text
contextWindowTruncatedOutputMessage =
    "Output exceeded the available model context and was truncated"

-- | Codex rewrites only a contiguous suffix of tool outputs when the
-- compaction request itself would exceed the model's usable context window.
-- This keeps call/output pairing valid while making room for the trigger.
trimRemoteCompactionHistoryToFit
    :: Int
    -> Maybe Text
    -> [ResponseItem]
    -> [ResponseItem]
trimRemoteCompactionHistoryToFit contextWindow instructionText history =
    go initialTokens (reverse sanitizedHistory) []
  where
    sanitizedHistory = map sanitizeOversizedToolCall history

    initialTokens =
        maybe 0 estimateTokens instructionText
            + estimateItemsTokens sanitizedHistory

    go _ [] rewritten = rewritten
    go tokens remaining rewritten
        | tokens <= contextWindow =
            reverse remaining <> rewritten
    go tokens (item : remaining) rewritten =
        case rewriteOversizedToolOutput item of
            Nothing ->
                reverse (item : remaining) <> rewritten
            Just replacement ->
                let nextTokens =
                        tokens
                            - estimateItemsTokens [item]
                            + estimateItemsTokens [replacement]
                in go nextTokens remaining (replacement : rewritten)

oversizedToolArgumentsMessage :: Text
oversizedToolArgumentsMessage =
    "Tool arguments exceeded the provider string limit and were omitted during compaction."

oversizedFunctionArguments :: Text
oversizedFunctionArguments =
    TextEncoding.decodeUtf8 . LBS.toStrict . Aeson.encode $ Aeson.object
        [ "compaction_notice" Aeson..= oversizedToolArgumentsMessage
        ]

sanitizeOversizedToolCall :: ResponseItem -> ResponseItem
sanitizeOversizedToolCall = \case
    FunctionCallItem call
        | Text.length call.arguments > remoteCompactionMaxStringLength ->
            FunctionCallItem FunctionCall
                { itemId = call.itemId
                , callId = call.callId
                , name = call.name
                , namespace = call.namespace
                , arguments = oversizedFunctionArguments
                , encryptedFunctionArgs = call.encryptedFunctionArgs
                , status = call.status
                , extraFields = call.extraFields
                }
    CustomToolCallItem call
        | Text.length call.input > remoteCompactionMaxStringLength ->
            CustomToolCallItem CustomToolCall
                { itemId = call.itemId
                , callId = call.callId
                , name = call.name
                , namespace = call.namespace
                , input = oversizedToolArgumentsMessage
                , status = call.status
                , extraFields = call.extraFields
                }
    item -> item

rewriteOversizedToolOutput :: ResponseItem -> Maybe ResponseItem
rewriteOversizedToolOutput = \case
    FunctionCallOutputItem output ->
        Just $ FunctionCallOutputItem FunctionCallOutput
            { itemId = output.itemId
            , callId = output.callId
            , name = output.name
            , namespace = output.namespace
            , output = Aeson.String contextWindowTruncatedOutputMessage
            , status = output.status
            , extraFields = output.extraFields
            }
    CustomToolCallOutputItem output ->
        Just $ CustomToolCallOutputItem CustomToolCallOutput
            { itemId = output.itemId
            , callId = output.callId
            , name = output.name
            , output = Aeson.String contextWindowTruncatedOutputMessage
            , status = output.status
            , extraFields = output.extraFields
            }
    ToolSearchOutputItem output ->
        Just $ ToolSearchOutputItem ToolSearchOutput
            { itemId = output.itemId
            , callId = output.callId
            , status = output.status
            , execution = output.execution
            , tools = []
            , extraFields = output.extraFields
            }
    _ -> Nothing

-- | A successful v2 stream must complete and contain exactly one compaction
-- output item. Other output item types are ignored, matching Codex.
extractRemoteCompactionItem :: Response -> Either Text ResponseItem
extractRemoteCompactionItem response
    | response.status /= ResponseCompleted =
        Left $
            "remote compaction v2 expected response.completed, got "
                <> Text.pack (show response.status)
    | otherwise =
        case
            [ item
            | item@(CompactionItemValue _) <- response.output
            ]
        of
            [item] -> Right item
            items ->
                Left $
                    "remote compaction v2 expected exactly one compaction "
                        <> "output item, got "
                        <> Text.pack (show (length items))
                        <> " from "
                        <> Text.pack (show (length response.output))
                        <> " output items"

-- | Install the newest eligible user/inter-agent messages under the retained
-- token budget, followed by the opaque checkpoint. Ordinary assistant output,
-- reasoning, tool calls/results, old checkpoints, and generated system or
-- developer messages are represented by the checkpoint and are discarded.
buildRemoteCompactedHistory
    :: Int
    -> [ResponseItem]
    -> ResponseItem
    -> [ResponseItem]
buildRemoteCompactedHistory budget history checkpoint =
    truncateRetainedGroups budget
        (filter (\group -> isRemoteRetainedItem group.retainedSource)
            (retainedGroups history))
        <> [checkpoint]

data RetainedGroup = RetainedGroup
    { retainedSource :: !ResponseItem
    , retainedNotice :: !(Maybe ResponseItem)
    }

retainedGroups :: [ResponseItem] -> [RetainedGroup]
retainedGroups = \case
    [] -> []
    source : notice : rest
        | isImageResizeNotice notice ->
            RetainedGroup source (Just notice) : retainedGroups rest
    source : rest ->
        RetainedGroup source Nothing : retainedGroups rest

isImageResizeNotice :: ResponseItem -> Bool
isImageResizeNotice = \case
    MessageItem message
        | message.role == RoleDeveloper ->
            maybe False
                (Text.isPrefixOf "<image_resize_notice>" . Text.stripStart)
                (messageText message)
    _ -> False

-- The Haskell harness currently lacks Codex's per-item metadata sidecar, so
-- recognize the contextual user wrappers it generates itself. Their contents
-- are already represented by the opaque checkpoint and, where applicable,
-- reinjected from current session state.
isGeneratedContextUserText :: Text -> Bool
isGeneratedContextUserText text =
    any (`Text.isPrefixOf` Text.stripStart text)
        [ "# AGENTS.md instructions for "
        , "# Skill instructions: "
        , "Plan mode is active. Do not make any edits or writes to the system except for the plan file."
        , "The user approved the plan. Plan mode is now off."
        , "<subagent_notification>"
        ]

isRemoteRetainedItem :: ResponseItem -> Bool
isRemoteRetainedItem = \case
    MessageItem message ->
        message.role == RoleUser
            && maybe True
                (not . isGeneratedContextUserText)
                (messageText message)
    AgentMessageItem message ->
        not (isDiscardedAgentMessage message)
            && itemTokenCount (AgentMessageItem message)
                <= maxRetainedAgentMessageTokens
    _ -> False

isDiscardedAgentMessage :: ResponseAgentMessage -> Bool
isDiscardedAgentMessage message =
    let firstText =
            listToMaybe
                [ text
                | InputTextPart { text } <- message.content
                ]
        descendantProgress =
            case (message.author, message.recipient, firstText) of
                (Just author, Just recipient, Just text) ->
                    Text.isPrefixOf (recipient <> "/") author
                        && Text.isPrefixOf "Message Type: MESSAGE\n" text
                _ -> False
        completion =
            maybe False
                (Text.isPrefixOf "Message Type: FINAL_ANSWER\n")
                firstText
    in descendantProgress || completion

truncateRetainedGroups :: Int -> [RetainedGroup] -> [ResponseItem]
truncateRetainedGroups maxTokens groups =
    concatMap groupItems (go (max 0 maxTokens) (reverse groups) [])
  where
    go _ [] selected = selected
    go 0 _ selected = selected
    go remaining (group : rest) selected
        | tokenCount <= remaining =
            go (remaining - tokenCount) rest (group : selected)
        | remaining > noticeTokens
        , Just source <- truncateItemText
            (remaining - noticeTokens)
            group.retainedSource =
                RetainedGroup source group.retainedNotice : selected
        | otherwise =
            go remaining rest selected
      where
        noticeTokens = maybe 0 itemTokenCount group.retainedNotice
        tokenCount =
            itemTokenCount group.retainedSource + noticeTokens

groupItems :: RetainedGroup -> [ResponseItem]
groupItems group =
    group.retainedSource : maybe [] pure group.retainedNotice

itemTokenCount :: ResponseItem -> Int
itemTokenCount = \case
    MessageItem message -> max 1 (messageTokenCount message)
    item -> estimateItemsTokens [item]

messageTokenCount :: ResponseMessage -> Int
messageTokenCount message = case message.content of
    MessageContentText text -> estimateTokens text
    MessageContentParts parts ->
        sum (map contentPartTokenCount parts)

contentPartTokenCount :: ResponseContentPart -> Int
contentPartTokenCount = \case
    InputTextPart { text } -> estimateTokens text
    OutputTextPart { text } -> estimateTokens text
    RefusalPart { refusal } -> estimateTokens refusal
    ReasoningTextPart { text } -> estimateTokens text
    SummaryTextPart { text } -> estimateTokens text
    PlainTextPart { text } -> estimateTokens text
    _ -> 0

truncateItemText :: Int -> ResponseItem -> Maybe ResponseItem
truncateItemText budget = \case
    MessageItem message ->
        MessageItem <$> truncateMessageText budget message
    _ -> Nothing

truncateMessageText :: Int -> ResponseMessage -> Maybe ResponseMessage
truncateMessageText budget message =
    case message.content of
        MessageContentText text ->
            let truncated = takeTokenBudget budget text
            in if Text.null truncated
                then Nothing
                else Just (replaceMessageContent
                    message
                    (MessageContentText truncated))
        MessageContentParts parts ->
            let truncated = truncateContentParts budget parts
            in if null truncated
                then Nothing
                else Just (replaceMessageContent
                    message
                    (MessageContentParts truncated))

replaceMessageContent :: ResponseMessage -> MessageContent -> ResponseMessage
replaceMessageContent message nextContent =
    ResponseMessage
        { messageId = message.messageId
        , content = nextContent
        , role = message.role
        , status = message.status
        , phase = message.phase
        , passthrough = message.passthrough
        , extraFields = message.extraFields
        }

truncateContentParts :: Int -> [ResponseContentPart] -> [ResponseContentPart]
truncateContentParts initialBudget = go (max 0 initialBudget)
  where
    go _ [] = []
    go remaining (part : rest) =
        case truncateTextPart remaining part of
            TextPartNonText ->
                part : go remaining rest
            TextPartDropped ->
                go remaining rest
            TextPartKept used truncated ->
                truncated : go (remaining - used) rest

data TruncatedTextPart
    = TextPartNonText
    | TextPartDropped
    | TextPartKept !Int !ResponseContentPart

truncateTextPart :: Int -> ResponseContentPart -> TruncatedTextPart
truncateTextPart budget part =
    case partText part of
        Nothing -> TextPartNonText
        Just text
            | budget <= 0 -> TextPartDropped
            | otherwise ->
                let tokens = estimateTokens text
                    used = min budget tokens
                    truncated = replacePartText (takeTokenBudget used text) part
                in if Text.null (partTextValue truncated)
                    then TextPartDropped
                    else TextPartKept used truncated

partText :: ResponseContentPart -> Maybe Text
partText = \case
    InputTextPart { text } -> Just text
    OutputTextPart { text } -> Just text
    RefusalPart { refusal } -> Just refusal
    ReasoningTextPart { text } -> Just text
    SummaryTextPart { text } -> Just text
    PlainTextPart { text } -> Just text
    _ -> Nothing

partTextValue :: ResponseContentPart -> Text
partTextValue = maybe "" id . partText

replacePartText :: Text -> ResponseContentPart -> ResponseContentPart
replacePartText value = \case
    InputTextPart { promptCacheBreakpoint, extraFields } ->
        InputTextPart value promptCacheBreakpoint extraFields
    OutputTextPart { annotations, logprobs, extraFields } ->
        OutputTextPart value annotations logprobs extraFields
    RefusalPart { extraFields } ->
        RefusalPart value extraFields
    ReasoningTextPart { extraFields } ->
        ReasoningTextPart value extraFields
    SummaryTextPart { extraFields } ->
        SummaryTextPart value extraFields
    PlainTextPart { extraFields } ->
        PlainTextPart value extraFields
    part -> part

takeTokenBudget :: Int -> Text -> Text
takeTokenBudget tokens =
    Text.take (max 0 tokens * 4)

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
    , passthrough = Nothing
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
        , passthrough = Nothing
        , extraFields = KeyMap.empty
        }

-- | Grok-style local rebuild: recent user texts + assistant summary.
buildLocalCompactedHistory :: Int -> [ResponseItem] -> Text -> [ResponseItem]
buildLocalCompactedHistory keepRecent history summary =
    map userTextItem (collectRecentUserTexts keepRecent history)
        <> [assistantSummaryItem summary]

-- | Legacy helper retained for API compatibility. Installed v2 snapshots must
-- be replayed in full because they intentionally place retained messages
-- before the checkpoint; new loop code therefore no longer calls this.
compactTranscriptAtLastCheckpoint :: [ResponseItem] -> [ResponseItem]
compactTranscriptAtLastCheckpoint items = go [] (reverse items)
  where
    go _ [] = items
    go after (item : before) =
        case item of
            CompactionItemValue{} -> item : after
            _ -> go (item : after) before

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

hasCompactionCheckpoint :: [ResponseItem] -> Bool
hasCompactionCheckpoint = any \case
    CompactionItemValue{} -> True
    MessageItem message
        | message.role == RoleAssistant ->
            maybe False
                (Text.isPrefixOf summaryPrefix . Text.stripStart)
                (messageText message)
    _ -> False
