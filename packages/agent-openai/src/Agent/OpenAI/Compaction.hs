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
    , trimRemoteCompactionRequestToFit
    , extractRemoteCompactionItem
    , buildRemoteCompactedHistory
    , estimateTokens
    , estimateItemsTokens
    , estimateResponseCreateParamsTokens
    , estimateRequestTokensWithItems
    , resizedImageBytesEstimate
    , trimResponseHistoryToFit
    , sanitizeCompactionHistory
    , collectRecentUserTexts
    , buildLocalCompactedHistory
    , buildLocalCompactedHistoryToFit
    , compactTranscriptAtLastCheckpoint
    , hasCompactionCheckpoint
    , hasReloadedGeneratedContextItems
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

import Agent.OpenAI.ModelMetadata (isCodexResponsesLiteModel)
import Agent.Responses.LoopBackend (withRequestInput)
import Agent.Responses.Types
import Agent.Json (RawJson, rawJsonFromEncoding)
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

-- | Build a normal streaming Responses request whose final input item asks the
-- model to emit an opaque compaction checkpoint.
--
-- Regular Codex compaction enables parallel tool calls. Responses Lite
-- rejects that value, so keep the flag false for sol/terra/luna.
buildRemoteCompactionRequest
    :: ResponseCreateParams
    -> [ResponseItem]
    -> ResponseCreateParams
buildRemoteCompactionRequest params history =
    case withRequestInput params (history <> [compactionTriggerItem]) of
        ResponseCreateParams{..} ->
            ResponseCreateParams
                { parallelToolCalls =
                    Just (not (maybe False isCodexResponsesLiteModel model))
                , previousResponseId = Nothing
                , store = Just False
                , stream = Just True
                , toolChoice = Just (ToolChoiceMode ToolChoiceAuto)
                , ..
                }

-- | Estimate the complete serialized request, including instructions, tools,
-- and all other request-level fields. @input_image.image_url@ data URLs are
-- counted as vision tokens rather than as base64 text; see
-- 'estimateEncodedValue'.
estimateRequestTokensWithItems
    :: ResponseCreateParams
    -> [ResponseItem]
    -> Int
estimateRequestTokensWithItems params items =
    estimateEncodedValue (withRequestInput params items)

estimateResponseCreateParamsTokens :: ResponseCreateParams -> Int
estimateResponseCreateParamsTokens = estimateEncodedValue

-- | Estimate serialized JSON at four characters per token, matching the rest
-- of compaction accounting. Base64 payloads on @input_image.image_url@ are
-- replaced with 'resizedImageBytesEstimate' because the model consumes those
-- images as vision tokens, not as the transport encoding. Ordinary strings
-- that happen to contain a data URL, including tool-output text, stay at raw
-- size.
estimateEncodedValue :: Aeson.ToJSON value => value -> Int
estimateEncodedValue value =
    estimateAdjustedJsonTokens (Aeson.toJSON value)

estimateAdjustedJsonTokens :: Aeson.Value -> Int
estimateAdjustedJsonTokens json =
    let encoded =
            TextEncoding.decodeUtf8 (LBS.toStrict (Aeson.encode json))
        (payloadBytes, replacementBytes) = mediaEstimateAdjustment json
        adjusted =
            max 0 (Text.length encoded - payloadBytes) + replacementBytes
    in max 1 (adjusted `div` 4)

-- | Approximate model-visible byte cost for one inline image, matching Codex.
-- Four bytes per token maps this to about 1,843 tokens. Attachments from this
-- harness use @detail: "auto"@, so original-detail patch counting is unused.
resizedImageBytesEstimate :: Int
resizedImageBytesEstimate = 7_373

mediaEstimateAdjustment :: Aeson.Value -> (Int, Int)
mediaEstimateAdjustment = \case
    Aeson.Array values ->
        foldl'
            (\acc value -> addPair acc (mediaEstimateAdjustment value))
            (0, 0)
            values
    Aeson.Object fields ->
        let isInputImage =
                KeyMap.lookup "type" fields == Just (Aeson.String "input_image")
        in foldl'
            (\acc (key, value) ->
                addPair acc (fieldAdjustment isInputImage key value))
            (0, 0)
            (KeyMap.toList fields)
    _ ->
        (0, 0)
  where
    addPair (payloadAcc, replacementAcc) (payload, replacement) =
        (payloadAcc + payload, replacementAcc + replacement)

    fieldAdjustment isInputImage key value
        | isInputImage && key == "image_url" =
            imageUrlAdjustment value
        | otherwise =
            mediaEstimateAdjustment value

imageUrlAdjustment :: Aeson.Value -> (Int, Int)
imageUrlAdjustment = \case
    Aeson.String text ->
        case parseBase64ImageDataUrl text of
            Just payload ->
                (Text.length payload, resizedImageBytesEstimate)
            Nothing ->
                (0, 0)
    _ ->
        (0, 0)

-- | Return the base64 payload of a @data:image/...;base64,...@ URL.
-- Hosted HTTP(S) image URLs and non-image data URLs stay at raw size.
parseBase64ImageDataUrl :: Text -> Maybe Text
parseBase64ImageDataUrl url
    | not (hasInsensitivePrefix "data:" url) = Nothing
    | otherwise =
        case Text.break (== ',') (Text.drop 5 url) of
            (_, payload)
                | Text.null payload -> Nothing
            (metadata, payload) ->
                let parts = Text.splitOn ";" metadata
                    mime = case parts of
                        (value : _) -> value
                        [] -> Text.empty
                    hasBase64 =
                        any (\part -> Text.toLower part == "base64") parts
                in if hasBase64 && hasInsensitivePrefix "image/" mime
                    then Just (Text.drop 1 payload)
                    else Nothing

hasInsensitivePrefix :: Text -> Text -> Bool
hasInsensitivePrefix prefix text =
    Text.toLower (Text.take (Text.length prefix) text) == Text.toLower prefix

contextWindowTruncatedOutputMessage :: Text
contextWindowTruncatedOutputMessage =
    "Output exceeded the available model context and was truncated"

-- | Rewrite the oldest oversized, safely-rewritable items when the compaction
-- request itself would exceed the model's usable context window. Items that
-- cannot be rewritten are preserved while newer outputs are considered.
trimRemoteCompactionHistoryToFit
    :: Int
    -> Maybe Text
    -> [ResponseItem]
    -> [ResponseItem]
trimRemoteCompactionHistoryToFit contextWindow instructionText history =
    trimCompactionHistoryToFitWith
        sanitizeRemoteCompactionHistory
        contextWindow
        requestTokens
        history
  where
    requestTokens items =
        maybe 0 estimateTokens instructionText
            + estimateItemsTokens (items <> [compactionTriggerItem])

-- | Trim a compaction request using the complete serialized request size.
-- Unlike the legacy helper above, this accounts for tools, instructions,
-- trigger overhead, and all other request fields preserved by the remote
-- compaction request.
trimRemoteCompactionRequestToFit
    :: Int
    -> ResponseCreateParams
    -> [ResponseItem]
    -> [ResponseItem]
trimRemoteCompactionRequestToFit contextWindow params =
    trimCompactionHistoryToFitWith
        sanitizeRemoteCompactionHistory
        contextWindow
        requestTokens
  where
    requestTokens history =
        estimateEncodedValue (buildRemoteCompactionRequest params history)

-- | Trim history for a normal Responses request with fixed trailing items.
-- Local summarization uses this to bound the transcript before appending its
-- summary prompt.
trimResponseHistoryToFit
    :: Int
    -> ResponseCreateParams
    -> [ResponseItem]
    -> [ResponseItem]
    -> [ResponseItem]
trimResponseHistoryToFit contextWindow params trailing =
    trimCompactionHistoryToFitWith
        sanitizeCompactionHistory
        contextWindow
        requestTokens
  where
    requestTokens history =
        estimateRequestTokensWithItems params (history <> trailing)

trimCompactionHistoryToFitWith
    :: ([ResponseItem] -> [ResponseItem])
    -> Int
    -> ([ResponseItem] -> Int)
    -> [ResponseItem]
    -> [ResponseItem]
trimCompactionHistoryToFitWith sanitize contextWindow requestTokens history =
    let sanitized = sanitize history
        rewritten = rewriteUntilFit sanitized
    in dropOldestUntilFit rewritten
  where
    rewriteUntilFit items
        | requestTokens items <= contextWindow = items
        | otherwise =
            case rewriteFirstReducible [] items of
                Just smaller -> rewriteUntilFit smaller
                Nothing -> items

    rewriteFirstReducible _ [] = Nothing
    rewriteFirstReducible prefix (item : remaining) =
        let withoutItem = prefix <> remaining
            availableTokens =
                max 0 (contextWindow - requestTokens withoutItem)
            original = prefix <> (item : remaining)
        in case rewriteItemForBudget availableTokens item of
            Just compacted
                | let candidate = prefix <> (compacted : remaining)
                , requestTokens candidate < requestTokens original ->
                    Just candidate
            _ ->
                rewriteFirstReducible (prefix <> [item]) remaining

    dropOldestUntilFit items
        | requestTokens items <= contextWindow = items
        | otherwise =
            case dropOldestProtocolUnit items of
                Nothing -> items
                Just smaller -> dropOldestUntilFit smaller

dropOldestProtocolUnit :: [ResponseItem] -> Maybe [ResponseItem]
dropOldestProtocolUnit = go []
  where
    go _ [] = Nothing
    go prefix (item@(KnownResponseItem ItemCompaction _) : rest) =
        go (prefix <> [item]) rest
    go prefix (FunctionCallItem call : rest) =
        Just (prefix <> dropMatchingFunctionOutput call.callId rest)
    go prefix (CustomToolCallItem call : rest) =
        Just (prefix <> dropMatchingCustomToolOutput call.callId rest)
    go prefix (KnownResponseItem itemType tagged : rest)
        | Just outputType <- pairedOutputType itemType =
            Just $
                prefix
                    <> dropMatchingTaggedOutput
                        (== outputType)
                        (taggedProtocolIds tagged)
                        rest
    go prefix (UnknownResponseItem tagged : rest)
        | Just outputTag <- pairedUnknownOutputTag tagged.tag =
            Just $
                prefix
                    <> dropMatchingTaggedOutput
                        (\case
                            ItemUnknownType itemType -> itemType == outputTag
                            _ -> False)
                        (taggedProtocolIds tagged)
                        rest
    go prefix (_ : rest) = Just (prefix <> rest)

dropMatchingFunctionOutput :: Text -> [ResponseItem] -> [ResponseItem]
dropMatchingFunctionOutput callId = go
  where
    go [] = []
    go (FunctionCallOutputItem output : rest)
        | identifiersMatch [callId] [output.callId] = rest
    go (item : rest) = item : go rest

dropMatchingCustomToolOutput :: Text -> [ResponseItem] -> [ResponseItem]
dropMatchingCustomToolOutput callId = go
  where
    go [] = []
    go (CustomToolCallOutputItem output : rest)
        | identifiersMatch [callId] [output.callId] = rest
    go (item : rest) = item : go rest

pairedOutputType :: ResponseItemType -> Maybe ResponseItemType
pairedOutputType = \case
    ItemComputerCall -> Just ItemComputerCallOutput
    ItemToolSearchCall -> Just ItemToolSearchOutput
    ItemLocalShellCall -> Just ItemLocalShellCallOutput
    ItemShellCall -> Just ItemShellCallOutput
    ItemApplyPatchCall -> Just ItemApplyPatchCallOutput
    ItemMcpApprovalRequest -> Just ItemMcpApprovalResponse
    ItemProgram -> Just ItemProgramOutput
    _ -> Nothing

pairedUnknownOutputTag :: Text -> Maybe Text
pairedUnknownOutputTag itemType
    | "_call" `Text.isSuffixOf` normalized =
        Just (normalized <> "_output")
    | otherwise = Nothing
  where
    normalized = Text.toLower (Text.strip itemType)

dropMatchingTaggedOutput
    :: (ResponseItemType -> Bool)
    -> [Text]
    -> [ResponseItem]
    -> [ResponseItem]
dropMatchingTaggedOutput isOutput callIds = go
  where
    go [] = []
    go (KnownResponseItem itemType tagged : rest)
        | isOutput itemType
        , identifiersMatch callIds (taggedProtocolIds tagged) =
            rest
    go (UnknownResponseItem tagged : rest)
        | isOutput (ItemUnknownType tagged.tag)
        , identifiersMatch callIds (taggedProtocolIds tagged) =
            rest
    go (item : rest) = item : go rest

taggedProtocolIds :: TaggedObject -> [Text]
taggedProtocolIds _ = []

identifiersMatch :: [Text] -> [Text] -> Bool
identifiersMatch expected actual =
    not (null expectedIds)
        && not (null actualIds)
        && any (`elem` actualIds) expectedIds
  where
    expectedIds = nonEmptyIdentifiers expected
    actualIds = nonEmptyIdentifiers actual

nonEmptyIdentifiers :: [Text] -> [Text]
nonEmptyIdentifiers =
    filter (not . Text.null) . map Text.strip

sanitizeRemoteCompactionHistory :: [ResponseItem] -> [ResponseItem]
sanitizeRemoteCompactionHistory =
    map sanitizeOversizedToolCall . sanitizeCompactionHistory

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
                }
    item -> item

oversizedToolArgumentsMessage :: Text
oversizedToolArgumentsMessage =
    "Tool arguments exceeded the provider string limit and were omitted during compaction."

oversizedFunctionArguments :: Text
oversizedFunctionArguments =
    TextEncoding.decodeUtf8 . LBS.toStrict . Aeson.encode $ Aeson.object
        [ "compaction_notice" Aeson..= oversizedToolArgumentsMessage
        ]

rewriteOversizedToolOutput :: ResponseItem -> Maybe ResponseItem
rewriteOversizedToolOutput = \case
    FunctionCallOutputItem output ->
        Just $ FunctionCallOutputItem FunctionCallOutput
            { itemId = output.itemId
            , callId = output.callId
            , name = output.name
            , namespace = output.namespace
            , output = truncatedOutputJson
            , status = output.status
            }
    CustomToolCallOutputItem output ->
        Just $ CustomToolCallOutputItem CustomToolCallOutput
            { itemId = output.itemId
            , callId = output.callId
            , name = output.name
            , output = truncatedOutputJson
            , status = output.status
            }
    ToolSearchOutputItem output ->
        Just $ ToolSearchOutputItem ToolSearchOutput
            { itemId = output.itemId
            , callId = output.callId
            , status = output.status
            , execution = output.execution
            , tools = []
            }
    _ -> Nothing

truncatedOutputJson :: RawJson
truncatedOutputJson =
    rawJsonFromEncoding (Aeson.toEncoding contextWindowTruncatedOutputMessage)

rewriteItemForBudget :: Int -> ResponseItem -> Maybe ResponseItem
rewriteItemForBudget budget item =
    case item of
        MessageItem{} ->
            truncateItemText budget item
        _ ->
            rewriteOversizedToolOutput item

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
            (retainedGroups (sanitizeCompactionHistory history)))
        <> [checkpoint]

-- | Remove unbounded inline payloads before a compacted snapshot is retained
-- or replayed. Textual content remains intact; rich content becomes a short
-- explanatory input message rather than a base64 URL or opaque JSON blob.
sanitizeCompactionHistory :: [ResponseItem] -> [ResponseItem]
sanitizeCompactionHistory = map sanitizeCompactionItem

sanitizeCompactionItem :: ResponseItem -> ResponseItem
sanitizeCompactionItem (MessageItem message) =
    MessageItem (sanitizeMessage message)
sanitizeCompactionItem item = item

sanitizeMessage :: ResponseMessage -> ResponseMessage
sanitizeMessage message =
    ResponseMessage
        { messageId = message.messageId
        , content = case message.content of
            MessageContentText _ -> message.content
            MessageContentParts parts ->
                MessageContentParts
                    (concatMap (sanitizeContentPart message.role) parts)
        , role = message.role
        , status = message.status
        , phase = message.phase
        , passthrough = message.passthrough
        }

sanitizeContentPart
    :: ResponseRole
    -> ResponseContentPart
    -> [ResponseContentPart]
sanitizeContentPart role part =
    case part of
        InputTextPart{} -> [part]
        OutputTextPart{} -> [part]
        RefusalPart{} -> [part]
        ReasoningTextPart{} -> [part]
        SummaryTextPart{} -> [part]
        _ -> [richContentNoticePart role (richContentNotice part)]

richContentNoticePart :: ResponseRole -> Text -> ResponseContentPart
richContentNoticePart role notice =
    case role of
        RoleAssistant ->
            OutputTextPart
                { text = notice
                , annotations = Nothing
                , logprobs = Nothing
                }
        _ ->
            InputTextPart
                { text = notice
                , promptCacheBreakpoint = Nothing
                }

richContentNotice :: ResponseContentPart -> Text
richContentNotice = \case
    InputImagePart{} ->
        "<image attachment omitted from compacted context>"
    InputFilePart{filename} ->
        "<file attachment omitted from compacted context"
            <> maybe "" (\name -> ": " <> Text.take 120 name) filename
            <> ">"
    InputAudioPart{} ->
        "<audio attachment omitted from compacted context>"
    UnknownContentPart tagged ->
        "<unsupported content omitted from compacted context: "
            <> Text.take 80 tagged.tag
            <> ">"
    _ ->
        "<rich content omitted from compacted context>"

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
    isReloadedGeneratedContextUserText text
        || any (`Text.isPrefixOf` Text.stripStart text)
            [ "# Skill instructions: "
            , "Plan mode is active. Do not make any edits or writes to the system except for the plan file."
            , "The user approved the plan. Plan mode is now off."
            , "<subagent_notification>"
            ]

isReloadedGeneratedContextUserText :: Text -> Bool
isReloadedGeneratedContextUserText text =
    any (`Text.isPrefixOf` Text.stripStart text)
        [ "# AGENTS.md instructions for "
        , "## Skills\nThe following reusable skills are available in this session."
        , "<learned-skills>\nThese are durable, reusable instructions learned from earlier sessions."
        , "<system-reminder>\nAs you answer the user's questions, you can use the following context"
        ]

-- | Whether persisted items prove that reloadable project and skill context
-- was consumed after a transcript reset. Ephemeral plan, subagent, and
-- individually invoked skill wrappers do not satisfy this check.
hasReloadedGeneratedContextItems :: [ResponseItem] -> Bool
hasReloadedGeneratedContextItems = any \case
    MessageItem message
        | message.role == RoleUser ->
            maybe False isReloadedGeneratedContextUserText
                (messageText message)
    _ -> False

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
itemTokenCount item = estimateItemsTokens [sanitizeCompactionItem item]

truncateItemText :: Int -> ResponseItem -> Maybe ResponseItem
truncateItemText budget item =
    case sanitizeCompactionItem item of
        MessageItem message ->
            MessageItem <$> truncateMessageText budget message
        _ -> Nothing

truncateMessageText :: Int -> ResponseMessage -> Maybe ResponseMessage
truncateMessageText budget message =
    search 0 (max 0 budget) Nothing
  where
    candidateFor textBudget =
        case message.content of
            MessageContentText text ->
                let truncated = takeTokenBudget textBudget text
                in if Text.null truncated
                    then Nothing
                    else Just (replaceMessageContent
                        message
                        (MessageContentText truncated))
            MessageContentParts parts ->
                let truncated = truncateContentParts textBudget parts
                in if null truncated
                    then Nothing
                    else Just (replaceMessageContent
                        message
                        (MessageContentParts truncated))

    search low high best
        | low > high = best
        | otherwise =
            let middle = (low + high) `div` 2
            in case candidateFor middle of
                Just candidate
                    | estimateItemsTokens [MessageItem candidate] <= budget ->
                        search (middle + 1) high (Just candidate)
                _ ->
                    search low (middle - 1) best

replaceMessageContent :: ResponseMessage -> MessageContent -> ResponseMessage
replaceMessageContent message nextContent =
    ResponseMessage
        { messageId = message.messageId
        , content = nextContent
        , role = message.role
        , status = message.status
        , phase = message.phase
        , passthrough = message.passthrough
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
    InputTextPart { promptCacheBreakpoint } ->
        InputTextPart value promptCacheBreakpoint
    OutputTextPart { annotations, logprobs } ->
        OutputTextPart value annotations logprobs
    RefusalPart {} ->
        RefusalPart value
    ReasoningTextPart {} ->
        ReasoningTextPart value
    SummaryTextPart {} ->
        SummaryTextPart value
    PlainTextPart {} ->
        PlainTextPart value
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
        , "The input may have been sanitized or truncated to fit the context window."
        , "Preserve: the user's goals, active project instructions, always-active"
        , "skill constraints, safety and policy constraints, required workflows,"
        , "important file paths, decisions made,"
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
    sum [estimateEncodedValue item | item <- items]

-- | Collect recent real user message texts (newest last), skipping /compact markers.
collectRecentUserTexts :: Int -> [ResponseItem] -> [Text]
collectRecentUserTexts keep items =
    reverse (take keep (reverse (mapMaybe userTextOf items)))
  where
    userTextOf = \case
        MessageItem message
            | message.role == RoleUser ->
                case messageText (sanitizeMessage message) of
                    Just text
                        | isCompactSessionTurn text -> Nothing
                        | isGeneratedContextUserText text -> Nothing
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
    , content = MessageContentParts [InputTextPart text Nothing]
    , role = RoleUser
    , status = Nothing
    , phase = Nothing
    , passthrough = Nothing
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
                ]
        , role = RoleAssistant
        , status = Nothing
        , phase = Nothing
        , passthrough = Nothing
        }

-- | Grok-style local rebuild: recent user texts + assistant summary.
buildLocalCompactedHistory :: Int -> [ResponseItem] -> Text -> [ResponseItem]
buildLocalCompactedHistory keepRecent history summary =
    map userTextItem (collectRecentUserTexts keepRecent history)
        <> [assistantSummaryItem summary]

-- | Build a local summary snapshot whose complete next-request size is
-- bounded. The generated summary is protected while recent user messages are
-- truncated or discarded oldest-first. Local snapshots target the same 64k
-- retained-item envelope as remote compaction while still accounting for
-- request-level instructions and tool schemas.
buildLocalCompactedHistoryToFit
    :: Int
    -> ResponseCreateParams
    -> Int
    -> [ResponseItem]
    -> Text
    -> [ResponseItem]
buildLocalCompactedHistoryToFit
        contextWindow params keepRecent history summary =
    let targetWindow =
            min
                (max 0 contextWindow)
                ( estimateRequestTokensWithItems params []
                    + remoteCompactionRetainedTokenBudget
                )
        summaryItem = fitLocalSummaryItem targetWindow params summary
        recentItems =
            map userTextItem (collectRecentUserTexts keepRecent history)
    in trimResponseHistoryToFit
        targetWindow
        params
        [summaryItem]
        recentItems
            <> [summaryItem]

fitLocalSummaryItem
    :: Int
    -> ResponseCreateParams
    -> Text
    -> ResponseItem
fitLocalSummaryItem targetWindow params summary
    | requestTokens fullItem <= targetWindow = fullItem
    | Text.null stripped = fullItem
    | otherwise = maybe fullItem id (search 1 (Text.length stripped - 1) Nothing)
  where
    stripped = Text.strip summary
    fullItem = assistantSummaryItem stripped
    truncationNotice = "\n\n[Summary truncated to fit compacted context.]"
    requestTokens item = estimateRequestTokensWithItems params [item]

    candidateFor characters =
        assistantSummaryItem
            (Text.take characters stripped <> truncationNotice)

    search low high best
        | low > high = best
        | otherwise =
            let middle = (low + high) `div` 2
                candidate = candidateFor middle
            in if requestTokens candidate <= targetWindow
                then search (middle + 1) high (Just candidate)
                else search low (middle - 1) best

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
