-- | Conversation compaction helpers shared by OpenAI remote compact and
-- xAI/OpenRouter/Gemini local summarization.
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
    , isRewindSessionTurn
    , isTranscriptResetTurn
    , compactSessionUserText
    , clearSessionUserText
    , newSessionUserText
    , rewindSessionUserText
    ) where

import Agent.OpenAI.Compaction.Request
    ( buildRemoteCompactionRequest
    , estimateEncodedValue
    , estimateRequestTokensWithItems
    , estimateResponseCreateParamsTokens
    , resizedImageBytesEstimate
    )
import Agent.OpenAI.Compaction.Commands
    ( clearSessionUserText
    , compactTranscriptAtLastCheckpoint
    , isClearSessionTurn
    , isCompactSessionTurn
    , isNewSessionTurn
    , isRewindSessionTurn
    , isTranscriptResetTurn
    , newSessionUserText
    , rewindSessionUserText
    )
import Agent.Responses.Types
import Agent.Tools.TaskPlan (isTaskPlanContextText)
import Agent.Json (RawJson, rawJsonFromEncoding)
import qualified Data.Aeson as Aeson
import Data.Maybe (isJust, listToMaybe, mapMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
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
        (estimateItemsTokens . pure)
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
        (estimateItemsTokens . pure)
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
        (estimateItemsTokens . pure)
  where
    requestTokens history =
        estimateRequestTokensWithItems params (history <> trailing)

trimCompactionHistoryToFitWith
    :: ([ResponseItem] -> [ResponseItem])
    -> Int
    -> ([ResponseItem] -> Int)
    -> (ResponseItem -> Int)
    -> [ResponseItem]
    -> [ResponseItem]
trimCompactionHistoryToFitWith
        sanitize
        contextWindow
        requestTokens
        estimateItem
        history =
    let sanitized = sanitize history
        -- A single exact check handles the common case without constructing
        -- any accounting state. The old implementation called this for every
        -- candidate while repeatedly rebuilding the whole prefix.
        initialRequestTokens = requestTokens sanitized
    in if initialRequestTokens <= contextWindow
        then sanitized
        else
            let entries =
                    [ (item, itemCost item)
                    | item <- sanitized
                    ]
                itemTokens =
                    sum [cost | (_, cost) <- entries]
                -- Keep the exact request-level overhead (instructions,
                -- tools, trailing items, and framing) out of the per-item
                -- accounting.  The full initial estimate remains at least
                -- the exact check, even when an item estimate rounds down.
                fixedTokens =
                    max
                        (toInteger (max 0 (requestTokens [])))
                        (toInteger initialRequestTokens - itemTokens)
                initialTotal = fixedTokens + itemTokens
                (rewritten, rewrittenTotal) =
                    rewriteEntries initialTotal entries
                (dropped, _) =
                    dropOldestUntilFit rewrittenTotal rewritten
                (retained, _) =
                    rewriteEntries
                        (fixedTokens + sum [cost | (_, cost) <- dropped])
                        dropped
            in repairExactFit fixedTokens retained
  where
    window = toInteger contextWindow

    -- Keep one independently rounded estimate per item.  Rewrites and group
    -- selection update a running total rather than serializing the request.
    itemCost item =
        toInteger (max 1 (estimateItem item))

    boundedBudget value =
        fromInteger
            (max 0 (min (toInteger (maxBound :: Int)) value))

    -- Consider each item at most twice, newest first, and update the running
    -- total after a successful rewrite. The second pass handles a boundary
    -- item that could not be rewritten until older protocol units were
    -- removed; this remains linear while preserving the newest usable item.
    rewriteEntries initialTotal entries =
        go initialTotal (reverse entries) []
      where
        go total [] rewritten =
            (rewritten, total)
        go total remaining@((item, oldCost) : rest) rewritten
            | total <= window =
                (reverse remaining <> rewritten, total)
            | otherwise =
                let available =
                        max 0 (window - (total - oldCost))
                in case rewriteItemForBudget
                        (boundedBudget available)
                        item of
                    Just compacted
                        | newCost <- itemCost compacted
                        , newCost < oldCost ->
                            go
                                (total - oldCost + newCost)
                                rest
                                ((compacted, newCost) : rewritten)
                    _ ->
                        go total rest ((item, oldCost) : rewritten)

    -- Drop complete protocol units in one pass. In particular, this avoids
    -- repeatedly appending a growing prefix (`prefix <> rest`) for large
    -- histories. Checkpoints are never put in a droppable unit.
    dropOldestUntilFit total entries =
        dropOldestWithBoundary True total entries

    -- Once the rewrite boundary has had a chance to shrink, an irreducible
    -- final unit may still exceed the window. At that point it is safe to
    -- discard it as a last resort, matching the old exact-fit fallback.
    dropAllOldestUntilFit total entries =
        dropOldestWithBoundary False total entries

    dropOldestWithBoundary preserveBoundary total entries =
        let items = map fst entries
            units = protocolDropUnits items
            costs =
                Map.fromList
                    [ (index, cost)
                    | (index, (_, cost)) <- zip [0 ..] entries
                    ]
            dropped = chooseDrops preserveBoundary total units costs
            result =
                [ entry
                | (index, entry) <- zip [0 ..] entries
                , not (Set.member index dropped)
                ]
            removedTotal =
                sum
                    [ Map.findWithDefault 0 index costs
                    | index <- Set.toList dropped
                    ]
        in (result, total - removedTotal)

    chooseDrops preserveBoundary total units costs =
        go total units dropCount Set.empty
      where
        itemCostAt index = Map.findWithDefault 0 index costs
        dropCount = length [() | DropUnit{} <- units]

        go _current [] _remainingDrops dropped = dropped
        go current (unit : rest) remainingDrops dropped
            | current <= window = dropped
            | otherwise =
                case unit of
                    KeepUnit -> go current rest remainingDrops dropped
                    DropUnit indices ->
                        if preserveBoundary && remainingDrops <= 1
                            then dropped
                            else
                                let newIndices =
                                        filter (`Set.notMember` dropped) indices
                                    removedTokens =
                                        sum (map itemCostAt newIndices)
                                in go
                                    (current - removedTokens)
                                    rest
                                    (remainingDrops - 1)
                                    (foldr Set.insert dropped newIndices)

    -- This final check is intentionally outside the accounting loop. Usually
    -- the additive estimate is conservative, but a request adapter can add
    -- conditional fields. If that rare under-estimate occurs, run one final
    -- linear drop pass rather than recursively serializing each candidate.
    repairExactFit fixedTokens entries =
        let items = map fst entries
            exactTokens = requestTokens items
            estimatedTokens =
                fixedTokens + sum [cost | (_, cost) <- entries]
        in if exactTokens <= contextWindow
            then items
            else
                map fst
                    (fst
                        (dropAllOldestUntilFit
                            (max estimatedTokens (toInteger exactTokens))
                            entries))

data ProtocolDropUnit
    = KeepUnit
    | DropUnit [Int]

data OutputKey
    = FunctionOutputKey
    | CustomToolOutputKey
    | ComputerOutputKey
    deriving stock (Eq, Ord, Show)

protocolDropUnits :: [ResponseItem] -> [ProtocolDropUnit]
protocolDropUnits items =
    let indexed = zip [0 ..] items
        outputIndices = buildOutputIndices indexed
    in go outputIndices Set.empty indexed
  where
    go _ _ [] = []
    go outputIndices paired ((index, item) : rest)
        | Set.member index paired =
            go outputIndices paired rest
        | isCompactionCheckpoint item
            || isJust (responseItemCompactionCheckpointOrigin item) =
            KeepUnit : go outputIndices paired rest
        | otherwise =
            case item of
                FunctionCallItem call ->
                    dropCallUnit
                        outputIndices
                        paired
                        index
                        rest
                        FunctionOutputKey
                        call.callId
                CustomToolCallItem call ->
                    dropCallUnit
                        outputIndices
                        paired
                        index
                        rest
                        CustomToolOutputKey
                        call.callId
                ComputerCallItem call ->
                    dropCallUnit
                        outputIndices
                        paired
                        index
                        rest
                        ComputerOutputKey
                        call.computerCallId
                KnownResponseItem itemType tagged
                    | Just outputType <- pairedOutputType itemType ->
                        DropUnit
                            (index : maybe [] pure
                                (findTaggedOutputIndex
                                    ((== outputType) . fst)
                                    (taggedProtocolIds tagged)
                                    rest))
                            : go outputIndices paired rest
                UnknownResponseItem tagged
                    | Just outputTag <- pairedUnknownOutputTag tagged.tag ->
                        DropUnit
                            (index : maybe [] pure
                                (findTaggedOutputIndex
                                    (\(itemType, _) ->
                                        case itemType of
                                            ItemUnknownType value ->
                                                value == outputTag
                                            _ -> False)
                                    (taggedProtocolIds tagged)
                                    rest))
                            : go outputIndices paired rest
                _ ->
                    DropUnit [index] : go outputIndices paired rest

    dropCallUnit outputIndices paired index rest outputType rawId =
        let outputIndex =
                findOutputIndex
                    outputIndices
                    paired
                    index
                    (outputType, rawId)
            nextPaired = maybe paired (`Set.insert` paired) outputIndex
        in DropUnit (index : maybe [] pure outputIndex)
            : go outputIndices nextPaired rest

    findOutputIndex outputIndices paired minIndex (outputType, rawId) =
        case nonEmptyIdentifiers [rawId] of
            [callId] ->
                Map.lookup (outputType, callId) outputIndices
                    >>= listToMaybe
                        . filter
                            (\index ->
                                index > minIndex
                                    && index `Set.notMember` paired)
            _ -> Nothing

    buildOutputIndices =
        foldl' addOutputIndex Map.empty

    addOutputIndex outputIndices (index, item) =
        case outputKey item of
            Just key ->
                Map.insertWith
                    (\new old -> old <> new)
                    key
                    [index]
                    outputIndices
            Nothing -> outputIndices

    outputKey = \case
        FunctionCallOutputItem output ->
            key FunctionOutputKey output.callId
        CustomToolCallOutputItem output ->
            key CustomToolOutputKey output.callId
        ComputerCallOutputItem output ->
            key ComputerOutputKey output.computerOutputCallId
        _ -> Nothing
      where
        key outputType rawId =
            case nonEmptyIdentifiers [rawId] of
                [callId] -> Just (outputType, callId)
                _ -> Nothing

    findTaggedOutputIndex predicate callIds items
        | null (nonEmptyIdentifiers callIds) = Nothing
        | otherwise =
            listToMaybe
                [ index
                | (index, item) <- items
                , case item of
                    KnownResponseItem itemType tagged ->
                        predicate (itemType, tagged)
                            && identifiersMatch
                                callIds
                                (taggedProtocolIds tagged)
                    UnknownResponseItem tagged ->
                        predicate (ItemUnknownType tagged.tag, tagged)
                            && identifiersMatch
                                callIds
                                (taggedProtocolIds tagged)
                    _ -> False
                ]

isCompactionCheckpoint :: ResponseItem -> Bool
isCompactionCheckpoint = \case
    CompactionItemValue{} -> True
    ContextCompactionItemValue{} -> True
    KnownResponseItem ItemCompaction _ -> True
    KnownResponseItem ItemContextCompaction _ -> True
    _ -> False

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

compactedScreenshotDataUrl :: Text
compactedScreenshotDataUrl =
    "data:image/png;base64,"
        <> "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVQ"
        <> "IHWP4z8DwHwAFgAI/ScL7WQAAAABJRU5ErkJggg=="

pairedUnknownOutputTag :: Text -> Maybe Text
pairedUnknownOutputTag itemType
    | "_call" `Text.isSuffixOf` normalized =
        Just (normalized <> "_output")
    | otherwise = Nothing
  where
    normalized = Text.toLower (Text.strip itemType)

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
                , provider = call.provider
                , arguments = oversizedFunctionArguments
                , encryptedFunctionArgs = call.encryptedFunctionArgs
                , status = call.status
                , async = call.async
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
                , async = call.async
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
            { localOutcome = output.localOutcome
            , itemId = output.itemId
            , callId = output.callId
            , name = output.name
            , namespace = output.namespace
            , provider = output.provider
            , output = truncatedOutputJson
            , status = output.status
            , async = output.async
            }
    ComputerCallOutputItem output ->
        Just $ ComputerCallOutputItem ComputerCallOutput
            { computerOutputItemId = output.computerOutputItemId
            , computerOutputCallId = output.computerOutputCallId
            , screenshotDataUrl = compactedScreenshotDataUrl
            , acknowledgedChecks = output.acknowledgedChecks
            , computerOutputStatus = output.computerOutputStatus
            , computerOutputExtra = output.computerOutputExtra
            }
    CustomToolCallOutputItem output ->
        Just $ CustomToolCallOutputItem CustomToolCallOutput
            { localOutcome = output.localOutcome
            , itemId = output.itemId
            , callId = output.callId
            , name = output.name
            , output = truncatedOutputJson
            , status = output.status
            , async = output.async
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
        || isTaskPlanContextText text
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
        , passthrough = Just InternalChatMetadata
            { turnId = Nothing
            , createTime = Nothing
            , contentItemKinds =
                Just [localCompactionSummaryContentItemKind]
            , executedToolCalls = Nothing
            }
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

hasCompactionCheckpoint :: [ResponseItem] -> Bool
hasCompactionCheckpoint = any \case
    CompactionItemValue{} -> True
    ContextCompactionItemValue{} -> True
    KnownResponseItem ItemCompaction _ -> True
    KnownResponseItem ItemContextCompaction _ -> True
    MessageItem message
        | message.role == RoleAssistant ->
            responseMessageHasContentItemKind
                localCompactionSummaryContentItemKind
                message
    _ -> False
