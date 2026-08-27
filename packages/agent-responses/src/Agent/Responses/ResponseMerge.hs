module Agent.Responses.ResponseMerge
    ( mergeCompletedResponseOutput
    , mergeDoneResponse
    , mergeResponseFragments
    , responseItemIdentities
    , responseItemKind
    ) where

import Agent.Json
    ( Extensions
    , extensionsToList
    , insertExtension
    , lookupExtension
    , rawJsonBytes
    )
import qualified Agent.Json.Decoder as Decoder
import Agent.Responses.Types
import Control.Applicative ((<|>))
import qualified Data.Set as Set
import Data.Text (Text)

-- | Merge output items observed in streaming @response.output_item.done@
-- events into a terminal response. Terminal output keeps its ordering and
-- values; only streamed items with no matching typed identity are appended.
mergeCompletedResponseOutput
    :: [ResponseItem]
    -> Response
    -> Response
mergeCompletedResponseOutput streamedItems response =
    response
        { output =
            mergeOutputItems response.output streamedItems
        }

-- | Overlay a terminal @response.done@ response on the latest lifecycle
-- response, normalize the event's status, and attach streamed output items.
mergeDoneResponse
    :: Maybe Response
    -> [ResponseItem]
    -> Response
    -> Response
mergeDoneResponse baseResponse streamedItems doneResponse =
    mergeCompletedResponseOutput streamedItems
        mergedResponse { status = ResponseCompleted }
  where
    mergedResponse =
        maybe doneResponse
            (`mergeResponseFragment` doneResponse)
            baseResponse

-- | Overlay lifecycle responses in wire order. Required scalar fields and
-- present optional values from later responses win. An empty decoded output
-- is treated as an omitted partial-response output so an earlier lifecycle
-- output remains available.
mergeResponseFragments :: [Response] -> Maybe Response
mergeResponseFragments [] = Nothing
mergeResponseFragments (first : rest) =
    Just (foldl' mergeResponseFragment first rest)

mergeResponseFragment :: Response -> Response -> Response
mergeResponseFragment base overlay = Response
    { responseId = nonEmptyText overlay.responseId base.responseId
    , createdAt =
        if overlay.createdAt == 0 then base.createdAt else overlay.createdAt
    , error = overlay.error <|> base.error
    , incompleteDetails =
        overlay.incompleteDetails <|> base.incompleteDetails
    , instructions = overlay.instructions <|> base.instructions
    , metadata = overlay.metadata <|> base.metadata
    , model = nonEmptyText overlay.model base.model
    , object = nonEmptyText overlay.object base.object
    , output =
        if null overlay.output then base.output else overlay.output
    , parallelToolCalls =
        overlay.parallelToolCalls <|> base.parallelToolCalls
    , temperature = overlay.temperature <|> base.temperature
    , toolChoice = overlay.toolChoice <|> base.toolChoice
    , tools = overlay.tools <|> base.tools
    , topP = overlay.topP <|> base.topP
    , background = overlay.background <|> base.background
    , completedAt = overlay.completedAt <|> base.completedAt
    , conversation = overlay.conversation <|> base.conversation
    , maxOutputTokens = overlay.maxOutputTokens <|> base.maxOutputTokens
    , maxToolCalls = overlay.maxToolCalls <|> base.maxToolCalls
    , moderation = overlay.moderation <|> base.moderation
    , previousResponseId =
        overlay.previousResponseId <|> base.previousResponseId
    , prompt = overlay.prompt <|> base.prompt
    , promptCacheKey = overlay.promptCacheKey <|> base.promptCacheKey
    , promptCacheOptions =
        overlay.promptCacheOptions <|> base.promptCacheOptions
    , promptCacheRetention =
        overlay.promptCacheRetention <|> base.promptCacheRetention
    , reasoning = overlay.reasoning <|> base.reasoning
    , safetyIdentifier =
        overlay.safetyIdentifier <|> base.safetyIdentifier
    , serviceTier = overlay.serviceTier <|> base.serviceTier
    , status = overlay.status
    , text = overlay.text <|> base.text
    , topLogprobs = overlay.topLogprobs <|> base.topLogprobs
    , truncation = overlay.truncation <|> base.truncation
    , usage = overlay.usage <|> base.usage
    , user = overlay.user <|> base.user
    , extraFields =
        mergeExtensions base.extraFields overlay.extraFields
    }

mergeOutputItems :: [ResponseItem] -> [ResponseItem] -> [ResponseItem]
mergeOutputItems finalItems streamedItems =
    finalItems <> filter (not . alreadyPresent) streamedItems
  where
    finalKeys = Set.fromList (concatMap itemIdentityKeys finalItems)
    alreadyPresent item =
        any (`Set.member` finalKeys) (itemIdentityKeys item)

type ItemIdentityKey = (Text, Text, Text)

itemIdentityKeys :: ResponseItem -> [ItemIdentityKey]
itemIdentityKeys item =
    [ (responseItemKind item, field, value)
    | (field, value) <- responseItemIdentities item
    ]

-- | Stable typed identities used to associate item events and suppress
-- terminal/output-item duplicates.
responseItemIdentities :: ResponseItem -> [(Text, Text)]
responseItemIdentities = \case
    MessageItem value -> optionalIdentity "id" value.messageId
    FunctionCallItem value ->
        optionalIdentity "id" value.itemId <> [("call_id", value.callId)]
    FunctionCallOutputItem value ->
        optionalIdentity "id" value.itemId <> [("call_id", value.callId)]
    CustomToolCallItem value ->
        optionalIdentity "id" value.itemId <> [("call_id", value.callId)]
    CustomToolCallOutputItem value ->
        optionalIdentity "id" value.itemId <> [("call_id", value.callId)]
    ReasoningItemValue value -> optionalIdentity "id" value.itemId
    ItemReferenceValue value -> [("id", value.itemId)]
    AgentMessageItem value -> optionalIdentity "id" value.messageId
    AdditionalToolsItemValue value -> optionalIdentity "id" value.itemId
    LocalShellCallItem value ->
        optionalIdentity "id" value.itemId
            <> optionalIdentity "call_id" value.callId
    ToolSearchCallItem value ->
        optionalIdentity "id" value.itemId
            <> optionalIdentity "call_id" value.callId
    ToolSearchOutputItem value ->
        optionalIdentity "id" value.itemId
            <> optionalIdentity "call_id" value.callId
    WebSearchCallItem value -> optionalIdentity "id" value.itemId
    ImageGenerationCallItem value -> optionalIdentity "id" value.itemId
    CompactionItemValue value -> optionalIdentity "id" value.itemId
    CompactionTriggerItemValue{} -> []
    ContextCompactionItemValue value -> optionalIdentity "id" value.itemId
    KnownResponseItem _ tagged -> taggedIdentities tagged
    UnknownResponseItem tagged -> taggedIdentities tagged

responseItemKind :: ResponseItem -> Text
responseItemKind = \case
    MessageItem{} -> "message"
    FunctionCallItem{} -> "function_call"
    FunctionCallOutputItem{} -> "function_call_output"
    CustomToolCallItem{} -> "custom_tool_call"
    CustomToolCallOutputItem{} -> "custom_tool_call_output"
    ReasoningItemValue{} -> "reasoning"
    ItemReferenceValue{} -> "item_reference"
    AgentMessageItem{} -> "agent_message"
    AdditionalToolsItemValue{} -> "additional_tools"
    LocalShellCallItem{} -> "local_shell_call"
    ToolSearchCallItem{} -> "tool_search_call"
    ToolSearchOutputItem{} -> "tool_search_output"
    WebSearchCallItem{} -> "web_search_call"
    ImageGenerationCallItem{} -> "image_generation_call"
    CompactionItemValue{} -> "compaction"
    CompactionTriggerItemValue{} -> "compaction_trigger"
    ContextCompactionItemValue{} -> "context_compaction"
    KnownResponseItem itemType _ -> responseItemTypeText itemType
    UnknownResponseItem tagged -> tagged.tag

taggedIdentities :: TaggedObject -> [(Text, Text)]
taggedIdentities tagged =
    foldMap identityFromField ["id", "call_id"]
  where
    identityFromField field = do
        value <- maybeToList
            (lookupExtension field tagged.fields >>= decodeText)
        pure (field, value)

    decodeText raw =
        either (const Nothing) Just
            (Decoder.decode Decoder.text (rawJsonBytes raw))

optionalIdentity :: Text -> Maybe Text -> [(Text, Text)]
optionalIdentity field = maybe [] (pure . (field,))

maybeToList :: Maybe value -> [value]
maybeToList = maybe [] pure

nonEmptyText :: Text -> Text -> Text
nonEmptyText newer older
    | newer == "" = older
    | otherwise = newer

mergeExtensions :: Extensions -> Extensions -> Extensions
mergeExtensions base overlay =
    foldl'
        (\fields (key, value) -> insertExtension key value fields)
        base
        (extensionsToList overlay)
