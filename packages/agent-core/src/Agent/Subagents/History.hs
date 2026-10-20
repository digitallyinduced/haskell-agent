-- | Transcript operations for delegated agents: the history a child inherits
-- from its parent, and the repair of a transcript before it is replayed in a
-- fresh request.
module Agent.Subagents.History
    ( forkSubagentTranscript
    , trimDanglingToolSuffix
    ) where

import Agent.Responses.Types
    ( ComputerCall(..)
    , ComputerCallOutput(..)
    , CustomToolCall(..)
    , CustomToolCallOutput(..)
    , FunctionCall(..)
    , FunctionCallOutput(..)
    , ResponseItem(..)
    , ResponseMessage(..)
    , ResponseRole(..)
    , InternalChatMetadata(..)
    , MessageContent(..)
    , ResponseContentPart(..)
    )
import Data.List (dropWhileEnd, findIndex)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text

-- | Codex V2 forks either no history or the complete parent history. Legacy
-- positive counts also mean full history; they no longer truncate context.
-- Parent-local role and usage hints must not redefine the child's identity.
forkSubagentTranscript :: Maybe Text -> [ResponseItem] -> [ResponseItem]
forkSubagentTranscript forkTurns items =
    let completeItems = trimDanglingToolSuffix (mapMaybe retainForkedItem items)
        normalized = Text.toLower . Text.strip <$> forkTurns
    in case normalized of
        Just "none" -> []
        _ -> completeItems

retainForkedItem :: ResponseItem -> Maybe ResponseItem
retainForkedItem (MessageItem message)
    | message.role == RoleDeveloper = MessageItem <$> retainForkedMessage message
retainForkedItem item = Just item

retainForkedMessage :: ResponseMessage -> Maybe ResponseMessage
retainForkedMessage message = case message.content of
    MessageContentText text
        | any parentKind (maybe [] id kinds) || parentText text -> Nothing
        | otherwise -> Just message
    MessageContentParts parts ->
        let aligned = maybe False ((== length parts) . length) kinds
            annotated = zip
                (if aligned then maybe [] (map Just) kinds else repeat Nothing)
                parts
            retained = filter (not . parentPart) annotated
            metadata = case message.passthrough of
                Just value | aligned -> Just value
                    { contentItemKinds = Just [kind | (Just kind, _) <- retained] }
                value -> value
        in if null retained then Nothing else Just message
            { content = MessageContentParts (map snd retained)
            , passthrough = metadata
            }
  where
    kinds = message.passthrough >>= (.contentItemKinds)
    parentKind kind = kind `elem`
        ["multi_agent.role_instructions", "multi_agent.usage_hint"]
    parentText text = any (`Text.isPrefixOf` Text.stripStart text)
        ["<multi_agent_role>", "<multi_agent_usage_hint>"]
    parentPart (kind, part) = maybe False parentKind kind || case part of
        InputTextPart text _ -> parentText text
        _ -> False

-- | Remove an incomplete tool-call suffix from a live transcript snapshot.
--
-- During a parent turn, provider output may already contain reasoning and tool
-- calls while their matching outputs have not been committed yet. Replaying
-- that torn suffix in a fresh request produces invalid tool pairing.
trimDanglingToolSuffix :: [ResponseItem] -> [ResponseItem]
trimDanglingToolSuffix items =
    retainCompleteToolPairs $
        case findIndex (isUnmatchedCall completed) suffix of
            Nothing -> items
            Just index -> prefix <> dropTrailingReasoning (take index suffix)
  where
    completed = outputCallIds items
    (prefix, suffix) = splitAfterLastMessage items

data ToolCallKey
    = FunctionCallKey !Text
    | CustomToolCallKey !Text
    | ComputerCallKey !Text
    deriving (Eq, Ord)

-- A fresh request cannot rely on provider-side continuation state. Keep only
-- tool calls whose matching output occurs later in the inherited transcript,
-- and discard orphan outputs as well as old unmatched calls.
retainCompleteToolPairs :: [ResponseItem] -> [ResponseItem]
retainCompleteToolPairs items =
    filter (belongsTo complete) items
  where
    complete = snd (foldl' collect (Set.empty, Set.empty) items)

    collect (seen, paired) item = case itemKey item of
        Just (True, key) -> (Set.insert key seen, paired)
        Just (False, key)
            | Set.member key seen -> (seen, Set.insert key paired)
        _ -> (seen, paired)

    belongsTo paired item = case itemKey item of
        Just (_, key) -> Set.member key paired
        Nothing -> True

    itemKey = \case
        FunctionCallItem call -> Just (True, FunctionCallKey call.callId)
        FunctionCallOutputItem output ->
            Just (False, FunctionCallKey output.callId)
        CustomToolCallItem call -> Just (True, CustomToolCallKey call.callId)
        CustomToolCallOutputItem output ->
            Just (False, CustomToolCallKey output.callId)
        ComputerCallItem call ->
            Just (True, ComputerCallKey call.computerCallId)
        ComputerCallOutputItem output ->
            Just (False, ComputerCallKey output.computerOutputCallId)
        _ -> Nothing

splitAfterLastMessage :: [ResponseItem] -> ([ResponseItem], [ResponseItem])
splitAfterLastMessage items =
    case lastMessageIndex items of
        Nothing -> ([], items)
        Just index -> splitAt (index + 1) items
  where
    lastMessageIndex =
        foldl'
            (\found (index, item) -> case item of
                MessageItem{} -> Just index
                _ -> found)
            Nothing
            . zip [0 :: Int ..]

outputCallIds :: [ResponseItem] -> Set Text
outputCallIds = Set.fromList . foldMap \case
    FunctionCallOutputItem output -> [output.callId]
    CustomToolCallOutputItem output -> [output.callId]
    ComputerCallOutputItem output -> [output.computerOutputCallId]
    _ -> []

isUnmatchedCall :: Set Text -> ResponseItem -> Bool
isUnmatchedCall completed = \case
    FunctionCallItem call -> Set.notMember call.callId completed
    CustomToolCallItem call -> Set.notMember call.callId completed
    ComputerCallItem call -> Set.notMember call.computerCallId completed
    _ -> False

dropTrailingReasoning :: [ResponseItem] -> [ResponseItem]
dropTrailingReasoning = dropWhileEnd isReasoning
  where
    isReasoning ReasoningItemValue{} = True
    isReasoning _ = False
