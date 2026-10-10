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
    )
import Agent.ToolArgs (readExactInt)
import Data.List (dropWhileEnd, findIndex)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text

-- | The history a spawned child starts with. @fork_turns@ is @none@, a
-- positive number of the parent's most recent user turns, or (by default)
-- the parent's complete history.
forkSubagentTranscript :: Maybe Text -> [ResponseItem] -> [ResponseItem]
forkSubagentTranscript forkTurns items =
    let completeItems = trimDanglingToolSuffix items
        normalized = Text.toLower . Text.strip <$> forkTurns
    in case normalized of
        Just "none" -> []
        Just turns
            | Just count <- readExactInt turns
            , count > 0 -> takeRecentTurns count completeItems
        _ -> completeItems

takeRecentTurns :: Int -> [ResponseItem] -> [ResponseItem]
takeRecentTurns count items =
    case drop (max 0 (length starts - count)) starts of
        start : _ -> drop start items
        [] -> items
  where
    starts =
        [ index
        | (index, MessageItem message) <- zip [0 :: Int ..] items
        , message.role == RoleUser
        ]

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
