module Agent.XAI.ContextTrim
    ( defaultResponsesItemHistoryTokenBudget
    , defaultResponsesItemIncidentGapSeconds
    , estimateCodexRequestInputTokens
    , estimateResponsesItemsTokens
    , trimResponsesItemsToTokenBudget
    , trimResponsesItemHistory
    ) where

import Agent.OpenAI.Responses.Types
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Foldable as Foldable
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime)

-- | Soft cap for replayed conversation history. The API model has a larger
-- context window, but keeping replay bounded keeps latency, cost, and model
-- attention predictable.
defaultResponsesItemHistoryTokenBudget :: Int
defaultResponsesItemHistoryTokenBudget = 100_000

-- | Idle gap that starts a new chat activation/incident for history trimming.
-- Tool and assistant rows stay attached to the latest user activation even if
-- a long-running tool takes more than this to finish.
defaultResponsesItemIncidentGapSeconds :: NominalDiffTime
defaultResponsesItemIncidentGapSeconds = 600

estimateCodexRequestInputTokens :: ResponseCreateParams -> Int
estimateCodexRequestInputTokens =
    estimateLazyByteStringTokens . Aeson.encode

-- | Cap history by removing oldest activations first. Each input tuple is
-- @(created_at, is_user_activation, item)@ and must be in chronological order.
trimResponsesItemsToTokenBudget :: Int -> [(UTCTime, Bool, ResponseItem)] -> [ResponseItem]
trimResponsesItemsToTokenBudget budget timedItems
    | budget <= 0 = []
    | totalTokens <= budget = items
    | otherwise = ensureToolPairs selectedItems
  where
    groups = groupTimedResponsesByIncident defaultResponsesItemIncidentGapSeconds timedItems
    items = map timedItemValue timedItems
    totalTokens = estimateResponsesItemsTokens items
    selectedGroups = keepNewestGroupsWithinBudget budget groups
    selectedItems = trimOversizedNewestGroup budget (map timedItemValue (concat selectedGroups))

timedItemValue :: (UTCTime, Bool, ResponseItem) -> ResponseItem
timedItemValue (_, _, item) = item

groupTimedResponsesByIncident
    :: NominalDiffTime
    -> [(UTCTime, Bool, ResponseItem)]
    -> [[(UTCTime, Bool, ResponseItem)]]
groupTimedResponsesByIncident gap entries =
    reverse allGroupsRev
  where
    (groupsRev, currentRev, _) = Foldable.foldl' step ([], [], Nothing) entries
    allGroupsRev = case currentRev of
        [] -> groupsRev
        _  -> reverse currentRev : groupsRev

    step (groups, current, lastUserAt) entry@(createdAt, isUserActivation, _item)
        | shouldStartNewGroup =
            (reverse current : groups, [entry], nextLastUserAt)
        | otherwise =
            (groups, entry : current, nextLastUserAt)
      where
        shouldStartNewGroup =
            isUserActivation
                && not (null current)
                && maybe False (\lastUser -> diffUTCTime createdAt lastUser > gap) lastUserAt
        nextLastUserAt =
            if isUserActivation then Just createdAt else lastUserAt

keepNewestGroupsWithinBudget
    :: Int
    -> [[(UTCTime, Bool, ResponseItem)]]
    -> [[(UTCTime, Bool, ResponseItem)]]
keepNewestGroupsWithinBudget budget groups =
    reverse (go [] 0 (reverse groups))
  where
    go kept _total [] = kept
    go kept total (group:olderGroups)
        | null kept =
            go [group] groupTokens olderGroups
        | total + groupTokens <= budget =
            go (group:kept) (total + groupTokens) olderGroups
        | otherwise =
            kept
      where
        groupTokens = estimateResponsesItemsTokens (map timedItemValue group)

-- | Cap a flat item list that carries no timestamps, dropping the oldest
-- items first and never leaving a tool call without its output. Transports
-- that keep their own running transcript use this; callers replaying a
-- persisted, timestamped history want 'trimResponsesItemsToTokenBudget',
-- which trims whole activations instead.
trimResponsesItemHistory :: Int -> [ResponseItem] -> [ResponseItem]
trimResponsesItemHistory budget items
    | budget <= 0 = items
    | otherwise = trimOversizedNewestGroup budget items

trimOversizedNewestGroup :: Int -> [ResponseItem] -> [ResponseItem]
trimOversizedNewestGroup budget items
    | estimateResponsesItemsTokens normalized <= budget = normalized
    | length normalized <= 1 = normalized
    | otherwise = trimOversizedNewestGroup budget (drop 1 normalized)
  where
    normalized = ensureToolPairs items

ensureToolPairs :: [ResponseItem] -> [ResponseItem]
ensureToolPairs items =
    filter keepItem items
  where
    callRefs = Set.fromList (mapMaybe callPairRef items)
    outputRefs = Set.fromList (mapMaybe outputPairRef items)
    pairedRefs = Set.intersection callRefs outputRefs

    keepItem item
        | Just pairRef <- callPairRef item = pairRef `Set.member` pairedRefs
        | Just pairRef <- outputPairRef item = pairRef `Set.member` pairedRefs
        | otherwise = True

data PairKind
    = FunctionPair
    | ToolSearchPair
    | CustomToolPair
    | ComputerPair
    deriving (Eq, Ord)

data PairRef = PairRef PairKind Text
    deriving (Eq, Ord)

callPairRef :: ResponseItem -> Maybe PairRef
callPairRef = \case
    FunctionCallItem functionCall ->
        Just (PairRef FunctionPair functionCall.callId)
    CustomToolCallItem customCall ->
        Just (PairRef CustomToolPair customCall.callId)
    KnownResponseItem _ tagged -> callPairRefTagged tagged
    UnknownResponseItem tagged -> callPairRefTagged tagged
    _ -> Nothing

outputPairRef :: ResponseItem -> Maybe PairRef
outputPairRef = \case
    FunctionCallOutputItem functionOutput ->
        Just (PairRef FunctionPair functionOutput.callId)
    CustomToolCallOutputItem customOutput ->
        Just (PairRef CustomToolPair customOutput.callId)
    KnownResponseItem _ tagged -> outputPairRefTagged tagged
    UnknownResponseItem tagged -> outputPairRefTagged tagged
    _ -> Nothing

callPairRefTagged :: TaggedObject -> Maybe PairRef
callPairRefTagged tagged = case tagged.tag of
    "function_call" -> PairRef FunctionPair <$> taggedText "call_id" tagged
    "local_shell_call" -> PairRef FunctionPair <$> taggedText "call_id" tagged
    "tool_search_call" -> PairRef ToolSearchPair <$> taggedText "call_id" tagged
    "custom_tool_call" -> PairRef CustomToolPair <$> taggedText "call_id" tagged
    "computer_call" -> PairRef ComputerPair <$> taggedText "call_id" tagged
    _ -> Nothing

outputPairRefTagged :: TaggedObject -> Maybe PairRef
outputPairRefTagged tagged = case tagged.tag of
    "function_call_output" -> PairRef FunctionPair <$> taggedText "call_id" tagged
    "tool_search_output"
        | taggedText "execution" tagged /= Just "server" ->
            PairRef ToolSearchPair <$> taggedText "call_id" tagged
    "custom_tool_call_output" -> PairRef CustomToolPair <$> taggedText "call_id" tagged
    "computer_call_output" -> PairRef ComputerPair <$> taggedText "call_id" tagged
    _ -> Nothing

estimateResponsesItemsTokens :: [ResponseItem] -> Int
estimateResponsesItemsTokens =
    sum . map estimateResponsesItemTokens

estimateResponsesItemTokens :: ResponseItem -> Int
estimateResponsesItemTokens =
    estimateLazyByteStringTokens . Aeson.encode

estimateLazyByteStringTokens :: LBS.ByteString -> Int
estimateLazyByteStringTokens bytes =
    max 1 ((byteLen + 3) `div` 4)
  where
    byteLen = fromIntegral (LBS.length bytes) :: Int

taggedText :: Text -> TaggedObject -> Maybe Text
taggedText key tagged =
    valueToText =<< KeyMap.lookup (AesonKey.fromText key) tagged.fields

valueToText :: Aeson.Value -> Maybe Text
valueToText = \case
    Aeson.String value -> Just value
    Aeson.Number value -> Just (Text.pack (show value))
    Aeson.Bool True -> Just "true"
    Aeson.Bool False -> Just "false"
    _ -> Nothing
