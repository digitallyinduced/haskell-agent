-- | Pure helpers shared by stateless Responses request projections.
module Agent.Responses.Request
    ( forceStatelessStreaming
    , mapResponseTools
    , selectConfiguredModel
    , setResponseModel
    , filterCompactionCheckpointsByOrigin
    , filterRequestCompactionCheckpointsByOrigin
    , isServerCompactionCheckpoint
    , stripLocalCompactionMarker
    , stripReplayedItemStatus
    , stripReplayedInputStatus
    ) where

import Agent.Responses.Types
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Maybe as Maybe
import Data.Text (Text)

-- | Disable remote transcript state and require a streaming response.
forceStatelessStreaming
    :: ResponseCreateParams
    -> ResponseCreateParams
forceStatelessStreaming request = request
    { store = Just False
    , stream = Just True
    , previousResponseId = Nothing
    }

-- | Transform or discard tools without changing unrelated request fields.
mapResponseTools
    :: (ResponseTool -> Maybe ResponseTool)
    -> ResponseCreateParams
    -> ResponseCreateParams
mapResponseTools project ResponseCreateParams{..} =
    ResponseCreateParams
        { tools = Maybe.mapMaybe project <$> tools
        , ..
        }

-- | Set the request model without relying on ambiguous record updates.
setResponseModel
    :: Text
    -> ResponseCreateParams
    -> ResponseCreateParams
setResponseModel selected ResponseCreateParams{..} =
    ResponseCreateParams
        { model = Just selected
        , ..
        }

-- | Apply an exact override, preserve a provider-native model identifier, or
-- fall back to the configured default when no model was supplied.
selectConfiguredModel
    :: Map Text Text
    -> (Text -> Bool)
    -> Text
    -> Maybe Text
    -> Text
selectConfiguredModel overrides isNative defaultModel = \case
    Nothing -> defaultModel
    Just model -> case Map.lookup model overrides of
        Just target -> target
        Nothing
            | isNative model -> model
            | otherwise -> defaultModel

-- | Keep opaque server checkpoints selected by their adjacent host-only
-- provenance marker. The predicate receives 'Nothing' for legacy unmarked
-- checkpoints. Provenance markers are always removed from the projected list.
filterCompactionCheckpointsByOrigin
    :: (Maybe Text -> Bool)
    -> [ResponseItem]
    -> [ResponseItem]
filterCompactionCheckpointsByOrigin keepOrigin = go
  where
    go (checkpoint : marker : rest)
        | isServerCompactionCheckpoint checkpoint
        , Just origin <- responseItemCompactionCheckpointOrigin marker =
            if keepOrigin (Just origin)
                then checkpoint : go rest
                else go rest
    go (checkpoint : rest)
        | isServerCompactionCheckpoint checkpoint =
            if keepOrigin Nothing
                then checkpoint : go rest
                else go rest
    go (item : rest)
        | Just _ <- responseItemCompactionCheckpointOrigin item = go rest
        | otherwise = item : go rest
    go [] = []

-- | Apply 'filterCompactionCheckpointsByOrigin' to typed request input.
filterRequestCompactionCheckpointsByOrigin
    :: (Maybe Text -> Bool)
    -> ResponseCreateParams
    -> ResponseCreateParams
filterRequestCompactionCheckpointsByOrigin keepOrigin
        ResponseCreateParams { input, .. } =
    ResponseCreateParams
        { input = filterInput <$> input
        , ..
        }
  where
    filterInput = \case
        ResponseInputItems items ->
            ResponseInputItems
                (filterCompactionCheckpointsByOrigin keepOrigin items)
        other -> other

-- | Whether an item replaces the provider transcript that preceded it.
isServerCompactionCheckpoint :: ResponseItem -> Bool
isServerCompactionCheckpoint = \case
    CompactionItemValue{} -> True
    ContextCompactionItemValue{} -> True
    KnownResponseItem ItemCompaction _ -> True
    KnownResponseItem ItemContextCompaction _ -> True
    _ -> False

-- | Remove host-only compaction metadata. These markers remain in persisted
-- state so compaction policy can recognize local summaries and the provider
-- that created an opaque checkpoint, but Responses-compatible providers must
-- never receive them.
stripLocalCompactionMarker
    :: ResponseCreateParams
    -> ResponseCreateParams
stripLocalCompactionMarker ResponseCreateParams { input, .. } =
    ResponseCreateParams
        { input = stripInput <$> input
        , ..
        }
  where
    stripInput = \case
        ResponseInputItems items ->
            ResponseInputItems (Maybe.mapMaybe stripItem items)
        other -> other

    stripItem item
        | Just _ <- responseItemCompactionCheckpointOrigin item =
            Nothing
        | otherwise = Just (stripItemMetadata item)

    stripItemMetadata = \case
        MessageItem ResponseMessage { passthrough, .. } ->
            MessageItem ResponseMessage
                { passthrough = stripMetadata passthrough
                , ..
                }
        AgentMessageItem ResponseAgentMessage { passthrough, .. } ->
            AgentMessageItem ResponseAgentMessage
                { passthrough = stripMetadata passthrough
                , ..
                }
        other -> other

    stripMetadata = \case
        Nothing -> Nothing
        Just metadata ->
            let remainingKinds = case metadata.contentItemKinds of
                    Nothing -> Nothing
                    Just kinds -> case
                        filter
                            (/= localCompactionSummaryContentItemKind)
                            kinds
                        of
                            [] -> Nothing
                            values -> Just values
                cleaned = metadata { contentItemKinds = remainingKinds }
            in if cleaned == emptyMetadata
                then Nothing
                else Just cleaned

    emptyMetadata = InternalChatMetadata
        { turnId = Nothing
        , createTime = Nothing
        , contentItemKinds = Nothing
        , executedToolCalls = Nothing
        }

-- | Drop the provider lifecycle @status@ from transcript items that are
-- replayed as request input.
--
-- Providers stamp @status@ (@completed@, @in_progress@, ...) on the items
-- they return. It is output metadata, not input: Codex never serializes it
-- when it replays a transcript, because its message, function-call,
-- tool-output, and reasoning item structs have no such field. Responses Lite
-- validates replayed input strictly and rejects it as an unknown parameter
-- (@input[N].status@) on reasoning items, which fails remote compaction,
-- resume without a response chain, and side calls for any transcript that
-- persisted the status. The in-memory transcript keeps its status for the UI;
-- only the wire projection changes.
--
-- Items whose @status@ Codex does send on input (local shell calls, custom
-- tool calls, hosted tool calls) keep theirs.
stripReplayedItemStatus :: ResponseItem -> ResponseItem
stripReplayedItemStatus = \case
    MessageItem ResponseMessage { status = _, .. } ->
        MessageItem ResponseMessage { status = Nothing, .. }
    FunctionCallItem FunctionCall { status = _, .. } ->
        FunctionCallItem FunctionCall { status = Nothing, .. }
    FunctionCallOutputItem FunctionCallOutput { status = _, .. } ->
        FunctionCallOutputItem FunctionCallOutput { status = Nothing, .. }
    CustomToolCallOutputItem CustomToolCallOutput { status = _, .. } ->
        CustomToolCallOutputItem CustomToolCallOutput { status = Nothing, .. }
    ReasoningItemValue ReasoningItem { status = _, .. } ->
        ReasoningItemValue ReasoningItem { status = Nothing, .. }
    item -> item

-- | 'stripReplayedItemStatus' over a whole request input.
stripReplayedInputStatus :: ResponseInput -> ResponseInput
stripReplayedInputStatus = \case
    ResponseInputItems items ->
        ResponseInputItems (map stripReplayedItemStatus items)
    other -> other
