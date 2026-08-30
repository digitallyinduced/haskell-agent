-- | Pure helpers shared by stateless Responses request projections.
module Agent.Responses.Request
    ( forceStatelessStreaming
    , mapResponseTools
    , selectConfiguredModel
    , setResponseModel
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
