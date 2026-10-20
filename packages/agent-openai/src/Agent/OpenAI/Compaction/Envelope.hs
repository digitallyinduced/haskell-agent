-- | Wire-envelope normalization for native Responses compaction requests.
module Agent.OpenAI.Compaction.Envelope (nativeCompactionRequestBody) where

import Agent.OpenAI.ModelMetadata (isCodexResponsesLiteModel)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector

-- | Generic JSON plumbing: preserve unknown provider fields while removing
-- replay-only metadata and features the compaction dialect cannot accept.
-- Lite compaction does not execute tools. Retain assistant search findings,
-- but remove hosted web-search declarations and their call items together.
nativeCompactionRequestBody :: Aeson.ToJSON request => Text -> request -> Aeson.Value
nativeCompactionRequestBody model request = case Aeson.toJSON request of
    Aeson.Object fields -> Aeson.Object $
        KeyMap.insert "stream" (Aeson.Bool True) $
        KeyMap.insert "store" (Aeson.Bool False) $
        KeyMap.insert "tool_choice" (Aeson.String "auto") $
        KeyMap.insert "parallel_tool_calls" (Aeson.Bool (not lite)) $
        KeyMap.delete "previous_response_id" $
        KeyMap.delete "prompt_cache_retention" $
        KeyMap.mapWithKey sanitizeField fields
    other -> other
  where
    lite = isCodexResponsesLiteModel model
    sanitizeField "input" (Aeson.Array items) = Aeson.Array $
        fmap sanitizeItem (if lite then Vector.filter (not . isWebSearchCall) items else items)
    sanitizeField "tools" (Aeson.Array tools)
        | lite = Aeson.Array (Vector.filter isLiteTool tools)
    sanitizeField _ value = value
    sanitizeItem (Aeson.Object fields) = Aeson.Object $
        KeyMap.mapWithKey (\key value -> if key == "passthrough" then sanitizeMetadata value else value) $
        KeyMap.delete "haskell_agent_message_metadata" $
        KeyMap.delete "content_item_kinds" $ KeyMap.delete "status" fields
    sanitizeItem other = other
    sanitizeMetadata (Aeson.Object fields) =
        Aeson.Object (KeyMap.delete "content_item_kinds" fields)
    sanitizeMetadata other = other
    isWebSearchCall value = case wireType value of
        Just itemType -> fromMaybe itemType (Text.stripPrefix "raw:" itemType) == "web_search_call"
        Nothing -> False
    isLiteTool value = case wireType value of
        Just "function" -> True
        Just "custom" -> True
        Just "tool_search" -> case value of
            Aeson.Object fields -> KeyMap.lookup "execution" fields == Just (Aeson.String "client")
            _ -> False
        _ -> False
    wireType (Aeson.Object fields) = case KeyMap.lookup "type" fields of
        Just (Aeson.String value) -> Just value
        _ -> Nothing
    wireType _ = Nothing
