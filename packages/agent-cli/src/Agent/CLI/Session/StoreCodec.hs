{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Boundary codecs between wire response items and storage records.
module Agent.CLI.Session.StoreCodec
    ( fromStoredResponseItem
    , toStoredResponseItem
    ) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding

import Agent.Responses.Types
    ( ResponseItem(..)
    , TaggedObject(..)
    , responseItemTypeText
    )
import Agent.Store.SessionItem (StoredResponseItem(..))

toStoredResponseItem :: ResponseItem -> StoredResponseItem
toStoredResponseItem item =
    StoredResponseItem
        { storedResponseItemType = itemTypeText item
        , storedResponseItemRepresentation = representation item
        , storedResponseItemPayload =
            TextEncoding.decodeUtf8
                (LazyByteString.toStrict (Aeson.encode item))
        }
  where
    itemTypeText = \case
        MessageItem{} -> "message"
        FunctionCallItem{} -> "function_call"
        FunctionCallOutputItem{} -> "function_call_output"
        CustomToolCallItem{} -> "custom_tool_call"
        CustomToolCallOutputItem{} -> "custom_tool_call_output"
        ReasoningItemValue{} -> "reasoning"
        ItemReferenceValue{} -> "item_reference"
        KnownResponseItem kind _ -> responseItemTypeText kind
        UnknownResponseItem tagged -> tagged.tag
    representation = \case
        KnownResponseItem{} -> "known"
        UnknownResponseItem{} -> "unknown"
        _ -> "core"

fromStoredResponseItem :: StoredResponseItem -> Either Text ResponseItem
fromStoredResponseItem item =
    case Aeson.eitherDecodeStrict' (TextEncoding.encodeUtf8 item.storedResponseItemPayload) of
        Left err -> Left ("stored response item: " <> Text.pack err)
        Right value -> Right value
