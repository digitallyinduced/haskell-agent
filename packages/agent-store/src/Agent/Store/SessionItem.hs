-- | Typed storage records for response items.
--
-- PostgreSQL persists these fields directly. Values whose provider-defined
-- shape is intentionally open are encoded and decoded by the caller and cross
-- the storage boundary as opaque text.
module Agent.Store.SessionItem
    ( StoredResponseItemRepresentation(..)
    , StoredOpaqueObject(..)
    , StoredOpaqueValue(..)
    , StoredToolOutputKind(..)
    , StoredToolOutput(..)
    , StoredMessageContent(..)
    , StoredContentPart(..)
    , StoredMessage(..)
    , StoredFunctionCall(..)
    , StoredFunctionCallOutput(..)
    , StoredCustomToolCall(..)
    , StoredCustomToolCallOutput(..)
    , StoredReasoningSummaryPart(..)
    , StoredReasoning(..)
    , StoredItemReference(..)
    , StoredTaggedItem(..)
    , StoredResponseItem(..)
    , storedResponseItemType
    , storedResponseItemRepresentation
    ) where

import Data.Text (Text)

data StoredResponseItemRepresentation
    = StoredCoreRepresentation
    | StoredKnownRepresentation
    | StoredUnknownRepresentation
    deriving (Eq, Show)

newtype StoredOpaqueObject = StoredOpaqueObject
    { storedOpaqueObjectText :: Text
    }
    deriving (Eq, Show)

newtype StoredOpaqueValue = StoredOpaqueValue
    { storedOpaqueValueText :: Text
    }
    deriving (Eq, Show)

data StoredToolOutputKind
    = StoredToolOutputText
    | StoredToolOutputEncoded
    deriving (Eq, Show)

data StoredToolOutput = StoredToolOutput
    { storedToolOutputKind :: !StoredToolOutputKind
    , storedToolOutputText :: !Text
    }
    deriving (Eq, Show)

data StoredMessageContent
    = StoredMessageText !Text
    | StoredMessageParts ![StoredContentPart]
    deriving (Eq, Show)

data StoredContentPart = StoredContentPart
    { storedContentPartType :: !Text
    , storedContentPartText :: !(Maybe Text)
    , storedContentPartRefusal :: !(Maybe Text)
    , storedContentPartDetail :: !(Maybe Text)
    , storedContentPartFileData :: !(Maybe Text)
    , storedContentPartFileId :: !(Maybe Text)
    , storedContentPartFileUrl :: !(Maybe Text)
    , storedContentPartFilename :: !(Maybe Text)
    , storedContentPartImageUrl :: !(Maybe Text)
    , storedContentPartInputAudio :: !(Maybe StoredOpaqueValue)
    , storedContentPartPromptCacheBreakpoint :: !(Maybe StoredOpaqueValue)
    , storedContentPartAnnotations :: !(Maybe StoredOpaqueValue)
    , storedContentPartLogprobs :: !(Maybe StoredOpaqueValue)
    , storedContentPartExtraFields :: !StoredOpaqueObject
    }
    deriving (Eq, Show)

data StoredMessage = StoredMessage
    { storedMessageProviderItemId :: !(Maybe Text)
    , storedMessageContent :: !StoredMessageContent
    , storedMessageRole :: !Text
    , storedMessageStatus :: !(Maybe Text)
    , storedMessagePhase :: !(Maybe Text)
    , storedMessageExtraFields :: !StoredOpaqueObject
    }
    deriving (Eq, Show)

data StoredFunctionCall = StoredFunctionCall
    { storedFunctionCallProviderItemId :: !(Maybe Text)
    , storedFunctionCallCallId :: !Text
    , storedFunctionCallName :: !Text
    , storedFunctionCallArguments :: !Text
    , storedFunctionCallStatus :: !(Maybe Text)
    , storedFunctionCallExtraFields :: !StoredOpaqueObject
    }
    deriving (Eq, Show)

data StoredFunctionCallOutput = StoredFunctionCallOutput
    { storedFunctionCallOutputProviderItemId :: !(Maybe Text)
    , storedFunctionCallOutputCallId :: !Text
    , storedFunctionCallOutputValue :: !StoredToolOutput
    , storedFunctionCallOutputStatus :: !(Maybe Text)
    , storedFunctionCallOutputExtraFields :: !StoredOpaqueObject
    }
    deriving (Eq, Show)

data StoredCustomToolCall = StoredCustomToolCall
    { storedCustomToolCallProviderItemId :: !(Maybe Text)
    , storedCustomToolCallCallId :: !Text
    , storedCustomToolCallName :: !Text
    , storedCustomToolCallInput :: !Text
    , storedCustomToolCallStatus :: !(Maybe Text)
    , storedCustomToolCallExtraFields :: !StoredOpaqueObject
    }
    deriving (Eq, Show)

data StoredCustomToolCallOutput = StoredCustomToolCallOutput
    { storedCustomToolCallOutputProviderItemId :: !(Maybe Text)
    , storedCustomToolCallOutputCallId :: !Text
    , storedCustomToolCallOutputName :: !(Maybe Text)
    , storedCustomToolCallOutputValue :: !StoredToolOutput
    , storedCustomToolCallOutputStatus :: !(Maybe Text)
    , storedCustomToolCallOutputExtraFields :: !StoredOpaqueObject
    }
    deriving (Eq, Show)

data StoredReasoningSummaryPart = StoredReasoningSummaryPart
    { storedReasoningSummaryPartType :: !Text
    , storedReasoningSummaryPartText :: !(Maybe Text)
    , storedReasoningSummaryPartExtraFields :: !StoredOpaqueObject
    }
    deriving (Eq, Show)

data StoredReasoning = StoredReasoning
    { storedReasoningProviderItemId :: !(Maybe Text)
    , storedReasoningSummary :: ![StoredReasoningSummaryPart]
    , storedReasoningContent :: !(Maybe [StoredContentPart])
    , storedReasoningEncryptedContent :: !(Maybe Text)
    , storedReasoningStatus :: !(Maybe Text)
    , storedReasoningExtraFields :: !StoredOpaqueObject
    }
    deriving (Eq, Show)

data StoredItemReference = StoredItemReference
    { storedItemReferenceProviderItemId :: !Text
    , storedItemReferenceExtraFields :: !StoredOpaqueObject
    }
    deriving (Eq, Show)

data StoredTaggedItem = StoredTaggedItem
    { storedTaggedItemRepresentation :: !StoredResponseItemRepresentation
    , storedTaggedItemWireTag :: !Text
    , storedTaggedItemFields :: !StoredOpaqueObject
    }
    deriving (Eq, Show)

data StoredResponseItem
    = StoredMessageItem !StoredMessage
    | StoredFunctionCallItem !StoredFunctionCall
    | StoredFunctionCallOutputItem !StoredFunctionCallOutput
    | StoredCustomToolCallItem !StoredCustomToolCall
    | StoredCustomToolCallOutputItem !StoredCustomToolCallOutput
    | StoredReasoningItem !StoredReasoning
    | StoredItemReferenceItem !StoredItemReference
    | StoredTaggedResponseItem !StoredTaggedItem
    deriving (Eq, Show)

storedResponseItemType :: StoredResponseItem -> Text
storedResponseItemType = \case
    StoredMessageItem{} -> "message"
    StoredFunctionCallItem{} -> "function_call"
    StoredFunctionCallOutputItem{} -> "function_call_output"
    StoredCustomToolCallItem{} -> "custom_tool_call"
    StoredCustomToolCallOutputItem{} -> "custom_tool_call_output"
    StoredReasoningItem{} -> "reasoning"
    StoredItemReferenceItem{} -> "item_reference"
    StoredTaggedResponseItem item -> item.storedTaggedItemWireTag

storedResponseItemRepresentation
    :: StoredResponseItem
    -> StoredResponseItemRepresentation
storedResponseItemRepresentation = \case
    StoredMessageItem{} -> StoredCoreRepresentation
    StoredFunctionCallItem{} -> StoredCoreRepresentation
    StoredFunctionCallOutputItem{} -> StoredCoreRepresentation
    StoredCustomToolCallItem{} -> StoredCoreRepresentation
    StoredCustomToolCallOutputItem{} -> StoredCoreRepresentation
    StoredReasoningItem{} -> StoredCoreRepresentation
    StoredItemReferenceItem{} -> StoredCoreRepresentation
    StoredTaggedResponseItem item -> item.storedTaggedItemRepresentation
