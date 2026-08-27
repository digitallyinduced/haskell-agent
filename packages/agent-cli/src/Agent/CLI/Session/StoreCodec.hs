{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Boundary codecs between wire response items and typed storage records.
module Agent.CLI.Session.StoreCodec
    ( fromStoredResponseItem
    , toStoredResponseItem
    ) where

import Control.Applicative ((<|>))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as LazyByteString
import Agent.Json.Decode qualified as Hermes
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding

import Agent.Responses.Types
    ( CustomToolCall(..)
    , CustomToolCallOutput(..)
    , FunctionCall(..)
    , FunctionCallOutput(..)
    , ItemReference(..)
    , ItemStatus(..)
    , MessageContent(..)
    , ReasoningItem(..)
    , ReasoningSummaryPart(..)
    , ResponseContentPart(..)
    , ResponseItem(..)
    , ResponseItemType(..)
    , ResponseMessage(..)
    , ResponseRole(..)
    , TaggedObject(..)
    , parseResponseItemType
    )
import Agent.Responses.Types.Items (responseItemDecoder)
import Agent.Responses.Types.Items.Known (internalChatMetadataDecoder)
import Agent.Json (RawJson, rawJsonBytes, rawJsonDecoder)
import Agent.Store.SessionItem

toStoredResponseItem :: ResponseItem -> StoredResponseItem
toStoredResponseItem = \case
    MessageItem message ->
        StoredMessageItem StoredMessage
            { storedMessageProviderItemId = message.messageId
            , storedMessageContent = toStoredMessageContent message.content
            , storedMessageRole = responseRoleText message.role
            , storedMessageStatus = itemStatusText <$> message.status
            , storedMessagePhase = message.phase
            , storedMessageExtraFields =
                encodeKnownFields
                    [("internal_chat_message_metadata_passthrough", Aeson.toJSON <$> message.passthrough)]
            }
    FunctionCallItem call ->
        StoredFunctionCallItem StoredFunctionCall
            { storedFunctionCallProviderItemId = call.itemId
            , storedFunctionCallCallId = call.callId
            , storedFunctionCallName = call.name
            , storedFunctionCallArguments = call.arguments
            , storedFunctionCallStatus = itemStatusText <$> call.status
            , storedFunctionCallExtraFields =
                encodeKnownFields
                    [ ("namespace", Aeson.toJSON <$> call.namespace)
                    , ("encrypted_function_args", Aeson.toJSON <$> call.encryptedFunctionArgs)
                    ]
            }
    FunctionCallOutputItem output ->
        StoredFunctionCallOutputItem StoredFunctionCallOutput
            { storedFunctionCallOutputProviderItemId = output.itemId
            , storedFunctionCallOutputCallId = output.callId
            , storedFunctionCallOutputValue =
                toStoredToolOutput output.output
            , storedFunctionCallOutputStatus =
                itemStatusText <$> output.status
            , storedFunctionCallOutputExtraFields =
                encodeKnownFields
                    [ ("name", Aeson.toJSON <$> output.name)
                    , ("namespace", Aeson.toJSON <$> output.namespace)
                    ]
            }
    CustomToolCallItem call ->
        StoredCustomToolCallItem StoredCustomToolCall
            { storedCustomToolCallProviderItemId = call.itemId
            , storedCustomToolCallCallId = call.callId
            , storedCustomToolCallName = call.name
            , storedCustomToolCallInput = call.input
            , storedCustomToolCallStatus = itemStatusText <$> call.status
            , storedCustomToolCallExtraFields =
                encodeKnownFields
                    [("namespace", Aeson.toJSON <$> call.namespace)]
            }
    CustomToolCallOutputItem output ->
        StoredCustomToolCallOutputItem StoredCustomToolCallOutput
            { storedCustomToolCallOutputProviderItemId = output.itemId
            , storedCustomToolCallOutputCallId = output.callId
            , storedCustomToolCallOutputName = output.name
            , storedCustomToolCallOutputValue =
                toStoredToolOutput output.output
            , storedCustomToolCallOutputStatus =
                itemStatusText <$> output.status
            , storedCustomToolCallOutputExtraFields =
                emptyOpaqueObject
            }
    ReasoningItemValue reasoning ->
        StoredReasoningItem StoredReasoning
            { storedReasoningProviderItemId = reasoning.itemId
            , storedReasoningSummary =
                map toStoredSummaryPart reasoning.summary
            , storedReasoningContent =
                fmap (map toStoredContentPart) reasoning.content
            , storedReasoningEncryptedContent =
                reasoning.encryptedContent
            , storedReasoningStatus = itemStatusText <$> reasoning.status
            , storedReasoningExtraFields =
                emptyOpaqueObject
            }
    ItemReferenceValue reference ->
        StoredItemReferenceItem StoredItemReference
            { storedItemReferenceProviderItemId = reference.itemId
            , storedItemReferenceExtraFields =
                emptyOpaqueObject
            }
    AgentMessageItem message ->
        storedTypedKnownItem "agent_message" message
    AdditionalToolsItemValue item ->
        storedTypedKnownItem "additional_tools" item
    LocalShellCallItem item ->
        storedTypedKnownItem "local_shell_call" item
    ToolSearchCallItem item ->
        storedTypedKnownItem "tool_search_call" item
    ToolSearchOutputItem item ->
        storedTypedKnownItem "tool_search_output" item
    WebSearchCallItem item ->
        storedTypedKnownItem "web_search_call" item
    ImageGenerationCallItem item ->
        storedTypedKnownItem "image_generation_call" item
    CompactionItemValue item ->
        storedTypedKnownItem "compaction" item
    CompactionTriggerItemValue item ->
        storedTypedKnownItem "compaction_trigger" item
    ContextCompactionItemValue item ->
        storedTypedKnownItem "context_compaction" item
    KnownResponseItem _ tagged ->
        StoredTaggedResponseItem StoredTaggedItem
            { storedTaggedItemRepresentation = StoredKnownRepresentation
            , storedTaggedItemWireTag = tagged.tag
            , storedTaggedItemFields = emptyOpaqueObject
            }
    UnknownResponseItem tagged ->
        StoredTaggedResponseItem StoredTaggedItem
            { storedTaggedItemRepresentation = StoredUnknownRepresentation
            , storedTaggedItemWireTag = tagged.tag
            , storedTaggedItemFields = emptyOpaqueObject
            }

fromStoredResponseItem :: StoredResponseItem -> Either Text ResponseItem
fromStoredResponseItem = \case
    StoredMessageItem message -> do
        content <- fromStoredMessageContent message.storedMessageContent
        role <- responseRoleFromText message.storedMessageRole
        status <- traverse itemStatusFromText message.storedMessageStatus
        passthrough <- decodeOptionalField
            "stored message extra fields"
            "internal_chat_message_metadata_passthrough"
            internalChatMetadataDecoder
            message.storedMessageExtraFields
        Right $ MessageItem ResponseMessage
            { messageId = message.storedMessageProviderItemId
            , content
            , role
            , status
            , phase = message.storedMessagePhase
            , passthrough
            }
    StoredFunctionCallItem call -> do
        status <- traverse itemStatusFromText call.storedFunctionCallStatus
        namespace <- decodeOptionalField "stored function-call extra fields"
            "namespace" Hermes.text call.storedFunctionCallExtraFields
        encryptedFunctionArgs <- decodeOptionalField
            "stored function-call extra fields"
            "encrypted_function_args"
            (Hermes.list Hermes.text)
            call.storedFunctionCallExtraFields
        Right $ FunctionCallItem FunctionCall
            { itemId = call.storedFunctionCallProviderItemId
            , callId = call.storedFunctionCallCallId
            , name = call.storedFunctionCallName
            , namespace
            , arguments = call.storedFunctionCallArguments
            , encryptedFunctionArgs
            , status
            }
    StoredFunctionCallOutputItem output -> do
        value <- fromStoredToolOutput
            "stored function-call output"
            output.storedFunctionCallOutputValue
        status <- traverse
            itemStatusFromText
            output.storedFunctionCallOutputStatus
        name <- decodeOptionalField "stored function-call-output extra fields"
            "name" Hermes.text output.storedFunctionCallOutputExtraFields
        namespace <- decodeOptionalField "stored function-call-output extra fields"
            "namespace" Hermes.text output.storedFunctionCallOutputExtraFields
        Right $ FunctionCallOutputItem FunctionCallOutput
            { itemId = output.storedFunctionCallOutputProviderItemId
            , callId = output.storedFunctionCallOutputCallId
            , name
            , namespace
            , output = value
            , status
            }
    StoredCustomToolCallItem call -> do
        status <- traverse itemStatusFromText call.storedCustomToolCallStatus
        namespace <- decodeOptionalField "stored custom-tool-call extra fields"
            "namespace" Hermes.text call.storedCustomToolCallExtraFields
        Right $ CustomToolCallItem CustomToolCall
            { itemId = call.storedCustomToolCallProviderItemId
            , callId = call.storedCustomToolCallCallId
            , name = call.storedCustomToolCallName
            , namespace
            , input = call.storedCustomToolCallInput
            , status
            }
    StoredCustomToolCallOutputItem output -> do
        value <- fromStoredToolOutput
            "stored custom-tool-call output"
            output.storedCustomToolCallOutputValue
        status <- traverse
            itemStatusFromText
            output.storedCustomToolCallOutputStatus
        Right $ CustomToolCallOutputItem CustomToolCallOutput
            { itemId = output.storedCustomToolCallOutputProviderItemId
            , callId = output.storedCustomToolCallOutputCallId
            , name = output.storedCustomToolCallOutputName
            , output = value
            , status
            }
    StoredReasoningItem reasoning -> do
        summary <- traverse
            fromStoredSummaryPart
            reasoning.storedReasoningSummary
        content <- traverse
            (traverse fromStoredContentPart)
            reasoning.storedReasoningContent
        status <- traverse itemStatusFromText reasoning.storedReasoningStatus
        Right $ ReasoningItemValue ReasoningItem
            { itemId = reasoning.storedReasoningProviderItemId
            , summary
            , content
            , encryptedContent = reasoning.storedReasoningEncryptedContent
            , status
            }
    StoredItemReferenceItem reference -> do
        Right $ ItemReferenceValue ItemReference
            { itemId = reference.storedItemReferenceProviderItemId
            }
    StoredTaggedResponseItem tagged -> do
        let value = TaggedObject { tag = tagged.storedTaggedItemWireTag }
        case tagged.storedTaggedItemRepresentation of
            StoredKnownRepresentation ->
                decodeKnownTaggedItem tagged
            StoredUnknownRepresentation ->
                Right (UnknownResponseItem value)
            StoredCoreRepresentation ->
                Left "stored tagged response item has a core representation"

toStoredMessageContent :: MessageContent -> StoredMessageContent
toStoredMessageContent = \case
    MessageContentText value -> StoredMessageText value
    MessageContentParts parts ->
        StoredMessageParts (map toStoredContentPart parts)

fromStoredMessageContent
    :: StoredMessageContent
    -> Either Text MessageContent
fromStoredMessageContent = \case
    StoredMessageText value -> Right (MessageContentText value)
    StoredMessageParts parts ->
        MessageContentParts <$> traverse fromStoredContentPart parts

toStoredSummaryPart
    :: ReasoningSummaryPart
    -> StoredReasoningSummaryPart
toStoredSummaryPart part = StoredReasoningSummaryPart
    { storedReasoningSummaryPartType = part.partType
    , storedReasoningSummaryPartText = part.text
    , storedReasoningSummaryPartExtraFields = emptyOpaqueObject
    }

fromStoredSummaryPart
    :: StoredReasoningSummaryPart
    -> Either Text ReasoningSummaryPart
fromStoredSummaryPart part =
    Right ReasoningSummaryPart
        { partType = part.storedReasoningSummaryPartType
        , text = part.storedReasoningSummaryPartText
        }

toStoredContentPart :: ResponseContentPart -> StoredContentPart
toStoredContentPart = \case
    InputTextPart{text, promptCacheBreakpoint} ->
        (emptyStoredContentPart "input_text")
            { storedContentPartText = Just text
            , storedContentPartPromptCacheBreakpoint =
                encodeRawValue <$> promptCacheBreakpoint
            }
    InputImagePart
        { detail, fileId, imageUrl, promptCacheBreakpoint } ->
            let (storedImageUrl, storedImageBinary) =
                    separateInlineBinary imageUrl
            in (emptyStoredContentPart "input_image")
                { storedContentPartDetail = detail
                , storedContentPartFileId = fileId
                , storedContentPartImageUrl = storedImageUrl
                , storedContentPartImageBinary = storedImageBinary
                , storedContentPartPromptCacheBreakpoint =
                    encodeRawValue <$> promptCacheBreakpoint
                }
    InputFilePart
        { detail, fileData, fileId, fileUrl, filename
        , promptCacheBreakpoint
        } ->
            let (storedFileData, storedFileBinary) =
                    separateInlineBinary fileData
            in (emptyStoredContentPart "input_file")
                { storedContentPartDetail = detail
                , storedContentPartFileData = storedFileData
                , storedContentPartFileBinary = storedFileBinary
                , storedContentPartFileId = fileId
                , storedContentPartFileUrl = fileUrl
                , storedContentPartFilename = filename
                , storedContentPartPromptCacheBreakpoint =
                    encodeRawValue <$> promptCacheBreakpoint
                }
    InputAudioPart{inputAudio} ->
        (emptyStoredContentPart "input_audio")
            { storedContentPartInputAudio = Just (encodeRawValue inputAudio)
            }
    OutputTextPart{text, annotations, logprobs} ->
        (emptyStoredContentPart "output_text")
            { storedContentPartText = Just text
            , storedContentPartAnnotations =
                encodeRawValues <$> annotations
            , storedContentPartLogprobs =
                encodeRawValues <$> logprobs
            }
    RefusalPart{refusal} ->
        (emptyStoredContentPart "refusal")
            { storedContentPartRefusal = Just refusal
            }
    ReasoningTextPart{text} ->
        (emptyStoredContentPart "reasoning_text")
            { storedContentPartText = Just text
            }
    SummaryTextPart{text} ->
        (emptyStoredContentPart "summary_text")
            { storedContentPartText = Just text
            }
    EncryptedContentPart{encryptedContent} ->
        (emptyStoredContentPart "encrypted_content")
            { storedContentPartText = Just encryptedContent
            }
    PlainTextPart{text} ->
        (emptyStoredContentPart "text")
            { storedContentPartText = Just text
            }
    UnknownContentPart tagged ->
        emptyStoredContentPart tagged.tag

fromStoredContentPart
    :: StoredContentPart
    -> Either Text ResponseContentPart
fromStoredContentPart part =
    case part.storedContentPartType of
        "input_text" ->
            InputTextPart
                <$> required "stored input_text text" part.storedContentPartText
                <*> traverse
                    (decodeRawValue "stored prompt_cache_breakpoint")
                    part.storedContentPartPromptCacheBreakpoint
        "input_image" ->
            InputImagePart
                part.storedContentPartDetail
                part.storedContentPartFileId
                ( (renderInlineBinary
                        <$> part.storedContentPartImageBinary)
                    <|> part.storedContentPartImageUrl
                )
                <$> traverse
                    (decodeRawValue "stored prompt_cache_breakpoint")
                    part.storedContentPartPromptCacheBreakpoint
        "input_file" ->
            InputFilePart
                part.storedContentPartDetail
                ( (renderInlineBinary
                        <$> part.storedContentPartFileBinary)
                    <|> part.storedContentPartFileData
                )
                part.storedContentPartFileId
                part.storedContentPartFileUrl
                part.storedContentPartFilename
                <$> traverse
                    (decodeRawValue "stored prompt_cache_breakpoint")
                    part.storedContentPartPromptCacheBreakpoint
        "input_audio" ->
            InputAudioPart
                <$> (required
                        "stored input_audio value"
                        part.storedContentPartInputAudio
                        >>= decodeRawValue "stored input_audio value")
        "output_text" ->
            OutputTextPart
                <$> required
                    "stored output_text text"
                    part.storedContentPartText
                <*> traverse
                    (decodeRawValues "stored output_text annotations")
                    part.storedContentPartAnnotations
                <*> traverse
                    (decodeRawValues "stored output_text logprobs")
                    part.storedContentPartLogprobs
        "refusal" ->
            RefusalPart
                <$> required
                    "stored refusal text"
                    part.storedContentPartRefusal
        "reasoning_text" ->
            ReasoningTextPart
                <$> required
                    "stored reasoning_text text"
                    part.storedContentPartText
        "summary_text" ->
            SummaryTextPart
                <$> required
                    "stored summary_text text"
                    part.storedContentPartText
        "encrypted_content" ->
            EncryptedContentPart
                <$> required
                    "stored encrypted_content text"
                    part.storedContentPartText
        "text" ->
            PlainTextPart
                <$> required
                    "stored text part"
                    part.storedContentPartText
        tag ->
            Right $ UnknownContentPart TaggedObject { tag }

emptyStoredContentPart :: Text -> StoredContentPart
emptyStoredContentPart partType = StoredContentPart
    { storedContentPartType = partType
    , storedContentPartText = Nothing
    , storedContentPartRefusal = Nothing
    , storedContentPartDetail = Nothing
    , storedContentPartFileData = Nothing
    , storedContentPartFileId = Nothing
    , storedContentPartFileUrl = Nothing
    , storedContentPartFilename = Nothing
    , storedContentPartImageUrl = Nothing
    , storedContentPartFileBinary = Nothing
    , storedContentPartImageBinary = Nothing
    , storedContentPartInputAudio = Nothing
    , storedContentPartPromptCacheBreakpoint = Nothing
    , storedContentPartAnnotations = Nothing
    , storedContentPartLogprobs = Nothing
    , storedContentPartExtraFields = emptyOpaqueObject
    }

separateInlineBinary
    :: Maybe Text
    -> (Maybe Text, Maybe StoredBinaryData)
separateInlineBinary value =
    case value >>= parseInlineBinary of
        Just binary -> (Nothing, Just binary)
        Nothing -> (value, Nothing)

parseInlineBinary :: Text -> Maybe StoredBinaryData
parseInlineBinary value = do
    body <- Text.stripPrefix "data:" value
    let (metadata, payloadWithComma) = Text.breakOn "," body
        base64Marker = ";base64"
    payload <- Text.stripPrefix "," payloadWithComma
    if Text.takeEnd (Text.length base64Marker) metadata /= base64Marker
        then Nothing
        else do
            let mimeType =
                    Text.dropEnd (Text.length base64Marker) metadata
            if Text.null mimeType
                then Nothing
                else case Base64.decode (TextEncoding.encodeUtf8 payload) of
                    Left _ -> Nothing
                    Right bytes -> Just StoredBinaryData
                        { storedBinaryDataMimeType = mimeType
                        , storedBinaryDataBytes = bytes
                        }

renderInlineBinary :: StoredBinaryData -> Text
renderInlineBinary binary =
    "data:"
        <> binary.storedBinaryDataMimeType
        <> ";base64,"
        <> TextEncoding.decodeUtf8
            (Base64.encode binary.storedBinaryDataBytes)

toStoredToolOutput :: RawJson -> StoredToolOutput
toStoredToolOutput value = StoredToolOutput
    { storedToolOutputKind = StoredToolOutputEncoded
    , storedToolOutputText = TextEncoding.decodeUtf8 (rawJsonBytes value)
    }

fromStoredToolOutput
    :: Text
    -> StoredToolOutput
    -> Either Text RawJson
fromStoredToolOutput label output =
    case output.storedToolOutputKind of
        StoredToolOutputText ->
            decodeRawJson label (encodeAeson (Aeson.String output.storedToolOutputText))
        StoredToolOutputEncoded ->
            decodeRawJson label output.storedToolOutputText

storedTypedKnownItem :: Aeson.ToJSON a => Text -> a -> StoredResponseItem
storedTypedKnownItem tag value =
    StoredTaggedResponseItem StoredTaggedItem
        { storedTaggedItemRepresentation = StoredKnownRepresentation
        , storedTaggedItemWireTag = tag
        , storedTaggedItemFields =
            encodeObject (objectWithoutType (Aeson.toJSON value))
        }

decodeKnownTaggedItem :: StoredTaggedItem -> Either Text ResponseItem
decodeKnownTaggedItem tagged =
    case parseResponseItemType tagged.storedTaggedItemWireTag of
        itemType | isPromotedKnownItem itemType ->
            decodeRawJsonWith
                ("stored " <> tagged.storedTaggedItemWireTag)
                responseItemDecoder
                (taggedItemJson tagged)
        itemType ->
            Right
                (KnownResponseItem itemType
                    (TaggedObject tagged.storedTaggedItemWireTag))

taggedItemJson :: StoredTaggedItem -> Text
taggedItemJson tagged =
    let typeObject = encodeAeson (Aeson.object
            ["type" Aeson..= tagged.storedTaggedItemWireTag])
        fields = Text.strip tagged.storedTaggedItemFields.storedOpaqueObjectText
        innerFields
            | Text.length fields >= 2 =
                Text.dropEnd 1 (Text.drop 1 fields)
            | otherwise = ""
    in if Text.null (Text.strip innerFields)
        then typeObject
        else Text.dropEnd 1 typeObject <> "," <> innerFields <> "}"

isPromotedKnownItem :: ResponseItemType -> Bool
isPromotedKnownItem = \case
    ItemAgentMessage -> True
    ItemAdditionalTools -> True
    ItemLocalShellCall -> True
    ItemToolSearchCall -> True
    ItemToolSearchOutput -> True
    ItemWebSearchCall -> True
    ItemImageGenerationCall -> True
    ItemCompaction -> True
    ItemCompactionTrigger -> True
    ItemContextCompaction -> True
    _ -> False

objectWithoutType :: Aeson.Value -> Aeson.Object
objectWithoutType = \case
    Aeson.Object object -> KeyMap.delete "type" object
    _ -> KeyMap.empty

encodeObject :: Aeson.Object -> StoredOpaqueObject
encodeObject =
    StoredOpaqueObject . encodeAeson . Aeson.Object

encodeRawValue :: RawJson -> StoredOpaqueValue
encodeRawValue = StoredOpaqueValue . TextEncoding.decodeUtf8 . rawJsonBytes

encodeRawValues :: [RawJson] -> StoredOpaqueValue
encodeRawValues =
    StoredOpaqueValue . encodeAeson

decodeRawValue :: Text -> StoredOpaqueValue -> Either Text RawJson
decodeRawValue label value =
    decodeRawJson label value.storedOpaqueValueText

decodeRawValues :: Text -> StoredOpaqueValue -> Either Text [RawJson]
decodeRawValues label value =
    decodeRawJsonWith
        label
        (Hermes.list rawJsonDecoder)
        value.storedOpaqueValueText

encodeAeson :: Aeson.ToJSON a => a -> Text
encodeAeson =
    TextEncoding.decodeUtf8
        . LazyByteString.toStrict
        . Aeson.encode

decodeRawJson :: Text -> Text -> Either Text RawJson
decodeRawJson label = decodeRawJsonWith label rawJsonDecoder

decodeRawJsonWith
    :: Text
    -> Hermes.Decoder a
    -> Text
    -> Either Text a
decodeRawJsonWith label decoder value =
    case Hermes.decodeEither decoder (TextEncoding.encodeUtf8 value) of
        Left err -> Left (label <> ": " <> Hermes.jsonErrorMessage err)
        Right decoded -> Right decoded

decodeOptionalField
    :: Text
    -> Text
    -> Hermes.Decoder a
    -> StoredOpaqueObject
    -> Either Text (Maybe a)
decodeOptionalField label key decoder value =
    decodeRawJsonWith label (Hermes.object (Hermes.atKeyOptional key decoder))
        value.storedOpaqueObjectText

encodeKnownFields :: [(Text, Maybe Aeson.Value)] -> StoredOpaqueObject
encodeKnownFields fields =
    StoredOpaqueObject . encodeAeson . Aeson.object $
        [ Key.fromText key Aeson..= value
        | (key, Just value) <- fields
        ]

emptyOpaqueObject :: StoredOpaqueObject
emptyOpaqueObject = StoredOpaqueObject "{}"

required :: Text -> Maybe a -> Either Text a
required label = maybe (Left (label <> " is missing")) Right

responseRoleText :: ResponseRole -> Text
responseRoleText = \case
    RoleUser -> "user"
    RoleAssistant -> "assistant"
    RoleSystem -> "system"
    RoleDeveloper -> "developer"
    RoleUnknown value -> value

responseRoleFromText :: Text -> Either Text ResponseRole
responseRoleFromText = Right . \case
    "user" -> RoleUser
    "assistant" -> RoleAssistant
    "system" -> RoleSystem
    "developer" -> RoleDeveloper
    value -> RoleUnknown value

itemStatusText :: ItemStatus -> Text
itemStatusText = \case
    ItemInProgress -> "in_progress"
    ItemCompleted -> "completed"
    ItemIncomplete -> "incomplete"
    ItemStatusUnknown value -> value

itemStatusFromText :: Text -> Either Text ItemStatus
itemStatusFromText = Right . \case
    "in_progress" -> ItemInProgress
    "completed" -> ItemCompleted
    "incomplete" -> ItemIncomplete
    value -> ItemStatusUnknown value
