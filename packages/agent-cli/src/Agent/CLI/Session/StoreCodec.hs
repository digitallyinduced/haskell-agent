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
                encodeObject
                    (insertOptionalJson
                        "internal_chat_message_metadata_passthrough"
                        message.passthrough
                        message.extraFields)
            }
    FunctionCallItem call ->
        StoredFunctionCallItem StoredFunctionCall
            { storedFunctionCallProviderItemId = call.itemId
            , storedFunctionCallCallId = call.callId
            , storedFunctionCallName = call.name
            , storedFunctionCallArguments = call.arguments
            , storedFunctionCallStatus = itemStatusText <$> call.status
            , storedFunctionCallExtraFields =
                encodeObject
                    (insertOptionalJson "namespace" call.namespace $
                        insertOptionalJson
                            "encrypted_function_args"
                            call.encryptedFunctionArgs
                            call.extraFields)
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
                encodeObject
                    (insertOptionalJson "name" output.name $
                        insertOptionalJson
                            "namespace"
                            output.namespace
                            output.extraFields)
            }
    CustomToolCallItem call ->
        StoredCustomToolCallItem StoredCustomToolCall
            { storedCustomToolCallProviderItemId = call.itemId
            , storedCustomToolCallCallId = call.callId
            , storedCustomToolCallName = call.name
            , storedCustomToolCallInput = call.input
            , storedCustomToolCallStatus = itemStatusText <$> call.status
            , storedCustomToolCallExtraFields =
                encodeObject
                    (insertOptionalJson
                        "namespace"
                        call.namespace
                        call.extraFields)
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
                encodeObject output.extraFields
            }
    ComputerCallItem item ->
        storedTypedKnownItem "computer_call" item
    ComputerCallOutputItem item ->
        storedTypedKnownItem "computer_call_output" item
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
                encodeObject reasoning.extraFields
            }
    ItemReferenceValue reference ->
        StoredItemReferenceItem StoredItemReference
            { storedItemReferenceProviderItemId = reference.itemId
            , storedItemReferenceExtraFields =
                encodeObject reference.extraFields
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
            , storedTaggedItemFields = encodeObject tagged.fields
            }
    UnknownResponseItem tagged ->
        StoredTaggedResponseItem StoredTaggedItem
            { storedTaggedItemRepresentation = StoredUnknownRepresentation
            , storedTaggedItemWireTag = tagged.tag
            , storedTaggedItemFields = encodeObject tagged.fields
            }

fromStoredResponseItem :: StoredResponseItem -> Either Text ResponseItem
fromStoredResponseItem = \case
    StoredMessageItem message -> do
        content <- fromStoredMessageContent message.storedMessageContent
        role <- responseRoleFromText message.storedMessageRole
        status <- traverse itemStatusFromText message.storedMessageStatus
        extraFields0 <- decodeObject
            "stored message extra fields"
            message.storedMessageExtraFields
        let (passthrough, extraFields) =
                takeOptionalJson extraFields0
                    "internal_chat_message_metadata_passthrough"
        Right $ MessageItem ResponseMessage
            { messageId = message.storedMessageProviderItemId
            , content
            , role
            , status
            , phase = message.storedMessagePhase
            , passthrough
            , extraFields
            }
    StoredFunctionCallItem call -> do
        status <- traverse itemStatusFromText call.storedFunctionCallStatus
        extraFields0 <- decodeObject
            "stored function-call extra fields"
            call.storedFunctionCallExtraFields
        let (namespace, extraFields1) =
                takeOptionalJson extraFields0 "namespace"
            (encryptedFunctionArgs, extraFields) =
                takeOptionalJson extraFields1 "encrypted_function_args"
        Right $ FunctionCallItem FunctionCall
            { itemId = call.storedFunctionCallProviderItemId
            , callId = call.storedFunctionCallCallId
            , name = call.storedFunctionCallName
            , namespace
            , arguments = call.storedFunctionCallArguments
            , encryptedFunctionArgs
            , status
            , extraFields
            }
    StoredFunctionCallOutputItem output -> do
        value <- fromStoredToolOutput
            "stored function-call output"
            output.storedFunctionCallOutputValue
        status <- traverse
            itemStatusFromText
            output.storedFunctionCallOutputStatus
        extraFields0 <- decodeObject
            "stored function-call-output extra fields"
            output.storedFunctionCallOutputExtraFields
        let (name, extraFields1) =
                takeOptionalJson extraFields0 "name"
            (namespace, extraFields) =
                takeOptionalJson extraFields1 "namespace"
        Right $ FunctionCallOutputItem FunctionCallOutput
            { itemId = output.storedFunctionCallOutputProviderItemId
            , callId = output.storedFunctionCallOutputCallId
            , name
            , namespace
            , output = value
            , status
            , extraFields
            }
    StoredCustomToolCallItem call -> do
        status <- traverse itemStatusFromText call.storedCustomToolCallStatus
        extraFields0 <- decodeObject
            "stored custom-tool-call extra fields"
            call.storedCustomToolCallExtraFields
        let (namespace, extraFields) =
                takeOptionalJson extraFields0 "namespace"
        Right $ CustomToolCallItem CustomToolCall
            { itemId = call.storedCustomToolCallProviderItemId
            , callId = call.storedCustomToolCallCallId
            , name = call.storedCustomToolCallName
            , namespace
            , input = call.storedCustomToolCallInput
            , status
            , extraFields
            }
    StoredCustomToolCallOutputItem output -> do
        value <- fromStoredToolOutput
            "stored custom-tool-call output"
            output.storedCustomToolCallOutputValue
        status <- traverse
            itemStatusFromText
            output.storedCustomToolCallOutputStatus
        extraFields <- decodeObject
            "stored custom-tool-call-output extra fields"
            output.storedCustomToolCallOutputExtraFields
        Right $ CustomToolCallOutputItem CustomToolCallOutput
            { itemId = output.storedCustomToolCallOutputProviderItemId
            , callId = output.storedCustomToolCallOutputCallId
            , name = output.storedCustomToolCallOutputName
            , output = value
            , status
            , extraFields
            }
    StoredReasoningItem reasoning -> do
        summary <- traverse
            fromStoredSummaryPart
            reasoning.storedReasoningSummary
        content <- traverse
            (traverse fromStoredContentPart)
            reasoning.storedReasoningContent
        status <- traverse itemStatusFromText reasoning.storedReasoningStatus
        extraFields <- decodeObject
            "stored reasoning extra fields"
            reasoning.storedReasoningExtraFields
        Right $ ReasoningItemValue ReasoningItem
            { itemId = reasoning.storedReasoningProviderItemId
            , summary
            , content
            , encryptedContent = reasoning.storedReasoningEncryptedContent
            , status
            , extraFields
            }
    StoredItemReferenceItem reference -> do
        extraFields <- decodeObject
            "stored item-reference extra fields"
            reference.storedItemReferenceExtraFields
        Right $ ItemReferenceValue ItemReference
            { itemId = reference.storedItemReferenceProviderItemId
            , extraFields
            }
    StoredTaggedResponseItem tagged -> do
        fields <- decodeObject
            "stored tagged response-item fields"
            tagged.storedTaggedItemFields
        let value = TaggedObject
                { tag = tagged.storedTaggedItemWireTag
                , fields
                }
        case tagged.storedTaggedItemRepresentation of
            StoredKnownRepresentation ->
                decodeKnownTaggedItem value
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
    , storedReasoningSummaryPartExtraFields =
        encodeObject part.extraFields
    }

fromStoredSummaryPart
    :: StoredReasoningSummaryPart
    -> Either Text ReasoningSummaryPart
fromStoredSummaryPart part = do
    extraFields <- decodeObject
        "stored reasoning-summary extra fields"
        part.storedReasoningSummaryPartExtraFields
    Right ReasoningSummaryPart
        { partType = part.storedReasoningSummaryPartType
        , text = part.storedReasoningSummaryPartText
        , extraFields
        }

toStoredContentPart :: ResponseContentPart -> StoredContentPart
toStoredContentPart = \case
    InputTextPart{text, promptCacheBreakpoint, extraFields} ->
        (emptyStoredContentPart "input_text" extraFields)
            { storedContentPartText = Just text
            , storedContentPartPromptCacheBreakpoint =
                encodeValue <$> promptCacheBreakpoint
            }
    InputImagePart
        { detail, fileId, imageUrl, promptCacheBreakpoint, extraFields } ->
            let (storedImageUrl, storedImageBinary) =
                    separateInlineBinary imageUrl
            in (emptyStoredContentPart "input_image" extraFields)
                { storedContentPartDetail = detail
                , storedContentPartFileId = fileId
                , storedContentPartImageUrl = storedImageUrl
                , storedContentPartImageBinary = storedImageBinary
                , storedContentPartPromptCacheBreakpoint =
                    encodeValue <$> promptCacheBreakpoint
                }
    InputFilePart
        { detail, fileData, fileId, fileUrl, filename
        , promptCacheBreakpoint, extraFields
        } ->
            let (storedFileData, storedFileBinary) =
                    separateInlineBinary fileData
            in (emptyStoredContentPart "input_file" extraFields)
                { storedContentPartDetail = detail
                , storedContentPartFileData = storedFileData
                , storedContentPartFileBinary = storedFileBinary
                , storedContentPartFileId = fileId
                , storedContentPartFileUrl = fileUrl
                , storedContentPartFilename = filename
                , storedContentPartPromptCacheBreakpoint =
                    encodeValue <$> promptCacheBreakpoint
                }
    InputAudioPart{inputAudio, extraFields} ->
        (emptyStoredContentPart "input_audio" extraFields)
            { storedContentPartInputAudio = Just (encodeValue inputAudio)
            }
    OutputTextPart{text, annotations, logprobs, extraFields} ->
        (emptyStoredContentPart "output_text" extraFields)
            { storedContentPartText = Just text
            , storedContentPartAnnotations =
                encodeValue . Aeson.toJSON <$> annotations
            , storedContentPartLogprobs =
                encodeValue . Aeson.toJSON <$> logprobs
            }
    RefusalPart{refusal, extraFields} ->
        (emptyStoredContentPart "refusal" extraFields)
            { storedContentPartRefusal = Just refusal
            }
    ReasoningTextPart{text, extraFields} ->
        (emptyStoredContentPart "reasoning_text" extraFields)
            { storedContentPartText = Just text
            }
    SummaryTextPart{text, extraFields} ->
        (emptyStoredContentPart "summary_text" extraFields)
            { storedContentPartText = Just text
            }
    EncryptedContentPart{encryptedContent, extraFields} ->
        (emptyStoredContentPart "encrypted_content" extraFields)
            { storedContentPartText = Just encryptedContent
            }
    PlainTextPart{text, extraFields} ->
        (emptyStoredContentPart "text" extraFields)
            { storedContentPartText = Just text
            }
    UnknownContentPart tagged ->
        emptyStoredContentPart tagged.tag tagged.fields

fromStoredContentPart
    :: StoredContentPart
    -> Either Text ResponseContentPart
fromStoredContentPart part = do
    extraFields <- decodeObject
        ("stored " <> part.storedContentPartType <> " extra fields")
        part.storedContentPartExtraFields
    case part.storedContentPartType of
        "input_text" ->
            InputTextPart
                <$> required "stored input_text text" part.storedContentPartText
                <*> traverse
                    (decodeValue "stored prompt_cache_breakpoint")
                    part.storedContentPartPromptCacheBreakpoint
                <*> pure extraFields
        "input_image" ->
            InputImagePart
                part.storedContentPartDetail
                part.storedContentPartFileId
                ( (renderInlineBinary
                        <$> part.storedContentPartImageBinary)
                    <|> part.storedContentPartImageUrl
                )
                <$> traverse
                    (decodeValue "stored prompt_cache_breakpoint")
                    part.storedContentPartPromptCacheBreakpoint
                <*> pure extraFields
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
                    (decodeValue "stored prompt_cache_breakpoint")
                    part.storedContentPartPromptCacheBreakpoint
                <*> pure extraFields
        "input_audio" ->
            InputAudioPart
                <$> (required
                        "stored input_audio value"
                        part.storedContentPartInputAudio
                        >>= decodeValue "stored input_audio value")
                <*> pure extraFields
        "output_text" ->
            OutputTextPart
                <$> required
                    "stored output_text text"
                    part.storedContentPartText
                <*> traverse
                    (decodeValues "stored output_text annotations")
                    part.storedContentPartAnnotations
                <*> traverse
                    (decodeValues "stored output_text logprobs")
                    part.storedContentPartLogprobs
                <*> pure extraFields
        "refusal" ->
            RefusalPart
                <$> required
                    "stored refusal text"
                    part.storedContentPartRefusal
                <*> pure extraFields
        "reasoning_text" ->
            ReasoningTextPart
                <$> required
                    "stored reasoning_text text"
                    part.storedContentPartText
                <*> pure extraFields
        "summary_text" ->
            SummaryTextPart
                <$> required
                    "stored summary_text text"
                    part.storedContentPartText
                <*> pure extraFields
        "encrypted_content" ->
            EncryptedContentPart
                <$> required
                    "stored encrypted_content text"
                    part.storedContentPartText
                <*> pure extraFields
        "text" ->
            PlainTextPart
                <$> required
                    "stored text part"
                    part.storedContentPartText
                <*> pure extraFields
        tag ->
            Right $ UnknownContentPart TaggedObject
                { tag
                , fields = extraFields
                }

emptyStoredContentPart
    :: Text
    -> Aeson.Object
    -> StoredContentPart
emptyStoredContentPart partType extraFields = StoredContentPart
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
    , storedContentPartExtraFields = encodeObject extraFields
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

toStoredToolOutput :: Aeson.Value -> StoredToolOutput
toStoredToolOutput = \case
    Aeson.String value -> StoredToolOutput
        { storedToolOutputKind = StoredToolOutputText
        , storedToolOutputText = value
        }
    value -> StoredToolOutput
        { storedToolOutputKind = StoredToolOutputEncoded
        , storedToolOutputText = encodeAeson value
        }

fromStoredToolOutput
    :: Text
    -> StoredToolOutput
    -> Either Text Aeson.Value
fromStoredToolOutput label output =
    case output.storedToolOutputKind of
        StoredToolOutputText ->
            Right (Aeson.String output.storedToolOutputText)
        StoredToolOutputEncoded ->
            decodeAeson label output.storedToolOutputText

storedTypedKnownItem :: Aeson.ToJSON a => Text -> a -> StoredResponseItem
storedTypedKnownItem tag value =
    StoredTaggedResponseItem StoredTaggedItem
        { storedTaggedItemRepresentation = StoredKnownRepresentation
        , storedTaggedItemWireTag = tag
        , storedTaggedItemFields =
            encodeObject (objectWithoutType (Aeson.toJSON value))
        }

decodeKnownTaggedItem :: TaggedObject -> Either Text ResponseItem
decodeKnownTaggedItem tagged =
    case parseResponseItemType tagged.tag of
        itemType | isPromotedKnownItem itemType ->
            case Aeson.fromJSON (Aeson.toJSON tagged) of
                Aeson.Success item -> Right item
                Aeson.Error err ->
                    Left ("stored " <> tagged.tag <> ": " <> Text.pack err)
        itemType -> Right (KnownResponseItem itemType tagged)

isPromotedKnownItem :: ResponseItemType -> Bool
isPromotedKnownItem = \case
    ItemAgentMessage -> True
    ItemAdditionalTools -> True
    ItemComputerCall -> True
    ItemComputerCallOutput -> True
    ItemLocalShellCall -> True
    ItemToolSearchCall -> True
    ItemToolSearchOutput -> True
    ItemWebSearchCall -> True
    ItemImageGenerationCall -> True
    ItemCompaction -> True
    ItemCompactionTrigger -> True
    ItemContextCompaction -> True
    _ -> False

insertOptionalJson
    :: Aeson.ToJSON a
    => Text
    -> Maybe a
    -> Aeson.Object
    -> Aeson.Object
insertOptionalJson name = \case
    Nothing -> id
    Just value ->
        KeyMap.insert (Key.fromText name) (Aeson.toJSON value)

takeOptionalJson
    :: Aeson.FromJSON a
    => Aeson.Object
    -> Text
    -> (Maybe a, Aeson.Object)
takeOptionalJson object name =
    let key = Key.fromText name
    in case KeyMap.lookup key object of
        Just value
            | Aeson.Success parsed <- Aeson.fromJSON value ->
                (Just parsed, KeyMap.delete key object)
        _ -> (Nothing, object)

objectWithoutType :: Aeson.Value -> Aeson.Object
objectWithoutType = \case
    Aeson.Object object -> KeyMap.delete "type" object
    _ -> KeyMap.empty

encodeObject :: Aeson.Object -> StoredOpaqueObject
encodeObject =
    StoredOpaqueObject . encodeAeson . Aeson.Object

decodeObject
    :: Text
    -> StoredOpaqueObject
    -> Either Text Aeson.Object
decodeObject label value =
    decodeAeson label value.storedOpaqueObjectText >>= \case
        Aeson.Object object -> Right object
        _ -> Left (label <> ": expected an object")

encodeValue :: Aeson.Value -> StoredOpaqueValue
encodeValue = StoredOpaqueValue . encodeAeson

decodeValue
    :: Text
    -> StoredOpaqueValue
    -> Either Text Aeson.Value
decodeValue label value =
    decodeAeson label value.storedOpaqueValueText

decodeValues
    :: Text
    -> StoredOpaqueValue
    -> Either Text [Aeson.Value]
decodeValues label value = do
    decoded <- decodeValue label value
    case Aeson.fromJSON decoded of
        Aeson.Error err -> Left (label <> ": " <> Text.pack err)
        Aeson.Success values -> Right values

encodeAeson :: Aeson.Value -> Text
encodeAeson =
    TextEncoding.decodeUtf8
        . LazyByteString.toStrict
        . Aeson.encode

decodeAeson :: Text -> Text -> Either Text Aeson.Value
decodeAeson label value =
    case Aeson.eitherDecodeStrict' (TextEncoding.encodeUtf8 value) of
        Left err -> Left (label <> ": " <> Text.pack err)
        Right decoded -> Right decoded

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
