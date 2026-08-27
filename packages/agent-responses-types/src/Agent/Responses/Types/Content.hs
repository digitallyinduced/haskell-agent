{-# LANGUAGE GADTs #-}

module Agent.Responses.Types.Content
    ( ResponseRole(..)
    , responseRoleEncoder
    , responseRoleDecoder
    , ItemStatus(..)
    , itemStatusEncoder
    , itemStatusDecoder
    , MessageContent(..)
    , messageContentEncoder
    , messageContentDecoder
    , ResponseContentPart(..)
    , responseContentPartEncoder
    , responseContentPartDecoder
    ) where

import Agent.Json
    ( Extensions
    , RawJson
    )
import qualified Agent.Json.Decoder as Decoder
import qualified Agent.Json.Encoder as Encoder
import Agent.Responses.Types.Common (TaggedObject(..))
import Data.Text (Text)

data ResponseRole = RoleUser | RoleAssistant | RoleSystem | RoleDeveloper | RoleUnknown !Text
    deriving stock (Eq, Show)

responseRoleText :: ResponseRole -> Text
responseRoleText = \case
    RoleUser -> "user"
    RoleAssistant -> "assistant"
    RoleSystem -> "system"
    RoleDeveloper -> "developer"
    RoleUnknown value -> value

responseRoleEncoder :: Encoder.Encoder ResponseRole
responseRoleEncoder = Encoder.contramap responseRoleText Encoder.text

responseRoleDecoder :: Decoder.Decoder ResponseRole
responseRoleDecoder = Decoder.mapDecoder decodeRole Decoder.text
  where
    decodeRole = \case
        "user" -> RoleUser
        "assistant" -> RoleAssistant
        "system" -> RoleSystem
        "developer" -> RoleDeveloper
        value -> RoleUnknown value

data ItemStatus = ItemInProgress | ItemCompleted | ItemIncomplete | ItemStatusUnknown !Text
    deriving stock (Eq, Show)

itemStatusText :: ItemStatus -> Text
itemStatusText = \case
    ItemInProgress -> "in_progress"
    ItemCompleted -> "completed"
    ItemIncomplete -> "incomplete"
    ItemStatusUnknown value -> value

itemStatusEncoder :: Encoder.Encoder ItemStatus
itemStatusEncoder = Encoder.contramap itemStatusText Encoder.text

itemStatusDecoder :: Decoder.Decoder ItemStatus
itemStatusDecoder = Decoder.mapDecoder decodeStatus Decoder.text
  where
    decodeStatus = \case
        "in_progress" -> ItemInProgress
        "completed" -> ItemCompleted
        "incomplete" -> ItemIncomplete
        value -> ItemStatusUnknown value

data MessageContent
    = MessageContentText !Text
    | MessageContentParts ![ResponseContentPart]
    deriving stock (Eq, Show)

-- The decoder first retains the value as validated opaque JSON, then dispatches
-- to the string or array codec.  This keeps the public API free of a DOM type.
messageContentDecoder :: Decoder.Decoder MessageContent
messageContentDecoder = Decoder.byType \case
    Decoder.JsonString -> MessageContentText <$> Decoder.text
    Decoder.JsonArray -> MessageContentParts <$> Decoder.array responseContentPartDecoder
    _ -> Decoder.mapEither (const (Left "MessageContent: expected string or array")) Decoder.skip

messageContentEncoder :: Encoder.Encoder MessageContent
messageContentEncoder = Encoder.choose \case
    MessageContentText {} ->
        Encoder.contramap messageText Encoder.text
    MessageContentParts {} ->
        Encoder.contramap
            messageParts
            (Encoder.list responseContentPartEncoder)
  where
    messageText = \case
        MessageContentText value -> value
        MessageContentParts _ ->
            error "messageContentEncoder: impossible parts variant"
    messageParts = \case
        MessageContentParts value -> value
        MessageContentText _ ->
            error "messageContentEncoder: impossible text variant"

data ResponseContentPart
    = InputTextPart
        { text                  :: !Text
        , promptCacheBreakpoint :: !(Maybe RawJson)
        , extraFields           :: !Extensions
        }
    | InputImagePart
        { detail                :: !(Maybe Text)
        , fileId                :: !(Maybe Text)
        , imageUrl              :: !(Maybe Text)
        , promptCacheBreakpoint :: !(Maybe RawJson)
        , extraFields           :: !Extensions
        }
    | InputFilePart
        { detail                :: !(Maybe Text)
        , fileData              :: !(Maybe Text)
        , fileId                :: !(Maybe Text)
        , fileUrl               :: !(Maybe Text)
        , filename              :: !(Maybe Text)
        , promptCacheBreakpoint :: !(Maybe RawJson)
        , extraFields           :: !Extensions
        }
    | InputAudioPart
        { inputAudio  :: !RawJson
        , extraFields :: !Extensions
        }
    | OutputTextPart
        { text        :: !Text
        , annotations :: !(Maybe [RawJson])
        , logprobs    :: !(Maybe [RawJson])
        , extraFields :: !Extensions
        }
    | RefusalPart
        { refusal     :: !Text
        , extraFields :: !Extensions
        }
    | ReasoningTextPart
        { text        :: !Text
        , extraFields :: !Extensions
        }
    | SummaryTextPart
        { text        :: !Text
        , extraFields :: !Extensions
        }
    | EncryptedContentPart
        { encryptedContent :: !Text
        , extraFields      :: !Extensions
        }
    | PlainTextPart
        { text        :: !Text
        , extraFields :: !Extensions
        }
    | UnknownContentPart !TaggedObject
    deriving stock (Eq, Show)

data ParsedPart = ParsedPart
    { parsedType :: !Text
    , parsedText :: !(Maybe Text)
    , parsedPromptCacheBreakpoint :: !(Maybe RawJson)
    , parsedDetail :: !(Maybe Text)
    , parsedFileData :: !(Maybe Text)
    , parsedFileId :: !(Maybe Text)
    , parsedFileUrl :: !(Maybe Text)
    , parsedImageUrl :: !(Maybe Text)
    , parsedFilename :: !(Maybe Text)
    , parsedInputAudio :: !(Maybe RawJson)
    , parsedAnnotations :: !(Maybe [RawJson])
    , parsedLogprobs :: !(Maybe [RawJson])
    , parsedRefusal :: !(Maybe Text)
    , parsedEncryptedContent :: !(Maybe Text)
    , parsedExtensions :: !Extensions
    }

partDecoder :: Decoder.Decoder ResponseContentPart
partDecoder =
    Decoder.mapEither finish $
        Decoder.objectFields $
            ParsedPart
                <$> Decoder.requiredField "type" Decoder.text
                <*> Decoder.optionalField "text" Decoder.text
                <*> Decoder.optionalField
                    "prompt_cache_breakpoint"
                    Decoder.rawJson
                <*> Decoder.optionalField "detail" Decoder.text
                <*> Decoder.optionalField "file_data" Decoder.text
                <*> Decoder.optionalField "file_id" Decoder.text
                <*> Decoder.optionalField "file_url" Decoder.text
                <*> Decoder.optionalField "image_url" Decoder.text
                <*> Decoder.optionalField "filename" Decoder.text
                <*> Decoder.optionalField "input_audio" Decoder.rawJson
                <*> Decoder.optionalField
                    "annotations"
                    (Decoder.array Decoder.rawJson)
                <*> Decoder.optionalField
                    "logprobs"
                    (Decoder.array Decoder.rawJson)
                <*> Decoder.optionalField "refusal" Decoder.text
                <*> Decoder.optionalField "encrypted_content" Decoder.text
                <*> Decoder.extensionFields
  where
    finish ParsedPart{..} =
        case parsedType of
            "input_text" ->
                InputTextPart
                    <$> require "text" parsedText
                    <*> Right parsedPromptCacheBreakpoint
                    <*> Right parsedExtensions
            "input_image" ->
                Right (InputImagePart
                    parsedDetail
                    parsedFileId
                    parsedImageUrl
                    parsedPromptCacheBreakpoint
                    parsedExtensions)
            "input_file" ->
                Right (InputFilePart
                    parsedDetail
                    parsedFileData
                    parsedFileId
                    parsedFileUrl
                    parsedFilename
                    parsedPromptCacheBreakpoint
                    parsedExtensions)
            "input_audio" ->
                InputAudioPart
                    <$> require "input_audio" parsedInputAudio
                    <*> Right parsedExtensions
            "output_text" ->
                OutputTextPart
                    <$> require "text" parsedText
                    <*> Right parsedAnnotations
                    <*> Right parsedLogprobs
                    <*> Right parsedExtensions
            "refusal" ->
                RefusalPart
                    <$> require "refusal" parsedRefusal
                    <*> Right parsedExtensions
            "reasoning_text" ->
                ReasoningTextPart
                    <$> require "text" parsedText
                    <*> Right parsedExtensions
            "summary_text" ->
                SummaryTextPart
                    <$> require "text" parsedText
                    <*> Right parsedExtensions
            "encrypted_content" ->
                EncryptedContentPart
                    <$> require "encrypted_content" parsedEncryptedContent
                    <*> Right parsedExtensions
            "text" ->
                PlainTextPart
                    <$> require "text" parsedText
                    <*> Right parsedExtensions
            tag ->
                Right (UnknownContentPart
                    (TaggedObject tag parsedExtensions))

    require name =
        maybe (Left ("missing required field " <> name)) Right

responseContentPartDecoder :: Decoder.Decoder ResponseContentPart
responseContentPartDecoder = partDecoder

responseContentPartEncoder :: Encoder.Encoder ResponseContentPart
responseContentPartEncoder = Encoder.choose \case
    InputTextPart {} -> Encoder.contramap pickInputText inputTextEncoder
    InputImagePart {} -> Encoder.contramap pickInputImage inputImageEncoder
    InputFilePart {} -> Encoder.contramap pickInputFile inputFileEncoder
    InputAudioPart {} -> Encoder.contramap pickInputAudio inputAudioEncoder
    OutputTextPart {} -> Encoder.contramap pickOutputText outputTextEncoder
    RefusalPart {} -> Encoder.contramap pickRefusal refusalEncoder
    ReasoningTextPart {} -> Encoder.contramap pickReasoning reasoningEncoder
    SummaryTextPart {} -> Encoder.contramap pickSummary summaryEncoder
    EncryptedContentPart {} -> Encoder.contramap pickEncrypted encryptedEncoder
    PlainTextPart {} -> Encoder.contramap pickPlain plainTextEncoder
    UnknownContentPart {} -> Encoder.contramap pickUnknown unknownEncoder
  where
    pickInputText value@InputTextPart {} = value; pickInputText _ = impossible
    pickInputImage value@InputImagePart {} = value; pickInputImage _ = impossible
    pickInputFile value@InputFilePart {} = value; pickInputFile _ = impossible
    pickInputAudio value@InputAudioPart {} = value; pickInputAudio _ = impossible
    pickOutputText value@OutputTextPart {} = value; pickOutputText _ = impossible
    pickRefusal value@RefusalPart {} = value; pickRefusal _ = impossible
    pickReasoning value@ReasoningTextPart {} = value; pickReasoning _ = impossible
    pickSummary value@SummaryTextPart {} = value; pickSummary _ = impossible
    pickEncrypted value@EncryptedContentPart {} = value; pickEncrypted _ = impossible
    pickPlain value@PlainTextPart {} = value; pickPlain _ = impossible
    pickUnknown value@UnknownContentPart {} = value; pickUnknown _ = impossible
    inputTextEncoder = Encoder.objectWithExtensions knownExtras
        [ Encoder.field "type" Encoder.text (const "input_text")
        , Encoder.field "text" Encoder.text text
        , Encoder.optionalField "prompt_cache_breakpoint" Encoder.rawJson prompt
        ]
    inputImageEncoder = Encoder.objectWithExtensions knownExtras
        [ Encoder.field "type" Encoder.text (const "input_image")
        , Encoder.optionalField "detail" Encoder.text detail
        , Encoder.optionalField "file_id" Encoder.text fileId
        , Encoder.optionalField "image_url" Encoder.text imageUrl
        , Encoder.optionalField "prompt_cache_breakpoint" Encoder.rawJson prompt
        ]
    inputFileEncoder = Encoder.objectWithExtensions knownExtras
        [ Encoder.field "type" Encoder.text (const "input_file")
        , Encoder.optionalField "detail" Encoder.text detail
        , Encoder.optionalField "file_data" Encoder.text fileData
        , Encoder.optionalField "file_id" Encoder.text fileId
        , Encoder.optionalField "file_url" Encoder.text fileUrl
        , Encoder.optionalField "filename" Encoder.text filename
        , Encoder.optionalField "prompt_cache_breakpoint" Encoder.rawJson prompt
        ]
    inputAudioEncoder = Encoder.objectWithExtensions knownExtras
        [ Encoder.field "type" Encoder.text (const "input_audio")
        , Encoder.field "input_audio" Encoder.rawJson inputAudio
        ]
    outputTextEncoder = Encoder.objectWithExtensions knownExtras
        [ Encoder.field "type" Encoder.text (const "output_text")
        , Encoder.field "text" Encoder.text text
        , Encoder.optionalField "annotations" (Encoder.list Encoder.rawJson) annotations
        , Encoder.optionalField "logprobs" (Encoder.list Encoder.rawJson) logprobs
        ]
    refusalEncoder = Encoder.objectWithExtensions knownExtras
        [ Encoder.field "type" Encoder.text (const "refusal")
        , Encoder.field "refusal" Encoder.text refusal
        ]
    reasoningEncoder = Encoder.objectWithExtensions knownExtras
        [ Encoder.field "type" Encoder.text (const "reasoning_text")
        , Encoder.field "text" Encoder.text text
        ]
    summaryEncoder = Encoder.objectWithExtensions knownExtras
        [ Encoder.field "type" Encoder.text (const "summary_text")
        , Encoder.field "text" Encoder.text text
        ]
    encryptedEncoder = Encoder.objectWithExtensions knownExtras
        [ Encoder.field "type" Encoder.text (const "encrypted_content")
        , Encoder.field "encrypted_content" Encoder.text encryptedContent
        ]
    plainTextEncoder = Encoder.objectWithExtensions knownExtras
        [ Encoder.field "type" Encoder.text (const "text")
        , Encoder.field "text" Encoder.text text
        ]
    unknownEncoder = Encoder.objectWithExtensions unknownFields
        [ Encoder.field "type" Encoder.text unknownTag
        ]
    text = \case InputTextPart { text = v } -> v; OutputTextPart { text = v } -> v; ReasoningTextPart { text = v } -> v; SummaryTextPart { text = v } -> v; PlainTextPart { text = v } -> v; _ -> impossible
    detail = \case InputImagePart { detail = v } -> v; InputFilePart { detail = v } -> v; _ -> impossible
    fileId = \case InputImagePart { fileId = v } -> v; InputFilePart { fileId = v } -> v; _ -> impossible
    imageUrl = \case InputImagePart { imageUrl = v } -> v; _ -> impossible
    fileData = \case InputFilePart { fileData = v } -> v; _ -> impossible
    fileUrl = \case InputFilePart { fileUrl = v } -> v; _ -> impossible
    filename = \case InputFilePart { filename = v } -> v; _ -> impossible
    prompt = \case
        InputTextPart { promptCacheBreakpoint = v } -> v
        InputImagePart { promptCacheBreakpoint = v } -> v
        InputFilePart { promptCacheBreakpoint = v } -> v
        _ -> Nothing
    inputAudio = \case InputAudioPart { inputAudio = v } -> v; _ -> impossible
    annotations = \case OutputTextPart { annotations = v } -> v; _ -> impossible
    logprobs = \case OutputTextPart { logprobs = v } -> v; _ -> impossible
    refusal = \case RefusalPart { refusal = v } -> v; _ -> impossible
    encryptedContent = \case EncryptedContentPart { encryptedContent = v } -> v; _ -> impossible
    knownExtras = \case
        InputTextPart { extraFields = value } -> value
        InputImagePart { extraFields = value } -> value
        InputFilePart { extraFields = value } -> value
        InputAudioPart { extraFields = value } -> value
        OutputTextPart { extraFields = value } -> value
        RefusalPart { extraFields = value } -> value
        ReasoningTextPart { extraFields = value } -> value
        SummaryTextPart { extraFields = value } -> value
        EncryptedContentPart { extraFields = value } -> value
        PlainTextPart { extraFields = value } -> value
        UnknownContentPart _ -> impossible
    unknownFields = \case
        UnknownContentPart TaggedObject { fields = value } -> value
        _ -> impossible
    unknownTag = \case
        UnknownContentPart TaggedObject { tag = value } -> value
        _ -> impossible
    impossible = error "responseContentPartEncoder: impossible variant"
