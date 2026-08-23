-- | Lossless wire types for the OpenAI Responses API.
--
-- These are the library's only request, response, item, tool, and streaming
-- event types. Records retain unknown object members in @extraFields@ and
-- unions retain unknown discriminators, so decoding and re-encoding does not
-- silently discard fields added by the service.
module Agent.Responses.Types
    ( -- * Create request
      ResponseCreateParams(..)
    , defaultResponseCreateParams
    , ResponseInput(..)
    , ResponseInclude(..)
    , ContextManagement(..)
    , Conversation(..)
    , Prompt(..)
    , PromptCacheOptions(..)
    , ReasoningConfig(..)
    , ResponseTextConfig(..)
    , ResponseFormat(..)
    , StreamOptions(..)
    , ToolChoice(..)
    , ToolChoiceMode(..)

      -- * Items and content
    , ResponseItem(..)
    , ResponseItemType(..)
    , responseItemTypeText
    , ResponseMessage(..)
    , ResponseRole(..)
    , MessageContent(..)
    , ResponseContentPart(..)
    , ItemStatus(..)
    , FunctionCall(..)
    , FunctionCallOutput(..)
    , CustomToolCall(..)
    , CustomToolCallOutput(..)
    , ReasoningItem(..)
    , ReasoningSummaryPart(..)
    , ItemReference(..)
    , TaggedObject(..)

      -- * Tools
    , ResponseTool(..)
    , ResponseToolType(..)
    , responseToolTypeText
    , FunctionTool(..)

      -- * Response
    , Response(..)
    , ResponseStatus(..)
    , ResponseError(..)
    , IncompleteDetails(..)
    , ResponseUsage(..)
    , TokenDetails(..)

      -- * Streaming
    , ResponseStreamEvent(..)
    , ResponseStreamError(..)
    , StreamEventType(..)
    , responseStreamEventType
    , responseStreamEventSequenceNumber
    , streamEventTypeText
    , parseStreamEventWithType
    ) where

import Control.Applicative ((<|>))
import Data.Aeson hiding (TaggedObject)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (Parser)
import Data.Maybe (catMaybes)
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as Text

--------------------------------------------------------------------------------
-- Object helpers
--------------------------------------------------------------------------------

type Field = (Key.Key, Aeson.Value)

field :: ToJSON a => Text -> a -> Field
field name value = (Key.fromText name, toJSON value)

optionalField :: ToJSON a => Text -> Maybe a -> Maybe Field
optionalField name = fmap (field name)

objectWith :: Aeson.Object -> [Maybe Field] -> Aeson.Value
objectWith extras members =
    Aeson.Object (foldl' insert extras (catMaybes members))
  where
    insert object (key, value) = KeyMap.insert key value object

without :: [Text] -> Aeson.Object -> Aeson.Object
without names object = foldl' (flip (KeyMap.delete . Key.fromText)) object names

--------------------------------------------------------------------------------
-- Request primitives
--------------------------------------------------------------------------------

newtype ResponseInclude = ResponseInclude { unResponseInclude :: Text }
    deriving stock (Eq, Show)

instance ToJSON ResponseInclude where
    toJSON ResponseInclude { unResponseInclude } = Aeson.String unResponseInclude

instance FromJSON ResponseInclude where
    parseJSON = withText "ResponseInclude" (pure . ResponseInclude)

data ContextManagement = ContextManagement
    { contextType     :: !Text
    , compactThreshold :: !(Maybe Int)
    , extraFields     :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON ContextManagement where
    toJSON ContextManagement { contextType, compactThreshold, extraFields } = objectWith extraFields
        [ Just (field "type" contextType)
        , optionalField "compact_threshold" compactThreshold
        ]

instance FromJSON ContextManagement where
    parseJSON = withObject "ContextManagement" $ \o -> ContextManagement
        <$> o .: "type"
        <*> o .:? "compact_threshold"
        <*> pure (without ["type", "compact_threshold"] o)

data Conversation
    = ConversationId !Text
    | ConversationObject
        { conversationId :: !Text
        , extraFields     :: !Aeson.Object
        }
    deriving stock (Eq, Show)

instance ToJSON Conversation where
    toJSON (ConversationId value) = Aeson.String value
    toJSON ConversationObject { conversationId, extraFields } = objectWith extraFields
        [Just (field "id" conversationId)]

instance FromJSON Conversation where
    parseJSON (Aeson.String value) = pure (ConversationId value)
    parseJSON value = withObject "Conversation" (\o -> ConversationObject
        <$> o .: "id"
        <*> pure (without ["id"] o)) value

data Prompt = Prompt
    { promptId       :: !Text
    , promptVariables :: !(Maybe Aeson.Object)
    , promptVersion  :: !(Maybe Text)
    , extraFields    :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON Prompt where
    toJSON Prompt { promptId, promptVariables, promptVersion, extraFields } = objectWith extraFields
        [ Just (field "id" promptId)
        , optionalField "variables" promptVariables
        , optionalField "version" promptVersion
        ]

instance FromJSON Prompt where
    parseJSON = withObject "Prompt" $ \o -> Prompt
        <$> o .: "id"
        <*> o .:? "variables"
        <*> o .:? "version"
        <*> pure (without ["id", "variables", "version"] o)

data PromptCacheOptions = PromptCacheOptions
    { mode        :: !(Maybe Text)
    , ttl         :: !(Maybe Text)
    , extraFields :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON PromptCacheOptions where
    toJSON PromptCacheOptions { mode, ttl, extraFields } = objectWith extraFields
        [ optionalField "mode" mode
        , optionalField "ttl" ttl
        ]

instance FromJSON PromptCacheOptions where
    parseJSON = withObject "PromptCacheOptions" $ \o -> PromptCacheOptions
        <$> o .:? "mode"
        <*> o .:? "ttl"
        <*> pure (without ["mode", "ttl"] o)

data ReasoningConfig = ReasoningConfig
    { context         :: !(Maybe Text)
    , effort          :: !(Maybe Text)
    , generateSummary :: !(Maybe Text)
    , reasoningMode   :: !(Maybe Text)
    , summary         :: !(Maybe Text)
    , extraFields     :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON ReasoningConfig where
    toJSON ReasoningConfig { context, effort, generateSummary, reasoningMode, summary, extraFields } = objectWith extraFields
        [ optionalField "context" context
        , optionalField "effort" effort
        , optionalField "generate_summary" generateSummary
        , optionalField "mode" reasoningMode
        , optionalField "summary" summary
        ]

instance FromJSON ReasoningConfig where
    parseJSON = withObject "ReasoningConfig" $ \o -> ReasoningConfig
        <$> o .:? "context"
        <*> o .:? "effort"
        <*> o .:? "generate_summary"
        <*> o .:? "mode"
        <*> o .:? "summary"
        <*> pure (without ["context", "effort", "generate_summary", "mode", "summary"] o)

data ResponseFormat
    = ResponseFormatText
        { extraFields :: !Aeson.Object }
    | ResponseFormatJsonObject
        { extraFields :: !Aeson.Object }
    | ResponseFormatJsonSchema
        { formatName        :: !Text
        , formatDescription :: !(Maybe Text)
        , formatSchema      :: !Aeson.Value
        , formatStrict      :: !(Maybe Bool)
        , extraFields       :: !Aeson.Object
        }
    | ResponseFormatUnknown !TaggedObject
    deriving stock (Eq, Show)

instance ToJSON ResponseFormat where
    toJSON ResponseFormatText { extraFields } = objectWith extraFields [Just (field "type" ("text" :: Text))]
    toJSON ResponseFormatJsonObject { extraFields } = objectWith extraFields [Just (field "type" ("json_object" :: Text))]
    toJSON ResponseFormatJsonSchema { formatName, formatDescription, formatSchema, formatStrict, extraFields } = objectWith extraFields
        [ Just (field "type" ("json_schema" :: Text))
        , Just (field "name" formatName)
        , optionalField "description" formatDescription
        , Just (field "schema" formatSchema)
        , optionalField "strict" formatStrict
        ]
    toJSON (ResponseFormatUnknown tagged) = toJSON tagged

instance FromJSON ResponseFormat where
    parseJSON value = withObject "ResponseFormat" (\o -> do
        tag <- o .: "type"
        case (tag :: Text) of
            "text" -> pure ResponseFormatText
                { extraFields = without ["type"] o }
            "json_object" -> pure ResponseFormatJsonObject
                { extraFields = without ["type"] o }
            "json_schema" -> ResponseFormatJsonSchema
                <$> o .: "name"
                <*> o .:? "description"
                <*> o .: "schema"
                <*> o .:? "strict"
                <*> pure (without ["type", "name", "description", "schema", "strict"] o)
            _ -> ResponseFormatUnknown <$> parseJSON value) value

data ResponseTextConfig = ResponseTextConfig
    { format      :: !(Maybe ResponseFormat)
    , verbosity   :: !(Maybe Text)
    , extraFields :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON ResponseTextConfig where
    toJSON ResponseTextConfig { format, verbosity, extraFields } = objectWith extraFields
        [ optionalField "format" format
        , optionalField "verbosity" verbosity
        ]

instance FromJSON ResponseTextConfig where
    parseJSON = withObject "ResponseTextConfig" $ \o -> ResponseTextConfig
        <$> (o .:? "format" <|> o .:? "format_")
        <*> o .:? "verbosity"
        <*> pure (without ["format", "format_", "verbosity"] o)

data StreamOptions = StreamOptions
    { includeObfuscation :: !(Maybe Bool)
    , extraFields        :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON StreamOptions where
    toJSON StreamOptions { includeObfuscation, extraFields } = objectWith extraFields
        [optionalField "include_obfuscation" includeObfuscation]

instance FromJSON StreamOptions where
    parseJSON = withObject "StreamOptions" $ \o -> StreamOptions
        <$> o .:? "include_obfuscation"
        <*> pure (without ["include_obfuscation"] o)

data ToolChoiceMode = ToolChoiceNone | ToolChoiceAuto | ToolChoiceRequired | ToolChoiceModeUnknown !Text
    deriving stock (Eq, Show)

toolChoiceModeText :: ToolChoiceMode -> Text
toolChoiceModeText = \case
    ToolChoiceNone -> "none"
    ToolChoiceAuto -> "auto"
    ToolChoiceRequired -> "required"
    ToolChoiceModeUnknown value -> value

parseToolChoiceMode :: Text -> ToolChoiceMode
parseToolChoiceMode = \case
    "none" -> ToolChoiceNone
    "auto" -> ToolChoiceAuto
    "required" -> ToolChoiceRequired
    value -> ToolChoiceModeUnknown value

data ToolChoice
    = ToolChoiceMode !ToolChoiceMode
    | ToolChoiceObject !TaggedObject
    deriving stock (Eq, Show)

instance ToJSON ToolChoice where
    toJSON (ToolChoiceMode choice) = Aeson.String (toolChoiceModeText choice)
    toJSON (ToolChoiceObject tagged) = toJSON tagged

instance FromJSON ToolChoice where
    parseJSON (Aeson.String value) = pure (ToolChoiceMode (parseToolChoiceMode value))
    parseJSON value = ToolChoiceObject <$> parseJSON value

--------------------------------------------------------------------------------
-- Content
--------------------------------------------------------------------------------

data ResponseRole = RoleUser | RoleAssistant | RoleSystem | RoleDeveloper | RoleUnknown !Text
    deriving stock (Eq, Show)

responseRoleText :: ResponseRole -> Text
responseRoleText = \case
    RoleUser -> "user"
    RoleAssistant -> "assistant"
    RoleSystem -> "system"
    RoleDeveloper -> "developer"
    RoleUnknown value -> value

instance ToJSON ResponseRole where
    toJSON = Aeson.String . responseRoleText

instance FromJSON ResponseRole where
    parseJSON = withText "ResponseRole" $ pure . \case
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

instance ToJSON ItemStatus where
    toJSON = Aeson.String . itemStatusText

instance FromJSON ItemStatus where
    parseJSON = withText "ItemStatus" $ pure . \case
        "in_progress" -> ItemInProgress
        "completed" -> ItemCompleted
        "incomplete" -> ItemIncomplete
        value -> ItemStatusUnknown value

data MessageContent
    = MessageContentText !Text
    | MessageContentParts ![ResponseContentPart]
    deriving stock (Eq, Show)

instance ToJSON MessageContent where
    toJSON (MessageContentText value) = Aeson.String value
    toJSON (MessageContentParts value) = toJSON value

instance FromJSON MessageContent where
    parseJSON (Aeson.String value) = pure (MessageContentText value)
    parseJSON value@(Aeson.Array _) = MessageContentParts <$> parseJSON value
    parseJSON value = fail ("MessageContent: expected string or array, got " <> show value)

data ResponseContentPart
    = InputTextPart
        { text                  :: !Text
        , promptCacheBreakpoint :: !(Maybe Aeson.Value)
        , extraFields           :: !Aeson.Object
        }
    | InputImagePart
        { detail                :: !(Maybe Text)
        , fileId                :: !(Maybe Text)
        , imageUrl              :: !(Maybe Text)
        , promptCacheBreakpoint :: !(Maybe Aeson.Value)
        , extraFields           :: !Aeson.Object
        }
    | InputFilePart
        { detail                :: !(Maybe Text)
        , fileData              :: !(Maybe Text)
        , fileId                :: !(Maybe Text)
        , fileUrl               :: !(Maybe Text)
        , filename              :: !(Maybe Text)
        , promptCacheBreakpoint :: !(Maybe Aeson.Value)
        , extraFields           :: !Aeson.Object
        }
    | InputAudioPart
        { inputAudio  :: !Aeson.Value
        , extraFields :: !Aeson.Object
        }
    | OutputTextPart
        { text        :: !Text
        , annotations :: !(Maybe [Aeson.Value])
        , logprobs    :: !(Maybe [Aeson.Value])
        , extraFields :: !Aeson.Object
        }
    | RefusalPart
        { refusal     :: !Text
        , extraFields :: !Aeson.Object
        }
    | ReasoningTextPart
        { text        :: !Text
        , extraFields :: !Aeson.Object
        }
    | SummaryTextPart
        { text        :: !Text
        , extraFields :: !Aeson.Object
        }
    | UnknownContentPart !TaggedObject
    deriving stock (Eq, Show)

instance ToJSON ResponseContentPart where
    toJSON InputTextPart { text, promptCacheBreakpoint, extraFields } = objectWith extraFields
        [ Just (field "type" ("input_text" :: Text))
        , Just (field "text" text)
        , optionalField "prompt_cache_breakpoint" promptCacheBreakpoint
        ]
    toJSON InputImagePart { detail, fileId, imageUrl, promptCacheBreakpoint, extraFields } = objectWith extraFields
        [ Just (field "type" ("input_image" :: Text))
        , optionalField "detail" detail
        , optionalField "file_id" fileId
        , optionalField "image_url" imageUrl
        , optionalField "prompt_cache_breakpoint" promptCacheBreakpoint
        ]
    toJSON InputFilePart { detail, fileData, fileId, fileUrl, filename, promptCacheBreakpoint, extraFields } = objectWith extraFields
        [ Just (field "type" ("input_file" :: Text))
        , optionalField "detail" detail
        , optionalField "file_data" fileData
        , optionalField "file_id" fileId
        , optionalField "file_url" fileUrl
        , optionalField "filename" filename
        , optionalField "prompt_cache_breakpoint" promptCacheBreakpoint
        ]
    toJSON InputAudioPart { inputAudio, extraFields } = objectWith extraFields
        [ Just (field "type" ("input_audio" :: Text))
        , Just (field "input_audio" inputAudio)
        ]
    toJSON OutputTextPart { text, annotations, logprobs, extraFields } = objectWith extraFields
        [ Just (field "type" ("output_text" :: Text))
        , Just (field "text" text)
        , optionalField "annotations" annotations
        , optionalField "logprobs" logprobs
        ]
    toJSON RefusalPart { refusal, extraFields } = objectWith extraFields
        [Just (field "type" ("refusal" :: Text)), Just (field "refusal" refusal)]
    toJSON ReasoningTextPart { text, extraFields } = objectWith extraFields
        [Just (field "type" ("reasoning_text" :: Text)), Just (field "text" text)]
    toJSON SummaryTextPart { text, extraFields } = objectWith extraFields
        [Just (field "type" ("summary_text" :: Text)), Just (field "text" text)]
    toJSON (UnknownContentPart tagged) = toJSON tagged

instance FromJSON ResponseContentPart where
    parseJSON value = withObject "ResponseContentPart" (\o -> do
        tag <- o .: "type"
        case (tag :: Text) of
            "input_text" -> InputTextPart
                <$> o .: "text"
                <*> o .:? "prompt_cache_breakpoint"
                <*> pure (without ["type", "text", "prompt_cache_breakpoint"] o)
            "input_image" -> InputImagePart
                <$> o .:? "detail"
                <*> o .:? "file_id"
                <*> o .:? "image_url"
                <*> o .:? "prompt_cache_breakpoint"
                <*> pure (without ["type", "detail", "file_id", "image_url", "prompt_cache_breakpoint"] o)
            "input_file" -> InputFilePart
                <$> o .:? "detail"
                <*> o .:? "file_data"
                <*> o .:? "file_id"
                <*> o .:? "file_url"
                <*> o .:? "filename"
                <*> o .:? "prompt_cache_breakpoint"
                <*> pure (without ["type", "detail", "file_data", "file_id", "file_url", "filename", "prompt_cache_breakpoint"] o)
            "input_audio" -> InputAudioPart
                <$> o .: "input_audio"
                <*> pure (without ["type", "input_audio"] o)
            "output_text" -> OutputTextPart
                <$> o .: "text"
                <*> o .:? "annotations"
                <*> o .:? "logprobs"
                <*> pure (without ["type", "text", "annotations", "logprobs"] o)
            "refusal" -> RefusalPart
                <$> o .: "refusal"
                <*> pure (without ["type", "refusal"] o)
            "reasoning_text" -> ReasoningTextPart
                <$> o .: "text"
                <*> pure (without ["type", "text"] o)
            "summary_text" -> SummaryTextPart
                <$> o .: "text"
                <*> pure (without ["type", "text"] o)
            _ -> UnknownContentPart <$> parseJSON value) value

--------------------------------------------------------------------------------
-- Items
--------------------------------------------------------------------------------

data ResponseItemType
    = ItemMessage
    | ItemAgentMessage
    | ItemFileSearchCall
    | ItemComputerCall
    | ItemComputerCallOutput
    | ItemWebSearchCall
    | ItemFunctionCall
    | ItemFunctionCallOutput
    | ItemToolSearchCall
    | ItemToolSearchOutput
    | ItemReasoning
    | ItemCompaction
    | ItemImageGenerationCall
    | ItemCodeInterpreterCall
    | ItemLocalShellCall
    | ItemLocalShellCallOutput
    | ItemShellCall
    | ItemShellCallOutput
    | ItemApplyPatchCall
    | ItemApplyPatchCallOutput
    | ItemMcpListTools
    | ItemMcpApprovalRequest
    | ItemMcpApprovalResponse
    | ItemMcpCall
    | ItemCustomToolCallOutput
    | ItemCustomToolCall
    | ItemCompactionTrigger
    | ItemReferenceType
    | ItemProgram
    | ItemProgramOutput
    | ItemUnknownType !Text
    deriving stock (Eq, Show)

responseItemTypeText :: ResponseItemType -> Text
responseItemTypeText = \case
    ItemMessage -> "message"
    ItemAgentMessage -> "agent_message"
    ItemFileSearchCall -> "file_search_call"
    ItemComputerCall -> "computer_call"
    ItemComputerCallOutput -> "computer_call_output"
    ItemWebSearchCall -> "web_search_call"
    ItemFunctionCall -> "function_call"
    ItemFunctionCallOutput -> "function_call_output"
    ItemToolSearchCall -> "tool_search_call"
    ItemToolSearchOutput -> "tool_search_output"
    ItemReasoning -> "reasoning"
    ItemCompaction -> "compaction"
    ItemImageGenerationCall -> "image_generation_call"
    ItemCodeInterpreterCall -> "code_interpreter_call"
    ItemLocalShellCall -> "local_shell_call"
    ItemLocalShellCallOutput -> "local_shell_call_output"
    ItemShellCall -> "shell_call"
    ItemShellCallOutput -> "shell_call_output"
    ItemApplyPatchCall -> "apply_patch_call"
    ItemApplyPatchCallOutput -> "apply_patch_call_output"
    ItemMcpListTools -> "mcp_list_tools"
    ItemMcpApprovalRequest -> "mcp_approval_request"
    ItemMcpApprovalResponse -> "mcp_approval_response"
    ItemMcpCall -> "mcp_call"
    ItemCustomToolCallOutput -> "custom_tool_call_output"
    ItemCustomToolCall -> "custom_tool_call"
    ItemCompactionTrigger -> "compaction_trigger"
    ItemReferenceType -> "item_reference"
    ItemProgram -> "program"
    ItemProgramOutput -> "program_output"
    ItemUnknownType value -> value

parseResponseItemType :: Text -> ResponseItemType
parseResponseItemType value = case value of
    "message" -> ItemMessage
    "agent_message" -> ItemAgentMessage
    "file_search_call" -> ItemFileSearchCall
    "computer_call" -> ItemComputerCall
    "computer_call_output" -> ItemComputerCallOutput
    "web_search_call" -> ItemWebSearchCall
    "function_call" -> ItemFunctionCall
    "function_call_output" -> ItemFunctionCallOutput
    "tool_search_call" -> ItemToolSearchCall
    "tool_search_output" -> ItemToolSearchOutput
    "reasoning" -> ItemReasoning
    "compaction" -> ItemCompaction
    "compaction_summary" -> ItemCompaction
    "image_generation_call" -> ItemImageGenerationCall
    "code_interpreter_call" -> ItemCodeInterpreterCall
    "local_shell_call" -> ItemLocalShellCall
    "local_shell_call_output" -> ItemLocalShellCallOutput
    "shell_call" -> ItemShellCall
    "shell_call_output" -> ItemShellCallOutput
    "apply_patch_call" -> ItemApplyPatchCall
    "apply_patch_call_output" -> ItemApplyPatchCallOutput
    "mcp_list_tools" -> ItemMcpListTools
    "mcp_approval_request" -> ItemMcpApprovalRequest
    "mcp_approval_response" -> ItemMcpApprovalResponse
    "mcp_call" -> ItemMcpCall
    "custom_tool_call_output" -> ItemCustomToolCallOutput
    "custom_tool_call" -> ItemCustomToolCall
    "compaction_trigger" -> ItemCompactionTrigger
    "item_reference" -> ItemReferenceType
    "program" -> ItemProgram
    "program_output" -> ItemProgramOutput
    other -> ItemUnknownType other

data TaggedObject = TaggedObject
    { tag    :: !Text
    , fields :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON TaggedObject where
    toJSON TaggedObject { tag, fields } = objectWith fields [Just (field "type" tag)]

instance FromJSON TaggedObject where
    parseJSON = withObject "TaggedObject" $ \o -> TaggedObject
        <$> o .: "type"
        <*> pure (without ["type"] o)

data ResponseMessage = ResponseMessage
    { messageId    :: !(Maybe Text)
    , content      :: !MessageContent
    , role         :: !ResponseRole
    , status       :: !(Maybe ItemStatus)
    , phase        :: !(Maybe Text)
    , extraFields  :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON ResponseMessage where
    toJSON ResponseMessage { messageId, content, role, status, phase, extraFields } = objectWith extraFields
        [ Just (field "type" ("message" :: Text))
        , optionalField "id" messageId
        , Just (field "content" content)
        , Just (field "role" role)
        , optionalField "status" status
        , optionalField "phase" phase
        ]

instance FromJSON ResponseMessage where
    parseJSON = withObject "ResponseMessage" $ \o -> ResponseMessage
        <$> o .:? "id"
        <*> o .: "content"
        <*> o .: "role"
        <*> o .:? "status"
        <*> o .:? "phase"
        <*> pure (without ["type", "id", "content", "role", "status", "phase"] o)

data FunctionCall = FunctionCall
    { itemId      :: !(Maybe Text)
    , callId      :: !Text
    , name        :: !Text
    , arguments   :: !Text
    , status      :: !(Maybe ItemStatus)
    , extraFields :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON FunctionCall where
    toJSON FunctionCall { itemId, callId, name, arguments, status, extraFields } = objectWith extraFields
        [ Just (field "type" ("function_call" :: Text))
        , optionalField "id" itemId
        , Just (field "call_id" callId)
        , Just (field "name" name)
        , Just (field "arguments" arguments)
        , optionalField "status" status
        ]

instance FromJSON FunctionCall where
    parseJSON = withObject "FunctionCall" $ \o -> FunctionCall
        <$> o .:? "id"
        <*> o .: "call_id"
        <*> o .: "name"
        <*> o .: "arguments"
        <*> o .:? "status"
        <*> pure (without ["type", "id", "call_id", "name", "arguments", "status"] o)

data FunctionCallOutput = FunctionCallOutput
    { itemId      :: !(Maybe Text)
    , callId      :: !Text
    , output      :: !Aeson.Value
    , status      :: !(Maybe ItemStatus)
    , extraFields :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON FunctionCallOutput where
    toJSON FunctionCallOutput { itemId, callId, output, status, extraFields } = objectWith extraFields
        [ Just (field "type" ("function_call_output" :: Text))
        , optionalField "id" itemId
        , Just (field "call_id" callId)
        , Just (field "output" output)
        , optionalField "status" status
        ]

instance FromJSON FunctionCallOutput where
    parseJSON = withObject "FunctionCallOutput" $ \o -> FunctionCallOutput
        <$> o .:? "id"
        <*> o .: "call_id"
        <*> o .: "output"
        <*> o .:? "status"
        <*> pure (without ["type", "id", "call_id", "output", "status"] o)

data CustomToolCall = CustomToolCall
    { itemId      :: !(Maybe Text)
    , callId      :: !Text
    , name        :: !Text
    , input       :: !Text
    , status      :: !(Maybe ItemStatus)
    , extraFields :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON CustomToolCall where
    toJSON CustomToolCall { itemId, callId, name, input, status, extraFields } = objectWith extraFields
        [ Just (field "type" ("custom_tool_call" :: Text))
        , optionalField "id" itemId
        , Just (field "call_id" callId)
        , Just (field "name" name)
        , Just (field "input" input)
        , optionalField "status" status
        ]

instance FromJSON CustomToolCall where
    parseJSON = withObject "CustomToolCall" $ \o -> CustomToolCall
        <$> o .:? "id"
        <*> o .: "call_id"
        <*> o .: "name"
        <*> o .: "input"
        <*> o .:? "status"
        <*> pure (without ["type", "id", "call_id", "name", "input", "status"] o)

data CustomToolCallOutput = CustomToolCallOutput
    { itemId      :: !(Maybe Text)
    , callId      :: !Text
    , name        :: !(Maybe Text)
    , output      :: !Aeson.Value
    , status      :: !(Maybe ItemStatus)
    , extraFields :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON CustomToolCallOutput where
    toJSON CustomToolCallOutput { itemId, callId, name, output, status, extraFields } = objectWith extraFields
        [ Just (field "type" ("custom_tool_call_output" :: Text))
        , optionalField "id" itemId
        , Just (field "call_id" callId)
        , optionalField "name" name
        , Just (field "output" output)
        , optionalField "status" status
        ]

instance FromJSON CustomToolCallOutput where
    parseJSON = withObject "CustomToolCallOutput" $ \o -> CustomToolCallOutput
        <$> o .:? "id"
        <*> o .: "call_id"
        <*> o .:? "name"
        <*> o .: "output"
        <*> o .:? "status"
        <*> pure (without ["type", "id", "call_id", "name", "output", "status"] o)

data ReasoningSummaryPart = ReasoningSummaryPart
    { partType    :: !Text
    , text        :: !(Maybe Text)
    , extraFields :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON ReasoningSummaryPart where
    toJSON ReasoningSummaryPart { partType, text, extraFields } = objectWith extraFields
        [Just (field "type" partType), optionalField "text" text]

instance FromJSON ReasoningSummaryPart where
    parseJSON = withObject "ReasoningSummaryPart" $ \o -> ReasoningSummaryPart
        <$> o .: "type"
        <*> o .:? "text"
        <*> pure (without ["type", "text"] o)

data ReasoningItem = ReasoningItem
    { itemId           :: !(Maybe Text)
    , summary          :: ![ReasoningSummaryPart]
    , content          :: !(Maybe [ResponseContentPart])
    , encryptedContent :: !(Maybe Text)
    , status           :: !(Maybe ItemStatus)
    , extraFields      :: !Aeson.Object
    } deriving stock (Eq)

-- Keep opaque encrypted reasoning payloads out of logs while retaining enough
-- structure to diagnose response-shape and lifecycle issues.
instance Show ReasoningItem where
    show item =
        "ReasoningItem { itemId = " <> show item.itemId
            <> ", summary = " <> show item.summary
            <> ", content = " <> show item.content
            <> ", encryptedContent = "
            <> maybe "Nothing" (const "Just <redacted>") item.encryptedContent
            <> ", status = " <> show item.status
            <> ", extraFields = " <> show item.extraFields
            <> " }"

instance ToJSON ReasoningItem where
    toJSON ReasoningItem { itemId, summary, content, encryptedContent, status, extraFields } = objectWith extraFields
        [ Just (field "type" ("reasoning" :: Text))
        , optionalField "id" itemId
        , Just (field "summary" summary)
        , optionalField "content" content
        , optionalField "encrypted_content" encryptedContent
        , optionalField "status" status
        ]

instance FromJSON ReasoningItem where
    parseJSON = withObject "ReasoningItem" $ \o -> ReasoningItem
        <$> o .:? "id"
        <*> o .:? "summary" .!= []
        <*> o .:? "content"
        <*> o .:? "encrypted_content"
        <*> o .:? "status"
        <*> pure (without ["type", "id", "summary", "content", "encrypted_content", "status"] o)

data ItemReference = ItemReference
    { itemId      :: !Text
    , extraFields :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON ItemReference where
    toJSON ItemReference { itemId, extraFields } = objectWith extraFields
        [Just (field "type" ("item_reference" :: Text)), Just (field "id" itemId)]

instance FromJSON ItemReference where
    parseJSON = withObject "ItemReference" $ \o -> ItemReference
        <$> o .: "id"
        <*> pure (without ["type", "id"] o)

data ResponseItem
    = MessageItem !ResponseMessage
    | FunctionCallItem !FunctionCall
    | FunctionCallOutputItem !FunctionCallOutput
    | CustomToolCallItem !CustomToolCall
    | CustomToolCallOutputItem !CustomToolCallOutput
    | ReasoningItemValue !ReasoningItem
    | ItemReferenceValue !ItemReference
    | KnownResponseItem !ResponseItemType !TaggedObject
    | UnknownResponseItem !TaggedObject
    deriving stock (Eq, Show)

instance ToJSON ResponseItem where
    toJSON = \case
        MessageItem value -> toJSON value
        FunctionCallItem value -> toJSON value
        FunctionCallOutputItem value -> toJSON value
        CustomToolCallItem value -> toJSON value
        CustomToolCallOutputItem value -> toJSON value
        ReasoningItemValue value -> toJSON value
        ItemReferenceValue value -> toJSON value
        KnownResponseItem _ value -> toJSON value
        UnknownResponseItem value -> toJSON value

instance FromJSON ResponseItem where
    parseJSON value = withObject "ResponseItem" (\o -> do
        tag <- o .: "type"
        case parseResponseItemType tag of
            ItemMessage -> MessageItem <$> parseJSON value
            ItemFunctionCall -> FunctionCallItem <$> parseJSON value
            ItemFunctionCallOutput -> FunctionCallOutputItem <$> parseJSON value
            ItemCustomToolCall -> CustomToolCallItem <$> parseJSON value
            ItemCustomToolCallOutput -> CustomToolCallOutputItem <$> parseJSON value
            ItemReasoning -> ReasoningItemValue <$> parseJSON value
            ItemReferenceType -> ItemReferenceValue <$> parseJSON value
            ItemUnknownType{} -> UnknownResponseItem <$> parseJSON value
            itemType -> KnownResponseItem itemType <$> parseJSON value) value

data ResponseInput
    = ResponseInputText !Text
    | ResponseInputItems ![ResponseItem]
    deriving stock (Eq, Show)

instance ToJSON ResponseInput where
    toJSON (ResponseInputText value) = Aeson.String value
    toJSON (ResponseInputItems values) = toJSON values

instance FromJSON ResponseInput where
    parseJSON (Aeson.String value) = pure (ResponseInputText value)
    parseJSON value@(Aeson.Array _) = ResponseInputItems <$> parseJSON value
    parseJSON value = fail ("ResponseInput: expected string or array, got " <> show value)

--------------------------------------------------------------------------------
-- Tools
--------------------------------------------------------------------------------

data ResponseToolType
    = ToolFunction
    | ToolFileSearch
    | ToolComputer
    | ToolComputerUsePreview
    | ToolWebSearch
    | ToolMcp
    | ToolCodeInterpreter
    | ToolProgrammaticToolCalling
    | ToolImageGeneration
    | ToolLocalShell
    | ToolShell
    | ToolCustom
    | ToolNamespace
    | ToolSearch
    | ToolWebSearchPreview
    | ToolApplyPatch
    | ToolUnknownType !Text
    deriving stock (Eq, Show)

responseToolTypeText :: ResponseToolType -> Text
responseToolTypeText = \case
    ToolFunction -> "function"
    ToolFileSearch -> "file_search"
    ToolComputer -> "computer"
    ToolComputerUsePreview -> "computer_use_preview"
    ToolWebSearch -> "web_search"
    ToolMcp -> "mcp"
    ToolCodeInterpreter -> "code_interpreter"
    ToolProgrammaticToolCalling -> "programmatic_tool_calling"
    ToolImageGeneration -> "image_generation"
    ToolLocalShell -> "local_shell"
    ToolShell -> "shell"
    ToolCustom -> "custom"
    ToolNamespace -> "namespace"
    ToolSearch -> "tool_search"
    ToolWebSearchPreview -> "web_search_preview"
    ToolApplyPatch -> "apply_patch"
    ToolUnknownType value -> value

parseResponseToolType :: Text -> ResponseToolType
parseResponseToolType value = case value of
    "function" -> ToolFunction
    "file_search" -> ToolFileSearch
    "computer" -> ToolComputer
    "computer_use_preview" -> ToolComputerUsePreview
    "computer_use" -> ToolComputer
    "web_search" -> ToolWebSearch
    "mcp" -> ToolMcp
    "code_interpreter" -> ToolCodeInterpreter
    "programmatic_tool_calling" -> ToolProgrammaticToolCalling
    "image_generation" -> ToolImageGeneration
    "local_shell" -> ToolLocalShell
    "shell" -> ToolShell
    "custom" -> ToolCustom
    "namespace" -> ToolNamespace
    "tool_search" -> ToolSearch
    "web_search_preview" -> ToolWebSearchPreview
    "apply_patch" -> ToolApplyPatch
    other -> ToolUnknownType other

data FunctionTool = FunctionTool
    { name         :: !Text
    , description  :: !(Maybe Text)
    , parameters   :: !(Maybe Aeson.Value)
    , strict       :: !(Maybe Bool)
    , extraFields  :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON FunctionTool where
    toJSON FunctionTool { name, description, parameters, strict, extraFields } = objectWith extraFields
        [ Just (field "type" ("function" :: Text))
        , Just (field "name" name)
        , optionalField "description" description
        , optionalField "parameters" parameters
        , optionalField "strict" strict
        ]

instance FromJSON FunctionTool where
    parseJSON = withObject "FunctionTool" $ \o -> FunctionTool
        <$> o .: "name"
        <*> o .:? "description"
        <*> o .:? "parameters"
        <*> o .:? "strict"
        <*> pure (without ["type", "name", "description", "parameters", "strict"] o)

data ResponseTool
    = FunctionToolValue !FunctionTool
    | KnownResponseTool !ResponseToolType !TaggedObject
    | UnknownResponseTool !TaggedObject
    deriving stock (Eq, Show)

instance ToJSON ResponseTool where
    toJSON (FunctionToolValue value) = toJSON value
    toJSON (KnownResponseTool _ value) = toJSON value
    toJSON (UnknownResponseTool value) = toJSON value

instance FromJSON ResponseTool where
    parseJSON value = withObject "ResponseTool" (\o -> do
        tag <- o .: "type"
        case parseResponseToolType tag of
            ToolFunction -> FunctionToolValue <$> parseJSON value
            ToolUnknownType{} -> UnknownResponseTool <$> parseJSON value
            toolType -> KnownResponseTool toolType <$> parseJSON value) value

--------------------------------------------------------------------------------
-- Create request
--------------------------------------------------------------------------------

data ResponseCreateParams = ResponseCreateParams
    { background           :: !(Maybe Bool)
    , contextManagement    :: !(Maybe [ContextManagement])
    , conversation         :: !(Maybe Conversation)
    , include              :: !(Maybe [ResponseInclude])
    , input                :: !(Maybe ResponseInput)
    , instructions         :: !(Maybe Text)
    , maxOutputTokens      :: !(Maybe Int)
    , maxToolCalls         :: !(Maybe Int)
    , metadata             :: !(Maybe Aeson.Object)
    , model                :: !(Maybe Text)
    , moderation           :: !(Maybe Aeson.Value)
    , parallelToolCalls    :: !(Maybe Bool)
    , previousResponseId   :: !(Maybe Text)
    , prompt               :: !(Maybe Prompt)
    , promptCacheKey       :: !(Maybe Text)
    , promptCacheOptions   :: !(Maybe PromptCacheOptions)
    , promptCacheRetention :: !(Maybe Text)
    , reasoning            :: !(Maybe ReasoningConfig)
    , safetyIdentifier     :: !(Maybe Text)
    , serviceTier          :: !(Maybe Text)
    , store                :: !(Maybe Bool)
    , stream               :: !(Maybe Bool)
    , streamOptions        :: !(Maybe StreamOptions)
    , temperature          :: !(Maybe Scientific)
    , text                 :: !(Maybe ResponseTextConfig)
    , toolChoice           :: !(Maybe ToolChoice)
    , tools                :: !(Maybe [ResponseTool])
    , topLogprobs          :: !(Maybe Int)
    , topP                 :: !(Maybe Scientific)
    , truncation           :: !(Maybe Text)
    , user                 :: !(Maybe Text)
    , extraFields          :: !Aeson.Object
    } deriving stock (Eq, Show)

defaultResponseCreateParams :: ResponseCreateParams
defaultResponseCreateParams = ResponseCreateParams
    { background = Nothing
    , contextManagement = Nothing
    , conversation = Nothing
    , include = Nothing
    , input = Nothing
    , instructions = Nothing
    , maxOutputTokens = Nothing
    , maxToolCalls = Nothing
    , metadata = Nothing
    , model = Nothing
    , moderation = Nothing
    , parallelToolCalls = Nothing
    , previousResponseId = Nothing
    , prompt = Nothing
    , promptCacheKey = Nothing
    , promptCacheOptions = Nothing
    , promptCacheRetention = Nothing
    , reasoning = Nothing
    , safetyIdentifier = Nothing
    , serviceTier = Nothing
    , store = Nothing
    , stream = Nothing
    , streamOptions = Nothing
    , temperature = Nothing
    , text = Nothing
    , toolChoice = Nothing
    , tools = Nothing
    , topLogprobs = Nothing
    , topP = Nothing
    , truncation = Nothing
    , user = Nothing
    , extraFields = KeyMap.empty
    }

responseCreateFieldNames :: [Text]
responseCreateFieldNames =
    [ "background", "context_management", "conversation", "include", "input"
    , "instructions", "max_output_tokens", "max_tool_calls", "metadata", "model"
    , "moderation", "parallel_tool_calls", "previous_response_id", "prompt"
    , "prompt_cache_key", "prompt_cache_options", "prompt_cache_retention", "reasoning"
    , "safety_identifier", "service_tier", "store", "stream", "stream_options"
    , "temperature", "text", "tool_choice", "tools", "top_logprobs", "top_p"
    , "truncation", "user"
    ]

instance ToJSON ResponseCreateParams where
    toJSON ResponseCreateParams { background, contextManagement, conversation, include, input, instructions
                                , maxOutputTokens, maxToolCalls, metadata, model, moderation, parallelToolCalls
                                , previousResponseId, prompt, promptCacheKey, promptCacheOptions
                                , promptCacheRetention, reasoning, safetyIdentifier, serviceTier, store, stream
                                , streamOptions, temperature, text, toolChoice, tools, topLogprobs, topP
                                , truncation, user, extraFields } = objectWith extraFields
        [ optionalField "background" background
        , optionalField "context_management" contextManagement
        , optionalField "conversation" conversation
        , optionalField "include" include
        , optionalField "input" input
        , optionalField "instructions" instructions
        , optionalField "max_output_tokens" maxOutputTokens
        , optionalField "max_tool_calls" maxToolCalls
        , optionalField "metadata" metadata
        , optionalField "model" model
        , optionalField "moderation" moderation
        , optionalField "parallel_tool_calls" parallelToolCalls
        , optionalField "previous_response_id" previousResponseId
        , optionalField "prompt" prompt
        , optionalField "prompt_cache_key" promptCacheKey
        , optionalField "prompt_cache_options" promptCacheOptions
        , optionalField "prompt_cache_retention" promptCacheRetention
        , optionalField "reasoning" reasoning
        , optionalField "safety_identifier" safetyIdentifier
        , optionalField "service_tier" serviceTier
        , optionalField "store" store
        , optionalField "stream" stream
        , optionalField "stream_options" streamOptions
        , optionalField "temperature" temperature
        , optionalField "text" text
        , optionalField "tool_choice" toolChoice
        , optionalField "tools" tools
        , optionalField "top_logprobs" topLogprobs
        , optionalField "top_p" topP
        , optionalField "truncation" truncation
        , optionalField "user" user
        ]

instance FromJSON ResponseCreateParams where
    parseJSON = withObject "ResponseCreateParams" $ \o -> ResponseCreateParams
        <$> o .:? "background"
        <*> o .:? "context_management"
        <*> o .:? "conversation"
        <*> o .:? "include"
        <*> o .:? "input"
        <*> o .:? "instructions"
        <*> o .:? "max_output_tokens"
        <*> o .:? "max_tool_calls"
        <*> o .:? "metadata"
        <*> o .:? "model"
        <*> o .:? "moderation"
        <*> o .:? "parallel_tool_calls"
        <*> o .:? "previous_response_id"
        <*> o .:? "prompt"
        <*> o .:? "prompt_cache_key"
        <*> o .:? "prompt_cache_options"
        <*> o .:? "prompt_cache_retention"
        <*> o .:? "reasoning"
        <*> o .:? "safety_identifier"
        <*> o .:? "service_tier"
        <*> o .:? "store"
        <*> o .:? "stream"
        <*> o .:? "stream_options"
        <*> o .:? "temperature"
        <*> o .:? "text"
        <*> o .:? "tool_choice"
        <*> o .:? "tools"
        <*> o .:? "top_logprobs"
        <*> o .:? "top_p"
        <*> o .:? "truncation"
        <*> o .:? "user"
        <*> pure (without responseCreateFieldNames o)

--------------------------------------------------------------------------------
-- Response
--------------------------------------------------------------------------------

data ResponseStatus
    = ResponseCompleted
    | ResponseFailed
    | ResponseInProgress
    | ResponseCancelled
    | ResponseQueued
    | ResponseIncomplete
    | ResponseStatusUnknown !Text
    deriving stock (Eq, Show)

responseStatusText :: ResponseStatus -> Text
responseStatusText = \case
    ResponseCompleted -> "completed"
    ResponseFailed -> "failed"
    ResponseInProgress -> "in_progress"
    ResponseCancelled -> "cancelled"
    ResponseQueued -> "queued"
    ResponseIncomplete -> "incomplete"
    ResponseStatusUnknown value -> value

instance ToJSON ResponseStatus where
    toJSON = Aeson.String . responseStatusText

instance FromJSON ResponseStatus where
    parseJSON = withText "ResponseStatus" $ pure . \case
        "completed" -> ResponseCompleted
        "failed" -> ResponseFailed
        "in_progress" -> ResponseInProgress
        "cancelled" -> ResponseCancelled
        "queued" -> ResponseQueued
        "incomplete" -> ResponseIncomplete
        value -> ResponseStatusUnknown value

data ResponseError = ResponseError
    { code        :: !Text
    , message     :: !Text
    , extraFields :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON ResponseError where
    toJSON ResponseError { code, message, extraFields } = objectWith extraFields
        [Just (field "code" code), Just (field "message" message)]

instance FromJSON ResponseError where
    parseJSON = withObject "ResponseError" $ \o -> ResponseError
        <$> o .: "code"
        <*> o .: "message"
        <*> pure (without ["code", "message"] o)

data IncompleteDetails = IncompleteDetails
    { reason      :: !Text
    , extraFields :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON IncompleteDetails where
    toJSON IncompleteDetails { reason, extraFields } = objectWith extraFields [Just (field "reason" reason)]

instance FromJSON IncompleteDetails where
    parseJSON = withObject "IncompleteDetails" $ \o -> IncompleteDetails
        <$> o .: "reason"
        <*> pure (without ["reason"] o)

data TokenDetails = TokenDetails
    { cachedTokens    :: !(Maybe Int)
    , reasoningTokens :: !(Maybe Int)
    , extraFields     :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON TokenDetails where
    toJSON TokenDetails { cachedTokens, reasoningTokens, extraFields } = objectWith extraFields
        [optionalField "cached_tokens" cachedTokens, optionalField "reasoning_tokens" reasoningTokens]

instance FromJSON TokenDetails where
    parseJSON = withObject "TokenDetails" $ \o -> TokenDetails
        <$> o .:? "cached_tokens"
        <*> o .:? "reasoning_tokens"
        <*> pure (without ["cached_tokens", "reasoning_tokens"] o)

data ResponseUsage = ResponseUsage
    { inputTokens         :: !Int
    , inputTokensDetails  :: !(Maybe TokenDetails)
    , outputTokens        :: !Int
    , outputTokensDetails :: !(Maybe TokenDetails)
    , totalTokens         :: !Int
    , extraFields         :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON ResponseUsage where
    toJSON ResponseUsage { inputTokens, inputTokensDetails, outputTokens, outputTokensDetails, totalTokens, extraFields } = objectWith extraFields
        [ Just (field "input_tokens" inputTokens)
        , optionalField "input_tokens_details" inputTokensDetails
        , Just (field "output_tokens" outputTokens)
        , optionalField "output_tokens_details" outputTokensDetails
        , Just (field "total_tokens" totalTokens)
        ]

instance FromJSON ResponseUsage where
    parseJSON = withObject "ResponseUsage" $ \o -> ResponseUsage
        <$> o .: "input_tokens"
        <*> o .:? "input_tokens_details"
        <*> o .: "output_tokens"
        <*> o .:? "output_tokens_details"
        <*> o .: "total_tokens"
        <*> pure (without ["input_tokens", "input_tokens_details", "output_tokens", "output_tokens_details", "total_tokens"] o)

data Response = Response
    { responseId           :: !Text
    , createdAt            :: !Scientific
    , error                :: !(Maybe ResponseError)
    , incompleteDetails    :: !(Maybe IncompleteDetails)
    , instructions         :: !(Maybe ResponseInput)
    , metadata             :: !(Maybe Aeson.Object)
    , model                :: !Text
    , object               :: !Text
    , output               :: ![ResponseItem]
    , parallelToolCalls    :: !(Maybe Bool)
    , temperature          :: !(Maybe Scientific)
    , toolChoice           :: !(Maybe ToolChoice)
    , tools                :: !(Maybe [ResponseTool])
    , topP                 :: !(Maybe Scientific)
    , background           :: !(Maybe Bool)
    , completedAt          :: !(Maybe Scientific)
    , conversation         :: !(Maybe Conversation)
    , maxOutputTokens      :: !(Maybe Int)
    , maxToolCalls         :: !(Maybe Int)
    , moderation           :: !(Maybe Aeson.Value)
    , previousResponseId   :: !(Maybe Text)
    , prompt               :: !(Maybe Prompt)
    , promptCacheKey       :: !(Maybe Text)
    , promptCacheOptions   :: !(Maybe PromptCacheOptions)
    , promptCacheRetention :: !(Maybe Text)
    , reasoning            :: !(Maybe ReasoningConfig)
    , safetyIdentifier     :: !(Maybe Text)
    , serviceTier          :: !(Maybe Text)
    , status               :: !ResponseStatus
    , text                 :: !(Maybe ResponseTextConfig)
    , topLogprobs          :: !(Maybe Int)
    , truncation           :: !(Maybe Text)
    , usage                :: !(Maybe ResponseUsage)
    , user                 :: !(Maybe Text)
    , extraFields          :: !Aeson.Object
    } deriving stock (Eq, Show)

responseFieldNames :: [Text]
responseFieldNames =
    [ "id", "created_at", "error", "incomplete_details", "instructions", "metadata", "model"
    , "object", "output", "parallel_tool_calls", "temperature", "tool_choice", "tools", "top_p"
    , "background", "completed_at", "conversation", "max_output_tokens", "max_tool_calls"
    , "moderation", "previous_response_id", "prompt", "prompt_cache_key", "prompt_cache_options"
    , "prompt_cache_retention", "reasoning", "safety_identifier", "service_tier", "status", "text"
    , "top_logprobs", "truncation", "usage", "user"
    ]

instance ToJSON Response where
    toJSON Response { responseId, createdAt, error, incompleteDetails, instructions, metadata, model, object
                    , output, parallelToolCalls, temperature, toolChoice, tools, topP, background, completedAt
                    , conversation, maxOutputTokens, maxToolCalls, moderation, previousResponseId, prompt
                    , promptCacheKey, promptCacheOptions, promptCacheRetention, reasoning, safetyIdentifier
                    , serviceTier, status, text, topLogprobs, truncation, usage, user, extraFields } = objectWith extraFields
        [ Just (field "id" responseId)
        , Just (field "created_at" createdAt)
        , Just (field "error" error)
        , Just (field "incomplete_details" incompleteDetails)
        , optionalField "instructions" instructions
        , Just (field "metadata" metadata)
        , Just (field "model" model)
        , Just (field "object" object)
        , Just (field "output" output)
        , optionalField "parallel_tool_calls" parallelToolCalls
        , optionalField "temperature" temperature
        , optionalField "tool_choice" toolChoice
        , optionalField "tools" tools
        , optionalField "top_p" topP
        , optionalField "background" background
        , optionalField "completed_at" completedAt
        , optionalField "conversation" conversation
        , optionalField "max_output_tokens" maxOutputTokens
        , optionalField "max_tool_calls" maxToolCalls
        , optionalField "moderation" moderation
        , optionalField "previous_response_id" previousResponseId
        , optionalField "prompt" prompt
        , optionalField "prompt_cache_key" promptCacheKey
        , optionalField "prompt_cache_options" promptCacheOptions
        , optionalField "prompt_cache_retention" promptCacheRetention
        , optionalField "reasoning" reasoning
        , optionalField "safety_identifier" safetyIdentifier
        , optionalField "service_tier" serviceTier
        , Just (field "status" status)
        , optionalField "text" text
        , optionalField "top_logprobs" topLogprobs
        , optionalField "truncation" truncation
        , Just (field "usage" usage)
        , optionalField "user" user
        ]

instance FromJSON Response where
    parseJSON = withObject "Response" $ \o -> Response
        <$> o .: "id"
        <*> o .: "created_at"
        <*> o .:? "error"
        <*> o .:? "incomplete_details"
        <*> o .:? "instructions"
        <*> o .:? "metadata"
        <*> o .: "model"
        <*> o .:? "object" .!= "response"
        <*> o .:? "output" .!= []
        <*> o .:? "parallel_tool_calls"
        <*> o .:? "temperature"
        <*> o .:? "tool_choice"
        <*> o .:? "tools"
        <*> o .:? "top_p"
        <*> o .:? "background"
        <*> o .:? "completed_at"
        <*> o .:? "conversation"
        <*> o .:? "max_output_tokens"
        <*> o .:? "max_tool_calls"
        <*> o .:? "moderation"
        <*> o .:? "previous_response_id"
        <*> o .:? "prompt"
        <*> o .:? "prompt_cache_key"
        <*> o .:? "prompt_cache_options"
        <*> o .:? "prompt_cache_retention"
        <*> o .:? "reasoning"
        <*> o .:? "safety_identifier"
        <*> o .:? "service_tier"
        <*> o .: "status"
        <*> o .:? "text"
        <*> o .:? "top_logprobs"
        <*> o .:? "truncation"
        <*> o .:? "usage"
        <*> o .:? "user"
        <*> pure (without responseFieldNames o)

--------------------------------------------------------------------------------
-- Streaming events
--------------------------------------------------------------------------------

data StreamEventType
    = EventResponseCreated
    | EventResponseInProgress
    | EventResponseCompleted
    | EventResponseFailed
    | EventResponseIncomplete
    | EventOutputItemAdded
    | EventOutputItemDone
    | EventContentPartAdded
    | EventContentPartDone
    | EventOutputTextDelta
    | EventOutputTextDone
    | EventRefusalDelta
    | EventRefusalDone
    | EventFunctionCallArgumentsDelta
    | EventFunctionCallArgumentsDone
    | EventFileSearchInProgress
    | EventFileSearchSearching
    | EventFileSearchCompleted
    | EventWebSearchInProgress
    | EventWebSearchSearching
    | EventWebSearchCompleted
    | EventReasoningSummaryPartAdded
    | EventReasoningSummaryPartDone
    | EventReasoningSummaryTextDelta
    | EventReasoningSummaryTextDone
    | EventReasoningTextDelta
    | EventReasoningTextDone
    | EventImageGenerationCompleted
    | EventImageGenerationGenerating
    | EventImageGenerationInProgress
    | EventImageGenerationPartialImage
    | EventMcpCallArgumentsDelta
    | EventMcpCallArgumentsDone
    | EventMcpCallCompleted
    | EventMcpCallFailed
    | EventMcpCallInProgress
    | EventMcpListToolsCompleted
    | EventMcpListToolsFailed
    | EventMcpListToolsInProgress
    | EventCodeInterpreterInProgress
    | EventCodeInterpreterInterpreting
    | EventCodeInterpreterCompleted
    | EventCodeInterpreterCodeDelta
    | EventCodeInterpreterCodeDone
    | EventOutputTextAnnotationAdded
    | EventResponseQueued
    | EventCustomToolInputDelta
    | EventCustomToolInputDone
    | EventError
    | EventAudioDelta
    | EventAudioDone
    | EventAudioTranscriptDelta
    | EventAudioTranscriptDone
    | EventShellCommandAdded
    | EventShellCommandDelta
    | EventShellCommandDone
    | EventShellOutputDelta
    | EventShellOutputDone
    | StreamEventUnknown !Text
    deriving stock (Eq, Show)

streamEventTypeText :: StreamEventType -> Text
streamEventTypeText = \case
    EventResponseCreated -> "response.created"
    EventResponseInProgress -> "response.in_progress"
    EventResponseCompleted -> "response.completed"
    EventResponseFailed -> "response.failed"
    EventResponseIncomplete -> "response.incomplete"
    EventOutputItemAdded -> "response.output_item.added"
    EventOutputItemDone -> "response.output_item.done"
    EventContentPartAdded -> "response.content_part.added"
    EventContentPartDone -> "response.content_part.done"
    EventOutputTextDelta -> "response.output_text.delta"
    EventOutputTextDone -> "response.output_text.done"
    EventRefusalDelta -> "response.refusal.delta"
    EventRefusalDone -> "response.refusal.done"
    EventFunctionCallArgumentsDelta -> "response.function_call_arguments.delta"
    EventFunctionCallArgumentsDone -> "response.function_call_arguments.done"
    EventFileSearchInProgress -> "response.file_search_call.in_progress"
    EventFileSearchSearching -> "response.file_search_call.searching"
    EventFileSearchCompleted -> "response.file_search_call.completed"
    EventWebSearchInProgress -> "response.web_search_call.in_progress"
    EventWebSearchSearching -> "response.web_search_call.searching"
    EventWebSearchCompleted -> "response.web_search_call.completed"
    EventReasoningSummaryPartAdded -> "response.reasoning_summary_part.added"
    EventReasoningSummaryPartDone -> "response.reasoning_summary_part.done"
    EventReasoningSummaryTextDelta -> "response.reasoning_summary_text.delta"
    EventReasoningSummaryTextDone -> "response.reasoning_summary_text.done"
    EventReasoningTextDelta -> "response.reasoning_text.delta"
    EventReasoningTextDone -> "response.reasoning_text.done"
    EventImageGenerationCompleted -> "response.image_generation_call.completed"
    EventImageGenerationGenerating -> "response.image_generation_call.generating"
    EventImageGenerationInProgress -> "response.image_generation_call.in_progress"
    EventImageGenerationPartialImage -> "response.image_generation_call.partial_image"
    EventMcpCallArgumentsDelta -> "response.mcp_call_arguments.delta"
    EventMcpCallArgumentsDone -> "response.mcp_call_arguments.done"
    EventMcpCallCompleted -> "response.mcp_call.completed"
    EventMcpCallFailed -> "response.mcp_call.failed"
    EventMcpCallInProgress -> "response.mcp_call.in_progress"
    EventMcpListToolsCompleted -> "response.mcp_list_tools.completed"
    EventMcpListToolsFailed -> "response.mcp_list_tools.failed"
    EventMcpListToolsInProgress -> "response.mcp_list_tools.in_progress"
    EventCodeInterpreterInProgress -> "response.code_interpreter_call.in_progress"
    EventCodeInterpreterInterpreting -> "response.code_interpreter_call.interpreting"
    EventCodeInterpreterCompleted -> "response.code_interpreter_call.completed"
    EventCodeInterpreterCodeDelta -> "response.code_interpreter_call_code.delta"
    EventCodeInterpreterCodeDone -> "response.code_interpreter_call_code.done"
    EventOutputTextAnnotationAdded -> "response.output_text.annotation.added"
    EventResponseQueued -> "response.queued"
    EventCustomToolInputDelta -> "response.custom_tool_call_input.delta"
    EventCustomToolInputDone -> "response.custom_tool_call_input.done"
    EventError -> "error"
    EventAudioDelta -> "response.audio.delta"
    EventAudioDone -> "response.audio.done"
    EventAudioTranscriptDelta -> "response.audio.transcript.delta"
    EventAudioTranscriptDone -> "response.audio.transcript.done"
    EventShellCommandAdded -> "response.shell_call_command.added"
    EventShellCommandDelta -> "response.shell_call_command.delta"
    EventShellCommandDone -> "response.shell_call_command.done"
    EventShellOutputDelta -> "response.shell_call_output_content.delta"
    EventShellOutputDone -> "response.shell_call_output_content.done"
    StreamEventUnknown value -> value

parseStreamEventType :: Text -> StreamEventType
parseStreamEventType value = case value of
    "response.created" -> EventResponseCreated
    "response.in_progress" -> EventResponseInProgress
    "response.completed" -> EventResponseCompleted
    "response.failed" -> EventResponseFailed
    "response.incomplete" -> EventResponseIncomplete
    "response.output_item.added" -> EventOutputItemAdded
    "response.output_item.done" -> EventOutputItemDone
    "response.content_part.added" -> EventContentPartAdded
    "response.content_part.done" -> EventContentPartDone
    "response.output_text.delta" -> EventOutputTextDelta
    "response.output_text.done" -> EventOutputTextDone
    "response.refusal.delta" -> EventRefusalDelta
    "response.refusal.done" -> EventRefusalDone
    "response.function_call_arguments.delta" -> EventFunctionCallArgumentsDelta
    "response.function_call_arguments.done" -> EventFunctionCallArgumentsDone
    "response.file_search_call.in_progress" -> EventFileSearchInProgress
    "response.file_search_call.searching" -> EventFileSearchSearching
    "response.file_search_call.completed" -> EventFileSearchCompleted
    "response.web_search_call.in_progress" -> EventWebSearchInProgress
    "response.web_search_call.searching" -> EventWebSearchSearching
    "response.web_search_call.completed" -> EventWebSearchCompleted
    "response.reasoning_summary_part.added" -> EventReasoningSummaryPartAdded
    "response.reasoning_summary_part.done" -> EventReasoningSummaryPartDone
    "response.reasoning_summary_text.delta" -> EventReasoningSummaryTextDelta
    "response.reasoning_summary_text.done" -> EventReasoningSummaryTextDone
    "response.reasoning_text.delta" -> EventReasoningTextDelta
    "response.reasoning_text.done" -> EventReasoningTextDone
    "response.image_generation_call.completed" -> EventImageGenerationCompleted
    "response.image_generation_call.generating" -> EventImageGenerationGenerating
    "response.image_generation_call.in_progress" -> EventImageGenerationInProgress
    "response.image_generation_call.partial_image" -> EventImageGenerationPartialImage
    "response.mcp_call_arguments.delta" -> EventMcpCallArgumentsDelta
    "response.mcp_call_arguments.done" -> EventMcpCallArgumentsDone
    "response.mcp_call.completed" -> EventMcpCallCompleted
    "response.mcp_call.failed" -> EventMcpCallFailed
    "response.mcp_call.in_progress" -> EventMcpCallInProgress
    "response.mcp_list_tools.completed" -> EventMcpListToolsCompleted
    "response.mcp_list_tools.failed" -> EventMcpListToolsFailed
    "response.mcp_list_tools.in_progress" -> EventMcpListToolsInProgress
    "response.code_interpreter_call.in_progress" -> EventCodeInterpreterInProgress
    "response.code_interpreter_call.interpreting" -> EventCodeInterpreterInterpreting
    "response.code_interpreter_call.completed" -> EventCodeInterpreterCompleted
    "response.code_interpreter_call_code.delta" -> EventCodeInterpreterCodeDelta
    "response.code_interpreter_call_code.done" -> EventCodeInterpreterCodeDone
    "response.output_text.annotation.added" -> EventOutputTextAnnotationAdded
    "response.queued" -> EventResponseQueued
    "response.custom_tool_call_input.delta" -> EventCustomToolInputDelta
    "response.custom_tool_call_input.done" -> EventCustomToolInputDone
    "error" -> EventError
    "response.audio.delta" -> EventAudioDelta
    "response.audio.done" -> EventAudioDone
    "response.audio.transcript.delta" -> EventAudioTranscriptDelta
    "response.audio.transcript.done" -> EventAudioTranscriptDone
    "response.shell_call_command.added" -> EventShellCommandAdded
    "response.shell_call_command.delta" -> EventShellCommandDelta
    "response.shell_call_command.done" -> EventShellCommandDone
    "response.shell_call_output_content.delta" -> EventShellOutputDelta
    "response.shell_call_output_content.done" -> EventShellOutputDone
    other -> StreamEventUnknown other

-- | Structured payload used by both the documented top-level @error@ event
-- and the nested error envelope emitted by the Codex WebSocket endpoint.
data ResponseStreamError = ResponseStreamError
    { errorType   :: !(Maybe Text)
    , code        :: !(Maybe Text)
    , message     :: !Text
    , param       :: !(Maybe Text)
    , retryAfter  :: !(Maybe Int)
    , extraFields :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON ResponseStreamError where
    toJSON ResponseStreamError { errorType, code, message, param, retryAfter, extraFields } =
        objectWith extraFields
            [ optionalField "type" errorType
            , optionalField "code" code
            , Just (field "message" message)
            , optionalField "param" param
            , optionalField "resets_in_seconds" retryAfter
            ]

instance FromJSON ResponseStreamError where
    parseJSON = withObject "ResponseStreamError" $ \o -> ResponseStreamError
        <$> o .:? "type"
        <*> o .:? "code"
        <*> o .: "message"
        <*> o .:? "param"
        <*> o .:? "resets_in_seconds"
        -- Keep optional known fields in the extras map as well, preserving the
        -- wire distinction between an absent member and an explicit null.
        <*> pure (without ["message"] o)

-- | Lossless tagged union for Responses streaming events. Variants that drive
-- transport control flow have typed payloads. Other documented and future
-- variants retain their exact object members in 'eventExtraFields'.
data ResponseStreamEvent
    = ResponseCreatedEvent
        { response         :: !Response
        , sequenceNumber   :: !(Maybe Int)
        , eventExtraFields :: !Aeson.Object
        }
    | ResponseInProgressEvent
        { response         :: !Response
        , sequenceNumber   :: !(Maybe Int)
        , eventExtraFields :: !Aeson.Object
        }
    | ResponseCompletedEvent
        { response         :: !Response
        , sequenceNumber   :: !(Maybe Int)
        , eventExtraFields :: !Aeson.Object
        }
    | ResponseFailedEvent
        { response         :: !Response
        , sequenceNumber   :: !(Maybe Int)
        , eventExtraFields :: !Aeson.Object
        }
    | ResponseIncompleteEvent
        { response         :: !Response
        , sequenceNumber   :: !(Maybe Int)
        , eventExtraFields :: !Aeson.Object
        }
    | ResponseQueuedEvent
        { response         :: !Response
        , sequenceNumber   :: !(Maybe Int)
        , eventExtraFields :: !Aeson.Object
        }
    | ResponseOutputItemAddedEvent
        { item             :: !ResponseItem
        , outputIndex      :: !Int
        , sequenceNumber   :: !(Maybe Int)
        , eventExtraFields :: !Aeson.Object
        }
    | ResponseOutputItemDoneEvent
        { item             :: !ResponseItem
        , outputIndex      :: !Int
        , sequenceNumber   :: !(Maybe Int)
        , eventExtraFields :: !Aeson.Object
        }
    | ResponseErrorEvent
        { streamError      :: !ResponseStreamError
        , sequenceNumber   :: !(Maybe Int)
        , eventExtraFields :: !Aeson.Object
        }
    | ResponseNestedErrorEvent
        { streamError      :: !ResponseStreamError
        , sequenceNumber   :: !(Maybe Int)
        , eventExtraFields :: !Aeson.Object
        }
    | OtherResponseStreamEvent
        { otherEventType   :: !StreamEventType
        , sequenceNumber   :: !(Maybe Int)
        , eventExtraFields :: !Aeson.Object
        }
    deriving stock (Eq, Show)

responseStreamEventType :: ResponseStreamEvent -> StreamEventType
responseStreamEventType = \case
    ResponseCreatedEvent{} -> EventResponseCreated
    ResponseInProgressEvent{} -> EventResponseInProgress
    ResponseCompletedEvent{} -> EventResponseCompleted
    ResponseFailedEvent{} -> EventResponseFailed
    ResponseIncompleteEvent{} -> EventResponseIncomplete
    ResponseQueuedEvent{} -> EventResponseQueued
    ResponseOutputItemAddedEvent{} -> EventOutputItemAdded
    ResponseOutputItemDoneEvent{} -> EventOutputItemDone
    ResponseErrorEvent{} -> EventError
    ResponseNestedErrorEvent{} -> EventError
    OtherResponseStreamEvent { otherEventType } -> otherEventType

responseStreamEventSequenceNumber :: ResponseStreamEvent -> Maybe Int
responseStreamEventSequenceNumber event = event.sequenceNumber

instance ToJSON ResponseStreamEvent where
    toJSON = \case
        ResponseCreatedEvent { response, sequenceNumber, eventExtraFields } ->
            lifecycleEvent "response.created" response sequenceNumber eventExtraFields
        ResponseInProgressEvent { response, sequenceNumber, eventExtraFields } ->
            lifecycleEvent "response.in_progress" response sequenceNumber eventExtraFields
        ResponseCompletedEvent { response, sequenceNumber, eventExtraFields } ->
            lifecycleEvent "response.completed" response sequenceNumber eventExtraFields
        ResponseFailedEvent { response, sequenceNumber, eventExtraFields } ->
            lifecycleEvent "response.failed" response sequenceNumber eventExtraFields
        ResponseIncompleteEvent { response, sequenceNumber, eventExtraFields } ->
            lifecycleEvent "response.incomplete" response sequenceNumber eventExtraFields
        ResponseQueuedEvent { response, sequenceNumber, eventExtraFields } ->
            lifecycleEvent "response.queued" response sequenceNumber eventExtraFields
        ResponseOutputItemAddedEvent { item, outputIndex, sequenceNumber, eventExtraFields } ->
            outputItemEvent "response.output_item.added" item outputIndex sequenceNumber eventExtraFields
        ResponseOutputItemDoneEvent { item, outputIndex, sequenceNumber, eventExtraFields } ->
            outputItemEvent "response.output_item.done" item outputIndex sequenceNumber eventExtraFields
        ResponseErrorEvent { streamError, sequenceNumber, eventExtraFields } ->
            topLevelErrorEvent streamError sequenceNumber eventExtraFields
        ResponseNestedErrorEvent { streamError, sequenceNumber, eventExtraFields } ->
            objectWith eventExtraFields
                [ Just (field "type" ("error" :: Text))
                , optionalField "sequence_number" sequenceNumber
                , Just (field "error" streamError)
                ]
        OtherResponseStreamEvent { otherEventType, sequenceNumber, eventExtraFields } ->
            objectWith eventExtraFields
                [ Just (field "type" (streamEventTypeText otherEventType))
                , optionalField "sequence_number" sequenceNumber
                ]
      where
        lifecycleEvent eventType response sequenceNumber extras = objectWith extras
            [ Just (field "type" (eventType :: Text))
            , optionalField "sequence_number" sequenceNumber
            , Just (field "response" response)
            ]
        outputItemEvent eventType item outputIndex sequenceNumber extras = objectWith extras
            [ Just (field "type" (eventType :: Text))
            , optionalField "sequence_number" sequenceNumber
            , Just (field "output_index" outputIndex)
            , Just (field "item" item)
            ]
        topLevelErrorEvent streamError sequenceNumber extras =
            objectWith (KeyMap.union extras streamError.extraFields)
                [ Just (field "type" ("error" :: Text))
                , optionalField "sequence_number" sequenceNumber
                , optionalField "code" streamError.code
                , Just (field "message" streamError.message)
                , optionalField "param" streamError.param
                , optionalField "resets_in_seconds" streamError.retryAfter
                ]

instance FromJSON ResponseStreamEvent where
    parseJSON = withObject "ResponseStreamEvent" $ \o -> do
        tag <- o .: "type"
        sequenceNumber <- o .:? "sequence_number"
        case parseStreamEventType tag of
            EventResponseCreated -> lifecycle ResponseCreatedEvent sequenceNumber o
            EventResponseInProgress -> lifecycle ResponseInProgressEvent sequenceNumber o
            EventResponseCompleted -> lifecycle ResponseCompletedEvent sequenceNumber o
            EventResponseFailed -> lifecycle ResponseFailedEvent sequenceNumber o
            EventResponseIncomplete -> lifecycle ResponseIncompleteEvent sequenceNumber o
            EventResponseQueued -> lifecycle ResponseQueuedEvent sequenceNumber o
            EventOutputItemAdded -> outputItem ResponseOutputItemAddedEvent sequenceNumber o
            EventOutputItemDone -> outputItem ResponseOutputItemDoneEvent sequenceNumber o
            EventError -> parseErrorEvent sequenceNumber o
            eventType -> pure OtherResponseStreamEvent
                { otherEventType = eventType
                , sequenceNumber
                , eventExtraFields = without ["type", "sequence_number"] o
                }
      where
        lifecycle constructor sequenceNumber object = constructor
            <$> object .: "response"
            <*> pure sequenceNumber
            <*> pure (without ["type", "sequence_number", "response"] object)
        outputItem constructor sequenceNumber object = constructor
            <$> object .: "item"
            <*> object .: "output_index"
            <*> pure sequenceNumber
            <*> pure (without ["type", "sequence_number", "item", "output_index"] object)
        parseErrorEvent sequenceNumber object = do
            nestedError <- object .:? "error"
            case nestedError of
                Just streamError -> pure ResponseNestedErrorEvent
                    { streamError
                    , sequenceNumber
                    , eventExtraFields = without ["type", "sequence_number", "error"] object
                    }
                Nothing -> do
                    streamError <- ResponseStreamError
                        <$> pure Nothing
                        <*> object .:? "code"
                        <*> object .: "message"
                        <*> object .:? "param"
                        <*> object .:? "resets_in_seconds"
                        <*> pure (without ["type", "sequence_number", "message"] object)
                    pure ResponseErrorEvent
                        { streamError
                        , sequenceNumber
                        , eventExtraFields = mempty
                        }

-- | Decode an SSE event when the transport supplied the event name separately.
-- A @type@ member in the JSON payload wins only when it agrees with the SSE
-- event name; disagreement is rejected instead of being silently normalised.
parseStreamEventWithType :: Text -> Aeson.Value -> Parser ResponseStreamEvent
parseStreamEventWithType suppliedType = withObject "ResponseStreamEvent" $ \o -> do
    payloadType <- o .:? "type"
    case payloadType of
        Just actual | actual /= suppliedType ->
            fail ("SSE event type " <> Text.unpack suppliedType <> " disagrees with JSON type " <> Text.unpack actual)
        _ -> parseJSON (Aeson.Object (KeyMap.insert "type" (Aeson.String suppliedType) o))
