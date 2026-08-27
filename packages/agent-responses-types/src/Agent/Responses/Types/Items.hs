-- | Input and output items carried by Responses requests and responses.
module Agent.Responses.Types.Items
    ( ResponseInput(..), responseInputEncoder, responseInputDecoder
    , ResponseItem(..), responseItemEncoder, responseItemDecoder
    , ResponseItemType(..), parseResponseItemType, responseItemTypeText
    , responseItemTypeEncoder, responseItemTypeDecoder
    , ResponseMessage(..), responseMessageEncoder, responseMessageDecoder
    , FunctionCall(..), functionCallEncoder, functionCallDecoder
    , FunctionCallOutput(..), functionCallOutputEncoder, functionCallOutputDecoder
    , CustomToolCall(..), customToolCallEncoder, customToolCallDecoder
    , CustomToolCallOutput(..), customToolCallOutputEncoder, customToolCallOutputDecoder
    , ReasoningItem(..), reasoningItemEncoder, reasoningItemDecoder
    , ReasoningSummaryPart(..), reasoningSummaryPartEncoder, reasoningSummaryPartDecoder
    , ItemReference(..), itemReferenceEncoder, itemReferenceDecoder
    , ResponseAgentMessage(..), responseAgentMessageEncoder, responseAgentMessageDecoder
    , InternalChatMetadata(..), internalChatMetadataEncoder, internalChatMetadataDecoder
    , AdditionalToolsItem(..), additionalToolsItemEncoder, additionalToolsItemDecoder
    , LocalShellCall(..), localShellCallEncoder, localShellCallDecoder
    , LocalShellAction(..), localShellActionEncoder, localShellActionDecoder
    , ToolSearchCall(..), toolSearchCallEncoder, toolSearchCallDecoder
    , ToolSearchOutput(..), toolSearchOutputEncoder, toolSearchOutputDecoder
    , WebSearchCall(..), webSearchCallEncoder, webSearchCallDecoder
    , WebSearchAction(..), webSearchActionEncoder, webSearchActionDecoder
    , ImageGenerationCall(..), imageGenerationCallEncoder, imageGenerationCallDecoder
    , CompactionItem(..), compactionItemEncoder, compactionItemDecoder
    , CompactionTriggerItem(..), compactionTriggerItemEncoder, compactionTriggerItemDecoder
    , ContextCompactionItem(..), contextCompactionItemEncoder, contextCompactionItemDecoder
    ) where

import Agent.Json
    ( Extensions, RawJson )
import Agent.Json.Decoder (Decoder)
import qualified Agent.Json.Decoder as D
import Agent.Json.Encoder (Encoder)
import qualified Agent.Json.Encoder as E
import Agent.Responses.Types.Common (TaggedObject(..))
import Agent.Responses.Types.Content
import Agent.Responses.Types.Items.Known
import Data.Text (Text)

data ResponseMessage = ResponseMessage
    { messageId :: !(Maybe Text), content :: !MessageContent
    , role :: !ResponseRole, status :: !(Maybe ItemStatus)
    , phase :: !(Maybe Text), passthrough :: !(Maybe InternalChatMetadata)
    , extraFields :: !Extensions
    } deriving stock (Eq, Show)

data FunctionCall = FunctionCall
    { itemId :: !(Maybe Text), callId :: !Text, name :: !Text
    , namespace :: !(Maybe Text), arguments :: !Text
    , encryptedFunctionArgs :: !(Maybe [Text]), status :: !(Maybe ItemStatus)
    , extraFields :: !Extensions
    } deriving stock (Eq, Show)

data FunctionCallOutput = FunctionCallOutput
    { itemId :: !(Maybe Text), callId :: !Text, name :: !(Maybe Text)
    , namespace :: !(Maybe Text), output :: !RawJson
    , status :: !(Maybe ItemStatus), extraFields :: !Extensions
    } deriving stock (Eq, Show)

data CustomToolCall = CustomToolCall
    { itemId :: !(Maybe Text), callId :: !Text, name :: !Text
    , namespace :: !(Maybe Text), input :: !Text
    , status :: !(Maybe ItemStatus), extraFields :: !Extensions
    } deriving stock (Eq, Show)

data CustomToolCallOutput = CustomToolCallOutput
    { itemId :: !(Maybe Text), callId :: !Text, name :: !(Maybe Text)
    , output :: !RawJson, status :: !(Maybe ItemStatus)
    , extraFields :: !Extensions
    } deriving stock (Eq, Show)

data ReasoningSummaryPart = ReasoningSummaryPart
    { partType :: !Text, text :: !(Maybe Text), extraFields :: !Extensions
    } deriving stock (Eq, Show)

data ReasoningItem = ReasoningItem
    { itemId :: !(Maybe Text), summary :: ![ReasoningSummaryPart]
    , content :: !(Maybe [ResponseContentPart])
    , encryptedContent :: !(Maybe Text), status :: !(Maybe ItemStatus)
    , extraFields :: !Extensions
    } deriving stock (Eq)

instance Show ReasoningItem where
    show item =
        "ReasoningItem { itemId = " <> show item.itemId
            <> ", summary = " <> show item.summary
            <> ", content = " <> show item.content
            <> ", encryptedContent = "
            <> maybe "Nothing" (const "Just <redacted>") item.encryptedContent
            <> ", status = " <> show item.status
            <> ", extraFields = " <> show item.extraFields <> " }"

data ItemReference = ItemReference
    { itemId :: !Text, extraFields :: !Extensions
    } deriving stock (Eq, Show)

data ResponseAgentMessage = ResponseAgentMessage
    { messageId :: !(Maybe Text), author :: !(Maybe Text)
    , recipient :: !(Maybe Text), content :: ![ResponseContentPart]
    , passthrough :: !(Maybe InternalChatMetadata), extraFields :: !Extensions
    } deriving stock (Eq, Show)

data AdditionalToolsItem = AdditionalToolsItem
    { itemId :: !(Maybe Text), role :: !Text, tools :: ![RawJson]
    , extraFields :: !Extensions
    } deriving stock (Eq, Show)

data LocalShellAction
    = LocalShellExec
        { command :: ![Text], timeoutMs :: !(Maybe Integer)
        , workingDirectory :: !(Maybe Text), env :: !(Maybe Extensions)
        , user :: !(Maybe Text), extraFields :: !Extensions
        }
    | LocalShellActionOther !TaggedObject
    deriving stock (Eq, Show)

data LocalShellCall = LocalShellCall
    { itemId :: !(Maybe Text), callId :: !(Maybe Text)
    , status :: !(Maybe ItemStatus), action :: !(Maybe LocalShellAction)
    , extraFields :: !Extensions
    } deriving stock (Eq, Show)

data ToolSearchCall = ToolSearchCall
    { itemId :: !(Maybe Text), callId :: !(Maybe Text), status :: !(Maybe Text)
    , execution :: !(Maybe Text), arguments :: !(Maybe RawJson)
    , extraFields :: !Extensions
    } deriving stock (Eq, Show)

data ToolSearchOutput = ToolSearchOutput
    { itemId :: !(Maybe Text), callId :: !(Maybe Text), status :: !(Maybe Text)
    , execution :: !(Maybe Text), tools :: ![RawJson]
    , extraFields :: !Extensions
    } deriving stock (Eq, Show)

data WebSearchAction
    = WebSearchQuery
        { query :: !(Maybe Text), queries :: !(Maybe [Text])
        , extraFields :: !Extensions
        }
    | WebSearchOpenPage
        { url :: !(Maybe Text), extraFields :: !Extensions }
    | WebSearchFindInPage
        { url :: !(Maybe Text), pattern :: !(Maybe Text)
        , extraFields :: !Extensions
        }
    | WebSearchActionOther !TaggedObject
    deriving stock (Eq, Show)

data WebSearchCall = WebSearchCall
    { itemId :: !(Maybe Text), status :: !(Maybe Text)
    , action :: !(Maybe WebSearchAction), extraFields :: !Extensions
    } deriving stock (Eq, Show)

data ImageGenerationCall = ImageGenerationCall
    { itemId :: !(Maybe Text), status :: !(Maybe Text)
    , revisedPrompt :: !(Maybe Text), result :: !(Maybe Text)
    , extraFields :: !Extensions
    } deriving stock (Eq, Show)

data CompactionItem = CompactionItem
    { itemId :: !(Maybe Text), encryptedContent :: !(Maybe Text)
    , extraFields :: !Extensions
    } deriving stock (Eq, Show)

data CompactionTriggerItem = CompactionTriggerItem
    { extraFields :: !Extensions
    } deriving stock (Eq, Show)

data ContextCompactionItem = ContextCompactionItem
    { itemId :: !(Maybe Text), encryptedContent :: !(Maybe Text)
    , extraFields :: !Extensions
    } deriving stock (Eq, Show)

-- Encoding -------------------------------------------------------------------

taggedObjectEncoder :: Encoder TaggedObject
taggedObjectEncoder = E.objectWithExtensions (.fields)
    [E.field "type" E.text (.tag)]

extensionsEncoder :: Encoder Extensions
extensionsEncoder = E.object [E.extensionsField id]

tag :: Text -> E.Field a
tag value = E.field "type" E.text (const value)

responseMessageEncoder :: Encoder ResponseMessage
responseMessageEncoder = E.objectWithExtensions (.extraFields)
    [ tag "message", E.optionalField "id" E.text (.messageId)
    , E.field "content" messageContentEncoder (.content)
    , E.field "role" responseRoleEncoder (.role)
    , E.optionalField "status" itemStatusEncoder (.status)
    , E.optionalField "phase" E.text (.phase)
    , E.optionalField "internal_chat_message_metadata_passthrough"
        internalChatMetadataEncoder (.passthrough)
    ]

functionCallEncoder :: Encoder FunctionCall
functionCallEncoder = E.objectWithExtensions (.extraFields)
    [ tag "function_call", E.optionalField "id" E.text (.itemId)
    , E.field "call_id" E.text (.callId), E.field "name" E.text (.name)
    , E.optionalField "namespace" E.text (.namespace)
    , E.field "arguments" E.text (.arguments)
    , E.optionalField "encrypted_function_args" (E.list E.text) (.encryptedFunctionArgs)
    , E.optionalField "status" itemStatusEncoder (.status)
    ]

functionCallOutputEncoder :: Encoder FunctionCallOutput
functionCallOutputEncoder = E.objectWithExtensions (.extraFields)
    [ tag "function_call_output", E.optionalField "id" E.text (.itemId)
    , E.field "call_id" E.text (.callId), E.optionalField "name" E.text (.name)
    , E.optionalField "namespace" E.text (.namespace)
    , E.field "output" E.rawJson (.output)
    , E.optionalField "status" itemStatusEncoder (.status)
    ]

customToolCallEncoder :: Encoder CustomToolCall
customToolCallEncoder = E.objectWithExtensions (.extraFields)
    [ tag "custom_tool_call", E.optionalField "id" E.text (.itemId)
    , E.field "call_id" E.text (.callId), E.field "name" E.text (.name)
    , E.optionalField "namespace" E.text (.namespace)
    , E.field "input" E.text (.input)
    , E.optionalField "status" itemStatusEncoder (.status)
    ]

customToolCallOutputEncoder :: Encoder CustomToolCallOutput
customToolCallOutputEncoder = E.objectWithExtensions (.extraFields)
    [ tag "custom_tool_call_output", E.optionalField "id" E.text (.itemId)
    , E.field "call_id" E.text (.callId), E.optionalField "name" E.text (.name)
    , E.field "output" E.rawJson (.output)
    , E.optionalField "status" itemStatusEncoder (.status)
    ]

reasoningSummaryPartEncoder :: Encoder ReasoningSummaryPart
reasoningSummaryPartEncoder = E.objectWithExtensions (.extraFields)
    [ E.field "type" E.text (.partType), E.optionalField "text" E.text (.text) ]

reasoningItemEncoder :: Encoder ReasoningItem
reasoningItemEncoder = E.objectWithExtensions (.extraFields)
    [ tag "reasoning", E.optionalField "id" E.text (.itemId)
    , E.field "summary" (E.list reasoningSummaryPartEncoder) (.summary)
    , E.optionalField "content" (E.list responseContentPartEncoder) (.content)
    , E.optionalField "encrypted_content" E.text (.encryptedContent)
    , E.optionalField "status" itemStatusEncoder (.status)
    ]

itemReferenceEncoder :: Encoder ItemReference
itemReferenceEncoder = E.objectWithExtensions (.extraFields)
    [tag "item_reference", E.field "id" E.text (.itemId)]

responseAgentMessageEncoder :: Encoder ResponseAgentMessage
responseAgentMessageEncoder = E.objectWithExtensions (.extraFields)
    [ tag "agent_message", E.optionalField "id" E.text (.messageId)
    , E.optionalField "author" E.text (.author)
    , E.optionalField "recipient" E.text (.recipient)
    , E.field "content" (E.list responseContentPartEncoder) (.content)
    , E.optionalField "internal_chat_message_metadata_passthrough"
        internalChatMetadataEncoder (.passthrough)
    ]

additionalToolsItemEncoder :: Encoder AdditionalToolsItem
additionalToolsItemEncoder = E.objectWithExtensions (.extraFields)
    [ tag "additional_tools", E.optionalField "id" E.text (.itemId)
    , E.field "role" E.text (.role), E.field "tools" (E.list E.rawJson) (.tools)
    ]

localShellExecEncoder :: Encoder LocalShellAction
localShellExecEncoder = E.objectWithExtensions (.extraFields)
    [ tag "exec", E.field "command" (E.list E.text) (.command)
    , E.optionalField "timeout_ms" E.integer (.timeoutMs)
    , E.optionalField "working_directory" E.text (.workingDirectory)
    , E.optionalField "env" extensionsEncoder (.env)
    , E.optionalField "user" E.text (.user)
    ]

localShellActionEncoder :: Encoder LocalShellAction
localShellActionEncoder = E.choose \case
    LocalShellExec{} -> localShellExecEncoder
    LocalShellActionOther{} ->
        E.contramap
            (\case
                LocalShellActionOther value -> value
                _ -> error "localShellActionEncoder: impossible constructor")
            taggedObjectEncoder

localShellCallEncoder :: Encoder LocalShellCall
localShellCallEncoder = E.objectWithExtensions (.extraFields)
    [ tag "local_shell_call", E.optionalField "id" E.text (.itemId)
    , E.optionalField "call_id" E.text (.callId)
    , E.optionalField "status" itemStatusEncoder (.status)
    , E.optionalField "action" localShellActionEncoder (.action)
    ]

toolSearchCallEncoder :: Encoder ToolSearchCall
toolSearchCallEncoder = E.objectWithExtensions (.extraFields)
    [ tag "tool_search_call", E.optionalField "id" E.text (.itemId)
    , E.optionalField "call_id" E.text (.callId)
    , E.optionalField "status" E.text (.status)
    , E.optionalField "execution" E.text (.execution)
    , E.optionalField "arguments" E.rawJson (.arguments)
    ]

toolSearchOutputEncoder :: Encoder ToolSearchOutput
toolSearchOutputEncoder = E.objectWithExtensions (.extraFields)
    [ tag "tool_search_output", E.optionalField "id" E.text (.itemId)
    , E.optionalField "call_id" E.text (.callId)
    , E.optionalField "status" E.text (.status)
    , E.optionalField "execution" E.text (.execution)
    , E.field "tools" (E.list E.rawJson) (.tools)
    ]

webSearchQueryEncoder, webSearchOpenPageEncoder, webSearchFindInPageEncoder
    :: Encoder WebSearchAction
webSearchQueryEncoder = E.objectWithExtensions (.extraFields)
    [ tag "search", E.optionalField "query" E.text (.query)
    , E.optionalField "queries" (E.list E.text) (.queries) ]
webSearchOpenPageEncoder = E.objectWithExtensions (.extraFields)
    [tag "open_page", E.optionalField "url" E.text (.url)]
webSearchFindInPageEncoder = E.objectWithExtensions (.extraFields)
    [ tag "find_in_page", E.optionalField "url" E.text (.url)
    , E.optionalField "pattern" E.text (.pattern) ]

webSearchActionEncoder :: Encoder WebSearchAction
webSearchActionEncoder = E.choose \case
    WebSearchQuery{} -> webSearchQueryEncoder
    WebSearchOpenPage{} -> webSearchOpenPageEncoder
    WebSearchFindInPage{} -> webSearchFindInPageEncoder
    WebSearchActionOther{} ->
        E.contramap
            (\case
                WebSearchActionOther value -> value
                _ -> error "webSearchActionEncoder: impossible constructor")
            taggedObjectEncoder

webSearchCallEncoder :: Encoder WebSearchCall
webSearchCallEncoder = E.objectWithExtensions (.extraFields)
    [ tag "web_search_call", E.optionalField "id" E.text (.itemId)
    , E.optionalField "status" E.text (.status)
    , E.optionalField "action" webSearchActionEncoder (.action) ]

imageGenerationCallEncoder :: Encoder ImageGenerationCall
imageGenerationCallEncoder = E.objectWithExtensions (.extraFields)
    [ tag "image_generation_call", E.optionalField "id" E.text (.itemId)
    , E.optionalField "status" E.text (.status)
    , E.optionalField "revised_prompt" E.text (.revisedPrompt)
    , E.optionalField "result" E.text (.result) ]

compactionItemEncoder :: Encoder CompactionItem
compactionItemEncoder = E.objectWithExtensions (.extraFields)
    [ tag "compaction", E.optionalField "id" E.text (.itemId)
    , E.optionalField "encrypted_content" E.text (.encryptedContent) ]

compactionTriggerItemEncoder :: Encoder CompactionTriggerItem
compactionTriggerItemEncoder = E.objectWithExtensions (.extraFields)
    [tag "compaction_trigger"]

contextCompactionItemEncoder :: Encoder ContextCompactionItem
contextCompactionItemEncoder = E.objectWithExtensions (.extraFields)
    [ tag "context_compaction", E.optionalField "id" E.text (.itemId)
    , E.optionalField "encrypted_content" E.text (.encryptedContent) ]

-- Decoding -------------------------------------------------------------------

extensionsDecoder :: Decoder Extensions
extensionsDecoder = D.objectFields D.extensionFields

directObject plan =
    D.objectFields (plan <* knownTypeField)
  where
    knownTypeField =
        D.defaultField () "type" (() <$ D.text)

responseMessageDecoder :: Decoder ResponseMessage
responseMessageDecoder = directObject $
    ResponseMessage
        <$> D.optionalField "id" D.text
        <*> D.requiredField "content" messageContentDecoder
        <*> D.requiredField "role" responseRoleDecoder
        <*> D.optionalField "status" itemStatusDecoder
        <*> D.optionalField "phase" D.text
        <*> D.optionalField
            "internal_chat_message_metadata_passthrough"
            internalChatMetadataDecoder
        <*> D.extensionFields

functionCallDecoder :: Decoder FunctionCall
functionCallDecoder = directObject $
    FunctionCall
        <$> D.optionalField "id" D.text
        <*> D.requiredField "call_id" D.text
        <*> D.requiredField "name" D.text
        <*> D.optionalField "namespace" D.text
        <*> D.requiredField "arguments" D.text
        <*> D.optionalField
            "encrypted_function_args"
            (D.list D.text)
        <*> D.optionalField "status" itemStatusDecoder
        <*> D.extensionFields

functionCallOutputDecoder :: Decoder FunctionCallOutput
functionCallOutputDecoder = directObject $
    FunctionCallOutput
        <$> D.optionalField "id" D.text
        <*> D.requiredField "call_id" D.text
        <*> D.optionalField "name" D.text
        <*> D.optionalField "namespace" D.text
        <*> D.requiredField "output" D.rawJson
        <*> D.optionalField "status" itemStatusDecoder
        <*> D.extensionFields

customToolCallDecoder :: Decoder CustomToolCall
customToolCallDecoder = directObject $
    CustomToolCall
        <$> D.optionalField "id" D.text
        <*> D.requiredField "call_id" D.text
        <*> D.requiredField "name" D.text
        <*> D.optionalField "namespace" D.text
        <*> D.requiredField "input" D.text
        <*> D.optionalField "status" itemStatusDecoder
        <*> D.extensionFields

customToolCallOutputDecoder :: Decoder CustomToolCallOutput
customToolCallOutputDecoder = directObject $
    CustomToolCallOutput
        <$> D.optionalField "id" D.text
        <*> D.requiredField "call_id" D.text
        <*> D.optionalField "name" D.text
        <*> D.requiredField "output" D.rawJson
        <*> D.optionalField "status" itemStatusDecoder
        <*> D.extensionFields

reasoningSummaryPartDecoder :: Decoder ReasoningSummaryPart
reasoningSummaryPartDecoder = D.objectFields $
    ReasoningSummaryPart
        <$> D.requiredField "type" D.text
        <*> D.optionalField "text" D.text
        <*> D.extensionFields

reasoningItemDecoder :: Decoder ReasoningItem
reasoningItemDecoder = directObject $
    ReasoningItem
        <$> D.optionalField "id" D.text
        <*> D.defaultField [] "summary"
            (D.list reasoningSummaryPartDecoder)
        <*> D.optionalField "content"
            (D.list responseContentPartDecoder)
        <*> D.optionalField "encrypted_content" D.text
        <*> D.optionalField "status" itemStatusDecoder
        <*> D.extensionFields

itemReferenceDecoder :: Decoder ItemReference
itemReferenceDecoder = directObject $
    ItemReference
        <$> D.requiredField "id" D.text
        <*> D.extensionFields

responseAgentMessageDecoder :: Decoder ResponseAgentMessage
responseAgentMessageDecoder = directObject $
    ResponseAgentMessage
        <$> D.optionalField "id" D.text
        <*> D.optionalField "author" D.text
        <*> D.optionalField "recipient" D.text
        <*> D.defaultField [] "content"
            (D.list responseContentPartDecoder)
        <*> D.optionalField
            "internal_chat_message_metadata_passthrough"
            internalChatMetadataDecoder
        <*> D.extensionFields

additionalToolsItemDecoder :: Decoder AdditionalToolsItem
additionalToolsItemDecoder = directObject $
    AdditionalToolsItem
        <$> D.optionalField "id" D.text
        <*> D.defaultField "developer" "role" D.text
        <*> D.defaultField [] "tools" (D.list D.rawJson)
        <*> D.extensionFields

taggedObjectDecoder :: Decoder TaggedObject
taggedObjectDecoder = D.objectFields $
    TaggedObject
        <$> D.requiredField "type" D.text
        <*> D.extensionFields

localShellExecDecoder :: Decoder LocalShellAction
localShellExecDecoder = directObject $
    LocalShellExec
        <$> D.defaultField [] "command" (D.list D.text)
        <*> D.optionalField "timeout_ms" D.integer
        <*> D.optionalField "working_directory" D.text
        <*> D.optionalField "env" extensionsDecoder
        <*> D.optionalField "user" D.text
        <*> D.extensionFields

localShellActionDecoder :: Decoder LocalShellAction
localShellActionDecoder =
    D.discriminatedObject "type" \case
        "exec" -> localShellExecDecoder
        _ -> LocalShellActionOther <$> taggedObjectDecoder

localShellCallDecoder :: Decoder LocalShellCall
localShellCallDecoder = directObject $
    LocalShellCall
        <$> D.optionalField "id" D.text
        <*> D.optionalField "call_id" D.text
        <*> D.optionalField "status" itemStatusDecoder
        <*> D.optionalField "action" localShellActionDecoder
        <*> D.extensionFields

toolSearchCallDecoder :: Decoder ToolSearchCall
toolSearchCallDecoder = directObject $
    ToolSearchCall
        <$> D.optionalField "id" D.text
        <*> D.optionalField "call_id" D.text
        <*> D.optionalField "status" D.text
        <*> D.optionalField "execution" D.text
        <*> D.optionalField "arguments" D.rawJson
        <*> D.extensionFields

toolSearchOutputDecoder :: Decoder ToolSearchOutput
toolSearchOutputDecoder = directObject $
    ToolSearchOutput
        <$> D.optionalField "id" D.text
        <*> D.optionalField "call_id" D.text
        <*> D.optionalField "status" D.text
        <*> D.optionalField "execution" D.text
        <*> D.defaultField [] "tools" (D.list D.rawJson)
        <*> D.extensionFields

webSearchQueryDecoder :: Decoder WebSearchAction
webSearchQueryDecoder = directObject $
    WebSearchQuery
        <$> D.optionalField "query" D.text
        <*> D.optionalField "queries" (D.list D.text)
        <*> D.extensionFields

webSearchOpenPageDecoder :: Decoder WebSearchAction
webSearchOpenPageDecoder = directObject $
    WebSearchOpenPage
        <$> D.optionalField "url" D.text
        <*> D.extensionFields

webSearchFindInPageDecoder :: Decoder WebSearchAction
webSearchFindInPageDecoder = directObject $
    WebSearchFindInPage
        <$> D.optionalField "url" D.text
        <*> D.optionalField "pattern" D.text
        <*> D.extensionFields

webSearchActionDecoder :: Decoder WebSearchAction
webSearchActionDecoder =
    D.discriminatedObject "type" \case
        "search" -> webSearchQueryDecoder
        "open_page" -> webSearchOpenPageDecoder
        "find_in_page" -> webSearchFindInPageDecoder
        _ -> WebSearchActionOther <$> taggedObjectDecoder

webSearchCallDecoder :: Decoder WebSearchCall
webSearchCallDecoder = directObject $
    WebSearchCall
        <$> D.optionalField "id" D.text
        <*> D.optionalField "status" D.text
        <*> D.optionalField "action" webSearchActionDecoder
        <*> D.extensionFields

imageGenerationCallDecoder :: Decoder ImageGenerationCall
imageGenerationCallDecoder = directObject $
    ImageGenerationCall
        <$> D.optionalField "id" D.text
        <*> D.optionalField "status" D.text
        <*> D.optionalField "revised_prompt" D.text
        <*> D.optionalField "result" D.text
        <*> D.extensionFields

compactionItemDecoder :: Decoder CompactionItem
compactionItemDecoder = directObject $
    CompactionItem
        <$> D.optionalField "id" D.text
        <*> D.optionalField "encrypted_content" D.text
        <*> D.extensionFields

compactionTriggerItemDecoder :: Decoder CompactionTriggerItem
compactionTriggerItemDecoder = directObject $
    CompactionTriggerItem <$> D.extensionFields

contextCompactionItemDecoder :: Decoder ContextCompactionItem
contextCompactionItemDecoder = directObject $
    ContextCompactionItem
        <$> D.optionalField "id" D.text
        <*> D.optionalField "encrypted_content" D.text
        <*> D.extensionFields

-- Item unions ----------------------------------------------------------------

data ResponseItem
    = MessageItem !ResponseMessage
    | FunctionCallItem !FunctionCall
    | FunctionCallOutputItem !FunctionCallOutput
    | CustomToolCallItem !CustomToolCall
    | CustomToolCallOutputItem !CustomToolCallOutput
    | ReasoningItemValue !ReasoningItem
    | ItemReferenceValue !ItemReference
    | AgentMessageItem !ResponseAgentMessage
    | AdditionalToolsItemValue !AdditionalToolsItem
    | LocalShellCallItem !LocalShellCall
    | ToolSearchCallItem !ToolSearchCall
    | ToolSearchOutputItem !ToolSearchOutput
    | WebSearchCallItem !WebSearchCall
    | ImageGenerationCallItem !ImageGenerationCall
    | CompactionItemValue !CompactionItem
    | CompactionTriggerItemValue !CompactionTriggerItem
    | ContextCompactionItemValue !ContextCompactionItem
    | KnownResponseItem !ResponseItemType !TaggedObject
    | UnknownResponseItem !TaggedObject
    deriving stock (Eq, Show)

responseItemEncoder :: Encoder ResponseItem
responseItemEncoder = E.choose \case
    MessageItem{} -> E.contramap
        (\case MessageItem v -> v; _ -> impossible) responseMessageEncoder
    FunctionCallItem{} ->
        E.contramap
            (\case FunctionCallItem v -> v; _ -> impossible)
            functionCallEncoder
    FunctionCallOutputItem{} ->
        E.contramap
            (\case FunctionCallOutputItem v -> v; _ -> impossible)
            functionCallOutputEncoder
    CustomToolCallItem{} ->
        E.contramap
            (\case CustomToolCallItem v -> v; _ -> impossible)
            customToolCallEncoder
    CustomToolCallOutputItem{} ->
        E.contramap
            (\case CustomToolCallOutputItem v -> v; _ -> impossible)
            customToolCallOutputEncoder
    ReasoningItemValue{} ->
        E.contramap
            (\case ReasoningItemValue v -> v; _ -> impossible)
            reasoningItemEncoder
    ItemReferenceValue{} ->
        E.contramap
            (\case ItemReferenceValue v -> v; _ -> impossible)
            itemReferenceEncoder
    AgentMessageItem{} ->
        E.contramap
            (\case AgentMessageItem v -> v; _ -> impossible)
            responseAgentMessageEncoder
    AdditionalToolsItemValue{} ->
        E.contramap
            (\case AdditionalToolsItemValue v -> v; _ -> impossible)
            additionalToolsItemEncoder
    LocalShellCallItem{} ->
        E.contramap
            (\case LocalShellCallItem v -> v; _ -> impossible)
            localShellCallEncoder
    ToolSearchCallItem{} ->
        E.contramap
            (\case ToolSearchCallItem v -> v; _ -> impossible)
            toolSearchCallEncoder
    ToolSearchOutputItem{} ->
        E.contramap
            (\case ToolSearchOutputItem v -> v; _ -> impossible)
            toolSearchOutputEncoder
    WebSearchCallItem{} ->
        E.contramap
            (\case WebSearchCallItem v -> v; _ -> impossible)
            webSearchCallEncoder
    ImageGenerationCallItem{} ->
        E.contramap
            (\case ImageGenerationCallItem v -> v; _ -> impossible)
            imageGenerationCallEncoder
    CompactionItemValue{} ->
        E.contramap
            (\case CompactionItemValue v -> v; _ -> impossible)
            compactionItemEncoder
    CompactionTriggerItemValue{} ->
        E.contramap
            (\case CompactionTriggerItemValue v -> v; _ -> impossible)
            compactionTriggerItemEncoder
    ContextCompactionItemValue{} ->
        E.contramap
            (\case ContextCompactionItemValue v -> v; _ -> impossible)
            contextCompactionItemEncoder
    KnownResponseItem{} ->
        E.contramap
            (\case KnownResponseItem _ v -> v; _ -> impossible)
            taggedObjectEncoder
    UnknownResponseItem{} ->
        E.contramap
            (\case UnknownResponseItem v -> v; _ -> impossible)
            taggedObjectEncoder
  where
    impossible = error "responseItemEncoder: impossible constructor"

responseItemDecoder :: Decoder ResponseItem
responseItemDecoder =
    D.discriminatedObject "type" \tagValue ->
        case parseResponseItemType tagValue of
            ItemMessage -> MessageItem <$> responseMessageDecoder
            ItemFunctionCall -> FunctionCallItem <$> functionCallDecoder
            ItemFunctionCallOutput ->
                FunctionCallOutputItem <$> functionCallOutputDecoder
            ItemCustomToolCall ->
                CustomToolCallItem <$> customToolCallDecoder
            ItemCustomToolCallOutput ->
                CustomToolCallOutputItem <$> customToolCallOutputDecoder
            ItemReasoning -> ReasoningItemValue <$> reasoningItemDecoder
            ItemReferenceType -> ItemReferenceValue <$> itemReferenceDecoder
            ItemAgentMessage ->
                AgentMessageItem <$> responseAgentMessageDecoder
            ItemAdditionalTools ->
                AdditionalToolsItemValue <$> additionalToolsItemDecoder
            ItemLocalShellCall ->
                LocalShellCallItem <$> localShellCallDecoder
            ItemToolSearchCall ->
                ToolSearchCallItem <$> toolSearchCallDecoder
            ItemToolSearchOutput ->
                ToolSearchOutputItem <$> toolSearchOutputDecoder
            ItemWebSearchCall ->
                WebSearchCallItem <$> webSearchCallDecoder
            ItemImageGenerationCall ->
                ImageGenerationCallItem <$> imageGenerationCallDecoder
            ItemCompaction ->
                CompactionItemValue <$> compactionItemDecoder
            ItemCompactionTrigger ->
                CompactionTriggerItemValue <$> compactionTriggerItemDecoder
            ItemContextCompaction ->
                ContextCompactionItemValue <$> contextCompactionItemDecoder
            ItemUnknownType{} ->
                UnknownResponseItem <$> taggedObjectDecoder
            itemType ->
                KnownResponseItem itemType <$> taggedObjectDecoder

data ResponseInput
    = ResponseInputText !Text
    | ResponseInputItems ![ResponseItem]
    deriving stock (Eq, Show)

responseInputEncoder :: Encoder ResponseInput
responseInputEncoder = E.choose \case
    ResponseInputText{} ->
        E.contramap
            (\case ResponseInputText value -> value; _ -> impossible)
            E.text
    ResponseInputItems{} ->
        E.contramap
            (\case ResponseInputItems values -> values; _ -> impossible)
            (E.list responseItemEncoder)
  where
    impossible = error "responseInputEncoder: impossible constructor"

responseInputDecoder :: Decoder ResponseInput
responseInputDecoder = D.byType \case
    D.JsonString -> D.mapDecoder ResponseInputText D.text
    D.JsonArray -> D.mapDecoder ResponseInputItems (D.list responseItemDecoder)
    _ -> D.mapEither
        (const (Left "ResponseInput: expected string or array"))
        D.skip
