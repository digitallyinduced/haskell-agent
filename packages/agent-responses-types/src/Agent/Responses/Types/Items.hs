-- | Input and output items carried by Responses requests and responses.
module Agent.Responses.Types.Items
    ( ResponseInput(..)
    , responseInputDecoder
    , ResponseItem(..)
    , responseItemDecoder
    , ResponseItemType(..)
    , parseResponseItemType
    , responseItemTypeText
    , ResponseMessage(..)
    , FunctionCall(..)
    , FunctionCallOutput(..)
    , CustomToolCall(..)
    , CustomToolCallOutput(..)
    , ReasoningItem(..)
    , ReasoningSummaryPart(..)
    , ItemReference(..)
    , ResponseAgentMessage(..)
    , InternalChatMetadata(..)
    , AdditionalToolsItem(..)
    , LocalShellCall(..)
    , LocalShellAction(..)
    , ToolSearchCall(..)
    , ToolSearchOutput(..)
    , WebSearchCall(..)
    , WebSearchAction(..)
    , ImageGenerationCall(..)
    , CompactionItem(..)
    , CompactionTriggerItem(..)
    , ContextCompactionItem(..)
    ) where

import Agent.Responses.Types.Common
import Agent.Responses.Types.Content
import Agent.Responses.Types.Items.Known
import Data.Aeson hiding (TaggedObject)
import qualified Data.Aeson as Aeson
import qualified Data.Hermes as Hermes
import Data.Scientific (floatingOrInteger)
import Data.Text (Text)


data ResponseMessage = ResponseMessage
    { messageId   :: !(Maybe Text)
    , content     :: !MessageContent
    , role        :: !ResponseRole
    , status      :: !(Maybe ItemStatus)
    , phase       :: !(Maybe Text)
    , passthrough :: !(Maybe InternalChatMetadata)
    , extraFields :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON ResponseMessage where
    toJSON ResponseMessage
        { messageId, content, role, status, phase, passthrough, extraFields } =
            objectWith extraFields
                [ Just (field "type" ("message" :: Text))
                , optionalField "id" messageId
                , Just (field "content" content)
                , Just (field "role" role)
                , optionalField "status" status
                , optionalField "phase" phase
                , optionalField
                    "internal_chat_message_metadata_passthrough"
                    passthrough
                ]


data FunctionCall = FunctionCall
    { itemId                 :: !(Maybe Text)
    , callId                 :: !Text
    , name                   :: !Text
    , namespace              :: !(Maybe Text)
    , arguments              :: !Text
    , encryptedFunctionArgs  :: !(Maybe [Text])
    , status                 :: !(Maybe ItemStatus)
    , extraFields            :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON FunctionCall where
    toJSON FunctionCall
        { itemId, callId, name, namespace, arguments, encryptedFunctionArgs
        , status, extraFields } =
            objectWith extraFields
                [ Just (field "type" ("function_call" :: Text))
                , optionalField "id" itemId
                , Just (field "call_id" callId)
                , Just (field "name" name)
                , optionalField "namespace" namespace
                , Just (field "arguments" arguments)
                , optionalField "encrypted_function_args" encryptedFunctionArgs
                , optionalField "status" status
                ]


data FunctionCallOutput = FunctionCallOutput
    { itemId      :: !(Maybe Text)
    , callId      :: !Text
    , name        :: !(Maybe Text)
    , namespace   :: !(Maybe Text)
    , output      :: !Aeson.Value
    , status      :: !(Maybe ItemStatus)
    , extraFields :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON FunctionCallOutput where
    toJSON FunctionCallOutput
        { itemId, callId, name, namespace, output, status, extraFields } =
            objectWith extraFields
                [ Just (field "type" ("function_call_output" :: Text))
                , optionalField "id" itemId
                , Just (field "call_id" callId)
                , optionalField "name" name
                , optionalField "namespace" namespace
                , Just (field "output" output)
                , optionalField "status" status
                ]


data CustomToolCall = CustomToolCall
    { itemId      :: !(Maybe Text)
    , callId      :: !Text
    , name        :: !Text
    , namespace   :: !(Maybe Text)
    , input       :: !Text
    , status      :: !(Maybe ItemStatus)
    , extraFields :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON CustomToolCall where
    toJSON CustomToolCall
        { itemId, callId, name, namespace, input, status, extraFields } =
            objectWith extraFields
                [ Just (field "type" ("custom_tool_call" :: Text))
                , optionalField "id" itemId
                , Just (field "call_id" callId)
                , Just (field "name" name)
                , optionalField "namespace" namespace
                , Just (field "input" input)
                , optionalField "status" status
                ]


data CustomToolCallOutput = CustomToolCallOutput
    { itemId      :: !(Maybe Text)
    , callId      :: !Text
    , name        :: !(Maybe Text)
    , output      :: !Aeson.Value
    , status      :: !(Maybe ItemStatus)
    , extraFields :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON CustomToolCallOutput where
    toJSON CustomToolCallOutput
        { itemId, callId, name, output, status, extraFields } =
            objectWith extraFields
                [ Just (field "type" ("custom_tool_call_output" :: Text))
                , optionalField "id" itemId
                , Just (field "call_id" callId)
                , optionalField "name" name
                , Just (field "output" output)
                , optionalField "status" status
                ]


data ReasoningSummaryPart = ReasoningSummaryPart
    { partType    :: !Text
    , text        :: !(Maybe Text)
    , extraFields :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON ReasoningSummaryPart where
    toJSON ReasoningSummaryPart { partType, text, extraFields } =
        objectWith extraFields
            [Just (field "type" partType), optionalField "text" text]


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
    toJSON ReasoningItem
        { itemId, summary, content, encryptedContent, status, extraFields } =
            objectWith extraFields
                [ Just (field "type" ("reasoning" :: Text))
                , optionalField "id" itemId
                , Just (field "summary" summary)
                , optionalField "content" content
                , optionalField "encrypted_content" encryptedContent
                , optionalField "status" status
                ]


data ItemReference = ItemReference
    { itemId      :: !Text
    , extraFields :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON ItemReference where
    toJSON ItemReference { itemId, extraFields } =
        objectWith extraFields
            [ Just (field "type" ("item_reference" :: Text))
            , Just (field "id" itemId)
            ]


data ResponseAgentMessage = ResponseAgentMessage
    { messageId   :: !(Maybe Text)
    , author      :: !(Maybe Text)
    , recipient   :: !(Maybe Text)
    , content     :: ![ResponseContentPart]
    , passthrough :: !(Maybe InternalChatMetadata)
    , extraFields :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON ResponseAgentMessage where
    toJSON ResponseAgentMessage
        { messageId, author, recipient, content, passthrough, extraFields } =
            objectWith extraFields
                [ Just (field "type" ("agent_message" :: Text))
                , optionalField "id" messageId
                , optionalField "author" author
                , optionalField "recipient" recipient
                , Just (field "content" content)
                , optionalField
                    "internal_chat_message_metadata_passthrough"
                    passthrough
                ]


data AdditionalToolsItem = AdditionalToolsItem
    { itemId      :: !(Maybe Text)
    , role        :: !Text
    , tools       :: ![Aeson.Value]
    , extraFields :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON AdditionalToolsItem where
    toJSON AdditionalToolsItem { itemId, role, tools, extraFields } =
        objectWith extraFields
            [ Just (field "type" ("additional_tools" :: Text))
            , optionalField "id" itemId
            , Just (field "role" role)
            , Just (field "tools" tools)
            ]


data LocalShellAction
    = LocalShellExec
        { command           :: ![Text]
        , timeoutMs         :: !(Maybe Integer)
        , workingDirectory  :: !(Maybe Text)
        , env               :: !(Maybe Aeson.Object)
        , user              :: !(Maybe Text)
        , extraFields       :: !Aeson.Object
        }
    | LocalShellActionOther !TaggedObject
    deriving stock (Eq, Show)

instance ToJSON LocalShellAction where
    toJSON LocalShellExec
        { command, timeoutMs, workingDirectory, env, user, extraFields } =
            objectWith extraFields
                [ Just (field "type" ("exec" :: Text))
                , Just (field "command" command)
                , optionalField "timeout_ms" timeoutMs
                , optionalField "working_directory" workingDirectory
                , optionalField "env" env
                , optionalField "user" user
                ]
    toJSON (LocalShellActionOther tagged) = toJSON tagged


data LocalShellCall = LocalShellCall
    { itemId      :: !(Maybe Text)
    , callId      :: !(Maybe Text)
    , status      :: !(Maybe ItemStatus)
    , action      :: !(Maybe LocalShellAction)
    , extraFields :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON LocalShellCall where
    toJSON LocalShellCall { itemId, callId, status, action, extraFields } =
        objectWith extraFields
            [ Just (field "type" ("local_shell_call" :: Text))
            , optionalField "id" itemId
            , optionalField "call_id" callId
            , optionalField "status" status
            , optionalField "action" action
            ]


data ToolSearchCall = ToolSearchCall
    { itemId      :: !(Maybe Text)
    , callId      :: !(Maybe Text)
    , status      :: !(Maybe Text)
    , execution   :: !(Maybe Text)
    , arguments   :: !(Maybe Aeson.Value)
    , extraFields :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON ToolSearchCall where
    toJSON ToolSearchCall
        { itemId, callId, status, execution, arguments, extraFields } =
            objectWith extraFields
                [ Just (field "type" ("tool_search_call" :: Text))
                , optionalField "id" itemId
                , optionalField "call_id" callId
                , optionalField "status" status
                , optionalField "execution" execution
                , optionalField "arguments" arguments
                ]


data ToolSearchOutput = ToolSearchOutput
    { itemId      :: !(Maybe Text)
    , callId      :: !(Maybe Text)
    , status      :: !(Maybe Text)
    , execution   :: !(Maybe Text)
    , tools       :: ![Aeson.Value]
    , extraFields :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON ToolSearchOutput where
    toJSON ToolSearchOutput
        { itemId, callId, status, execution, tools, extraFields } =
            objectWith extraFields
                [ Just (field "type" ("tool_search_output" :: Text))
                , optionalField "id" itemId
                , optionalField "call_id" callId
                , optionalField "status" status
                , optionalField "execution" execution
                , Just (field "tools" tools)
                ]


data WebSearchAction
    = WebSearchQuery
        { query       :: !(Maybe Text)
        , queries     :: !(Maybe [Text])
        , extraFields :: !Aeson.Object
        }
    | WebSearchOpenPage
        { url         :: !(Maybe Text)
        , extraFields :: !Aeson.Object
        }
    | WebSearchFindInPage
        { url         :: !(Maybe Text)
        , pattern     :: !(Maybe Text)
        , extraFields :: !Aeson.Object
        }
    | WebSearchActionOther !TaggedObject
    deriving stock (Eq, Show)

instance ToJSON WebSearchAction where
    toJSON WebSearchQuery { query, queries, extraFields } =
        objectWith extraFields
            [ Just (field "type" ("search" :: Text))
            , optionalField "query" query
            , optionalField "queries" queries
            ]
    toJSON WebSearchOpenPage { url, extraFields } =
        objectWith extraFields
            [ Just (field "type" ("open_page" :: Text))
            , optionalField "url" url
            ]
    toJSON WebSearchFindInPage { url, pattern, extraFields } =
        objectWith extraFields
            [ Just (field "type" ("find_in_page" :: Text))
            , optionalField "url" url
            , optionalField "pattern" pattern
            ]
    toJSON (WebSearchActionOther tagged) = toJSON tagged


data WebSearchCall = WebSearchCall
    { itemId      :: !(Maybe Text)
    , status      :: !(Maybe Text)
    , action      :: !(Maybe WebSearchAction)
    , extraFields :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON WebSearchCall where
    toJSON WebSearchCall { itemId, status, action, extraFields } =
        objectWith extraFields
            [ Just (field "type" ("web_search_call" :: Text))
            , optionalField "id" itemId
            , optionalField "status" status
            , optionalField "action" action
            ]


data ImageGenerationCall = ImageGenerationCall
    { itemId         :: !(Maybe Text)
    , status         :: !(Maybe Text)
    , revisedPrompt  :: !(Maybe Text)
    , result         :: !(Maybe Text)
    , extraFields    :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON ImageGenerationCall where
    toJSON ImageGenerationCall
        { itemId, status, revisedPrompt, result, extraFields } =
            objectWith extraFields
                [ Just (field "type" ("image_generation_call" :: Text))
                , optionalField "id" itemId
                , optionalField "status" status
                , optionalField "revised_prompt" revisedPrompt
                , optionalField "result" result
                ]


data CompactionItem = CompactionItem
    { itemId            :: !(Maybe Text)
    , encryptedContent  :: !(Maybe Text)
    , extraFields       :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON CompactionItem where
    toJSON CompactionItem { itemId, encryptedContent, extraFields } =
        objectWith extraFields
            [ Just (field "type" ("compaction" :: Text))
            , optionalField "id" itemId
            , optionalField "encrypted_content" encryptedContent
            ]


data CompactionTriggerItem = CompactionTriggerItem
    { extraFields :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON CompactionTriggerItem where
    toJSON CompactionTriggerItem { extraFields } =
        objectWith extraFields
            [Just (field "type" ("compaction_trigger" :: Text))]


data ContextCompactionItem = ContextCompactionItem
    { itemId           :: !(Maybe Text)
    , encryptedContent :: !(Maybe Text)
    , extraFields      :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON ContextCompactionItem where
    toJSON ContextCompactionItem { itemId, encryptedContent, extraFields } =
        objectWith extraFields
            [ Just (field "type" ("context_compaction" :: Text))
            , optionalField "id" itemId
            , optionalField "encrypted_content" encryptedContent
            ]


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

instance ToJSON ResponseItem where
    toJSON = \case
        MessageItem value -> toJSON value
        FunctionCallItem value -> toJSON value
        FunctionCallOutputItem value -> toJSON value
        CustomToolCallItem value -> toJSON value
        CustomToolCallOutputItem value -> toJSON value
        ReasoningItemValue value -> toJSON value
        ItemReferenceValue value -> toJSON value
        AgentMessageItem value -> toJSON value
        AdditionalToolsItemValue value -> toJSON value
        LocalShellCallItem value -> toJSON value
        ToolSearchCallItem value -> toJSON value
        ToolSearchOutputItem value -> toJSON value
        WebSearchCallItem value -> toJSON value
        ImageGenerationCallItem value -> toJSON value
        CompactionItemValue value -> toJSON value
        CompactionTriggerItemValue value -> toJSON value
        ContextCompactionItemValue value -> toJSON value
        KnownResponseItem _ value -> toJSON value
        UnknownResponseItem value -> toJSON value


data ResponseInput
    = ResponseInputText !Text
    | ResponseInputItems ![ResponseItem]
    deriving stock (Eq, Show)

instance ToJSON ResponseInput where
    toJSON (ResponseInputText value) = Aeson.String value
    toJSON (ResponseInputItems values) = toJSON values


responseInputDecoder :: Hermes.Decoder ResponseInput
responseInputDecoder =
    Hermes.getType >>= \case
        Hermes.VString -> ResponseInputText <$> Hermes.text
        Hermes.VArray -> ResponseInputItems <$> Hermes.list responseItemDecoder
        _ -> fail "ResponseInput: expected string or array"

responseItemDecoder :: Hermes.Decoder ResponseItem
responseItemDecoder =
    Hermes.object do
        wireType <- Hermes.atKey "type" Hermes.text
        Hermes.liftObjectDecoder $ case parseResponseItemType wireType of
            ItemMessage -> MessageItem <$> responseMessageDecoder
            ItemFunctionCall -> FunctionCallItem <$> functionCallDecoder
            ItemFunctionCallOutput ->
                FunctionCallOutputItem <$> functionCallOutputDecoder
            ItemCustomToolCall -> CustomToolCallItem <$> customToolCallDecoder
            ItemCustomToolCallOutput ->
                CustomToolCallOutputItem <$> customToolCallOutputDecoder
            ItemReasoning -> ReasoningItemValue <$> reasoningItemDecoder
            ItemReferenceType -> ItemReferenceValue <$> itemReferenceDecoder
            ItemAgentMessage ->
                AgentMessageItem <$> responseAgentMessageDecoder
            ItemAdditionalTools ->
                AdditionalToolsItemValue <$> additionalToolsItemDecoder
            ItemLocalShellCall -> LocalShellCallItem <$> localShellCallDecoder
            ItemToolSearchCall -> ToolSearchCallItem <$> toolSearchCallDecoder
            ItemToolSearchOutput ->
                ToolSearchOutputItem <$> toolSearchOutputDecoder
            ItemWebSearchCall -> WebSearchCallItem <$> webSearchCallDecoder
            ItemImageGenerationCall ->
                ImageGenerationCallItem <$> imageGenerationCallDecoder
            ItemCompaction -> CompactionItemValue <$> compactionItemDecoder
            ItemCompactionTrigger ->
                CompactionTriggerItemValue <$> compactionTriggerItemDecoder
            ItemContextCompaction ->
                ContextCompactionItemValue <$> contextCompactionItemDecoder
            ItemUnknownType{} ->
                pure (UnknownResponseItem (TaggedObject wireType mempty))
            itemType ->
                pure (KnownResponseItem itemType (TaggedObject wireType mempty))

responseMessageDecoder :: Hermes.Decoder ResponseMessage
responseMessageDecoder = Hermes.object $
    ResponseMessage
        <$> optionalAtKey "id" Hermes.text
        <*> Hermes.atKey "content" messageContentDecoder
        <*> Hermes.atKey "role" responseRoleDecoder
        <*> optionalAtKey "status" itemStatusDecoder
        <*> optionalAtKey "phase" Hermes.text
        <*> optionalAtKey
            "internal_chat_message_metadata_passthrough"
            internalChatMetadataDecoder
        <*> pure mempty

functionCallDecoder :: Hermes.Decoder FunctionCall
functionCallDecoder = Hermes.object $
    FunctionCall
        <$> optionalAtKey "id" Hermes.text
        <*> Hermes.atKey "call_id" Hermes.text
        <*> Hermes.atKey "name" Hermes.text
        <*> optionalAtKey "namespace" Hermes.text
        <*> Hermes.atKey "arguments" Hermes.text
        <*> optionalAtKey "encrypted_function_args" (Hermes.list Hermes.text)
        <*> optionalAtKey "status" itemStatusDecoder
        <*> pure mempty

functionCallOutputDecoder :: Hermes.Decoder FunctionCallOutput
functionCallOutputDecoder = Hermes.object $
    FunctionCallOutput
        <$> optionalAtKey "id" Hermes.text
        <*> Hermes.atKey "call_id" Hermes.text
        <*> optionalAtKey "name" Hermes.text
        <*> optionalAtKey "namespace" Hermes.text
        <*> Hermes.atKey "output" aesonValueDecoder
        <*> optionalAtKey "status" itemStatusDecoder
        <*> pure mempty

customToolCallDecoder :: Hermes.Decoder CustomToolCall
customToolCallDecoder = Hermes.object $
    CustomToolCall
        <$> optionalAtKey "id" Hermes.text
        <*> Hermes.atKey "call_id" Hermes.text
        <*> Hermes.atKey "name" Hermes.text
        <*> optionalAtKey "namespace" Hermes.text
        <*> Hermes.atKey "input" Hermes.text
        <*> optionalAtKey "status" itemStatusDecoder
        <*> pure mempty

customToolCallOutputDecoder :: Hermes.Decoder CustomToolCallOutput
customToolCallOutputDecoder = Hermes.object $
    CustomToolCallOutput
        <$> optionalAtKey "id" Hermes.text
        <*> Hermes.atKey "call_id" Hermes.text
        <*> optionalAtKey "name" Hermes.text
        <*> Hermes.atKey "output" aesonValueDecoder
        <*> optionalAtKey "status" itemStatusDecoder
        <*> pure mempty

reasoningSummaryPartDecoder :: Hermes.Decoder ReasoningSummaryPart
reasoningSummaryPartDecoder = Hermes.object $
    ReasoningSummaryPart
        <$> Hermes.atKey "type" Hermes.text
        <*> optionalAtKey "text" Hermes.text
        <*> pure mempty

reasoningItemDecoder :: Hermes.Decoder ReasoningItem
reasoningItemDecoder = Hermes.object $
    ReasoningItem
        <$> optionalAtKey "id" Hermes.text
        <*> (maybe [] id
            <$> optionalAtKey
                "summary"
                (Hermes.list reasoningSummaryPartDecoder))
        <*> optionalAtKey
            "content"
            (Hermes.list responseContentPartDecoder)
        <*> optionalAtKey "encrypted_content" Hermes.text
        <*> optionalAtKey "status" itemStatusDecoder
        <*> pure mempty

itemReferenceDecoder :: Hermes.Decoder ItemReference
itemReferenceDecoder = Hermes.object $
    ItemReference
        <$> Hermes.atKey "id" Hermes.text
        <*> pure mempty

responseAgentMessageDecoder :: Hermes.Decoder ResponseAgentMessage
responseAgentMessageDecoder = Hermes.object $
    ResponseAgentMessage
        <$> optionalAtKey "id" Hermes.text
        <*> optionalAtKey "author" Hermes.text
        <*> optionalAtKey "recipient" Hermes.text
        <*> (maybe [] id <$> optionalAtKey
            "content"
            (Hermes.list responseContentPartDecoder))
        <*> optionalAtKey
            "internal_chat_message_metadata_passthrough"
            internalChatMetadataDecoder
        <*> pure mempty

additionalToolsItemDecoder :: Hermes.Decoder AdditionalToolsItem
additionalToolsItemDecoder = Hermes.object $
    AdditionalToolsItem
        <$> optionalAtKey "id" Hermes.text
        <*> (maybe "developer" id <$> optionalAtKey "role" Hermes.text)
        <*> (maybe [] id <$> optionalAtKey
            "tools"
            (Hermes.list aesonValueDecoder))
        <*> pure mempty

localShellActionDecoder :: Hermes.Decoder LocalShellAction
localShellActionDecoder =
    Hermes.object do
        wireType <- optionalAtKey "type" Hermes.text
        case wireType of
            Just "exec" -> LocalShellExec
                <$> (maybe [] id <$> optionalAtKey
                    "command"
                    (Hermes.list Hermes.text))
                <*> optionalAtKey "timeout_ms" integerDecoder
                <*> optionalAtKey "working_directory" Hermes.text
                <*> optionalAtKey "env" aesonObjectDecoder
                <*> optionalAtKey "user" Hermes.text
                <*> pure mempty
            _ -> pure $
                LocalShellActionOther
                    (TaggedObject (maybe "" id wireType) mempty)

localShellCallDecoder :: Hermes.Decoder LocalShellCall
localShellCallDecoder = Hermes.object $
    LocalShellCall
        <$> optionalAtKey "id" Hermes.text
        <*> optionalAtKey "call_id" Hermes.text
        <*> optionalAtKey "status" itemStatusDecoder
        <*> optionalAtKey "action" localShellActionDecoder
        <*> pure mempty

toolSearchCallDecoder :: Hermes.Decoder ToolSearchCall
toolSearchCallDecoder = Hermes.object $
    ToolSearchCall
        <$> optionalAtKey "id" Hermes.text
        <*> optionalAtKey "call_id" Hermes.text
        <*> optionalAtKey "status" Hermes.text
        <*> optionalAtKey "execution" Hermes.text
        <*> optionalAtKey "arguments" aesonValueDecoder
        <*> pure mempty

toolSearchOutputDecoder :: Hermes.Decoder ToolSearchOutput
toolSearchOutputDecoder = Hermes.object $
    ToolSearchOutput
        <$> optionalAtKey "id" Hermes.text
        <*> optionalAtKey "call_id" Hermes.text
        <*> optionalAtKey "status" Hermes.text
        <*> optionalAtKey "execution" Hermes.text
        <*> (maybe [] id <$> optionalAtKey
            "tools"
            (Hermes.list aesonValueDecoder))
        <*> pure mempty

webSearchActionDecoder :: Hermes.Decoder WebSearchAction
webSearchActionDecoder =
    Hermes.object do
        wireType <- optionalAtKey "type" Hermes.text
        Hermes.liftObjectDecoder $ Hermes.object $ case wireType of
            Just "search" -> WebSearchQuery
                <$> optionalAtKey "query" Hermes.text
                <*> optionalAtKey "queries" (Hermes.list Hermes.text)
                <*> pure mempty
            Just "open_page" -> WebSearchOpenPage
                <$> optionalAtKey "url" Hermes.text
                <*> pure mempty
            Just "find_in_page" -> WebSearchFindInPage
                <$> optionalAtKey "url" Hermes.text
                <*> optionalAtKey "pattern" Hermes.text
                <*> pure mempty
            _ -> pure $
                WebSearchActionOther
                    (TaggedObject (maybe "" id wireType) mempty)

webSearchCallDecoder :: Hermes.Decoder WebSearchCall
webSearchCallDecoder = Hermes.object $
    WebSearchCall
        <$> optionalAtKey "id" Hermes.text
        <*> optionalAtKey "status" Hermes.text
        <*> optionalAtKey "action" webSearchActionDecoder
        <*> pure mempty

imageGenerationCallDecoder :: Hermes.Decoder ImageGenerationCall
imageGenerationCallDecoder = Hermes.object $
    ImageGenerationCall
        <$> optionalAtKey "id" Hermes.text
        <*> optionalAtKey "status" Hermes.text
        <*> optionalAtKey "revised_prompt" Hermes.text
        <*> optionalAtKey "result" Hermes.text
        <*> pure mempty

compactionItemDecoder :: Hermes.Decoder CompactionItem
compactionItemDecoder = Hermes.object $
    CompactionItem
        <$> optionalAtKey "id" Hermes.text
        <*> optionalAtKey "encrypted_content" Hermes.text
        <*> pure mempty

compactionTriggerItemDecoder :: Hermes.Decoder CompactionTriggerItem
compactionTriggerItemDecoder =
    CompactionTriggerItem mempty <$ Hermes.object (pure ())

contextCompactionItemDecoder :: Hermes.Decoder ContextCompactionItem
contextCompactionItemDecoder = Hermes.object $
    ContextCompactionItem
        <$> optionalAtKey "id" Hermes.text
        <*> optionalAtKey "encrypted_content" Hermes.text
        <*> pure mempty

integerDecoder :: Hermes.Decoder Integer
integerDecoder = do
    value <- Hermes.scientific
    case floatingOrInteger value of
        Right integer -> pure integer
        Left (_ :: Double) -> fail "expected an integer"
