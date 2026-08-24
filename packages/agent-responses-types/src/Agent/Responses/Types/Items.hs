-- | Input and output items carried by Responses requests and responses.
module Agent.Responses.Types.Items
    ( ResponseInput(..)
    , ResponseItem(..)
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
    ) where

import Agent.Responses.Types.Common
import Agent.Responses.Types.Content
import Data.Aeson hiding (TaggedObject)
import qualified Data.Aeson as Aeson
import Data.Text (Text)

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

data ResponseMessage = ResponseMessage
    { messageId   :: !(Maybe Text)
    , content     :: !MessageContent
    , role        :: !ResponseRole
    , status      :: !(Maybe ItemStatus)
    , phase       :: !(Maybe Text)
    , extraFields :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON ResponseMessage where
    toJSON ResponseMessage
        { messageId, content, role, status, phase, extraFields } =
            objectWith extraFields
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
        <*> pure
            (without ["type", "id", "content", "role", "status", "phase"] o)

data FunctionCall = FunctionCall
    { itemId      :: !(Maybe Text)
    , callId      :: !Text
    , name        :: !Text
    , arguments   :: !Text
    , status      :: !(Maybe ItemStatus)
    , extraFields :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON FunctionCall where
    toJSON FunctionCall
        { itemId, callId, name, arguments, status, extraFields } =
            objectWith extraFields
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
        <*> pure
            (without ["type", "id", "call_id", "name", "arguments", "status"] o)

data FunctionCallOutput = FunctionCallOutput
    { itemId      :: !(Maybe Text)
    , callId      :: !Text
    , output      :: !Aeson.Value
    , status      :: !(Maybe ItemStatus)
    , extraFields :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON FunctionCallOutput where
    toJSON FunctionCallOutput
        { itemId, callId, output, status, extraFields } =
            objectWith extraFields
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
    toJSON CustomToolCall
        { itemId, callId, name, input, status, extraFields } =
            objectWith extraFields
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

instance FromJSON CustomToolCallOutput where
    parseJSON = withObject "CustomToolCallOutput" $ \o ->
        CustomToolCallOutput
            <$> o .:? "id"
            <*> o .: "call_id"
            <*> o .:? "name"
            <*> o .: "output"
            <*> o .:? "status"
            <*> pure
                (without
                    ["type", "id", "call_id", "name", "output", "status"] o)

data ReasoningSummaryPart = ReasoningSummaryPart
    { partType    :: !Text
    , text        :: !(Maybe Text)
    , extraFields :: !Aeson.Object
    } deriving stock (Eq, Show)

instance ToJSON ReasoningSummaryPart where
    toJSON ReasoningSummaryPart { partType, text, extraFields } =
        objectWith extraFields
            [Just (field "type" partType), optionalField "text" text]

instance FromJSON ReasoningSummaryPart where
    parseJSON = withObject "ReasoningSummaryPart" $ \o ->
        ReasoningSummaryPart
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

instance FromJSON ReasoningItem where
    parseJSON = withObject "ReasoningItem" $ \o -> ReasoningItem
        <$> o .:? "id"
        <*> o .:? "summary" .!= []
        <*> o .:? "content"
        <*> o .:? "encrypted_content"
        <*> o .:? "status"
        <*> pure
            (without
                [ "type", "id", "summary", "content", "encrypted_content"
                , "status"
                ]
                o)

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
            ItemFunctionCallOutput ->
                FunctionCallOutputItem <$> parseJSON value
            ItemCustomToolCall -> CustomToolCallItem <$> parseJSON value
            ItemCustomToolCallOutput ->
                CustomToolCallOutputItem <$> parseJSON value
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
    parseJSON value =
        fail ("ResponseInput: expected string or array, got " <> show value)
