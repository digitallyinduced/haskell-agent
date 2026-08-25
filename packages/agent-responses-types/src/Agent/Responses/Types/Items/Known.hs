{-# LANGUAGE LambdaCase #-}

module Agent.Responses.Types.Items.Known
    ( ResponseItemType(..)
    , parseResponseItemType
    , responseItemTypeText
    , InternalChatMetadata(..)
    ) where

import Agent.Responses.Types.Common
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
    | ItemContextCompaction
    | ItemAdditionalTools
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
    ItemContextCompaction -> "context_compaction"
    ItemAdditionalTools -> "additional_tools"
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
    "context_compaction" -> ItemContextCompaction
    "additional_tools" -> ItemAdditionalTools
    "item_reference" -> ItemReferenceType
    "program" -> ItemProgram
    "program_output" -> ItemProgramOutput
    other -> ItemUnknownType other

data InternalChatMetadata = InternalChatMetadata
    { turnId            :: !(Maybe Text)
    , createTime        :: !(Maybe Aeson.Value)
    , contentItemKinds  :: !(Maybe [Text])
    , executedToolCalls :: !(Maybe Aeson.Value)
    , extraFields       :: !Aeson.Object
    }
    deriving stock (Eq, Show)

instance ToJSON InternalChatMetadata where
    toJSON InternalChatMetadata
        { turnId, createTime, contentItemKinds, executedToolCalls
        , extraFields } =
            objectWith extraFields
                [ optionalField "turn_id" turnId
                , optionalField "create_time" createTime
                , optionalField "content_item_kinds" contentItemKinds
                , optionalField "executed_tool_calls" executedToolCalls
                ]

instance FromJSON InternalChatMetadata where
    parseJSON = withObject "InternalChatMetadata" $ \o ->
        InternalChatMetadata
            <$> o .:? "turn_id"
            <*> o .:? "create_time"
            <*> o .:? "content_item_kinds"
            <*> o .:? "executed_tool_calls"
            <*> pure
                (without
                    [ "turn_id", "create_time", "content_item_kinds"
                    , "executed_tool_calls"
                    ]
                    o)
