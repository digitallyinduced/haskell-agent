{-# LANGUAGE LambdaCase #-}

module Agent.Responses.Types.Items.Known
    ( ResponseItemType(..)
    , parseResponseItemType
    , responseItemTypeText
    , responseItemTypeEncoder
    , responseItemTypeDecoder
    , InternalChatMetadata(..)
    , internalChatMetadataEncoder
    , internalChatMetadataDecoder
    ) where

import Agent.Json
    ( Extensions
    , RawJson
    , emptyExtensions
    , insertExtension
    )
import Agent.Json.Decoder (Decoder)
import qualified Agent.Json.Decoder as D
import Agent.Json.Encoder (Encoder)
import qualified Agent.Json.Encoder as E
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

responseItemTypeEncoder :: Encoder ResponseItemType
responseItemTypeEncoder = E.contramap responseItemTypeText E.text

responseItemTypeDecoder :: Decoder ResponseItemType
responseItemTypeDecoder = D.mapDecoder parseResponseItemType D.text

data InternalChatMetadata = InternalChatMetadata
    { turnId            :: !(Maybe Text)
    , createTime        :: !(Maybe RawJson)
    , contentItemKinds  :: !(Maybe [Text])
    , executedToolCalls :: !(Maybe RawJson)
    , extraFields       :: !Extensions
    }
    deriving stock (Eq, Show)

internalChatMetadataEncoder :: Encoder InternalChatMetadata
internalChatMetadataEncoder = E.objectWithExtensions (.extraFields)
    [ E.optionalField "turn_id" E.text (.turnId)
    , E.optionalField "create_time" E.rawJson (.createTime)
    , E.optionalField "content_item_kinds" (E.list E.text) (.contentItemKinds)
    , E.optionalField "executed_tool_calls" E.rawJson (.executedToolCalls)
    ]

data InternalChatMetadataState = InternalChatMetadataState
    { stateTurnId :: !(Maybe Text)
    , stateCreateTime :: !(Maybe RawJson)
    , stateContentItemKinds :: !(Maybe [Text])
    , stateExecutedToolCalls :: !(Maybe RawJson)
    , stateExtraFields :: !Extensions
    }

internalChatMetadataDecoder :: Decoder InternalChatMetadata
internalChatMetadataDecoder =
    D.object
        (InternalChatMetadataState Nothing Nothing Nothing Nothing emptyExtensions)
        [ D.field "turn_id" (D.nullable D.text) \v s ->
            Right s { stateTurnId = v }
        , D.field "create_time" (D.nullable D.rawJson) \v s ->
            Right s { stateCreateTime = v }
        , D.field "content_item_kinds" (D.nullable (D.list D.text)) \v s ->
            Right s { stateContentItemKinds = v }
        , D.field "executed_tool_calls" (D.nullable D.rawJson) \v s ->
            Right s { stateExecutedToolCalls = v }
        ]
        (D.unknownField D.rawJson \key value s ->
            Right s { stateExtraFields =
                insertExtension key value s.stateExtraFields })
        \state -> Right InternalChatMetadata
            { turnId = state.stateTurnId
            , createTime = state.stateCreateTime
            , contentItemKinds = state.stateContentItemKinds
            , executedToolCalls = state.stateExecutedToolCalls
            , extraFields = state.stateExtraFields
            }
