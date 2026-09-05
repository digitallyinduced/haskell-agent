-- | Coalescing and presentation of consecutive read/list/search tool calls.
module Agent.TUI.Model.Inspection
    ( startInspectionCall
    , renderInspectionGroup
    , isGroupableInspectionTool
    ) where

import Agent.TUI.Model.Block (appendBlock, closeInspectionGroups)
import Agent.TUI.Model.Types
import Agent.TUI.Presentation
    ( formatToolDiffRelative
    , toolCallHeaderRelative
    , toolCallTitleRelative
    )
import Agent.ToolDispatch (ToolCall(..), canonicalToolName)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text

startInspectionCall :: ToolCall -> UiState -> UiState
startInspectionCall call state =
    case Seq.viewr state.uiBlocks of
        _ Seq.:> block
            | block.blockKind == BlockInspect
            , Just group <- Map.lookup block.blockId state.uiInspectionGroups
            , group.inspectionGroupOpen ->
                extend block group
        _ -> start
  where
    activity = toolCallTitleRelative state.uiWorkspaceRoot call
    (title, headerDetail) =
        toolCallHeaderRelative state.uiWorkspaceRoot call
    detail = fromMaybe "" headerDetail
    item = InspectionItem
        { inspectionCallId = call.callId
        , inspectionToolName = canonicalToolName call.name
        , inspectionTitle = title
        , inspectionDetail = detail
        , inspectionBody =
            formatToolDiffRelative state.uiWorkspaceRoot call
        , inspectionState = BlockRunning
        }
    common current =
        current
            { uiRunning = True
            , uiGenerating = False
            , uiAwaitingInput = False
            , uiActivity = activity
            }
    extend block group =
        let
            blockIndex = Seq.length state.uiBlocks - 1
            updatedGroup =
                group
                    { inspectionGroupItems =
                        group.inspectionGroupItems <> [item]
                    }
        in common state
            { uiBlocks =
                Seq.adjust
                    (renderInspectionGroup updatedGroup)
                    blockIndex
                    state.uiBlocks
            , uiInspectionGroups =
                Map.insert
                    block.blockId
                    updatedGroup
                    state.uiInspectionGroups
            , uiToolCalls =
                Map.insert
                    call.callId
                    (blockIndex, call)
                    state.uiToolCalls
            }
    start =
        let
            prepared = closeInspectionGroups state
            blockIndex = Seq.length prepared.uiBlocks
            ident = BlockId prepared.uiNextBlockId
            group = InspectionGroup
                { inspectionGroupOpen = True
                , inspectionGroupItems = [item]
                }
            appended =
                appendBlock
                    BlockInspect
                    title
                    item.inspectionBody
                    detail
                    BlockRunning
                    (Just call.callId)
                    (common prepared)
        in appended
            { uiBlocks =
                Seq.adjust
                    (\block ->
                        block { blockInspectionGroupable = True })
                    blockIndex
                    appended.uiBlocks
            , uiInspectionGroups =
                Map.insert ident group appended.uiInspectionGroups
            , uiToolCalls =
                Map.insert
                    call.callId
                    (blockIndex, call)
                    appended.uiToolCalls
            }

renderInspectionGroup :: InspectionGroup -> UiBlock -> UiBlock
renderInspectionGroup group block =
    block
        { blockTitle = inspectionGroupTitle items
        , blockBody = inspectionGroupBody items
        , blockState = inspectionGroupState items
        , blockDetail = inspectionGroupDetail items
        , blockCallId = (.inspectionCallId) <$> listToMaybe items
        , blockInspectionGroupable =
            block.blockInspectionGroupable && length items == 1
        }
  where
    items = group.inspectionGroupItems

inspectionGroupTitle :: [InspectionItem] -> Text
inspectionGroupTitle [] = "Inspected"
inspectionGroupTitle [item] = item.inspectionTitle
inspectionGroupTitle items =
    Text.intercalate ", " (inspectionSummaries items)
        <> statusSuffix
  where
    failed =
        length
            (filter
                ((== BlockFailed) . (.inspectionState))
                items)
    denied =
        length
            (filter
                ((== BlockDenied) . (.inspectionState))
                items)
    suffixes =
        [ Text.pack (show failed) <> " failed" | failed > 0 ]
            <> [ Text.pack (show denied) <> " denied" | denied > 0 ]
    statusSuffix =
        case suffixes of
            [] -> ""
            _ -> " · " <> Text.intercalate ", " suffixes

inspectionGroupDetail :: [InspectionItem] -> Text
inspectionGroupDetail [item] = item.inspectionDetail
inspectionGroupDetail _ = ""

inspectionSummaries :: [InspectionItem] -> [Text]
inspectionSummaries =
    map render . foldl add []
  where
    add counts item =
        let label = inspectionSummaryLabel item.inspectionToolName
        in case break ((== label) . fst) counts of
            (before, (_, count) : after) ->
                before <> [(label, count + 1)] <> after
            _ -> counts <> [(label, 1)]
    render :: (Text, Int) -> Text
    render (label, count) =
        label
            <> " "
            <> Text.pack (show count)
            <> if count == 1 then " item" else " items"

inspectionSummaryLabel :: Text -> Text
inspectionSummaryLabel name
    | name `elem` ["read_file", "read_tool_output", "mcp_read_resource"] =
        "Read"
    | name
        `elem` ["list_dir", "mcp_list_resources", "list_agents", "ListAgents", "Glob"] =
        "Listed"
    | name
        `elem`
            [ "grep"
            , "search_tool_output"
            , "mcp_search"
            , "search_tool"
            , "conversation_search"
            , "skill_search"
            , "WebSearch"
            , "ToolSearch"
            ] =
        "Searched"
    | otherwise = "Inspected"

-- Image rendering and long-running task-output polling attach lifecycle data
-- to one exact block, so only compact read/list/search calls join a burst.
isGroupableInspectionTool :: Text -> Bool
isGroupableInspectionTool rawName =
    canonicalToolName rawName
        `elem`
            [ "read_file"
            , "list_dir"
            , "grep"
            , "read_tool_output"
            , "search_tool_output"
            , "mcp_search"
            , "search_tool"
            , "mcp_list_resources"
            , "mcp_read_resource"
            , "conversation_search"
            , "skill_search"
            , "view_skill"
            , "list_agents"
            , "Glob"
            , "WebSearch"
            , "ToolSearch"
            , "ListAgents"
            ]

inspectionGroupBody :: [InspectionItem] -> Text
inspectionGroupBody [item] = item.inspectionBody
inspectionGroupBody items =
    Text.intercalate "\n" headers
        <> if null details
            then ""
            else "\n\n" <> Text.intercalate "\n\n" details
  where
    headers = map inspectionItemHeader items
    details =
        [ inspectionItemTitle item <> "\n"
            <> Text.unlines
                (map ("    " <>) (Text.lines item.inspectionBody))
        | item <- items
        , not (Text.null (Text.strip item.inspectionBody))
        ]

inspectionItemHeader :: InspectionItem -> Text
inspectionItemHeader item =
    "  "
        <> inspectionStateGlyph item.inspectionState
        <> " "
        <> inspectionItemTitle item

inspectionItemTitle :: InspectionItem -> Text
inspectionItemTitle item
    | Text.null item.inspectionDetail = item.inspectionTitle
    | otherwise = item.inspectionTitle <> " " <> item.inspectionDetail

inspectionStateGlyph :: BlockState -> Text
inspectionStateGlyph = \case
    BlockComplete -> "◇"
    BlockFailed -> "✗"
    BlockCancelled -> "⊘"
    BlockDenied -> "⊘"
    BlockStreaming -> "◆"
    BlockRunning -> "◆"

inspectionGroupState :: [InspectionItem] -> BlockState
inspectionGroupState items
    | any ((== BlockRunning) . (.inspectionState)) items = BlockRunning
    | any ((== BlockFailed) . (.inspectionState)) items = BlockFailed
    | any ((== BlockDenied) . (.inspectionState)) items = BlockDenied
    | any ((== BlockCancelled) . (.inspectionState)) items = BlockCancelled
    | otherwise = BlockComplete
