-- | Renderer-independent state for the retained terminal UI.
module Agent.TUI.Model
    ( BlockId(..)
    , BlockKind(..)
    , BlockState(..)
    , Focus(..)
    , NoticeKind(..)
    , RetryCountdown(..)
    , PermissionOverlay(..)
    , PromptLimitStatus(..)
    , PromptState(..)
    , InspectionGroup(..)
    , InspectionItem(..)
    , UiBlock(..)
    , UiEvent(..)
    , UiNotice(..)
    , UiState(..)
    , errorNotice
    , infoNotice
    , initialUiState
    , conversationIsEmpty
    , deleteToLineStart
    , deleteToLineEnd
    , deleteWordAfter
    , deleteWordBefore
    , lineEndCursor
    , lineStartCursor
    , moveWordLeft
    , moveWordRight
    , reduceUi
    , lookupBlock
    , selectedBlockIndex
    , timestampNewMessageBlocks
    , progressNotice
    , successNotice
    , visibleTodoList
    , uiNextDeadlineMillis
    , uiNeedsTick
    , uiTokensPerSecond
    , uiTokensPerSecondEstimated
    , warningNotice
    , advanceUiTime
    , blockCodeLanguage
    ) where

import Agent.TUI.Presentation
    ( formatToolDiffRelative
    , formatToolDiffRelativeWithOutput
    , formatToolOutputRelative
    , isInspectionTool
    , todoListFromToolArguments
    , todoListFromToolOutput
    , toolCallHeaderRelative
    , toolCallInput
    , toolCallTitleRelative
    )
import Agent.TUI.TextWidth (clampGraphemeCursor)
import Agent.TUI.Model.Edit
import Agent.TUI.Model.State
import Agent.TUI.Model.Timing
import Agent.TUI.Model.Types
import Agent.TUI.Motion
    ( completionStatusDurationMillis )
import qualified Agent.Json.Decode as Hermes
import Agent.Loop
    ( LoopEvent(..)
    , TokenUsage(..)
    , TurnOutput(..)
    , generationTokensPerSecond
    )
import Agent.Telemetry (telemetrySummary)
import Agent.ToolDispatch
    ( ToolOutcome(..)
    , toolCallResultOutcome
    , ToolCall(..)
    , ToolCallResult(..)
    , canonicalToolName
    )
import Control.Applicative ((<|>))
import Control.Monad (guard)
import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Char (isSpace)

reduceUi :: UiEvent -> UiState -> UiState
reduceUi event state = case event of
    UiLoop loopEvent -> reduceLoop loopEvent state
    UiUserSubmitted text ->
        appendBlock BlockUser "You" text "" BlockComplete Nothing
            state
                { uiAwaitingInput = False
                , uiFollow = True
                , uiNotice = Nothing
                , uiNoticeElapsedMillis = 0
                }
    UiDraftSubmitted ->
        state
            { uiDraft = ""
            , uiCursor = 0
            , uiAwaitingInput = False
            , uiNotice = Nothing
            , uiNoticeElapsedMillis = 0
            }
    UiInputSteered text ->
        appendBlock BlockUser "You" text "" BlockComplete Nothing
            state
                { uiDraft = ""
                , uiCursor = 0
                , uiFollow = True
                , uiNotice =
                    Just (progressNotice "Steering the current turn…")
                , uiNoticeElapsedMillis = 0
                }
    UiInputQueued text ->
        state
            { uiDraft = ""
            , uiCursor = 0
            , uiQueuedInputs = state.uiQueuedInputs Seq.|> text
            , uiNotice = Nothing
            , uiNoticeElapsedMillis = 0
            }
    UiInputPromoted text ->
        state
            { uiDraft = ""
            , uiCursor = 0
            , uiQueuedInputs = text Seq.<| state.uiQueuedInputs
            , uiNotice =
                Just $
                    warningNotice
                        "Cancelling the current turn; sending this prompt next…"
            , uiNoticeElapsedMillis = 0
            }
    UiQueuedInputStarted ->
        state
            { uiQueuedInputs = Seq.drop 1 state.uiQueuedInputs
            , uiAwaitingInput = False
            , uiNotice = Nothing
            , uiNoticeElapsedMillis = 0
            }
    UiSetDraft text cursor ->
        state
            { uiDraft = text
            , uiCursor = clampGraphemeCursor text cursor
            }
    UiSetPrompt prompt ->
        state { uiPrompt = prompt }
    UiSetPromptTarget model account ->
        state
            { uiPrompt =
                state.uiPrompt
                    { promptModel = model
                    , promptAccount = account
                    }
            }
    UiSetPromptEffort effort ->
        state
            { uiPrompt = state.uiPrompt { promptEffort = effort }
            }
    UiSetPromptLimitStatus limitStatus ->
        state
            { uiPrompt =
                state.uiPrompt { promptLimitStatus = limitStatus }
            }
    UiSetContextUsage tokens contextWindow ->
        state
            { uiContextTokens = tokens
            , uiContextWindow = contextWindow
            }
    UiSetAwaitingInput awaiting -> setAwaitingInput awaiting state
    UiSetRepository branch cwd workspace ->
        state { uiBranch = branch, uiCwd = cwd, uiWorkspaceRoot = workspace }
    UiSetNotice notice ->
        state { uiNotice = notice, uiNoticeElapsedMillis = 0 }
    UiMoveSelection delta ->
        moveSelection delta state
    UiSelectBlock ident ->
        selectBlock ident state
    UiActivateBlock ident ->
        toggleSelected (selectBlock ident state)
    UiToggleSelected ->
        toggleSelected state
    UiFocusChanged focus ->
        state { uiFocus = focus }
    UiPermissionShown summary ->
        state
            { uiPermission = Just PermissionOverlay
                { permissionSummary = summary
                , permissionIndex = 0
                }
            , uiFocus = FocusPermission
            }
    UiPermissionMoved delta ->
        state
            { uiPermission =
                (\permission ->
                    permission
                        { permissionIndex =
                            (permission.permissionIndex + delta) `mod` 4
                        })
                    <$> state.uiPermission
            }
    UiPermissionHidden ->
        state { uiPermission = Nothing, uiFocus = FocusComposer }
    UiHistory history
        | Text.null (Text.strip history) -> state
        | otherwise ->
            appendBlock BlockSystem "Previous conversation" history ""
                BlockComplete Nothing state
    UiAssistantHistory message
        | Text.null (Text.strip message) -> state
        | otherwise ->
            appendBlock BlockAssistant "Assistant" message ""
                BlockComplete Nothing state
    UiSystemMessage message ->
        appendBlock BlockSystem "System" message "" BlockComplete Nothing state
    UiRecapStarted ->
        replaceOrAppendRecap "Generating recap…" BlockRunning state
    UiRecapReady summary ->
        replaceOrAppendRecap summary BlockComplete state
    UiRecapUnavailable message -> recapUnavailable message state
    UiErrorMessage message ->
        (appendBlock BlockError "Error" message "" BlockFailed Nothing state)
            { uiRetryCountdown = Nothing }
    UiRetryCountdown prefix remainingMillis suffix ->
        setRetryCountdown prefix remainingMillis suffix state
    UiConversationCleared -> clearConversation state
    UiSetFollow follow -> setFollow follow state
    UiTurnEnded terminalState ->
        finalizeTurn terminalState state
    UiTurnRestarted -> restartTurn state

setAwaitingInput :: Bool -> UiState -> UiState
setAwaitingInput awaiting state =
    (if awaiting then finalizeStreams state else state)
        { uiAwaitingInput = awaiting
        , uiRunning = if awaiting then False else state.uiRunning
        , uiGenerating = if awaiting then False else state.uiGenerating
        , uiActivity =
            if awaiting && state.uiCompletionRemainingMillis == 0
                then "Ready"
                else state.uiActivity
        }

recapUnavailable :: Text -> UiState -> UiState
recapUnavailable message state =
    withNotice
        (maybe state (`removeBlockAt` state) (latestRecapIndex state))
  where
    withNotice current =
        current
            { uiNotice = Just (warningNotice message)
            , uiNoticeElapsedMillis = 0
            }

setRetryCountdown :: Text -> Int -> Text -> UiState -> UiState
setRetryCountdown prefix remainingMillis suffix state =
    withBlock
        { uiRetryCountdown =
            if remaining == 0
                then Nothing
                else Just RetryCountdown
                    { retryCountdownBlockId = ident
                    , retryCountdownPrefix = prefix
                    , retryCountdownRemainingMillis = remaining
                    , retryCountdownSuffix = suffix
                    }
        }
  where
    ident = BlockId state.uiNextBlockId
    remaining = max 0 remainingMillis
    withBlock =
        appendBlock
            BlockError
            "Error"
            (retryCountdownText prefix remaining suffix)
            ""
            BlockFailed
            Nothing
            state

clearConversation :: UiState -> UiState
clearConversation state =
    state
        { uiBlocks = Seq.empty
        , uiSelectedBlock = Nothing
        , uiSelectedBlockIndex = Nothing
        , uiBlockIndices = Map.empty
        , uiNextBlockId = 1
        , uiTurnStartBlock = 0
        , uiAttemptStartBlock = 0
        , uiToolCalls = Map.empty
        , uiInspectionGroups = Map.empty
        , uiShellProcesses = Map.empty
        , uiShellPolls = Map.empty
        , uiRetryCountdown = Nothing
        , uiTodos = []
        , uiGenerating = False
        , uiGenerationChars = 0
        , uiGenerationMillis = 0
        , uiGenerationLastDeltaMillis = 0
        , uiResponseMillis = 0
        , uiLastTokensPerSecond = Nothing
        }

setFollow :: Bool -> UiState -> UiState
setFollow follow state =
    state
        { uiFollow = follow
        , uiSelectedBlock =
            if follow
                then (.blockId) <$> Seq.lookup lastIndex state.uiBlocks
                else state.uiSelectedBlock
        , uiSelectedBlockIndex =
            if follow
                then
                    if Seq.null state.uiBlocks
                        then Nothing
                        else Just lastIndex
                else state.uiSelectedBlockIndex
        }
  where
    lastIndex = Seq.length state.uiBlocks - 1

restartTurn :: UiState -> UiState
restartTurn state =
    state
        { uiBlocks = blocks
        , uiSelectedBlock = (.blockId) . snd <$> selected
        , uiSelectedBlockIndex = fst <$> selected
        , uiBlockIndices =
            Map.filter (< Seq.length blocks) state.uiBlockIndices
        , uiRunning = False
        , uiGenerating = False
        , uiActivity = "Restarting…"
        , uiNotice = Just (progressNotice "Restarting current turn…")
        , uiNoticeElapsedMillis = 0
        , uiCompletionRemainingMillis = 0
        , uiToolCalls = Map.empty
        , uiInspectionGroups = Map.empty
        , uiShellProcesses = processes
        , uiShellPolls = retainShellPolls processes state.uiShellPolls
        , uiAttemptStartBlock = state.uiTurnStartBlock
        }
  where
    blocks = Seq.take state.uiTurnStartBlock state.uiBlocks
    selected = selectionAfterTruncate blocks state.uiSelectedBlockIndex
    processes = retainShellProcesses blocks state.uiShellProcesses

resetGeneration :: UiState -> UiState
resetGeneration state =
    -- The live character estimate starts with the first visible delta. The
    -- provider response clock starts here because provider output-token usage
    -- can also include hidden reasoning generated before that first delta.
    state
        { uiGenerating = False
        , uiGenerationChars = 0
        , uiGenerationMillis = 0
        , uiGenerationLastDeltaMillis = 0
        , uiResponseMillis = 0
        }

appendGenerationChars :: Text -> UiState -> UiState
appendGenerationChars delta state =
    state
        { uiGenerating = True
        , uiGenerationChars =
            state.uiGenerationChars + Text.length delta
        , uiGenerationLastDeltaMillis = state.uiGenerationMillis
        }

snapshotGenerationRate :: TokenUsage -> UiState -> UiState
snapshotGenerationRate usage state =
    let
        generationMillis = state.uiGenerationLastDeltaMillis
        responseMillis = state.uiResponseMillis
    in state
        { uiGenerating = False
        , uiGenerationMillis = generationMillis
        , uiLastTokensPerSecond =
            generationTokensPerSecond
                usage.outputTokens
                responseMillis
        }

reduceLoop :: LoopEvent -> UiState -> UiState
reduceLoop event state = case event of
    TurnStarted -> startTurn state
    ReasoningDelta delta ->
        appendOrExtend BlockThinking "Thought" delta BlockStreaming $
            appendGenerationChars delta state
                { uiActivity = "Thinking…"
                }
    TextDelta delta ->
        appendOrExtend BlockAssistant "Assistant" delta BlockStreaming $
            appendGenerationChars delta state
                { uiActivity = "Writing…"
                }
    ActivityUpdated activity ->
        state { uiActivity = activity }
    ProviderLimitUpdated
        { providerLimitText = text
        , providerLimitWarning = warning
        } -> setProviderLimit text warning state
    WarningRaised warning ->
        state
            { uiNotice = Just (warningNotice warning)
            , uiNoticeElapsedMillis = 0
            }
    ResponseRestarted message -> restartResponse message state
    ToolStarted call -> startOrUpdateToolCall call state
    ToolUpdated call ->
        updateToolCall call state
    ToolArgumentsUpdated call ->
        updateToolCall call state
    ToolOutputUpdated callId output ->
        updateToolOutput callId output state
    ToolFinished result ->
        finishToolResult result state
    ToolRetracted callId ->
        retractToolCall callId state
    ResponseAttemptDiscarded ->
        discardResponseAttempt state
    ResponseAttemptFailed ->
        finalizeTurn BlockFailed state
    NativeAgentStarted{} ->
        state
    NativeAgentOutput{} ->
        state
    NativeAgentFinished{} ->
        state
    TurnFinished output -> finishLoopTurn output state

startTurn :: UiState -> UiState
startTurn state =
    resetGeneration
        state
            { uiRunning = True
            , uiAwaitingInput = False
            , uiActivity = "Thinking…"
            , uiNotice = Nothing
            , uiNoticeElapsedMillis = 0
            , uiElapsedMillis = 0
            , uiCompletionRemainingMillis = 0
            , uiTurnStartBlock = Seq.length state.uiBlocks
            , uiAttemptStartBlock = Seq.length state.uiBlocks
            , uiToolCalls = Map.empty
            , uiInspectionGroups = Map.empty
            }

setProviderLimit :: Text -> Bool -> UiState -> UiState
setProviderLimit text warning state =
    state
        { uiPrompt =
            state.uiPrompt
                { promptLimitStatus =
                    Just PromptLimitStatus
                        { promptLimitText = text
                        , promptLimitWarning = warning
                        }
                }
        }

restartResponse :: Text -> UiState -> UiState
restartResponse message state =
    resetGeneration
        finalized
            { uiRunning = True
            , uiActivity = "Retrying response…"
            , uiNotice = Just (warningNotice message)
            , uiNoticeElapsedMillis = 0
            , uiAttemptStartBlock = Seq.length finalized.uiBlocks
            , uiToolCalls = Map.empty
            , uiInspectionGroups = Map.empty
            }
  where
    finalized = finalizeAttempt BlockFailed (finalizeStreams state)

startOrUpdateToolCall :: ToolCall -> UiState -> UiState
startOrUpdateToolCall call state
    | Map.member call.callId state.uiToolCalls
        || Map.member call.callId state.uiShellPolls =
        -- A streaming backend may announce the call before execution;
        -- the core loop announces it again once the response is complete.
        -- Refresh the canonical metadata without adding another block.
        updateToolCall call
            state
                { uiRunning = True
                , uiGenerating = False
                , uiAwaitingInput = False
                }
    | otherwise = startToolCall call state

finishLoopTurn :: TurnOutput -> UiState -> UiState
finishLoopTurn output state =
    snapshotGenerationRate output.tokenUsage withFallback
        { uiRunning = continuing
        , uiActivity =
            if continuing
                then "Running tools…"
                else finishedActivity
        , uiCompletionRemainingMillis =
            if continuing then 0 else completionStatusDurationMillis
        }
  where
    finalized = finalizeStreams state
    continuing = not (null output.toolCalls)
    finishedActivity =
        case telemetrySummary <$> output.providerTelemetry of
            Just summary
                | not (Text.null summary) ->
                    "Finished · " <> summary
            _ -> "Finished"
    withFallback = case output.assistantText of
        Just text
            | not (Text.null (Text.strip text))
            , not
                (hasAssistantTextSince
                    finalized.uiAttemptStartBlock
                    finalized) ->
                appendBlock BlockAssistant "Assistant" text ""
                    BlockComplete Nothing finalized
        _ -> finalized

appendOrExtend
    :: BlockKind
    -> Text
    -> Text
    -> BlockState
    -> UiState
    -> UiState
appendOrExtend kind title delta streamState state =
    case Seq.viewr state.uiBlocks of
        rest Seq.:> block
            | block.blockKind == kind
            , block.blockState == BlockStreaming ->
                state
                    { uiBlocks =
                        rest Seq.|> block
                            { blockBody = block.blockBody <> delta }
                    }
        _ ->
            appendBlock kind title delta "" streamState Nothing state

startToolCall :: ToolCall -> UiState -> UiState
startToolCall call state
    | Just sessionId <- emptyWriteStdinSession call
    , Map.member sessionId state.uiShellProcesses =
        closeInspectionGroups state
            { uiRunning = True
            , uiGenerating = False
            , uiAwaitingInput = False
            , uiActivity = "Waiting for background terminal…"
            , uiShellPolls =
                Map.insert call.callId sessionId state.uiShellPolls
            }
    | isTodoTool call.name =
        closeInspectionGroups state
            { uiRunning = True
            , uiGenerating = False
            , uiAwaitingInput = False
            , uiActivity = toolCallTitleRelative state.uiWorkspaceRoot call
            , uiToolCalls =
                Map.insert
                    call.callId
                    (Seq.length state.uiBlocks, call)
                    state.uiToolCalls
            }
    | toolBlockKind call.name == BlockInspect
    , isGroupableInspectionTool call.name =
        startInspectionCall call state
    | otherwise =
        let
            kind = toolBlockKind call.name
            activity = toolCallTitleRelative state.uiWorkspaceRoot call
            (title, headerDetail) =
                toolCallHeaderRelative state.uiWorkspaceRoot call
            blockIndex = Seq.length state.uiBlocks
            body = formatToolDiffRelative state.uiWorkspaceRoot call
            detail = fromMaybe (toolCallInput call) headerDetail
        in appendBlock kind title body detail
            BlockRunning (Just call.callId)
            state
                { uiRunning = True
                , uiGenerating = False
                , uiAwaitingInput = False
                , uiActivity = activity
                , uiToolCalls =
                    Map.insert
                        call.callId
                        (blockIndex, call)
                        state.uiToolCalls
                }

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

-- | Close a burst when another visible event intervenes. Keep its item
-- metadata until the turn ends so a provider can still retract an individual
-- completed call without removing the rest of the grouped block.
closeInspectionGroups :: UiState -> UiState
closeInspectionGroups state =
    state
        { uiInspectionGroups =
            Map.map
                (\group -> group { inspectionGroupOpen = False })
                state.uiInspectionGroups
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

replaceOrAppendRecap :: Text -> BlockState -> UiState -> UiState
replaceOrAppendRecap body blockState state =
    case latestRecapIndex state of
        Just index ->
            state
                { uiBlocks =
                    Seq.adjust
                        (\block ->
                            block
                                { blockBody = body
                                , blockState
                                })
                        index
                        state.uiBlocks
                }
        Nothing ->
            appendBlock BlockRecap "Recap" body "" blockState Nothing state

latestRecapIndex :: UiState -> Maybe Int
latestRecapIndex state =
    case Seq.findIndexR ((== BlockRecap) . (.blockKind)) state.uiBlocks of
        Just index -> Just index
        Nothing -> Nothing

selectedIndexFor :: Maybe BlockId -> Seq UiBlock -> Maybe Int
selectedIndexFor selected remaining =
    selected >>= \ident -> Seq.findIndexL ((== ident) . (.blockId)) remaining

removeBlockAt :: Int -> UiState -> UiState
removeBlockAt index state =
    let remaining = Seq.deleteAt index state.uiBlocks
        processes =
            retainShellProcesses remaining state.uiShellProcesses
        selected
            | Just selectedId <- state.uiSelectedBlock
            , any ((== selectedId) . (.blockId)) remaining =
                Just selectedId
            | otherwise =
                listToMaybe
                    [ block.blockId
                    | idx <- [min index (max 0 (Seq.length remaining - 1))]
                    , idx >= 0
                    , idx < Seq.length remaining
                    , let block = Seq.index remaining idx
                    ]
        adjustIndex oldIndex
            | oldIndex > index = oldIndex - 1
            | otherwise = oldIndex
    in state
        { uiBlocks = remaining
        , uiSelectedBlock = selected
        , uiSelectedBlockIndex = selectedIndexFor selected remaining
        , uiBlockIndices =
            Map.fromList
                [ (block.blockId, idx)
                | (idx, block) <- zip [0 ..] (Foldable.toList remaining)
                ]
        , uiToolCalls =
            Map.mapMaybe
                (\(blockIndex, call) ->
                    if blockIndex == index
                        then Nothing
                        else Just (adjustIndex blockIndex, call))
                state.uiToolCalls
        , uiInspectionGroups =
            Map.filterWithKey
                (\blockId _ ->
                    any ((== blockId) . (.blockId)) remaining)
                state.uiInspectionGroups
        , uiShellProcesses = processes
        , uiShellPolls = retainShellPolls processes state.uiShellPolls
        , uiTurnStartBlock = adjustIndex state.uiTurnStartBlock
        , uiAttemptStartBlock = adjustIndex state.uiAttemptStartBlock
        }

appendBlock
    :: BlockKind
    -> Text
    -> Text
    -> Text
    -> BlockState
    -> Maybe Text
    -> UiState
    -> UiState
appendBlock kind title body detail blockState callId state =
    let prepared = closeInspectionGroups state
        index = Seq.length prepared.uiBlocks
        ident = BlockId prepared.uiNextBlockId
        block = UiBlock
            { blockId = ident
            , blockKind = kind
            , blockTitle = title
            , blockBody = body
            , blockTimestamp = ""
            , blockDetail = detail
            , blockState
            , blockExpanded =
                kind `elem` [BlockUser, BlockAssistant, BlockSystem, BlockRecap, BlockError]
                    || (kind == BlockShell
                        && blockState `elem` [BlockStreaming, BlockRunning])
            , blockCallId = callId
            , blockInspectionGroupable = False
            }
    in prepared
        { uiBlocks = prepared.uiBlocks Seq.|> block
        , uiNextBlockId = prepared.uiNextBlockId + 1
        , uiSelectedBlock = Just ident
        , uiSelectedBlockIndex = Just index
        , uiBlockIndices = Map.insert ident index prepared.uiBlockIndices
        }

-- | Attach one captured wall-clock label to newly appended conversation
-- messages. Tool and status blocks deliberately remain unstamped.
timestampNewMessageBlocks :: Int -> Text -> UiState -> UiState
timestampNewMessageBlocks firstNewIndex timestamp state
    | Text.null timestamp = state
    | otherwise =
        state
            { uiBlocks =
                Seq.mapWithIndex
                    (\index block ->
                        if index >= firstNewIndex
                            && Text.null block.blockTimestamp
                            && block.blockKind `elem` [BlockUser, BlockAssistant]
                            then block { blockTimestamp = timestamp }
                            else block)
                    state.uiBlocks
            }

completeTool :: Int -> ToolCall -> ToolCallResult -> UiState -> UiState
completeTool blockIndex call result state =
    let resultState = resultBlockState result
        diff =
            formatToolDiffRelativeWithOutput
                state.uiWorkspaceRoot
                call
                result.output
        body
            | resultState == BlockComplete
            , not (Text.null (Text.strip diff)) = diff
            | otherwise = result.output
    in case Seq.lookup blockIndex state.uiBlocks of
        Just block
            | Just group <-
                Map.lookup block.blockId state.uiInspectionGroups
            , any
                ((== result.callId) . (.inspectionCallId))
                group.inspectionGroupItems ->
                    let
                        updatedGroup =
                            group
                                { inspectionGroupItems =
                                    map
                                        (\item ->
                                            if item.inspectionCallId
                                                == result.callId
                                                then item
                                                    { inspectionBody = body
                                                    , inspectionState =
                                                        resultState
                                                    }
                                                else item)
                                        group.inspectionGroupItems
                                }
                    in state
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
                        }
        _ ->
            state
                { uiBlocks =
                    Seq.adjust
                        (\block ->
                            if block.blockCallId == Just result.callId
                                then
                                    setBlockState resultState block
                                        { blockBody = body }
                                else block)
                        blockIndex
                        state.uiBlocks
                }

finishToolResult :: ToolCallResult -> UiState -> UiState
finishToolResult result state =
    case Map.lookup result.callId state.uiShellPolls of
        Just sessionId ->
            finishShellPoll sessionId result state
        Nothing ->
            finishVisibleTool result state

finishVisibleTool :: ToolCallResult -> UiState -> UiState
finishVisibleTool result state =
    let
        activeCall = Map.lookup result.callId state.uiToolCalls
        displayed = case activeCall of
            Nothing -> result
            Just (_, call) ->
                result
                    { output =
                        formatToolOutputRelative
                            state.uiWorkspaceRoot
                            call
                            result.output
                    }
        todos =
            case activeCall of
                Just (_, call)
                    | isTodoTool call.name ->
                        fromMaybe state.uiTodos
                            (todoListFromToolOutput result.output
                                <|> todoListFromToolArguments call.arguments)
                _ -> state.uiTodos
        next =
            state
                { uiRunning = True
                , uiAwaitingInput = False
                , uiActivity = "Thinking…"
                , uiToolCalls =
                    Map.delete result.callId state.uiToolCalls
                , uiTodos = todos
                }
    in case activeCall of
        Nothing -> next
        Just (blockIndex, call)
            | blockIndex < Seq.length state.uiBlocks
            , Just _ <- trackedShellOwner call next ->
                reconcileVisibleShellContinuation
                    call
                    result
                    (completeTool blockIndex call displayed next)
            | blockIndex < Seq.length state.uiBlocks
            , isShellProcessTool call.name
            , Just (sessionId, output) <-
                runningShellOutcome result
            , Map.notMember sessionId next.uiShellProcesses ->
                retainRunningShell blockIndex sessionId output next
            | blockIndex < Seq.length state.uiBlocks ->
                completeTool blockIndex call displayed next
        Just _ -> next

trackedShellOwner :: ToolCall -> UiState -> Maybe BlockId
trackedShellOwner call state = do
    sessionId <- writeStdinSession call
    Map.lookup sessionId state.uiShellProcesses

-- A non-empty write_stdin remains visible as an input action, but it can also
-- observe the underlying command finishing. Keep the process owner in sync
-- without duplicating the continuation output across both blocks.
reconcileVisibleShellContinuation
    :: ToolCall
    -> ToolCallResult
    -> UiState
    -> UiState
reconcileVisibleShellContinuation call result state =
    case writeStdinSession call of
        Nothing -> state
        Just sessionId ->
            case Map.lookup sessionId state.uiShellProcesses of
                Nothing -> state
                Just ownerId ->
                    case runningShellOutcome result of
                        Just (returnedSessionId, _) ->
                            state
                                { uiShellProcesses =
                                    Map.insert
                                        returnedSessionId
                                        ownerId
                                        (Map.delete
                                            sessionId
                                            state.uiShellProcesses)
                                }
                        Nothing ->
                            case terminalShellOutcome result of
                                Nothing -> state
                                Just (blockState, _) ->
                                    let
                                        processes =
                                            Map.delete
                                                sessionId
                                                state.uiShellProcesses
                                        blocks =
                                            case lookupBlockIndex ownerId state of
                                                Nothing -> state.uiBlocks
                                                Just (blockIndex, _) ->
                                                    Seq.adjust
                                                        (setBlockState blockState)
                                                        blockIndex
                                                        state.uiBlocks
                                    in state
                                        { uiBlocks = blocks
                                        , uiShellProcesses = processes
                                        , uiShellPolls =
                                            retainShellPolls
                                                processes
                                                state.uiShellPolls
                                        }

retainRunningShell :: Int -> Int -> Text -> UiState -> UiState
retainRunningShell blockIndex sessionId output state =
    case Seq.lookup blockIndex state.uiBlocks of
        Nothing -> state
        Just owner ->
            state
                { uiBlocks =
                    Seq.adjust
                        (\block ->
                            setBlockState BlockRunning block
                                { blockBody = output })
                        blockIndex
                        state.uiBlocks
                , uiShellProcesses =
                    Map.insert
                        sessionId
                        owner.blockId
                        state.uiShellProcesses
                }

finishShellPoll :: Int -> ToolCallResult -> UiState -> UiState
finishShellPoll sessionId result state =
    let
        next =
            state
                { uiRunning = True
                , uiAwaitingInput = False
                , uiActivity = "Thinking…"
                , uiShellPolls =
                    Map.delete result.callId state.uiShellPolls
                }
        withoutSession =
            Map.delete sessionId next.uiShellProcesses
    in case Map.lookup sessionId state.uiShellProcesses of
        Nothing ->
            next { uiShellProcesses = withoutSession }
        Just ownerId ->
            case lookupBlockIndex ownerId next of
                Nothing ->
                    next { uiShellProcesses = withoutSession }
                Just (blockIndex, _) ->
                    case runningShellOutcome result of
                        Just (returnedSessionId, output) ->
                            appendShellOutput blockIndex output BlockRunning
                                next
                                    { uiShellProcesses =
                                        Map.insert
                                            returnedSessionId
                                            ownerId
                                            withoutSession
                                    }
                        Nothing ->
                            case terminalShellOutcome result of
                                Nothing -> next
                                Just (blockState, output) ->
                                    appendShellOutput
                                        blockIndex
                                        output
                                        blockState
                                        next
                                            { uiShellProcesses =
                                                withoutSession
                                            }

appendShellOutput :: Int -> Text -> BlockState -> UiState -> UiState
appendShellOutput blockIndex output blockState state =
    state
        { uiBlocks =
            Seq.adjust
                (\block ->
                    setBlockState blockState block
                        { blockBody = block.blockBody <> output })
                blockIndex
                state.uiBlocks
        }

finalizeAttempt :: BlockState -> UiState -> UiState
finalizeAttempt terminalState state =
    let
        terminalBlocks =
            Seq.mapWithIndex
                (\index block ->
                    if index >= state.uiAttemptStartBlock
                        && block.blockState == BlockRunning
                        then setBlockState terminalState block
                        else block)
                state.uiBlocks
        blocks =
            finalizeInspectionBlocks
                terminalState
                state.uiBlockIndices
                state.uiInspectionGroups
                terminalBlocks
        processes = retainShellProcesses blocks state.uiShellProcesses
    in state
        { uiBlocks = blocks
        , uiShellProcesses = processes
        , uiShellPolls = retainShellPolls processes state.uiShellPolls
        }

finalizeInspectionBlocks
    :: BlockState
    -> Map.Map BlockId Int
    -> Map.Map BlockId InspectionGroup
    -> Seq UiBlock
    -> Seq UiBlock
finalizeInspectionBlocks terminalState indices groups blocks =
    Map.foldlWithKey' finalizeGroup blocks groups
  where
    finalizeGroup blocks blockId group =
        case Map.lookup blockId indices of
            Nothing -> blocks
            Just index ->
                let finalized =
                        group
                            { inspectionGroupItems =
                                map finish group.inspectionGroupItems
                            }
                in Seq.adjust
                    (renderInspectionGroup finalized)
                    index
                    blocks

    finish item
        | item.inspectionState `elem` [BlockRunning, BlockStreaming] =
            item { inspectionState = terminalState }
        | otherwise = item

updateToolCall :: ToolCall -> UiState -> UiState
updateToolCall call state =
    case emptyWriteStdinSession call of
        Just sessionId
            | Map.member sessionId state.uiShellProcesses ->
                coalesceShellPoll call sessionId state
        _
            | Map.member call.callId state.uiShellPolls ->
                startToolCall call
                    state
                        { uiShellPolls =
                            Map.delete call.callId state.uiShellPolls
                        }
        _ -> updateVisibleToolCall call state

updateVisibleToolCall :: ToolCall -> UiState -> UiState
updateVisibleToolCall call state =
    case Map.lookup call.callId state.uiToolCalls of
        Nothing -> state
        Just (blockIndex, previous) ->
            let activity =
                    toolCallTitleRelative state.uiWorkspaceRoot call
                (title, headerDetail) =
                    toolCallHeaderRelative state.uiWorkspaceRoot call
                body = formatToolDiffRelative state.uiWorkspaceRoot call
                inspectionUpdate = do
                    block <- Seq.lookup blockIndex state.uiBlocks
                    group <-
                        Map.lookup block.blockId state.uiInspectionGroups
                    guard $
                        any
                            ((== call.callId) . (.inspectionCallId))
                            group.inspectionGroupItems
                    let updatedGroup =
                            group
                                { inspectionGroupItems =
                                    map updateItem group.inspectionGroupItems
                                }
                    pure (block.blockId, updatedGroup)
                updateItem item
                    | item.inspectionCallId == call.callId =
                        item
                            { inspectionToolName =
                                canonicalToolName call.name
                            , inspectionTitle = title
                            , inspectionDetail =
                                fromMaybe "" headerDetail
                            , inspectionBody =
                                if Text.null body
                                    then item.inspectionBody
                                    else body
                            }
                    | otherwise = item
                blocks
                    | isTodoTool previous.name = state.uiBlocks
                    | Just (_, group) <- inspectionUpdate =
                        Seq.adjust
                            (renderInspectionGroup group)
                            blockIndex
                            state.uiBlocks
                    | otherwise =
                        Seq.adjust
                            (\block ->
                                if block.blockCallId == Just call.callId
                                    then block
                                        { blockKind = toolBlockKind call.name
                                        , blockTitle = title
                                        , blockBody =
                                            if Text.null body
                                                then block.blockBody
                                                else body
                                        , blockDetail =
                                            fromMaybe
                                                (toolCallInput call)
                                                headerDetail
                                        }
                                    else block)
                            blockIndex
                            state.uiBlocks
                groups =
                    case inspectionUpdate of
                        Just (blockId, group) ->
                            Map.insert
                                blockId
                                group
                                state.uiInspectionGroups
                        Nothing -> state.uiInspectionGroups
            in state
                { uiBlocks = blocks
                , uiActivity = activity
                , uiInspectionGroups = groups
                , uiToolCalls =
                    Map.insert
                        call.callId
                        (blockIndex, call)
                        state.uiToolCalls
                }

coalesceShellPoll :: ToolCall -> Int -> UiState -> UiState
coalesceShellPoll call sessionId state =
    let
        withoutVisibleBlock =
            case Map.lookup call.callId state.uiToolCalls of
                Just (blockIndex, previous)
                    | not (isTodoTool previous.name) ->
                        removeBlockAt blockIndex state
                _ ->
                    state
                        { uiToolCalls =
                            Map.delete call.callId state.uiToolCalls
                        }
    in withoutVisibleBlock
        { uiRunning = True
        , uiGenerating = False
        , uiAwaitingInput = False
        , uiActivity = "Waiting for background terminal…"
        , uiShellPolls =
            Map.insert
                call.callId
                sessionId
                withoutVisibleBlock.uiShellPolls
        }

retractToolCall :: Text -> UiState -> UiState
retractToolCall callId state =
    case Map.lookup callId state.uiShellPolls of
        Just _ ->
            state { uiShellPolls = Map.delete callId state.uiShellPolls }
        Nothing -> retractVisibleToolCall callId state

retractVisibleToolCall :: Text -> UiState -> UiState
retractVisibleToolCall callId state =
    case Map.lookup callId state.uiToolCalls of
        Nothing ->
            case inspectionBlockIndexForCall callId state of
                Just blockIndex ->
                    fromMaybe state
                        (retractInspectionItemAt callId blockIndex state)
                Nothing ->
                    case Seq.findIndexL
                        ((== Just callId) . (.blockCallId))
                        state.uiBlocks of
                        Just blockIndex -> removeBlockAt blockIndex state
                        Nothing -> state
        Just (_, call)
            | isTodoTool call.name ->
                state
                    { uiToolCalls = Map.delete callId state.uiToolCalls }
        Just (blockIndex, _) ->
            case retractInspectionItemAt callId blockIndex state of
                Just retracted -> retracted
                Nothing ->
                    case Seq.lookup blockIndex state.uiBlocks of
                        Just block
                            | block.blockCallId == Just callId ->
                                removeBlockAt blockIndex state
                        _ ->
                            state
                                { uiToolCalls =
                                    Map.delete callId state.uiToolCalls }

inspectionBlockIndexForCall :: Text -> UiState -> Maybe Int
inspectionBlockIndexForCall callId state =
    listToMaybe
        [ blockIndex
        | (blockId, group) <- Map.toList state.uiInspectionGroups
        , any
            ((== callId) . (.inspectionCallId))
            group.inspectionGroupItems
        , Just blockIndex <- [Map.lookup blockId state.uiBlockIndices]
        ]

retractInspectionItemAt :: Text -> Int -> UiState -> Maybe UiState
retractInspectionItemAt callId blockIndex state = do
    block <- Seq.lookup blockIndex state.uiBlocks
    group <- Map.lookup block.blockId state.uiInspectionGroups
    let remaining =
            filter
                ((/= callId) . (.inspectionCallId))
                group.inspectionGroupItems
    guard (length remaining < length group.inspectionGroupItems)
    pure $
        if null remaining
            then
                removeBlockAt
                    blockIndex
                    state
                        { uiToolCalls =
                            Map.delete callId state.uiToolCalls
                        }
            else
                let updatedGroup =
                        group { inspectionGroupItems = remaining }
                in state
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
                        Map.delete callId state.uiToolCalls
                    }

discardResponseAttempt :: UiState -> UiState
discardResponseAttempt state =
    let boundary =
            min (Seq.length state.uiBlocks) state.uiAttemptStartBlock
        blocks = Seq.take boundary state.uiBlocks
        selected =
            selectionAfterTruncate blocks state.uiSelectedBlockIndex
        processes =
            retainShellProcesses blocks state.uiShellProcesses
    in state
        { uiBlocks = blocks
        , uiSelectedBlock = (.blockId) . snd <$> selected
        , uiSelectedBlockIndex = fst <$> selected
        , uiBlockIndices =
            Map.filter (< boundary) state.uiBlockIndices
        , uiToolCalls =
            Map.filter ((< boundary) . fst) state.uiToolCalls
        , uiInspectionGroups =
            Map.filterWithKey
                (\blockId _ ->
                    any ((== blockId) . (.blockId)) blocks)
                state.uiInspectionGroups
        , uiShellProcesses = processes
        , uiShellPolls = retainShellPolls processes state.uiShellPolls
        , uiGenerating = False
        }

updateToolOutput :: Text -> Text -> UiState -> UiState
updateToolOutput callId output state =
    case Map.lookup callId state.uiToolCalls of
        Nothing -> state
        Just (blockIndex, _) ->
            case Seq.lookup blockIndex state.uiBlocks of
                Just block
                    | Just group <-
                        Map.lookup block.blockId state.uiInspectionGroups ->
                            let updatedGroup =
                                    group
                                        { inspectionGroupItems =
                                            map
                                                (\item ->
                                                    if item.inspectionCallId
                                                        == callId
                                                        && item.inspectionState
                                                            == BlockRunning
                                                        then item
                                                            { inspectionBody =
                                                                output
                                                            }
                                                        else item)
                                                group.inspectionGroupItems
                                        }
                            in state
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
                                }
                _ ->
                    state
                        { uiBlocks =
                            Seq.adjust
                                (\block ->
                                    if block.blockCallId == Just callId
                                        && block.blockState == BlockRunning
                                        then block { blockBody = output }
                                        else block)
                                blockIndex
                                state.uiBlocks
                        }

finalizeTurn :: BlockState -> UiState -> UiState
finalizeTurn terminalState state =
    let
        generationMillis = state.uiGenerationLastDeltaMillis
        shellOwners = Map.elems state.uiShellProcesses
        terminalBlocks =
            Seq.mapWithIndex
                (\index block ->
                    if (index >= state.uiTurnStartBlock
                            || block.blockId `elem` shellOwners)
                        && block.blockState
                            `elem` [BlockStreaming, BlockRunning]
                        then setBlockState terminalState block
                        else block)
                state.uiBlocks
        blocks =
            finalizeInspectionBlocks
                terminalState
                state.uiBlockIndices
                state.uiInspectionGroups
                terminalBlocks
    in state
        { uiBlocks = blocks
        , uiRunning = False
        , uiGenerating = False
        , uiGenerationMillis = generationMillis
        , uiLastTokensPerSecond =
            state.uiLastTokensPerSecond
        , uiActivity =
            if terminalState == BlockComplete
                then "Finished"
                else "Ready"
        , uiCompletionRemainingMillis =
            if terminalState == BlockComplete
                then completionStatusDurationMillis
                else 0
        , uiNotice = case state.uiNotice of
            Just notice
                | notice.noticeKind == NoticeProgress -> Nothing
            other -> other
        , uiNoticeElapsedMillis = case state.uiNotice of
            Just notice
                | notice.noticeKind == NoticeProgress -> 0
            _ -> state.uiNoticeElapsedMillis
        , uiToolCalls = Map.empty
        , uiInspectionGroups = Map.empty
        , uiShellProcesses = Map.empty
        , uiShellPolls = Map.empty
        }

-- Shell commands start open while output is live, then compact to a one-line
-- summary. Preserve any manual folding while they are still running; a user
-- can also expand the completed block again with the normal block toggle.
setBlockState :: BlockState -> UiBlock -> UiBlock
setBlockState blockState block =
    block
        { blockState
        , blockExpanded =
            if block.blockKind == BlockShell
                && block.blockState `elem` [BlockStreaming, BlockRunning]
                && blockState `notElem` [BlockStreaming, BlockRunning]
                then False
                else block.blockExpanded
        }

infoNotice, successNotice, warningNotice, progressNotice, errorNotice
    :: Text -> UiNotice
infoNotice = transientNotice NoticeInfo
successNotice = transientNotice NoticeSuccess
warningNotice = transientNotice NoticeWarning
errorNotice = transientNotice NoticeError
progressNotice text = UiNotice
    { noticeKind = NoticeProgress
    , noticeText = text
    , noticeTransient = False
    }

transientNotice :: NoticeKind -> Text -> UiNotice
transientNotice kind text = UiNotice
    { noticeKind = kind
    , noticeText = text
    , noticeTransient = True
    }

finalizeStreams :: UiState -> UiState
finalizeStreams state =
    state
        { uiBlocks =
            fmap
                (\block ->
                    if block.blockState == BlockStreaming
                        then setBlockState BlockComplete block
                        else block)
                state.uiBlocks
        }

hasAssistantTextSince :: Int -> UiState -> Bool
hasAssistantTextSince start =
    any
        (\block ->
            block.blockKind == BlockAssistant
                && not (Text.null (Text.strip block.blockBody)))
        . Seq.drop start
        . (.uiBlocks)

moveSelection :: Int -> UiState -> UiState
moveSelection delta state =
    case Seq.lookup next blocks of
        Nothing ->
            state
                { uiSelectedBlock = Nothing
                , uiSelectedBlockIndex = Nothing
                }
        Just block ->
            state
                { uiSelectedBlock = Just block.blockId
                , uiSelectedBlockIndex = Just next
                , uiFollow = next == lastIndex
                }
  where
    blocks = state.uiBlocks
    lastIndex = Seq.length blocks - 1
    next = max 0 (min lastIndex (selectedBlockIndex state + delta))

selectBlock :: BlockId -> UiState -> UiState
selectBlock ident state =
    case lookupBlockIndex ident state of
        Nothing -> state
        Just (index, _) ->
            state
                { uiSelectedBlock = Just ident
                , uiSelectedBlockIndex = Just index
                , uiFollow =
                    index == Seq.length state.uiBlocks - 1
                }

lookupBlock :: BlockId -> UiState -> Maybe UiBlock
lookupBlock ident state =
    snd <$> lookupBlockIndex ident state

lookupBlockIndex :: BlockId -> UiState -> Maybe (Int, UiBlock)
lookupBlockIndex ident state = do
    index <- Map.lookup ident state.uiBlockIndices
    block <- Seq.lookup index state.uiBlocks
    if block.blockId == ident
        then Just (index, block)
        else Nothing

selectedBlockIndex :: UiState -> Int
selectedBlockIndex state =
    maybe fallback fst (selectedBlockEntry state)
  where
    fallback = max 0 (Seq.length state.uiBlocks - 1)

toggleSelected :: UiState -> UiState
toggleSelected state =
    case selectedBlockEntry state of
        Nothing -> state
        Just (index, _) ->
            state
                { uiBlocks =
                    Seq.adjust
                        (\block ->
                            block
                                { blockExpanded = not block.blockExpanded })
                        index
                        state.uiBlocks
                }

selectedBlockEntry :: UiState -> Maybe (Int, UiBlock)
selectedBlockEntry state = do
    ident <- state.uiSelectedBlock
    index <- state.uiSelectedBlockIndex
    storedIndex <- Map.lookup ident state.uiBlockIndices
    block <- Seq.lookup index state.uiBlocks
    if storedIndex == index && block.blockId == ident
        then Just (index, block)
        else Nothing

selectionAfterTruncate
    :: Seq UiBlock
    -> Maybe Int
    -> Maybe (Int, UiBlock)
selectionAfterTruncate blocks selected =
    case selected of
        Just index
            | Just block <- Seq.lookup index blocks ->
                Just (index, block)
        _ ->
            let index = Seq.length blocks - 1
            in (\block -> (index, block)) <$> Seq.lookup index blocks

retainShellProcesses
    :: Seq UiBlock
    -> Map.Map Int BlockId
    -> Map.Map Int BlockId
retainShellProcesses blocks =
    Map.filter \ownerId ->
        any
            (\block ->
                block.blockId == ownerId
                    && block.blockState == BlockRunning)
            blocks

retainShellPolls
    :: Map.Map Int BlockId
    -> Map.Map Text Int
    -> Map.Map Text Int
retainShellPolls processes =
    Map.filter (`Map.member` processes)

writeStdinInput :: ToolCall -> Maybe (Int, Maybe Text)
writeStdinInput call = do
    guard (canonicalToolName call.name == "write_stdin")
    either (const Nothing) Just $
        Hermes.decodeText
            (Hermes.object $
                (,)
                    <$> Hermes.atKey "session_id" Hermes.int
                    <*> Hermes.optionalKey "chars" Hermes.text)
            call.arguments

writeStdinSession :: ToolCall -> Maybe Int
writeStdinSession = fmap fst . writeStdinInput

emptyWriteStdinSession :: ToolCall -> Maybe Int
emptyWriteStdinSession call = do
    (sessionId, chars) <- writeStdinInput call
    guard (maybe True Text.null chars)
    pure sessionId

-- Older/imported results have no execution facts and use the legacy adapter.
resultBlockState :: ToolCallResult -> BlockState
resultBlockState result = maybe (toolResultState result.output) outcomeBlockState
    (toolCallResultOutcome result)

outcomeBlockState :: ToolOutcome -> BlockState
outcomeBlockState = \case
    ToolSucceeded -> BlockComplete
    ToolFailed -> BlockFailed
    ToolDenied -> BlockDenied
    ToolCancelled -> BlockCancelled
    ShellRunning _ -> BlockRunning
    ShellExited 0 -> BlockComplete
    ShellExited _ -> BlockFailed
    ShellCancelled -> BlockCancelled
    ShellTimedOut -> BlockFailed

runningShellOutcome :: ToolCallResult -> Maybe (Int, Text)
runningShellOutcome result = case toolCallResultOutcome result of
    Just (ShellRunning sessionId) -> Just (sessionId, dropOutputLine (dropOutputLine result.output))
    Just _ -> Nothing
    Nothing -> runningShellResult result.output

terminalShellOutcome :: ToolCallResult -> Maybe (BlockState, Text)
terminalShellOutcome result = case toolCallResultOutcome result of
    Just outcome@(ShellExited _) -> Just (outcomeBlockState outcome, dropOutputLine result.output)
    Just ShellCancelled -> Just (BlockCancelled, result.output)
    Just ShellTimedOut -> Just (BlockFailed, result.output)
    Just _ -> Nothing
    Nothing -> terminalShellResult result.output

-- Only remove the transport prologue for presentation. It is never parsed for
-- execution status, exit codes or session identifiers.
dropOutputLine :: Text -> Text
dropOutputLine = Text.drop 1 . Text.dropWhile (/= '\n')

runningShellResult :: Text -> Maybe (Int, Text)
runningShellResult output = do
    rest <- Text.stripPrefix
        "Process still running.\nsession_id: "
        output
    let (sessionText, withNewline) = Text.breakOn "\n" rest
    body <- Text.stripPrefix "\n" withNewline
    sessionId <- readWholeInt sessionText
    pure (sessionId, body)

terminalShellResult :: Text -> Maybe (BlockState, Text)
terminalShellResult output =
    case Text.stripPrefix "Exit code: " output of
        Just rest ->
            let (codeText, withNewline) = Text.breakOn "\n" rest
            in case (readWholeInt codeText, Text.stripPrefix "\n" withNewline) of
                (Just code, Just body) ->
                    Just
                        ( if code == 0 then BlockComplete else BlockFailed
                        , body
                        )
                _ -> Nothing
        Nothing
            | "Error: Command cancelled" `Text.isPrefixOf` output ->
                Just (toolResultState output, output)
            | "Error: Command timed out" `Text.isPrefixOf` output ->
                Just (toolResultState output, output)
            | otherwise -> Nothing

readWholeInt :: Text -> Maybe Int
readWholeInt text =
    case reads (Text.unpack text) of
        [(value, "")] -> Just value
        _ -> Nothing

isTodoTool :: Text -> Bool
isTodoTool name =
    canonicalToolName name `elem` ["todo_write", "update_plan"]

isShellProcessTool :: Text -> Bool
isShellProcessTool name =
    canonicalToolName name `elem` ["shell_command", "write_stdin"]

toolBlockKind :: Text -> BlockKind
toolBlockKind rawName
    | name `elem` ["run_terminal_cmd", "shell_command", "write_stdin", "run_ghci", "exec"] =
        BlockShell
    | name `elem` ["search_replace", "apply_patch", "Write", "NotebookEdit"] =
        BlockEdit
    | name `elem` ["todo_write", "update_plan"] =
        BlockTodo
    | isInspectionTool rawName = BlockInspect
    | otherwise = BlockTool
  where
    name = canonicalToolName rawName

-- | Syntax grammar for code carried in a shell-style tool block.
-- The title is retained alongside the source after the original call leaves
-- the live-tool map, so it also identifies exec's JavaScript code here.
blockCodeLanguage :: UiBlock -> Maybe Text
blockCodeLanguage block
    | block.blockKind /= BlockShell = Nothing
    | Text.null (Text.strip block.blockDetail) = Nothing
    | block.blockTitle == "$ exec" = Just "javascript"
    | otherwise = Just "haskell"

outputLooksFailed :: Text -> Bool
outputLooksFailed output =
    let lowered = Text.toLower (Text.strip output)
    in "error:" `Text.isPrefixOf` lowered
        || "exit: 1" `Text.isPrefixOf` lowered
        || "exit: 2" `Text.isPrefixOf` lowered

toolResultState :: Text -> BlockState
toolResultState output
    | "tool call rejected by user" `Text.isInfixOf` lowered =
        BlockDenied
    | structuredCancellation lowered =
        BlockCancelled
    | outputLooksFailed output = BlockFailed
    | Just code <- exitCodeFrom lowered
    , code /= 0 = BlockFailed
    | otherwise = BlockComplete
  where
    lowered = Text.toLower (Text.strip output)

structuredCancellation :: Text -> Bool
structuredCancellation output =
    let header = Text.strip (headLine output)
    in header == "exit: cancelled"
        || header == "error: command cancelled"
        || header == "cancelled"
        || "cancelled (" `Text.isPrefixOf` header

headLine :: Text -> Text
headLine = Text.takeWhile (/= '\n')

exitCodeFrom :: Text -> Maybe Int
exitCodeFrom text = do
    rest <-
        Text.stripPrefix "exit:" text
            <|> Text.stripPrefix "exit code:" text
    case reads (Text.unpack (Text.takeWhile (not . isSpace) (Text.strip rest))) of
        [(code, "")] -> Just code
        _ -> Nothing
