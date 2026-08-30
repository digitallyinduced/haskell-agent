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
    , formatToolOutputRelative
    , todoListFromToolArguments
    , todoListFromToolOutput
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
import Agent.Loop
    ( LoopEvent(..)
    , TokenUsage(..)
    , TurnOutput(..)
    , generationTokensPerSecond
    )
import Agent.ToolDispatch
    ( ToolCall(..)
    , ToolCallResult(..)
    , canonicalToolName
    )
import Control.Applicative ((<|>))
import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Char (isSpace)
import Data.Maybe (fromMaybe)

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
    UiSetPromptEffort effort ->
        state
            { uiPrompt = state.uiPrompt { promptEffort = effort }
            }
    UiSetPromptLimitStatus limitStatus ->
        state
            { uiPrompt =
                state.uiPrompt { promptLimitStatus = limitStatus }
            }
    UiSetAwaitingInput awaiting ->
        (if awaiting then finalizeStreams state else state)
            { uiAwaitingInput = awaiting
            , uiRunning = if awaiting then False else state.uiRunning
            , uiGenerating =
                if awaiting then False else state.uiGenerating
            , uiActivity =
                if awaiting && state.uiCompletionRemainingMillis == 0
                    then "Ready"
                    else state.uiActivity
            }
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
    UiRecapUnavailable message ->
        case latestRecapIndex state of
            Just index ->
                (removeBlockAt index state)
                    { uiNotice = Just (warningNotice message)
                    , uiNoticeElapsedMillis = 0
                    }
            Nothing ->
                state
                    { uiNotice = Just (warningNotice message)
                    , uiNoticeElapsedMillis = 0
                    }
    UiErrorMessage message ->
        (appendBlock BlockError "Error" message "" BlockFailed Nothing state)
            { uiRetryCountdown = Nothing }
    UiRetryCountdown prefix remainingMillis suffix ->
        let
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
        in withBlock
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
    UiConversationCleared ->
        state
            { uiBlocks = Seq.empty
            , uiSelectedBlock = Nothing
            , uiSelectedBlockIndex = Nothing
            , uiBlockIndices = Map.empty
            , uiNextBlockId = 1
            , uiTurnStartBlock = 0
            , uiAttemptStartBlock = 0
            , uiToolCalls = Map.empty
            , uiRetryCountdown = Nothing
            , uiTodos = []
            , uiGenerating = False
            , uiGenerationChars = 0
            , uiGenerationMillis = 0
            , uiGenerationLastDeltaMillis = 0
            , uiResponseMillis = 0
            , uiLastTokensPerSecond = Nothing
            }
    UiSetFollow follow ->
        state
            { uiFollow = follow
            , uiSelectedBlock =
                if follow
                    then (.blockId) <$> Seq.lookup
                        (Seq.length state.uiBlocks - 1)
                        state.uiBlocks
                    else state.uiSelectedBlock
            , uiSelectedBlockIndex =
                if follow
                    then
                        if Seq.null state.uiBlocks
                            then Nothing
                            else Just (Seq.length state.uiBlocks - 1)
                    else state.uiSelectedBlockIndex
            }
    UiTurnEnded terminalState ->
        finalizeTurn terminalState state
    UiTurnRestarted ->
        let blocks = Seq.take state.uiTurnStartBlock state.uiBlocks
            selected =
                selectionAfterTruncate
                    blocks
                    state.uiSelectedBlockIndex
        in state
            { uiBlocks = blocks
            , uiSelectedBlock = (.blockId) . snd <$> selected
            , uiSelectedBlockIndex = fst <$> selected
            , uiBlockIndices =
                Map.filter (< Seq.length blocks) state.uiBlockIndices
            , uiRunning = False
            , uiGenerating = False
            , uiActivity = "Restarting…"
            , uiNotice =
                Just (progressNotice "Restarting current turn…")
            , uiNoticeElapsedMillis = 0
            , uiCompletionRemainingMillis = 0
            , uiToolCalls = Map.empty
            , uiAttemptStartBlock = state.uiTurnStartBlock
            }

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
    TurnStarted ->
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
                }
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
        } ->
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
    WarningRaised warning ->
        state
            { uiNotice = Just (warningNotice warning)
            , uiNoticeElapsedMillis = 0
            }
    ResponseRestarted message ->
        let finalized =
                finalizeAttempt BlockFailed (finalizeStreams state)
        in resetGeneration
            finalized
                { uiRunning = True
                , uiActivity = "Retrying response…"
                , uiNotice = Just (warningNotice message)
                , uiNoticeElapsedMillis = 0
                , uiAttemptStartBlock = Seq.length finalized.uiBlocks
                , uiToolCalls = Map.empty
                }
    ToolStarted call
        | Map.member call.callId state.uiToolCalls ->
            -- A streaming backend may announce the call before execution;
            -- the core loop announces it again once the response is complete.
            -- Refresh the canonical metadata without adding another block.
            updateToolCall call
                state
                    { uiRunning = True
                    , uiGenerating = False
                    , uiAwaitingInput = False
                    }
        | isTodoTool call.name ->
            state
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
        | otherwise ->
            let
                kind = toolBlockKind call.name
                title = toolCallTitleRelative state.uiWorkspaceRoot call
                blockIndex = Seq.length state.uiBlocks
                body = formatToolDiffRelative state.uiWorkspaceRoot call
                detail = toolCallInput call
            in appendBlock kind title body detail
                BlockRunning (Just call.callId)
                state
                    { uiRunning = True
                    , uiGenerating = False
                    , uiAwaitingInput = False
                    , uiActivity = title
                    , uiToolCalls =
                        Map.insert
                            call.callId
                            (blockIndex, call)
                            state.uiToolCalls
                    }
    ToolUpdated call ->
        updateToolCall call state
    ToolArgumentsUpdated call ->
        updateToolCall call state
    ToolOutputUpdated callId output ->
        updateToolOutput callId output state
    ToolFinished result ->
        let activeCall = Map.lookup result.callId state.uiToolCalls
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
                                    <|> todoListFromToolArguments
                                        call.arguments)
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
            Just (blockIndex, _)
                | blockIndex < Seq.length state.uiBlocks ->
                    completeTool blockIndex displayed next
            Just _ -> next
    ToolRetracted callId ->
        retractToolCall callId state
    ResponseAttemptDiscarded ->
        discardResponseAttempt state
    NativeAgentStarted{} ->
        state
    NativeAgentOutput{} ->
        state
    NativeAgentFinished{} ->
        state
    TurnFinished output ->
        let finalized = finalizeStreams state
            continuing = not (null output.toolCalls)
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
        in snapshotGenerationRate output.tokenUsage withFallback
            { uiRunning = continuing
            , uiActivity =
                if continuing
                    then "Running tools…"
                    else "Finished"
            , uiCompletionRemainingMillis =
                if continuing then 0 else completionStatusDurationMillis
            }

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
    let index = Seq.length state.uiBlocks
        ident = BlockId state.uiNextBlockId
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
            , blockCallId = callId
            }
    in state
        { uiBlocks = state.uiBlocks Seq.|> block
        , uiNextBlockId = state.uiNextBlockId + 1
        , uiSelectedBlock = Just ident
        , uiSelectedBlockIndex = Just index
        , uiBlockIndices = Map.insert ident index state.uiBlockIndices
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

completeTool :: Int -> ToolCallResult -> UiState -> UiState
completeTool blockIndex result state =
    state
        { uiBlocks =
            Seq.adjust
                (\block ->
                    if block.blockCallId == Just result.callId
                        then block
                            { blockBody = result.output
                            , blockState = toolResultState result.output
                            }
                        else block)
                blockIndex
                state.uiBlocks
        }

finalizeAttempt :: BlockState -> UiState -> UiState
finalizeAttempt terminalState state =
    state
        { uiBlocks =
            Seq.mapWithIndex
                (\index block ->
                    if index >= state.uiAttemptStartBlock
                        && block.blockState == BlockRunning
                        then block { blockState = terminalState }
                        else block)
                state.uiBlocks
        }

updateToolCall :: ToolCall -> UiState -> UiState
updateToolCall call state =
    case Map.lookup call.callId state.uiToolCalls of
        Nothing -> state
        Just (blockIndex, previous) ->
            let title =
                    toolCallTitleRelative state.uiWorkspaceRoot call
                body = formatToolDiffRelative state.uiWorkspaceRoot call
                blocks
                    | isTodoTool previous.name = state.uiBlocks
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
                                        , blockDetail = toolCallInput call
                                        }
                                    else block)
                            blockIndex
                            state.uiBlocks
            in state
                { uiBlocks = blocks
                , uiActivity = title
                , uiToolCalls =
                    Map.insert
                        call.callId
                        (blockIndex, call)
                        state.uiToolCalls
                }

retractToolCall :: Text -> UiState -> UiState
retractToolCall callId state =
    case Map.lookup callId state.uiToolCalls of
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
            case Seq.lookup blockIndex state.uiBlocks of
                Just block
                    | block.blockCallId == Just callId ->
                        removeBlockAt blockIndex state
                _ ->
                    state
                        { uiToolCalls =
                            Map.delete callId state.uiToolCalls }

discardResponseAttempt :: UiState -> UiState
discardResponseAttempt state =
    let boundary =
            min (Seq.length state.uiBlocks) state.uiAttemptStartBlock
        blocks = Seq.take boundary state.uiBlocks
        selected =
            selectionAfterTruncate blocks state.uiSelectedBlockIndex
    in state
        { uiBlocks = blocks
        , uiSelectedBlock = (.blockId) . snd <$> selected
        , uiSelectedBlockIndex = fst <$> selected
        , uiBlockIndices =
            Map.filter (< boundary) state.uiBlockIndices
        , uiToolCalls =
            Map.filter ((< boundary) . fst) state.uiToolCalls
        , uiGenerating = False
        }

updateToolOutput :: Text -> Text -> UiState -> UiState
updateToolOutput callId output state =
    case Map.lookup callId state.uiToolCalls of
        Nothing -> state
        Just (blockIndex, _) ->
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
    let generationMillis = state.uiGenerationLastDeltaMillis
    in state
        { uiBlocks =
            Seq.mapWithIndex
                (\index block ->
                    if index >= state.uiTurnStartBlock
                        && block.blockState
                            `elem` [BlockStreaming, BlockRunning]
                        then block { blockState = terminalState }
                        else block)
                state.uiBlocks
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
                        then block { blockState = BlockComplete }
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

isTodoTool :: Text -> Bool
isTodoTool name =
    canonicalToolName name `elem` ["todo_write", "update_plan"]

toolBlockKind :: Text -> BlockKind
toolBlockKind rawName
    | name `elem` ["run_terminal_cmd", "shell_command", "write_stdin", "run_ghci", "exec"] =
        BlockShell
    | name `elem` ["search_replace", "apply_patch", "Write", "NotebookEdit"] =
        BlockEdit
    | name `elem` ["todo_write", "update_plan"] =
        BlockTodo
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
    rest <- Text.stripPrefix "exit:" text
    case reads (Text.unpack (Text.takeWhile (not . isSpace) (Text.strip rest))) of
        [(code, "")] -> Just code
        _ -> Nothing
