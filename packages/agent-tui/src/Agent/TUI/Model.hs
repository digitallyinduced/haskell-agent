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
    , deleteWordBefore
    , lineEndCursor
    , lineStartCursor
    , moveWordLeft
    , moveWordRight
    , reduceUi
    , selectedBlockIndex
    , timestampNewMessageBlocks
    , progressNotice
    , successNotice
    , uiNextDeadlineMillis
    , uiNeedsTick
    , warningNotice
    , advanceUiTime
    ) where

import Agent.TUI.Presentation
    ( formatSearchReplaceDiff
    , formatToolOutput
    , toolCallInput
    , toolCallTitle
    )
import Agent.TUI.Motion
    ( completionStatusDurationMillis
    , transientNoticeDurationMillis
    )
import Agent.Loop
    ( LoopEvent(..)
    , TokenUsage
    , TurnOutput(..)
    , emptyTokenUsage
    )
import Agent.ToolDispatch
    ( ToolCall(..)
    , ToolCallResult(..)
    , canonicalToolName
    )
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Char (isSpace)

newtype BlockId = BlockId Int
    deriving (Eq, Ord, Show)

data BlockKind
    = BlockUser
    | BlockAssistant
    | BlockThinking
    | BlockTool
    | BlockShell
    | BlockEdit
    | BlockSystem
    | BlockError
    deriving (Eq, Show)

data NoticeKind
    = NoticeInfo
    | NoticeSuccess
    | NoticeWarning
    | NoticeProgress
    | NoticeError
    deriving (Eq, Show)

data UiNotice = UiNotice
    { noticeKind :: !NoticeKind
    , noticeText :: !Text
    , noticeTransient :: !Bool
    }
    deriving (Eq, Show)

-- | A live retry deadline attached to one retained error block.
data RetryCountdown = RetryCountdown
    { retryCountdownBlockId :: !BlockId
    , retryCountdownPrefix :: !Text
    , retryCountdownRemainingMillis :: !Int
    , retryCountdownSuffix :: !Text
    }
    deriving (Eq, Show)

data BlockState
    = BlockStreaming
    | BlockRunning
    | BlockComplete
    | BlockFailed
    | BlockCancelled
    | BlockDenied
    deriving (Eq, Show)

data UiBlock = UiBlock
    { blockId :: !BlockId
    , blockKind :: !BlockKind
    , blockTitle :: !Text
    , blockBody :: !Text
    , blockTimestamp :: !Text
    , blockDetail :: !Text
    , blockState :: !BlockState
    , blockExpanded :: !Bool
    , blockCallId :: !(Maybe Text)
    }
    deriving (Eq, Show)

data Focus
    = FocusComposer
    | FocusScrollback
    | FocusPermission
    deriving (Eq, Show)

data PermissionOverlay = PermissionOverlay
    { permissionSummary :: !Text
    , permissionIndex :: !Int
    }
    deriving (Eq, Show)

data PromptLimitStatus = PromptLimitStatus
    { promptLimitText :: !Text
    , promptLimitWarning :: !Bool
    }
    deriving (Eq, Show)

data PromptState = PromptState
    { promptModel :: !Text
    , promptEffort :: !Text
    , promptMode :: !Text
    , promptAccount :: !Text
    , promptAccountSelectable :: !Bool
    , promptUsage :: !TokenUsage
    , promptLimitStatus :: !(Maybe PromptLimitStatus)
    , promptAttachments :: !Int
    }
    deriving (Eq, Show)

data UiState = UiState
    { uiBlocks :: !(Seq UiBlock)
    , uiNextBlockId :: !Int
    , uiDraft :: !Text
    , uiCursor :: !Int
    , uiQueuedInputs :: !(Seq Text)
    , uiFocus :: !Focus
    , uiSelectedBlock :: !(Maybe BlockId)
    , uiSelectedBlockIndex :: !(Maybe Int)
    , uiBlockIndices :: !(Map.Map BlockId Int)
    , uiFollow :: !Bool
    , uiRunning :: !Bool
    , uiAwaitingInput :: !Bool
    , uiActivity :: !Text
    , uiPrompt :: !PromptState
    , uiBranch :: !Text
    , uiCwd :: !Text
    , uiPermission :: !(Maybe PermissionOverlay)
    , uiNotice :: !(Maybe UiNotice)
    , uiRetryCountdown :: !(Maybe RetryCountdown)
    , uiNoticeElapsedMillis :: !Int
    , uiElapsedMillis :: !Int
    , uiCompletionRemainingMillis :: !Int
    , uiTurnStartBlock :: !Int
    , uiAttemptStartBlock :: !Int
    , uiToolCalls :: !(Map.Map Text (Int, ToolCall))
    }
    deriving (Eq, Show)

data UiEvent
    = UiLoop !LoopEvent
    | UiUserSubmitted !Text
    | UiDraftSubmitted
    | UiInputQueued !Text
    | UiInputPromoted !Text
    | UiQueuedInputStarted
    | UiSetDraft !Text !Int
    | UiSetPrompt !PromptState
    | UiSetPromptEffort !Text
    | UiSetPromptLimitStatus !(Maybe PromptLimitStatus)
    | UiSetAwaitingInput !Bool
    | UiSetRepository !Text !Text
    | UiSetNotice !(Maybe UiNotice)
    | UiMoveSelection !Int
    | UiSelectBlock !BlockId
    | UiActivateBlock !BlockId
    | UiToggleSelected
    | UiFocusChanged !Focus
    | UiPermissionShown !Text
    | UiPermissionMoved !Int
    | UiPermissionHidden
    | UiHistory !Text
    | UiAssistantHistory !Text
    | UiSystemMessage !Text
    | UiErrorMessage !Text
    -- | Append an error whose retry guidance counts down in place.
    | UiRetryCountdown !Text !Int !Text
    | UiConversationCleared
    | UiSetFollow !Bool
    | UiTurnEnded !BlockState
    | UiTurnRestarted
    deriving (Eq, Show)

initialUiState :: UiState
initialUiState = UiState
    { uiBlocks = Seq.empty
    , uiNextBlockId = 1
    , uiDraft = ""
    , uiCursor = 0
    , uiQueuedInputs = Seq.empty
    , uiFocus = FocusComposer
    , uiSelectedBlock = Nothing
    , uiSelectedBlockIndex = Nothing
    , uiBlockIndices = Map.empty
    , uiFollow = True
    , uiRunning = False
    , uiAwaitingInput = False
    , uiActivity = "Ready"
    , uiPrompt = PromptState
        { promptModel = ""
        , promptEffort = ""
        , promptMode = "ask"
        , promptAccount = ""
        , promptAccountSelectable = False
        , promptUsage = emptyTokenUsage
        , promptLimitStatus = Nothing
        , promptAttachments = 0
        }
    , uiBranch = ""
    , uiCwd = ""
    , uiPermission = Nothing
    , uiNotice = Nothing
    , uiRetryCountdown = Nothing
    , uiNoticeElapsedMillis = 0
    , uiElapsedMillis = 0
    , uiCompletionRemainingMillis = 0
    , uiTurnStartBlock = 0
    , uiAttemptStartBlock = 0
    , uiToolCalls = Map.empty
    }

-- | Status-only blocks can appear before the first user turn, but the
-- conversation is still empty from the user's perspective.
conversationIsEmpty :: UiState -> Bool
conversationIsEmpty state =
    all isStartupStatus state.uiBlocks
  where
    isStartupStatus block =
        block.blockKind == BlockSystem
            && block.blockTitle == "System"

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
            , uiCursor = max 0 (min (Text.length text) cursor)
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
            , uiActivity =
                if awaiting && state.uiCompletionRemainingMillis == 0
                    then "Ready"
                    else state.uiActivity
            }
    UiSetRepository branch cwd ->
        state { uiBranch = branch, uiCwd = cwd }
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
            , uiActivity = "Restarting…"
            , uiNotice =
                Just (progressNotice "Restarting current turn…")
            , uiNoticeElapsedMillis = 0
            , uiCompletionRemainingMillis = 0
            , uiToolCalls = Map.empty
            , uiAttemptStartBlock = state.uiTurnStartBlock
            }

advanceUiTime :: Int -> UiState -> UiState
advanceUiTime rawElapsedMillis state =
    let
        elapsedMillis = max 0 rawElapsedMillis
        completionRemainingMillis =
            max 0
                (state.uiCompletionRemainingMillis - elapsedMillis)
        noticeElapsedMillis =
            case state.uiNotice of
                Just notice
                    | notice.noticeTransient ->
                        state.uiNoticeElapsedMillis + elapsedMillis
                _ ->
                    0
        notice
            | noticeElapsedMillis >= transientNoticeDurationMillis
            , maybe False (.noticeTransient) state.uiNotice =
                Nothing
            | otherwise = state.uiNotice
        countdown =
            advanceRetryCountdown elapsedMillis state
    in countdown
        { uiElapsedMillis =
            if state.uiRunning
                then state.uiElapsedMillis + elapsedMillis
                else state.uiElapsedMillis
        , uiActivity =
            if state.uiCompletionRemainingMillis > 0
                && completionRemainingMillis == 0
                then "Ready"
                else state.uiActivity
        , uiCompletionRemainingMillis = completionRemainingMillis
        , uiNotice = notice
        , uiNoticeElapsedMillis =
            if notice == Nothing then 0 else noticeElapsedMillis
        }

uiNeedsTick :: UiState -> Bool
uiNeedsTick state =
    state.uiRunning
        || state.uiCompletionRemainingMillis > 0
        || maybe False ((> 0) . (.retryCountdownRemainingMillis))
            state.uiRetryCountdown
        || maybe False noticeNeedsTick state.uiNotice
  where
    noticeNeedsTick notice =
        notice.noticeTransient

uiNextDeadlineMillis :: UiState -> Maybe Int
uiNextDeadlineMillis state =
    minimumMaybe $
        completionDeadline <> countdownDeadline <> noticeDeadline
  where
    completionDeadline =
        [state.uiCompletionRemainingMillis
        | state.uiCompletionRemainingMillis > 0]
    countdownDeadline =
        case state.uiRetryCountdown of
            Just countdown ->
                [ min
                    countdown.retryCountdownRemainingMillis
                    (millisecondsUntilNextDisplayedSecond
                        countdown.retryCountdownRemainingMillis)
                ]
            Nothing -> []
    noticeDeadline =
        case state.uiNotice of
            Just notice
                | notice.noticeTransient ->
                    [ max 0
                        (transientNoticeDurationMillis
                            - state.uiNoticeElapsedMillis)
                    ]
            _ ->
                []
    minimumMaybe [] = Nothing
    minimumMaybe values = Just (minimum values)

advanceRetryCountdown :: Int -> UiState -> UiState
advanceRetryCountdown elapsedMillis state =
    case state.uiRetryCountdown of
        Nothing -> state
        Just countdown ->
            let
                remaining =
                    max 0
                        ( countdown.retryCountdownRemainingMillis
                            - elapsedMillis
                        )
                body =
                    retryCountdownText
                        countdown.retryCountdownPrefix
                        remaining
                        countdown.retryCountdownSuffix
                blocks =
                    case Map.lookup
                        countdown.retryCountdownBlockId
                        state.uiBlockIndices of
                        Nothing -> state.uiBlocks
                        Just index ->
                            Seq.adjust
                                (\block -> block { blockBody = body })
                                index
                                state.uiBlocks
            in state
                { uiBlocks = blocks
                , uiRetryCountdown =
                    if remaining == 0
                        then Nothing
                        else Just countdown
                            { retryCountdownRemainingMillis = remaining }
                }

retryCountdownText :: Text -> Int -> Text -> Text
retryCountdownText prefix remainingMillis suffix =
    prefix
        <> (if remainingMillis <= 0
                then "Try again now"
                else
                    "Try again in "
                        <> formatCountdownSeconds
                            ((remainingMillis + 999) `div` 1000))
        <> suffix

formatCountdownSeconds :: Int -> Text
formatCountdownSeconds rawSeconds =
    let
        total = max 0 rawSeconds
        hours = total `div` 3600
        minutes = (total `mod` 3600) `div` 60
        seconds = total `mod` 60
        showText = Text.pack . show
        pad2 value
            | value < 10 = "0" <> showText value
            | otherwise = showText value
    in if hours > 0
        then showText hours <> "h" <> pad2 minutes <> "m" <> pad2 seconds <> "s"
        else if minutes > 0
            then showText minutes <> "m" <> pad2 seconds <> "s"
            else showText seconds <> "s"

millisecondsUntilNextDisplayedSecond :: Int -> Int
millisecondsUntilNextDisplayedSecond remainingMillis =
    let remainder = remainingMillis `mod` 1000
    in if remainder == 0 then 1000 else remainder

reduceLoop :: LoopEvent -> UiState -> UiState
reduceLoop event state = case event of
    TurnStarted ->
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
        appendOrExtend BlockThinking "Thought" delta BlockStreaming state
            { uiActivity = "Thinking…" }
    TextDelta delta ->
        appendOrExtend BlockAssistant "Assistant" delta BlockStreaming state
            { uiActivity = "Writing…" }
    ActivityUpdated activity ->
        state { uiActivity = activity }
    WarningRaised warning ->
        state
            { uiNotice = Just (warningNotice warning)
            , uiNoticeElapsedMillis = 0
            }
    ResponseRestarted message ->
        let finalized = finalizeStreams state
        in finalized
            { uiRunning = True
            , uiActivity = "Retrying response…"
            , uiNotice = Just (warningNotice message)
            , uiNoticeElapsedMillis = 0
            , uiAttemptStartBlock = Seq.length finalized.uiBlocks
            }
    ToolStarted call ->
        let
            kind = toolBlockKind call.name
            title = toolCallTitle call
            blockIndex = Seq.length state.uiBlocks
            body = case call.name of
                "search_replace" ->
                    formatSearchReplaceDiff call.arguments
                _ -> ""
            detail = toolCallInput call
        in appendBlock kind title body detail
            BlockRunning (Just call.callId)
            state
                { uiRunning = True
                , uiAwaitingInput = False
                , uiActivity = title
                , uiToolCalls =
                    Map.insert
                        call.callId
                        (blockIndex, call)
                        state.uiToolCalls
                }
    ToolOutputUpdated callId output ->
        updateToolOutput callId output state
    ToolFinished result ->
        let activeCall = Map.lookup result.callId state.uiToolCalls
            displayed = case activeCall of
                Nothing -> result
                Just (_, call) ->
                    result
                        { output = formatToolOutput call result.output }
            next =
                state
                    { uiRunning = True
                    , uiAwaitingInput = False
                    , uiActivity = "Thinking…"
                    , uiToolCalls =
                        Map.delete result.callId state.uiToolCalls
                    }
        in case activeCall of
            Nothing -> next
            Just (blockIndex, _) ->
                completeTool blockIndex displayed next
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
        in withFallback
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
                kind `elem` [BlockUser, BlockAssistant, BlockSystem, BlockError]
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
    state
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
    case Map.lookup ident state.uiBlockIndices of
        Nothing -> state
        Just index -> case Seq.lookup index state.uiBlocks of
            Just block
                | block.blockId == ident ->
                    state
                        { uiSelectedBlock = Just ident
                        , uiSelectedBlockIndex = Just index
                        , uiFollow =
                            index == Seq.length state.uiBlocks - 1
                        }
            _ -> state

selectedBlockIndex :: UiState -> Int
selectedBlockIndex state =
    maybe fallback fst (selectedBlockEntry state)
  where
    fallback = max 0 (Seq.length state.uiBlocks - 1)

-- | Delete whitespace and the previous non-whitespace word before the cursor.
deleteWordBefore :: Text -> Int -> (Text, Int)
deleteWordBefore text cursor =
    let cursor' = max 0 (min (Text.length text) cursor)
        before = Text.take cursor' text
        after = Text.drop cursor' text
        reversed = Text.reverse before
        withoutSpace = Text.dropWhile isSpace reversed
        remaining = Text.dropWhile (not . isSpace) withoutSpace
        kept = Text.reverse remaining
        after'
            | Text.isSuffixOf " " kept
            , Text.isPrefixOf " " after =
                Text.drop 1 after
            | otherwise = after
    in (kept <> after', Text.length kept)

-- | Delete from the cursor to the beginning of its logical line.
deleteToLineStart :: Text -> Int -> (Text, Int)
deleteToLineStart text cursor =
    let cursor' = max 0 (min (Text.length text) cursor)
        before = Text.take cursor' text
        after = Text.drop cursor' text
        kept = case Text.breakOnEnd "\n" before of
            ("", _) -> ""
            (prefix, _) -> prefix
    in (kept <> after, Text.length kept)

-- | Delete from the cursor to the end of its logical line.
deleteToLineEnd :: Text -> Int -> (Text, Int)
deleteToLineEnd text cursor =
    let cursor' = max 0 (min (Text.length text) cursor)
        before = Text.take cursor' text
        after = Text.drop cursor' text
        keptAfter = case Text.break (== '\n') after of
            (_, rest) -> rest
    in (before <> keptAfter, cursor')

lineStartCursor :: Text -> Int -> Int
lineStartCursor text cursor =
    let cursor' = max 0 (min (Text.length text) cursor)
        before = Text.take cursor' text
    in Text.length (fst (Text.breakOnEnd "\n" before))

lineEndCursor :: Text -> Int -> Int
lineEndCursor text cursor =
    let cursor' = max 0 (min (Text.length text) cursor)
        after = Text.drop cursor' text
        (line, _) = Text.break (== '\n') after
    in cursor' + Text.length line

moveWordLeft :: Text -> Int -> Int
moveWordLeft text cursor =
    let cursor' = max 0 (min (Text.length text) cursor)
        reversed = Text.reverse (Text.take cursor' text)
        withoutSpace = Text.dropWhile isSpace reversed
        remaining = Text.dropWhile (not . isSpace) withoutSpace
    in Text.length remaining

moveWordRight :: Text -> Int -> Int
moveWordRight text cursor =
    let cursor' = max 0 (min (Text.length text) cursor)
        after = Text.drop cursor' text
        withoutWord = Text.dropWhile (not . isSpace) after
        remaining = Text.dropWhile isSpace withoutWord
    in Text.length text - Text.length remaining

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

toolBlockKind :: Text -> BlockKind
toolBlockKind rawName
    | name `elem` ["run_terminal_cmd", "shell_command", "write_stdin", "run_ghci"] =
        BlockShell
    | name `elem` ["search_replace", "apply_patch"] =
        BlockEdit
    | otherwise = BlockTool
  where
    name = canonicalToolName rawName

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
