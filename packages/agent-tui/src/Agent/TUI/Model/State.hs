-- | Initial state and top-level retained UI queries.
module Agent.TUI.Model.State
    ( initialUiState
    , visibleTodoList
    , conversationIsEmpty
    ) where

import Agent.Loop (emptyTokenUsage)
import Agent.TUI.Model.Types
import Agent.TUI.Presentation
    ( TodoDisplayLine
    , todoListHasInProgress
    , todoListHasOpenWork
    )
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq

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
        , promptEffortOptions = []
        , promptMode = "ask"
        , promptAccount = ""
        , promptAccountSelectable = False
        , promptUsage = emptyTokenUsage
        , promptLimitStatus = Nothing
        , promptAttachments = 0
        }
    , uiBranch = ""
    , uiCwd = ""
    , uiWorkspaceRoot = ""
    , uiPermission = Nothing
    , uiNotice = Nothing
    , uiRetryCountdown = Nothing
    , uiNoticeElapsedMillis = 0
    , uiElapsedMillis = 0
    , uiCompletionRemainingMillis = 0
    , uiTurnStartBlock = 0
    , uiAttemptStartBlock = 0
    , uiToolCalls = Map.empty
    , uiShellProcesses = Map.empty
    , uiShellPolls = Map.empty
    , uiTodos = []
    , uiGenerating = False
    , uiGenerationChars = 0
    , uiGenerationMillis = 0
    , uiGenerationLastDeltaMillis = 0
    , uiResponseMillis = 0
    , uiLastTokensPerSecond = Nothing
    }

-- | Checklist shown above the prompt during a turn, or while an item is still
-- in progress. Pending-only lists hide once the session is idle so they do
-- not linger into the next prompt.
visibleTodoList :: UiState -> [TodoDisplayLine]
visibleTodoList state
    | todoListHasInProgress state.uiTodos = state.uiTodos
    | state.uiRunning && todoListHasOpenWork state.uiTodos = state.uiTodos
    | otherwise = []

-- | Status-only blocks can appear before the first user turn, but the
-- conversation is still empty from the user's perspective.
conversationIsEmpty :: UiState -> Bool
conversationIsEmpty state =
    all isStartupStatus state.uiBlocks
  where
    isStartupStatus block =
        block.blockKind == BlockSystem
            && block.blockTitle == "System"
