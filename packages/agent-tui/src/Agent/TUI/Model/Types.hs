-- | Data model for the retained terminal UI.
module Agent.TUI.Model.Types
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
    ) where

import Agent.Loop (LoopEvent, TokenUsage)
import Agent.ToolDispatch (ToolCall)
import Agent.TUI.Presentation (TodoDisplayLine)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import Data.Text (Text)

newtype BlockId = BlockId Int
    deriving (Eq, Ord, Show)

data BlockKind
    = BlockUser
    | BlockAssistant
    | BlockThinking
    | BlockTool
    | BlockInspect
    | BlockTodo
    | BlockShell
    | BlockEdit
    | BlockSystem
    | BlockRecap
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
    , promptEffortOptions :: ![Text]
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
    , uiWorkspaceRoot :: !Text
    , uiPermission :: !(Maybe PermissionOverlay)
    , uiNotice :: !(Maybe UiNotice)
    , uiRetryCountdown :: !(Maybe RetryCountdown)
    , uiNoticeElapsedMillis :: !Int
    , uiElapsedMillis :: !Int
    , uiCompletionRemainingMillis :: !Int
    , uiTurnStartBlock :: !Int
    , uiAttemptStartBlock :: !Int
    , uiToolCalls :: !(Map.Map Text (Int, ToolCall))
    -- | Background shell session IDs mapped to the block that owns their
    -- lifecycle. Empty write_stdin calls update that block instead of adding
    -- one retained block per poll.
    , uiShellProcesses :: !(Map.Map Int BlockId)
    -- | Empty write_stdin call IDs currently waiting on a known shell session.
    , uiShellPolls :: !(Map.Map Text Int)
    , uiTodos :: ![TodoDisplayLine]
    , uiGenerating :: !Bool
    , uiGenerationChars :: !Int
    , uiGenerationMillis :: !Int
    , uiGenerationLastDeltaMillis :: !Int
    , uiResponseMillis :: !Int
    , uiLastTokensPerSecond :: !(Maybe Double)
    }
    deriving (Eq, Show)

data UiEvent
    = UiLoop !LoopEvent
    | UiUserSubmitted !Text
    | UiDraftSubmitted
    | UiInputSteered !Text
    | UiInputQueued !Text
    | UiInputPromoted !Text
    | UiQueuedInputStarted
    | UiSetDraft !Text !Int
    | UiSetPrompt !PromptState
    | UiSetPromptTarget !Text !Text
    | UiSetPromptEffort !Text
    | UiSetPromptLimitStatus !(Maybe PromptLimitStatus)
    | UiSetAwaitingInput !Bool
    | UiSetRepository !Text !Text !Text
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
    | UiRecapStarted
    | UiRecapReady !Text
    | UiRecapUnavailable !Text
    | UiErrorMessage !Text
    -- | Append an error whose retry guidance counts down in place.
    | UiRetryCountdown !Text !Int !Text
    | UiConversationCleared
    | UiSetFollow !Bool
    | UiTurnEnded !BlockState
    | UiTurnRestarted
    deriving (Eq, Show)
