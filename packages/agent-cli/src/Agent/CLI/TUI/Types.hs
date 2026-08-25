-- | Shared types for the retained fullscreen terminal application.
module Agent.CLI.TUI.Types
    ( AppEvent(..)
    , AppEventMailbox(..)
    , AppState(..)
    , AgentHover(..)
    , DictationJob(..)
    , DictationSession(..)
    , ChoicePresentation(..)
    , ChoiceOverlay(..)
    , FullscreenInput(..)
    , FullscreenInputBuffer(..)
    , FullscreenHistorySource(..)
    , HistoryCommit(..)
    , FullscreenRuntime(..)
    , FullscreenSessionActions(..)
    , Name(..)
    , PendingAppEvent(..)
    , PendingUiEvent(..)
    , ResumeOverlay(..)
    , TerminalFocus(..)
    , TextInputMode(..)
    , TextOverlay(..)
    ) where

import Agent.CLI.AgentViewport (AgentEntry, AgentTarget)
import Agent.CLI.Command (SkillCommand, SlashCatalog)
import Agent.CLI.Input.Types (ReplLine)
import Agent.CLI.Interrupt (CtrlCDecision)
import Agent.CLI.Permission (PermissionChoice)
import Agent.CLI.Resume (ResumeBrowser, ResumeEntry)
import Agent.CLI.TUI.ImagePreview (TuiImagePreview)
import Agent.CLI.TUI.History
    ( HistoryGeneration
    , HistoryPage
    , HistoryRequest
    , HistoryTurn
    , HistoryWindow
    )
import qualified Agent.CLI.TUI.Scroll as Scroll
import Agent.Loop (ImageAttachment)
import Agent.TUI.Model (BlockId, UiEvent, UiState)
import Agent.Syntax (SyntaxHighlighter)
import Agent.TUI.Motion (MotionDemand, MotionMode)
import Brick (Location)
import Brick.BChan (BChan)
import Control.Concurrent (MVar)
import Control.Concurrent.STM (TMVar, TQueue, TVar)
import Control.Exception.Safe (SomeException)
import Data.IORef (IORef)
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime)
import Data.Word (Word64)
import qualified Graphics.Vty as V

data Name
    = ConversationViewport
    | ConversationReserve
    | OverlayViewport
    | ConversationBlock !AgentTarget !BlockId
    | ConversationChunkCache
        !AgentTarget
        !BlockId
        !BlockId
    | ConversationBlockCache
        !AgentTarget
        !BlockId
        !Bool
        !Bool
        !(Maybe (Int, Bool))
    | ConversationBodyCache
        !AgentTarget
        !BlockId
        !Bool
    | CodeBlockCache !AgentTarget !BlockId !Int
    | CodeCopy !AgentTarget !BlockId !Int
    | MarkdownLink !Text
    | ComposerArea
    | ComposerCursor
    | ComposerModel
    | ComposerEffort
    | ComposerMode
    | ComposerAccount
    | QuickStartWorktree
    | QuickStartResume
    | QuickStartCommands
    | QuickStartModel
    | ChoiceRow !Int
    | ResumeViewport
    | ResumeRow !Text
    | ResumeSearchCursor
    | PermissionRow !Int
    | SlashRow !Int
    | OverlayCursor
    | AgentPane
    | AgentRow !AgentTarget
    | AgentPopover !AgentTarget
    deriving (Eq, Ord, Show)

data AppEvent
    = AppUi !UiEvent
    | AppUiBatch !(NonEmpty UiEvent)
    | AppAskPermission !Text !(TMVar (Maybe PermissionChoice))
    | AppAskChoice
        !ChoicePresentation
        !Text
        !Text
        !Int
        ![(Text, Text)]
        !(TMVar (Maybe Int))
    | AppAskText
        !TextInputMode
        !Text
        !Text
        !Text
        !(TMVar (Maybe Text))
    | AppAskResume
        !ResumeBrowser
        !(Text -> IO (Either Text ResumeEntry))
        !(Text -> IO (Either Text ()))
        !(Text -> IO (Either Text [ResumeEntry]))
        !(TMVar (Maybe ResumeEntry))
    | forall a. AppSuspend !(IO a) !(TMVar (Either SomeException a))
    | AppSetSlashCatalog !SlashCatalog
      -- ^ Atomically replace commands, capabilities, skills, and model ids.
    | AppSetSkillCommands ![SkillCommand]
      -- ^ Legacy compatibility; prefer 'AppSetSlashCatalog'.
    | AppSetModelIds ![Text]
      -- ^ Legacy compatibility; prefer 'AppSetSlashCatalog'.
    | AppSetImagePreviews ![(ImageAttachment, TuiImagePreview)]
    | AppCommitImagePreviews ![(ImageAttachment, TuiImagePreview)]
    | AppDictationPartial !Text
    | AppDictationFinished !(Either Text Text)
    | AppAgentSnapshot !AgentTarget ![AgentEntry]
    | AppSetWindowTitle !Text
    | AppSyntaxHighlighterLoaded !(Maybe SyntaxHighlighter)
    | AppHistoryReset !HistoryPage
    | AppHistoryLoaded
        !HistoryRequest
        !(Either Text HistoryPage)
    | AppHistoryCommitted
        !HistoryGeneration
        !HistoryTurn
        !HistoryCommit
    | AppConversationReflow
    | AppMotionTick
    | AppRecapPoll
    | AppStop

data PendingAppEvent
    = PendingEvent !AppEvent
    | PendingUi !PendingUiEvent

data PendingUiEvent
    = PendingExactUi !UiEvent
    | PendingTextDeltas !(Seq Text)
    | PendingReasoningDeltas !(Seq Text)

newtype AppEventMailbox =
    AppEventMailbox (TVar (Seq PendingAppEvent))

data FullscreenInput = FullscreenInput
    { fullscreenInputLine :: !ReplLine
    , fullscreenInputQueued :: !Bool
    , fullscreenInputDisplay :: !(Maybe Text)
    }

newtype FullscreenInputBuffer =
    FullscreenInputBuffer (TVar (Seq FullscreenInput))

data FullscreenHistorySource = FullscreenHistorySource
    { historySourceKey :: !Text
    , historySourceLoad
        :: !(HistoryRequest -> IO (Either Text HistoryPage))
    }

data HistoryCommit
    = HistoryCommitAppend
    | HistoryCommitReplace
    | HistoryCommitReset
    deriving (Eq, Show)

data FullscreenRuntime = FullscreenRuntime
    { runtimeEvents :: !(BChan AppEvent)
    , runtimeMailbox :: !AppEventMailbox
    , runtimeInput :: !FullscreenInputBuffer
    , runtimeCancel :: !(IO ())
    , runtimeSteer :: !(Text -> IO ())
    , runtimeBtw :: !(Text -> IO ())
    , runtimeRecap :: !(IO ())
    , runtimeRestartEffort :: !(Text -> IO ())
    , runtimeCtrlC :: !(IO CtrlCDecision)
    , runtimeCopy :: !(Text -> IO Bool)
    , runtimeSetWindowTitle :: !(Text -> IO ())
    , runtimeWindowTitle :: !(IORef (Maybe Text))
    , runtimeNativeProgress :: !(Bool -> IO ())
    , runtimeAgentSnapshot :: !(IO (AgentTarget, [AgentEntry]))
    , runtimeAgentSelect :: !(AgentTarget -> IO ())
    , runtimeFirstFrame :: !(IO ())
    , runtimeMotionSchedule :: !(TVar (MotionDemand, Int, Int))
    , runtimeMotionTickQueued :: !(TVar Bool)
    , runtimeMotionMode :: !MotionMode
    , runtimeImagePreviews :: !(IORef [(ImageAttachment, TuiImagePreview)])
    , runtimeImagePreviewRevision :: !(IORef Int)
    , runtimeImagePreviewVisible :: !(IORef Bool)
    , runtimeImagePreviewIdBase :: !Int
    , runtimeNativeImagePreviews :: !Bool
    , runtimeColor :: !Bool
    , runtimeWaveTrough :: !V.Color
    , runtimeLoadSyntaxHighlighter
        :: !(IO (Either Text SyntaxHighlighter))
    , runtimeSyntaxLoadFinished :: !(NominalDiffTime -> IO ())
    , runtimeSyntaxRequests :: !(TQueue Text)
    , runtimeSyntaxHighlighter
        :: !(IORef (Maybe SyntaxHighlighter))
    , runtimeInitial :: !UiState
    , runtimeSessionActions :: !(IORef FullscreenSessionActions)
    , runtimeHistoryRequests :: !(TQueue HistoryRequest)
    , runtimeHistorySource :: !(IORef (Maybe FullscreenHistorySource))
    , runtimeHistoryGeneration :: !(IORef Int64)
    , runtimeDictationJobs :: !(TQueue DictationJob)
    }

data DictationJob = DictationJob
    { dictationJobWaitForStop :: IO ()
    }

data DictationSession = DictationSession
    { dictationStop :: !(MVar ())
    , dictationAbort :: !(IORef Bool)
    }

-- | Provider/session-scoped actions behind one long-lived terminal runtime.
-- Replacing the record atomically prevents the retained UI from calling into
-- resources belonging to a backend that has already shut down.
data FullscreenSessionActions = FullscreenSessionActions
    { sessionCancel :: !(IO ())
    , sessionSteer :: !(Text -> IO ())
    , sessionBtw :: !(Text -> IO ())
    , sessionRecap :: !(IO ())
    , sessionRestartEffort :: !(Text -> IO ())
    , sessionCtrlC :: !(IO CtrlCDecision)
    , sessionAgentSnapshot :: !(IO (AgentTarget, [AgentEntry]))
    , sessionAgentSelect :: !(AgentTarget -> IO ())
    }

data AppState = AppState
    { appUi :: !UiState
    , appHistoryWindow :: !HistoryWindow
    , appHistorySelectedBlock :: !(Maybe BlockId)
    , appHistoryLiveStart :: !(Maybe Int)
    , appNextHistoryBlockId :: !Int
    , appPermissionReply :: !(Maybe (TMVar (Maybe PermissionChoice)))
    , appRuntime :: !FullscreenRuntime
    , appSlashIndex :: !Int
    , appChoice :: !(Maybe ChoiceOverlay)
    , appChoiceReply :: !(Maybe (Maybe Int -> IO ()))
    , appResume :: !(Maybe ResumeOverlay)
    , appResumeReply :: !(Maybe (TMVar (Maybe ResumeEntry)))
    , appResumeLoad :: !(Maybe (Text -> IO (Either Text ResumeEntry)))
    , appResumeDelete :: !(Maybe (Text -> IO (Either Text ())))
    , appResumeSearch :: !(Maybe (Text -> IO (Either Text [ResumeEntry])))
    , appTextPrompt :: !(Maybe TextOverlay)
    , appTextReply :: !(Maybe (TMVar (Maybe Text)))
    , appSlashDismissed :: !Bool
    , appPasted :: !Bool
    , appHistory :: ![Text]
    , appHistoryIndex :: !(Maybe Int)
    , appHistoryDraft :: !Text
    , appKillBuffer :: !Text
      -- | True while the previous composer key was a kill command, so a
      -- consecutive kill accumulates into the kill buffer readline-style.
    , appKillChain :: !Bool
      -- | Editor undo log of (draft, cursor) states, most recent first.
    , appUndo :: ![(Text, Int)]
    , appDictation :: !(Maybe DictationSession)
    , appSlashCatalog :: !SlashCatalog
    , appImagePreviews :: ![TuiImagePreview]
    , appSubmittedImagePreviews :: !(Map.Map BlockId [TuiImagePreview])
    , appAgentSelected :: !AgentTarget
    , appAgentEntries :: ![AgentEntry]
    , appAgentHover :: !(Maybe AgentHover)
    , appHoveredControl :: !(Maybe Name)
    , appPressedControl :: !(Maybe Name)
    , appWorkerStopped :: !Bool
    , appConversationAnchor :: !(Maybe Scroll.ConversationAnchor)
    , appFocusLostAt :: !(Maybe Word64)
    , appAutoRecapShownThisAway :: !Bool
    , appLastAutoRecapAttemptAt :: !(Maybe Word64)
    , appLastTurnCompletedAt :: !(Maybe Word64)
    , appConversationReflowQueued :: !Bool
    , appWindowTitle :: !(Maybe Text)
    , appMotionElapsedMillis :: !Int
    , appCompletionFlashes :: !(Map.Map BlockId Int)
    , appMotionScheduleReset :: !Bool
    , appClockNanos :: !Word64
    , appNativeProgressKeepaliveBucket :: !Int
    , appSyntaxHighlighter :: !(Maybe SyntaxHighlighter)
    , appSyntaxRequested :: !(Set.Set Text)
    , appTerminalFocus :: !TerminalFocus
    }

-- | Best-effort focus state reported by the terminal. Unknown preserves the
-- normal rendering cadence for terminals that do not support focus events.
data TerminalFocus
    = TerminalFocusUnknown
    | TerminalFocused
    | TerminalUnfocused
    deriving (Eq, Show)

data AgentHover = AgentHover
    { agentHoverTarget :: !AgentTarget
    , agentHoverUpperLeft :: !Location
    , agentHoverPaneUpperLeft :: !Location
    , agentHoverPaneWidth :: !Int
    }

data ChoicePresentation
    = ChoiceDialog
    | ChoiceOnboarding
    deriving (Eq, Show)

data ChoiceOverlay = ChoiceOverlay
    { choicePresentation :: !ChoicePresentation
    , choiceTitle :: !Text
    , choiceBody :: !Text
    , choiceIndex :: !Int
    , choiceRows :: ![(Text, Text)]
    , choiceCloseOnTurnEnd :: !Bool
    }

data ResumeOverlay = ResumeOverlay
    { resumeOverlayBrowser :: !ResumeBrowser
    }

data TextOverlay = TextOverlay
    { textTitle :: !Text
    , textBody :: !Text
    , textDraft :: !Text
    , textCursor :: !Int
    , textInputMode :: !TextInputMode
    }

data TextInputMode
    = TextInputPlain
    | TextInputSecret
    deriving (Eq, Show)
