-- | Shared types for the retained fullscreen terminal application.
module Agent.CLI.TUI.Types
    ( AppEvent(..)
    , AppEventMailbox(..)
    , AppState(..)
    , AgentHover(..)
    , ChoicePresentation(..)
    , ChoiceOverlay(..)
    , FullscreenInput(..)
    , FullscreenInputBuffer(..)
    , FullscreenRuntime(..)
    , FullscreenSessionActions(..)
    , Name(..)
    , PendingAppEvent(..)
    , PendingUiEvent(..)
    , ResumeOverlay(..)
    , TextOverlay(..)
    ) where

import Agent.CLI.AgentViewport (AgentEntry, AgentTarget)
import Agent.CLI.Command (SkillCommand)
import Agent.CLI.Input (ReplLine)
import Agent.CLI.Interrupt (CtrlCDecision)
import Agent.CLI.Permission (PermissionChoice)
import Agent.CLI.Resume (ResumeBrowser, ResumeEntry)
import Agent.CLI.TUI.ImagePreview (TuiImagePreview)
import qualified Agent.CLI.TUI.Scroll as Scroll
import Agent.Loop (ImageAttachment)
import Agent.TUI.Model (BlockId, UiEvent, UiState)
import Agent.Syntax (SyntaxHighlighter)
import Agent.TUI.Motion (MotionDemand, MotionMode)
import Brick (Location)
import Brick.BChan (BChan)
import Control.Concurrent.STM (TMVar, TVar)
import Control.Exception.Safe (SomeException)
import Data.IORef (IORef)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime)
import Data.Word (Word64)

data Name
    = ConversationViewport
    | ConversationReserve
    | OverlayViewport
    | ConversationBlock !BlockId
    | ConversationBlockCache
        !BlockId
        !Bool
        !Bool
        !(Maybe (Int, Bool))
    | CodeBlockCache !BlockId !Int
    | CodeCopy !BlockId !Int
    | ComposerArea
    | ComposerCursor
    | ComposerModel
    | ComposerEffort
    | ComposerMode
    | ComposerAccount
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
        !Text
        !Text
        !Text
        !(TMVar (Maybe Text))
    | AppAskResume
        !ResumeBrowser
        !(Text -> IO (Either Text ResumeEntry))
        !(Text -> IO (Either Text ()))
        !(TMVar (Maybe ResumeEntry))
    | forall a. AppSuspend !(IO a) !(TMVar (Either SomeException a))
    | AppSetSkillCommands ![SkillCommand]
    | AppSetImagePreviews ![ImageAttachment]
    | AppAgentSnapshot !AgentTarget ![AgentEntry]
    | AppSetWindowTitle !Text
    | AppSyntaxHighlighterLoaded !(Maybe SyntaxHighlighter)
    | AppConversationReflow
    | AppMotionTick
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

data FullscreenRuntime = FullscreenRuntime
    { runtimeEvents :: !(BChan AppEvent)
    , runtimeMailbox :: !AppEventMailbox
    , runtimeInput :: !FullscreenInputBuffer
    , runtimeCancel :: !(IO ())
    , runtimeRestartEffort :: !(Text -> IO ())
    , runtimeCtrlC :: !(IO CtrlCDecision)
    , runtimeCopy :: !(Text -> IO Bool)
    , runtimeSetWindowTitle :: !(Text -> IO ())
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
    , runtimeLoadSyntaxHighlighter
        :: !(IO (Either Text SyntaxHighlighter))
    , runtimeSyntaxLoadFinished :: !(NominalDiffTime -> IO ())
    , runtimeInitial :: !UiState
    , runtimeSessionActions :: !(IORef FullscreenSessionActions)
    }

-- | Provider/session-scoped actions behind one long-lived terminal runtime.
-- Replacing the record atomically prevents the retained UI from calling into
-- resources belonging to a backend that has already shut down.
data FullscreenSessionActions = FullscreenSessionActions
    { sessionCancel :: !(IO ())
    , sessionRestartEffort :: !(Text -> IO ())
    , sessionCtrlC :: !(IO CtrlCDecision)
    , sessionAgentSnapshot :: !(IO (AgentTarget, [AgentEntry]))
    , sessionAgentSelect :: !(AgentTarget -> IO ())
    }

data AppState = AppState
    { appUi :: !UiState
    , appPermissionReply :: !(Maybe (TMVar (Maybe PermissionChoice)))
    , appRuntime :: !FullscreenRuntime
    , appSlashIndex :: !Int
    , appChoice :: !(Maybe ChoiceOverlay)
    , appChoiceReply :: !(Maybe (Maybe Int -> IO ()))
    , appResume :: !(Maybe ResumeOverlay)
    , appResumeReply :: !(Maybe (TMVar (Maybe ResumeEntry)))
    , appResumeLoad :: !(Maybe (Text -> IO (Either Text ResumeEntry)))
    , appResumeDelete :: !(Maybe (Text -> IO (Either Text ())))
    , appTextPrompt :: !(Maybe TextOverlay)
    , appTextReply :: !(Maybe (TMVar (Maybe Text)))
    , appSlashDismissed :: !Bool
    , appPasted :: !Bool
    , appHistory :: ![Text]
    , appHistoryIndex :: !(Maybe Int)
    , appHistoryDraft :: !Text
    , appKillBuffer :: !Text
    , appSkillCommands :: ![SkillCommand]
    , appImagePreviews :: ![TuiImagePreview]
    , appAgentSelected :: !AgentTarget
    , appAgentEntries :: ![AgentEntry]
    , appAgentHover :: !(Maybe AgentHover)
    , appHoveredControl :: !(Maybe Name)
    , appPressedControl :: !(Maybe Name)
    , appWorkerStopped :: !Bool
    , appConversationAnchor :: !(Maybe Scroll.ConversationAnchor)
    , appConversationReflowQueued :: !Bool
    , appWindowTitle :: !(Maybe Text)
    , appMotionElapsedMillis :: !Int
    , appCompletionFlashes :: !(Map.Map BlockId Int)
    , appMotionScheduleReset :: !Bool
    , appClockNanos :: !Word64
    , appNativeProgressKeepaliveBucket :: !Int
    , appSyntaxHighlighter :: !(Maybe SyntaxHighlighter)
    }

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
    }

data ResumeOverlay = ResumeOverlay
    { resumeOverlayBrowser :: !ResumeBrowser
    }

data TextOverlay = TextOverlay
    { textTitle :: !Text
    , textBody :: !Text
    , textDraft :: !Text
    , textCursor :: !Int
    }
