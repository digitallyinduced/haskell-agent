-- | Shared types for the retained fullscreen terminal application.
module Agent.CLI.TUI.Types
    ( AppEvent(..)
    , AppEventMailbox(..)
    , AppEventMailboxState(..)
    , AppState(..)
    , AgentHover(..)
    , DictationJob(..)
    , DictationSession(..)
    , ChoicePresentation(..)
    , ChoiceOverlay(..)
    , choiceVisibleRows
    , selectedChoiceIndex
    , FullscreenInput(..)
    , FullscreenInputBuffer(..)
    , FullscreenHistorySource(..)
    , HistoryCommit(..)
    , FullscreenRuntime(..)
    , FullscreenSessionActions(..)
    , MetaConsoleOverlay(..)
    , Name(..)
    , PendingAppEvent(..)
    , PendingUiEvent(..)
    , ResumeOverlay(..)
    , SyntaxHighlighterState(..)
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
import Agent.CLI.TUI.ImagePreview
    ( NativePreviewPlacement
    , TuiImagePreview
    )
import Agent.CLI.TUI.History
    ( HistoryGeneration
    , HistoryPage
    , HistoryRequest
    , HistoryTurn
    , HistoryWindow
    )
import qualified Agent.CLI.TUI.Scroll as Scroll
import Agent.Loop (ImageAttachment)
import Agent.Provider (Provider)
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
import qualified Data.Text as Text
import Data.Time.Clock (NominalDiffTime)
import Data.Word (Word64)
import qualified Graphics.Vty as V

data Name
    = ConversationViewport
    | ConversationViewportExtent
    | ConversationReserve
    | ConversationImage !BlockId !Int
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
    | ComposerImageRemove !Int
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
    | MetaConsoleCursor
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
    | AppAskFilterChoice
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
    | AppToolImage !Text !TuiImagePreview
      -- ^ An image the agent displayed through @show_image@, keyed by the
      -- originating tool call id.
    | AppDictationPartial !Text
    | AppDictationFinished !(Either Text Text)
    | AppAgentSnapshot !AgentTarget ![AgentEntry]
    | AppSetWindowTitle !Text
    | AppSyntaxHighlighterChanged
    | AppHistoryReset !HistoryPage
    | AppHistoryLoaded
        !HistoryRequest
        !(Either Text HistoryPage)
    | AppHistoryCommitted
        !HistoryGeneration
        !HistoryTurn
        !HistoryCommit
    | AppHistoryLiveStarted
    | AppConversationReflow
    | AppSyncSubmittedImagePlacements
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

data AppEventMailboxState = AppEventMailboxState
    { mailboxPendingEvents :: !(Seq PendingAppEvent)
    , mailboxPendingCount :: !Int
    , mailboxPendingBytes :: !Int
    , mailboxHighWaterCount :: !Int
    , mailboxHighWaterBytes :: !Int
    }

newtype AppEventMailbox =
    AppEventMailbox (TVar AppEventMailboxState)

data FullscreenInput = FullscreenInput
    { fullscreenInputLine :: !ReplLine
    , fullscreenInputQueued :: !Bool
    , fullscreenInputDisplay :: !(Maybe Text)
    }

-- | Runtime ownership of the syntax grammar cache. Keeping the active bit and
-- cache in one 'IORef' makes focus-loss eviction atomic with worker updates:
-- an XML load which finishes late cannot restore a cache after the terminal
-- has moved into the background.
data SyntaxHighlighterState
    = SyntaxHighlighterUnloaded !Word64
      -- ^ Enabled, but the lightweight syntax index has not been loaded yet.
    | SyntaxHighlighterActive !Word64 !(Maybe SyntaxHighlighter)
      -- ^ Loading was attempted; 'Nothing' records a failed initializer.
    | SyntaxHighlighterInactive !Word64

data FullscreenInputBuffer =
    FullscreenInputBuffer
        !(TVar (Seq FullscreenInput))
        !(TVar Int)

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
    , runtimeSteer :: !(Text -> IO (Either Text ()))
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
    , runtimeSubmittedImagePlacements
        :: !(IORef [NativePreviewPlacement])
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
        :: !(IORef SyntaxHighlighterState)
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
    { sessionProvider :: !(Maybe Provider)
    , sessionCancel :: !(IO ())
    , sessionSteer :: !(Text -> IO (Either Text ()))
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
    , appMetaConsole :: !(Maybe MetaConsoleOverlay)
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
      -- | Previews attached to conversation blocks: images the user
      -- submitted with a prompt and images the agent displayed from a tool
      -- call. Native placements are synchronized from this map after each
      -- reflow.
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
    , choiceSearch :: !Bool
    , choiceQuery :: !Text
    , choiceCloseOnTurnEnd :: !Bool
    }

-- | Rows currently visible in a choice overlay.  Searchable overlays retain
-- each row's source index so callers can return the original choice even
-- after filtering; ordinary overlays expose the identity mapping.
choiceVisibleRows :: ChoiceOverlay -> [(Int, (Text, Text))]
choiceVisibleRows choice
    | not choice.choiceSearch = zip [0 ..] choice.choiceRows
    | otherwise =
        filter (matches choice.choiceQuery . snd)
            (zip [0 ..] choice.choiceRows)
  where
    matches query (label, detail)
        | Text.null needle = True
        | otherwise =
            needle `Text.isInfixOf` Text.toCaseFold label
                || needle `Text.isInfixOf` Text.toCaseFold detail
      where
        needle = Text.toCaseFold query

-- | Resolve the selected row to its source index.  Empty searchable results
-- have no selection; static choices retain their historical index behavior.
selectedChoiceIndex :: ChoiceOverlay -> Maybe Int
selectedChoiceIndex choice
    | not choice.choiceSearch = Just choice.choiceIndex
    | otherwise =
        fst <$> listAt choice.choiceIndex (choiceVisibleRows choice)
  where
    listAt index values
        | index < 0 = Nothing
        | otherwise = case drop index values of
            value : _ -> Just value
            [] -> Nothing

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

data MetaConsoleOverlay = MetaConsoleOverlay
    { metaConsoleDraft :: !Text
    , metaConsoleCursor :: !Int
    }

data TextInputMode
    = TextInputPlain
    | TextInputSecret
    deriving (Eq, Show)
