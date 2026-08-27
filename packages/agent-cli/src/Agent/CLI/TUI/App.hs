-- This module coordinates IO and Brick events; optimizing its large state
-- machine costs considerably more than it benefits this non-rendering path.
{-# OPTIONS_GHC -O0 -Wno-unused-imports #-}

-- | Retained fullscreen terminal application and its session bridge.
module Agent.CLI.TUI.App
    ( FullscreenInputBuffer
    , FullscreenRuntime
    , advanceCompletionFlashes
    , agentEntryWindow
    , agentPaneEntryLimit
    , agentPaneVisible
    , completionFlashTransitions
    , conversationScrollbarRenderer
    , choiceRowColumns
    , choiceClosesOnUiTransition
    , drawApp
    , elapsedMillisSince
    , emitUiEvent
    , externalUrlCommand
    , hasQueuedFullscreenInput
    , initialFullscreenAppState
    , mergeConversationView
    , motionDemandFor
    , motionDemandForTerminalFocus
    , motionModeForTerminalFocus
    , lambdaArtWidget
    , quickStartRows
    , quickStartVisible
    , nativeProgressKeepaliveDue
    , nextMotionSchedule
    , newFullscreenInputBuffer
    , newFullscreenRuntime
    , newFullscreenRuntimeWithSyntaxLoader
    , selectedAgentConversation
    , loadSyntaxHighlighterForRuntime
    , queuedFullscreenInputDisplays
    , readFullscreenLine
    , readFullscreenLineWithCatalog
    , readFullscreenLineWithModels
    , readFullscreenLineOr
    , readFullscreenLineOrWithCatalog
    , readFullscreenLineOrWithModels
    , repositoryHeaderText
    , resumeSearchCursorColumn
    , onboardingVisibleRowIndices
    , requestFullscreenPermission
    , requestFullscreenChoice
    , requestFullscreenChoiceWithBody
    , requestFullscreenOnboarding
    , requestFullscreenResume
    , requestFullscreenSecret
    , requestFullscreenText
    , runFullscreen
    , commitFullscreenImagePreviews
    , commitFullscreenHistoryTurn
    , beginFullscreenLiveHistory
    , clearFullscreenHistorySource
    , reloadFullscreenHistorySource
    , setFullscreenHistorySource
    , setFullscreenSessionActions
    , fullscreenBounds
    , fullscreenVtyConfig
    , fullscreenSurface
    , wrapFullscreenKeyboardVty
    , withTrackedVtyBuilder
    , setFullscreenImagePreviews
    , setFullscreenWindowTitle
    , applyStoredFullscreenWindowTitle
    , turnCompletionRequiresRedraw
    , syntaxLanguagesForBlocks
    , uiEventRestartsMotionSchedule
    , applyTextPromptEdit
    , maskedSecretText
    , normalizeTextOverlayInsertion
    , textOverlayDisplayText
    , withFullscreenSuspended
    ) where

import Agent.CLI.Clipboard
    ( formatImageSize
    )
import Agent.CLI.Dictation
    ( DictationControl(..)
    , DictationResult(..)
    , dictateWith
    , insertDictation
    )
import Agent.CLI.Secret (sanitizeSecretPromptText)
import Agent.CLI.Artifact (fencedCodeBlock)
import Agent.CLI.Input
    ( ReplLine(..)
    , readReplHistory
    , terminalTextWidth
    , truncateDisplayText
    )
import Agent.CLI.AgentViewport
    ( AgentEntry(..)
    , AgentStep(..)
    , AgentStepState(..)
    , AgentTarget(..)
    , agentDisplayName
    , agentEntryTreeLabelWithGlyphModel
    , agentStatusGlyph
    , lookupAgentEntry
    )
import Agent.CLI.Interrupt (CtrlCDecision(..))
import Agent.CLI.ImagePreview
    ( ImagePreviewProtocol(..)
    , detectImagePreviewProtocol
    , kittyDeleteImageSequence
    , kittyPlacedImageSequence
    , positionImagePayload
    )
import Agent.CLI.Command
    ( SkillCommand
    , SlashCatalog(..)
    , defaultSlashCatalog
    , slashCatalogWithSkills
    )
import Agent.CLI.Permission (PermissionChoice(..))
import Agent.CLI.Resume
    ( ResumeBrowser(..)
    , ResumeEntry(..)
    , applyResumeSearchResults
    , beginResumeSearch
    , cycleResumeSource
    , endResumeSearch
    , groupResumeEntries
    , insertResumeSearch
    , moveResumeBrowser
    , removeResumeEntry
    , replaceResumeEntry
    , resumeRelativeAge
    , resumeSourceLabel
    , selectedResumeBrowser
    , setResumeDeletePending
    , setResumeNotice
    , toggleResumeExpanded
    , visibleResumeBrowser
    )
import Agent.CLI.Render (formatElapsed)
import Agent.CLI.Style (motionGlyphSet)
import Agent.CLI.WindowTitle (oscWindowTitleBytes)
import Agent.CLI.Status (formatTokenUsage)
import Agent.CLI.Timestamp (currentShortMessageTimestamp)
import Agent.CLI.Terminal
    ( TerminalCapabilities(..)
    , detectTerminalCapabilities
    , kittyAltCsiBodies
    , kittyCtrlCsiBodies
    , kittyCtrlUnderscoreCsiBodies
    , kittyKeyboardDisambiguatePush
    , kittyKeyboardPop
    , kittySuperVCsiBodies
    , shiftEnterCsiBodies
    )
import qualified Agent.TUI.Theme as Theme
import qualified Agent.CLI.TUI.Bridge as Bridge
import qualified Agent.CLI.TUI.Composer as Composer
import Agent.CLI.TUI.History
    ( HistoryCursor(..)
    , HistoryDirection(..)
    , HistoryGeneration(..)
    , HistoryPage(..)
    , HistoryRequest(..)
    , HistoryTurn(..)
    , HistoryWindow(..)
    , appendHistoryTurn
    , applyHistoryPage
    , clearHistoryRequest
    , emptyHistoryWindow
    , historyWindowBlock
    , historyWindowOlderAvailable
    , historyWindowRequest
    , unarchivedLiveStart
    , historyWindowSetAnchors
    , markHistoryRequest
    , setHistoryWindowTurns
    )
import Agent.CLI.TUI.LambdaArt
    ( lambdaArtWidget
    )
import Agent.CLI.TUI.Motion
    ( advanceCompletionFlashes
    , appMotionTiming
    , completionFlashTransitions
    , elapsedMillisSince
    , hasBackgroundActivity
    , isBackgroundAgentActive
    , motionDemandFor
    , motionDemandForTerminalFocus
    , motionModeForTerminalFocus
    , nativeProgressKeepaliveDue
    , nextMotionSchedule
    , turnCompletionRequiresRedraw
    , uiEventRestartsMotionSchedule
    , userActionPending
    )
import Agent.CLI.TUI.Render
    ( agentEntryWindow
    , agentPaneEntryLimit
    , agentPaneVisible
    , applyChildConversationUiEvent
    , choiceRowColumns
    , conversationUiForTarget
    , conversationScrollbarRenderer
    , drawApp
    , fullscreenBounds
    , fullscreenSurface
    , onboardingVisibleRowIndices
    , normalizeTextOverlayInsertion
    , maskedSecretText
    , quickStartRows
    , quickStartVisible
    , repositoryHeaderText
    , resumeSearchCursorColumn
    , selectedAgentConversation
    , textOverlayDisplayText
    )
import Agent.CLI.TUI.ImagePreview
    ( NativePreviewPlacement(..)
    , TuiImagePreview(..)
    , nativePreviewPlacements
    , prepareTuiImagePreview
    , previewCountForWidth
    , previewCellSize
    , renderTuiImagePreview
    , sameNativePreviewLayout
    )
import Agent.TUI.Markdown
    ( codeWidgetWithSyntaxHighlighting
    , markdownWidgetWithLinks
    , markdownWidgetWithSyntaxHighlightingAndLinks
    )
import Agent.TUI.FencedCode
    ( FencedBlock(..)
    , fencedBlocks
    )
import Agent.TUI.TextWidth
    ( clampGraphemeCursor
    , displayTerminalText
    , nextGraphemeBoundary
    , previousGraphemeBoundary
    )
import Agent.Syntax
    ( SyntaxHighlighter
    , loadSyntaxLanguage
    , newSyntaxHighlighter
    , resolveFenceLanguage
    )
import qualified Agent.CLI.TUI.Scroll as Scroll
import qualified Agent.CLI.TUI.Transcript as Transcript
import Agent.CLI.TUI.Types
import Agent.TUI.Model
import Agent.TUI.Motion
    ( MotionDemand(..)
    , MotionMode(..)
    , backgroundIndicator
    , completionFlashDurationMillis
    , foregroundIndicator
    , quietIndicator
    , waitingIndicator
    )
import Agent.TUI.Presentation
    ( permissionToolCallPromptRelative )
import Agent.Loop (ImageAttachment(..), LoopEvent(..))
import Agent.ToolDispatch (ToolCall(..))
import Brick
import qualified Brick.Types as B
import Brick.BChan
    ( newBChan
    , writeBChan
    )
import Brick.Widgets.Border (borderWithLabel)
import qualified Brick.Widgets.Border as Border
import Brick.Widgets.Border.Style (unicodeRounded)
import Brick.Widgets.Center (center, centerLayer, hCenter)
import Codec.Picture (pixelAt)
import Control.Applicative ((<|>))
import Control.Concurrent.Async (wait, waitCatch, withAsync)
import Control.Concurrent (threadDelay)
import Control.Monad (forever, unless, void, when, (>=>))
import Control.Concurrent.STM
    ( STM
    , atomically
    , check
    , flushTQueue
    , newEmptyTMVarIO
    , newTQueueIO
    , newTVarIO
    , orElse
    , putTMVar
    , readTVar
    , readTMVar
    , readTQueue
    , registerDelay
    , retry
    , takeTMVar
    , writeTQueue
    , writeTVar
    )
import Agent.CLI.Recap
    ( autoRecapAwayThreshold
    , autoRecapIdleThreshold
    , autoRecapRetryInterval
    )
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (modify')
import Control.Exception.Safe (finally, mask, onException, throwIO, tryAny)
import Control.Exception (AsyncException(UserInterrupt))
import Data.Char (isControl, isSpace)
import Data.Foldable (toList)
import Data.IORef
    ( atomicModifyIORef'
    , modifyIORef'
    , newIORef
    , readIORef
    , writeIORef
    )
import Data.List
    ( find
    , findIndex
    , intersperse
    , nub
    , sort
    , sortOn
    )
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe, maybeToList)
import Data.Sequence (Seq, ViewL(..), ViewR(..), (|>))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Time.Clock (NominalDiffTime, UTCTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as Vty
import System.Environment (lookupEnv)
import System.Info (os)
import System.IO (stdout)
import System.Posix.Process (getProcessID)
import System.Process (callProcess)

newFullscreenInputBuffer :: IO FullscreenInputBuffer
newFullscreenInputBuffer = Composer.newFullscreenInputBuffer

newFullscreenRuntime
    :: FullscreenInputBuffer
    -> IO ()
    -> (Text -> IO ())
    -> IO CtrlCDecision
    -> (Text -> IO Bool)
    -> (Text -> IO ())
    -> (Bool -> IO ())
    -> IO (AgentTarget, [AgentEntry])
    -> (AgentTarget -> IO ())
    -> IO ()
    -> (NominalDiffTime -> IO ())
    -> MotionMode
    -> Bool
    -> UiState
    -> IO FullscreenRuntime
newFullscreenRuntime =
    newFullscreenRuntimeWithSyntaxLoader newSyntaxHighlighter

newFullscreenRuntimeWithSyntaxLoader
    :: IO (Either Text SyntaxHighlighter)
    -> FullscreenInputBuffer
    -> IO ()
    -> (Text -> IO ())
    -> IO CtrlCDecision
    -> (Text -> IO Bool)
    -> (Text -> IO ())
    -> (Bool -> IO ())
    -> IO (AgentTarget, [AgentEntry])
    -> (AgentTarget -> IO ())
    -> IO ()
    -> (NominalDiffTime -> IO ())
    -> MotionMode
    -> Bool
    -> UiState
    -> IO FullscreenRuntime
newFullscreenRuntimeWithSyntaxLoader
    syntaxLoader
    inputBuffer
    cancelAction
    restartEffortAction
    ctrlCAction
    copyAction
    setWindowTitle
    nativeProgress
    agentSnapshot
    agentSelect
    firstFrame
    syntaxLoadFinished
    motionMode
    color
    initial = do
        events <- newBChan 512
        mailbox <- AppEventMailbox <$> newTVarIO Seq.empty
        motionSchedule <- newTVarIO (MotionNone, 1000000, 0)
        motionTickQueued <- newTVarIO False
        historyRequests <- newTQueueIO
        syntaxRequests <- newTQueueIO
        syntaxHighlighter <-
            newIORef (SyntaxHighlighterUnloaded 0)
        historySource <- newIORef Nothing
        historyGeneration <- newIORef 0
        dictationJobs <- newTQueueIO
        imagePreviews <- newIORef []
        submittedImagePlacements <- newIORef []
        imagePreviewRevision <- newIORef 0
        imagePreviewVisible <- newIORef True
        imagePreviewIdBase <- allocateNativePreviewImageIdBase
        imagePreviewProtocol <- detectImagePreviewProtocol stdout
        imagePreviewInTmux <- isJust <$> lookupEnv "TMUX"
        colorFgBg <- lookupEnv "COLORFGBG"
        windowTitle <- newIORef Nothing
        sessionActions <- newIORef FullscreenSessionActions
            { sessionCancel = cancelAction
            , sessionSteer = const (pure ())
            , sessionBtw = const (pure ())
            , sessionRecap = pure ()
            , sessionRestartEffort = restartEffortAction
            , sessionCtrlC = ctrlCAction
            , sessionAgentSnapshot = agentSnapshot
            , sessionAgentSelect = agentSelect
            }
        pure FullscreenRuntime
            { runtimeEvents = events
            , runtimeMailbox = mailbox
            , runtimeInput = inputBuffer
            , runtimeCancel =
                readIORef sessionActions >>= (.sessionCancel)
            , runtimeSteer = \text ->
                readIORef sessionActions >>= \actions ->
                    actions.sessionSteer text
            , runtimeBtw = \question ->
                readIORef sessionActions >>= \actions ->
                    actions.sessionBtw question
            , runtimeRecap =
                readIORef sessionActions >>= (.sessionRecap)
            , runtimeRestartEffort = \level ->
                readIORef sessionActions >>= \actions ->
                    actions.sessionRestartEffort level
            , runtimeCtrlC =
                readIORef sessionActions >>= (.sessionCtrlC)
            , runtimeCopy = copyAction
            , runtimeSetWindowTitle = setWindowTitle
            , runtimeWindowTitle = windowTitle
            , runtimeNativeProgress = nativeProgress
            , runtimeAgentSnapshot =
                readIORef sessionActions >>= (.sessionAgentSnapshot)
            , runtimeAgentSelect = \target ->
                readIORef sessionActions >>= \actions ->
                    actions.sessionAgentSelect target
            , runtimeFirstFrame = firstFrame
            , runtimeMotionSchedule = motionSchedule
            , runtimeMotionTickQueued = motionTickQueued
            , runtimeMotionMode = motionMode
            , runtimeImagePreviews = imagePreviews
            , runtimeSubmittedImagePlacements = submittedImagePlacements
            , runtimeImagePreviewRevision = imagePreviewRevision
            , runtimeImagePreviewVisible = imagePreviewVisible
            , runtimeImagePreviewIdBase = imagePreviewIdBase
            , runtimeNativeImagePreviews =
                imagePreviewProtocol == PreviewKitty
                    && not imagePreviewInTmux
            , runtimeColor = color
            , runtimeWaveTrough = Theme.waveTroughFromColorFgBg colorFgBg
            , runtimeLoadSyntaxHighlighter = syntaxLoader
            , runtimeSyntaxLoadFinished = syntaxLoadFinished
            , runtimeSyntaxRequests = syntaxRequests
            , runtimeSyntaxHighlighter = syntaxHighlighter
            , runtimeInitial = initial
            , runtimeSessionActions = sessionActions
            , runtimeHistoryRequests = historyRequests
            , runtimeHistorySource = historySource
            , runtimeHistoryGeneration = historyGeneration
            , runtimeDictationJobs = dictationJobs
            }

setFullscreenSessionActions
    :: FullscreenRuntime
    -> IO ()
    -> (Text -> IO ())
    -> (Text -> IO ())
    -> IO ()
    -> (Text -> IO ())
    -> IO CtrlCDecision
    -> IO (AgentTarget, [AgentEntry])
    -> (AgentTarget -> IO ())
    -> IO ()
setFullscreenSessionActions
    runtime
    cancelAction
    steerAction
    btwAction
    recapAction
    restartEffortAction
    ctrlCAction
    agentSnapshot
    agentSelect =
        writeIORef runtime.runtimeSessionActions FullscreenSessionActions
            { sessionCancel = cancelAction
            , sessionSteer = steerAction
            , sessionBtw = btwAction
            , sessionRecap = recapAction
            , sessionRestartEffort = restartEffortAction
            , sessionCtrlC = ctrlCAction
            , sessionAgentSnapshot = agentSnapshot
            , sessionAgentSelect = agentSelect
            }

setFullscreenHistorySource
    :: FullscreenRuntime
    -> Text
    -> (HistoryRequest -> IO (Either Text HistoryPage))
    -> HistoryPage
    -> IO ()
setFullscreenHistorySource runtime key loader initialPage = do
    previous <- readIORef runtime.runtimeHistorySource
    writeIORef runtime.runtimeHistorySource $
        Just FullscreenHistorySource
            { historySourceKey = key
            , historySourceLoad = loader
            }
    case previous of
        Just source
            | source.historySourceKey == key -> pure ()
        _ -> resetFullscreenHistory runtime initialPage

reloadFullscreenHistorySource
    :: FullscreenRuntime
    -> Text
    -> (HistoryRequest -> IO (Either Text HistoryPage))
    -> HistoryPage
    -> IO ()
reloadFullscreenHistorySource runtime key loader initialPage = do
    writeIORef runtime.runtimeHistorySource $
        Just FullscreenHistorySource
            { historySourceKey = key
            , historySourceLoad = loader
            }
    resetFullscreenHistory runtime initialPage

clearFullscreenHistorySource :: FullscreenRuntime -> IO ()
clearFullscreenHistorySource runtime = do
    writeIORef runtime.runtimeHistorySource Nothing
    resetFullscreenHistory runtime
        (HistoryPage
            { historyPageGeneration = HistoryGeneration 0
            , historyPageDirection = HistoryNewer
            , historyPageTurns = Seq.empty
            , historyPageGenerationStart = HistoryCursor 0
            , historyPageTotalTurns = 0
            , historyPageHasOlder = False
            , historyPageHasNewer = False
            })

beginFullscreenLiveHistory :: FullscreenRuntime -> IO ()
beginFullscreenLiveHistory runtime = do
    source <- readIORef runtime.runtimeHistorySource
    case source of
        Nothing -> pure ()
        Just _ -> enqueueAppEvent runtime AppHistoryLiveStarted

commitFullscreenHistoryTurn
    :: FullscreenRuntime
    -> HistoryTurn
    -> HistoryCommit
    -> IO ()
commitFullscreenHistoryTurn runtime turn commit = do
    source <- readIORef runtime.runtimeHistorySource
    case source of
        Nothing -> pure ()
        Just _ -> do
            generation <- case commit of
                HistoryCommitAppend ->
                    HistoryGeneration
                        <$> readIORef runtime.runtimeHistoryGeneration
                _ ->
                    atomicModifyIORef'
                        runtime.runtimeHistoryGeneration
                        \current ->
                            let next = current + 1
                            in (next, HistoryGeneration next)
            enqueueAppEvent runtime
                (AppHistoryCommitted generation turn commit)

resetFullscreenHistory :: FullscreenRuntime -> HistoryPage -> IO ()
resetFullscreenHistory runtime initialPage = do
    generation <- atomicModifyIORef'
        runtime.runtimeHistoryGeneration
        \current ->
            let next = current + 1
            in (next, HistoryGeneration next)
    enqueueAppEvent runtime
        (AppHistoryReset
            initialPage { historyPageGeneration = generation })

loadSyntaxHighlighterForRuntime :: FullscreenRuntime -> IO ()
loadSyntaxHighlighterForRuntime runtime = do
    readIORef runtime.runtimeSyntaxHighlighter >>= \case
        SyntaxHighlighterInactive _ -> pure ()
        state -> do
            let generation = syntaxHighlighterGeneration state
            startedAt <- getMonotonicTimeNSec
            result <- tryAny runtime.runtimeLoadSyntaxHighlighter
            finishedAt <- getMonotonicTimeNSec
            let highlighter = case result of
                    Left _ -> Nothing
                    Right loaded -> either (const Nothing) Just loaded
            published <-
                publishSyntaxHighlighter runtime generation highlighter
            when published $
                enqueueAppEvent runtime AppSyntaxHighlighterChanged
            void $
                tryAny $
                    runtime.runtimeSyntaxLoadFinished
                        (nanosecondsToNominalDiffTime
                            (finishedAt - startedAt))

runSyntaxHighlighterForRuntime :: FullscreenRuntime -> IO ()
runSyntaxHighlighterForRuntime runtime = do
    loadSyntaxHighlighterForRuntime runtime
    forever do
        languages <-
            atomically $
                (:) <$> readTQueue runtime.runtimeSyntaxRequests
                    <*> flushTQueue runtime.runtimeSyntaxRequests
        ensureSyntaxHighlighterForRuntime runtime
        readIORef runtime.runtimeSyntaxHighlighter >>= \case
            SyntaxHighlighterInactive _ -> pure ()
            SyntaxHighlighterUnloaded _ -> pure ()
            SyntaxHighlighterActive _ Nothing -> pure ()
            SyntaxHighlighterActive generation (Just highlighter) -> do
                (changed, loaded) <-
                    foldSyntaxRequests highlighter languages
                when changed do
                    published <-
                        publishSyntaxHighlighter
                            runtime
                            generation
                            (Just loaded)
                    when published $
                        enqueueAppEvent
                            runtime
                            AppSyntaxHighlighterChanged
  where
    foldSyntaxRequests current = \case
        [] -> pure (False, current)
        language : remaining ->
            tryAny (loadSyntaxLanguage current language) >>= \case
                Left _ -> foldSyntaxRequests current remaining
                Right (Left _) -> foldSyntaxRequests current remaining
                Right (Right loaded) -> do
                    (_, final) <- foldSyntaxRequests loaded remaining
                    pure (True, final)

ensureSyntaxHighlighterForRuntime :: FullscreenRuntime -> IO ()
ensureSyntaxHighlighterForRuntime runtime =
    readIORef runtime.runtimeSyntaxHighlighter >>= \case
        SyntaxHighlighterUnloaded _ ->
            loadSyntaxHighlighterForRuntime runtime
        SyntaxHighlighterActive{} -> pure ()
        SyntaxHighlighterInactive _ -> pure ()

publishSyntaxHighlighter
    :: FullscreenRuntime
    -> Word64
    -> Maybe SyntaxHighlighter
    -> IO Bool
publishSyntaxHighlighter runtime generation highlighter =
    atomicModifyIORef' runtime.runtimeSyntaxHighlighter \case
        SyntaxHighlighterUnloaded current
            | current == generation ->
                (SyntaxHighlighterActive current highlighter, True)
        SyntaxHighlighterActive current _
            | current == generation ->
                (SyntaxHighlighterActive current highlighter, True)
        current ->
            (current, False)

syntaxHighlighterGeneration :: SyntaxHighlighterState -> Word64
syntaxHighlighterGeneration = \case
    SyntaxHighlighterUnloaded generation -> generation
    SyntaxHighlighterActive generation _ -> generation
    SyntaxHighlighterInactive generation -> generation

nanosecondsToNominalDiffTime :: Word64 -> NominalDiffTime
nanosecondsToNominalDiffTime nanoseconds =
    realToFrac nanoseconds / 1_000_000_000

emitUiEvent :: FullscreenRuntime -> UiEvent -> IO ()
emitUiEvent runtime event =
    enqueueAppEvent runtime (AppUi event)

setFullscreenWindowTitle :: FullscreenRuntime -> Text -> IO ()
setFullscreenWindowTitle runtime title = do
    writeIORef runtime.runtimeWindowTitle (Just title)
    enqueueAppEvent runtime (AppSetWindowTitle title)

-- | Brick/Vty owns the terminal, so titles must go through Vty output
-- rather than stdout OSC writes. Use UTF-8 OSC bytes; Vty's title setter
-- Latin-1 packs the string and garbles braille spinner frames.
applyStoredFullscreenWindowTitle :: FullscreenRuntime -> V.Output -> IO ()
applyStoredFullscreenWindowTitle runtime output =
    readIORef runtime.runtimeWindowTitle
        >>= mapM_ (writeOutputWindowTitle output)

writeOutputWindowTitle :: V.Output -> Text -> IO ()
writeOutputWindowTitle output title =
    V.outputByteBuffer output (oscWindowTitleBytes title)

setFullscreenImagePreviews
    :: FullscreenRuntime
    -> [ImageAttachment]
    -> IO ()
setFullscreenImagePreviews runtime images = do
    previous <- readIORef runtime.runtimeImagePreviews
    prepared <-
        if map fst previous == images
            then pure previous
            else prepareFullscreenImagePreviews runtime images
    enqueueAppEvent runtime (AppSetImagePreviews prepared)

-- | Move pending composer previews into the next submitted user message.
commitFullscreenImagePreviews
    :: FullscreenRuntime
    -> [ImageAttachment]
    -> IO ()
commitFullscreenImagePreviews runtime images = do
    previous <- readIORef runtime.runtimeImagePreviews
    prepared <-
        if map fst previous == images
            then pure previous
            else prepareFullscreenImagePreviews runtime images
    -- Unsupported terminals render only the compact image summary. Native
    -- terminals retain the encoded attachment for a viewport-aware placement.
    enqueueAppEvent runtime (AppCommitImagePreviews prepared)

prepareFullscreenImagePreviews
    :: FullscreenRuntime
    -> [ImageAttachment]
    -> IO [(ImageAttachment, TuiImagePreview)]
prepareFullscreenImagePreviews runtime images = do
    let prepared =
            mapMaybe
                (\image ->
                    case prepareTuiImagePreview image of
                        Left _ -> Nothing
                        Right preview -> Just (image, preview))
                images
    -- ANSI previews force the sampled image during Brick drawing. Build that
    -- sample here on the model worker instead of stalling the render thread.
    unless runtime.runtimeNativeImagePreviews $
        mapM_
            (\(_, preview) ->
                void $ pure $! pixelAt preview.previewSample 0 0)
            prepared
    pure prepared

hasQueuedFullscreenInput :: FullscreenRuntime -> IO Bool
hasQueuedFullscreenInput runtime =
    atomically do
        queued <- Composer.readFullscreenInputs runtime.runtimeInput
        pure (not (Seq.null queued))

queuedFullscreenInputDisplays
    :: FullscreenInputBuffer
    -> IO (Seq.Seq Text)
queuedFullscreenInputDisplays =
    Composer.queuedFullscreenInputDisplays

readFullscreenLine
    :: FullscreenRuntime
    -> [SkillCommand]
    -> PromptState
    -> Text
    -> IO ReplLine
readFullscreenLine runtime skills prompt initial = do
    result <- readFullscreenLineOrWithModels
        runtime skills [] prompt initial retry
    case result of
        Left impossible -> pure impossible
        Right line -> pure line

readFullscreenLineWithCatalog
    :: FullscreenRuntime
    -> SlashCatalog
    -> PromptState
    -> Text
    -> IO ReplLine
readFullscreenLineWithCatalog runtime catalog prompt initial = do
    result <- readFullscreenLineOrWithCatalog
        runtime catalog prompt initial retry
    case result of
        Left impossible -> pure impossible
        Right line -> pure line

readFullscreenLineWithModels
    :: FullscreenRuntime
    -> [SkillCommand]
    -> [Text]
    -> PromptState
    -> Text
    -> IO ReplLine
readFullscreenLineWithModels runtime skills modelIds prompt initial = do
    result <- readFullscreenLineOrWithModels
        runtime skills modelIds prompt initial retry
    case result of
        Left impossible -> pure impossible
        Right line -> pure line

-- | Wait for either user input or a session-level wakeup. The input branch is
-- deliberately left-biased: once Enter has queued a prompt, provider startup
-- fallback must let that prompt run instead of consuming and losing it during
-- a backend restart.
readFullscreenLineOr
    :: FullscreenRuntime
    -> [SkillCommand]
    -> PromptState
    -> Text
    -> STM wake
    -> IO (Either wake ReplLine)
readFullscreenLineOr runtime skills prompt initial wake = do
    readFullscreenLineOrWithModels runtime skills [] prompt initial wake

readFullscreenLineOrWithModels
    :: FullscreenRuntime
    -> [SkillCommand]
    -> [Text]
    -> PromptState
    -> Text
    -> STM wake
    -> IO (Either wake ReplLine)
readFullscreenLineOrWithModels
        runtime skills modelIds prompt initial wake = do
    readFullscreenLineOrWithCatalog
        runtime
        ((slashCatalogWithSkills skills defaultSlashCatalog)
            { slashCatalogModelIds = modelIds
            })
        prompt
        initial
        wake

readFullscreenLineOrWithCatalog
    :: FullscreenRuntime
    -> SlashCatalog
    -> PromptState
    -> Text
    -> STM wake
    -> IO (Either wake ReplLine)
readFullscreenLineOrWithCatalog
        runtime catalog prompt initial wake = do
    enqueueAppEvent runtime (AppSetSlashCatalog catalog)
    emitUiEvent runtime (UiSetPrompt prompt)
    -- Keep anything the user started typing while the previous turn was
    -- running. Non-empty explicit drafts (for example after cycling mode or
    -- pasting an attachment) still take precedence.
    when (not (Text.null initial)) $
        emitUiEvent runtime (UiSetDraft initial (Text.length initial))
    emitUiEvent runtime (UiSetAwaitingInput True)
    result <- atomically $
        Composer.takeFullscreenInputOr runtime.runtimeInput wake
    case result of
        Left signal -> pure (Left signal)
        Right input -> do
            when input.fullscreenInputQueued $
                emitUiEvent runtime $
                    case input.fullscreenInputDisplay of
                        Just _ -> UiQueuedInputStarted
                        Nothing -> UiSetAwaitingInput False
            pure (Right input.fullscreenInputLine)

-- | Fullscreen Vty configuration, including enhanced-keyboard encodings that
-- are not present in the default terminfo input table. Without these entries,
-- Vty emits the payload of modified-key sequences as printable characters.
fullscreenVtyConfig :: V.VtyUserConfig
fullscreenVtyConfig =
    V.defaultConfig
        { V.configPreferredColorMode = Just V.FullColor
        , V.configInputMap =
            [ ( Nothing
              , "\ESC[" <> body
              , V.EvKey V.KEnter [V.MShift]
              )
            | body <- shiftEnterCsiBodies
            ]
            <> [ ( Nothing
                 , "\ESC[" <> body
                 , V.EvKey (V.KChar character) [V.MCtrl]
                 )
               | character <- ['a'..'z']
               , body <- kittyCtrlCsiBodies character
               ]
            <> [ ( Nothing
                 , "\ESC[" <> body
                 , V.EvKey (V.KChar 'v') [V.MMeta]
                 )
               | body <- kittySuperVCsiBodies
               ]
            <> [ ( Nothing
                 , "\ESC[" <> body
                 , V.EvKey (V.KChar '_') [V.MCtrl]
                 )
               | body <- kittyCtrlUnderscoreCsiBodies
               ]
            <> [ ( Nothing
                 , "\ESC[" <> body
                 , V.EvKey (V.KChar character) [V.MMeta]
                 )
               | character <- ['b', 'd', 'f']
               , body <- kittyAltCsiBodies character
               ]
        }

-- | Enable the smallest Kitty keyboard protocol mode needed for modified
-- printable keys such as Cmd+V. The mode is tied to the Vty lifecycle so
-- Brick suspension pops it before handing the terminal to another process and
-- a rebuilt Vty pushes it again on resume.
wrapFullscreenKeyboardVty :: Bool -> V.Vty -> IO V.Vty
wrapFullscreenKeyboardVty enabled vty
    | not enabled = pure vty
    | otherwise = do
        emit kittyKeyboardDisambiguatePush
            `onException` V.shutdown vty
        pure vty
            { V.shutdown = do
                alreadyShutdown <- V.isShutdown vty
                unless alreadyShutdown $
                    emit kittyKeyboardPop `finally` V.shutdown vty
            }
  where
    emit =
        V.outputByteBuffer (V.outputIface vty)
            . TextEncoding.encodeUtf8

-- | Run an action with a Vty builder while retaining ownership of the most
-- recently built handle. Brick replaces its Vty during 'suspendAndResume',
-- but its exception cleanup can still target the original handle. Shutting
-- down the latest handle here ensures terminal modes are restored on exit.
withTrackedVtyBuilder
    :: IO V.Vty
    -> (IO V.Vty -> IO a)
    -> IO a
withTrackedVtyBuilder build action = do
    latestVty <- newIORef Nothing
    let trackedBuild =
            mask \restore -> do
                vty <- restore build
                writeIORef latestVty (Just vty)
                pure vty
        shutdownLatest =
            readIORef latestVty >>= maybe (pure ()) V.shutdown
    action trackedBuild `finally` shutdownLatest

requestFullscreenPermission
    :: FullscreenRuntime
    -> Text
    -> ToolCall
    -> IO (Maybe PermissionChoice)
requestFullscreenPermission runtime workspace call = do
    reply <- newEmptyTMVarIO
    let summary = permissionToolCallPromptRelative workspace call
    enqueueAppEvent runtime (AppAskPermission summary reply)
    atomically (readTMVar reply)

requestFullscreenChoice
    :: FullscreenRuntime
    -> Text
    -> Int
    -> [(Text, Text)]
    -> IO (Maybe Int)
requestFullscreenChoice runtime title initial rows = do
    requestFullscreenChoiceWithBody runtime title "" initial rows

requestFullscreenChoiceWithBody
    :: FullscreenRuntime
    -> Text
    -> Text
    -> Int
    -> [(Text, Text)]
    -> IO (Maybe Int)
requestFullscreenChoiceWithBody runtime title body initial rows = do
    reply <- newEmptyTMVarIO
    enqueueAppEvent runtime
        (AppAskChoice ChoiceDialog title body initial rows reply)
    atomically (readTMVar reply)

requestFullscreenOnboarding
    :: FullscreenRuntime
    -> Text
    -> Text
    -> [(Text, Text)]
    -> IO (Maybe Int)
requestFullscreenOnboarding runtime title body rows = do
    reply <- newEmptyTMVarIO
    enqueueAppEvent runtime
        (AppAskChoice ChoiceOnboarding title body 0 rows reply)
    atomically (readTMVar reply)

requestFullscreenResume
    :: FullscreenRuntime
    -> ResumeBrowser
    -> (Text -> IO (Either Text ResumeEntry))
    -> (Text -> IO (Either Text ()))
    -> (Text -> IO (Either Text [ResumeEntry]))
    -> IO (Maybe ResumeEntry)
requestFullscreenResume runtime browser loadEntry deleteEntry searchEntries = do
    reply <- newEmptyTMVarIO
    enqueueAppEvent runtime
        (AppAskResume browser loadEntry deleteEntry searchEntries reply)
    atomically (readTMVar reply)

requestFullscreenText
    :: FullscreenRuntime
    -> Text
    -> Text
    -> Text
    -> IO (Maybe Text)
requestFullscreenText runtime title body initial = do
    reply <- newEmptyTMVarIO
    enqueueAppEvent runtime
        (AppAskText TextInputPlain title body initial reply)
    atomically (readTMVar reply)

-- | Request a secret through a masked fullscreen prompt.
--
-- The returned value exists only in transient overlay state and the reply
-- 'TMVar'; it is never rendered or added to normal prompt history.
requestFullscreenSecret
    :: FullscreenRuntime
    -> Text
    -> Text
    -> IO (Maybe Text)
requestFullscreenSecret runtime title body = do
    reply <- newEmptyTMVarIO
    enqueueAppEvent runtime
        (AppAskText
            TextInputSecret
            (sanitizeSecretPromptText title)
            (sanitizeSecretPromptText body)
            ""
            reply)
    atomically (takeTMVar reply)

withFullscreenSuspended :: FullscreenRuntime -> IO a -> IO a
withFullscreenSuspended runtime action = do
    reply <- newEmptyTMVarIO
    enqueueAppEvent runtime (AppSuspend action reply)
    atomically (readTMVar reply) >>= either throwIO pure

runFullscreen :: FullscreenRuntime -> IO a -> IO a
runFullscreen runtime workerAction = do
    history <- readReplHistory
    (initialAgent, initialAgents) <- runtime.runtimeAgentSnapshot
    initialClock <- getMonotonicTimeNSec
    terminal <- detectTerminalCapabilities stdout
    let makeVty = do
            vty <- Vty.mkVty fullscreenVtyConfig
            let setupVty = do
                    let output = V.outputIface vty
                    -- Without this mode terminals paste image clipboard
                    -- fallbacks (paths, URLs, or other text representations)
                    -- as ordinary key events, so the composer renders them as
                    -- text. Vty turns the bracketed sequence into one EvPaste
                    -- that we can classify.
                    when (V.supportsMode output V.BracketedPaste) $
                        V.setMode output V.BracketedPaste True
                    when (V.supportsMode output V.Mouse) $
                        V.setMode output V.Mouse True
                    when (V.supportsMode output V.Focus) $
                        V.setMode output V.Focus True
                    -- Vty deliberately leaves OSC 8 output disabled by
                    -- default even when rendered attributes contain URLs.
                    when (V.supportsMode output V.Hyperlink) $
                        V.setMode output V.Hyperlink True
                    when (V.supportsMode output V.Focus) $
                        V.setMode output V.Focus True
                    wrapped <-
                        wrapNativePreviewVty runtime vty
                            >>= wrapFullscreenKeyboardVty
                                terminal.terminalKittyKeyboard
                    applyStoredFullscreenWindowTitle
                        runtime
                        (V.outputIface wrapped)
                    pure wrapped
            setupVty `onException` V.shutdown vty
    withTrackedVtyBuilder makeVty \buildVty -> do
        initialVty <- buildVty
        let
            initialState =
                initialFullscreenAppState
                    runtime
                    history
                    initialAgent
                    initialAgents
                    initialClock
            (initialDemand, initialDelay) =
                appMotionTiming initialState
        atomically $
            writeTVar
                runtime.runtimeMotionSchedule
                (initialDemand, initialDelay, 0)
        withAsync workerAction \worker ->
            withAsync uiTicker \_uiTicker ->
                withAsync (agentTicker (initialAgent, initialAgents)) \_agentTicker ->
                    withAsync (eventPump runtime) \_eventPump ->
                        withAsync (recapTicker runtime) \_recapTicker ->
                            withAsync
                                historyLoader
                                \_historyLoader ->
                                withAsync
                                    dictationWorker
                                    \_dictationWorker ->
                                    withAsync
                                        (runSyntaxHighlighterForRuntime runtime)
                                        \_syntaxLoader ->
                                            withAsync
                                                (void (waitCatch worker)
                                                    >> enqueueAppEvent runtime AppStop)
                                                \_notifier -> do
                                                    finalState <-
                                                        customMain
                                                            initialVty
                                                            buildVty
                                                            (Just runtime.runtimeEvents)
                                                            fullscreenApp
                                                            initialState
                                                        `finally`
                                                            runtime.runtimeNativeProgress False
                                                    mapM_
                                                        (`Composer.requestDictationStop` True)
                                                        finalState.appDictation
                                                    when (not finalState.appWorkerStopped) $
                                                        atomically $
                                                            Composer.appendFullscreenInput
                                                                runtime.runtimeInput
                                                                FullscreenInput
                                                                    { fullscreenInputLine =
                                                                        ReplEof
                                                                    , fullscreenInputQueued =
                                                                        False
                                                                    , fullscreenInputDisplay =
                                                                        Nothing
                                                                    }
                                                    wait worker
  where
    recapTicker _runtime = forever do
        threadDelay 20_000_000
        enqueueAppEvent runtime AppRecapPoll

    uiTicker = waitForDemand
      where
        waitForDemand = do
            (demand, delayMicros, generation) <- atomically do
                schedule@(current, _, _) <-
                    readTVar runtime.runtimeMotionSchedule
                if current == MotionNone then retry else pure schedule
            tickActive demand delayMicros generation

        tickActive demand delayMicros generation = do
            timer <- registerDelay delayMicros
            outcome <- atomically $
                (do
                    current <-
                        readTVar runtime.runtimeMotionSchedule
                    check (current /= (demand, delayMicros, generation))
                    pure (Left current))
                    `orElse`
                (do
                    ready <- readTVar timer
                    check ready
                    Right
                        <$> readTVar runtime.runtimeMotionSchedule)
            case outcome of
                Left (MotionNone, _, _) ->
                    waitForDemand
                Left (active, nextDelay, nextGeneration) ->
                    tickActive active nextDelay nextGeneration
                Right (MotionNone, _, _) ->
                    waitForDemand
                Right (active, nextDelay, nextGeneration) -> do
                    enqueueMotionTick runtime
                    tickActive active nextDelay nextGeneration

    agentTicker previous = do
        threadDelay 500000
        next <- tryAny runtime.runtimeAgentSnapshot
        previous' <- case next of
            Left _ -> pure previous
            Right snapshot
                | snapshot == previous -> pure previous
                | otherwise -> do
                    enqueueAppEvent runtime
                        (uncurry AppAgentSnapshot snapshot)
                    pure snapshot
        agentTicker previous'

    historyLoader = do
        request <- atomically (readTQueue runtime.runtimeHistoryRequests)
        source <- readIORef runtime.runtimeHistorySource
        result <- case source of
            Nothing ->
                pure (Left "Session history is unavailable.")
            Just current ->
                tryAny (current.historySourceLoad request) >>= \case
                    Left err ->
                        pure (Left (Text.pack (show err)))
                    Right loaded ->
                        pure loaded
        let normalized =
                fmap
                    (\page ->
                        page
                            { historyPageGeneration =
                                request.historyRequestGeneration
                            , historyPageDirection =
                                request.historyRequestDirection
                            })
                    result
        enqueueAppEvent runtime
            (AppHistoryLoaded request normalized)
        historyLoader

    dictationWorker = forever do
        job <- atomically (readTQueue runtime.runtimeDictationJobs)
        result <-
            dictateWith
                DictationControl
                    { dictationWaitForStop = job.dictationJobWaitForStop
                    , dictationOnTranscript =
                        enqueueAppEvent runtime . AppDictationPartial
                    }
        enqueueAppEvent runtime $
            AppDictationFinished $
                case result of
                    DictationTranscript transcript -> Right transcript
                    DictationFailed message -> Left message

-- | Construct the retained application state shared by the live entry point
-- and renderer tests. Generated tests should start from the same defaults as
-- a real fullscreen session instead of assembling an approximate state.
initialFullscreenAppState
    :: FullscreenRuntime
    -> [Text]
    -> AgentTarget
    -> [AgentEntry]
    -> Word64
    -> AppState
initialFullscreenAppState runtime history initialAgent initialAgents initialClock =
    AppState
        { appUi = runtime.runtimeInitial
        , appHistoryWindow =
            emptyHistoryWindow
                (HistoryGeneration 0)
                historyWindowTurnBudget
                historyWindowBlockBudget
                historyWindowByteBudget
        , appHistorySelectedBlock = Nothing
        , appHistoryLiveStart = Nothing
        , appNextHistoryBlockId = -1
        , appPermissionReply = Nothing
        , appRuntime = runtime
        , appSlashIndex = 0
        , appChoice = Nothing
        , appChoiceReply = Nothing
        , appResume = Nothing
        , appResumeReply = Nothing
        , appResumeLoad = Nothing
        , appResumeDelete = Nothing
        , appResumeSearch = Nothing
        , appTextPrompt = Nothing
        , appTextReply = Nothing
        , appSlashDismissed = False
        , appPasted = False
        , appHistory = Bridge.trimHistory history
        , appHistoryIndex = Nothing
        , appHistoryDraft = ""
        , appKillBuffer = ""
        , appKillChain = False
        , appUndo = []
        , appDictation = Nothing
        , appSlashCatalog = defaultSlashCatalog
        , appImagePreviews = []
        , appSubmittedImagePreviews = Map.empty
        , appAgentSelected = initialAgent
        , appAgentEntries = initialAgents
        , appAgentHover = Nothing
        , appHoveredControl = Nothing
        , appPressedControl = Nothing
        , appWorkerStopped = False
        , appConversationAnchor = Nothing
        , appFocusLostAt = Nothing
        , appAutoRecapShownThisAway = False
        , appLastAutoRecapAttemptAt = Nothing
        , appLastTurnCompletedAt = Nothing
        , appConversationReflowQueued = False
        , appWindowTitle = Nothing
        , appMotionElapsedMillis = 0
        , appCompletionFlashes = Map.empty
        , appMotionScheduleReset = False
        , appClockNanos = initialClock
        , appNativeProgressKeepaliveBucket = 0
        , appSyntaxHighlighter = Nothing
        , appSyntaxRequested = Set.empty
        , appTerminalFocus = TerminalFocusUnknown
        }

historyWindowTurnBudget :: Int
historyWindowTurnBudget = 200

historyWindowBlockBudget :: Int
historyWindowBlockBudget = 1200

historyWindowByteBudget :: Int
historyWindowByteBudget = 8 * 1024 * 1024

resetHistoryPage :: HistoryPage -> AppState -> AppState
resetHistoryPage page state =
    let
        empty =
            emptyHistoryWindow
                page.historyPageGeneration
                historyWindowTurnBudget
                historyWindowBlockBudget
                historyWindowByteBudget
        (nextBlockId, remapped) =
            remapHistoryPage state.appNextHistoryBlockId page
        window =
            either (const empty) id (applyHistoryPage remapped empty)
    in state
        { appUi = reduceUi UiConversationCleared state.appUi
        , appHistoryWindow = window
        , appHistorySelectedBlock = Nothing
        , appHistoryLiveStart = Nothing
        , appNextHistoryBlockId = nextBlockId
        , appCompletionFlashes = Map.empty
        , appConversationAnchor = Nothing
        }

setHistoryGeneration :: HistoryGeneration -> AppState -> AppState
setHistoryGeneration generation state =
    state
        { appHistoryWindow =
            state.appHistoryWindow
                { historyWindowGeneration = generation
                , historyWindowPending = Set.empty
                }
        }

applyLoadedHistoryPage :: HistoryPage -> AppState -> AppState
applyLoadedHistoryPage page state =
    if page.historyPageGeneration
        /= state.appHistoryWindow.historyWindowGeneration
        then state
        else
            let
                (nextBlockId, remapped) =
                    remapHistoryPage state.appNextHistoryBlockId page
                anchored =
                    historyWindowSetAnchors
                        (historyEdgeCursor
                            page.historyPageDirection
                            state.appHistoryWindow)
                        (state.appHistorySelectedBlock >>=
                            historyCursorForBlock
                                state.appHistoryWindow)
                        state.appHistoryWindow
                window =
                    either
                        (const anchored)
                        id
                        (applyHistoryPage remapped anchored)
                selected =
                    state.appHistorySelectedBlock >>= \blockId ->
                        if historyContainsBlock blockId window
                            then Just blockId
                            else Nothing
            in state
                { appHistoryWindow = window
                , appHistorySelectedBlock = selected
                , appNextHistoryBlockId = nextBlockId
                }

clearHistoryPending :: HistoryRequest -> AppState -> AppState
clearHistoryPending request state =
    state
        { appHistoryWindow =
            clearHistoryRequest request state.appHistoryWindow
        }

commitLiveHistoryTurn
    :: HistoryTurn
    -> HistoryCommit
    -> AppState
    -> AppState
commitLiveHistoryTurn durableTurn commit state =
    let
        start =
            case state.appHistoryLiveStart of
                Just index -> index
                Nothing
                    | commit == HistoryCommitReset -> 0
                    | otherwise ->
                        unarchivedLiveStart
                            state.appUi.uiBlocks
                            durableTurn.historyTurnBlocks
        (nextBlockId, remappedBlocks) =
            remapHistoryBlocks
                state.appNextHistoryBlockId
                durableTurn.historyTurnBlocks
        remappedTurn =
            durableTurn { historyTurnBlocks = remappedBlocks }
        baseWindow =
            case commit of
                HistoryCommitReset ->
                    emptyHistoryWindow
                        state.appHistoryWindow.historyWindowGeneration
                        historyWindowTurnBudget
                        historyWindowBlockBudget
                        historyWindowByteBudget
                _ ->
                    state.appHistoryWindow
        replacementPage =
            HistoryPage
                { historyPageGeneration =
                    state.appHistoryWindow.historyWindowGeneration
                , historyPageDirection = HistoryNewer
                , historyPageTurns = Seq.singleton remappedTurn
                , historyPageGenerationStart =
                    remappedTurn.historyTurnCursor
                , historyPageTotalTurns = 1
                , historyPageHasOlder = False
                , historyPageHasNewer = False
                }
        window =
            case commit of
                HistoryCommitReset ->
                    either
                        (const baseWindow)
                        id
                        (applyHistoryPage replacementPage baseWindow)
                _ ->
                    appendHistoryTurn remappedTurn baseWindow
        ui = truncateUiBlocks start state.appUi
    in state
        { appUi = ui
        , appHistoryWindow = window
        , appHistorySelectedBlock = Nothing
        , appHistoryLiveStart = Nothing
        , appNextHistoryBlockId = nextBlockId
        , appConversationAnchor = Nothing
        , appCompletionFlashes =
            retainExistingFlashes ui state.appCompletionFlashes
        }

truncateUiBlocks :: Int -> UiState -> UiState
truncateUiBlocks count ui =
    let
        blocks = Seq.take (max 0 count) ui.uiBlocks
        indices =
            Map.fromList
                [ (block.blockId, index)
                | (index, block) <- zip [0 ..] (toList blocks)
                ]
        selectedIndex =
            ui.uiSelectedBlock >>= (`Map.lookup` indices)
    in ui
        { uiBlocks = blocks
        , uiSelectedBlock =
            selectedIndex >>= \index ->
                (.blockId) <$> Seq.lookup index blocks
        , uiSelectedBlockIndex = selectedIndex
        , uiBlockIndices = indices
        , uiTurnStartBlock = min count ui.uiTurnStartBlock
        , uiAttemptStartBlock = min count ui.uiAttemptStartBlock
        , uiToolCalls =
            Map.filter
                (\(index, _) -> index < count)
                ui.uiToolCalls
        , uiRetryCountdown = Nothing
        }

remapHistoryPage :: Int -> HistoryPage -> (Int, HistoryPage)
remapHistoryPage nextId page =
    let
        (remaining, turns) =
            foldl'
                (\(current, accumulated) turn ->
                    let (next, blocks) =
                            remapHistoryBlocks
                                current
                                turn.historyTurnBlocks
                    in (next, accumulated |> turn
                        { historyTurnBlocks = blocks }))
                (nextId, Seq.empty)
                page.historyPageTurns
    in (remaining, page { historyPageTurns = turns })

remapHistoryBlocks :: Int -> Seq UiBlock -> (Int, Seq UiBlock)
remapHistoryBlocks nextId =
    foldl'
        (\(current, blocks) block ->
            ( current - 1
            , blocks |> block { blockId = BlockId current }
            ))
        (nextId, Seq.empty)

historyContainsBlock :: BlockId -> HistoryWindow -> Bool
historyContainsBlock blockId =
    any
        (any ((== blockId) . (.blockId))
            . toList
            . (.historyTurnBlocks))
        . toList
        . (.historyWindowTurns)

historyCursorForBlock
    :: HistoryWindow
    -> BlockId
    -> Maybe HistoryCursor
historyCursorForBlock window blockId =
    (.historyTurnCursor)
        <$> find
            (any ((== blockId) . (.blockId))
                . toList
                . (.historyTurnBlocks))
            (toList window.historyWindowTurns)

historyEdgeCursor
    :: HistoryDirection
    -> HistoryWindow
    -> Maybe HistoryCursor
historyEdgeCursor direction window =
    (.historyTurnCursor) <$> case direction of
        HistoryOlder -> window.historyWindowTurns Seq.!? 0
        HistoryNewer ->
            window.historyWindowTurns
                Seq.!? (Seq.length window.historyWindowTurns - 1)

historyPageAnchorBlock
    :: HistoryDirection
    -> HistoryWindow
    -> Maybe BlockId
historyPageAnchorBlock direction window =
    edgeTurn >>= edgeBlock
  where
    turns = window.historyWindowTurns
    edgeTurn = case direction of
        HistoryOlder -> turns Seq.!? 0
        HistoryNewer -> turns Seq.!? (Seq.length turns - 1)
    edgeBlock turn =
        let blocks = turn.historyTurnBlocks
        in case direction of
            HistoryOlder -> (.blockId) <$> blocks Seq.!? 0
            HistoryNewer ->
                (.blockId) <$> blocks Seq.!? (Seq.length blocks - 1)

wrapNativePreviewVty :: FullscreenRuntime -> V.Vty -> IO V.Vty
wrapNativePreviewVty runtime vty
    | not runtime.runtimeNativeImagePreviews = pure vty
    | otherwise = do
        rendered <- newIORef Nothing
        let output = V.outputIface vty
            deletePayload imageId =
                kittyDeleteImageSequence imageId
            placementPayload placement =
                let attachment = placement.nativePreviewAttachment
                    graphics =
                        kittyPlacedImageSequence
                            placement.nativePreviewImageId
                            placement.nativePreviewImageId
                            placement.nativePreviewColumns
                            placement.nativePreviewRows
                            attachment.imageMime
                            attachment.imageBytes
                in positionImagePayload
                    placement.nativePreviewRow
                    placement.nativePreviewColumn
                    graphics
            renderNative force = do
                revision <- readIORef runtime.runtimeImagePreviewRevision
                bounds@(terminalColumns, terminalRows) <-
                    V.displayBounds output
                previous <- readIORef rendered
                let unchanged = case previous of
                        Just (oldRevision, oldBounds, _) ->
                            oldRevision == revision && oldBounds == bounds
                        Nothing -> False
                when (force || not unchanged) do
                    visible <-
                        readIORef runtime.runtimeImagePreviewVisible
                    previews <-
                        if visible
                            then readIORef runtime.runtimeImagePreviews
                            else pure []
                    submitted <-
                        if visible
                            then
                                readIORef
                                    runtime.runtimeSubmittedImagePlacements
                            else pure []
                    let placements
                            | null previews = submitted
                            | otherwise =
                                nativePreviewPlacements
                                    runtime.runtimeImagePreviewIdBase
                                    terminalColumns
                                    terminalRows
                                    previews
                        oldImageIds = case previous of
                            Just (_, _, imageIds) -> imageIds
                            Nothing -> []
                        payload =
                            Text.concat
                                ( map deletePayload oldImageIds
                                    <> map placementPayload placements
                                )
                    when (not (Text.null payload)) $
                        V.outputByteBuffer output
                            (TextEncoding.encodeUtf8 payload)
                    writeIORef rendered $
                        Just
                            ( revision
                            , bounds
                            , map (.nativePreviewImageId) placements
                            )
            clearNative = do
                previous <- readIORef rendered
                let imageIds = case previous of
                        Just (_, _, ids) -> ids
                        Nothing -> []
                    payload = Text.concat (map deletePayload imageIds)
                when (not (Text.null payload)) $
                    V.outputByteBuffer output
                        (TextEncoding.encodeUtf8 payload)
                writeIORef rendered Nothing
        pure vty
            { V.update = \picture ->
                V.update vty picture >> renderNative False
            , V.refresh =
                V.refresh vty >> renderNative True
            , V.shutdown =
                clearNative `finally` V.shutdown vty
            }

allocateNativePreviewImageIdBase :: IO Int
allocateNativePreviewImageIdBase = do
    micros <- floor . (* 1_000_000) <$> getPOSIXTime :: IO Integer
    pid <- fromIntegral <$> getProcessID :: IO Integer
    -- Kitty ids are terminal-global uint32s. Leave room for the other two
    -- simultaneously displayed previews and vary the base per process/runtime.
    let availableBases = 4_294_967_293 :: Integer
    pure $
        fromInteger ((micros * 65_537 + pid) `mod` availableBases) + 1

-- | Move events from the producer-facing mailbox into Brick. UI updates are
-- collected for one frame so a fast token stream causes at most one redraw
-- every ~16 ms. Blocking on Brick's bounded channel only blocks this pump,
-- never the model/tool worker publishing into the mailbox.
eventPump :: FullscreenRuntime -> IO ()
eventPump runtime = loop
  where
    loop = do
        pending <- atomically (takePendingAppEvent runtime.runtimeMailbox)
        delivered <- case pending of
            PendingUi first -> do
                threadDelay uiFrameDelayMicros
                rest <- atomically $
                    takePendingUiEventPrefix
                        (uiFrameBatchLimit - 1)
                        runtime.runtimeMailbox
                pure (AppUiBatch
                    (pendingUiEvent first :| map pendingUiEvent rest))
            PendingEvent event ->
                pure event
        writeBChan runtime.runtimeEvents delivered
        loop

uiFrameDelayMicros :: Int
uiFrameDelayMicros = 16000

uiFrameBatchLimit :: Int
uiFrameBatchLimit = 256

enqueueAppEvent :: FullscreenRuntime -> AppEvent -> IO ()
enqueueAppEvent runtime event =
    atomically do
        let AppEventMailbox pendingRef = runtime.runtimeMailbox
        pending <- readTVar pendingRef
        writeTVar pendingRef (appendAppEvent event pending)

enqueueMotionTick :: FullscreenRuntime -> IO ()
enqueueMotionTick runtime =
    atomically do
        queued <- readTVar runtime.runtimeMotionTickQueued
        unless queued do
            writeTVar runtime.runtimeMotionTickQueued True
            let AppEventMailbox pendingRef = runtime.runtimeMailbox
            pending <- readTVar pendingRef
            writeTVar pendingRef (appendAppEvent AppMotionTick pending)

appendAppEvent :: AppEvent -> Seq PendingAppEvent -> Seq PendingAppEvent
appendAppEvent event pending = case event of
    AppUi (UiLoop (TextDelta delta)) ->
        case Seq.viewr pending of
            rest :> PendingUi (PendingTextDeltas deltas) ->
                rest |> PendingUi (PendingTextDeltas (deltas |> delta))
            _ ->
                pending |> PendingUi
                    (PendingTextDeltas (Seq.singleton delta))
    AppUi (UiLoop (ReasoningDelta delta)) ->
        case Seq.viewr pending of
            rest :> PendingUi (PendingReasoningDeltas deltas) ->
                rest |> PendingUi
                    (PendingReasoningDeltas (deltas |> delta))
            _ ->
                pending |> PendingUi
                    (PendingReasoningDeltas (Seq.singleton delta))
    AppUi uiEvent ->
        appendExactUiEvent uiEvent pending
    _ ->
        appendExactAppEvent event pending

appendExactUiEvent
    :: UiEvent
    -> Seq PendingAppEvent
    -> Seq PendingAppEvent
appendExactUiEvent event pending =
    case Seq.viewr pending of
        rest :> PendingUi (PendingExactUi previous)
            | Just merged <- Bridge.mergeUiEvents previous event ->
                rest |> PendingUi (PendingExactUi merged)
        _ ->
            pending |> PendingUi (PendingExactUi event)

appendExactAppEvent
    :: AppEvent
    -> Seq PendingAppEvent
    -> Seq PendingAppEvent
appendExactAppEvent AppMotionTick pending
    | any isPendingMotionTick pending =
        pending
  where
    isPendingMotionTick = \case
        PendingEvent AppMotionTick -> True
        _ -> False
appendExactAppEvent event pending =
    case (Seq.viewr pending, event) of
        ( rest :> PendingEvent (AppAgentSnapshot _ _)
            , AppAgentSnapshot selected entries
            ) ->
                rest |> PendingEvent (AppAgentSnapshot selected entries)
        (rest :> PendingEvent (AppSetWindowTitle _), AppSetWindowTitle title) ->
            rest |> PendingEvent (AppSetWindowTitle title)
        (rest :> PendingEvent (AppDictationPartial _), AppDictationPartial text) ->
            rest |> PendingEvent (AppDictationPartial text)
        _ ->
            pending |> PendingEvent event

takePendingAppEvent :: AppEventMailbox -> STM PendingAppEvent
takePendingAppEvent (AppEventMailbox pendingRef) = do
    pending <- readTVar pendingRef
    case Seq.viewl pending of
        EmptyL -> retry
        event :< rest -> do
            writeTVar pendingRef rest
            pure event

takePendingUiEventPrefix
    :: Int
    -> AppEventMailbox
    -> STM [PendingUiEvent]
takePendingUiEventPrefix limit (AppEventMailbox pendingRef) = do
    pending <- readTVar pendingRef
    let (events, rest) = go limit [] pending
    writeTVar pendingRef rest
    pure events
  where
    go remaining acc pending
        | remaining <= 0 = (reverse acc, pending)
        | otherwise =
            case Seq.viewl pending of
                PendingUi event :< rest ->
                    go (remaining - 1) (event : acc) rest
                _ ->
                    (reverse acc, pending)

pendingUiEvent :: PendingUiEvent -> UiEvent
pendingUiEvent = \case
    PendingExactUi event -> event
    PendingTextDeltas deltas ->
        UiLoop (TextDelta (Text.concat (toList deltas)))
    PendingReasoningDeltas deltas ->
        UiLoop (ReasoningDelta (Text.concat (toList deltas)))

handleResumeKey :: V.Event -> EventM Name AppState ()
handleResumeKey event = do
    state <- get
    case state.appResume of
        Nothing -> pure ()
        Just overlay ->
            let browser = overlay.resumeOverlayBrowser
            in case browser.resumeBrowserDeletePending of
                Just sessionId -> handleDeleteConfirmation state browser sessionId event
                Nothing
                    | browser.resumeBrowserSearching ->
                        handleResumeSearch browser event
                    | otherwise ->
                        handleResumeNormal browser event

handleDeleteConfirmation
    :: AppState
    -> ResumeBrowser
    -> Text
    -> V.Event
    -> EventM Name AppState ()
handleDeleteConfirmation state browser sessionId = \case
    V.EvKey (V.KChar 'y') [] ->
        case state.appResumeDelete of
            Nothing ->
                setBrowser (setResumeNotice (Just "Session deletion is unavailable.") browser)
            Just deleteEntry -> do
                result <- liftIO (deleteEntry sessionId)
                case result of
                    Left err -> setBrowser (setResumeNotice (Just err) browser)
                    Right () -> do
                        setBrowser (removeResumeEntry sessionId browser)
                        revealSelectedResume
    V.EvKey (V.KChar 'n') [] ->
        setBrowser (setResumeDeletePending Nothing browser)
    V.EvKey V.KEsc [] ->
        setBrowser (setResumeDeletePending Nothing browser)
    _ -> pure ()

handleResumeSearch :: ResumeBrowser -> V.Event -> EventM Name AppState ()
handleResumeSearch browser event
    | Just delta <- resumeNavigationDelta event =
        moveAndReveal delta browser
    | otherwise = case event of
        V.EvKey V.KEsc [] ->
            setBrowser (endResumeSearch browser)
        V.EvKey V.KEnter [] -> do
            state <- get
            case state.appResumeSearch of
                Nothing ->
                    setBrowser (endResumeSearch browser)
                Just searchEntries -> do
                    result <- liftIO (searchEntries browser.resumeBrowserQuery)
                    case result of
                        Left err ->
                            setBrowser (setResumeNotice (Just err) browser)
                        Right entries -> do
                            setBrowser
                                (applyResumeSearchResults
                                    browser.resumeBrowserQuery
                                    entries
                                    browser)
                            revealSelectedResume
        V.EvKey V.KBS [] ->
            setAndReveal $
                insertResumeSearch ""
                    browser
                        { resumeBrowserQuery =
                            Text.dropEnd 1 browser.resumeBrowserQuery
                        , resumeBrowserIndex = 0
                        }
        V.EvKey (V.KChar 'u') modifiers
            | V.MCtrl `elem` modifiers ->
                setAndReveal $
                    insertResumeSearch ""
                        browser
                            { resumeBrowserQuery = ""
                            , resumeBrowserIndex = 0
                            }
        V.EvKey (V.KChar char) []
            | not (isControl char) ->
                setAndReveal (insertResumeSearch (Text.singleton char) browser)
        _ -> pure ()

handleResumeNormal :: ResumeBrowser -> V.Event -> EventM Name AppState ()
handleResumeNormal browser event
    | Just delta <- resumeNavigationDelta event =
        moveAndReveal delta browser
    | otherwise = case event of
        V.EvKey V.KEnter [] ->
            resolveResume True
        V.EvKey V.KEsc [] ->
            resolveResume False
        V.EvKey (V.KChar 'e') [] ->
            expandSelectedResume browser
        V.EvKey (V.KChar 'f') [] -> do
            setBrowser (cycleResumeSource browser)
            revealSelectedResume
        V.EvKey (V.KChar 'd') [] ->
            case selectedResumeBrowser browser of
                Nothing -> pure ()
                Just entry ->
                    setBrowser (setResumeDeletePending (Just entry.resumeId) browser)
        V.EvKey (V.KChar '/') [] ->
            setBrowser (beginResumeSearch browser)
        V.EvKey (V.KChar char) []
            | not (isControl char) ->
                setAndReveal
                    (insertResumeSearch
                        (Text.singleton char)
                        (beginResumeSearch browser))
        _ -> pure ()

resumeNavigationDelta :: V.Event -> Maybe Int
resumeNavigationDelta = \case
    V.EvKey V.KUp [] -> Just (-1)
    V.EvKey V.KDown [] -> Just 1
    V.EvKey V.KPageUp [] -> Just (-10)
    V.EvKey V.KPageDown [] -> Just 10
    V.EvMouseDown _ _ V.BScrollUp _ -> Just (-mouseScrollLines)
    V.EvMouseDown _ _ V.BScrollDown _ -> Just mouseScrollLines
    _ -> Nothing

expandSelectedResume :: ResumeBrowser -> EventM Name AppState ()
expandSelectedResume browser =
    case selectedResumeBrowser browser of
        Nothing -> pure ()
        Just entry
            | browser.resumeBrowserExpanded == Just entry.resumeId ->
                setBrowser (toggleResumeExpanded browser)
            | entry.resumeLoaded ->
                setBrowser (toggleResumeExpanded browser)
            | otherwise -> do
                state <- get
                case state.appResumeLoad of
                    Nothing ->
                        setBrowser
                            (setResumeNotice
                                (Just "Session details are unavailable.")
                                browser)
                    Just loadEntry -> do
                        loaded <- liftIO (loadEntry entry.resumeId)
                        case loaded of
                            Left err ->
                                setBrowser (setResumeNotice (Just err) browser)
                            Right fullEntry ->
                                setBrowser $
                                    toggleResumeExpanded
                                        (replaceResumeEntry fullEntry browser)

moveAndReveal :: Int -> ResumeBrowser -> EventM Name AppState ()
moveAndReveal delta browser = do
    setBrowser (moveResumeBrowser delta browser)
    revealSelectedResume

revealSelectedResume :: EventM Name AppState ()
revealSelectedResume = do
    state <- get
    case state.appResume >>= selectedResumeBrowser . (.resumeOverlayBrowser) of
        Nothing -> pure ()
        Just entry -> makeVisible (ResumeRow entry.resumeId)

setBrowser :: ResumeBrowser -> EventM Name AppState ()
setBrowser browser =
    modify' \state ->
        state
            { appResume =
                (\overlay -> overlay { resumeOverlayBrowser = browser })
                    <$> state.appResume
            }

setAndReveal :: ResumeBrowser -> EventM Name AppState ()
setAndReveal browser = do
    setBrowser browser
    revealSelectedResume

resolveResume :: Bool -> EventM Name AppState ()
resolveResume confirmed = do
    state <- get
    case state.appResumeReply of
        Nothing -> pure ()
        Just reply ->
            liftIO $ atomically $
                putTMVar reply $
                    if confirmed
                        then state.appResume
                            >>= selectedResumeBrowser . (.resumeOverlayBrowser)
                        else Nothing
    modify' \current ->
        current
            { appResume = Nothing
            , appResumeReply = Nothing
            , appResumeLoad = Nothing
            , appResumeDelete = Nothing
            , appResumeSearch = Nothing
            }
    resumeNativeProgressIfRunning

confirmResumeId :: Text -> EventM Name AppState ()
confirmResumeId sessionId = do
    state <- get
    case state.appResume of
        Nothing -> pure ()
        Just overlay ->
            case
                find
                    ((== sessionId) . (.resumeId))
                    (visibleResumeBrowser overlay.resumeOverlayBrowser)
            of
                Nothing -> pure ()
                Just _ -> do
                    let visible = visibleResumeBrowser overlay.resumeOverlayBrowser
                        index = fromMaybe 0 (findIndex ((== sessionId) . (.resumeId)) visible)
                    setBrowser
                        overlay.resumeOverlayBrowser
                            { resumeBrowserIndex = index
                            }
                    resolveResume True

handleChoiceKey :: V.Event -> EventM Name AppState ()
handleChoiceKey = \case
    V.EvKey V.KUp [] -> moveChoice (-1)
    V.EvKey V.KDown [] -> moveChoice 1
    V.EvKey V.KBackTab [] -> moveChoice (-1)
    V.EvKey (V.KChar '\t') [] -> moveChoice 1
    V.EvKey V.KPageUp [] ->
        vScrollPage (viewportScroll OverlayViewport) Up
    V.EvKey V.KPageDown [] ->
        vScrollPage (viewportScroll OverlayViewport) Down
    V.EvMouseDown _ _ V.BScrollUp _ ->
        vScrollBy (viewportScroll OverlayViewport) (-mouseScrollLines)
    V.EvMouseDown _ _ V.BScrollDown _ ->
        vScrollBy (viewportScroll OverlayViewport) mouseScrollLines
    V.EvKey V.KEnter [] -> resolveChoice True
    V.EvKey V.KEsc [] -> resolveChoice False
    V.EvKey (V.KChar 'q') [] -> resolveChoice False
    V.EvKey (V.KChar 'c') modifiers
        | V.MCtrl `elem` modifiers -> do
            state <- get
            _ <- handleCtrlC
            when state.appUi.uiRunning (resolveChoice False)
    _ -> pure ()
  where
    moveChoice delta =
        modify' \state ->
            state
                { appChoice =
                    (\choice ->
                        let count = length choice.choiceRows
                        in if count == 0
                            then choice
                            else choice
                                { choiceIndex =
                                    (choice.choiceIndex + delta) `mod` count
                                })
                        <$> state.appChoice
                }

confirmChoiceAt :: Int -> EventM Name AppState ()
confirmChoiceAt index = do
    state <- get
    case state.appChoice of
        Just choice
            | index >= 0
            , index < length choice.choiceRows -> do
                modify' \current ->
                    current
                        { appChoice =
                            (\overlay -> overlay { choiceIndex = index })
                                <$> current.appChoice
                        }
                resolveChoice True
        _ -> pure ()

activateControl :: Name -> EventM Name AppState ()
activateControl = \case
    ComposerModel ->
        Composer.handlePromptControlClick
            applyLocalUiEventWith
            ReplChooseModel
    ComposerEffort ->
        Composer.handleEffortControlClick applyLocalUiEventWith
    ComposerMode ->
        Composer.handlePromptControlClick
            applyLocalUiEventWith
            ReplCycleMode
    ComposerAccount ->
        Composer.handlePromptControlClick
            applyLocalUiEventWith
            ReplChooseAccount
    QuickStartWorktree ->
        activateQuickStartCommand "/worktree"
    QuickStartResume ->
        activateQuickStartCommand "/resume"
    QuickStartCommands ->
        applyLocalUiEventWith
            (UiSetDraft "/" 1)
            (applyUiEvent (UiFocusChanged FocusComposer))
    QuickStartModel ->
        Composer.handlePromptControlClick
            applyLocalUiEventWith
            ReplChooseModel
    ChoiceRow index ->
        confirmChoiceAt index
    ResumeRow sessionId ->
        confirmResumeId sessionId
    CodeCopy target blockId codeIndex ->
        copyCodeBlock target blockId codeIndex
    MarkdownLink url ->
        openMarkdownLink url
    _ ->
        pure ()

activateQuickStartCommand :: Text -> EventM Name AppState ()
activateQuickStartCommand command =
    Composer.handlePromptControlClick
        applyLocalUiEventWith
        (const (ReplText command))

isInteractiveControl :: Name -> Bool
isInteractiveControl = \case
    ComposerModel -> True
    ComposerEffort -> True
    ComposerMode -> True
    ComposerAccount -> True
    QuickStartWorktree -> True
    QuickStartResume -> True
    QuickStartCommands -> True
    QuickStartModel -> True
    ChoiceRow _ -> True
    ResumeRow _ -> True
    CodeCopy _ _ _ -> True
    _ -> False

isQuickStartControl :: Name -> Bool
isQuickStartControl = \case
    QuickStartWorktree -> True
    QuickStartResume -> True
    QuickStartCommands -> True
    QuickStartModel -> True
    _ -> False

openMarkdownLink :: Text -> EventM Name AppState ()
openMarkdownLink url = do
    opened <- liftIO (openExternalUrl url)
    unless opened $
        modify' $
            applyUiEvent
                (UiSetNotice
                    (Just (warningNotice "Could not open that link.")))

openExternalUrl :: Text -> IO Bool
openExternalUrl url =
    case externalUrlCommand url of
        Nothing -> pure False
        Just (command, arguments) ->
            either (const False) (const True)
                <$> tryAny (callProcess command arguments)

externalUrlCommand :: Text -> Maybe (FilePath, [String])
externalUrlCommand url
    | Text.length url > 4096 = Nothing
    | Text.any (\character -> isControl character || isSpace character) url =
        Nothing
    | not (isWebUrl url) = Nothing
    | os == "darwin" = Just ("/usr/bin/open", [Text.unpack url])
    | os == "mingw32" =
        Just
            ( "rundll32"
            , ["url.dll,FileProtocolHandler", Text.unpack url]
            )
    | otherwise = Just ("xdg-open", [Text.unpack url])
  where
    isWebUrl value =
        let lower = Text.toLower value
        in Text.isPrefixOf "https://" lower
            || Text.isPrefixOf "http://" lower

copyCodeBlock
    :: AgentTarget
    -> BlockId
    -> Int
    -> EventM Name AppState ()
copyCodeBlock target blockId codeIndex = do
    state <- get
    let code =
            (case target of
                AgentRoot ->
                    historyBlock state.appHistoryWindow blockId
                        <|> selectedBlock state.appUi blockId
                AgentChild _ ->
                    conversationUiForTarget target state
                        >>= \ui -> selectedBlock ui blockId
                AgentNative _ ->
                    conversationUiForTarget target state
                        >>= \ui -> selectedBlock ui blockId)
                >>= fencedCodeBlock codeIndex . (.blockBody)
    case code of
        Nothing ->
            applyLocalUiEvent $
                UiSetNotice $
                    Just $
                        warningNotice
                            "Code block is no longer available."
        Just payload -> do
            copied <- liftIO (state.appRuntime.runtimeCopy payload)
            applyLocalUiEvent $
                UiSetNotice $
                    Just $
                        if copied
                            then successNotice "Copied code block."
                            else warningNotice
                                "Terminal clipboard is unavailable."

resolveChoice :: Bool -> EventM Name AppState ()
resolveChoice confirmed = do
    state <- get
    case state.appChoiceReply of
        Nothing -> pure ()
        Just reply ->
            liftIO $ reply $
                if confirmed
                    then (.choiceIndex) <$> state.appChoice
                    else Nothing
    modify' \current ->
        current
            { appChoice = Nothing
            , appChoiceReply = Nothing
            }
    resumeNativeProgressIfRunning

handleTextPromptKey :: V.Event -> EventM Name AppState ()
handleTextPromptKey event = case event of
    V.EvKey V.KEsc [] -> resolveTextPrompt False
    V.EvKey (V.KChar 'c') modifiers
        | V.MCtrl `elem` modifiers -> do
            state <- get
            _ <- handleCtrlC
            when state.appUi.uiRunning (resolveTextPrompt False)
    V.EvKey V.KEnter [] -> resolveTextPrompt True
    V.EvKey V.KPageUp [] ->
        vScrollPage (viewportScroll OverlayViewport) Up
    V.EvKey V.KPageDown [] ->
        vScrollPage (viewportScroll OverlayViewport) Down
    V.EvMouseDown _ _ V.BScrollUp _ ->
        vScrollBy (viewportScroll OverlayViewport) (-mouseScrollLines)
    V.EvMouseDown _ _ V.BScrollDown _ ->
        vScrollBy (viewportScroll OverlayViewport) mouseScrollLines
    _ ->
        modify' \state ->
            state
                { appTextPrompt =
                    (\prompt ->
                        fromMaybe prompt (applyTextPromptEdit event prompt))
                        <$> state.appTextPrompt
                }

-- | Apply one text-editing key to a fullscreen prompt overlay.
--
-- Cursor offsets are normalized to grapheme boundaries before and after the
-- edit so movement, deletion, and rendering agree for multi-code-point glyphs.
applyTextPromptEdit :: V.Event -> TextOverlay -> Maybe TextOverlay
applyTextPromptEdit event prompt = case event of
    V.EvKey V.KEnter [V.MShift] ->
        Just $
            if prompt.textInputMode == TextInputPlain
                then insert "\n"
                else prompt
    V.EvKey V.KBS [] ->
        Just $ edit \draft cursor ->
            let previous = previousGraphemeBoundary draft cursor
            in ( Text.take previous draft <> Text.drop cursor draft
               , previous
               )
    V.EvKey V.KDel [] ->
        Just $ edit \draft cursor ->
            let next = nextGraphemeBoundary draft cursor
            in ( Text.take cursor draft <> Text.drop next draft
               , cursor
               )
    V.EvKey V.KLeft [] ->
        Just $ move previousGraphemeBoundary
    V.EvKey V.KRight [] ->
        Just $ move nextGraphemeBoundary
    V.EvKey V.KHome [] ->
        Just $ edit \draft cursor ->
            (draft, lineStartCursor draft cursor)
    V.EvKey V.KEnd [] ->
        Just $ edit \draft cursor ->
            (draft, lineEndCursor draft cursor)
    V.EvKey (V.KChar 'w') modifiers
        | V.MCtrl `elem` modifiers ->
            Just $ edit deleteWordBefore
    V.EvKey (V.KChar 'u') modifiers
        | V.MCtrl `elem` modifiers ->
            Just $ edit deleteToLineStart
    V.EvKey (V.KChar 'k') modifiers
        | V.MCtrl `elem` modifiers ->
            Just $ edit deleteToLineEnd
    V.EvKey (V.KChar character) [] ->
        Just $ insert (Text.singleton character)
    V.EvPaste bytes ->
        Just $ insert (Composer.decodePaste bytes)
    _ -> Nothing
  where
    edit = editWith clampGraphemeCursor
    editWith clampCursor change =
        let sourceCursor =
                clampGraphemeCursor prompt.textDraft prompt.textCursor
            (draft, requestedCursor) =
                change prompt.textDraft sourceCursor
        in prompt
            { textDraft = draft
            , textCursor =
                clampCursor draft requestedCursor
            }
    insert raw =
        let inserted =
                normalizeTextOverlayInsertion prompt.textInputMode raw
        in editWith clampInsertedCursor \draft cursor ->
            ( Text.take cursor draft <> inserted <> Text.drop cursor draft
            , cursor + Text.length inserted
            )
    move boundary =
        edit \draft cursor -> (draft, boundary draft cursor)
    -- Inserted text can combine with the following source text. In that case
    -- the requested post-insertion offset lies inside the new grapheme, so
    -- keep the cursor after that grapheme rather than moving it backward.
    clampInsertedCursor draft requestedCursor =
        let boundedCursor =
                max 0 (min (Text.length draft) requestedCursor)
            previous =
                clampGraphemeCursor draft boundedCursor
        in if previous == boundedCursor
            then previous
            else nextGraphemeBoundary draft boundedCursor

resolveTextPrompt :: Bool -> EventM Name AppState ()
resolveTextPrompt confirmed = do
    state <- get
    case state.appTextReply of
        Nothing -> pure ()
        Just reply ->
            liftIO $ atomically $
                putTMVar reply $
                    if confirmed
                        then (.textDraft) <$> state.appTextPrompt
                        else Nothing
    modify' \current ->
        current
            { appTextPrompt = Nothing
            , appTextReply = Nothing
            }
    resumeNativeProgressIfRunning

fullscreenApp :: App AppState AppEvent Name
fullscreenApp = App
    { appDraw = drawApp
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleEvent
    , appStartEvent = do
        state <- get
        liftIO state.appRuntime.runtimeFirstFrame
        vScrollToEnd (viewportScroll ConversationViewport)
    , appAttrMap = \state ->
        if state.appRuntime.runtimeColor
            then Theme.terminalDefault
            else Theme.monochrome
    }

handleUiEvents :: NonEmpty UiEvent -> EventM Name AppState ()
handleUiEvents uiEvents = do
    stored <- get
    viewportBounds <-
        if stored.appAgentSelected == AgentRoot
            then
                lookupViewport ConversationViewport >>= \case
                    Just (VP _ top (_, height) (_, contentHeight)) ->
                        pure (Just (top, height, contentHeight))
                    Nothing ->
                        pure Nothing
            else pure Nothing
    let reconciledFollow =
            if stored.appAgentSelected == AgentRoot
                then
                    Scroll.reconcileConversationFollow
                        stored.appUi.uiFollow
                        viewportBounds
                else stored.appUi.uiFollow
        initial =
            stored
                { appUi =
                    stored.appUi
                        { uiFollow = reconciledFollow
                        }
                }
    timestamp <- liftIO currentShortMessageTimestamp
    renderedContentHeight <-
        if any isSubmittedPrompt uiEvents
            then conversationUnpaddedContentHeight
            else pure 0
    let
        (final, nativeProgress, shouldFollow, shouldInvalidate) =
            foldl'
                (applyOne timestamp renderedContentHeight)
                (initial, Nothing, False, False)
                uiEvents
    put final
    when (any (== UiConversationCleared) uiEvents) $
        clearSubmittedImagePlacements final.appRuntime
    case nativeProgress of
        Nothing -> pure ()
        Just active ->
            liftIO (final.appRuntime.runtimeNativeProgress active)
    when shouldInvalidate invalidateCache
    when shouldFollow $
        case final.appConversationAnchor of
            Just _ -> do
                when (any isSubmittedPrompt uiEvents) $
                    vScrollToEnd (viewportScroll ConversationViewport)
                queueConversationReflow
            Nothing ->
                vScrollToEnd (viewportScroll ConversationViewport)
  where
    applyOne
        timestamp
        renderedContentHeight
        (state, previousProgress, followed, invalidated)
        uiEvent =
            let
                unstamped =
                    applyUiEvent uiEvent $
                        applyConversationUiEvent
                            renderedContentHeight
                            uiEvent
                            state
                next =
                    unstamped
                        { appUi =
                            timestampNewMessageBlocks
                                (Seq.length state.appUi.uiBlocks)
                                timestamp
                                unstamped.appUi
                        }
                progress =
                    case Bridge.nativeProgressSignal
                        (userActionPending next)
                        uiEvent
                        next.appUi of
                        Nothing -> previousProgress
                        signal -> signal
                follows =
                    followed
                        || (Bridge.eventFollows uiEvent
                            && next.appUi.uiFollow)
                invalidates =
                    invalidated || uiEvent == UiConversationCleared
            in (next, progress, follows, invalidates)

applyConversationUiEvent :: Int -> UiEvent -> AppState -> AppState
applyConversationUiEvent renderedContentHeight uiEvent state =
    case uiEvent of
        UiUserSubmitted text ->
            state
                { appConversationAnchor =
                    Just $
                        Scroll.startConversationAnchor
                            (BlockId state.appUi.uiNextBlockId)
                            text
                            (if null state.appUi.uiBlocks
                                then 0
                                else renderedContentHeight)
                , appHistoryLiveStart =
                    case state.appHistoryLiveStart of
                        Just start -> Just start
                        Nothing -> Just (Seq.length state.appUi.uiBlocks)
                }
        UiConversationCleared ->
            state
                { appConversationAnchor = Nothing
                , appConversationReflowQueued = False
                , appHistoryWindow =
                    emptyHistoryWindow
                        state.appHistoryWindow.historyWindowGeneration
                        historyWindowTurnBudget
                        historyWindowBlockBudget
                        historyWindowByteBudget
                , appHistorySelectedBlock = Nothing
                , appHistoryLiveStart = Nothing
                , appNextHistoryBlockId = -1
                , appSubmittedImagePreviews = Map.empty
                }
        _ -> state

applyUiEvent :: UiEvent -> AppState -> AppState
applyUiEvent uiEvent state =
    let
        previousUi = state.appUi
        nextUi = reduceUi uiEvent previousUi
        retainedFlashes =
            case uiEvent of
                UiConversationCleared ->
                    Map.empty
                UiTurnRestarted ->
                    retainExistingFlashes
                        nextUi
                        state.appCompletionFlashes
                _ ->
                    state.appCompletionFlashes
        transitionIds
            | uiEventCanCompleteBlocks uiEvent =
                completionFlashTransitions previousUi nextUi
            | otherwise =
                []
        newFlashes =
            if state.appRuntime.runtimeMotionMode == MotionOff
                then Map.empty
                else Map.fromList
                    [ (blockId, completionFlashDurationMillis)
                    | blockId <- transitionIds
                    ]
        restartSchedule =
            uiEventRestartsMotionSchedule
                uiEvent
                previousUi
                nextUi
                newFlashes
        nextState0 =
            state
                { appUi = nextUi
                , appAutoRecapShownThisAway =
                    case uiEvent of
                        UiRecapReady _ -> True
                        _ -> state.appAutoRecapShownThisAway
                , appLastTurnCompletedAt =
                    case uiEvent of
                        UiLoop (TurnFinished _) -> Just state.appClockNanos
                        UiTurnEnded BlockComplete -> Just state.appClockNanos
                        _ -> state.appLastTurnCompletedAt
                , appCompletionFlashes =
                    Map.union newFlashes retainedFlashes
                , appMotionScheduleReset =
                    state.appMotionScheduleReset || restartSchedule
                , appNativeProgressKeepaliveBucket =
                    if nextUi.uiElapsedMillis < previousUi.uiElapsedMillis
                        then 0
                        else state.appNativeProgressKeepaliveBucket
                }
        nextState =
            case state.appChoice of
                Just choice
                    | choiceClosesOnUiTransition
                        previousUi
                        nextUi
                        choice ->
                        nextState0
                            { appChoice = Nothing
                            , appChoiceReply = Nothing
                            }
                _ -> nextState0
    in Composer.applyComposerUiEvent uiEvent nextState

-- | Turn-scoped choices, such as the live effort selector, become invalid
-- when their turn stops running. Ordinary idle dialogs remain open, and a
-- model round that continues into tools keeps the selector visible.
choiceClosesOnUiTransition
    :: UiState
    -> UiState
    -> ChoiceOverlay
    -> Bool
choiceClosesOnUiTransition previous next choice =
    choice.choiceCloseOnTurnEnd
        && previous.uiRunning
        && not next.uiRunning

retainExistingFlashes
    :: UiState
    -> Map.Map BlockId Int
    -> Map.Map BlockId Int
retainExistingFlashes ui =
    Map.filterWithKey
        (\blockId _ ->
            any ((== blockId) . (.blockId))
                (toList ui.uiBlocks))

uiEventCanCompleteBlocks :: UiEvent -> Bool
uiEventCanCompleteBlocks = \case
    UiLoop (ToolFinished _) -> True
    UiLoop (TurnFinished _) -> True
    UiLoop (ResponseRestarted _) -> True
    UiSetAwaitingInput True -> True
    UiTurnEnded _ -> True
    _ -> False

advanceAppTime :: Word64 -> AppState -> AppState
advanceAppTime now state =
    let
        (elapsedMillis, nextClock) =
            elapsedMillisSince state.appClockNanos now
    in state
        { appUi = advanceUiTime elapsedMillis state.appUi
        , appMotionElapsedMillis =
            state.appMotionElapsedMillis + elapsedMillis
        , appCompletionFlashes =
            advanceCompletionFlashes
                elapsedMillis
                state.appCompletionFlashes
        , appClockNanos = nextClock
        }

advanceAppClockNow :: EventM Name AppState ()
advanceAppClockNow = do
    now <- liftIO getMonotonicTimeNSec
    modify' (advanceAppTime now)

noteTerminalFocusLost :: EventM Name AppState ()
noteTerminalFocusLost = do
    now <- liftIO getMonotonicTimeNSec
    state <- get
    liftIO $
        atomicModifyIORef'
            state.appRuntime.runtimeSyntaxHighlighter
            \syntaxState ->
                ( SyntaxHighlighterInactive
                    (syntaxHighlighterGeneration syntaxState + 1)
                , ()
                )
    liftIO $ atomically $ void $ flushTQueue
        state.appRuntime.runtimeSyntaxRequests
    modify' \state ->
        state
            { appTerminalFocus = TerminalUnfocused
            , appFocusLostAt = Just now
            , appAutoRecapShownThisAway = False
            , appLastAutoRecapAttemptAt = Nothing
            , appSyntaxHighlighter = Nothing
            , appSyntaxRequested = Set.empty
            }
    invalidateCache

noteTerminalFocusGained :: EventM Name AppState ()
noteTerminalFocusGained = do
    maybeRequestAutoRecap
    state <- get
    liftIO $
        atomicModifyIORef'
            state.appRuntime.runtimeSyntaxHighlighter
            \case
                SyntaxHighlighterInactive generation ->
                    (SyntaxHighlighterUnloaded generation, ())
                active ->
                    (active, ())
    modify' \state ->
        state
            { appTerminalFocus = TerminalFocused
            , appFocusLostAt = Nothing
            , appMotionScheduleReset = True
            , appSyntaxRequested = Set.empty
            }
    requestVisibleSyntaxLanguages
    invalidateCache
    getVtyHandle >>= liftIO . V.refresh

maybeRequestAutoRecap :: EventM Name AppState ()
maybeRequestAutoRecap = do
    now <- liftIO getMonotonicTimeNSec
    state <- get
    when (shouldRequestAutoRecap now state) do
        modify' \current ->
            current { appLastAutoRecapAttemptAt = Just now }
        liftIO state.appRuntime.runtimeRecap

shouldRequestAutoRecap :: Word64 -> AppState -> Bool
shouldRequestAutoRecap now state =
    state.appTerminalFocus == TerminalUnfocused
        && not state.appAutoRecapShownThisAway
        && not (userActionPending state)
        && not state.appUi.uiRunning
        && not (hasBackgroundActivity state.appAgentEntries)
        && elapsedSeconds now state.appFocusLostAt >= autoRecapAwayThreshold
        && elapsedSeconds now state.appLastTurnCompletedAt
            >= autoRecapIdleThreshold
        && ( case state.appLastAutoRecapAttemptAt of
                Nothing -> True
                Just attempted ->
                    elapsedSeconds now (Just attempted)
                        >= autoRecapRetryInterval
           )

elapsedSeconds :: Word64 -> Maybe Word64 -> NominalDiffTime
elapsedSeconds _ Nothing = 0
elapsedSeconds now (Just started) =
    realToFrac (now - started) / 1_000_000_000

applyLocalUiEvent :: UiEvent -> EventM Name AppState ()
applyLocalUiEvent event =
    applyLocalUiEventWith event id

applyLocalUiEventWith
    :: UiEvent
    -> (AppState -> AppState)
    -> EventM Name AppState ()
applyLocalUiEventWith event update = do
    advanceAppClockNow
    modify' (update . applyUiEvent event)

refreshNativeProgressKeepalive :: EventM Name AppState ()
refreshNativeProgressKeepalive = do
    state <- get
    let bucket = state.appUi.uiElapsedMillis `div` 5000
    when
        (nativeProgressKeepaliveDue
            (userActionPending state)
            state.appNativeProgressKeepaliveBucket
            state.appUi) do
        liftIO (state.appRuntime.runtimeNativeProgress True)
        modify' \current ->
            current { appNativeProgressKeepaliveBucket = bucket }

handleEvent :: BrickEvent Name AppEvent -> EventM Name AppState ()
handleEvent event = do
    advanceAppClockNow
    stateBeforeEvent <- get
    when (isMotionTick event) refreshNativeProgressKeepalive
    handleEventInner event
    when (eventMayExposeSyntax event) requestVisibleSyntaxLanguages
    state <- get
    let visible =
            isNothing state.appTextPrompt
                && isNothing state.appChoice
                && isNothing state.appResume
                && isNothing state.appUi.uiPermission
                && isNothing state.appAgentHover
    liftIO do
        previous <-
            readIORef state.appRuntime.runtimeImagePreviewVisible
        when (previous /= visible) do
            writeIORef
                state.appRuntime.runtimeImagePreviewVisible
                visible
            modifyIORef'
                state.appRuntime.runtimeImagePreviewRevision
                (+ 1)
    syncMotionDemand
    stateAfterMotionSync <- get
    when
        ( stateAfterMotionSync.appTerminalFocus == TerminalUnfocused
            && not
                (turnCompletionRequiresRedraw
                    stateBeforeEvent.appUi
                    stateAfterMotionSync.appUi)
        ) $
        continueWithoutRedraw
  where
    isMotionTick = \case
        AppEvent AppMotionTick -> True
        _ -> False

eventMayExposeSyntax :: BrickEvent Name AppEvent -> Bool
eventMayExposeSyntax = \case
    AppEvent (AppUi uiEvent) ->
        uiEventMayExposeSyntax uiEvent
    AppEvent (AppUiBatch uiEvents) ->
        any uiEventMayExposeSyntax uiEvents
    AppEvent AppSyntaxHighlighterChanged -> True
    AppEvent (AppHistoryReset _) -> True
    AppEvent (AppHistoryLoaded _ _) -> True
    AppEvent (AppHistoryCommitted _ _ _) -> True
    AppEvent AppHistoryLiveStarted -> True
    AppEvent (AppAgentSnapshot _ _) -> True
    _ -> False

uiEventMayExposeSyntax :: UiEvent -> Bool
uiEventMayExposeSyntax = \case
    UiLoop (TextDelta delta) ->
        Text.isInfixOf "```" delta
            || Text.isInfixOf "~~~" delta
    UiLoop (ReasoningDelta _) -> False
    UiLoop (ActivityUpdated _) -> False
    UiLoop (WarningRaised _) -> False
    UiLoop (ToolOutputUpdated _ _) -> False
    UiSetDraft _ _ -> False
    UiSetPrompt _ -> False
    UiSetPromptEffort _ -> False
    UiSetPromptLimitStatus _ -> False
    UiSetAwaitingInput _ -> False
    UiSetRepository _ _ _ -> False
    UiSetNotice _ -> False
    UiMoveSelection _ -> False
    UiSelectBlock _ -> False
    UiActivateBlock _ -> False
    UiToggleSelected -> False
    UiFocusChanged _ -> False
    UiPermissionShown _ -> False
    UiPermissionMoved _ -> False
    UiPermissionHidden -> False
    UiSetFollow _ -> False
    _ -> True

requestVisibleSyntaxLanguages :: EventM Name AppState ()
requestVisibleSyntaxLanguages = do
    state <- get
    let
        languages =
            syntaxLanguagesForBlocks (visibleConversationBlocks state)
        missing =
            Set.difference languages state.appSyntaxRequested
    when
        ( state.appTerminalFocus /= TerminalUnfocused
            && not (Set.null missing)
        ) do
        liftIO $
            atomically $
                mapM_
                    (writeTQueue state.appRuntime.runtimeSyntaxRequests)
                    (Set.toList missing)
        modify' \current ->
            current
                { appSyntaxRequested =
                    Set.union current.appSyntaxRequested missing
                }

visibleConversationBlocks :: AppState -> [UiBlock]
visibleConversationBlocks state =
    conversationBlocks state.appAgentSelected state

syntaxLanguagesForBlocks :: [UiBlock] -> Set.Set Text
syntaxLanguagesForBlocks =
    Set.fromList . concatMap syntaxLanguagesForBlock

syntaxLanguagesForBlock :: UiBlock -> [Text]
syntaxLanguagesForBlock block =
    case block.blockKind of
        BlockAssistant ->
            mapMaybe
                (resolveFenceLanguage . (.fencedInfo))
                (fencedBlocks block.blockBody)
        BlockShell
            | not (Text.null (Text.strip block.blockDetail)) ->
                ["haskell"]
        _ -> []

syncMotionDemand :: EventM Name AppState ()
syncMotionDemand = do
    advanceAppClockNow
    state <- get
    let
        (demand, delayMicros) = appMotionTiming state
        resetSchedule = state.appMotionScheduleReset
    liftIO $
        atomically do
            current <-
                readTVar state.appRuntime.runtimeMotionSchedule
            let next =
                    nextMotionSchedule
                        resetSchedule
                        demand
                        delayMicros
                        current
            when (next /= current) $
                writeTVar
                    state.appRuntime.runtimeMotionSchedule
                    next
    modify' \current ->
        current
            { appMotionScheduleReset = False }

handleEventInner :: BrickEvent Name AppEvent -> EventM Name AppState ()
handleEventInner event = case event of
    AppEvent AppMotionTick -> do
        state <- get
        liftIO $
            atomically $
                writeTVar
                    state.appRuntime.runtimeMotionTickQueued
                    False
    AppEvent AppRecapPoll ->
        maybeRequestAutoRecap
    AppEvent AppStop -> do
        state <- get
        liftIO $
            mapM_
                (`Composer.requestDictationStop` True)
                state.appDictation
        modify' \current ->
            current
                { appWorkerStopped = True
                , appDictation = Nothing
                }
        halt
    AppEvent (AppSetSlashCatalog catalog) -> do
        state <- get
        if state.appSlashCatalog == catalog
            then pure ()
            else modify' \current -> current
                { appSlashCatalog = catalog
                , appSlashIndex = 0
                , appSlashDismissed = False
                }
    AppEvent (AppSetSkillCommands skills) -> do
        state <- get
        if state.appSlashCatalog.slashCatalogSkills == skills
            then pure ()
            else
                let catalog =
                        slashCatalogWithSkills skills state.appSlashCatalog
                in modify' \current -> current
                    { appSlashCatalog = catalog
                    , appSlashIndex = 0
                    , appSlashDismissed = False
                    }
    AppEvent (AppSetModelIds modelIds) -> do
        state <- get
        if state.appSlashCatalog.slashCatalogModelIds == modelIds
            then pure ()
            else
                let catalog =
                        state.appSlashCatalog
                            { slashCatalogModelIds = modelIds
                            }
                in modify' \current -> current
                    { appSlashCatalog = catalog
                    , appSlashIndex = 0
                    , appSlashDismissed = False
                    }
    AppEvent (AppSetImagePreviews prepared) ->
        do
            state <- get
            previous <-
                liftIO $
                    readIORef state.appRuntime.runtimeImagePreviews
            let unchanged = map fst previous == map fst prepared
            liftIO do
                when (not unchanged) do
                    writeIORef
                        state.appRuntime.runtimeImagePreviews
                        prepared
                    modifyIORef'
                        state.appRuntime.runtimeImagePreviewRevision
                        (+ 1)
            modify' \current ->
                current
                    { appImagePreviews = map snd prepared
                    }
    AppEvent (AppCommitImagePreviews prepared) -> do
        state <- get
        let previews = map snd prepared
            nextBlockId = BlockId state.appUi.uiNextBlockId
            submitted =
                if null previews
                    then Map.delete
                        nextBlockId
                        state.appSubmittedImagePreviews
                    else Map.insert
                        nextBlockId
                        previews
                        state.appSubmittedImagePreviews
        liftIO do
            writeIORef state.appRuntime.runtimeImagePreviews []
            modifyIORef'
                state.appRuntime.runtimeImagePreviewRevision
                (+ 1)
        modify' \current ->
            current
                { appImagePreviews = []
                , appSubmittedImagePreviews = submitted
                }
        queueConversationReflow
    AppEvent (AppDictationPartial text) -> do
        state <- get
        when (isJust state.appDictation) $
            applyLocalUiEvent
                (UiSetNotice (Just (Composer.dictationProgressNotice text)))
    AppEvent (AppDictationFinished result) -> do
        state <- get
        aborted <-
            case state.appDictation of
                Just session ->
                    liftIO (readIORef session.dictationAbort)
                Nothing ->
                    pure False
        modify' \current -> current { appDictation = Nothing }
        if aborted
            then applyLocalUiEvent $
                UiSetNotice $
                    Just (infoNotice "Dictation cancelled.")
            else case result of
                Left message ->
                    applyLocalUiEvent $
                        UiSetNotice $
                            Just $
                                warningNotice ("Dictation failed: " <> message)
                Right transcript -> do
                    let ui = state.appUi
                        (draft, cursor) =
                            insertDictation ui.uiDraft ui.uiCursor transcript
                    applyLocalUiEvent (UiSetDraft draft cursor)
                    applyLocalUiEvent $
                        UiSetNotice $
                            Just $
                                successNotice "Dictation inserted."
    AppEvent (AppSetWindowTitle title) -> do
        vty <- getVtyHandle
        liftIO (writeOutputWindowTitle (V.outputIface vty) title)
        modify' \current -> current { appWindowTitle = Just title }
    AppEvent AppSyntaxHighlighterChanged -> do
        state <- get
        when (state.appTerminalFocus /= TerminalUnfocused) do
            highlighter <-
                liftIO $
                    readIORef state.appRuntime.runtimeSyntaxHighlighter
            modify' \current ->
                current
                    { appSyntaxHighlighter =
                        case highlighter of
                            SyntaxHighlighterActive _ loaded -> loaded
                            SyntaxHighlighterUnloaded _ -> Nothing
                            SyntaxHighlighterInactive _ -> Nothing
                    }
            invalidateCache
    AppEvent (AppHistoryReset page) -> do
        modify' (resetHistoryPage page)
        invalidateCache
        queueConversationReflow
    AppEvent (AppHistoryLoaded request result) -> do
        state <- get
        when
            (request.historyRequestGeneration
                == state.appHistoryWindow.historyWindowGeneration)
            do
                let anchorBlock =
                        historyPageAnchorBlock
                            request.historyRequestDirection
                            state.appHistoryWindow
                case result of
                    Left err ->
                        modify' \current ->
                            applyUiEvent
                                (UiSetNotice
                                    (Just (warningNotice
                                        ("Could not load session history: "
                                            <> err))))
                                (clearHistoryPending request current)
                    Right page ->
                        modify' (applyLoadedHistoryPage page)
                invalidateCache
                case anchorBlock of
                    Nothing -> pure ()
                    Just blockId ->
                        makeVisible
                            (ConversationBlock AgentRoot blockId)
                queueConversationReflow
    AppEvent AppHistoryLiveStarted ->
        modify' \state ->
            state
                { appHistoryLiveStart =
                    case state.appHistoryLiveStart of
                        Just start -> Just start
                        Nothing ->
                            Just (Seq.length state.appUi.uiBlocks)
                }
    AppEvent (AppHistoryCommitted generation turn commit) -> do
        state <- get
        let currentGeneration =
                state.appHistoryWindow.historyWindowGeneration
            applicable = case commit of
                HistoryCommitAppend ->
                    currentGeneration == generation
                _ ->
                    currentGeneration < generation
        when applicable do
            modify'
                (commitLiveHistoryTurn turn commit
                    . setHistoryGeneration generation)
            invalidateCache
            queueConversationReflow
    AppEvent (AppAgentSnapshot selected entries) -> do
        state <- get
        let normalized =
                Bridge.normalizeAgentSelection selected entries
            mergedEntries =
                preserveAgentConversationView
                    normalized
                    state.appAgentEntries
                    entries
            selectionChanged =
                state.appAgentSelected /= normalized
            selectedConversationChanged =
                case normalized of
                    AgentRoot -> False
                    target ->
                        fmap (.agentConversation)
                            (lookupAgentEntry target state.appAgentEntries)
                            /= fmap (.agentConversation)
                                (lookupAgentEntry target mergedEntries)
        if state.appAgentSelected == normalized
            && state.appAgentEntries == mergedEntries
            then pure ()
            else do
                modify' \current ->
                    current
                        { appAgentSelected = normalized
                        , appAgentEntries = mergedEntries
                        , appAgentHover =
                            if normalized /= current.appAgentSelected
                                || length entries <= 1
                                || agentLayoutTargets mergedEntries
                                    /= agentLayoutTargets
                                        current.appAgentEntries
                                then Nothing
                                else
                                    current.appAgentHover >>= \hover ->
                                        hover <$
                                            lookupAgentEntry
                                                hover.agentHoverTarget
                                                mergedEntries
                        }
                when selectedConversationChanged invalidateCache
                if selectionChanged
                    then resumeConversationFollow
                    else when
                        (selectedConversationChanged
                            && state.appUi.uiFollow)
                        do
                            vScrollToEnd
                                (viewportScroll ConversationViewport)
                            queueConversationReflow
                when
                    ((length state.appAgentEntries > 1)
                        /= (length mergedEntries > 1))
                    do
                        invalidateCache
                        queueConversationReflow
    AppEvent (AppUi uiEvent) ->
        handleUiEvents (uiEvent :| [])
    AppEvent (AppUiBatch uiEvents) ->
        handleUiEvents uiEvents
    AppEvent AppConversationReflow -> do
        modify' \state ->
            state { appConversationReflowQueued = False }
        reflowConversation
        state <- get
        liftIO $
            enqueueAppEvent
                state.appRuntime
                AppSyncSubmittedImagePlacements
    AppEvent AppSyncSubmittedImagePlacements ->
        syncSubmittedImagePlacements
    AppEvent (AppAskPermission summary reply) -> do
        state <- get
        liftIO (state.appRuntime.runtimeNativeProgress False)
        applyLocalUiEventWith
            (UiPermissionShown summary)
            \current ->
                current
                    { appPermissionReply = Just reply
                    , appAgentHover = Nothing
                    }
    AppEvent (AppAskChoice presentation title body initial rows reply) -> do
        state <- get
        liftIO (state.appRuntime.runtimeNativeProgress False)
        modify' \state ->
            state
                { appChoice = Just ChoiceOverlay
                    { choicePresentation = presentation
                    , choiceTitle = title
                    , choiceBody = body
                    , choiceIndex =
                        max 0 (min (max 0 (length rows - 1)) initial)
                    , choiceRows = rows
                    , choiceCloseOnTurnEnd = False
                    }
                , appChoiceReply = Just (atomically . putTMVar reply)
                , appAgentHover = Nothing
                }
        vScrollToBeginning (viewportScroll OverlayViewport)
    AppEvent
        (AppAskResume browser loadEntry deleteEntry searchEntries reply) -> do
        state <- get
        liftIO (state.appRuntime.runtimeNativeProgress False)
        modify' \state ->
            state
                { appResume = Just ResumeOverlay
                    { resumeOverlayBrowser = browser
                    }
                , appResumeReply = Just reply
                , appResumeLoad = Just loadEntry
                , appResumeDelete = Just deleteEntry
                , appResumeSearch = Just searchEntries
                , appAgentHover = Nothing
                }
        vScrollToBeginning (viewportScroll ResumeViewport)
    AppEvent (AppAskText mode title body initial reply) -> do
        state <- get
        liftIO (state.appRuntime.runtimeNativeProgress False)
        modify' \state ->
            state
                { appTextPrompt = Just TextOverlay
                    { textTitle = title
                    , textBody = body
                    , textDraft = initial
                    , textCursor = Text.length initial
                    , textInputMode = mode
                    }
                , appTextReply = Just reply
                , appAgentHover = Nothing
                }
        vScrollToBeginning (viewportScroll OverlayViewport)
    AppEvent (AppSuspend action reply) -> do
        state <- get
        suspendAndResume do
            result <- tryAny action
            mapM_
                (setFullscreenWindowTitle state.appRuntime)
                state.appWindowTitle
            atomically (putTMVar reply result)
            pure state
                { appAgentHover = Nothing
                , appTerminalFocus = TerminalFocusUnknown
                , appMotionScheduleReset = True
                }
    MouseDown name button _ _ -> do
        unless (isAgentHoverSurface name) clearAgentHover
        state <- get
        case state.appResume of
            Just _ ->
                case (name, button) of
                    (ResumeRow sessionId, V.BLeft) ->
                        Composer.handleControlMouseDown
                            (ResumeRow sessionId)
                    (_, V.BScrollUp) ->
                        handleResumeKey
                            (V.EvMouseDown 0 0 V.BScrollUp [])
                    (_, V.BScrollDown) ->
                        handleResumeKey
                            (V.EvMouseDown 0 0 V.BScrollDown [])
                    _ -> pure ()
            Nothing ->
                case ( state.appTextPrompt
                     , state.appChoice
                     , state.appUi.uiPermission
                     ) of
                    (Just _, _, _) ->
                        case button of
                            V.BScrollUp ->
                                vScrollBy
                                    (viewportScroll OverlayViewport)
                                    (-mouseScrollLines)
                            V.BScrollDown ->
                                vScrollBy
                                    (viewportScroll OverlayViewport)
                                    mouseScrollLines
                            _ -> pure ()
                    (Nothing, Nothing, Nothing) ->
                        case (name, button) of
                            (ComposerModel, V.BLeft) ->
                                Composer.handleControlMouseDown ComposerModel
                            (ComposerEffort, V.BLeft) ->
                                Composer.handleControlMouseDown ComposerEffort
                            (ComposerMode, V.BLeft) ->
                                Composer.handleControlMouseDown ComposerMode
                            (ComposerAccount, V.BLeft) ->
                                Composer.handleControlMouseDown ComposerAccount
                            (name, V.BLeft)
                                | isQuickStartControl name ->
                                    Composer.handleControlMouseDown name
                            (CodeCopy target blockId codeIndex, V.BLeft) ->
                                Composer.handleControlMouseDown
                                    (CodeCopy target blockId codeIndex)
                            (SlashRow index, V.BLeft) ->
                                Composer.activateSlashAt
                                    applyLocalUiEventWith
                                    handleCtrlC
                                    scrollConversationPage
                                    index
                            (SlashRow _, V.BScrollUp) ->
                                Composer.handleComposerKey
                                    applyLocalUiEventWith
                                    handleCtrlC
                                    scrollConversationPage
                                    (V.EvKey V.KUp [])
                            (SlashRow _, V.BScrollDown) ->
                                Composer.handleComposerKey
                                    applyLocalUiEventWith
                                    handleCtrlC
                                    scrollConversationPage
                                    (V.EvKey V.KDown [])
                            (AgentRow target, V.BLeft) -> do
                                clearAgentHover
                                selectAgentView target
                            (AgentPopover target, V.BLeft) -> do
                                keepAgentHover target
                                selectAgentView target
                            (link@MarkdownLink{}, V.BLeft) ->
                                Composer.handleControlMouseDown link
                            _ -> handleMouseDown name button
                    (Nothing, Just _, _) ->
                        case (name, button) of
                            (ChoiceRow index, V.BLeft) ->
                                Composer.handleControlMouseDown (ChoiceRow index)
                            (ChoiceRow _, V.BScrollUp) ->
                                handleChoiceKey (V.EvKey V.KUp [])
                            (ChoiceRow _, V.BScrollDown) ->
                                handleChoiceKey (V.EvKey V.KDown [])
                            (_, V.BScrollUp) ->
                                vScrollBy
                                    (viewportScroll OverlayViewport)
                                    (-mouseScrollLines)
                            (_, V.BScrollDown) ->
                                vScrollBy
                                    (viewportScroll OverlayViewport)
                                    mouseScrollLines
                            _ -> pure ()
                    (Nothing, Nothing, Just _) ->
                        case (name, button) of
                            (PermissionRow index, V.BLeft) ->
                                resolvePermission (permissionChoiceAt index)
                            _ -> pure ()
    -- The patched vty-unix backend represents no-button pointer motion as
    -- MouseUp Nothing so Brick can route it through clickable extents.
    MouseUp (AgentRow target) Nothing _ ->
        rememberAgentHover target
    MouseUp (AgentPopover target) Nothing _ ->
        keepAgentHover target
    MouseUp AgentPane Nothing _ ->
        pure ()
    MouseUp link@MarkdownLink{} (Just V.BLeft) _ -> do
        clearAgentHover
        Composer.handleControlMouseUp link (activateControl link)
    MouseUp name button _
        | isInteractiveControl name
        , button == Just V.BLeft || button == Nothing -> do
            clearAgentHover
            Composer.handleControlMouseUp name (activateControl name)
    MouseUp _ Nothing _ ->
        modify' \state ->
            state
                { appHoveredControl = Nothing
                , appAgentHover = Nothing
                }
    VtyEvent (V.EvMouseDown _ _ V.BLeft _) ->
        modify' \state ->
            state
                { appHoveredControl = Nothing
                , appAgentHover = Nothing
                }
    VtyEvent (V.EvMouseUp _ _ _) ->
        modify' \state ->
            state
                { appHoveredControl = Nothing
                , appPressedControl = Nothing
                , appAgentHover = Nothing
                }
    VtyEvent V.EvLostFocus ->
        noteTerminalFocusLost
    VtyEvent V.EvGainedFocus ->
        noteTerminalFocusGained
    VtyEvent V.EvResize{} -> do
        clearAgentHover
        invalidateCache
        -- A focused resize can leave cells from the previous geometry. Hidden
        -- terminals defer the reset until their focus-gained refresh.
        state <- get
        when (state.appTerminalFocus /= TerminalUnfocused) $
            getVtyHandle >>= liftIO . V.refresh
        queueConversationReflow
    VtyEvent vtyEvent -> do
        clearAgentHover
        state <- get
        case state.appResume of
            Just _ -> handleResumeKey vtyEvent
            Nothing ->
                case (state.appTextPrompt, state.appChoice, state.appUi.uiPermission) of
                    (Just _, _, _) -> handleTextPromptKey vtyEvent
                    (Nothing, Just _, _) -> handleChoiceKey vtyEvent
                    (Nothing, Nothing, Just _) -> handlePermissionKey vtyEvent
                    (Nothing, Nothing, Nothing) -> handleNormalKey vtyEvent
    _ -> pure ()

handlePermissionKey :: V.Event -> EventM Name AppState ()
handlePermissionKey = \case
    V.EvKey V.KUp [] -> movePermission (-1)
    V.EvKey V.KDown [] -> movePermission 1
    V.EvKey V.KBackTab [] -> movePermission (-1)
    V.EvKey (V.KChar '\t') [] -> movePermission 1
    V.EvKey (V.KChar 'y') [] -> resolvePermission PermissionAllowOnce
    V.EvKey (V.KChar 'A') [] -> resolvePermission PermissionAllowAll
    V.EvKey (V.KChar 'a') [] -> resolvePermission PermissionAllowTool
    V.EvKey (V.KChar 'n') [] -> resolvePermission PermissionDeny
    V.EvKey V.KEsc [] -> resolvePermission PermissionDeny
    V.EvKey (V.KChar 'c') modifiers
        | V.MCtrl `elem` modifiers -> do
            _ <- handleCtrlC
            resolvePermission PermissionDeny
    V.EvKey V.KEnter [] -> do
        state <- get
        let choice = case state.appUi.uiPermission of
                Just permission ->
                    permissionChoiceAt permission.permissionIndex
                Nothing -> PermissionDeny
        resolvePermission choice
    _ -> pure ()
  where
    movePermission delta =
        applyLocalUiEvent (UiPermissionMoved delta)

permissionChoiceAt :: Int -> PermissionChoice
permissionChoiceAt = \case
    0 -> PermissionAllowOnce
    1 -> PermissionAllowAll
    2 -> PermissionAllowTool
    _ -> PermissionDeny

resolvePermission
    :: PermissionChoice
    -> EventM Name AppState ()
resolvePermission choice = do
    state <- get
    case state.appPermissionReply of
        Nothing -> pure ()
        Just reply ->
            liftIO $ atomically (putTMVar reply (Just choice))
    applyLocalUiEventWith UiPermissionHidden \current ->
        current { appPermissionReply = Nothing }
    resumeNativeProgressIfRunning

resumeNativeProgressIfRunning :: EventM Name AppState ()
resumeNativeProgressIfRunning = do
    state <- get
    when
        (state.appUi.uiRunning
            && not (userActionPending state)) do
        liftIO (state.appRuntime.runtimeNativeProgress True)
        modify' \current ->
            current
                { appNativeProgressKeepaliveBucket =
                    current.appUi.uiElapsedMillis `div` 5000
                }

handleCtrlC :: EventM Name AppState CtrlCDecision
handleCtrlC = do
    state <- get
    decision <- liftIO state.appRuntime.runtimeCtrlC
    case decision of
        SoftCancel ->
            applyLocalUiEvent $
                UiSetNotice $
                    Just $
                        warningNotice
                            "Interrupted; press Ctrl-C again to exit."
        WarnExit ->
            applyLocalUiEvent $
                UiSetNotice $
                    Just $
                        warningNotice
                            "Press Ctrl-C again to exit."
        ForceExit ->
            liftIO (throwIO UserInterrupt)
    pure decision

handleNormalKey :: V.Event -> EventM Name AppState ()
handleNormalKey event = do
    state <- get
    case state.appDictation of
        Just session ->
            Composer.handleDictationKey handleCtrlC session event
        Nothing
            | Bridge.isSendNowKey event ->
                Composer.handleComposerKey
                    applyLocalUiEventWith
                    handleCtrlC
                    scrollConversationPage
                    event
            | otherwise ->
                case event of
                    V.EvMouseDown _ _ V.BScrollUp _ ->
                        scrollConversationBy (-mouseScrollLines)
                    V.EvMouseDown _ _ V.BScrollDown _ ->
                        scrollConversationBy mouseScrollLines
                    _ ->
                        case state.appUi.uiFocus of
                            FocusScrollback -> handleScrollbackKey event
                            FocusComposer ->
                                Composer.handleComposerKey
                                    applyLocalUiEventWith
                                    handleCtrlC
                                    scrollConversationPage
                                    event
                            FocusPermission -> pure ()

rememberAgentHover :: AgentTarget -> EventM Name AppState ()
rememberAgentHover target = do
    rowExtent <- lookupExtent (AgentRow target)
    paneExtent <- lookupExtent AgentPane
    case rowExtent of
        Nothing -> pure ()
        Just extent ->
            let Location (rowX, rowY) = extent.extentUpperLeft
                fallbackPaneUpperLeft =
                    Location (max 0 (rowX - 2), max 0 (rowY - 2))
                fallbackPaneWidth = extentWidth extent + 4
            in
            modify' \state ->
                state
                    { appAgentHover =
                        Just AgentHover
                            { agentHoverTarget = target
                            , agentHoverUpperLeft = extent.extentUpperLeft
                            , agentHoverPaneUpperLeft =
                                maybe
                                    fallbackPaneUpperLeft
                                    (.extentUpperLeft)
                                    paneExtent
                            , agentHoverPaneWidth =
                                maybe
                                    fallbackPaneWidth
                                    extentWidth
                                    paneExtent
                            }
                    }
  where
    extentWidth extent = fst extent.extentSize

keepAgentHover :: AgentTarget -> EventM Name AppState ()
keepAgentHover target = do
    state <- get
    case state.appAgentHover of
        Just hover
            | hover.agentHoverTarget == target -> pure ()
        _ -> rememberAgentHover target

clearAgentHover :: EventM Name AppState ()
clearAgentHover =
    modify' \state ->
        state { appAgentHover = Nothing }

selectAgentView :: AgentTarget -> EventM Name AppState ()
selectAgentView target = do
    state <- get
    liftIO (state.appRuntime.runtimeAgentSelect target)
    when (state.appAgentSelected /= target) do
        modify' \current ->
            current { appAgentSelected = target }
        requestVisibleSyntaxLanguages
        resumeConversationFollow

isAgentHoverSurface :: Name -> Bool
isAgentHoverSurface = \case
    AgentPane -> True
    AgentRow _ -> True
    AgentPopover _ -> True
    _ -> False

agentLayoutTargets :: [AgentEntry] -> [AgentTarget]
agentLayoutTargets =
    map (.agentTarget) . sortOn (.agentPath)

preserveAgentConversationView
    :: AgentTarget
    -> [AgentEntry]
    -> [AgentEntry]
    -> [AgentEntry]
preserveAgentConversationView selected previous =
    map preserve
  where
    preserve incoming
        | incoming.agentTarget /= selected = incoming
        | otherwise =
            case lookupAgentEntry incoming.agentTarget previous of
                Nothing -> incoming
                Just old ->
                    incoming
                        { agentConversation =
                            mergeConversationView
                                old.agentConversation
                                incoming.agentConversation
                        }

mergeConversationView :: UiState -> UiState -> UiState
mergeConversationView previous incoming
    | Seq.null incoming.uiBlocks =
        incoming
            { uiTodos =
                if null incoming.uiTodos
                    then previous.uiTodos
                    else incoming.uiTodos
            }
    | otherwise =
        incoming
            { uiBlocks = mergedBlocks
            , uiSelectedBlock = selected
            , uiSelectedBlockIndex =
                selected >>= (`Map.lookup` incoming.uiBlockIndices)
            , uiTodos =
                if null incoming.uiTodos
                    then previous.uiTodos
                    else incoming.uiTodos
            }
  where
    previousBlocks =
        Map.fromList
            [ (block.blockId, block)
            | block <- toList previous.uiBlocks
            ]
    mergedBlocks =
        fmap
            (\block ->
                case Map.lookup block.blockId previousBlocks of
                    Just old
                        | sameConversationBlock old block ->
                            block
                                { blockExpanded = old.blockExpanded
                                }
                    _ -> block)
            incoming.uiBlocks
    selected =
        case previous.uiSelectedBlock of
            Just ident
                | Just old <- Map.lookup ident previousBlocks
                , Just new <-
                    Map.lookup ident incoming.uiBlockIndices
                        >>= \index -> Seq.lookup index incoming.uiBlocks
                , sameConversationBlock old new ->
                    Just ident
            _ -> incoming.uiSelectedBlock

sameConversationBlock :: UiBlock -> UiBlock -> Bool
sameConversationBlock previous incoming =
    previous.blockKind == incoming.blockKind
        && previous.blockTitle == incoming.blockTitle
        && case (previous.blockCallId, incoming.blockCallId) of
            (Just oldCall, Just newCall) -> oldCall == newCall
            (Nothing, Nothing) ->
                previous.blockBody == incoming.blockBody
            _ -> False

handleMouseDown :: Name -> V.Button -> EventM Name AppState ()
handleMouseDown name button =
    case button of
        V.BScrollUp -> scrollConversationBy (-mouseScrollLines)
        V.BScrollDown -> scrollConversationBy mouseScrollLines
        V.BLeft -> case name of
            ConversationBlock target ident ->
                activateConversationBlock target ident
            ComposerArea ->
                applyLocalUiEvent (UiFocusChanged FocusComposer)
            _ -> pure ()
        _ -> pure ()

mouseScrollLines :: Int
mouseScrollLines = 3

handleScrollbackKey :: V.Event -> EventM Name AppState ()
handleScrollbackKey = \case
    V.EvKey V.KUp [] -> moveBlock (-1)
    V.EvKey V.KDown [] -> moveBlock 1
    V.EvKey V.KPageUp [] -> scrollConversationPage Up
    V.EvKey V.KPageDown [] -> scrollConversationPage Down
    V.EvKey (V.KChar 'k') modifiers
        | V.MCtrl `elem` modifiers -> scrollConversationBy (-1)
    V.EvKey (V.KChar 'j') modifiers
        | V.MCtrl `elem` modifiers -> scrollConversationBy 1
    V.EvKey (V.KChar 'u') modifiers
        | V.MCtrl `elem` modifiers -> scrollConversationHalfPage Up
    V.EvKey (V.KChar 'd') modifiers
        | V.MCtrl `elem` modifiers -> scrollConversationHalfPage Down
    V.EvKey V.KHome [] -> do
        leaveFollow
        requestHistoryPage HistoryOlder
        vScrollToBeginning scroll
        queueConversationReflow
    V.EvKey V.KEnd [] -> resumeConversationFollow
    V.EvKey (V.KChar 'g') [] -> do
        leaveFollow
        requestHistoryPage HistoryOlder
        vScrollToBeginning scroll
        queueConversationReflow
    V.EvKey (V.KChar 'G') [] -> resumeConversationFollow
    V.EvKey V.KLeft [] -> toggle
    V.EvKey V.KRight [] -> toggle
    V.EvKey V.KEnter [] -> toggle
    V.EvKey (V.KChar 'y') [] -> copySelected
    V.EvKey (V.KChar ' ') [] -> focusComposer
    V.EvKey (V.KChar '\t') [] -> focusComposer
    V.EvKey V.KEsc [] -> focusComposer
    _ -> pure ()
  where
    scroll = viewportScroll ConversationViewport
    moveBlock delta = do
        state <- get
        let target = state.appAgentSelected
            blocks = conversationBlocks target state
            selected =
                selectedConversationBlockId target state
            current =
                fromMaybe
                    (if delta < 0 then length blocks else -1)
                    (selected >>= \ident ->
                        findIndex ((== ident) . (.blockId)) blocks)
            nextIndex =
                max 0 (min (length blocks - 1) (current + delta))
        case drop nextIndex blocks of
            block : _ -> do
                selectConversationBlock target block.blockId
                makeVisible (ConversationBlock target block.blockId)
                queueConversationReflow
            [] -> pure ()
    toggle = do
        state <- get
        case (state.appAgentSelected, state.appHistorySelectedBlock) of
            (AgentRoot, Just blockId) ->
                modify' \current ->
                    current
                        { appHistoryWindow =
                            mapHistoryBlock
                                blockId
                                (\block ->
                                    block
                                        { blockExpanded =
                                            not block.blockExpanded
                                        })
                                current.appHistoryWindow
                        }
            _ -> applyActiveConversationUiEvent UiToggleSelected
        queueConversationReflow
    focusComposer =
        do
            modify' \state -> state { appHistorySelectedBlock = Nothing }
            applyLocalUiEvent (UiFocusChanged FocusComposer)
    leaveFollow =
        applyLocalUiEvent (UiSetFollow False)
    copySelected = do
        state <- get
        case selectedConversationBlock state.appAgentSelected state of
            Nothing -> pure ()
            Just block -> do
                copied <- liftIO $
                    state.appRuntime.runtimeCopy block.blockBody
                applyLocalUiEvent $
                    UiSetNotice $
                        Just $
                            if copied
                                then successNotice
                                    "Copied selected block."
                                else warningNotice
                                    "Terminal clipboard is unavailable."

resumeConversationFollow :: EventM Name AppState ()
resumeConversationFollow = do
    applyLocalUiEventWith (UiSetFollow True) \state ->
        state
            { appConversationAnchor =
                Scroll.followConversationTail
                    <$> state.appConversationAnchor
            }
    vScrollToEnd (viewportScroll ConversationViewport)
    queueConversationReflow

applyActiveConversationUiEvent :: UiEvent -> EventM Name AppState ()
applyActiveConversationUiEvent uiEvent = do
    state <- get
    case state.appAgentSelected of
        AgentRoot ->
            applyLocalUiEvent uiEvent
        target@(AgentChild _) ->
            modify' (applyChildConversationUiEvent target uiEvent)
        target@(AgentNative _) ->
            modify' (applyChildConversationUiEvent target uiEvent)

scrollConversationPage :: Direction -> EventM Name AppState ()
scrollConversationPage direction = do
    height <- conversationViewportHeight
    scrollConversationBy $
        case direction of
            Up -> negate height
            Down -> height

scrollConversationHalfPage :: Direction -> EventM Name AppState ()
scrollConversationHalfPage direction = do
    height <- conversationViewportHeight
    let amount = max 1 (height `div` 2)
    scrollConversationBy $
        case direction of
            Up -> negate amount
            Down -> amount

conversationViewportHeight :: EventM Name AppState Int
conversationViewportHeight =
    lookupViewport ConversationViewport >>= \case
        Just (VP _ _ (_, height) _) -> pure (max 1 height)
        Nothing -> pure 1

scrollConversationBy :: Int -> EventM Name AppState ()
scrollConversationBy amount = do
    state <- get
    when (state.appAgentSelected == AgentRoot && amount /= 0) $
        requestHistoryNear
            (if amount < 0 then HistoryOlder else HistoryNewer)
            amount
    viewportBounds <-
        lookupViewport ConversationViewport >>= \case
            Just (VP _ top (_, height) (_, contentHeight)) ->
                pure (Just (top, height, contentHeight))
            Nothing ->
                pure Nothing
    case
        Scroll.conversationScrollGesture
            (state.appAgentSelected == AgentRoot
                && historyWindowOlderAvailable state.appHistoryWindow)
            amount
            viewportBounds of
        Scroll.IgnoreConversationScroll ->
            pure ()
        Scroll.PauseAndScrollConversation -> do
            setConversationFollow False
            vScrollBy scroll amount
            queueConversationReflow
        Scroll.ResumeConversationFollow -> do
            setConversationFollow True
            vScrollToEnd scroll
            queueConversationReflow
  where
    scroll = viewportScroll ConversationViewport

requestHistoryNear
    :: HistoryDirection
    -> Int
    -> EventM Name AppState ()
requestHistoryNear direction amount =
    lookupViewport ConversationViewport >>= \case
        Nothing -> requestHistoryPage direction
        Just (VP _ top (_, height) (_, contentHeight)) ->
            let nearEdge = case direction of
                    HistoryOlder ->
                        top + amount <= max 1 height
                    HistoryNewer ->
                        top + height + amount
                            >= contentHeight - max 1 height
            in when nearEdge (requestHistoryPage direction)

requestHistoryPage
    :: HistoryDirection
    -> EventM Name AppState ()
requestHistoryPage direction = do
    state <- get
    case
        if state.appAgentSelected == AgentRoot
            then historyWindowRequest direction state.appHistoryWindow
            else Nothing of
        Nothing -> pure ()
        Just request -> do
            modify' \current ->
                current
                    { appHistoryWindow =
                        markHistoryRequest
                            direction
                            current.appHistoryWindow
                    }
            liftIO $
                atomically $
                    writeTQueue
                        state.appRuntime.runtimeHistoryRequests
                        request

setConversationFollow :: Bool -> EventM Name AppState ()
setConversationFollow follow =
    applyLocalUiEvent (UiSetFollow follow)

conversationUnpaddedContentHeight :: EventM Name AppState Int
conversationUnpaddedContentHeight =
    lookupViewport ConversationViewport >>= \case
        Just (VP _ _ _ (_, contentHeight)) -> do
            renderedReserveRows <- conversationRenderedReserveRows
            pure $ max 0 (contentHeight - renderedReserveRows)
        Nothing -> pure 0

queueConversationReflow :: EventM Name AppState ()
queueConversationReflow = do
    state <- get
    unless state.appConversationReflowQueued do
        modify' \current ->
            current { appConversationReflowQueued = True }
        liftIO $
            enqueueAppEvent
                state.appRuntime
                AppConversationReflow

reflowConversation :: EventM Name AppState ()
reflowConversation = do
    state <- get
    case state.appConversationAnchor of
        Nothing -> pure ()
        Just anchor ->
            lookupViewport ConversationViewport >>= \case
                Nothing -> pure ()
                Just (VP _ top (_, height) (_, contentHeight)) -> do
                    renderedReserveRows <-
                        conversationRenderedReserveRows
                    let unpaddedContentHeight =
                            max 0
                                (contentHeight - renderedReserveRows)
                        (next, scrollAction) =
                            Scroll.reflowConversationAnchor
                                state.appUi.uiFollow
                                top
                                height
                                unpaddedContentHeight
                                anchor
                    modify' \current ->
                        current { appConversationAnchor = Just next }
                    case scrollAction of
                        Scroll.KeepConversationPosition -> pure ()
                        Scroll.ScrollConversationToEnd ->
                            vScrollToEnd
                                (viewportScroll ConversationViewport)

syncSubmittedImagePlacements :: EventM Name AppState ()
syncSubmittedImagePlacements = do
    state <- get
    when state.appRuntime.runtimeNativeImagePreviews do
        let submitted =
                [ (blockId, index, preview)
                | (blockId, previews) <-
                    Map.toAscList state.appSubmittedImagePreviews
                , (index, preview) <- zip [0 ..] previews
                ]
        viewportExtent <- lookupExtent ConversationViewportExtent
        placements <-
            case viewportExtent of
                Nothing -> pure []
                Just viewportBounds ->
                    fmap concat $
                        sequence
                            [ placementFor
                                state
                                viewportBounds
                                ordinal
                                blockId
                                index
                                preview
                            | (ordinal, (blockId, index, preview)) <-
                                zip [0 ..] submitted
                            ]
        previous <-
            liftIO $
                readIORef
                    state.appRuntime.runtimeSubmittedImagePlacements
        when (not (sameNativePreviewLayout previous placements)) $
            liftIO do
                writeIORef
                    state.appRuntime.runtimeSubmittedImagePlacements
                    placements
                modifyIORef'
                    state.appRuntime.runtimeImagePreviewRevision
                    (+ 1)

placementFor
    :: AppState
    -> Extent Name
    -> Int
    -> BlockId
    -> Int
    -> TuiImagePreview
    -> EventM Name AppState [NativePreviewPlacement]
placementFor state viewportBounds ordinal blockId index preview =
    lookupExtent (ConversationImage blockId index) >>= \case
        Just imageBounds
            | extentInside viewportBounds imageBounds ->
                let Location (column, row) = imageBounds.extentUpperLeft
                    (columns, rows) = imageBounds.extentSize
                    imageId =
                        submittedImageId
                            state.appRuntime.runtimeImagePreviewIdBase
                            ordinal
                in pure
                    [ NativePreviewPlacement
                        { nativePreviewImageId = imageId
                        , nativePreviewRow = row
                        , nativePreviewColumn = column
                        , nativePreviewColumns = columns
                        , nativePreviewRows = rows
                        , nativePreviewAttachment =
                            preview.previewKittyAttachment
                        }
                    ]
        _ -> pure []

extentInside :: Extent Name -> Extent Name -> Bool
extentInside outer inner =
    innerColumn >= outerColumn
        && innerRow >= outerRow
        && innerColumn + innerWidth <= outerColumn + outerWidth
        && innerRow + innerHeight <= outerRow + outerHeight
        && innerWidth > 0
        && innerHeight > 0
  where
    Location (outerColumn, outerRow) = outer.extentUpperLeft
    (outerWidth, outerHeight) = outer.extentSize
    Location (innerColumn, innerRow) = inner.extentUpperLeft
    (innerWidth, innerHeight) = inner.extentSize

submittedImageId :: Int -> Int -> Int
submittedImageId imageIdBase ordinal =
    fromInteger $
        ((toInteger imageIdBase + 2 + toInteger ordinal)
            `mod` 4_294_967_295)
            + 1

clearSubmittedImagePlacements :: FullscreenRuntime -> EventM Name AppState ()
clearSubmittedImagePlacements runtime = do
    previous <-
        liftIO $ readIORef runtime.runtimeSubmittedImagePlacements
    when (not (null previous)) $
        liftIO do
            writeIORef runtime.runtimeSubmittedImagePlacements []
            modifyIORef' runtime.runtimeImagePreviewRevision (+ 1)

conversationRenderedReserveRows :: EventM Name AppState Int
conversationRenderedReserveRows =
    lookupExtent ConversationReserve >>= \case
        Just (Extent _ _ (_, reserveRows)) -> pure reserveRows
        Nothing -> pure 0

isSubmittedPrompt :: UiEvent -> Bool
isSubmittedPrompt = \case
    UiUserSubmitted _ -> True
    _ -> False

selectedBlock :: UiState -> BlockId -> Maybe UiBlock
selectedBlock state ident = lookupBlock ident state

conversationBlocks :: AgentTarget -> AppState -> [UiBlock]
conversationBlocks target state =
    case target of
        AgentRoot ->
            concatMap
                (toList . (.historyTurnBlocks))
                (toList state.appHistoryWindow.historyWindowTurns)
                <> toList state.appUi.uiBlocks
        AgentChild _ ->
            maybe [] (toList . (.uiBlocks))
                (conversationUiForTarget target state)
        AgentNative _ ->
            maybe [] (toList . (.uiBlocks))
                (conversationUiForTarget target state)

historyBlock :: HistoryWindow -> BlockId -> Maybe UiBlock
historyBlock window ident =
    historyWindowBlock ident window

selectedConversationBlockId
    :: AgentTarget
    -> AppState
    -> Maybe BlockId
selectedConversationBlockId target state =
    case target of
        AgentRoot ->
            state.appHistorySelectedBlock
                <|> state.appUi.uiSelectedBlock
        AgentChild _ ->
            conversationUiForTarget target state
                >>= (.uiSelectedBlock)
        AgentNative _ ->
            conversationUiForTarget target state
                >>= (.uiSelectedBlock)

selectedConversationBlock
    :: AgentTarget
    -> AppState
    -> Maybe UiBlock
selectedConversationBlock target state =
    selectedConversationBlockId target state >>= \ident ->
        case target of
            AgentRoot ->
                historyBlock state.appHistoryWindow ident
                    <|> lookupBlock ident state.appUi
            AgentChild _ ->
                conversationUiForTarget target state >>= lookupBlock ident
            AgentNative _ ->
                conversationUiForTarget target state >>= lookupBlock ident

selectConversationBlock
    :: AgentTarget
    -> BlockId
    -> EventM Name AppState ()
selectConversationBlock target ident = do
    state <- get
    case target of
        AgentRoot ->
            case historyBlock state.appHistoryWindow ident of
                Just _ ->
                    modify' \current ->
                        current
                            { appHistorySelectedBlock = Just ident
                            , appUi =
                                (reduceUi
                                    (UiFocusChanged FocusScrollback)
                                    current.appUi)
                                    { uiSelectedBlock = Nothing
                                    , uiSelectedBlockIndex = Nothing
                                    }
                            }
                Nothing -> do
                    modify' \current ->
                        current { appHistorySelectedBlock = Nothing }
                    applyLocalUiEventWith
                        (UiSelectBlock ident)
                        (applyUiEvent
                            (UiFocusChanged FocusScrollback))
        AgentChild _ -> do
            modify' \current ->
                (applyChildConversationUiEvent
                    target
                    (UiSelectBlock ident)
                    current)
                    { appHistorySelectedBlock = Nothing
                    , appUi =
                        reduceUi
                            (UiFocusChanged FocusScrollback)
                            current.appUi
                    }
        AgentNative _ -> do
            modify' \current ->
                (applyChildConversationUiEvent
                    target
                    (UiSelectBlock ident)
                    current)
                    { appHistorySelectedBlock = Nothing
                    , appUi =
                        reduceUi
                            (UiFocusChanged FocusScrollback)
                            current.appUi
                    }

activateConversationBlock
    :: AgentTarget
    -> BlockId
    -> EventM Name AppState ()
activateConversationBlock target ident = do
    state <- get
    case target of
        AgentRoot ->
            case historyBlock state.appHistoryWindow ident of
                Just _ -> do
                    selectConversationBlock target ident
                    modify' \current ->
                        current
                            { appHistoryWindow =
                                mapHistoryBlock
                                    ident
                                    (\block ->
                                        block
                                            { blockExpanded =
                                                not block.blockExpanded
                                            })
                                    current.appHistoryWindow
                            }
                Nothing -> do
                    modify' \current ->
                        current { appHistorySelectedBlock = Nothing }
                    applyLocalUiEventWith
                        (UiActivateBlock ident)
                        (applyUiEvent
                            (UiFocusChanged FocusScrollback))
        AgentChild _ -> do
            modify' \current ->
                (applyChildConversationUiEvent
                    target
                    (UiActivateBlock ident)
                    current)
                    { appHistorySelectedBlock = Nothing
                    , appUi =
                        reduceUi
                            (UiFocusChanged FocusScrollback)
                            current.appUi
                    }
        AgentNative _ -> do
            modify' \current ->
                (applyChildConversationUiEvent
                    target
                    (UiActivateBlock ident)
                    current)
                    { appHistorySelectedBlock = Nothing
                    , appUi =
                        reduceUi
                            (UiFocusChanged FocusScrollback)
                            current.appUi
                    }
    queueConversationReflow

mapHistoryBlock
    :: BlockId
    -> (UiBlock -> UiBlock)
    -> HistoryWindow
    -> HistoryWindow
mapHistoryBlock ident update window =
    setHistoryWindowTurns
        (fmap
            (\turn ->
                turn
                    { historyTurnBlocks =
                        fmap
                            (\block ->
                                if block.blockId == ident
                                    then update block
                                    else block)
                            turn.historyTurnBlocks
                    })
            window.historyWindowTurns)
        window
