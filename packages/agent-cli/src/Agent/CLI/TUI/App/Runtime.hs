{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -O0 -Wno-unused-imports #-}
module Agent.CLI.TUI.App.Runtime where

import Agent.CLI.TUI.App.Mailbox
    ( appEventChannelCapacity
    , enqueueAppEvent
    )

import Agent.Provider (Provider)
import Agent.CLI.Clipboard ( formatImageSize )
import Agent.CLI.Dictation ( DictationControl(..)
    , DictationResult(..)
    , dictateWith
    , insertDictation
    )
import Agent.CLI.Secret (sanitizeSecretPromptText)
import Agent.CLI.Artifact (fencedCodeBlock)
import Agent.CLI.Input ( ReplLine(..)
    , readReplHistory
    , terminalTextWidth
    , truncateDisplayText
    )
import Agent.CLI.AgentViewport ( AgentEntry(..)
    , AgentStep(..)
    , AgentStepState(..)
    , AgentTarget(..)
    , agentDisplayName
    , agentEntryTreeLabelWithGlyphModel
    , agentStatusGlyph
    , lookupAgentEntry
    )
import Agent.CLI.Interrupt (CtrlCDecision(..))
import Agent.CLI.ImagePreview ( ImagePreviewProtocol(..)
    , detectImagePreviewProtocol
    , kittyDeleteImageSequence
    , kittyPlacedImageSequence
    , positionImagePayload
    )
import Agent.CLI.Command ( SkillCommand , SlashCatalog(..)
    , defaultSlashCatalog
    , slashCatalogWithSkills
    )
import Agent.CLI.Permission
    ( PermissionChoice(..)
    , approvalToolCallPromptRelative
    )
import Agent.CLI.Resume ( ResumeBrowser(..)
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
import Agent.CLI.Terminal ( TerminalCapabilities(..)
    , detectTerminalCapabilities
    , kittyAltCsiBodies
    , kittyCtrlCsiBodies
    , kittyCtrlUnderscoreCsiBodies
    , kittyEscapeCsiBodies
    , kittyKeyboardDisambiguatePush
    , kittyKeyboardPop
    , kittySuperCsiBodies
    , kittySuperVCsiBodies
    , shiftEnterCsiBodies
    )
import qualified Agent.TUI.Theme as Theme
import qualified Agent.CLI.TUI.Bridge as Bridge
import qualified Agent.CLI.TUI.Composer as Composer
import Agent.CLI.TUI.History ( HistoryCursor(..)
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
import Agent.CLI.TUI.LambdaArt ( lambdaArtWidget )
import Agent.CLI.TUI.Motion ( advanceCompletionFlashes , appMotionTiming , completionFlashTransitions , elapsedMillisSince , hasBackgroundActivity , isBackgroundAgentActive , motionDemandFor , motionDemandForTerminalFocus , motionModeForTerminalFocus , nativeProgressKeepaliveDue , nextMotionSchedule , turnCompletionRequiresRedraw , uiEventRestartsMotionSchedule , userActionPending )
import Agent.CLI.TUI.Render ( agentEntryWindow , agentPaneEntryLimit , agentPaneVisible , applyChildConversationUiEvent , choiceRowColumns , conversationUiForTarget , conversationScrollbarRenderer , drawApp , fullscreenBounds , fullscreenSurface , onboardingVisibleRowIndices , normalizeTextOverlayInsertion , maskedSecretText , quickStartRows , quickStartVisible , repositoryHeaderText , resumeSearchCursorColumn , selectedAgentConversation , textOverlayDisplayText )
import Agent.CLI.TUI.ImagePreview ( NativePreviewPlacement(..)
    , TuiImagePreview(..)
    , nativePreviewPlacements
    , prepareNativeTuiImagePreview
    , prepareTuiImagePreview
    , previewCountForWidth
    , previewCellSize
    , renderTuiImagePreview
    , sameNativePreviewLayout
    )
import Agent.TUI.Markdown ( codeWidgetWithSyntaxHighlighting , markdownWidgetWithLinks , markdownWidgetWithSyntaxHighlightingAndLinks )
import Agent.TUI.FencedCode ( FencedBlock(..)
    , fencedBlocks
    )
import Agent.TUI.TextWidth ( clampGraphemeCursor , displayTerminalText , nextGraphemeBoundary , previousGraphemeBoundary )
import Agent.Syntax ( SyntaxHighlighter , loadSyntaxLanguage , newSyntaxHighlighter , resolveFenceLanguage )
import qualified Agent.CLI.TUI.Scroll as Scroll
import qualified Agent.CLI.TUI.Transcript as Transcript
import Agent.CLI.TUI.Types
import Agent.TUI.Model
import Agent.TUI.Motion ( MotionDemand(..)
    , MotionMode(..)
    , backgroundIndicator
    , completionFlashDurationMillis
    , foregroundIndicator
    , quietIndicator
    , waitingIndicator
    )
import Agent.Loop (ImageAttachment(..), LoopEvent(..))
import Agent.ToolDispatch (ToolCall(..))
import Brick
import qualified Brick.Types as B
import Brick.BChan ( newBChan , writeBChan )
import Brick.Widgets.Border (borderWithLabel)
import qualified Brick.Widgets.Border as Border
import Brick.Widgets.Border.Style (unicodeRounded)
import Brick.Widgets.Center (center, centerLayer, hCenter)
import Codec.Picture (pixelAt)
import Control.Applicative ((<|>))
import Control.Concurrent.Async (wait, waitCatch, withAsync)
import Control.Concurrent (threadDelay)
import Control.Monad (forever, unless, void, when, (>=>))
import Control.Concurrent.STM ( STM , atomically , check , flushTQueue , newEmptyTMVarIO , newTQueueIO , newTVarIO , orElse , putTMVar , readTVar , readTMVar , readTQueue , registerDelay , retry , takeTMVar , writeTQueue , writeTVar )
import Agent.CLI.Notification
    ( AttentionRequest(PermissionRequested)
    , notifyAttention
    )
import Agent.CLI.Recap ( autoRecapAwayThreshold , autoRecapIdleThreshold , autoRecapRetryInterval )
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (modify')
import Control.Exception.Safe (finally, mask, onException, throwIO, tryAny)
import Control.Exception (AsyncException(UserInterrupt))
import Data.Char (isControl, isSpace)
import Data.Foldable (toList)
import Data.IORef ( atomicModifyIORef' , modifyIORef' , newIORef , readIORef , writeIORef )
import Data.List ( find , findIndex , intersperse , nub , sort , sortOn )
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
import System.IO (stderr, stdout)
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
        events <- newBChan appEventChannelCapacity
        mailbox <- AppEventMailbox <$> newTVarIO AppEventMailboxState
            { mailboxPendingEvents = Seq.empty
            , mailboxPendingCount = 0
            , mailboxPendingBytes = 0
            , mailboxHighWaterCount = 0
            , mailboxHighWaterBytes = 0
            }
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
            { sessionProvider = Nothing
            , sessionCancel = cancelAction
            , sessionSteer = const (pure (Right ()))
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
    -> Maybe Provider
    -> IO ()
    -> (Text -> IO (Either Text ()))
    -> (Text -> IO ())
    -> IO ()
    -> (Text -> IO ())
    -> IO CtrlCDecision
    -> IO (AgentTarget, [AgentEntry])
    -> (AgentTarget -> IO ())
    -> IO ()
setFullscreenSessionActions
    runtime
    provider
    cancelAction
    steerAction
    btwAction
    recapAction
    restartEffortAction
    ctrlCAction
    agentSnapshot
    agentSelect =
        writeIORef runtime.runtimeSessionActions FullscreenSessionActions
            { sessionProvider = provider
            , sessionCancel = cancelAction
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

-- | Attach an agent-displayed image to the tool block that produced it. The
-- preview is prepared on the calling tool thread: ANSI previews force the
-- sampled bitmap here so the Brick draw thread never decodes an image.
showFullscreenToolImage
    :: FullscreenRuntime
    -> Text
    -> ImageAttachment
    -> IO (Either Text ())
showFullscreenToolImage runtime callId image =
    case prepareForRuntime runtime image of
        Left err -> pure (Left ("cannot decode image: " <> err))
        Right preview -> do
            unless runtime.runtimeNativeImagePreviews $
                void $ pure $! pixelAt preview.previewSample 0 0
            enqueueAppEvent runtime (AppToolImage callId preview)
            pure (Right ())

prepareFullscreenImagePreviews
    :: FullscreenRuntime
    -> [ImageAttachment]
    -> IO [(ImageAttachment, TuiImagePreview)]
prepareFullscreenImagePreviews runtime images = do
    let prepared =
            mapMaybe
                (\image ->
                    case prepareForRuntime runtime image of
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

prepareForRuntime
    :: FullscreenRuntime
    -> ImageAttachment
    -> Either Text TuiImagePreview
prepareForRuntime runtime
    | runtime.runtimeNativeImagePreviews =
        prepareNativeTuiImagePreview
    | otherwise =
        prepareTuiImagePreview

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
              , V.EvKey V.KEsc []
              )
            | body <- kittyEscapeCsiBodies
            ]
            <>
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
                 , V.EvKey (V.KChar 'k') [V.MMeta]
                 )
               | body <- kittySuperCsiBodies 'k'
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
    let summary = approvalToolCallPromptRelative workspace call
    notifyAttention stderr PermissionRequested
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

-- | Open a choice overlay whose rows can be narrowed by typing. The returned
-- index always refers to the original row list, even while the visible rows
-- are filtered.
requestFullscreenFilterChoice
    :: FullscreenRuntime
    -> Text
    -> Int
    -> [(Text, Text)]
    -> IO (Maybe Int)
requestFullscreenFilterChoice runtime title initial rows = do
    reply <- newEmptyTMVarIO
    enqueueAppEvent runtime
        (AppAskFilterChoice title initial rows reply)
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

allocateNativePreviewImageIdBase :: IO Int
allocateNativePreviewImageIdBase = do
    micros <- floor . (* 1_000_000) <$> getPOSIXTime :: IO Integer
    pid <- fromIntegral <$> getProcessID :: IO Integer
    -- Kitty ids are terminal-global uint32s. Leave room for the other two
    -- simultaneously displayed previews and vary the base per process/runtime.
    let availableBases = 4_294_967_293 :: Integer
    pure $
        fromInteger ((micros * 65_537 + pid) `mod` availableBases) + 1
