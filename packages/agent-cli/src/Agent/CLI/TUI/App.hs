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
    , choiceClosesOnUiTransition
    , elapsedMillisSince
    , emitUiEvent
    , hasQueuedFullscreenInput
    , motionDemandFor
    , lambdaArtWidget
    , nativeProgressKeepaliveDue
    , nextMotionSchedule
    , newFullscreenInputBuffer
    , newFullscreenRuntime
    , newFullscreenRuntimeWithSyntaxLoader
    , selectedAgentConversation
    , loadSyntaxHighlighterForRuntime
    , queuedFullscreenInputDisplays
    , readFullscreenLine
    , readFullscreenLineWithModels
    , readFullscreenLineOr
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
    , setFullscreenSessionActions
    , fullscreenVtyConfig
    , setFullscreenImagePreviews
    , setFullscreenWindowTitle
    , uiEventRestartsMotionSchedule
    , maskedSecretText
    , normalizeTextOverlayInsertion
    , textOverlayDisplayText
    , withFullscreenSuspended
    ) where

import Agent.CLI.Clipboard
    ( formatImageSize
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
    , agentEntryTreeLabelWithGlyph
    , agentStatusGlyph
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
    )
import Agent.CLI.Permission (PermissionChoice(..))
import Agent.CLI.Resume
    ( ResumeBrowser(..)
    , ResumeEntry(..)
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
import Agent.CLI.Status (formatTokenUsage)
import Agent.CLI.Terminal
    ( kittyCtrlVCsiBodies
    , kittySuperVCsiBodies
    , shiftEnterCsiBodies
    )
import qualified Agent.TUI.Theme as Theme
import qualified Agent.CLI.TUI.Bridge as Bridge
import qualified Agent.CLI.TUI.Composer as Composer
import Agent.CLI.TUI.ImagePreview
    ( NativePreviewPlacement(..)
    , TuiImagePreview(..)
    , nativePreviewPlacements
    , prepareTuiImagePreview
    , previewCountForWidth
    , previewCellSize
    , renderTuiImagePreview
    )
import Agent.TUI.Markdown
    ( codeWidgetWithSyntaxHighlighting
    , markdownWidget
    , markdownWidgetWithSyntaxHighlighting
    )
import Agent.Syntax
    ( SyntaxHighlighter
    , loadSyntaxHighlighter
    )
import qualified Agent.CLI.TUI.Scroll as Scroll
import Agent.CLI.TUI.Types
import Agent.TUI.Model
import Agent.TUI.Motion
    ( MotionDemand(..)
    , MotionMode(..)
    , backgroundIndicator
    , completionFlashDurationMillis
    , foregroundIndicator
    , motionDelayMicros
    , quietIndicator
    , waitingIndicator
    )
import Agent.TUI.Presentation (permissionToolCallPrompt)
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
import Control.Concurrent.Async (wait, waitCatch, withAsync)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
    ( STM
    , atomically
    , check
    , newEmptyTMVarIO
    , newTVarIO
    , orElse
    , putTMVar
    , readTVar
    , readTMVar
    , registerDelay
    , retry
    , takeTMVar
    , writeTVar
    )
import Control.Monad (unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (modify')
import Control.Exception.Safe (finally, throwIO, tryAny)
import Control.Exception (AsyncException(UserInterrupt))
import Data.Char (isControl)
import Data.Foldable (toList)
import Data.IORef
    ( modifyIORef'
    , newIORef
    , readIORef
    , writeIORef
    )
import Data.List (find, findIndex, intersperse, nub, sort, sortOn)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe)
import Data.Sequence (Seq, ViewL(..), ViewR(..), (|>))
import qualified Data.Sequence as Seq
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
import System.IO (stdout)
import System.Posix.Process (getProcessID)

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
    newFullscreenRuntimeWithSyntaxLoader loadSyntaxHighlighter

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
        imagePreviews <- newIORef []
        imagePreviewRevision <- newIORef 0
        imagePreviewVisible <- newIORef True
        imagePreviewIdBase <- allocateNativePreviewImageIdBase
        imagePreviewProtocol <- detectImagePreviewProtocol stdout
        imagePreviewInTmux <- isJust <$> lookupEnv "TMUX"
        sessionActions <- newIORef FullscreenSessionActions
            { sessionCancel = cancelAction
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
            , runtimeRestartEffort = \level ->
                readIORef sessionActions >>= \actions ->
                    actions.sessionRestartEffort level
            , runtimeCtrlC =
                readIORef sessionActions >>= (.sessionCtrlC)
            , runtimeCopy = copyAction
            , runtimeSetWindowTitle = setWindowTitle
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
            , runtimeImagePreviewRevision = imagePreviewRevision
            , runtimeImagePreviewVisible = imagePreviewVisible
            , runtimeImagePreviewIdBase = imagePreviewIdBase
            , runtimeNativeImagePreviews =
                imagePreviewProtocol == PreviewKitty
                    && not imagePreviewInTmux
            , runtimeColor = color
            , runtimeLoadSyntaxHighlighter = syntaxLoader
            , runtimeSyntaxLoadFinished = syntaxLoadFinished
            , runtimeInitial = initial
            , runtimeSessionActions = sessionActions
            }

setFullscreenSessionActions
    :: FullscreenRuntime
    -> IO ()
    -> (Text -> IO ())
    -> IO CtrlCDecision
    -> IO (AgentTarget, [AgentEntry])
    -> (AgentTarget -> IO ())
    -> IO ()
setFullscreenSessionActions
    runtime
    cancelAction
    restartEffortAction
    ctrlCAction
    agentSnapshot
    agentSelect =
        writeIORef runtime.runtimeSessionActions FullscreenSessionActions
            { sessionCancel = cancelAction
            , sessionRestartEffort = restartEffortAction
            , sessionCtrlC = ctrlCAction
            , sessionAgentSnapshot = agentSnapshot
            , sessionAgentSelect = agentSelect
            }

loadSyntaxHighlighterForRuntime :: FullscreenRuntime -> IO ()
loadSyntaxHighlighterForRuntime runtime = do
    startedAt <- getMonotonicTimeNSec
    result <- tryAny runtime.runtimeLoadSyntaxHighlighter
    finishedAt <- getMonotonicTimeNSec
    let highlighter = case result of
            Left _ -> Nothing
            Right loaded -> either (const Nothing) Just loaded
    enqueueAppEvent runtime (AppSyntaxHighlighterLoaded highlighter)
    void $
        tryAny $
            runtime.runtimeSyntaxLoadFinished
                (nanosecondsToNominalDiffTime (finishedAt - startedAt))

nanosecondsToNominalDiffTime :: Word64 -> NominalDiffTime
nanosecondsToNominalDiffTime nanoseconds =
    realToFrac nanoseconds / 1_000_000_000

emitUiEvent :: FullscreenRuntime -> UiEvent -> IO ()
emitUiEvent runtime event =
    enqueueAppEvent runtime (AppUi event)

setFullscreenWindowTitle :: FullscreenRuntime -> Text -> IO ()
setFullscreenWindowTitle runtime =
    enqueueAppEvent runtime . AppSetWindowTitle

setFullscreenImagePreviews
    :: FullscreenRuntime
    -> [ImageAttachment]
    -> IO ()
setFullscreenImagePreviews runtime images = do
    previous <- readIORef runtime.runtimeImagePreviews
    prepared <-
        if map fst previous == images
            then pure previous
            else do
                let next =
                        mapMaybe
                            (\image ->
                                case prepareTuiImagePreview image of
                                    Left _ -> Nothing
                                    Right preview -> Just (image, preview))
                            images
                -- ANSI previews force the sampled image during Brick drawing.
                -- Build that sample here on the model worker instead of
                -- stalling the Brick event/render thread.
                unless runtime.runtimeNativeImagePreviews $
                    mapM_
                        (\(_, preview) ->
                            void $
                                pure $!
                                    pixelAt preview.previewSample 0 0)
                        next
                pure next
    enqueueAppEvent runtime (AppSetImagePreviews prepared)

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
    enqueueAppEvent runtime (AppSetSkillCommands skills)
    enqueueAppEvent runtime (AppSetModelIds modelIds)
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
                 , V.EvKey (V.KChar 'v') [modifier]
                 )
               | (modifier, bodies) <-
                    [ (V.MCtrl, kittyCtrlVCsiBodies)
                    , (V.MMeta, kittySuperVCsiBodies)
                    ]
               , body <- bodies
               ]
        }

requestFullscreenPermission
    :: FullscreenRuntime
    -> ToolCall
    -> IO (Maybe PermissionChoice)
requestFullscreenPermission runtime call = do
    reply <- newEmptyTMVarIO
    let summary = permissionToolCallPrompt call
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
    -> IO (Maybe ResumeEntry)
requestFullscreenResume runtime browser loadEntry deleteEntry = do
    reply <- newEmptyTMVarIO
    enqueueAppEvent runtime
        (AppAskResume browser loadEntry deleteEntry reply)
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
    let buildVty = do
            vty <- Vty.mkVty fullscreenVtyConfig
            let output = V.outputIface vty
            -- Without this mode terminals paste image clipboard fallbacks
            -- (paths, URLs, or other text representations) as ordinary key
            -- events, so the composer renders them as text. Vty turns the
            -- bracketed sequence into one EvPaste that we can classify.
            when (V.supportsMode output V.BracketedPaste) $
                V.setMode output V.BracketedPaste True
            when (V.supportsMode output V.Mouse) $
                V.setMode output V.Mouse True
            -- Vty deliberately leaves OSC 8 output disabled by default even
            -- when rendered attributes contain URLs.
            when (V.supportsMode output V.Hyperlink) $
                V.setMode output V.Hyperlink True
            wrapNativePreviewVty runtime vty
    initialVty <- buildVty
    let
        initialState = AppState
            { appUi = runtime.runtimeInitial
            , appPermissionReply = Nothing
            , appRuntime = runtime
            , appSlashIndex = 0
            , appChoice = Nothing
            , appChoiceReply = Nothing
            , appResume = Nothing
            , appResumeReply = Nothing
            , appResumeLoad = Nothing
            , appResumeDelete = Nothing
            , appTextPrompt = Nothing
            , appTextReply = Nothing
            , appSlashDismissed = False
            , appPasted = False
            , appHistory = history
            , appHistoryIndex = Nothing
            , appHistoryDraft = ""
            , appKillBuffer = ""
            , appSkillCommands = []
            , appModelIds = []
            , appImagePreviews = []
            , appAgentSelected = initialAgent
            , appAgentEntries = initialAgents
            , appAgentHover = Nothing
            , appHoveredControl = Nothing
            , appPressedControl = Nothing
            , appWorkerStopped = False
            , appConversationAnchor = Nothing
            , appConversationReflowQueued = False
            , appWindowTitle = Nothing
            , appMotionElapsedMillis = 0
            , appCompletionFlashes = Map.empty
            , appMotionScheduleReset = False
            , appClockNanos = initialClock
            , appNativeProgressKeepaliveBucket = 0
            , appSyntaxHighlighter = Nothing
            }
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
                    withAsync
                        (loadSyntaxHighlighterForRuntime runtime)
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
                    let placements =
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

motionDemandFor
    :: MotionMode
    -> Bool
    -- ^ A permission, question, or approval is waiting on the user.
    -> Bool
    -- ^ Background child-agent work remains.
    -> Bool
    -- ^ At least one completed block is still flashing.
    -> UiState
    -> MotionDemand
motionDemandFor mode waitingForUser backgroundActive completionFlashing ui =
    case mode of
        MotionFull ->
            maximum
                [ semanticDemand
                , if not waitingForUser && ui.uiRunning
                    then MotionFast
                    else MotionNone
                , if waitingForUser
                    then MotionSlow
                    else MotionNone
                , if not waitingForUser && progressNoticeActive ui
                    then MotionFast
                    else MotionNone
                , if completionFlashing
                    then MotionFast
                    else MotionNone
                , if backgroundActive
                    || conversationIsEmpty ui
                    then MotionSlow
                    else MotionNone
                ]
        MotionReduced ->
            maximum
                [ semanticDemand
                , if completionFlashing
                    then MotionSlow
                    else MotionNone
                ]
        MotionOff ->
            semanticDemand
  where
    semanticDemand =
        if uiNeedsTick ui then MotionSlow else MotionNone

appMotionDemand :: AppState -> MotionDemand
appMotionDemand state =
    motionDemandFor
        state.appRuntime.runtimeMotionMode
        (userActionPending state)
        (hasBackgroundActivity state.appAgentEntries)
        (not (Map.null state.appCompletionFlashes))
        state.appUi

appMotionTiming :: AppState -> (MotionDemand, Int)
appMotionTiming state =
    ( demand
    , motionDelayMicros
        state.appRuntime.runtimeMotionMode
        demand
        (appNextDeadlineMillis state)
    )
  where
    demand = appMotionDemand state

appNextDeadlineMillis :: AppState -> Maybe Int
appNextDeadlineMillis state =
    minimumMaybe $
        maybeToList (uiNextDeadlineMillis state.appUi)
            <> Map.elems state.appCompletionFlashes
  where
    maybeToList = maybe [] pure
    minimumMaybe [] = Nothing
    minimumMaybe values = Just (minimum values)

userActionPending :: AppState -> Bool
userActionPending state =
    isJust state.appTextPrompt
        || isJust state.appChoice
        || isJust state.appResume
        || isJust state.appUi.uiPermission

progressNoticeActive :: UiState -> Bool
progressNoticeActive ui =
    maybe False ((== NoticeProgress) . (.noticeKind)) ui.uiNotice

hasBackgroundActivity :: [AgentEntry] -> Bool
hasBackgroundActivity =
    any isBackgroundAgentActive

isBackgroundAgentActive :: AgentEntry -> Bool
isBackgroundAgentActive entry =
    entry.agentTarget /= AgentRoot
        && Text.toLower entry.agentStatus `elem` ["pending", "running"]

completionFlashTransitions :: UiState -> UiState -> [BlockId]
completionFlashTransitions previous next =
    [ block.blockId
    | block <- toList next.uiBlocks
    , Just oldBlock <- [Map.lookup block.blockId previousById]
    , blockWasLive oldBlock.blockState
    , block.blockState == BlockComplete
    , block.blockKind
        `elem` [BlockThinking, BlockTool, BlockShell, BlockEdit]
    ]
  where
    previousById =
        Map.fromList
            [ (block.blockId, block)
            | block <- toList previous.uiBlocks
            ]
    blockWasLive blockState =
        blockState `elem` [BlockRunning, BlockStreaming]

advanceCompletionFlashes
    :: Int
    -> Map.Map BlockId Int
    -> Map.Map BlockId Int
advanceCompletionFlashes rawElapsedMillis =
    Map.mapMaybe \remaining ->
        let next = remaining - max 0 rawElapsedMillis
        in if next <= 0 then Nothing else Just next

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
        V.EvKey V.KEnter [] ->
            setBrowser (endResumeSearch browser)
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
    ChoiceRow index ->
        confirmChoiceAt index
    ResumeRow sessionId ->
        confirmResumeId sessionId
    CodeCopy blockId codeIndex ->
        copyCodeBlock blockId codeIndex
    _ ->
        pure ()

isInteractiveControl :: Name -> Bool
isInteractiveControl = \case
    ComposerModel -> True
    ComposerEffort -> True
    ComposerMode -> True
    ComposerAccount -> True
    ChoiceRow _ -> True
    ResumeRow _ -> True
    CodeCopy _ _ -> True
    _ -> False

copyCodeBlock :: BlockId -> Int -> EventM Name AppState ()
copyCodeBlock blockId codeIndex = do
    state <- get
    let code =
            selectedBlock state.appUi blockId
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
handleTextPromptKey = \case
    V.EvKey V.KEsc [] -> resolveTextPrompt False
    V.EvKey (V.KChar 'c') modifiers
        | V.MCtrl `elem` modifiers -> do
            state <- get
            _ <- handleCtrlC
            when state.appUi.uiRunning (resolveTextPrompt False)
    V.EvKey V.KEnter [] -> resolveTextPrompt True
    V.EvKey V.KEnter [V.MShift] -> do
        state <- get
        case (.textInputMode) <$> state.appTextPrompt of
            Just TextInputPlain -> insert "\n"
            _ -> pure ()
    V.EvKey V.KPageUp [] ->
        vScrollPage (viewportScroll OverlayViewport) Up
    V.EvKey V.KPageDown [] ->
        vScrollPage (viewportScroll OverlayViewport) Down
    V.EvMouseDown _ _ V.BScrollUp _ ->
        vScrollBy (viewportScroll OverlayViewport) (-mouseScrollLines)
    V.EvMouseDown _ _ V.BScrollDown _ ->
        vScrollBy (viewportScroll OverlayViewport) mouseScrollLines
    V.EvKey V.KBS [] -> edit \draft cursor ->
        if cursor <= 0
            then (draft, cursor)
            else
                ( Text.take (cursor - 1) draft <> Text.drop cursor draft
                , cursor - 1
                )
    V.EvKey V.KDel [] -> edit \draft cursor ->
        if cursor >= Text.length draft
            then (draft, cursor)
            else
                ( Text.take cursor draft <> Text.drop (cursor + 1) draft
                , cursor
                )
    V.EvKey V.KLeft [] -> move (-1)
    V.EvKey V.KRight [] -> move 1
    V.EvKey V.KHome [] -> edit \draft cursor ->
        (draft, lineStartCursor draft cursor)
    V.EvKey V.KEnd [] -> edit \draft cursor ->
        (draft, lineEndCursor draft cursor)
    V.EvKey (V.KChar 'w') modifiers
        | V.MCtrl `elem` modifiers ->
            edit deleteWordBefore
    V.EvKey (V.KChar 'u') modifiers
        | V.MCtrl `elem` modifiers ->
            edit deleteToLineStart
    V.EvKey (V.KChar 'k') modifiers
        | V.MCtrl `elem` modifiers ->
            edit deleteToLineEnd
    V.EvKey (V.KChar character) [] ->
        insertForMode (Text.singleton character)
    V.EvPaste bytes ->
        insertForMode (Composer.decodePaste bytes)
    _ -> pure ()
  where
    insertForMode raw = do
        state <- get
        let mode =
                maybe TextInputPlain (.textInputMode) state.appTextPrompt
        insert (normalizeTextOverlayInsertion mode raw)
    edit change =
        modify' \state ->
            state
                { appTextPrompt =
                    (\prompt ->
                        let (draft, cursor) =
                                change prompt.textDraft prompt.textCursor
                        in prompt
                            { textDraft = draft
                            , textCursor =
                                max 0 (min (Text.length draft) cursor)
                            })
                        <$> state.appTextPrompt
                }
    insert inserted =
        edit \draft cursor ->
            ( Text.take cursor draft <> inserted <> Text.drop cursor draft
            , cursor + Text.length inserted
            )
    move delta =
        edit \draft cursor -> (draft, cursor + delta)

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
            then Theme.solarizedDark
            else Theme.monochrome
    }

drawApp :: AppState -> [Widget Name]
drawApp state =
    let mainLayers = stickyPromptLayers state <> [drawMain state]
        interactiveLayers =
            agentPopoverLayers state
                <> imagePreviewLayers
                    state.appRuntime.runtimeNativeImagePreviews
                    state.appImagePreviews
                <> mainLayers
        dimmedMainLayers = map (forceAttr Theme.dimAttr) mainLayers
    in
    case state.appResume of
        Just resume ->
            drawResume state resume : dimmedMainLayers
        Nothing ->
            case (state.appTextPrompt, state.appChoice, state.appUi.uiPermission) of
                (Just prompt, _, _) ->
                    drawTextPrompt state prompt : dimmedMainLayers
                (Nothing, Just choice, _) ->
                    drawChoice state choice : dimmedMainLayers
                (Nothing, Nothing, Just permission) ->
                    drawPermission state permission : dimmedMainLayers
                (Nothing, Nothing, Nothing) -> interactiveLayers

drawMain :: AppState -> Widget Name
drawMain state =
    withAttr Theme.baseAttr $
        vBox
            [ drawHeader state
            , drawWorkspace state
            , drawNotice state
            , Composer.drawQueuedInputs state.appUi
            , Composer.drawSlashMenu state
            , drawFollowStatus state.appUi
            , Composer.drawComposer state
            , drawFooter state
            ]

imagePreviewLayers :: Bool -> [TuiImagePreview] -> [Widget Name]
imagePreviewLayers native previews =
    case takeLast 3 previews of
        [] -> []
        shown -> [centerLayer (drawImagePreviews native shown)]
  where
    takeLast count values =
        drop (max 0 (length values - count)) values

drawImagePreviews :: Bool -> [TuiImagePreview] -> Widget Name
drawImagePreviews native previews =
    Widget Fixed Fixed do
        context <- getContext
        let maxWidth = viewportPreviewSize context.availWidth
            maxHeight = viewportPreviewSize context.availHeight
            visible =
                drop
                    (max
                        0
                        (length previews - previewCountForWidth maxWidth))
                    previews
            gaps = max 0 (length visible - 1) * previewGap
            previewWidth =
                max 1 ((maxWidth - gaps) `div` max 1 (length visible))
            previewHeight = max 1 (maxHeight - 1)
            content =
                hBox $
                    intersperse
                        (hLimit previewGap (fill ' '))
                        (map (drawPreview previewWidth previewHeight) visible)
        render $
            hLimit maxWidth $
                vLimit maxHeight content
  where
    previewGap = 2

    drawPreview maxWidth maxHeight preview =
        hLimit maxWidth $
            vBox
                [ hCenter $
                    if native
                        then nativeImagePlaceholder maxWidth maxHeight preview
                        else renderTuiImagePreview maxWidth maxHeight preview
                , hCenter $
                    withAttr Theme.mutedAttr $
                        txt $
                            "🖼 "
                                <> preview.previewMime
                                <> " · "
                                <> Text.pack (show preview.previewSourceWidth)
                                <> "×"
                                <> Text.pack (show preview.previewSourceHeight)
                                <> " · "
                                <> formatImageSize preview.previewBytes
                ]

    nativeImagePlaceholder maxWidth maxHeight preview =
        let (width, height) =
                previewCellSize maxWidth maxHeight preview
        in hLimit width $
            vLimit height $
                fill ' '

viewportPreviewSize :: Int -> Int
viewportPreviewSize available =
    max 1 (available * 3 `div` 5)

drawWorkspace :: AppState -> Widget Name
drawWorkspace state =
    Widget Greedy Greedy do
        context <- getContext
        render $
            padTop (Pad 1) $
                hBox $
                    [drawConversationPane state]
                        <> if not
                            (agentPaneVisible
                                context.availWidth
                                context.availHeight
                                state.appAgentEntries)
                            then []
                            else
                                [ hLimitPercent 40 $
                                    hLimit agentPaneWidth $
                                        padLeft (Pad 1) $
                                            drawAgentPane
                                                state
                                                (agentPaneEntryLimit
                                                    context.availHeight)
                                                state.appAgentSelected
                                                ((.agentHoverTarget)
                                                    <$> state.appAgentHover)
                                                state.appAgentEntries
                                ]

drawConversationPane :: AppState -> Widget Name
drawConversationPane state =
    case selectedChildEntry state of
        Just entry ->
            withVScrollBarRenderer conversationScrollbarRenderer $
                withVScrollBars OnRight $
                    viewport ConversationViewport Vertical $
                        padLeftRight 2 (drawAgentConversation entry)
        Nothing
            | conversationIsEmpty state.appUi ->
                padLeftRight 2 $
                    vBox
                        [ drawTranscript state
                        , drawEmptyConversation state
                        ]
            | otherwise ->
                withVScrollBarRenderer conversationScrollbarRenderer $
                    withVScrollBars OnRight $
                        viewport ConversationViewport Vertical $
                            padLeftRight 2 (drawTranscript state)

selectedChildEntry :: AppState -> Maybe AgentEntry
selectedChildEntry state =
    selectedAgentConversation
        state.appAgentSelected
        state.appAgentEntries

selectedAgentConversation
    :: AgentTarget
    -> [AgentEntry]
    -> Maybe AgentEntry
selectedAgentConversation selected entries = case selected of
    AgentRoot -> Nothing
    target ->
        find ((== target) . (.agentTarget)) entries

drawAgentConversation :: AgentEntry -> Widget Name
drawAgentConversation entry =
    vBox
        [ withAttr Theme.headingAttr $
            txt ("Viewing " <> entry.agentPath)
        , withAttr Theme.mutedAttr $
            txt
                (entry.agentStatus
                    <> " · input is sent to /root")
        , padTop (Pad 1) $
            case entry.agentTranscript of
                [] ->
                    withAttr Theme.mutedAttr $
                        txt "(no transcript yet)"
                rows ->
                    vBox (map drawAgentTranscriptLine rows)
        ]

drawAgentTranscriptLine :: Text -> Widget Name
drawAgentTranscriptLine line =
    padBottom (Pad 1) $
        case Text.breakOn ": " line of
            ("user", body) ->
                withAttr Theme.userAttr $
                    padAll 1 (txtWrap (Text.drop 2 body))
            ("assistant", body) ->
                padLeft (Pad 3) $
                    withAttr Theme.assistantAttr $
                        txtWrap (Text.drop 2 body)
            ("error", body) ->
                withAttr Theme.errorAttr $
                    txtWrap (Text.drop 2 body)
            _ ->
                withAttr Theme.mutedAttr (txtWrap line)

-- Brick's default scrollbar uses a full block for the thumb and a blank
-- space for the trough. During rapid viewport reflow, some terminals can
-- leave the old full-block cells behind because the replacement trough is
-- visually identical to the surrounding background. A visible one-column
-- trough makes every old thumb position get explicitly repainted.
conversationScrollbarRenderer :: VScrollbarRenderer n
conversationScrollbarRenderer =
    VScrollbarRenderer
        { renderVScrollbar =
            withAttr Theme.mutedAttr (fill '┃')
        , renderVScrollbarTrough =
            withAttr Theme.borderAttr (fill '│')
        , renderVScrollbarHandleBefore = emptyWidget
        , renderVScrollbarHandleAfter = emptyWidget
        , scrollbarWidthAllocation = 1
        }

agentPaneWidth :: Int
agentPaneWidth = 42

agentPaneMinScreenWidth :: Int
agentPaneMinScreenWidth = 72

agentPaneMinAvailableHeight :: Int
agentPaneMinAvailableHeight = 10

agentPaneVisible :: Int -> Int -> [AgentEntry] -> Bool
agentPaneVisible width height entries =
    width >= agentPaneMinScreenWidth
        && height >= agentPaneMinAvailableHeight
        && length entries > 1

agentPaneEntryLimit :: Int -> Int
agentPaneEntryLimit availableHeight =
    -- Reserve the outer top pad, four pane chrome rows (border and padding),
    -- and two possible truncation indicators above and below.
    max 1 (min 12 (availableHeight - 7))

drawAgentPane
    :: AppState
    -> Int
    -> AgentTarget
    -> Maybe AgentTarget
    -> [AgentEntry]
    -> Widget Name
drawAgentPane state entryLimit selected hovered entries =
    clickable AgentPane $
        withAttr Theme.borderAttr $
            withBorderStyle unicodeRounded $
                borderWithLabel
                    (txt
                        (" Agents · "
                            <> Text.pack (show childCount)
                            <> " ")) $
                    padAll 1 $
                        vBox agentRows
  where
    ordered = sortOn (.agentPath) entries
    childCount =
        length
            [ ()
            | entry <- ordered
            , entry.agentTarget /= AgentRoot
            ]
    (hiddenBefore, shownEntries, hiddenAfter) =
        agentEntryWindow entryLimit selected ordered
    agentRows =
        [ drawHidden hiddenBefore "above"
        | hiddenBefore > 0
        ]
            <> map drawEntry shownEntries
            <> [ drawHidden hiddenAfter "below"
               | hiddenAfter > 0
               ]
    drawHidden count direction =
        withAttr Theme.mutedAttr $
            txt
                ("  … "
                    <> Text.pack (show count)
                    <> " "
                    <> direction)
    drawEntry entry =
        let
            index =
                maybe 0 id $
                    findIndex ((== entry.agentTarget) . (.agentTarget)) ordered
            marker =
                if entry.agentTarget == selected then "› " else "  "
            statusGlyph
                | isBackgroundAgentActive entry =
                    quietIndicator
                        motionGlyphSet
                        state.appRuntime.runtimeMotionMode
                        state.appMotionElapsedMillis
                | otherwise =
                    agentStatusGlyph entry.agentStatus
            row = hBox
                [ txt
                    (marker
                        <> agentEntryTreeLabelWithGlyph
                            statusGlyph
                            ordered
                            index
                            entry)
                , fill ' '
                ]
            styled =
                if hovered == Just entry.agentTarget
                    then withAttr Theme.controlLinkHoverAttr row
                    else if entry.agentTarget == selected
                        then withAttr Theme.successAttr row
                        else row
        in clickable (AgentRow entry.agentTarget) (vLimit 1 styled)

agentPopoverLayers :: AppState -> [Widget Name]
agentPopoverLayers state =
    case (length state.appAgentEntries > 1, state.appAgentHover) of
        (False, _) -> []
        (_, Nothing) -> []
        (True, Just hover) ->
            case find
                ((== hover.agentHoverTarget) . (.agentTarget))
                state.appAgentEntries of
                Nothing -> []
                Just entry -> [positionAgentPopover state hover entry]

agentPopoverPreferredWidth :: Int
agentPopoverPreferredWidth = 40

agentPopoverMinWidth :: Int
agentPopoverMinWidth = 24

agentPopoverHeight :: Int
agentPopoverHeight = 9

agentPopoverGap :: Int
agentPopoverGap = 1

positionAgentPopover :: AppState -> AgentHover -> AgentEntry -> Widget Name
positionAgentPopover state hover entry =
    Widget Fixed Fixed do
        context <- getContext
        let screenWidth = max 0 context.availWidth
            screenHeight = max 0 context.availHeight
            Location (_, anchorY) = hover.agentHoverUpperLeft
            Location (paneX, _) = hover.agentHoverPaneUpperLeft
            paneRight = paneX + max 1 hover.agentHoverPaneWidth
            leftAvailable = max 0 (paneX - agentPopoverGap)
            rightAvailable =
                max 0 (screenWidth - paneRight - agentPopoverGap)
            placeLeft =
                leftAvailable >= agentPopoverMinWidth
                    || leftAvailable >= rightAvailable
            available =
                if placeLeft then leftAvailable else rightAvailable
            width = min agentPopoverPreferredWidth available
            height = min agentPopoverHeight screenHeight
            x
                | placeLeft =
                    max 0 (paneX - agentPopoverGap - width)
                | otherwise =
                    min
                        (max 0 (screenWidth - width - agentPopoverGap))
                        paneRight
            y = max 0 (min anchorY (screenHeight - height))
        if screenWidth < agentPaneMinScreenWidth
            || width < agentPopoverMinWidth
            || height < 5
            then render emptyWidget
            else
                render $
                    translateBy (Location (x, y)) $
                        drawAgentPopover state placeLeft width height entry

drawAgentPopover :: AppState -> Bool -> Int -> Int -> AgentEntry -> Widget Name
drawAgentPopover state placeLeft width height entry =
    clickable (AgentPopover entry.agentTarget) surface
  where
    surface
        | placeLeft = hBox [popover, bridge]
        | otherwise = hBox [bridge, popover]
    bridge =
        hLimit agentPopoverGap $
            vLimit height (fill ' ')
    popover =
        hLimit width $
            vLimit height $
                withAttr Theme.baseAttr $
                    overrideAttr Border.borderAttr Theme.borderActiveAttr $
                        withBorderStyle unicodeRounded $
                            borderWithLabel
                                (withAttr Theme.headingAttr $
                                    txt
                                        (" "
                                            <> truncateDisplayText
                                                (max 1 (width - 6))
                                                (agentDisplayName entry.agentPath)
                                            <> " ")) $
                                padAll 1 $
                                    vBox
                                        ( intersperse
                                            (vLimit 1 (fill ' '))
                                            (map
                                                (drawAgentStep
                                                    state
                                                    (max 1 (width - 4)))
                                                steps)
                                            <> [fill ' ']
                                        )
    steps =
        case take 2 entry.agentSteps of
            [] ->
                [ AgentStep
                    { agentStepState = AgentStepInfo
                    , agentStepTitle = "No recent activity"
                    , agentStepDetail = Just entry.agentStatus
                    }
                ]
            recent -> recent

drawAgentStep :: AppState -> Int -> AgentStep -> Widget Name
drawAgentStep state width step =
    vLimit 2 $
        vBox
            [ hBox
                [ withAttr (agentStepAttr step.agentStepState) $
                    txt (agentStepGlyph state step.agentStepState)
                , txt " "
                , withAttr Theme.assistantAttr $
                    txt
                        (truncateDisplayText
                            (max 1 (width - 2))
                            step.agentStepTitle)
                ]
            , padLeft (Pad 2) $
                withAttr Theme.mutedAttr $
                    txt
                        (truncateDisplayText
                            (max 1 (width - 2))
                            (fromMaybe
                                (agentStepStateLabel step.agentStepState)
                                step.agentStepDetail))
            ]

agentStepGlyph :: AppState -> AgentStepState -> Text
agentStepGlyph state = \case
    AgentStepRunning ->
        quietIndicator
            motionGlyphSet
            state.appRuntime.runtimeMotionMode
            state.appMotionElapsedMillis
    AgentStepCompleted -> "✓"
    AgentStepFailed -> "✕"
    AgentStepInfo -> "◆"

agentStepStateLabel :: AgentStepState -> Text
agentStepStateLabel = \case
    AgentStepRunning -> "running"
    AgentStepCompleted -> "completed"
    AgentStepFailed -> "failed"
    AgentStepInfo -> "recent activity"

agentStepAttr :: AgentStepState -> AttrName
agentStepAttr = \case
    AgentStepRunning -> Theme.thinkingAttr
    AgentStepCompleted -> Theme.successAttr
    AgentStepFailed -> Theme.errorAttr
    AgentStepInfo -> Theme.toolAttr

agentEntryWindow
    :: Int
    -> AgentTarget
    -> [AgentEntry]
    -> (Int, [AgentEntry], Int)
agentEntryWindow count selected entries
    | count <= 0 = (0, [], length entries)
    | length entries <= count = (0, entries, 0)
    | otherwise =
        ( start
        , take count (drop start entries)
        , length entries - start - count
        )
  where
    selectedIndex =
        maybe 0 id $
            findIndex ((== selected) . (.agentTarget)) entries
    start =
        max 0 $
            min
                (selectedIndex - count `div` 2)
                (length entries - count)

drawHeader :: AppState -> Widget Name
drawHeader state =
    withAttr Theme.headerAttr $
        padLeftRight 2 $
            hBox
                [ hLimitPercent 68 (drawRepositoryHeader state.appUi)
                , vLimit 1 (fill ' ')
                , drawHeaderRight state
                ]

drawRepositoryHeader :: UiState -> Widget Name
drawRepositoryHeader state
    | Text.null state.uiBranch =
        withAttr Theme.mutedAttr (txt state.uiCwd)
    | otherwise =
        hBox
            [ txt "\xE0A0 "
            , withAttr Theme.mutedAttr $
                txt (repositoryHeaderText state.uiBranch state.uiCwd)
            ]

repositoryHeaderText :: Text -> Text -> Text
repositoryHeaderText branch cwd =
    Text.intercalate "  " $
        filter (not . Text.null) [branch, cwd]

drawHeaderRight :: AppState -> Widget Name
drawHeaderRight state =
    hBox
        [ activityWidget
        , withAttr Theme.mutedAttr (txt elapsed)
        , withAttr Theme.mutedAttr (txt usage)
        ]
  where
    ui = state.appUi
    waiting = userActionPending state
    background = hasBackgroundActivity state.appAgentEntries
    mode = state.appRuntime.runtimeMotionMode
    motionMillis = state.appMotionElapsedMillis
    activityWidget
        | waiting =
            hBox
                [ withAttr (waitingIndicatorAttr state) $
                    txt (waitingIndicator motionGlyphSet mode motionMillis)
                , withAttr Theme.thinkingAttr (txt " Waiting for you")
                ]
        | otherwise =
            withAttr activityAttr (txt activity)
    activityAttr
        | ui.uiRunning = Theme.thinkingAttr
        | ui.uiCompletionRemainingMillis > 0 = Theme.successAttr
        | background = Theme.toolAttr
        | otherwise = Theme.mutedAttr
    activity
        | ui.uiRunning =
            foregroundIndicator motionGlyphSet mode motionMillis
                <> " "
                <> ui.uiActivity
        | ui.uiCompletionRemainingMillis > 0 =
            ui.uiActivity
        | background =
            backgroundIndicator motionGlyphSet mode motionMillis
                <> " Background work"
        | otherwise =
            ui.uiActivity
    elapsed =
        if ui.uiRunning
            then " · "
                <> formatElapsed
                    (fromIntegral ui.uiElapsedMillis / 1000)
            else ""
    formattedUsage = formatTokenUsage ui.uiPrompt.promptUsage
    usage =
        if Text.null formattedUsage then "" else " │ " <> formattedUsage

waitingIndicatorAttr :: AppState -> AttrName
waitingIndicatorAttr state =
    case waitingIndicator
        motionGlyphSet
        state.appRuntime.runtimeMotionMode
        state.appMotionElapsedMillis of
        "◇" -> Theme.waitingDimAttr
        "." -> Theme.waitingDimAttr
        "◈" -> Theme.waitingMidAttr
        "*" -> Theme.waitingMidAttr
        _ -> Theme.thinkingAttr

drawTranscript :: AppState -> Widget Name
drawTranscript state =
    vBox $
        [vBox (map (drawBlock state) blocks)]
            <> conversationReserveWidgets anchor
  where
    blocks = toList state.appUi.uiBlocks
    anchor = state.appConversationAnchor

stickyPromptLayers :: AppState -> [Widget Name]
stickyPromptLayers state =
    case (state.appAgentSelected, state.appConversationAnchor) of
        (AgentRoot, Just anchor)
            | Scroll.conversationAnchorSticky anchor ->
                [ translateBy (Location (0, 2)) $
                    hLimitPercent conversationWidth $
                        padLeftRight 2 $
                            withAttr Theme.userAttr $
                                vLimit 5 $
                                    padAll 1 $
                                        txtWrap
                                            (stickyPromptPreview
                                                anchor.anchorText)
                ]
        _ -> []
  where
    conversationWidth =
        if length state.appAgentEntries <= 1 then 100 else 68

stickyPromptPreview :: Text -> Text
stickyPromptPreview text =
    case splitAt 3 (Text.lines text) of
        (shown, []) -> Text.intercalate "\n" shown
        (shown, _ : _) ->
            Text.intercalate "\n" (take 2 shown <> ["…"])

conversationReserveWidgets
    :: Maybe Scroll.ConversationAnchor
    -> [Widget Name]
conversationReserveWidgets = \case
    Just anchor
        | anchor.anchorReserveRows > 0 ->
            [ reportExtent ConversationReserve $
                vLimit anchor.anchorReserveRows (fill ' ')
            ]
    _ -> []

drawEmptyConversation :: AppState -> Widget Name
drawEmptyConversation state =
    center (lambdaArtWidget frame)
  where
    frame
        | userActionPending state = 0
        | otherwise = case state.appRuntime.runtimeMotionMode of
            MotionFull -> state.appMotionElapsedMillis `div` 160
            MotionReduced -> 0
            MotionOff -> 0

data LambdaComposition = LambdaComposition
    { lambdaUpperHeight :: !Int
    , lambdaLowerHeight :: !Int
    , lambdaStrokeWidth :: !Int
    , lambdaMarginX :: !Int
    , lambdaMarginY :: !Int
    , lambdaHasOrbit :: !Bool
    }

data LambdaPalette = LambdaPalette
    { lambdaDim :: !V.Attr
    , lambdaTrail :: !V.Attr
    , lambdaGlow :: !V.Attr
    , lambdaSpark :: !V.Attr
    }

lambdaArtWidget :: Int -> Widget Name
lambdaArtWidget frame =
    B.Widget B.Fixed B.Fixed do
        context <- B.getContext
        dimAttr <- B.lookupAttrName Theme.lambdaDimAttr
        trailAttr <- B.lookupAttrName Theme.lambdaTrailAttr
        glowAttr <- B.lookupAttrName Theme.lambdaGlowAttr
        sparkAttr <- B.lookupAttrName Theme.lambdaSparkAttr
        let
            composition =
                lambdaComposition context.availWidth context.availHeight
            palette = LambdaPalette
                { lambdaDim = dimAttr
                , lambdaTrail = trailAttr
                , lambdaGlow = glowAttr
                , lambdaSpark = sparkAttr
                }
            rows =
                buildSolidLambdaRows
                    composition.lambdaUpperHeight
                    composition.lambdaLowerHeight
                    composition.lambdaStrokeWidth
            logoWidth = maximum (0 : map length rows)
            canvasWidth = logoWidth + 2 * composition.lambdaMarginX
            canvasHeight = length rows + 2 * composition.lambdaMarginY
            particles =
                lambdaOrbitParticles
                    palette
                    frame
                    canvasWidth
                    canvasHeight
                    composition.lambdaHasOrbit
            rendered =
                V.vertCat
                    [ V.horizCat
                        [ renderLambdaCell
                            palette
                            frame
                            composition
                            rows
                            particles
                            x
                            y
                        | x <- [0 .. canvasWidth - 1]
                        ]
                    | y <- [0 .. canvasHeight - 1]
                    ]
            bounded =
                V.crop
                    (max 0 context.availWidth)
                    (max 0 context.availHeight)
                    rendered
        pure B.emptyResult { B.image = bounded }

lambdaComposition :: Int -> Int -> LambdaComposition
lambdaComposition width height
    | width >= 42
    , height >= 21 =
        LambdaComposition
            { lambdaUpperHeight = 8
            , lambdaLowerHeight = 11
            , lambdaStrokeWidth = 3
            , lambdaMarginX = 8
            , lambdaMarginY = 1
            , lambdaHasOrbit = True
            }
    | width >= 24
    , height >= 14 =
        LambdaComposition
            { lambdaUpperHeight = 5
            , lambdaLowerHeight = 7
            , lambdaStrokeWidth = 2
            , lambdaMarginX = 4
            , lambdaMarginY = 1
            , lambdaHasOrbit = True
            }
    | otherwise =
        LambdaComposition
            { lambdaUpperHeight = 3
            , lambdaLowerHeight = 4
            , lambdaStrokeWidth = 2
            , lambdaMarginX = 0
            , lambdaMarginY = 0
            , lambdaHasOrbit = False
            }

buildSolidLambdaRows :: Int -> Int -> Int -> [String]
buildSolidLambdaRows upperHeight lowerHeight strokeWidth =
    map row [0 .. totalHeight - 1]
  where
    totalHeight = upperHeight + lowerHeight
    mainStart =
        max 0 (strokeWidth + lowerHeight - 1 - upperHeight)
    width = mainColumn (totalHeight - 1) + strokeWidth
    row rowIndex =
        map (cell rowIndex) [0 .. width - 1]
    cell rowIndex column
        | rowIndex >= upperHeight
        , column >= branchColumn rowIndex
        , column < branchColumn rowIndex + strokeWidth =
            '/'
        | column >= mainColumn rowIndex
        , column < mainColumn rowIndex + strokeWidth =
            '\\'
        | otherwise = ' '
      where
        branchColumn index =
            mainStart
                + upperHeight
                - strokeWidth
                - (index - upperHeight)
    mainColumn index =
        mainStart + index

renderLambdaCell
    :: LambdaPalette
    -> Int
    -> LambdaComposition
    -> [String]
    -> [((Int, Int), Char, V.Attr)]
    -> Int
    -> Int
    -> V.Image
renderLambdaCell palette frame composition rows particles x y =
    case lambdaLogoChar rows composition.lambdaMarginX
        composition.lambdaMarginY x y of
        ' ' ->
            case find
                (\(position, _, _) -> position == (x, y))
                particles of
                Just (_, character, attr) ->
                    V.char attr character
                Nothing ->
                    V.char palette.lambdaDim ' '
        character ->
            let
                localX = x - composition.lambdaMarginX
                localY = y - composition.lambdaMarginY
                (attr, animatedCharacter) =
                    animatedLambdaStroke
                        palette
                        frame
                        localX
                        localY
                        character
            in V.char attr animatedCharacter

lambdaLogoChar :: [String] -> Int -> Int -> Int -> Int -> Char
lambdaLogoChar rows marginX marginY x y
    | localX < 0 || localY < 0 = ' '
    | otherwise =
        case drop localY rows of
            row : _ ->
                case drop localX row of
                    character : _ -> character
                    [] -> ' '
            [] -> ' '
  where
    localX = x - marginX
    localY = y - marginY

animatedLambdaStroke
    :: LambdaPalette
    -> Int
    -> Int
    -> Int
    -> Char
    -> (V.Attr, Char)
animatedLambdaStroke palette frame x y character
    | distance == 0 =
        (palette.lambdaSpark, energizedStroke character)
    | distance <= 2 =
        (palette.lambdaGlow, character)
    | distance <= 5 =
        (palette.lambdaTrail, character)
    | otherwise =
        (palette.lambdaDim, character)
  where
    period = 36
    phase = frame `mod` period
    oppositePhase = (phase + period `div` 2) `mod` period
    cellPhase = (y * 5 + x * 3) `mod` period
    distance =
        min
            (circularDistance period phase cellPhase)
            (circularDistance period oppositePhase cellPhase)

energizedStroke :: Char -> Char
energizedStroke = \case
    '_' -> '='
    _ -> '*'

circularDistance :: Int -> Int -> Int -> Int
circularDistance period left right =
    let direct = abs (left - right)
    in min direct (period - direct)

lambdaOrbitParticles
    :: LambdaPalette
    -> Int
    -> Int
    -> Int
    -> Bool
    -> [((Int, Int), Char, V.Attr)]
lambdaOrbitParticles _ _ _ _ False = []
lambdaOrbitParticles palette frame width height True =
    [ (position, character, attr)
    | (offset, (character, attr)) <- zip offsets particleStyles
    , Just position <- [cyclicAt path (frame + offset)]
    ]
  where
    path = lambdaOrbitPath width height
    pathLength = length path
    offsets =
        [ 0
        , pathLength `div` 3
        , 2 * pathLength `div` 3
        ]
    particleStyles =
        [ ('*', palette.lambdaSpark)
        , ('+', palette.lambdaGlow)
        , ('.', palette.lambdaTrail)
        ]

lambdaOrbitPath :: Int -> Int -> [(Int, Int)]
lambdaOrbitPath width height =
    top <> right <> bottom <> left
  where
    horizontal = [2, 4 .. width - 3]
    vertical = [2, 4 .. height - 3]
    top = [(x, 0) | x <- horizontal]
    right = [(width - 1, y) | y <- vertical]
    bottom = [(x, height - 1) | x <- reverse horizontal]
    left = [(0, y) | y <- reverse vertical]

cyclicAt :: [a] -> Int -> Maybe a
cyclicAt [] _ = Nothing
cyclicAt values index =
    case drop (index `mod` length values) values of
        value : _ -> Just value
        [] -> Nothing

drawBlock :: AppState -> UiBlock -> Widget Name
drawBlock state block =
    let ui = state.appUi
        selected = ui.uiSelectedBlock == Just block.blockId
        highlighted = selected && ui.uiFocus == FocusScrollback
        content = case block.blockKind of
            BlockUser ->
                withAttr Theme.userAttr $
                    padAll 1 (txtWrap block.blockBody)
            BlockAssistant ->
                padLeft (Pad 3) $
                    withAttr Theme.assistantAttr
                        (markdownWidgetWithSyntaxHighlighting
                            state.appSyntaxHighlighter
                            (\codeIndex ->
                                cached
                                    (CodeBlockCache
                                        block.blockId
                                        codeIndex))
                            (codeBlockHeader state block.blockId)
                            block.blockBody)
            BlockThinking ->
                accentBlock (thinkingBlockAttr state block)
                    (blockStateGlyph state block <> block.blockTitle)
                    (visibleBody block)
            BlockTool ->
                accentBlock (statusAttr state block)
                    (blockStateGlyph state block <> block.blockTitle <> detailSuffix block)
                    (visibleBody block)
            BlockShell ->
                accentCodeBlock
                    state.appSyntaxHighlighter
                    (statusAttr state block)
                    (blockStateGlyph state block <> block.blockTitle)
                    block.blockDetail
                    (visibleShellBody block)
            BlockEdit ->
                accentBlock (statusAttr state block)
                    (blockStateGlyph state block <> block.blockTitle <> detailSuffix block)
                    (visibleBody block)
            BlockSystem ->
                withAttr Theme.mutedAttr (txtWrap block.blockBody)
            BlockError ->
                withAttr Theme.errorAttr (txtWrap block.blockBody)
        framed =
            if highlighted
                then withAttr Theme.selectedAttr content
                else content
        rendered =
            clickable (ConversationBlock block.blockId) $
                padBottom (Pad 1) $
                    hBox
                        [ withAttr
                            (if highlighted
                                then Theme.borderActiveAttr
                                else Theme.mutedAttr)
                            (txt (if highlighted then "❯ " else "  "))
                        , framed
                        ]
    in if cacheableBlock state block
        then cached
            (ConversationBlockCache
                block.blockId
                highlighted
                block.blockExpanded
                (codeCopyCacheState state block.blockId))
            rendered
        else rendered

codeBlockHeader :: AppState -> BlockId -> Int -> Text -> Widget Name
codeBlockHeader state blockId codeIndex language =
    hBox
        [ if Text.null language
            then emptyWidget
            else withAttr Theme.mutedAttr (txt language)
        , vLimit 1 (fill ' ')
        , clickable name $
            withAttr
                (Composer.controlAttr state name Theme.controlLinkAttr)
                (txt " Copy ")
        ]
  where
    name = CodeCopy blockId codeIndex

codeCopyCacheState :: AppState -> BlockId -> Maybe (Int, Bool)
codeCopyCacheState state blockId =
    case state.appHoveredControl of
        Just (CodeCopy hoveredBlock codeIndex)
            | hoveredBlock == blockId ->
                Just
                    ( codeIndex
                    , state.appPressedControl
                        == Just (CodeCopy blockId codeIndex)
                    )
        _ -> Nothing

cacheableBlock :: AppState -> UiBlock -> Bool
cacheableBlock state block =
    block.blockState
        `notElem` [BlockStreaming, BlockRunning]
        && maybe
            True
            ((/= block.blockId) . (.retryCountdownBlockId))
            state.appUi.uiRetryCountdown
        && not (blockFlashing state block)

blockStateGlyph :: AppState -> UiBlock -> Text
blockStateGlyph state block = case block.blockState of
    BlockRunning -> liveGlyph
    BlockStreaming -> liveGlyph
    BlockComplete -> "✓ "
    BlockFailed -> "✗ "
    BlockDenied -> "⊘ "
    BlockCancelled -> "⊘ "
  where
    liveGlyph
        | userActionPending state =
            waitingIndicator
                motionGlyphSet
                MotionOff
                state.appMotionElapsedMillis
                <> " "
        | otherwise =
            foregroundIndicator
                motionGlyphSet
                state.appRuntime.runtimeMotionMode
                state.appMotionElapsedMillis
                <> " "

accentBlock :: AttrName -> Text -> Text -> Widget Name
accentBlock accent title body =
    accentBlockWithSections accent title $
        if Text.null (Text.strip body)
            then []
            else [txtWrap body]

accentCodeBlock
    :: Maybe SyntaxHighlighter
    -> AttrName
    -> Text
    -> Text
    -> Text
    -> Widget Name
accentCodeBlock syntaxHighlighter accent title code body =
    accentBlockWithSections accent title $
        [ codeWidgetWithSyntaxHighlighting syntaxHighlighter "haskell" code
        | not (Text.null (Text.strip code))
        ]
            <> [ txtWrap body
               | not (Text.null (Text.strip body))
               ]

accentBlockWithSections
    :: AttrName
    -> Text
    -> [Widget Name]
    -> Widget Name
accentBlockWithSections accent title sections =
    hBox
        [ withAttr accent (txt "❙")
        , padLeft (Pad 2) $
            vBox $
                [withAttr accent (txtWrap title)]
                    <> map (padTop (Pad 1)) sections
        ]

visibleBody :: UiBlock -> Text
visibleBody block
    | block.blockExpanded = block.blockBody
    | otherwise =
        let rows = Text.lines block.blockBody
            shown = take 3 rows
            hidden = length rows - length shown
        in Text.unlines shown
            <> if hidden > 0
                then "… +" <> Text.pack (show hidden) <> " lines"
                else ""

visibleShellBody :: UiBlock -> Text
visibleShellBody block
    | block.blockExpanded = block.blockBody
    | otherwise =
        let rows = Text.lines block.blockBody
            shown
                | length rows <= 5 = rows
                | otherwise = take 2 rows <> ["…"] <> drop (length rows - 3) rows
        in Text.unlines shown

detailSuffix :: UiBlock -> Text
detailSuffix block
    | Text.null (Text.strip block.blockDetail) = ""
    | otherwise = "  " <> block.blockDetail

statusAttr :: AppState -> UiBlock -> AttrName
statusAttr state block
    | blockFlashing state block
    , block.blockState == BlockComplete =
        Theme.completionFlashAttr
    | otherwise = case block.blockState of
        BlockFailed -> Theme.errorAttr
        BlockCancelled -> Theme.mutedAttr
        BlockDenied -> Theme.errorAttr
        BlockComplete -> Theme.successAttr
        BlockRunning -> Theme.toolAttr
        BlockStreaming -> Theme.thinkingAttr

thinkingBlockAttr :: AppState -> UiBlock -> AttrName
thinkingBlockAttr state block
    | blockFlashing state block
    , block.blockState == BlockComplete =
        Theme.completionFlashAttr
    | otherwise =
        Theme.thinkingAttr

blockFlashing :: AppState -> UiBlock -> Bool
blockFlashing state block =
    Map.member block.blockId state.appCompletionFlashes

drawNotice :: AppState -> Widget Name
drawNotice state = case state.appUi.uiNotice of
    Nothing -> emptyWidget
    Just notice ->
        let (attr, prefix) = noticePresentation state notice.noticeKind
        in withAttr attr $
            padLeftRight 2 (txtWrap (prefix <> notice.noticeText))

noticePresentation :: AppState -> NoticeKind -> (AttrName, Text)
noticePresentation state = \case
    NoticeInfo -> (Theme.footerAttr, "• ")
    NoticeSuccess -> (Theme.successAttr, "✓ ")
    NoticeWarning -> (Theme.thinkingAttr, "⚠ ")
    NoticeProgress ->
        ( Theme.thinkingAttr
        , foregroundIndicator
            motionGlyphSet
            (if userActionPending state
                then MotionOff
                else state.appRuntime.runtimeMotionMode)
            state.appMotionElapsedMillis
            <> " "
        )
    NoticeError -> (Theme.errorAttr, "✗ ")

drawFollowStatus :: UiState -> Widget Name
drawFollowStatus state
    | state.uiFollow = emptyWidget
    | otherwise =
        withAttr Theme.thinkingAttr $
            padLeftRight 2 $
                txt "↓ Live output paused · End to resume"

drawFooter :: AppState -> Widget Name
drawFooter state =
    withAttr Theme.footerAttr $
        padLeftRight 2 $
            txt footer
  where
    footer = case (state.appTextPrompt, state.appChoice, state.appUi.uiFocus) of
        (Just _, _, _) ->
            if state.appUi.uiRunning
                then "Enter submit  │  Shift+Enter newline  │  PgUp/PgDn scroll  │  Esc close  │  Ctrl+C cancel turn"
                else "Enter submit  │  Shift+Enter newline  │  PgUp/PgDn scroll  │  Esc cancel"
        (Nothing, Just _, _) ->
            if state.appUi.uiRunning
                then "↑↓ select  │  Enter choose  │  Esc close  │  Ctrl+C cancel turn"
                else "↑↓ select  │  Enter choose  │  Esc cancel"
        (Nothing, Nothing, focus) ->
                case focus of
                    FocusPermission ->
                        "↑↓ select  │  Enter choose  │  Esc deny"
                    FocusScrollback ->
                        "↑↓ blocks  │  Ctrl+J/K lines  │  PgUp/PgDn pages  │  wheel scroll  │  Tab/Space prompt"
                    FocusComposer
                        | not state.appUi.uiAwaitingInput ->
                            "Enter queue  │  Ctrl+Enter/Ctrl+O send now  │  Shift+Enter newline  │  Esc/Ctrl+C cancel  │  Tab scrollback"
                        | otherwise ->
                            "Enter send  │  Shift+Enter newline  │  PgUp/PgDn or wheel scroll  │  Tab scrollback"

drawPermission :: AppState -> PermissionOverlay -> Widget Name
drawPermission state permission =
    centerLayer $
        hLimitPercent 78 $
            overrideAttr Border.borderAttr Theme.borderActiveAttr $
                withBorderStyle unicodeRounded $
                    borderWithLabel
                        (waitingOverlayLabel state "Permission") $
                        padAll 1 $
                            vBox
                                [ txtWrap permission.permissionSummary
                                , padTop (Pad 1) $
                                    vBox $
                                        zipWith
                                            (permissionRow permission.permissionIndex)
                                            [0 ..]
                                            [ "Allow once"
                                            , "Always allow this tool this session"
                                            , "Deny"
                                            ]
                                ]

permissionRow :: Int -> Int -> Text -> Widget Name
permissionRow selected index label =
    let prefix = if selected == index then "› " else "  "
        widget = txt (prefix <> label)
        styled =
            if selected == index
                then withAttr Theme.selectedAttr widget
                else widget
    in clickable (PermissionRow index) styled

drawResume :: AppState -> ResumeOverlay -> Widget Name
drawResume state overlay =
    centerLayer $
        hLimitPercent 82 $
            vLimitPercent 82 $
                overrideAttr Border.borderAttr Theme.borderActiveAttr $
                    withBorderStyle unicodeRounded $
                        borderWithLabel
                            (waitingOverlayLabel state "Resume session") $
                            vBox
                                [ padLeftRight 1 (resumeHeader browser)
                                , Border.hBorder
                                , withVScrollBarRenderer conversationScrollbarRenderer $
                                    withVScrollBars OnRight $
                                        viewport ResumeViewport Vertical $
                                            padLeftRight 1 (resumeList browser)
                                , Border.hBorder
                                , padLeftRight 1 (resumeFooter browser)
                                ]
  where
    browser = overlay.resumeOverlayBrowser

resumeHeader :: ResumeBrowser -> Widget Name
resumeHeader browser =
    hBox
        [ search
        , vLimit 1 (fill ' ')
        , withAttr Theme.mutedAttr $
            txt (resumeSourceLabel browser.resumeBrowserSource <> "  f")
        ]
  where
    prefix
        | browser.resumeBrowserSearching = "search: "
        | Text.null browser.resumeBrowserQuery = "/ to search"
        | otherwise = "search: "
    search
        | browser.resumeBrowserSearching =
            showCursor
                ResumeSearchCursor
                (Location
                    (resumeSearchCursorColumn
                        prefix
                        browser.resumeBrowserQuery, 0))
                (txt (prefix <> browser.resumeBrowserQuery <> " "))
        | Text.null browser.resumeBrowserQuery =
            withAttr Theme.mutedAttr (txt prefix)
        | otherwise =
            txt (prefix <> browser.resumeBrowserQuery)

resumeSearchCursorColumn :: Text -> Text -> Int
resumeSearchCursorColumn prefix query =
    terminalTextWidth prefix + terminalTextWidth query

resumeList :: ResumeBrowser -> Widget Name
resumeList browser =
    case visibleResumeBrowser browser of
        [] ->
            padTop (Pad 1) $
                withAttr Theme.mutedAttr (txt "  No matches")
        entries ->
            vBox $
                intersperse (txt "") $
                    map (resumeGroup browser selectedId) (groupResumeEntries entries)
  where
    selectedId = (.resumeId) <$> selectedResumeBrowser browser

resumeGroup
    :: ResumeBrowser
    -> Maybe Text
    -> (Text, [ResumeEntry])
    -> Widget Name
resumeGroup browser selectedId (project, entries) =
    vBox
        [ hBox
            [ withAttr Theme.mutedAttr $
                txt (" " <> project <> " ")
            , withAttr Theme.mutedAttr (vLimit 1 (fill '─'))
            ]
        , vBox (map (resumeRow browser selectedId) entries)
        ]

resumeRow :: ResumeBrowser -> Maybe Text -> ResumeEntry -> Widget Name
resumeRow browser selectedId entry =
    clickable (ResumeRow entry.resumeId) $
        if selected
            then forceAttr Theme.selectedAttr body
            else body
  where
    selected = selectedId == Just entry.resumeId
    expanded = browser.resumeBrowserExpanded == Just entry.resumeId
    marker
        | expanded = "◆ "
        | otherwise = "› "
    summary =
        hBox
            [ hLimitPercent 78 (txt (marker <> entry.resumeTitle))
            , vLimit 1 (fill ' ')
            , withAttr Theme.mutedAttr $
                txt (resumeRelativeAge browser.resumeBrowserNow entry.resumeUpdatedAt)
            ]
    body
        | expanded =
            vBox
                [ summary
                , padLeft (Pad 4) $
                    vBox
                        [ resumeDetail "ID" entry.resumeId
                        , resumeDetail "CWD" entry.resumeCwd
                        , resumeDetail "Model" entry.resumeModel
                        , resumeDetail
                            "Created"
                            (resumeAbsoluteTime entry.resumeCreatedAt)
                        , resumeDetail
                            "Updated"
                            (resumeAbsoluteTime entry.resumeUpdatedAt)
                        , resumeDetail
                            "Source"
                            ("local · " <> entry.resumeProvider)
                        , resumeDetail
                            "Messages"
                            (Text.pack (show entry.resumeMessageCount))
                        , resumeDetail
                            "Turns"
                            ( Text.pack (show entry.resumeTurnCount)
                                <> "    Tools  "
                                <> Text.pack (show entry.resumeToolCount)
                            )
                        , resumeDetail
                            "Prompt"
                            (if Text.null entry.resumePrompt
                                then "(none)"
                                else entry.resumePrompt)
                        ]
                ]
        | otherwise = summary

resumeDetail :: Text -> Text -> Widget Name
resumeDetail label value =
    hBox
        [ withAttr Theme.mutedAttr (txt (Text.justifyLeft 12 ' ' label))
        , txtWrap value
        ]

resumeAbsoluteTime :: UTCTime -> Text
resumeAbsoluteTime =
    Text.pack . formatTime defaultTimeLocale "%b %e, %Y %H:%M UTC"

resumeFooter :: ResumeBrowser -> Widget Name
resumeFooter browser =
    withAttr attr (txt footer)
  where
    hasRows = not (null (visibleResumeBrowser browser))
    (attr, footer) =
        case browser.resumeBrowserNotice of
            Just notice -> (Theme.errorAttr, notice)
            Nothing
                | isJust browser.resumeBrowserDeletePending ->
                    (Theme.thinkingAttr, "y confirm delete  │  n cancel")
                | browser.resumeBrowserSearching ->
                    (Theme.footerAttr, "type to search  │  Enter/Esc done  │  ↑↓ nav")
                | not hasRows ->
                    (Theme.footerAttr, "f filter  │  / search  │  Esc cancel")
                | otherwise ->
                    ( Theme.footerAttr
                    , "↑↓ nav  │  Enter resume  │  e expand  │  / search  │  f filter  │  d delete  │  Esc cancel"
                    )

drawChoice :: AppState -> ChoiceOverlay -> Widget Name
drawChoice appState choice = case choice.choicePresentation of
    ChoiceDialog -> drawDialogChoice appState choice
    ChoiceOnboarding -> drawOnboardingChoice appState choice

drawDialogChoice :: AppState -> ChoiceOverlay -> Widget Name
drawDialogChoice appState choice =
    centerLayer $
        hLimitPercent 82 $
            vLimitPercent 78 $
                overrideAttr Border.borderAttr Theme.borderActiveAttr $
                    withBorderStyle unicodeRounded $
                        borderWithLabel
                            (waitingOverlayLabel appState choice.choiceTitle) $
                            padAll 1 $
                                vBox
                                    [ if Text.null (Text.strip choice.choiceBody)
                                        then emptyWidget
                                        else padBottom (Pad 1) $
                                            vLimitPercent 65 $
                                                viewport OverlayViewport Vertical $
                                                    markdownWidget choice.choiceBody
                                    , vBox $
                                        zipWith
                                            (choiceRow
                                                appState
                                                choice.choiceIndex)
                                            [start ..]
                                            rows
                                    ]
  where
    count = length choice.choiceRows
    start =
        max 0 (min choice.choiceIndex (max 0 (count - 14)))
    rows = take 14 (drop start choice.choiceRows)

drawOnboardingChoice :: AppState -> ChoiceOverlay -> Widget Name
drawOnboardingChoice appState choice =
    Widget Greedy Greedy do
        context <- getContext
        render $
            withAttr Theme.baseAttr $
                padRight Max $
                    padBottom Max $
                        padLeft (Pad 3) $
                            vBox
                                [ onboardingRow context.availWidth sourceIndex
                                | sourceIndex <-
                                    onboardingVisibleRowIndices
                                        context.availHeight
                                        choice.choiceIndex
                                        (length choice.choiceRows)
                                ]
  where
    choiceStart = 8
    choiceEnd = choiceStart + length choice.choiceRows
    onboardingRow width sourceIndex
        | sourceIndex >= choiceStart
        , sourceIndex < choiceEnd =
            case drop (sourceIndex - choiceStart) choice.choiceRows of
                row : _ ->
                    onboardingChoiceRow
                        appState
                        width
                        choice.choiceIndex
                        (sourceIndex - choiceStart)
                        row
                [] -> emptyWidget
        | otherwise =
            vLimit 1 $
                case sourceIndex of
                    0 -> withAttr Theme.headingAttr (txt choice.choiceTitle)
                    2 -> txtWrap choice.choiceBody
                    3 ->
                        withAttr Theme.mutedAttr $
                            txt "Choose a sign-in option below, or add your own API key."
                    5 ->
                        withAttr Theme.mutedAttr $
                            txt "You can change this anytime with /login."
                    7 -> withAttr Theme.strongAttr (txt "Get started")
                    12 ->
                        withAttr Theme.mutedAttr $
                            txt "Credentials are stored locally on this computer."
                    15 ->
                        withAttr Theme.mutedAttr $
                            txt "Esc to exit · Explore all commands with /help"
                    _ -> txt " "

onboardingChoiceRow
    :: AppState
    -> Int
    -> Int
    -> Int
    -> (Text, Text)
    -> Widget Name
onboardingChoiceRow appState width selected index (label, detail) =
    clickable name interactive
  where
    prefix = if selected == index then "› " else "  "
    name = ChoiceRow index
    showDetail = width >= 72 && not (Text.null detail)
    row =
        vLimit 1 $
            if showDetail
                then hBox
                    [ hLimit 36 $
                        padRight Max (txt (prefix <> label))
                    , withAttr Theme.mutedAttr (txt detail)
                    ]
                else txt (prefix <> label)
    styled =
        if selected == index
            then withAttr Theme.selectedAttr row
            else row
    interactive = case Composer.controlInteractionAttr appState name of
        Nothing -> styled
        Just attr -> forceAttr attr row

-- | Project the 18-row onboarding surface into a short terminal while keeping
-- the selected action and all setup paths visible before explanatory copy.
onboardingVisibleRowIndices :: Int -> Int -> Int -> [Int]
onboardingVisibleRowIndices availableHeight selected choiceCount
    | availableHeight <= 0 = []
    | availableHeight >= onboardingRowCount =
        [0 .. onboardingRowCount - 1]
    | otherwise =
        sort $
            take availableHeight $
                nub $
                    [ choiceStart + clampedSelected
                    , choiceStart + max 0 (choiceCount - 1)
                    ]
                        <> [choiceStart .. choiceStart + choiceCount - 1]
                        <> [7, 12, 15, 5, 0, 2, 3, 1, 4, 6, 13, 14, 16, 17]
  where
    onboardingRowCount = 18
    choiceStart = 8
    clampedSelected =
        max 0 (min (max 0 (choiceCount - 1)) selected)

waitingOverlayLabel :: AppState -> Text -> Widget Name
waitingOverlayLabel state label =
    hBox
        [ txt " "
        , withAttr (waitingIndicatorAttr state) $
            txt
                (waitingIndicator
                    motionGlyphSet
                    state.appRuntime.runtimeMotionMode
                    state.appMotionElapsedMillis)
        , txt (" " <> label <> " ")
        ]

drawTextPrompt :: AppState -> TextOverlay -> Widget Name
drawTextPrompt state prompt =
    centerLayer $
        hLimitPercent 82 $
            vLimitPercent 78 $
                overrideAttr Border.borderAttr Theme.borderAttr $
                    withBorderStyle unicodeRounded $
                        borderWithLabel
                            (waitingOverlayLabel state prompt.textTitle) $
                            padAll 1 $
                                vBox
                                    [ if Text.null (Text.strip prompt.textBody)
                                        then emptyWidget
                                        else padBottom (Pad 1) $
                                            vLimitPercent 60 $
                                                viewport OverlayViewport Vertical $
                                                    markdownWidget prompt.textBody
                                    , overrideAttr Border.borderAttr Theme.borderActiveAttr $
                                        withBorderStyle unicodeRounded $
                                            borderWithLabel (txt " Answer ") $
                                                padLeftRight 1 $
                                                    hBox
                                                        [ renderTextDraft prompt
                                                        , vLimit 1 (fill ' ')
                                                        ]
                                    ]

renderTextDraft :: TextOverlay -> Widget Name
renderTextDraft prompt =
    let displayDraft = textOverlayDisplayText prompt
        content =
            if Text.null displayDraft
                then withAttr Theme.mutedAttr (txt " ")
                else txt displayDraft
        (row, column) =
            Composer.draftCursorLocation displayDraft prompt.textCursor
    in showCursor OverlayCursor (Location (column, row)) content

-- | Replace every code point with one fixed-width masking glyph.
maskedSecretText :: Text -> Text
maskedSecretText value =
    Text.replicate (Text.length value) "•"

-- | Text that may be painted for an overlay draft.
textOverlayDisplayText :: TextOverlay -> Text
textOverlayDisplayText prompt = case prompt.textInputMode of
    TextInputPlain -> prompt.textDraft
    TextInputSecret -> maskedSecretText prompt.textDraft

-- | Secret prompts are deliberately single-line. Plain overlays preserve
-- multiline input, while secret pastes stop before the first line ending.
normalizeTextOverlayInsertion :: TextInputMode -> Text -> Text
normalizeTextOverlayInsertion = \case
    TextInputPlain -> id
    TextInputSecret -> Text.takeWhile \character ->
        character /= '\n' && character /= '\r'

choiceRow :: AppState -> Int -> Int -> (Text, Text) -> Widget Name
choiceRow appState selected index (label, detail) =
    let prefix = if selected == index then "› " else "  "
        name = ChoiceRow index
        row =
            hBox
                [ txt (prefix <> label)
                , vLimit 1 (fill ' ')
                , withAttr Theme.mutedAttr (txt detail)
                ]
        styled =
            if selected == index
                then withAttr Theme.selectedAttr row
                else row
        interactive = case Composer.controlInteractionAttr appState name of
            Nothing -> styled
            Just attr -> forceAttr attr row
    in clickable name interactive

handleUiEvents :: NonEmpty UiEvent -> EventM Name AppState ()
handleUiEvents uiEvents = do
    initial <- get
    renderedContentHeight <-
        if any isSubmittedPrompt uiEvents
            then conversationUnpaddedContentHeight
            else pure 0
    let
        (final, nativeProgress, shouldFollow, shouldInvalidate) =
            foldl'
                (applyOne renderedContentHeight)
                (initial, Nothing, False, False)
                uiEvents
    put final
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
        renderedContentHeight
        (state, previousProgress, followed, invalidated)
        uiEvent =
            let
                next =
                    applyUiEvent uiEvent $
                        applyConversationUiEvent
                            renderedContentHeight
                            uiEvent
                            state
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
                }
        UiConversationCleared ->
            state
                { appConversationAnchor = Nothing
                , appConversationReflowQueued = False
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

uiEventRestartsMotionSchedule
    :: UiEvent
    -> UiState
    -> UiState
    -> Map.Map BlockId Int
    -> Bool
uiEventRestartsMotionSchedule event previous next newFlashes =
    explicitReset
        || (previous.uiCompletionRemainingMillis == 0
            && next.uiCompletionRemainingMillis > 0)
        || not (Map.null newFlashes)
  where
    explicitReset = case event of
        UiLoop TurnStarted -> True
        UiLoop (WarningRaised _) -> True
        UiLoop (ResponseRestarted _) -> True
        UiSetNotice (Just _) -> True
        UiInputPromoted _ -> True
        UiTurnRestarted -> True
        _ -> False

nextMotionSchedule
    :: Bool
    -> MotionDemand
    -> Int
    -> (MotionDemand, Int, Int)
    -> (MotionDemand, Int, Int)
nextMotionSchedule
    resetSchedule
    demand
    delayMicros
    current@(currentDemand, currentDelay, generation)
    | resetSchedule
        || currentDemand /= demand
        || currentDelay /= delayMicros =
        (demand, delayMicros, generation + 1)
    | otherwise =
        current

elapsedMillisSince :: Word64 -> Word64 -> (Int, Word64)
elapsedMillisSince previous now
    | now <= previous =
        (0, previous)
    | otherwise =
        let elapsedMillis = (now - previous) `div` 1000000
        in
        ( fromIntegral elapsedMillis
        , previous + elapsedMillis * 1000000
        )

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

nativeProgressKeepaliveDue :: Bool -> Int -> UiState -> Bool
nativeProgressKeepaliveDue blocked previousBucket ui =
    not blocked
        && ui.uiRunning
        && ui.uiElapsedMillis `div` 5000 > previousBucket

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
    when (isMotionTick event) refreshNativeProgressKeepalive
    handleEventInner event
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
  where
    isMotionTick = \case
        AppEvent AppMotionTick -> True
        _ -> False

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
    AppEvent AppStop -> do
        modify' \state -> state { appWorkerStopped = True }
        halt
    AppEvent (AppSetSkillCommands skills) -> do
        state <- get
        if state.appSkillCommands == skills
            then pure ()
            else modify' \current -> current
                { appSkillCommands = skills
                , appSlashIndex = 0
                , appSlashDismissed = False
                }
    AppEvent (AppSetModelIds modelIds) -> do
        state <- get
        if state.appModelIds == modelIds
            then pure ()
            else modify' \current -> current
                { appModelIds = modelIds
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
    AppEvent (AppSetWindowTitle title) -> do
        state <- get
        liftIO (state.appRuntime.runtimeSetWindowTitle title)
        modify' \current -> current { appWindowTitle = Just title }
    AppEvent (AppSyntaxHighlighterLoaded highlighter) ->
        case highlighter of
            Nothing -> pure ()
            Just loaded -> do
                modify' \current ->
                    current { appSyntaxHighlighter = Just loaded }
                invalidateCache
    AppEvent (AppAgentSnapshot selected entries) -> do
        state <- get
        let normalized =
                Bridge.normalizeAgentSelection selected entries
        if state.appAgentSelected == normalized
            && state.appAgentEntries == entries
            then pure ()
            else do
                modify' \current ->
                    current
                        { appAgentSelected = normalized
                        , appAgentEntries = entries
                        , appAgentHover =
                            if normalized /= current.appAgentSelected
                                || length entries <= 1
                                || agentLayoutTargets entries
                                    /= agentLayoutTargets
                                        current.appAgentEntries
                                then Nothing
                                else
                                    current.appAgentHover >>= \hover ->
                                        if any
                                            ((== hover.agentHoverTarget)
                                                . (.agentTarget))
                                            entries
                                            then Just hover
                                            else Nothing
                        }
                when
                    ((length state.appAgentEntries > 1)
                        /= (length entries > 1))
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
    AppEvent (AppAskResume browser loadEntry deleteEntry reply) -> do
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
            pure state { appAgentHover = Nothing }
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
                            (CodeCopy blockId codeIndex, V.BLeft) ->
                                Composer.handleControlMouseDown
                                    (CodeCopy blockId codeIndex)
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
                                liftIO
                                    (state.appRuntime.runtimeAgentSelect target)
                                modify' \current ->
                                    current { appAgentSelected = target }
                            (AgentPopover target, V.BLeft) -> do
                                keepAgentHover target
                                liftIO
                                    (state.appRuntime.runtimeAgentSelect target)
                                modify' \current ->
                                    current { appAgentSelected = target }
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
    VtyEvent V.EvResize{} -> do
        clearAgentHover
        invalidateCache
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
    1 -> PermissionAllowTool
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
handleNormalKey event
    | Bridge.isSendNowKey event =
        Composer.handleComposerKey
            applyLocalUiEventWith
            handleCtrlC
            scrollConversationPage
            event
    | otherwise = do
        case event of
            V.EvMouseDown _ _ V.BScrollUp _ ->
                scrollConversationBy (-mouseScrollLines)
            V.EvMouseDown _ _ V.BScrollDown _ ->
                scrollConversationBy mouseScrollLines
            _ -> do
                state <- get
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

isAgentHoverSurface :: Name -> Bool
isAgentHoverSurface = \case
    AgentPane -> True
    AgentRow _ -> True
    AgentPopover _ -> True
    _ -> False

agentLayoutTargets :: [AgentEntry] -> [AgentTarget]
agentLayoutTargets =
    map (.agentTarget) . sortOn (.agentPath)

handleMouseDown :: Name -> V.Button -> EventM Name AppState ()
handleMouseDown name button =
    case button of
        V.BScrollUp -> scrollConversationBy (-mouseScrollLines)
        V.BScrollDown -> scrollConversationBy mouseScrollLines
        V.BLeft -> case name of
            ConversationBlock ident ->
                applyLocalUiEventWith
                    (UiActivateBlock ident)
                    (applyUiEvent (UiFocusChanged FocusScrollback))
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
        vScrollToBeginning scroll
        queueConversationReflow
    V.EvKey V.KEnd [] -> resumeFollow
    V.EvKey (V.KChar 'g') [] -> do
        leaveFollow
        vScrollToBeginning scroll
        queueConversationReflow
    V.EvKey (V.KChar 'G') [] -> resumeFollow
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
        applyLocalUiEvent (UiMoveSelection delta)
        state <- get
        case state.appUi.uiSelectedBlock of
            Just ident -> do
                makeVisible (ConversationBlock ident)
                queueConversationReflow
            Nothing -> pure ()
    toggle = do
        applyLocalUiEvent UiToggleSelected
        queueConversationReflow
    focusComposer =
        applyLocalUiEvent (UiFocusChanged FocusComposer)
    leaveFollow =
        applyLocalUiEvent (UiSetFollow False)
    resumeFollow = do
        applyLocalUiEventWith (UiSetFollow True) \state ->
            state
                { appConversationAnchor =
                    Scroll.followConversationTail
                        <$> state.appConversationAnchor
                }
        vScrollToEnd scroll
        queueConversationReflow
    copySelected = do
        state <- get
        case state.appUi.uiSelectedBlock >>= selectedBlock state.appUi of
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
scrollConversationBy amount
    | amount == 0 = pure ()
    | amount < 0 = do
        setConversationFollow False
        vScrollBy scroll amount
        queueConversationReflow
    | otherwise =
        lookupViewport ConversationViewport >>= \case
            Just (VP _ top (_, height) (_, contentHeight))
                | top + height + amount >= contentHeight -> do
                    setConversationFollow True
                    vScrollToEnd scroll
                    queueConversationReflow
            _ -> do
                setConversationFollow False
                vScrollBy scroll amount
                queueConversationReflow
  where
    scroll = viewportScroll ConversationViewport

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
selectedBlock state ident =
    find ((== ident) . (.blockId)) (toList state.uiBlocks)
