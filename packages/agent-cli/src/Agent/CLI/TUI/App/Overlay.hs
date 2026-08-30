{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -O0 -Wno-unused-imports #-}
module Agent.CLI.TUI.App.Overlay where

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
import Agent.CLI.Permission (PermissionChoice(..))
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
    , kittyKeyboardDisambiguatePush
    , kittyKeyboardPop
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
import Agent.TUI.Presentation ( permissionToolCallPromptRelative )
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
import Agent.CLI.Recap ( autoRecapAwayThreshold , autoRecapIdleThreshold , autoRecapRetryInterval )
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (gets, modify')
import Control.Exception.Safe (finally, mask, onException, throwIO, tryAny)
import Control.Exception (AsyncException(UserInterrupt))
import Data.Char (isControl, isPrint, isSpace)
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
import System.Exit (ExitCode(ExitSuccess))
import System.Info (os)
import System.IO (stdout)
import System.Posix.Process (getProcessID)
import System.Process
    ( StdStream(NoStream)
    , close_fds
    , createProcess
    , create_group
    , getProcessExitCode
    , new_session
    , proc
    , std_err
    , std_in
    , std_out
    )
import Agent.CLI.TUI.App.Runtime
import Agent.CLI.TUI.App.Mailbox
import Agent.CLI.TUI.App.Reduce
import Agent.CLI.TUI.App.Navigation
    ( mouseScrollLines, historyBlock, selectedBlock )

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
handleChoiceKey event = do
    state <- get
    case state.appChoice of
        Just choice
            | choice.choiceSearch -> handleFilterChoiceKey event
        _ -> handleStaticChoiceKey event

handleStaticChoiceKey :: V.Event -> EventM Name AppState ()
handleStaticChoiceKey = \case
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

handleFilterChoiceKey :: V.Event -> EventM Name AppState ()
handleFilterChoiceKey event = case event of
    V.EvKey V.KUp [] -> moveFilteredChoice (-1)
    V.EvKey V.KDown [] -> moveFilteredChoice 1
    V.EvKey V.KBackTab [] -> moveFilteredChoice (-1)
    V.EvKey (V.KChar '\t') [] -> moveFilteredChoice 1
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
    V.EvKey V.KBS [] -> updateChoiceQuery (Text.dropEnd 1)
    V.EvKey (V.KChar char) []
        | isPrint char ->
            updateChoiceQuery (`Text.snoc` char)
    V.EvPaste bytes ->
        updateChoiceQuery
            (<> Text.filter isPrint (Composer.decodePaste bytes))
    V.EvKey (V.KChar 'c') modifiers
        | V.MCtrl `elem` modifiers -> do
            state <- get
            _ <- handleCtrlC
            when state.appUi.uiRunning (resolveChoice False)
    _ -> pure ()
  where
    moveFilteredChoice delta = do
        modify' \state ->
            state
                { appChoice =
                    (\choice ->
                        let count = length (choiceVisibleRows choice)
                        in choice
                            { choiceIndex =
                                if count == 0
                                    then 0
                                    else
                                        (choice.choiceIndex + delta)
                                            `mod` count
                            })
                        <$> state.appChoice
                }

    updateChoiceQuery update = do
        modify' \state ->
            state
                { appChoice =
                    (\choice ->
                        choice
                            { choiceQuery = update choice.choiceQuery
                            , choiceIndex = 0
                            })
                        <$> state.appChoice
                }
        vScrollToBeginning (viewportScroll OverlayViewport)

confirmChoiceAt :: Int -> EventM Name AppState ()
confirmChoiceAt index = do
    state <- get
    case state.appChoice of
        Just choice
            | choice.choiceSearch ->
                case findIndex
                        ((== index) . fst)
                        (choiceVisibleRows choice) of
                    Just visibleIndex -> do
                        modify' \current ->
                            current
                                { appChoice =
                                    (\overlay ->
                                        overlay
                                            { choiceIndex = visibleIndex })
                                        <$> current.appChoice
                                }
                        resolveChoice True
                    Nothing -> pure ()
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
    ComposerImageRemove index ->
        Composer.handlePromptControlClick
            applyLocalUiEventWith
            (\draft -> ReplRemovePendingImage draft index)
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
    ComposerImageRemove _ -> True
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
    unless opened $ do
        copyAction <- gets (.appRuntime.runtimeCopy)
        copied <- liftIO (copyAction url)
        modify' $
            applyUiEvent
                (UiSetNotice
                    (Just (warningNotice
                        (if copied
                            then "Could not open that link; URL copied."
                            else "Could not open that link."))))

openExternalUrl :: Text -> IO Bool
openExternalUrl url =
    case externalUrlCommand url of
        Nothing -> pure False
        Just command ->
            launchExternalUrlCommand command

-- | Start the platform URL opener without waiting for it to exit. Browser
-- launchers may remain attached for the lifetime of the browser, and waiting
-- for them would block the Brick event loop.
launchExternalUrlCommand :: (FilePath, [String]) -> IO Bool
launchExternalUrlCommand (command, arguments) = do
    result <- tryAny $ do
        (_, _, _, processHandle) <-
            createProcess
            (proc command arguments)
                { std_in = NoStream
                , std_out = NoStream
                , std_err = NoStream
                , close_fds = True
                , create_group = True
                , new_session = True
                }
        -- Catch launchers that fail immediately while still avoiding an
        -- unbounded wait on browser processes.
        threadDelay 100_000
        maybe True (== ExitSuccess)
            <$> getProcessExitCode processHandle
    case result of
        Right opened -> pure opened
        Left _ -> pure False

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
                    then state.appChoice >>= selectedChoiceIndex
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

isMetaConsoleToggle :: V.Event -> Bool
isMetaConsoleToggle = \case
    V.EvKey (V.KChar 'k') modifiers ->
        modifiers == [V.MMeta] || modifiers == [V.MAlt]
    _ -> False

openMetaConsole :: EventM Name AppState ()
openMetaConsole = do
    state <- get
    liftIO (state.appRuntime.runtimeNativeProgress False)
    modify' \current ->
        current
            { appMetaConsole =
                Just MetaConsoleOverlay
                    { metaConsoleDraft = ""
                    , metaConsoleCursor = 0
                    }
            , appAgentHover = Nothing
            , appHoveredControl = Nothing
            , appPressedControl = Nothing
            }

closeMetaConsole :: EventM Name AppState ()
closeMetaConsole = do
    modify' \state -> state { appMetaConsole = Nothing }
    resumeNativeProgressIfRunning

handleMetaConsoleKey :: V.Event -> EventM Name AppState ()
handleMetaConsoleKey event
    | isMetaConsoleToggle event =
        closeMetaConsole
    | otherwise = case event of
        V.EvKey V.KEsc [] ->
            closeMetaConsole
        V.EvKey V.KEnter [] ->
            submitMetaConsole
        _ ->
            modify' \state ->
                state
                    { appMetaConsole =
                        (\overlay ->
                            fromMaybe
                                overlay
                                (applyMetaConsoleEdit event overlay))
                            <$> state.appMetaConsole
                    }

-- | Reuse the prompt editor so Meta Console cursor movement and deletion stay
-- grapheme-safe without coupling its state to blocking text prompts.
applyMetaConsoleEdit
    :: V.Event
    -> MetaConsoleOverlay
    -> Maybe MetaConsoleOverlay
applyMetaConsoleEdit event overlay = do
    edited <-
        applyTextPromptEdit
            event
            TextOverlay
                { textTitle = ""
                , textBody = ""
                , textDraft = overlay.metaConsoleDraft
                , textCursor = overlay.metaConsoleCursor
                , textInputMode = TextInputPlain
                }
    pure MetaConsoleOverlay
        { metaConsoleDraft = edited.textDraft
        , metaConsoleCursor = edited.textCursor
        }

submitMetaConsole :: EventM Name AppState ()
submitMetaConsole = do
    state <- get
    case (Text.strip . (.metaConsoleDraft)) <$> state.appMetaConsole of
        Nothing ->
            pure ()
        Just request
            | Text.null request ->
                pure ()
            | otherwise -> do
                let queued = state.appUi.uiRunning
                result <- liftIO $ atomically $
                    Composer.appendFullscreenInput
                        state.appRuntime.runtimeInput
                        FullscreenInput
                            { fullscreenInputLine = ReplMeta request
                            , fullscreenInputQueued = queued
                            , fullscreenInputDisplay = Nothing
                            }
                case result of
                    Left message ->
                        applyLocalUiEvent $
                            UiSetNotice (Just (warningNotice message))
                    Right () -> do
                        if queued
                            then
                                applyLocalUiEvent $
                                    UiSetNotice $
                                        Just $
                                            progressNotice
                                                "Meta Console request queued for the next safe boundary."
                            else
                                applyLocalUiEvent (UiSetAwaitingInput False)
                        closeMetaConsole


handleCtrlC :: EventM Name AppState CtrlCDecision
handleCtrlC = do
    state <- get
    decision <- liftIO state.appRuntime.runtimeCtrlC
    case decision of
        SoftCancel ->
            applyLocalUiEvent $ UiSetNotice $
                Just $ warningNotice "Interrupted; press Ctrl-C again to exit."
        WarnExit ->
            applyLocalUiEvent $ UiSetNotice $
                Just $ warningNotice "Press Ctrl-C again to exit."
        ForceExit -> liftIO (throwIO UserInterrupt)
    pure decision
