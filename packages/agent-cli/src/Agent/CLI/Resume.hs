-- | Session resume picker for @/resume@.
module Agent.CLI.Resume
    ( ResumeEntry(..)
    , ResumeBrowser(..)
    , ResumeSourceFilter(..)
    , ResumeState(..)
    , applyResumeKey
    , beginResumeSearch
    , cycleResumeSource
    , endResumeSearch
    , formatResumeListing
    , groupResumeEntries
    , initialResumeBrowser
    , initialResumeState
    , insertResumeSearch
    , loadResumeEntry
    , moveResumeBrowser
    , pickResumeSession
    , removeResumeEntry
    , renderResumeFrame
    , renderResumeFrameFor
    , replaceResumeEntry
    , resumeEntryFromMeta
    , resumeEntriesFrom
    , resumeRelativeAge
    , resumeSourceLabel
    , selectedResumeBrowser
    , setResumeDeletePending
    , setResumeNotice
    , toggleResumeExpanded
    , visibleResumeBrowser
    , visibleResume
    ) where

import Agent.CLI.Picker (PickerKey(..), runOverlay)
import Agent.CLI.Session
    ( SessionMeta(..)
    , SessionTurn(..)
    , loadSession
    )
import Agent.CLI.Style (roleMuted, rolePrompt, roleSuccess)
import Agent.CLI.TextLayout
    ( SplitPaneFrame(..)
    , clampSelectionIndex
    , renderSplitPaneFrame
    )
import Agent.OsPath (toText)
import Agent.Provider (providerSlug)
import Agent.Responses.Types (ResponseItem(..))
import Control.Monad (forM)
import Data.Char (isAlphaNum)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time.Clock (UTCTime, diffUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.Console.ANSI (getTerminalSize)
import System.OsPath (OsPath, takeDirectory, takeFileName)
import System.IO (hFlush, hIsTerminalDevice, stderr, stdin)

data ResumeSourceFilter
    = ResumeAll
    | ResumeProvider !Text
    deriving (Eq, Show)

data ResumeEntry = ResumeEntry
    { resumeId :: !Text
    , resumeTitle :: !Text
    , resumeModel :: !Text
    , resumeCwd :: !Text
    , resumeProject :: !Text
    , resumeWhen :: !Text
    , resumeProvider :: !Text
    , resumeCreatedAt :: !UTCTime
    , resumeUpdatedAt :: !UTCTime
    , resumeMessageCount :: !Int
    , resumeTurnCount :: !Int
    , resumeToolCount :: !Int
    , resumePrompt :: !Text
    , resumeLoaded :: !Bool
    , resumeTranscript :: ![Text]
    }
    deriving (Eq, Show)

data ResumeBrowser = ResumeBrowser
    { resumeBrowserAll :: ![ResumeEntry]
    , resumeBrowserQuery :: !Text
    , resumeBrowserSearching :: !Bool
    , resumeBrowserIndex :: !Int
    , resumeBrowserSource :: !ResumeSourceFilter
    , resumeBrowserExpanded :: !(Maybe Text)
    , resumeBrowserDeletePending :: !(Maybe Text)
    , resumeBrowserNotice :: !(Maybe Text)
    , resumeBrowserNow :: !UTCTime
    }
    deriving (Eq, Show)

data ResumeState = ResumeState
    { resumeAll :: ![ResumeEntry]
    , resumeFilter :: !Text
    , resumeIndex :: !Int
    }
    deriving (Eq, Show)

-- | Build picker entries from already loaded sessions.
resumeEntriesFrom :: [(SessionMeta, [SessionTurn])] -> [ResumeEntry]
resumeEntriesFrom = map (uncurry entryFrom)

resumeEntryFromMeta :: SessionMeta -> ResumeEntry
resumeEntryFromMeta meta = entryFromWith False meta []

loadResumeEntry :: OsPath -> Text -> IO (Either Text ResumeEntry)
loadResumeEntry root sessionId =
    fmap (fmap (uncurry entryFrom)) (loadSession root sessionId)

entryFrom :: SessionMeta -> [SessionTurn] -> ResumeEntry
entryFrom = entryFromWith True

entryFromWith :: Bool -> SessionMeta -> [SessionTurn] -> ResumeEntry
entryFromWith loaded meta turns =
    ResumeEntry
        { resumeId = meta.metaId
        , resumeTitle =
            if Text.null meta.metaTitle then "(untitled)" else meta.metaTitle
        , resumeModel = meta.metaModel
        , resumeCwd = toText meta.metaCwd
        , resumeProject = projectLabel meta.metaCwd
        , resumeWhen =
            Text.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M" meta.metaUpdatedAt)
        , resumeProvider = providerSlug meta.metaProvider
        , resumeCreatedAt = meta.metaCreatedAt
        , resumeUpdatedAt = meta.metaUpdatedAt
        , resumeMessageCount = messageCount turns
        , resumeTurnCount = length turns
        , resumeToolCount =
            length
                [ ()
                | turn <- turns
                , item <- turn.turnItems
                , isToolCall item
                ]
        , resumePrompt =
            fromMaybe ""
                (firstNonEmpty (map (.turnUserText) turns))
        , resumeLoaded = loaded
        , resumeTranscript = transcriptLines turns
        }
  where
    messageCount =
        sum . map \turn ->
            fromEnum (not (Text.null (Text.strip turn.turnUserText)))
                + fromEnum
                    (maybe False (not . Text.null . Text.strip) turn.turnAssistantText)

    firstNonEmpty = \case
        [] -> Nothing
        text : rest
            | Text.null (Text.strip text) -> firstNonEmpty rest
            | otherwise -> Just (Text.strip text)

    isToolCall = \case
        FunctionCallItem{} -> True
        CustomToolCallItem{} -> True
        _ -> False

projectLabel :: OsPath -> Text
projectLabel cwd =
    case filter (not . Text.null) [parent, name] of
        [] -> toText cwd
        [only] -> only
        parts -> Text.intercalate "-" parts
  where
    name = toText (takeFileName cwd)
    parent = toText (takeFileName (takeDirectory cwd))

initialResumeBrowser :: UTCTime -> [ResumeEntry] -> ResumeBrowser
initialResumeBrowser now entries =
    ResumeBrowser
        { resumeBrowserAll = entries
        , resumeBrowserQuery = ""
        , resumeBrowserSearching = False
        , resumeBrowserIndex = 0
        , resumeBrowserSource = ResumeAll
        , resumeBrowserExpanded = Nothing
        , resumeBrowserDeletePending = Nothing
        , resumeBrowserNotice = Nothing
        , resumeBrowserNow = now
        }

visibleResumeBrowser :: ResumeBrowser -> [ResumeEntry]
visibleResumeBrowser browser =
    filter matchesQuery (sourceEntries browser)
  where
    needle = Text.toLower (Text.strip browser.resumeBrowserQuery)
    matchesQuery entry
        | Text.null needle = True
        | otherwise =
            any
                (Text.isInfixOf needle . Text.toLower)
                [ entry.resumeTitle
                , entry.resumeId
                , entry.resumeModel
                , entry.resumeCwd
                , entry.resumeProvider
                , entry.resumePrompt
                ]

sourceEntries :: ResumeBrowser -> [ResumeEntry]
sourceEntries browser = case browser.resumeBrowserSource of
    ResumeAll -> browser.resumeBrowserAll
    ResumeProvider provider ->
        filter ((== provider) . (.resumeProvider)) browser.resumeBrowserAll

selectedResumeBrowser :: ResumeBrowser -> Maybe ResumeEntry
selectedResumeBrowser browser =
    case visibleResumeBrowser browser of
        [] -> Nothing
        entries ->
            Just
                (entries
                    !! clampSelectionIndex
                        (length entries)
                        browser.resumeBrowserIndex)

moveResumeBrowser :: Int -> ResumeBrowser -> ResumeBrowser
moveResumeBrowser delta browser =
    let count = length (visibleResumeBrowser browser)
    in clearTransient $
        if count == 0
            then browser { resumeBrowserIndex = 0 }
            else
                browser
                    { resumeBrowserIndex =
                        (clampSelectionIndex count browser.resumeBrowserIndex + delta)
                            `mod` count
                    }

beginResumeSearch :: ResumeBrowser -> ResumeBrowser
beginResumeSearch browser =
    clearTransient browser { resumeBrowserSearching = True }

endResumeSearch :: ResumeBrowser -> ResumeBrowser
endResumeSearch browser =
    clearTransient browser { resumeBrowserSearching = False }

insertResumeSearch :: Text -> ResumeBrowser -> ResumeBrowser
insertResumeSearch text browser =
    clampBrowser $
        clearTransient browser
            { resumeBrowserQuery = browser.resumeBrowserQuery <> text
            , resumeBrowserSearching = True
            , resumeBrowserIndex = 0
            }

cycleResumeSource :: ResumeBrowser -> ResumeBrowser
cycleResumeSource browser =
    clampBrowser $
        clearTransient browser
            { resumeBrowserSource =
                case browser.resumeBrowserSource of
                    ResumeAll ->
                        maybe ResumeAll ResumeProvider (first providers)
                    ResumeProvider current ->
                        case dropWhile (/= current) providers of
                            _ : next : _ -> ResumeProvider next
                            _ -> ResumeAll
            , resumeBrowserIndex = 0
            }
  where
    providers = nub (map (.resumeProvider) browser.resumeBrowserAll)
    first = \case
        [] -> Nothing
        value : _ -> Just value

toggleResumeExpanded :: ResumeBrowser -> ResumeBrowser
toggleResumeExpanded browser =
    clearTransient browser
        { resumeBrowserExpanded =
            case selectedResumeBrowser browser of
                Nothing -> Nothing
                Just entry
                    | browser.resumeBrowserExpanded == Just entry.resumeId ->
                        Nothing
                    | otherwise -> Just entry.resumeId
        }

setResumeDeletePending :: Maybe Text -> ResumeBrowser -> ResumeBrowser
setResumeDeletePending sessionId browser =
    browser
        { resumeBrowserDeletePending = sessionId
        , resumeBrowserNotice = Nothing
        }

setResumeNotice :: Maybe Text -> ResumeBrowser -> ResumeBrowser
setResumeNotice notice browser =
    browser
        { resumeBrowserDeletePending = Nothing
        , resumeBrowserNotice = notice
        }

replaceResumeEntry :: ResumeEntry -> ResumeBrowser -> ResumeBrowser
replaceResumeEntry replacement browser =
    browser
        { resumeBrowserAll =
            map
                (\entry ->
                    if entry.resumeId == replacement.resumeId
                        then replacement
                        else entry)
                browser.resumeBrowserAll
        }

removeResumeEntry :: Text -> ResumeBrowser -> ResumeBrowser
removeResumeEntry sessionId browser =
    clampBrowser
        browser
            { resumeBrowserAll =
                filter ((/= sessionId) . (.resumeId)) browser.resumeBrowserAll
            , resumeBrowserExpanded =
                if browser.resumeBrowserExpanded == Just sessionId
                    then Nothing
                    else browser.resumeBrowserExpanded
            , resumeBrowserDeletePending = Nothing
            , resumeBrowserNotice = Nothing
            }

resumeSourceLabel :: ResumeSourceFilter -> Text
resumeSourceLabel = \case
    ResumeAll -> "All"
    ResumeProvider "openai" -> "OpenAI"
    ResumeProvider "xai" -> "xAI"
    ResumeProvider "openrouter" -> "OpenRouter"
    ResumeProvider "claude-code" -> "Claude Code"
    ResumeProvider provider -> provider

groupResumeEntries :: [ResumeEntry] -> [(Text, [ResumeEntry])]
groupResumeEntries =
    foldl' addGroup []
  where
    addGroup groups entry =
        case break ((== entry.resumeProject) . fst) groups of
            (_, []) ->
                groups <> [(entry.resumeProject, [entry])]
            (before, (project, entries) : after) ->
                before <> ((project, entries <> [entry]) : after)

resumeRelativeAge :: UTCTime -> UTCTime -> Text
resumeRelativeAge now updated =
    let seconds = max 0 (floor (diffUTCTime now updated) :: Int)
        amount unit divisor = Text.pack (show (seconds `div` divisor)) <> unit <> " ago"
    in if seconds < 60
        then "now"
        else if seconds < 60 * 60
            then amount "m" 60
            else if seconds < 24 * 60 * 60
                then amount "h" (60 * 60)
                else if seconds < 7 * 24 * 60 * 60
                    then amount "d" (24 * 60 * 60)
                    else if seconds < 35 * 24 * 60 * 60
                        then amount "w" (7 * 24 * 60 * 60)
                        else Text.pack
                            (formatTime defaultTimeLocale "%Y-%m-%d" updated)

clearTransient :: ResumeBrowser -> ResumeBrowser
clearTransient browser =
    browser
        { resumeBrowserDeletePending = Nothing
        , resumeBrowserNotice = Nothing
        }

clampBrowser :: ResumeBrowser -> ResumeBrowser
clampBrowser browser =
    browser
        { resumeBrowserIndex =
            clampSelectionIndex
                (length (visibleResumeBrowser browser))
                browser.resumeBrowserIndex
        }

initialResumeState :: [ResumeEntry] -> ResumeState
initialResumeState entries =
    ResumeState { resumeAll = entries, resumeFilter = "", resumeIndex = 0 }

visibleResume :: ResumeState -> [ResumeEntry]
visibleResume state
    | Text.null needle = state.resumeAll
    | otherwise =
        filter
            (\e ->
                needle `Text.isInfixOf` Text.toLower e.resumeTitle
                    || needle `Text.isInfixOf` Text.toLower e.resumeId
                    || needle `Text.isInfixOf` Text.toLower e.resumeModel)
            state.resumeAll
  where
    needle = Text.toLower state.resumeFilter

selectedResume :: ResumeState -> Maybe ResumeEntry
selectedResume state =
    case visibleResume state of
        [] -> Nothing
        opts ->
            let i = clampSelectionIndex (length opts) state.resumeIndex
            in Just (opts !! i)

applyResumeKey :: PickerKey -> ResumeState -> Either (Maybe ResumeEntry) ResumeState
applyResumeKey key state = case key of
    PickerKeyCancel -> Left Nothing
    PickerKeyConfirm -> Left (selectedResume state)
    PickerKeyUp -> Right (move (-1) state)
    PickerKeyDown -> Right (move 1 state)
    PickerKeyBackspace ->
        Right $ clampSel state
            { resumeFilter = Text.dropEnd 1 state.resumeFilter
            , resumeIndex = 0
            }
    PickerKeyChar c
        | isFilterChar c ->
            Right $ clampSel state
                { resumeFilter = state.resumeFilter <> Text.singleton c
                , resumeIndex = 0
                }
        | otherwise -> Right state

move :: Int -> ResumeState -> ResumeState
move delta state =
    let n = length (visibleResume state)
    in if n == 0
        then state { resumeIndex = 0 }
        else
            state
                { resumeIndex =
                    (clampSelectionIndex n state.resumeIndex + delta) `mod` n
                }

clampSel :: ResumeState -> ResumeState
clampSel state =
    state
        { resumeIndex =
            clampSelectionIndex
                (length (visibleResume state))
                state.resumeIndex
        }

isFilterChar :: Char -> Bool
isFilterChar c =
    isAlphaNum c || c `elem` ("-_/." :: String)

-- | Stable default size for tests and non-interactive callers.
renderResumeFrame :: Bool -> ResumeState -> Text
renderResumeFrame color = renderResumeFrameFor color 24 100

-- | Render a fixed-height, two-column picker. The left column follows the
-- selection through the recent session titles; the right column shows the
-- tail of that selected session's transcript.
renderResumeFrameFor :: Bool -> Int -> Int -> ResumeState -> Text
renderResumeFrameFor color terminalRows terminalCols state =
    renderSplitPaneFrame SplitPaneFrame
        { splitPaneMinColumns = 12
        , splitPaneColumns = terminalCols
        -- Leave the final terminal row unused. Redrawing a frame that exactly
        -- fills the viewport would scroll its first line and duplicate the header.
        , splitPaneBodyRows = max 1 (terminalRows - 4)
        , splitPaneLeftMinWidth = 8
        , splitPaneLeftMaxWidth = 34
        , splitPaneDivider = " │ "
        , splitPaneTitle = "resume"
        , splitPaneHeaderDetail = const filterText
        , splitPaneLeftHeading = "sessions"
        , splitPaneRightHeading =
            \selected ->
                "transcript"
                    <> maybe "" (\entry -> " · " <> entry.resumeTitle) selected
        , splitPaneItems = visibleResume state
        , splitPaneSelectedIndex = state.resumeIndex
        , splitPaneLeftLabel = \_ entry -> entry.resumeTitle
        , splitPaneTranscript = (.resumeTranscript)
        , splitPaneEmptyTranscript = "(no sessions)"
        , splitPaneFooter =
            "↑↓/jk or scroll · click/enter resume · esc/q cancel · type to filter"
        , splitPanePromptStyle = rolePrompt color
        , splitPaneMutedStyle = roleMuted color
        , splitPaneSelectedStyle = roleSuccess color
        }
  where
    filterText
        | Text.null state.resumeFilter = "type to filter"
        | otherwise = "filter: " <> state.resumeFilter

transcriptLines :: [SessionTurn] -> [Text]
transcriptLines = concatMap turnLines
  where
    turnLines turn =
        labelled "user: " turn.turnUserText
            <> maybe [] (labelled "assistant: ") turn.turnAssistantText
            <> [""]

    labelled prefix raw =
        case Text.lines (Text.strip raw) of
            [] -> []
            firstLine : rest ->
                (prefix <> firstLine)
                    : map (Text.replicate (Text.length prefix) " " <>) rest

formatResumeListing :: Bool -> [ResumeEntry] -> Text
formatResumeListing color entries =
    if null entries
        then roleMuted color "no sessions in ~/.haskell-agent/sessions"
        else Text.intercalate "\n" (map (formatOne color) entries)

formatOne :: Bool -> ResumeEntry -> Text
formatOne color entry =
    roleMuted color (Text.take 8 entry.resumeId)
        <> "  "
        <> entry.resumeTitle
        <> roleMuted color ("  " <> entry.resumeModel)

-- | TTY picker; non-TTY prints the list. Confirm returns the session id.
-- Loading is capped to the latest sessions so opening the picker stays cheap.
pickResumeSession :: Bool -> OsPath -> [SessionMeta] -> IO (Maybe Text)
pickResumeSession color root metas = do
    isTty <- hIsTerminalDevice stdin
    loaded <- loadRecentSessions root metas
    let entries = resumeEntriesFrom loaded
    if not isTty
        then do
            Text.hPutStrLn stderr (formatResumeListing color entries)
            hFlush stderr
            pure Nothing
        else do
            size <- getTerminalSize
            let (rows, cols) = maybe (24, 100) id size
            result <-
                runOverlay
                    (renderResumeFrameFor color rows cols)
                    applyResumeKey
                    (initialResumeState entries)
            pure $ case result of
                Just (Just entry) -> Just entry.resumeId
                _ -> Nothing

loadRecentSessions :: OsPath -> [SessionMeta] -> IO [(SessionMeta, [SessionTurn])]
loadRecentSessions root metas =
    forM (take 20 metas) \meta ->
        loadSession root meta.metaId >>= \case
            Right loaded -> pure loaded
            Left err ->
                pure
                    ( meta
                    , [ SessionTurn
                            { turnAt = meta.metaUpdatedAt
                            , turnUserText = ""
                            , turnAssistantText =
                                Just ("Transcript unavailable: " <> err)
                            , turnError = Nothing
                            , turnResponseId = Nothing
                            , turnItems = []
                            , turnUsage = Nothing
                            }
                      ]
                    )
