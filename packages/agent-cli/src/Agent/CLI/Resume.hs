-- | Session resume picker for @/resume@.
module Agent.CLI.Resume
    ( ResumeEntry(..)
    , ResumeState(..)
    , applyResumeKey
    , formatResumeListing
    , initialResumeState
    , pickResumeSession
    , renderResumeFrame
    , renderResumeFrameFor
    , resumeEntriesFrom
    , visibleResume
    ) where

import Agent.CLI.Picker (PickerKey(..), runOverlay)
import Agent.CLI.Session
    ( SessionMeta(..)
    , SessionTurn(..)
    , loadSession
    )
import Agent.CLI.Style (roleMuted, rolePrompt, roleSuccess)
import Agent.OsPath (toText)
import Agent.Provider (providerSlug)
import Control.Monad (forM)
import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.Console.ANSI (getTerminalSize)
import System.OsPath (OsPath, takeFileName)
import System.IO (hFlush, hIsTerminalDevice, stderr, stdin)

data ResumeEntry = ResumeEntry
    { resumeId :: !Text
    , resumeTitle :: !Text
    , resumeModel :: !Text
    , resumeCwd :: !Text
    , resumeWhen :: !Text
    , resumeProvider :: !Text
    , resumeTranscript :: ![Text]
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

entryFrom :: SessionMeta -> [SessionTurn] -> ResumeEntry
entryFrom meta turns =
    ResumeEntry
        { resumeId = meta.metaId
        , resumeTitle =
            if Text.null meta.metaTitle then "(untitled)" else meta.metaTitle
        , resumeModel = meta.metaModel
        , resumeCwd = toText (takeFileName meta.metaCwd)
        , resumeWhen =
            Text.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M" meta.metaUpdatedAt)
        , resumeProvider = providerSlug meta.metaProvider
        , resumeTranscript = transcriptLines turns
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
            let i = clamp (length opts) state.resumeIndex
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
        else state { resumeIndex = (clamp n state.resumeIndex + delta) `mod` n }

clampSel :: ResumeState -> ResumeState
clampSel state =
    state { resumeIndex = clamp (length (visibleResume state)) state.resumeIndex }

clamp :: Int -> Int -> Int
clamp n i
    | n <= 0 = 0
    | i < 0 = 0
    | i >= n = n - 1
    | otherwise = i

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
    Text.intercalate "\n" (header : headings : body <> [footer])
  where
    cols = max 12 terminalCols
    -- Leave the final terminal row unused. Redrawing a frame that exactly
    -- fills the viewport would scroll its first line and duplicate the header.
    bodyRows = max 1 (terminalRows - 4)
    divider = roleMuted color " │ "
    leftWidth = max 8 (min 34 ((cols - 3) * 2 `div` 5))
    rightWidth = max 1 (cols - leftWidth - 3)
    visible = visibleResume state
    n = length visible
    idx = clamp n state.resumeIndex
    shown = sessionWindow bodyRows idx visible
    selected = selectedResume state
    filterText
        | Text.null state.resumeFilter = "type to filter"
        | otherwise = "filter: " <> state.resumeFilter
    header =
        rolePrompt color "resume"
            <> roleMuted color
                (fitCell (max 0 (cols - 6)) (" · " <> filterText))
    headings =
        rolePrompt color (fitCell leftWidth "sessions")
            <> divider
            <> rolePrompt color
                (fitCell rightWidth
                    ("transcript"
                        <> maybe "" (\entry -> " · " <> entry.resumeTitle) selected))
    leftRows =
        map
            (\(absoluteIndex, entry) ->
                let prefix = if absoluteIndex == idx then "› " else "  "
                    text = fitCell leftWidth (prefix <> entry.resumeTitle)
                in if absoluteIndex == idx
                    then roleSuccess color text
                    else text)
            shown
            <> repeat (Text.replicate leftWidth " ")
    rightRows = case selected of
        Nothing -> roleMuted color (fitCell rightWidth "(no sessions)") : repeat ""
        Just entry ->
            let preview = previewRows rightWidth bodyRows entry.resumeTranscript
            in map (fitCell rightWidth) preview <> repeat ""
    body =
        take bodyRows $
            zipWith
                (\left right -> left <> divider <> right)
                leftRows
                rightRows
    footer =
        roleMuted color $
            fitCell cols
                "↑↓/jk or scroll · click/enter resume · esc/q cancel · type to filter"

sessionWindow :: Int -> Int -> [a] -> [(Int, a)]
sessionWindow count selected xs =
    let total = length xs
        start = max 0 (min selected (total - count))
    in zip [start ..] (take count (drop start xs))

previewRows :: Int -> Int -> [Text] -> [Text]
previewRows width count logicalLines =
    let wrapped = concatMap (hardWrap width) logicalLines
        rows
            | null wrapped = ["(empty transcript)"]
            | otherwise = wrapped
    in drop (max 0 (length rows - count)) rows

hardWrap :: Int -> Text -> [Text]
hardWrap width raw
    | Text.null raw = [""]
    | otherwise = go raw
  where
    width' = max 1 width
    go text
        | Text.null text = []
        | otherwise =
            let (line, rest) = Text.splitAt width' text
            in line : go rest

fitCell :: Int -> Text -> Text
fitCell width raw
    | width <= 0 = ""
    | Text.length clean <= width =
        clean <> Text.replicate (width - Text.length clean) " "
    | width == 1 = "…"
    | otherwise = Text.take (width - 1) clean <> "…"
  where
    clean = Text.map (\c -> if c == '\t' || c == '\r' || c == '\n' then ' ' else c) raw

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
