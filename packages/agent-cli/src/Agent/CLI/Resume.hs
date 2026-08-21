-- | Session resume picker for @/resume@.
module Agent.CLI.Resume
    ( ResumeEntry(..)
    , ResumeState(..)
    , applyResumeKey
    , formatResumeListing
    , initialResumeState
    , pickResumeSession
    , renderResumeFrame
    , resumeEntriesFrom
    , visibleResume
    ) where

import Agent.CLI.Picker (PickerKey(..), runOverlay)
import Agent.CLI.Session (SessionMeta(..))
import Agent.OsPath (toText)
import Agent.CLI.Style (roleMuted, rolePrompt, roleSuccess, roleWarn)
import Agent.Provider (providerSlug)
import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.OsPath (takeFileName)
import System.IO (hFlush, hIsTerminalDevice, stderr, stdin)

data ResumeEntry = ResumeEntry
    { resumeId :: !Text
    , resumeTitle :: !Text
    , resumeModel :: !Text
    , resumeCwd :: !Text
    , resumeWhen :: !Text
    , resumeProvider :: !Text
    }
    deriving (Eq, Show)

data ResumeState = ResumeState
    { resumeAll :: ![ResumeEntry]
    , resumeFilter :: !Text
    , resumeIndex :: !Int
    }
    deriving (Eq, Show)

resumeEntriesFrom :: [SessionMeta] -> [ResumeEntry]
resumeEntriesFrom = map toEntry
  where
    toEntry meta =
        ResumeEntry
            { resumeId = meta.metaId
            , resumeTitle =
                if Text.null meta.metaTitle then "(untitled)" else meta.metaTitle
            , resumeModel = meta.metaModel
            , resumeCwd = toText (takeFileName meta.metaCwd)
            , resumeWhen =
                Text.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M" meta.metaUpdatedAt)
            , resumeProvider = providerSlug meta.metaProvider
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

renderResumeFrame :: Bool -> ResumeState -> Text
renderResumeFrame color state =
    let visible = visibleResume state
        n = length visible
        idx = clamp n state.resumeIndex
        header = rolePrompt color "resume" <> roleMuted color " · pick a session"
        filterLine
            | Text.null state.resumeFilter =
                roleMuted color "filter: (type to narrow)"
            | otherwise =
                roleMuted color "filter: " <> roleWarn color state.resumeFilter
        body = case visible of
            [] -> [roleMuted color "(no sessions)"]
            opts ->
                zipWith
                    (\i e -> renderRow color (i == idx) e)
                    [0 ..]
                    opts
        footer = roleMuted color "↑↓/jk · enter · esc/q · type to filter"
    in Text.intercalate "\n" (header : filterLine : body <> [footer])

renderRow :: Bool -> Bool -> ResumeEntry -> Text
renderRow color selected e =
    let cursor = if selected then roleWarn color "› " else "  "
        title = if selected then roleSuccess color e.resumeTitle else e.resumeTitle
        meta =
            roleMuted color
                (e.resumeWhen <> "  " <> e.resumeModel <> "  " <> e.resumeCwd)
    in cursor <> title <> "\n    " <> meta

formatResumeListing :: Bool -> [ResumeEntry] -> Text
formatResumeListing color entries =
    if null entries
        then roleMuted color "no sessions in ~/.haskell-agent/sessions"
        else Text.intercalate "\n" (map (formatOne color) entries)

formatOne :: Bool -> ResumeEntry -> Text
formatOne color e =
    roleMuted color (Text.take 8 e.resumeId)
        <> "  "
        <> e.resumeTitle
        <> roleMuted color ("  " <> e.resumeModel)

-- | TTY picker; non-TTY prints the list. Confirm returns the session id.
pickResumeSession :: Bool -> [SessionMeta] -> IO (Maybe Text)
pickResumeSession color metas = do
    let entries = resumeEntriesFrom metas
    isTty <- hIsTerminalDevice stdin
    if not isTty
        then do
            Text.hPutStrLn stderr (formatResumeListing color entries)
            hFlush stderr
            pure Nothing
        else do
            result <-
                runOverlay
                    (renderResumeFrame color)
                    applyResumeKey
                    (initialResumeState entries)
            pure $ case result of
                Just (Just entry) -> Just entry.resumeId
                _ -> Nothing
