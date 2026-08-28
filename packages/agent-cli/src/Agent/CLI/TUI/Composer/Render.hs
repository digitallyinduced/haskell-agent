-- | Fullscreen composer rendering helpers.
module Agent.CLI.TUI.Composer.Render
    ( composerScrollbackAvailable
    , drawSlashMenu
    , slashMenuWindowStart
    , drawQueuedInputs
    , drawComposer
    , controlAttr
    , controlInteractionAttr
    ) where

import Agent.CLI.Command
    ( SlashMenu(..)
    , SlashSuggestion(..)
    )
import Agent.CLI.Input (truncateDisplayText, displayEditorText)
import Agent.CLI.TUI.Composer.Edit (wrapDraftWindow)
import Agent.CLI.TUI.Composer.Logic (currentSlashMenu)
import Agent.CLI.TUI.History (HistoryWindow, historyWindowHasBlocks)
import Agent.CLI.TUI.Types
import Agent.TUI.Model
import Agent.TUI.TextWidth (terminalTextImage)
import qualified Agent.TUI.Theme as Theme
import Brick
import qualified Brick.Types as B
import qualified Brick.Widgets.Border as Border
import Brick.Widgets.Border.Style (unicodeRounded)
import qualified Brick.Widgets.Border.Style as BorderStyle
import Data.List (intersperse)
import Data.Sequence (ViewL(..))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text

composerScrollbackAvailable :: UiState -> HistoryWindow -> Bool
composerScrollbackAvailable ui history =
    not (Seq.null ui.uiBlocks) || historyWindowHasBlocks history

drawSlashMenu :: AppState -> Widget Name
drawSlashMenu state = case currentSlashMenu state of
    Nothing -> emptyWidget
    Just menu ->
        let allSuggestions = menu.slashMenuSuggestions
            count = length allSuggestions
            selected
                | count == 0 = 0
                | otherwise = state.appSlashIndex `mod` count
            start = slashMenuWindowStart visibleSlashRows count selected
            suggestions = take visibleSlashRows (drop start allSuggestions)
        in padLeftRight 2 $
            withAttr Theme.borderAttr $
                withBorderStyle unicodeRounded $
                    Border.borderWithLabel (txt (menuLabel menu)) $
                        vBox $
                            zipWith
                                (drawSlashRow selected)
                                [start ..]
                                suggestions
  where
    menuLabel menu =
        case menu.slashMenuSuggestions of
            suggestions@(_ : _)
                | all
                    (("$" `Text.isPrefixOf`)
                        . (.slashSuggestionDisplay))
                    suggestions ->
                        " Skills "
            _ -> " Commands "

visibleSlashRows :: Int
visibleSlashRows = 6

-- | First visible row of the slash menu window. The selection stays roughly
-- centered once the menu scrolls, so moving through a long menu shifts the
-- window at the edges instead of pinning the highlight to the first row.
slashMenuWindowStart :: Int -> Int -> Int -> Int
slashMenuWindowStart visible count selected
    | count <= visible = 0
    | otherwise =
        max 0 (min (selected - (visible - 1) `div` 2) (count - visible))

drawSlashRow :: Int -> Int -> SlashSuggestion -> Widget Name
drawSlashRow selected index suggestion =
    let prefix = if selected == index then "❯ " else "  "
        row =
            hBox
                [ terminalTxt
                    (prefix <> suggestion.slashSuggestionDisplay)
                , vLimit 1 (fill ' ')
                , withAttr Theme.mutedAttr
                    (terminalTxt suggestion.slashSuggestionSummary)
                ]
        styled =
            if selected == index
                then withAttr Theme.selectedAttr row
                else row
    in clickable (SlashRow index) styled

drawQueuedInputs :: UiState -> Widget Name
drawQueuedInputs state =
    case Seq.viewl state.uiQueuedInputs of
        EmptyL -> emptyWidget
        next :< _ ->
            padLeftRight 2 $
                vLimit 1 $
                    hBox
                        [ withAttr Theme.toolAttr (txt "◇ ")
                        , withAttr Theme.footerAttr
                            (txt
                                ("queued "
                                    <> Text.pack
                                        (show (Seq.length state.uiQueuedInputs))
                                    <> " · "))
                        , withAttr Theme.mutedAttr
                            (terminalTxt (queuePreview next))
                        ]

queuePreview :: Text -> Text
queuePreview text =
    truncateDisplayText 100 (Text.unwords (Text.words text))

drawComposer :: AppState -> Widget Name
drawComposer appState =
    Widget Greedy Fixed do
        context <- getContext
        let focused = state.uiFocus == FocusComposer
            attr = if focused then Theme.borderActiveAttr else Theme.borderAttr
            draftWidth = max 1 (context.availWidth - composerDraftChromeWidth)
            draftLayout@(draftRows, _) =
                wrapDraftWindow
                    maxComposerRows
                    draftWidth
                    state.uiDraft
                    state.uiCursor
            bodyHeight = min maxComposerRows (length draftRows)
            leading =
                filter (not . Text.null)
                    [ if state.uiPrompt.promptAttachments > 0
                        then "image "
                            <> Text.pack
                                (show state.uiPrompt.promptAttachments)
                        else ""
                    , if Seq.null state.uiQueuedInputs
                        then
                            if not state.uiAwaitingInput
                                then "steer"
                                else ""
                        else "queued "
                            <> Text.pack
                                (show (Seq.length state.uiQueuedInputs))
                    ]
            label =
                if null leading
                    then Nothing
                    else Just $
                        hBox (intersperse (txt " · ") (map txt leading))
            editor =
                clickable ComposerArea $
                    padLeftRight 1 $
                        hBox
                            [ withAttr
                                (if focused
                                    then Theme.borderActiveAttr
                                    else Theme.mutedAttr)
                                (txt "❯ ")
                            , withAttr Theme.assistantAttr
                                (renderDraft
                                    focused
                                    bodyHeight
                                    state
                                    draftLayout)
                            ]
            composer =
                withBorderStyle unicodeRounded $
                    composerBorder
                        bodyHeight
                        (withAttr Theme.footerAttr <$> label)
                        (drawComposerStatus appState)
                        editor
        render (overrideAttr Border.borderAttr attr composer)
  where
    state = appState.appUi

-- | Columns consumed around the draft text: the two border columns, the
-- one-column padding on each side, and the two-cell @❯ @ prompt. The draft is
-- wrapped once in 'drawComposer' at the remaining width and the rows are
-- passed to 'renderDraft', so body height and cursor placement always agree;
-- drift between this constant and the actual chrome only changes the wrap
-- width.
composerDraftChromeWidth :: Int
composerDraftChromeWidth = 6

maxComposerRows :: Int
maxComposerRows = 8

composerBorder
    :: Int
    -> Maybe (Widget Name)
    -> Widget Name
    -> Widget Name
    -> Widget Name
composerBorder bodyHeight topLabel bottomLabel body =
    vBox
        [ hBox
            [ Border.borderElem BorderStyle.bsCornerTL
            , topBorder
            , Border.borderElem BorderStyle.bsCornerTR
            ]
        , vLimit bodyHeight $
            hBox [Border.vBorder, body, Border.vBorder]
        , hBox
            [ Border.borderElem BorderStyle.bsCornerBL
            , bottomBorder
            , Border.borderElem BorderStyle.bsCornerBR
            ]
        ]
  where
    topBorder = case topLabel of
        Nothing -> Border.hBorder
        Just label ->
            hBox
                [ hLimit 1 Border.hBorder
                , txt " "
                , label
                , txt " "
                , Border.hBorder
                ]
    bottomBorder =
        hBox
            [ Border.hBorder
            , txt " "
            , bottomLabel
            , txt " "
            ]

controlAttr :: AppState -> Name -> AttrName -> AttrName
controlAttr state name fallback =
    case controlInteractionAttr state name of
        Just attr -> attr
        Nothing -> fallback

controlInteractionAttr :: AppState -> Name -> Maybe AttrName
controlInteractionAttr state name
    | state.appPressedControl == Just name
    , state.appHoveredControl == Just name =
        Just Theme.controlLinkActiveAttr
    | state.appHoveredControl == Just name =
        Just Theme.controlLinkHoverAttr
    | otherwise =
        Nothing

-- | Render the precomputed bounded draft layout. Wrapping happens once in
-- 'drawComposer', so the visible height and cursor row agree without scanning
-- the complete draft.
renderDraft
    :: Bool
    -> Int
    -> UiState
    -> ([Text], (Int, Int))
    -> Widget Name
renderDraft focused height state (rows, (row, column)) =
    padRight Max cursorContent
  where
    firstVisibleRow = max 0 (row - height + 1)
    visibleRows = take height (drop firstVisibleRow rows)
    visibleCursorRow = row - firstVisibleRow
    content
        | Text.null state.uiDraft =
            withAttr Theme.mutedAttr $
                txt
                    (if not state.uiAwaitingInput
                        then "Type guidance…"
                        else " ")
        | otherwise =
            vBox (map renderRow visibleRows)
    cursorContent
        | focused =
            showCursor
                ComposerCursor
                (Location (column, visibleCursorRow))
                content
        | otherwise = content

    -- Empty visual rows still need one cell so Brick preserves their height
    -- and can place the insertion cursor on them.
    renderRow row
        | Text.null row = txt " "
        | otherwise = terminalTxt (displayEditorText row)

terminalTxt :: Text -> Widget n
terminalTxt text =
    B.Widget B.Fixed B.Fixed do
        context <- B.getContext
        attr <- B.lookupAttrName (B.ctxAttrName context)
        pure B.emptyResult
            { B.image = terminalTextImage attr text
            }

drawComposerStatus :: AppState -> Widget Name
drawComposerStatus state =
    hBox $
        intersperse (withAttr Theme.mutedAttr (txt " · ")) $
            accountLimit
                <> modelAndEffort
                <> [ modeControl
                   | not (Text.null mode)
                   ]
                <> [ if prompt.promptAccountSelectable
                        then accountControl
                        else withAttr Theme.mutedAttr (terminalTxt account)
                   | not (Text.null account)
                   ]
  where
    prompt = state.appUi.uiPrompt
    mode = prompt.promptMode
    account = prompt.promptAccount
    accountLimit =
        [ withAttr
            Theme.controlLinkAttr
            (terminalTxt limitStatus.promptLimitText)
        | limitStatus <- maybeToList prompt.promptLimitStatus
        ]
    modelControl =
        clickable ComposerModel $
            forceAttr
                (controlAttr state ComposerModel Theme.controlLinkAttr)
                (terminalTxt prompt.promptModel)
    effortControl =
        clickable ComposerEffort $
            forceAttr
                (controlAttr state ComposerEffort Theme.controlLinkAttr)
                (terminalTxt ("(" <> prompt.promptEffort <> ")"))
    modelAndEffort
        | Text.null prompt.promptModel = []
        | Text.null prompt.promptEffort = [modelControl]
        | otherwise = [hBox [modelControl, txt " ", effortControl]]
    modeControl =
        clickable ComposerMode $
            forceAttr
                (controlAttr state ComposerMode Theme.controlLinkAttr)
                (terminalTxt mode)
    accountControl =
        clickable ComposerAccount $
            forceAttr
                (controlAttr state ComposerAccount Theme.controlLinkAttr)
                (terminalTxt account)

    maybeToList = \case
        Nothing -> []
        Just value -> [value]
