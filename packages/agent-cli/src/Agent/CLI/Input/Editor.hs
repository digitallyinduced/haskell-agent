-- | Pure state transitions for the first-party inline editor.
--
-- Terminal reads, redraws, dictation, interrupts, and history persistence stay
-- in "Agent.CLI.Input"; this module decides what the next editor state and
-- effect should be for each decoded key.
module Agent.CLI.Input.Editor
    ( EditorEffect(..)
    , EditorStep(..)
    , currentMenu
    , initialEditorState
    , reduceEditorKey
    ) where

import Agent.CLI.Command
    ( SlashCatalog
    , SlashMenu(..)
    , SlashSuggestion(..)
    , slashMenuForCatalog
    )
import Agent.CLI.Input.Types
    ( EditorKey(..)
    , EditorState(..)
    , ReplLine(..)
    )
import Agent.TUI.TextWidth
    ( nextGraphemeBoundary
    , previousGraphemeBoundary
    )
import Data.Char (isSpace)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

-- | The side effect requested by one pure editor transition.
data EditorEffect
    = RedrawEditor
    | SubmitEditor
    | ReturnEditor !ReplLine
    | CheckEditorInterrupt
    | ClearEditorScreen
    | DictateIntoEditor
    | ReportEditorError !Text
    | IgnoreEditorInput
    deriving (Eq, Show)

-- | The state and effect produced by reducing one decoded key.
data EditorStep = EditorStep
    { editorStepState :: !EditorState
    , editorStepEffect :: !EditorEffect
    }
    deriving (Eq, Show)

initialEditorState :: SlashCatalog -> Bool -> Text -> EditorState
initialEditorState catalog slashEnabled initial =
    EditorState
        { editorText = initial
        , editorCursor = Text.length initial
        , editorSelected = 0
        , editorHistoryIndex = Nothing
        , editorHistoryDraft = initial
        , editorKillBuffer = ""
        , editorPasted = False
        , editorSlashEnabled = slashEnabled
        , editorSlashDismissed = False
        , editorSlashCatalog = catalog
        }

-- | Decide the next editor state and requested effect for a decoded key.
--
-- History is passed newest-first, matching Haskeline's 'historyLines'.
reduceEditorKey :: [Text] -> EditorState -> EditorKey -> EditorStep
reduceEditorKey entries state key =
    case key of
        EditorEnter ->
            case selectedSuggestion state menu of
                Just suggestion
                    | not (exactCommandSelected state suggestion) ->
                        let accepted = acceptSuggestion state menu suggestion
                        in if suggestion.slashSuggestionTakesArguments
                            then redraw accepted
                            else effect accepted SubmitEditor
                _ -> effect state SubmitEditor
        EditorCycleMode ->
            effect state (ReturnEditor (ReplCycleMode state.editorText))
        EditorClipboardPaste images ->
            effect state $
                ReturnEditor (ReplClipboardPaste state.editorText images)
        EditorInterrupt ->
            effect state CheckEditorInterrupt
        EditorEof
            | Text.null state.editorText ->
                effect state (ReturnEditor ReplEof)
            | otherwise ->
                redraw (deleteAtCursor state)
        EditorUp
            | menuHasRows menu ->
                redraw (moveMenuSelection (-1) menu state)
            | otherwise ->
                redraw (historyMove (-1) entries state)
        EditorDown
            | menuHasRows menu ->
                redraw (moveMenuSelection 1 menu state)
            | otherwise ->
                redraw (historyMove 1 entries state)
        EditorTab ->
            case selectedSuggestion state menu of
                Nothing -> effect state IgnoreEditorInput
                Just suggestion ->
                    redraw (acceptSuggestion state menu suggestion)
        EditorEscape
            | menuHasRows menu ->
                redraw state
                    { editorSelected = 0
                    , editorSlashDismissed = True
                    }
            | otherwise ->
                effect state IgnoreEditorInput
        EditorBackspace -> redraw (backspace state)
        EditorDelete -> redraw (deleteAtCursor state)
        EditorLeft -> redraw state
            { editorCursor =
                previousGraphemeBoundary
                    state.editorText
                    state.editorCursor
            , editorSelected = 0
            , editorSlashDismissed = False
            }
        EditorRight -> redraw state
            { editorCursor =
                nextGraphemeBoundary
                    state.editorText
                    state.editorCursor
            , editorSelected = 0
            , editorSlashDismissed = False
            }
        EditorHome -> redraw state
            { editorCursor = 0
            , editorSelected = 0
            , editorSlashDismissed = False
            }
        EditorEnd -> redraw state
            { editorCursor = Text.length state.editorText
            , editorSelected = 0
            , editorSlashDismissed = False
            }
        EditorKillStart -> redraw (killStart state)
        EditorKillEnd -> redraw (killEnd state)
        EditorKillWord -> redraw (killWord state)
        EditorYank -> redraw (insertText state.editorKillBuffer state)
        EditorClearScreen -> effect state ClearEditorScreen
        EditorDictate -> effect state DictateIntoEditor
        EditorPaste pasted ->
            let pastedState =
                    (insertText pasted state) { editorPasted = True }
            in effect state $
                ReturnEditor
                    (ReplClipboardPasteOrText
                        state.editorText
                        pasted
                        pastedState.editorText)
        EditorInputError message ->
            effect state (ReportEditorError message)
        EditorChar char ->
            redraw (insertText (Text.singleton char) state)
        EditorIgnore ->
            effect state IgnoreEditorInput
  where
    menu = currentMenu state
    effect next requested = EditorStep
        { editorStepState = next
        , editorStepEffect = requested
        }
    redraw = flip effect RedrawEditor . normalizeSelection

menuHasRows :: Maybe SlashMenu -> Bool
menuHasRows = maybe False (not . null . (.slashMenuSuggestions))

currentMenu :: EditorState -> Maybe SlashMenu
currentMenu state
    | not state.editorSlashEnabled = Nothing
    | state.editorSlashDismissed = Nothing
    | otherwise =
        slashMenuForCatalog
            state.editorSlashCatalog
            state.editorText
            state.editorCursor

normalizeSelection :: EditorState -> EditorState
normalizeSelection state =
    case currentMenu state of
        Nothing -> state { editorSelected = 0 }
        Just menu ->
            let count = length menu.slashMenuSuggestions
            in state
                { editorSelected =
                    if count == 0 then 0 else state.editorSelected `mod` count
                }

moveMenuSelection :: Int -> Maybe SlashMenu -> EditorState -> EditorState
moveMenuSelection delta menu state =
    case menu of
        Nothing -> state
        Just SlashMenu{slashMenuSuggestions}
            | null slashMenuSuggestions -> state
            | otherwise ->
                let count = length slashMenuSuggestions
                    selected = (state.editorSelected + delta) `mod` count
                in state { editorSelected = selected }

selectedSuggestion
    :: EditorState
    -> Maybe SlashMenu
    -> Maybe SlashSuggestion
selectedSuggestion state menu = do
    SlashMenu{slashMenuSuggestions} <- menu
    if null slashMenuSuggestions
        then Nothing
        else
            Just $
                slashMenuSuggestions
                    !! (state.editorSelected `mod` length slashMenuSuggestions)

exactCommandSelected :: EditorState -> SlashSuggestion -> Bool
exactCommandSelected state suggestion =
    Text.strip state.editorText == suggestion.slashSuggestionDisplay

acceptSuggestion
    :: EditorState
    -> Maybe SlashMenu
    -> SlashSuggestion
    -> EditorState
acceptSuggestion state menu suggestion =
    case menu of
        Nothing -> state
        Just SlashMenu{slashMenuReplaceStart, slashMenuReplaceEnd} ->
            let (before, rest) =
                    Text.splitAt slashMenuReplaceStart state.editorText
                (_, after) =
                    Text.splitAt
                        (slashMenuReplaceEnd - slashMenuReplaceStart)
                        rest
                replacement = suggestion.slashSuggestionReplacement
                text = before <> replacement <> after
            in state
                { editorText = text
                , editorCursor =
                    slashMenuReplaceStart + Text.length replacement
                , editorSelected = 0
                , editorHistoryIndex = Nothing
                , editorHistoryDraft = text
                , editorSlashDismissed = False
                }

insertText :: Text -> EditorState -> EditorState
insertText inserted state =
    let (before, after) =
            Text.splitAt state.editorCursor state.editorText
        text = before <> inserted <> after
    in state
        { editorText = text
        , editorCursor = state.editorCursor + Text.length inserted
        , editorSelected = 0
        , editorHistoryIndex = Nothing
        , editorHistoryDraft = text
        , editorSlashDismissed = False
        }

backspace :: EditorState -> EditorState
backspace state
    | state.editorCursor <= 0 = state
    | otherwise =
        let start =
                previousGraphemeBoundary
                    state.editorText
                    state.editorCursor
            (before, rest) = Text.splitAt start state.editorText
            (_, after) =
                Text.splitAt (state.editorCursor - start) rest
            text = before <> after
        in state
            { editorText = text
            , editorCursor = start
            , editorSelected = 0
            , editorHistoryIndex = Nothing
            , editorHistoryDraft = text
            , editorSlashDismissed = False
            }

deleteAtCursor :: EditorState -> EditorState
deleteAtCursor state
    | state.editorCursor >= Text.length state.editorText = state
    | otherwise =
        let (before, rest) =
                Text.splitAt state.editorCursor state.editorText
            end =
                nextGraphemeBoundary
                    state.editorText
                    state.editorCursor
            (_, after) =
                Text.splitAt (end - state.editorCursor) rest
            text = before <> after
        in state
            { editorText = text
            , editorSelected = 0
            , editorHistoryIndex = Nothing
            , editorHistoryDraft = text
            , editorSlashDismissed = False
            }

killStart :: EditorState -> EditorState
killStart state =
    let (killed, after) =
            Text.splitAt state.editorCursor state.editorText
    in state
        { editorText = after
        , editorCursor = 0
        , editorSelected = 0
        , editorHistoryIndex = Nothing
        , editorHistoryDraft = after
        , editorKillBuffer = killed
        , editorSlashDismissed = False
        }

killEnd :: EditorState -> EditorState
killEnd state =
    let (before, killed) =
            Text.splitAt state.editorCursor state.editorText
    in state
        { editorText = before
        , editorSelected = 0
        , editorHistoryIndex = Nothing
        , editorHistoryDraft = before
        , editorKillBuffer = killed
        , editorSlashDismissed = False
        }

killWord :: EditorState -> EditorState
killWord state
    | state.editorCursor <= 0 = state
    | otherwise =
        let before = Text.take state.editorCursor state.editorText
            trailingSpaces =
                Text.length (Text.takeWhileEnd isSpace before)
            withoutSpaces = Text.dropEnd trailingSpaces before
            wordLength =
                Text.length (Text.takeWhileEnd (not . isSpace) withoutSpaces)
            start = state.editorCursor - trailingSpaces - wordLength
            killed =
                Text.take
                    (state.editorCursor - start)
                    (Text.drop start state.editorText)
            text =
                Text.take start state.editorText
                    <> Text.drop state.editorCursor state.editorText
        in state
            { editorText = text
            , editorCursor = start
            , editorSelected = 0
            , editorHistoryIndex = Nothing
            , editorHistoryDraft = text
            , editorKillBuffer = killed
            , editorSlashDismissed = False
            }

historyMove :: Int -> [Text] -> EditorState -> EditorState
historyMove delta entries state
    | null entries = state
    | otherwise =
        let current = fromMaybe (-1) state.editorHistoryIndex
            next = current - delta
        in if next < 0
            then state
                { editorText = state.editorHistoryDraft
                , editorCursor = Text.length state.editorHistoryDraft
                , editorHistoryIndex = Nothing
                , editorSelected = 0
                , editorSlashDismissed = False
                }
            else if next >= length entries
                then state
                else
                    let text = entries !! next
                        draft =
                            case state.editorHistoryIndex of
                                Nothing -> state.editorText
                                Just _ -> state.editorHistoryDraft
                    in state
                        { editorText = text
                        , editorCursor = Text.length text
                        , editorHistoryIndex = Just next
                        , editorHistoryDraft = draft
                        , editorSelected = 0
                        , editorSlashDismissed = False
                        }
