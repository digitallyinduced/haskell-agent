{-# LANGUAGE LambdaCase #-}

module Agent.CLI.TUI.Composer.Logic
    ( ComposerEscapeAction(..)
    , KillDirection(..)
    , applyComposerUiEvent
    , combineKill
    , composerEscapeAction
    , currentSlashMenu
    , immediateBtwQuestion
    , isKillKey
    , selectedSlashSuggestion
    , slashReplacement
    , undoLimit
    ) where

import Agent.CLI.Command
    ( ReplAction(..)
    , SlashMenu(..)
    , SlashSuggestion(..)
    , parseReplLine
    , slashMenuForCatalog
    )
import Agent.CLI.Input (ReplLine(..))
import Agent.CLI.TUI.Types (AppState(..))
import Agent.TUI.Model (UiEvent(..), UiState(..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Graphics.Vty as V

data KillDirection = KillBackward | KillForward
    deriving (Eq, Show)

combineKill :: KillDirection -> Text -> Text -> Text
combineKill KillBackward killed buffer = killed <> buffer
combineKill KillForward killed buffer = buffer <> killed

undoLimit :: Int
undoLimit = 200

isKillKey :: V.Event -> Bool
isKillKey = \case
    V.EvKey V.KBS modifiers ->
        any (`elem` modifiers) [V.MMeta, V.MAlt, V.MCtrl]
    V.EvKey (V.KChar 'w') modifiers -> V.MCtrl `elem` modifiers
    V.EvKey (V.KChar 'u') modifiers -> V.MCtrl `elem` modifiers
    V.EvKey (V.KChar 'k') modifiers -> V.MCtrl `elem` modifiers
    V.EvKey (V.KChar 'd') modifiers ->
        (V.MMeta `elem` modifiers || V.MAlt `elem` modifiers)
            && V.MCtrl `notElem` modifiers
    _ -> False

immediateBtwQuestion :: UiState -> ReplLine -> Maybe Text
immediateBtwQuestion ui replLine
    | not ui.uiRunning = Nothing
    | otherwise = case replLine of
        ReplText text -> fromText text
        ReplPasted text -> fromText text
        _ -> Nothing
  where
    fromText text = case parseReplLine text of
        ReplBtw question -> Just question
        _ -> Nothing

applyComposerUiEvent :: UiEvent -> AppState -> AppState
applyComposerUiEvent uiEvent state =
    state
        { appSlashDismissed = resetOnDraft state.appSlashDismissed
        , appPasted = resetOnDraft state.appPasted
        , appHistoryIndex =
            if isDraft then Nothing else state.appHistoryIndex
        , appHistoryDraft =
            if isDraft then draftText else state.appHistoryDraft
        }
  where
    isDraft = case uiEvent of
        UiSetDraft _ _ -> True
        _ -> False
    draftText = case uiEvent of
        UiSetDraft text _ -> text
        _ -> state.appHistoryDraft
    resetOnDraft old = if isDraft then False else old

currentSlashMenu :: AppState -> Maybe SlashMenu
currentSlashMenu state
    | state.appSlashDismissed = Nothing
    | otherwise =
        slashMenuForCatalog state.appSlashCatalog
            state.appUi.uiDraft state.appUi.uiCursor

data ComposerEscapeAction
    = EscapeCancelTurn
    | EscapeDismissSlashMenu
    | EscapePreserveDraft
    deriving (Eq, Show)

composerEscapeAction :: Bool -> Bool -> ComposerEscapeAction
composerEscapeAction awaitingInput hasSlashMenu
    | not awaitingInput = EscapeCancelTurn
    | hasSlashMenu = EscapeDismissSlashMenu
    | otherwise = EscapePreserveDraft

selectedSlashSuggestion :: AppState -> SlashMenu -> Maybe SlashSuggestion
selectedSlashSuggestion state menu =
    case menu.slashMenuSuggestions of
        [] -> Nothing
        suggestions ->
            Just (suggestions !! (state.appSlashIndex `mod` length suggestions))

slashReplacement :: Text -> SlashMenu -> SlashSuggestion -> Text
slashReplacement draft menu suggestion =
    Text.take menu.slashMenuReplaceStart draft
        <> suggestion.slashSuggestionReplacement
        <> Text.drop menu.slashMenuReplaceEnd draft
