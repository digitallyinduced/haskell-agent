{-# LANGUAGE LambdaCase #-}

module Agent.CLI.TUI.Composer.Logic
    ( ComposerEscapeAction(..)
    , KillDirection(..)
    , applyComposerUiEvent
    , combineKill
    , composerEscapeAction
    , currentSlashMenu
    , imagePlaceholder
    , immediateBtwQuestion
    , immediateReplCommand
    , insertImagePlaceholders
    , isKillKey
    , removeImagePlaceholderAt
    , selectedSlashSuggestion
    , slashReplacement
    , undoLimit
    ) where

import Agent.CLI.Command
    ( CopyRequest(..)
    , ReplAction(..)
    , SlashMenu(..)
    , SlashSuggestion(..)
    , parseReplLine
    , slashMenuForCatalog
    )
import Agent.CLI.Input (ReplLine(..))
import Agent.CLI.TUI.Types (AppState(..))
import Agent.TUI.Model (PromptState(..), UiEvent(..), UiState(..))
import Data.Char (isSpace)
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

-- | 1-based label inserted at the paste caret, e.g. @[image 1]@.
imagePlaceholder :: Int -> Text
imagePlaceholder n = "[image " <> Text.pack (show n) <> "]"

-- | Insert labels for newly attached images at the caret so the model can
-- tell which later word refers to which image.
insertImagePlaceholders :: Text -> Int -> Int -> Int -> (Text, Int)
insertImagePlaceholders draft cursor firstIndex addedCount
    | addedCount <= 0 || firstIndex < 1 = (draft, clamped)
    | otherwise = insertPlaceholderText draft clamped placeholders
  where
    clamped = clampCursor draft cursor
    placeholders =
        Text.unwords
            [ imagePlaceholder n
            | n <- [firstIndex .. firstIndex + addedCount - 1]
            ]

-- | Drop the label for a removed attachment and shift later labels down so
-- they stay aligned with the remaining attachment order.
removeImagePlaceholderAt :: Text -> Int -> Int -> Int -> (Text, Int)
removeImagePlaceholderAt draft cursor removedIndex oldCount
    | removedIndex < 1 || removedIndex > oldCount =
        (draft, clampCursor draft cursor)
    | otherwise =
        let (without, mapRemoved) =
                removeFirstPlaceholder draft (imagePlaceholder removedIndex)
            cursorAfterRemove = mapRemoved (clampCursor draft cursor)
            (renumbered, cursorFinal) =
                foldl'
                    (\(text, cur) n ->
                        replaceAllAdjustingCursor
                            text
                            (imagePlaceholder n)
                            (imagePlaceholder (n - 1))
                            cur)
                    (without, cursorAfterRemove)
                    [removedIndex + 1 .. oldCount]
        in (renumbered, clampCursor renumbered cursorFinal)

insertPlaceholderText :: Text -> Int -> Text -> (Text, Int)
insertPlaceholderText draft cursor inserted
    | Text.null inserted = (draft, cursor)
    | otherwise =
        let before = Text.take cursor draft
            after = Text.drop cursor draft
            leading
                | needsSpaceAfter before inserted = " "
                | otherwise = ""
            trailing
                | Text.null after = " "
                | needsSpaceBefore (leading <> inserted) after = " "
                | otherwise = ""
            text = leading <> inserted <> trailing
        in (before <> text <> after, cursor + Text.length text)

removeFirstPlaceholder :: Text -> Text -> (Text, Int -> Int)
removeFirstPlaceholder haystack needle
    | Text.null needle = (haystack, id)
    | otherwise =
        case Text.breakOn needle haystack of
            (_, rest) | Text.null rest -> (haystack, id)
            (before, rest) ->
                let after = Text.drop (Text.length needle) rest
                    start = Text.length before
                    end = start + Text.length needle
                    collapsedAfter =
                        Text.take 1 after == " "
                            && (Text.null before || Text.takeEnd 1 before == " ")
                    collapsedBefore =
                        Text.null after && Text.takeEnd 1 before == " "
                    deletedStart = start - if collapsedBefore then 1 else 0
                    deletedEnd = end + if collapsedAfter then 1 else 0
                    joined
                        | collapsedBefore = Text.dropEnd 1 before
                        | collapsedAfter && Text.null before = Text.drop 1 after
                        | collapsedAfter = before <> Text.drop 1 after
                        | otherwise = before <> after
                    mapCursor cursor
                        | cursor <= deletedStart = cursor
                        | cursor >= deletedEnd =
                            cursor - (deletedEnd - deletedStart)
                        | otherwise = deletedStart
                in (joined, mapCursor)

replaceAllAdjustingCursor :: Text -> Text -> Text -> Int -> (Text, Int)
replaceAllAdjustingCursor haystack old new cursor
    | Text.null old || old == new = (haystack, cursor)
    | otherwise =
        let pieces = Text.splitOn old haystack
        in (Text.intercalate new pieces, mapCursor pieces)
  where
    oldLen = Text.length old
    newLen = Text.length new
    mapCursor pieces = go 0 0 pieces
    go _ newOff [] = newOff
    go oldOff newOff [_] = newOff + max 0 (cursor - oldOff)
    go oldOff newOff (piece : rest) =
        let pieceLen = Text.length piece
            needleStart = oldOff + pieceLen
            needleEnd = needleStart + oldLen
        in if cursor <= needleStart
            then newOff + (cursor - oldOff)
            else if cursor < needleEnd
                then newOff + pieceLen
                else go needleEnd (newOff + pieceLen + newLen) rest

clampCursor :: Text -> Int -> Int
clampCursor text cursor = max 0 (min (Text.length text) cursor)

needsSpaceAfter :: Text -> Text -> Bool
needsSpaceAfter left right =
    maybe False (not . isSpace) (lastChar left)
        && maybe False (not . startsWithPunctuation) (firstChar right)

needsSpaceBefore :: Text -> Text -> Bool
needsSpaceBefore left right =
    maybe False (not . isSpace) (lastChar left)
        && maybe False (not . isSpace) (firstChar right)

firstChar :: Text -> Maybe Char
firstChar = fmap fst . Text.uncons

lastChar :: Text -> Maybe Char
lastChar = fmap snd . Text.unsnoc

startsWithPunctuation :: Char -> Bool
startsWithPunctuation char =
    char `elem` (".,;:!?)]}" :: String)

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

-- | Read-only inspection and clipboard commands may safely run while the
-- provider turn continues.
immediateReplCommand :: UiState -> ReplLine -> Maybe ReplAction
immediateReplCommand ui replLine
    | not ui.uiRunning = Nothing
    | otherwise = case replLine of
        ReplText text -> fromText text
        ReplPasted text -> fromText text
        _ -> Nothing
  where
    fromText text = case parseReplLine text of
        action@(ReplCopy request)
            | request.copyResponseIndex == 1
            , Nothing <- request.copyDestination ->
                Just action
        action@ReplCopyCode{} -> Just action
        ReplCopyDiff -> Just ReplCopyDiff
        ReplCopyPath -> Just ReplCopyPath
        ReplCopySession -> Just ReplCopySession
        ReplQueue -> Just ReplQueue
        ReplContext -> Just ReplContext
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
        , appUi =
            case uiEvent of
                UiSetPrompt prompt | state.appComposerOwnsImagePreviews ->
                    let ui = state.appUi
                    in ui { uiPrompt =
                        prompt { promptAttachments = length state.appImagePreviews } }
                _ -> state.appUi
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
