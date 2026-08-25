-- | Terminal-cell width classification shared by line editors and renderers.
module Agent.TUI.TextWidth
    ( charCellWidth
    , displayCharCellWidth
    , displayTerminalChar
    , displayTerminalText
    , isWideCharacter
    ) where

import Data.Char
    ( GeneralCategory(..)
    , chr
    , generalCategory
    , ord
    )
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Graphics.Vty as V

-- | Width of a Unicode character before any control-character visualization.
--
-- Combining marks occupy no additional cells. Control and unassigned
-- characters are also zero-width; callers that render them as replacement
-- glyphs should measure the replacement instead.
charCellWidth :: Char -> Int
charCellWidth char
    | category `elem` [NonSpacingMark, SpacingCombiningMark, EnclosingMark] = 0
    | category `elem` [Control, Surrogate, NotAssigned] = 0
    | isWideCharacter char = 2
    | otherwise = 1
  where
    category = generalCategory char

-- | Width used when a renderer displays terminal controls as visible
-- one-cell placeholders rather than treating them as zero-width input.
displayCharCellWidth :: Char -> Int
displayCharCellWidth char
    | char == '\n' || char == '\r' || char == '\t' = 1
    | code <= 0x1f || code == 0x7f = 1
    | code >= 0x80 && code <= 0x9f = 1
    | generalCategory char == Format = 1
    | otherwise = charCellWidth char
  where
    code = ord char

-- | Replace terminal control characters with visible, inert glyphs.
--
-- Newlines remain structural so width-aware renderers can split on them.
-- Every other replacement has the width reported by 'displayCharCellWidth'.
displayTerminalChar :: Char -> Text
displayTerminalChar char
    | char == '\n' = "\n"
    | char == '\r' = "↵"
    | char == '\t' = "⇥"
    | code >= 0 && code <= 0x1f =
        Text.singleton (chr (0x2400 + code))
    | code == 0x7f = "␡"
    | code >= 0x80 && code <= 0x9f = "�"
    | generalCategory char == Format = "�"
    | otherwise = Text.singleton char
  where
    code = ord char

displayTerminalText :: Text -> Text
displayTerminalText = Text.concatMap displayTerminalChar

-- | Report whether Vty's terminal-width table treats a character as wide.
--
-- Vty intentionally treats many standalone emoji code points as one cell,
-- while the harness has historically reserved two cells for the emoji block.
-- Keep that UI policy as a narrow override and delegate the rest of Unicode
-- width classification to the renderer's own table.
isWideCharacter :: Char -> Bool
isWideCharacter char =
    V.safeWcwidth char == 2
        || code >= 0x1f300 && code <= 0x1faff
  where
    code = ord char
