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

-- | Approximate the wide/full-width ranges used by terminal emulators.
isWideCharacter :: Char -> Bool
isWideCharacter char =
    let code = ord char
    in code >= 0x1100
        && ( code <= 0x115f
            || code == 0x2329
            || code == 0x232a
            || (code >= 0x2e80 && code <= 0xa4cf && code /= 0x303f)
            || (code >= 0xac00 && code <= 0xd7a3)
            || (code >= 0xf900 && code <= 0xfaff)
            || (code >= 0xfe10 && code <= 0xfe19)
            || (code >= 0xfe30 && code <= 0xfe6f)
            || (code >= 0xff00 && code <= 0xff60)
            || (code >= 0xffe0 && code <= 0xffe6)
            || (code >= 0x1f300 && code <= 0x1faff)
            || (code >= 0x20000 && code <= 0x3fffd)
           )
